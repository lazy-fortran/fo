#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>

#define JOURNAL_LINE_MAX (256 * 1024)
#define JSON_DEPTH_MAX 64
#define COMPLETION_ID_MAX 4095

typedef struct {
    const char *text;
    size_t length;
    size_t offset;
} json_parser_t;

typedef struct {
    char completion_id[COMPLETION_ID_MAX + 1];
    char outcome[16];
    int have_id;
    int have_outcome;
} receipt_fields_t;

static void skip_space(json_parser_t *parser) {
    while (parser->offset < parser->length) {
        char c = parser->text[parser->offset];
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n') break;
        parser->offset++;
    }
}

static int hex_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

static int parse_hex_quad(json_parser_t *parser, uint32_t *value) {
    int digit;
    int i;
    uint32_t result = 0;
    if (parser->length - parser->offset < 4) return 1;
    for (i = 0; i < 4; i++) {
        digit = hex_value(parser->text[parser->offset++]);
        if (digit < 0) return 1;
        result = (result << 4) | (uint32_t)digit;
    }
    *value = result;
    return 0;
}

/* Parse a JSON string, decoding ASCII into output when it is supplied. */
static int parse_string(json_parser_t *parser, char *output, size_t capacity,
                        size_t *output_length, int *is_ascii) {
    size_t used = 0;
    int ascii = 1;
    if (parser->offset >= parser->length || parser->text[parser->offset++] != '"')
        return 1;
    while (parser->offset < parser->length) {
        unsigned char c = (unsigned char)parser->text[parser->offset++];
        uint32_t codepoint;
        if (c == '"') {
            if (output != NULL) {
                if (used >= capacity) return 1;
                output[used] = '\0';
            }
            if (output_length != NULL) *output_length = used;
            if (is_ascii != NULL) *is_ascii = ascii;
            return 0;
        }
        if (c < 0x20) return 1;
        if (c == '\\') {
            if (parser->offset >= parser->length) return 1;
            c = (unsigned char)parser->text[parser->offset++];
            if (c == '"' || c == '\\' || c == '/') {
                /* The escaped ASCII byte is already decoded. */
            } else if (c == 'b') c = '\b';
            else if (c == 'f') c = '\f';
            else if (c == 'n') c = '\n';
            else if (c == 'r') c = '\r';
            else if (c == 't') c = '\t';
            else if (c == 'u') {
                if (parse_hex_quad(parser, &codepoint) != 0) return 1;
                if (codepoint >= 0xd800 && codepoint <= 0xdbff) {
                    uint32_t low;
                    if (parser->length - parser->offset < 2 ||
                        parser->text[parser->offset++] != '\\' ||
                        parser->text[parser->offset++] != 'u' ||
                        parse_hex_quad(parser, &low) != 0 ||
                        low < 0xdc00 || low > 0xdfff) return 1;
                    codepoint = 0x10000 + ((codepoint - 0xd800) << 10) +
                                (low - 0xdc00);
                } else if (codepoint >= 0xdc00 && codepoint <= 0xdfff) {
                    return 1;
                }
                if (codepoint > 0x7f) {
                    ascii = 0;
                    continue;
                }
                c = (unsigned char)codepoint;
            } else {
                return 1;
            }
        } else if (c >= 0x80) {
            ascii = 0;
            continue;
        }
        if (output != NULL && ascii) {
            if (used + 1 >= capacity) return 1;
            output[used++] = (char)c;
        }
    }
    return 1;
}

static int parse_value(json_parser_t *parser, unsigned int depth);

static int parse_object(json_parser_t *parser, unsigned int depth,
                        receipt_fields_t *fields, int top_level) {
    char key[128];
    size_t key_length;
    int key_ascii;
    if (depth > JSON_DEPTH_MAX || parser->offset >= parser->length ||
        parser->text[parser->offset++] != '{') return 1;
    skip_space(parser);
    if (parser->offset < parser->length && parser->text[parser->offset] == '}') {
        parser->offset++;
        return 0;
    }
    for (;;) {
        int is_id, is_outcome;
        skip_space(parser);
        if (parse_string(parser, key, sizeof(key), &key_length, &key_ascii) != 0)
            return 1;
        (void)key_length;
        skip_space(parser);
        if (parser->offset >= parser->length || parser->text[parser->offset++] != ':')
            return 1;
        skip_space(parser);
        is_id = top_level && key_ascii && strcmp(key, "completion_id") == 0;
        is_outcome = top_level && key_ascii && strcmp(key, "outcome") == 0;
        if (is_id || is_outcome) {
            char *target = is_id ? fields->completion_id : fields->outcome;
            size_t capacity = is_id ? sizeof(fields->completion_id) :
                                      sizeof(fields->outcome);
            size_t value_length;
            int value_ascii;
            if ((is_id && fields->have_id) || (is_outcome && fields->have_outcome) ||
                parse_string(parser, target, capacity, &value_length, &value_ascii) != 0 ||
                !value_ascii || value_length == 0) return 1;
            if (is_id && value_length != strlen(target)) return 1;
            if (is_id) fields->have_id = 1;
            else fields->have_outcome = 1;
        } else if (parse_value(parser, depth + 1) != 0) {
            return 1;
        }
        skip_space(parser);
        if (parser->offset >= parser->length) return 1;
        if (parser->text[parser->offset] == '}') {
            parser->offset++;
            return 0;
        }
        if (parser->text[parser->offset++] != ',') return 1;
        skip_space(parser);
    }
}

static int parse_array(json_parser_t *parser, unsigned int depth) {
    if (depth > JSON_DEPTH_MAX || parser->offset >= parser->length ||
        parser->text[parser->offset++] != '[') return 1;
    skip_space(parser);
    if (parser->offset < parser->length && parser->text[parser->offset] == ']') {
        parser->offset++;
        return 0;
    }
    for (;;) {
        if (parse_value(parser, depth + 1) != 0) return 1;
        skip_space(parser);
        if (parser->offset >= parser->length) return 1;
        if (parser->text[parser->offset] == ']') {
            parser->offset++;
            return 0;
        }
        if (parser->text[parser->offset++] != ',') return 1;
    }
}

static int parse_number(json_parser_t *parser) {
    size_t start = parser->offset;
    if (parser->offset < parser->length && parser->text[parser->offset] == '-')
        parser->offset++;
    if (parser->offset >= parser->length) return 1;
    if (parser->text[parser->offset] == '0') {
        parser->offset++;
        if (parser->offset < parser->length && parser->text[parser->offset] >= '0' &&
            parser->text[parser->offset] <= '9') return 1;
    } else {
        if (parser->text[parser->offset] < '1' || parser->text[parser->offset] > '9')
            return 1;
        while (parser->offset < parser->length && parser->text[parser->offset] >= '0' &&
               parser->text[parser->offset] <= '9') parser->offset++;
    }
    if (parser->offset < parser->length && parser->text[parser->offset] == '.') {
        parser->offset++;
        if (parser->offset >= parser->length || parser->text[parser->offset] < '0' ||
            parser->text[parser->offset] > '9') return 1;
        while (parser->offset < parser->length && parser->text[parser->offset] >= '0' &&
               parser->text[parser->offset] <= '9') parser->offset++;
    }
    if (parser->offset < parser->length &&
        (parser->text[parser->offset] == 'e' || parser->text[parser->offset] == 'E')) {
        parser->offset++;
        if (parser->offset < parser->length &&
            (parser->text[parser->offset] == '+' || parser->text[parser->offset] == '-'))
            parser->offset++;
        if (parser->offset >= parser->length || parser->text[parser->offset] < '0' ||
            parser->text[parser->offset] > '9') return 1;
        while (parser->offset < parser->length && parser->text[parser->offset] >= '0' &&
               parser->text[parser->offset] <= '9') parser->offset++;
    }
    return parser->offset == start ? 1 : 0;
}

static int parse_literal(json_parser_t *parser, const char *literal) {
    size_t length = strlen(literal);
    if (parser->length - parser->offset < length ||
        memcmp(parser->text + parser->offset, literal, length) != 0) return 1;
    parser->offset += length;
    return 0;
}

static int parse_value(json_parser_t *parser, unsigned int depth) {
    if (depth > JSON_DEPTH_MAX) return 1;
    skip_space(parser);
    if (parser->offset >= parser->length) return 1;
    switch (parser->text[parser->offset]) {
    case '{': return parse_object(parser, depth, NULL, 0);
    case '[': return parse_array(parser, depth);
    case '"': return parse_string(parser, NULL, 0, NULL, NULL);
    case 't': return parse_literal(parser, "true");
    case 'f': return parse_literal(parser, "false");
    case 'n': return parse_literal(parser, "null");
    default: return parse_number(parser);
    }
}

static int parse_receipt(const char *text, size_t length, receipt_fields_t *fields) {
    json_parser_t parser = {text, length, 0};
    memset(fields, 0, sizeof(*fields));
    skip_space(&parser);
    if (parser.offset >= parser.length || parser.text[parser.offset] != '{' ||
        parse_object(&parser, 1, fields, 1) != 0) return 1;
    skip_space(&parser);
    if (parser.offset != parser.length || !fields->have_id || !fields->have_outcome)
        return 1;
    if (strcmp(fields->outcome, "pass") != 0 &&
        strcmp(fields->outcome, "fail") != 0) return 1;
    return 0;
}

static int validate_receipt(const char *record, size_t length,
                            receipt_fields_t *fields) {
    if (record == NULL || length == 0) return 1;
    if (length > JOURNAL_LINE_MAX) return 4;
    if (memchr(record, '\n', length) != NULL || memchr(record, '\r', length) != NULL ||
        memchr(record, '\0', length) != NULL) return 1;
    return parse_receipt(record, length, fields);
}

static int sync_parent(const char *path) {
    char *copy = strdup(path), *slash;
    int fd, result;
    if (copy == NULL) return -1;
    slash = strrchr(copy, '/');
    if (slash == NULL) strcpy(copy, ".");
    else if (slash == copy) slash[1] = '\0';
    else *slash = '\0';
    fd = open(copy, O_RDONLY | O_DIRECTORY);
    free(copy);
    if (fd < 0) return -1;
    result = fsync(fd);
    close(fd);
    return result;
}

int fo_c_gremlin_journal_validate_record(const char *record) {
    receipt_fields_t fields;
    size_t length;
    if (record == NULL) return 1;
    length = strlen(record);
    if (validate_receipt(record, length, &fields) != 0) return 1;
    return 0;
}

int fo_c_gremlin_journal_append(const char *path, const char *completion_id,
                                const char *record) {
    int fd = -1, result = 2;
    size_t length, written, id_length;
    receipt_fields_t fields, existing;
    struct stat st;
    off_t end, complete_end = 0, scan_offset = 0;
    char line[JOURNAL_LINE_MAX + 1];
    char chunk[8192];
    size_t line_length = 0;
    int oversized = 0;
    int found = 0, found_identical = 0;

    if (path == NULL || completion_id == NULL || record == NULL ||
        path[0] == '\0' || completion_id[0] == '\0') return 1;
    id_length = strlen(completion_id);
    if (id_length > COMPLETION_ID_MAX) return 1;
    for (size_t i = 0; i < id_length; i++) {
        unsigned char c = (unsigned char)completion_id[i];
        if (c < 0x20 || c > 0x7e) return 1;
    }
    length = strlen(record);
    result = validate_receipt(record, length, &fields);
    if (result != 0) return result;
    if (strcmp(fields.completion_id, completion_id) != 0) return 1;
    result = 2;

    fd = open(path, O_CREAT | O_EXCL | O_RDWR | O_APPEND, 0600);
    if (fd < 0 && errno == EEXIST) fd = open(path, O_RDWR | O_APPEND);
    if (fd < 0) return 2;
    if (flock(fd, LOCK_EX) != 0) goto done;
    if (fstat(fd, &st) != 0) goto done;
    end = st.st_size;
    while (scan_offset < end) {
        size_t want = sizeof(chunk);
        ssize_t count;
        size_t i;
        off_t remaining = end - scan_offset;
        if ((off_t)want > remaining) want = (size_t)remaining;
        count = pread(fd, chunk, want, scan_offset);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) goto done;
        for (i = 0; i < (size_t)count; i++) {
            unsigned char c = (unsigned char)chunk[i];
            if (c == '\n') {
                if (oversized) {
                    result = 4;
                    goto done;
                }
                line[line_length] = '\0';
                result = validate_receipt(line, line_length, &existing);
                if (result != 0) {
                    result = 1;
                    goto done;
                }
                if (strcmp(existing.completion_id, completion_id) == 0) {
                    if (found) {
                        result = 3;
                        goto done;
                    }
                    found = 1;
                    found_identical = line_length == length &&
                        memcmp(line, record, length) == 0;
                }
                complete_end = scan_offset + (off_t)i + 1;
                line_length = 0;
                oversized = 0;
            } else if (!oversized) {
                if (line_length == JOURNAL_LINE_MAX) oversized = 1;
                else line[line_length++] = (char)c;
            }
        }
        scan_offset += count;
    }
    if (found) {
        result = found_identical && fsync(fd) == 0 && sync_parent(path) == 0 ? 0 :
                 (found_identical ? 2 : 3);
        goto done;
    }
    if (complete_end < end && ftruncate(fd, complete_end) != 0) goto done;
    if (lseek(fd, 0, SEEK_END) < 0) goto done;
    written = 0;
    while (written < length) {
        ssize_t count = write(fd, record + written, length - written);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) goto done;
        written += (size_t)count;
    }
    {
        ssize_t count;
        do {
            count = write(fd, "\n", 1);
        } while (count < 0 && errno == EINTR);
        if (count != 1) goto done;
    }
    if (fsync(fd) != 0 || sync_parent(path) != 0) goto done;
    result = 0;
done:
    flock(fd, LOCK_UN);
    close(fd);
    return result;
}
