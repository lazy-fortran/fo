module fo_mcp_response
    use fo_json, only: json_bool, json_int, json_escape, extract_json_field
    implicit none
    private
    public :: make_initialize_response, make_tools_list_response
    public :: make_resources_list_response, make_run_start_response
    public :: make_tool_text_response

contains

    subroutine make_initialize_response(id_str, line, response)
        character(len=*), intent(in) :: id_str, line
        character(len=*), intent(out) :: response

        character(len=32) :: proto_ver

        call extract_json_field(line, '"protocolVersion"', proto_ver)
        if (len_trim(proto_ver) == 0) proto_ver = '2025-03-26'
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"protocolVersion":"'//trim(proto_ver)//'",'// &
                   '"capabilities":{"tools":{"listChanged":false},'// &
                   '"resources":{"listChanged":false}},'// &
                   '"serverInfo":{"name":"fo","version":"0.1.0"}}}'
    end subroutine make_initialize_response

    subroutine make_tools_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"tools":[{"name":"fo",'// &
                   '"description":"Fortran build driver",'// &
                   '"inputSchema":{"type":"object","properties":{'// &
                   '"action":{"type":"string",'// &
                   '"enum":["check","status","diagnostics","cancel",'// &
                   '"build","test","graph","info","changed","clean","lint"],'// &
                   '"description":"Action to run"}},'// &
                   '"required":["action"]}}]}}'
    end subroutine make_tools_list_response

    subroutine make_resources_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"resources":[{"uri":"fo://diagnostics",'// &
                   '"name":"diagnostics",'// &
                   '"description":"Current fo check diagnostics",'// &
                   '"mimeType":"text/plain"}]}}'
    end subroutine make_resources_list_response

    subroutine make_run_start_response(id_str, run_id, pending, response)
        character(len=*), intent(in) :: id_str
        integer, intent(in) :: run_id
        logical, intent(in) :: pending
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"run_id":'//trim(json_int(run_id))// &
                   ',"state":"'
        if (pending) then
            response = trim(response)//'rerun-pending"'
        else
            response = trim(response)//'running"'
        end if
        response = trim(response)//',"pending":'//trim(json_bool(pending))//'}}'
    end subroutine make_run_start_response

    subroutine make_tool_text_response(id_str, output_text, exitcode, response)
        character(len=*), intent(in) :: id_str, output_text
        integer, intent(in) :: exitcode
        character(len=*), intent(out) :: response

        character(len=16384) :: escaped

        escaped = output_text
        call json_escape(escaped)
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                   '"result":{"content":[{"type":"text",'// &
                   '"text":"'//trim(escaped)//'"}],"isError":'// &
                   trim(json_bool(exitcode /= 0))//'}}'
    end subroutine make_tool_text_response

end module fo_mcp_response
