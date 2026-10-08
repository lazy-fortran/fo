param(
    [string]$ToolchainBin = 'C:\fo\msys64\ucrt64\bin',
    [string]$FxInclude = (Join-Path $PSScriptRoot 'include'),
    [string]$OutputDirectory = (Join-Path $env:TEMP 'fo-watch-native-probe')
)
$ErrorActionPreference = 'Stop'
$env:PATH = "$ToolchainBin;$env:SystemRoot\System32;$env:SystemRoot"
$cc = Join-Path $ToolchainBin 'gcc.exe'
$fc = Join-Path $ToolchainBin 'gfortran.exe'
# A frozen flat closure or the repository's production source; never installed Fo.
$source = Join-Path $PSScriptRoot 'fo_change_watch.c'
if (!(Test-Path $source)) {
    $source = Join-Path (Split-Path (Split-Path $PSScriptRoot)) 'src\watch\fo_change_watch.c'
}
New-Item -ItemType Directory -Force $OutputDirectory | Out-Null
$watch = Join-Path $OutputDirectory 'watch.o'
$helpers = Join-Path $OutputDirectory 'helpers.o'
$cprobe = Join-Path $OutputDirectory 'watch-probe.exe'
$fprobe = Join-Path $OutputDirectory 'watch-fortran.exe'
& $cc -std=c11 -Wall -Wextra -Werror -I $FxInclude -c $source -o $watch
if ($LASTEXITCODE) { throw 'watch provider compile failed' }
& $cc -std=c11 -Wall -Wextra -Werror -I $FxInclude $watch "$PSScriptRoot\test_change_watch_windows.c" -o $cprobe
if ($LASTEXITCODE) { throw 'C oracle compile failed' }
& $cprobe
if ($LASTEXITCODE) { throw 'C oracle failed' }
& $cc -std=c11 -Wall -Wextra -Werror -DWP_HELPERS_ONLY -I $FxInclude -c "$PSScriptRoot\test_change_watch_windows.c" -o $helpers
if ($LASTEXITCODE) { throw 'ABI helper compile failed' }
& $fc -Wall -Wextra -Werror -fcheck=all "$PSScriptRoot\test_change_watch_windows.f90" $helpers $watch -o $fprobe
if ($LASTEXITCODE) { throw 'Fortran oracle compile failed' }
& $fprobe
if ($LASTEXITCODE) { throw 'Fortran oracle failed' }
$hprobe = Join-Path $OutputDirectory 'watch-handles.exe'
& $cc -std=c11 -Wall -Wextra -Werror -I $FxInclude "$PSScriptRoot\test_change_watch_handles.c" $watch $helpers -o $hprobe
if ($LASTEXITCODE) { throw 'handle oracle compile failed' }
& $hprobe
if ($LASTEXITCODE) { throw 'handle oracle failed' }
