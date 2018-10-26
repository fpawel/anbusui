unit runhostapp;

interface

procedure Run_Host_App;

implementation

uses sysutils, shellapi, findproc, Winapi.Windows, vcl.forms;

procedure Run_Host_App;
var
    exe_dir, exe_file_name: string;
begin
    exe_dir := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable
      ('HOMEPATH') + '\.panalib\';
    exe_file_name := exe_dir + 'panalibhost.exe';
    ShellExecute(0, 'open', PChar(exe_file_name), '-waitpeer=true',
      PChar(exe_dir), SW_SHOWNORMAL);
    while not ProcessExists('panalibhost.exe') do
        Application.ProcessMessages;
end;

end.
