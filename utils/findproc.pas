unit findproc;

interface

function ProcessExists(exeFileName: string): Boolean;

implementation

uses Winapi.Windows, TLHelp32, System.SysUtils;

function ProcessExists(exeFileName: string): Boolean;
var
    ContinueLoop: BOOL;
    FSnapshotHandle: THandle;
    FProcessEntry32: TProcessEntry32;
    FProcessEntry32_szExeFile: string;
begin
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    Result := False;
    exeFileName := UpperCase(exeFileName);

    while Integer(ContinueLoop) <> 0 do
    begin
        FProcessEntry32_szExeFile := UpperCase(FProcessEntry32.szExeFile);
        if (ExtractFileName(FProcessEntry32_szExeFile) = exeFileName) or
          (FProcessEntry32_szExeFile = exeFileName) then
        begin
            CloseHandle(FSnapshotHandle);
            exit(true);
        end;
        ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
end;

end.
