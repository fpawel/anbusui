unit app;

interface
uses  inifiles, server_data_types;

var
    AppSets :  TInifile;
    AppVars: TArray<TVar>;


function AppVarName(code:integer):string;
function AppVarCode(name:string):integer;
function GetAppHttpAddr: string;

implementation

uses registry, winapi.windows, sysutils, stringutils;

function AppVarName(code:integer):string;
var
  I: Integer;
begin
    for I := 0 to Length(AppVars)-1 do
        if AppVars[i].Code = code then
            exit(AppVars[i].Name);
    exit(inttostr2(code));
end;

function AppVarCode(name:string):integer;
var
  I: Integer;
begin
    for I := 0 to Length(AppVars)-1 do
        if AppVars[i].Name = name then
            exit(AppVars[i].Code);
    exit(-1);
end;

function GetAppHttpAddr: string;
var
    key: TRegistry;
begin
    key := TRegistry.Create(KEY_READ);
    try
        if not key.OpenKey('anbus\http', False) then
            raise Exception.Create('cant open anbus\http');
        result := key.ReadString('addr');
    finally
        key.CloseKey;
        key.Free;
    end;
end;


initialization

AppSets := TIniFile.Create(ChangeFileExt(paramstr(0), '.ini'));

end.

