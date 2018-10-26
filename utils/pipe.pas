unit pipe;

interface

uses System.Classes, Winapi.Windows, REST.Json, System.SyncObjs,
    System.Generics.Collections,
    sysutils, ujsonrpc, superobject;

function GetPipeResponse(pipeServer, request: string): string;

function GetPipeJsonRpcResponse(const pipeServer: string; const id: string;
  const method: string; params: ISuperObject): ISuperObject;

implementation

uses System.WideStrUtils, System.dateutils, vcl.forms,
    math, inifiles;

function _LastError: string;
var
    Buffer: array [0 .. 2047] of Char;
    strWinError: string;
begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, $400, @Buffer,
      SizeOf(Buffer), nil);
    exit(Trim(string(Buffer)));
end;

procedure _WriteFile(hPipe: THANDLE; var Buffer; numberOfbytesToWrite: DWORD);
var
    writen_count: DWORD;
begin
    if not(WriteFile(hPipe, Buffer, numberOfbytesToWrite, writen_count, nil))
    then
        raise Exception.Create('WriteFile: ' + _LastError);

    if writen_count <> numberOfbytesToWrite then
        raise Exception.Create(Format('WriteFile: writen_count=%d, must be %d',
          [writen_count, numberOfbytesToWrite]));

end;

procedure _ReadFile(hPipe: THANDLE; var Buffer; numberOfbytesToRead: DWORD);
var
    read_count: DWORD;
begin
    if not ReadFile(hPipe, Buffer, numberOfbytesToRead, read_count, nil) then
        raise Exception.Create('ReadFile: ' + _LastError);

    if read_count <> numberOfbytesToRead then

        raise Exception.Create(Format('ReadFile: writen_count=%d, must be %d',
          [read_count, numberOfbytesToRead]));

end;

function GetPipeResponse(pipeServer, request: string): string;
var
    hPipe: THANDLE;
    len: Integer;
    ptr_bytes: TBytes;
    read_count: DWORD;

    read_byte_Buffer, read_Buffer: TBytes;
begin
    hPipe := CreateFileW(PWideChar(pipeServer), GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if hPipe = INVALID_HANDLE_VALUE then
        raise Exception.Create('can not connect server pipe: ' + _LastError);
    ptr_bytes := WideBytesOf(request);
    len := Length(ptr_bytes);
    _WriteFile(hPipe, ptr_bytes[0], len);

    SetLength(read_byte_Buffer, 1);
    SetLength(read_Buffer, 0);

    while ReadFile(hPipe, read_byte_Buffer, 1, read_count, nil) do
    begin
        SetLength(read_Buffer, Length(read_Buffer) + 1);
        read_Buffer[Length(read_Buffer) - 1] := read_byte_Buffer[0];
    end;

    result := UTF8String(String(read_Buffer[0]));

end;

function GetPipeJsonRpcResponse(const pipeServer: string; const id: string;
  const method: string; params: ISuperObject): ISuperObject;
var
    requestObj: IJsonRpcMessage;
    strResponse: string;
begin
    requestObj := TJsonRpcMessage.request(id, method, params);
    strResponse := GetPipeResponse(pipeServer, requestObj.AsJSon(true, true));
    exit(SO(strResponse));
end;

end.
