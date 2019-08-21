
unit notify_services;

interface

uses superobject, Winapi.Windows, Winapi.Messages, server_data_types;

type
    TStringHandler = reference to procedure (x:string);
    TReadVarHandler = reference to procedure (x:TReadVar);
    TProcedure = reference to procedure;
    

procedure HandleCopydata(var Message: TMessage);
procedure CloseServerWindow;

procedure SetOnWriteConsoleInfo( AHandler : TStringHandler);
procedure SetOnWriteConsoleError( AHandler : TStringHandler);
procedure SetOnStatus( AHandler : TStringHandler);
procedure SetOnWorkError( AHandler : TStringHandler);
procedure SetOnReadVar( AHandler : TReadVarHandler);
procedure SetOnNewSeries( AHandler : TProcedure);

procedure NotifyServices_SetEnabled(enabled:boolean);

implementation 

uses Grijjy.Bson.Serialization, stringutils, sysutils;

type
    TServerAppCmd = (CmdWriteConsoleInfo, CmdWriteConsoleError, CmdStatus, CmdWorkError, CmdReadVar, 
    CmdNewSeries);

    type _deserializer = record
        class function deserialize<T>(str:string):T;static;
    end;

var
    _OnWriteConsoleInfo : TStringHandler;
    _OnWriteConsoleError : TStringHandler;
    _OnStatus : TStringHandler;
    _OnWorkError : TStringHandler;
    _OnReadVar : TReadVarHandler;
    _OnNewSeries : TProcedure;
    _enabled:boolean;

procedure CloseServerWindow;
begin
    SendMessage(FindWindow('AnbusServerWindow', nil), WM_CLOSE, 0, 0)
end;

class function _deserializer.deserialize<T>(str:string):T;
begin
    TgoBsonSerializer.Deserialize(str, Result);
end;

procedure NotifyServices_SetEnabled(enabled:boolean);
begin
   _enabled := enabled;
end;

procedure HandleCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: TServerAppCmd;
    str:string;
begin
    if not _enabled then
        exit;
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := TServerAppCmd(Message.WParam);
    Message.result := 1;
    SetString(str, PWideChar(cd.lpData), cd.cbData div 2);
    case cmd of
        CmdWriteConsoleInfo:
        begin
            if not Assigned(_OnWriteConsoleInfo) then
                raise Exception.Create('_OnWriteConsoleInfo must be set');
            _OnWriteConsoleInfo(str);
        end;
        CmdWriteConsoleError:
        begin
            if not Assigned(_OnWriteConsoleError) then
                raise Exception.Create('_OnWriteConsoleError must be set');
            _OnWriteConsoleError(str);
        end;
        CmdStatus:
        begin
            if not Assigned(_OnStatus) then
                raise Exception.Create('_OnStatus must be set');
            _OnStatus(str);
        end;
        CmdWorkError:
        begin
            if not Assigned(_OnWorkError) then
                raise Exception.Create('_OnWorkError must be set');
            _OnWorkError(str);
        end;
        CmdReadVar:
        begin
            if not Assigned(_OnReadVar) then
                raise Exception.Create('_OnReadVar must be set');
            _OnReadVar(_deserializer.deserialize<TReadVar>(str));
        end;
        CmdNewSeries:
        begin
            if not Assigned(_OnNewSeries) then
                raise Exception.Create('_OnNewSeries must be set');
            _OnNewSeries();
        end;
        
    else
        raise Exception.Create('wrong message: ' + IntToStr(Message.WParam));
    end;
end;

procedure SetOnWriteConsoleInfo( AHandler : TStringHandler);
begin
    if Assigned(_OnWriteConsoleInfo) then
        raise Exception.Create('_OnWriteConsoleInfo already set');
    _OnWriteConsoleInfo := AHandler;
end;
procedure SetOnWriteConsoleError( AHandler : TStringHandler);
begin
    if Assigned(_OnWriteConsoleError) then
        raise Exception.Create('_OnWriteConsoleError already set');
    _OnWriteConsoleError := AHandler;
end;
procedure SetOnStatus( AHandler : TStringHandler);
begin
    if Assigned(_OnStatus) then
        raise Exception.Create('_OnStatus already set');
    _OnStatus := AHandler;
end;
procedure SetOnWorkError( AHandler : TStringHandler);
begin
    if Assigned(_OnWorkError) then
        raise Exception.Create('_OnWorkError already set');
    _OnWorkError := AHandler;
end;
procedure SetOnReadVar( AHandler : TReadVarHandler);
begin
    if Assigned(_OnReadVar) then
        raise Exception.Create('_OnReadVar already set');
    _OnReadVar := AHandler;
end;
procedure SetOnNewSeries( AHandler : TProcedure);
begin
    if Assigned(_OnNewSeries) then
        raise Exception.Create('_OnNewSeries already set');
    _OnNewSeries := AHandler;
end;


initialization
    _enabled := false;

end.