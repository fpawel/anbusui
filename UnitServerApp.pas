unit UnitServerApp;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
    serverapp_msg, superobject, ujsonrpc, model_network, model_config;

type
    TServerApp = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
        hWndServer: HWND;
        procedure init_hWndServer;
        procedure MustSendMessage(Msg: UINT; wParam: wParam; lParam: lParam);
    public
        { Public declarations }
        
        procedure MustSendStr(sourceHWND: HWND; Msg: TServerAppDataMsg;
          data: string);
        procedure MustSendJSON(sourceHWND: HWND; Msg: TServerAppDataMsg;
          data: TObject);

        procedure CloseServer;

        function GetResponse(const id: string; const method: string;
          params: ISuperObject): IJsonRpcParsed;

        function GetResponseNetwork(const id: string; const method: string;
          params: ISuperObject): TNetwork;

        function SetConfigProperyValue(p: TChangedPropertyValue): string;

    end;

var
    ServerApp: TServerApp;

implementation

uses rest.json, pipe;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

var
    pipe_conn: TPipe;

procedure TServerApp.DataModuleCreate(Sender: TObject);
begin
    init_hWndServer;
    pipe_conn := TPipe.Create('\\.\pipe\anbus');

end;

procedure TServerApp.CloseServer;
begin
    SendMessage(hWndServer, WM_CLOSE, 0, 0);
end;

function TServerApp.SetConfigProperyValue(p: TChangedPropertyValue): String;
var
    i: IJsonRpcParsed;
begin
    i := pipe_conn.GetResponse('0', 'SetsSvc.SetValue',
      SO(TJson.ObjectToJsonString(p)));

    case i.GetMessageType of
        jotSuccess:
            exit('');
        jotError:
            exit(i.GetMessagePayload.AsJsonObject['error'].AsString);
    else
        raise Exception.Create(Format('SetsSvc.SetValue'#13'%s',
          [i.GetMessagePayload.AsJSon(true, true)]));
    end;

    if i.GetMessageType <> jotSuccess then

end;

function TServerApp.GetResponseNetwork(const id: string; const method: string;
  params: ISuperObject): TNetwork;
var
    p: IJsonRpcParsed;
    network: TNetwork;
    str_response: string;
begin

    p := ServerApp.GetResponse(id, method, params);
    str_response := p.GetMessagePayload.AsJsonObject['result'].AsString;
    result := TJson.JsonToObject<TNetwork>(str_response);
end;

function TServerApp.GetResponse(const id: string; const method: string;
  params: ISuperObject): IJsonRpcParsed;
begin
    result := pipe_conn.GetResponse(id, method, params);

    if result.GetMessageType <> jotSuccess then
        raise Exception.Create(Format('%s%s'#13'%s', [method, params.AsString,
          result.GetMessagePayload.AsJSon(true, true)]));
end;

procedure TServerApp.MustSendMessage(Msg: UINT; wParam: wParam; lParam: lParam);
begin
    if SendMessage(hWndServer, Msg, wParam, lParam) = 0 then
    begin
        init_hWndServer;
        if SendMessage(hWndServer, Msg, wParam, lParam) = 0 then
            raise Exception.Create('server is not responding');
    end;
end;

procedure TServerApp.init_hWndServer;
begin
    hWndServer := FindWindow('AnbusServerAppWindow', nil);
    if hWndServer = 0 then
        raise Exception.Create('server not found');

end;

procedure TServerApp.MustSendStr(sourceHWND: HWND; Msg: TServerAppDataMsg;
  data: string);
var
    cd: COPYDATASTRUCT;
    ptr_bytes: TBytes;
begin
    ptr_bytes := WideBytesOf(data);
    cd.cbData := Length(ptr_bytes);
    cd.lpData := ptr_bytes;
    cd.dwData := integer(Msg);
    MustSendMessage(WM_COPYDATA, sourceHWND, integer(@cd));
end;

procedure TServerApp.MustSendJSON(sourceHWND: HWND; Msg: TServerAppDataMsg;
  data: TObject);
begin
    MustSendStr(sourceHWND, Msg, TJson.ObjectToJsonString(data));
end;

end.
