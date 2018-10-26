unit UnitServerApp;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
    serverapp_msg;

type
    TServerApp = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
        hWndServer: HWND;
        procedure init_hWndServer;
        procedure MustSendMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
    public
        { Public declarations }
        function SendMsg(Msg: TServerAppUserMsg; wParam: wParam; lParam: lParam): LRESULT;
        procedure MustSendUserMsg(Msg: TServerAppUserMsg; wParam: wParam; lParam: lParam);

        procedure MustSendStr(sourceHWND:HWND; msg :TServerAppDataMsg; data:string);
        procedure MustSendJSON(sourceHWND:HWND; msg:TServerAppDataMsg; data:TObject);

        procedure CloseServer;
    end;

var
    ServerApp: TServerApp;

implementation

uses rest.json;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure TServerApp.DataModuleCreate(Sender: TObject);
begin
    init_hWndServer;

end;

procedure TServerApp.CloseServer;
begin
    SendMessage(hWndServer, WM_CLOSE, 0, 0);
end;

procedure TServerApp.MustSendMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
begin
    if SendMessage(hWndServer, Msg, wParam, lParam) = 0 then
    begin
        init_hWndServer;
        if SendMessage(hWndServer, Msg, wParam, lParam) = 0 then
            raise Exception.Create('server is not responding');
    end;
end;

procedure TServerApp.MustSendUserMsg(Msg: TServerAppUserMsg; wParam: wParam; lParam: lParam);
begin
    MustSendMessage(WM_USER + integer(Msg), wParam, lParam);
end;

function TServerApp.SendMsg(Msg: TServerAppUserMsg; wParam: wParam; lParam: lParam): LRESULT;
begin
    result := SendMessage(hWndServer, WM_USER + integer(Msg), wParam, lParam);
end;

procedure TServerApp.init_hWndServer;
begin
    hWndServer := FindWindow('AnbusServerAppWindow', nil);
    if hWndServer = 0 then
        raise Exception.Create('server not found');

end;

procedure TServerApp.MustSendStr(sourceHWND:HWND; msg:TServerAppDataMsg; data:string);
var
    cd: COPYDATASTRUCT;
    ptr_bytes: TBytes;
begin
    ptr_bytes := WideBytesOf(data);
    cd.cbData := Length(ptr_bytes);
    cd.lpData := ptr_bytes;
    cd.dwData := integer(msg);
    MustSendMessage(WM_COPYDATA, sourceHWND, integer(@cd));
end;

procedure TServerApp.MustSendJSON(sourceHWND:HWND; msg:TServerAppDataMsg; data:TObject);
begin
    MustSendStr(sourceHWND, msg, TJson.ObjectToJsonString(data));
end;

end.
