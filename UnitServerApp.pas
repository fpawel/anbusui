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

    public
        { Public declarations }

        function MustGetResult(const method: string; params: ISuperObject)
          : ISuperObject;

        function GetResponse(const method: string; params: ISuperObject)
          : IJsonRpcParsed;

        function MustGetResponseNetwork(const method: string;
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
    pipe_conn := TPipe.Create('\\.\pipe\anbus');

end;

function TServerApp.SetConfigProperyValue(p: TChangedPropertyValue): String;
var
    i: IJsonRpcParsed;
begin
    i := pipe_conn.GetResponse('SetsSvc.SetValue',
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

function TServerApp.MustGetResponseNetwork(const method: string;
  params: ISuperObject): TNetwork;
var
    p: ISuperObject;
    network: TNetwork;
    str_response: string;
begin
    p := ServerApp.MustGetResult(method, params);
    str_response := p.AsJSon;
    result := TJson.JsonToObject<TNetwork>(str_response);
end;

function TServerApp.GetResponse(const method: string; params: ISuperObject)
  : IJsonRpcParsed;
begin
    exit(pipe_conn.GetResponse(method, params));
end;

function TServerApp.MustGetResult(const method: string; params: ISuperObject)
  : ISuperObject;
begin
    with pipe_conn.GetResponse(method, params) do
    begin
        if GetMessageType <> jotSuccess then
            raise Exception.Create(Format('%s%s'#13'%s',
              [method, params.AsString, GetMessagePayload.AsJSon(true, true)]));
        result := GetMessagePayload.AsJsonObject['result'];

    end;

end;

end.
