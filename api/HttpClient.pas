unit HttpClient;

interface

uses System.SysUtils, System.classes;

const
    { Timeout for operations }
    TIMEOUT_CONNECT: integer = 500;
    TIMEOUT_RECV: integer = 10000;

type
    TUpdateGUIProcedure = reference to procedure(bytes: TBytes);

procedure AnbusHttpGetResponseAsync(AUrl: string;
  UpdateGUIProcedure: TUpdateGUIProcedure);

implementation

uses Grijjy.Http, HttpExceptions, app;

type
    TReqThread = class(TThread)

    public

        FResponse: TBytes;
        FUrl: string;
        FUpdateGUIProcedure: TUpdateGUIProcedure;

        procedure Execute; override;
        procedure UpdateGUI;
        destructor Destroy; override;
    end;

procedure AnbusHttpGetResponseAsync(AUrl: string;
  UpdateGUIProcedure: TUpdateGUIProcedure);
begin
    with TReqThread.Create(true) do
    begin
        FreeOnTerminate := true;
        FUrl := AUrl;
        FUpdateGUIProcedure := UpdateGUIProcedure;
        Start;
    end;
end;

destructor TReqThread.Destroy;
begin
    FUrl := '';
end;

procedure TReqThread.UpdateGUI;
begin
    FUpdateGUIProcedure(FResponse);
end;

procedure TReqThread.Execute;
var
    url: string;
    Http: TgoHttpClient;
begin

    Http := TgoHttpClient.Create(false, True);

    try
        if not Http.Get(FUrl, FResponse, TIMEOUT_CONNECT, TIMEOUT_RECV) then
            raise ERpcNoResponseException.Create('нет связи с хост процессом');
        Synchronize(UpdateGUI);
    finally
        HttpClientManager.Release(Http);
    end;
end;

end.
