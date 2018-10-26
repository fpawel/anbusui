unit Unit1;

interface

uses
  System.SysUtils, System.Classes, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, IdContext;

type
  TDataModule1 = class(TDataModule)
    IdHTTPServer1: TIdHTTPServer;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
    AResponseInfo.ContentText := '<html><head><title>My First Response</title></head>' +
  '<body>Command: ' + ARequestInfo.Command +
  '<br />Host: ' + ARequestInfo.Host +
  '<br />URI: ' + ARequestInfo.URI +
  '<br />UserAgent: ' + ARequestInfo.UserAgent +
  '</body></html>';
end;

end.
