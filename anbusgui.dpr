program anbusgui;

uses
  Vcl.Forms,
  UnitAnbusMainForm in 'UnitAnbusMainForm.pas' {AnbusMainForm},
  findproc in 'utils\findproc.pas',
  vclutils in 'utils\vclutils.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  stringutils in 'utils\stringutils.pas',
  listports in 'utils\listports.pas',
  UnitFormReadVars in 'UnitFormReadVars.pas' {FormReadVars},
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  ComponentBaloonHintU in 'utils\ComponentBaloonHintU.pas',
  richeditutils in 'utils\richeditutils.pas',
  ujsonrpc in 'utils\jsonrpc\ujsonrpc.pas',
  superdate in 'utils\superobject\superdate.pas',
  superobject in 'utils\superobject\superobject.pas',
  supertimezone in 'utils\superobject\supertimezone.pas',
  supertypes in 'utils\superobject\supertypes.pas',
  superxmlparser in 'utils\superobject\superxmlparser.pas',
  notify_services in 'api\notify_services.pas',
  server_data_types in 'api\server_data_types.pas',
  services in 'api\services.pas',
  Grijjy.BinaryCoding in 'grijjy\Grijjy.BinaryCoding.pas',
  Grijjy.Bson.IO in 'grijjy\Grijjy.Bson.IO.pas',
  Grijjy.Bson in 'grijjy\Grijjy.Bson.pas',
  Grijjy.Bson.Serialization in 'grijjy\Grijjy.Bson.Serialization.pas',
  Grijjy.Collections in 'grijjy\Grijjy.Collections.pas',
  Grijjy.Console in 'grijjy\Grijjy.Console.pas',
  Grijjy.DateUtils in 'grijjy\Grijjy.DateUtils.pas',
  Grijjy.Hash in 'grijjy\Grijjy.Hash.pas',
  Grijjy.Hooking in 'grijjy\Grijjy.Hooking.pas',
  Grijjy.Http in 'grijjy\Grijjy.Http.pas',
  Grijjy.JWT in 'grijjy\Grijjy.JWT.pas',
  Grijjy.MemoryPool in 'grijjy\Grijjy.MemoryPool.pas',
  Grijjy.OpenSSL.API in 'grijjy\Grijjy.OpenSSL.API.pas',
  Grijjy.OpenSSL in 'grijjy\Grijjy.OpenSSL.pas',
  Grijjy.PropertyBag in 'grijjy\Grijjy.PropertyBag.pas',
  Grijjy.SocketPool.Win in 'grijjy\Grijjy.SocketPool.Win.pas',
  Grijjy.System in 'grijjy\Grijjy.System.pas',
  Grijjy.SysUtils in 'grijjy\Grijjy.SysUtils.pas',
  Grijjy.Uri in 'grijjy\Grijjy.Uri.pas',
  Grijjy.Winsock2 in 'grijjy\Grijjy.Winsock2.pas',
  HttpClient in 'api\HttpClient.pas',
  HttpExceptions in 'api\HttpExceptions.pas',
  HttpRpcClient in 'api\HttpRpcClient.pas',
  SuperObjectHelp in 'utils\SuperObjectHelp.pas',
  app in 'app.pas',
  UnitFormChartSeries in 'UnitFormChartSeries.pas' {FormChartSeries},
  UnitFormCharts in 'UnitFormCharts.pas' {FormCharts};

{$R *.res}

begin

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TAnbusMainForm, AnbusMainForm);
  Application.CreateForm(TFormReadVars, FormReadVars);
  Application.CreateForm(TFormPopup, FormPopup);
  Application.CreateForm(TFormChartSeries, FormChartSeries);
  Application.CreateForm(TFormChartSeries, FormChartSeries);
  Application.CreateForm(TFormCharts, FormCharts);
  Application.Run;

end.
