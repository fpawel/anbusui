program anbusui;

uses
    Vcl.Forms,
    UnitAnbusMainForm in 'UnitAnbusMainForm.pas' {AnbusMainForm} ,
    findproc in 'utils\findproc.pas',
    runhostapp in 'runhostapp.pas',
    vclutils in 'utils\vclutils.pas',
    model_config in 'models\model_config.pas',
    stringgridutils in 'utils\stringgridutils.pas',
    stringutils in 'utils\stringutils.pas',
    listports in 'utils\listports.pas',
    model_network in 'models\model_network.pas',
    UnitFormReadVars in 'UnitFormReadVars.pas' {FormReadVars} ,
    UnitFormPopup in 'UnitFormPopup.pas' {FormPopup} ,
    UnitServerApp in 'UnitServerApp.pas' {ServerApp: TDataModule} ,
    serverapp_msg in 'messages\serverapp_msg.pas',
    ComponentBaloonHintU in 'utils\ComponentBaloonHintU.pas',
    richeditutils in 'utils\richeditutils.pas',
    UnitFormChartSeries in 'UnitFormChartSeries.pas' {FormChartSeries} ,
    ujsonrpc in 'utils\jsonrpc\ujsonrpc.pas',
    superdate in 'utils\superobject\superdate.pas',
    superobject in 'utils\superobject\superobject.pas',
    supertimezone in 'utils\superobject\supertimezone.pas',
    supertypes in 'utils\superobject\supertypes.pas',
    superxmlparser in 'utils\superobject\superxmlparser.pas',
    pipe in 'utils\pipe.pas',
    PropertiesFormUnit in 'PropertiesFormUnit.pas' {PropertiesForm} ,
    PropertyValueEditors in 'PropertyValueEditors.pas',
    UnitFormBuckets in 'UnitFormBuckets.pas' {FormBuckets};

{$R *.res}

begin

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TServerApp, ServerApp);
    Application.CreateForm(TAnbusMainForm, AnbusMainForm);
    Application.CreateForm(TFormReadVars, FormReadVars);
    Application.CreateForm(TFormPopup, FormPopup);
    Application.CreateForm(TFormChartSeries, FormChartSeries);
    Application.CreateForm(TPropertiesForm, PropertiesForm);
    Application.CreateForm(TFormBuckets, FormBuckets);
    Application.Run;

end.
