unit UnitAnbusMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls,
    Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Grids, System.ImageList, Vcl.ImgList,
    ComponentBaloonHintU, server_data_types;

type

    EHostApplicationPanic = class(Exception);

    TAnbusMainForm = class(TForm)
        ImageList4: TImageList;
        PageControlMain: TPageControl;
        TabSheetVars: TTabSheet;
        TabSheetCharts: TTabSheet;
        Panel14: TPanel;
        Panel4: TPanel;
        PopupMenu1: TPopupMenu;
        N4: TMenuItem;
        N5: TMenuItem;
        N1: TMenuItem;
        N2: TMenuItem;
        N3: TMenuItem;
        N8: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        Panel2: TPanel;
        PanelInput: TPanel;
        PanelNetwork: TPanel;
        Splitter1: TSplitter;
        Panel5: TPanel;
        PanelStatus: TPanel;
        ToolBar4: TToolBar;
        ToolButtonConsoleHide: TToolButton;
        ImageList3: TImageList;
        Splitter2: TSplitter;
        Panel1: TPanel;
        ToolBar1: TToolBar;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        ToolButton5: TToolButton;
        ToolButton6: TToolButton;
        ImageList1: TImageList;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ToolButton7: TToolButton;
        Panel3: TPanel;
        ComboBoxComport: TComboBox;
    ToolButton8: TToolButton;
        procedure FormCreate(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: integer; const Rect: TRect; Active: boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ToolButtonConsoleHideClick(Sender: TObject);
        procedure ToolButton4MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: integer);
        procedure ToolButton6MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: integer);
        procedure ToolButton3Click(Sender: TObject);
        procedure ToolButton4Click(Sender: TObject);
        procedure ToolButton5Click(Sender: TObject);
        procedure ToolButton6Click(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
        procedure ToolButton7Click(Sender: TObject);
        procedure ComboBoxComportDropDown(Sender: TObject);
        procedure ComboBoxComportChange(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    private
        { Private declarations }
        FhWndTip: THandle;

        procedure AppException(Sender: TObject; E: Exception);

        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;

        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;

    public
        { Public declarations }
        procedure NewBallonTip(c: TWinControl; Icon: TIconKind;
          const Title, Text: string);
        procedure SetStatusText(ok: boolean; AText: string);

    end;

var
    AnbusMainForm: TAnbusMainForm;

implementation

{$R *.dfm}

uses vclutils,

    JclDebug, UnitFormReadVars, stringutils,
    richeditutils, Unit1, superobject, ujsonrpc,
    UnitFormChartSeries, System.StrUtils, System.Types, VclTee.Chart,
    UnitFormPopup, notify_services, UnitFormCharts, app, services,
    HttpRpcClient,
    HttpExceptions, comport;

procedure TAnbusMainForm.FormCreate(Sender: TObject);
begin
    HttpRpcClient.HttpHostAddr := app.GetAppHttpAddr;
    Application.OnException := AppException;
    ToolButtonConsoleHide.Click;

end;

procedure TAnbusMainForm.FormShow(Sender: TObject);
var
    FileName: String;
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
begin

    AppVars := TConfigSvc.vars;

    FileName := ChangeFileExt(paramstr(0), '.position');
    if FileExists(FileName) then
    begin
        fs := TFileStream.Create(FileName, fmOpenRead);
        fs.Read(wp, SizeOf(wp));
        fs.Free;
        SetWindowPlacement(Handle, wp);
    end;

    OnShow := nil;

    with FormReadVars do
    begin
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Parent := PanelNetwork;
        Show;
    end;

    with FormChartSeries do
    begin
        Parent := TabSheetVars;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
        Font.Assign(self.Font);
        Panel2.Parent := FormCharts;
        Panel2.Align := alRight;
        Panel2.Font.Assign(self.font);
        NewChart;
    end;

//    with FormCharts do
//    begin
//        Parent := PanelNetwork;
//        Align := alBottom;
//        Height := 200;
//        BorderStyle := bsNone;
//        Visible := true;
//        Font.Assign(self.Font);
//    end;
    FormReadVars.UpdateNetwork;

    SetOnWorkError(
        procedure(s: string)
        begin
            SetStatusText(false, s);
        end);

    SetOnReadVar(FormReadVars.HandleReadVar);

    SetOnNewSeries(FormCharts.FetchYearsMonths);

    EnumComports(ComboBoxComport.Items);
    ComboBoxComport.ItemIndex := ComboBoxComport.Items.IndexOf
      (TConfigSvc.ComportName);
    FormCharts.FetchYearsMonths;

    NotifyServices_SetEnabled(true);

end;

procedure TAnbusMainForm.ComboBoxComportChange(Sender: TObject);
begin
    TConfigSvc.SetComportName(ComboBoxComport.Text);
end;

procedure TAnbusMainForm.ComboBoxComportDropDown(Sender: TObject);
var
    s: string;
begin
    s := ComboBoxComport.Text;
    EnumComports(ComboBoxComport.Items);
    ComboBoxComport.ItemIndex := ComboBoxComport.Items.IndexOf(s);
end;

procedure TAnbusMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
begin
    NotifyServices_SetEnabled(false);
    HttpRpcClient.TIMEOUT_CONNECT := 10;
    // notify_services.CloseServerWindow;

    fs := TFileStream.Create(ChangeFileExt(paramstr(0), '.position'),
      fmOpenWrite or fmCreate);
    if not GetWindowPlacement(Handle, wp) then
        raise Exception.Create('GetWindowPlacement: false');
    fs.Write(wp, SizeOf(wp));
    fs.Free;

end;

procedure TAnbusMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
    FormChartSeries.ChangeAxisOrder(GetVCLControlAtPos(self, MousePos),
      WheelDelta);
end;

procedure TAnbusMainForm.NewBallonTip(c: TWinControl; Icon: TIconKind;
const Title, Text: string);
begin
    CloseWindow(FhWndTip);
    FhWndTip := c.ShowBalloonTip(Icon, Title, Text);

end;

procedure TAnbusMainForm.HandleCopydata(var Message: TMessage);
begin
    notify_services.HandleCopydata(Message);
end;

procedure TAnbusMainForm.PageControlMainChange(Sender: TObject);
begin
    (Sender as TPageControl).Repaint;
    if PageControlMain.ActivePage = TabSheetCharts then
        FormCharts.FetchYearsMonths;
end;

procedure TAnbusMainForm.PageControlMainDrawTab(Control: TCustomTabControl;
TabIndex: integer; const Rect: TRect; Active: boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;

procedure TAnbusMainForm.WMEnterSizeMove(var Msg: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TAnbusMainForm.WMWindowPosChanged(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TAnbusMainForm.WMActivateApp(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TAnbusMainForm.ToolButton3Click(Sender: TObject);
begin
    FormReadVars.SetNetwork(TConfigSvc.AddPlace);
end;

procedure TAnbusMainForm.ToolButton4Click(Sender: TObject);
begin
    FormReadVars.SetNetwork(TConfigSvc.DelPlace);
end;

procedure TAnbusMainForm.ToolButton4MouseMove(Sender: TObject;
Shift: TShiftState; X, Y: integer);
begin
    with FormReadVars.StringGrid1 do
    begin
        ToolButton4.Hint := 'Удалить адрес ' + Cells[Colcount - 1, 0];

    end;

end;

procedure TAnbusMainForm.ToolButton5Click(Sender: TObject);
begin
    FormReadVars.SetNetwork(TConfigSvc.AddVar);
end;

procedure TAnbusMainForm.ToolButton6Click(Sender: TObject);
begin
    FormReadVars.SetNetwork(TConfigSvc.DelVar);
end;

procedure TAnbusMainForm.ToolButton6MouseMove(Sender: TObject;
Shift: TShiftState; X, Y: integer);
begin
    with FormReadVars.StringGrid1 do
    begin
        ToolButton6.Hint := 'Удалить регистр ' + Cells[0, rowcount - 1];

    end;
end;

procedure TAnbusMainForm.ToolButton7Click(Sender: TObject);
var
    pt: TPoint;
begin
    GetCursorPos(pt);
    with FormPopup do
    begin
        RichEdit1.Font.Color := clNavy;
        RichEdit1.Text :=
          '- [B1] [код команды] [байты данных в HEX через пробел] : отправка запроса MODBUS c контрольной суммой'#13
          + #10'B1 - адрес MODBUS или ALL'#13 +
          '- [B1] W32 [код команды 32] [аргумент float] : запись в регистр 32';
        Left := pt.X + 5;
        Top := pt.Y + 5;
        Show;
    end;
end;

procedure TAnbusMainForm.ToolButton8Click(Sender: TObject);
begin
    with ToolButton8 do
        with ClientToScreen(Point(0, Height)) do
        begin
            with FormCharts do
            begin
                Left := x - 5 - Width;
                Top := Y + 5;
                Show;
            end;
        end;
end;

procedure TAnbusMainForm.ToolButtonConsoleHideClick(Sender: TObject);
begin
    if PanelInput.Height > 32 then
    begin
        PanelInput.Height := 32;
        ToolButtonConsoleHide.ImageIndex := 1;
        Splitter2.Visible := false;
    end
    else
    begin
        Splitter2.Visible := true;
        Splitter2.Top := 0;
        PanelInput.Height := 150;
        ToolButtonConsoleHide.ImageIndex := 0;
    end;

end;

procedure TAnbusMainForm.SetStatusText(ok: boolean; AText: string);
begin
    if ok then
        PanelStatus.Font.Color := clBlack
    else
        PanelStatus.Font.Color := clRed;
    PanelStatus.Caption := '    ' + AText;
end;

procedure TAnbusMainForm.AppException(Sender: TObject; E: Exception);
var
    stackList: TJclStackInfoList; // JclDebug.pas
    sl: TStringList;
    stacktrace: string;

    FErrorLog: TextFile;
    ErrorLogFileName: string;
begin

    stackList := JclCreateStackList(false, 0, Caller(0, false));
    sl := TStringList.Create;
    stackList.AddToStrings(sl, true, false, true, false);
    stacktrace := sl.Text;
    sl.Free;
    stackList.Free;
    OutputDebugStringW(PWideChar(E.Message + #10#13 + stacktrace));

    ErrorLogFileName := ChangeFileExt(paramstr(0), '.log');
    AssignFile(FErrorLog, ErrorLogFileName, CP_UTF8);
    if FileExists(ErrorLogFileName) then
        Append(FErrorLog)
    else
        Rewrite(FErrorLog);

    Writeln(FErrorLog, FormatDateTime('dd/MM/yy hh:nn:ss', now), ' ',
      'delphi_exception', ' ', E.ClassName, ' ', stringreplace(Trim(E.Message),
      #13, ' ', [rfReplaceAll, rfIgnoreCase]));

    Writeln(FErrorLog, StringOfChar('-', 120));

    Writeln(FErrorLog, stringreplace(Trim(stacktrace), #13, ' ',
      [rfReplaceAll, rfIgnoreCase]));

    Writeln(FErrorLog, StringOfChar('-', 120));

    CloseFile(FErrorLog);

    if E is EHostApplicationPanic then
    begin
        Application.ShowException(E);
        Application.Terminate;
        exit;
    end;

    if MessageDlg(E.Message, mtError, [mbAbort, mbIgnore], 0) = mrAbort then
    begin
        NotifyServices_SetEnabled(false);
        HttpRpcClient.TIMEOUT_CONNECT := 10;
        notify_services.CloseServerWindow;

        Application.OnException := nil;
        Application.Terminate;
        exit;
    end;
end;

end.
