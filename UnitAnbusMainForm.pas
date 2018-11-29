unit UnitAnbusMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls,
    Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Grids, System.ImageList, Vcl.ImgList,
    UnitServerApp, ComponentBaloonHintU;

type
    THostAppCommand = (cmdNotifyText, cmdReadVar);

    TPlace = class
        FAddr: integer;
        FUnchecked: boolean;
    end;

    TVar = class
        FVar: integer;
        FUnchecked: boolean;
    end;

    TNotifyText = class
        FText: string;
        FLevel: string;
        FKind: string;
    end;

    TAnbusMainForm = class(TForm)
        ImageList4: TImageList;
        PageControlMain: TPageControl;
        TabSheetVars: TTabSheet;
        TabSheetArchive: TTabSheet;
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
        RichEdit1: TRichEdit;
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
        ComboBox1: TComboBox;
        ToolButton1: TToolButton;
    ToolButton2: TToolButton;
        procedure FormCreate(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: integer; const Rect: TRect; Active: boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
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
        procedure RichEdit1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    private
        { Private declarations }
        FhWndTip: THandle;

        procedure AppException(Sender: TObject; E: Exception);

        procedure HandleNotifyText(t: TNotifyText);

        procedure SetStatusText(level: string; AText: string);

        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;

        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;

    public
        { Public declarations }
        procedure AddConsoleText(level: string; AText: string);

        procedure NewBallonTip(c: TWinControl; Icon: TIconKind;
          const Title, Text: string);

    end;

var
    AnbusMainForm: TAnbusMainForm;

const
    n_console = 'console';
    n_status = 'status';
    n_info = 'info';
    n_err = 'error';

implementation

{$R *.dfm}

uses serverapp_msg, rest.json, runhostapp, json, vclutils,
    model_config, PropertiesFormUnit,
    UnitFormReadVars, stringutils, model_network,
    richeditutils, UnitFormChartSeries, Unit1, superobject, ujsonrpc,
    UnitFormBuckets, System.StrUtils, System.Types, VclTee.Chart, pipe;

function CommandsFileName: string;
begin
    result := ExtractFilePath(ParamStr(0)) + '\commands.txt'
end;

procedure TAnbusMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if ParamStr(1) = '-must-close-server' then
        SendMessage(FindWindow('AnbusServerAppWindow', nil), WM_CLOSE, 0, 0);
end;

procedure TAnbusMainForm.FormCreate(Sender: TObject);
begin
    Application.OnException := AppException;
    ToolButtonConsoleHide.Click;
    if FileExists(CommandsFileName) then
        ComboBox1.Items.LoadFromFile(CommandsFileName);
end;

procedure TAnbusMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
    FormChartSeries.ChangeAxisOrder(GetVCLControlAtPos(self, MousePos),
      WheelDelta);
end;

procedure TAnbusMainForm.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
    r: IJsonRpcParsed;
    t: TJsonRpcObjectType;
    words: System.Types.TStringDynArray;
    I: integer;
    s: string;
begin
    with ComboBox1 do
        case Key of

            VK_DELETE:
                begin
                    if (ssCtrl in Shift) and (Items.IndexOf(ComboBox1.Text) > -1)
                    then
                    begin
                        Key := 0;
                        Items.delete(Items.IndexOf(ComboBox1.Text));
                        Items.SaveToFile(CommandsFileName);
                        ComboBox1.Text := '';
                    end;

                end;
            VK_RETURN:
                begin
                    Text := Trim(Text);
                    if Text <> '' then
                    begin
                        words := SplitString(Text, ' ');

                        if (Length(words) > 1) AND (words[0] = 'call') then
                        begin
                            if Length(words) = 2 then
                                r := ServerApp.GetResponse(words[1], SO('{}'))
                            else
                            begin
                                s := words[2];
                                for I := 3 to Length(words) - 1 do
                                    s := s + ' ' + words[I];
                                r := ServerApp.GetResponse(words[1], SO(s));
                            end;

                        end
                        else
                            r := ServerApp.GetResponse('MainSvc.PerformTextCommand',
                              SO(Format('["%s"]', [Text])));
                        t := r.GetMessageType;
                        if t = jotError then
                        begin
                            CloseWindow(FhWndTip);
                            FhWndTip := ComboBox1.ShowBalloonTip
                              (TIconKind.Error, 'Ошибка ввода',
                              r.GetMessagePayload.AsJsonObject['error']
                              ['message'].AsString);
                        end;

                        if words[0] = 'call' then
                            RichEdit1.Text := r.GetMessagePayload.AsJSon
                              (true, true);

                        if Items.IndexOf(Text) > -1 then
                            Items.Exchange(Items.IndexOf(Text), 0)
                        else
                            Items.insert(0, Text);
                        Items.SaveToFile(CommandsFileName);
                        Text := '';
                    end;
                    Key := 0;
                end;

        end;
end;

procedure TAnbusMainForm.NewBallonTip(c: TWinControl; Icon: TIconKind;
  const Title, Text: string);
begin
    CloseWindow(FhWndTip);
    FhWndTip := c.ShowBalloonTip(Icon, Title, Text);

end;

procedure TAnbusMainForm.HandleCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: THostAppCommand;
    response: TObject;
    read_var: TReadVar;
begin
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := THostAppCommand(Message.WParam);

    Message.result := 1;

    case cmd of
        cmdReadVar:
            begin
                read_var := TJson.JsonToObject<TReadVar>(StrFromCopydata(cd));
                FormReadVars.HandleReadVar(read_var);
                if read_var.FError = '' then
                begin
                    SetStatusText(n_info,
                      Format('%s: %g',
                      [FormReadVars.FormatAddrPlace(read_var.FPlace,
                      read_var.FVarIndex), read_var.FValue]));
                end
                else
                begin
                    SetStatusText(n_err,
                      Format('%s: %s',
                      [FormReadVars.FormatAddrPlace(read_var.FPlace,
                      read_var.FVarIndex), read_var.FError]));
                end;
                read_var.Free;
            end;

        cmdNotifyText:
            HandleNotifyText(TJson.JsonToObject<TNotifyText>
              (StrFromCopydata(cd)));

    end;
end;

procedure TAnbusMainForm.FormShow(Sender: TObject);
begin
    OnShow := nil;

    with PropertiesForm do
    begin
        Font.Assign(self.Font);
//        BorderStyle := bsNone;
//        Align := alClient;
//        Parent := PanelConfig;
//        Show;
    end;

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
        Panel2.Hide;
        NewChart;
    end;

    with FormBuckets do
    begin
        Parent := TabSheetArchive;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
        Font.Assign(self.Font);
    end;

    FormReadVars.UpdateNetwork('SetsSvc.Network');

    PropertiesForm.SetConfig(TJson.JsonToObject<TConfig>
      (ServerApp.MustGetResult('SetsSvc.UserConfig', SO('{}')).AsString));

end;

procedure TAnbusMainForm.PageControlMainChange(Sender: TObject);
begin
    (Sender as TPageControl).Repaint;
    if FormBuckets.TreeView1.IsEmpty then
        FormBuckets.CreateYearsNodes;
end;

procedure TAnbusMainForm.PageControlMainDrawTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: TRect; Active: boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;

procedure TAnbusMainForm.RichEdit1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if Button = TMouseButton.mbRight then
        RichEdit_PopupMenu(RichEdit1);
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

procedure TAnbusMainForm.ToolButton1Click(Sender: TObject);
begin
    RichEdit1.Text := '';
end;

procedure TAnbusMainForm.ToolButton2Click(Sender: TObject);
begin
    PropertiesForm.Position := TPosition.poScreenCenter;
    PropertiesForm.Show;

end;

procedure TAnbusMainForm.ToolButton3Click(Sender: TObject);
begin
    FormReadVars.UpdateNetwork('SetsSvc.AddPlace');
end;

procedure TAnbusMainForm.ToolButton4Click(Sender: TObject);
begin
    FormReadVars.UpdateNetwork('SetsSvc.DelPlace');
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
    FormReadVars.UpdateNetwork('SetsSvc.AddVar');
end;

procedure TAnbusMainForm.ToolButton6Click(Sender: TObject);
begin
    FormReadVars.UpdateNetwork('SetsSvc.DelVar');
end;

procedure TAnbusMainForm.ToolButton6MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
    with FormReadVars.StringGrid1 do
    begin
        ToolButton6.Hint := 'Удалить регистр ' + Cells[0, rowcount - 1];

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

procedure TAnbusMainForm.SetStatusText(level: string; AText: string);
begin
    if level = n_info then
        PanelStatus.Font.Color := clBlack
    else if level = n_err then
        PanelStatus.Font.Color := clRed
    else
        PanelStatus.Font.Color := clBlack;
    PanelStatus.Caption := '    ' + AText;
end;

procedure TAnbusMainForm.AddConsoleText(level: string; AText: string);
begin
    with RichEdit1 do
    begin
        SendMessage(Handle, EM_SCROLL, SB_LINEDOWN, 0);
        SelStart := Length(RichEdit1.Text);
        if level = n_info then
            RichEdit_AddText(RichEdit1, clBlack, AText)
        else
            RichEdit_AddText2(RichEdit1, clRed, cl3dLight, AText);
        SendMessage(Handle, EM_SCROLL, SB_LINEDOWN, 0);
    end;
end;

procedure TAnbusMainForm.HandleNotifyText(t: TNotifyText);
begin
    SetStatusText(t.FLevel, t.FText);
    if t.FKind = n_console then
        AddConsoleText(t.FLevel, t.FText);

end;

procedure TAnbusMainForm.AppException(Sender: TObject; E: Exception);
begin
    OutputDebugStringW(PWideChar(e.Message));
    if e is EPipeBusy then
    begin
        exit;
    end;
    Application.ShowException(E);
end;

end.
