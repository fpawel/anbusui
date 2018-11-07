unit vclutils;

interface

uses vcl.stdctrls, vcl.controls, vcl.ComCtrls, vcl.graphics, system.Types;

type
    TControlProc = reference to procedure(const AControl: TControl);

procedure SetButtonMultiline(b: TButton);

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);

procedure ConvertImagesToHighColor(ImageList: TImageList);

// hWnd - control window handle to attach the baloon to.
// Icon - icon index; 0 = none, 1 = info, 2 = warning, 3 = error.
// BackCL - background color or clDefault to use system setting.
// TextCL - text and border colors or clDefault to use system setting.
// Title - tooltip title (bold first line).
// Text - tooltip text.

function ShowBalloonTip(hWnd: THandle; Icon: integer; BackCL, TextCL: TColor;
  Title: string; Text: string): THandle;

function GetVCLControlAtPos(c: TWinControl; mousePos: TPoint): TWinControl;

implementation

uses Winapi.Windows, Winapi.commctrl, Winapi.messages;

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);
var
    i: integer;
begin
    if AControl = nil then
        Exit;
    if AControl is TWinControl then
    begin
        for i := 0 to TWinControl(AControl).ControlCount - 1 do
            ModifyControl(TWinControl(AControl).controls[i], ARef);
    end;
    ARef(AControl);
end;

procedure SetButtonMultiline(b: TButton);
begin
    SetWindowLong(b.Handle, GWL_STYLE, GetWindowLong(b.Handle, GWL_STYLE) or
      BS_MULTILINE);
end;

function PageControl_visible_pages(Control: TCustomTabControl)
  : TArray<TTabSheet>;
var
    i: integer;
begin
    SetLength(Result, 0);
    with Control as TPageControl do
        for i := 0 to PageCount - 1 do
            if Pages[i].TabVisible then
            begin
                SetLength(Result, Length(Result) + 1);
                Result[Length(Result) - 1] := Pages[i];
            end;
end;

function page_tab_index(Pages: TArray<TTabSheet>; page: TTabSheet): integer;
var
    pg: TTabSheet;
begin
    Result := 0;
    for pg in Pages do
    begin
        if pg = page then
            Exit;
        Result := Result + 1;
    end;
    Result := -1;
end;

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);
var
    i: integer;
    PageControl: TPageControl;
    Text: string;
    page_index, x, y: integer;
    txt_width, txt_height: double;
    page: TTabSheet;

begin
    PageControl := Control as TPageControl;
    page := PageControl_visible_pages(PageControl)[TabIndex];
    Text := page.Caption;
    Active := PageControl.ActivePage = page;

    txt_width := PageControl.Canvas.TextWidth(Text);
    txt_height := PageControl.Canvas.TextHeight(Text);

    x := Rect.Left + round((Rect.Width - txt_width) / 2.0);
    y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    if Active then
    begin
        PageControl.Canvas.Brush.Color := clGradientInactiveCaption;
        PageControl.Canvas.Font.Color := clNavy;
    end
    else
    begin
        PageControl.Canvas.Brush.Color := clWindow;
        PageControl.Canvas.Font.Color := clBlack;
    end;

    PageControl.Canvas.TextRect(Rect, x, y, Text);
end;

procedure ConvertImagesToHighColor(ImageList: TImageList);

// To show smooth images we have to convert the image list from 16 colors to high color.

var
    IL: TImageList;

begin
    // Have to create a temporary copy of the given list, because the list is cleared on handle creation.
    IL := TImageList.Create(nil);
    IL.Assign(ImageList);

    with ImageList do
        Handle := ImageList_Create(Width, Height, ILC_COLOR16 or ILC_MASK,
          Count, AllocBy);
    ImageList.Assign(IL);
    IL.Free;
end;


// hWnd - control window handle to attach the baloon to.
// Icon - icon index; 0 = none, 1 = info, 2 = warning, 3 = error.
// BackCL - background color or clDefault to use system setting.
// TextCL - text and border colors or clDefault to use system setting.
// Title - tooltip title (bold first line).
// Text - tooltip text.

function ShowBalloonTip(hWnd: THandle; Icon: integer; BackCL, TextCL: TColor;
  Title: string; Text: string): THandle;
const
    TOOLTIPS_CLASS = 'tooltips_class32';
    TTS_ALWAYSTIP = $01;
    TTS_NOPREFIX = $02;
    TTS_BALLOON = $40;
    TTF_SUBCLASS = $0010;
    TTF_TRANSPARENT = $0100;
    TTF_CENTERTIP = $0002;
    TTM_ADDTOOL = $0400 + 50;
    TTM_SETTITLE = (WM_USER + 32);
    ICC_WIN95_CLASSES = $000000FF;
type
    TOOLINFO = packed record
        cbSize: integer;
        uFlags: integer;
        hWnd: THandle;
        uId: integer;
        Rect: TRect;
        hinst: THandle;
        lpszText: PWideChar;
        lParam: integer;
    end;

var
    ti: TOOLINFO;
begin
    Result := CreateWindow(TOOLTIPS_CLASS, nil, WS_POPUP or TTS_CLOSE or
      TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP, 0, 0, 0, 0, hWnd, 0,
      HInstance, nil);

    if Result = 0 then
        Exit;
    SetWindowPos(Result, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE);

    ti.cbSize := SizeOf(ti);
    ti.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
    ti.hWnd := hWnd;
    ti.lpszText := PChar(Text);

    GetClientRect(hWnd, ti.Rect);
    if BackCL <> clDefault then
        SendMessage(Result, TTM_SETTIPBKCOLOR, BackCL, 0);
    if TextCL <> clDefault then
        SendMessage(Result, TTM_SETTIPTEXTCOLOR, TextCL, 0);
    SendMessage(Result, TTM_ADDTOOL, 1, integer(@ti));
    SendMessage(Result, TTM_SETTITLE, Icon,
      integer(PAnsiChar(AnsiString(Title))));

    // TTM_TRACKACTIVATE => Makes sure you have to close the hint you self
    SendMessage(Result, TTM_TRACKACTIVATE, integer(true), integer(@ti));
end;

function GetVCLControlAtPos(c: TWinControl; mousePos: TPoint): TWinControl;
var
    p: TPoint;
begin

    p := c.ScreenToClient(mousePos);
    c := TWinControl(c.ControlAtPos(p, false, true));
    while Assigned(c) do
    begin
        Result := c;
        p := c.ScreenToClient(mousePos);
        c := TWinControl(c.ControlAtPos(p, false, true));
    end;
end;

end.
