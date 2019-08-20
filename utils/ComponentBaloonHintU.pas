unit ComponentBaloonHintU;

interface

uses
    Controls, CommCtrl, Graphics;

{$SCOPEDENUMS ON}

type
    TIconKind = (None = TTI_NONE, Info = TTI_INFO, Warning = TTI_WARNING,
      Error = TTI_ERROR, Info_Large = TTI_INFO_LARGE,
      Warning_Large = TTI_WARNING_LARGE, Eror_Large = TTI_ERROR_LARGE);

    TComponentBaloonhint = class helper for TWinControl
    public
        function ShowBalloonTip(Icon: TIconKind;
          const Title, Text: string): THandle;
    end;

function ShowBalloonTip(control: TWinControl; Icon: TIconKind;
  const Title, Text: string): THandle;

implementation

uses
    Windows;

function ShowBalloonTip(control: TWinControl; Icon: TIconKind;
  const Title, Text: string): THandle;
begin
    result := control.ShowBalloonTip(Icon, Title, Text);
end;

{ TComponentBaloonhint }

function TComponentBaloonhint.ShowBalloonTip(Icon: TIconKind;
  const Title, Text: string): THandle;
var
    hWndTip: THandle;
    ToolInfo: TToolInfo;
    BodyText: pWideChar;
begin
    hWndTip := CreateWindow(TOOLTIPS_CLASS, nil, WS_POPUP or TTS_CLOSE or
      TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP, 0, 0, 0, 0, Handle, 0,
      HInstance, nil);

    if hWndTip = 0 then
        exit;

    GetMem(BodyText, 2 * 256);

    try
        ToolInfo.cbSize := SizeOf(TToolInfo);
        ToolInfo.uFlags := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
        ToolInfo.hWnd := Handle;
        ToolInfo.lpszText := StringToWideChar(Text, BodyText, 2 * 256);
        SetWindowPos(hWndTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or
          SWP_NOMOVE or SWP_NOSIZE);
        ToolInfo.Rect := GetClientRect;

        SendMessage(hWndTip, TTM_ADDTOOL, 1, integer(@ToolInfo));
        SendMessage(hWndTip, TTM_SETTITLE, integer(Icon),
          integer(PChar(Title)));
        SendMessage(hWndTip, TTM_TRACKACTIVATE, integer(true),
          integer(@ToolInfo));
    finally
        FreeMem(BodyText);
    end;
    exit(hWndTip);
end;

end.
