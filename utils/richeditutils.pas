unit richeditutils;

interface

uses vcl.comctrls, Graphics, RichEdit;

type
    TLogLevel = (llTrace, llDebug, llInfo, llWarning, llError);

procedure RichEdit_AddText(RichEdit1: TRichEdit;
  font_color: TColor; text: string);

procedure RichEdit_AddText2(RichEdit1: TRichEdit;
  font_color, back_color: TColor; text: string);

function RichEdit_CurrentlineNumber(re: TRichEdit): Integer;

procedure RichEdit_PopupMenu(re: TRichEdit);

procedure RichEdit_DeleteEmptiLines(re: TRichEdit);

implementation

uses System.SysUtils, Winapi.Windows, Winapi.Messages, Clipbrd,
    vcl.forms, vcl.controls;

function RichEdit_CurrentlineNumber(re: TRichEdit): Integer;
var
    Pos: TPoint;
begin
    Pos.Y := re.Perform(EM_LINEFROMCHAR, re.SelStart, 0);
    Result := Pos.Y + 1;
end;

procedure setBackcolor(r: TRichEdit; c: TColor);
var
    cf: TCharFormat2;
begin
    FillChar(cf, sizeof(cf), 0);
    cf.cbSize := sizeof(cf);
    cf.dwMask := CFM_BACKCOLOR;
    cf.crBackColor := ColorToRGB(c);
    r.Perform(EM_SETCHARFORMAT, SCF_SELECTION, lparam(@cf));
end;

procedure RichEdit_AddText(RichEdit1: TRichEdit;
  font_color: TColor; text: string);
begin
    RichEdit1.SelAttributes.Color := font_color;
    RichEdit1.SelText := text + #13;
end;

procedure RichEdit_AddText2(RichEdit1: TRichEdit;
  font_color, back_color: TColor; text: string);
begin
    RichEdit1.SelAttributes.Color := font_color;
    setBackcolor(RichEdit1, back_color);
    RichEdit1.SelText := text + #13;
end;

procedure RichEdit_DeleteEmptiLines(re: TRichEdit);
var i:integer;
begin
    i := 0;
    with re do
    begin
        while I < lines.Count do
            if trim(Lines[i]) = '' then
                Lines.Delete(Lines.Count-1)
            else
                i := i + 1;
        Text := trim(text);
    end;

end;

procedure RichEdit_PopupMenu(re: TRichEdit);
const
    IDM_UNDO = WM_UNDO;
    IDM_CUT = WM_CUT;
    IDM_COPY = WM_COPY;
    IDM_PASTE = WM_PASTE;
    IDM_DELETE = WM_CLEAR;
    IDM_SELALL = EM_SETSEL;
    IDM_RTL = $8000; // WM_APP ?

    Enables: array [boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
    Checks: array [boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
var
    hUser32: HMODULE;
    hmnu, hmenuTrackPopup: HMENU;
    Cmd: DWORD;
    Flags: Cardinal;
    HasSelText: boolean;
    FormHandle: HWND;
    // IsRTL: Boolean;
begin
    hUser32 := LoadLibraryEx(user32, 0, LOAD_LIBRARY_AS_DATAFILE);
    if (hUser32 <> 0) then
        try
            hmnu := LoadMenu(hUser32, MAKEINTRESOURCE(1));
            if (hmnu <> 0) then
                try
                    hmenuTrackPopup := GetSubMenu(hmnu, 0);

                    HasSelText := Length(re.SelText) <> 0;
                    EnableMenuItem(hmnu, IDM_UNDO, Enables[re.CanUndo]);
                    EnableMenuItem(hmnu, IDM_CUT, Enables[HasSelText]);
                    EnableMenuItem(hmnu, IDM_COPY, Enables[HasSelText]);
                    EnableMenuItem(hmnu, IDM_PASTE,
                      Enables[Clipboard.HasFormat(CF_TEXT)]);
                    EnableMenuItem(hmnu, IDM_DELETE, Enables[HasSelText]);
                    EnableMenuItem(hmnu, IDM_SELALL,
                      Enables[Length(re.text) <> 0]);

                    // IsRTL := GetWindowLong(re.Handle, GWL_EXSTYLE) and WS_EX_RTLREADING <> 0;
                    // EnableMenuItem(hmnu, IDM_RTL, Enables[True]);
                    // CheckMenuItem(hmnu, IDM_RTL, Checks[IsRTL]);

                    FormHandle := GetParentForm(re).Handle;
                    Flags := TPM_LEFTALIGN or TPM_RIGHTBUTTON or TPM_NONOTIFY or
                      TPM_RETURNCMD;
                    Cmd := DWORD(TrackPopupMenu(hmenuTrackPopup, Flags,
                      Mouse.CursorPos.X, Mouse.CursorPos.Y, 0,
                      FormHandle, nil));
                    if Cmd <> 0 then
                    begin
                        case Cmd of
                            IDM_UNDO:
                                re.Undo;
                            IDM_CUT:
                                re.CutToClipboard;
                            IDM_COPY:
                                re.CopyToClipboard;
                            IDM_PASTE:
                                re.PasteFromClipboard;
                            IDM_DELETE:
                                re.ClearSelection;
                            IDM_SELALL:
                                re.SelectAll;
                            IDM_RTL:
                                ; // ?
                        end;
                    end;
                finally
                    DestroyMenu(hmnu);
                end;
        finally
            FreeLibrary(hUser32);
        end;
end;

end.
