unit stringgridutils;

interface

uses vcl.grids, system.types, system.Classes, vcl.graphics, vcl.controls;

procedure StringGrid_Clear(grd: TStringGrid);
procedure StringGrid_DrawCheckBoxCell(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean);
procedure StringGrid_Redraw(grd: TStringGrid);
procedure StringGrid_RedrawCell(grd: TStringGrid; acol, arow: integer);
procedure StringGrid_RedrawRow(grd: TStringGrid; arow: integer);

procedure DrawCheckbox(par_ctrl: TWinControl; cnv: TCanvas; Rect: TRect;
  checked: boolean; text: string);

procedure StringGrid_DrawCheckBoxCellBmp(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean;
  bitmaps: TArray<vcl.graphics.TBitmap>);

procedure StringGrid_DrawCellBounds(cnv: TCanvas; acol, arow: integer;
  Rect: TRect);

procedure StringGrid_DrawCellBmp(grd: TStringGrid; Rect: TRect;
  bmp: vcl.graphics.TBitmap);

procedure StringGrid_RedrawCol(grd: TStringGrid; acol: integer);

procedure DrawCellText(StringGrid1: TStringGrid; acol, arow: integer;
  Rect: TRect; ta: TAlignment; text: string);

procedure StringGrid_CopytoClipboard(StringGrid1: TStringGrid);

procedure StringGrid_DrawMeregedCell(grd: TStringGrid; AText: string;
  arow: integer; Rect: TRect);

procedure StringGrid_SetupColumnWidth(grd: TStringGrid; acol: integer);
procedure StringGrid_SetupColumnsWidth(grd: TStringGrid);
procedure StringGrid_Unselect(grd: TStringGrid);

implementation

uses Clipbrd, winapi.windows, system.math, winapi.uxtheme, stringutils;

procedure StringGrid_CopytoClipboard(StringGrid1: TStringGrid);
var
    c, r: integer;
    s: string;
begin
    s := '';
    with StringGrid1 do
        for r := Selection.Top to Selection.Bottom do
            for c := Selection.Left to Selection.Right do
            begin
                s := s + Cells[c, r];
                if c < Selection.Right then
                    s := s + #9
                else
                begin
                    if r < Selection.Bottom then
                        s := s + #13;
                end;
            end;
    Clipboard.AsText := s;
end;

procedure DrawCellText(StringGrid1: TStringGrid; acol, arow: integer;
  Rect: TRect; ta: TAlignment; text: string);
var
    x, Y, txt_width, txt_height: integer;
begin
    with StringGrid1.Canvas do
    begin

        if TextWidth(text) + 3 > Rect.Width then
            text := cut_str(text, StringGrid1.Canvas, Rect.Width);
        txt_width := TextWidth(text);
        txt_height := TextHeight(text);
        x := Rect.Left + 3;
        if ta = taRightJustify then
            x := Rect.Right - 3 - round(txt_width)
        else if ta = taCenter then
            x := Rect.Left + round((Rect.Width - txt_width) / 2.0);
        Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);
        TextRect(Rect, x, Y, text);
    end;
end;

procedure StringGrid_DrawCellBounds(cnv: TCanvas; acol, arow: integer;
  Rect: TRect);
begin
    with cnv do
    begin
        Pen.Color := $00BCBCBC;
        Pen.Width := -1;
        if arow = 0 then
        begin
            MoveTo(Rect.Left, Rect.Top);
            LineTo(Rect.Right, Rect.Top);
        end
        else
            MoveTo(Rect.Right, Rect.Top);

        LineTo(Rect.Right, Rect.Bottom);
        LineTo(Rect.Left, Rect.Bottom);
        if acol = 0 then
            LineTo(Rect.Left, Rect.Top);
    end;
end;

procedure StringGrid_Redraw(grd: TStringGrid);
var
    acol, arow: integer;
begin
    with grd do
        for acol := 0 to colcount - 1 do
            for arow := 0 to rowcount - 1 do
                Cells[acol, arow] := Cells[acol, arow];
end;

procedure StringGrid_RedrawCell(grd: TStringGrid; acol, arow: integer);
begin
    with grd do
        if (acol >= 0) AND (acol < colcount) AND (arow >= 0) AND
          (arow < rowcount) then
            Cells[acol, arow] := Cells[acol, arow];
end;

procedure StringGrid_RedrawRow(grd: TStringGrid; arow: integer);
var
    cl: integer;
begin
    with grd do
        if (arow >= 0) AND (arow < rowcount) then
            for cl := 0 to colcount - 1 do
                Cells[cl, arow] := Cells[cl, arow];
end;

procedure StringGrid_RedrawCol(grd: TStringGrid; acol: integer);
var
    ARow: integer;
begin
    with grd do
        if (acol >= 0) AND (acol < colcount) then
            for ARow := 0 to rowcount - 1 do
                Cells[ACol, ARow] := Cells[ACol, ARow];
end;

procedure StringGrid_Clear(grd: TStringGrid);
var
    acol, arow: integer;
begin
    with grd do
        for acol := 1 to colcount - 1 do
            for arow := 1 to rowcount - 1 do
                Cells[acol, arow] := '';
end;

procedure StringGrid_DrawCheckBoxCell(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean);
begin
    grd.Canvas.FillRect(Rect);
    DrawCheckbox(grd, grd.Canvas, Rect, checked, grd.Cells[acol, arow]);
end;

procedure DrawCheckbox(par_ctrl: TWinControl; cnv: TCanvas; Rect: TRect;
  checked: boolean; text: string);
const
    PADDING = 4;
var
    h: HTHEME;
    s: TSize;
    r: TRect;
begin
    s.cx := GetSystemMetrics(SM_CXMENUCHECK);
    s.cy := GetSystemMetrics(SM_CYMENUCHECK);
    if UseThemes then
    begin
        h := OpenThemeData(par_ctrl.Handle, 'BUTTON');
        if h <> 0 then
            try
                GetThemePartSize(par_ctrl.Handle, cnv.Handle, BP_CHECKBOX,
                  CBS_CHECKEDNORMAL, nil, TS_DRAW, s);
                r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
                r.Bottom := r.Top + s.cy;
                r.Left := Rect.Left + PADDING;
                r.Right := r.Left + s.cx;
                DrawThemeBackground(h, cnv.Handle, BP_CHECKBOX,
                  IfThen(checked, CBS_CHECKEDNORMAL,
                  CBS_UNCHECKEDNORMAL), r, nil);
            finally
                CloseThemeData(h);
            end;
    end
    else
    begin
        r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
        r.Bottom := r.Top + s.cy;
        r.Left := Rect.Left + PADDING;
        r.Right := r.Left + s.cx;
        DrawFrameControl(cnv.Handle, r, DFC_BUTTON,
          IfThen(checked, DFCS_CHECKED, DFCS_BUTTONCHECK));
    end;

    r := system.Classes.Rect(r.Right + PADDING, Rect.Top, Rect.Right,
      Rect.Bottom);
    r.Right := r.Right - 3;

    DrawText(cnv.Handle, text, length(text), r, DT_SINGLELINE or DT_VCENTER or
      DT_RIGHT or DT_END_ELLIPSIS);

end;

procedure StringGrid_DrawCheckBoxCellBmp(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean;
  bitmaps: TArray<vcl.graphics.TBitmap>);

const
    PADDING = 4;
var
    h: HTHEME;
    s: TSize;
    r, text_rect: TRect;
    bmp: vcl.graphics.TBitmap;
    x: integer;
begin
    grd.Canvas.FillRect(Rect);
    s.cx := GetSystemMetrics(SM_CXMENUCHECK);
    s.cy := GetSystemMetrics(SM_CYMENUCHECK);
    if UseThemes then
    begin
        h := OpenThemeData(grd.Handle, 'BUTTON');
        if h <> 0 then
            try
                GetThemePartSize(grd.Handle, grd.Canvas.Handle, BP_CHECKBOX,
                  CBS_CHECKEDNORMAL, nil, TS_DRAW, s);
                r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
                r.Bottom := r.Top + s.cy;
                r.Left := Rect.Left + PADDING;
                r.Right := r.Left + s.cx;
                DrawThemeBackground(h, grd.Canvas.Handle, BP_CHECKBOX,
                  IfThen(checked, CBS_CHECKEDNORMAL,
                  CBS_UNCHECKEDNORMAL), r, nil);
            finally
                CloseThemeData(h);
            end;
    end
    else
    begin
        r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
        r.Bottom := r.Top + s.cy;
        r.Left := Rect.Left + PADDING;
        r.Right := r.Left + s.cx;
        DrawFrameControl(grd.Canvas.Handle, r, DFC_BUTTON,
          IfThen(checked, DFCS_CHECKED, DFCS_BUTTONCHECK));
    end;

    text_rect := system.Classes.Rect(r.Right + PADDING, Rect.Top, Rect.Right,
      Rect.Bottom);
    text_rect.Right := text_rect.Right - 3;

    x := r.Right + 2;
    for bmp in bitmaps do
    begin
        grd.Canvas.Draw(x, Rect.Top + 3, bmp);
        x := x + bmp.Width + 2;
    end;

    DrawText(grd.Canvas.Handle, grd.Cells[acol, arow],
      length(grd.Cells[acol, arow]), text_rect, DT_SINGLELINE or DT_VCENTER or
      DT_RIGHT or DT_END_ELLIPSIS);

end;

procedure StringGrid_DrawCellBmp(grd: TStringGrid; Rect: TRect;
  bmp: vcl.graphics.TBitmap);
const
    PADDING = 4;
var
    h: HTHEME;
    s: TSize;
    r: TRect;
    x: integer;
begin
    grd.Canvas.FillRect(Rect);
    s.cx := GetSystemMetrics(SM_CXMENUCHECK);
    s.cy := GetSystemMetrics(SM_CYMENUCHECK);
    if UseThemes then
    begin
        h := OpenThemeData(grd.Handle, 'BUTTON');
        if h <> 0 then
            try
                GetThemePartSize(grd.Handle, grd.Canvas.Handle, BP_CHECKBOX,
                  CBS_CHECKEDNORMAL, nil, TS_DRAW, s);
                r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
                r.Bottom := r.Top + s.cy;
                r.Left := Rect.Left + PADDING;
                r.Right := r.Left + s.cx;
            finally
                CloseThemeData(h);
            end;
    end
    else
    begin
        r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
        r.Bottom := r.Top + s.cy;
        r.Left := Rect.Left + PADDING;
        r.Right := r.Left + s.cx;
    end;
    x := r.Right + 2;
    grd.Canvas.Draw(r.Left + 2, Rect.Top + 3, bmp);
end;

procedure StringGrid_DrawMeregedCell(grd: TStringGrid; AText: string;
  arow: integer; Rect: TRect);
var
    ACOl1, ACOl2, TxtW, TxtH, txtX, txtY: integer;
    txtRct: TRect;
begin
    ACOl1 := grd.LeftCol;
    ACOl2 := grd.LeftCol + grd.VisibleColCount;

    txtRct.Top := grd.CellRect(ACOl1, arow).Top;
    txtRct.Left := grd.CellRect(ACOl1, arow).Left;
    txtRct.Right := grd.CellRect(ACOl2, arow).Right;
    txtRct.Bottom := grd.CellRect(ACOl2, arow).Bottom;

    TxtW := grd.Canvas.TextWidth(AText);
    TxtH := grd.Canvas.TextHeight(AText);
    // txtX := txtRct.Left + round((txtRct.Right - txtRct.Left - TxtW) / 2);
    txtX := txtRct.Left + 5;
    txtY := txtRct.Top + round((txtRct.Bottom - txtRct.Top - TxtH) / 2);

    // grd.Canvas.FillRect(RegionRect);
    grd.Canvas.TextRect(txtRct, txtX, txtY, AText);
    // grd.Canvas.TextOut(txtX, txtY, txt);

    // обрамление
    grd.Canvas.MoveTo(txtRct.Left, txtRct.Bottom);
    grd.Canvas.LineTo(txtRct.Right, txtRct.Bottom);
end;

procedure StringGrid_SetupColumnWidth(grd: TStringGrid; acol: integer);
var
    w, arow: integer;
begin
    with grd do
    begin
        ColWidths[acol] := DefaultColWidth;
        for arow := 0 to rowcount - 1 do
        begin
            w := Canvas.TextWidth(Cells[acol, arow]);
            if ColWidths[acol] < w + 5 then
                ColWidths[acol] := w + 5;
        end;
    end;

end;

procedure StringGrid_Unselect(grd: TStringGrid);
var
    gr: TGridRect;
begin
    gr.Left := -1;
    gr.Right := -1;
    gr.Top := -1;
    gr.Bottom := -1;
    grd.Selection := gr;

end;

procedure StringGrid_SetupColumnsWidth(grd: TStringGrid);
var
    acol: integer;
begin
    with grd do
        for acol := 0 to colcount - 1 do
            StringGrid_SetupColumnWidth(grd, acol);
end;

end.
