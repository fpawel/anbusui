unit stringgridutils;

interface

uses vcl.grids, system.types, system.Classes,vcl.graphics, vcl.controls;

procedure StringGrid_Clear(grd: TStringGrid);
procedure StringGrid_DrawCheckBoxCell(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean);
procedure StringGrid_Redraw(grd: TStringGrid);
procedure StringGrid_RedrawCell(grd: TStringGrid; acol, arow: integer);
procedure StringGrid_RedrawRow(grd: TStringGrid; arow: integer);
procedure DrawCheckbox(par_ctrl:TWinControl; cnv:TCanvas; Rect: TRect; checked:boolean; text:string );

procedure StringGrid_DrawCellBounds(cnv: TCanvas; acol, arow: integer;
  Rect: TRect);

procedure StringGrid_RedrawCol(grd: TStringGrid; acol: integer);


implementation

uses winapi.windows, system.math, winapi.uxtheme;

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

procedure StringGrid_RedrawCol(grd: TStringGrid; acol: integer);
var
    I: integer;
begin
    with grd do
        if (acol >= 0) AND (acol < colcount) then
            for I := 0 to rowcount - 1 do
                Cells[acol,i] := Cells[acol,i];
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
    I: integer;
begin
    with grd do
        if (arow >= 0) AND (arow < rowcount) then
            for I := 0 to colcount - 1 do

                Cells[I, arow] := Cells[I, arow];
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
    DrawCheckbox(grd,grd.Canvas, rect, checked, grd.Cells[acol, arow]);
end;

procedure DrawCheckbox(par_ctrl:TWinControl; cnv:TCanvas;Rect: TRect;  checked:boolean; text:string );
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

    DrawText(cnv.Handle, text,
      length(text), r, DT_SINGLELINE or DT_VCENTER or
      DT_RIGHT or DT_END_ELLIPSIS);

end;

end.
