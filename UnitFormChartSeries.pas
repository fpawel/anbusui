unit UnitFormChartSeries;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    VclTee.TeeProcs, VclTee.Chart, Vcl.StdCtrls, Vcl.ExtCtrls,
    System.Generics.collections, System.Generics.Defaults,
    VclTee.Series, Vcl.ComCtrls,
    Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.CheckLst;

type
    ProductVar = record
        Addr, VarID: integer;
    end;

    TFormChartSeries = class(TForm)
        Panel8: TPanel;
        Panel10: TPanel;
        Panel2: TPanel;
        ListBox1: TListBox;
        Panel3: TPanel;
        Chart1: TChart;
        ImageList1: TImageList;
        Panel5: TPanel;
        ToolBar1: TToolBar;
        ToolButton3: TToolButton;
        Panel6: TPanel;
        Splitter1: TSplitter;
        Panel9: TPanel;
        ListBox2: TListBox;
        Panel7: TPanel;
        Panel11: TPanel;
        Panel12: TPanel;
        ToolButton1: TToolButton;
        GridPanel1: TGridPanel;
        Memo1: TMemo;
        Memo2: TMemo;
        ToolBar2: TToolBar;
        ToolButton6: TToolButton;
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure Chart1AfterDraw(Sender: TObject);
        procedure Chart1ClickLegend(Sender: TCustomChart; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure ToolButton3Click(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure Chart1UndoZoom(Sender: TObject);
        procedure Memo2MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: integer);
    private
        { Private declarations }
        FSeries: TDictionary<ProductVar, TFastLineSeries>;
        FChart1OriginalWndMethod: TWndMethod;

        procedure Chart1WndMethod(var Message: TMessage);
        procedure SetActiveSeries(ser: TFastLineSeries);
        function GetActiveSeries: TFastLineSeries;

        procedure ShowCurrentScaleValues;

    public
        { Public declarations }
        FBucketID: int64;
        FFileName: string;

        procedure AddValue(Addr, var_id: integer; value: double;
          time: TDateTime);

        function SeriesOf(Addr, var_id: integer): TFastLineSeries;

        procedure NewChart;

        property ActiveSeries: TFastLineSeries read GetActiveSeries
          write SetActiveSeries;

        function ToggleSeries(Addr, var_id: integer): boolean;
        procedure SetAddrVarSeries(Addr, var_id: integer; visible: boolean);

        procedure ChangeAxisOrder(c: TWinControl; WheelDelta: integer);

    end;

var
    FormChartSeries: TFormChartSeries;

implementation

{$R *.dfm}

uses stringutils, dateutils, StrUtils, math, System.Types, vclutils,
    ComponentBaloonHintU, app;

function pow2(X: Extended): Extended;
begin
    exit(IntPower(X, 2));
end;

function IsPointInChartRect(Chart: TChart; X, Y: integer): boolean;
begin
    exit(PtInRect(Chart.ChartRect, Point(X - Chart.Width3D,
      Y + Chart.Height3D)));
end;

function SeriesGetCursorValueIndex(ser: TChartSeries; X: double): integer;
var
    i, i1, i2: integer;
    x1, x2, d1, d2: double;
begin
    for i := 1 to ser.Count - 1 do
    begin
        i1 := i - 1;
        i2 := i;
        x1 := ser.XValue[i1];
        x2 := ser.XValue[i2];
        if (x1 <= X) AND (x2 >= X) then
        begin
            d1 := X - x1;
            d2 := x2 - X;
            if (d1 < d2) then
                exit(i1)
            else
                exit(i2);
        end;
    end;
    exit(-1);
end;



function CompareStrInt(List: TStringList; Index1, Index2: integer): integer;
var
    d1, d2: integer;
begin
    d1 := strtoint(List[Index1]);
    d2 := strtoint(List[Index2]);

    if d1 < d2 then
        Result := -1
    else if d1 > d2 then
        Result := 1
    else
        Result := 0;
end;

procedure sortListBox(AListbox: TListBox);
var
    sl: TStringList;
begin
    sl := TStringList.create;
    sl.Assign(AListbox.Items);
    sl.CustomSort(CompareStrInt);
    AListbox.Items.Assign(sl);
    sl.Free;
end;


procedure AddVarListBox(AListbox: TListBox; nvar: integer);
var
    n: integer;

begin
    if AListbox.Items.IndexOf(AppVarName(nvar)) > -1 then
        exit;
    n := AListbox.Items.Add(AppVarName(nvar));
    AListbox.Selected[n] := true;
    if Assigned( AListbox.OnClick ) then
        AListbox.OnClick(AListbox);

end;

procedure AddStrIntListBox(AListbox: TListBox; new_item: integer);
var
    n: integer;

begin
    if AListbox.Items.IndexOf(inttostr(new_item)) > -1 then
        exit;
    n := AListbox.Items.Add(inttostr(new_item));
    AListbox.Selected[n] := true;
    if Assigned( AListbox.OnClick ) then
        AListbox.OnClick(AListbox);

end;

procedure TFormChartSeries.FormCreate(Sender: TObject);
var
    c: TColor;
begin
    FSeries := TDictionary<ProductVar, TFastLineSeries>.create;
    Chart1.title.visible := false;
    FChart1OriginalWndMethod := Chart1.WindowProc;
    Chart1.WindowProc := Chart1WndMethod;
    SetLength(Chart1.ColorPalette, 12);

    Chart1.ColorPalette[0] := clRed;
    Chart1.ColorPalette[1] := clBlue;
    Chart1.ColorPalette[2] := clGreen;
    Chart1.ColorPalette[3] := clBlack;
    Chart1.ColorPalette[4] := clFuchsia;
    Chart1.ColorPalette[5] := clMaroon;
    Chart1.ColorPalette[6] := clTeal;
    Chart1.ColorPalette[7] := $FF8C00;
    Chart1.ColorPalette[8] := clWebDarkGoldenRod;
    Chart1.ColorPalette[9] := clWebIndigo;
    Chart1.ColorPalette[10] := clWebDeepPink;
    Chart1.ColorPalette[11] := clWebDarkSlateGray;

    with Chart1.BottomAxis.Grid do
    begin
        Style := psDashDotDot;
        Color := clGray;
        Width := 0;
    end;

    with Chart1.LeftAxis.Grid do
    begin
        Style := psDashDotDot;
        Color := clGray;
        Width := 0;
    end;

end;

procedure TFormChartSeries.ListBox1Click(Sender: TObject);
var
    i, j, dev_var, Addr: integer;
    k: ProductVar;
    xs: array of ProductVar;
    Item: TPair<ProductVar, TFastLineSeries>;
begin
    ActiveSeries := nil;

    SetLength(xs, 0);
    Chart1.RemoveAllSeries;
    for i := 0 to ListBox1.Items.Count - 1 do
    begin
        if ListBox1.Selected[i] then
        begin
            for j := 0 to ListBox2.Items.Count - 1 do
            begin
                if ListBox2.Selected[j] then
                begin
                    k.Addr := strtoint(ListBox2.Items[j]);
                    k.VarID := AppVarCode(ListBox1.Items[i]);
                    SetLength(xs, length(xs) + 1);
                    xs[length(xs) - 1] := k;
                end;
            end;
        end;
    end;
    TArray.Sort<ProductVar>(xs, TDelegatedComparer<ProductVar>.Construct(
        function(const a, b: ProductVar): integer
        begin
            Result := TComparer<integer>.Default.Compare(a.VarID, b.VarID);
            if Result = 0 then
                Result := TComparer<integer>.Default.Compare(a.Addr, b.Addr);
        end));
    for i := 0 to length(xs) - 1 do
        if FSeries.ContainsKey(xs[i]) then
            Chart1.AddSeries(FSeries[xs[i]]);

end;

procedure TFormChartSeries.Memo2MouseMove(Sender: TObject; Shift: TShiftState;
X, Y: integer);
begin
    TMemo(Sender).SetFocus;
end;

procedure TFormChartSeries.NewChart;
var
    ser: TFastLineSeries;
    k: ProductVar;
begin
    Chart1.RemoveAllSeries;
    for ser in FSeries.Values do
    begin
        ser.Free;
    end;
    FSeries.Clear;
    ListBox1.Clear;
    ListBox1.Height := 40;
    ListBox2.Clear;
    // Panel12.Caption := format('%s %s', [datetimetostr(now), FChartTitle]);
end;

procedure TFormChartSeries.Chart1AfterDraw(Sender: TObject);
var
    i, xPos, yPos, a, b: integer;
    ser: TChartSeries;

    marker_place: boolean;
    marker_rects: array of TRect;
    marker_rect, r2: TRect;
    marker_text: string;
begin

    ShowCurrentScaleValues;

    if (not ToolButton1.Down) and (not ToolButton3.Down) then
        exit;

    Chart1.Canvas.Pen.Style := psSolid;
    Chart1.Canvas.Pen.Width := 1;
    Chart1.Canvas.Pen.Mode := pmCopy;
    Chart1.Canvas.Font.Size := 10;

    for ser in Chart1.SeriesList do
    begin
        if not ser.Active then
            Continue;
        Chart1.Canvas.Pen.Color := ser.Color;
        if ser.Tag > 0 then
            Chart1.Canvas.Brush.Color := ser.Color;

        for i := ser.FirstValueIndex to ser.LastValueIndex do
        begin
            xPos := ser.CalcXPos(i);
            yPos := ser.CalcYPos(i);

            if not PtInRect(Chart1.ChartRect, Point(xPos, yPos)) then
                Continue;

            if (i > ser.FirstValueIndex) AND (i < ser.LastValueIndex) AND
              (pow2(xPos - a) + pow2(yPos - b) < pow2(7)) then
                Continue;

            if ser.Tag > 0 then
            begin
                Chart1.Canvas.Ellipse(xPos - 5, yPos - 5, xPos + 5, yPos + 5);
            end
            else
            begin
                // Parameters are
                // X-Coord, Y-Coord, X-Radius, Y-Radius, Start Angle, End Angle, Hole%
                Chart1.Canvas.Donut(xPos, yPos, 3, 3, -1, 361, 100);
            end;
            a := xPos;
            b := yPos;

            marker_text := Format('%s • %g',
              [formatDatetime('h:n:s.zzz', ser.XValues[i]), ser.YValues[i]]);
            with marker_rect do
            begin
                Left := xPos;
                Top := yPos - Canvas.TextHeight(marker_text);
                Right := xPos + Canvas.TextWidth(marker_text);
                Bottom := yPos;
            end;

            marker_place := ToolButton3.Down;
            for r2 in marker_rects do
            begin
                if System.Types.IntersectRect(marker_rect, r2) then
                begin
                    marker_place := false;
                    break;
                end;
            end;
            if marker_place then
            begin
                Chart1.Canvas.Font.Color := ser.Color;
                Chart1.Canvas.TextOut(marker_rect.Left, marker_rect.Top,
                  marker_text);
                SetLength(marker_rects, length(marker_rects) + 1);
                marker_rects[length(marker_rects) - 1] := marker_rect;
            end;
        end;
    end;

    ser := ActiveSeries;
    if not Assigned(ser) then
        exit;

end;

procedure TFormChartSeries.Chart1ClickLegend(Sender: TCustomChart;
Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
    i: TPair<ProductVar, TFastLineSeries>;
    clicked: boolean;
    new_active_series: TFastLineSeries;
begin
    new_active_series := nil;

    for i in FSeries do
    begin
        clicked := i.value = Chart1.Series[Chart1.Legend.clicked(X, Y)];
        if (Button = mbRight) and clicked then
            i.value.Active := not i.value.Active;
        if i.value.Active AND (i.value.Tag = 0) AND clicked then
            new_active_series := i.value;
    end;
    ActiveSeries := new_active_series;

end;

procedure TFormChartSeries.Chart1UndoZoom(Sender: TObject);
begin
    Chart1.BottomAxis.Automatic := true;
    Chart1.LeftAxis.Automatic := true;
end;

function TFormChartSeries.SeriesOf(Addr, var_id: integer): TFastLineSeries;
var
    k: ProductVar;
begin
    k.Addr := Addr;
    k.VarID := var_id;
    if not FSeries.TryGetValue(k, Result) then
    begin
        Result := TFastLineSeries.create(nil);
        Result.XValues.DateTime := true;
        Result.title := inttostr2(Addr) + ':' + AppVarName(var_id);
        FSeries.Add(k, Result);
    end;

end;

procedure TFormChartSeries.AddValue(Addr, var_id: integer; value: double;
time: TDateTime);
var
    ser: TFastLineSeries;
    n, i: integer;
begin
    AddVarListBox(ListBox1, var_id);
    ListBox1.Height := ListBox1.ItemHeight * ListBox1.Items.Count + 10;
    AddStrIntListBox(ListBox2, Addr);

    ser := SeriesOf(Addr, var_id);
    ser.AddXY(time, value);
end;

procedure TFormChartSeries.SetActiveSeries(ser: TFastLineSeries);
var
    i: TPair<ProductVar, TFastLineSeries>;
begin
    for i in FSeries do
    begin
        if i.value <> ser then
        begin
            i.value.Tag := 0;
            i.value.LinePen.Width := 1;
        end
        else
        begin
            i.value.Tag := 1;
            i.value.LinePen.Width := 4;
        end;
    end;
end;

procedure TFormChartSeries.ToolButton1Click(Sender: TObject);
begin
    if ToolButton1.Down then
        ToolButton3.Down := false;
    Chart1.Repaint;
end;

procedure TFormChartSeries.ToolButton3Click(Sender: TObject);
begin
    if ToolButton3.Down then
        ToolButton1.Down := false;
    Chart1.Repaint;
end;

function TFormChartSeries.GetActiveSeries: TFastLineSeries;
var
    i: TPair<ProductVar, TFastLineSeries>;
begin
    for i in FSeries do

        if i.value.Tag > 0 then
            exit(i.value);
    exit(nil);
end;

procedure TFormChartSeries.ShowCurrentScaleValues;
var
    s, s1, s2, s3: string;
    v: double;
begin

    with Chart1.Axes.Bottom do
    begin
        v := Maximum - Minimum;

        s1 := TimetoStr(Minimum);
        s2 := TimetoStr(Maximum);
        s3 := TimetoStr(v);


        if v = 0 then
            s := 'нет значений'
        else if v < IncSecond(0, 1) then
            s := inttostr(MilliSecondsBetween(Maximum, Minimum )) + 'мс'
        else if v < IncMinute(0, 1) then
            s := inttostr(SecondsBetween(Maximum, Minimum)) + ' c'
        else if v < Inchour(0, 1) then
            s := inttostr(minutesBetween(Maximum, Minimum)) + ' минут'
        else if v < Incday(0, 1) then
            s := inttostr(hoursBetween(Maximum, Minimum)) + ' часов'
        else
            s := inttostr(daysBetween(Maximum, Minimum)) + ' дней';

    end;
    Memo1.Text := Format('X: %s', [s]);
    with Chart1.Axes.Left do
    begin
        if Maximum = Minimum then
            Memo2.Text := 'Y: нет значений'
        else
            Memo2.Text := Format('Y: %g', [Maximum - Minimum]);
    end;

end;

procedure TFormChartSeries.Chart1WndMethod(var Message: TMessage);
begin
    case Message.Msg of
        WM_MOUSELEAVE:
            begin

            end;
        WM_MOUSEMOVE:
            ;
    end;
    FChart1OriginalWndMethod(Message);
end;

procedure TFormChartSeries.SetAddrVarSeries(Addr, var_id: integer;
visible: boolean);
var
    ser: TFastLineSeries;
begin
    ser := SeriesOf(Addr, var_id);
    if visible then
    begin
        AddVarListBox(ListBox1, var_id);
        ListBox1.Height := ListBox1.ItemHeight * ListBox1.Items.Count + 10;
        AddStrIntListBox(ListBox2, Addr);
        ser.ParentChart := Chart1;
        ser.Active := true;
    end
    else
        ser.ParentChart := nil;
end;

function TFormChartSeries.ToggleSeries(Addr, var_id: integer): boolean;
var
    k: ProductVar;
    ser: TFastLineSeries;
begin
    k.Addr := Addr;
    k.VarID := var_id;
    if FSeries.ContainsKey(k) then
    begin
        ser := FSeries[k];
        SetAddrVarSeries(Addr, var_id, ser.ParentChart <> Chart1);
        exit(ser.ParentChart <> Chart1);
    end;
    exit(false);
end;

procedure TFormChartSeries.ChangeAxisOrder(c: TWinControl; WheelDelta: integer);
var
    a: TChartAxis;
    step: double;
begin
    if c = Memo2 then
        a := Chart1.LeftAxis
    else if c = Memo1 then
        a := Chart1.BottomAxis
    else
        exit;
    if a.Minimum = a.Maximum then
        exit;

    step := (a.Maximum - a.Minimum) * 0.03;
    if WheelDelta < 0 then
        step := step * -1;

    if a.Minimum - step >= a.Maximum + step then
        exit;

    a.SetMinMax(a.Minimum - step, a.Maximum + step);

end;

function time_to_json(t: TDateTime): string;
var
    Y, mo, d, h, mn, s, ml: Word;
begin
    DecodeDateTime(t, Y, mo, d, h, mn, s, ml);
    Result := Format
      ('{"Year":%d,  "Month":%d, "Day":%d, "Hour":%d, "Minute":%d, "Second":%d, "Millisecond":%d}',
      [Y, mo, d, h, mn, s, ml]);
end;


end.
