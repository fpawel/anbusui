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
        Panel14: TPanel;
        Panel1: TPanel;
        Panel4: TPanel;
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
        ToolButton4: TToolButton;
        Panel6: TPanel;
        Splitter1: TSplitter;
        Panel9: TPanel;
        ListBox2: TListBox;
        Panel7: TPanel;
        Panel11: TPanel;
        Memo1: TMemo;
        Panel12: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure Chart1AfterDraw(Sender: TObject);
        procedure Chart1ClickLegend(Sender: TCustomChart; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: integer);
        procedure ToolButton3Click(Sender: TObject);
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
        procedure AddValue(Addr, var_id: integer; value: double;
          time: TDateTime);

        function SeriesOf(Addr, var_id: integer): TFastLineSeries;

        procedure NewChart;

        property ActiveSeries: TFastLineSeries read GetActiveSeries
          write SetActiveSeries;
    end;

var
    FormChartSeries: TFormChartSeries;

implementation

{$R *.dfm}

uses stringutils, dateutils, StrUtils, math, System.Types;

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

// procedure Chart_DrawCrossHair(Chart: TChart; ax, ay: integer; color: TColor);
// begin
// Chart.Canvas.Brush.color := Chart.BackColor;
// Chart.Canvas.Pen.Style := psSolid;
// Chart.Canvas.Pen.Width := 1;
// Chart.Canvas.Pen.color := color;
// Chart.Canvas.MoveTo(Chart.ChartRect.Left + Chart.Width3D, ay);
// Chart.Canvas.LineTo(ax, ay);
// Chart.Canvas.LineTo(ax, Chart.ChartRect.Bottom - Chart.Height3D);
// end;

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

procedure AddStrIntListBox(AListbox: TListBox; new_item: integer);
var
    n, i: integer;
    selected_items: TDictionary<string, integer>;
begin
    if AListbox.Items.IndexOf(inttostr(new_item)) > -1 then
        exit;
    n := AListbox.Items.Add(inttostr(new_item));
    if n = 0 then
        AListbox.Selected[n] := true;

    selected_items := TDictionary<string, integer>.create;

    for i := 0 to AListbox.Items.Count - 1 do
        if AListbox.Selected[i] then
            selected_items.AddOrSetValue(AListbox.Items[i], 0);

    sortListBox(AListbox);
    for i := 0 to AListbox.Items.Count - 1 do
        AListbox.Selected[i] := selected_items.ContainsKey(AListbox.Items[i]);

    selected_items.Free;
end;

procedure TFormChartSeries.FormCreate(Sender: TObject);
begin
    FSeries := TDictionary<ProductVar, TFastLineSeries>.create;
    Chart1.title.Visible := false;

    FChart1OriginalWndMethod := Chart1.WindowProc;
    Chart1.WindowProc := Chart1WndMethod;

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
                    k.VarID := strtoint(ListBox1.Items[i]);
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

    if not ToolButton3.Down then
        exit;

    Chart1.Canvas.Pen.Style := psSolid;
    Chart1.Canvas.Pen.Width := 1;
    Chart1.Canvas.Pen.Mode := pmCopy;
    Chart1.Canvas.Font.Size := 10;

    for ser in Chart1.SeriesList do
    begin
        if not ser.Active then
            Continue;
        Chart1.Canvas.Pen.color := ser.color;
        if ser.Tag > 0 then
            Chart1.Canvas.Brush.color := ser.color;

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

            marker_text := Format('%s � %g',
              [formatDatetime('h:n:s.zzz', ser.XValues[i]), ser.YValues[i]]);
            with marker_rect do
            begin
                Left := xPos;
                Top := yPos - Canvas.TextHeight(marker_text);
                Right := xPos + Canvas.TextWidth(marker_text);
                Bottom := yPos;
            end;

            marker_place := true;
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
                Chart1.Canvas.Font.color := ser.color;
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

procedure TFormChartSeries.Chart1MouseMove(Sender: TObject; Shift: TShiftState;
X, Y: integer);

begin
    //
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
        Result.title := inttostr3(Addr) + ':' + inttostr(var_id);
        FSeries.Add(k, Result);
    end;

end;

procedure TFormChartSeries.AddValue(Addr, var_id: integer; value: double;
time: TDateTime);
var
    ser: TFastLineSeries;
    n, i: integer;
begin
    AddStrIntListBox(ListBox1, var_id);
    AddStrIntListBox(ListBox2, Addr);

    ser := SeriesOf(Addr, var_id);
    ser.AddXY(time, value);

    if ListBox1.Selected[ListBox1.Items.IndexOf(inttostr(var_id))] AND
      ListBox2.Selected[ListBox2.Items.IndexOf(inttostr(Addr))] then
    begin
        if ser.ParentChart = nil then
            Chart1.AddSeries(ser);
    end;

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

procedure TFormChartSeries.ToolButton3Click(Sender: TObject);
begin
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
    s: string;
    v: double;
begin
    with Chart1.Axes.Bottom do
    begin
        v := Maximum - Minimum;
        if v = 0 then
            s := '��� ��������'
        else if v < IncSecond(0, 1) then
            s := inttostr(MilliSecondsBetween(Minimum, Maximum)) + '��'
        else if v < IncMinute(0, 1) then
            s := inttostr(SecondsBetween(Minimum, Maximum)) + ' c'
        else if v < Inchour(0, 1) then
            s := inttostr(minutesBetween(Minimum, Maximum)) + ' �����'
        else if v < Incday(0, 1) then
            s := inttostr(hoursBetween(Minimum, Maximum)) + ' �����'
        else
            s := inttostr(daysBetween(Minimum, Maximum)) + ' ����';

    end;

    with Chart1.Axes.Left do
    begin
        Memo1.Text := Format('X: %s � Y: %g', [s, Maximum - Minimum]);
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

end.
