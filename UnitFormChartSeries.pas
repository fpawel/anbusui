unit UnitFormChartSeries;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    VclTee.TeeProcs, VclTee.Chart, Vcl.StdCtrls, Vcl.ExtCtrls,
    System.Generics.collections, System.Generics.Defaults,
    VclTee.Series, Vcl.ComCtrls,
    Vcl.ToolWin, System.ImageList, Vcl.ImgList;

type
    ProductVar = record
        ProductSerial, VarID: integer;
    end;

    TFormChartSeries = class(TForm)
        Panel14: TPanel;
        Panel1: TPanel;
        Panel4: TPanel;
        Panel8: TPanel;
        Panel10: TPanel;
        ImageList1: TImageList;
        ListBox1: TListBox;
        PanelConsolePlaceholder: TPanel;
        Chart1: TChart;
        Label1: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure Chart1AfterDraw(Sender: TObject);
        procedure Chart1ClickLegend(Sender: TCustomChart; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: integer);
    private
        { Private declarations }
        FSeries: TDictionary<ProductVar, TFastLineSeries>;
        FChart1OriginalWndMethod: TWndMethod;


        procedure Chart1WndMethod(var Message: TMessage);
        procedure SetActiveSeries(ser: TFastLineSeries);
        function GetActiveSeries: TFastLineSeries;
    public
        { Public declarations }
        procedure AddValue(addr, var_id: integer; value: double;
          time: TDateTime);

        function AddVar(avar: integer): integer;
        procedure sortListBox;

        function SeriesOf(addr, var_id: integer): TFastLineSeries;

        procedure NewChart;

        property ActiveSeries: TFastLineSeries read GetActiveSeries
          write SetActiveSeries;
    end;

var
    FormChartSeries: TFormChartSeries;

implementation

{$R *.dfm}

uses stringutils, dateutils, StrUtils, math;

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

procedure Chart_DrawCrossHair(Chart: TChart; ax, ay: integer; color: TColor);
begin
    Chart.Canvas.Brush.color := Chart.BackColor;
    Chart.Canvas.Pen.Style := psSolid;
    Chart.Canvas.Pen.Width := 1;
    Chart.Canvas.Pen.color := color;
    Chart.Canvas.MoveTo(Chart.ChartRect.Left + Chart.Width3D, ay);
    Chart.Canvas.LineTo(ax, ay);
    Chart.Canvas.LineTo(ax, Chart.ChartRect.Bottom - Chart.Height3D);
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
    i, dev_var: integer;
    k: ProductVar;
    xs: array of ProductVar;
    Item: TPair<ProductVar, TFastLineSeries>;
begin
    ActiveSeries := nil;

    SetLength(xs, 0);
    Chart1.RemoveAllSeries;
    for i := 0 to ListBox1.Items.Count - 1 do
    begin
        if not ListBox1.Selected[i] then
            Continue;
        dev_var := strtoint(ListBox1.Items[i]);
        for k in FSeries.Keys do
        begin
            if k.VarID = dev_var then
            begin
                SetLength(xs, length(xs) + 1);
                xs[length(xs) - 1] := k;
            end;
        end;
    end;
    TArray.Sort<ProductVar>(xs, TDelegatedComparer<ProductVar>.Construct(
        function(const a, b: ProductVar): integer
        begin
            Result := TComparer<integer>.Default.Compare(a.VarID, b.VarID);
            if Result = 0 then
                Result := TComparer<integer>.Default.Compare(a.ProductSerial,
                  b.ProductSerial);
        end));
    for i := 0 to length(xs) - 1 do
    begin
        Chart1.AddSeries(FSeries[xs[i]]);
    end;
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
    // Panel12.Caption := format('%s %s', [datetimetostr(now), FChartTitle]);
end;

function CompareVars(List: TStringList; Index1, Index2: integer): integer;
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

function TFormChartSeries.AddVar(avar: integer): integer;
begin
    exit(ListBox1.Items.Add(inttostr(avar)));
end;

procedure TFormChartSeries.Chart1AfterDraw(Sender: TObject);
var
    i, xPos, yPos, a, b: integer;
    ser: TChartSeries;
begin

    Chart1.Canvas.Pen.Style := psSolid;
    Chart1.Canvas.Pen.Width := 1;
    Chart1.Canvas.Pen.Mode := pmCopy;

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
            Chart1.Canvas.Font.Color := ser.color;
            Chart1.Canvas.TextOut(
                xPos,
                yPos - Canvas.TextHeight('A'),
                Format('%s:%g',[
                formatDatetime('s.zzz',ser.XValues[i]),ser.YValues[i]
            ]));
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

procedure TFormChartSeries.sortListBox;
var
    sl: TStringList;
begin
    sl := TStringList.create;
    sl.Assign(ListBox1.Items);
    sl.CustomSort(CompareVars);
    ListBox1.Items.Assign(sl);
    sl.Free;
end;

function TFormChartSeries.SeriesOf(addr, var_id: integer): TFastLineSeries;
var
    k: ProductVar;
begin
    k.ProductSerial := addr;
    k.VarID := var_id;
    if not FSeries.TryGetValue(k, Result) then
    begin
        Result := TFastLineSeries.create(nil);
        Result.XValues.DateTime := true;
        Result.title := inttostr3(addr) + ':' + inttostr(var_id);
        FSeries.Add(k, Result);
    end;

end;

procedure TFormChartSeries.AddValue(addr, var_id: integer; value: double;
time: TDateTime);
var
    ser: TFastLineSeries;
    n, i: integer;
    selected_vars: TDictionary<string, integer>;
begin

    if ListBox1.Items.IndexOf(inttostr(var_id)) = -1 then
    begin
        n := AddVar(var_id);
        if var_id = 0 then
            ListBox1.Selected[n] := true;

        selected_vars := TDictionary<string, integer>.create;

        for i := 0 to ListBox1.Items.Count - 1 do
            if ListBox1.Selected[i] then
                selected_vars.AddOrSetValue(ListBox1.Items[i], 0);

        sortListBox;
        for i := 0 to ListBox1.Items.Count - 1 do
            ListBox1.Selected[i] := selected_vars.ContainsKey
              (ListBox1.Items[i]);

        selected_vars.Free;
    end;

    ser := SeriesOf(addr, var_id);
    ser.AddXY(time, value);

    with ListBox1 do
    begin
        if Selected[Items.IndexOf(inttostr(var_id))] then
        begin
            if ser.ParentChart = nil then
                Chart1.AddSeries(ser);
        end;
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

function TFormChartSeries.GetActiveSeries: TFastLineSeries;
var
    i: TPair<ProductVar, TFastLineSeries>;
begin
    for i in FSeries do

        if i.value.Tag > 0 then
            exit(i.value);
    exit(nil);
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
