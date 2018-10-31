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
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
    procedure Chart1AfterDraw(Sender: TObject);
    private
        { Private declarations }
        FSeries: TDictionary<ProductVar, TFastLineSeries>;

    public
        { Public declarations }
        procedure AddValue(addr, var_id: integer; value: double;
          time: TDateTime);

        function AddVar(avar: integer): integer;
        procedure sortListBox;

        function SeriesOf(addr, var_id: integer):TFastLineSeries;

        procedure NewChart;
    end;

var
    FormChartSeries: TFormChartSeries;

implementation

{$R *.dfm}

uses stringutils, dateutils, StrUtils;

procedure TFormChartSeries.FormCreate(Sender: TObject);
begin
    FSeries := TDictionary<ProductVar, TFastLineSeries>.create;
    Chart1.title.Visible := false;

end;

procedure TFormChartSeries.ListBox1Click(Sender: TObject);
var
    i, dev_var: integer;
    k: ProductVar;
    xs: array of ProductVar;
begin
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
  xPos, yPos : integer;
begin
  Chart1.Canvas.Pen.Color := clRed;
  Chart1.Canvas.Pen.Style := psSolid;
  Chart1.Canvas.Pen.Width := 1;
  Chart1.Canvas.Pen.Mode := pmCopy;

  xPos := Chart1.BottomAxis.CalcPosValue(CurrentXValue);
  yPos := Chart1.LeftAxis.CalcPosValue(CurrentYValue);

  // Parameters are
  //  X-Coord, Y-Coord, X-Radius, Y-Radius, Start Angle, End Angle, Hole%
  Chart1.Canvas.Donut(xPos, yPos, 3, 3, 0, 360, 0);
end;

procedure TFormChartSeries.sortListBox;
var sl: TStringList;
begin
    sl := TStringList.create;
    sl.Assign(ListBox1.Items);
    sl.CustomSort(CompareVars);
    ListBox1.Items.Assign(sl);
    sl.Free;
end;

function TFormChartSeries.SeriesOf(addr, var_id: integer):TFastLineSeries;
var
    k: ProductVar;
begin
    k.ProductSerial := addr;
    k.VarID := var_id;
    if not FSeries.TryGetValue(k, result) then
    begin
        result := TFastLineSeries.create(nil);
        result.XValues.DateTime := true;
        result.title :=  inttostr3(addr) + ':' + inttostr(var_id);
        FSeries.Add(k, result);
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

    ser := SeriesOf(addr,var_id);
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

end.
