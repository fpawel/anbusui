unit UnitFormCharts;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
    server_data_types, UnitFormChartSeries;

type
    TFormCharts = class(TForm)
        Panel1: TPanel;
        Panel3: TPanel;
        ComboBox1: TComboBox;
        StringGrid1: TStringGrid;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure FormShow(Sender: TObject);
        procedure FormResize(Sender: TObject);
    private
        { Private declarations }
        FBuckets: TArray<TChartsBucket>;
        FYearMonth: TArray<TYearMonth>;

        procedure OnResponse(AResponse: TBytes);

    public
        { Public declarations }
        procedure FetchYearsMonths;
        procedure AddValue(Addr, var_id: Integer; value: double;
          time: TDateTime);
    end;

var
    FormCharts: TFormCharts;

implementation

{$R *.dfm}

uses Grijjy.Http, httprpcclient, dateutils, stringgridutils, services,
    stringutils, app, HttpClient;

procedure TFormCharts.FormCreate(Sender: TObject);
begin
    //
end;

procedure TFormCharts.FormResize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := Panel1.Width - ColWidths[0] - 10;
        Repaint;
    end;
end;

procedure TFormCharts.FormShow(Sender: TObject);
begin
    //
end;

procedure TFormCharts.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;

begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption;

    ta := taLeftJustify;
    case ACol of
        0:
            begin
                ta := taCenter;
                cnv.Font.Color := clGreen;
            end;
        1:
            begin
                ta := taLeftJustify;
                cnv.Font.Color := clBlack;
            end;
    end;
    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
end;

procedure TFormCharts.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    s: string;
begin
    if ARow - 1 >= length(FBuckets) then
        exit;
    FormChartSeries.Hide;
    with FBuckets[ARow - 1], FYearMonth[ComboBox1.ItemIndex] do
    begin
        if IsLast = true then
        begin
            s := ', текущий';
            FormChartSeries.MemoTitle.Font.Color := clBlue;
        end
        else
        begin
            s := '';
            FormChartSeries.MemoTitle.Font.Color := clNavy;
        end;
        FormChartSeries.MemoTitle.Text := format('График №%d, %d.%s.%s %s:%s%s',
          [BucketID, Year, inttostr2(Month), inttostr2(Day), inttostr2(Hour),
          inttostr2(Minute), s]);

        AnbusHttpGetResponseAsync(format(GetAppHttpAddr + '/chart?bucket=%d',
          [BucketID]), OnResponse);

    end;

end;

procedure TFormCharts.ComboBox1Change(Sender: TObject);
var
    I: Integer;
begin

    with StringGrid1 do
    begin
        ColCount := 2;

        OnSelectCell := nil;
        with FYearMonth[ComboBox1.ItemIndex] do
            FBuckets := TChartsSvc.BucketsOfYearMonth(Year, Month);
        RowCount := length(FBuckets) + 1;
        if RowCount = 1 then
            exit;

        FixedRows := 1;
        Cells[0, 0] := 'День';
        Cells[1, 0] := 'Время';

        for I := 0 to length(FBuckets) - 1 do
            with FBuckets[I] do
            begin
                Cells[0, I + 1] := inttostr2(Day);
                Cells[1, I + 1] :=
                  format('%s:%s', [inttostr2(Hour), inttostr2(Minute)]);
            end;

        OnSelectCell := StringGrid1SelectCell;
        Row := RowCount - 1;
    end;

end;

procedure TFormCharts.AddValue(Addr, var_id: Integer; value: double;
  time: TDateTime);
var
    I: Integer;
begin
    for I := 0 to length(FBuckets) do
        with FBuckets[I] do
            with StringGrid1 do
                if (Row = I + 1) AND IsLast then
                begin
                    FormChartSeries.AddValue(Addr, var_id, value, time);
                end;
end;

procedure TFormCharts.FetchYearsMonths;
var
    I: Integer;
    ym: TYearMonth;
begin
    ComboBox1.Clear;
    FYearMonth := TChartsSvc.YearsMonths;
    if length(FYearMonth) = 0 then
        with ym do
        begin
            Year := YearOf(now);
            Month := MonthOf(now);
            FYearMonth := [ym];
        end;

    for I := 0 to length(FYearMonth) - 1 do
        with FYearMonth[I] do
            ComboBox1.Items.Add(format('%d %s',
              [Year, FormatDateTime('MMMM', IncMonth(0, Month))]));

    ComboBox1.ItemIndex := 0;
    ComboBox1Change(nil);
end;

procedure TFormCharts.OnResponse(AResponse: TBytes);
var
    stored_at: TDateTime;
    address, Month, Day, Hour, Minute, second: byte;

    variable, Year, millisecond: word;
    value: double;
    ms: TMemoryStream;
    BR: TBinaryReader;

    I: Integer;
    n: LongInt;
begin
    ms := TMemoryStream.Create;
    BR := TBinaryReader.Create(ms);

    FormChartSeries.NewChart;

    ms.Write(AResponse, 0, length(AResponse));
    ms.Seek(0, TSeekOrigin.soBeginning);
    n := BR.ReadInt64;
    for I := 0 to n - 1 do
    begin
        address := BR.ReadByte;
        variable := BR.ReadWord;
        Year := BR.ReadWord;
        Month := BR.ReadByte;
        Day := BR.ReadByte;
        Hour := BR.ReadByte;
        Minute := BR.ReadByte;
        second := BR.ReadByte;
        millisecond := BR.ReadWord;

        stored_at := EncodeDateTime(Year, Month, Day, Hour, Minute, second,
          millisecond);

        value := BR.ReadDouble;

        FormChartSeries.AddValue(address, variable, value, stored_at);

    end;
    FormChartSeries.show;

    ms.Free;
    BR.Free;
end;

end.
