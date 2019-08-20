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
        StringGrid1: TStringGrid;
        Panel3: TPanel;
        ComboBox1: TComboBox;
        Splitter1: TSplitter;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure FormShow(Sender: TObject);
        procedure Panel1Resize(Sender: TObject);
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

procedure TFormCharts.FormShow(Sender: TObject);
begin
    //
end;

procedure TFormCharts.Panel1Resize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := 70;
        ColWidths[2] := Panel1.Width - ColWidths[0] - ColWidths[1] - 10;
        Repaint;
    end;
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
begin
    if ARow - 1 >= length(FBuckets) then
        exit;
    FormChartSeries.Hide;
    Mil82HttpGetResponseAsync(Format(GetAppHttpAddr + '/chart?bucket=%d',
      [FBuckets[ARow - 1].BucketID]), OnResponse);
end;

procedure TFormCharts.ComboBox1Change(Sender: TObject);
var
    I: Integer;
begin

    with StringGrid1 do
    begin
        ColCount := 3;

        OnSelectCell := nil;
        with FYearMonth[ComboBox1.ItemIndex] do
            FBuckets := TChartsSvc.BucketsOfYearMonth(year, month);
        RowCount := length(FBuckets) + 1;
        if RowCount = 1 then
            exit;

        FixedRows := 1;
        Cells[0, 0] := 'День';
        Cells[1, 0] := 'Вермя';
        Cells[2, 0] := 'Работа';

        for I := 0 to length(FBuckets) - 1 do
            with FBuckets[I] do
            begin
                Cells[0, I + 1] := IntToStr2(day);
                Cells[1, I + 1] :=
                  Format('%s:%s', [IntToStr2(hour), IntToStr2(minute)]);
                Cells[2, I + 1] := Name;
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
            if (StringGrid1.Row = I + 1) AND Last then
                FormChartSeries.AddValue(Addr, var_id, value, time);
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
            year := YearOf(now);
            month := MonthOf(now);
            FYearMonth := [ym];
        end;

    for I := 0 to length(FYearMonth) - 1 do
        with FYearMonth[I] do
            ComboBox1.Items.Add(Format('%d %s',
              [year, FormatDateTime('MMMM', IncMonth(0, month))]));

    ComboBox1.ItemIndex := 0;
    ComboBox1Change(nil);
end;

procedure TFormCharts.OnResponse(AResponse: TBytes);
var
    stored_at: TDateTime;
    address, month, day, hour, minute, second: byte;

    variable, year, millisecond: word;
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
        year := BR.ReadWord;
        month := BR.ReadByte;
        day := BR.ReadByte;
        hour := BR.ReadByte;
        minute := BR.ReadByte;
        second := BR.ReadByte;
        millisecond := BR.ReadWord;

        stored_at := EncodeDateTime(year, month, day, hour, minute, second,
          millisecond);

        value := BR.ReadDouble;

        FormChartSeries.AddValue(address, variable, value, stored_at);

    end;
    FormChartSeries.show;

    ms.Free;
    BR.Free;
end;

end.
