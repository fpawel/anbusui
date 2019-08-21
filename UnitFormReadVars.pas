unit UnitFormReadVars;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ExtCtrls, System.Generics.collections, server_data_types;

type
    TStringGridEx = class helper for TStringGrid
    public
        function GetInplaceEditor(): TInplaceEdit;
    end;

    TFormReadVars = class(TForm)
        StringGrid1: TStringGrid;
        procedure StringGrid1DblClick(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
        procedure FormCreate(Sender: TObject);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
          const Value: string);
        procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
          X, Y: Integer);
    private
        { Private declarations }
        Last_Edited_Col, Last_Edited_Row: Integer;

        FPlace: Integer;
        FVarIndex: Integer;
        FErrors: TDictionary<string, string>;
        FNetwork: TConfigNetwork;
        FInitialized: Boolean;

        procedure ToggleRowChecked(row: Integer);
        procedure ToggleColChecked(col: Integer);

        function AddrByIndex(place: Integer): Integer;
        function VarByIndex(varindex: Integer): Integer;

    public
        { Public declarations }


        function FormatAddrPlace(place, varindex: Integer): string;

        procedure SetNetwork(ANetwork: TConfigNetwork);
        procedure HandleReadVar(X: TReadVar);
        procedure reset;

        procedure UpdateNetwork;
    end;

var
    FormReadVars: TFormReadVars;

implementation

{$R *.dfm}

uses stringgridutils, rest.json, stringutils, UnitFormPopup,
    UnitFormChartSeries, ujsonrpc, superobject, services, UnitAnbusMainForm,
  UnitFormCharts;

function TStringGridEx.GetInplaceEditor: TInplaceEdit;
begin
    Result := InplaceEditor; // get access to InplaceEditor
end;

function col2place(col: Integer): Integer;
begin
    Result := col - 1;
end;

function row2var(row: Integer): Integer;
begin
    Result := row - 1;
end;

function place2col(place: Integer): Integer;
begin
    Result := place + 1;
end;

function var2row(varindex: Integer): Integer;
begin
    Result := varindex + 1;
end;

function plk(place: Integer): string;
begin
    Result := inttostr(place) + '_';
end;

function pvk(place, varindex: Integer): string;
begin
    Result := inttostr(place) + '_' + inttostr(varindex);
end;

function pvkk(col, row: Integer): string;
begin
    if row = 0 then
        exit(plk(col2place(col)))
    else
        exit(pvk(col2place(col), row2var(row)));
end;

procedure TFormReadVars.FormCreate(Sender: TObject);
begin
    FPlace := -1;
    FVarIndex := -1;
    FErrors := TDictionary<string, string>.Create;
    FInitialized := false;

end;

procedure TFormReadVars.StringGrid1DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    place, varInd: Integer;
    k: string;
    ACol, ARow: Integer;

    requestObj: TJsonRpcMessage;

    response: IJsonRpcParsed;
    network: TConfigNetwork;
    str_response: string;
begin

    with StringGrid1 do
    begin
        GetCursorPos(pt);
        pt := ScreenToClient(pt);
        MouseToCell(pt.X, pt.Y, ACol, ARow);

        if (ACol = 0) AND (ARow = 0) then
        begin
            SetNetwork(TConfigSvc.ToggleNetwork);
            exit;
        end;

        varInd := row2var(ACol);
        place := col2place(ARow);
        k := pvkk(ACol, ARow);
        r := CellRect(ACol, ARow);
        pt := ClientToScreen(r.BottomRight);
    end;

    if FErrors.ContainsKey(k) then
        with FormPopup do
        begin
            RichEdit1.Font.Color := clRed;
            RichEdit1.Text := stringreplace(FErrors[k], ': ',
      #13#10#9' - ', [rfReplaceAll, rfIgnoreCase]);

            Left := pt.X + 5;
            Top := pt.Y + 5;
            Show;
        end;

end;

procedure TFormReadVars.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    X, Y: Integer;
    txt_width, txt_height: double;
    s: string;
    Checked_col, Checked_row: Boolean;
    // pv: RProductVarOrder;
    place, var_ind: Integer;
const
    lineColor: TColor = $00BCBCBC;
begin
    place := col2place(ACol);
    var_ind := row2var(ARow);

    grd := TStringGrid(Sender);
    s := grd.Cells[ACol, ARow];
    if (ACol = 0) and (ARow > 0) then
        s := '';
    cnv := grd.Canvas;
    cnv.Font.Assign(self.Font);

    Checked_row := false;
    if var_ind > -1 then
        Checked_row := FNetwork.Vars[var_ind].Check;

    Checked_col := false;
    if place > -1 then
        Checked_col := FNetwork.Places[place].Check;

    if (ACol = 0) or (ARow = 0) then
        cnv.Brush.Color := cl3DLight
    else if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if Checked_row and Checked_col then
        cnv.Brush.Color := grd.Color
    else
        cnv.Brush.Color := clBtnFace;

    if FErrors.ContainsKey(pvkk(ACol, ARow)) then
    begin
        cnv.Font.Color := clRed;

    end;

    if (place >= 0) and (var_ind >= 0) and (place = FPlace) and
      (var_ind = FVarIndex) then
        cnv.Brush.Color := clMoneyGreen;

    if cnv.TextWidth(s) + 3 > Rect.Width then
        s := cut_str(s, cnv, Rect.Width);

    txt_width := cnv.TextWidth(s);
    txt_height := cnv.TextHeight(s);

    X := Rect.Left + 3;
    // x := Rect.left + round((Rect.Width - txt_width) / 2.0);

    if (ARow > 0) AND (ACol <> 1) then
        X := Rect.Right - 3 - round(txt_width);

    Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    cnv.TextRect(Rect, X, Y, s);

    if (ACol = 0) and (ARow > 0) then
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked_row);

    if (ARow = 0) and (ACol > 0) then
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked_col);

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);

end;

procedure TFormReadVars.StringGrid1KeyPress(Sender: TObject; var Key: Char);
var
    g: TStringGrid;
    ARow, ACol: Integer;
    v: Boolean;
    addr, var_: Integer;
begin
    g := Sender as TStringGrid;
    if (g.row > 0) AND (ord(Key) in [27, 32]) then
    begin
        v := FormChartSeries.SeriesOf(AddrByIndex(col2place(g.Selection.Left)),
          VarByIndex(row2var(g.Selection.Top))).ParentChart <> nil;
        for ARow := g.Selection.Top to g.Selection.Bottom do
            for ACol := g.Selection.Left to g.Selection.Right do
            begin
                addr := AddrByIndex(col2place(ACol));
                var_ := VarByIndex(row2var(ARow));
                if (addr >= 0) ANd (var_ >= 0) then
                    FormChartSeries.SetAddrVarSeries(addr, var_, not v);
            end;

    end;

    if ord(Key) = 24 then
    begin
        v := FormChartSeries.SeriesOf(AddrByIndex(col2place(1)),
          VarByIndex(row2var(1))).ParentChart <> nil;
        for ARow := 1 to g.RowCount - 1 do
            for ACol := 1 to g.ColCount - 1 do
            begin
                addr := AddrByIndex(col2place(ACol));
                var_ := VarByIndex(row2var(ARow));
                if (addr >= 0) ANd (var_ >= 0) then
                    FormChartSeries.SetAddrVarSeries(addr, var_, not v);
            end;
    end;
end;

procedure TFormReadVars.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    ACol, ARow: Integer;
    Rect: TRect;
begin
    if (GetAsyncKeyState(VK_LBUTTON) >= 0) then
        exit;
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if ((ACol = 0) and (ARow = 0)) OR ((ACol <> 0) AND (ARow <> 0)) then
        exit;

    Rect := StringGrid1.CellRect(ACol, ARow);

    if X > (Rect.Right + Rect.Left) div 2 then
        with StringGrid1 do
        begin
            if EditorMode then
                EditorMode := false;

            col := ACol;
            row := ARow;
            Options := Options + [goEditing];
            EditorMode := True;
            with GetInplaceEditor() do
            begin
                SelStart := 0;
                SelLength := Length(Cells[ACol, ARow]);
            end;
            exit;
        end;

    if ACol = 0 then
    begin
        ToggleRowChecked(ARow);
        StringGrid_RedrawRow(StringGrid1, ARow);
    end
    else
    begin
        ToggleColChecked(ACol);
        StringGrid_RedrawCol(StringGrid1, ACol);
    end;
end;

procedure TFormReadVars.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    with Sender as TStringGrid do
    begin
        if not EditorMode then
        begin
            FixedRows := 1;
        end;

    end;

end;

procedure TFormReadVars.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    r: TRect;
    grd: TStringGrid;

begin
    if (ACol = 0) AND (ARow = 0) then
    begin
        CanSelect := false;
        exit;
    end;

    with Sender as TStringGrid do
    begin
        if ARow = 0 then
            FixedRows := 0;
        // When selecting a cell
        if EditorMode then
        begin // It was a cell being edited
            EditorMode := false; // Deactivate the editor
            // Do an extra check if the LastEdited_ACol and LastEdited_ARow are not -1 already.
            // This is to be able to use also the arrow-keys up and down in the Grid.
            if (Last_Edited_Col <> -1) and (Last_Edited_Row <> -1) then
            begin
                StringGrid1SetEditText(grd, Last_Edited_Col, Last_Edited_Row,
                  Cells[Last_Edited_Col, Last_Edited_Row]);

            end;
            // Just make the call
        end;
        // Do whatever else wanted
        Options := Options - [goEditing];

        // if (ARow <> 0)  then
        // FixedRows := 1;

    end;

end;

procedure TFormReadVars.StringGrid1SetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
    s: string;
    v: extended;
    n: Integer;
begin

    With StringGrid1 do
    begin
        // Fired on every change
        if Not EditorMode // goEditing must be 'True' in Options
        then
        begin // Only after user ends editing the cell
            Last_Edited_Col := -1; // Indicate no cell is edited
            Last_Edited_Row := -1; // Indicate no cell is edited
            // Do whatever wanted after user has finish editing a cell
            if ARow = 0 then
            begin

                if TryStrToInt(Value, n) and (n > 0) and (n < 256) then
                    TConfigSvc.SetAddr(col2place(ACol), n)
                else
                    UpdateNetwork;
            end;

            if ACol = 0 then
            begin
                if TryStrToInt(Value, n) and (n > -1) then
                    TConfigSvc.SetVar(row2var(ARow), n)
                else
                    UpdateNetwork;
            end;
        end
        else
        begin // The cell is being editted
            Last_Edited_Col := ACol; // Remember column of cell being edited
            Last_Edited_Row := ARow; // Remember row of cell being edited
        end;
    end;

end;

procedure TFormReadVars.SetNetwork(ANetwork: TConfigNetwork);
var
    ACol, ARow, place, varInd: Integer;
begin

    FNetwork := ANetwork;
    FErrors.Clear;
    StringGrid_Clear(StringGrid1);
    with StringGrid1 do
    begin
        EditorMode := false;
        Options := Options - [goEditing];
        RowCount := Length(FNetwork.Vars) + 1;
        // FixedRows := 1;
        ColCount := Length(FNetwork.Places) + 1;
        Cells[0, 0] := '№';
        Cells[1, 0] := 'Параметр';
        for varInd := 0 to Length(FNetwork.Vars) - 1 do
        begin
            ARow := var2row(varInd);
            Cells[0, ARow] := inttostr(FNetwork.Vars[varInd].Code);
        end;

        for place := 0 to Length(FNetwork.Places) - 1 do
        begin
            ACol := place2col(place);
            Cells[ACol, 0] := inttostr(FNetwork.Places[place].Addr);
        end;
    end;
    StringGrid_Redraw(StringGrid1);
    FInitialized := True;
end;

procedure TFormReadVars.reset;
var
    prev_place, prev_var: Integer;
begin
    prev_place := FPlace;
    prev_var := FVarIndex;
    FPlace := -1;
    FVarIndex := -1;
    if (prev_place >= 0) and (prev_var >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(prev_place),
          var2row(prev_var));

    if (FPlace >= 0) and (FVarIndex >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(FPlace),
          var2row(FVarIndex));

end;

procedure TFormReadVars.HandleReadVar(X: TReadVar);
var
    prev_place, prev_var: Integer;
    prev_place_err: Boolean;
    what:string;
begin
    if not FInitialized then
        exit;
    prev_place := FPlace;
    prev_var := FVarIndex;
    FPlace := X.Place;
    FVarIndex := X.VarIndex;
    what := Format('$%s.%s', [ inttohex(x.Addr),X.VarName ]);
    if X.Error <> '' then
    begin
        FErrors.AddOrSetValue(pvk(FPlace, FVarIndex), X.Error);
        StringGrid1.Cells[place2col(FPlace), var2row(FVarIndex)] := X.Error;
        AnbusMainForm.SetStatusText(false, x.Error + ': '+what);
    end
    else
    begin
        FErrors.Remove(pvk(FPlace, FVarIndex));
        StringGrid1.Cells[place2col(FPlace), var2row(FVarIndex)] :=
          floattostr(X.Value);
        FormCharts.AddValue(AddrByIndex(X.Place), VarByIndex(X.VarIndex),
          X.Value, now);
        AnbusMainForm.SetStatusText(true, what + '='+floattostr(x.Value));
    end;

    if (prev_place >= 0) and (prev_var >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(prev_place),
          var2row(prev_var));

    if (FPlace >= 0) and (FVarIndex >= 0) then
        StringGrid_RedrawCell(StringGrid1, place2col(FPlace),
          var2row(FVarIndex));

    prev_place_err := FErrors.ContainsKey(plk(FPlace));

    if Pos('нет ответа', LowerCase(X.Error)) > 0 then
        FErrors.AddOrSetValue(plk(FPlace), X.Error)
    else
        FErrors.Remove(plk(FPlace));
    if prev_place_err <> FErrors.ContainsKey(plk(FPlace)) then
        StringGrid_RedrawCell(StringGrid1, place2col(FPlace), 0);

end;

procedure TFormReadVars.ToggleRowChecked(row: Integer);
begin
    FNetwork.Vars[row2var(row)].Check := not FNetwork.Vars[row2var(row)].Check;
    StringGrid_RedrawRow(StringGrid1, row);
    TConfigSvc.ToggleVar(row2var(row));
end;

procedure TFormReadVars.ToggleColChecked(col: Integer);
begin
    FNetwork.Places[col2place(col)].Check := not FNetwork.Places
      [col2place(col)].Check;
    StringGrid_RedrawCol(StringGrid1, col);
    TconfigSvc.TogglePlace(col2place(col));
end;

function TFormReadVars.FormatAddrPlace(place, varindex: Integer): string;
var
    cl, ro: Integer;
    s1, s2: string;
begin
    cl := place2col(place);
    ro := var2row(varindex);
    if (cl > -1) AND (cl < StringGrid1.ColCount) then
        s1 := StringGrid1.Cells[place2col(place), 0]
    else
        s1 := Format('№%d', [place + 1]);
    if (ro > -1) AND (ro < StringGrid1.RowCount) then
        s2 := StringGrid1.Cells[0, var2row(varindex)]
    else
        s2 := Format('№%d', [varindex + 1]);
    Result := s1 + ': ' + s2;
end;

function TFormReadVars.AddrByIndex(place: Integer): Integer;
var
    cl: Integer;
begin
    cl := place2col(place);
    if (cl > 0) AND (cl < StringGrid1.ColCount) then
        if TryStrToInt(StringGrid1.Cells[cl, 0], Result) then
            exit(Result);
    exit(-1);
end;

function TFormReadVars.VarByIndex(varindex: Integer): Integer;
var
    ro: Integer;
begin
    ro := var2row(varindex);
    if (ro > 0) AND (ro < StringGrid1.RowCount) then
        if TryStrToInt(StringGrid1.Cells[0, ro], Result) then
            exit(Result);
    exit(-1);

end;

procedure TFormReadVars.UpdateNetwork;
begin
    SetNetwork(TConfigSvc.Network);
end;

end.
