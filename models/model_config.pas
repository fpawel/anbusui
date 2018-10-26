unit model_config;

interface

type


    TPropertyValueType = (VtInt, VtFloat, VtString, VtComportName,
      VtBaud, VtBool);

    TExtremum = class
        FValue : double;
    end;



    TConfigProperty = class
    private
        function GetPropertyValueType: TPropertyValueType;
    public
        FHint: string;
        FValue: string;
        FError: string;
        FName: string;
        FDefaultValue: String;
        FMin: TExtremum;
        FMax: TExtremum;
        FValueType: integer;
        FList: TArray<string>;
        procedure SetStr(str: string);
        function GetHasError:boolean;

        property ValueType : TPropertyValueType read GetPropertyValueType;
        property HasError : boolean read GetHasError;
    end;

    TConfigSection = class
    public
        FName: string;
        FHint: string;
        FSortOrder: integer;
        FProperties: TArray<TConfigProperty>;
        function HasError: boolean;
    end;

    RConfigData = record
       Prop : TConfigProperty;
       Sect : TConfigSection;
       function GetHasError:boolean;
       property HasError : boolean read GetHasError;
    end;
    PConfigData = ^RConfigData;

    TConfig = class
    public
        FSections : TArray<TConfigSection>;
    end;

    TChangedPropertyValue = class
    public
        FValue: string;
        FName: string;
        FSection: string;
        constructor Create(p:PConfigData);
    end;

implementation

uses stringutils, sysutils;

function TConfigProperty.GetHasError:boolean;
begin
    result := FError <> '';
end;

constructor TChangedPropertyValue.Create(p:PConfigData);
begin
    inherited Create;
    FValue := p.Prop.FValue;
    FName := p.Prop.FName;
    FSection := p.Sect.FName;
end;

function RConfigData.GetHasError:boolean;
begin
    if not Assigned(prop) then
        exit(sect.HasError) ;
    exit (prop.HasError);
end;

function TConfigSection.HasError: boolean;
var
    i: integer;
begin
    for i := 0 to length(self.FProperties) - 1 do
        if FProperties[i].HasError then
            exit(true);
    exit(false);
end;

function TConfigProperty.GetPropertyValueType: TPropertyValueType;
begin
    exit(TPropertyValueType(FValueType));
end;

procedure TConfigProperty.SetStr(str: string);
var
    v: double;
    i, vInt: integer;
    ok: boolean;
begin
    FError := '';
    str := str_validate_decimal_separator(str).Trim;
    FValue := str;
    if str = '' then
    begin
        FError := 'нет значения';
        exit;
    end;

    if length(FList) > 0 then
    begin
        ok := false;
        for i := 0 to length(FList) - 1 do
        begin
            if FList[i] = str then
                ok := true;
        end;
        if not ok then
        begin
            FError := 'значение должно быть из списка: ' + FList[0];
            for i := 1 to length(FList) - 1 do
                FError := FError + '; ' + FList[i];
            exit;
        end;
    end;

    ok := true;
    if (ValueType = VtInt) or (ValueType = VtBaud) or (ValueType = VtBool) then
    begin
        ok := TryStrToInt(str, vInt);
        v := vInt;
        if not ok then
            FError := 'не правильный синтаксис целого числа';
    end
    else if ValueType = VtFloat then
    begin
        ok := TryStrToFloat(str, v);
        if not ok then
            FError := 'не правильный синтаксис числа c плавающей точкой';
    end;

    if ok then
    begin
        if  Assigned(FMin) and (v < FMin.FValue) then
            FError := 'меньше ' + floattostr(FMin.FValue)
        else if Assigned(FMax) and (v > FMax.FValue) then
            FError := 'больше ' + floattostr(FMax.FValue);
    end;

end;

end.
