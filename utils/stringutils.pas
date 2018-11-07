unit stringutils;

interface

uses Vcl.Graphics, Winapi.Windows, system.StrUtils;

function str_validate_decimal_separator(s: string): string;
function str_to_float(s: string): Double;
function float_to_str(v: Double ): string;
function str_replace_unicode_chars(s: string): string;
function inttostr2(n: integer): string;
function inttostr3(n: integer): string;
function cut_str(s: string; c: TCanvas; w: integer): string;

function StrFromCopydata(cd: PCOPYDATASTRUCT): string;

function month_name(month_number: integer): string;

function MyBoolToStr(v: boolean): string;
function MyStrToBool(v: string): boolean;


implementation

uses System.SysUtils, System.DateUtils;

function float_to_str(v: Double ): string;
begin
    result := StringReplace(FloatToStr(v), ',', '.', [rfReplaceAll]);
end;



function month_name(month_number: integer): string;
begin
    Result := FormatDateTime('mmmm', EncodeDateTime(2000, month_number, 1, 0,
      0, 0, 0));
end;

function StrFromCopydata(cd: PCOPYDATASTRUCT): string;
begin
    SetString(Result, PWideChar(cd.lpData), cd.cbData div 2);
end;

function inttostr2(n: integer): string;
begin
    Result := inttostr(n);
    if n < 10 then
        Result := '0' + Result;
end;

function inttostr3(n: integer): string;
begin
    Result := inttostr(n);
    if n < 10 then
        Result := '00' + Result
    else if n < 99 then
        Result := '0' + Result;
end;

function str_replace_unicode_chars(s: string): string;
begin
    s := StringReplace(s, '₄', '_4_', [rfReplaceAll]);
    s := StringReplace(s, '₂', '_2_', [rfReplaceAll]);
    s := StringReplace(s, '₃', '_3_', [rfReplaceAll]);
    s := StringReplace(s, '₈', '_8_', [rfReplaceAll]);
    s := StringReplace(s, '∑', '_sum_', [rfReplaceAll]);
    exit(s);

end;

function str_to_float(s: string): Double;
begin
    exit(StrToFloat(str_validate_decimal_separator(s)));
end;

function str_validate_decimal_separator(s: string): string;
var
    i: integer;
begin
    for i := 1 to length(s) do
        if (s[i] = '.') or (s[i] = ',') then
            s[i] := FormatSettings.DecimalSeparator;
    exit(s);
end;

function cut_str(s: string; c: TCanvas; w: integer): string;
var
    i, w1: integer;
    s1: string;
begin
    if w > c.TextWidth(s) then
        exit(s);
    Result := s;
    w1 := c.TextWidth('...');
    Result := '';
    for i := 1 to length(s) do
    begin
        s1 := Result + s[i];
        if c.TextWidth(s1) + w1 + 3 > w then
            break;
        Result := s1;
    end;
    Result := Result + '...';
end;

function MyBoolToStr(v: boolean): string;
begin
    if v then
        exit('true');
    exit('false');
end;

function MyStrToBool(v: string): boolean;
begin
    if LowerCase(v) = 'true' then
        exit(true);
    if LowerCase(v) = 'false' then
        exit(false);
    raise Exception.Create('not bool string: ' + v);
end;

end.
