unit stringutils;

interface

uses Vcl.Graphics;

function str_validate_decimal_separator(s: string): string;
function str_to_float(s: string): Double;
function str_replace_unicode_chars(s: string): string;
function inttostr2(n: integer): string;
function cut_str(s: string; c: TCanvas; w: integer): string;
function month_name(month_number: integer): string;
function try_str_to_float(s: string; var v: Double): boolean;

function BytesToHex(BA: TArray<byte>; Sep: string = ' ';
  index_from: integer = -1; index_to: integer = -1): string;

implementation

uses SysUtils, dateutils;

function BytesToHex(BA: TArray<byte>; Sep: string; index_from: integer;
  index_to: integer): string;
var
    i, k: integer;
begin
    result := '';

    if index_from = -1 then
        index_from := low(BA);
    if index_to = -1 then
        index_to := high(BA);

    if Sep = '' then
    begin
        for i := index_from to index_to do
            result := result + IntToHex(BA[i], 2);
    end
    else
    begin
        k := index_to;
        for i := index_from to k do
        begin
            result := result + IntToHex(BA[i], 2);
            if k <> i then
                result := result + Sep;
        end;
    end;
end;

function month_name(month_number: integer): string;
begin
    result := FormatDateTime('mmmm', EncodeDateTime(2000, month_number, 1, 0,
      0, 0, 0));
end;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
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

function try_str_to_float(s: string; var v: Double): boolean;
begin
    result := TryStrToFloat(str_validate_decimal_separator(s), v);

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
    result := s;
    w1 := c.TextWidth('...');
    result := '';
    for i := 1 to length(s) do
    begin
        s1 := result + s[i];
        if c.TextWidth(s1) + w1 + 3 > w then
            break;
        result := s1;
    end;
    result := result + '...';
end;

end.
