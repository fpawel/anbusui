unit stringutils;

interface

uses Vcl.Graphics, Winapi.Windows;

function str_validate_decimal_separator(s: string): string;
function str_to_float(s: string): Double;
function str_replace_unicode_chars(s: string): string;
function inttostr2(n: integer): string;
function cut_str(s:string;c:TCanvas; w:integer):string;

function StrFromCopydata(cd: PCOPYDATASTRUCT):string;

function month_name(month_number:integer):string;

implementation


uses System.SysUtils, System.DateUtils;

function month_name(month_number:integer):string;
begin
    Result := FormatDateTime('mmmm', EncodeDateTime(2000, month_number, 1, 0, 0, 0, 0));
end;

function StrFromCopydata(cd: PCOPYDATASTRUCT):string;
begin
    SetString(result, PWideChar(cd.lpData), cd.cbData div 2);
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
    exit( s);


end;

function str_to_float(s: string): Double;
begin
    exit( StrToFloat(str_validate_decimal_separator(s)) );
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


function cut_str(s:string;c:TCanvas; w:integer):string;
 var i, w1 :integer;
    s1:string;
begin
    if w > c.TextWidth( s ) then
        exit(s);
    result := s;
    w1 := c.TextWidth( '...' );
    result := '';
    for I := 1 to Length(s) do
    begin
        s1 := result + s[i];
        if c.TextWidth(s1)+w1+3 > w then
        	break;
        result := s1;
    end;
    result := result + '...';
end;

end.
