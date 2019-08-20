unit SuperObjectHelp;

interface

uses superobject;

procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: int64); overload;
procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: double); overload;
procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: boolean); overload;
procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: string); overload;

procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: TArray<string>); overload;

  procedure SuperObject_SetField(x: ISuperObject; field: string; v: TArray<Int64>); overload;

procedure SuperObject_Get(x: ISuperObject; var v: int64); overload;
procedure SuperObject_Get(x: ISuperObject; var v: integer); overload;
procedure SuperObject_Get(x: ISuperObject; var v: double); overload;
procedure SuperObject_Get(x: ISuperObject; var v: boolean); overload;
procedure SuperObject_Get(x: ISuperObject; var v: string); overload;


implementation

uses Rest.Json;

procedure SuperObject_SetField(x: ISuperObject; field: string; v: int64);
begin
    x.I[field] := v;
end;

procedure SuperObject_SetField(x: ISuperObject; field: string; v: double);
begin
    x.D[field] := v;
end;

procedure SuperObject_SetField(x: ISuperObject; field: string; v: boolean);
begin
    x.B[field] := v;
end;

procedure SuperObject_SetField(x: ISuperObject; field: string; v: string);
begin
    x.S[field] := v;
end;

procedure SuperObject_Get(x: ISuperObject; var v: int64);
begin
    v := x.AsInteger;
end;

procedure SuperObject_Get(x: ISuperObject; var v: double);
begin
    v := x.AsDouble;
end;

procedure SuperObject_Get(x: ISuperObject; var v: boolean);
begin
    v := x.AsBoolean;
end;

procedure SuperObject_Get(x: ISuperObject; var v: string);
begin
    v := x.AsString;
end;

procedure SuperObject_Get(x: ISuperObject; var v: integer); overload;
begin
    v := x.AsInteger;
end;

procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: TArray<string>);
var
    I: integer;
begin
    x.O[field] := SA([]);
    for I := 0 to length(v) - 1 do
        x.A[field].S[I] := v[I];
end;

procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: TObject); overload;
begin
    x[field] := SO(TJson.ObjectToJsonString(v));
end;

procedure SuperObject_SetField(x: ISuperObject; field: string;
  v: TArray<Int64>); overload;
var
    I: integer;
begin
    x.O[field] := SA([]);
    for I := 0 to length(v) - 1 do
        x.A[field].I[I] := v[I];
end;




end.
