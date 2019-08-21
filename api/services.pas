
unit services;

interface

uses superobject, server_data_types;

type 
    TConfigSvc = class
    public
        class function AddPlace:TConfigNetwork;static;
        class function AddVar:TConfigNetwork;static;
        class function ComportName:string;static;
        class function DelPlace:TConfigNetwork;static;
        class function DelVar:TConfigNetwork;static;
        class function GetEditConfig:string;static;
        class function Network:TConfigNetwork;static;
        class procedure SetAddr(Place:Integer; Addr:Byte);static;
        class procedure SetComportName(param1:string);static;
        class function SetEditConfig(param1:string):string;static;
        class procedure SetVar(VarIndex:Integer; VarCode:Word);static;
        class function ToggleNetwork:TConfigNetwork;static;
        class procedure TogglePlace(param1:Integer);static;
        class procedure ToggleVar(param1:Integer);static;
        class function Vars:TArray<TVar>;static;
         
    end;

    TChartsSvc = class
    public
        class function BucketsOfYearMonth(Year:Integer; Month:Integer):TArray<TChartsBucket>;static;
        class function DeletePoints(BucketID:Int64; Addr:Byte; VarCode:Word; ValueMinimum:Double; ValueMaximum:Double; TimeMinimum:TTimeDelphi; TimeMaximum:TTimeDelphi):Int64;static;
        class function YearsMonths:TArray<TYearMonth>;static;
         
    end;

    TTaskSvc = class
    public
        class procedure Send(Addr:Integer; Cmd:Byte; Bytes:string);static;
        class procedure Write32(Addr:Integer; Cmd:Word; Value:Double);static;
         
    end;

    

implementation 

uses HttpRpcClient, SuperObjectHelp, Grijjy.Bson.Serialization;

 
class function TConfigSvc.AddPlace:TConfigNetwork;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.AddPlace', req, Result); 
end;


class function TConfigSvc.AddVar:TConfigNetwork;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.AddVar', req, Result); 
end;


class function TConfigSvc.ComportName:string;
var
    req : ISuperobject;
begin
    req := SO;

    

    SuperObject_Get(ThttpRpcClient.GetResponse('ConfigSvc.ComportName', req), Result); 
end;


class function TConfigSvc.DelPlace:TConfigNetwork;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.DelPlace', req, Result); 
end;


class function TConfigSvc.DelVar:TConfigNetwork;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.DelVar', req, Result); 
end;


class function TConfigSvc.GetEditConfig:string;
var
    req : ISuperobject;
begin
    req := SO;

    

    SuperObject_Get(ThttpRpcClient.GetResponse('ConfigSvc.GetEditConfig', req), Result); 
end;


class function TConfigSvc.Network:TConfigNetwork;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.Network', req, Result); 
end;


class procedure TConfigSvc.SetAddr(Place:Integer; Addr:Byte);
var
    req : ISuperobject;s:string;
begin
    req := SO;

    SuperObject_SetField(req, 'Place', Place); 
    SuperObject_SetField(req, 'Addr', Addr); 
    

    ThttpRpcClient.GetResponse('ConfigSvc.SetAddr', req); 
end;


class procedure TConfigSvc.SetComportName(param1:string);
var
    req : ISuperobject;
begin
    req := SA([]);

    req.AsArray.Add(param1); 
    

    ThttpRpcClient.GetResponse('ConfigSvc.SetComportName', req); 
end;


class function TConfigSvc.SetEditConfig(param1:string):string;
var
    req : ISuperobject;
begin
    req := SA([]);

    req.AsArray.Add(param1); 
    

    SuperObject_Get(ThttpRpcClient.GetResponse('ConfigSvc.SetEditConfig', req), Result); 
end;


class procedure TConfigSvc.SetVar(VarIndex:Integer; VarCode:Word);
var
    req : ISuperobject;s:string;
begin
    req := SO;

    SuperObject_SetField(req, 'VarIndex', VarIndex); 
    SuperObject_SetField(req, 'VarCode', VarCode); 
    

    ThttpRpcClient.GetResponse('ConfigSvc.SetVar', req); 
end;


class function TConfigSvc.ToggleNetwork:TConfigNetwork;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.ToggleNetwork', req, Result); 
end;


class procedure TConfigSvc.TogglePlace(param1:Integer);
var
    req : ISuperobject;
begin
    req := SA([]);

    req.AsArray.Add(param1); 
    

    ThttpRpcClient.GetResponse('ConfigSvc.TogglePlace', req); 
end;


class procedure TConfigSvc.ToggleVar(param1:Integer);
var
    req : ISuperobject;
begin
    req := SA([]);

    req.AsArray.Add(param1); 
    

    ThttpRpcClient.GetResponse('ConfigSvc.ToggleVar', req); 
end;


class function TConfigSvc.Vars:TArray<TVar>;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ConfigSvc.Vars', req, Result); 
end;

 
class function TChartsSvc.BucketsOfYearMonth(Year:Integer; Month:Integer):TArray<TChartsBucket>;
var
    req : ISuperobject;s:string;
begin
    req := SO;

    SuperObject_SetField(req, 'Year', Year); 
    SuperObject_SetField(req, 'Month', Month); 
    

    ThttpRpcClient.Call('ChartsSvc.BucketsOfYearMonth', req, Result); 
end;


class function TChartsSvc.DeletePoints(BucketID:Int64; Addr:Byte; VarCode:Word; ValueMinimum:Double; ValueMaximum:Double; TimeMinimum:TTimeDelphi; TimeMaximum:TTimeDelphi):Int64;
var
    req : ISuperobject;s:string;
begin
    req := SO;

    SuperObject_SetField(req, 'BucketID', BucketID); 
    SuperObject_SetField(req, 'Addr', Addr); 
    SuperObject_SetField(req, 'VarCode', VarCode); 
    SuperObject_SetField(req, 'ValueMinimum', ValueMinimum); 
    SuperObject_SetField(req, 'ValueMaximum', ValueMaximum); 
    TgoBsonSerializer.serialize(TimeMinimum, s); req['TimeMinimum'] := SO(s); 
    TgoBsonSerializer.serialize(TimeMaximum, s); req['TimeMaximum'] := SO(s); 
    

    SuperObject_Get(ThttpRpcClient.GetResponse('ChartsSvc.DeletePoints', req), Result); 
end;


class function TChartsSvc.YearsMonths:TArray<TYearMonth>;
var
    req : ISuperobject;
begin
    req := SO;

    

    ThttpRpcClient.Call('ChartsSvc.YearsMonths', req, Result); 
end;

 
class procedure TTaskSvc.Send(Addr:Integer; Cmd:Byte; Bytes:string);
var
    req : ISuperobject;s:string;
begin
    req := SO;

    SuperObject_SetField(req, 'Addr', Addr); 
    SuperObject_SetField(req, 'Cmd', Cmd); 
    SuperObject_SetField(req, 'Bytes', Bytes); 
    

    ThttpRpcClient.GetResponse('TaskSvc.Send', req); 
end;


class procedure TTaskSvc.Write32(Addr:Integer; Cmd:Word; Value:Double);
var
    req : ISuperobject;s:string;
begin
    req := SO;

    SuperObject_SetField(req, 'Addr', Addr); 
    SuperObject_SetField(req, 'Cmd', Cmd); 
    SuperObject_SetField(req, 'Value', Value); 
    

    ThttpRpcClient.GetResponse('TaskSvc.Write32', req); 
end;

 
end.