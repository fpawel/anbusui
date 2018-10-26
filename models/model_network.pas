unit model_network;

interface

type TNetworkPlace = class
    FAddr : integer;
    FUnchecked : boolean;
end;

type TDeviceVar = class
    FVar : integer;
    FUnchecked : boolean;
end;

type TNetwork = class
    FVars : TArray<TDeviceVar>;
    FPlaces : TArray<TNetworkPlace>;
end;


type TReadVar = class
    FPlace : integer;
    FVarIndex : integer;
    FValue: double;
    FError: string;
end;

type TPanalibTextMessage = class
    FText: string;
    FOk: boolean;
end;

implementation

end.
