
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TPlace = record
    public
        Addr : Byte;
        Check : Boolean;
        
    end;
    
    TDevVar = record
    public
        Code : Word;
        Check : Boolean;
        
    end;
    
    TConfigNetwork = record
    public
        Places : TArray<TPlace>;
        Vars : TArray<TDevVar>;
        
    end;
    
    TVar = record
    public
        Code : Word;
        Name : string;
        
    end;
    
    TChartsBucket = record
    public
        Hour : Integer;
        Minute : Integer;
        BucketID : Int64;
        Name : string;
        IsLast : Boolean;
        Day : Integer;
        
    end;
    
    TTimeDelphi = record
    public
        Month : Integer;
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        Second : Integer;
        Millisecond : Integer;
        Year : Integer;
        
    end;
    
    TYearMonth = record
    public
        Year : Integer;
        Month : Integer;
        
    end;
    
    TReadVar = record
    public
        Value : Double;
        Error : string;
        Place : Integer;
        VarIndex : Integer;
        VarCode : Word;
        Addr : Byte;
        VarName : string;
        
    end;
    

implementation

end.