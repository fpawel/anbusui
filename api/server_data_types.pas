
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
        BucketID : Int64;
        Name : string;
        Last : Boolean;
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        
    end;
    
    TTimeDelphi = record
    public
        Year : Integer;
        Month : Integer;
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        Second : Integer;
        Millisecond : Integer;
        
    end;
    
    TYearMonth = record
    public
        Year : Integer;
        Month : Integer;
        
    end;
    
    TReadVar = record
    public
        Place : Integer;
        VarIndex : Integer;
        Value : Double;
        Error : string;
        
    end;
    

implementation

end.