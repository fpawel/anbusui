unit comport;

interface

uses classes, sysutils, hardware_errors;

type

    EComportError = class(EHardwareError);

    TConfigGetResponse = record
        TimeoutMillis, ByteTimeoutMillis, MaxAttemptCount: integer;
        constructor Create(ATimeoutMillis, AByteTimeoutMillis, AMaxAttemptCount
          : integer);
    end;

    TBytes = TArray<byte>;

    TParseResponse = reference to procedure(request: TBytes);

    TBackgroundWork = reference to procedure;

    TComportWorker = record
        HComport: THandle;
        Config: TConfigGetResponse;
        BackgroundWork: TBackgroundWork;
        constructor Create(AComport: THandle; AConfig: TConfigGetResponse;
          ABackgroundWork: TBackgroundWork);
    end;


    TComportLogEntry = record
        HComport: THandle;
        Request,Response: TBytes;
        Millis: integer;
        Attempt : integer;
    end;

    TComportLogHook = reference to procedure (_:TComportLogEntry);

procedure SetcomportLogHook(logHook:TComportLogHook);

procedure EnumComports(const Ports: TStrings);

// EComportError
procedure WriteComport(HComport: THandle; bytes: TBytes);

// EComportError
function OpenComport(portName: string; baud: integer): THandle;

// EBadResponse
// EDeadlineExceeded
// EComportError
function GetResponse(request: TBytes; w: TComportWorker;
  ParseResponse: TParseResponse): TBytes;

implementation

uses registry, windows, stringutils;

var _logHook : TComportLogHook;

procedure SetcomportLogHook(logHook:TComportLogHook);
begin
    _logHook := logHook;
end;

constructor TComportWorker.Create(AComport: THandle; AConfig: TConfigGetResponse;
  ABackgroundWork: TBackgroundWork);
begin
    HComport := AComport;
    Config := AConfig;
    BackgroundWork := ABackgroundWork;
end;

constructor TConfigGetResponse.Create(ATimeoutMillis, AByteTimeoutMillis,
  AMaxAttemptCount: integer);
begin
    TimeoutMillis := ATimeoutMillis;
    ByteTimeoutMillis := AByteTimeoutMillis;
    MaxAttemptCount := AMaxAttemptCount;

end;

function NewDCB(baud: integer): TDCB;
const
    dcb_Binary = $00000001;
begin
    ZeroMemory(@result, sizeof(result));
    with result do
    begin
        DCBlength := sizeof(result);
        BaudRate := baud;
        ByteSize := 8;
        Flags := dcb_Binary;
        Parity := NoParity;
        StopBits := OneStopBit;
        wReserved := 0;
    end;
end;

function portNameAvail(portName: string): bool;
var
    Ports: TStringList;
begin
    Ports := TStringList.Create;
    try
        EnumComports(Ports);
        result := Ports.IndexOf(portName) > -1;
    finally
        Ports.Free;
    end;
end;

function setupComport(HComport: THandle; baud: integer): boolean;
var
    dcb: TDCB;
    commTimeouts: TCommTimeouts;
begin
    dcb := NewDCB(baud);
    ZeroMemory(@commTimeouts, sizeof(commTimeouts));
    commTimeouts.ReadIntervalTimeout := MAXDWORD;
    result := SetCommState(HComport, dcb) and
      SetCommTimeouts(HComport, commTimeouts) and
      SetupComm(HComport, 100000, 100000);
end;

function doOpenComport(portName: string; baud: integer): THandle;
begin
    if not portNameAvail(portName) then
        raise EComportError.Create('нет COM порта с таким именем');

    portName := '\\.\' + portName;

    result := CreateFile(PChar(portName), GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, 0, 0);

    if result = INVALID_HANDLE_VALUE then
        raise EComportError.Create(SysErrorMessage(GetLastError));

    if not setupComport(result, baud) then
    begin
        CloseHandle(result);
        raise EComportError.Create(SysErrorMessage(GetLastError));
    end;
end;

function OpenComport(portName: string; baud: integer): THandle;
begin
    try
        result := doOpenComport(portName, baud);
    except
        on e:Exception do
        begin
            CloseHandle(result);
            e.Message := portName + ': ' + e.Message;
            raise;
        end;

    end;
end;

procedure WriteComport(HComport: THandle; bytes: TBytes);
var
    writen_count: cardinal;
begin
    if not PurgeComm(HComport, PURGE_TXABORT or PURGE_RXABORT or
      PURGE_TXCLEAR or PURGE_RXCLEAR) then
        raise EComportError.Create(SysErrorMessage(GetLastError));

    if not WriteFile(HComport, bytes[0], length(bytes), writen_count, nil) then
        raise EComportError.Create(SysErrorMessage(GetLastError));

    if writen_count <> length(bytes) then
        raise EComportError.Create(format('передано %d байт из %d',
          [writen_count, length(bytes)]));
end;

procedure EnumComports(const Ports: TStrings);
var
    nInd: integer;
begin
    with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKey('hardware\devicemap\serialcomm', false) then
            begin
                Ports.BeginUpdate();
                GetValueNames(Ports);
                for nInd := Ports.count - 1 downto 0 do
                    Ports.Strings[nInd] := ReadString(Ports.Strings[nInd]);
            end;

        finally
            Ports.EndUpdate();
            CloseKey();
            Free();
        end
end;

function since_tickcount(t: longint): integer;
begin
    exit(longint(GetTickCount) - t);
end;

function _ReadResponse(w: TComportWorker): TBytes;
var
    t: longint;
    commErrors: DWORD;
    commStat: TComstat;
    n, readedCount: DWORD;
begin
    SetLength(result, 0);
    t := GetTickCount();
    while (since_tickcount(t) < w.Config.TimeoutMillis) do
    begin

        w.BackgroundWork();
        Sleep(5);

        if not ClearCommError(w.HComport, commErrors, @commStat) then
            raise EComportError.Create(SysErrorMessage(GetLastError));

        if commStat.cbInQue = 0 then
            Continue;

        n := length(result);
        SetLength(result, n + commStat.cbInQue);
        if not ReadFile(w.HComport, result[n], commStat.cbInQue, &readedCount,
          nil) then
            raise EComportError.Create(SysErrorMessage(GetLastError));

        t := GetTickCount();
        while (since_tickcount(t) < w.Config.ByteTimeoutMillis) do
        begin

            w.BackgroundWork();
            Sleep(5);

            if not ClearCommError(w.HComport, commErrors, @commStat) then
                raise EComportError.Create(SysErrorMessage(GetLastError));

            if commStat.cbInQue = 0 then
                Continue;

            n := length(result);
            SetLength(result, n + commStat.cbInQue);

            if not ReadFile(w.HComport, result[n], commStat.cbInQue,
              &readedCount, nil) then
                raise EComportError.Create(SysErrorMessage(GetLastError));
            t := GetTickCount();
        end;
        break;

    end;
end;

function GetResponse(request: TBytes; w: TComportWorker;
  ParseResponse: TParseResponse): TBytes;
var
    attempt_number: integer;
    t: longint;
    s, err: string;
    logEntry: TComportLogEntry;
begin
    if w.Config.MaxAttemptCount < 1 then
        w.Config.MaxAttemptCount := 1;

    t := GetTickCount;
    for attempt_number := 1 to w.Config.MaxAttemptCount do
    begin
        w.BackgroundWork();

        SetLength(result, 0);

        WriteComport(w.HComport, request);

        try
            result := _ReadResponse(w);
        finally
            if Assigned( _logHook ) then
            begin
                logEntry.HComport := w.HComport;
                logEntry.Request := Request;
                logEntry.Response := result;
                logEntry.Millis := GetTickCount - t;
                logEntry.Attempt := attempt_number;
                _logHook(logEntry);
            end;
        end;


        try
            if length(result) = 0 then
                Continue;
            ParseResponse(result);
            exit;
        except
            on e: EBadResponse do
            begin
                err := e.Message;
                // Sleep(config.ByteTimeoutMillis);
            end;
        end;
    end;
    s := '';
    if length(result) <> 0 then
        s := ' --> ' + BytesToHex(result);
    s := format('%s%s %d мс попытка %d', [BytesToHex(request), s,
      since_tickcount(t), attempt_number - 1]);

    if length(result) = 0 then
        raise EDeadlineExceeded.Create('не отвечает:  ' + s);
    raise EBadResponse.Create(err + ': ' + s);
end;

initialization

_logHook := nil;

end.
