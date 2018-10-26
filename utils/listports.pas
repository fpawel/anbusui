unit listports;

interface

uses classes;

procedure EnumComPorts(const Ports: TStrings);
procedure EnumBaudRates(const Items: TStrings);

implementation

uses registry, windows;

procedure EnumBaudRates(const Items: TStrings);
begin
    Items.Add('1200');
                Items.Add('2400');
                Items.Add('4800');
                Items.Add('9600');
                Items.Add('14400');
                Items.Add('19200');
                Items.Add('38400');
                Items.Add('56000');
                Items.Add('57600');
                Items.Add('115200');
                Items.Add('128000');
                Items.Add('256000');
end;

procedure EnumComPorts(const Ports: TStrings);
var
    nInd: Integer;
begin { EnumComPorts }
    with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKey('hardware\devicemap\serialcomm', False) then
                try
                    Ports.BeginUpdate();
                    try
                        GetValueNames(Ports);
                        for nInd := Ports.Count - 1 downto 0 do
                            Ports.Strings[nInd] :=
                              ReadString(Ports.Strings[nInd]);

                    finally
                        Ports.EndUpdate()
                    end { try-finally }
                finally
                    CloseKey()
                end { try-finally }
            else
                Ports.Clear()
        finally
            Free()
        end { try-finally }
end { EnumComPorts };

end.
