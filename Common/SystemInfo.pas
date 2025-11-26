unit SystemInfo;

interface

uses
  Winapi.Windows, Winapi.IpHlpApi, Winapi.IpTypes, System.SysUtils, System.Classes,
  System.Win.Registry, Winapi.ActiveX, System.Win.ComObj, Winapi.Winsock;

function GetComputerName: string;
function GetLocalIPAddress: string;
function GetMACAddress: string;
function GetAntiVirusName: string;
function GetOSVersion: string;

implementation

function GetComputerName: string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  if Winapi.Windows.GetComputerName(Buffer, Size) then
    Result := Buffer
  else
    Result := 'Unknown';
end;

function GetLocalIPAddress: string;
var
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  HostName: array[0..255] of AnsiChar;
  Addr: PAnsiChar;
begin
  Result := '127.0.0.1';
  try
    if WSAStartup($0202, WSAData) = 0 then
    begin
      try
        if gethostname(HostName, SizeOf(HostName)) = 0 then
        begin
          HostEnt := gethostbyname(HostName);
          if HostEnt <> nil then
          begin
            Addr := HostEnt^.h_addr_list^;
            if Addr <> nil then
              Result := Format('%d.%d.%d.%d', [Byte(Addr[0]), Byte(Addr[1]),
                                               Byte(Addr[2]), Byte(Addr[3])]);
          end;
        end;
      finally
        WSACleanup;
      end;
    end;
  except
    Result := '127.0.0.1';
  end;
end;

function GetMACAddress: string;
var
  AdapterInfo: PIP_ADAPTER_INFO;
  BufLen: ULONG;
  Status: DWORD;
  i: Integer;
begin
  Result := '00-00-00-00-00-00';
  BufLen := SizeOf(TIP_ADAPTER_INFO);
  GetMem(AdapterInfo, BufLen);
  try
    Status := GetAdaptersInfo(AdapterInfo, BufLen);
    if Status = ERROR_BUFFER_OVERFLOW then
    begin
      ReallocMem(AdapterInfo, BufLen);
      Status := GetAdaptersInfo(AdapterInfo, BufLen);
    end;

    if Status = ERROR_SUCCESS then
    begin
      Result := '';
      for i := 0 to AdapterInfo^.AddressLength - 1 do
      begin
        if i > 0 then
          Result := Result + '-';
        Result := Result + IntToHex(AdapterInfo^.Address[i], 2);
      end;
    end;
  finally
    FreeMem(AdapterInfo);
  end;
end;

function GetAntiVirusName: string;
var
  Reg: TRegistry;
  KeyNames: TStringList;
  i: Integer;
begin
  Result := 'None';
  Reg := TRegistry.Create(KEY_READ);
  KeyNames := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Verificar Windows Defender
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows Defender') then
    begin
      Result := 'Windows Defender';
      Reg.CloseKey;
      Exit;
    end;

    // Verificar outros antivírus comuns
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall') then
    begin
      Reg.GetKeyNames(KeyNames);
      Reg.CloseKey;

      for i := 0 to KeyNames.Count - 1 do
      begin
        if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + KeyNames[i]) then
        begin
          try
            var DisplayName := Reg.ReadString('DisplayName');
            if (Pos('Avast', DisplayName) > 0) or
               (Pos('AVG', DisplayName) > 0) or
               (Pos('Norton', DisplayName) > 0) or
               (Pos('McAfee', DisplayName) > 0) or
               (Pos('Kaspersky', DisplayName) > 0) or
               (Pos('Bitdefender', DisplayName) > 0) or
               (Pos('ESET', DisplayName) > 0) or
               (Pos('Antivirus', DisplayName) > 0) or
               (Pos('AntiVirus', DisplayName) > 0) then
            begin
              Result := DisplayName;
              Reg.CloseKey;
              Exit;
            end;
          except
            // Ignora chaves sem DisplayName
          end;
          Reg.CloseKey;
        end;
      end;
    end;
  finally
    KeyNames.Free;
    Reg.Free;
  end;
end;

function GetOSVersion: string;
var
  VersionInfo: TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(VersionInfo) then
  begin
    Result := Format('Windows %d.%d Build %d',
      [VersionInfo.dwMajorVersion, VersionInfo.dwMinorVersion, VersionInfo.dwBuildNumber]);
  end
  else
    Result := 'Unknown';
end;

end.
