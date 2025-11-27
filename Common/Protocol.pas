unit Protocol;

interface

uses
  System.SysUtils, System.Classes;

const
  // Comandos do protocolo
  CMD_CLIENT_INFO = $01;      // Cliente envia informações
  CMD_PING = $02;             // Ping/Pong
  CMD_PONG = $03;             // Resposta ao ping
  CMD_SCREEN_START = $04;     // Iniciar captura de tela
  CMD_SCREEN_STOP = $05;      // Parar captura de tela
  CMD_SCREEN_DATA = $06;      // Dados da tela
  CMD_MOUSE_MOVE = $07;       // Mover mouse
  CMD_MOUSE_CLICK = $08;      // Click mouse
  CMD_KEYBOARD = $09;         // Tecla
  CMD_LOCK_SCREEN = $0A;      // Travar tela
  CMD_UNLOCK_SCREEN = $0B;    // Destravar tela
  CMD_DISCONNECT = $FF;       // Desconectar

type
  TPacketHeader = packed record
    Command: Byte;
    DataSize: Cardinal;
  end;

  TClientInfo = record
    ComputerName: string;
    IPAddress: string;
    MACAddress: string;
    AntiVirus: string;
    OSVersion: string;
  end;

  TMouseCommand = packed record
    X: Integer;
    Y: Integer;
    Button: Byte; // 0=move, 1=left, 2=right, 3=middle
  end;

  TKeyboardCommand = packed record
    VirtualKey: Word;
    KeyDown: Boolean;
  end;

function CreatePacket(Command: Byte; const Data: TBytes): TBytes;
function ParsePacket(const Buffer: TBytes; out Command: Byte; out Data: TBytes): Boolean;
function ClientInfoToBytes(const Info: TClientInfo): TBytes;
function BytesToClientInfo(const Data: TBytes): TClientInfo;

implementation

function CreatePacket(Command: Byte; const Data: TBytes): TBytes;
var
  Header: TPacketHeader;
  DataLen: Cardinal;
begin
  DataLen := Length(Data);
  Header.Command := Command;
  Header.DataSize := DataLen;

  SetLength(Result, SizeOf(TPacketHeader) + DataLen);
  Move(Header, Result[0], SizeOf(TPacketHeader));
  if DataLen > 0 then
    Move(Data[0], Result[SizeOf(TPacketHeader)], DataLen);
end;

function ParsePacket(const Buffer: TBytes; out Command: Byte; out Data: TBytes): Boolean;
var
  Header: TPacketHeader;
begin
  Result := False;
  if Length(Buffer) < SizeOf(TPacketHeader) then
    Exit;

  Move(Buffer[0], Header, SizeOf(TPacketHeader));
  Command := Header.Command;

  if Length(Buffer) < SizeOf(TPacketHeader) + Integer(Header.DataSize) then
    Exit;

  SetLength(Data, Header.DataSize);
  if Header.DataSize > 0 then
    Move(Buffer[SizeOf(TPacketHeader)], Data[0], Header.DataSize);

  Result := True;
end;

function ClientInfoToBytes(const Info: TClientInfo): TBytes;
var
  Stream: TMemoryStream;
  Len: Integer;
  Bytes: TBytes;
begin
  Stream := TMemoryStream.Create;
  try
    // ComputerName
    Bytes := TEncoding.UTF8.GetBytes(Info.ComputerName);
    Len := Length(Bytes);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Bytes[0], Len);

    // IPAddress
    Bytes := TEncoding.UTF8.GetBytes(Info.IPAddress);
    Len := Length(Bytes);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Bytes[0], Len);

    // MACAddress
    Bytes := TEncoding.UTF8.GetBytes(Info.MACAddress);
    Len := Length(Bytes);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Bytes[0], Len);

    // AntiVirus
    Bytes := TEncoding.UTF8.GetBytes(Info.AntiVirus);
    Len := Length(Bytes);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Bytes[0], Len);

    // OSVersion
    Bytes := TEncoding.UTF8.GetBytes(Info.OSVersion);
    Len := Length(Bytes);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Bytes[0], Len);

    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Result[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function BytesToClientInfo(const Data: TBytes): TClientInfo;
var
  Stream: TMemoryStream;
  Len: Integer;
  Bytes: TBytes;
begin
  Result.ComputerName := '';
  Result.IPAddress := '';
  Result.MACAddress := '';
  Result.AntiVirus := '';
  Result.OSVersion := '';

  Stream := TMemoryStream.Create;
  try
    Stream.Write(Data[0], Length(Data));
    Stream.Position := 0;

    // ComputerName
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      SetLength(Bytes, Len);
      Stream.Read(Bytes[0], Len);
      Result.ComputerName := TEncoding.UTF8.GetString(Bytes);
    end;

    // IPAddress
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      SetLength(Bytes, Len);
      Stream.Read(Bytes[0], Len);
      Result.IPAddress := TEncoding.UTF8.GetString(Bytes);
    end;

    // MACAddress
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      SetLength(Bytes, Len);
      Stream.Read(Bytes[0], Len);
      Result.MACAddress := TEncoding.UTF8.GetString(Bytes);
    end;

    // AntiVirus
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      SetLength(Bytes, Len);
      Stream.Read(Bytes[0], Len);
      Result.AntiVirus := TEncoding.UTF8.GetString(Bytes);
    end;

    // OSVersion
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      SetLength(Bytes, Len);
      Stream.Read(Bytes[0], Len);
      Result.OSVersion := TEncoding.UTF8.GetString(Bytes);
    end;
  finally
    Stream.Free;
  end;
end;

end.
