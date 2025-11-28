unit ClientConnection;

interface

uses
  Winapi.Windows, Winapi.WinSock, System.SysUtils, System.Classes,
  Protocol, DesktopDuplication, Compression;

type
  TConnectedEvent = procedure of object;
  TDisconnectedEvent = procedure of object;
  TCommandEvent = procedure(Command: Byte; const Data: TBytes) of object;

  TClientConnection = class
  private
    FSocket: TSocket;
    FConnected: Boolean;
    FReceiveThread: TThread;
    FScreenCaptureThread: TThread;
    FCapturing: Boolean;
    FDuplicator: TDesktopDuplicator;
    FOnConnected: TConnectedEvent;
    FOnDisconnected: TDisconnectedEvent;
    FOnCommand: TCommandEvent;
    procedure ReceiveData;
    procedure CaptureAndSendScreen;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const ServerIP: string; ServerPort: Integer): Boolean;
    procedure Disconnect;
    procedure SendData(const Data: TBytes);
    procedure SendPong;
    procedure StartScreenCapture;
    procedure StopScreenCapture;
    procedure ProcessMouseCommand(const Data: TBytes);
    procedure ProcessKeyboardCommand(const Data: TBytes);
    procedure LockWorkStation;
    property Connected: Boolean read FConnected;
    property OnConnected: TConnectedEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TDisconnectedEvent read FOnDisconnected write FOnDisconnected;
    property OnCommand: TCommandEvent read FOnCommand write FOnCommand;
  end;

implementation

uses
  Winapi.Messages, Vcl.Graphics, Vcl.Imaging.jpeg;

{ TClientConnection }

constructor TClientConnection.Create;
begin
  inherited Create;
  FConnected := False;
  FCapturing := False;
  FSocket := INVALID_SOCKET;
  FDuplicator := TDesktopDuplicator.Create;
end;

destructor TClientConnection.Destroy;
begin
  Disconnect;
  FDuplicator.Free;
  inherited;
end;

function TClientConnection.Connect(const ServerIP: string; ServerPort: Integer): Boolean;
var
  WSAData: TWSAData;
  Addr: TSockAddrIn;
begin
  Result := False;

  if FConnected then
    Disconnect;

  // Inicializar Winsock
  if WSAStartup($0202, WSAData) <> 0 then
    Exit;

  // Criar socket
  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
  begin
    WSACleanup;
    Exit;
  end;

  // Conectar
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(ServerPort);
  Addr.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(ServerIP)));

  if Winapi.WinSock.connect(FSocket, Addr, SizeOf(Addr)) <> 0 then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    WSACleanup;
    Exit;
  end;

  FConnected := True;

  // Thread para receber dados
  FReceiveThread := TThread.CreateAnonymousThread(ReceiveData);
  FReceiveThread.Start;

  if Assigned(FOnConnected) then
    FOnConnected;

  Result := True;
end;

procedure TClientConnection.Disconnect;
begin
  FConnected := False;
  StopScreenCapture;

  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;

  if Assigned(FReceiveThread) then
  begin
    FReceiveThread.Terminate;
    FReceiveThread := nil;
  end;

  WSACleanup;

  if Assigned(FOnDisconnected) then
    FOnDisconnected;
end;

procedure TClientConnection.ReceiveData;
var
  Buffer: array[0..8191] of Byte;
  RecvLen: Integer;
  Data: TBytes;
  Command: Byte;
  PacketData: TBytes;
begin
  while FConnected do
  begin
    RecvLen := recv(FSocket, Buffer, SizeOf(Buffer), 0);

    if RecvLen <= 0 then
    begin
      Disconnect;
      Break;
    end;

    SetLength(Data, RecvLen);
    Move(Buffer[0], Data[0], RecvLen);

    if ParsePacket(Data, Command, PacketData) then
    begin
      if Assigned(FOnCommand) then
        FOnCommand(Command, PacketData);
    end;
  end;
end;

procedure TClientConnection.SendData(const Data: TBytes);
begin
  if not FConnected then
    Exit;

  if Length(Data) > 0 then
    send(FSocket, Data[0], Length(Data), 0);
end;

procedure TClientConnection.SendPong;
var
  Packet: TBytes;
begin
  Packet := CreatePacket(CMD_PONG, nil);
  SendData(Packet);
end;

procedure TClientConnection.StartScreenCapture;
begin
  if FCapturing then
    Exit;

  FCapturing := True;
  FScreenCaptureThread := TThread.CreateAnonymousThread(CaptureAndSendScreen);
  FScreenCaptureThread.Start;
end;

procedure TClientConnection.StopScreenCapture;
begin
  FCapturing := False;

  if Assigned(FScreenCaptureThread) then
  begin
    FScreenCaptureThread.Terminate;
    FScreenCaptureThread := nil;
  end;
end;

function CaptureScreenGDI(Quality: Integer): TBytes;
var
  ScreenDC: HDC;
  Bitmap: TBitmap;
  JPEGImage: TJPEGImage;
  Stream: TMemoryStream;
  ScreenWidth, ScreenHeight: Integer;
begin
  SetLength(Result, 0);

  ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  ScreenHeight := GetSystemMetrics(SM_CYSCREEN);

  Bitmap := TBitmap.Create;
  JPEGImage := TJPEGImage.Create;
  Stream := TMemoryStream.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := ScreenWidth;
    Bitmap.Height := ScreenHeight;

    ScreenDC := GetDC(0);
    try
      BitBlt(Bitmap.Canvas.Handle, 0, 0, ScreenWidth, ScreenHeight,
             ScreenDC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(0, ScreenDC);
    end;

    JPEGImage.Assign(Bitmap);
    JPEGImage.CompressionQuality := Quality;
    JPEGImage.SaveToStream(Stream);

    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Result[0], Stream.Size);
  finally
    Stream.Free;
    JPEGImage.Free;
    Bitmap.Free;
  end;
end;

procedure TClientConnection.CaptureAndSendScreen;
var
  ScreenData: TBytes;
  CompressedData: TBytes;
  Packet: TBytes;
  UseGDI: Boolean;
begin
  UseGDI := False; // Tentar Desktop Duplication primeiro

  while FCapturing and FConnected do
  begin
    try
      if not UseGDI then
      begin
        // Tentar Desktop Duplication API
        ScreenData := FDuplicator.CaptureScreenToJPEG(75);

        if Length(ScreenData) = 0 then
        begin
          // Falhou, mudar para GDI
          UseGDI := True;
        end;
      end;

      if UseGDI then
      begin
        // Usar GDI como fallback (funciona sem admin)
        ScreenData := CaptureScreenGDI(75);
      end;

      if Length(ScreenData) > 0 then
      begin
        // Compactar dados
        CompressedData := CompressData(ScreenData);

        // Enviar ao servidor
        Packet := CreatePacket(CMD_SCREEN_DATA, CompressedData);
        SendData(Packet);
      end;

      // Aguardar antes da próxima captura (aproximadamente 10 FPS)
      Sleep(100);
    except
      on E: Exception do
      begin
        // Em caso de erro, tentar GDI
        UseGDI := True;
        Sleep(500);
      end;
    end;
  end;
end;

procedure TClientConnection.ProcessMouseCommand(const Data: TBytes);
var
  MouseCmd: TMouseCommand;
  Input: TInput;
begin
  if Length(Data) < SizeOf(TMouseCommand) then
    Exit;

  Move(Data[0], MouseCmd, SizeOf(TMouseCommand));

  // Mover o cursor
  SetCursorPos(MouseCmd.X, MouseCmd.Y);

  // Simular clique se necessário
  if MouseCmd.Button > 0 then
  begin
    ZeroMemory(@Input, SizeOf(TInput));
    Input.Itype := INPUT_MOUSE;

    case MouseCmd.Button of
      1: // Left button
      begin
        Input.mi.dwFlags := MOUSEEVENTF_LEFTDOWN;
        SendInput(1, Input, SizeOf(TInput));
        Input.mi.dwFlags := MOUSEEVENTF_LEFTUP;
        SendInput(1, Input, SizeOf(TInput));
      end;

      2: // Right button
      begin
        Input.mi.dwFlags := MOUSEEVENTF_RIGHTDOWN;
        SendInput(1, Input, SizeOf(TInput));
        Input.mi.dwFlags := MOUSEEVENTF_RIGHTUP;
        SendInput(1, Input, SizeOf(TInput));
      end;

      3: // Middle button
      begin
        Input.mi.dwFlags := MOUSEEVENTF_MIDDLEDOWN;
        SendInput(1, Input, SizeOf(TInput));
        Input.mi.dwFlags := MOUSEEVENTF_MIDDLEUP;
        SendInput(1, Input, SizeOf(TInput));
      end;
    end;
  end;
end;

procedure TClientConnection.ProcessKeyboardCommand(const Data: TBytes);
var
  KeyCmd: TKeyboardCommand;
  Input: TInput;
begin
  if Length(Data) < SizeOf(TKeyboardCommand) then
    Exit;

  Move(Data[0], KeyCmd, SizeOf(TKeyboardCommand));

  ZeroMemory(@Input, SizeOf(TInput));
  Input.Itype := INPUT_KEYBOARD;
  Input.ki.wVk := KeyCmd.VirtualKey;

  if KeyCmd.KeyDown then
    Input.ki.dwFlags := 0
  else
    Input.ki.dwFlags := KEYEVENTF_KEYUP;

  SendInput(1, Input, SizeOf(TInput));
end;

procedure TClientConnection.LockWorkStation;
begin
  Winapi.Windows.LockWorkStation;
end;

end.
