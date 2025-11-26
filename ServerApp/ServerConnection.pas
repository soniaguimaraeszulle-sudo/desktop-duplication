unit ServerConnection;

interface

uses
  Winapi.Windows, Winapi.WinSock, System.SysUtils, System.Classes,
  System.Generics.Collections, Protocol;

type
  TClientConnectedEvent = procedure(Socket: TSocket) of object;
  TClientDisconnectedEvent = procedure(Socket: TSocket) of object;
  TClientDataEvent = procedure(Socket: TSocket; Command: Byte; const Data: TBytes) of object;

  TClientThread = class(TThread)
  private
    FSocket: TSocket;
    FOnDisconnected: TClientDisconnectedEvent;
    FOnData: TClientDataEvent;
    procedure ProcessData(const Buffer: TBytes);
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket: TSocket);
    property Socket: TSocket read FSocket;
    property OnDisconnected: TClientDisconnectedEvent read FOnDisconnected write FOnDisconnected;
    property OnData: TClientDataEvent read FOnData write FOnData;
  end;

  TServerConnection = class
  private
    FServerSocket: TSocket;
    FPort: Integer;
    FActive: Boolean;
    FAcceptThread: TThread;
    FClients: TThreadList<TClientThread>;
    FOnClientConnected: TClientConnectedEvent;
    FOnClientDisconnected: TClientDisconnectedEvent;
    FOnClientData: TClientDataEvent;
    procedure AcceptClients;
    procedure OnClientThreadDisconnected(Socket: TSocket);
    procedure OnClientThreadData(Socket: TSocket; Command: Byte; const Data: TBytes);
  public
    constructor Create;
    destructor Destroy; override;
    function Start(Port: Integer): Boolean;
    procedure Stop;
    procedure SendData(Socket: TSocket; const Data: TBytes);
    property OnClientConnected: TClientConnectedEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TClientDisconnectedEvent read FOnClientDisconnected write FOnClientDisconnected;
    property OnClientData: TClientDataEvent read FOnClientData write FOnClientData;
  end;

implementation

{ TClientThread }

constructor TClientThread.Create(ASocket: TSocket);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FSocket := ASocket;
end;

procedure TClientThread.Execute;
var
  Buffer: array[0..8191] of Byte;
  RecvLen: Integer;
  Data: TBytes;
begin
  while not Terminated do
  begin
    RecvLen := recv(FSocket, Buffer, SizeOf(Buffer), 0);

    if RecvLen <= 0 then
    begin
      // Cliente desconectou
      if Assigned(FOnDisconnected) then
        FOnDisconnected(FSocket);
      Break;
    end;

    SetLength(Data, RecvLen);
    Move(Buffer[0], Data[0], RecvLen);
    ProcessData(Data);
  end;

  closesocket(FSocket);
end;

procedure TClientThread.ProcessData(const Buffer: TBytes);
var
  Command: Byte;
  Data: TBytes;
begin
  if ParsePacket(Buffer, Command, Data) then
  begin
    if Assigned(FOnData) then
      FOnData(FSocket, Command, Data);
  end;
end;

{ TServerConnection }

constructor TServerConnection.Create;
begin
  inherited Create;
  FActive := False;
  FClients := TThreadList<TClientThread>.Create;
end;

destructor TServerConnection.Destroy;
begin
  Stop;
  FClients.Free;
  inherited;
end;

function TServerConnection.Start(Port: Integer): Boolean;
var
  WSAData: TWSAData;
  Addr: TSockAddrIn;
  OptVal: Integer;
begin
  Result := False;
  FPort := Port;

  // Inicializar Winsock
  if WSAStartup($0202, WSAData) <> 0 then
    Exit;

  // Criar socket
  FServerSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FServerSocket = INVALID_SOCKET then
  begin
    WSACleanup;
    Exit;
  end;

  // Configurar socket para reusar endereço
  OptVal := 1;
  setsockopt(FServerSocket, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@OptVal), SizeOf(OptVal));

  // Bind
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  Addr.sin_addr.S_addr := INADDR_ANY;

  if bind(FServerSocket, Addr, SizeOf(Addr)) <> 0 then
  begin
    closesocket(FServerSocket);
    WSACleanup;
    Exit;
  end;

  // Listen
  if listen(FServerSocket, SOMAXCONN) <> 0 then
  begin
    closesocket(FServerSocket);
    WSACleanup;
    Exit;
  end;

  FActive := True;

  // Thread para aceitar conexões
  FAcceptThread := TThread.CreateAnonymousThread(AcceptClients);
  FAcceptThread.Start;

  Result := True;
end;

procedure TServerConnection.Stop;
var
  List: TList<TClientThread>;
  i: Integer;
begin
  FActive := False;

  if FServerSocket <> INVALID_SOCKET then
  begin
    closesocket(FServerSocket);
    FServerSocket := INVALID_SOCKET;
  end;

  // Fechar todas as conexões de clientes
  List := FClients.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      List[i].Terminate;
      closesocket(List[i].Socket);
    end;
    List.Clear;
  finally
    FClients.UnlockList;
  end;

  if Assigned(FAcceptThread) then
  begin
    FAcceptThread.Terminate;
    FAcceptThread := nil;
  end;

  WSACleanup;
end;

procedure TServerConnection.AcceptClients;
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddrIn;
  AddrLen: Integer;
  ClientThread: TClientThread;
  List: TList<TClientThread>;
begin
  while FActive do
  begin
    AddrLen := SizeOf(ClientAddr);
    ClientSocket := accept(FServerSocket, @ClientAddr, @AddrLen);

    if ClientSocket = INVALID_SOCKET then
      Continue;

    // Criar thread para o cliente
    ClientThread := TClientThread.Create(ClientSocket);
    ClientThread.OnDisconnected := OnClientThreadDisconnected;
    ClientThread.OnData := OnClientThreadData;

    List := FClients.LockList;
    try
      List.Add(ClientThread);
    finally
      FClients.UnlockList;
    end;

    // Notificar conexão
    if Assigned(FOnClientConnected) then
      FOnClientConnected(ClientSocket);
  end;
end;

procedure TServerConnection.OnClientThreadDisconnected(Socket: TSocket);
var
  List: TList<TClientThread>;
  i: Integer;
begin
  // Remover da lista
  List := FClients.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      if List[i].Socket = Socket then
      begin
        List.Delete(i);
        Break;
      end;
    end;
  finally
    FClients.UnlockList;
  end;

  // Notificar desconexão
  if Assigned(FOnClientDisconnected) then
    FOnClientDisconnected(Socket);
end;

procedure TServerConnection.OnClientThreadData(Socket: TSocket; Command: Byte; const Data: TBytes);
begin
  if Assigned(FOnClientData) then
    FOnClientData(Socket, Command, Data);
end;

procedure TServerConnection.SendData(Socket: TSocket; const Data: TBytes);
begin
  if Length(Data) > 0 then
    send(Socket, Data[0], Length(Data), 0);
end;

end.
