unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.Winsock, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, ServerConnection, Protocol, System.Generics.Collections;

type
  TFormRemoteView = class;  // Forward declaration

  TClientData = class
    ClientID: Integer;
    Socket: TSocket;
    Info: TClientInfo;
    LastPing: TDateTime;
    Active: Boolean;
    RemoteForm: TFormRemoteView;  // Referência ao formulário remoto
  end;

  TFormMain = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    edtPort: TEdit;
    Label1: TLabel;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    FServer: TServerConnection;
    FClients: TObjectList<TClientData>;
    FNextClientID: Integer;
    procedure OnClientConnected(Socket: TSocket);
    procedure OnClientDisconnected(Socket: TSocket);
    procedure OnClientData(Socket: TSocket; Command: Byte; const Data: TBytes);
    procedure UpdateClientList;
    procedure AddClient(Socket: TSocket);
    procedure RemoveClient(Socket: TSocket);
    function FindClient(Socket: TSocket): TClientData;
    procedure SendPingToAll;
    function CalculatePing(Client: TClientData): Integer;
    procedure Log(const Msg: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  RemoteViewForm, System.DateUtils;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FClients := TObjectList<TClientData>.Create(True);
  FNextClientID := 1;

  // Configurar ListView
  ListView1.ViewStyle := vsReport;
  ListView1.RowSelect := True;
  ListView1.GridLines := True;

  with ListView1.Columns.Add do
  begin
    Caption := 'ID CLIENTE';
    Width := 100;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'MÁQUINA';
    Width := 150;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'IP';
    Width := 120;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'MAC';
    Width := 130;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'ANTIVIRUS';
    Width := 150;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'PING (ms)';
    Width := 80;
  end;

  edtPort.Text := '9999';
  StatusBar1.SimpleText := 'Servidor parado';
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
  begin
    FServer.Stop;
    FServer.Free;
  end;
  FClients.Free;
end;

procedure TFormMain.btnStartClick(Sender: TObject);
var
  Port: Integer;
begin
  if not TryStrToInt(edtPort.Text, Port) then
  begin
    ShowMessage('Porta inválida!');
    Exit;
  end;

  FServer := TServerConnection.Create;
  FServer.OnClientConnected := OnClientConnected;
  FServer.OnClientDisconnected := OnClientDisconnected;
  FServer.OnClientData := OnClientData;

  if FServer.Start(Port) then
  begin
    btnStart.Enabled := False;
    btnStop.Enabled := True;
    edtPort.Enabled := False;
    Timer1.Enabled := True;
    StatusBar1.SimpleText := Format('Servidor iniciado na porta %d', [Port]);
    Log(Format('Servidor iniciado na porta %d', [Port]));
    Log('Aguardando conexões de clientes...');
  end
  else
  begin
    ShowMessage('Erro ao iniciar servidor!');
    FreeAndNil(FServer);
    Log('ERRO: Falha ao iniciar servidor!');
  end;
end;

procedure TFormMain.btnStopClick(Sender: TObject);
begin
  Timer1.Enabled := False;

  if Assigned(FServer) then
  begin
    FServer.Stop;
    FreeAndNil(FServer);
  end;

  FClients.Clear;
  ListView1.Items.Clear;

  btnStart.Enabled := True;
  btnStop.Enabled := False;
  edtPort.Enabled := True;
  StatusBar1.SimpleText := 'Servidor parado';
  Log('Servidor parado');
  Log('---');
end;

procedure TFormMain.OnClientConnected(Socket: TSocket);
var
  SocketCopy: TSocket;
begin
  SocketCopy := Socket;
  TThread.Queue(nil,
    procedure
    begin
      AddClient(SocketCopy);
      StatusBar1.SimpleText := Format('Cliente conectado - Total: %d', [FClients.Count]);
      Log(Format('Cliente conectado [Socket: %d] - Total: %d', [SocketCopy, FClients.Count]));
    end);
end;

procedure TFormMain.OnClientDisconnected(Socket: TSocket);
var
  SocketCopy: TSocket;
  Client: TClientData;
  ClientInfo: string;
  RemoteFormToClose: TFormRemoteView;
begin
  SocketCopy := Socket;

  // Obter informações do cliente antes de remover
  Client := FindClient(SocketCopy);
  RemoteFormToClose := nil;

  if Assigned(Client) then
  begin
    if Client.Info.ClientID <> '' then
      ClientInfo := Format('[%s - %s]', [Client.Info.ClientID, Client.Info.ComputerName])
    else
      ClientInfo := Format('[ID: %d]', [Client.ClientID]);

    // Se há um RemoteForm aberto, precisamos fechá-lo
    if Assigned(Client.RemoteForm) then
    begin
      RemoteFormToClose := Client.RemoteForm;
      Client.RemoteForm := nil;
    end;
  end
  else
    ClientInfo := Format('[Socket: %d]', [SocketCopy]);

  TThread.Queue(nil,
    procedure
    var
      InfoCopy: string;
    begin
      InfoCopy := ClientInfo;

      // Fechar RemoteForm se estava aberto
      if Assigned(RemoteFormToClose) then
      begin
        RemoteFormToClose.Close;
      end;

      RemoveClient(SocketCopy);
      UpdateClientList;
      StatusBar1.SimpleText := Format('Cliente desconectado - Total: %d', [FClients.Count]);
      Log(Format('Cliente desconectado %s - Total: %d', [InfoCopy, FClients.Count]));
    end);
end;

procedure TFormMain.OnClientData(Socket: TSocket; Command: Byte; const Data: TBytes);
var
  Client: TClientData;
  SocketCopy: TSocket;
  CommandCopy: Byte;
  DataCopy: TBytes;
begin
  SocketCopy := Socket;
  CommandCopy := Command;
  DataCopy := Copy(Data);

  TThread.Queue(nil,
    procedure
    var
      ClientIdentifier: string;
    begin
      Client := FindClient(SocketCopy);
      if not Assigned(Client) then
        Exit;

      case CommandCopy of
        CMD_CLIENT_INFO:
        begin
          Client.Info := BytesToClientInfo(DataCopy);
          UpdateClientList;

          // Log de informações do cliente
          if Client.Info.ClientID <> '' then
            ClientIdentifier := Client.Info.ClientID
          else
            ClientIdentifier := IntToStr(Client.ClientID);

          Log(Format('Informações recebidas de %s:', [ClientIdentifier]));
          Log(Format('  Computador: %s', [Client.Info.ComputerName]));
          Log(Format('  IP: %s | MAC: %s', [Client.Info.IPAddress, Client.Info.MACAddress]));
          Log(Format('  SO: %s', [Client.Info.OSVersion]));
        end;

        CMD_PONG:
        begin
          Client.LastPing := Now;
          UpdateClientList;

          // Log de ping (a cada 20 pings para não spammar)
          if Random(20) = 0 then
          begin
            if Client.Info.ClientID <> '' then
              ClientIdentifier := Client.Info.ClientID
            else
              ClientIdentifier := IntToStr(Client.ClientID);
            Log(Format('Ping recebido de %s', [ClientIdentifier]));
          end;
        end;

        CMD_SCREEN_DATA:
        begin
          // Encaminhar dados para o RemoteViewForm se estiver aberto
          if Assigned(Client.RemoteForm) then
          begin
            Client.RemoteForm.OnDataReceived(SocketCopy, CommandCopy, DataCopy);
          end;

          // Log de recebimento de frame (apenas de vez em quando para não encher o log)
          if Random(100) < 5 then  // 5% das vezes
          begin
            if Client.Info.ClientID <> '' then
              ClientIdentifier := Client.Info.ClientID
            else
              ClientIdentifier := IntToStr(Client.ClientID);
            Log(Format('Recebendo frames de %s... (%.2f KB)', [ClientIdentifier, Length(DataCopy) / 1024]));
          end;
        end;
      end;
    end);
end;

procedure TFormMain.AddClient(Socket: TSocket);
var
  Client: TClientData;
begin
  Client := TClientData.Create;
  Client.ClientID := FNextClientID;
  Inc(FNextClientID);
  Client.Socket := Socket;
  Client.Active := True;
  Client.LastPing := Now;

  FClients.Add(Client);
  UpdateClientList;
end;

procedure TFormMain.RemoveClient(Socket: TSocket);
var
  i: Integer;
begin
  for i := 0 to FClients.Count - 1 do
  begin
    if FClients[i].Socket = Socket then
    begin
      FClients.Delete(i);
      Break;
    end;
  end;
end;

function TFormMain.FindClient(Socket: TSocket): TClientData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FClients.Count - 1 do
  begin
    if FClients[i].Socket = Socket then
    begin
      Result := FClients[i];
      Break;
    end;
  end;
end;

procedure TFormMain.UpdateClientList;
var
  i: Integer;
  Item: TListItem;
  Client: TClientData;
  Ping: Integer;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    for i := 0 to FClients.Count - 1 do
    begin
      Client := FClients[i];
      Item := ListView1.Items.Add;
      // Exibir o ID do cliente ao invés do ID sequencial
      if Client.Info.ClientID <> '' then
        Item.Caption := Client.Info.ClientID
      else
        Item.Caption := IntToStr(Client.ClientID);
      Item.SubItems.Add(Client.Info.ComputerName);
      Item.SubItems.Add(Client.Info.IPAddress);
      Item.SubItems.Add(Client.Info.MACAddress);
      Item.SubItems.Add(Client.Info.AntiVirus);

      Ping := CalculatePing(Client);
      if Ping >= 0 then
        Item.SubItems.Add(IntToStr(Ping))
      else
        Item.SubItems.Add('-');

      Item.Data := Client;
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

function TFormMain.CalculatePing(Client: TClientData): Integer;
var
  ElapsedMs: Int64;
begin
  if Client.LastPing = 0 then
  begin
    Result := -1;
    Exit;
  end;

  ElapsedMs := MilliSecondsBetween(Now, Client.LastPing);
  if ElapsedMs > 5000 then // Mais de 5 segundos sem resposta
    Result := -1
  else
    Result := ElapsedMs;
end;

procedure TFormMain.SendPingToAll;
var
  i: Integer;
  Packet: TBytes;
begin
  if not Assigned(FServer) then
    Exit;

  Packet := CreatePacket(CMD_PING, nil);

  for i := 0 to FClients.Count - 1 do
  begin
    FClients[i].LastPing := Now;
    FServer.SendData(FClients[i].Socket, Packet);
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  SendPingToAll;
  UpdateClientList;
end;

procedure TFormMain.ListView1DblClick(Sender: TObject);
var
  Client: TClientData;
  RemoteForm: TFormRemoteView;
begin
  if ListView1.Selected = nil then
    Exit;

  Client := TClientData(ListView1.Selected.Data);
  if not Assigned(Client) then
    Exit;

  // Se já existe um RemoteForm aberto para este cliente, focar nele
  if Assigned(Client.RemoteForm) then
  begin
    Client.RemoteForm.BringToFront;
    Exit;
  end;

  RemoteForm := TFormRemoteView.Create(Self);
  RemoteForm.SetClient(FServer, Client.Socket, Client.Info.ComputerName);

  // Associar o formulário ao cliente
  Client.RemoteForm := RemoteForm;

  // Quando o formulário for fechado, limpar a referência
  RemoteForm.OnFormClose := procedure(ClientSocket: TSocket)
    begin
      Client.RemoteForm := nil;
    end;

  RemoteForm.Show;
end;

procedure TFormMain.Log(const Msg: string);
begin
  if Memo1.Lines.Count > 500 then
    Memo1.Lines.Delete(0);  // Limpar linhas antigas para não ficar muito grande

  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
end;

end.
