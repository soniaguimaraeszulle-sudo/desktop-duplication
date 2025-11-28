unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.Winsock, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, ServerConnection, Protocol, System.Generics.Collections;

type
  TClientData = class
    ClientID: Integer;
    Socket: TSocket;
    Info: TClientInfo;
    LastPing: TDateTime;
    Active: Boolean;
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
    Caption := 'ID';
    Width := 50;
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
  end
  else
  begin
    ShowMessage('Erro ao iniciar servidor!');
    FreeAndNil(FServer);
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
    end);
end;

procedure TFormMain.OnClientDisconnected(Socket: TSocket);
var
  SocketCopy: TSocket;
begin
  SocketCopy := Socket;
  TThread.Queue(nil,
    procedure
    begin
      RemoveClient(SocketCopy);
      UpdateClientList;
      StatusBar1.SimpleText := Format('Cliente desconectado - Total: %d', [FClients.Count]);
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
    begin
      Client := FindClient(SocketCopy);
      if not Assigned(Client) then
        Exit;

      case CommandCopy of
        CMD_CLIENT_INFO:
        begin
          Client.Info := BytesToClientInfo(DataCopy);
          UpdateClientList;
        end;

        CMD_PONG:
        begin
          Client.LastPing := Now;
          UpdateClientList;
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

  RemoteForm := TFormRemoteView.Create(Self);
  RemoteForm.SetClient(FServer, Client.Socket, Client.Info.ComputerName);
  RemoteForm.Show;
end;

end.
