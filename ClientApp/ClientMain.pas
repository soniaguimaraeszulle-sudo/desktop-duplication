unit ClientMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, ClientConnection, Protocol, SystemInfo;

type
  TFormClientMain = class(TForm)
    Panel1: TPanel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    edtServerIP: TEdit;
    edtServerPort: TEdit;
    edtClientID: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    chkAutoStart: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FClient: TClientConnection;
    FClientID: string;
    procedure OnConnected;
    procedure OnDisconnected;
    procedure OnCommand(Command: Byte; const Data: TBytes);
    procedure Log(const Msg: string);
    procedure SendClientInfo;
  public
  end;

var
  FormClientMain: TFormClientMain;

implementation

{$R *.dfm}

procedure TFormClientMain.FormCreate(Sender: TObject);
begin
  FClient := TClientConnection.Create;
  FClient.OnConnected := OnConnected;
  FClient.OnDisconnected := OnDisconnected;
  FClient.OnCommand := OnCommand;
  FClient.OnLog := Log;

  edtServerIP.Text := '127.0.0.1';
  edtServerPort.Text := '9999';
  StatusBar1.SimpleText := 'Desconectado';
end;

procedure TFormClientMain.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

procedure TFormClientMain.FormShow(Sender: TObject);
begin
  if chkAutoStart.Checked then
    btnConnectClick(nil);
end;

procedure TFormClientMain.btnConnectClick(Sender: TObject);
var
  ServerIP: string;
  ServerPort: Integer;
begin
  ServerIP := edtServerIP.Text;
  if not TryStrToInt(edtServerPort.Text, ServerPort) then
  begin
    ShowMessage('Porta inválida!');
    Exit;
  end;

  // Validar ID do cliente
  FClientID := Trim(edtClientID.Text);
  if FClientID = '' then
  begin
    ShowMessage('Por favor, informe o ID do cliente!');
    Exit;
  end;

  Log('Conectando ao servidor ' + ServerIP + ':' + IntToStr(ServerPort));
  Log('ID do Cliente: ' + FClientID);

  if FClient.Connect(ServerIP, ServerPort) then
  begin
    Log('Conectado com sucesso!');
  end
  else
  begin
    Log('Erro ao conectar!');
    ShowMessage('Erro ao conectar ao servidor!');
  end;
end;

procedure TFormClientMain.btnDisconnectClick(Sender: TObject);
begin
  FClient.Disconnect;
  Log('Desconectado');
end;

procedure TFormClientMain.OnConnected;
begin
  TThread.Queue(nil,
    procedure
    begin
      btnConnect.Enabled := False;
      btnDisconnect.Enabled := True;
      edtServerIP.Enabled := False;
      edtServerPort.Enabled := False;
      edtClientID.Enabled := False;
      StatusBar1.SimpleText := 'Conectado';
    end);

  // Enviar informações do cliente
  SendClientInfo;

  // Iniciar captura de tela automaticamente
  TThread.Queue(nil,
    procedure
    begin
      Log('Iniciando captura de tela automática...');
    end);
  FClient.StartScreenCapture;
end;

procedure TFormClientMain.OnDisconnected;
begin
  TThread.Queue(nil,
    procedure
    begin
      btnConnect.Enabled := True;
      btnDisconnect.Enabled := False;
      edtServerIP.Enabled := True;
      edtServerPort.Enabled := True;
      edtClientID.Enabled := True;
      StatusBar1.SimpleText := 'Desconectado';
    end);
end;

procedure TFormClientMain.OnCommand(Command: Byte; const Data: TBytes);
begin
  case Command of
    CMD_PING:
    begin
      // Responder ao ping
      FClient.SendPong;
      TThread.Queue(nil,
        procedure
        begin
          Log('Ping recebido');
        end);
    end;

    CMD_SCREEN_START:
    begin
      FClient.StartScreenCapture;
      TThread.Queue(nil,
        procedure
        begin
          Log('Captura de tela iniciada');
        end);
    end;

    CMD_SCREEN_STOP:
    begin
      FClient.StopScreenCapture;
      TThread.Queue(nil,
        procedure
        begin
          Log('Captura de tela parada');
        end);
    end;

    CMD_MOUSE_MOVE:
    begin
      FClient.ProcessMouseCommand(Data);
    end;

    CMD_MOUSE_CLICK:
    begin
      FClient.ProcessMouseCommand(Data);
    end;

    CMD_KEYBOARD:
    begin
      FClient.ProcessKeyboardCommand(Data);
    end;

    CMD_LOCK_SCREEN:
    begin
      FClient.LockWorkStation;
      TThread.Queue(nil,
        procedure
        begin
          Log('Tela travada');
        end);
    end;

    CMD_UNLOCK_SCREEN:
    begin
      // Não é possível destravar programaticamente
      TThread.Queue(nil,
        procedure
        begin
          Log('Comando de destravamento recebido');
        end);
    end;

    CMD_CHANGE_MONITOR:
    begin
      if Length(Data) >= SizeOf(Byte) then
      begin
        FClient.SetMonitorIndex(Data[0]);
        TThread.Queue(nil,
          procedure
          var
            MonitorIdx: Byte;
          begin
            MonitorIdx := Data[0];
            Log('Monitor alterado para: Monitor ' + IntToStr(MonitorIdx + 1));
          end);
      end;
    end;
  end;
end;

procedure TFormClientMain.SendClientInfo;
var
  Info: TClientInfo;
  Data: TBytes;
  Packet: TBytes;
begin
  Info.ClientID := FClientID;
  Info.ComputerName := GetComputerName;
  Info.IPAddress := GetLocalIPAddress;
  Info.MACAddress := GetMACAddress;
  Info.AntiVirus := GetAntiVirusName;
  Info.OSVersion := GetOSVersion;

  Data := ClientInfoToBytes(Info);
  Packet := CreatePacket(CMD_CLIENT_INFO, Data);
  FClient.SendData(Packet);

  TThread.Queue(nil,
    procedure
    var
      InfoCopy: TClientInfo;
    begin
      InfoCopy := Info;
      Log('Informações enviadas ao servidor');
      Log('  ID: ' + InfoCopy.ClientID);
      Log('  Computador: ' + InfoCopy.ComputerName);
      Log('  IP: ' + InfoCopy.IPAddress);
      Log('  MAC: ' + InfoCopy.MACAddress);
      Log('  Antivírus: ' + InfoCopy.AntiVirus);
      Log('  SO: ' + InfoCopy.OSVersion);
    end);
end;

procedure TFormClientMain.Log(const Msg: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
end;

end.
