unit ClientMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ClientConnection, Protocol, SystemInfo;

type
  TFormClientMain = class(TForm)
    Panel1: TPanel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    edtServerIP: TEdit;
    edtServerPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
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

  Log('Conectando ao servidor ' + ServerIP + ':' + IntToStr(ServerPort));

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
  TThread.Synchronize(nil,
    procedure
    begin
      btnConnect.Enabled := False;
      btnDisconnect.Enabled := True;
      edtServerIP.Enabled := False;
      edtServerPort.Enabled := False;
      StatusBar1.SimpleText := 'Conectado';
    end);

  // Enviar informações do cliente
  SendClientInfo;
end;

procedure TFormClientMain.OnDisconnected;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      btnConnect.Enabled := True;
      btnDisconnect.Enabled := False;
      edtServerIP.Enabled := True;
      edtServerPort.Enabled := True;
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
      TThread.Synchronize(nil,
        procedure
        begin
          Log('Ping recebido');
        end);
    end;

    CMD_SCREEN_START:
    begin
      FClient.StartScreenCapture;
      TThread.Synchronize(nil,
        procedure
        begin
          Log('Captura de tela iniciada');
        end);
    end;

    CMD_SCREEN_STOP:
    begin
      FClient.StopScreenCapture;
      TThread.Synchronize(nil,
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
      TThread.Synchronize(nil,
        procedure
        begin
          Log('Tela travada');
        end);
    end;

    CMD_UNLOCK_SCREEN:
    begin
      // Não é possível destravar programaticamente
      TThread.Synchronize(nil,
        procedure
        begin
          Log('Comando de destravamento recebido');
        end);
    end;
  end;
end;

procedure TFormClientMain.SendClientInfo;
var
  Info: TClientInfo;
  Data: TBytes;
  Packet: TBytes;
begin
  Info.ComputerName := GetComputerName;
  Info.IPAddress := GetLocalIPAddress;
  Info.MACAddress := GetMACAddress;
  Info.AntiVirus := GetAntiVirusName;
  Info.OSVersion := GetOSVersion;

  Data := ClientInfoToBytes(Info);
  Packet := CreatePacket(CMD_CLIENT_INFO, Data);
  FClient.SendData(Packet);

  TThread.Synchronize(nil,
    procedure
    begin
      Log('Informações enviadas ao servidor');
      Log('  Computador: ' + Info.ComputerName);
      Log('  IP: ' + Info.IPAddress);
      Log('  MAC: ' + Info.MACAddress);
      Log('  Antivírus: ' + Info.AntiVirus);
      Log('  SO: ' + Info.OSVersion);
    end);
end;

procedure TFormClientMain.Log(const Msg: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
end;

end.
