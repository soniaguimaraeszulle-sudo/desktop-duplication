unit RemoteViewForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, ServerConnection, Protocol, Compression, Winapi.WinSock;

type
  TFormRemoteView = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    btnStartView: TButton;
    btnStopView: TButton;
    btnLockScreen: TButton;
    btnUnlockScreen: TButton;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnStartViewClick(Sender: TObject);
    procedure btnStopViewClick(Sender: TObject);
    procedure btnLockScreenClick(Sender: TObject);
    procedure btnUnlockScreenClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FServer: TServerConnection;
    FClientSocket: TSocket;
    FClientName: string;
    FViewing: Boolean;
    FLastScreenData: TBytes;
    procedure ProcessScreenData(const Data: TBytes);
  public
    procedure SetClient(Server: TServerConnection; ClientSocket: TSocket; const ClientName: string);
    procedure OnDataReceived(Socket: TSocket; Command: Byte; const Data: TBytes);
  end;

var
  FormRemoteView: TFormRemoteView;

implementation

{$R *.dfm}

procedure TFormRemoteView.FormCreate(Sender: TObject);
begin
  FViewing := False;
  Caption := 'Visualização Remota';

  // Configurar ScrollBox
  ScrollBox1.Align := alClient;
  Image1.Parent := ScrollBox1;
  Image1.AutoSize := True;
end;

procedure TFormRemoteView.FormDestroy(Sender: TObject);
begin
  if FViewing then
    btnStopViewClick(nil);
end;

procedure TFormRemoteView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormRemoteView.SetClient(Server: TServerConnection; ClientSocket: TSocket;
  const ClientName: string);
begin
  FServer := Server;
  FClientSocket := ClientSocket;
  FClientName := ClientName;
  Caption := 'Visualização Remota - ' + ClientName;
end;

procedure TFormRemoteView.btnStartViewClick(Sender: TObject);
var
  Packet: TBytes;
begin
  if not Assigned(FServer) then
    Exit;

  Packet := CreatePacket(CMD_SCREEN_START, nil);
  FServer.SendData(FClientSocket, Packet);

  FViewing := True;
  btnStartView.Enabled := False;
  btnStopView.Enabled := True;
  StatusBar1.SimpleText := 'Visualizando...';
end;

procedure TFormRemoteView.btnStopViewClick(Sender: TObject);
var
  Packet: TBytes;
begin
  if not Assigned(FServer) then
    Exit;

  Packet := CreatePacket(CMD_SCREEN_STOP, nil);
  FServer.SendData(FClientSocket, Packet);

  FViewing := False;
  btnStartView.Enabled := True;
  btnStopView.Enabled := False;
  StatusBar1.SimpleText := 'Parado';
end;

procedure TFormRemoteView.btnLockScreenClick(Sender: TObject);
var
  Packet: TBytes;
begin
  if not Assigned(FServer) then
    Exit;

  Packet := CreatePacket(CMD_LOCK_SCREEN, nil);
  FServer.SendData(FClientSocket, Packet);
end;

procedure TFormRemoteView.btnUnlockScreenClick(Sender: TObject);
var
  Packet: TBytes;
begin
  if not Assigned(FServer) then
    Exit;

  Packet := CreatePacket(CMD_UNLOCK_SCREEN, nil);
  FServer.SendData(FClientSocket, Packet);
end;

procedure TFormRemoteView.OnDataReceived(Socket: TSocket; Command: Byte; const Data: TBytes);
begin
  if Socket <> FClientSocket then
    Exit;

  if Command = CMD_SCREEN_DATA then
  begin
    FLastScreenData := Data;
    TThread.Synchronize(nil,
      procedure
      begin
        ProcessScreenData(Data);
      end);
  end;
end;

procedure TFormRemoteView.ProcessScreenData(const Data: TBytes);
var
  Stream: TMemoryStream;
  JPEGImage: TJPEGImage;
  DecompressedData: TBytes;
begin
  try
    // Descompactar dados
    DecompressedData := DecompressData(Data);

    if Length(DecompressedData) = 0 then
      Exit;

    Stream := TMemoryStream.Create;
    JPEGImage := TJPEGImage.Create;
    try
      Stream.Write(DecompressedData[0], Length(DecompressedData));
      Stream.Position := 0;

      JPEGImage.LoadFromStream(Stream);
      Image1.Picture.Assign(JPEGImage);

      StatusBar1.SimpleText := Format('Visualizando - %dx%d - %.2f KB',
        [JPEGImage.Width, JPEGImage.Height, Length(Data) / 1024]);
    finally
      JPEGImage.Free;
      Stream.Free;
    end;
  except
    on E: Exception do
      StatusBar1.SimpleText := 'Erro: ' + E.Message;
  end;
end;

procedure TFormRemoteView.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MouseCmd: TMouseCommand;
  Data: TBytes;
  Packet: TBytes;
begin
  if not FViewing then
    Exit;

  MouseCmd.X := X;
  MouseCmd.Y := Y;

  case Button of
    mbLeft: MouseCmd.Button := 1;
    mbRight: MouseCmd.Button := 2;
    mbMiddle: MouseCmd.Button := 3;
  else
    MouseCmd.Button := 0;
  end;

  SetLength(Data, SizeOf(TMouseCommand));
  Move(MouseCmd, Data[0], SizeOf(TMouseCommand));

  Packet := CreatePacket(CMD_MOUSE_CLICK, Data);
  FServer.SendData(FClientSocket, Packet);
end;

procedure TFormRemoteView.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  MouseCmd: TMouseCommand;
  Data: TBytes;
  Packet: TBytes;
begin
  if not FViewing then
    Exit;

  MouseCmd.X := X;
  MouseCmd.Y := Y;
  MouseCmd.Button := 0;

  SetLength(Data, SizeOf(TMouseCommand));
  Move(MouseCmd, Data[0], SizeOf(TMouseCommand));

  Packet := CreatePacket(CMD_MOUSE_MOVE, Data);
  FServer.SendData(FClientSocket, Packet);
end;

procedure TFormRemoteView.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  KeyCmd: TKeyboardCommand;
  Data: TBytes;
  Packet: TBytes;
begin
  if not FViewing then
    Exit;

  KeyCmd.VirtualKey := Key;
  KeyCmd.KeyDown := True;

  SetLength(Data, SizeOf(TKeyboardCommand));
  Move(KeyCmd, Data[0], SizeOf(TKeyboardCommand));

  Packet := CreatePacket(CMD_KEYBOARD, Data);
  FServer.SendData(FClientSocket, Packet);
end;

procedure TFormRemoteView.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  KeyCmd: TKeyboardCommand;
  Data: TBytes;
  Packet: TBytes;
begin
  if not FViewing then
    Exit;

  KeyCmd.VirtualKey := Key;
  KeyCmd.KeyDown := False;

  SetLength(Data, SizeOf(TKeyboardCommand));
  Move(KeyCmd, Data[0], SizeOf(TKeyboardCommand));

  Packet := CreatePacket(CMD_KEYBOARD, Data);
  FServer.SendData(FClientSocket, Packet);
end;

end.
