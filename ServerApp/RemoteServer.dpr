program RemoteServer;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  RemoteViewForm in 'RemoteViewForm.pas' {FormRemoteView},
  ServerConnection in 'ServerConnection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
