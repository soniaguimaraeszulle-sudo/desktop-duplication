program RemoteClient;

uses
  Vcl.Forms,
  ClientMain in 'ClientMain.pas' {FormClientMain},
  ClientConnection in 'ClientConnection.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormClientMain, FormClientMain);
  Application.Run;
end.
