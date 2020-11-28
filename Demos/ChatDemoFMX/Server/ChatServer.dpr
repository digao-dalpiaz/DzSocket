program ChatServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFrmServer in 'UFrmServer.pas' {FrmServer},
  UClient in 'UClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmServer, FrmServer);
  Application.Run;
end.
