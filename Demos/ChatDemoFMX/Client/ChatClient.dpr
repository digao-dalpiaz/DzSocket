program ChatClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  UFrmClient in 'UFrmClient.pas' {FrmClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmClient, FrmClient);
  Application.Run;
end.
