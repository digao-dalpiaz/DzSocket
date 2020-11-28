program DzSocketDemo;

{$R *.dres}

uses
  Vcl.Forms,
  UFrm in 'UFrm.pas' {Frm},
  UFrmPrint in 'UFrmPrint.pas' {FrmPrint},
  UClasses in 'UClasses.pas',
  UPrint in 'UPrint.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrm, Frm);
  Application.CreateForm(TFrmPrint, FrmPrint);
  Application.Run;
end.
