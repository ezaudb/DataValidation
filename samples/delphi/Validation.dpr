program Validation;

uses
  Vcl.Forms,
  untValidation in 'untValidation.pas' {frmValidation},
  untDataValidation in '..\..\src\untDataValidation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmValidation, frmValidation);
  Application.Run;
end.
