unit untValidation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmValidation }

  TfrmValidation = class(TForm)
    btnValidacao: TButton;
    cmbSexo: TComboBox;
    edtIdade: TLabeledEdit;
    edtNome: TLabeledEdit;
    labErro: TLabel;
    labSexo: TLabel;
    memErro: TMemo;
    rdgSaidaValidacao: TRadioGroup;
    procedure btnValidacaoClick(Sender: TObject);
  private

  public

  end;

var
  frmValidation: TfrmValidation;

implementation

{$R *.lfm}

{ TfrmValidation }

uses
 untDataValidation;

procedure TfrmValidation.btnValidacaoClick(Sender: TObject);
var
  DataValidation: TDataValidation;
  Idade: Integer;
begin
  memErro.Lines.Clear;

  DataValidation := TDataValidation.Create;
  try
    Idade := StrToIntDef(edtIdade.Text, 0);

    DataValidation.Add(TEmptyValidation.Create('Nome', edtNome.Text));
    DataValidation.Add(TNumberIntervalValidation.Create('Idade', Idade, TNumberInterval.Create(18, 60)));
    DataValidation.Add(TStrInValidation.Create('Sexo', cmbSexo.Text, ['Masculino', 'Feminino']));

    if rdgSaidaValidacao.ItemIndex = 0 then
      DataValidation.CheckAll()
    else
      memErro.Lines.Text := DataValidation.CheckAll(False).Text;
  finally
    FreeAndNil(DataValidation);
  end;
end;

end.

