unit untValidation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls;

type
  TfrmValidation = class(TForm)
    edtNome: TLabeledEdit;
    edtIdade: TLabeledEdit;
    cmbSexo: TComboBox;
    labSexo: TLabel;
    btnValidacao: TButton;
    memErro: TMemo;
    labErro: TLabel;
    rdgSaidaValidacao: TRadioGroup;
    procedure btnValidacaoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmValidation: TfrmValidation;

implementation

uses
  untDataValidation;

{$R *.dfm}

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
