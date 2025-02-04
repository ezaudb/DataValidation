object frmValidation: TfrmValidation
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Validation'
  ClientHeight = 308
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  TextHeight = 15
  object labSexo: TLabel
    Left = 167
    Top = 59
    Width = 25
    Height = 15
    Caption = 'Sexo'
  end
  object labErro: TLabel
    Left = 8
    Top = 107
    Width = 98
    Height = 15
    Caption = 'Resultado Valia'#231#227'o'
  end
  object edtNome: TLabeledEdit
    Left = 8
    Top = 26
    Width = 297
    Height = 23
    EditLabel.Width = 33
    EditLabel.Height = 15
    EditLabel.Caption = 'Nome'
    TabOrder = 0
    Text = ''
  end
  object edtIdade: TLabeledEdit
    Left = 8
    Top = 76
    Width = 153
    Height = 23
    EditLabel.Width = 29
    EditLabel.Height = 15
    EditLabel.Caption = 'Idade'
    TabOrder = 1
    Text = ''
  end
  object cmbSexo: TComboBox
    Left = 167
    Top = 76
    Width = 138
    Height = 23
    TabOrder = 2
    Items.Strings = (
      'Masculino'
      'Feminino')
  end
  object btnValidacao: TButton
    Left = 8
    Top = 274
    Width = 297
    Height = 25
    Caption = 'Validar'
    TabOrder = 3
    OnClick = btnValidacaoClick
  end
  object memErro: TMemo
    Left = 8
    Top = 126
    Width = 297
    Height = 89
    TabOrder = 4
  end
  object rdgSaidaValidacao: TRadioGroup
    Left = 8
    Top = 225
    Width = 297
    Height = 41
    Caption = 'Tipo Sa'#237'da Valida'#231#227'o'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Exception'
      'Personalizado (Memo)')
    TabOrder = 5
  end
end
