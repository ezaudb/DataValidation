# DataValidation

Classes para validação de dados em Delphi e Lazarus. Oferece validações modulares e extensíveis utilizando interfaces e generics, incluindo validação de campos vazios, intervalos numéricos e valores pré-definidos.

## Funcionalidades

- **Validação de campos vazios:** `TEmptyValidation`  
- **Validação de intervalo numérico:** `TNumberIntervalValidation`  
  - Suporte a formatação de números com ou sem casas decimais.
- **Validação de valor em lista:** `TStrInValidation`  
- **Agregação de validações:** `TDataValidation`  
  - Permite adicionar múltiplas validações e verificar todas de uma só vez.
  - Acumula mensagens de erro e, opcionalmente, lança exceção se houver erros.

## Como Usar
```delphi
uses
  untDataValidation, Vcl.StdCtrls, Vcl.Forms, SysUtils;

procedure TForm1.btnValidateClick(Sender: TObject);
var
  Validator: TDataValidation;
  Idade: Integer;
begin
  Validator := TDataValidation.Create;
  try
    Idade := StrToIntDef(EditAge.Text, 0);

    Validator.Add(TEmptyValidation.Create('Nome', edtNome.Text));
    Validator.Add(TNumberIntervalValidation.Create('Idade', Idade, TNumberInterval.Create(18, 60)));
    Validator.Add(TStrInValidation.Create('Sexo', cmbSexo.Text, ['Masculino', 'Feminino']));

    // Opção 1: Lança exceção se houver erros
    Validator.CheckAll();

    // Opção 2: Apenas retorna as mensagens de erro para tratamento customizado
    memErro.Lines.Text := Validator.CheckAll(False).Text;
  finally
    Validator.Free;
  end;
end;
```

## Requisitos

**Delphi:** Testado no Delphi 11.

**Lazarus:** Testado no Lazarus 3.2