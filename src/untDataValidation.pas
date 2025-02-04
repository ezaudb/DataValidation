{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

unit untDataValidation;

interface

uses
  Classes, Generics.Collections;


type TNumberInterval = record
  Min, Max: Double;

  class function Create(AMin, AMax: Double): TNumberInterval; static;
end;

type IValidation = interface
  ['{E2A684B9-4413-4343-A3E3-49EABDCF22A7}']

  function GetMessageErro: String;

  property MessageErro: String read GetMessageErro;

  function Check: Boolean;
end;


type TEmptyValidation = class(TInterfacedObject, IValidation)
  private
    FCaption, FMessageErro: String;
    FValue: Variant;

    function GetMessageErro: String;
  public
    property MessageErro: String read GetMessageErro;

    function Check: Boolean;

    constructor Create(ACaption, AValue: Variant);
end;

type TNumberIntervalValidation = class(TInterfacedObject, IValidation)
  private
    FCaption, FMessageErro: String;
    FValue: Double;
    FInterval: TNumberInterval;
    FDecimal: Boolean;

    function GetMessageErro: String;
  public
    property MessageErro: String read GetMessageErro;

    function Check: Boolean;

    constructor Create(ACaption: String; AValue: Double; AInterval: TNumberInterval; ADecimal: Boolean = False);
end;

type TStrInValidation = class(TInterfacedObject, IValidation)
    FCaption, FMessageErro, FValue: String;
    FList: TArray<String>;

    function GetMessageErro: String;
  public
    property MessageErro: String read GetMessageErro;

    function Check: Boolean;

    constructor Create(ACaption, AValue: String; AList: TArray<String>);
end;

type TDataValidation = class
  private
    FValidation: TList<IValidation>;
    FMessageErro: TStringList;
  public
    function CheckAll(AValue: Boolean = True): TStringList;

    procedure Add(AValue: IValidation);

    constructor Create;
    destructor Destroy; override;
end;

implementation

uses
  SysUtils, Variants, StrUtils;


class function TNumberInterval.Create(AMin, AMax: Double): TNumberInterval;
begin
  if AMax < AMin then
    raise Exception.Create('Intervalo Inválido');

  Result.Min := AMin;
  Result.Max := AMax;
end;

{ TDataValidation }

procedure TDataValidation.Add(AValue: IValidation);
begin
  FValidation.Add(AValue);
end;

function TDataValidation.CheckAll(AValue: Boolean): TStringList;
var
  Item: IValidation;
begin
  FMessageErro.Clear;

  for Item in FValidation do
  begin
    if Item.Check then
      FMessageErro.Add(Item.MessageErro);
  end;

  if FMessageErro.Count > 0 then
  begin
    FMessageErro.Insert(0, 'Os seguintes Campos Precisam de Atenção:' + sLineBreak);

    if AValue then
      raise Exception.Create(FMessageErro.Text);
  end;

  Result := FMessageErro;
end;

constructor TDataValidation.Create;
begin
  FValidation  := TList<IValidation>.Create;
  FMessageErro := TStringList.Create;
end;

destructor TDataValidation.Destroy;
begin
  FreeAndNil(FValidation);
  FreeAndNil(FMessageErro);

  inherited;
end;

{ TEmptyValidation }

function TEmptyValidation.Check: Boolean;
begin
  Result := VarIsEmpty(FValue) or VarIsNull(FValue);

  if not Result then
    Result := Trim(VarToStr(FValue)).IsEmpty;

  FMessageErro := IfThen(Result, Format('O Campo %s Não Está Preenchido', [FCaption]) , EmptyStr);
end;

constructor TEmptyValidation.Create(ACaption, AValue: Variant);
begin
  FCaption := ACaption;
  FValue   := AValue;
end;

function TEmptyValidation.GetMessageErro: String;
begin
  Result := FMessageErro;
end;

{ TNumberIntervalValidation }

function TNumberIntervalValidation.Check: Boolean;
var
  MinStr, MaxStr: String;
begin
  if FDecimal then
  begin
    MinStr := FormatFloat('#,##0.00', FInterval.Min);
    MaxStr := FormatFloat('#,##0.00', FInterval.Max);
  end else
  begin
    MinStr := FormatFloat('#', FInterval.Min);
    MaxStr := FormatFloat('#', FInterval.Max);
  end;

  Result := (FValue < FInterval.Min) or (FValue > FInterval.Max);
  FMessageErro := IfThen(Result, Format('O Campo %s Não Está no Intervalo: %s / %s', [FCaption, MinStr, MaxStr]), EmptyStr);
end;

constructor TNumberIntervalValidation.Create(ACaption: String; AValue: Double; AInterval: TNumberInterval; ADecimal: Boolean = False);
begin
  FCaption  := ACaption;
  FValue    := AValue;
  FInterval := AInterval;
  FDecimal  := ADecimal;
end;

function TNumberIntervalValidation.GetMessageErro: String;
begin
  Result := FMessageErro;
end;

{ TStrInValidation }

function TStrInValidation.Check: Boolean;
var
  I: Integer;
  UpperList: TArray<String>;
begin
  SetLength(UpperList, Length(FList));
  for I := 0 to High(FList) do
    UpperList[I] := AnsiUpperCase(FList[I]);

  Result := AnsiIndexStr(AnsiUpperCase(FValue), UpperList) = -1;

  FMessageErro := IfThen(Result, Format('O Campo %s Com Valor Inválido', [FCaption]), EmptyStr);
end;

constructor TStrInValidation.Create(ACaption, AValue: String; AList: TArray<String>);
begin
  FCaption := ACaption;
  FValue := AValue;

  FList := Alist;
end;

function TStrInValidation.GetMessageErro: String;
begin
  Result := FMessageErro;
end;

end.
