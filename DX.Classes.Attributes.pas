unit DX.Classes.Attributes;

interface

uses
  System.Classes, System.SysUtils;

type
  StringValueAttribute = class abstract(TCustomAttribute)

  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

implementation

{ StringValueAttribute }

constructor StringValueAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

end.
