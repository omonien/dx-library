unit DX.Classes.Strings;

interface

uses
  System.Classes, System.SysUtils;

type

  /// <summary>
  /// StringList is a simple, managed string collection. No Create or Free
  /// required.
  /// </summary>
  StringList = TArray<String>;

  TStringListHelper = record helper for StringList
    procedure Add(const AString: string);
    procedure AddStrings(const AStrings: TStrings); overload;
    procedure AddStrings(const AStrings: StringList); overload;
    procedure Clear;
    function Contains(const AValue: string): boolean;
    function IsEmpty: boolean;
    function Text: string;
  end;

implementation

{ TStringListHelper }

procedure TStringListHelper.Add(const AString: string);
begin
  SetLength(self, Length(self) + 1);
  self[Length(self) - 1] := AString;
end;

procedure TStringListHelper.AddStrings(const AStrings: TStrings);
var
  s: String;
begin
  for s in AStrings do
  begin
    self.Add(s);
  end;
end;

procedure TStringListHelper.AddStrings(const AStrings: StringList);
var
  s: String;
begin
  for s in AStrings do
  begin
    self.Add(s);
  end;
end;

procedure TStringListHelper.Clear;
begin
  SetLength(self, 0);
end;

function TStringListHelper.Contains(const AValue: string): boolean;
var
  s: string;
begin
  result := false;
  for s in self do
  begin
    if s = AValue then
    begin
      result := true;
      break
    end;
  end;
end;

function TStringListHelper.IsEmpty: boolean;
begin
  result := Length(self) = 0;
end;

function TStringListHelper.Text: string;
var
  s: string;
begin
  result := '';
  for s in self do
  begin
    result := result + s + #13#10;
  end;
  result.TrimRight([#10, #13]);
end;

end.
