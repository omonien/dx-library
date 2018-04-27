unit FormMainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm71 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form71: TForm71;

implementation

uses
  DX.Sort.Introsort, System.Generics.Collections, System.Math;

{$R *.dfm}

procedure TForm71.Button1Click(Sender: TObject);
var
  LArrayToSort: TArray<integer>;
  LMax: integer;
  LTime: TDatetime;

  procedure PrepareReverseArray;
  var
    i: integer;
  begin
    // Prepare reverse-sorted Array
    SetLength(LArrayToSort, LMax);
    for i := Low(LArrayToSort) to High(LArrayToSort) do
      LArrayToSort[i] := LMax - i;
    // Ensure Array is reverse sorted
    Assert(LArrayToSort[High(LArrayToSort)] = LMax - High(LArrayToSort), 'Invalid low item');
    Assert(LArrayToSort[Low(LArrayToSort)] = LMax - Low(LArrayToSort), 'Invalid max item');
  end;

  procedure PrepareRandomSortedArray;
  var
    i: integer;
  begin
    SetLength(LArrayToSort, LMax);
    for i := Low(LArrayToSort) to High(LArrayToSort) do
      LArrayToSort[i] := Random(LMax);
  end;

  procedure CheckArray;
  var
    i: integer;
  begin
    for i := Low(LArrayToSort) to High(LArrayToSort) - 1 do
      Assert(LArrayToSort[i] <= LArrayToSort[i + 1], 'Invalid order');
  end;

begin
  LMax := 1000000;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Integer Array');
  Memo1.Lines.Add('-------------');
  // Introsort
  PrepareRandomSortedArray;
  LTime := now;
  DX.Sort.Introsort.TArray.Sort<integer>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Introsort random sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  PrepareReverseArray;
  LTime := now;
  DX.Sort.Introsort.TArray.Sort<integer>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Introsort reverse sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  LTime := now;
  DX.Sort.Introsort.TArray.Sort<integer>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Introsort pre-sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  Memo1.Lines.Add('');

  // Quicksort
  PrepareRandomSortedArray;
  LTime := now;
  System.Generics.Collections.TArray.Sort<integer>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Quicksort random sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  PrepareReverseArray;
  LTime := now;
  System.Generics.Collections.TArray.Sort<integer>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Quicksort reverse sorted time time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  LTime := now;
  System.Generics.Collections.TArray.Sort<integer>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Quicksort pre-sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

end;

procedure TForm71.Button2Click(Sender: TObject);
var
  LArrayToSort: TArray<string>;
  LMax: integer;
  LTime: TDatetime;

  function CreateRandomWord: string;
  var
    LWordLength: integer;
    LWord: string;
    i: integer;
  begin
    LWord := '';
    LWordLength := Random(20);
    for i := 1 to LWordLength do
      LWord := LWord + chr(max(32, Random(128)));
    Result := LWord;
  end;

  procedure PrepareReverseArray;
  var
    i: integer;
    LTempArray: TArray<string>;
  begin
    // Prepare reverse-sorted Array
    SetLength(LTempArray, LMax);
    // Create some random string content
    for i := Low(LTempArray) to High(LTempArray) do
      LTempArray[i] := CreateRandomWord;

    // Sort it
    System.Generics.Collections.TArray.Sort<string>(LTempArray);

    // Revert it into target array
    SetLength(LArrayToSort, LMax);
    for i := Low(LTempArray) to High(LTempArray) do
      LArrayToSort[LMax - Low(LTempArray)] := LTempArray[i];

  end;

  procedure PrepareRandomSortedArray;
  var
    i: integer;
  begin
    SetLength(LArrayToSort, LMax);
    for i := Low(LArrayToSort) to High(LArrayToSort) do
      LArrayToSort[i] := CreateRandomWord;;
  end;

  procedure CheckArray;
  var
    i: integer;
  begin
    for i := Low(LArrayToSort) to High(LArrayToSort) - 1 do
      Assert(LArrayToSort[i] <= LArrayToSort[i + 1], 'Invalid order');
  end;

begin
  LMax := 1000000;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('String Array');
  Memo1.Lines.Add('------------');
  // Introsort
  PrepareRandomSortedArray;
  LTime := now;
  DX.Sort.Introsort.TArray.Sort<string>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Introsort random sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  PrepareReverseArray;
  LTime := now;
  DX.Sort.Introsort.TArray.Sort<string>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Introsort reverse sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  LTime := now;
  DX.Sort.Introsort.TArray.Sort<string>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Introsort pre-sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  Memo1.Lines.Add('');

  // Quicksort
  PrepareRandomSortedArray;
  LTime := now;
  System.Generics.Collections.TArray.Sort<string>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Quicksort random sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  PrepareReverseArray;
  LTime := now;
  System.Generics.Collections.TArray.Sort<string>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Quicksort reverse sorted time time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

  LTime := now;
  System.Generics.Collections.TArray.Sort<string>(LArrayToSort);
  LTime := now - LTime;
  CheckArray;
  Memo1.Lines.Add('Quicksort pre-sorted time: ' + FormatDateTime('hh:nn:ss,zzz', LTime));

end;

end.
