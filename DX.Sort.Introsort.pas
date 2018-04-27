unit DX.Sort.Introsort;

interface

uses
  Generics.Defaults;

type
  TArray = record
  private
    const IntrosortSizeThreshold = 16;
    class function GetDepthLimit(count: Integer): Integer; static;

    class procedure Swap<T>(var left, right: T); static; inline;

    class procedure SortTwoItems<T>(const comparer: IComparer<T>; var left, right: T); static;
    class procedure SortThreeItems<T>(const comparer: IComparer<T>; var left, mid, right: T); static;

    class procedure InsertionSort<T>(var values: array of T; const comparer: IComparer<T>; left, right: Integer); static;

    class procedure DownHeap<T>(var values: array of T; const comparer: IComparer<T>; left, count, i: Integer); static;
    class procedure HeapSort<T>(var values: array of T; const comparer: IComparer<T>; left, right: Integer); static;

    class function QuickSortPartition<T>(var values: array of T; const comparer: IComparer<T>; left, right: Integer): Integer; static;

    class procedure IntroSort<T>(var values: array of T; const comparer: IComparer<T>; left, right, depthLimit: Integer); static;
  public
    class procedure Sort<T>(var values: array of T); overload; static;
    class procedure Sort<T>(var values: array of T; const comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var values: array of T; const comparer: IComparer<T>; index, count: Integer); overload; static;
  end;

procedure SwapPtr(var left, right); inline;

implementation

uses
  RTLConsts,
  SysUtils;

procedure SwapPtr(var left, right);
var
  temp: Pointer;
begin
  temp := Pointer(left);
  Pointer(left) := Pointer(right);
  Pointer(right) := temp;
end;

{ TArray }

class function TArray.GetDepthLimit(count: Integer): Integer;
begin
  Result := 0;
  while count > 0 do
  begin
    Inc(Result);
    count := count div 2;
  end;
  Result := Result * 2;
end;

class procedure TArray.Swap<T>(var left, right: T);
var
  temp: T;
begin
{$IF CompilerVersion >= 28} // XE7 and higher
  case GetTypeKind(T) of
{$IFDEF AUTOREFCOUNT}
    tkClass,
{$ENDIF AUTOREFCOUNT}
    tkInterface,
    tkDynArray,
    tkUString:
      SwapPtr(left, right);
  else
    temp := left;
    left := right;
    right := temp;
  end;
{$ELSE}
  temp := left;
  left := right;
  right := temp;
{$IFEND}
end;

class procedure TArray.SortTwoItems<T>(const comparer: IComparer<T>;
  var left, right: T);
begin
  if comparer.Compare(left, right) > 0 then
    Swap<T>(left, right);
end;

class procedure TArray.SortThreeItems<T>(const comparer: IComparer<T>;
  var left, mid, right: T);
begin
  if comparer.Compare(left, mid) > 0 then
    Swap<T>(left, mid);
  if comparer.Compare(left, right) > 0 then
    Swap<T>(left, right);
  if comparer.Compare(mid, right) > 0 then
    Swap<T>(mid, right);
end;

class procedure TArray.DownHeap<T>(var values: array of T;
  const comparer: IComparer<T>; left, count, i: Integer);
var
  temp: T;
  child, n, x: Integer;
begin
  temp := values[left + i - 1];
  n := count div 2;
  while i <= n do
  begin
    child := i * 2;
    if (child < count) and (comparer.Compare(values[left + child - 1], values[left + child]) < 0) then
      Inc(child);
    if not comparer.Compare(temp, values[left + child - 1]) < 0 then
      Break;
    values[left + i - 1] := values[left + child - 1];
    i := child;
  end;
  values[left + i - 1] := temp;
end;

class procedure TArray.HeapSort<T>(var values: array of T;
  const comparer: IComparer<T>; left, right: Integer);
var
  count, i: Integer;
begin
  count := right - left + 1;
  for i := count div 2 downto 1 do
    DownHeap<T>(values, comparer, left, count, i);
  for i := count downto 2 do
  begin
    Swap<T>(values[left], values[left + i - 1]);
    DownHeap<T>(values, comparer, left, i - 1, 1);
  end;
end;

class procedure TArray.InsertionSort<T>(var values: array of T;
  const comparer: IComparer<T>; left, right: Integer);
var
  i, j: Integer;
  temp: T;
begin
  for i := left + 1 to right do
  begin
    j := i;
    temp := values[i];
    while (j > left) and (comparer.Compare(temp, values[j - 1]) < 0) do
    begin
      values[j] := values[j - 1];
      Dec(j);
    end;
    values[j] := temp;
  end;
end;

class function TArray.QuickSortPartition<T>(var values: array of T;
  const comparer: IComparer<T>; left, right: Integer): Integer;
var
  mid, pivotIndex: Integer;
  pivot: T;
begin
  mid := left + (right - left) div 2;

  SortThreeItems<T>(comparer, values[left], values[mid], values[right]);

  Dec(right);
  pivotIndex := right;

  pivot := values[mid];
  Swap<T>(values[mid], values[right]);

  while left < right do
  begin
    repeat
      Inc(left);
    until comparer.Compare(values[left], pivot) >= 0;
    repeat
      Dec(right);
    until comparer.Compare(pivot, values[right]) >= 0;

    if left >= right then
      Break;

    Swap<T>(values[left], values[right]);
  end;

  Swap<T>(values[left], values[pivotIndex]);
  Result := left;
end;

class procedure TArray.IntroSort<T>(var values: array of T;
  const comparer: IComparer<T>; left, right, depthLimit: Integer);
var
  count, pivot: Integer;
begin
  while right > left do
  begin
    count := right - left + 1;
    if count = 1 then
      Exit;
    if count = 2 then
    begin
      SortTwoItems<T>(comparer, values[left], values[right]);
      Exit;
    end;
    if count = 3 then
    begin
      SortThreeItems<T>(comparer, values[left], values[right - 1], values[right]);
      Exit;
    end;
    if count <= IntrosortSizeThreshold then
    begin
      InsertionSort<T>(values, comparer, left, right);
      Exit;
    end;

    if depthLimit = 0 then
    begin
      HeapSort<T>(values, comparer, left, right);
      Exit;
    end;

    Dec(depthLimit);
    pivot := QuickSortPartition<T>(values, comparer, left, right);
    IntroSort<T>(values, comparer, pivot + 1, right, depthLimit);
    right := pivot - 1;
  end;
end;

class procedure TArray.Sort<T>(var values: array of T);
begin
  IntroSort<T>(values, TComparer<T>.Default, Low(values), High(values), GetDepthLimit(Length(values)));
end;

class procedure TArray.Sort<T>(var values: array of T;
  const comparer: IComparer<T>);
begin
  IntroSort<T>(values, comparer, Low(values), High(values), GetDepthLimit(Length(values)));
end;

class procedure TArray.Sort<T>(var values: array of T;
  const comparer: IComparer<T>; index, count: Integer);
begin
  if (index < Low(values)) or ((index > High(values)) and (count > 0))
    or (index + count - 1 > High(values)) or (count < 0)
    or (index + count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if count <= 1 then
    Exit;
  IntroSort<T>(values, comparer, index, index + count - 1, GetDepthLimit(count));
end;

end.
