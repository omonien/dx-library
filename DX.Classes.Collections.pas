/// <summary>
/// This unit provides various classes implementing additional functionality
/// for collection-type classes.
/// </summary>
unit DX.Classes.Collections;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Defaults, System.Generics.Collections, System.RTTI;

type
  /// <summary>
  /// TListHelper offers class methods extending TList&lt;T&gt;
  /// functionality.
  /// </summary>
  TListHelper<T> = class(TObject)
  protected
    class function CompareByProperty(Item1, Item2: T; Const APropertyName: string): integer;

  public
    /// <summary>
    /// RemoveDuplicates removes duplicates (in terms of instances of T) from
    /// ASource. Its implementation follows a simple lookup approach and may
    /// behave O(n^2).
    /// </summary>
    /// <param name="AList">
    /// A list containing instances of type T.
    /// </param>
    class procedure RemoveDuplicates(AList: TList<T>);

    /// <summary>
    ///   "Paginates" the List by returing only the elements that belong to the
    ///   given page.
    /// </summary>
    /// <returns>
    ///   Number of total pages
    /// </returns>
    class function Paginate(AList: TList<T>; APage, APageSize: integer): integer; static;
  end;

implementation

uses
  System.Math;

{ TListHelper<T> }

class function TListHelper<T>.CompareByProperty(Item1, Item2: T; Const APropertyName: string): integer;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Ctx := TRttiContext.Create;
  try
    var
    LType := Ctx.GetType(TypeInfo(T));
    Prop := LType.GetProperty(APropertyName);

    if LType.IsRecord then
      raise ENotImplemented.Create('CompareByProperty() not implemented for Records');
    var
    LItem1 := TObject(Pointer(Addr(Item1))^);
    var
    LItem2 := TObject(Pointer(Addr(Item2))^);
    Result := CompareText(Prop.GetValue(LItem1).AsString, Prop.GetValue(LItem2).AsString);

  finally
    Ctx.Free;
  end;
end;

class procedure TListHelper<T>.SortByProperty(List: TList<T>; const APropertyName: string);
var
  Comparer: IComparer<T>;
begin
  Comparer := TComparer<T>.Construct(
    function(const Left, Right: T): integer
    begin
      Result := CompareByProperty(Left, Right, APropertyName);
    end);
  List.Sort(Comparer);
end;

class function TListHelper<T>.Paginate(AList: TList<T>; APage, APageSize: integer): integer;
begin
  if APage < 1 then
    raise Exception.Create('Page must be larger than 0!');

  if APageSize < 1 then
    raise Exception.Create('PageSize must be larger than 0!');

  var
  LCount := AList.Count;
  var
  LTotalPages := LCount div APageSize;
  // APage/LTotalPages are 1-based
  if (LCount mod APageSize) > 0 then
  begin
    inc(LTotalPages);
  end;
  // APage := Min(APage, LTotalPages);
  // LFrom is 0-based
  var
  LFrom := (APage - 1) * APageSize;

  // Alle Elemente vor LFrom entfernen
  AList.DeleteRange(0, Min(AList.Count, LFrom));
  // Alle Elemente nach einer PageSize entfernen
  if AList.Count > APageSize then
  begin
    AList.DeleteRange(APageSize, AList.Count - APageSize);
  end;
  result := LTotalPages;
end;



class procedure TListHelper<T>.RemoveDuplicates(AList: TList<T>);
begin
  if Assigned(AList) then
  begin
    var LTempList := TList<T>.Create;
    try
      for var LItem in AList do
      begin
        if not LTempList.Contains(LItem) then
        begin
          LTempList.Add(LItem);
        end;
      end;
      AList.Clear;
      AList.AddRange(LTempList);
    finally
      FreeAndNil(LTempList);
    end;
  end;
end;

end.
