/// <summary>
/// This unit provides various classes implementing additional functionality
/// for collection-type classes.
/// </summary>
unit DX.Classes.Collections;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  /// <summary>
  /// TListHelper offers class methods extending TList&lt;T&gt;
  /// functionality.
  /// </summary>
  TListHelper<T> = class(TObject)
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
    /// "Paginates" the List by returing only the elements that belong to the given page.
    /// </summary>
    class procedure Paginate(AList: TList<T>; APage, APageSize: integer); static;
  end;

implementation

uses
  System.Math;

{ TListHelper<T> }


class procedure TListHelper<T>.Paginate(
  AList: TList<T>;
  APage, APageSize: integer);
begin
  if APage < 1 then
    raise Exception.Create('Page must be larger than 0!');

  if APageSize < 1 then
    raise Exception.Create('PageSize must be larger than 0!');

  var
  LCount := AList.Count;
  var
  LPages := LCount div APageSize;
  // APage/LPages are 1-based
  if (LCount mod APageSize) > 0 then
  begin
    inc(LPages);
  end;
  // APage := Min(APage, LPages);
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
