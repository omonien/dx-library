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
    /// <param name="ASource">
    /// A list containing instances of type T.
    /// </param>
    class procedure RemoveDuplicates(AList: TList<T>);
  end;

implementation

{ TListHelper<T> }

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
