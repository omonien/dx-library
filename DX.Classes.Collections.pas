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
    class procedure RemoveDuplicates(ASource: TList<T>);
  end;

implementation

{ TListHelper<T> }

class procedure TListHelper<T>.RemoveDuplicates(ASource: TList<T>);
begin
  if Assigned(ASource) then
  begin
    var
    LResult := TList<T>.Create;
    for var LItem in ASource do
    begin
      if not LResult.Contains(LItem) then
      begin
        LResult.Add(LItem);
      end;
    end;
    ASource.Clear;
    ASource.AddRange(LResult);
  end;
end;

end.
