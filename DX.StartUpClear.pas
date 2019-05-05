/// <summary>
///   This unit implements a mechanism that complements System.StartUpCopy.
///   StartUpCopy copies all bundled assets (as configured in deployment
///   manager) to the app's location on the target device. Important to know is
///   that StartUpCopy never overwrites existing files. <br /><br />If you want
///   that (with new installations) all (updated) assets are really copied to
///   the app's location, then place DX.StartUpClear BEFORE System.StartUpCopy
///   in the DPR file.
/// </summary>
/// <remarks>
///   StartUpClear will delete all assets in the App's library directory.
/// </remarks>
unit DX.StartUpClear;

interface

implementation

uses
  System.IOUtils, System.Types;

/// <summary>
///   Deletes all files in ./library
/// </summary>
/// <remarks>
///   Does NOT delete subdirectories
/// </remarks>
procedure ClearLibrary;
var
  LLibraryPath: string;
  LFilesToDelete: TStringDynArray;
  LFile: string;
begin
  LLibraryPath := TPath.GetLibraryPath;
  LFilesToDelete := TDirectory.GetFiles(LLibraryPath);
  for LFile in LFilesToDelete do
  begin
    TFile.Delete(LFile);
  end;
end;

initialization

{$IF Defined(IOS)}
  ClearLibrary;
{$ENDIF}

end.
