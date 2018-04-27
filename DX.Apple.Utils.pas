{$REGION 'Documentation'}
/// <summary>
///   This unit provides helper functions for use under iOS and macOS.
/// </summary>
/// <remarks>
///   <para>
///     There is a dependency to Apple.Utils.pas, which ships with XE4 and
///     can usually be found here: C:\Users\Public\Documents\RAD
///     Studio\11.0\Samples\Delphi\RTL\CrossPlatform Utils
///   </para>
///   <para>
///     Make sure use the most recent version - that samples folder above is
///     connected to an SVN repository of Embarcadero's.
///   </para>
/// </remarks>
{$ENDREGION}
unit DX.Apple.Utils;

interface

uses
  System.SysUtils,
{$IF Defined(IOS)}
  iOSApi.Foundation;
{$ELSEIF Defined(MACOS)}
Macapi.Foundation;
{$ENDIF}
/// <summary>
/// Logs to the console
/// </summary>
/// <param name="AMessage">
/// Message to be logged
/// </param>
/// <remarks>
/// <para>
/// Named NSLog2 to avoid name clash with original NSLog in Foundation.
/// </para>
/// <para>
/// iOS device: Xcode - Organizer -&gt; Device - Console
/// </para>
/// <para>
/// Mac / iOS Simulator: Mac - Console
/// </para>
/// </remarks>
procedure NSLog2(const AMessage: string; AAddTimeStamp: boolean = false);

/// <summary>
/// Raises an Exception if ANSError is <> nil
/// </summary>
procedure RaiseOnNSError(ANSError: Pointer);

/// <summary>
/// Performs the magic to cast an NSObject to an iOS/Mac pointer, as required by some Cocoa functions
/// </summary>
function NSObjectToPointer(AObject: NSObject): Pointer;

{$IFDEF IOS}
/// <summary>
/// Retrieves the vendor specific device ID - DO NOT USE UIDevice.uniqueIdentifier - this would lead to AppStore rejection since May 1st, 2013!
/// </summary>
function VendorIdentifier: string;

/// <summary>
/// checks if the given URL can be openend by any App on the device
/// This is for custom URL schemes like fb://profile/omonien, which would open
/// the Facebook app - if installed on the device - and navigate to the given profile
/// </summary>
function CanOpenURL(const AURL: string): boolean;

/// <summary>
/// Marks the given file not to be backed up by iOS.
/// See  https://developer.apple.com/library/ios/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW4
/// </summary>
procedure ExcludeFromBackup(const APath: string);

{$ENDIF IOS}

implementation

uses
{$IF Defined(IOS) or Defined(MACOS)}
  Macapi.ObjectiveC,
  Macapi.Helpers,
{$ENDIF}
{$IF Defined(IOS)}
  iOSApi.UIKit,
  iOSApi.QuartzCore,
  iOSApi.CocoaTypes,
  FMX.Helpers.iOS
{$ELSEIF Defined(MACOS)}
  Macapi.ObjCRuntime
{$ENDIF}
    ;

type
  /// <summary>
  /// Exception class which is use to raise Exceptions originating from NSErrors
  /// </summary>
  ENSError = class(Exception);

  // For better readability / portability
  PNSString = Pointer;
  id = Pointer;

const
  libFoundation = '/System/Library/Frameworks/Foundation.framework/Foundation';

{$IFDEF IOS}

  // Hack to import forgotten classes/functions and properties
  // Be careful - classes with same name may already exist in iOSApi!!
type

  // **** NSUUID

  NSUUIDClass = interface(NSObjectClass)
    ['{D9518F5E-DDBC-4702-A555-411D32B85340}']
  end;

  // We just need  the UUIDString here
  NSUUID = interface(NSObject)
    ['{4C137FF5-E854-461F-B77E-8CD357FD4E9C}']
    function UUIDString: NSString; cdecl;
  end;

  TNSUUIDDX = class(TOCGenericImport<NSUUIDClass, NSUUID>)
  end;

  // **** UIDevice

  UIDeviceClass = interface(NSObjectClass)
    ['{D5105207-FBA7-4F55-BC7B-1ADACE347ECA}']
    { class } function currentDevice: Pointer; cdecl;
  end;

  UIDevice = interface(NSObject)
    ['{481E431F-2C02-4F2D-86C5-7728480ECF48}']
    function identifierForVendor: NSUUID; cdecl;
  end;

  TUIDeviceDX = class(TOCGenericImport<UIDeviceClass, UIDevice>)
  end;
{$ELSE}
{$IFDEF MACOS}

procedure NSLog(format: PNSString); cdecl; varargs; external libFoundation name _PU + 'NSLog';

{$ENDIF MACOS}
{$ENDIF IOS}

function NSObjectToPointer(AObject: NSObject): Pointer;
begin
  Result := (AObject as ILocalObject).GetObjectID;
end;

procedure NSLog2(const AMessage: string; AAddTimeStamp: boolean = false);
var
  LMessage: NSString;
  LTimeStamp: String;
begin
  // NSLog actually already logs the current time - but not including milliseconds, which is why we add a "full" timestamp here
  if AAddTimeStamp then
    LTimeStamp := '(' + FormatDateTime('hh:nn:ss,zzz', now) + ') - '
  else
    LTimeStamp := '';
  LMessage := StrToNSStr(LTimeStamp + AMessage);
  NSLog(NSObjectToPointer(LMessage));
end;

procedure RaiseOnNSError(ANSError: Pointer);
var
  LError: NSError;
  LMsg: string;
begin
  if ANSError <> nil then
  begin
    LError := TNSError.Wrap(ANSError);
    LMsg := NSStrToStr(LError.LocalizedDescription);
    raise ENSError.Create(LMsg);
  end;
end;

{$IFDEF IOS}

procedure ExcludeFromBackup(const APath: string);
var
  FileManager: NSFileManager;
  Url: NSURL;
  LNSError: Pointer;
begin
  FileManager := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  if FileExists(APath) then
  begin
    Url := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(APath)));

    // https://developer.apple.com/library/ios/qa/qa1719/_index.html
    if not Url.setResourceValue(id(TNSNumber.OCClass.numberWithBool(True)),
      CocoaNSStringConst(libFoundation, 'NSURLIsExcludedFromBackupKey'), @LNSError) then
      RaiseOnNSError(LNSError);
  end;
end;

// Don't expose this - its just a partial import
function currentDevice: DX.Apple.Utils.UIDevice;
begin
  Result := TUIDeviceDX.Wrap(TUIDeviceDX.OCClass.currentDevice);
end;

function VendorIdentifier: string;
var
  LDevice: DX.Apple.Utils.UIDevice;
begin
  LDevice := currentDevice;
  Result := NSStrToStr(LDevice.identifierForVendor.UUIDString);
end;

function CanOpenURL(const AURL: string): boolean;
begin
  Result := SharedApplication.CanOpenURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(AURL))));
end;
{$ENDIF}

end.
