/// <summary>
/// This unit provides helper functions for use under iOS and MacOSX.
/// </summary>
/// <remarks>
/// <para>
/// This library also imports certain classes or parts of them which are
/// currently not available as imported by EMBT
/// </para>
/// <para>
/// The code is intended to be used with Delphi XE4 only. Other versions
/// may or may not work.
/// </para>
/// <para>
/// There is a dependency to Apple.Utils.pas, which ships with XE4 and can
/// usually be found here: C:\Users\Public\Documents\RAD
/// Studio\11.0\Samples\Delphi\RTL\CrossPlatform Utils
/// </para>
/// <para>
/// Make sure use the most recent version - that samples folder above is
/// connected to an SVN repository of Embarcaderos'.
/// </para>
/// <para>
/// This library is licensed under the terms of The MIT License
/// </para>
/// <para>
/// Copyright (c) 2013 Developer Experts, LLC
/// </para>
/// <para>
/// <see href="http://www.developer-experts.net">www.developer-experts.net</see>
/// </para>
/// <para>
/// Permission is hereby granted, free of charge, to any person obtaining a
/// copy of this software and associated documentation files (the
/// "Software"), to deal in the Software without restriction, including
/// without limitation the rights to use, copy, modify, merge, publish,
/// distribute, sublicense, and/or sell copies of the Software, and to
/// permit persons to whom the Software is furnished to do so, subject to
/// the following conditions: The above copyright notice and this
/// permission notice shall be included in all copies or substantial
/// portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT
/// WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
/// THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
/// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
/// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
/// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
/// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
/// </para>
/// </remarks>
unit DX.Apple.Utils;

interface

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
function CanOpenURL(AURL: string): boolean;

{$ENDIF IOS}

implementation

uses
  System.SysUtils,
  Apple.Utils,
  // Apple.Utils.pas ships with XE4 & XE5 and can usually be found here:
  // C:\Users\Public\Documents\RADStudio\11.0\Samples\Delphi\RTL\CrossPlatform Utils
  // C:\Users\Public\Documents\RAD Studio\12.0\Samples\Delphi\RTL\CrossPlatform Utils

{$IFDEF IOS}
  Macapi.ObjectiveC,
  iOSApi.Foundation,
  iOSApi.UIKit,
  iOSApi.QuartzCore,
  iOSApi.CocoaTypes
{$ELSE}
{$IFDEF MACOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  Macapi.Foundation
{$ENDIF MACOS}
{$ENDIF IOS}
    ;

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
type PNSString = Pointer;

const
  libFoundation = '/System/Library/Frameworks/Foundation.framework/Foundation';

procedure NSLog(format: PNSString); cdecl; varargs; external libFoundation name _PU + 'NSLog';
{$ENDIF MACOS}
{$ENDIF IOS}

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
  LMessage := NSSTR(LTimeStamp + AMessage);
  NSLog(PtrForObject(LMessage));
end;

{$IFDEF IOS}

// Don't expose this - its just a partial import
function currentDevice: DX.Apple.Utils.UIDevice;
begin
  result := TUIDeviceDX.Wrap(TUIDeviceDX.OCClass.currentDevice);
end;

function VendorIdentifier: string;
var
  LDevice: DX.Apple.Utils.UIDevice;
begin
  LDevice := currentDevice;
  result := NSStringToString(LDevice.identifierForVendor.UUIDString);
end;

function CanOpenURL(AURL: string): boolean;
begin
  result := SharedApplication.CanOpenURL(StringToNSUrl(AURL));
end;
{$ENDIF}

end.
