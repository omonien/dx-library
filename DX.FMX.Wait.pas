{$REGION 'Documentation'}
/// <summary>
///   DX.FMX.Wait provides an easy to display a "Please Wait..." popup for FMX applications. It shows a turning wheel
///   and a message centered on the screen. It prevents interaction with controls currently showing behind it.
/// </summary>
/// <remarks>
///   <para>
///     DX.FMX.Wait is part of the DX.Library
///   </para>
///   <para>
///     Copyright (c)2018 <see href="http://www.developer-experts.net">Developer Experts</see><br /><br />Permission
///     is hereby granted, free of charge, to any person obtaining a copy of this software and associated
///     documentation files (the "Software"), to deal in the Software without restriction, including without
///     limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
///     Software, and to permit persons to whom the Software is furnished to do so, subject to the following
///     conditions: <br /><br />The above copyright notice and this permission notice shall be included in all copies
///     or substantial portions of the Software. <br /><br />THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
///     ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
///     PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
///     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
///     OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
///   </para>
/// </remarks>
{$ENDREGION}
unit DX.FMX.Wait;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Surfaces, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Ani, FMX.Objects;

type
  TWait = class(TCustomPopupForm)
  private
    class var FInstance: TWait;
  private
    FLabelWait: TLabel;
    FRectWait: TRectangle;
    FShowCount: Integer;
    FWheelAnimation: TBitmapListAnimation;
    FWheelBitmap: TBitmap;
    FWheelImage: TImage;
    function GetMessage: string;
    procedure SetMessage(const Value: string);
  protected
    constructor Create; reintroduce;
    destructor Destroy; reintroduce;
    property ShowCount: Integer read FShowCount write FShowCount;
  public
    class constructor Create;
    class procedure Start(const AMessage: string);
    class procedure Stop;
    class procedure EnableNetworkActivityIndicator(AEnable: Boolean);
    property Message: string read GetMessage write SetMessage;
  end;

implementation

uses
  System.IOUtils, System.UIConsts, DX.CrossPlatform
{$IFDEF iOS}
    , iOSapi.Helpers
{$ENDIF}
    , FMX.Platform;
{$R wheel.res}

constructor TWait.Create;
var
  LStream: TResourceStream;
  LColor: TAlphaColorRec;
begin
  // Important: due to an open issue, we need to set FormStyle to be Popup, before calling teh actual constructor
  // Setting FormStyle does some Voodoo stuff though, which requires an already existing FWinService
  // See: https://quality.embarcadero.com/browse/RSP-21414
  if not TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, FWinService) then
    raise EUnsupportedPlatformService.Create('IFMXWindowService');
  FormStyle := TFormStyle.Popup;
  inherited CreateNew(nil);
  // Little inconsistency in FMX: A TCustomPopupForm's size is NOT defined by width and height!
  // Even though we specify a sice, the popup itself will be covering the whole screen in the end.
  Size := TSizeF.Create(160, 96);
  // We want the Wait Popup to be centered over the screen, so we need a proper PlacementTarget
  // Unfortunately the Screen itself cannot be set as such a target, but we can specify a target's
  // rectangle alternatively

  // In mobile applications, we want to cover the whole screen, for desktop apps we
  // rather cover the current form
  if not(IsMobilePlatform) and Assigned(Screen.ActiveForm) then
  begin
    PlacementRectangle.Left := Screen.ActiveForm.Left;
    PlacementRectangle.Top := Screen.ActiveForm.Top;
    PlacementRectangle.Right := PlacementRectangle.Left + Screen.ActiveForm.Width;
    PlacementRectangle.Bottom := PlacementRectangle.Top + Screen.ActiveForm.Height;
  end
  else
  begin
    PlacementRectangle.Left := 0;
    PlacementRectangle.Top := 0;
    PlacementRectangle.Right := Screen.Size.Width;
    PlacementRectangle.Bottom := Screen.Size.Height;
  end;
  Padding.Left := 0;
  Padding.Right := 0;
  Padding.Top := 0;
  Padding.Bottom := 0;

  // We want it centered
  Placement := TPlacement.Center;
  Transparency := True;

  FRectWait := TRectangle.Create(self);
  with FRectWait do
  begin
    Parent := self;
    Align := TAlignLayout.Center;
    Height := 96.0;
    Width := 160.0;
    Fill.Color := TAlphaColorRec.Gray;
    Sides := [];
  end;

  FLabelWait := TLabel.Create(self);
  with FLabelWait do
  begin
    Parent := FRectWait;
    Align := TAlignLayout.Bottom;
    Height := 50.0;
    Width := 152.0;
    Margins.Left := 4.0;
    Margins.Top := 4.0;
    Margins.Right := 4.0;
    Margins.Bottom := 4.0;
    StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
    TextSettings.FontColor := TAlphaColorRec.White;
    TextSettings.HorzAlign := TTextAlign.Center;
    Text := '';
  end;

  FWheelImage := TImage.Create(self);
  with FWheelImage do
  begin
    Parent := FRectWait;
    Height := 38.0;
    Width := 38.0;
    Position.X := 61.0;
    Position.Y := 8.0;
  end;

  LStream := TResourceStream.Create(HInstance, 'WHEEL', RT_RCDATA);
  FWheelBitmap := TBitmap.CreateFromStream(LStream);

  FWheelAnimation := TBitmapListAnimation.Create(self);
  with FWheelAnimation do
  begin
    Parent := FWheelImage;
    AnimationBitmap := FWheelBitmap;
    PropertyName := 'Bitmap';
    AnimationCount := 8;
    Duration := 1.0;
    Loop := True;
    Enabled := True;
  end;

  FShowCount := 0;
end;

class constructor TWait.Create;
begin
  FInstance := nil;
end;

destructor TWait.Destroy;
begin
  FreeAndNil(FLabelWait);
  FreeAndNil(FWheelAnimation);
  FreeAndNil(FWheelImage);
  FreeAndNil(FWheelBitmap);
  inherited;
end;

class procedure TWait.EnableNetworkActivityIndicator(AEnable: Boolean);
begin
{$IFDEF IOS}
  //Make this callable from anywhere
  TThread.Queue(nil,
    procedure
    begin
      TiOSHelper.SharedApplication.setNetworkActivityIndicatorVisible(AEnable);
    end);
{$ENDIF}
end;

function TWait.GetMessage: string;
begin
  Result := FLabelWait.Text;
end;

procedure TWait.SetMessage(const Value: string);
begin
  FLabelWait.Text := Value;
end;

class procedure TWait.Start(const AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if not Assigned(FInstance) then
      begin
        FInstance := TWait.Create;
      end;
      FInstance.FWheelAnimation.Enabled := True;
      FInstance.ShowCount := FInstance.ShowCount + 1;
      FInstance.Message := AMessage;
      FInstance.Show;
      // Make sure the dialog will be shown - even if the main thread will be busy afterwards
      Application.ProcessMessages;
    end);
end;

class procedure TWait.Stop;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FInstance) then
      begin
        if FInstance.ShowCount > 0 then
        begin
          FInstance.ShowCount := FInstance.ShowCount - 1;
          if FInstance.ShowCount = 0 then
          begin
            FInstance.Close;
            FInstance.FWheelAnimation.Enabled := False;
          end;
          // We could keep the instance - but every byte counts on mobile devices ;-)
          FInstance.DisposeOf;
          FInstance := nil;
        end;
      end;
    end);
end;

end.
