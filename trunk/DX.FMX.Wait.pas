{$REGION 'Documentation'}
/// <summary>
/// DX.FMX.Wait provides an easy to use "Please Wait..." dialog for FMX applications
/// It shows a turning wheel and a message centered on the screen. It
/// prevents interaction with controls currently showing behind it.
/// </summary>
/// <remarks>
/// <para>
/// DX.FMX.Wait is part of DX.Library
/// </para>
/// <para>
/// See: <see href="http://code.google.com/p/dx-library/" />
/// </para>
/// </remarks>
/// <example>
/// <para>
/// uses
/// </para>
/// <para>
/// DX.FMX.Wait;
/// </para>
/// <para>
/// TWait.Start('Loading data ...');
/// ...
/// TWait.Stop;
/// </para>
/// </example>
{$ENDREGION}
unit DX.FMX.Wait;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Surfaces, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Ani, FMX.Objects;

type
  TWait = class(TObject)
  private
    FLabelWait: TLabel;
    FPopup: TCustomPopupForm;
    FRectDim: TRectangle;
    FRectWait: TRectangle;
    FShowCount: Integer;
    FWheelAnimation: TBitmapListAnimation;
    FWheelBitmap: TBitmap;
    FWheelImage: TImage;
    FDim: Boolean;
    function GetMessage: string;
    procedure SetMessage(const Value: string);
    procedure SetDim(const Value: Boolean);
  protected
    constructor Create;
    destructor Destroy; reintroduce;
    property ShowCount: Integer read FShowCount write FShowCount;
  public
    class procedure Start(AMessage: string; ADimBackground: Boolean = False);
    class procedure Stop;
    property Message: string read GetMessage write SetMessage;
    property Dim: Boolean read FDim write SetDim;
  end;

implementation

uses
  System.IOUtils, System.UIConsts, DX.CrossPlatform;
{$R wheel.res}

var
  GInstance: TWait = nil;

constructor TWait.Create;
var
  LStream: TResourceStream;
  LColor: TAlphaColorRec;
begin
  inherited;
  FPopup := TCustomPopupForm.Create(nil, nil, nil);
  with FPopup do
  begin
    // Little inconsistency in FMX: A TCustomPopupForm's size is NOT defined by width and height!
    // Even though we specify a sice, the popup itself will be covering the whole screen in the end.
    Size := TSizeF.Create(160, 96);
    // We want the Wait Popup to be centered over the screen, which FMX calls a PlacementTarget
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
    Transparency := true;
    FormStyle := TFormStyle.StayOnTop;
  end;

  // In order to dim the background a semi-transparent, white rectangle is used
  FRectDim := TRectangle.Create(FPopup);
  with FRectDim do
  begin
    Parent := FPopup;
    Align := TAlignLayout.Client;
    LColor := TAlphaColorRec.Create(TAlphaColorRec.White);
    LColor.A := 128;
    Fill.Color := LColor.Color;
    Sides := [];
  end;

  FRectWait := TRectangle.Create(FRectDim);
  with FRectWait do
  begin
    Parent := FPopup;
    Align := TAlignLayout.Center;
    Height := 96.0;
    Width := 160.0;
    Fill.Color := TAlphaColorRec.Gray;
    Sides := [];
  end;

  FLabelWait := TLabel.Create(FPopup);
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
    TextSettings.FontColor := TAlphaColorRec.Beige;
    TextSettings.HorzAlign := TTextAlign.Center;
    Text := '';
  end;

  FWheelImage := TImage.Create(FPopup);
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

  FWheelAnimation := TBitmapListAnimation.Create(FPopup);
  with FWheelAnimation do
  begin
    Parent := FWheelImage;
    AnimationBitmap := FWheelBitmap;
    PropertyName := 'Bitmap';
    AnimationCount := 8;
    Duration := 1.0;
    Loop := true;
    Enabled := true;
  end;

  FShowCount := 0;
end;

destructor TWait.Destroy;
begin
  FreeAndNil(FLabelWait);
  FreeAndNil(FWheelAnimation);
  FreeAndNil(FWheelImage);
  FreeAndNil(FWheelBitmap);
  FreeAndNil(FPopup);
  inherited;
end;

function TWait.GetMessage: string;
begin
  Result := FLabelWait.Text;
end;

procedure TWait.SetDim(const Value: Boolean);
var
  LColor: TAlphaColorRec;
begin
  FDim := Value;
  LColor := TAlphaColorRec.Create(TAlphaColorRec.White);
  if FDim then
    LColor.A := 128
  else
    LColor.A := 0;
  FRectDim.Fill.Color := LColor.Color;
end;

procedure TWait.SetMessage(const Value: string);
begin
  FLabelWait.Text := Value;
end;

class procedure TWait.Start(AMessage: string; ADimBackground: Boolean = False);
begin
  Assert(TThread.CurrentThread.ThreadID = MainThreadID, 'TWait.Start must be called from the application''s main thread');

  if not Assigned(GInstance) then
  begin
    GInstance := TWait.Create;
  end;

  GInstance.ShowCount := GInstance.ShowCount + 1;
  GInstance.Message := AMessage;
  GInstance.Dim := ADimBackground;
  GInstance.FPopup.Show;
  //Make sure the dialog will be shown - even if the main thread will be busy afterwards
  Application.ProcessMessages;
end;

class procedure TWait.Stop;
begin
  if Assigned(GInstance) then
  begin
    if GInstance.ShowCount > 0 then
    begin
      GInstance.ShowCount := GInstance.ShowCount - 1;
      if GInstance.ShowCount = 0 then
        GInstance.FPopup.Hide;
      // We could be lazy and not destroy - but every byte counts on mobile devices ;-)
      FreeAndNil(GInstance);
    end;
  end;
end;

end.
