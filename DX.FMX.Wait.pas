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
    FRectWait: TRectangle;
    FShowCount: Integer;
    FWheelAnimation: TBitmapListAnimation;
    FWheelBitmap: TBitmap;
    FWheelImage: TImage;
    function GetMessage: string;
    procedure SetMessage(const Value: string);
  protected
    constructor Create;
    destructor Destroy; override;
    property ShowCount: Integer read FShowCount write FShowCount;
  public
    class procedure Start(AMessage: string);
    class procedure Stop;
    property Message: string read GetMessage write SetMessage;
  end;

implementation

uses
  System.IOUtils;
{$R wheel.res}

var
  GInstance: TWait = nil;

constructor TWait.Create;
var
  LStream: TResourceStream;

begin
  inherited;
  FPopup := TCustomPopupForm.Create(nil, nil, nil);
  with FPopup do
  begin
    // Little inconsistency in FMX: A TCustomPopupForm's size is NOT defined by width and height!
    Size := TSizeF.Create(160, 96);
    // We want the Wait Popup to be centered over the screen, which FMX calls a PlacementTarget
    // Unfortunately the Screen itself cannot be set as such a target, but we can specify a target's
    // rectangle alternatively
    PlacementRectangle.Left := 0;
    PlacementRectangle.Top := 0;
    PlacementRectangle.Right := Screen.Size.Width;
    PlacementRectangle.Bottom := Screen.Size.Height;
    // We want it centered
    Placement := TPlacement.Center;
    Transparency := true;
    FormStyle := TFormStyle.StayOnTop;
  end;

  FRectWait := TRectangle.Create(FPopup);
  with FRectWait do
  begin
    // Name := 'RectWait';
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
    // Name := 'LabelWait';
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
    // Name := 'WheelImage';
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
    // Name := 'WheelAnimation';
    Parent := FWheelImage;
    AnimationBitmap := FWheelBitmap;
    PropertyName := 'Bitmap';
    AnimationCount := 8;
    Duration := 1.0;
    Loop := True;
    Enabled := True;
  end;

  // if FileExists(TPath.Combine(TPath.GetDocumentsPath, 'wheel.png')) then
  // FWheelBitmap := TBitmap.CreateFromFile(TPath.Combine(TPath.GetDocumentsPath, 'wheel.png'));

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

procedure TWait.SetMessage(const Value: string);
begin
  FLabelWait.Text := Value;
end;

class procedure TWait.Start(AMessage: string);
begin
  if not Assigned(GInstance) then
  begin
    GInstance := TWait.Create;
  end;

  GInstance.ShowCount := GInstance.ShowCount + 1;
  GInstance.Message := AMessage;

  GInstance.FPopup.Show;
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
