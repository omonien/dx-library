/// <summary>
/// DX.FMX.Wait provides an easy to display a "Please Wait..." popup for FMX applications. It shows a turning wheel
/// and a message centered on the screen. It prevents interaction with controls currently showing behind it.
/// </summary>
unit DX.FMX.Wait;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Surfaces, FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts, FMX.Ani, FMX.Objects;

type
  TWait = class(TPanel)
  private
    class var FInstance: TWait;
  private
    FLabelWait: TLabel;
    FRectWait: TRectangle;
    FShowCount: Integer;
    FContainer: TLayout;
    FWheelAnimation: TBitmapListAnimation;
    FWheelBitmap: TBitmap;
    FWheelImage: TImage;
    function GetMessage: string;
    procedure SetMessage(const Value: string);
  protected
    property ShowCount: Integer read FShowCount write FShowCount;
  public
    constructor Create; reintroduce;
  public
    class procedure Start(const AMessage: string);
    class procedure Stop;
    class procedure EnableNetworkActivityIndicator(AEnable: Boolean); virtual;
    class procedure ShowNetworkActivityIndicator;
    class procedure HideNetworkActivityIndicator;
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
  LActiveForm: TCommonCustomForm;
begin
  LActiveForm := Screen.ActiveForm;
  if not Assigned(LActiveForm) then
  begin
    LActiveForm := Application.MainForm;
  end;

  Inherited Create(LActiveForm);
  Parent := LActiveForm;
  Position.x := 0;
  Position.y := 0;
  Width := LActiveForm.Width;
  Height := LActiveForm.Height;
  //We are now on top of all other controls we blur the background
  Opacity := 0.6;

  //Create a Layout, which will be on top - and which won't inherit opacity
  FContainer := TLayout.Create(self);
  FContainer.Parent := LActiveForm;
  FContainer.Position.X := 0;
  FContainer.Position.Y := 0;
  FContainer.Width := LActiveForm.Width;
  FContainer.Height := LActiveForm.Height;


  FRectWait := TRectangle.Create(Self);
  with FRectWait do
  begin
    Parent := FContainer;
    Align := TAlignLayout.Center;
    Height := 96.0;
    Width := 160.0;
    Fill.Color := TAlphaColorRec.Gray;
    Sides := [];
  end;

  FLabelWait := TLabel.Create(Self);
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

  FWheelImage := TImage.Create(Self);
  with FWheelImage do
  begin
    Parent := FRectWait;
    Height := 38.0;
    Width := 38.0;
    Position.x := 61.0;
    Position.Y := 8.0;
  end;

  LStream := TResourceStream.Create(HInstance, 'WHEEL', RT_RCDATA);
  FWheelBitmap := TBitmap.CreateFromStream(LStream);

  FWheelAnimation := TBitmapListAnimation.Create(Self);
  with FWheelAnimation do
  begin
    Parent := FWheelImage;
    AnimationBitmap := FWheelBitmap;
    PropertyName := 'Bitmap';
    AnimationCount := 8;
    Duration := 1.0;
    Loop := true;
    Enabled := false;
  end;

  FShowCount := 0;
end;

class procedure TWait.EnableNetworkActivityIndicator(AEnable: Boolean);
begin
{$IFDEF IOS}
  // Make this callable from anywhere
  TThread.Queue(nil,
    procedure
    begin
      TiOSHelper.SharedApplication.setNetworkActivityIndicatorVisible(AEnable);
    end);
{$ENDIF}
end;

class procedure TWait.ShowNetworkActivityIndicator;
begin
  TWait.EnableNetworkActivityIndicator(true);
end;

function TWait.GetMessage: string;
begin
  Result := FLabelWait.Text;
end;

class procedure TWait.HideNetworkActivityIndicator;
begin
  TWait.EnableNetworkActivityIndicator(false);
end;

procedure TWait.SetMessage(const Value: string);
begin
  FLabelWait.Text := Value;
end;

class procedure TWait.Start(const AMessage: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      if not Assigned(FInstance) then
      begin
        FInstance := TWait.Create;
      end;
      FInstance.FWheelAnimation.Enabled := true;
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
          // Todo: If showing multiple times, keep track of messages
          FInstance.ShowCount := FInstance.ShowCount - 1;
          if FInstance.ShowCount = 0 then
          begin
            FInstance.Hide;
            FInstance.FWheelAnimation.Enabled := false;
            FInstance.DisposeOf;
            FInstance := nil;
          end;
        end;
      end;
    end);
end;

end.
