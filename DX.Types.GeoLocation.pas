unit DX.Types.GeoLocation;
/// <summary>
/// DX.Types.GeoLocation provides a GeoLocation data type which can be mapped to
/// SQL Server's Geography type
/// </summary>
/// <remarks>
/// DX.Types.GeoLocation is part of DX.Librayry<br />See:
/// http://code.google.com/p/dx-library/
/// </remarks>

interface

uses
  SysUtils, Classes;

type
  TGeoLocation = class(TObject)
  type

    //Only used internally
    TGeography = class(TObject)
    private
      FFormat: TFormatSettings;
      FLatitude: double;
      FLongitude: double;
      procedure CalcLatLong;
      procedure CalcWellKnownText;
      procedure SetLatitude(const Value: double);
      procedure SetLongitude(const Value: double);
      function GetLatitude: double;
      function GetLongitude: double;
    public
      CoordinateSystemId: Integer;
      WellKnownText: string;
      property Latitude: double read GetLatitude write SetLatitude;
      property Longitude: double read GetLongitude write SetLongitude;
      constructor Create;
    end;
  private
    //This is only used internally to map SQL Servers Geography Type via JSON
    Geography: TGeography;

    function GetLatitude: double;
    function GetLongitude: double;
    procedure SetLatitude(const Value: double);
    procedure SetLongitude(const Value: double);
  public
    property Latitude: double read GetLatitude write SetLatitude;
    property Longitude: double read GetLongitude write SetLongitude;
    constructor Create;
  end;

implementation

{ TGeoLocation }

constructor TGeoLocation.Create;
begin
  inherited;
  Geography := TGeography.Create;
end;

function TGeoLocation.GetLatitude: double;
begin
  result := Geography.Latitude;
end;

function TGeoLocation.GetLongitude: double;
begin
  result := Geography.Longitude;
end;

procedure TGeoLocation.SetLatitude(const Value: double);
begin
  Geography.Latitude := Value;
end;

procedure TGeoLocation.SetLongitude(const Value: double);
begin
  Geography.Longitude := Value;
end;

{ TGeoLocation.TGeography }

constructor TGeoLocation.TGeography.Create;
begin
  FFormat := TFormatSettings.Create;
  FFormat.DecimalSeparator := '.';
  FFormat.ThousandSeparator := ',';
  Longitude := 0.0;
  Latitude := 0.0;
  CoordinateSystemId := 4326; // WGS8
end;

function TGeoLocation.TGeography.GetLatitude: double;
begin
  CalcLatLong;
  result := FLatitude;
end;

function TGeoLocation.TGeography.GetLongitude: double;
begin
  CalcLatLong;
  result := FLongitude;
end;

procedure TGeoLocation.TGeography.CalcWellKnownText;
begin

  WellKnownText := 'POINT(' + FloatToStr(FLatitude, FFormat) + ' ' + FloatToStr(FLongitude, FFormat) + ')';
end;

procedure TGeoLocation.TGeography.SetLatitude(const Value: double);
begin
  FLatitude := Value;
  CalcWellKnownText;
end;

procedure TGeoLocation.TGeography.SetLongitude(const Value: double);
begin
  FLongitude := Value;
  CalcWellKnownText;
end;

procedure TGeoLocation.TGeography.CalcLatLong;
var
  s: string;
  LCoords: TArray<string>;
begin
  // POINT(50.130125 8.75081)
  s := WellKnownText.Trim;
  Delete(s, 1, pos('(', s)); // 50.130125 8.75081)
  Delete(s, s.Length, 1); // 50.130125 8.75081
  LCoords := s.Split([' ']);
  if Length(LCoords) <> 2 then
    raise Exception.Create('TGeoLocation: Invalid WellKnownText! Supported format: POINT(50.130125 8.75081)');
  try
    FLatitude := StrToFloat(LCoords[0], FFormat);
    FLongitude := StrToFloat(LCoords[1], FFormat);
  except
    raise Exception.Create('TGeoLocation: Invalid WellKnownText! Supported format: POINT(50.130125 8.75081)');
  end;
end;

end.
