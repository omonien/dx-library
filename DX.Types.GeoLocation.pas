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

type
  TGeoLocation = class(TObject)
  type
    TGeography = class(TObject)
    private
      procedure SetWellKnownText(const Value: string);
      function GetWellKnownText: string;
    public
      CoordinateSystemId: Integer;
      Latitude: double;
      Longitude: double;

      property WellKnownText: string read GetWellKnownText write SetWellKnownText;
      constructor Create;
    end;
  private
  public
    Geography: TGeography;
    constructor Create;
  end;

implementation

uses
  SysUtils, Classes;

{ TGeoLocation }

constructor TGeoLocation.Create;
begin
  Geography := TGeography.Create;
end;

{ TGeoLocation.TGeography }

constructor TGeoLocation.TGeography.Create;
begin
  Longitude := 0.0;
  Latitude := 0.0;
  CoordinateSystemId := 4326; // WGS8
end;

function TGeoLocation.TGeography.GetWellKnownText: string;
begin
  result := 'POINT(' + FloatToStr(Latitude) + ' ' + FloatToStr(Longitude) + ')';
end;

procedure TGeoLocation.TGeography.SetWellKnownText(const Value: string);
var
  s: string;
  LCoords: TArray<string>;
  LFormat: TFormatSettings;
begin
  // POINT(50.130125 8.75081)
  s := Value.Trim;
  Delete(s, 1, 6); // 50.130125 8.75081)
  Delete(s, s.Length, 1); // 50.130125 8.75081
  LCoords := s.Split([' ']);
  if Length(LCoords) <> 2 then
    raise Exception.Create('TGeoLocation: Invalid WellKnownText! Supported format: POINT(50.130125 8.75081)');
  try
    LFormat.DecimalSeparator := '.';
    Latitude := StrToFloat(LCoords[0], LFormat);
    Longitude := StrToFloat(LCoords[1], LFormat);
  except
    raise Exception.Create('TGeoLocation: Invalid WellKnownText! Supported format: POINT(50.130125 8.75081)');
  end;
end;

end.
