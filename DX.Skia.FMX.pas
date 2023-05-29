unit DX.Skia.FMX;

interface

uses
  System.Classes, System.SysUtils
{$IFDEF SKIA}
    , FMX.Types
    , Skia.FMX
{$ENDIF}
    ;

implementation

initialization
{$IFDEF SKIA}
  GlobalUseSkia := True;
  GlobalUseMetal := True;
{$ENDIF}

end.
