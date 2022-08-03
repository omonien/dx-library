unit DX.Classes.Configuration.Intf;

interface

uses System.Classes, System.SysUtils, System.IniFiles;

type

  IConfiguration = interface(IInterface)
    ['{DED1A30D-4C03-4BD6-BC20-DCFC78FB0857}']
    function GetConfiguration: TCustomIniFile;
    property Configuration: TCustomIniFile read GetConfiguration;
  end;

implementation

end.
