object ServiceBase: TServiceBase
  OnCreate = ServiceCreate
  AllowPause = False
  DisplayName = 'Service Base'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 314
  Width = 464
  PixelsPerInch = 96
end
