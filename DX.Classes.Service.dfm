object ServiceBase: TServiceBase
  OldCreateOrder = True
  OnCreate = ServiceCreate
  AllowPause = False
  DisplayName = 'Service Base'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 314
  Width = 464
end
