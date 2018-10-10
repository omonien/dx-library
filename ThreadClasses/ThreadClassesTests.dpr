program ThreadClassesTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  DX.Generics.ThreadSafe in '..\DX.Generics.ThreadSafe.pas',
  DX.Generics.ThreadSafe.Tests.Dictionary in 'DX.Generics.ThreadSafe.Tests.Dictionary.pas',
  DX.Generics.ThreadSafe.Tests in 'DX.Generics.ThreadSafe.Tests.pas',
  DX.Generics.ThreadSafe.Tests.ObjectDictionary in 'DX.Generics.ThreadSafe.Tests.ObjectDictionary.pas',
  DX.Generics.ThreadSafe.Tests.List in 'DX.Generics.ThreadSafe.Tests.List.pas',
  DX.Generics.ThreadSafe.Tests.ObjectList in 'DX.Generics.ThreadSafe.Tests.ObjectList.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  DUnitTestRunner.RunRegisteredTests;

end.
