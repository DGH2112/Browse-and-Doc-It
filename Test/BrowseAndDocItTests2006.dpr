﻿program BrowseAndDocItTests2006;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

{%TogetherDiagram 'ModelSupport_BrowseAndDocItTests2006\default.txaPackage'}
{%File '..\..\..\LIBRARY\CompilerDefinitions.inc'}

uses
  ExceptionLog,
  SysUtils,
  Forms,
  Windows,
  TestFramework,
  GUITestRunner,
  JclDebug,
  TextTestRunner,
  dghlibrary in '..\..\..\LIBRARY\dghlibrary.pas',
  TestDGHLibrary in '..\..\..\LIBRARY\Test\Source\TestDGHLibrary.pas',
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  TestBaseLanguageModule in '..\..\..\Library\Test\Source\TestBaseLanguageModule.pas',
  VBModule in '..\..\..\LIBRARY\VBModule.pas',
  TestVBModule in '..\..\..\Library\Test\Source\TestVBModule.pas',
  ModuleDispatcher in '..\..\..\LIBRARY\ModuleDispatcher.pas',
  PascalModule in '..\..\..\LIBRARY\PascalModule.pas',
  TestPascalModule in '..\..\..\LIBRARY\Test\Source\TestPascalModule.pas',
  CommonIDEFunctions in '..\Source\CommonIDEFunctions.pas',
  TestCommonIDEFunctions in '..\Source\Tests\Source\TestCommonIDEFunctions.pas';

{$R *.RES}

Var
  T : TTestResult;
  iErrors : Integer;
  lpMode : Cardinal;

begin
  {$IFDEF D2006}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$ENDIF}
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  Application.Initialize;
  If IsConsole Then
    Begin
      JclDebug.RemoveIgnoredException(EAbort);
      T := TextTestRunner.RunRegisteredTests;
      Try
        iErrors := T.FailureCount + T.ErrorCount;
      Finally
        T.Free;
      End;
      If DebugHook <> 0 Then                                           // Pause in IDE
        If GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), lpMode) Then // Check redirection
          Begin
            Writeln('Press <Enter> to finish...');
            Readln;
          End;
      If iErrors > 0 Then
        Halt(iErrors);
    End else
      GUITestRunner.RunRegisteredTests;
end.


