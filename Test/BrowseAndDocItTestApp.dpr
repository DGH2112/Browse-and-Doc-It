(**

  This is the test applications project file.

  @Date    01 Aug 2008
  @Author  David Hoyle
  @Version 1.0

**)
program BrowseAndDocItTestApp;

{$R '..\ExplorerImages.res' '..\ExplorerImages.RC'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\default.txaPackage'}

uses
  ExceptionLog,
  Forms,
  BrowseAndDocItTestForm in 'Source\BrowseAndDocItTestForm.pas' {frmBrowseAndDocItTestForm},
  SpecialTagForm in '..\..\..\Library\SpecialTagForm.pas' {frmSpecialTag},
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  ModuleExplorerFrame in '..\..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in '..\..\..\Library\OptionsForm.pas' {frmOptions},
  PascalDocModule in '..\..\..\Library\PascalDocModule.pas',
  ProgressForm in '..\Source\ProgressForm.pas' {frmProgressForm},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm},
  dghlibrary in '..\..\..\Library\dghlibrary.pas',
  ModuleDispatcher in '..\..\..\Library\ModuleDispatcher.pas',
  MethodDescriptionForm in '..\..\..\LIBRARY\MethodDescriptionForm.pas' {frmMethodDescriptions};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  SetEurekaLogState(DebugHook = 0);
  Application.Initialize;
  Application.CreateForm(TfrmBrowseAndDocItTestForm, frmBrowseAndDocItTestForm);
  Application.Run;
end.
