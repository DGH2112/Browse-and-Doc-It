(**
  
  This is the test applications project file.

  @Date    07 Jun 2006
  @Author  David Hoyle
  @Version 1.0
  
**)
program BrowseAndDocItTestApp;

{%TogetherDiagram 'ModelSupport_BrowseAnDocItTestApp\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\default.txaPackage'}

uses
  Forms,
  BrowseAndDocItTestForm in 'Source\BrowseAndDocItTestForm.pas' {frmBrowseAndDocItTestForm},
  SpecialTagForm in 'Source\SpecialTagForm.pas' {frmSpecialTag},
  BaseLanguageModule in 'Source\BaseLanguageModule.pas',
  ModuleExplorerFrame in 'Source\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  PascalDocChecker in 'Source\PascalDocChecker.pas',
  PascalDocModule in 'Source\PascalDocModule.pas',
  ProgressForm in 'Source\ProgressForm.pas' {frmProgressForm},
  TokenForm in 'Source\TokenForm.pas' {frmTokenForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBrowseAndDocItTestForm, frmBrowseAndDocItTestForm);
  Application.Run;
end.
