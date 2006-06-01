program BrowseAnDocItTestApp;

{%TogetherDiagram 'ModelSupport_BrowseAnDocItTestApp\default.txaPackage'}

uses
  Forms,
  BrowseAndDocItTestForm in 'Source\BrowseAndDocItTestForm.pas' {frmBrowseAndDocItTestForm},
  SpecialTagForm in 'Source\SpecialTagForm.pas' {frmSpecialTag},
  BaseLanguageModule in 'Source\BaseLanguageModule.pas',
  ModuleExplorerFrame in 'Source\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  PascalDocChecker in 'Source\PascalDocChecker.pas',
  PascalDocModule in 'Source\PascalDocModule.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBrowseAndDocItTestForm, frmBrowseAndDocItTestForm);
  Application.Run;
end.
