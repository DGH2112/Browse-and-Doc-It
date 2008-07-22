(**
  
  This is the test applications project file.

  @Date    22 Jul 2008
  @Author  David Hoyle
  @Version 1.0
  
**)
program BrowseAndDocItTestApp;

{$R '..\ExplorerImages.res' '..\ExplorerImages.RC'}

uses
  Forms,
  BrowseAndDocItTestForm in 'Source\BrowseAndDocItTestForm.pas' {frmBrowseAndDocItTestForm},
  SpecialTagForm in '..\Source\SpecialTagForm.pas' {frmSpecialTag},
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  ModuleExplorerFrame in '..\..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in '..\Source\OptionsForm.pas' {frmOptions},
  PascalDocChecker in '..\..\..\Library\PascalDocChecker.pas',
  PascalDocModule in '..\..\..\Library\PascalDocModule.pas',
  ProgressForm in '..\Source\ProgressForm.pas' {frmProgressForm},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBrowseAndDocItTestForm, frmBrowseAndDocItTestForm);
  Application.Run;
end.
