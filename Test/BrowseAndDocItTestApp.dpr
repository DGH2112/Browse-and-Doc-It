(**
  
  This is the test applications project file.

  @Date    27 Dec 2006
  @Author  David Hoyle
  @Version 1.0
  
**)
program BrowseAndDocItTestApp;

{%TogetherDiagram 'ModelSupport_BrowseAnDocItTestApp\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\SpecialTagForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\PascalDocModule\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\BrowseAndDocItTestApp\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\BrowseAndDocItTestForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\BaseLanguageModule\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\OptionsForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\TokenForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\ProgressForm\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\PascalDocChecker\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\ModuleExplorerFrame\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\ModuleExplorerFrame\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\PascalDocChecker\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\ProgressForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\TokenForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\OptionsForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\BaseLanguageModule\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\BrowseAndDocItTestForm\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\BrowseAndDocItTestApp\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\PascalDocModule\default.txvpck'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocItTestApp\SpecialTagForm\default.txvpck'}
{$R 'ExplorerImages.res' 'ExplorerImages.RC'}
{%HTMLTool '..\..\..\..\Desktop\Untitled1.htm'}

uses
  Forms,
  BrowseAndDocItTestForm in 'Source\BrowseAndDocItTestForm.pas' {frmBrowseAndDocItTestForm},
  SpecialTagForm in 'Source\SpecialTagForm.pas' {frmSpecialTag},
  BaseLanguageModule in '..\..\Library\BaseLanguageModule.pas',
  ModuleExplorerFrame in '..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in 'Source\OptionsForm.pas' {frmOptions},
  PascalDocChecker in '..\..\Library\PascalDocChecker.pas',
  PascalDocModule in '..\..\Library\PascalDocModule.pas',
  ProgressForm in 'Source\ProgressForm.pas' {frmProgressForm},
  TokenForm in '..\..\Library\TokenForm.pas' {frmTokenForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBrowseAndDocItTestForm, frmBrowseAndDocItTestForm);
  Application.Run;
end.
