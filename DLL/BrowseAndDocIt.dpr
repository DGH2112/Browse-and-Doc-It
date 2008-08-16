(**

  This module defines a DLL which can be loaded ny the BDS IDE.
  
  @Version 1.0
  @Author  David Hoyle
  @Date    16 Aug 2008
  
**)
library BrowseAndDocIt;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R '..\ExplorerImages.res' '..\ExplorerImages.RC'}
{$R '..\SplashScreenIcon.res' '..\SplashScreenIcon.RC'}
uses
  ShareMem,
  ExceptionLog,
  SysUtils,
  Classes,
  DGHLibrary in '..\..\..\Library\DGHLibrary.pas',
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  BrowseAndDocItWizard in '..\Source\BrowseAndDocItWizard.pas',
  DockableModuleExplorer in '..\Source\DockableModuleExplorer.pas' {frmDockableModuleExplorer},
  ModuleExplorerFrame in '..\..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in '..\..\..\Library\OptionsForm.pas' {frmOptions},
  PascalDocModule in '..\..\..\Library\PascalDocModule.pas',
  ProgressForm in '..\Source\ProgressForm.pas' {frmProgress},
  SpecialTagForm in '..\..\..\Library\\SpecialTagForm.pas' {frmSpecialTag},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm},
  ToolsAPIUtils in '..\Source\ToolsAPIUtils.pas',
  ModuleDispatcher in '..\..\..\Library\ModuleDispatcher.pas';

{$R *.res}

begin
end.
