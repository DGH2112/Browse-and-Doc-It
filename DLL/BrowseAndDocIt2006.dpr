(**

  This module defines a DLL which can be loaded by the BDS IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    16 Apr 2009

**)
library BrowseAndDocIt2009;

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
{$R '..\..\..\Library\BrowseAndDocItHTMLResources.res' '..\..\..\Library\BrowseAndDocItHTMLResources.RC'}
{$R '..\DUnitTemplateResources.res' '..\DUnitTemplateResources.RC'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocIt2006\default.txaPackage'}
{%File '..\..\..\LIBRARY\CompilerDefinitions.inc'}
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
  PascalModule in '..\..\..\LIBRARY\PascalModule.pas',
  ProgressForm in '..\..\..\Library\ProgressForm.pas' {frmProgress},
  SpecialTagForm in '..\..\..\Library\SpecialTagForm.pas' {frmSpecialTag},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm},
  ToolsAPIUtils in '..\Source\ToolsAPIUtils.pas',
  ModuleDispatcher in '..\..\..\Library\ModuleDispatcher.pas',
  MethodDescriptionForm in '..\..\..\LIBRARY\MethodDescriptionForm.pas' {frmMethodDescriptions},
  DocumentationDispatcher in '..\..\..\Library\DocumentationDispatcher.pas',
  HTMLDocumentation in '..\..\..\Library\HTMLDocumentation.pas',
  BaseDocumentation in '..\..\..\Library\BaseDocumentation.pas',
  DocumentationOptionsForm in '..\..\..\LIBRARY\DocumentationOptionsForm.pas' {frmDocumentationOptions},
  GenericTokenizer in '..\..\..\LIBRARY\GenericTokenizer.pas',
  checkforupdates in '..\..\..\LIBRARY\checkforupdates.pas',
  CheckForUpdatesForm in '..\..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  MSXML2_TLB in '..\..\..\LIBRARY\MSXML2_TLB.pas',
  VBModule in '..\..\..\LIBRARY\VBModule.pas',
  MSAAIntf in '..\..\..\LIBRARY\Virtual Treeview\Source\MSAAIntf.pas',
  VirtualTrees in '..\..\..\LIBRARY\Virtual Treeview\Source\VirtualTrees.pas',
  VTAccessibility in '..\..\..\LIBRARY\Virtual Treeview\Source\VTAccessibility.pas',
  VTAccessibilityFactory in '..\..\..\LIBRARY\Virtual Treeview\Source\VTAccessibilityFactory.pas',
  VTHeaderPopup in '..\..\..\LIBRARY\Virtual Treeview\Source\VTHeaderPopup.pas',
  DUnitForm in '..\Source\DUnitForm.pas' {frmDUnit},
  DUnitCreator in '..\Source\DUnitCreator.pas',
  CommonIDEFunctions in '..\Source\CommonIDEFunctions.pas',
  DGHEllipsisLabel in '..\..\..\Components\Source\DGHEllipsisLabel.pas',
  GIFImage in '..\..\..\LIBRARY\TGIFImage\GIFImage.pas';

{$R *.res}

begin
end.
