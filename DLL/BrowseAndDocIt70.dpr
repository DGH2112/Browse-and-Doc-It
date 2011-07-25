(**

  This module defines a DLL which can be loaded by the BDS IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    29 Mar 2010

**)
library BrowseAndDocIt70;

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
{%File '..\..\..\Library\HTML Files\BrowseAndDocItScreen.css'}
{%File '..\..\..\Library\HTML Files\BrowseAndDocItPrint.css'}
{%HTMLTool '..\..\..\Library\HTML Files\BrowseAndDocItHTMLTemplate.html'}
{%TogetherDiagram 'ModelSupport_BrowseAndDocIt\default.txaPackage'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml1-strict.dtd'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml-lat1.ent'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml-special.ent'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml-symbol.ent'}
{%File '..\..\..\LIBRARY\CompilerDefinitions.inc'}
{%File '..\Source\DUnit Templates\DUnitProjectTemplate.txt'}
uses
  ShareMem,
  SysUtils,
  Classes,
  DGHLibrary in '..\..\..\Library\DGHLibrary.pas',
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  BrowseAndDocItWizard in '..\Source\BrowseAndDocItWizard.pas',
  DockableModuleExplorer in '..\Source\DockableModuleExplorer.pas' {frmDockableModuleExplorer},
  ModuleExplorerFrame in '..\..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in '..\..\..\Library\OptionsForm.pas' {frmOptions},
  PascalModule in '..\..\..\Library\PascalModule.pas',
  ProgressForm in '..\..\..\Library\ProgressForm.pas' {frmProgress},
  SpecialTagForm in '..\..\..\Library\SpecialTagForm.pas' {frmSpecialTag},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm},
  ToolsAPIUtils in '..\Source\ToolsAPIUtils.pas',
  ModuleDispatcher in '..\..\..\Library\ModuleDispatcher.pas',
  MethodDescriptionForm in '..\..\..\LIBRARY\MethodDescriptionForm.pas' {frmMethodDescriptions},
  DocumentationDispatcher in '..\..\..\Library\DocumentationDispatcher.pas',
  HTMLDocumentation in '..\..\..\Library\HTMLDocumentation.pas',
  BaseDocumentation in '..\..\..\Library\BaseDocumentation.pas',
  GIFImage in '..\..\..\Library\TGIFImage\GIFImage.pas',
  DocumentationOptionsForm in '..\..\..\LIBRARY\DocumentationOptionsForm.pas' {frmDocumentationOptions},
  GenericTokenizer in '..\..\..\LIBRARY\GenericTokenizer.pas',
  checkforupdates in '..\..\..\LIBRARY\checkforupdates.pas',
  CheckForUpdatesForm in '..\..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  MSXML2_TLB in '..\..\..\LIBRARY\MSXML2_TLB.pas',
  VTAccessibilityFactory in '..\..\..\LIBRARY\Virtual Treeview\Source\VTAccessibilityFactory.pas',
  VirtualTrees in '..\..\..\LIBRARY\Virtual Treeview\Source\VirtualTrees.pas',
  MSAAIntf in '..\..\..\LIBRARY\Virtual Treeview\Source\MSAAIntf.pas',
  VBModule in '..\..\..\LIBRARY\VBModule.pas',
  DUnitForm in '..\Source\DUnitForm.pas' {frmDUnit},
  DUnitCreator in '..\Source\DUnitCreator.pas',
  CommonIDEFunctions in '..\..\..\Library\CommonIDEFunctions.pas',
  DGHEllipsisLabel in '..\..\..\Components\Source\DGHEllipsisLabel.pas',
  BackusNaurModule in '..\..\..\LIBRARY\BackusNaurModule.pas',
  XMLModule in '..\..\..\LIBRARY\XMLModule.pas',
  DFMModule in '..\..\..\LIBRARY\DFMModule.pas',
  EidolonModule in '..\..\..\LIBRARY\EidolonModule.pas',
  BNFHighlighter in '..\Source\BNFHighlighter.pas',
  KeyboardBindings in '..\Source\KeyboardBindings.pas',
  EditorNotifier in '..\Source\EditorNotifier.pas',
  EidolonHighlighter in '..\Source\EidolonHighlighter.pas',
  ProfilingForm in '..\Source\ProfilingForm.pas' {frmProfiling},
  EidolonTypes in '..\..\..\LIBRARY\EidolonTypes.pas';

{$R *.res}

begin
end.


