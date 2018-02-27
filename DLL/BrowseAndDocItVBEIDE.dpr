(**

  This module contains a definition of a COM DLL that can be loaded by the
  VBE IDE.

  @Version 1.0
  @Date    05 Aug 2013
  @Author  David Hoyle

**)
library BrowseAndDocItVBEIDE;

{$R 'ITHVerInfoBADIVBEIDE.res' 'ITHVerInfoBADIVBEIDE.RC'}

uses
  ExceptionLog,
  ComServ,
  BrowseAndDocItVBEIDE_TLB in 'BrowseAndDocItVBEIDE_TLB.pas',
  VBIDE_TLB in '..\..\..\LIBRARY\VBIDE_TLB.pas',
  AddInDesignerObjects_TLB in '..\..\..\LIBRARY\AddInDesignerObjects_TLB.pas',
  CodeFragmentsForm in '..\Source\CodeFragmentsForm.pas' {frmInsertCodeFragments},
  ExportForm in '..\Source\ExportForm.pas' {frmExport},
  Functions in '..\Source\Functions.pas',
  IDETools in '..\Source\IDETools.pas',
  BrowseAndDocItAddin in '..\Source\BrowseAndDocItAddin.pas',
  SynEdit in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEdit.pas',
  SynEditHighlighter in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditHighlighter.pas',
  SynHighlighterVB in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynHighlighterVB.pas',
  SynEditMiscClasses in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditMiscClasses.pas',
  SynEditKeyConst in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditKeyConst.pas',
  SynEditTypes in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditTypes.pas',
  SynEditMiscProcs in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditMiscProcs.pas',
  SynHighlighterMulti in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynHighlighterMulti.pas',
  SynEditStrConst in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditStrConst.pas',
  SynRegExpr in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynRegExpr.pas',
  SynEditKbdHandler in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditKbdHandler.pas',
  SynEditKeyCmds in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditKeyCmds.pas',
  SynEditTextBuffer in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditTextBuffer.pas',
  SynTextDrawer in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynTextDrawer.pas',
  SynEditWordWrap in '..\..\..\LIBRARY\SynEdit\SynEdit\Source\SynEditWordWrap.pas',
  Office2000_TLB in '..\..\..\Library\Office2000_TLB.pas',
  EventSink in '..\..\..\Library\EventSink.pas',
  VBEIDEModuleExplorer in '..\Source\VBEIDEModuleExplorer.pas' {frmDockableModuleExplorer},
  ProgressForm in '..\..\..\Library\ProgressForm.pas' {frmProgress},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm},
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  dghlibrary in '..\..\..\Library\dghlibrary.pas',
  VBModule in '..\..\..\Library\VBModule.pas',
  OptionsForm in '..\..\..\Library\OptionsForm.pas' {frmOptions},
  MethodDescriptionForm in '..\..\..\Library\MethodDescriptionForm.pas' {frmMethodDescriptions},
  SpecialTagForm in '..\..\..\Library\SpecialTagForm.pas' {frmSpecialTag},
  DocumentationOptionsForm in '..\..\..\Library\DocumentationOptionsForm.pas' {frmDocumentationOptions},
  DocumentationDispatcher in '..\..\..\Library\DocumentationDispatcher.pas',
  BaseDocumentation in '..\..\..\Library\BaseDocumentation.pas',
  HTMLDocumentation in '..\..\..\Library\HTMLDocumentation.pas',
  GenericTokenizer in '..\..\..\Library\GenericTokenizer.pas',
  ModuleExplorerFrame in '..\..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  VirtualTrees in '..\..\..\Library\Virtual Treeview\Source\VirtualTrees.pas',
  VTAccessibilityFactory in '..\..\..\Library\Virtual Treeview\Source\VTAccessibilityFactory.pas',
  CommonIDEFunctions in '..\..\..\Library\CommonIDEFunctions.pas',
  CheckForUpdatesForm in '..\..\..\LIBRARY\CheckForUpdatesForm.pas' {frmCheckForUpdates},
  checkforupdates in '..\..\..\LIBRARY\checkforupdates.pas',
  MSXML2_TLB in '..\..\..\LIBRARY\MSXML2_TLB.pas',
  DGHEllipsisLabel in '..\..\..\Components\Source\DGHEllipsisLabel.pas',
  SynUnicode in '..\..\..\Library\SynEdit\SynEdit\Source\SynUnicode.pas',
  CheckForUpdatesOptionsForm in '..\..\..\Library\CheckForUpdatesOptionsForm.pas' {frmCheckForUpdatesOptions};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

{$R '..\ExplorerImages.res' '..\ExplorerImages.RC'}
{$R '..\SplashScreenIcon.res' '..\SplashScreenIcon.RC'}
{$R '..\..\..\Library\BrowseAndDocItHTMLResources.res' '..\..\..\Library\BrowseAndDocItHTMLResources.RC'}
{%File '..\..\..\Library\HTML Files\BrowseAndDocItScreen.css'}
{%File '..\..\..\Library\HTML Files\BrowseAndDocItPrint.css'}
{%HTMLTool '..\..\..\Library\HTML Files\BrowseAndDocItHTMLTemplate.html'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml1-strict.dtd'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml-lat1.ent'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml-special.ent'}
{%File '..\..\..\LIBRARY\HTML Files\xhtml-symbol.ent'}
{%File '..\..\..\LIBRARY\CompilerDefinitions.inc'}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
end.
