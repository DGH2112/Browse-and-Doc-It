(**

  This module contains a definition of a COM DLL that can be loaded by the
  VBE IDE.

  @Version 1.762
  @Date    02 Sep 2023
  @Author  David Hoyle

**)
library BrowseAndDocItVBEIDE;

{ $R 'ITHVerInfoBADIVBEIDE.res' 'ITHVerInfoBADIVBEIDE.RC'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDebugJCL,
  EDebugExports,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  ComServ,
  BrowseAndDocItVBEIDE_TLB in 'BrowseAndDocItVBEIDE_TLB.pas',
  VBIDE_TLB in '..\..\LIBRARY\VBIDE_TLB.pas',
  AddInDesignerObjects_TLB in '..\..\LIBRARY\AddInDesignerObjects_TLB.pas',
  CodeFragmentsForm in 'Source\CodeFragmentsForm.pas' {frmInsertCodeFragments},
  ExportForm in 'Source\ExportForm.pas' {frmExport},
  BrowseAndDocItAddin in 'Source\BrowseAndDocItAddin.pas',
  Office2000_TLB in '..\..\Library\Office2000_TLB.pas',
  EventSink in '..\..\Library\EventSink.pas',
  VBEIDEModuleExplorer in 'Source\VBEIDEModuleExplorer.pas' {frmDockableModuleExplorer},
  BADI.IDETools in 'Source\BADI.IDETools.pas',
  BADI.Functions in 'Source\BADI.Functions.pas',
  BADI.ProgressForm in 'Source\BADI.ProgressForm.pas' {frmProgress},
  BADI.TokenForm in 'Source\BADI.TokenForm.pas' {frmTokenForm},
  BADI.Base.Module in 'Source\BADI.Base.Module.pas',
  BADI.VB.Module in 'Source\BADI.VB.Module.pas',
  BADI.OptionsForm in 'Source\BADI.OptionsForm.pas' {frmOptions},
  BADI.MethodDescriptionForm in 'Source\BADI.MethodDescriptionForm.pas' {frmMethodDescriptions},
  BADI.SpecialTagForm in 'Source\BADI.SpecialTagForm.pas' {frmSpecialTag},
  BADI.DocumentationOptionsForm in 'Source\BADI.DocumentationOptionsForm.pas' {frmDocumentationOptions},
  BADI.Documentation.Dispatcher in 'Source\BADI.Documentation.Dispatcher.pas',
  BADI.Base.Documentation in 'Source\BADI.Base.Documentation.pas',
  BADI.HTMLDocumentation in 'Source\BADI.HTMLDocumentation.pas',
  BADI.Generic.Tokenizer in 'Source\BADI.Generic.Tokenizer.pas',
  BADI.ModuleExplorerFrame in 'Source\BADI.ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  BADI.CommonIDEFunctions in 'Source\BADI.CommonIDEFunctions.pas',
  BADI.Options in 'Source\BADI.Options.pas',
  BADI.Interfaces in 'Source\BADI.Interfaces.pas',
  BADI.Types in 'Source\BADI.Types.pas',
  BADI.Exclusions in 'Source\BADI.Exclusions.pas',
  BADI.Constants in 'Source\BADI.Constants.pas',
  BADI.ResourceStrings in 'Source\BADI.ResourceStrings.pas',
  BADI.Module.Dispatcher in 'Source\BADI.Module.Dispatcher.pas',
  BADI.ModuleInfo in 'Source\BADI.ModuleInfo.pas',
  BADI.TokenInfo in 'Source\BADI.TokenInfo.pas',
  BADI.CompilerConditionStack in 'Source\BADI.CompilerConditionStack.pas',
  BADI.ElementContainer in 'Source\BADI.ElementContainer.pas',
  BADI.Comment in 'Source\BADI.Comment.pas',
  BADI.Comment.Tag in 'Source\BADI.Comment.Tag.pas',
  BADI.Base.Container in 'Source\BADI.Base.Container.pas',
  BADI.Generic.Parameter in 'Source\BADI.Generic.Parameter.pas',
  BADI.SpellingIssue in 'Source\BADI.SpellingIssue.pas',
  BADI.DocIssue in 'Source\BADI.DocIssue.pas',
  BADI.Generic.TypeDecl in 'Source\BADI.Generic.TypeDecl.pas',
  BADI.CompilerConditionData in 'Source\BADI.CompilerConditionData.pas',
  BADI.TickOption in 'Source\BADI.TickOption.pas',
  BADI.IDEEditorColours in 'Source\BADI.IDEEditorColours.pas',
  BADI.Generic.FunctionDecl in 'Source\BADI.Generic.FunctionDecl.pas',
  BADI.Thread.Manager in 'Source\BADI.Thread.Manager.pas',
  BADI.MenuShortcutsFrame in 'Source\BADI.MenuShortcutsFrame.pas',
  badi.methoddescriptionsframe in 'Source\badi.methoddescriptionsframe.pas',
  badi.moduleexploreropsframe in 'Source\badi.moduleexploreropsframe.pas',
  BADI.ModuleExtensionsFrame in 'Source\BADI.ModuleExtensionsFrame.pas',
  BADI.SpecialTagsFrame in 'Source\BADI.SpecialTagsFrame.pas',
  badi.codebrowsingframe in 'Source\badi.codebrowsingframe.pas',
  badi.excludeddocfilesframe in 'Source\badi.excludeddocfilesframe.pas',
  BADI.GeneralOptionsFrame in 'Source\BADI.GeneralOptionsFrame.pas',
  badi.customoptionsframe in 'Source\badi.customoptionsframe.pas',
  BADI.CustomVirtualStringTree in 'Source\BADI.CustomVirtualStringTree.pas',
  BADI.ModuleExplorer.CustomHintWindow in 'Source\BADI.ModuleExplorer.CustomHintWindow.pas',
  BADI.ModuleExplorer.VirtualStringTree in 'Source\BADI.ModuleExplorer.VirtualStringTree.pas',
  BADI.ModuleExplorer.TreeNodeInfo in 'Source\BADI.ModuleExplorer.TreeNodeInfo.pas',
  BADI.LineDocIssue in 'Source\BADI.LineDocIssue.pas',
  BADI.DocIssueTotals in 'Source\BADI.DocIssueTotals.pas',
  BADI.VB.VariableDecl in 'Source\BADI.VB.VariableDecl.pas',
  BADI.VB.Interfaces in 'Source\BADI.VB.Interfaces.pas',
  BADI.VB.Types in 'Source\BADI.VB.Types.pas',
  BADI.Generic.MethodDecl in 'Source\BADI.Generic.MethodDecl.pas',
  BADI.Generic.Variable in 'Source\BADI.Generic.Variable.pas',
  BADI.VB.EventDecl in 'Source\BADI.VB.EventDecl.pas',
  BADI.VB.FieldDecl in 'Source\BADI.VB.FieldDecl.pas',
  BADI.VB.ImplementedItem in 'Source\BADI.VB.ImplementedItem.pas',
  BADI.VB.MethodDecl in 'Source\BADI.VB.MethodDecl.pas',
  BADI.VB.Option in 'Source\BADI.VB.Option.pas',
  BADI.VB.Parameter in 'Source\BADI.VB.Parameter.pas',
  BADI.VB.PropertyDecl in 'Source\BADI.VB.PropertyDecl.pas',
  BADI.VB.RecordDecl in 'Source\BADI.VB.RecordDecl.pas',
  BADI.VB.ResourceStrings in 'Source\BADI.VB.ResourceStrings.pas',
  BADI.VB.TypeDecl in 'Source\BADI.VB.TypeDecl.pas',
  BADI.VB.Version in 'Source\BADI.VB.Version.pas',
  BADI.VB.Attribute in 'Source\BADI.VB.Attribute.pas',
  BADI.VB.Comment in 'Source\BADI.VB.Comment.pas',
  BADI.VB.ConstantDecl in 'Source\BADI.VB.ConstantDecl.pas',
  BADI.VB.Constants in 'Source\BADI.VB.Constants.pas',
  BADI.VB.EnumerateDecl in 'Source\BADI.VB.EnumerateDecl.pas',
  BADI.VB.EnumIdent in 'Source\BADI.VB.EnumIdent.pas',
  BADI.VB.ExceptionHandling in 'Source\BADI.VB.ExceptionHandling.pas',
  BADI.Generic.PropertyDecl in 'Source\BADI.Generic.PropertyDecl.pas',
  BADI.Generic.Constant in 'Source\BADI.Generic.Constant.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

{ $R 'ExplorerImages.res' 'ExplorerImages.RC'}
{ $R 'SplashScreenIcon.res' 'SplashScreenIcon.RC'}
{ $R 'Source\BrowseAndDocItHTMLResources.res' 'Source\BrowseAndDocItHTMLResources.RC'}
{ %File 'HTML Files\BrowseAndDocItScreen.css'}
{ %File 'HTML Files\BrowseAndDocItPrint.css'}
{ %HTMLTool 'HTML Files\BrowseAndDocItHTMLTemplate.html'}
{ %File 'HTML Files\xhtml1-strict.dtd'}
{ %File 'HTML Files\xhtml-lat1.ent'}
{ %File 'HTML Files\xhtml-special.ent'}
{ %File 'HTML Files\xhtml-symbol.ent'}
{ %File 'CompilerDefinitions.inc'}

{$R 'LayeredExplorerImages.res' 'LayeredExplorerImages.RC'}
{$R 'SplashScreenIcon.res' 'SplashScreenIcon.RC'}
{$R 'DUnitTemplateResources.res' 'DUnitTemplateResources.RC'}
{$R 'BADIMenuImages.res' 'Source\BADIMenuImages.rc'}
{$R 'BADIVerInfo.res' 'BADIVerInfo.RC'}
{$R 'BADI.HTMLResources.res' 'Source\BADI.HTMLResources.RC'}

{$INCLUDE 'Source\CompilerDefinitions.inc'}


begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
end.

