(**

  This module defines a DLL which can be loaded by the RAD Studio IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    14 Oct 2018

  @nocheck EmptyBEGINEND

  @bug      nohint does not always find tags!
  @bug      Hints do not align the comment of the method!
  @bug      Literal strings found in external procedure references!
  @bug      Check and metric table headers do not draw properly (overwriting)!

  @todo     Add optional percentages to the metrics output.
  @todo     Splits explorer and documentation options.
  @todo     Add the ability to set individual metric font colours and styles.
  @todo     Add Checks to documentation
  @todo     Add Metrics to documentation
  @todo     Add CHM format to documentation
  @todo     Create a Word/PDF manual for BADI instead of the web page.

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

{$R 'LayeredExplorerImages.res' 'LayeredExplorerImages.RC'}
{$R 'SplashScreenIcon.res' 'SplashScreenIcon.RC'}
{$R 'DUnitTemplateResources.res' 'DUnitTemplateResources.RC'}
{$R 'BADIMenuImages.res' 'Source\BADIMenuImages.rc'}
{$R 'BADIVerInfo.res' 'BADIVerInfo.RC'}
{$R 'BADI.HTMLResources.res' 'Source\BADI.HTMLResources.RC'}

{$INCLUDE 'Source\CompilerDefinitions.inc'}
{$INCLUDE 'Source\LibrarySuffixes.inc'}

uses
  ShareMem,
  SysUtils,
  Classes,
  {$IFDEF PROFILECODE}
  Profiler in '..\..\Library\Profiler.pas',
  {$ENDIF }
  BADI.Initialisation in 'Source\BADI.Initialisation.pas',
  BADI.Options in 'Source\BADI.Options.pas',
  BADI.Module.Dispatcher in 'Source\BADI.Module.Dispatcher.pas',
  BADI.Base.Module in 'Source\BADI.Base.Module.pas',
  BADI.Wizard in 'Source\BADI.Wizard.pas',
  BADI.DockableModuleExplorer in 'Source\BADI.DockableModuleExplorer.pas' {frmDockableModuleExplorer},
  BADI.ModuleExplorerFrame in 'Source\BADI.ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  BADI.OptionsForm in 'Source\BADI.OptionsForm.pas' {frmOptions},
  BADI.Pascal.Module in 'Source\BADI.Pascal.Module.pas',
  ProgressForm in '..\..\Library\ProgressForm.pas' {frmProgress},
  BADI.SpecialTagForm in 'Source\BADI.SpecialTagForm.pas' {frmSpecialTag},
  BADI.TokenForm in 'Source\BADI.TokenForm.pas' {frmTokenForm},
  BADI.ToolsAPIUtils in 'Source\BADI.ToolsAPIUtils.pas',
  BADI.MethodDescriptionForm in 'Source\BADI.MethodDescriptionForm.pas' {frmMethodDescriptions},
  BADI.Documentation.Dispatcher in 'Source\BADI.Documentation.Dispatcher.pas',
  BADI.HTMLDocumentation in 'Source\BADI.HTMLDocumentation.pas',
  BADI.Base.Documentation in 'Source\BADI.Base.Documentation.pas',
  BADI.DocumentationOptionsForm in 'Source\BADI.DocumentationOptionsForm.pas' {frmDocumentationOptions},
  BADI.VB.Module in 'Source\BADI.VB.Module.pas',
  BADI.DUnitForm in 'Source\BADI.DUnitForm.pas' {frmDUnit},
  BADI.DUnitCreator in 'Source\BADI.DUnitCreator.pas',
  BADI.CommonIDEFunctions in 'Source\BADI.CommonIDEFunctions.pas',
  DGHEllipsisLabel in '..\..\Components\DGHControls\Source\DGHEllipsisLabel.pas',
  BADI.BackusNaur.Module in 'Source\BADI.BackusNaur.Module.pas',
  BADI.BNFHighlighter in 'Source\BADI.BNFHighlighter.pas',
  BADI.EditorNotifier in 'Source\BADI.EditorNotifier.pas',
  BADI.EidolonHighlighter in 'Source\BADI.EidolonHighlighter.pas',
  BADI.XML.Module in 'Source\BADI.XML.Module.pas',
  BADI.DFM.Module in 'Source\BADI.DFM.Module.pas',
  BADI.Eidolon.Module in 'Source\BADI.Eidolon.Module.pas',
  BADI.ProfilingForm in 'Source\BADI.ProfilingForm.pas' {frmProfiling},
  BADI.Eidolon.Types in 'Source\BADI.Eidolon.Types.pas',
  BADI.Eidolon.TLSSchematic.Module in 'Source\BADI.Eidolon.TLSSchematic.Module.pas',
  BADI.InitialiseOTAInterfaces in 'Source\BADI.InitialiseOTAInterfaces.pas',
  BADI.INI.Module in 'Source\BADI.INI.Module.pas',
  BADI.SpecialTagsFrame in 'Source\BADI.SpecialTagsFrame.pas' {fmBADISpecialTagsFrame: TFrame},
  BADI.ModuleExplorerOpsFrame in 'Source\BADI.ModuleExplorerOpsFrame.pas' {fmBADIModuleExplorerFrame: TFrame},
  BADI.MethodDescriptionsFrame in 'Source\BADI.MethodDescriptionsFrame.pas' {fmBADIMethodDescriptionsFrame: TFrame},
  BADI.GeneralOptionsFrame in 'Source\BADI.GeneralOptionsFrame.pas' {fmBADIGeneralOptions: TFrame},
  BADI.ExcludedDocFilesFrame in 'Source\BADI.ExcludedDocFilesFrame.pas' {fmBADIExcludedDocFilesFrame: TFrame},
  BADI.CodeBrowsingFrame in 'Source\BADI.CodeBrowsingFrame.pas' {fmBADICodeBrowsingFrame: TFrame},
  BADI.IDEOptionsHandler in 'Source\BADI.IDEOptionsHandler.pas',
  BADI.CustomOptionsFrame in 'Source\BADI.CustomOptionsFrame.pas',
  BADI.IDEOptionsInstaller in 'Source\BADI.IDEOptionsInstaller.pas',
  BADI.IDEMenuInstaller in 'Source\BADI.IDEMenuInstaller.pas',
  BADI.SplashScreen in 'Source\BADI.SplashScreen.pas',
  BADI.AboutBox in 'Source\BADI.AboutBox.pas',
  BADI.Comment in 'Source\BADI.Comment.pas',
  BADI.Comment.Tag in 'Source\BADI.Comment.Tag.pas',
  BADI.Constants in 'Source\BADI.Constants.pas',
  BADI.DocIssue in 'Source\BADI.DocIssue.pas',
  BADI.ElementContainer in 'Source\BADI.ElementContainer.pas',
  BADI.Functions in 'Source\BADI.Functions.pas',
  BADI.Generic.Constant in 'Source\BADI.Generic.Constant.pas',
  BADI.Generic.Variable in 'Source\BADI.Generic.Variable.pas',
  BADI.Generic.TypeDecl in 'Source\BADI.Generic.TypeDecl.pas',
  BADI.Generic.Tokenizer in 'Source\BADI.Generic.Tokenizer.pas',
  BADI.Generic.PropertyDecl in 'Source\BADI.Generic.PropertyDecl.pas',
  BADI.Generic.Parameter in 'Source\BADI.Generic.Parameter.pas',
  BADI.Generic.MethodDecl in 'Source\BADI.Generic.MethodDecl.pas',
  BADI.Generic.FunctionDecl in 'Source\BADI.Generic.FunctionDecl.pas',
  BADI.ModuleInfo in 'Source\BADI.ModuleInfo.pas',
  BADI.ResourceStrings in 'Source\BADI.ResourceStrings.pas',
  BADI.Types in 'Source\BADI.Types.pas',
  BADI.TokenInfo in 'Source\BADI.TokenInfo.pas',
  BADI.TickOption in 'Source\BADI.TickOption.pas',
  BADI.Base.Container in 'Source\BADI.Base.Container.pas',
  BADI.CompilerConditionStack in 'Source\BADI.CompilerConditionStack.pas',
  BADI.CompilerConditionData in 'Source\BADI.CompilerConditionData.pas',
  BADI.Pascal.ClassDecl in 'Source\BADI.Pascal.ClassDecl.pas',
  BADI.Pascal.Comment in 'Source\BADI.Pascal.Comment.pas',
  BADI.Pascal.ConstantDecl in 'Source\BADI.Pascal.ConstantDecl.pas',
  BADI.Pascal.Constants in 'Source\BADI.Pascal.Constants.pas',
  BADI.Pascal.DispInterfaceDecl in 'Source\BADI.Pascal.DispInterfaceDecl.pas',
  BADI.Pascal.ExportsItem in 'Source\BADI.Pascal.ExportsItem.pas',
  BADI.Pascal.FieldDecl in 'Source\BADI.Pascal.FieldDecl.pas',
  BADI.Pascal.FinalizationDecl in 'Source\BADI.Pascal.FinalizationDecl.pas',
  BADI.Pascal.Functions in 'Source\BADI.Pascal.Functions.pas',
  BADI.Pascal.IdentList in 'Source\BADI.Pascal.IdentList.pas',
  BADI.Pascal.InitializationDecl in 'Source\BADI.Pascal.InitializationDecl.pas',
  BADI.Pascal.InterfaceDecl in 'Source\BADI.Pascal.InterfaceDecl.pas',
  BADI.Pascal.MethodDecl in 'Source\BADI.Pascal.MethodDecl.pas',
  BADI.Pascal.ObjectDecl in 'Source\BADI.Pascal.ObjectDecl.pas',
  BADI.Pascal.ParameterDecl in 'Source\BADI.Pascal.ParameterDecl.pas',
  BADI.Pascal.PropertyDecl in 'Source\BADI.Pascal.PropertyDecl.pas',
  BADI.Pascal.PropertySpec in 'Source\BADI.Pascal.PropertySpec.pas',
  BADI.Pascal.RecordDecl in 'Source\BADI.Pascal.RecordDecl.pas',
  BADI.Pascal.ResourceStringDecl in 'Source\BADI.Pascal.ResourceStringDecl.pas',
  BADI.Pascal.ResourceStrings in 'Source\BADI.Pascal.ResourceStrings.pas',
  BADI.Pascal.TempCntr in 'Source\BADI.Pascal.TempCntr.pas',
  BADI.Pascal.ThreadVariableDecl in 'Source\BADI.Pascal.ThreadVariableDecl.pas',
  BADI.Pascal.TypeDecl in 'Source\BADI.Pascal.TypeDecl.pas',
  BADI.Pascal.Types in 'Source\BADI.Pascal.Types.pas',
  BADI.Pascal.VariableDecl in 'Source\BADI.Pascal.VariableDecl.pas',
  BADI.Pascal.UsesList in 'Source\BADI.Pascal.UsesList.pas',
  BADI.BackusNaur.Comment in 'Source\BADI.BackusNaur.Comment.pas',
  BADI.BackusNaur.Rule in 'Source\BADI.BackusNaur.Rule.pas',
  BADI.XML.BaseElement in 'Source\BADI.XML.BaseElement.pas',
  BADI.XML.Comment in 'Source\BADI.XML.Comment.pas',
  BADI.XML.DocType in 'Source\BADI.XML.DocType.pas',
  BADI.XML.ResourceStrings in 'Source\BADI.XML.ResourceStrings.pas',
  BADI.XML.XMLDecl in 'Source\BADI.XML.XMLDecl.pas',
  BADI.XML.XMLElemDecl in 'Source\BADI.XML.XMLElemDecl.pas',
  BADI.XML.XMLElement in 'Source\BADI.XML.XMLElement.pas',
  BADI.XML.XMLIgnoreElement in 'Source\BADI.XML.XMLIgnoreElement.pas',
  BADI.XML.XMLIncludeElement in 'Source\BADI.XML.XMLIncludeElement.pas',
  BADI.XML.XMLPERef in 'Source\BADI.XML.XMLPERef.pas',
  BADI.XML.XMLPI in 'Source\BADI.XML.XMLPI.pas',
  BADI.DFM.Item in 'Source\BADI.DFM.Item.pas',
  BADI.DFM.ObjectDecl in 'Source\BADI.DFM.ObjectDecl.pas',
  BADI.DFM.PropertyDecl in 'Source\BADI.DFM.PropertyDecl.pas',
  BADI.DFM.Types in 'Source\BADI.DFM.Types.pas',
  BADI.Eidolon.Association in 'Source\BADI.Eidolon.Association.pas',
  BADI.Eidolon.Bar in 'Source\BADI.Eidolon.Bar.pas',
  BADI.Eidolon.BaseTable in 'Source\BADI.Eidolon.BaseTable.pas',
  BADI.Eidolon.Comment in 'Source\BADI.Eidolon.Comment.pas',
  BADI.Eidolon.ConnectionDef in 'Source\BADI.Eidolon.ConnectionDef.pas',
  BADI.Eidolon.Constants in 'Source\BADI.Eidolon.Constants.pas',
  BADI.Eidolon.CustomFillSymbol in 'Source\BADI.Eidolon.CustomFillSymbol.pas',
  BADI.Eidolon.DatabaseDef in 'Source\BADI.Eidolon.DatabaseDef.pas',
  BADI.Eidolon.DBConnection in 'Source\BADI.Eidolon.DBConnection.pas',
  BADI.Eidolon.DBTable in 'Source\BADI.Eidolon.DBTable.pas',
  BADI.Eidolon.Diamond in 'Source\BADI.Eidolon.Diamond.pas',
  BADI.Eidolon.Ellipse in 'Source\BADI.Eidolon.Ellipse.pas',
  BADI.Eidolon.FieldDef in 'Source\BADI.Eidolon.FieldDef.pas',
  BADI.Eidolon.Functions in 'Source\BADI.Eidolon.Functions.pas',
  BADI.Eidolon.Line in 'Source\BADI.Eidolon.Line.pas',
  BADI.Eidolon.OutputTable in 'Source\BADI.Eidolon.OutputTable.pas',
  BADI.Eidolon.Rectangle in 'Source\BADI.Eidolon.Rectangle.pas',
  BADI.Eidolon.RequirementsTable in 'Source\BADI.Eidolon.RequirementsTable.pas',
  BADI.Eidolon.ResourceStrings in 'Source\BADI.Eidolon.ResourceStrings.pas',
  BADI.Eidolon.SuperBar in 'Source\BADI.Eidolon.SuperBar.pas',
  BADI.Eidolon.Symbol in 'Source\BADI.Eidolon.Symbol.pas',
  BADI.Eidolon.TableNameDef in 'Source\BADI.Eidolon.TableNameDef.pas',
  BADI.Eidolon.TextTable in 'Source\BADI.Eidolon.TextTable.pas',
  BADI.Eidolon.TextTableDef in 'Source\BADI.Eidolon.TextTableDef.pas',
  BADI.Eidolon.TimeLocationTable in 'Source\BADI.Eidolon.TimeLocationTable.pas',
  BADI.Eidolon.TLSSchematic.Comment in 'Source\BADI.Eidolon.TLSSchematic.Comment.pas',
  BADI.Eidolon.TLSSchematic.Constants in 'Source\BADI.Eidolon.TLSSchematic.Constants.pas',
  BADI.Eidolon.TLSSchematic.NoText in 'Source\BADI.Eidolon.TLSSchematic.NoText.pas',
  BADI.Eidolon.TLSSchematic.ResourceStrings in 'Source\BADI.Eidolon.TLSSchematic.ResourceStrings.pas',
  BADI.Eidolon.TLSSchematic.SchematicSetting in 'Source\BADI.Eidolon.TLSSchematic.SchematicSetting.pas',
  BADI.Eidolon.TLSSchematic.TLSObject in 'Source\BADI.Eidolon.TLSSchematic.TLSObject.pas',
  BADI.Eidolon.TLSSchematic.TLSRoad in 'Source\BADI.Eidolon.TLSSchematic.TLSRoad.pas',
  BADI.Eidolon.TLSSchematic.TLSShape in 'Source\BADI.Eidolon.TLSSchematic.TLSShape.pas',
  BADI.Eidolon.TLSSchematic.TLSStatic in 'Source\BADI.Eidolon.TLSSchematic.TLSStatic.pas',
  BADI.Eidolon.Triangle in 'Source\BADI.Eidolon.Triangle.pas',
  BADI.INI.Comment in 'Source\BADI.INI.Comment.pas',
  BADI.INI.KeyValuePair in 'Source\BADI.INI.KeyValuePair.pas',
  BADI.VB.Attribute in 'Source\BADI.VB.Attribute.pas',
  BADI.VB.Comment in 'Source\BADI.VB.Comment.pas',
  BADI.VB.ConstantDecl in 'Source\BADI.VB.ConstantDecl.pas',
  BADI.VB.Constants in 'Source\BADI.VB.Constants.pas',
  BADI.VB.EnumerateDecl in 'Source\BADI.VB.EnumerateDecl.pas',
  BADI.VB.EnumIdent in 'Source\BADI.VB.EnumIdent.pas',
  BADI.VB.EventDecl in 'Source\BADI.VB.EventDecl.pas',
  BADI.VB.ExceptionHandling in 'Source\BADI.VB.ExceptionHandling.pas',
  BADI.VB.FieldDecl in 'Source\BADI.VB.FieldDecl.pas',
  BADI.VB.ImplementedItem in 'Source\BADI.VB.ImplementedItem.pas',
  BADI.VB.Interfaces in 'Source\BADI.VB.Interfaces.pas',
  BADI.VB.MethodDecl in 'Source\BADI.VB.MethodDecl.pas',
  BADI.VB.Option in 'Source\BADI.VB.Option.pas',
  BADI.VB.Parameter in 'Source\BADI.VB.Parameter.pas',
  BADI.VB.PropertyDecl in 'Source\BADI.VB.PropertyDecl.pas',
  BADI.VB.RecordDecl in 'Source\BADI.VB.RecordDecl.pas',
  BADI.VB.ResourceStrings in 'Source\BADI.VB.ResourceStrings.pas',
  BADI.VB.TypeDecl in 'Source\BADI.VB.TypeDecl.pas',
  BADI.VB.Types in 'Source\BADI.VB.Types.pas',
  BADI.VB.VariableDecl in 'Source\BADI.VB.VariableDecl.pas',
  BADI.VB.Version in 'Source\BADI.VB.Version.pas',
  BADI.MenuShortcutsFrame in 'Source\BADI.MenuShortcutsFrame.pas' {fmBADIMenuShortcuts: TFrame},
  BADI.ParentFrame in 'Source\BADI.ParentFrame.pas' {fmBADIParentFrame: TFrame},
  BADI.Interfaces in 'Source\BADI.Interfaces.pas',
  BADI.CPP.Module in 'Source\BADI.CPP.Module.pas',
  BADI.VB.ModuleFull in 'Source\BADI.VB.ModuleFull.pas',
  BADI.ModuleExtensionsFrame in 'Source\BADI.ModuleExtensionsFrame.pas' {fmBADIModuleExtensionsFrame: TFrame},
  BADI.ModuleExplorer.TreeNodeInfo in 'Source\BADI.ModuleExplorer.TreeNodeInfo.pas',
  BADI.ModuleExplorer.CustomHintWindow in 'Source\BADI.ModuleExplorer.CustomHintWindow.pas',
  BADI.ModuleExplorer.VirtualStringTree in 'Source\BADI.ModuleExplorer.VirtualStringTree.pas',
  BADI.Module.Metrics.Options.Frame in 'Source\BADI.Module.Metrics.Options.Frame.pas' {frameBADIModuleMetricsOptions: TFrame},
  BADI.Refactor.Constant in 'Source\BADI.Refactor.Constant.pas',
  BADI.RefactorConstantForm in 'Source\BADI.RefactorConstantForm.pas' {frmBADIRefactorConstant},
  BADI.Refactoring.Functions in 'Source\BADI.Refactoring.Functions.pas',
  BADI.Module.Metrics in 'source\BADI.Module.Metrics.pas',
  BADI.Module.Metrics.EditorView.Frame in 'Source\BADI.Module.Metrics.EditorView.Frame.pas' {frameBADIModuleMetricsEditorView: TFrame},
  BADI.Module.Metrics.SubView in 'source\BADI.Module.Metrics.SubView.pas',
  BADI.Module.Metrics.SubView.Frame in 'source\BADI.Module.Metrics.SubView.Frame.pas' {frameBADIModuleMetricsSubView: TFrame},
  BADI.Module.Checks.Options.Frame in 'source\BADI.Module.Checks.Options.Frame.pas' {frameBADIModuleChecksOptions: TFrame},
  BADI.CustomVirtualStringTree in 'Source\BADI.CustomVirtualStringTree.pas',
  BADI.Module.Checks in 'Source\BADI.Module.Checks.pas',
  BADI.Module.Checks.EditorView.Frame in 'Source\BADI.Module.Checks.EditorView.Frame.pas' {frameBADIModuleChecksEditorView: TFrame},
  BADI.Module.Checks.SubView.Frame in 'Source\BADI.Module.Checks.SubView.Frame.pas' {frameBADIModuleChecksSubView: TFrame},
  BADI.Module.Checks.SubView in 'Source\BADI.Module.Checks.SubView.pas',
  BADI.IDEEditorColours in 'Source\BADI.IDEEditorColours.pas',
  BADI.IDEThemingNotifier in 'Source\BADI.IDEThemingNotifier.pas';

{$R *.res}

begin

end.






