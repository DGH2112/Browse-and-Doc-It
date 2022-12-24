(**

  This module defines a RAD Studio plug-in DLL which provides the ability to
  browse, check and document your code.

  @Version 1.358
  @Author  David Hoyle
  @Date    15 Oct 2022

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

  @nocheck EmptyBEGINEND

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
  Graphics,
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF PROFILECODE}
  BADI.Initialisation in 'Source\BADI.Initialisation.pas',
  BADI.Options in 'Source\BADI.Options.pas',
  BADI.Module.Dispatcher in 'Source\BADI.Module.Dispatcher.pas',
  BADI.Base.Module in 'Source\BADI.Base.Module.pas',
  BADI.Wizard in 'Source\BADI.Wizard.pas',
  BADI.DockableModuleExplorer in 'Source\BADI.DockableModuleExplorer.pas' {frmDockableModuleExplorer},
  BADI.ModuleExplorerFrame in 'Source\BADI.ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  BADI.OptionsForm in 'Source\BADI.OptionsForm.pas' {frmOptions},
  BADI.Pascal.Module in 'Source\BADI.Pascal.Module.pas',
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
  BADI.BackusNaur.Module in 'Source\BADI.BackusNaur.Module.pas',
  BADI.BNFHighlighter in 'Source\BADI.BNFHighlighter.pas',
  BADI.EditorNotifier in 'Source\BADI.EditorNotifier.pas',
  BADI.XML.Module in 'Source\BADI.XML.Module.pas',
  BADI.DFM.Module in 'Source\BADI.DFM.Module.pas',
  BADI.ProfilingForm in 'Source\BADI.ProfilingForm.pas' {frmProfiling},
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
  BADI.IDEThemingNotifier in 'Source\BADI.IDEThemingNotifier.pas',
  BADI.Exclusions in 'Source\BADI.Exclusions.pas',
  BADI.EidolonHighlighter in 'Source\BADI.EidolonHighlighter.pas',
  BADI.IDENotifier in 'Source\BADI.IDENotifier.pas',
  BADI.ModuleNotifierList in 'Source\BADI.ModuleNotifierList.pas',
  BADI.ModuleNotifier in 'Source\BADI.ModuleNotifier.pas',
  BADI.ModuleStats in 'Source\BADI.ModuleStats.pas',
  BADI.ModuleStatsList in 'Source\BADI.ModuleStatsList.pas',
  BADI.SourceEditorNotifier in 'Source\BADI.SourceEditorNotifier.pas',
  BADI.EditViewNotifier in 'Source\BADI.EditViewNotifier.pas',
  BADI.LineDocIssue in 'Source\BADI.LineDocIssue.pas',
  BADI.DocIssueTotals in 'Source\BADI.DocIssueTotals.pas',
  BADI.DocIssuesHintWindow in 'Source\BADI.DocIssuesHintWindow.pas',
  BADI.ProjectNotifier in 'Source\BADI.ProjectNotifier.pas',
  BADI.Module.Spelling.EditorView.Frame in 'Source\BADI.Module.Spelling.EditorView.Frame.pas' {frameBADIModuleSpellingEditorView: TFrame},
  BADI.Spelling.OpsFrame in 'Source\BADI.Spelling.OpsFrame.pas' {frameBADISpellingOpions: TFrame},
  BADI.SpellingIssue in 'Source\BADI.SpellingIssue.pas',
  BADI.Spelling.DictionaryEditorForm in 'Source\BADI.Spelling.DictionaryEditorForm.pas' {frmDictionaryEditor},
  BADI.Module.Spelling in 'Source\BADI.Module.Spelling.pas',
  BADI.FileInfo.Manager in 'Source\BADI.FileInfo.Manager.pas',
  BADI.Frame.Manager in 'Source\BADI.Frame.Manager.pas',
  BADI.ProgressForm in 'Source\BADI.ProgressForm.pas' {frmProgress},
  BADI.CommentCodeForm in 'Source\BADI.CommentCodeForm.pas' {frmCommentCode},
  BADI.Thread.Manager in 'Source\BADI.Thread.Manager.pas';

{$R *.res}

{$IFDEF DEBUG}
Const
  (** A category label for the CodeSite messages during debugging. **)
  strBADICodeSiteCategory = 'BADI';
  (** This is the lowest colour level to randomise from. **)
  iBaseColour = $C0;
  (** This is the remaining level to randomise over. **)
  iAddColour = $FF - iBaseColour;
{$ENDIF DEBUG} 

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  CodeSite.Category := strBADICodeSiteCategory;
  Randomize;
  CodeSite.CategoryColor :=
    (iBaseColour + Random(iAddColour)) Shl 0 +
    (iBaseColour + Random(iAddColour)) Shl 8 +
    (iBaseColour + Random(iAddColour)) Shl 16;
  {$ENDIF DEBUG} 
end.



