(**

  This is the test applications project file.

  @Date    21 Oct 2009
  @Author  David Hoyle
  @Version 1.0

**)
program BrowseAndDocItTestApp2009;

{$R 'ExplorerImages.res' '..\ExplorerImages.RC'}
{$R 'BrowseAndDocItHTMLResources.res' '..\..\..\Library\BrowseAndDocItHTMLResources.RC'}
{$R 'DUnitTemplateResources.res' '..\DUnitTemplateResources.RC'}

uses
  ExceptionLog,
  Forms,
  BrowseAndDocItTestForm in 'Source\BrowseAndDocItTestForm.pas' {frmBrowseAndDocItTestForm},
  SpecialTagForm in '..\..\..\Library\SpecialTagForm.pas' {frmSpecialTag},
  BaseLanguageModule in '..\..\..\Library\BaseLanguageModule.pas',
  ModuleExplorerFrame in '..\..\..\Library\ModuleExplorerFrame.pas' {frameModuleExplorer: TFrame},
  OptionsForm in '..\..\..\Library\OptionsForm.pas' {frmOptions},
  PascalModule in '..\..\..\LIBRARY\PascalModule.pas',
  ProgressForm in '..\..\..\Library\ProgressForm.pas' {frmProgressForm},
  TokenForm in '..\..\..\Library\TokenForm.pas' {frmTokenForm},
  dghlibrary in '..\..\..\Library\dghlibrary.pas',
  ModuleDispatcher in '..\..\..\Library\ModuleDispatcher.pas',
  MethodDescriptionForm in '..\..\..\LIBRARY\MethodDescriptionForm.pas' {frmMethodDescriptions},
  HTMLDocumentation in '..\..\..\Library\HTMLDocumentation.pas',
  BaseDocumentation in '..\..\..\Library\BaseDocumentation.pas',
  DocumentationDispatcher in '..\..\..\Library\DocumentationDispatcher.pas',
  DocumentationOptionsForm in '..\..\..\LIBRARY\DocumentationOptionsForm.pas' {frmDocumentationOptions},
  GenericTokenizer in '..\..\..\LIBRARY\GenericTokenizer.pas',
  SynEdit in '..\..\..\LIBRARY\SynEdit\Source\SynEdit.pas',
  SynEditHighlighter in '..\..\..\LIBRARY\SynEdit\Source\SynEditHighlighter.pas',
  SynHighlighterPas in '..\..\..\LIBRARY\SynEdit\Source\SynHighlighterPas.pas',
  SynEditMiscClasses in '..\..\..\LIBRARY\SynEdit\Source\SynEditMiscClasses.pas',
  SynEditTypes in '..\..\..\LIBRARY\SynEdit\Source\SynEditTypes.pas',
  SynEditKeyConst in '..\..\..\LIBRARY\SynEdit\Source\SynEditKeyConst.pas',
  SynEditMiscProcs in '..\..\..\LIBRARY\SynEdit\Source\SynEditMiscProcs.pas',
  SynHighlighterMulti in '..\..\..\LIBRARY\SynEdit\Source\SynHighlighterMulti.pas',
  SynEditStrConst in '..\..\..\LIBRARY\SynEdit\Source\SynEditStrConst.pas',
  SynRegExpr in '..\..\..\LIBRARY\SynEdit\Source\SynRegExpr.pas',
  SynEditKbdHandler in '..\..\..\LIBRARY\SynEdit\Source\SynEditKbdHandler.pas',
  SynEditKeyCmds in '..\..\..\LIBRARY\SynEdit\Source\SynEditKeyCmds.pas',
  SynEditTextBuffer in '..\..\..\LIBRARY\SynEdit\Source\SynEditTextBuffer.pas',
  SynTextDrawer in '..\..\..\LIBRARY\SynEdit\Source\SynTextDrawer.pas',
  SynEditWordWrap in '..\..\..\LIBRARY\SynEdit\Source\SynEditWordWrap.pas',
  VirtualTrees in '..\..\..\LIBRARY\Virtual Treeview\Source\VirtualTrees.pas',
  VTAccessibilityFactory in '..\..\..\LIBRARY\Virtual Treeview\Source\VTAccessibilityFactory.pas',
  MSAAIntf in '..\..\..\LIBRARY\Virtual Treeview\Source\MSAAIntf.pas',
  VBModule in '..\..\..\LIBRARY\VBModule.pas',
  SynHighlighterVB in '..\..\..\Library\SynEdit\Source\SynHighlighterVB.pas',
  ExclusionsForm in 'Source\ExclusionsForm.pas' {frmExclusions},
  SynUnicode in '..\..\..\LIBRARY\SynEdit\Source\SynUnicode.pas',
  DGHEllipsisLabel in '..\..\..\Components\Source\DGHEllipsisLabel.pas',
  BackusNaurModule in '..\..\..\LIBRARY\BackusNaurModule.pas',
  SynHighlighterCpp in '..\..\..\LIBRARY\SynEdit\Source\SynHighlighterCpp.pas',
  CommonIDEFunctions in '..\..\..\Library\CommonIDEFunctions.pas',
  XMLModule in '..\..\..\LIBRARY\XMLModule.pas',
  SynHighlighterXML in '..\..\..\LIBRARY\SynEdit\Source\SynHighlighterXML.pas',
  DFMModule in '..\..\..\LIBRARY\DFMModule.pas',
  EidolonModule in '..\..\..\LIBRARY\EidolonModule.pas',
  FolderConfig in 'Source\FolderConfig.pas' {frmFolders},
  UsefulSynEditFunctions in '..\..\..\LIBRARY\UsefulSynEditFunctions.pas',
  DGHSynEdit in '..\..\..\LIBRARY\DGHSynEdit.pas',
  EditorOptionsForm in '..\..\..\LIBRARY\EditorOptionsForm.pas' {frmEditorOptions},
  SynHighlighterDfm in '..\..\..\LIBRARY\SynEdit\Source\SynHighlighterDfm.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmBrowseAndDocItTestForm, frmBrowseAndDocItTestForm);
  Application.Run;
end.
