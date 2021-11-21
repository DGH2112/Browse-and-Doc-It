(**

  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Author  David Hoyle
  @Version 14.827
  @Date    03 May 2021

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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

**)
Unit BrowseAndDocItTestForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  SynEditHighlighter,
  SynHighlighterPas,
  SynEdit,
  ExtCtrls,
  BADI.ModuleExplorerFrame,
  BADI.Base.Module,
  StdCtrls,
  FileCtrl,
  ComCtrls,
  Contnrs,
  SynHighlighterVB,
  SynHighlighterCpp,
  SynHighlighterINI,
  Menus,
  StdActns,
  ActnList,
  BADI.ProgressForm,
  Buttons,
  ImgList,
  ToolWin,
  XPMan,
  SynHighlighterXML,
  DGHSynEdit,
  SynHighlighterDfm,
  System.Actions,
  System.ImageList,
  DGHCustomGraphicsControl,
  DGHMemoryMonitorControl,
  System.Generics.Collections,
  System.Generics.Defaults;

{$INCLUDE '..\..\Source\CompilerDefinitions.inc'}

Type
  (** A record to define the doc conflicts, hints and errors for files and
      directories. **)
  TScanResults = Record
    iDocConflicts, iHints, iWarnings, iErrors: Integer;
  End;

  (** An enumerate to define the type of information passed to GetErrors. **)
  TSourceType = (stFile, stCode);

  (** This is thre class that defined the main interface form. **)
  TfrmBrowseAndDocItTestForm = Class(TForm)
    pnlModuleExplorer: TPanel;
    Splitter1: TSplitter;
    lvFileList: TListView;
    ilImages: TImageList;
    sbrStatusBar: TStatusBar;
    sptFiles: TSplitter;
    amActions: TActionList;
    actFileExit: TFileExit;
    actViewSpecialCharacters: TAction;
    actFileRecurseFolders: TAction;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditDelete: TEditDelete;
    actViewShowTokens: TAction;
    actToolsOptions: TAction;
    actToolsDocumentation: TAction;
    actViewDocConflicts: TAction;
    actViewHints: TAction;
    actViewWarnings: TAction;
    actViewErrors: TAction;
    actFileScan: TAction;
    actFileExcludeFile: TAction;
    actFileDeleteFolder: TAction;
    actToolsExclusions: TAction;
    atbToolbar: TToolBar;
    tbtnFileScan: TToolButton;
    tbtnFileExit: TToolButton;
    tbtnFileRecurseFolders: TToolButton;
    tbtnFileExcludeFile: TToolButton;
    btnViewDocConflicts: TToolButton;
    tbtnViewHints: TToolButton;
    tbtnViewWarnings: TToolButton;
    tbtnViewErrors: TToolButton;
    tbtnSep1: TToolButton;
    tbtnViewSpecialChars: TToolButton;
    tbtnViewShowTokens: TToolButton;
    tbtnSep2: TToolButton;
    tbtnToolsOptions: TToolButton;
    tbtnToolsDocumentation: TToolButton;
    tbtnToolsExclusions: TToolButton;
    ilDirStatus: TImageList;
    pmEdit: TPopupMenu;
    Undo1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    XPManifest: TXPManifest;
    actViewWordWrap: TAction;
    btnWordwrap: TToolButton;
    tbtnSep3: TToolButton;
    actFileFolders: TAction;
    tbtnFolderCongih: TToolButton;
    actToolsSynEditOptions: TAction;
    ToolButton: TToolButton;
    DGHMemoryMonitor: TDGHMemoryMonitor;
    actResurseZipFiles: TAction;
    btnRecurseZipFiles: TToolButton;
    pmStatusBar: TPopupMenu;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
    Procedure SelectionChange(Const iIdentLine, iIdentCol, iCommentLine: Integer);
    Procedure Focus(Sender: TObject);
    Procedure ShowTokensClick(Sender: TObject);
    Procedure SpecialCharactersClick(Sender: TObject);
    Procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    Procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    Procedure btnOptionsClick(Sender: TObject);
    Procedure DocumentationClick(Sender: TObject);
    Procedure FilterChange(Sender: TObject);
    Procedure actFileScanExecute(Sender: TObject);
    Procedure ChangeVisibleItems(Sender: TObject);
    Procedure actFileRecurseFoldersExecute(Sender: TObject);
    Procedure actFileExcludeFileExecute(Sender: TObject);
    Procedure lvFilesCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure actToolsExclusionsExecute(Sender: TObject);
    Procedure actViewWordWrapExecute(Sender: TObject);
    Procedure actViewWordWrapUpdate(Sender: TObject);
    Procedure actFileFoldersExecute(Sender: TObject);
    Procedure actToolsSynEditOptionsExecute(Sender: TObject);
    procedure actResurseZipFilesExecute(Sender: TObject);
    procedure actResurseZipFilesUpdate(Sender: TObject);
    procedure sbrStatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer);
  Strict Private
    Type
      (** A record to describe a name index pairing for use in sorting the Highighters and VCL
          Themings @nohints **)
      TNameIndexRec = Record
        FName  : String;
        FIndex : Integer;
        Constructor Create(Const strName : String; Const iIndex : Integer);
      End;
    (** An IComparer class to allow for custom sorting of the TList<T> collection. **)
    TNameIndexComparer = Class(TComparer<TNameIndexRec>)
    Strict Private
    Strict Protected
    Public
      Function Compare(Const Left, Right : TNameIndexRec) : Integer; Override;
    End;
  Strict Private
    { Private declarations }
    FFileName           : String;
    FPathRoot           : String;
    FModuleExplorerFrame: TframeModuleExplorer;
    FProgressForm       : TfrmProgress;
    FINIFileName        : String;
    FSynEdit            : TDGHSynEdit;
    FSynPasSyn          : TSynPasSyn;
    FSynVBSyn           : TSynVBSyn;
    FSynCPPSyn          : TSynCPPSyn;
    FSynXMLSyn          : TSynXMLSyn;
    FSynDFMSyn          : TSynDFMSyn;
    FSynINISyn          : TSynINISyn;
    FParseRecords       : TObjectList;
    FFileExcludeList    : TStringList;
    FTimer              : TTimer;
    FLastEdit           : Int64;
    FFolders            : TStringList;
    FIndex              : Integer;
    FRecurseZipFiles    : Boolean;
    Function GetFileName: String;
    Procedure SetFileName(Const Value: String);
    Procedure LoadSettings;
    Procedure LoadThemes;
    Procedure SaveSettings;
    Function GetPathRoot: String;
    Procedure SetPathRoot(Const Value: String);
    Function RecurseDirectories(Const strRoot, strDirectory: String;
      Var iPosition: Integer; Const strExtensions: String;
      Const boolScan: Boolean = False): TScanResults;
    Procedure RefreshExplorer(Sender: TObject);
    Procedure PopulateListView;
    Function ExcludeFileFromResults(Const strFileName: String): Boolean;
    Procedure GetErrors(Const strFileName, strSource: String; Var iHints, iWarnings,
      iErrors, iConflicts: Integer; Const SourceType: TSourceType);
    Procedure TimerEvent(Sender: TObject);
    Procedure RefreshModuleExplorer;
    Procedure SaveResults;
    Procedure LoadResults;
    Procedure ExtractFile(Sender: TObject; var FileName: String; var FileAttr: LongWord;
      const Comment: AnsiString);
    Procedure UpdateStatusBar;
    Procedure ShowVCLThemePopup(Const Pt: TPoint);
    Procedure VCLThemeClick(Sender : TObject);
    (**
      A property to define the currently selected file.
      @precon  None.
      @postcon Sets the current file and initiates the file being displayed in
               the editor or returns the current file name.
      @return  a String
    **)
    Property FileName: String Read GetFileName Write SetFileName;
    (**
      This property get or sets the path root of the selected file.
      @precon  None.
      @postcon Get or sets the path root of the selected file.
      @return  a String
    **)
    Property PathRoot: String Read GetPathRoot Write SetPathRoot;
  Public
    { Public declarations }
  End;

  (** This is a class which represents a parser record of information. **)
  TParseRecord = Class
    {$IFDEF D2006} Strict {$ENDIF} Private
    FFileName : String;
    FPathRoot : String;
    FErrors   : Integer;
    FWarnings : Integer;
    FHints    : Integer;
    FConflicts: Integer;
  Public
    Constructor Create(Const strFileName, strPathRoot: String; Const iErrors, iWarnings, iHints,
      iConflicts: Integer);
    (**
      This property returns the file name of the parser record.
      @precon  None.
      @postcon Returns the file name of the parser record.
      @return  a String
    **)
    Property FileName: String Read FFileName;
    (**
      This property get or sets the path root of the selected file.
      @precon  None.
      @postcon Get or sets the path root of the selected file.
      @return  a String
    **)
    Property PathRoot: String Read FPathRoot;
    (**
      This property returns the number of errors in the file.
      @precon  None.
      @postcon Returns the number of errors in the file.
      @return  an Integer
    **)
    Property Errors: Integer Read FErrors Write FErrors;
    (**
      This property returns the number of warnings in the file.
      @precon  None.
      @postcon Returns the number of warnings in the file.
      @return  an Integer
    **)
    Property Warnings: Integer Read FWarnings Write FWarnings;
    (**
      This property returns the number of hints in the file.
      @precon  None.
      @postcon Returns the number of hints in the file.
      @return  an Integer
    **)
    Property Hints: Integer Read FHints Write FHints;
    (**
      This property returns the number of conflicts in the file.
      @precon  None.
      @postcon Returns the number of conflicts in the file.
      @return  an Integer
    **)
    Property Conflicts: Integer Read FConflicts Write FConflicts;
  End;

Var
  (** This is a global form variable so that the Delphi IDE can auto create
      the form on application startup. **)
  frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm;

Implementation

Uses
  CodeSiteLogging,
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  EExceptionManager,
  {$ENDIF}
  BADI.TokenForm,
  IniFiles,
  DGHLibrary,
  BADI.OptionsForm,
  BADI.Documentation.Dispatcher,
  BADI.Base.Documentation,
  ShellAPI,
  Math,
  BADI.DocumentationOptionsForm,
  ExclusionsForm,
  FolderConfig,
  UsefulSynEditFunctions,
  SynEditOptionsForm,
  UITypes, BADI.Module.Dispatcher, BADI.Types, BADI.ElementContainer, BADI.ResourceStrings,
  BADI.Options, Vcl.Themes;

{$R *.dfm}


(**

  This is an on execute event handler for the Edit Folder action.

  @precon  None.
  @postcon Allows the user to edit a selected directory from the list.

  @param   Sender as a TObject

**)
(**

  This is an on execute event handler for the Exclude File action.

  @precon  None.
  @postcon Allows the user to exclude a file based on the whole or part of its
           path.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actFileExcludeFileExecute(Sender: TObject);

ResourceString
  strExcludeFileFromResult = 'Exclude File from Result';
  strAreYouSure = 'Are you sure you want to exclude the below file from the results.';

Var
  strFileName: String;

Begin
  If FileName = '' Then
    Exit;
  strFileName := FileName;
  Delete(strFileName, 1, Length(PathRoot));
  strFileName := '*' + strFileName;
  If InputQuery(strExcludeFileFromResult,
    strAreYouSure,
    strFileName) Then
    Begin
      FFileExcludeList.Add(strFileName);
      actFileRecurseFoldersExecute(Sender);
    End;
End;

(**

  This is an on execute event handler for the Folder Options action.

  @precon  None.
  @postcon Displays the folder options dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actFileFoldersExecute(Sender: TObject);

Begin
  TfrmFolders.Execute(FINIFileName, FFolders);
End;

(**

  This is an on execute event handler for the Recurse Folders action.

  @precon  None.
  @postcon Chnages the scanning to use sub directories or Not.
  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actFileRecurseFoldersExecute(Sender: TObject);

ResourceString
  strWouldYouLikeToReScanCodeFiles = 'Would you like to re-scan the code files?';

Begin
  If MessageDlg(strWouldYouLikeToReScanCodeFiles, mtConfirmation,
    [mbYes, mbNo, mbCancel], 0) = mrYes Then
    actFileScanExecute(Sender);
End;

(**

  This is an on execute event handler for the Scan action.

  @precon  None.
  @postcon Starts the process of scanning the files.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actFileScanExecute(Sender: TObject);

ResourceString
  strPreparingToScan = 'Preparing to Scan';
  strPleaseWait = 'Please wait...';
  strScanningDirectories = 'Scanning Directories';

Var
  i        : Integer;
  R        : TScanResults;
  iPosition: Integer;

Begin
  Application.ProcessMessages;
  lvFileList.Items.Clear;
  FParseRecords.Clear;
  FProgressForm.Init(-1, strPreparingToScan, strPleaseWait);
  Try
    iPosition := 0;
    For i     := 0 To FFolders.Count - 1 Do
      If Integer(FFolders.Objects[i]) > 0 Then
        RecurseDirectories(FFolders.Names[i], FFolders.Names[i],
          iPosition, FFolders.ValueFromIndex[i]);
    FProgressForm.Init(iPosition, strScanningDirectories, strPleaseWait);
    iPosition := 0;
    For i     := 0 To FFolders.Count - 1 Do
      If Integer(FFolders.Objects[i]) > 0 Then
        Begin
          R := RecurseDirectories(FFolders.Names[i], FFolders.Names[i], iPosition,
            FFolders.ValueFromIndex[i], True);
          {
          FFolders.[i].SubItems.Add(Format('%d', [R.iDocConflicts]));
          FFolders.[i].SubItems.Add(Format('%d', [R.iHints]));
          FFolders.[i].SubItems.Add(Format('%d', [R.iWarnings]));
          FFolders.[i].SubItems.Add(Format('%d', [R.iErrors]));
          }
        End;
  Finally
    Try
      PopulateListView;
    Finally
      FProgressForm.Hide;
    End;
  End;
End;

(**

  This is an on execute event handler for the Recurse Zip Files action.

  @precon  None.
  @postcon Toggles the ability to recurse zip folders.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actResurseZipFilesExecute(Sender: TObject);

Begin
  FRecurseZipFiles := Not FRecurseZipFiles;
End;

(**

  This is an on update event handler for the Recurse Zip Files action.

  @precon  None.
  @postcon Updates the checked property of the action based on the recurse zip files
           property.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actResurseZipFilesUpdate(Sender: TObject);

Begin
  If Sender Is TAction Then
    (Sender As TAction).Checked := FRecurseZipFiles;
End;

(**

  This is an on execute event handler for the Tools Exclusions action.

  @precon  None.
  @postcon Invokes a dialogue that allows the editing of the list of exclusions.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actToolsExclusionsExecute(Sender: TObject);
Begin
  TfrmExclusions.Execute(FFileExcludeList);
End;

(**

  This is an on execute event handler for the Tools SynEdit Options action.

  @precon  None.
  @postcon Allows the editing of the editor options.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actToolsSynEditOptionsExecute(
  Sender: TObject);
Begin
  TfrmEditorOptions.Execute(Self, FSynEdit, False);
End;

(**

  This is an on execute event handler for the Wire Word Wrap action.

  @precon  None.
  @postcon Toggles the word wrap of the editor.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actViewWordWrapExecute(Sender: TObject);
Begin
  FSynEdit.WordWrap := Not FSynEdit.WordWrap;
End;

(**

  This is an on update event handler for the View Word Wrap action.

  @precon  None.
  @postcon Updates the checked property of the action.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.actViewWordWrapUpdate(Sender: TObject);
Begin
  (Sender As TAction).Checked := FSynEdit.WordWrap;
End;

(**

  This is an on click event handler for the options button.

  @precon  None.
  @postcon Displays the Options dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.btnOptionsClick(Sender: TObject);

Begin
  If TfrmOptions.Execute([Low(TVisibleTab) .. High(TVisibleTab)]) Then
    SynEdit1Change(Sender);
End;

(**

  This is a on execute event handler for the Doc Conflict, Hint, Warning and
  Error buttons.

  @precon  None.
  @postcon Repopulates the list based on what items are to be seen.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.ChangeVisibleItems(Sender: TObject);
Begin
  PopulateListView;
End;

(**


  This is an on click event handler for the Documentation Button.

  @precon  None.
  @postcon Invokes the documentation mechanism.

  @nocheck HardCodedString HardCodedInteger

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.DocumentationClick(Sender: TObject);

Const
  strTESTDocumentationFolder = '\TEST Documentation\';
  strTestDocumentationTitle = 'Test Documentation';
  strOPENVerb = 'OPEN';

Var
  i       : Integer;
  ADocType: TDocType;
  D : TBaseDocumentation;

Begin
  If TfrmDocumentationOptions.Execute(ADocType) Then
    Begin
      D := DocumentDispatcher(ExtractFilePath(ParamStr(0)) + strTESTDocumentationFolder,
        strTestDocumentationTitle, ADocType);
      Try
        For i := 0 To lvFileList.Items.Count - 1 Do
          D.Add(lvFileList.Items[i].SubItems[4]);
        D.OutputDocumentation;
        ShellExecute(hInstance, strOPENVerb, PChar(D.MainDocument), '', PChar(GetCurrentDir), SW_NORMAL);
      Finally
        Free;
      End;
    End;  
End;

(**

  This method tests whether the given file should be included or excluded from
  the result set.

  @precon  None .
  @postcon Returns true if the file should be excluded from the results .

  @note    The exclusion text is only tested for in the path / filename section
           after the root path , i . e . the root path is ignored .

  @param   strFileName as a String as a constant
  @return  a Boolean

**)
Function TfrmBrowseAndDocItTestForm.ExcludeFileFromResults(
  Const strFileName: String): Boolean;

Var
  i: Integer;

Begin
  Result := False;
  For i  := 0 To FFileExcludeList.Count - 1 Do
    If Like(FFileExcludeList[i], strFileName) Then
      Begin
        Result := True;
        Break;
      End;
End;

(**

  This is an on extract file event handler for the ZipForge component.

  @precon  None.
  @postcon Outputs the filename to the codesite view.

  @param   Sender   as a TObject
  @param   FileName as a String as a reference
  @param   FileAttr as a LongWord as a reference
  @param   Comment  as an AnsiString as a constant

**)
procedure TfrmBrowseAndDocItTestForm.ExtractFile(Sender: TObject;
  var FileName: String; var FileAttr: LongWord; const Comment: AnsiString);

ResourceString
  strExtractFileFailure = 'Extract File Failure';

begin
  CodeSite.Send(strExtractFileFailure, FileName);
end;

(**

  This is a check button change event handler for all the error, hint, etc
  checkboxes.

  @precon  None.
  @postcon Populates the list view.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.FilterChange(Sender: TObject);
Begin
  PopulateListView;
End;

(**

  This method focuses the editor when the focus event is fired.

  @precon  None.
  @postcon Focuses the editor when the focus event is fired.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.Focus(Sender: TObject);

Begin
  FSynEdit.SetFocus;
End;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Creates an instance of the Module Explorer Frame within the panel
           and initialises the SpecialTags global variable.

  @nocheck HardCodedString HardCodedInteger

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.FormCreate(Sender: TObject);

Const
  strWIN32 = 'WIN32';
  {$IFDEF WIN32}
  strMSWINDOWS = 'MSWINDOWS';
  {$ELSE}
  strLINUX = 'LINUX';
  {$ENDIF}

Begin
  FINIFileName            := BuildRootKey(Nil, Nil);
  FParseRecords           := TObjectList.Create(True);
  FSynEdit                := TDGHSynEdit.Create(Nil);
  FSynEdit.Parent         := Self;
  FSynEdit.Align          := alClient;
  FSynEdit.HideSelection  := False;
  FSynEdit.PopupMenu      := pmEdit;
  FSynEdit.OnChange       := SynEdit1Change;
  FSynEdit.OnStatusChange := SynEdit1StatusChange;
  FSynEdit.LoadFromINIFile(FINIFileName);
  FSynPasSyn := TSynPasSyn.Create(Nil);
  FSynVBSyn  := TSynVBSyn.Create(Nil);
  FSynCPPSyn := TSynCPPSyn.Create(Nil);
  FSynXMLSyn := TSynXMLSyn.Create(Nil);
  FSynDFMSyn := TSynDFMSyn.Create(Nil);
  FSynINISyn := TSynINISyn.Create(Nil);
  {$IFDEF WIN32}
  TBADIOptions.BADIOptions.Defines.Add(strWIN32);
  TBADIOptions.BADIOptions.Defines.Add(strMSWINDOWS);
  {$ELSE}
  TBADIOptions.BADIOptions.Defines.Add(strLINUX);
  {$ENDIF}
  FProgressForm                          := TfrmProgress.Create(Nil);
  FModuleExplorerFrame                   := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent            := pnlModuleExplorer;
  FModuleExplorerFrame.Align             := alClient;
  FModuleExplorerFrame.OnSelectionChange := SelectionChange;
  FModuleExplorerFrame.OnFocus           := Focus;
  FModuleExplorerFrame.OnRefresh         := RefreshExplorer;
  ActiveControl                          := lvFileList;
  FFileExcludeList                       := TStringList.Create;
  FFolders                               := TStringList.Create;
  LoadResults;
  LoadSettings;
  PopulateListView;
  FLastEdit       := 0;
  FTimer          := TTimer.Create(Self);
  FTimer.Interval := 100;
  FTimer.OnTimer  := TimerEvent;
End;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Cleans up the created instances from OnCreate.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.FormDestroy(Sender: TObject);

Begin
  FTimer.Free;
  SaveResults;
  SaveSettings;
  FSynEdit.Highlighter := Nil;
  FSynEdit.SaveToINIFile(FINIFileName);
  FSynEdit.Free;
  FSynINISyn.Free;
  FSynDFMSyn.Free;
  FSynXMLSyn.Free;
  FSynVBSyn.Free;
  FSynPasSyn.Free;
  FSynCPPSyn.Free;
  FFolders.Free;
  FFileExcludeList.Free;
  FModuleExplorerFrame.Free;
  FProgressForm.Free;
  FParseRecords.Free;
End;

(**

  This method gets the number of errors for the given files name.

  @precon  None.
  @postcon Gets the number of errors for the given files name.

  @nocheck ExceptionEating

  @param   strFileName as a String as a constant
  @param   strSource   as a String as a constant
  @param   iHints      as an Integer as a reference
  @param   iWarnings   as an Integer as a reference
  @param   iErrors     as an Integer as a reference
  @param   iConflicts  as an Integer as a reference
  @param   SourceType  as a TSourceType

**)
Procedure TfrmBrowseAndDocItTestForm.GetErrors(Const strFileName, strSource: String;
  Var iHints, iWarnings, iErrors, iConflicts: Integer;
  Const SourceType: TSourceType);

Var
  Source: TStringList;
  M     : TBaseLanguageModule;
  i     : Integer;
  C     : TElementContainer;

Begin
  iHints     := 0;
  iWarnings  := 0;
  iErrors    := 0;
  iConflicts := 0;
  Source     := TStringList.Create;;
  Try
    If SourceType = stFile Then
      Source.LoadFromFile(strFileName)
    Else
      Source.Text := strSource;
    Try
      M             := TBADIDispatcher.BADIDispatcher.Dispatcher(Source.Text, strFileName, False,
        [moParse, moCheckForDocumentConflicts]);
      If M <> Nil Then
        Try
          If M.FindElement(strHints) <> Nil Then
            iHints := M.FindElement(strHints).ElementCount;
          If M.FindElement(strWarnings) <> Nil Then
            iWarnings := M.FindElement(strWarnings).ElementCount;
          If M.FindElement(strErrors) <> Nil Then
            iErrors := M.FindElement(strErrors).ElementCount;
          C         := M.FindElement(strDocumentationConflicts);
          If C <> Nil Then
            Begin
              For i := 1 To C.ElementCount Do
                Inc(iConflicts, C.Elements[i].ElementCount);
            End;
        Finally
          M.Free;
        End;
    Except
      On E: Exception Do
        Begin
          {$IFDEF EUREKALOG_VER7}
          ExceptionManager.StandardEurekaNotify(ExceptObject, ExceptAddr)
          {$ELSE}
          FFileName := E.Message;
          {$ENDIF}
        End;
    End;
  Finally
    Source.Free;
  End;
End;

(**

  This is a getter method for the FileName property.

  @precon  None.
  @postcon Sets the returns the file name property based on the file list box
           selection.

  @return  a String

**)
Function TfrmBrowseAndDocItTestForm.GetFileName: String;
Begin
  Result := FFileName;
End;

(**

  This is a getter method for the Path Root property.

  @precon  None.
  @postcon Returns the root path of the selected file.

  @return  a String

**)
Function TfrmBrowseAndDocItTestForm.GetPathRoot: String;
Begin
  Result := FPathRoot;
End;

(**

  This method loads previous results from a text file.

  @precon  None.
  @postcon Loads previous results from a text file.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TfrmBrowseAndDocItTestForm.LoadResults;

Const
  strTxtExt = '.txt';

ResourceString
  strStartingUp = 'Starting Up...';
  strLoadingScanResults = 'Loading Scan Results...';

Var
  sl                                    : TStringList;
  iRecord                               : Integer;
  iErrors, iWarnings, iHints, iConflicts: Integer;
  strFileName                           : String;
  iErrorCode                            : Integer;
  iRootPath                             : Integer;

Begin
  sl := TStringList.Create;
  Try
    If FileExists(ChangeFileExt(FINIFileName, strTxtExt)) Then
      sl.LoadFromFile(ChangeFileExt(FINIFileName, strTxtExt));
    FProgressForm.Init(sl.Count, strStartingUp, strLoadingScanResults);
    Try
      For iRecord := 0 To sl.Count - 1 Do
        Begin
          strFileName := sl.Names[iRecord];
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 1), iErrors, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 2), iWarnings, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 3), iHints, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 4), iConflicts, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 5), iRootPath, iErrorCode);
          FParseRecords.Add(
            TParseRecord.Create(
              strFileName,
              Copy(strFileName, 1, iRootPath),
              iErrors,
              iWarnings,
              iHints,
              iConflicts
            )
          );
          If iRecord Mod 10 = 0 Then
            FProgressForm.UpdateProgress(iRecord, strFileName);
        End;
    Finally
      FProgressForm.Hide;
    End;
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the applications settings from an INI file.

  @precon  None.
  @postcon Loads the applications settings from an INI file.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TfrmBrowseAndDocItTestForm.LoadSettings;

Var
  i      : Integer;
  sl     : TStringList;
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    Top := iniFile.ReadInteger('Position', 'Top', Top);
    Left := iniFile.ReadInteger('Position', 'Left', Left);
    Height := iniFile.ReadInteger('Position', 'Height', Height);
    Width := iniFile.ReadInteger('Position', 'Width', Width);
    FRecurseZipFiles := iniFile.ReadBool('ZipFiles', 'Recuse', False);
    WindowState := TWindowState(iniFile.ReadInteger('Position', 'WindowState',
      Integer(wsNormal)));
    lvFileList.Width := iniFile.ReadInteger('Position', 'FileSplitter', lvFileList.Width);
    lvFileList.Columns[0].Width := iniFile.ReadInteger('Columns', '1',
      lvFileList.Columns[0].Width);
    lvFileList.Columns[1].Width := iniFile.ReadInteger('Columns', '2',
      lvFileList.Columns[1].Width);
    lvFileList.Columns[2].Width := iniFile.ReadInteger('Columns', '3',
      lvFileList.Columns[2].Width);
    lvFileList.Columns[3].Width := iniFile.ReadInteger('Columns', '4',
      lvFileList.Columns[3].Width);
    lvFileList.Columns[4].Width := iniFile.ReadInteger('Columns', '5',
      lvFileList.Columns[4].Width);
    pnlModuleExplorer.Width := iniFile.ReadInteger('Position', 'Splitter',
      pnlModuleExplorer.Width);
    sl := TStringList.Create;
    Try
      iniFile.ReadSection('Folders', sl);
      For i := 0 To sl.Count - 1 Do
        FFolders.AddObject(Format('%s=%s', [sl[i],
          iniFile.ReadString('Folders', sl[i], '')]),
          TObject(iniFile.ReadBool('Folders.Enabled', sl[i], False)));
      iniFile.ReadSection('ExcludedFiles', sl);
      For i := 0 To sl.Count - 1 Do
        FFileExcludeList.Add(iniFile.ReadString('ExcludedFiles', sl[i], ''));
    Finally
      sl.Free;
    End;
    i := iniFile.ReadInteger('Setup', 'Selection', 0);
    actFileRecurseFolders.Checked := iniFile.ReadBool('Setup', 'Recurse', False);
    actViewErrors.Checked := iniFile.ReadBool('Setup', 'Errors', False);
    actViewWarnings.Checked := iniFile.ReadBool('Setup', 'Warnings', False);
    actViewHints.Checked := iniFile.ReadBool('Setup', 'Hints', False);
    actViewDocConflicts.Checked := iniFile.ReadBool('Setup', 'Conflicts', False);
    If lvFileList.Items.Count > i Then
      lvFileList.ItemIndex := i;
    FIndex := iniFile.ReadInteger('Setup', 'SelectedItem', -1);
    LoadHighlighterFromINIFile(iniFile, FSynPasSyn);
    LoadHighlighterFromINIFile(iniFile, FSynVBSyn);
    LoadHighlighterFromINIFile(iniFile, FSynCPPSyn);
    LoadHighlighterFromINIFile(iniFile, FSynXMLSyn);
    LoadHighlighterFromINIFile(iniFile, FSynDFMSyn);
    LoadHighlighterFromINIFile(iniFile, FSynINISyn);
    LoadThemes;
    TStyleManager.TrySetStyle(iniFile.ReadString('Setup', 'VCL Style', 'Windows'), False);
    sbrStatusBar.Panels[4].Text := TStyleManager.ActiveStyle.Name;
  Finally
    iniFile.Free;
  End;
End;

(**

  This method loads any .VSF theme files from either the EXE location, INI location or the parent to the
  INI location.

  @precon  None.
  @postcon Any found .VSF files are loaded if they are valid.

**)
Procedure TfrmBrowseAndDocItTestForm.LoadThemes;

  (**

    This procedure searches for style files in the given directory.

    @precon  None.
    @postcon Any style files in the given directory are loaded if they are valid.

    @param   strPath as a String as a constant

  **)
  Procedure SearchForThemes(Const strPath : String);

  Const
    strVCLStyleFileExt = '*.vsf';

  Var
    recSearch: TSearchRec;
    iResult: Integer;
  
  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadThemes/SearchForThemes', tmoTiming);{$ENDIF}
    iResult := System.SysUtils.FindFirst(strPath + strVCLStyleFileExt, faAnyFile, recSearch);
    Try
      While iResult = 0 Do
        Begin
          If TStyleManager.IsValidStyle(strPath + recSearch.Name) Then
            TStyleManager.LoadFromFile(strPath + recSearch.Name);
          iResult := FindNext(recSearch);
        End;
    Finally
      System.SysUtils.FindClose(recSearch);
    End;
  End;

Var
  strPath : String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadThemes', tmoTiming);{$ENDIF}
  // EXE Location
  strPath := ExtractFilePath(ParamStr(0));
  SearchForThemes(strPath);
  // %appdata%\Season's Fall\
  strPath := ExtractFilePath(FINIFileName);
  SearchForThemes(strPath);
End;

(**

  This is a Select item event handler for the list view.

  @precon  None.
  @postcon Saves any changes and options the new file selected.

  @nocheck HardCodedString HardCodedInteger

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TfrmBrowseAndDocItTestForm.lvFileListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

Begin
  If lvFileList.Selected <> Nil Then
    Begin
      FileName := lvFileList.Selected.SubItems[5];
      PathRoot := lvFileList.Selected.SubItems[6];
      RefreshModuleExplorer;
    End;
End;

(**

  This is an on custom draw item event handler for the list views.

  @precon  None.
  @postcon Draws the list view items with colours, path ellipses and alignments.

  @nocheck HardCodedString HardCodedInteger

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TfrmBrowseAndDocItTestForm.lvFilesCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Var DefaultDraw: Boolean);

Var
  iLength   : Integer;
  R         : TRect;
  i         : Integer;
  iValue    : Integer;
  iErrorCode: Integer;

  (**

    This function returns the rectangle for the indexed sub item of the listview.

    @precon  iIndex must be a valid index into the column collection.
    @postcon Returns the rectangle for the indexed sub item of the listview.

    @nocheck HardCodedString HardCodedInteger

    @param   iIndex as an Integer
    @return  a TRect

  **)
  Function GetSubItemRect(Const iIndex: Integer): TRect;

  Var
    j: Integer;

  Begin
    Result := Item.DisplayRect(drBounds);
    For j  := 0 To iIndex Do
      Begin
        Inc(Result.Left, Sender.Column[j].Width);
        Result.Right := Result.Left + Sender.Column[j + 1].Width;
      End;
  End;

Begin
  DefaultDraw               := False;
  R                         := Item.DisplayRect(drSelectBounds);
  Sender.Canvas.Brush.Color := clWindow;
  If Item.Selected Then
    Begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color  := clHighlightText;
    End;
  Sender.Canvas.FillRect(R);
  R := Item.DisplayRect(drLabel);
  Dec(R.Right);
  iLength := Length(Item.Caption);
  DrawText(Sender.Canvas.Handle, PChar(Item.Caption), iLength, R,
    DT_LEFT Or DT_PATH_ELLIPSIS);
  R := GetSubItemRect(0);
  iLength := Length(Item.SubItems[0]);
  DrawText(Sender.Canvas.Handle, PChar(Item.SubItems[0]), iLength, R,
    DT_LEFT Or DT_PATH_ELLIPSIS);
  For i := 1 To Min(Item.SubItems.Count - 1, 4) Do
    Begin
      R                         := GetSubItemRect(i);
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color  := clWindowText;
      Val(Item.SubItems[i], iValue, iErrorCode);
      If iValue > 0 Then
        Begin
          Case i Of
            1: Sender.Canvas.Brush.Color := clSkyBlue;
            2: Sender.Canvas.Brush.Color := clGreen;
            3: Sender.Canvas.Brush.Color := clYellow;
            4: Sender.Canvas.Brush.Color := clRed;
          End;
          Case i Of
            1: Sender.Canvas.Font.Color := clBlack;
            2: Sender.Canvas.Font.Color := clWhite;
            3: Sender.Canvas.Font.Color := clRed;
            4: Sender.Canvas.Font.Color := clYellow;
          End;
        End;
      If Item.Selected Then
        Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.FillRect(R);
      Inc(R.Left, 2);
      Inc(R.Top, 2);
      Dec(R.Right, 2);
      Dec(R.Bottom, 2);
      Sender.Canvas.Refresh;
      DrawText(Sender.Canvas.Handle, PChar(Item.SubItems[i]),
        Length(Item.SubItems[i]), R, DT_RIGHT);
      R.Left := R.Right;
    End;
End;

(**

  This method popluates the list view based on the filter checkboxes.

  @precon  None.
  @postcon Popluates the list view based on the filter checkboxes.

**)
Procedure TfrmBrowseAndDocItTestForm.PopulateListView;

Var
  i          : Integer;
  rec        : TParseRecord;
  liItem     : TListItem;
  boolInclude: Boolean;

Begin
  Try
    lvFileList.Clear;
    lvFileList.Items.BeginUpdate;
    For i := 0 To FParseRecords.Count - 1 Do
      Begin
        rec         := FParseRecords[i] As TParseRecord;
        boolInclude :=
          Not actViewErrors.Checked And
          Not actViewWarnings.Checked And
          Not actViewHints.Checked And
          Not actViewDocConflicts.Checked;
        boolInclude := boolInclude Or (actViewErrors.Checked And (rec.Errors > 0));
        boolInclude := boolInclude Or (actViewWarnings.Checked And (rec.Warnings > 0));
        boolInclude := boolInclude Or (actViewHints.Checked And (rec.Hints > 0));
        boolInclude := boolInclude Or
          (actViewDocConflicts.Checked And (rec.Conflicts > 0));
        If boolInclude Then
          Begin
            liItem         := lvFileList.Items.Add;
            liItem.Caption := IntToStr(i);
            liItem.SubItems.Add(ExtractFileName(rec.FileName));
            liItem.SubItems.Add(Format('%d', [rec.Conflicts]));
            liItem.SubItems.Add(Format('%d', [rec.Hints]));
            liItem.SubItems.Add(Format('%d', [rec.Warnings]));
            liItem.SubItems.Add(Format('%d', [rec.Errors]));
            liItem.SubItems.Add(rec.FileName);
            liItem.SubItems.Add(rec.PathRoot);
          End;
      End;
    If FIndex > lvFileList.Items.Count - 1 Then
      FIndex             := lvFileList.Items.Count - 1;
    lvFileList.ItemIndex := FIndex;
    If lvFileList.Selected <> Nil Then
      lvFileList.Selected.MakeVisible(False);
  Finally
    lvFileList.Items.EndUpdate;
  End;
  UpdateStatusBar;
End;

(**

  This method recurse the directories searching for files.

  @precon  None.
  @postcon Recurse the directories searching for files.

  @param   strRoot       as a String as a constant
  @param   strDirectory  as a String as a constant
  @param   iPosition     as an Integer as a reference
  @param   strExtensions as a String as a constant
  @param   boolScan      as a Boolean
  @return  a TScanResults

**)
Function TfrmBrowseAndDocItTestForm.RecurseDirectories(Const strRoot,
  strDirectory: String; Var iPosition: Integer; Const strExtensions: String;
  Const boolScan: Boolean = False): TScanResults;

ResourceString
  strScanning = 'Scanning %s %1.0n';

Var
  recFile                   : TSearchRec;
  iResult                   : Integer;
  iHints, iWarnings, iErrors: Integer;
  iConflicts                : Integer;
  strFileName               : String;
  R                         : TScanResults;
  slExts                    : TStringList;
  i                         : Integer;
  {: @debug boolResult                : Boolean; }
  strSource                 : String;

Begin
  Result.iDocConflicts := 0;
  Result.iHints        := 0;
  Result.iWarnings     := 0;
  Result.iErrors       := 0;
  FprogressForm.UpdateProgress(iPosition, strDirectory);
  slExts               := TStringList.Create;
  Try
    slExts.Text := strExtensions;
    slExts.Text := StringReplace(slExts.Text, ';', #13#10, [rfReplaceAll]);
    For i       := 0 To slExts.Count - 1 Do
      Begin
        iResult := FindFirst(strDirectory + '\*' + slExts[i], faAnyFile, recFile);
        Try
          While iResult = 0 Do
            Begin
              strFileName := strDirectory + '\' + recFile.Name;
              If Not ExcludeFileFromResults(strFileName) Then
                Begin
                  Inc(iPosition);
                  If boolScan And
                    TBADIDispatcher.BADIDispatcher.CanParseDocument(ExtractFileExt(recFile.Name)) Then
                    Begin
                      FProgressForm.UpdateProgress(iPosition, strFileName);
                      GetErrors(strFileName, strSource, iHints, iWarnings,
                        iErrors, iConflicts, stFile);
                      Inc(Result.iDocConflicts, iConflicts);
                      Inc(Result.iHints, iHints);
                      Inc(Result.iWarnings, iWarnings);
                      Inc(Result.iErrors, iErrors);
                      FParseRecords.Add(TParseRecord.Create(strFileName, strRoot,
                        iErrors, iWarnings, iHints, iConflicts));
                      Application.ProcessMessages;
                    End
                  Else
                    FProgressForm.UpdateProgress(iPosition,
                      Format(strScanning, [strDirectory, Int(iPosition)]));
                End;
              iResult := FindNext(recFile);
            End;
        Finally
          FindClose(recFile);
        End;
      End;
  Finally
    slExts.Free;
  End;
  If Not actFileRecurseFolders.Checked Then
    Exit;
  iResult := FindFirst(strDirectory + '\*.*', faAnyFile, recFile);
  Try
    While iResult = 0 Do
      Begin
        If (recFile.Attr And faDirectory <> 0) And (recFile.Name[1] <> '.') Then
          Begin
            R := RecurseDirectories(strRoot, strDirectory + '\' + recFile.Name,
              iPosition, strExtensions, boolScan);
            Inc(Result.iDocConflicts, R.iDocConflicts);
            Inc(Result.iHints, R.iHints);
            Inc(Result.iWarnings, R.iWarnings);
            Inc(Result.iErrors, R.iErrors);
          End;
        {: @debug If FRecurseZipFiles And Like('*.zip', recFile.Name) Then
          Begin
            Z := TZipForge.Create(Nil);
            Try
              Z.OnProcessFileFailure := ProcessFileFailure;
              //Z.OnRequestBlankVolume := RequestBlankVolume;
              //Z.OnRequestFirstVolume;
              //Z.OnRequestLastVolume;
              //Z.OnRequestMiddleVolume;
              Z.OnExtractFile := ExtractFile;
              Z.FileName := strDirectory + '\' + recFile.Name;
              Try
                Z.OpenArchive;
                Try
                  slExts := TStringList.Create;
                  Try
                    slExts.Text := strExtensions;
                    slExts.Text := StringReplace(slExts.Text, ';', #13#10, [rfReplaceAll]);
                    For i       := 0 To slExts.Count - 1 Do
                      Begin
                        boolResult := Z.FindFirst('*' + slExts[i], recZip);
                        If recZip.Encrypted Then
                          boolResult := False;
                        While boolResult Do
                          Begin
                            strFileName := strDirectory + '\' + recFile.Name + '\' +
                              recZip.StoredPath + recZip.FileName;
                            If Not ExcludeFileFromResults(strFileName) Then
                              Begin
                                Inc(iPosition);
                                If boolScan And TBADIDispatcher.BADIDispatcher.CanParseDocument(ExtractFileExt(recZip.FileName)) Then
                                  Begin
                                    FProgressForm.UpdateProgress(iPosition, strFileName);
                                    Z.ExtractToString(recZip.StoredPath +
                                      recZip.FileName, strSource);
                                    GetErrors(strFileName, strSource, iHints,
                                      iWarnings, iErrors, iConflicts, stCode);
                                    Inc(Result.iDocConflicts, iConflicts);
                                    Inc(Result.iHints, iHints);
                                    Inc(Result.iWarnings, iWarnings);
                                    Inc(Result.iErrors, iErrors);
                                    FParseRecords.Add(
                                      TParseRecord.Create(
                                        strFileName,
                                        strRoot,
                                        iErrors,
                                        iWarnings,
                                        iHints,
                                        iConflicts));
                                    Application.ProcessMessages;
                                  End
                                Else
                                  FProgressForm.UpdateProgress(iPosition,
                                    Format('Please wait... %1.0n', [Int(iPosition)]));
                              End;
                            boolResult := Z.FindNext(recZip);
                          End;
                      End;
                  Finally
                    slExts.Free;
                  End;
                Finally
                  Z.CloseArchive;
                End;
              Except
                On E : Exception Do
                  CodeSite.Send('Zip Failure', Z.FileName);
              End;
            Finally
              Z.Free;
            End;
          End;}
        iResult := FindNext(recFile);
      End;
  Finally
    FindClose(recFile);
  End;
End;

(**

  This is an on refresh event handler for the module explorer called back.

  @precon  None.
  @postcon Refreshes the module explorer.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.RefreshExplorer(Sender: TObject);
Begin
  SynEdit1Change(Sender);
End;

(**

  This method updates the module explorer.

  @precon  None.
  @postcon Updates the module explorer.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TfrmBrowseAndDocItTestForm.RefreshModuleExplorer;

Var
  M: TBaseLanguageModule;
  iErrors : Integer;
  iWarnings : Integer;
  iHints : Integer;
  iConflicts : Integer;
  C: TElementContainer;
  i: Integer;
  R: TParseRecord;

Begin
  M := TBADIDispatcher.BADIDispatcher.Dispatcher(FSynEdit.Text, FileName, FSynEdit.Modified, [moParse,
    moCheckForDocumentConflicts]);
  If M <> Nil Then
    Try
      FModuleExplorerFrame.RenderModule(M);
      iHints := 0;
      If M.FindElement(strHints) <> Nil Then
        iHints := M.FindElement(strHints).ElementCount;
      iWarnings := 0;
      If M.FindElement(strWarnings) <> Nil Then
        iWarnings := M.FindElement(strWarnings).ElementCount;
      iErrors := 0;
      If M.FindElement(strErrors) <> Nil Then
        iErrors := M.FindElement(strErrors).ElementCount;
      iConflicts := 0;
      C         := M.FindElement(strDocumentationConflicts);
      If C <> Nil Then
        Begin
          For i := 1 To C.ElementCount Do
            Inc(iConflicts, C.Elements[i].ElementCount);
        End;
      R := FParseRecords[StrToInt(lvFileList.Selected.Caption)] As TParseRecord;
      R.Errors := iErrors;
      R.Warnings := iWarnings;
      R.Hints := iHints;
      R.Conflicts := iConflicts;
      UpdateStatusBar;
      If lvFileList.Selected <> Nil Then
        Begin
          lvFileList.Selected.SubItems[4] := IntToStr(iErrors);
          lvFileList.Selected.SubItems[3] := IntToStr(iWarnings);
          lvFileList.Selected.SubItems[2] := IntToStr(iHints);
          lvFileList.Selected.SubItems[1] := IntToStr(iConflicts);
        End;
    Finally
      M.Free;
    End;
  FLastEdit := 0; // Reset code timer to no changes
End;

(**

  This method saves the results to a text file.

  @precon  None.
  @postcon Saves the results to a text file.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TfrmBrowseAndDocItTestForm.SaveResults;

Var
  sl     : TStringList;
  iRecord: Integer;
  P: TParseRecord;

Begin
  sl := TStringList.Create;
  Try
    FProgressForm.Init(FParseRecords.Count, 'Shutting Down',
      'Saving Scan Results...');
    Try
      For iRecord := 0 To FParseRecords.Count - 1 Do
        Begin
          P := FParseRecords[iRecord] As TParseRecord;
          If iRecord Mod 10 = 0 Then
            FProgressForm.UpdateProgress(iRecord, P.FileName);
            sl.Add(Format('%s=%d,%d,%d,%d,%d', [P.FileName, P.Errors, P.Warnings,
              P.Hints, P.Conflicts, Length(P.PathRoot)]));
        End;
      sl.SaveToFile(ChangeFileExt(FINIFileName, '.txt'));
    Finally
      FProgressForm.Hide;
    End;
  Finally
    sl.Free;
  End;
End;

(**

  This method saves the applications settings to an INI file.

  @precon  None.
  @postcon Saves the applications settings to an INI file.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TfrmBrowseAndDocItTestForm.SaveSettings;

Var
  i         : Integer;
  recWndPlmt: TWindowPlacement;
  iniFile   : TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    recWndPlmt.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @recWndPlmt);
    iniFile.WriteInteger('Position', 'Top', recWndPlmt.rcNormalPosition.Top);
    iniFile.WriteInteger('Position', 'Left', recWndPlmt.rcNormalPosition.Left);
    iniFile.WriteInteger('Position', 'Height',
      recWndPlmt.rcNormalPosition.Bottom - recWndPlmt.rcNormalPosition.Top);
    iniFile.WriteInteger('Position', 'Width',
      recWndPlmt.rcNormalPosition.Right - recWndPlmt.rcNormalPosition.Left);
    iniFile.WriteInteger('Position', 'WindowState', Integer(WindowState));
    iniFile.WriteInteger('Position', 'FileSplitter', lvFileList.Width);
    iniFile.WriteBool('ZipFiles', 'Recuse', FRecurseZipFiles);
    iniFile.WriteInteger('Columns', '1', lvFileList.Columns[0].Width);
    iniFile.WriteInteger('Columns', '2', lvFileList.Columns[1].Width);
    iniFile.WriteInteger('Columns', '3', lvFileList.Columns[2].Width);
    iniFile.WriteInteger('Columns', '4', lvFileList.Columns[3].Width);
    iniFile.WriteInteger('Columns', '5', lvFileList.Columns[4].Width);
    iniFile.WriteInteger('Position', 'Splitter', pnlModuleExplorer.Width);
    iniFile.EraseSection('Folders');
    iniFile.EraseSection('Folders.Enabled');
    For i := 0 To FFolders.Count - 1 Do
      Begin
        iniFile.WriteString('Folders', FFolders.Names[i], FFolders.ValueFromIndex[i]);
        iniFile.WriteBool('Folders.Enabled', FFolders.Names[i],
          Boolean(Integer(FFolders.Objects[i])));
      End;
    iniFile.EraseSection('ExcludedFiles');
    For i := 0 To FFileExcludeList.Count - 1 Do
      iniFile.WriteString('ExcludedFiles', Format('File%d', [i]), FFileExcludeList[i]);
    iniFile.WriteInteger('Setup', 'Selection', lvFileList.ItemIndex);
    iniFile.WriteBool('Setup', 'Recurse', actFileRecurseFolders.Checked);
    iniFile.WriteBool('Setup', 'Errors', actViewErrors.Checked);
    iniFile.WriteBool('Setup', 'Warnings', actViewWarnings.Checked);
    iniFile.WriteBool('Setup', 'Hints', actViewHints.Checked);
    iniFile.WriteBool('Setup', 'Conflicts', actViewDocConflicts.Checked);
    iniFile.WriteInteger('Setup', 'SelectedItem', lvFileList.ItemIndex);
    SaveHighlighterToINIFile(iniFile, FSynPasSyn);
    SaveHighlighterToINIFile(iniFile, FSynVBSyn);
    SaveHighlighterToINIFile(iniFile, FSynCPPSyn);
    SaveHighlighterToINIFile(iniFile, FSynXMLSyn);
    SaveHighlighterToINIFile(iniFile, FSynDFMSyn);
    SaveHighlighterToINIFile(iniFile, FSynINISyn);
    iniFile.WriteString('Setup', 'VCL Style', TStyleManager.ActiveStyle.Name);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This is an on mouse down event handler for the statusbar control.

  @precon  None.
  @postcon Displays a context menu for the VCL Themes that are available.

  @nocheck HardCodedInteger

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfrmBrowseAndDocItTestForm.sbrStatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);

Var
  iPanel: Integer;
  P: TStatusPanel;
  iLeft: Integer;
  Pt: TPoint;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'sbrStatusbarMouseDown', tmoTiming); {$ENDIF}
  iLeft := 0;
  If (Shift = [ssRight]) And (mbRight = Button) Then
    For iPanel := 0 To sbrStatusBar.Panels.Count - 1 Do
      Begin
        P := sbrStatusBar.Panels[iPanel];
        Pt := Point(X, Y);
        Pt := sbrStatusBar.ClientToScreen(Pt);
        If (X >= iLeft) And (X <= iLeft + P.Width) Then
          Begin
            Case iPanel Of
              4: ShowVCLThemePopup(Pt);
            End;
            Break;
          End;
        Inc(iLeft, P.Width);
      End;
End;

(**

  This is an OnSelctionChange event handler for the module explorer.

  @precon  None.
  @postcon Moves the cursor to select item in the code.

  @param   iIdentLine   as an Integer
  @param   iIdentCol    as an Integer
  @param   iCommentLine as an Integer

**)
Procedure TfrmBrowseAndDocItTestForm.SelectionChange(Const iIdentLine, iIdentCol, iCommentLine : Integer);

Begin
  If iIdentLine * iIdentCol > 0 Then
    Begin
      FSynEdit.CaretX  := iIdentCol;
      FSynEdit.CaretY  := iIdentLine;
      FSynEdit.TopLine := iIdentLine - FSynEdit.LinesInWindow Div 2;
    End;
End;

(**

  This is a setter method for the FileName property.

  @precon  None.
  @postcon Sets the SynEdit file to the file named in the FileName property.

  @param   Value as a String as a constant

**)
Procedure TfrmBrowseAndDocItTestForm.SetFileName(Const Value: String);

ResourceString
  strMsg = 'The file "%s" has changed.'#13#10'Do you want to save the changes?';
  
Var
  strExt       : String;
  {: @debug strSource    : String;
  strFileName  : String;
  iPos         : Integer; }

Begin
  If FSynEdit.Modified Then
    Case MessageDlg(Format(strMsg, [FFilename]), mtConfirmation, [mbYes, mbNo,
      mbCancel], 0) Of
      mrYes: FSynEdit.Lines.SaveToFile(FFileName);
      mrCancel: Abort;
    End;
  FFileName := Value;
  Caption   := FFileName;
  If Not Like('*.zip\*', FFileName) Then
    Begin
      FSynEdit.Lines.LoadFromFile(FFileName);
      FSynEdit.ReadOnly := (GetFileAttributes(PChar(FFileName)) And FILE_ATTRIBUTE_READONLY > 0)
    End {: @debug Else
    Begin
      Z := TZipForge.Create(Nil);
      Try
        strFileName := LowerCase(FFileName);
        iPos        := Pos('.zip', strFileName);
        Z.FileName  := Copy(FFileName, 1, iPos + 3);
        Z.OpenArchive;
        Try
          Z.ExtractToString(Copy(FFileName, iPos + 5, MAX_PATH), strSource);
          FSynEdit.Lines.Text := strSource;
          FSynEdit.ReadOnly := True;
        Finally
          Z.CloseArchive;
        End;
      Finally
        Z.Free;
      End;
    End};
  FSynEdit.Modified := False;
  strExt            := LowerCase(ExtractFileExt(FFileName));
  If IsKeyWord(strExt, ['.dpk', '.dpr', '.pas']) Then
    FSynEdit.Highlighter := FSynPasSyn
  Else If IsKeyWord(strExt, ['.dfm']) Then
    FSynEdit.Highlighter := FSynDFMSyn
  Else If IsKeyWord(strExt, ['.bnf']) Then
    FSynEdit.Highlighter := FSynCPPSyn
  Else If IsKeyWord(strExt, ['.htm', '.html', '.xml']) Then
    FSynEdit.Highlighter := FSynXMLSyn
  Else If IsKeyWord(strExt, ['.bas', '.cls', '.frm']) Then
    FSynEdit.Highlighter := FSynVBSyn
  Else If IsKeyWord(strExt, ['.ini', '.tli']) Then
    FSynEdit.Highlighter := FSynINISyn
  Else
    FSynEdit.Highlighter := Nil;
  SynEdit1Change(Self);
End;

(**

  This is a setter method for the PathRoot property.

  @precon  None.
  @postcon Sets the root path of the selected file.

  @param   Value as a String as a constant

**)
Procedure TfrmBrowseAndDocItTestForm.SetPathRoot(Const Value: String);
Begin
  FPathRoot := Value;
End;

(**

  This is an on click event handler for the Tokens button.

  @precon  None.
  @postcon Displays a form showing the tokens from the current file.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.ShowTokensClick(Sender: TObject);

Var
  M: TBaseLanguageModule;

Begin
  M := TBADIDispatcher.BADIDispatcher.Dispatcher(FSynEdit.Text, FileName, True,
    [moParse, moCheckForDocumentConflicts]);
  If M <> Nil Then
    Try
      TfrmTokenForm.Execute(M);
    Finally
      M.Free;
    End;
End;

Procedure TfrmBrowseAndDocItTestForm.ShowVCLThemePopup(Const Pt: TPoint);

Var
  astrNames : TArray<String>;
  iName: Integer;
  MenuItem : TMenuItem;
  Names : TList<TNameIndexRec>;
  
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ShowVCLThemePopup', tmoTiming);{$ENDIF}
  pmStatusbar.Items.Clear;
  astrNames := TStyleManager.StyleNames;
  Names := TList<TNameIndexRec>.Create(TNameIndexComparer.Create);
  Try
    For iName := Low(astrNames) To High(astrNames) Do
      Names.Add(TNameIndexRec.Create(astrNames[iName], iName));
    Names.Sort;
    For iName := 0 To Names.Count - 1 Do
      Begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := Names[iName].FName;
        MenuItem.Tag := Names[iName].FIndex;
        MenuItem.OnClick := VCLThemeClick;
        pmStatusbar.Items.Add(MenuItem);
      End;
    pmStatusbar.Popup(Pt.X, Pt.Y);
  Finally
    Names.Free;
  End;
End;

(**

  This is an on click event handler for the Checkbox1 control.

  @precon  None
  @postcon Displays or hides the special character markers in the SynEdit1
           control.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.SpecialCharactersClick(Sender: TObject);
Begin
  If actViewSpecialCharacters.Checked Then
    FSynEdit.Options := FSynEdit.Options + [eoShowSpecialChars]
  Else
    FSynEdit.Options := FSynEdit.Options - [eoShowSpecialChars];
End;

(**

  This is an on change event handler for the SynEdit1 control.

  @precon  None.
  @postcon Creates a PascalDocModule class and passes it to the module explorer.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.SynEdit1Change(Sender: TObject);

Begin
  FLastEdit := GetTickCount;
End;

(**

  This is an on status change event handler for the editor.

  @precon  None.
  @postcon Updates the top panels with the cursor position.

  @nocheck HardCodedString HardCodedInteger

  @param   Sender  as a TObject
  @param   Changes as a TSynStatusChanges

**)
Procedure TfrmBrowseAndDocItTestForm.SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);

ResourceString
  strLineColumn = 'Line %d, Column %d';

Begin
  FLastEdit               := GetTickCount;
  sbrStatusBar.Panels[4].Text := Format(strLineColumn, [FSynEdit.CaretY, FSynEdit.CaretX]);
End;

(**

  This is an on timer event handler.

  @precon  None.
  @postcon Checks to see if the editor has been idle for more than a second
           after changed, if so refreshes the module explorer.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.TimerEvent(Sender: TObject);

Const
  iInterval: Integer = 1000; // 1 second

Begin
  If (FLastEdit > 0) And (FLastEdit + iInterval < GetTickCount) Then
    Begin
      FTimer.Enabled := False;
      Try
        RefreshModuleExplorer;
      Finally
        FTimer.Enabled := True;
      End;
    End;
End;

(**

  This method updates the statusbar with the current number of errors, warnings, etc.

  @precon  None.
  @postcon Teh status bar is updated.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TfrmBrowseAndDocItTestForm.UpdateStatusbar;

Var
  iErrors, iWarnings, iHints, iConflicts : Integer;
  i : Integer;
  R : TParseRecord;

Begin
  iErrors := 0;
  iWarnings := 0;
  iHints := 0;
  iConflicts := 0;
  For i := 0 To FParseRecords.Count - 1 Do
    Begin
      R := FParseRecords[i] As TParseRecord;
      Inc(iErrors, R.Errors);
      Inc(iWarnings, R.Warnings);
      Inc(iHints, R.Hints);
      Inc(iConflicts, R.Conflicts);
    End;
  sbrStatusBar.Panels[0].Text := Format('%1.0n Conflicts', [Int(iConflicts)]);
  sbrStatusBar.Panels[1].Text := Format('%1.0n Hints', [Int(iHints)]);
  sbrStatusBar.Panels[2].Text := Format('%1.0n Warnings', [Int(iWarnings)]);
  sbrStatusBar.Panels[3].Text := Format('%1.0n Errors', [Int(iErrors)]);
End;

(**

  This method applies the select VCL style from the context menu.

  @precon  None.
  @postcon The VCL style selected is applied.

  @nocheck HardCodedInteger

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.VCLThemeClick(Sender: TObject);

Var
  MI : TMenuItem;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'VCLThemeClick', tmoTiming);{$ENDIF}
  If Sender Is TMenuItem Then
    Begin
      MI := Sender As TMenuItem;
      TStyleManager.TrySetStyle(TStyleManager.StyleNames[MI.Tag]);
      sbrStatusBar.Panels[4].Text := TStyleManager.ActiveStyle.Name;
    End;
End;

{ TParseRecord }

(**

  This is a constructor for the TParseRecord class.

  @precon  None .
  @postcon Initialises the record class with information .

  @param   strFileName as a String as a constant
  @param   strPathRoot as a String as a constant
  @param   iErrors     as an Integer
  @param   iWarnings   as an Integer
  @param   iHints      as an Integer
  @param   iConflicts  as an Integer

**)
Constructor TParseRecord.Create(Const strFileName, strPathRoot: String; Const iErrors,
  iWarnings, iHints, iConflicts: Integer);

Begin
  FFileName  := strFileName;
  FPathRoot  := strPathRoot;
  FErrors    := iErrors;
  FWarnings  := iWarnings;
  FHints     := iHints;
  FConflicts := iConflicts;
End;

(**

  A constructor for the TGENameIndexRec class.

  @precon  None.
  @postcon Initialises the record.

  @param   strName as a String as a constant
  @param   iIndex  as an Integer as a constant

**)
Constructor TfrmBrowseAndDocItTestForm.TNameIndexRec.Create(Const strName: String; Const iIndex: Integer); //FI:W525

Begin
  FName := strName;
  FIndex := iIndex;
End;

(**

  This is an overridden Ciompare method of the IComparer interface.

  @precon  None.
  @postcon This method sorts the TGENameIndexRec records by their FName field.

  @param   Left  as a TGENameIndexRec as a constant
  @param   Right as a TGENameIndexRec as a constant
  @return  an Integer

**)
Function TfrmBrowseAndDocItTestForm.TNameIndexComparer.Compare(Const Left, Right: TNameIndexRec): Integer;

Begin
  Result := CompareText(Left.FName, Right.FName);
End;

End.
