(**

  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Version 1.0
  @Date    28 Apr 2013
  @Author  David Hoyle

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
  ModuleExplorerFrame,
  BaseLanguageModule,
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
  ProgressForm,
  Buttons,
  ImgList,
  ToolWin,
  XPMan,
  SynHighlighterXML,
  DGHSynEdit,
  ZipForge,
  SynHighlighterDfm;

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}


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
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol: Integer);
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
    {$IFDEF D2005} Strict {$ENDIF} Private
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
    Function GetFileName: String;
    Procedure SetFileName(Const Value: String);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Function GetPathRoot: String;
    Procedure SetPathRoot(Const Value: String);
    Function RecurseDirectories(strRoot, strDirectory: String;
      Var iPosition: Integer; strExtensions: String;
      boolScan: Boolean = False): TScanResults;
    Procedure RefreshExplorer(Sender: TObject);
    Procedure PopulateListView;
    Function ExcludeFileFromResults(strFileName: String): Boolean;
    Procedure GetErrors(strFileName, strSource: String; Var iHints, iWarnings,
      iErrors, iConflicts: Integer; SourceType: TSourceType);
    Procedure TimerEvent(Sender: TObject);
    Procedure RefreshModuleExplorer;
    Procedure SaveResults;
    Procedure LoadResults;
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
  Private
    FIndex: Integer;
    Procedure ProcessFileFailure(Sender: TObject; FileName: String;
      Operation: TZFProcessOperation; NativeError: Integer; ErrorCode: Integer; 
      ErrorMessage: String; var Action: TZFAction);
    Procedure ExtractFile(Sender: TObject; var FileName: String; var FileAttr: LongWord;
      const Comment: AnsiString);
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
    Constructor Create(strFileName, strPathRoot: String; iErrors, iWarnings, iHints,
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
    Property Errors: Integer Read FErrors;
    (**
      This property returns the number of warnings in the file.
      @precon  None.
      @postcon Returns the number of warnings in the file.
      @return  an Integer
    **)
    Property Warnings: Integer Read FWarnings;
    (**
      This property returns the number of hints in the file.
      @precon  None.
      @postcon Returns the number of hints in the file.
      @return  an Integer
    **)
    Property Hints: Integer Read FHints;
    (**
      This property returns the number of conflicts in the file.
      @precon  None.
      @postcon Returns the number of conflicts in the file.
      @return  an Integer
    **)
    Property Conflicts: Integer Read FConflicts;
  End;

Var
  (** This is a global form variable so that the Delphi IDE can auto create
      the form on application startup. **)
  frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm;

Implementation

Uses
  CodeSiteLogging,
  TokenForm,
  IniFiles,
  DGHLibrary,
  OptionsForm,
  DocumentationDispatcher,
  BaseDocumentation,
  ShellAPI,
  Math,
  DocumentationOptionsForm,
  ExclusionsForm,
  FolderConfig,
  UsefulSynEditFunctions,
  EditorOptionsForm;

{$R *.dfm}


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
  M := ModuleDispatcher.Dispatcher(FSynEdit.Text, FileName, True,
    [moParse, moCheckForDocumentConflicts]);
  If M <> Nil Then
    Try
      TfrmTokenForm.Execute(M);
    Finally
      M.Free;
    End;
End;

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

Var
  strFileName: String;

Begin
  If FileName = '' Then
    Exit;
  strFileName := FileName;
  Delete(strFileName, 1, Length(PathRoot));
  If InputQuery('Exclude File from Result',
    'Are you sure you want to exclude the below file from the results.',
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
Procedure TfrmBrowseAndDocItTestForm.actFileRecurseFoldersExecute(
  Sender: TObject);
Begin
  If MessageDlg('Would you like to re-scan the code files?', mtConfirmation,
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

Var
  i        : Integer;
  R        : TScanResults;
  iPosition: Integer;

Begin
  Application.ProcessMessages;
  lvFileList.Items.Clear;
  FParseRecords.Clear;
  FProgressForm.Init(-1, 'Preparing to Scan', 'Please wait...');
  Try
    iPosition := 0;
    For i     := 0 To FFolders.Count - 1 Do
      If Integer(FFolders.Objects[i]) > 0 Then
        RecurseDirectories(FFolders.Names[i], FFolders.Names[i],
          iPosition, FFolders.ValueFromIndex[i]);
    FProgressForm.Init(iPosition, 'Scanning Directories', 'Please wait...');
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
  TfrmEditorOptions.Execute(FSynEdit, False);
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

  This method tests whether the given file should be included or excluded from
  the result set.

  @precon  None .
  @postcon Returns true if the file should be excluded from the results . 

  @note    The exclusion text is only tested for in the path / filename section 
           after the root path , i . e . the root path is ignored . 

  @param   strFileName as a String
  @return  a Boolean

**)
Function TfrmBrowseAndDocItTestForm.ExcludeFileFromResults(
  strFileName: String): Boolean;

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
  
begin
  CodeSite.Send('Extract File Failure', FileName);
end;

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


  This is an on click event handler for the Documentation Button.

  @precon  None.
  @postcon Invokes the documentation mechanism.


  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.DocumentationClick(Sender: TObject);

Var
  i       : Integer;
  ADocType: TDocType;

Begin
  If TfrmDocumentationOptions.Execute(ADocType) Then
    With DocumentDispatcher(ExtractFilePath(ParamStr(0)) + '\TEST Documentation\',
      'Test Documentation', ADocType) Do
      Try
        For i := 0 To lvFileList.Items.Count - 1 Do
          Add(lvFileList.Items[i].SubItems[4]);
        OutputDocumentation;
        ShellExecute(hInstance, 'OPEN', PChar(MainDocument), '',
          PChar(GetCurrentDir), SW_NORMAL);
      Finally
        Free;
      End;
End;

(**

  This method recurse the directories searching for files.

  @precon  None.
  @postcon Recurse the directories searching for files.

  @param   strRoot       as a String
  @param   strDirectory  as a String
  @param   iPosition     as an Integer as a reference
  @param   strExtensions as a String
  @param   boolScan      as a Boolean
  @return  a TScanResults

**)
Function TfrmBrowseAndDocItTestForm.RecurseDirectories(strRoot,
  strDirectory: String; Var iPosition: Integer; strExtensions: String;
  boolScan: Boolean = False): TScanResults;

Var
  recFile                   : TSearchRec;
  iResult                   : Integer;
  iHints, iWarnings, iErrors: Integer;
  iConflicts                : Integer;
  strFileName               : String;
  R                         : TScanResults;
  slExts                    : TStringList;
  i                         : Integer;
  Z                         : TZipForge;
  recZip                    : TZFArchiveItem;
  boolResult                : Boolean;
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
                    ModuleDispatcher.CanParseDocument(ExtractFileExt(recFile.Name)) Then
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
                      Format('Scanning %s %1.0n', [strDirectory, Int(iPosition)]));
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
        If Like('*.zip', recFile.Name) Then
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
                                If boolScan And ModuleDispatcher.CanParseDocument(ExtractFileExt(recZip.FileName)) Then
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
                                    FParseRecords.Add(TParseRecord.Create(strFileName,
                                      strRoot,
                                      iErrors, iWarnings, iHints, iConflicts));
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
          End;
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

**)
Procedure TfrmBrowseAndDocItTestForm.RefreshModuleExplorer;

Var
  M: TBaseLanguageModule;

Begin
  M := ModuleDispatcher.Dispatcher(FSynEdit.Text, FileName, FSynEdit.Modified, [moParse,
    moCheckForDocumentConflicts]);
  If M <> Nil Then
    Try
      FModuleExplorerFrame.RenderModule(M);
    Finally
      M.Free;
    End;
End;

(**

  This method gets the number of errors for the given files name.

  @precon  None.
  @postcon Gets the number of errors for the given files name.

  @param   strFileName as a String
  @param   strSource   as a String
  @param   iHints      as an Integer as a reference
  @param   iWarnings   as an Integer as a reference
  @param   iErrors     as an Integer as a reference
  @param   iConflicts  as an Integer as a reference
  @param   SourceType  as a TSourceType

**)
Procedure TfrmBrowseAndDocItTestForm.GetErrors(strFileName, strSource: String;
  Var iHints, iWarnings, iErrors, iConflicts: Integer;
  SourceType: TSourceType);

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
    M             := ModuleDispatcher.Dispatcher(Source.Text, strFileName, False,
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
  Finally
    Source.Free;
  End;
End;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Creates an instance of the Module Explorer Frame within the panel
           and initialises the SpecialTags global variable.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.FormCreate(Sender: TObject);

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
  BrowseAndDocItOptions.Defines.Add('WIN32');
  BrowseAndDocItOptions.Defines.Add('MSWINDOWS');
  {$ELSE}
  BrowseAndDocItOptions.Defines.Add('LINUX');
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

  This is an on custom draw item event handler for the list views.

  @precon  None.
  @postcon Draws the list view items with colours, path ellipses and alignments.

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

    @param   iIndex as an Integer
    @return  a TRect

  **)
  Function GetSubItemRect(iIndex: Integer): TRect;

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
  For i := 0 To Min(Item.SubItems.Count - 1, 3) Do
    Begin
      R                         := GetSubItemRect(i);
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color  := clWindowText;
      Val(Item.SubItems[i], iValue, iErrorCode);
      If iValue > 0 Then
        Begin
          Case i Of
            0:
              Sender.Canvas.Brush.Color := clSkyBlue;
            1:
              Sender.Canvas.Brush.Color := clGreen;
            2:
              Sender.Canvas.Brush.Color := clYellow;
            3:
              Sender.Canvas.Brush.Color := clRed;
          End;
          Case i Of
            0:
              Sender.Canvas.Font.Color := clBlack;
            1:
              Sender.Canvas.Font.Color := clWhite;
            2:
              Sender.Canvas.Font.Color := clRed;
            3:
              Sender.Canvas.Font.Color := clYellow;
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

  This is a Select item event handler for the list view.

  @precon  None.
  @postcon Saves any changes and options the new file selected.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TfrmBrowseAndDocItTestForm.lvFileListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

Begin
  If lvFileList.Selected <> Nil Then
    Begin
      FileName := lvFileList.Selected.SubItems[4];
      PathRoot := lvFileList.Selected.SubItems[5];
      RefreshModuleExplorer;
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
            liItem.Caption := ExtractFileName(rec.FileName);
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
End;

(**

  This is an on Process File Failure event handler for the ZipForge component.

  @precon  None.
  @postcon Outputs the filename to the codesite viewer.

  @param   Sender       as a TObject
  @param   FileName     as a String
  @param   Operation    as a TZFProcessOperation
  @param   NativeError  as an Integer
  @param   ErrorCode    as an Integer
  @param   ErrorMessage as a String
  @param   Action       as a TZFAction as a reference

**)
procedure TfrmBrowseAndDocItTestForm.ProcessFileFailure(Sender: TObject; FileName: String;
  Operation: TZFProcessOperation; NativeError, ErrorCode: Integer; ErrorMessage: String;
  var Action: TZFAction);
begin
  Action := fxaIgnore;
  CodeSite.Send('Process File Failure', FileName);
end;

(**

  This method saves the results to a text file.

  @precon  None.
  @postcon Saves the results to a text file.

**)
Procedure TfrmBrowseAndDocItTestForm.SaveResults;

Var
  sl     : TStringList;
  iRecord: Integer;

Begin
  sl := TStringList.Create;
  Try
    FProgressForm.Init(FParseRecords.Count, 'Shutting Down',
      'Saving Scan Results...');
    Try
      For iRecord := 0 To FParseRecords.Count - 1 Do
        With FParseRecords[iRecord] As TParseRecord Do
          Begin
            If iRecord Mod 10 = 0 Then
              FProgressForm.UpdateProgress(iRecord, FileName);
            sl.Add(Format('%s=%d,%d,%d,%d', [FileName, Errors, Warnings, Hints,
              Conflicts]));
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

**)
Procedure TfrmBrowseAndDocItTestForm.SaveSettings;

Var
  i         : Integer;
  recWndPlmt: TWindowPlacement;
  iniFile   : TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  With iniFile Do
    Try
      recWndPlmt.Length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Handle, @recWndPlmt);
      WriteInteger('Position', 'Top', recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Position', 'Left', recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Position', 'Height',
        recWndPlmt.rcNormalPosition.Bottom - recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Position', 'Width',
        recWndPlmt.rcNormalPosition.Right - recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Position', 'WindowState', Integer(WindowState));
      WriteInteger('Position', 'FileSplitter', lvFileList.Width);
      WriteInteger('Columns', '1', lvFileList.Columns[0].Width);
      WriteInteger('Columns', '2', lvFileList.Columns[1].Width);
      WriteInteger('Columns', '3', lvFileList.Columns[2].Width);
      WriteInteger('Columns', '4', lvFileList.Columns[3].Width);
      WriteInteger('Columns', '5', lvFileList.Columns[4].Width);
      WriteInteger('Position', 'Splitter', pnlModuleExplorer.Width);
      EraseSection('Folders');
      EraseSection('Folders.Enabled');
      For i := 0 To FFolders.Count - 1 Do
        Begin
          WriteString('Folders', FFolders.Names[i], FFolders.ValueFromIndex[i]);
          WriteBool('Folders.Enabled', FFolders.Names[i],
            Boolean(Integer(FFolders.Objects[i])));
        End;
      EraseSection('ExcludedFiles');
      For i := 0 To FFileExcludeList.Count - 1 Do
        WriteString('ExcludedFiles', Format('File%d', [i]), FFileExcludeList[i]);
      WriteInteger('Setup', 'Selection', lvFileList.ItemIndex);
      WriteBool('Setup', 'Recurse', actFileRecurseFolders.Checked);
      WriteBool('Setup', 'Errors', actViewErrors.Checked);
      WriteBool('Setup', 'Warnings', actViewWarnings.Checked);
      WriteBool('Setup', 'Hints', actViewHints.Checked);
      WriteBool('Setup', 'Conflicts', actViewDocConflicts.Checked);
      WriteInteger('Setup', 'SelectedItem', lvFileList.ItemIndex);
      SaveHighlighterToINIFile(iniFile, FSynPasSyn);
      SaveHighlighterToINIFile(iniFile, FSynVBSyn);
      SaveHighlighterToINIFile(iniFile, FSynCPPSyn);
      SaveHighlighterToINIFile(iniFile, FSynXMLSyn);
      SaveHighlighterToINIFile(iniFile, FSynDFMSyn);
      SaveHighlighterToINIFile(iniFile, FSynINISyn);
      UpdateFile;
    Finally
      Free;
    End;
End;

(**

  This method loads previous results from a text file.

  @precon  None.
  @postcon Loads previous results from a text file.

**)
Procedure TfrmBrowseAndDocItTestForm.LoadResults;

Var
  sl                                    : TStringList;
  iRecord                               : Integer;
  iErrors, iWarnings, iHints, iConflicts: Integer;
  strFileName                           : String;
  iErrorCode                            : Integer;

Begin
  sl := TStringList.Create;
  Try
    If FileExists(ChangeFileExt(FINIFileName, '.txt')) Then
      sl.LoadFromFile(ChangeFileExt(FINIFileName, '.txt'));
    FProgressForm.Init(sl.Count, 'Starting Up...', 'Loading Scan Results...');
    Try
      For iRecord := 0 To sl.Count - 1 Do
        Begin
          strFileName := sl.Names[iRecord];
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 1), iErrors, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 2), iWarnings, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 3), iHints, iErrorCode);
          Val(GetField(sl.ValueFromIndex[iRecord], ',', 4), iConflicts, iErrorCode);
          FParseRecords.Add(TParseRecord.Create(strFileName, '', iErrors,
            iWarnings, iHints, iConflicts));
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

**)
Procedure TfrmBrowseAndDocItTestForm.LoadSettings;

Var
  i      : Integer;
  sl     : TStringList;
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  With iniFile Do
    Try
      Top         := ReadInteger('Position', 'Top', Top);
      Left        := ReadInteger('Position', 'Left', Left);
      Height      := ReadInteger('Position', 'Height', Height);
      Width       := ReadInteger('Position', 'Width', Width);
      WindowState := TWindowState(ReadInteger('Position', 'WindowState',
        Integer(wsNormal)));
      lvFileList.Width := ReadInteger('Position', 'FileSplitter', lvFileList.Width);
      lvFileList.Columns[0].Width := ReadInteger('Columns', '1',
        lvFileList.Columns[0].Width);
      lvFileList.Columns[1].Width := ReadInteger('Columns', '2',
        lvFileList.Columns[1].Width);
      lvFileList.Columns[2].Width := ReadInteger('Columns', '3',
        lvFileList.Columns[2].Width);
      lvFileList.Columns[3].Width := ReadInteger('Columns', '4',
        lvFileList.Columns[3].Width);
      lvFileList.Columns[4].Width := ReadInteger('Columns', '5',
        lvFileList.Columns[4].Width);
      pnlModuleExplorer.Width := ReadInteger('Position', 'Splitter',
        pnlModuleExplorer.Width);
      sl := TStringList.Create;
      Try
        ReadSection('Folders', sl);
        For i := 0 To sl.Count - 1 Do
          FFolders.AddObject(Format('%s=%s', [sl[i],
            ReadString('Folders', sl[i], '')]),
            TObject(ReadBool('Folders.Enabled', sl[i], False)));
        ReadSection('ExcludedFiles', sl);
        For i := 0 To sl.Count - 1 Do
          FFileExcludeList.Add(ReadString('ExcludedFiles', sl[i], ''));
      Finally
        sl.Free;
      End;
      i                             := ReadInteger('Setup', 'Selection', 0);
      actFileRecurseFolders.Checked := ReadBool('Setup', 'Recurse', False);
      actViewErrors.Checked         := ReadBool('Setup', 'Errors', False);
      actViewWarnings.Checked       := ReadBool('Setup', 'Warnings', False);
      actViewHints.Checked          := ReadBool('Setup', 'Hints', False);
      actViewDocConflicts.Checked   := ReadBool('Setup', 'Conflicts', False);
      If lvFileList.Items.Count > i Then
        lvFileList.ItemIndex := i;
      FIndex                 := ReadInteger('Setup', 'SelectedItem', -1);
      LoadHighlighterFromINIFile(iniFile, FSynPasSyn);
      LoadHighlighterFromINIFile(iniFile, FSynVBSyn);
      LoadHighlighterFromINIFile(iniFile, FSynCPPSyn);
      LoadHighlighterFromINIFile(iniFile, FSynXMLSyn);
      LoadHighlighterFromINIFile(iniFile, FSynDFMSyn);
      LoadHighlighterFromINIFile(iniFile, FSynINISyn);
    Finally
      Free;
    End;
End;

(**

  This is an OnSelctionChange event handler for the module explorer.

  @precon  None.
  @postcon Moves the cursor to select item in the code.

  @param   iIdentLine   as an Integer
  @param   iIdentCol    as an Integer
  @param   iCommentLine as an Integer
  @param   iCommentCol  as an Integer

**)
Procedure TfrmBrowseAndDocItTestForm.SelectionChange(iIdentLine, iIdentCol, iCommentLine,
  iCommentCol: Integer);

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

Var
  strExt     : String;
  Z          : TZipForge;
  strSource  : String;
  strFileName: String;
  iPos       : Integer;

Begin
  FFileName := Value;
  Caption   := FFileName;
  If Not Like('*.zip\*', FFileName) Then
    FSynEdit.Lines.LoadFromFile(FFileName)
  Else
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
        Finally
          Z.CloseArchive;
        End;
      Finally
        Z.Free;
      End;
    End;
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

  @param   Sender  as a TObject
  @param   Changes as a TSynStatusChanges

**)
Procedure TfrmBrowseAndDocItTestForm.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
Begin
  FLastEdit               := GetTickCount;
  sbrStatusBar.SimpleText := Format('Line %d, Column %d',
    [FSynEdit.CaretY, FSynEdit.CaretX]);
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
        FLastEdit      := 0;
      End;
    End;
End;

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

{ TParseRecord }

(**

  This is a constructor for the TParseRecord class.

  @precon  None . 
  @postcon Initialises the record class with information . 

  @param   strFileName as a String
  @param   strPathRoot as a String
  @param   iErrors     as an Integer
  @param   iWarnings   as an Integer
  @param   iHints      as an Integer
  @param   iConflicts  as an Integer

**)
Constructor TParseRecord.Create(strFileName, strPathRoot: String; iErrors,
  iWarnings, iHints, iConflicts: Integer);

Begin
  FFileName  := strFileName;
  FPathRoot  := strPathRoot;
  FErrors    := iErrors;
  FWarnings  := iWarnings;
  FHints     := iHints;
  FConflicts := iConflicts;
End;

End.
