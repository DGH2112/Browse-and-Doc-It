(**

  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Version 1.0
  @Date    18 Jul 2009
  @Author  David Hoyle

**)
unit BrowseAndDocItTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, SynEditHighlighter, SynHighlighterPas,
  SynEdit, ExtCtrls, ModuleExplorerFrame, BaseLanguageModule, StdCtrls,
  FileCtrl, ComCtrls, Contnrs, SynHighlighterVB, SynHighlighterCpp,
  Menus, StdActns, ActnList, ProgressForm, Buttons, ImgList, ToolWin, XPMan;

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

type
  (** A record to define the doc conflicts, hints and errors for files and
      directories. **)
  TScanResults = Record
    iDocConflicts, iHints, iWarnings, iErrors : Integer;
  End;

  (** This is thre class that defined the main interface form. **)
  TfrmBrowseAndDocItTestForm = class(TForm)
    pnlModuleExplorer: TPanel;
    pnlFileList: TPanel;
    Splitter1: TSplitter;
    lvFileList: TListView;
    ilImages: TImageList;
    sbrStatusBar: TStatusBar;
    sptFiles: TSplitter;
    sptDirs: TSplitter;
    lvDirectories: TListView;
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
    actFileAddFolder: TAction;
    actFileEditFolder: TAction;
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
    tbtnFileAddFolder: TToolButton;
    tbtnFileExit: TToolButton;
    tbtnFileRecurseFolders: TToolButton;
    tbtnFileEditFolder: TToolButton;
    tbtnFileDeleteFolder: TToolButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer);
    Procedure Focus(Sender : TObject);
    procedure ShowTokensClick(Sender: TObject);
    procedure SpecialCharactersClick(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnOptionsClick(Sender: TObject);
    procedure DocumentationClick(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure actFileAddFolderExecute(Sender: TObject);
    procedure actFileEditFolderExecute(Sender: TObject);
    procedure actFileScanExecute(Sender: TObject);
    procedure ChangeVisibleItems(Sender: TObject);
    procedure actFileRecurseFoldersExecute(Sender: TObject);
    procedure actFileExcludeFileExecute(Sender: TObject);
    procedure lvDirectoriesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvDirectoriesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actFileDeleteFolderExecute(Sender: TObject);
    procedure lvDirectoriesCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure actToolsExclusionsExecute(Sender: TObject);
    procedure lvDirectoriesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  {$IFDEF D2005} Strict {$ENDIF} Private
    { Private declarations }
    FFileName: String;
    FPathRoot: String;
    FModuleExplorerFrame : TframeModuleExplorer;
    FProgressForm : TfrmProgress;
    FINIFileName : String;
    FSynEdit: TSynEdit;
    FSynPasSyn: TSynPasSyn;
    FSynVBSyn: TSynVBSyn;
    FSynCPPSyn : TSynCPPSyn;
    FParseRecords : TObjectList;
    FFileExcludeList : TStringList;
    FTimer : TTimer;
    FLastEdit: Int64;
    function GetFileName: String;
    procedure SetFileName(const Value: String);
    procedure LoadSettings;
    procedure SaveSettings;
    function GetPathRoot: String;
    procedure SetPathRoot(const Value: String);
    Function RecurseDirectories(strRoot, strDirectory : String;
      iPosition : Integer) : TScanResults;
    Procedure RefreshExplorer(Sender : TObject);
    Procedure PopulateListView;
    Function ExcludeFileFromResults(strFileName : String) : Boolean;
    Procedure GetErrors(strFileName : String; var iHints, iWarnings, iErrors, iConflicts : Integer);
    Procedure TimerEvent(Sender : TObject);
    Procedure RefreshModuleExplorer;
    (**
      A property to define the currently selected file.
      @precon  None.
      @postcon Sets the current file and initiates the file being displayed in
               the editor or returns the current file name.
      @return  a String
    **)
    Property FileName : String Read GetFileName Write SetFileName;
    (**
      This property get or sets the path root of the selected file.
      @precon  None.
      @postcon Get or sets the path root of the selected file.
      @return  a String
    **)
    Property PathRoot : String Read GetPathRoot Write SetPathRoot;
  public
    { Public declarations }
  end;

  (** This is a class which represents a parser record of information. **)
  TParseRecord = Class
  {$IFDEF D2006} Strict {$ENDIF} Private
    FFileName  : String;
    FPathRoot  : String;
    FErrors    : Integer;
    FWarnings  : Integer;
    FHints     : Integer;
    FConflicts : Integer;
  Public
    Constructor Create(strFileName, strPathRoot : String; iErrors, iWarnings, iHints,
      iConflicts : Integer);
    (**
      This property returns the file name of the parser record.
      @precon  None.
      @postcon Returns the file name of the parser record.
      @return  a String
    **)
    Property FileName  : String Read FFileName;
    (**
      This property get or sets the path root of the selected file.
      @precon  None.
      @postcon Get or sets the path root of the selected file.
      @return  a String
    **)
    Property PathRoot  : String Read FPathRoot;
    (**
      This property returns the number of errors in the file.
      @precon  None.
      @postcon Returns the number of errors in the file.
      @return  an Integer
    **)
    Property Errors    : Integer Read FErrors;
    (**
      This property returns the number of warnings in the file.
      @precon  None.
      @postcon Returns the number of warnings in the file.
      @return  an Integer
    **)
    Property Warnings  : Integer Read FWarnings;
    (**
      This property returns the number of hints in the file.
      @precon  None.
      @postcon Returns the number of hints in the file.
      @return  an Integer
    **)
    Property Hints     : Integer Read FHints;
    (**
      This property returns the number of conflicts in the file.
      @precon  None.
      @postcon Returns the number of conflicts in the file.
      @return  an Integer
    **)
    Property Conflicts : Integer Read FConflicts;
  End;

var
  (** This is a global form variable so that the Delphi IDE can auto create
      the form on application startup. **)
  frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm;

implementation

Uses
  TokenForm, IniFiles, DGHLibrary, OptionsForm, ModuleDispatcher,
  DocumentationDispatcher, BaseDocumentation, ShellAPI, Math,
  DocumentationOptionsForm, ExclusionsForm;

{$R *.dfm}

(**

  This is an on click event handler for the Tokens button.

  @precon  None.
  @postcon Displays a form showing the tokens from the current file.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.ShowTokensClick(Sender: TObject);

Var
  M : TBaseLanguageModule;

begin
  M := Dispatcher(FSynEdit.Text, FileName, True, [moParse, moCheckForDocumentConflicts]);
  If M <> Nil Then
    Try
      TfrmTokenForm.Execute(M);
    Finally
      M.Free;
    End;
end;

(**

  This is an on execute event handler for the Add Folder action.

  @precon  None.
  @postcon Allows the user to select a directory to add to the list. 

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actFileAddFolderExecute(Sender: TObject);
var
  Item: TListItem;
  strDir: String;
begin
  If SelectDirectory(strDir, [], 0) Then
    Begin
      Item := lvDirectories.Items.Add;
      Item.Caption := strDir;
    End;
end;

(**

  This is an on execute event handler for the File Delete Folder action.

  @precon  None.
  @postcon Deletes the selected folder.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actFileDeleteFolderExecute(
  Sender: TObject);

begin
  If lvDirectories.Selected = Nil Then
    Exit;
  If MessageDlg(Format('Are you sure your wish to delete ''%s''?', [
    lvDirectories.Selected.Caption]),  mtConfirmation, [mbOK, mbYes,
    mbCancel], 0) = mrYes Then
    lvDirectories.DeleteSelected;
end;

(**

  This is an on execute event handler for the Edit Folder action.

  @precon  None.
  @postcon Allows the user to edit a selected directory from the list.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actFileEditFolderExecute(Sender: TObject);

Var
  Item : TListItem;
  strDir: String;

begin
  If lvDirectories.Selected = Nil Then
    Exit;
  Item := lvDirectories.Selected;
  strDir := Item.Caption;
  If SelectDirectory(strDir, [], 0) Then
    Item.Caption := strDir;
end;

(**

  This is an on execute event handler for the Exclude File action.

  @precon  None.
  @postcon Allows the user to exclude a file based on the whole or part of its
           path.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actFileExcludeFileExecute(Sender: TObject);

Var
  strFileName : String;

begin
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
end;

(**

  This is an on execute event handler for the Recurse Folders action.

  @precon  None.
  @postcon Chnages the scanning to use sub directories or Not.
  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actFileRecurseFoldersExecute(
  Sender: TObject);
begin
  If MessageDlg('Would you like to re-scan the code files?', mtConfirmation,
    [mbYes, mbNo, mbCancel], 0) = mrYes Then
    actFileScanExecute(Sender);
end;

(**

  This is an on execute event handler for the Scan action.

  @precon  None.
  @postcon Starts the process of scanning the files.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actFileScanExecute(Sender: TObject);

var
  i: Integer;
  R : TScanResults;

begin
  For i := 0 To lvDirectories.Items.Count - 1 Do
    lvDirectories.Items[i].SubItems.Clear;
  Application.ProcessMessages;
  lvFileList.Items.Clear;
  FParseRecords.Clear;
  FProgressForm.Init(lvDirectories.Items.Count, 'Scanning Directories', 'Please wait...');
  Try
    For i := 0 To lvDirectories.Items.Count - 1 Do
      If lvDirectories.Items[i].Checked Then
        Begin
          R := RecurseDirectories(lvDirectories.Items[i].Caption,
            lvDirectories.Items[i].Caption, i);
          lvDirectories.Items[i].SubItems.Add(Format('%d', [R.iDocConflicts]));
          lvDirectories.Items[i].SubItems.Add(Format('%d', [R.iHints]));
          lvDirectories.Items[i].SubItems.Add(Format('%d', [R.iWarnings]));
          lvDirectories.Items[i].SubItems.Add(Format('%d', [R.iErrors]));
        End;
  Finally
    Try
      PopulateListView;
    Finally
      FProgressForm.Hide;
    End;
  End;
end;

(**

  This is an on execute event handler for the Tools Exclusions action.

  @precon  None.
  @postcon Invokes a dialogue that allows the editing of the list of exclusions.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.actToolsExclusionsExecute(Sender: TObject);
begin
  TfrmExclusions.Execute(FFileExcludeList);
end;

(**

  This is a on execute event handler for the Doc Conflict, Hint, Warning and
  Error buttons.

  @precon  None.
  @postcon Repopulates the list based on what items are to be seen.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.ChangeVisibleItems(Sender: TObject);
begin
  PopulateListView;
end;

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
function TfrmBrowseAndDocItTestForm.ExcludeFileFromResults(
  strFileName : String): Boolean;

Var
  i: Integer;

begin
  Result := False;
  For i := 0 To FFileExcludeList.Count - 1 Do
    If Pos(FFileExcludeList[i], strFileName) > 0 Then
      Begin
        Result := True;
        Break;
      End;
End;

(**

  This is an on click event handler for the options button.

  @precon  None.
  @postcon Displays the Options dialogue.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.btnOptionsClick(Sender: TObject);

begin
  If TfrmOptions.Execute Then
    SynEdit1Change(Sender);
end;

(**

  This is an on click event handler for the Checkbox1 control.

  @precon  None
  @postcon Displays or hides the special character markers in the SynEdit1
           control.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.SpecialCharactersClick(Sender: TObject);
begin
  If actViewSpecialCharacters.Checked Then
    FSynEdit.Options := FSynEdit.Options + [eoShowSpecialChars]
  Else
    FSynEdit.Options := FSynEdit.Options - [eoShowSpecialChars];
end;

(**


  This is an on click event handler for the Documentation Button.

  @precon  None.
  @postcon Invokes the documentation mechanism.


  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.DocumentationClick(Sender: TObject);

Var
  i : Integer;
  ADocType: TDocType;

begin
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
end;

(**

  This method recurse the directories searching for files.

  @precon  None . 
  @postcon Recurse the directories searching for files . 

  @param   strRoot      as a String
  @param   strDirectory as a String
  @param   iPosition    as an Integer
  @return  a TScanResults

**)
Function TfrmBrowseAndDocItTestForm.RecurseDirectories(strRoot,
  strDirectory : String; iPosition : Integer) : TScanResults;

Var
  recFile : TSearchRec;
  iResult : Integer;
  iHints, iWarnings, iErrors: Integer;
  iConflicts: Integer;
  strFileName: String;
  R: TScanResults;

Begin
  Result.iDocConflicts := 0;
  Result.iHints := 0;
  Result.iWarnings := 0;
  Result.iErrors := 0;
  iResult := FindFirst(strDirectory + '\*.*', faAnyFile, recFile);
  Try
    While iResult = 0 Do
      Begin
        If CanParseDocument(ExtractFileExt(recFile.Name)) Then
          Begin
            strFileName := strDirectory + '\' + recFile.Name;
            If Not ExcludeFileFromResults(strFileName) Then
              Begin
                FProgressForm.UpdateProgress(iPosition, strFileName);
                GetErrors(strFileName, iHints, iWarnings, iErrors, iConflicts);
                Inc(Result.iDocConflicts, iConflicts);
                Inc(Result.iHints, iHints);
                Inc(Result.iWarnings, iWarnings);
                Inc(Result.iErrors, iErrors);
                FParseRecords.Add(TParseRecord.Create(strFileName, strRoot,
                  iErrors, iWarnings, iHints, iConflicts));
                Application.ProcessMessages;
              End;
          End;
        If actFileRecurseFolders.Checked Then
          If (recFile.Attr And faDirectory <> 0) And (recFile.Name[1] <> '.') Then
            Begin
              R := RecurseDirectories(strRoot, strDirectory + '\' + recFile.Name,
                iPosition);
              Inc(Result.iDocConflicts, R.iDocConflicts);
              Inc(Result.iHints, R.iHints);
              Inc(Result.iWarnings, R.iWarnings);
              Inc(Result.iErrors, R.iErrors);
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
procedure TfrmBrowseAndDocItTestForm.RefreshExplorer(Sender: TObject);
begin
  SynEdit1Change(Sender);
end;

procedure TfrmBrowseAndDocItTestForm.RefreshModuleExplorer;

Var
  M : TbaseLanguageModule;

begin
  M := Dispatcher(FSynEdit.Text, FileName, FSynEdit.Modified, [moParse,
    moCheckForDocumentConflicts]);
  If M <> Nil Then
    Try
      FModuleExplorerFrame.RenderModule(M);
    Finally
      M.Free;
    End;
end;

(**


  This method gets the number of errors for the given files name.


  @precon  None.

  @postcon Gets the number of errors for the given files name.


  @param   strFileName as a String
  @param   iHints      as an Integer as a reference
  @param   iWarnings   as an Integer as a reference
  @param   iErrors     as an Integer as a reference
  @param   iConflicts  as an Integer as a reference

**)
Procedure TfrmBrowseAndDocItTestForm.GetErrors(strFileName : String;
  var iHints, iWarnings, iErrors, iConflicts : Integer);

Var
  Source : TStringList;
  M : TBaseLanguageModule;
  i : Integer;
  C: TElementContainer;

Begin
  iHints := 0;
  iWarnings := 0;
  iErrors := 0;
  iConflicts := 0;
  Source := TStringList.Create;;
  Try
    Source.LoadFromFile(strFileName);
    M := Dispatcher(Source.Text, strFileName, False, [moParse, moCheckForDocumentConflicts]);
    If M <> Nil Then
      Try
        If M.FindElement(strHints) <> Nil Then
          iHints := M.FindElement(strHints).ElementCount;
        If M.FindElement(strWarnings) <> Nil Then
          iWarnings := M.FindElement(strWarnings).ElementCount;
        If M.FindElement(strErrors) <> Nil Then
          iErrors := M.FindElement(strErrors).ElementCount;
        C := M.FindElement(strDocumentationConflicts);
        If C <> Nil Then
          Begin
            For i := 1 To C.ElementCount Do
              Inc(iConflicts,C.Elements[i].ElementCount);
          End;
      Finally
        M.Free;
      End;
  Finally
    Source.Free;
  End;
End;

(**

  This is an on close event handler for the main form.

  @precon  None.
  @postcon Prompts to save the file if the file has been modified.

  @param   Sender   as a TObject
  @param   CanClose as a Boolean as a reference

**)
procedure TfrmBrowseAndDocItTestForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

begin
  If FSynEdit.Modified Then
    Begin
      FSynEdit.Lines.SaveToFile(FFileName);
      FSynEdit.Modified := False;
    End;
end;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Creates an instance of the Module Explorer Frame within the panel
           and initialises the SpecialTags global variable.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FormCreate(Sender: TObject);

begin
  FParseRecords := TObjectList.Create(True);
  FSynEdit := TSynEdit.Create(Nil);
  With FSynEdit Do
    Begin
      Parent := Self;
      Highlighter := FSynPasSyn;
      Align := alClient;
      ActiveLineColor := clSkyBlue;
      Gutter.ShowLineNumbers := True;
      PopupMenu := pmEdit;
      OnChange := SynEdit1Change;
      OnStatusChange:= SynEdit1StatusChange;
    End;
  FSynPasSyn := TSynPasSyn.Create(Nil);
  With FSynPasSyn Do
    Begin
      AsmAttri.Foreground := clMaroon;
      CommentAttri.Foreground := clPurple;
      DirectiveAttri.Foreground := clGreen;
      DirectiveAttri.Style := [fsBold];
      IdentifierAttri.Foreground := clNavy;
      NumberAttri.Foreground := clGreen;
      FloatAttri.Foreground := clGreen;
      HexAttri.Foreground := clGreen;
      StringAttri.Foreground := clTeal;
      CharAttri.Foreground := clTeal;
      SymbolAttri.Foreground := clGreen;
    End;
  FSynVBSyn := TSynVBSyn.Create(Nil);
  With FSynVBSyn Do
    Begin
      CommentAttri.Foreground := clPurple;
      IdentifierAttri.Foreground := clNavy;
      NumberAttri.Foreground := clGreen;
      StringAttri.Foreground := clTeal;
      SymbolAttri.Foreground := clGreen;
    End;
  FSynCPPSyn := TSynCPPSyn.Create(Nil);
  With FSynCPPSyn Do
    Begin
      AsmAttri.Foreground := clMaroon;
      CommentAttri.Foreground := clPurple;
      IdentifierAttri.Foreground := clNavy;
      NumberAttri.Foreground := clGreen;
      StringAttri.Foreground := clTeal;
      SymbolAttri.Foreground := clGreen;
      FloatAttri.Foreground := clGreen;
      HexAttri.Foreground := clGreen;
      CharAttri.Foreground := clOlive;
    End;
  FINIFileName := BuildRootKey(Nil, Nil);
  {$IFDEF WIN32}
  BrowseAndDocItOptions.Defines.Add('WIN32');
  BrowseAndDocItOptions.Defines.Add('MSWINDOWS');
  {$ELSE}
  BrowseAndDocItOptions.Defines.Add('LINUX');
  {$ENDIF}
  FProgressForm := TfrmProgress.Create(Nil);
  FModuleExplorerFrame := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent := pnlModuleExplorer;
  FModuleExplorerFrame.Align := alClient;
  FModuleExplorerFrame.OnSelectionChange := SelectionChange;
  FModuleExplorerFrame.OnFocus := Focus;
  FModuleExplorerFrame.OnRefresh := RefreshExplorer;
  FFileExcludeList := TStringList.Create;
  LoadSettings;
  FLastEdit := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 100;
  FTimer.OnTimer := TimerEvent;
end;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Cleans up the created instances from OnCreate.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FormDestroy(Sender: TObject);

begin
  FTimer.Free;
  FSynEdit.Highlighter := Nil;
  FSynEdit.Free;
  FSynVBSyn.Free;
  FSynPasSyn.Free;
  FSynCPPSyn.Free;
  SaveSettings;
  FFileExcludeList.Free;
  FModuleExplorerFrame.Free;
  FProgressForm.Free;
  lvDirectories.OnChange := Nil;
  FParseRecords.Free;
end;

(**

  This is a getter method for the FileName property.

  @precon  None.
  @postcon Sets the returns the file name property based on the file list box
           selection.

  @return  a String

**)
function TfrmBrowseAndDocItTestForm.GetFileName: String;
begin
  Result := FFileName;
end;

(**

  This is a getter method for the Path Root property.

  @precon  None.
  @postcon Returns the root path of the selected file.

  @return  a String

**)
function TfrmBrowseAndDocItTestForm.GetPathRoot: String;
begin
  Result := FPathRoot;
end;

(**

  This is an onchange event handler for the directories list view.

  @precon  None.
  @postcon Forces the list view of the files to be re-drawn based on selected
           directories.

  @param   Sender as a TObject
  @param   Item   as a TListItem
  @param   Change as a TItemChange

**)
procedure TfrmBrowseAndDocItTestForm.lvDirectoriesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  PopulateListView;
end;

(**

  This is an on custom draw item event handler for the list views.

  @precon  None.
  @postcon Draws the list view items with colours, path ellipses and alignments.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmBrowseAndDocItTestForm.lvDirectoriesCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);

Var
  iLength : Integer;
  R: TRect;
  i: Integer;
  iValue: Integer;
  iErrorCode: Integer;

  (**

    This function returns the rectangle for the indexed sub item of the listview.

    @precon  iIndex must be a valid index into the column collection.
    @postcon Returns the rectangle for the indexed sub item of the listview.

    @param   iIndex as an Integer
    @return  a TRect

  **)
  Function GetSubItemRect(iIndex : Integer) : TRect;

  Var
    j : Integer;

  Begin
    Result := Item.DisplayRect(drBounds);
    For j := 0 To iIndex Do
      Begin
        Inc(Result.Left, Sender.Column[j].Width);
        Result.Right := Result.Left + Sender.Column[j + 1].Width;
      End;
  End;

begin
  DefaultDraw := False;
  R := Item.DisplayRect(drSelectBounds);
  Sender.Canvas.Brush.Color := clWindow;
  If Item.Selected Then
    Begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color := clHighlightText;
    End;
  Sender.Canvas.FillRect(R);
  If Sender = lvDirectories Then
    Begin
      R := Item.DisplayRect(drBounds);
      Sender.Canvas.FillRect(R);
      ilDirStatus.Draw(Sender.Canvas, R.Left, R.Top, Integer(Item.Checked), True);
    End;
  R := Item.DisplayRect(drLabel);
  Dec(R.Right);
  iLength := Length(Item.Caption);
  DrawText(Sender.Canvas.Handle, PChar(Item.Caption), iLength, R,
    DT_LEFT Or DT_PATH_ELLIPSIS);
  For i := 0 To Min(Item.SubItems.Count - 1, 3) Do
    Begin
      R := GetSubItemRect(i);
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := clWindowText;
      Val(Item.SubItems[i], iValue, iErrorCode);
      If iValue > 0 Then
        Begin
          Case i Of
            0: Sender.Canvas.Brush.Color := clSkyBlue;
            1: Sender.Canvas.Brush.Color := clGreen;
            2: Sender.Canvas.Brush.Color := clYellow;
            3: Sender.Canvas.Brush.Color := clRed;
          End;
          Case i Of
            0: Sender.Canvas.Font.Color := clBlack;
            1: Sender.Canvas.Font.Color := clWhite;
            2: Sender.Canvas.Font.Color := clRed;
            3: Sender.Canvas.Font.Color := clYellow;
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

  This is an on drag drop event handler for the directory list view.

  @precon  None.
  @postcon Swaps the selected item for the item the selected item is drop on.

  @param   Sender as a TObject
  @param   Source as a TObject
  @param   X      as an Integer
  @param   Y      as an Integer

**)
procedure TfrmBrowseAndDocItTestForm.lvDirectoriesDragDrop(Sender,
  Source: TObject; X, Y: Integer);

var
  Item: TListItem;
  strText : String;
  boolChecked : Boolean;
  i: Integer;

begin
  Item := (Source As TListView).GetItemAt(X, Y);
  With (Source As TListView) Do
    Begin
      strText := Item.Caption;
      boolChecked := Item.Checked;
      Item.Caption := Selected.Caption;
      Item.Checked := Selected.Checked;
      Selected.Caption := strText;
      Selected.Checked := boolChecked;
      For i := 0 to Item.SubItems.Count -1 Do
        Begin
          strText := Item.SubItems[i];
          Item.SubItems[i] := Selected.SubItems[i];
          Selected.SubItems[i] := strText;
        End;
      Selected := Nil;
    End;
end;

(**

  This is an on drag over event handler for the directory list view.

  @precon  None.
  @postcon Enabled or disbled the drop based on where the drop is.

  @param   Sender as a TObject
  @param   Source as a TObject
  @param   X      as an Integer
  @param   Y      as an Integer
  @param   State  as a TDragState
  @param   Accept as a Boolean as a reference

**)
procedure TfrmBrowseAndDocItTestForm.lvDirectoriesDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

Var
  OverItem : TListItem;

begin
  OverItem := (Source As TListView).GetItemAt(X, Y);
  Accept := (OverItem <> (Source As TListView).Selected) And (OverItem <> Nil);
end;

(**

  This is a Select item event handler for the list view.

  @precon  None.
  @postcon Saves any changes and options the new file selected.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
procedure TfrmBrowseAndDocItTestForm.lvFileListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

Var
  boolCanClose :Boolean;

begin
  FormCloseQuery(Self, boolCanClose);
  If lvFileList.Selected <> Nil Then
    If FileExists(lvFileList.Selected.SubItems[4]) Then
      Begin
        FileName := lvFileList.Selected.SubItems[4];
        PathRoot := lvFileList.Selected.SubItems[5];
        RefreshModuleExplorer;
      End Else
        MessageDlg(Format('The file "%s" was not found.',
          [lvFileList.Selected.SubItems[4]]), mtError, [mbOK], 0);
end;

(**

  This method popluates the list view based on the filter checkboxes.

  @precon  None.
  @postcon Popluates the list view based on the filter checkboxes.

**)
procedure TfrmBrowseAndDocItTestForm.PopulateListView;

Var
  i : Integer;
  rec : TParseRecord;
  liItem: TListItem;
  boolInclude : Boolean;

  (**

    This function determines of the current record should be shown based on
    whether the root directory is selected or no selection is present.

    @precon  None.
    @postcon Returns true if the record should be shown in the results set.

    @return  a Boolean

  **)
  Function IsFolderSelected : Boolean;

  Var
    k : Integer;
    S: TListItem;

  Begin
    k := 0;
    S := lvDirectories.Selected;
    While S <> Nil Do
      Begin
        Inc(k);
        If S.Caption = rec.PathRoot Then
          Begin
            Result := True;
            Exit;
          End;
        S := lvDirectories.GetNextItem(S, sdBelow, [isSelected]);
      End;
    Result := (k = 0);
  End;

begin
  Try
    lvFileList.Clear;
    lvFileList.Items.BeginUpdate;
    For i := 0 To FParseRecords.Count - 1 Do
      Begin
        rec := FParseRecords[i] As TParseRecord;
        boolInclude :=
          Not actViewErrors.Checked And
          Not actViewWarnings.Checked And
          Not actViewHints.Checked And
          Not actViewDocConflicts.Checked;
        boolInclude := boolInclude Or (actViewErrors.Checked And (rec.Errors > 0));
        boolInclude := boolInclude Or (actViewWarnings.Checked And (rec.Warnings > 0));
        boolInclude := boolInclude Or (actViewHints.Checked And (rec.Hints > 0));
        boolInclude := boolInclude Or (actViewDocConflicts.Checked And (rec.Conflicts > 0));
        boolInclude := boolInclude And IsFolderSelected;
        If boolInclude Then
          Begin
            liItem := lvFileList.Items.Add;
            liItem.Caption := ExtractFileName(rec.FileName);
            liItem.SubItems.Add(Format('%d', [rec.Conflicts]));
            liItem.SubItems.Add(Format('%d', [rec.Hints]));
            liItem.SubItems.Add(Format('%d', [rec.Warnings]));
            liItem.SubItems.Add(Format('%d', [rec.Errors]));
            liItem.SubItems.Add(rec.FileName);
            liItem.SubItems.Add(rec.PathRoot);
          End;
      End;
  Finally
    lvFileList.Items.EndUpdate;
  End;
end;

(**

  This method saves the applications settings to an INI file.

  @precon  None.
  @postcon Saves the applications settings to an INI file.

**)
procedure TfrmBrowseAndDocItTestForm.SaveSettings;

var
  i: Integer;
  recWndPlmt : TWindowPlacement;

begin
  with TIniFile.Create(FINIFileName) do
    try
      recWndPlmt.Length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Handle, @recWndPlmt);
      WriteInteger('Position', 'Top', recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Position', 'Left', recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Position', 'Height',
        recWndPlmt.rcNormalPosition.Bottom - recWndPlmt.rcNormalPosition.Top);
      WriteInteger('Position', 'Width',
        recWndPlmt.rcNormalPosition.Right - recWndPlmt.rcNormalPosition.Left);
      WriteInteger('Position', 'WindowState', Integer(WindowState));
      WriteInteger('Position', 'FileSplitter', pnlFileList.Width);
      WriteInteger('Columns', '1', lvFileList.Columns[0].Width);
      WriteInteger('Columns', '2', lvFileList.Columns[1].Width);
      WriteInteger('Columns', '3', lvFileList.Columns[2].Width);
      WriteInteger('Columns', '4', lvFileList.Columns[3].Width);
      WriteInteger('Columns', '5', lvFileList.Columns[4].Width);
      WriteInteger('Columns', 'A', lvDirectories.Columns[0].Width);
      WriteInteger('Columns', 'B', lvDirectories.Columns[1].Width);
      WriteInteger('Columns', 'B', lvDirectories.Columns[2].Width);
      WriteInteger('Columns', 'D', lvDirectories.Columns[3].Width);
      WriteInteger('Columns', 'E', lvDirectories.Columns[4].Width);
      WriteInteger('Position', 'Splitter', pnlModuleExplorer.Width);
      WriteInteger('Position', 'DirHeight', lvDirectories.Height);
      For i := 0 To lvDirectories.Items.Count - 1 Do
        Begin
          WriteString('Folders', Format('Folder%d', [i]), lvDirectories.Items[i].Caption);
          WriteBool('FolderChecks', Format('Folder%d', [i]), lvDirectories.Items[i].Checked);
        End;
      For i := 0 To FFileExcludeList.Count - 1 Do
        WriteString('ExcludedFiles', Format('File%d', [i]), FFileExcludeList[i]);
      WriteInteger('Setup', 'Selection', lvFileList.ItemIndex);
      WriteBool('Setup', 'Recurse', actFileRecurseFolders.Checked);
      WriteBool('Setup', 'Errors', actViewErrors.Checked);
      WriteBool('Setup', 'Warnings', actViewWarnings.Checked);
      WriteBool('Setup', 'Hints', actViewHints.Checked);
      WriteBool('Setup', 'Conflicts', actViewDocConflicts.Checked);
    finally
      Free;
    end;
end;

(**

  This method loads the applications settings from an INI file.

  @precon  None.
  @postcon Loads the applications settings from an INI file.

**)
procedure TfrmBrowseAndDocItTestForm.LoadSettings;

var
  j: Integer;
  sl : TStringList;
  Item : TListItem;

begin
  with TIniFile.Create(FINIFileName) do
    try
      Top := ReadInteger('Position', 'Top', Top);
      Left := ReadInteger('Position', 'Left', Left);
      Height := ReadInteger('Position', 'Height', Height);
      Width := ReadInteger('Position', 'Width', Width);
      WindowState := TWindowState(ReadInteger('Position', 'WindowState',
        Integer(wsNormal)));
      pnlFileList.Width := ReadInteger('Position', 'FileSplitter', pnlFileList.Width);
      lvFileList.Columns[0].Width := ReadInteger('Columns', '1', lvFileList.Columns[0].Width);
      lvFileList.Columns[1].Width := ReadInteger('Columns', '2', lvFileList.Columns[1].Width);
      lvFileList.Columns[2].Width := ReadInteger('Columns', '3', lvFileList.Columns[2].Width);
      lvFileList.Columns[3].Width := ReadInteger('Columns', '4', lvFileList.Columns[3].Width);
      lvFileList.Columns[4].Width := ReadInteger('Columns', '5', lvFileList.Columns[4].Width);
      lvDirectories.Columns[0].Width := ReadInteger('Columns', 'A', lvDirectories.Columns[0].Width);
      lvDirectories.Columns[1].Width := ReadInteger('Columns', 'B', lvDirectories.Columns[1].Width);
      lvDirectories.Columns[2].Width := ReadInteger('Columns', 'B', lvDirectories.Columns[2].Width);
      lvDirectories.Columns[3].Width := ReadInteger('Columns', 'D', lvDirectories.Columns[3].Width);
      lvDirectories.Columns[4].Width := ReadInteger('Columns', 'E', lvDirectories.Columns[4].Width);
      pnlModuleExplorer.Width := ReadInteger('Position', 'Splitter', pnlModuleExplorer.Width);
      lvDirectories.Height := ReadInteger('Position', 'DirHeight', lvDirectories.Height);
      sl := TStringList.Create;
      Try
        ReadSection('Folders', sl);
        For j := 0 To sl.Count - 1 Do
          Begin
            Item := lvDirectories.Items.Add;
            Item.Caption := ReadString('Folders', sl[j], '#Error#');
            Item.Checked := ReadBool('FolderChecks', sl[j], False);
          End;
        ReadSection('ExcludedFiles', sl);
        For j := 0 To sl.Count - 1 Do
          FFileExcludeList.Add(ReadString('ExcludedFiles', sl[j], ''));
      Finally
        sl.Free;
      End;
      j := ReadInteger('Setup', 'Selection', 0);
      actFileRecurseFolders.Checked := ReadBool('Setup', 'Recurse', False);
      actViewErrors.Checked := ReadBool('Setup', 'Errors', False);
      actViewWarnings.Checked := ReadBool('Setup', 'Warnings', False);
      actViewHints.Checked := ReadBool('Setup', 'Hints', False);
      actViewDocConflicts.Checked := ReadBool('Setup', 'Conflicts', False);
      if lvFileList.Items.Count > j then
        lvFileList.ItemIndex := j;
    finally
      Free;
    end;
end;

(**

  This is an OnSelctionChange event handler for the module explorer.

  @precon  None.
  @postcon Moves the cursor to select item in the code.

  @param   iIdentLine   as an Integer
  @param   iIdentCol    as an Integer
  @param   iCommentLine as an Integer
  @param   iCommentCol  as an Integer

**)
procedure TfrmBrowseAndDocItTestForm.SelectionChange(iIdentLine, iIdentCol, iCommentLine,
  iCommentCol: Integer);

begin
  If iIdentLine * iIdentCol > 0 Then
    Begin
      FSynEdit.CaretX := iIdentCol;
      FSynEdit.CaretY := iIdentLine;
      FSynEdit.TopLine := iIdentLine - FSynEdit.LinesInWindow Div 2;
    End;
end;

(**

  This is a setter method for the FileName property.

  @precon  None.
  @postcon Sets the SynEdit file to the file named in the FileName property.

  @param   Value as a String as a constant

**)
procedure TfrmBrowseAndDocItTestForm.SetFileName(const Value: String);
var
  strExt: String;
begin
  FFileName := Value;
  Caption := FFileName;
  FSynEdit.Lines.LoadFromFile(FFileName);
  FSynedit.Modified := False;
  strExt := LowerCase(ExtractFileExt(FFileName));
  If IsKeyWord(strExt, ['.dpk', '.dpr', '.pas']) Then
    FSynEdit.Highlighter := FSynPasSyn
  Else If IsKeyWord(strExt, ['.bnf']) Then
    FSynEdit.Highlighter := FSynCPPSyn
  Else
    FSynEdit.Highlighter := FSynVBSyn;
  SynEdit1Change(Self);
end;

(**

  This is a setter method for the PathRoot property.

  @precon  None.
  @postcon Sets the root path of the selected file.

  @param   Value as a String as a constant

**)
procedure TfrmBrowseAndDocItTestForm.SetPathRoot(const Value: String);
begin
  FPathRoot := Value;
end;

(**

  This is an on change event handler for the SynEdit1 control.

  @precon  None.
  @postcon Creates a PascalDocModule class and passes it to the module explorer.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.SynEdit1Change(Sender: TObject);

begin
  FLastEdit := GetTickCount;
end;

(**

  This is an on status change event handler for the editor.

  @precon  None.
  @postcon Updates the top panels with the cursor position.

  @param   Sender  as a TObject
  @param   Changes as a TSynStatusChanges

**)
procedure TfrmBrowseAndDocItTestForm.SynEdit1StatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  sbrStatusBar.SimpleText := Format('Line %d, Column %d', [FSynEdit.CaretY, FSynEdit.CaretX]);
end;

procedure TfrmBrowseAndDocItTestForm.TimerEvent(Sender: TObject);

Const
  iInterval : Integer = 1000; // 1 second

begin
  If (FLastEdit > 0)  And (FLastEdit + iInterval < GetTickCount) Then
    Begin
      FTimer.Enabled := False;
      Try
        RefreshModuleExplorer;
      Finally
        FTimer.Enabled := True;
        FLastEdit := 0;
      End;
    End;
end;

(**

  This is a check button change event handler for all the error, hint, etc
  checkboxes.

  @precon  None.
  @postcon Populates the list view.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FilterChange(Sender: TObject);
begin
  PopulateListView;
end;

(**

  This method focuses the editor when the focus event is fired.

  @precon  None.
  @postcon Focuses the editor when the focus event is fired.

  @param   Sender as a TObject

**)
Procedure TfrmBrowseAndDocItTestForm.Focus(Sender : TObject);

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
constructor TParseRecord.Create(strFileName, strPathRoot: String; iErrors,
  iWarnings, iHints,iConflicts: Integer);

begin
  FFileName  := strFileName;
  FPathRoot  := strPathRoot;
  FErrors    := iErrors;
  FWarnings  := iWarnings;
  FHints     := iHints;
  FConflicts := iConflicts;
end;

End.

