(**

  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Version 1.0
  @Date    26 Dec 2008
  @Author  David Hoyle

**)
unit BrowseAndDocItTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, SynEditHighlighter, SynHighlighterPas,
  SynEdit, ExtCtrls, ModuleExplorerFrame, BaseLanguageModule, StdCtrls,
  FileCtrl, ComCtrls, Contnrs, SynHighlighterVB,
  Menus, StdActns, ActnList, ProgressForm, Buttons, ImgList, ActnCtrls, ToolWin,
  ActnMan, ActnMenus, XPStyleActnCtrls, ActnPopup;

type
  (** This is thre class that defined the main interface form. **)
  TfrmBrowseAndDocItTestForm = class(TForm)
    pnlModuleExplorer: TPanel;
    pnlFileList: TPanel;
    Splitter1: TSplitter;
    lvFileList: TListView;
    DirectoryListBox: TDirectoryListBox;
    DriveComboBox: TDriveComboBox;
    chkHints: TCheckBox;
    chkWarnings: TCheckBox;
    chkErrors: TCheckBox;
    chkConflicts: TCheckBox;
    amActions: TActionManager;
    atbToolbar: TActionToolBar;
    ilImages: TImageList;
    actFileExit: TFileExit;
    actViewSpecialCharacters: TAction;
    actFileRecurseFolders: TAction;
    sbrStatusBar: TStatusBar;
    actEditCut: TEditCut;
    actEditCopy: TEditCopy;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    actEditDelete: TEditDelete;
    popActionBar: TPopupActionBar;
    Undo1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    actViewShowTokens: TAction;
    actToolsOptions: TAction;
    actToolsDocumentation: TAction;
    sptFiles: TSplitter;
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
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure edtDirectoryChange(Sender: TObject);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnOptionsClick(Sender: TObject);
    procedure RecurseFolders(Sender: TObject);
    procedure DocumentationClick(Sender: TObject);
    procedure lvFileListCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FilterChange(Sender: TObject);
  Strict Private
    { Private declarations }
    FFileName: String;
    FModuleExplorerFrame : TframeModuleExplorer;
    FDirectory : String;
    FProgressForm : TfrmProgress;
    FINIFileName : String;
    FSynEdit: TSynEdit;
    FSynPasSyn: TSynPasSyn;
    FSynVBSyn: TSynVBSyn;
    FParseRecords : TObjectList;
    function GetFileName: String;
    procedure SetFileName(const Value: String);
    procedure SetDirectory(const Value: String);
    procedure LoadSettings;
    procedure SaveSettings;
  Strict Protected
    Procedure RecurseDirectories(strDirectory : String);
    Procedure RefreshExplorer(Sender : TObject);
    Procedure PopulateListView;
    (**
      A property to define the directory for searching.
      @precon  None.
      @postcon Sets the directory for searching and initiates a new search or
               returns the durrent search directory.
      @return  a String
    **)
    property Directory : String Read FDirectory Write SetDirectory;
    (**
      A property to define the currently selected file.
      @precon  None.
      @postcon Sets the current file and initiates the file being displayed in
               the editor or returns the current file name.
      @return  a String
    **)
    Property FileName : String Read GetFileName Write SetFileName;
  public
    { Public declarations }
  end;

  (** This is an example helper class to demonstrate the coding, **)
  TMyHelper = Class Helper For TfrmBrowseAndDocItTestForm
  Private
  Protected
    Procedure GetErrors(strFileName : String; var iHints, iWarnings, iErrors, iConflicts : Integer);
  Public
  End;

  (** This is a class which represents a parser record of information. **)
  TParseRecord = Class
  Strict Private
    FFileName  : String;
    FErrors    : Integer;
    FWarnings  : Integer;
    FHints     : Integer;
    FConflicts : Integer;
  Public
    Constructor Create(strFileName : String; iErrors, iWarnings, iHints,
      iConflicts : Integer);
    (**
      This property returns the file name of the parser record.
      @precon  None.
      @postcon Returns the file name of the parser record.
      @return  a String
    **)
    Property FileName  : String Read FFileName;
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
  DocumentationDispatcher, BaseDocumentation, ShellAPI,
  DocumentationOptionsForm;

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
  Source : TMemoryStream;

begin
  Source := TMemoryStream.Create;
  Try
    FSynEdit.Lines.SaveToStream(Source);
    Source.Position := 0;
    M := Dispatcher(Source, FileName, True, [moParse, moCheckForDocumentConflicts]);
    If M <> Nil Then
      Try
        TfrmTokenForm.Execute(M);
      Finally
        M.Free;
      End;
  Finally
    Source.Free;
  End;
end;

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

  This is an on click event handler for the Recurse check box.

  @precon  None.
  @postcon Determines whether recursive searching is undertaken.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.RecurseFolders(Sender: TObject);
begin
  edtDirectoryChange(Sender);
end;

(**

  This is an on chnage event handler for the directory list control.

  @precon  None.
  @postcon Adds a list of PAS files and the number of errors to the list box.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.DirectoryListBox1Change(Sender: TObject);

begin
  lvFileList.Items.Clear;
  FParseRecords.Clear;
  FProgressForm.Init(1, 'Scanning Directories', 'Please wait...');
  RecurseDirectories(FDirectory);
  PopulateListView;
  FProgressForm.Hide;
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

  @precon  None.
  @postcon Recurse the directories searching for files.

  @param   strDirectory as a String

**)
Procedure TfrmBrowseAndDocItTestForm.RecurseDirectories(strDirectory : String);

Var
  recFile : TSearchRec;
  iResult : Integer;
  iHints, iWarnings, iErrors: Integer;
  iConflicts: Integer;

Begin
  iResult := FindFirst(strDirectory + '\*.*', faAnyFile, recFile);
  Try
    While iResult = 0 Do
      Begin
        If CanParseDocument(ExtractFileExt(recFile.Name)) Then
          Begin
            FProgressForm.UpdateProgress(0, 'Scanning: ' +
              Copy(strDirectory + '\' + recFile.Name, Length(FDirectory) + 1, 255) + '...');
            GetErrors(strDirectory + '\' + recFile.Name, iHints, iWarnings,
              iErrors, iConflicts);
            FParseRecords.Add(TParseRecord.Create(strDirectory + '\' + recFile.Name,
              iErrors, iWarnings, iHints, iConflicts));
            Application.ProcessMessages;
          End;
        If actFileRecurseFolders.Checked Then
          If (recFile.Attr And faDirectory <> 0) And (recFile.Name[1] <> '.') Then
            RecurseDirectories(strDirectory + '\' + recFile.Name);
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
Procedure TMyHelper.GetErrors(strFileName : String;
  var iHints, iWarnings, iErrors, iConflicts : Integer);

Var
  Source : TFileStream;
  M : TBaseLanguageModule;
  i : Integer;
  C: TElementContainer;

Begin
  iHints := 0;
  iWarnings := 0;
  iErrors := 0;
  iConflicts := 0;
  Source := TFileStream.Create(strFileName, fmOpenRead);
  Try
    Source.Position := 0;
    M := Dispatcher(Source, strFileName, False, [moParse, moCheckForDocumentConflicts]);
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
      PopupMenu := popActionBar;
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
  LoadSettings;
end;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Cleans up the created instances from OnCreate.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FormDestroy(Sender: TObject);

begin
  FSynEdit.Highlighter := Nil;
  FSynEdit.Free;
  FSynVBSyn.Free;
  FSynPasSyn.Free;
  SaveSettings;
  FModuleExplorerFrame.Free;
  FProgressForm.Free;
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

  This is a custom draw item event handler for the list view.

  @precon  None.
  @postcon Colours Errors, Hints, Warnings, etc.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmBrowseAndDocItTestForm.lvFileListCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);

Var
  iErrors, iWarnings, iHints, iConflicts : Integer;

begin
  iErrors := 0;
  iWarnings := 0;
  iHints := 0;
  iConflicts := 0;
  If Item.SubItems.Count > 2 Then
    iErrors := StrToInt(Item.SubItems[2]);
  If Item.SubItems.Count > 1 Then
    iWarnings := StrToInt(Item.SubItems[1]);
  If Item.SubItems.Count > 0 Then
    iHints := StrToInt(Item.SubItems[0]);
  If Item.SubItems.Count > 3 Then
    iConflicts := StrToInt(Item.SubItems[3]);
  If iConflicts > 0 Then
    Sender.Canvas.Brush.Color := clYellow;
  If iHints > 0 Then
    Sender.Canvas.Brush.Color := clLime;
  If iWarnings > 0 Then
    Sender.Canvas.Brush.Color := $0080FF;
  If iErrors > 0 Then
    Sender.Canvas.Brush.Color := clRed;
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
      FileName := lvFileList.Selected.SubItems[4]
    Else
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

begin
  Try
    lvFileList.Clear;
    lvFileList.Items.BeginUpdate;
    For i := 0 To FParseRecords.Count - 1 Do
      Begin
        rec := FParseRecords[i] As TParseRecord;
        boolInclude := Not chkErrors.Checked And Not chkWarnings.Checked And
          Not chkHints.Checked And Not chkConflicts.Checked;
        boolInclude := boolInclude Or (chkErrors.Checked And (rec.Errors > 0));
        boolInclude := boolInclude Or (chkWarnings.Checked And (rec.Warnings > 0));
        boolInclude := boolInclude Or (chkHints.Checked And (rec.Hints > 0));
        boolInclude := boolInclude Or (chkConflicts.Checked And (rec.Conflicts > 0));
        If boolInclude Then
          Begin
            liItem := lvFileList.Items.Add;
            liItem.Caption := ExtractFileName(rec.FileName);
            liItem.SubItems.Add(Format('%d', [rec.Hints]));
            liItem.SubItems.Add(Format('%d', [rec.Warnings]));
            liItem.SubItems.Add(Format('%d', [rec.Errors]));
            liItem.SubItems.Add(Format('%d', [rec.Conflicts]));
            liItem.SubItems.Add(rec.FileName);
          End;
      End;
  Finally
    lvFileList.Items.EndUpdate;
  End;
end;

procedure TfrmBrowseAndDocItTestForm.SaveSettings;
begin
  with TIniFile.Create(FINIFileName) do
    try
      WriteInteger('Position', 'Top', Top);
      WriteInteger('Position', 'Left', Left);
      WriteInteger('Position', 'Height', Height);
      WriteInteger('Position', 'Width', Width);
      WriteInteger('Position', 'FileSplitter', pnlFileList.Width);
      WriteInteger('Columns', '1', lvFileList.Columns[0].Width);
      WriteInteger('Columns', '2', lvFileList.Columns[1].Width);
      WriteInteger('Columns', '3', lvFileList.Columns[2].Width);
      WriteInteger('Columns', '4', lvFileList.Columns[3].Width);
      WriteInteger('Columns', '5', lvFileList.Columns[4].Width);
      WriteInteger('Position', 'Splitter', pnlModuleExplorer.Width);
      WriteString('Position', 'Directory', DirectoryListBox.Directory);
      WriteInteger('Setup', 'Selection', lvFileList.ItemIndex);
      WriteBool('Setup', 'Recurse', actFileRecurseFolders.Checked);
      WriteBool('Setup', 'Errors', chkErrors.Checked);
      WriteBool('Setup', 'Warnings', chkWarnings.Checked);
      WriteBool('Setup', 'Hints', chkHints.Checked);
      WriteBool('Setup', 'Conflicts', chkConflicts.Checked);
    finally
      Free;
    end;
end;

procedure TfrmBrowseAndDocItTestForm.LoadSettings;
var
  j: Integer;
begin
  with TIniFile.Create(FINIFileName) do
    try
      Top := ReadInteger('Position', 'Top', Top);
      Left := ReadInteger('Position', 'Left', Left);
      Height := ReadInteger('Position', 'Height', Height);
      Width := ReadInteger('Position', 'Width', Width);
      pnlFileList.Width := ReadInteger('Position', 'FileSplitter', pnlFileList.Width);
      lvFileList.Columns[0].Width := ReadInteger('Columns', '1', lvFileList.Columns[0].Width);
      lvFileList.Columns[1].Width := ReadInteger('Columns', '2', lvFileList.Columns[1].Width);
      lvFileList.Columns[2].Width := ReadInteger('Columns', '3', lvFileList.Columns[2].Width);
      lvFileList.Columns[3].Width := ReadInteger('Columns', '4', lvFileList.Columns[3].Width);
      lvFileList.Columns[4].Width := ReadInteger('Columns', '5', lvFileList.Columns[4].Width);
      pnlModuleExplorer.Width := ReadInteger('Position', 'Splitter', pnlModuleExplorer.Width);
      DirectoryListBox.Directory := ReadString('Position', 'Directory', GetCurrentDir);
      j := ReadInteger('Setup', 'Selection', 0);
      actFileRecurseFolders.Checked := ReadBool('Setup', 'Recurse', False);
      RecurseFolders(Self);
      chkErrors.Checked := ReadBool('Setup', 'Errors', False);
      chkWarnings.Checked := ReadBool('Setup', 'Warnings', False);
      chkHints.Checked := ReadBool('Setup', 'Hints', False);
      chkConflicts.Checked := ReadBool('Setup', 'Conflicts', False);
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

  @param   Value as a String constant

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
  Else
    FSynEdit.Highlighter := FSynVBSyn;
  SynEdit1Change(Self);
end;

(**

  This is an on change event handler for the SynEdit1 control.

  @precon  None.
  @postcon Creates a PascalDocModule class and passes it to the module explorer.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.SynEdit1Change(Sender: TObject);

Var
  M : TBaseLanguageModule;
  Source : TMemoryStream;

begin
  Source := TMemoryStream.Create;
  Try
    FSynEdit.Lines.SaveToStream(Source);
    Source.Position := 0;
    M := Dispatcher(Source, FileName, FSynEdit.Modified, [moParse, moCheckForDocumentConflicts]);
    If M <> Nil Then
      Try
        FModuleExplorerFrame.RenderModule(M);
      Finally
        M.Free;
      End;
  Finally
    Source.Free;
  End;
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

(**

  This is a setter method for the Directory property.

  @precon  None.
  @postcon Forces the list box of files to be updated when the directory changes.

  @param   Value as a String constant

**)
procedure TfrmbrowseAndDocItTestForm.SetDirectory(Const Value : String);
begin
  FDirectory := Value;
  DirectoryListBox.Directory := FDirectory;
  DirectoryListBox1Change(Self);
end;

(**

  This is an edit on change event handler.

  @precon  None.
  @postcon Updates the Directory based on the directory typed in the edit
           control.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.edtDirectoryChange(Sender: TObject);
begin
  If DirectoryExists(DirectoryListBox.Directory) Then
    Directory := DirectoryListBox.Directory;
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

  @precon  None.
  @postcon Initialises the record class with information.

  @param   strFileName as a String
  @param   iErrors     as an Integer
  @param   iWarnings   as an Integer
  @param   iHints      as an Integer
  @param   iConflicts  as an Integer

**)
constructor TParseRecord.Create(strFileName: String; iErrors, iWarnings, iHints,
  iConflicts: Integer);
begin
  FFileName  := strFileName;
  FErrors    := iErrors;
  FWarnings  := iWarnings;
  FHints     := iHints;
  FConflicts := iConflicts;
end;

End.

