(**

  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Version 1.0
  @Date    01 Aug 2008
  @Author  David Hoyle

**)
unit BrowseAndDocItTestForm;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF VER180} Variants, {$ENDIF} Classes,
  Graphics, Controls, Forms, Dialogs, SynEditHighlighter, SynHighlighterPas,
  SynEdit, ExtCtrls, ModuleExplorerFrame, BaseLanguageModule, StdCtrls,
  {$WARN UNIT_PLATFORM OFF} FileCtrl {$WARN UNIT_PLATFORM ON}, ComCtrls,
  Menus, StdActns, ActnList, ProgressForm;

type
  (** This is thre class that defined the main interface form. **)
  TfrmBrowseAndDocItTestForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Panel3: TPanel;
    Splitter1: TSplitter;
    btnQuit: TButton;
    lvFileList: TListView;
    edtDirectory: TEdit;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    Undo1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    btnOptions: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer; SelectType : TSelectType);
    Procedure Focus(Sender : TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnQuitClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure edtDirectoryChange(Sender: TObject);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnOptionsClick(Sender: TObject);
  private
    { Private declarations }
    FModuleExplorerFrame : TframeModuleExplorer;
    FDirectory : String;
    FFileName : String;
    FProgressForm : TfrmProgress;
    FINIFileName : String;
    FSynEdit: TSynEdit;
    FSynPasSyn: TSynPasSyn;
    function GetFileName: String;
    procedure SetDirectory(const Value: String);
    procedure SetFileName(const Value: String);
    Function GetErrors(strFileName : String) : Integer;
    Procedure RecurseDirectories(strDirectory : String);
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

var
  (** This is a global form variable so that the Delphi IDE can auto create
      the form on application startup. **)
  frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm;

implementation

Uses
  PascalDocModule, TokenForm, IniFiles, DGHLibrary, OptionsForm;

{$R *.dfm}

(**

  This is an on click event handler for the Tokens button.

  @precon  None.
  @postcon Displays a form showing the tokens from the current file.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.Button1Click(Sender: TObject);

Var
  M : TPascalDocModule;
  Source : TMemoryStream;

begin
  Source := TMemoryStream.Create;
  Try
    FSynEdit.Lines.SaveToStream(Source);
    Source.Position := 0;
    M := TPascalDocModule.Create(Source, FileName, True,
      [moParse, moCheckForDocumentConflicts]);
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

Var
  i : Integer;
  strHelp : String;

begin
  i := 5;
  If TfrmOptions.Execute(i, strHelp) Then
    SynEdit1Change(Sender);
end;

(**

  This is an on click event handler for the Quit Button.

  @precon  None.
  @postcon Closes the application.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

(**

  This is an on click event handler for the Checkbox1 control.

  @precon  None.
  @postcon Displays or hides the special character markers in the SynEdit1
           control.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.CheckBox1Click(Sender: TObject);
begin
  If CheckBox1.Checked Then
    FSynEdit.Options := FSynEdit.Options + [eoShowSpecialChars]
  Else
    FSynEdit.Options := FSynEdit.Options - [eoShowSpecialChars];
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
  FProgressForm.Init(1, 'Scanning Directories', 'Please wait...');
  RecurseDirectories(FDirectory);
  FProgressForm.Hide;
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
  liItem : TListItem;

Begin
  iResult := FindFirst(strDirectory + '\*.*', faAnyFile, recFile);
  Try
    While iResult = 0 Do
      Begin
        If IsKeyWord(ExtractFileExt(recFile.Name), ['.dpk', '.dpr', '.pas']) Then
          Begin
            FProgressForm.UpdateProgress(0, 'Scanning: ' +
              Copy(strDirectory + '\' + recFile.Name, Length(FDirectory) + 1, 255) + '...');
            liItem := lvFileList.Items.Add;
            liItem.Caption := recFile.Name;
            liItem.SubItems.Add(Format('%d', [
              GetErrors(strDirectory + '\' + recFile.Name)]));
            liItem.SubItems.Add(strDirectory + '\' + recFile.Name);
            Application.ProcessMessages;
          End;
        If (recFile.Attr And faDirectory <> 0) And (recFile.Name[1] <> '.') Then
          RecurseDirectories(strDirectory + '\' + recFile.Name);
        iResult := FindNext(recFile);
      End;
  Finally
    FindClose(recFile);
  End;
End;

(**

  This method gets the number of errors for the given files name.

  @precon  None.
  @postcon Gets the number of errors for the given files name.

  @param   strFileName as a String
  @return  an Integer    

**)
Function TfrmBrowseAndDocItTestForm.GetErrors(strFileName : String) : Integer;

Var
  Source : TFileStream;
  M : TPascalDocModule;

Begin
  Source := TFileStream.Create(strFileName, fmOpenRead);
  Try
    Source.Position := 0;
    M := TPascalDocModule.Create(Source, strFileName, False, [moParse]);
    Try
      Result := 0;
      If M.FindElement(strErrorsAndWarnings) <> Nil Then
        Result := M.FindElement(strErrorsAndWarnings).ElementCount;
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

Var
  i : TDocOption;
  j : Integer;

begin
  FSynEdit := TSynEdit.Create(Nil);
  With FSynEdit Do
    Begin
      Parent := Self;
      Highlighter := FSynPasSyn;
      Align := alClient;
      ActiveLineColor := clSkyBlue;
      Gutter.ShowLineNumbers := True;
      PopupMenu := PopupMenu1;
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
  FSynEdit.Highlighter := FSynPasSyn;
  FINIFileName := BuildRootKey(Nil, Nil);
  OutputDebugString('Started');
  FProgressForm := TfrmProgress.Create(Nil);
  FModuleExplorerFrame := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent := Panel1;
  FModuleExplorerFrame.Align := alClient;
  FModuleExplorerFrame.OnSelectionChange := SelectionChange;
  FModuleExplorerFrame.OnFocus := Focus;
  With TIniFile.Create(FINIFileName) Do
    Try
      For i := Low(TDocOption) to High(TDocOption) Do
        If ReadBool('Options', DocOptionInfo[i].FDescription,
          DocOptionInfo[i].FEnabled) Then
          BrowseAndDocItOptions.Options := BrowseAndDocItOptions.Options + [i]
        Else
          BrowseAndDocItOptions.Options := BrowseAndDocItOptions.Options - [i];
      For j := 0 To SpecialTags.Count - 1 Do
        SpecialTags.Objects[j] := TObject(
          ReadInteger('SpecialTags', SpecialTags.Names[j],
          Integer(SpecialTags.Objects[j]))
        );
      Top := ReadInteger('Position', 'Top', Top);
      Left := ReadInteger('Position', 'Left', Left);
      Height := ReadInteger('Position', 'Height', Height);
      Width := ReadInteger('Position', 'Width', Width);
      Panel1.Width := ReadInteger('Position', 'Splitter', Panel1.Width);
      edtDirectory.Text := ReadString('Position', 'Directory', GetCurrentDir);
      j := ReadInteger('Setup', 'Selection', 0);
      If lvFileList.Items.Count > j Then
        lvFileList.ItemIndex := j;
    Finally
      Free;
    End;
end;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Cleans up the created instances from OnCreate.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FormDestroy(Sender: TObject);

Var
  i : TDocOption;
  j : Integer;

begin
  FSynEdit.Highlighter := Nil;
  FSynEdit.Free;
  FSynPasSyn.Free;
  With TIniFile.Create(FINIFileName) Do
    Try
      For i := Low(TDocOption) to High(TDocOption) Do
        WriteBool('Options', DocOptionInfo[i].FDescription,
          i In BrowseAndDocItOptions.Options);
      For j := 0 To SpecialTags.Count - 1 Do
        WriteInteger('SpecialTags', SpecialTags.Names[j],
          Integer(SpecialTags.Objects[j]));
      WriteInteger('Position', 'Top', Top);
      WriteInteger('Position', 'Left', Left);
      WriteInteger('Position', 'Height', Height);
      WriteInteger('Position', 'Width', Width);
      WriteInteger('Position', 'Splitter', Panel1.Width);
      WriteString('Position', 'Directory', edtDirectory.Text);
      WriteInteger('Setup', 'Selection', lvFileList.ItemIndex);
    Finally
      Free;
    End;
  FModuleExplorerFrame.Free;
  FProgressForm.Free;
  OutputDebugString('Finished');
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
    If FileExists(lvFileList.Selected.SubItems[1]) Then
      FileName := lvFileList.Selected.SubItems[1]
    Else
      MessageDlg(Format('The file "%s" was not found.',
        [lvFileList.Selected.SubItems[1]]), mtError, [mbOK], 0);
end;

(**

  This is an OnSelctionChange event handler for the module explorer.

  @precon  None.
  @postcon Moves the cursor to select item in the code.

  @param   iIdentLine   as an Integer
  @param   iIdentCol    as an Integer
  @param   iCommentLine as an Integer
  @param   iCommentCol  as an Integer
  @param   SelectType   as a TSelectType

**)
procedure TfrmBrowseAndDocItTestForm.SelectionChange(iIdentLine, iIdentCol, iCommentLine,
  iCommentCol: Integer; SelectType : TSelectType);

begin
  FSynEdit.CaretX := iIdentCol;
  FSynEdit.CaretY := iIdentLine;
  FSynEdit.TopLine := iIdentLine - FSynEdit.LinesInWindow Div 2;
end;

(**

  This is a setter method for the FileName property.

  @precon  None.
  @postcon Sets the SynEdit file to the file named in the FileName property.

  @param   Value as a String constant

**)
procedure TfrmBrowseAndDocItTestForm.SetFileName(const Value: String);
begin
  FFileName := Value;
  Caption := FFileName;
  FSynEdit.Lines.LoadFromFile(FFileName);
  FSynedit.Modified := False;
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
  M : TPascalDocModule;
  Source : TMemoryStream;

begin
  //: @todo Only update module explorer IF change and been stead for several seconds.
  Source := TMemoryStream.Create;
  Try
    FSynEdit.Lines.SaveToStream(Source);
    Source.Position := 0;
    M := TPascalDocModule.Create(Source, FileName, FSynEdit.Modified,
      [moParse, moCheckForDocumentConflicts]);
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
  Panel2.Caption := Format('Line %d, Column %d', [FSynEdit.CaretY, FSynEdit.CaretX]);
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
  edtDirectory.Text := FDirectory;
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
  If DirectoryExists(edtDirectory.Text) Then
    Directory := edtDirectory.Text;
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


end.

