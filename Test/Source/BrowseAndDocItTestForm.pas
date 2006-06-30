(**
  
  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Version 1.0
  @Date    24 Jun 2006
  @Author  David Hoyle

**)
unit BrowseAndDocItTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, ExtCtrls,
  ModuleExplorerFrame, BaseLanguageModule, StdCtrls, FileCtrl, ComCtrls,
  Menus, StdActns, ActnList, ProgressForm;

type
  (** This is thre class that defined the main interface form. **)
  TfrmBrowseAndDocItTestForm = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FileListClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure edtDirectoryChange(Sender: TObject);
  private
    { Private declarations }
    FModuleExplorerFrame : TframeModuleExplorer;
    FDocOptions : TDocOptions;
    FDirectory : String;
    FFileName : String;
    FProgressForm : TfrmProgress;
    function GetFileName: String;
    procedure SetDirectory(const Value: String);
    procedure SetFileName(const Value: String);
    Function GetErrors(strFileName : String) : Integer;
    Procedure RecurseDirectories(strDirectory : String);
    property Directory : String Read FDirectory Write SetDirectory;
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
  PascalDocModule, TokenForm, Registry;

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
    SynEdit1.Lines.SaveToStream(Source);
    Source.Position := 0;
    M := TPascalDocModule.Create(Source, FileName, True,
      [moParse, moCheckForDocumentConflicts], FDocOptions);
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
    SynEdit1.Options := SynEdit1.Options + [eoShowSpecialChars]
  Else
    SynEdit1.Options := SynEdit1.Options - [eoShowSpecialChars];
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

Function TfrmBrowseAndDocItTestForm.GetErrors(strFileName : String) : Integer;

Var
  Source : TFileStream;
  M : TPascalDocModule;

Begin
  Source := TFileStream.Create(strFileName, fmOpenRead);
  Try
    Source.Position := 0;
    M := TPascalDocModule.Create(Source, strFileName, False,
      [moParse], FDocOptions);
    Try
      Result := M.Errors.Count;
    Finally
      M.Free;
    End;
  Finally
    Source.Free;
  End;
End;

(**

  This is an on click event handler for the FileListBox1 control.

  @precon  None.
  @postcon Sets the filename property to the selected file.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FileListClick(Sender: TObject);

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

  This is an on close event handler for the main form.

  @precon  None.
  @postcon Prompts to save the file if the file has been modified.

  @param   Sender   as a TObject
  @param   CanClose as a Boolean as a reference

**)
procedure TfrmBrowseAndDocItTestForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

Const
  strMsg = 'The file "%s" has been modified. Do you want to save the changes?';

begin
  If SynEdit1.Modified Then
    Case MessageDlg(Format(strMsg, [FFileName]), mtConfirmation,
      [mbYes, mbNo, mbCancel], 0) Of
      mrYes: SynEdit1.Lines.SaveToFile(FFileName);
      mrCancel: CanClose := False;
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

begin
  FProgressForm := TfrmProgress.Create(Nil);
  For i := Low(TDocOption) To High(TDocOption) Do
    Include(FDocOptions, i);
  SpecialTags := TStringList.Create;
  FModuleExplorerFrame := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent := Panel1;
  FModuleExplorerFrame.Align := alClient;
  FModuleExplorerFrame.OnSelectionChange := SelectionChange;
  With TRegIniFile.Create Do
    Try
      Top := ReadInteger(strRegRootKey + 'Position', 'Top', Top);
      Left := ReadInteger(strRegRootKey + 'Position', 'Left', Left);
      Height := ReadInteger(strRegRootKey + 'Position', 'Height', Height);
      Width := ReadInteger(strRegRootKey + 'Position', 'Width', Width);
      Panel1.Width := ReadInteger(strRegRootKey + 'Position', 'Splitter', Panel1.Width);
      edtDirectory.Text := ReadString(strRegRootKey + 'Position', 'Directory', GetCurrentDir);
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

Const
  strMsg = 'The file "%s" has been modified. Do you want to save the changes?';

begin
  With TRegIniFile.Create Do
    Try
      WriteInteger(strRegRootKey + 'Position', 'Top', Top);
      WriteInteger(strRegRootKey + 'Position', 'Left', Left);
      WriteInteger(strRegRootKey + 'Position', 'Height', Height);
      WriteInteger(strRegRootKey + 'Position', 'Width', Width);
      WriteInteger(strRegRootKey + 'Position', 'Splitter', Panel1.Width);
      WriteString(strRegRootKey + 'Position', 'Directory', edtDirectory.Text);
    Finally
      Free;
    End;
  FModuleExplorerFrame.Free;
  SpecialTags.Free;
  FProgressForm.Free;
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
  SynEdit1.CaretX := iIdentCol;
  SynEdit1.CaretY := iIdentLine;
  SynEdit1.TopLine := iIdentLine - SynEdit1.LinesInWindow Div 2;
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
  SynEdit1.Lines.LoadFromFile(FFileName);
  Synedit1.Modified := False;
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
  Source := TMemoryStream.Create;
  Try
    SynEdit1.Lines.SaveToStream(Source);
    Source.Position := 0;
    M := TPascalDocModule.Create(Source, FileName, SynEdit1.Modified,
      [moParse, moCheckForDocumentConflicts], FDocOptions);
    Try
      FModuleExplorerFrame.RenderModule(M, FDocOptions);
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
  Panel2.Caption := Format('Line %d, Column %d', [SynEdit1.CaretY, SynEdit1.CaretX]);
end;

procedure TfrmbrowseAndDocItTestForm.SetDirectory(Const Value : String);
begin
  FDirectory := Value;
  edtDirectory.Text := FDirectory;
  DirectoryListBox1Change(Self);
end;

procedure TfrmBrowseAndDocItTestForm.edtDirectoryChange(Sender: TObject);
begin
  If DirectoryExists(edtDirectory.Text) Then
    Directory := edtDirectory.Text;
end;

end.

