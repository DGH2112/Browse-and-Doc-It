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
  ModuleExplorerFrame, BaseLanguageModule, StdCtrls, FileCtrl, ComCtrls;

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
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    btnQuit: TButton;
    lvFileList: TListView;
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
  private
    { Private declarations }
    FModuleExplorerFrame : TframeModuleExplorer;
    FDocOptions : TDocOptions;
    function GetFileName: String;
    procedure SetFileName(const Value: String);
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

Var
  recFile : TSearchRec;
  iResult : Integer;
  liItem : TListItem;
  Source : TFileStream;
  M : TPascalDocModule;

begin
  lvFileList.Items.Clear;
  iResult := FindFirst(DirectoryListBox1.Directory + '\*.pas', faAnyFile, recFile);
  Try
    While iResult = 0 Do
      Begin
        liItem := lvFileList.Items.Add;
        liItem.Caption := recFile.Name;
        Source := TFileStream.Create(DirectoryListBox1.Directory + '\' +
          recFile.Name, fmOpenRead);
        Try
          Source.Position := 0;
          M := TPascalDocModule.Create(Source,
            DirectoryListBox1.Directory + '\' + recFile.Name, False,
            [moParse], FDocOptions);
          Try
            liItem.SubItems.Add(Format('%d', [M.Errors.Count]));
            Application.ProcessMessages;
          Finally
            M.Free;
          End;
        Finally
          Source.Free;
        End;
        iResult := FindNext(recFile);
      End;
  Finally
    FindClose(recFile);
  End;
end;

(**

  This is an on click event handler for the FileListBox1 control.

  @precon  None.
  @postcon Sets the filename property to the selected file.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FileListClick(Sender: TObject);
begin
  If lvFileList.Selected <> Nil Then
    If FileExists(lvFileList.Selected.Caption) Then
      FileName := lvFileList.Selected.Caption
    Else
      MessageDlg(Format('The file "%s" was not found.',
        [lvFileList.Selected.Caption]), mtError, [mbOK], 0);
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
    Case MessageDlg(Format(strMsg, [FileName]), mtConfirmation,
      [mbYes, mbNo, mbCancel], 0) Of
      mrYes: SynEdit1.Lines.SaveToFile(FileName);
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
    Finally
      Free;
    End;
  FModuleExplorerFrame.Free;
  SpecialTags.Free;
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
  If lvFileList.Selected <> Nil Then
    Result := lvFileList.Selected.Caption;
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
  SynEdit1.Lines.LoadFromFile(FileName);
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

end.

