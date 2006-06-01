(**
  
  This module contains a simple interface for to test the PascalDocModule parser
  and how it can better handle errors.

  @Version 1.0
  @Date    01 Jun 2006
  @Author  David Hoyle

**)
unit BrowseAndDocItTestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, ExtCtrls,
  ModuleExplorerFrame, BaseLanguageModule;

type
  (** This is thre class that defined the main interface form. **)
  TfrmBrowseAndDocItTestForm = class(TForm)
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer);
  private
    { Private declarations }
    FModuleExplorerFrame : TframeModuleExplorer;
    FDocOptions : TDocOptions;
    FFileName : String;
  public
    { Public declarations }
  end;

var
  (** This is a global form variable so that the Delphi IDE can auto create
      the form on application startup. **)
  frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm;

implementation

Uses
  PascalDocModule;

{$R *.dfm}

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
  FFileName := ExtractFilePath(ParamStr(0)) + 'Test Units\Test File.pas';
  SynEdit1.Lines.LoadFromFile(FFileName);
  SynEdit1Change(Self);
end;

(**

  This is the forms on destroy event handler.

  @precon  None.
  @postcon Cleans up the created instances from OnCreate.

  @param   Sender as a TObject

**)
procedure TfrmBrowseAndDocItTestForm.FormDestroy(Sender: TObject);
begin
  SynEdit1.Lines.SaveToFile(FFileName);
  FModuleExplorerFrame.Free;
  SpecialTags.Free;
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
  SynEdit1.TopLine := iIdentLine;
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
    M := TPascalDocModule.Create(Source, FFileName, True,
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

end.

