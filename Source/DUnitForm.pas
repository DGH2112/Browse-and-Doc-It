(**

  This module defines a class which reprepsents a form for selecting
  DUnit Unit Tetsing Options.

  @Version 1.0
  @Author  David Hoyle
  @Date    29 Mar 2009

**)
unit DUnitForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DUnitCreator, VirtualTrees, BaseLanguageModule,
  ImgList;

type
  (** A class to represent the form interface. **)
  TfrmDUnit = class(TForm)
    gbxProject: TGroupBox;
    rdoExistingProject: TRadioButton;
    cbxExistingProject: TComboBox;
    rdoNewProject: TRadioButton;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    gbxUnit: TGroupBox;
    rdoExistingUnit: TRadioButton;
    cbxExistingUnit: TComboBox;
    rdoNewUnit: TRadioButton;
    edtNewUnitName: TEdit;
    edtNewProjectName: TEdit;
    vstTestCases: TVirtualStringTree;
    ilScopeImages: TImageList;
    lblBaseClass: TLabel;
    cbxBaseClass: TComboBox;
    lblTestSuiteName: TLabel;
    edtTestSuiteName: TEdit;
    procedure rdoNewExistingProject(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rdoNewExistingUnit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstTestCasesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstTestCasesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FDUnitCreator : TDUnitCreator;
    FModule: PVirtualNode;
    FRootElement: TLabelContainer;
    FTestCases : TStringList;
    Procedure InitialiseTreeView;
    Function AddNode(P : PVirtualNode; Element : TElementContainer) : PVirtualNode;
    procedure RenderContainers(RootNode: PVirtualNode;
      Container: TElementContainer);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure BuildTestCaseList;
    Function AddUniqueName(slList : TStrings; strText : String) : String;
    Procedure ErrorProc(strMsg : String);
  public
    { Public declarations }
    Class Procedure Execute(objDUnitCreator : TDUnitCreator);
  end;

implementation

Uses
  ToolsAPIUtils, PascalModule, IniFiles, dghlibrary;

Type
  (** This is a record to describe the data stored in the virtual tree view. **)
  TTreeData = Record
    Element : TElementContainer;
  End;

{$R *.dfm}

(**

  This method adds a node (module element) to the virtual tree view with the
  parent P.

  @precon  P and Element must both be a valid instances.
  @postcon Adds a node (module element) to the virtual tree view with the
           parent P.

  @param   P       as a PVirtualNode
  @param   Element as a TElementContainer
  @return  a PVirtualNode

**)
function TfrmDUnit.AddNode(P : PVirtualNode; Element : TElementContainer) : PVirtualNode;

Var
  NodeData : ^TTreeData;

begin
  Result := vstTestCases.AddChild(P);
  NodeData := vstTestCases.GetNodeData(Result);
  NodeData.Element := Element;
  Result.CheckType := ctTriStateCheckBox;
end;

(**

  This method ensures that a unique name is added to the string list.

  @precon  slList must be a valid string list . 
  @postcon Ensures that a unique name is added to the string list . 

  @param   slList  as a TStrings
  @param   strText as a String
  @return  a String

**)
Function TfrmDUnit.AddUniqueName(slList: TStrings; strText: String) : String;

Var
  iIndex: Integer;

begin
  Result := strText;
  If slList.IndexOf(strText) = -1 Then
    Exit;
  iIndex := 1;
  While slList.IndexOf(Format('%s%d', [strText, iIndex])) > -1 Do
    Inc(iIndex);
  Result := Format('%s%d', [strText, iIndex]);
end;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Checks that there are existing projects to work with.

  @param   Sender as a TObject

**)
procedure TfrmDUnit.btnOKClick(Sender: TObject);

ResourceString
  strThereAreNoExistingDUnitProjects = 'There are no existing DUnit projects ' +
    'to use.';
  strTheProjectNameExists = 'The project name ''%s'' already exists in the ' +
    'current project group!';
  strTheUnitNameExists = 'The project already contains a unit named ''%s''!';
  strMustSelectUnitTest = 'You must select at least 1 unit to test!';

begin
  If rdoExistingProject.Checked Then
    If cbxExistingProject.ItemIndex = -1 Then
      Begin
        MessageDlg(strThereAreNoExistingDUnitProjects, mtError, [mbOK], 0);
        ModalResult := mrNone;
      End;
  If rdoNewProject.Checked Then
    If FDUnitCreator.DoesProjectExist(edtNewProjectName.Text) Then
      Begin
        MessageDlg(Format(strTheProjectNameExists, [edtNewProjectName.Text]),
          mtError, [mbOK], 0);
        ModalResult := mrNone;
      End;
  If rdoExistingProject.Checked Then
    If rdoNewUnit.Checked Then
      If FDUnitCreator.DoesUnitExist(cbxExistingProject.ItemIndex,
        edtNewUnitName.Text) Then
        Begin
          MessageDlg(Format(strTheUnitNameExists, [edtNewUnitName.Text]), mtError,
            [mbOK], 0);
          ModalResult := mrNone;
        End;
  If FModule.CheckState In [csUncheckedNormal] Then
    Begin
      MessageDlg(strMustSelectUnitTest, mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
end;

(**

  This method builds a string list of the test cases 'Class=Method' from the
  selected nodes in the tree view.

  @precon  None.
  @postcon Builds a string list of the test cases 'Class=Method' from the
           selected nodes in the tree view.

**)
procedure TfrmDUnit.BuildTestCaseList;

Var
  C: PVirtualNode;
  M: PVirtualNode;
  Cls : ^TTreeData;
  Method : ^TTreeData;
  strMethod: String;

begin
  C := vstTestCases.GetFirstChild(FModule);
  While C <> Nil Do
    Begin
      Cls := vstTestCases.GetNodeData(C);
      M := vstTestCases.GetFirstChild(C);
      If M <> Nil Then
        While M <> Nil Do
          Begin
            If M.CheckState In [csCheckedNormal] Then
              Begin
                Method := vstTestCases.GetNodeData(M);
                If (Method.Element Is TPascalMethod) Or
                  (Method.Element Is TPascalProperty) Then
                  Begin
                    strMethod := AddUniqueName(FTestCases, Format('%s=%s',
                      [Cls.Element.Identifier, Method.Element.Identifier]));
                    FTestCases.Add(strMethod);
                  End;
              End;
            M := vstTestCases.GetNextSibling(M);
          End
      Else
        If C.CheckState In [csCheckedNormal] Then
          Begin
            strMethod := AddUniqueName(FTestCases, Format('%s=%s',
              ['', Cls.Element.Identifier]));
            FTestCases.Add(strMethod);
          End;
      C := vstTestCases.GetNextSibling(C);
    End;
end;

(**

  This is an error event hanlder for errors raised in the DUnitCreator module.

  @precon  None.
  @postcon Displays an error in a dialogue box.

  @param   strMsg as a String

**)
Procedure TfrmDUnit.ErrorProc(strMsg : String);

Begin
  MessageDlg(strMsg, mtError, [mbOK], 0);
End;

(**

  This is the classes main interface method(Singleton class).

  @precon  None .
  @postcon Creates an instance of the Singleton class .

  @param   objDUnitCreator as an TDUnitCreator

**)
Class Procedure TfrmDUnit.Execute(objDUnitCreator : TDUnitCreator);

var
  strUnitTobeTested: String;

Begin
  With TfrmDUnit.Create(Nil) Do
    Try
      objDUnitCreator.Errors := ErrorProc;
      FDUnitCreator := objDUnitCreator;
      InitialiseTreeView;
      rdoNewExistingProject(Nil);
      rdoNewExistingUnit(Nil);
      If ShowModal = mrOK Then
        Begin
          If cbxBaseClass.Items.IndexOf(cbxBaseClass.Text) = -1 Then
            cbxBaseClass.Items.Add(cbxBaseClass.Text);
          SaveSettings;
          strUnitTobeTested := ActiveSourceEditor.FileName;
          BuildTestCaseList;
          If rdoNewProject.Checked Then
            objDUnitCreator.CreateTestProject(edtNewProjectName.Text)
          Else
            objDUnitCreator.ActivateProject(cbxExistingProject.ItemIndex);
          objDUnitCreator.AddUnitToBeTested(strUnitToBeTested);
          strUnitToBeTested := ChangeFileExt(ExtractFileName(strUnitToBeTested), '');
          If rdoNewUnit.Checked Then
            objDUnitCreator.CreateTestUnit(edtNewUnitName.Text,
              strUnitToBeTested, FTestCases, cbxBaseClass.Text,
              edtTestSuiteName.Text)
          Else
            objDUnitCreator.UpdateTestUnit(cbxExistingUnit.ItemIndex,
              strUnitToBeTested, FTestCases, cbxBaseClass.Text,
              edtTestSuiteName.Text);
        End;
    Finally
      Free;
    End;
End;

(**

  This is the forms on create event handler.

  @precon  None.
  @postcon Initialises the project and unit name edit fields.

  @param   Sender as a TObject

**)
procedure TfrmDUnit.FormCreate(Sender: TObject);

Type
  T = BaseLanguageModule.TImageIndex;

Var
  strFileName : String;
  i : T;

begin
  FTestCases := TStringList.Create;
  ActiveControl := rdoNewProject;
  LoadSettings;
  If cbxBaseClass.Items.Count = 0 Then
    Begin
      cbxBaseClass.Items.Add('TTestCase');
      cbxBaseClass.ItemIndex := 0;
    End;
  vstTestCases.NodeDataSize := SizeOf(TTreeData);
  ilScopeImages.Clear;
  For i := Succ(Low(T)) to High(T) Do
    If Not ilScopeImages.GetInstRes(hInstance, rtBitmap,
      ImageList[i].FResourceName, 16, [lrDefaultColor], ImageList[i].FMaskColour) Then
      ShowMessage(Format('Resource "%s" not found.', [ImageList[i].FResourceName]));
  strFileName := ChangeFileExt(ExtractFileName(ActiveProject.FileName), '') +
    'Tests.dpr';
  edtNewProjectName.Text := AddUniqueName(cbxExistingProject.Items, strFileName);
  strFileName := 'Test' + ExtractFileName(ActiveSourceEditor.FileName);
  edtNewUnitName.Text := AddUniqueName(cbxExistingUnit.Items, strFileName);

end;

(**

  This is an OnFormDestroy Event Hanlder for the TfrmDUnit class.

  @precon  None.
  @postcon Frees the memory used by the Root Element of the tree view.

  @param   Sender as a TObject

**)
procedure TfrmDUnit.FormDestroy(Sender: TObject);
begin
  FRootElement.Free;
  FTestCases.Free;
end;

(**

  This method starts the rendering on testable elements in the module.

  @precon  None.
  @postcon Starts the rendering on testable elements in the module.

**)
procedure TfrmDUnit.InitialiseTreeView;

ResourceString
  strNotAPascalModule = 'Can only create unit test for Pascal modules!';

Var
  M, I : TElementContainer;
  N: PVirtualNode;
  Node : ^TTreeData;
  strClass: String;
  T: TElementContainer;
  C: TElementContainer;
  P: TElementContainer;
  j: Integer;

begin
  M := FDUnitCreator.Module;
  If M Is TPascalModule Then
    Begin
      FRootElement := TLabelContainer.Create(Format('Test Units for %s', [
        M.AsString(True, False)]), scNone, 0, 0, iiNone, Nil);
      I := M.FindElement(strImplementedMethodsLabel);
      T := M.FindElement(strTypesLabel);
      If I <> Nil Then
        Begin
          FModule := AddNode(Nil, FRootElement);
          RenderContainers(FModule, I);
          vstTestCases.Expanded[FModule] := True;
          N := vstTestCases.GetFirstChild(FModule);
          While N <> Nil Do
            Begin
              Node := vstTestCases.GetNodeData(N);
              strClass := Node.Element.Identifier;
              If T <> Nil Then
                Begin
                  C := T.FindElement(strClass);
                  If C <> Nil Then
                    Begin
                      P := C.FindElement(strPropertiesLabel);
                      If P <> Nil Then
                        For j := 1 To P.ElementCount Do
                          If P.Elements[j].Scope In [scPublic, scPublished] Then
                            AddNode(N, P.Elements[j]);
                    End;
                End;
              N := vstTestCases.GetNextSibling(N);
            End;
        End;
    End Else
      MessageDlg(strNotAPascalModule, mtError, [mbOK], 0);
end;

(**

  This method loads the dialogues settings from the INI file.

  @precon  None.
  @postcon Loads the dialogues settings from the INI file.

**)
procedure TfrmDUnit.LoadSettings;

Var
  i : Integer;
  sl : TStringList;

begin
  With TIniFile.Create(BrowseAndDocitOptions.IniFileName) Do
    Try
      Top := ReadInteger('DUnitDlg', 'Top', (Screen.Height - Height) Div 2);
      Left := ReadInteger('DUnitDlg', 'Left', (Screen.Width - Width) Div 2);
      Height := ReadInteger('DUnitDlg', 'Height', Height);
      Width := ReadInteger('DUnitDlg', 'Width', Width);
      sl := TStringList.Create;
      Try
        ReadSection('DUnitBaseClasses', sl);
        For i := 0 To sl.Count - 1 Do
          cbxBaseClass.Items.Add(ReadString('DUnitBaseClasses',
            sl[i], ''));
      Finally
        sl.Free;
      End;
      cbxBaseClass.Text := ReadString('DUnitDlg', 'BaseClass', 'TTestCase');
      edtTestSuiteName.Text := ReadString('DUnitDlg', 'TestSuiteName', '');
    Finally
      Free;
    End;
end;

(**

  This is an on click event handler for the Existing Radio Button.

  @precon  None.
  @postcon Rebuilds the existing project list.

  @param   Sender as a TObject

**)
procedure TfrmDUnit.rdoNewExistingProject(Sender: TObject);

Var
  i : Integer;

begin
  cbxExistingProject.Enabled := rdoExistingProject.Checked;
  edtNewProjectName.Enabled := rdoNewProject.Checked;
  rdoExistingUnit.Enabled := Not rdoNewProject.Checked;
  If rdoNewProject.Checked Then
    rdoNewUnit.Checked := True;
  If rdoNewProject.Checked Then
    rdoNewUnit.Checked := True;
  FDUnitCreator.GetExistingDUnitProjects;
  cbxExistingProject.Clear;
  For i := 0 To FDUnitCreator.ProjectCount - 1 Do
    cbxExistingProject.Items.Add(ExtractfileName(FDUnitCreator.Projects[i]));
  If cbxExistingProject.Items.Count > 0 Then
    cbxExistingProject.ItemIndex := 0;
end;

(**

  This is an on click event handler for the New and Existing Unit radio buttons.

  @precon  None.
  @postcon Rebuilds the units lists.

  @param   Sender as a TObject

**)
procedure TfrmDUnit.rdoNewExistingUnit(Sender: TObject);

Var
  i: Integer;

begin
  cbxExistingUnit.Enabled := rdoExistingUnit.Checked;
  edtNewUnitName.Enabled := rdoNewUnit.Checked;
  edtNewUnitName.Enabled := rdoNewUnit.Checked;
  FDUnitCreator.GetExistingDUnitUnits(cbxExistingProject.ItemIndex);
  cbxExistingUnit.Clear;
  For i := 0 To FDUnitCreator.UnitCount - 1 Do
    cbxExistingUnit.Items.Add(ExtractfileName(FDUnitCreator.Units[i]));
  If cbxExistingUnit.Items.Count > 0 Then
    cbxExistingUnit.ItemIndex := 0;
end;

(**

  This method is a recursive method to render an element and its child elements.

  @precon  RootNode and Container must be valid instance.
  @postcon Recursively renders elements and their children.

  @param   RootNode  as a PVirtualNode
  @param   Container as a TElementContainer

**)
procedure TfrmDUnit.RenderContainers(RootNode : PVirtualNode;
  Container: TElementContainer);

Var
  i : Integer;
  NewNode : PVirtualNode;

begin
  For i := 1 To Container.ElementCount Do
    If Container.Elements[i].Scope In [scPublic, scPublished, scNone, scGlobal] Then
      Begin
        NewNode := AddNode(RootNode, Container.Elements[i]);
        RenderContainers(NewNode, Container[i]);
      End;
end;

(**

  This method saves the dialogues settings to the INI file.

  @precon  None.
  @postcon Saves the dialogues settings to the INI file.

**)
procedure TfrmDUnit.SaveSettings;

Var
  i: Integer;

begin
  With TIniFile.Create(BrowseAndDocitOptions.IniFileName) Do
    Try
      WriteInteger('DUnitDlg', 'Top', Top);
      WriteInteger('DUnitDlg', 'Left', Left);
      WriteInteger('DUnitDlg', 'Height', Height);
      WriteInteger('DUnitDlg', 'Width', Width);
      For i := 0 To cbxBaseClass.Items.Count - 1 Do
        WriteString('DUnitBaseClasses', Format('BaseClass%d', [i]),
          cbxBaseClass.Items[i]);
      WriteString('DUnitDlg', 'BaseClass', cbxBaseClass.Text);
      WriteString('DUnitDlg', 'TestSuiteName', edtTestSuiteName.Text);
    Finally
      Free;
    End;
end;

(**

  This is an OnGetImageIndex for the virtual tree view.

  @precon  None.
  @postcon Returns the adjusted image index for scope for the associated module
           element.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as an Integer as a reference

**)
procedure TfrmDUnit.vstTestCasesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

Var
  NodeData : ^TTreeData;

begin
  NodeData := vstTestCases.GetNodeData(Node);
  ImageIndex := NodeData.Element.ImageIndexAdjustedForScope;
end;

(**

  This is an OnGetText event handler for the virtual tree view.

  @precon  None.
  @postcon Returns the AsString text of the associated module element.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a WideString as a reference

**)
procedure TfrmDUnit.vstTestCasesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

Var
  NodeData : ^TTreeData;

begin
  NodeData := vstTestCases.GetNodeData(Node);
  CellText := NodeData.Element.AsString(True, False);
end;

end.
