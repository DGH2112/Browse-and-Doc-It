(**

  This module defines a class which reprepsents a form for selecting
  DUnit Unit Tetsing Options.

  @Version 1.0
  @Author  David Hoyle
  @Date    09 Apr 2017

**)
unit BADI.DUnitForm;

interface

{$INCLUDE 'CompilerDefinitions.inc'}

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  BADI.DUnitCreator,
  VirtualTrees,
  BADI.Base.Module,
  ImgList,
  System.ImageList,
  ExtCtrls,
  BADI.ElementContainer;

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
    chkRemoveIAndTFromObject: TCheckBox;
    gpNameOptions: TGridPanel;
    lblClassName: TLabel;
    lblMethodName: TLabel;
    edtClassName: TEdit;
    edtMethodName: TEdit;
    procedure rdoNewExistingProject(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rdoNewExistingUnit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    {$IFNDEF D2009}
    procedure vstTestCasesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    {$ELSE}
    procedure vstTestCasesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    {$ENDIF}
    procedure vstTestCasesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure cbxExistingUnitChange(Sender: TObject);
  private
    { Private declarations }
    FDUnitCreator : TDUnitCreator;
    FModule: PVirtualNode;
    FRootElement: TLabelContainer;
    FTestCases : TStringList;
    FImplementedTests : TStringList;
    Procedure InitialiseTreeView;
    Function AddNode(P : PVirtualNode; Element : TElementContainer) : PVirtualNode;
    procedure RenderContainers(RootNode: PVirtualNode;
      Container: TElementContainer);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure BuildTestCaseList;
    Function AddUniqueName(slList : TStrings; Const strText : String) : String;
    Procedure ErrorProc(Const strMsg : String);
    Procedure CheckImplementedTests;
    Procedure UpdateImplementedTests;
    Function  CanRenderContainer(Element : TElementContainer) : Boolean;
    Function  NodeContainsMethods(Node : PVirtualNode) : Boolean;
    Function  MaskClassName(Const strText : String) : String;
    Function  MaskMethodName(Const strText : String) : String;
  public
    { Public declarations }
    Class Procedure Execute(objDUnitCreator : TDUnitCreator);
  end;

implementation

Uses
  //CodeSiteLogging,
  ToolsAPI,
  BADI.ToolsAPIUtils,
  BADI.Pascal.Module,
  IniFiles,
  dghlibrary,
  BADI.Generic.FunctionDecl,
  BADI.Types,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.Options,
  BADI.Pascal.RecordDecl,
  BADI.Pascal.MethodDecl,
  BADI.Pascal.PropertyDecl;

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
  @param   strText as a String as a constant
  @return  a String

**)
Function TfrmDUnit.AddUniqueName(slList: TStrings; Const strText: String) : String;

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
  strClassNameMaskMustHaveInsertionPoint = 'The class name mask must have an insert point (%s)!';
  strMethodNameMaskMustHaveInsertionPoint = 'The method name mask must have an insert point (%s)!';

begin
  If rdoExistingProject.Checked Then
    If cbxExistingProject.ItemIndex = -1 Then
      Begin
        MessageDlg(strThereAreNoExistingDUnitProjects, mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      End;
  If rdoNewProject.Checked Then
    If FDUnitCreator.DoesProjectExist(edtNewProjectName.Text) Then
      Begin
        MessageDlg(Format(strTheProjectNameExists, [edtNewProjectName.Text]),
          mtError, [mbOK], 0);
        edtNewProjectName.SetFocus;
        ModalResult := mrNone;
        Exit;
      End;
  If rdoExistingProject.Checked Then
    If rdoNewUnit.Checked Then
      If FDUnitCreator.DoesUnitExist(cbxExistingProject.ItemIndex,
        edtNewUnitName.Text) Then
        Begin
          MessageDlg(Format(strTheUnitNameExists, [edtNewUnitName.Text]), mtError,
            [mbOK], 0);
          edtNewUnitName.SetFocus;
          ModalResult := mrNone;
          Exit;
        End;
  If FModule.CheckState In [csUncheckedNormal] Then
    Begin
      MessageDlg(strMustSelectUnitTest, mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    End;
  If Pos('%s', LowerCase(edtClassName.Text)) = 0 Then
    Begin
      MessageDlg(strClassNameMaskMustHaveInsertionPoint, mtError, [mbOK], 0);
      edtClassName.SetFocus;
      ModalResult := mrNone;
      Exit;
    End;
  If Pos('%s', LowerCase(edtMethodName.Text)) = 0 Then
    Begin
      MessageDlg(strMethodNameMaskMustHaveInsertionPoint, mtError, [mbOK], 0);
      edtMethodName.SetFocus;
      ModalResult := mrNone;
      Exit;
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

begin
  vstTestCases.IterateSubtree(
    FModule,
    Procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; Var Abort: Boolean)

    Var
      NodeData         : ^TTreeData;
      M                : TGenericFunction;
      strQualifiedIdent: String;
      P                : TElementContainer;
      strTestCase      : String;
      iPos             : Integer;
      iIndex           : Integer;

    Begin
      NodeData := Sender.GetNodeData(Node);
      If NodeData.Element <> Nil Then
        If NodeData.Element Is TGenericFunction Then
          If Sender.CheckState[Node] = csCheckedNormal Then
            Begin
              M := NodeData.Element As TGenericFunction;
              strQualifiedIdent := '=' + M.QualifiedName;
              P := M.Parent; // Get class parent Cls > Methods > Method
              If P <> Nil Then
                P := P.Parent;
              If P Is TRecordDecl Then
                strQualifiedIdent := (P As TRecordDecl).Identifier + strQualifiedIdent;
              // Only add test cases not found in FImplementedTests.
              strTestCase := strQualifiedIdent;
              iPos := Pos('=', strTestCase);
              Case iPos Of
                1: strTestCase := MaskClassName('') + '=' +
                  MaskMethodName(Copy(strTestCase, 2, Length(strTestCase) - 1));
              Else
                strTestCase := MaskClassName(Copy(strTestCase, 1, iPos - 1)) + '=' +
                  MaskMethodName(Copy(strTestCase, iPos + 1, Length(strTestCase) - iPos));
              End;
              If Not FImplementedTests.Find(strTestCase, iIndex) Then
                FTestCases.Add(strTestCase);
            End;
    End,
    Nil
  );
end;

(**

  This method returns true if the passed element can be rendered in the Dunit treeview.

  @precon  Element must be a valid instance.
  @postcon returns true if the passed element can be rendered in the Dunit treeview (elements of
           type TLabelContainer with Identifiers of 'methods', 'properties' or 'types' or other
           elements which have publicly visible scope.).

  @param   Element as a TElementContainer
  @return  a Boolean

**)
Function TfrmDUnit.CanRenderContainer(Element: TElementContainer): Boolean;

Begin
  Result := Element.Scope In [scPublic, scPublished, scNone, scGlobal];
  If Element Is TLabelContainer Then
    Result := Result And IsKeyWord(Element.Identifier, ['methods', 'properties', 'types']);
End;

(**

  This is an on change event handler for the Existing Units combo control.

  @precon  None.
  @postcon Updates the checked status of the DUnit method to be created.

  @param   Sender as a TObject

**)
Procedure TfrmDUnit.cbxExistingUnitChange(Sender: TObject);

Begin
  CheckImplementedTests;
End;

(**

  This method parses the the selected existing DUnit and finds its implemented tests and stores
  them in a string list.

  @precon  None.
  @postcon The implemented test in the existing DUnit file are stored in a string list.

**)
Procedure TfrmDUnit.CheckImplementedTests;

Var
  iUnit : Integer;
  Module : TPascalModule;
  Unt : IOTAModule;
  Types, Methods : TElementContainer;
  iElements : Integer;
  RecDecl : TRecordDecl;
  iMethod : Integer;
  Method : TPascalMethod;

Begin
  FImplementedTests.Clear;
  FDUnitCreator.GetExistingDUnitUnits(cbxExistingProject.ItemIndex);
  For iUnit := 0 To FDUnitCreator.UnitCount - 1 Do
    If CompareText(ExtractFileName(FDUnitCreator.Units[iUnit]), cbxExistingUnit.Text) = 0  Then
      Begin
        Unt := (BorlandIDEServices As IOTAModuleServices).OpenModule(FDUnitCreator.Units[iUnit]);
        If Unt <> Nil Then
          Begin
            Module := TPascalModule.CreateParser(EditorAsString(SourceEditor(Unt)),
              cbxExistingUnit.Text, Unt.CurrentEditor.Modified, [moParse]);
            Try
              Types := Module.FindElement(strTypesLabel);
              If Types <> Nil Then
                For iElements := 1 To Types.ElementCount Do
                  If Types.Elements[iElements] Is TRecordDecl Then
                    Begin
                      RecDecl := Types.Elements[iElements] As TRecordDecl;
                      Methods := RecDecl.FindElement(strMethodsLabel);
                      If Methods <> Nil Then
                        For iMethod := 1 To Methods.ElementCount Do
                          If Methods.Elements[iMethod] Is TPascalMethod Then
                            Begin
                              Method := Methods.Elements[iMethod] As TPascalMethod;
                              If Method.Scope In [scPublished] Then
                                FImplementedTests.Add(Format('%s=%s', [RecDecl.Identifier,
                                  Method.Identifier]));
                            End;
                    End;
            Finally
              Module.Free;
            End;
          End;
        Break;
      End;
  UpdateImplementedTests;
End;

(**

  This is an error event hanlder for errors raised in the DUnitCreator module.

  @precon  None.
  @postcon Displays an error in a dialogue box.

  @param   strMsg as a String as a constant

**)
Procedure TfrmDUnit.ErrorProc(Const strMsg : String);

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
              edtTestSuiteName.Text, edtClassName.Text, edtMethodName.Text)
          Else
            objDUnitCreator.UpdateTestUnit(cbxExistingUnit.ItemIndex,
              strUnitToBeTested, FTestCases, cbxBaseClass.Text,
              edtTestSuiteName.Text, edtClassName.Text, edtMethodName.Text);
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
  T = TBADIImageIndex;

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
  vstTestCases.OnGetText := vstTestCasesGetText;
  ilScopeImages.Clear;
  For i := Succ(Low(T)) to High(T) Do
    If Not ilScopeImages.GetInstRes(hInstance, rtBitmap,
      BADIImageList[i].FResourceName, 16, [lrDefaultColor],
      BADIImageList[i].FMaskColour) Then
      ShowMessage(Format('Resource "%s" not found.', [BADIImageList[i].FResourceName]));
  strFileName := ChangeFileExt(ExtractFileName(ActiveProject.FileName), '') +
    'Tests.dpr';
  edtNewProjectName.Text := AddUniqueName(cbxExistingProject.Items, strFileName);
  strFileName := 'Test' + ExtractFileName(ActiveSourceEditor.FileName);
  edtNewUnitName.Text := AddUniqueName(cbxExistingUnit.Items, strFileName);
  FImplementedTests := TStringList.Create;
  FImplementedTests.Sorted := True;
end;

(**

  This is an OnFormDestroy Event Hanlder for the TfrmDUnit class.

  @precon  None.
  @postcon Frees the memory used by the Root Element of the tree view.

  @param   Sender as a TObject

**)
Procedure TfrmDUnit.FormDestroy(Sender: TObject);

Begin
  FImplementedTests.Free;
  FRootElement.Free;
  FTestCases.Free;
End;

(**

  This method starts the rendering on testable elements in the module.

  @precon  None.
  @postcon Starts the rendering on testable elements in the module.

**)
procedure TfrmDUnit.InitialiseTreeView;

ResourceString
  strNotAPascalModule = 'Can only create unit test for Pascal modules!';

Var
  M : TElementContainer;
  T: TElementContainer;

begin
  M := FDUnitCreator.Module;
  If M Is TPascalModule Then
    Begin
      FRootElement := TLabelContainer.Create(Format('Test Units for %s', [
        M.AsString(True, False)]), scNone, 0, 0, iiNone, Nil);
      T := M.FindElement(strTypesLabel);
      If T <> Nil Then
        Begin
          FModule := AddNode(Nil, FRootElement);
          RenderContainers(FModule, T);
          vstTestCases.Expanded[FModule] := True;
        End;
      T := M.FindElement(strExportedHeadingsLabel);
      If T <> Nil Then
        Begin
          If FModule = Nil Then
            FModule := AddNode(Nil, FRootElement);
          RenderContainers(FModule, T);
          vstTestCases.Expanded[FModule] := True;
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
  With TMemIniFile.Create(TBADIOptions.BADIOptions.IniFileName) Do
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
      edtClassName.Text := ReadString('DUnit Mask Options', 'ClassNameMask', 'Test%s');
      edtMethodName.Text := ReadString('DUnit Mask Options', 'MethodNameMask', 'Test%s');
      chkRemoveIAndTFromObject.Checked := ReadBool('DUnit Mask Options', 'RemoveObjectFirstLetter',
        False);
    Finally
      Free;
    End;
end;

(**

  This method returns the name of the class appropriately placed within the class name mask.

  @precon  None.
  @postcon Returns the name of the class appropriately placed within the class name mask.

  @param   strText as a String as a constant
  @return  a String

**)
Function TfrmDUnit.MaskClassName(Const strText: String): String;

Begin
  Result := strText;
  If (Length(Result) > 0) And (chkRemoveIAndTFromObject.Checked) And
     CharInSet(Result[1], ['i', 'I', 't', 'T']) Then
    Delete(Result, 1, 1);
  If Result = '' Then
    Result := 'Functions';
  If Pos('%s', LowerCase(edtClassName.Text)) > 0 Then
    Result := Format(edtClassName.Text, [Result]);
End;

(**

  This method returns the name of the method appropriately placed within the method name mask.

  @precon  None.
  @postcon Returns the name of the method appropriately placed within the method name mask.

  @param   strText as a String as a constant
  @return  a String

**)
Function TfrmDUnit.MaskMethodName(Const strText: String): String;

Begin
  Result := strText;
  If Pos('%s', LowerCase(edtMethodName.Text)) > 0 Then
    Result := Format(edtMethodName.Text, [Result]);
End;

(**

  This method returns true if the given node or any of its children are a method or property.

  @precon  Node must be a valid instance.
  @postcon Returns true if the given node or any of its children are a method or property.

  @param   Node as a PVirtualNode
  @return  a Boolean

**)
Function TfrmDUnit.NodeContainsMethods(Node: PVirtualNode): Boolean;

  (**

    This function returns true if the given node is either a TPascalMethod or a TPascalProperty.

    @precon  N must be a valid instance.
    @postcon Returns true if the given node is either a TPascalMethod or a TPascalProperty.

    @param   N as a PVirtualNode
    @return  a Boolean

  **)
  Function IsMethodOrProperty(N : PVirtualNode) : Boolean;

  Var
    ND : ^TTreeData;

  Begin
    ND := vstTestCases.GetNodeData(N);
    Result := (ND.Element Is TPascalMethod) Or (ND.Element Is TPascalProperty);
  End;

Var
  N: PVirtualNode;

Begin
  Result := IsMethodOrProperty(Node);
  If Result Then
    Exit;
  N := vstTestCases.GetFirstChild(Node);
  While N <> Nil Do
    Begin
      Result := IsMethodOrProperty(N);
      If Result Then
        Break;
      Result := NodeContainsMethods(N);
      If Result Then
        Break;
      N := vstTestCases.GetNextSibling(N);
    End;
End;

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
  FDUnitCreator.GetExistingDUnitUnits(cbxExistingProject.ItemIndex);
  cbxExistingUnit.Clear;
  For i := 0 To FDUnitCreator.UnitCount - 1 Do
    cbxExistingUnit.Items.Add(ExtractfileName(FDUnitCreator.Units[i]));
  If cbxExistingUnit.Items.Count > 0 Then
    cbxExistingUnit.ItemIndex := 0;
  i := cbxExistingUnit.Items.IndexOf(edtNewUnitName.Text);
  If i > -1 Then
    cbxExistingUnit.ItemIndex := i;
  FImplementedTests.Clear;
  If rdoExistingUnit.Checked Then
    CheckImplementedTests;
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
    If CanRenderContainer(Container.Elements[i]) Then
      Begin
        NewNode := AddNode(RootNode, Container.Elements[i]);
        RenderContainers(NewNode, Container[i]);
        If Not NodeContainsMethods(NewNode) Then
          vstTestCases.DeleteNode(NewNode);
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
  With TMemIniFile.Create(TBADIOptions.BADIOptions.IniFileName) Do
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
      WriteString('DUnit Mask Options', 'ClassNameMask', edtClassName.Text);
      WriteString('DUnit Mask Options', 'MethodNameMask', edtMethodName.Text);
      WriteBool('DUnit Mask Options', 'RemoveObjectFirstLetter', chkRemoveIAndTFromObject.Checked);
      UpdateFile;
    Finally
      Free;
    End;
end;

(**

  This method iterates through the tree view of methods and properties and checks those items that
  have already been implemented in the existing DUnit.

  @precon  None.
  @postcon Methods that have been implemented in the existing DUnit are checked in the treeview.

**)
Procedure TfrmDUnit.UpdateImplementedTests;

Var
  N: PVirtualNode;

Begin
  vstTestCases.IterateSubtree(
    FModule,
    Procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; Var Abort: Boolean)
    Begin
      vstTestCases.CheckState[Node] := csUncheckedNormal;
    End,
    Nil
  );
  N := vstTestCases.GetFirstChild(FModule);
  While N <> Nil Do
    Begin
      vstTestCases.Expanded[N] := False;
      N := vstTestCases.GetNextSibling(N);
    End;
  vstTestCases.IterateSubtree(
    FModule,
    Procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; Var Abort: Boolean)

    Var
      NodeData : ^TTreeData;
      strMethodName, strClassName, strTestCase : String;
      M : TGenericFunction;
      P : TElementContainer;
      iIndex : Integer;
      PN : PVirtualNode;

    Begin
      NodeData := Sender.GetNodeData(Node);
      If NodeData.Element <> Nil Then
        If NodeData.Element Is TGenericFunction Then
          Begin
            M := NodeData.Element As TGenericFunction;
            strMethodName := M.Identifier;
            P := M.Parent; // Get class parent Cls > Methods > Method
            If P <> Nil Then
              P := P.Parent;
            If P Is TRecordDecl Then
              strClassName := (P As TRecordDecl).Identifier;
            strTestCase := MaskClassName(strClassName) + '=' + MaskMethodName(strMethodName);
            If FImplementedTests.Find(strTestCase, iIndex) Then
              Begin
                vstTestCases.CheckState[Node] := csCheckedNormal;
                PN := Node.Parent;
                While (PN <> Nil) And (PN <> vstTestCases.RootNode) Do
                  Begin
                    vstTestCases.Expanded[PN] := True;
                    PN := PN.Parent;
                  End;
              End;
          End;
    End,
    Nil
  );
End;

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
  @param   CellText as a String as a reference

**)
procedure TfrmDUnit.vstTestCasesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
{$IFNDEF D2009}
  var CellText: WideString);
{$ELSE}
  var CellText: String);
{$ENDIF}

Var
  NodeData : ^TTreeData;

begin
  NodeData := vstTestCases.GetNodeData(Node);
  If NodeData.Element <> Nil Then
  CellText := NodeData.Element.AsString(True, False);
end;

end.
