(**

  This module contains a class which represents a form for selecting and
  deselecting methods which need profiling.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Aug 2010

**)
unit ProfilingForm;

interface

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, VirtualTrees, BaseLanguageModule, ImgList, Contnrs,
  ExtCtrls;

type
  (** An enumerate to define if the profile job is an insertion or a removal. **)
  TProfileCodeType = (pctInsert, pctRemove);

  (** A class to describe a profile job. A profile jobs is the method name,
      line numbers and whether the profile information needs inserting or
      removing. **)
  TProfileJob = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMethod    : String;
    FCodeType  : TProfileCodeType;
    FStartLine : Integer;
    FEndLine   : Integer;
    FIndent    : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strMethod : String; CodeType : TProfileCodeType;
      iStartLine, iEndLine, iIndent : Integer);
    (**
      This property get the Method Name of the profile job.
      @precon  None.
      @postcon Returns the Method Name of the profile job.
      @return  a String
    **)
    Property Method : String Read FMethod;
    (**
      This property get the Code Type of the profile job.
      @precon  None.
      @postcon Returns the Code Type of the profile job.
      @return  a TProfileCodeType
    **)
    Property CodeType : TProfileCodeType Read FCodetype;
    (**
      This property get the Start Line of the profile job.
      @precon  None.
      @postcon Returns the Start Line of the profile job.
      @return  a Integer
    **)
    Property StartLine : Integer Read FStartLine;
    (**
      This property get the End Line of the profile job.
      @precon  None.
      @postcon Returns the End Line of the profile job.
      @return  a Integer
    **)
    Property EndLine : Integer Read FEndLine;
    (**
      This property get the Indent of the profile job.
      @precon  None.
      @postcon Returns the Indent of the profile job.
      @return  a Integer
    **)
    Property Indent : Integer Read FIndent;
  End;

  (** This class decsribes a collection of profile jobs. **)
  TProfileJobs = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FProfileJobs : TObjectList;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetCount : Integer;
    Function GetProfileJob(iIndex : Integer) : TProfileJob;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(strMethod : String; CodeType : TProfileCodeType;
      iStartLine, iEndLine, iIndent : Integer);
    (**
      This property returns the number of Profile Jobs in the collection.
      @precon  None.
      @postcon Returns the number of Profile Jobs in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      This property returns the indexed Profile Job from the collection.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon Returns the indexed Profile Job from the collection.
      @param   iIndex as an Integer
      @return  a TProfileJob
    **)
    Property ProfileJob[iIndex : Integer] : TProfileJob Read GetProfileJob;
  End;

  (** A class to represent the form interface. **)
  TfrmProfiling = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    vstTestCases: TVirtualStringTree;
    ilScopeImages: TImageList;
    mmoCode: TMemo;
    pnlPanel: TPanel;
    Splitter: TSplitter;
    procedure btnOKClick(Sender: TObject);
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
  private
    { Private declarations }
    FModule: PVirtualNode;
    FRootElement: TLabelContainer;
    Procedure InitialiseTreeView(M : TBaseLanguageModule);
    Function AddNode(P : PVirtualNode; Element : TElementContainer) : PVirtualNode;
    procedure RenderContainers(RootNode: PVirtualNode;
      Container: TElementContainer);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure ProcessJobs(Collection : TProfileJobs);
  public
    { Public declarations }
    Class Function Execute(Module : TBaseLanguageModule) : TProfileJobs;
  end;

implementation

Uses
  ToolsAPIUtils, PascalModule, IniFiles, dghlibrary;

resourcestring
  (** A resource string to signify a missing expansion point in the template. **)
  strProfilingTemplate = 'You profiling template code MUST contain a refe' +
    'rnce to %s.';

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
function TfrmProfiling.AddNode(P : PVirtualNode; Element : TElementContainer) : PVirtualNode;

Var
  NodeData : ^TTreeData;

begin
  Result := vstTestCases.AddChild(P);
  NodeData := vstTestCases.GetNodeData(Result);
  NodeData.Element := Element;
  Result.CheckType := ctTriStateCheckBox;
  If Nodedata.Element Is TGenericFunction Then
    If (NodeData.Element As TGenericFunction).HasProfiling Then
      vstTestCases.CheckState[Result] := csCheckedNormal;
end;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Checks that there are existing projects to work with.

  @param   Sender as a TObject

**)
procedure TfrmProfiling.btnOKClick(Sender: TObject);

begin
  If Not Like('*$METHODCODE$*', mmoCode.Lines.Text) Then
    Begin
      MessageDlg(Format(strProfilingTemplate, ['$METHODCODE$']), mtError, [mbOK], 0);
      ModalResult := mrNone;
    End Else
  If Not Like('*$METHODNAME$*', mmoCode.Lines.Text) Then
    Begin
      MessageDlg(Format(strProfilingTemplate, ['$METHODNAME$']), mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
end;

(**

  This is the classes main interface method(Singleton class).

  @precon  None.
  @postcon Creates an instance of the Singleton class .

  @param   Module as a TBaseLanguageModule
  @return  a TProfileJobs

**)
Class Function TfrmProfiling.Execute(Module : TBaseLanguageModule) : TProfileJobs;

Begin
  Result := Nil;
  With TfrmProfiling.Create(Nil) Do
    Try
      InitialiseTreeView(Module);
      mmoCode.Lines.Text := BrowseAndDocItOptions.ProfilingCode[Module.FileName];
      If ShowModal = mrOK Then
        Begin
          Result := TProfileJobs.Create;
          ProcessJobs(Result);
          BrowseAndDocItOptions.ProfilingCode[Module.FileName] := mmoCode.Lines.Text;
          SaveSettings;
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
procedure TfrmProfiling.FormCreate(Sender: TObject);

Type
  T = BaseLanguageModule.TImageIndex;

Var
  i : T;

begin
  LoadSettings;
  vstTestCases.NodeDataSize := SizeOf(TTreeData);
  vstTestCases.OnGetText := vstTestCasesGetText;
  ilScopeImages.Clear;
  For i := Succ(Low(T)) to High(T) Do
    If Not ilScopeImages.GetInstRes(hInstance, rtBitmap,
      ImageList[i].FResourceName, 16, [lrDefaultColor], ImageList[i].FMaskColour) Then
      ShowMessage(Format('Resource "%s" not found.', [ImageList[i].FResourceName]));
end;

(**

  This is an OnFormDestroy Event Hanlder for the TfrmDUnit class.

  @precon  None.
  @postcon Frees the memory used by the Root Element of the tree view.

  @param   Sender as a TObject

**)
procedure TfrmProfiling.FormDestroy(Sender: TObject);
begin
  FRootElement.Free;
end;

(**

  This method starts the rendering on testable elements in the module.

  @precon  None.
  @postcon Starts the rendering on testable elements in the module.

  @param   M as a TBaseLanguageModule

**)
procedure TfrmProfiling.InitialiseTreeView(M : TBaseLanguageModule);

Var
  I : TElementContainer;

begin
  FRootElement := TLabelContainer.Create(Format('Profilable Methods for %s', [
    M.AsString(True, False)]), scNone, 0, 0, iiNone, Nil);
  I := M.FindElement(strImplementedMethodsLabel);
  If I <> Nil Then
    Begin
      FModule := AddNode(Nil, FRootElement);
      RenderContainers(FModule, I);
      vstTestCases.Expanded[FModule] := True;
    End;
end;

(**

  This method loads the dialogues settings from the INI file.

  @precon  None.
  @postcon Loads the dialogues settings from the INI file.

**)
procedure TfrmProfiling.LoadSettings;

begin
  With TIniFile.Create(BrowseAndDocitOptions.IniFileName) Do
    Try
      Top := ReadInteger('DUnitDlg', 'Top', (Screen.Height - Height) Div 2);
      Left := ReadInteger('DUnitDlg', 'Left', (Screen.Width - Width) Div 2);
      Height := ReadInteger('DUnitDlg', 'Height', Height);
      Width := ReadInteger('DUnitDlg', 'Width', Width);
    Finally
      Free;
    End;
end;

(**

  This method recurses the tree looking for TGenericFunctions and building a list
  of profile jobs to be done.

  @precon  Collection must be a valid instance.
  @postcon Recurses the tree looking for TGenericFunctions and building a list
           of profile jobs to be done in Collection.

  @param   Collection as a TProfileJobs

**)
procedure TfrmProfiling.ProcessJobs(Collection: TProfileJobs);

  (**

    This procedure recurses the nodes in the tree view building a list of
    profiling that needs to be inserted or removed based on the current state
    of the profiling in the method and whether the node if checked or not.

    @precon  None.
    @postcon Recurses the nodes in the tree view building a list of
             profiling that needs to be inserted or removed.

    @param   Node as a PVirtualNode

  **)
  Procedure RecurseNodes(Node : PVirtualNode);

  Var
    N : PVirtualNode;
    NodeData : ^TTreeData;
    F: TGenericFunction;

  Begin
    N := Node.FirstChild;
    While N <> Nil Do
      Begin
        RecurseNodes(N);
        NodeData := vstTestCases.GetNodeData(N);
        If NodeData.Element Is TGenericFunction Then
          Begin
            F := NodeDAta.Element As TGenericFunction;
            If N.CheckState = csCheckedNormal Then
              If Not F.HasProfiling Then
                Collection.Add(F.QualifiedName, pctInsert, F.StartLine,
                  F.EndLine, F.Indent);
            If N.CheckState = csUncheckedNormal Then
              If F.HasProfiling Then
                Collection.Add(F.QualifiedName, pctRemove, F.StartLine,
                  F.EndLine, F.Indent);
          End;
        N := N.NextSibling;
      End;
  End;

begin
  RecurseNodes(vstTestCases.RootNode);
end;

(**

  This method is a recursive method to render an element and its child elements.

  @precon  RootNode and Container must be valid instance.
  @postcon Recursively renders elements and their children.

  @param   RootNode  as a PVirtualNode
  @param   Container as a TElementContainer

**)
procedure TfrmProfiling.RenderContainers(RootNode : PVirtualNode;
  Container: TElementContainer);

Var
  i : Integer;
  NewNode : PVirtualNode;

begin
  For i := 1 To Container.ElementCount Do
    If (Container.Elements[i] Is TGenericFunction) Or
      ((Container.Elements[i] Is TLabelContainer) And
      (Container.Elements[i].Scope In [scPublic, scGlobal, scNone])) Then
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
procedure TfrmProfiling.SaveSettings;

begin
  With TIniFile.Create(BrowseAndDocitOptions.IniFileName) Do
    Try
      WriteInteger('DUnitDlg', 'Top', Top);
      WriteInteger('DUnitDlg', 'Left', Left);
      WriteInteger('DUnitDlg', 'Height', Height);
      WriteInteger('DUnitDlg', 'Width', Width);
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
procedure TfrmProfiling.vstTestCasesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

Var
  NodeData : ^TTreeData;

begin
  If Column = 0 Then
    Begin
      NodeData := vstTestCases.GetNodeData(Node);
      ImageIndex := NodeData.Element.ImageIndexAdjustedForScope;
    End;
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
procedure TfrmProfiling.vstTestCasesGetText(Sender: TBaseVirtualTree;
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
  Case Column Of
    0: CellText := NodeData.Element.AsString(True, False);
  Else
    If NodeData.Element Is TGenericFunction Then
      Case Column Of
        1: CellText := IntToStr((NodeData.Element as TGenericFunction).StartLine);
        2: CellText := IntToStr((NodeData.Element as TGenericFunction).EndLine);
        3: CellText := IntToStr((NodeData.Element as TGenericFunction).LineofCode);
        4: CellText := BoolToStr((NodeData.Element as TGenericFunction).HasProfiling);
      End
    Else CellText := '';
  End;
end;

{ TProfileJob }

(**

  A constructor for the TProfileJob class.

  @precon  None.
  @postcon Creates an instance of a TProfileJob class.

  @param   strMethod  as a String
  @param   CodeType   as a TProfileCodeType
  @param   iStartLine as an Integer
  @param   iEndLine   as an Integer
  @param   iIndent    as an Integer

**)
constructor TProfileJob.Create(strMethod: String; CodeType: TProfileCodeType;
  iStartLine, iEndLine, iIndent: Integer);
begin
  FMethod := strMethod;
  FCodeType := codeType;
  FStartLine := iStartLine;
  FEndLine := iEndLine;
  FIndent := iIndent;
end;

{ TProfileJobs }

(**

  This is a compare function for the Profile Jobs collection to sort in
  descending order.

  @precon  None.
  @postcon Returns an integer to define the sort order.

  @param   ProfileJob1 as a Pointer
  @param   ProfileJob2 as a Pointer
  @return  an Integer

**)
Function SortProfileJobs(ProfileJob1, ProfileJob2 : Pointer) : Integer;

Begin
  Result := TProfileJob(ProfileJob2).StartLine - TProfileJob(ProfileJob1).StartLine;
End;

(**

  This method adds a profile job to the collection.

  @precon  None.
  @postcon Adds a profile job to the collection.

  @param   strMethod  as a String
  @param   CodeType   as a TProfileCodeType
  @param   iStartLine as an Integer
  @param   iEndLine   as an Integer
  @param   iIndent    as an Integer

**)
procedure TProfileJobs.Add(strMethod: String; CodeType: TProfileCodeType;
  iStartLine, iEndLine, iIndent: Integer);
begin
  FProfileJobs.Add(TProfileJob.Create(strMethod, CodeType, iStartLine, iEndLine,
    iIndent));
  FProfileJobs.Sort(SortProfileJobs);
end;

(**

  A constructor for the TProfileJobs class.

  @precon  None.
  @postcon Creates an empty collection.

**)
constructor TProfileJobs.Create;
begin
  FProfileJobs := TObjectList.Create(True);
end;

(**

  A destructor for the TProfileJobs class.

  @precon  None.
  @postcon Frees the collection.

**)
destructor TProfileJobs.Destroy;
begin
  FProfileJobs.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of the jobs in the profile collection.

  @return  an Integer

**)
function TProfileJobs.GetCount: Integer;
begin
  Result := FProfileJobs.Count;
end;

(**

  This is a getter method for the ProfileJob property.

  @precon  iIndex must be between 0 and Count - 1.
  @postcon Returns an instance of the indexed Profile Job.

  @param   iIndex as an Integer
  @return  a TProfileJob

**)
function TProfileJobs.GetProfileJob(iIndex: Integer): TProfileJob;
begin
  Result := FProfileJobs[iIndex] As TProfileJob;
end;

end.
