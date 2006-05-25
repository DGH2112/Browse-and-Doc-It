(**

  This module contains a dockable form which will become the Module Explorer.

  @Author  David Hoyle
  @Date    25 May 2006
  @Version 1.0

**)
unit DockableModuleExplorer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DockForm, ActnList, IniFiles, ImgList, ComCtrls, StdCtrls, ExtCtrls,
  Contnrs, BaseLanguageModule;

type
  (** This class represents information about each nodes. Used in a collection
      instead of hanging the data off the tree nodes. **)
  TTreeNodeInfo = Class
  Private
    FLine : Integer;
    FCol : Integer;
    FComment : TComment;
  Public
    Constructor Create(iLine, iCol : Integer; Comment : TComment); Overload;
    Destructor Destroy; Override;
    (**
      This property returns the line number of the tree node comment.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      This property returns the column number of the tree node comment.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      This property returns the comment associated with the tree node info.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment;
  End;

  (** This record contains information about the special tag nodes **)
  TSpecialTagNode = Record
    Node : TTreeNode;
    strTagName : String;
    strTagDesc : String;
    boolShow : Boolean;
    boolExpand : Boolean;
  End;

  (** This is a custom hint window for displaying hints about the tree view
      items. Its customisation support the syntax highlighting custom draw
      support of the tree view. **)
  TCustomHintWindow = Class(THintWindow)
  Private
    FComment : TComment;
    FNodeLevel : Integer;
    FCustomDraw : Boolean;
  Public
    Procedure Paint; Override;
    Function CalcHintRect(MinWidth, MaxWidth : Integer; Node : TTreeNode;
      SyntaxHighlight : Boolean; Comment : TComment) : TRect; Reintroduce; Overload;
    Procedure ActivateHint(Rect : TRect; Node : TTreeNode;
      SyntaxHighlight : Boolean; Comment : TComment); Reintroduce; Overload;
    Function DrawSpecialTag(Comment : TComment; strSpecialTag : String) : Boolean;
  End;

  (** This is a procedure type for the positioning of the cursor in the
      current module. **)
  TSelectionChange = Procedure(iIdentLine, iIdentCol, iCommentLine,
    iCommentCol : Integer) Of Object;

  (** This class represents a dockable form that displays the heirarchical
      representation of the modules contents. **)
  TfrmDockableModuleExplorer = class(TDockableForm)
    stbStatusBar: TStatusBar;
    ilScopeImages: TImageList;
    pnlSplitter: TPanel;
    tvExplorer: TTreeView;
    procedure tvExplorerChange(Sender: TObject; Node: TTreeNode);
    procedure tvExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    FModule : TTreeNode;
    FOptions : TDocOptions;
    FNodeInfo : TObjectList;
    FSelectionChange : TSelectionChange;
    FUpdating : Boolean;
    FExpandedNodes : TStringList;
    FSpecialTagNodes : Array Of TSpecialTagNode;
    FHintWin : TCustomHintWindow;
    FLastNode : TTreeNode;
    procedure DisplayClause(U : TIdentList; P : TTreeNode; strLabel : String;
      iIcon, iLabelIcon : Integer); Overload;
    procedure DisplayClause(C : TGenericContainerCollection; P : TTreeNode;
      strLabel : String; iStartIcon, iLabelIcon : Integer); Overload;
    procedure DisplayProcs(M: TMethodCollection; P: TTreeNode;
      strLabel : String; boolImplementation : Boolean; iLabelIcon : Integer);
    procedure DisplayClasses(Classes : TGenericContainerCollection; P : TTreeNode;
      strNodeLabel, strClassName : String; iIconStart, iLabelIcon : Integer);
    procedure SetNodeIcon(Scope: TScope; iStartIndex: Integer;
      var Node: TTreeNode);
    { Private declarations }
    procedure GetBodyCommentTags(M : TBaseLanguageModule);
    Function AddNode(P : TTreeNode; strText : String; iLine, iCol,
      iImageIndex : Integer; Comment : TComment) : TTreeNode;
    function GetTreeNodeInfo(iIndex: Integer): TTreeNodeInfo;
    procedure CreateSpecialTagNodes;
    function IsInOptions(Scope: TScope): Boolean;
    Function GetNodeComment(iLine, iCol : Integer; C : TComment) : Integer;
    procedure ExpandNodes;
    procedure OutputModuleInfo(M : TBaseLanguageModule);
    procedure DisplayClassMethods(Cls : TObjectDecl; S : TTreeNode);
    procedure DisplayClassProperties(Cls : TClassDecl; S : TTreeNode);
    procedure DisplayClassFields(Cls : TRecordDecl; S : TTreeNode);
    Procedure DisplayErrors(M : TBaseLanguageModule);
    Procedure CMMouseLeave(var Msg : TMessage); Message CM_MOUSELEAVE;
    procedure RenderModule(M : TBaseLanguageModule; DocExplorerOptions : TDocOptions);
    Procedure DisplayDocumentConflicts(M : TBaseLanguageModule);
    (**
      This property returns the indexed NodeInfo class for the collection.
      @param   iIndex as an Integer
      @return  a TTreeNodeInfo
    **)
    Property NodeInfo[iIndex : Integer] : TTreeNodeInfo Read GetTreeNodeInfo;
    (**
      This is an event for the selection change in the browser tree.
      @return  a TSelectionChange
    **)
    Property OnSelectionChange : TSelectionChange Read FSelectionChange
      Write FSelectionChange;
    Procedure LoadSettings;
    Procedure SaveSettings;
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Procedure ShowDockableModuleExplorer;
    Class Procedure RemoveDockableModuleExplorer;
    Class Procedure CreateDockableModuleExplorer;
    Class Procedure RenderDocumentTree(BaseLanguageModule : TBaseLanguageModule;
      DocExplorerOptions : TDocOptions);
    Class Procedure HookEventHandlers(SelectionChangeProc : TSelectionChange);
  end;

  (** This is a classifier for the dockable form so that it can be registered
      with the IDE **)
  TfrmDockableModuleExplorerClass = Class of TfrmDockableModuleExplorer;

implementation

Uses
  DeskUtil, Registry;

Const
  (** Icon index for a plane folder. **)
  iFolder = 0;
  (** Icon index for an error folder. **)
  iErrorFolder = 1;
  (** Icon Index for a to do folder. **)
  iToDoFolder = 2;
  (** Icon index for the todo item. **)
  iToDoItem = 3;
  (** Icon index for the document conflict folder. **)
  iDocConflictFolder = 4;
  (** Icon index for the a document conflict item. **)
  iDocConflictItem = 5;
  (** Icon index for the a document conflict missing item. **)
  iDocConflictMissing = 6;
  (** Icon index for the a document conflict error item. **)
  iDocConflictError = 7;
  (** Icon index for the an error item. **)
  iError = 8;
  (** Icon index for a warning item. **)
  iWarning = 9;
  (** Icon index for the an unit item. **)
  iUnit = 10;
  (** Icon index for the class items. **)
  iClass = 11;
  (** Icon index for the field items. **)
  iField = 16;
  (** Icon index for the property items. **)
  iProperty = 21;
  (** Icon index for the procedure items. **)
  iProcedure = 26;
  (** Icon index for the function items. **)
  iFunction = 31;
  (** Icon index for the constructor items. **)
  iConstructor = 36;
  (** Icon index for the destructor items. **)
  iDestructor = 41;
  (** Icon index for the record items. **)
  iRecord = 46;
  (** Icon index for the interface items. **)
  iInterface = 51;
  (** Icon index for the object items. **)
  iObject = 56;
  (** Icon index for the type items. **)
  iTypes = 61;
  (** Icon index for the var items. **)
  iVars = 66;
  (** Icon index for the constant items. **)
  iConstants = 71;
  (** Icon index for the exported headings items. **)
  iExportHeadings = 76;
  (** Icon index for the field collection items. **)
  iFieldCollection = 81;
  (** Icon index for the property collection items. **)
  iPropertyCollection = 82;
  (** Icon index for the method collection items. **)
  iMethodCollection = 83;
  (** Icon index for the uses clause label **)
  iUnitLabel = 84;
  (** Icon index for the types clause label **)
  iTypeLabel = 85;
  (** Icon index for the class label **)
  iClassLabel = 86;
  (** Icon index for the interface label **)
  iInterfaceLabel = 87;
  (** Icon index for the object label **)
  iObjectLabel = 88;
  (** Icon index for the record label **)
  iRecordLabel = 89;
  (** Icon index for the constants clause label **)
  iConstantsLabel = 90;
  (** Icon index for the variables clause label **)
  iVarsLabel = 91;
  (** Icon index for the implemented methods label **)
  iImplementedMethodsLabel = 92;
  (** Icon index for the exported heading label **)
  iExportedHeadingsLabel = 93;
  (** Icon index for the exports clause label **)
  iExportsLabel = 94;
  (** Icon index for the Res String clause **)
  iResString = 95;
  (** Icon index for the Thread Var clause **)
  iThreadVar = 100;
  (** Icon index for the module image **)
  iModule = 105;
  (** Icon index for the constants clause label **)
  iResStringLabel = 106;
  (** Icon index for the variables clause label **)
  iThreadVarLabel = 107;
  (** Icon index for spacing **)
  iSpacer  = 108;
  (** Icon index for inheritance **)
  iInheritance = 109;
  (** Icon index for initialization sections **)
  iInitialization = 110;
  (** Icon index for Finalization sections **)
  iFinalization = 111;
  (** This is the maximum number of refreshes to keep expanded nodes for unless
      there counter has been reset by another appearance in a refresh. **)
  iMaxRefreshes = 50;

{$R *.dfm}

Var
  (** This is a private varaible to hold the singleton instance of the
      dockable form. **)
  FormInstance : TfrmDockableModuleExplorer;

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon   strText si the line of text to be tokenised
  @postcon  Returns a new string list of the tokenized string
  @note     The string list returnsed must be destroyed be the calling method.

  @param   strText as a String
  @param   strReservedWords as an Array Of String
  @param   strDirectives as an Array Of String
  @return  a TStringList

**)
Function Tokenize(strText : String; strReservedWords,
  strDirectives : Array Of String) : TStringList;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  (** Token buffer. **)
  strToken : String;
  CurToken : TTokenType;
  LastToken : TTokenType;
  BlockType : TBlockType;
  (** Token size **)
  iTokenLen : Integer;
  i : Integer;

Begin
  Result := TStringList.Create;
  BlockType := btNoBlock;
  strToken := '';
  CurToken := ttUnknown;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For i := 1 To Length(strText) Do
    Begin
      LastToken := CurToken;
      CurToken := GetTokenType(strText[i], LastToken);

      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If ((BlockType In [btStringLiteral]) And (CurToken <> ttLineEnd)) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := strText[i];
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  If IsKeyWord(strToken, strReservedWords) Then
                    LastToken := ttReservedWord;
                  If IsKeyWord(strToken, strDirectives) Then
                    LastToken := ttDirective;
                  Result.AddObject(strToken, TObject(LastToken));
                End;
             BlockType := btNoBlock;
             iTokenLen := 1;
             SetLength(strToken, iTokenCapacity);
             strToken[iTokenLen] := strText[i];
            End;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strText[i];
        End;

      // Check for string literals
      If CurToken = ttStringLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If IsKeyWord(strToken, strReservedWords) Then
        CurToken := ttReservedWord;
      If IsKeyWord(strToken, strDirectives) Then
        CurToken := ttDirective;
      Result.AddObject(strToken, TObject(CurToken));
    End;
End;

(**

  This procedure sets the font of the passed canvas to the appropiate style and
  colour for the words stored in the string list.

  @precon  sl is a string list of the tokenized word to display, i is the index
           of the word to change the canvas for, Level is the current
           indentation level of the tree node and Canvas is the canvas to be
           affected but the other parameters.
  @postcon Sets the font of the passed canvas to the appropiate style and
           colour for the words stored in the string list.

  @param   sl     as a TStringList
  @param   i      as an Integer
  @param   Level  as an Integer
  @param   Canvas as a TCanvas

**)
Procedure GetFontInfo(sl : TStringList; i : Integer; Level : Integer; Canvas : TCanvas);

Begin
  With Canvas Do
    Begin
      Refresh;
      If Level <> 1 Then
        Begin
          Font.Color := clNavy;
          Font.Style := [];
          Case TTokenType(sl.Objects[i]) Of
            ttReservedWord, ttDirective :
              Begin
                Font.Style := [fsBold];
                Font.Color := clBlack;
              End;
            ttSymbol, ttNumber :
              Begin
                Font.Style := [fsBold];
                Font.Color := clGreen;
              End;
            ttStringLiteral :
              Begin
                Font.Style := [fsBold];
                Font.Color := clTeal;
              End;
          End;
        End Else
        Begin
          Font.Color := clMaroon;
          Font.Style := [fsBold];
        End;
   End;
End;

(**

  This procedure makes the dockable module explorer visible.

  @precon  None.
  @postcon Makes the dockable module explorer visible.

  @param   Form as a TfrmDockableModuleExplorer

**)
Procedure ShowDockableForm(Form : TfrmDockableModuleExplorer);

Begin
  If Not Assigned(Form) Then
    Exit;
  If Not Form.Floating Then
    Begin
      Form.ForceShow;
      FocusWindow(Form);
    End Else
      Form.Show;
End;

(**

  This procedure registers the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is registered with the IDE.

  @param   FormClass as a TfrmDockableModuleExplorerClass
  @param   FormVar   as   @param   FormName  as a String constant

**)
Procedure RegisterDockableForm(FormClass : TfrmDockableModuleExplorerClass;
  var FormVar; Const FormName : String);

Begin
  If @RegisterFieldAddress <> Nil Then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
End;

(**

  This method unregisters the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is unregistered with the IDE.

  @param   FormVar  as   @param   FormName as a String constant

  @bug     Procedure not rendered in explorer tree properly.

**)
Procedure UnRegisterDockableForm(var FormVar; Const FormName : String);
Begin
  If @UnRegisterFieldAddress <> Nil Then
    UnregisterFieldAddress(@FormVar);
End;

(**

  This procedure creates an instance of the dockable form.

  @precon  FormVar is the instance reference and FormCass is the type of class
           to be created..
  @postcon The form instance is created.

  @param   FormVar   as a TfrmDockableModuleExplorer as a reference
  @param   FormClass as a TfrmDockableModuleExplorerClass

**)
Procedure CreateDockableForm(var FormVar : TfrmDockableModuleExplorer;
  FormClass : TfrmDockableModuleExplorerClass);
Begin
  TCustomForm(FormVar) := FormClass.Create(Nil);
  RegisterDockableform(FormClass, FormVar, TCustomForm(FormVar).Name);
End;

(**

  This procedure frees the instance of the dockable form.

  @precon  None.
  @postcon Free the instance of the dockable form.

  @param   FormVar as a TfrmDockableModuleExplorer as a reference

**)
Procedure FreeDockableForm(var FormVar : TfrmDockableModuleExplorer);
Begin
  If Assigned(FormVar) Then
    Begin
      UnRegisterDockableForm(FormVar, FormVar.Name);
      FreeAndNil(FormVar);
    End;
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment
           and This comment to be added to the node info object.
  @postcon Initialises the class.

  @param   iLine   as an Integer
  @param   iCol    as an Integer
  @param   Comment as a TComment

**)
Constructor TTreeNodeInfo.Create(iLine, iCol : Integer; Comment : TComment);

Begin
  Inherited Create;
  FLine := ILine;
  FCol := iCol;
  FComment := Nil;
  If Comment <> Nil Then
    FComment := TComment.Create('', Comment.Line, Comment.Col)
  Else
    FComment := TComment.Create('', 0, 0);
  FComment.Assign(Comment);
End;

(**

  This is the destructor method for the TTreeNodeInfo class.

  @precon  None.
  @postcon Destroys the instance of the Tree node info class.

**)
Destructor TTreeNodeInfo.Destroy;

Begin
  FComment.Free;
  Inherited Destroy;
End;

(**

  This is the constructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Initialises the class.

  @param   AOwner as a TComponent

**)
Constructor TfrmDockableModuleExplorer.Create(AOwner: TComponent);
begin
  Inherited;
  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  FNodeInfo := TObjectList.Create(True);
  FExpandedNodes := TStringList.Create;
  FExpandedNodes.Sorted := True;
  FExpandedNodes.Duplicates := dupIgnore;
  LoadSettings;
  FHintWin := TCustomHintWindow.Create(Self);
  FHintWin.Color := clInfoBk;
  FHintWin.Canvas.Font.Assign(tvExplorer.Font);
end;

(**

  This is a class method to create the dockable form instance.

  @precon  None.
  @postcon The form instance is created if one is not already present.

**)
class procedure TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
begin
  If Not Assigned(FormInstance) Then
    CreateDockableForm(FormInstance, TfrmDockableModuleExplorer);
end;

(**

  This is the destructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon destroy the instance of the dockable form.

**)
Destructor TfrmDockableModuleExplorer.Destroy;
begin
  FHintWin.Free;
  SaveSettings;
  FExpandedNodes.Free;
  FNodeInfo.Free;
  SaveStateNecessary := True;
  Inherited;
end;

(**

  This is a class method to remove the dockable form.

  @precon  None.
  @postcon Removes the instance of the dockable form.

**)
class procedure TfrmDockableModuleExplorer.RemoveDockableModuleExplorer;
begin
  FreeDockableForm(FormInstance);
end;

(**

  This method is a class method for displaying the dockable form. If the form
  does not already exist it is created first.

  @precon  None.
  @postcon Displays the dockable form.

**)
class procedure TfrmDockableModuleExplorer.ShowDockableModuleExplorer;
begin
  CreateDockableModuleExplorer;
  ShowDockableForm(FormInstance);
end;

(**

  This method is a class method to all a calling class to render the given
  module in the Dockable Module Explorer.

  @precon  None.
  @postcon If the form module explorer exists then the passed module is
           rendered.

  @param   BaseLanguageModule    as a TBaseLanguageModule
  @param   DocExplorerOptions as a TDocOptions

**)
class procedure TfrmDockableModuleExplorer.RenderDocumentTree(
  BaseLanguageModule: TBaseLanguageModule; DocExplorerOptions : TDocOptions);
begin
  If Assigned(FormInstance) Then
    If FormInstance.Visible Then
      FormInstance.RenderModule(BaseLanguageModule, DocExplorerOptions);
end;

(**

  This method gets the comment for the supplied comment and extracts todo
  comment as the same time.

  @precon  iLine is the line of the comment in the module, iCol is the column of
           the comment in the module and C is a valid comment or nil to be
           created and associated with a node.
  @postcon Returns the index of the newly created node comment.

  @param   iLine as an Integer
  @param   iCol  as an Integer
  @param   C     as a TComment
  @return  an Integer

**)
Function TfrmDockableModuleExplorer.GetNodeComment(iLine, iCol : Integer;
  C : TComment) : Integer;

Var
  i, j : Integer;
  info : TTreeNodeInfo;

Begin
  info := TTreeNodeInfo.Create(iLine, iCol, C);
  Result := FNodeInfo.Add(info);
  If C <>  Nil Then
    For i := 0 To C.TagCount - 1 Do
      For j := Low(FSpecialTagNodes) To High(FSpecialTagNodes) Do
        If FSpecialTagNodes[j].boolShow Then
          If AnsiCompareText(C.Tag[i].TagName, FSpecialTagNodes[j].strTagName) = 0 Then
            AddNode(FSpecialTagNodes[j].Node, C.Tag[i].AsString(False),
              C.Tag[i].Line, C.Tag[i].Column, iToDoItem, Nil);
End;

(**

  This method dislpays the given clause as children of the given tree node.

  @precon  U is a identification list to be displayed in the tree, P is the
           parent node for all the items in the list, strLabel is the label for
           a category node between the parent and the list, iIcon is an index
           for the items icons and iLabelIcon is the index of the icon to be
           used for the clause label
  @postcon Displays the TIdent clause given.

  @param   U          as a TIdentList
  @param   P          as a TTreeNode
  @param   strLabel   as a String
  @param   iIcon      as an Integer
  @param   iLabelIcon as an Integer

**)
procedure TfrmDockableModuleExplorer.DisplayClause(U : TIdentList; P : TTreeNode;
  strLabel : String; iIcon, iLabelIcon : Integer);

Var
  N : TTreeNode;
  i : Integer;

Begin
  // Uses Clause
  If U <> Nil Then
    Begin
      N := AddNode(P, strLabel, 0, 0, iLabelIcon, U.Comment);
      For i := 0 To U.Count - 1 Do
        AddNode(N, U[i].Ident, U[i].Line, U[i].Col, iIcon, U[i].Comment);
      If N.Count = 0 Then
        N.Delete;
    End;
End;

(**

  This method displays the given clause as children if the given tree node.

  @precon  C is a valid generic container collection to be displayed in the
           tree, P is the parent tree node to attach the items to, strLabel is a
           label for the intermediate node between the parent and the items,
           iStartIcon is the starting icon index for the items and iStartIcon is
           the icon index for the collection label.
  @postcon Displays the given clause infromation.

  @param   C          as a TGenericContainerCollection
  @param   P          as a TTreeNode
  @param   strLabel   as a String
  @param   iStartIcon as an Integer
  @param   iLabelIcon as an Integer

**)
procedure TfrmDockableModuleExplorer.DisplayClause(C : TGenericContainerCollection;
  P : TTreeNode; strLabel : String; iStartIcon, iLabelIcon : Integer);

Var
  N, Item : TTreeNode;
  i : Integer;

Begin
  // Const Clause
  If C.Count > 0 Then
    Begin
      If strLabel <> '' Then
        N := Addnode(P, strLabel, 0, 0, iLabelIcon, C.Comment)
      Else
        N := P;
      For i := 0 To C.Count - 1 Do
        // Display all types that are not records, objects, classes, and interfaces
        If Not (C[i] Is TRecordDecl) Then
          If IsInOptions(C[i].Scope) Then
            Begin
              Item := AddNode(N, C[i].Identifier + #32 + C[i].AsString(True),
                C[i].Line, C[i].Col, -1, C[i].Comment);
              SetNodeIcon(C[i].Scope, iStartIcon, Item);
            End;
      If (strLabel <> '') And (N.Count = 0) Then
        N.Delete;
    End;
End;

(**

  This method renders the documentation conflicts for the given module.

  @precon  M must be a valid Pascal Doc Module.
  @postcon renders the docuemntation conflicts for the given module.

  @param   M as a TBaseLanguageModule

**)
procedure TfrmDockableModuleExplorer.DisplayDocumentConflicts(
  M: TBaseLanguageModule);

Var
  i : Integer;
  DocConflictNode : TTreeNode;
  strLastCategory : String;
  InsertionNode : TTreeNode;
  iImage : Integer;
  ConflictComment : TComment;

begin
  DocConflictNode := Nil;
  InsertionNode := Nil;
  For i := 0 To M.DocumentConflictCount - 1 Do
    Begin
      If DocConflictNode = Nil Then
        DocConflictNode := AddNode(FModule, strDocumentationConflicts, 0, 0,
          iDocConflictFolder, Nil);
      If AnsiCompareText(strLastCategory, M.DocumentConflict[i].Category) <> 0 Then
        Begin
          InsertionNode := AddNode(DocConflictNode,
            M.DocumentConflict[i].Category, 0, 0, iDocConflictFolder, Nil);
          strLastCategory := M.DocumentConflict[i].Category;
        End;
      Case M.DocumentConflict[i].DocConflictType Of
        dctModuleMissingDate,
        dctModuleMissingVersion,
        dctTypeClauseUndocumented,
        dctConstantClauseUndocumented,
        dctResourceStringClauseUndocumented,
        dctVariableClauseUndocumented,
        dctThreadVarClauseUndocumented,
        dctRecordClauseUndocumented,
        dctObjectClauseUndocumented,
        dctClassClauseUndocumented,
        dctInterfaceClauseUndocumented,
        dctDispinterfaceClauseUndocumented,
        dctMethodUndocumented,
        dctMethodHasNoDesc,
        dctMethodPostconNotDocumented,
        dctPropertyUndocumented,
        dctPropertyHasNoDesc,
        dctPropertyPreconNotDocumented,
        dctPropertyPostconNotDocumented
          : iImage := iDocConflictItem;
        dctModuleMissingDocumentation,
        dctModuleMissingAuthor,
        dctMethodUndocumentedParam,
        dctMethodUndocumentedReturn,
        dctMethodPreconNotDocumented,
        dctMethodMissingPrecon,
        dctMethodMissingPostcon,
        dctPropertyUndocumentedParam,
        dctPropertyUndocumentedReturn,
        dctPropertyMissingPrecon,
        dctPropertyMissingPostcon
          : iImage := iDocConflictMissing;
        dctModuleIncorrectDate,
        dctModuleCheckDateError,
        dctMethodDiffParamCount,
        dctMethodIncorrectParamType,
        dctMethodIncorrectReturnType,
        dctMethodTooManyPrecons,
        dctMethodTooManyPostcons,
        dctPropertyDiffParamCount,
        dctPropertyIncorrectParamType,
        dctPropertyIncorrectReturnType,
        dctPropertyTooManyPrecons,
        dctPropertyTooManyPostcons
          : iImage := iDocConflictError;
      Else
        iImage := iDocConflictError;
      End;
      ConflictComment := TComment.Create(M.DocumentConflict[i].Description,
        M.DocumentConflict[i].CommentLine,
        M.DocumentConflict[i].CommentColumn);
      Try
        AddNode(InsertionNode, M.DocumentConflict[i].Message,
          M.DocumentConflict[i].IdentLine, M.DocumentConflict[i].IdentColumn,
          iImage, ConflictComment);
      Finally
        ConflictComment.Free;
      End;
    End;
  If (DocConflictNode <> Nil) And (doShowConflicts In FOptions) Then
    DocConflictNode.Expand(True);
end;

(**

  This method returns true if the given scope is in the options set.

  @precon Scope is the scope if the item to be tested.
  @postcon Return true if the scope if is in the options.

  @param   Scope as a TScope
  @return  a Boolean

**)
Function TfrmDockableModuleExplorer.IsInOptions(Scope : TScope) : Boolean;

Begin
  Result := False;
  Case Scope Of
    scLocal     : If doShowLocals In FOptions Then Result := True;
    scPrivate   : If doShowPrivates In FOptions Then Result := True;
    scProtected : If doShowProtecteds In FOptions Then Result := True;
    scPublic    : If doShowPublics In FOptions Then Result := True;
    scPublished : If doShowPublisheds In FOptions Then Result := True;
  End;
End;

(**

  This method loads the managed expanded nodes list from the registry.

  @precon  None.
  @postcon Loads the managed expanded nodes list from the registry.

**)
procedure TfrmDockableModuleExplorer.LoadSettings;

Var
  sl :  TStringList;
  i : Integer;
  iValue : Integer;

begin
  With TRegIniFile.Create() Do
    Try
      sl := TStringList.Create;
      Try
        ReadSection(strRegRootKey + 'ManagedExpandedNodes', sl);
      Finally
        For i := 0 To sl.Count - 1 Do
          Begin
            iValue := ReadInteger(strRegRootKey + 'ManagedExpandedNodes', sl[i], 0);
            FExpandedNodes.AddObject(sl[i], TObject(iValue));
          End;
      End;
    Finally
      Free;
    End;
end;

(**

  This method displays the methods of the given method collection as children of
  the passed tree node. If also recursively called itself and other routines to
  display local method, types and vars, etc...

  @precon  M is a method collection to be displayed in the tree, P is the parent
           node to display the items under, strLabel is the label of an
           intermediate item between the parent and the methods,
           boolImplementation indicates whether the methods are in the
           imlpementation section or not and iLabelIcon is the icon to be used
           for the collection label.
  @postcon Displays the procedures in the collection.

  @param   M                  as a TMethodCollection
  @param   P                  as a TTreeNode
  @param   strLabel           as a String
  @param   boolImplementation as a Boolean
  @param   iLabelIcon         as an Integer

**)
procedure TfrmDockableModuleExplorer.DisplayProcs(M : TMethodCollection; P : TTreeNode;
  strLabel : String; boolImplementation : Boolean; iLabelIcon : Integer);

Var
  N, F : TTreeNode;
  i : Integer;
  C : TTreeNode;
  strLastClassName : String;

Begin
  // Get Functions / Procedures
  strLastClassName := '';
  If M.Count > 0 Then
    Begin
      If strLabel <> '' Then
        N := AddNode(P, strLabel, 0, 0, iLabelIcon, Nil)
      Else
        N := P;
      C := N;
      For i := 0 To M.Count - 1 Do
        If IsInOptions(M[i].Scope) Or ((M[i].Scope = scLocal) And
          (doShowLocalProcs In FOptions)) Then
          Begin
            If strLastClassName <> M[i].ClsName Then
              Begin
                If M[i].ClsName <> '' Then
                  Begin
                    C := AddNode(N, M[i].ClsName, 0, 0, -1, Nil);
                    SetNodeIcon(scPublic, iClass, C);
                  End Else
                    C := N;
                strLastClassName := M[i].ClsName;
              End;
            F := AddNode(C, M[i].GetAsString(False, False), M[i].Line, M[i].Col,
              -1, M[i].Comment);
            Case M[i].MethodType Of
              mtProcedure : SetNodeIcon(M[i].Scope, iProcedure, F);
              mtFunction : SetNodeIcon(M[i].Scope, iFunction, F);
              mtConstructor : SetNodeIcon(M[i].Scope, iConstructor, F);
              mtDestructor : SetNodeIcon(M[i].Scope, iDestructor, F);
            End;
            DisplayClasses(M[i].Types, F, '', 'TRecordDecl', iRecord, iRecordLabel);
            DisplayClause(M[i].Types, F, '', iTypes, iTypeLabel);
            DisplayClause(M[i].Vars, F, '', iVars, iVarsLabel);
            DisplayClause(M[i].Consts, F, '', iConstants, iConstantsLabel);
            DisplayClause(M[i].ResStrings, F, '', iConstants, iConstantsLabel);
            DisplayProcs(M[i].LocalMethods, F, '', True, iImplementedMethodsLabel);
          End;
      If (strLabel <> '') And (N.Count = 0) Then
        N.Delete;
    End;
End;

(**

  This method saves the managed expand nodess list to the registry.

  @precon  None.
  @postcon Saves the managed expand nodess list to the registry.

**)
procedure TfrmDockableModuleExplorer.SaveSettings;

Var
  i : Integer;

begin
  With TRegIniFile.Create() Do
    Try
      For i := 0 To FExpandedNodes.Count - 1 Do
        WriteInteger(strRegRootKey + 'ManagedExpandedNodes', FExpandedNodes[i],
          Integer(FExpandedNodes.Objects[i]));
    Finally
      Free;
    End;
end;

(**

  This method for the NodeIcon property.

  @precon  Scope is the scope if the node icon to set, iStartIndex is the start
           index of the items icon set and Node is the node to assign the icons
           too.
  @postcon Sets the tree nodes image based on the starting index and the scope.

  @param   Scope       as a TScope
  @param   iStartIndex as an Integer
  @param   Node        as a TTreeNode as a reference

**)
Procedure TfrmDockableModuleExplorer.SetNodeIcon(Scope : TScope;
  iStartIndex : Integer; var Node : TTreeNode);

  (**

    This procedure sets the nodes image and select index to the value passed.

    @precon  i is the icon index to be set.
    @postcon Sets the image indexes of the passed tree node.

    @param   i as an Integer

  **)
  procedure SetIndex(i : Integer);

  begin
    Node.ImageIndex := i;
    Node.SelectedIndex := i;
  end;

Begin
  Case Scope Of
    scPublic, scGlobal : SetIndex(iStartIndex);
    scPrivate          : SetIndex(iStartIndex + 1);
    scPublished        : SetIndex(iStartIndex + 2);
    scProtected        : SetIndex(iStartIndex + 3);
    scLocal            : SetIndex(iStartIndex + 4);
  End;
End;

(**

  This method displays the fields, properties and method of the classes in the
  supplied class collection as children of the given tree node.

  @precon  Classes is a generic container collection of class like items to be
           displayed, P is the parent node to display the items under,
           strNodeLabel is the label of an intermediate node between the parent
           and the items to be displayed, strClassName is the name of the class
           of items to be displayed, iIconStart is the starting icon for the
           items to be displayed and iLabelIcon is the icon to use on the
           contain label.
  @postcon Displays the classes in the container.

  @param   Classes      as a TGenericContainerCollection
  @param   P            as a TTreeNode
  @param   strNodeLabel as a String - Node label
  @param   strClassName as a String - Class name of type to display
  @param   iIconStart   as an Integer - Start point of scope icons
  @param   iLabelIcon   as an Integer

**)
procedure TfrmDockableModuleExplorer.DisplayClasses(Classes : TGenericContainerCollection;
  P : TTreeNode; strNodeLabel, strClassName : String;
  iIconStart, iLabelIcon : Integer);

Var
  N, S : TTreeNode;
  i : Integer;

Begin
  // Get Functions / Procedures
  If Classes.Count > 0 Then
    Begin
      If strNodeLabel <> '' Then
        N := AddNode(P, strNodeLabel, 0, 0, iLabelIcon, Nil)
      Else
        N := P;
      For i := 0 To Classes.Count - 1 Do
        If Classes[i].ClassNameIs(strClassName) Then
          Begin
            If IsInOptions(Classes[i].Scope) Then
              Begin
                S := AddNode(N, Classes[i].AsString(True), Classes[i].Line,
                  Classes[i].Col, -1, Classes[i].Comment);
                SetNodeIcon(Classes[i].Scope, iIconStart, S);
                If Classes[i] Is TRecordDecl Then
                  DisplayClassFields(Classes[i] As TRecordDecl, S);
                If Classes[i] Is TClassDecl Then
                  DisplayClassProperties(Classes[i] As TClassDecl, S);
                If Classes[i] Is TObjectDecl Then
                  DisplayClassMethods(Classes[i] As TObjectDecl, S);
            End;
        End;
      If strNodeLabel <> '' Then
        If N.Count = 0 Then
          N.Delete;
    End;
End;

(**

  This method displays the give classes fields as children of the give tree
  node.

  @precon  Cls is a record whos fields require displaying and S is the tree node
           to attach the fields too.
  @postcon Display the class fields.

  @param   Cls as a TRecordDecl
  @param   S   as a TTreeNode

**)
procedure TfrmDockableModuleExplorer.DisplayClassFields(Cls : TRecordDecl; S : TTreeNode);

Var
  F : TTreeNode;
  j : Integer;

begin
  If Cls.ParameterCount > 0 Then
    Begin
      For j := 0 To Cls.ParameterCount - 1 Do
        If IsInOptions(Cls.Parameter[j].Scope) Then
          Begin
            F := AddNode(S, Cls.Parameter[j].Identifier + ' : ' +
              Cls.Parameter[j].ParamType.AsString(True), Cls.Parameter[j].Line,
                Cls.Parameter[j].Col, -1, Cls.Parameter[j].Comment);
            SetNodeIcon(Cls.Parameter[j].Scope, iField, F);
          End;
    End;
end;


(**

  This method displays the give classes properties as children of the given
  tree node.

  @precon  Cls is a class whos properties require displaying and S is the tree
           node to attach the fields too.
  @postcon This method displays the class properties.

  @param   Cls as a TClassDecl
  @param   S   as a TTreeNode

**)
procedure TfrmDockableModuleExplorer.DisplayClassProperties(Cls : TClassDecl;
  S : TTreeNode);

Var
  F : TTreeNode;
  j : Integer;

begin
  If Cls.PropertyCount > 0 Then
    Begin
      For j := 0 To Cls.PropertyCount - 1 Do
        If IsInOptions(Cls.Properties[j].Scope) Then
          Begin
            F := AddNode(S, Cls.Properties[j].AsString, Cls.Properties[j].Line,
              Cls.Properties[j].Col, -1, Cls.Properties[j].Comment);
            SetNodeIcon(Cls.Properties[j].Scope, iProperty, F);
          End;
    End;
end;


(**

  This method displays the give classes method as children to the supplied
  treenode.

  @precon  Cls is an object whos methods require displaying and S is the tree
           node to attach the fields too.
  @postcon Displays the Class methods.

  @param   Cls as a TObjectDecl
  @param   S   as a TTreeNode

**)
procedure TfrmDockableModuleExplorer.DisplayClassMethods(Cls : TObjectDecl;
  S : TTreeNode);

Var
  F : TTreeNode;
  j : Integer;

begin
  If Cls.MethodCount > 0 Then
    Begin
      For j := 0 To Cls.MethodCount - 1 Do
        If IsInOptions(Cls.Method[j].Scope) Then
          Begin
            F := AddNode(S, Cls.Method[j].GetAsString(True, False),
              Cls.Method[j].Line, Cls.Method[j].Col, -1,
              Cls.Method[j].Comment);
            Case Cls.Method[j].MethodType Of
              mtProcedure : SetNodeIcon(Cls.Method[j].Scope, iProcedure, F);
              mtFunction : SetNodeIcon(Cls.Method[j].Scope, iFunction, F);
              mtConstructor : SetNodeIcon(Cls.Method[j].Scope, iConstructor, F);
              mtDestructor : SetNodeIcon(Cls.Method[j].Scope, iDestructor, F);
            End;
          End
    End;

end;


(**

  This method retrieves comments from the body comment collection and adds them
  to the comment node of the tree view.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon This method cycles through the body comments extracting special tags.

  @param   M as a TBaseLanguageModule

**)
procedure TfrmDockableModuleExplorer.GetBodyCommentTags(M : TBaseLanguageModule);

Var
  i, j, k : Integer;

Begin
  For i := 0 To M.BodyCommentCount - 1 Do
    With M.BodyComment[i] Do
      For j := 0 To TagCount - 1 Do
        For k := Low(FSpecialTagNodes) To High(FSpecialTagNodes) Do
          If FSpecialTagNodes[k].boolShow Then
            If AnsiCompareText(Tag[j].TagName, FSpecialTagNodes[k].strTagName) = 0 Then
              AddNode(FSpecialTagNodes[k].Node, Tag[j].AsString(False),
                M.BodyComment[i].Tag[j].Line, M.BodyComment[i].Tag[j].Column,
                  iToDoItem, Nil);
End;

(**

  This method displays the specified module in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed and
           strStatus is a text string to be displayed in the forms status bar.
  @postcon Renders the module information for the given module.

  @param   M                  as a TBaseLanguageModule
  @param   docExplorerOptions as a TDocOptions

**)
procedure TfrmDockableModuleExplorer.RenderModule(M : TBaseLanguageModule;
  DocExplorerOptions : TDocOptions);

Var
  i : Integer;
  strTop : String;
  strSelection : String;
  tmp : TTreeNode;

  (**

    This routine manages the expanded node list. It increments the counter for
    each node to indicate the age of the node and deletes nodes that are more
    that iMaxRefreshes refreshes old.

    @precon  None.
    @postcon Keeps nodes alive which are not older than iMaxrefreshes.

  **)
  Procedure ManageExpanedNodes;

  Var
    iNode : Integer;

  Begin
    For iNode := FExpandedNodes.Count - 1 DownTo 0 Do
      Begin
        FExpandedNodes.Objects[iNode] := TObject(Integer(
          FExpandedNodes.Objects[iNode]) + 1);
        If Integer(FExpandedNodes.Objects[iNode]) > iMaxRefreshes Then
          FExpandedNodes.Delete(iNode);
      End;
  End;
  
  (**

    This method returns the path of the specified tree node.

    @precon  Node is the tree node to be pathed.
    @postcon Returns a string representation of the tree nodes path excluding
             the root item.

    @param   Node as a TTreeNode
    @return  a String

  **)
  Function GetNodePath(Node : TTreeNode) : String;

  Var
    P : TTreeNode;
    str : String;

  Begin
    str := '';
    P := Node;
    While P <> Nil Do
      Begin
        If P.Parent <> Nil Then
          str := P.Text + '.' + str;
        P := P.Parent;
      End;
    Result := str;
  End;

  (**

    This method gets the tree nodes that are currently expanded and stores them
    in a string list.

    @precon  Node is the tree node to be tested for expansion.
    @postcon Adds, update or deletes nodes from the expanded node list depending
             whether thhey are now expanded.

    @param   Node as a TTreeNode

  **)
  Procedure GetExpandedNodes(Node : TTreeNode);

  Var
    str : String;
    iIndex : Integer;

  Begin
    If Node.Count = 0 Then Exit;
    str := GetNodePath(Node);
    If Node.Expanded Then
      Begin
        If Not FExpandedNodes.Find(str, iIndex) Then
          iIndex := FExpandedNodes.Add(str);
        FExpandedNodes.Objects[iIndex] := TObject(1);
      End Else
        If FExpandedNodes.Find(str, iIndex) Then
          FExpandedNodes.Delete(iIndex);
  End;

  (**

    This method expands the tree view nodes if they are foudn in the list..

    @precon  Node is the tree node to be expanded.
    @postcon Sets the node as expanded if it was in the edpanded node list.

    @param   Node as a TTreeNode

  **)
  Procedure SetExpandedNodes(Node : TTreeNode);

  Var
    j : Integer;

  Begin
    If Node.Count = 0 Then Exit;
    If FExpandedNodes.Find(GetNodePath(Node), j) Then
      Node.Expanded := True;
  End;

  (**

    This function finds the tree node that has the path specified by the
    passed text.

    @precon  strText is the string representation of the node path to be
             found.
    @postcon Returns tree node index of the item corresponding to the given
             path.

    @param   strText as a String
    @return  an Integer

  **)
  Function FindTreeItem(strText : String) : Integer;

  Var
    j : Integer;

  Begin
    Result := -1;
    For j := 0 To tvExplorer.Items.Count - 1 Do
      If AnsiCompareText(GetNodePath(tvExplorer.Items[j]), strText) = 0 Then
        Begin
          Result := j;
          Exit;
        End;
  End;

Begin
  FOptions := DocExplorerOptions;
  If doManageExpandedNodes In FOptions Then ManageExpanedNodes;
  FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
  With tvExplorer Do
    Begin
      For i := 0 To tvExplorer.Items.Count - 1 Do
        GetExpandedNodes(tvExplorer.Items[i]);
      // Find and store the top item and the selected item in the tree view
      strTop := '';
      If tvExplorer.TopItem <> Nil Then
        strTop := GetNodePath(tvExplorer.TopItem);
      strSelection := '';
      If tvExplorer.Selected <> Nil Then
        strSelection := GetNodePath(tvExplorer.Selected);
      FUpdating := True;
      Items.BeginUpdate;
      Try
        Items.Clear;
        FNodeInfo.Clear;
        FModule := Nil;
        SetLength(FSpecialTagNodes, SpecialTags.Count);
        Try
          For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
            Begin
              FSpecialTagNodes[i].strTagName := SpecialTags.Names[i];
              FSpecialTagNodes[i].strTagDesc :=
                SpecialTags.Values[SpecialTags.Names[i]];
              FSpecialTagNodes[i].boolShow :=
                Integer(SpecialTags.Objects[i]) And iShowInTree <> 0;
              FSpecialTagNodes[i].boolExpand :=
                Integer(SpecialTags.Objects[i]) And iAutoExpand <> 0;
            End;
          If M = Nil Then Exit;
          FModule := AddNode(Nil, strModuleTypes[M.ModuleType] + #32 +
            M.ModuleName, M.ModuleNameLine, M.ModuleNameCol, iModule,
            Nil); // Can not process the comment here as the root node doesn't
                  // exist yet.
          DisplayErrors(M);
          DisplayDocumentConflicts(M);
          CreateSpecialTagNodes;
          FModule.Data := TObject(GetNodeComment(M.ModuleNameLine,
            M.ModuleNameCol, M.ModuleComment)); // Process module comment here.
          OutputModuleInfo(M);
          // Expand previously expanded branches
          For i := 0 To tvExplorer.Items.Count - 1 Do
            SetExpandedNodes(tvExplorer.Items[i]);
          ExpandNodes;
          // Restore top and selected items
          i := FindTreeItem(strTop);
          If i <> - 1 Then
            tvExplorer.TopItem := tvExplorer.Items[i];
          i := FindTreeItem(strSelection);
          If i <> - 1 Then
            tvExplorer.Selected := tvExplorer.Items[i];
        Finally
          FSpecialTagNodes := Nil;
        End;
      Finally
        Items.EndUpdate;
        FUpdating := False;
      End;
    End;
  M.AddTickCount('Render');
  //: @debug Body comments!
  tmp := AddNode(FModule, 'Body Comments', 0, 0, iDocConflictFolder, Nil);
  For i := 0 To M.BodyCommentCount - 1 Do
    AddNode(tmp, M.BodyComment[i].AsString(0, 1024, False),
      M.BodyComment[i].Line, M.BodyComment[i].Col, iDocConflictItem, Nil);
  With stbStatusBar Do
    Begin
      SimpleText := '';
      For i := 1 To M.OpTickCounts - 1 Do
        Begin
          If SimpleText <> '' Then SimpleText := SimpleText + ', ';
          SimpleText := SimpleText + Format('%s: %d', [
            Copy(M.OpTickCountName[i],1, 25),
            M.OpTickCountByIndex[i] - M.OpTickCountByIndex[i - 1]]);
        End;
    End;
End;

(**

  This method displays any errors found during the parsing of the module.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon Displays the error in the module.

  @param   M as a TBaseLanguageModule

**)
Procedure TfrmDockableModuleExplorer.DisplayErrors(M : TBaseLanguageModule);

Var
  i : Integer;
  N : TTreeNode;
  FError : TTreeNode;

Begin
  FError := Nil;
  If M.Errors.Count > 0 Then
    FError := AddNode(FModule, strErrorsAndWarnings, 0, 0, iErrorFolder, Nil);
  For i := 0 To M.Errors.Count - 1 Do
    Begin
      N := AddNode(FError, M.Errors[i].Msg{ + ' [' +
        M.Errors[i].ExceptionMethod + ']'}, M.Errors[i].Line,
        M.Errors[i].Col, -1, Nil);
      Case M.Errors[i].ErrorType Of
        etWarning :
          Begin
            N.ImageIndex := iWarning;
            N.SelectedIndex := iWarning;
          End;
        etError :
          Begin
            N.ImageIndex := iError;
            N.SelectedIndex := iError;
          End;
      End;
      FError.Expand(True);
    End;
End;


(**

  This method outputs the modules information as items in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon Outputs the different parts of the module.

  @param   M as a TBaseLanguageModule

**)
procedure TfrmDockableModuleExplorer.OutputModuleInfo(M : TBaseLanguageModule);

begin
  DisplayClause(M.UsesCls, FModule, strUses, iUnit, iUnitLabel);
  DisplayClause(M.Types, FModule, strTypesLabel, iTypes, iTypeLabel);
  DisplayClasses(M.Types, FModule, strClasses, 'TClassDecl', iClass, iClassLabel);
  DisplayClasses(M.Types, FModule, strInterfaces, 'TInterfaceDecl', iInterface, iInterfaceLabel);
  DisplayClasses(M.Types, FModule, strDispInterfaces, 'TDispInterfaceDecl', iInterface, iInterfaceLabel);
  DisplayClasses(M.Types, FModule, strObjects, 'TObjectDecl', iObject, iObjectLabel);
  DisplayClasses(M.Types, FModule, strRecords, 'TRecordDecl', iRecord, iRecordLabel);
  DisplayClause(M.Constants, FModule, strConstants, iConstants, iConstantsLabel);
  DisplayClause(M.ResourceStrings, FModule, strResourceStrings, iResString, iResStringLabel);
  DisplayClause(M.Vars, FModule, strVars, iVars, iVarsLabel);
  DisplayClause(M.ThreadVars, FModule, strThreadVars, iThreadVar, iThreadVarLabel);
  DisplayProcs(M.ExportedHeadings, FModule, strExportedHeadings, False, iExportedHeadingsLabel);
  DisplayClause(M.ExportsClause, FModule, strExports, iExportHeadings, iExportsLabel);
  DisplayProcs(M.ImplementedMethods, FModule, strImplementedMethods, True, iImplementedMethodsLabel);
  DisplayClause(M.Requires, FModule, strRequires, iUnit, iUnitLabel);
  DisplayClause(M.Contains, FModule, strContains, iUnit, iUnitLabel);
  // Initialization Clause
  If M.InitComment <> Nil Then
    AddNode(FModule, strInitialization, M.InitComment.Line,
      M.InitComment.Col, iInitialization, M.InitComment);
  // Finalization Clause
  If M.FinalComment <> Nil Then
    AddNode(FModule, strFinalization, M.FinalComment.Line,
      M.FinalComment.Col, iFinalization, M.FinalComment);
  GetBodyCommentTags(M);
end;


(**

  This method expands, collapses or delete various nodes depending on the
  options and their contents.

  @precon  None.
  @postcon Expands and

**)
procedure TfrmDockableModuleExplorer.ExpandNodes;

Var
  i : Integer;

begin
  FModule.Expand(False);
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If Not FSpecialTagNodes[i].Node.HasChildren Then
      Begin
        FSpecialTagNodes[i].Node.Delete;
        FSpecialTagNodes[i].Node := Nil;
      End;
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[i].Node <> Nil Then
      If FSpecialTagNodes[i].boolExpand Then
        FSpecialTagNodes[i].Node.Expand(True);
end;


(**

  This method create both the todo folder node and the document conflict
  folders in the treeview.

  @precon  None.
  @postcon Creates the special tag nodes.

**)
Procedure TfrmDockableModuleExplorer.CreateSpecialTagNodes;

Var
  i : Integer;

Begin
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    Begin
      FSpecialTagNodes[i].Node := tvExplorer.Items.AddChild(FModule,
        FSpecialTagNodes[i].strTagDesc);
      FSpecialTagNodes[i].Node.ImageIndex := iToDoFolder;
      FSpecialTagNodes[i].Node.SelectedIndex := iToDoFolder;
    End;
End;

(**

  This is the treeviews on change event. It displays the nodes comment and
  fires the OnSelectionChnage event if assigned.

  @precon  Sender is a object that initiates the event and Node is the tree node
           that has changed.
  @postcon Fires an event noting that the selection has changed.

  @param   Sender as a TObject
  @param   Node   as a TTreeNode

**)
procedure TfrmDockableModuleExplorer.tvExplorerChange(Sender: TObject;
  Node: TTreeNode);

Var
  N : TTreeNodeInfo;

begin
  If Assigned(FSelectionChange) And Not FUpdating Then
    Begin
      N := NodeInfo[Integer(Node.Data)];
      If N.Comment = Nil Then
        FSelectionChange(N.Line, N.Col, N.Line, N.Col)
      Else
        FSelectionChange(N.Line, N.Col, N.Comment.Line, N.Comment.Col);
    End;
end;

(**

  This method adds a node to the treeview as a child of the give node. This
  method makes sure the text is not longer then 250 character as theres a memory
  bug the manifests itself when the text is too long. It assigns the line, column
  and comment information to the node.

  @precon  P is the parent node to attach this new child too, strText is the text
           to be displayed in the node, iLine is the line number of the item in
           the source module, iCol is the column number of the item in the
           source module and Comment is a valid comment to be attached to the
           node else nil.
  @postcon Returns a instance of the newly add / created tree node.

  @param   P           as a TTreeNode
  @param   strText     as a String
  @param   iLine       as an Integer
  @param   iCol        as an Integer
  @param   iImageIndex as a Integer
  @param   Comment     as a TComment
  @return  a TTreeNode

**)
Function TfrmDockableModuleExplorer.AddNode(P: TTreeNode; strText: String;
  iLine, iCol, iImageIndex : Integer; Comment: TComment) : TTreeNode;

Const
  iTreeLimit = 250;

Var
  str : String;

begin
  str := strText;
  If Length(str) > iTreeLimit Then
    str := Copy(str, 1, iTreeLimit);
  Result := tvExplorer.Items.AddChild(P, str);
  Result.Data := TObject(GetNodeComment(iLine, iCol, Comment));
  Result.ImageIndex := iImageIndex;
  Result.SelectedIndex := iImageIndex;
end;

(** @debug

  This is a getter method for the NodeInfoCount property.

  @postcon Returns the number of items on the node info collection.

  @return  an Integer


function TfrmDockableModuleExplorer.GetNodeInfoCount: Integer;
begin
  Result := FNodeInfo.Count;
end;**)

(**

  This is a getter method for the TreeNodeInfo property.

  @precon iIndex is the index of the tree node info item required.
  @postcon Returns an instance of the indexed tree node information.

  @param   iIndex as an Integer
  @return  a TTreeNodeInfo

**)
function TfrmDockableModuleExplorer.GetTreeNodeInfo(iIndex: Integer): TTreeNodeInfo;
begin
  Result := Nil;
  If (iIndex >= 0) And (iIndex < FNodeInfo.Count) Then
    Result := FNodeInfo[iIndex] As TTreeNodeInfo;
end;

(**

  This is a class method which accepots even handler from the calling class to
  hand the dockable module explorer's SelectionChange event handler.

  @precon  None.
  @postcon Sets the SelectionChange event handler for the dockable form.

  @param   SelectionChangeProc as a TSelectionChange

**)
class procedure TfrmDockableModuleExplorer.HookEventHandlers(
  SelectionChangeProc: TSelectionChange);
begin
  If Assigned(FormInstance) Then
    FormInstance.OnSelectionChange := SelectionChangeProc;
end;

(**

  This method is an OnDrawItem event handler for the treeview. Depending on
  the options it renders the syntax highlighted version of the tree.

  @precon  Sender is the control that invoked the event, Node is the node in
           the treeview to be drawn, State is the current drawing state of the
           node and DefaultDraw - this is change to false to allow custom
           drawing.
  @postcon This event handler draw a custom item in the tree view.

  @param   Sender      as a TCustomTreeView
  @param   Node        as a TTreeNode
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
procedure TfrmDockableModuleExplorer.tvExplorerCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);

Const
  iTreeColour = clGray;

Var
  NodeRect : TRect;
  i : Integer;
  P : TTreeNode;
  iOffset  :Integer;
  sl : TStringList;
  iPos : Integer;
  iCentre : Integer;

begin
  DefaultDraw := Not (doCustomDrawing In FOptions);
  With Sender.Canvas Do
    If Not DefaultDraw Then
      Begin
        iOffset := GetScrollPos(Sender.Handle, SB_HORZ);
        //: @bug This needs fixing properly!!!
        sl := Tokenize(Node.Text, [], []);
        Try
          // Highlight selected item.
          If cdsSelected In State Then
            Begin
              Brush.Color := clHighlight;
              NodeRect := Node.DisplayRect(True);
              // Need to amend the width of the rectangle for the custom drawing
              iPos := 5;
              For i := 0 To sl.Count - 1 Do
                Begin
                  GetFontInfo(sl, i, Node.Level, Sender.Canvas);
                  Inc(iPos, TextWidth(sl[i]));
                End;
              NodeRect.Right := NodeRect.Left + iPos;
              Brush.Color := clAqua;
              FillRect(NodeRect);
              Pen.Color := clBlack;
              Rectangle(NodeRect);
            End;
          NodeRect := Node.DisplayRect(False);
          iCentre := (NodeRect.Top + NodeRect.Bottom) Div 2;
          // Draw Tree
          NodeRect.Left := NodeRect.Left + (Node.Level * tvExplorer.Indent) - iOffset;
          // Draw vertical tree lines
          P := Node.Parent;
          For i := Node.Level - 1 DownTo 0 Do
            Begin
              If (P <> Nil) And (P.Parent <> Nil) Then
                If P.Index < P.Parent.Count - 1  Then
                  Begin
                    Pen.Color := iTreeColour;
                    Pen.Style := psSolid;
                    MoveTo(tvExplorer.Indent * i + 8 - iOffset, NodeRect.Top);
                    LineTo(tvExplorer.Indent * i + 8 - iOffset, NodeRect.Bottom);
                  End;
              P := P.Parent;
            End;
          // Draw top half of node connector
          Pen.Color := iTreeColour;
          Pen.Style := psSolid;
          MoveTo(NodeRect.Left + 8, iCentre);
          LineTo(NodeRect.Left + tvExplorer.Indent, iCentre);
          If Node.Parent <> Nil Then
            Begin
              // Draw connection to item
              Pen.Color := iTreeColour;
              Pen.Style := psSolid;
              MoveTo(NodeRect.Left + 8, NodeRect.Top);
              LineTo(NodeRect.Left + 8, iCentre);
              If Node.Index < Node.Parent.Count - 1 Then
                Begin
                  // Draw connector to next node.
                  Pen.Color := iTreeColour;
                  Pen.Style := psSolid;
                  MoveTo(NodeRect.Left + 8, iCentre);
                  LineTo(NodeRect.Left + 8, NodeRect.Bottom);
                End;
            End;
          If Node.Count > 0 Then
            Begin
              // Draw button
              Pen.Color := iTreeColour;
              Pen.Style := psSolid;
              Rectangle(NodeRect.Left + 4, iCentre - 4,
                NodeRect.Left + 13, iCentre + 5);
              // Draw negative side
              Pen.Color := clBlack;
              MoveTo(NodeRect.Left + 6, iCentre);
              LineTo(NodeRect.Left + 11, iCentre);
              If Not Node.Expanded Then
                Begin
                  // Make positive sign
                  MoveTo(NodeRect.Left + 8, iCentre - 2);
                  LineTo(NodeRect.Left + 8, iCentre + 3);
                End;
            End;
          //Draw Image
          NodeRect.Left := NodeRect.Left + tvExplorer.Indent;
          ilScopeImages.Draw(Sender.Canvas, NodeRect.Left, NodeRect.top, Node.ImageIndex);

          // Draw text
          NodeRect.Left := NodeRect.Left + ilScopeImages.Width + 3;
          iPos := NodeRect.Left + 2;
          For i := 0 To sl.Count - 1 Do
            Begin
              GetFontInfo(sl, i, Node.Level, Sender.Canvas);
              TextOut(iPos, NodeRect.Top + 1, sl[i]);
              Inc(iPos, TextWidth(sl[i]));
            End;
        Finally
          sl.Free;
        End;
      End;
end;

(**

  This method is a mouse OnMouseMove event. If fires every time the mouse moves
  over the treeview and is used to show the hint messages.

  @precon  Sender is the control that invoked the event, Shift is the status of
           the keyboard modifiers, X is the horizontal position of the mouse
           over the control and Y is the vertical position of the mouse over the
           control
  @postcon Handles the displaying of hints when the mouse is over a tree branch.

  @param   Sender as a TObject
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
procedure TfrmDockableModuleExplorer.tvExplorerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

Var
  Node : TTreeNode;
  Rect : TRect;
  C : TComment;
  HT : THitTests;

begin
  C := Nil;
  HT := tvExplorer.GetHitTestInfoAt(X, Y);
  If (htOnItem In HT) Then
    Begin
      Node := tvExplorer.GetNodeAt(X, Y);
      If (Node <> Nil) And (Node <> FLastNode) Then
        Begin
          FLastNode := Node;
          If doShowCommentHints In FOptions Then
            C := NodeInfo[Integer(Node.Data)].Comment;
          Rect := FHintWin.CalcHintRect(tvExplorer.ClientWidth, Screen.Width,
            Node, doCustomDrawing In FOptions, C);
          If (Rect.Right <= tvExplorer.ClientWidth) And ((C = Nil) Or
            ((C.TokenCount = 0) And (C.TagCount = 0))) Then
            Begin
              FHintWin.ReleaseHandle;
              Exit;
            End;
          Rect.TopLeft := tvExplorer.ClientToScreen(Rect.TopLeft);
          Rect.BottomRight := tvExplorer.ClientToScreen(Rect.BottomRight);
          FHintWin.ActivateHint(Rect, Node, doCustomDrawing In FOptions, C);
          FLastNode := Node;
        End Else
          If (Node <> FLastNode) Then
            FHintWin.ReleaseHandle;
    End Else
    Begin
      FHintWin.ReleaseHandle;
      FLastNode := Nil;
    End;
end;

(**

  This is a message handler for the mouse leave message. It hides the hint
  window when the mouse leaves the control.

  @precon  Msg is the window message to handle.
  @postcon The is a mouse event event which hides the hint window.

  @param   Msg as a TMessage as a reference

**)
Procedure TfrmDockableModuleExplorer.CMMouseLeave(var Msg : TMessage);

Begin
  FHintWin.ReleaseHandle;
End;

(**

  This method is the paint method for the customised hint window.

  @precon  None.
  @postcon Draws the custom hint window.

**)
Procedure TCustomHintWindow.Paint;

Var
  sl : TStringList;
  iPos : Integer;
  i, j : Integer;
  R : TRect;
  str : String;

Begin
  With Canvas Do
    Begin
      If FCustomDraw Then
        Begin
          //: @bug This needs fixing properly!!!
          sl := Tokenize(Caption, [], []);
          Try
            iPos := 2;
            For i := 0 To sl.Count - 1 Do
              Begin
                GetFontInfo(sl, i, FNodeLevel, Canvas);
                TextOut(iPos, 0, sl[i]);
                Inc(iPos, TextWidth(sl[i]));
              End;
          Finally
            sl.Free;
          End;
        End Else
        Begin
          Refresh;
          Font.Color := clInfoText;
          Font.Style := [];
          TextOut(2, 0, Caption);
        End;
      If (FComment <> Nil) And ((FComment.TokenCount > 0) Or
        (FComment.TagCount > 0)) Then
        Begin
          FillRect(Rect(0, 15, Width, Height));
          Pen.Color := clMaroon;
          MoveTo(0, 15);
          Lineto(Width, 15);
          Refresh;
          Font.Style := [];
          Font.Color := clNavy;
          str := FComment.AsString(0, MaxInt, False);
          R := Rect(2, 17, Width - 2, Height);
          iPos := DrawText(Canvas.Handle, PChar(str), -1, R,
            DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly);
          R := Rect(2, 17 + iPos, Width - 4, Height);
          For i := 0 To SpecialTags.Count - 1 Do
            Begin
              If DrawSpecialTag(FComment, SpecialTags.Names[i]) Then
                Begin
                  Refresh;
                  Font.Style := [fsBold, fsUnderline];
                  Font.Color := clPurple;
                  Inc(R.Top, 5);
                  R := Rect(2, R.Top, Width - 2, Height);
                  str := SpecialTags.Values[SpecialTags.Names[i]];
                  Inc(R.Top, DrawText(Canvas.Handle, PChar(str), -1, R,
                    DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                    DrawTextBiDiModeFlagsReadingOnly));
                  For j := 0 To FComment.TagCount - 1 Do
                    If AnsiCompareText(SpecialTags.Names[i], FComment.Tag[j].TagName) = 0 Then
                      Begin
                        Pen.Color := clBlack;
                        Brush.Color := clBlack;
                        Ellipse(3, R.Top + 5, 7, R.Top + 9);
                        Brush.Color := clInfoBk;
                        Refresh;
                        Font.Style := [];
                        Font.Color := clMaroon;
                        R := Rect(10, R.Top, Width - 2, Height);
                        str := FComment.Tag[j].AsString(False);
                        Inc(R.Top, DrawText(Canvas.Handle, PChar(str), -1, R,
                          DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                          DrawTextBiDiModeFlagsReadingOnly));
                      End;
                End;
            End;
          (*For j := 0 To FComment.TagCount - 1 Do
            If IsSpecial(FComment.Tag[j].TagName) = -1 Then
              Begin
                Refresh;
                Font.Style := [];
                Font.Color := clMaroon;
                R := Rect(2, R.Top, Width - 2, Height);
                str := FComment.Tag[j].TagName + '   : ' + FComment.Tag[j].AsString;
                DrawText(Canvas.Handle, PChar(str), -1, R,
                  DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                  DrawTextBiDiModeFlagsReadingOnly);
                Refresh;
                Font.Style := [fsBold];
                Inc(R.Top, DrawText(Canvas.Handle, PChar(FComment.Tag[j].TagName), -1, R,
                  DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                  DrawTextBiDiModeFlagsReadingOnly));
              End;*)
        End;
    End;
End;

(**

  This method calcalates the size of the hint window based on the tree node
  display window, a max and min width , whether to display syntax highlighting
  and a comment.

  @precon  MinWidth is the minimum width of the hint window to be displayed,
           MaxWidth is the minimum width of the hint window to be displayed,
           Node is the tree node to calculate the hint window for,
           SyntaxHighlight tells the routine to print either a plain text
           representation of the information or one with syntax highlighing,
           and Comment is the comment associated with the tree node and
  @postcon Returns the newly calculated rectangle of the hint window.

  @param   MinWidth        as an Integer
  @param   MaxWidth        as an Integer
  @param   Node            as a TTreeNode
  @param   SyntaxHighlight as a Boolean
  @param   Comment         as a TComment
  @return  a TRect

**)
Function TCustomHintWindow.CalcHintRect(MinWidth, MaxWidth : Integer;
  Node : TTreeNode; SyntaxHighlight : Boolean; Comment : TComment) : TRect;

Var
  sl : TStringList;
  iPos : Integer;
  i, j : Integer;
  str : String;
  R : TRect;

Begin
  Result := Node.DisplayRect(True);
  If SyntaxHighlight Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := 5;
      //: @bug This needs to be fixed properly!!!
      sl := Tokenize(Node.Text, [], []);
      Try
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, Node.Level, Canvas);
            Inc(iPos, Canvas.TextWidth(sl[i]));
          End;
        Result.Right := Result.Left + iPos;
      Finally
        sl.Free;
      End;
    End;
  Dec(Result.Bottom, 4);
  Dec(Result.Left, 1);
  Inc(Result.Right, 2);
  // Check for comment
  If (Comment <> Nil) And ((Comment.TokenCount > 0) Or (Comment.TagCount > 0)) Then
    Begin
      If Result.Right - Result.Left < MinWidth Then
        Result.Right := Result.Left + MinWidth;
      Inc(Result.Bottom, 5);
      Refresh;
      Canvas.Font.Style := [];
      str := Comment.AsString(0, MaxInt, False);
      R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
      Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
        DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
        DrawTextBiDiModeFlagsReadingOnly));
      For i := 0 To SpecialTags.Count - 1 Do
        Begin
          If DrawSpecialTag(Comment, SpecialTags.Names[i]) Then
            Begin
              Refresh;
              Canvas.Font.Style := [fsBold, fsUnderline];
              R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
              str := SpecialTags.Values[SpecialTags.Names[i]];
              Inc(Result.Bottom, 5 + DrawText(Canvas.Handle, PChar(str), -1, R,
                DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                DrawTextBiDiModeFlagsReadingOnly));
              For j := 0 To Comment.TagCount - 1 Do
                If AnsiCompareText(SpecialTags.Names[i], Comment.Tag[j].TagName) = 0 Then
                  Begin
                    Refresh;
                    Canvas.Font.Style := [];
                    R := Rect(Result.Left + 2, 0, Result.Right - 12, 0);
                    str := Comment.Tag[j].AsString(False);
                    Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
                      DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                      DrawTextBiDiModeFlagsReadingOnly));
                  End;
            End;
        End;
      (*For j := 0 To Comment.TagCount - 1 Do
        If IsSpecial(Comment.Tag[j].TagName) = -1 Then
          Begin
            Refresh;
            Canvas.Font.Style := [];
            R := Rect(2, 17, Width - 4, Height);
            str := Comment.Tag[j].TagName + ' : ' + Comment.Tag[j].AsString;
            Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
              DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
              DrawTextBiDiModeFlagsReadingOnly));
          End;*)
    End;
End;

(**

  This method is an overridden ActivateHint method to allow for the passing of
  more information to the hint windows internal routines.

  @precon  Rect is the rectangle to display the hint in, Node is the tree node
           to create a hint for, SyntaxHighlight tells the routine to print
           either a plain text representation of the information or one with
           syntax highlighing and Comment is the comment associated with the
           tree node.
  @postcon Activates the hint window.

  @param   Rect            as a TRect
  @param   Node            as a TTreeNode
  @param   SyntaxHighlight as a Boolean
  @param   Comment         as a TComment

**)
Procedure TCustomHintWindow.ActivateHint(Rect : TRect; Node : TTreeNode;
  SyntaxHighlight : Boolean; Comment : TComment);

Begin
  FComment := Comment;
  FNodeLevel := Node.Level;
  FCustomDraw := SyntaxHighlight;
  ActivateHint(Rect, Node.Text);
End;

(**

  This method determines if the special tag need to be rendered in the hint
  window.

  @precon  Comment is the comment to get tags from and strSpecialTag is the
           special tag information to look for.
  @postcon Return true is the special tag was found in the comment.

  @param   Comment       as a TComment
  @param   strSpecialTag as a String
  @return  a Boolean

**)
Function TCustomHintWindow.DrawSpecialTag(Comment : TComment;
  strSpecialTag : String) : Boolean;
Var
  i : Integer;

Begin
  Result := False;
  For i := 0 To Comment.TagCount - 1 Do
    If AnsiCompareText(strSpecialTag, Comment.Tag[i].TagName) = 0 Then
      Begin
        Result := True;
        Exit;
      End;
End;

End.
