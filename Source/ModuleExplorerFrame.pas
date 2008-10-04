(**

  This module contains a frame which holds all the functionality of the
  module browser so that it can be independant of the application specifics.

  @Date    04 Oct 2008
  @Author  David Hoyle
  @Version 1.0

**)
unit ModuleExplorerFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, ExtCtrls, Contnrs, BaseLanguageModule,
  ActnList, ToolWin;

type
  (** This enumerate represents the type of the item that generates the
      selection change event. **)
  TSelectType = (stIdentifier, stError, stConflict, stSpecialTags);

  (** This class represents information about each nodes. Used in a collection
      instead of hanging the data off the tree nodes. **)
  TTreeNodeInfo = Class
  Private
    FLine : Integer;
    FCol : Integer;
    FComment : TComment;
    FSelectType : TSelectType;
  Public
    Constructor Create(iLine, iCol : Integer; AComment : TComment;
      SelectType : TSelectType); Overload;
    Destructor Destroy; Override;
    (**
      This property returns the line number of the tree node comment.
      @precon  None.
      @postcon Returns the line associated with the node info structure.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      This property returns the column number of the tree node comment.
      @precon  None.
      @postcon Returns the column associated with the node info structure.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      This property returns the comment associated with the tree node info.
      @precon  None.
      @postcon Returns the comment associated with the node info structure.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment;
    (**
      A property to define the type of select being made.
      @precon  None.
      @postcon Returns the type of selection made.
      @return  a TSelectType
    **)
    Property SelectType : TSelectType Read FSelectType;
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
      support of the tree view.
  **)
  TCustomHintWindow = Class(THintWindow)
  Private
    FComment : TComment;
    FNodeLevel : Integer;
    FCustomDraw : Boolean;
    FTreeView: TTreeView;
  Public
    Constructor Create(AOwner : TComponent; ATreeView : TTreeView); ReIntroduce;
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
    iCommentCol : Integer; SelectType : TSelectType) Of Object;

  (** This is a frame class to contain all the functionality of the module
      explorer so that it can be placed inside any container required and
      therefore does not need to know about things like BDS 2006 IDEs or
      application specifics. **)
  TframeModuleExplorer = class(TFrame)
    stbStatusBar: TStatusBar;
    ilScopeImages: TImageList;
    tvExplorer: TTreeView;
    tbrExplorerScope: TToolBar;
    ilToolbar: TImageList;
    alToolbar: TActionList;
    actLocal: TAction;
    actPrivate: TAction;
    actProtected: TAction;
    actPublic: TAction;
    actPublished: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure tvExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvExplorerClick(Sender: TObject);
    procedure actLocalUpdate(Sender: TObject);
    procedure actLocalExecute(Sender: TObject);
    procedure tvExplorerKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FModule : TTreeNode;
    FNodeInfo : TObjectList;
    FSelectionChange : TSelectionChange;
    FFocus : TNotifyEvent;
    FSpecialTagNodes : Array Of TSpecialTagNode;
    FHintWin : TCustomHintWindow;
    FLastNode : TTreeNode;
    FINIFileName: String;
    FSelectionChanging: Boolean;
    FRendering: Boolean;
    FRefresh : TNotifyEvent;
    { Private declarations }
    procedure GetBodyCommentTags(M : TBaseLanguageModule);
    Function AddNode(P : TTreeNode; strText : String; iLine, iCol,
      iImageIndex : Integer; Comment : TComment; SelectType : TSelectType) : TTreeNode;
    function GetTreeNodeInfo(iIndex: Integer): TTreeNodeInfo;
    procedure CreateSpecialTagNodes;
    Function GetNodeComment(iLine, iCol : Integer; C : TComment;
      SelectType : TSelectType) : Integer;
    procedure ExpandNodes;
    procedure OutputModuleInfo(Container : TElementContainer);
    function FindTreeItem(strText: String): Integer;
    procedure GetExpandedNodes;
    function GetNodePath(Node: TTreeNode): String;
    procedure SetExpandedNodes;
    Procedure RenderContainers(RootNode : TTreenode; Container : TElementContainer);
    Procedure UpdateStatusBar(M : TBaseLanguageModule);
    (**
      This property returns the indexed NodeInfo class for the collection.
      @precon  iIndex must eb a valid index.
      @postcon Returns the indexed NodeInfo class for the collection.
      @param   iIndex as an Integer
      @return  a TTreeNodeInfo
    **)
    Property NodeInfo[iIndex : Integer] : TTreeNodeInfo Read GetTreeNodeInfo;
  Protected
    Procedure CMMouseLeave(var Msg : TMessage); Message CM_MOUSELEAVE;
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    procedure RenderModule(M : TBaseLanguageModule);
    (**
      This is an event for the selection change in the browser tree.
      @precon  None.
      @postcon Hooks an event handler for the On Selection change event.
      @return  a TSelectionChange
    **)
    Property OnSelectionChange : TSelectionChange Read FSelectionChange
      Write FSelectionChange;
    (**
      This is an event for the on focus change in the browser.
      @precon  None.
      @postcon Hooks an event handler for the On Focus change event.
      @return  a TNotifyEvent
    **)
    Property OnFocus : TNotifyEvent Read FFocus Write FFocus;
    (**
      This is an event handler for the On Options Change event.
      @precon  None.
      @postcon Hooks an event handler for the change of options event.
      @return  a TNotifyEvent
    **)
    Property OnRefresh : TNotifyEvent Read FRefresh Write FRefresh;
  end;

implementation

Uses
  IniFiles, Types, Math, DGHLibrary, GenericTokenizer;

Resourcestring
  (** A format pattern for the statusbar text. **)
  strStatusbarText = '%1.0n Bytes, %1.0n Tokens and %1.0n Lines.';

{$R *.dfm}

Var
  (** A private variable which is assigned the key words array. **)
  strKeyWords : TKeyWords;

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
          Case TTokenType(sl.Objects[i]) Of
            ttIdentifier:
              Begin
                Font.Color := BrowseAndDocItOptions.TokenFontInfo[ttIdentifier].FColour;
                Font.Style := BrowseAndDocItOptions.TokenFontInfo[ttIdentifier].FStyles;
              End;
            ttReservedWord:
              Begin
                Font.Color := BrowseAndDocItOptions.TokenFontInfo[ttReservedWord].FColour;
                Font.Style := BrowseAndDocItOptions.TokenFontInfo[ttReservedWord].FStyles;
              End;
            ttDirective :
              Begin
                Font.Color := BrowseAndDocItOptions.TokenFontInfo[ttDirective].FColour;
                Font.Style := BrowseAndDocItOptions.TokenFontInfo[ttDirective].FStyles;
              End;
            ttSymbol :
              Begin
                Font.Color := BrowseAndDocItOptions.TokenFontInfo[ttSymbol].FColour;
                Font.Style := BrowseAndDocItOptions.TokenFontInfo[ttSymbol].FStyles;
              End;
            ttNumber :
              Begin
                Font.Color := BrowseAndDocItOptions.TokenFontInfo[ttNumber].FColour;
                Font.Style := BrowseAndDocItOptions.TokenFontInfo[ttNumber].FStyles;
              End;
            ttStringLiteral :
              Begin
                Font.Color := BrowseAndDocItOptions.TokenFontInfo[ttStringLiteral].FColour;
                Font.Style := BrowseAndDocItOptions.TokenFontInfo[ttStringLiteral].FStyles;
              End;
          Else
            Font.Color := clBlack;
            Font.Style := [];
          End;
        End Else
        Begin
          Font.Color := clMaroon;
          Font.Style := [fsBold];
        End;
   End;
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment
           and This comment to be added to the node info object.
  @postcon Initialises the class.

  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @param   AComment   as a TComment
  @param   SelectType as a TSelectType

**)
Constructor TTreeNodeInfo.Create(iLine, iCol : Integer; AComment : TComment;
  SelectType : TSelectType);

Begin
  Inherited Create;
  FLine := ILine;
  FCol := iCol;
  FComment := TComment.Create(AComment);
  FSelectType := SelectType;
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

  This is the constructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon Initialises the class.

  @param   AOwner as a TComponent

**)
Constructor TframeModuleExplorer.Create(AOwner: TComponent);

Var
  i : TImageIndex;

begin
  Inherited;
  FINIFileName := BuildRootKey(Nil, Nil);
  FNodeInfo := TObjectList.Create(True);
  FHintWin := TCustomHintWindow.Create(Self, tvExplorer);
  FHintWin.Color := clInfoBk;
  FHintWin.Canvas.Font.Assign(tvExplorer.Font);
  ilScopeImages.Clear;
  For i := Succ(Low(TImageIndex)) to High(TImageIndex) Do
    If Not ilScopeImages.GetInstRes(hInstance, rtBitmap, ImageList[i].FResourceName, 16,
      [lrDefaultColor], ImageList[i].FMaskColour) Then
      ShowMessage(Format('Resource "%s" not found.', [ImageList[i].FResourceName]))
end;

(**

  This is the destructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon destroy the instance of the dockable form.

**)
Destructor TframeModuleExplorer.Destroy;

begin
  GetExpandedNodes;
  FHintWin.Free;
  FNodeInfo.Free;
  Inherited;
end;

(**

  This method gets the comment for the supplied comment and extracts todo
  comment as the same time.

  @precon  iLine is the line of the comment in the module, iCol is the column of
           the comment in the module and C is a valid comment or nil to be
           created and associated with a node.
  @postcon Returns the index of the newly created node comment.

  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @param   C          as a TComment
  @param   SelectType as a TSelectType
  @return  an Integer

**)
Function TframeModuleExplorer.GetNodeComment(iLine, iCol : Integer;
  C : TComment; SelectType : TSelectType) : Integer;

Var
  Info : TTreeNodeInfo;

Begin
  Info := TTreeNodeInfo.Create(iLine, iCol, C, SelectType);
  Result := FNodeInfo.Add(info);
End;

(**

  This method retrieves comments from the body comment collection and adds them
  to the comment node of the tree view.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon This method cycles through the body comments extracting special tags.

  @param   M as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.GetBodyCommentTags(M : TBaseLanguageModule);

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
                  Integer(iiToDoItem) - 1, Nil, stSpecialTags);
End;

(**

  This method renders the modules sub-containers recursively.

  @precon  RootNode must be a valid tree node and Container must be a valid
           container.
  @postcon Renders the modules sub-containers recursively.

  @param   RootNode  as a TTreenode
  @param   Container as a TElementContainer

**)
procedure TframeModuleExplorer.RenderContainers(RootNode : TTreenode; Container: TElementContainer);

Var
  i : Integer;
  NewNode : TTreeNode;

begin
  For i := 1 To Container.ElementCount Do
    If Container.Elements[i].Scope In BrowseAndDocItOptions.ScopesToRender +
      [scNone, scGlobal] Then
      Begin
        NewNode := AddNode(RootNode, Container[i].AsString, Container[i].Line,
          Container[i].Column, Container[i].ImageIndexAdjustedForScope,
          Container[i].Comment, stIdentifier);
        RenderContainers(NewNode, Container[i]);
      End;
end;

(**

  This method returns the path of the specified tree node.

  @precon  Node is the tree node to be pathed.
  @postcon Returns a string representation of the tree nodes path excluding
           the root item.

  @param   Node as a TTreeNode
  @return  a String

**)
Function TframeModuleExplorer.GetNodePath(Node : TTreeNode) : String;

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

**)
Procedure TframeModuleExplorer.GetExpandedNodes;

Var
  str : String;
  i, iIndex : Integer;
  Node : TTreeNode;

Begin
  For i := 0 To tvExplorer.Items.Count - 1 Do
    Begin
      Node := tvExplorer.Items[i];
      If Node.Count = 0 Then
        Continue;
      str := GetNodePath(Node);
      If Node.Expanded And (str <> '') Then
        Begin
          If Not BrowseAndDocItOptions.ExpandedNodes.Find(str, iIndex) Then
            iIndex := BrowseAndDocItOptions.ExpandedNodes.Add(str);
          BrowseAndDocItOptions.ExpandedNodes.Objects[iIndex] := TObject(1);
        End Else
          If BrowseAndDocItOptions.ExpandedNodes.Find(str, iIndex) Then
            BrowseAndDocItOptions.ExpandedNodes.Delete(iIndex);
    End;
End;

(**

  This method expands the tree view nodes if they are foudn in the list..

  @precon  Node is the tree node to be expanded.
  @postcon Sets the node as expanded if it was in the edpanded node list.

**)
Procedure TframeModuleExplorer.SetExpandedNodes;

Var
  i, j : Integer;
  str : String;
  Node : TTreeNode;

Begin
  For i := 0 To tvExplorer.Items.Count - 1 Do
    Begin
      Node := tvExplorer.Items[i];
      If Node.Count = 0 Then
        Continue;
      str := GetNodePath(Node);
      If BrowseAndDocItOptions.ExpandedNodes.Find(str, j) Then
        Node.Expanded := True;
    End;
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
Function TframeModuleExplorer.FindTreeItem(strText : String) : Integer;

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

(**

  This method displays the specified module in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed and
           strStatus is a text string to be displayed in the forms status bar.
  @postcon Renders the module information for the given module.

  @param   M as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.RenderModule(M : TBaseLanguageModule);

Var
  i : Integer;
  strTop : String;
  strSelection : String;

Begin
  tvExplorer.Color := BRowseAndDocItOptions.BGColour;
  If M = Nil Then
    strKeyWords := Nil
  Else
    strKeyWords := M.KeyWords;
  If FRendering Then
    Exit;
  FRendering := True;
  Try
    FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
    With tvExplorer Do
      Begin
        Font.Name := BrowseAndDocItOptions.FontName;
        Font.Size := BrowseAndDocItOptions.FontSize;
        GetExpandedNodes;
        // Find and store the top item and the selected item in the tree view
        strTop := '';
        If tvExplorer.TopItem <> Nil Then
          strTop := GetNodePath(tvExplorer.TopItem);
        strSelection := '';
        If tvExplorer.Selected <> Nil Then
          strSelection := GetNodePath(tvExplorer.Selected);
        Items.BeginUpdate;
        Try
          Items.Clear;
          FNodeInfo.Clear;
          FModule := Nil;
          SetLength(FSpecialTagNodes, BrowseAndDocItOptions.SpecialTags.Count);
          Try
            For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
              Begin
                FSpecialTagNodes[i].strTagName := BrowseAndDocItOptions.SpecialTags.Names[i];
                FSpecialTagNodes[i].strTagDesc :=
                  BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
                FSpecialTagNodes[i].boolShow :=
                  Integer(BrowseAndDocItOptions.SpecialTags.Objects[i]) And iShowInTree <> 0;
                FSpecialTagNodes[i].boolExpand :=
                  Integer(BrowseAndDocItOptions.SpecialTags.Objects[i]) And iAutoExpand <> 0;
              End;
            If M = Nil Then
              Exit;
            M.AddTickCount('Clear');
            // Create Root Tree Node
            FModule := AddNode(Nil, M.ModuleName, M.ModuleNameLine,
              M.ModuleNameCol, M.ImageIndexAdjustedForScope, M.Comment,
              stIdentifier);
            CreateSpecialTagNodes;
            OutputModuleInfo(M);
            M.AddTickCount('Build');
            SetExpandedNodes;
            ExpandNodes;
            // Restore top and selected items
            If M.FindElement(strErrors) = Nil Then // Only if no errors.
              Begin
                i := FindTreeItem(strTop);
                If i <> - 1 Then
                  tvExplorer.TopItem := tvExplorer.Items[i];
              End Else
                tvExplorer.TopItem := tvExplorer.Items[0];
            i := FindTreeItem(strSelection);
            If i <> - 1 Then
              tvExplorer.Selected := tvExplorer.Items[i];
          Finally
            FSpecialTagNodes := Nil;
          End;
        Finally
          If M <> Nil Then
            M.AddTickCount('Setup');
          Items.EndUpdate;
        End;
      End;
    M.AddTickCount('Render');
    UpdateStatusBar(M);
  Finally
    FRendering := False;
  End;
End;

(**

  This method outputs the modules information as items in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon Outputs the different parts of the module.

  @param   Container as a TElementContainer

**)
procedure TframeModuleExplorer.OutputModuleInfo(Container : TElementContainer);

Const
  strPromotedLabels : Array[1..4] Of String = (strDocumentationConflicts,
    strHints, strWarnings, strErrors);

var
  i, j: Integer;

begin
  RenderContainers(FModule, Container);
  GetBodyCommentTags(Container As TBaseLanguageModule);
  For j := Low(strPromotedLabels) To High(strPromotedLabels) Do
    For i := 0 To FModule.Count - 1 Do
      If FModule.Item[i].Text = strPromotedLabels[j] Then
        FModule.Item[i].MoveTo(FModule.getFirstChild, naAddFirst);
end;


(**

  This method expands, collapses or delete various nodes depending on the
  options and their contents.

  @precon  None.
  @postcon Expands, collapses or delete various nodes depending on the
           options and their contents.

**)
procedure TframeModuleExplorer.ExpandNodes;

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
  For i := 0 To FModule.Count - 1 Do
    Begin
      If AnsiCompareText(FModule.Item[i].Text, strDocumentationConflicts) = 0 Then
        If doShowConflicts In BrowseAndDocItOptions.Options Then
          FModule.Item[i].Expand(True);
      If AnsiCompareText(FModule.Item[i].Text, strHints) = 0 Then
        If doShowHints In BrowseAndDocItOptions.Options Then
          FModule.Item[i].Expand(True);
      If AnsiCompareText(FModule.Item[i].Text, strWarnings) = 0 Then
        If doShowWarnings In BrowseAndDocItOptions.Options Then
          FModule.Item[i].Expand(True);
      If AnsiCompareText(FModule.Item[i].Text, strErrors) = 0 Then
        If doShowErrors In BrowseAndDocItOptions.Options Then
          FModule.Item[i].Expand(True);
    End;
end;


(**

  This method create both the todo folder node and the document conflict
  folders in the treeview.

  @precon  None.
  @postcon Creates the special tag nodes.

**)
Procedure TframeModuleExplorer.CreateSpecialTagNodes;

Var
  i : Integer;

Begin
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    Begin
      FSpecialTagNodes[i].Node := tvExplorer.Items.AddChild(FModule,
        FSpecialTagNodes[i].strTagDesc);
      FSpecialTagNodes[i].Node.Data := TObject($FFFFFFFF);
      FSpecialTagNodes[i].Node.ImageIndex := Integer(iiToDoFolder) - 1;
      FSpecialTagNodes[i].Node.SelectedIndex := Integer(iiToDoFolder) - 1;
    End;
End;

(**

  This is an on execute event handler for all the scope actions.

  @precon  None.
  @postcon Updates the ScopesToRender set based on the action invoked.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.actLocalExecute(Sender: TObject);

  (**

    This procedure adds or removed the given scope from the ScopesToRender set.

    @precon  None.
    @postcon Adds or removed the given scope from the ScopesToRender set.

    @param   AScope as a TScope

  **)
  procedure UpdateScopes(AScope : TScope);

  Begin
    With BrowseAndDocItOptions Do
      If AScope In ScopesToRender Then
        ScopesToRender := ScopesToRender - [AScope]
      Else
        ScopesToRender := ScopesToRender + [AScope];
  End;

begin
  With BrowseAndDocItOptions Do
    If Sender = actLocal Then
      UpdateScopes(scLocal)
    Else If Sender = actPrivate Then
      UpdateScopes(scPrivate)
    Else If Sender = actProtected Then
      UpdateScopes(scProtected)
    Else If Sender = actPublic Then
      UpdateScopes(scPublic)
    Else If Sender = actPublished Then
      UpdateScopes(scPublished);
  If Assigned(FRefresh) Then
    FRefresh(Sender);
end;

(**

  This is an on update event handler for the sceop actions.

  @precon  None.
  @postcon Checks the action depending on the scopes in ScopesToRender.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.actLocalUpdate(Sender: TObject);
begin
  If Sender = actLocal Then
    (Sender As TAction).Checked := scLocal In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actPrivate Then
    (Sender As TAction).Checked := scPrivate In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actProtected Then
    (Sender As TAction).Checked := scProtected In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actPublic Then
    (Sender As TAction).Checked := scPublic In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actPublished Then
    (Sender As TAction).Checked := scPublished In BrowseAndDocItOptions.ScopesToRender;
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
  @param   SelectType  as a TSelectType
  @return  a TTreeNode

**)
Function TframeModuleExplorer.AddNode(P: TTreeNode; strText: String;
  iLine, iCol, iImageIndex : Integer; Comment: TComment;
  SelectType : TSelectType) : TTreeNode;

Const
  //: A limit to work around a bug in Delphi 5 TreeView
  iTreeLimit = {$IFDEF VER120} 250 {$ELSE} 0 {$ENDIF};

Var
  str : String;

begin
  str := strText;
  If (Length(str) > iTreeLimit) And (iTreeLimit > 0) Then
    str := Copy(str, 1, iTreeLimit);
  Result := tvExplorer.Items.AddChild(P, str);
  Result.Data := TObject(GetNodeComment(iLine, iCol, Comment, SelectType));
  Result.ImageIndex := iImageIndex;
  Result.SelectedIndex := iImageIndex;
end;

(**

  This is a getter method for the TreeNodeInfo property.

  @precon iIndex is the index of the tree node info item required.
  @postcon Returns an instance of the indexed tree node information.

  @param   iIndex as an Integer
  @return  a TTreeNodeInfo

**)
function TframeModuleExplorer.GetTreeNodeInfo(iIndex: Integer): TTreeNodeInfo;
begin
  Result := Nil;
  If (iIndex >= 0) And (iIndex < FNodeInfo.Count) Then
    Result := FNodeInfo[iIndex] As TTreeNodeInfo;
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
procedure TframeModuleExplorer.tvExplorerCustomDrawItem(
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
  DefaultDraw := Not (doCustomDrawing In BrowseAndDocItOptions.Options);
  With Sender.Canvas Do
    If Not DefaultDraw Then
      Begin
        iOffset := GetScrollPos(Sender.Handle, SB_HORZ);
        sl := Tokenize(Node.Text, strKeyWords, BrowseAndDocItOptions.TokenLimit);
        Try
          // Highlight selected item.
          NodeRect := Node.DisplayRect(False);
          Brush.Color := tvExplorer.Color;
          FillRect(NodeRect);
          If cdsSelected In State Then
            Begin
              NodeRect := Node.DisplayRect(True);
              // Need to amend the width of the rectangle for the custom drawing
              iPos := 5;
              For i := 0 To sl.Count - 1 Do
                Begin
                  GetFontInfo(sl, i, Node.Level, Sender.Canvas);
                  Inc(iPos, TextWidth(sl[i]) + 1);
                End;
              NodeRect.Right := NodeRect.Left + iPos;
              If cdsFocused In State Then
                Begin
                  Brush.Color := clInfoBk;
                  FillRect(NodeRect);
                End;
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
              Inc(iPos, TextWidth(sl[i]) + 1);
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
procedure TframeModuleExplorer.tvExplorerMouseMove(Sender: TObject;
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
          If doShowCommentHints In BrowseAndDocItOptions.Options Then
            If Node.Data <> TObject($FFFFFFFF) Then
              C := NodeInfo[Integer(Node.Data)].Comment;
          Rect := FHintWin.CalcHintRect(tvExplorer.ClientWidth, Screen.Width,
            Node, doCustomDrawing In BrowseAndDocItOptions.Options, C);
          If (Rect.Right <= tvExplorer.ClientWidth) And ((C = Nil) Or
            ((C.TokenCount = 0) And (C.TagCount = 0))) Then
            Begin
              FHintWin.ReleaseHandle;
              Exit;
            End;
          Rect.TopLeft := tvExplorer.ClientToScreen(Rect.TopLeft);
          Rect.BottomRight := tvExplorer.ClientToScreen(Rect.BottomRight);
          FHintWin.ActivateHint(Rect, Node,
            doCustomDrawing In BrowseAndDocItOptions.Options, C);
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

  This method update the status of the module explorer statusbar.

  @precon  M must be a valid instance of a TBaseLanguageModule.
  @postcon Update the status of the module explorer statusbar.

  @param   M as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.UpdateStatusBar(M: TBaseLanguageModule);

Var
  strTickLabel: String;
  iTicks: Integer;
  i : Integer;

begin
  If doShowPerformanceCountersInModuleExplorer In BrowseAndDocItOptions.Options Then
    Begin
      stbStatusBar.SimpleText := '';
      For i := 1 To M.OpTickCounts - 1 Do
        Begin
          strTickLabel := M.OpTickCountName[i];
          If strTickLabel <> '' Then
            strTickLabel := strTickLabel + ':';
          iTicks := M.OpTickCountByIndex[i] - M.OpTickCountByIndex[i - 1];
          If iTicks > 0 Then
            Begin
              If stbStatusBar.SimpleText <> '' Then
                stbStatusBar.SimpleText := stbStatusBar.SimpleText + ', ';
              stbStatusBar.SimpleText := stbStatusBar.SimpleText +
                Format('%s%d', [strTickLabel, iTicks]);
            End;
        End;
      If stbStatusBar.SimpleText <> '' Then
        stbStatusBar.SimpleText := stbStatusBar.SimpleText + ', ';
      stbStatusBar.SimpleText := stbStatusBar.SimpleText +
        Format('%s: %d', ['Total', M.OpTickCountByIndex[M.OpTickCounts - 1] -
        M.OpTickCountByIndex[0]]);
    End Else
    Begin
      stbStatusBar.SimpleText := Format(strStatusbarText, [
        Int(M.Bytes), Int(M.TokenCount), Int(M.Lines)]);
    End;
end;

(**

  This is a message handler for the mouse leave message. It hides the hint
  window when the mouse leaves the control.

  @precon  Msg is the window message to handle.
  @postcon The is a mouse event event which hides the hint window.

  @param   Msg as a TMessage as a reference

**)
Procedure TframeModuleExplorer.CMMouseLeave(var Msg : TMessage);

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
  iLines : Integer;
  iLine : Integer;
  iHeight: Integer;

Begin
  iLines := 1;
  iLine := 0;
  With Canvas Do
    Begin
      Font.Assign(FTreeView.Font);
      sl := Tokenize(Caption, strKeyWords, BrowseAndDocItOptions.TokenLimit);
      Try
        iPos := 2;
        For i := 0 To sl.Count - 1 Do
          Begin
            If FCustomDraw Then
              GetFontInfo(sl, i, FNodeLevel, Canvas)
            Else
              Begin
                Refresh;
                Font.Color := clInfoText;
                Font.Style := [];
              End;
            If iPos + TextWidth(sl[i]) > Width Then
              Begin
                iPos := 2 + 10; // Indent multiple lines
                Inc(iLines);
              End;
            iLine := (iLines - 1) * TextHeight(sl[i]);
            TextOut(iPos, iLine, sl[i]);
            Inc(iPos, TextWidth(sl[i]) + 1);
          End;
      Finally
        sl.Free;
      End;
      If (FComment <> Nil) And ((FComment.TokenCount > 0) Or
        (FComment.TagCount > 0)) Then
        Begin
          iHeight := TextHeight('Wp');
          FillRect(Rect(0, iHeight + 2 + iLine, Width, Height));
          Pen.Color := clMaroon;
          MoveTo(0, iHeight + 4 + iLine);
          Lineto(Width, iHeight + 4 + iLine);
          Refresh;
          Font.Style := [];
          Font.Color := clNavy;
          str := FComment.AsString(0, MaxInt, False);
          R := Rect(2, iHeight + 6 + iLine, Width - 2, Height);
          iPos := DrawText(Canvas.Handle, PChar(str), -1, R,
            DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly);
          R := Rect(2, iHeight + 6 + iLine + iPos, Width - 4, Height);
          For i := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
            Begin
              If DrawSpecialTag(FComment, BrowseAndDocItOptions.SpecialTags.Names[i]) Then
                Begin
                  Refresh;
                  Font.Style := [fsBold, fsUnderline];
                  Font.Color := clPurple;
                  Inc(R.Top, 5);
                  R := Rect(2, R.Top, Width - 2, Height);
                  str := BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
                  Inc(R.Top, DrawText(Canvas.Handle, PChar(str), -1, R,
                    DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                    DrawTextBiDiModeFlagsReadingOnly) + 1);
                  For j := 0 To FComment.TagCount - 1 Do
                    If AnsiCompareText(BrowseAndDocItOptions.SpecialTags.Names[i], FComment.Tag[j].TagName) = 0 Then
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
  iPos, iMaxPos, iLastMax : Integer;
  i, j : Integer;
  str : String;
  R : TRect;

Begin
  Canvas.Font.Assign(FTreeView.Font);
  Result := Node.DisplayRect(True);
  iMaxPos := Node.TreeView.ScreenToClient(Point(Screen.WorkAreaRect.Right, 0)).X -
    Result.Left;
  If SyntaxHighlight Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := 5;
      sl := Tokenize(Node.Text, strKeyWords, BrowseAndDocItOptions.TokenLimit);
      Try
        iLastmax := 0;
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, Node.Level, Canvas);
            Inc(iPos, Canvas.TextWidth(sl[i]) + 1);
            If iPos > iMaxPos Then
              Begin
                Inc(Result.Bottom, Canvas.TextHeight(sl[i]) + 2);
                iPos := 5 + 10; // indent multiple lines
                iLastMax := iMaxPos;
              End Else
                If iPos > iLastMax Then
                  iLastMax := iPos;
          End;
        Result.Right := Result.Left + iLastMax;
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
        DrawTextBiDiModeFlagsReadingOnly) + 2);
      For i := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
        Begin
          If DrawSpecialTag(Comment, BrowseAndDocItOptions.SpecialTags.Names[i]) Then
            Begin
              Refresh;
              Canvas.Font.Style := [fsBold, fsUnderline];
              R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
              str := BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
              Inc(Result.Bottom, 5 + DrawText(Canvas.Handle, PChar(str), -1, R,
                DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                DrawTextBiDiModeFlagsReadingOnly) + 2);
              For j := 0 To Comment.TagCount - 1 Do
                If AnsiCompareText(BrowseAndDocItOptions.SpecialTags.Names[i], Comment.Tag[j].TagName) = 0 Then
                  Begin
                    Refresh;
                    Canvas.Font.Style := [];
                    R := Rect(Result.Left + 2, 0, Result.Right - 12, 0);
                    str := Comment.Tag[j].AsString(False);
                    Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
                      DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                      DrawTextBiDiModeFlagsReadingOnly) + 2);
                  End;
            End;
        End;
    End;
End;

(**


  This is the constructor method for the TCustomHintWindow class.

  @precon  ATreeView must be a valid instance of the explorer treeview.
  @postcon Ensures the hint can get the treeviews font information.


  @param   AOwner    as a TComponent
  @param   ATreeView as a TTreeView

**)
constructor TCustomHintWindow.Create(AOwner : TComponent; ATreeView: TTreeView);
begin
  Inherited Create(AOwner);
  FTreeView := ATreeView;
end;

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

(**

  This is an on click event handler for the explorer tree view.

  @precon  None.
  @postcon Fires a SelectionChange event for the specifically selected item.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.tvExplorerClick(Sender: TObject);

Var
  N : TTreeNodeInfo;


begin
  If FSelectionChanging Then
    Exit;
  FSelectionChanging := True;
  Try
    If tvExplorer.Selected <> Nil Then
      If Assigned(FSelectionChange) And Not FRendering Then
        Begin
          N := Nil;
          If tvExplorer.Selected.Data <> TObject($FFFFFFFF) Then
            N := NodeInfo[Integer(tvExplorer.Selected.Data)];
          If N <> Nil Then
            If N.Comment = Nil Then
              FSelectionChange(N.Line, N.Col, N.Line, N.Col, N.SelectType)
            Else
              FSelectionChange(N.Line, N.Col, N.Comment.Line, N.Comment.Col,
                N.SelectType);
        End;
  Finally
    FSelectionChanging := False;
  End;
end;

(**


  This method is an on key down event handler for the tree view. 


  @precon  None. 

  @postcon If an on focus event handler is assigned it is fired. 


  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
procedure TframeModuleExplorer.tvExplorerKeyPress(Sender: TObject; var Key: Char);

  begin
  If Key = #13 Then
    Begin
      tvExplorerClick(Sender);
      If Assigned(OnFocus) Then
        FFocus(Sender);
      Key := #0;
    End;

end;

end.
