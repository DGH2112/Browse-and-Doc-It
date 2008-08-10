(**

  This module contains a frame which holds all the functionality of the
  module browser so that it can be independant of the application specifics.

  @Date    10 Aug 2008
  @Author  David Hoyle
  @Version 1.0

  @bug     Option ShowConflicts does not expand that node.

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
    Constructor Create(iLine, iCol : Integer; Comment : TComment;
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
    iCommentCol : Integer; SelectType : TSelectType) Of Object;

  (** This is a frame class to contain all the functionality of the module
      explorer so that it can be placed inside any container required and
      therefore does not need to know about things like BDS 2006 IDEs or
      application specifics. **)
  TframeModuleExplorer = class(TFrame)
    stbStatusBar: TStatusBar;
    ilScopeImages: TImageList;
    tvExplorer: TTreeView;
    procedure tvExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvExplorerClick(Sender: TObject);
    procedure tvExplorerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FModule : TTreeNode;
    FNodeInfo : TObjectList;
    FSelectionChange : TSelectionChange;
    FFocus : TNotifyEvent;
    FExpandedNodes : TStringList;
    FSpecialTagNodes : Array Of TSpecialTagNode;
    FHintWin : TCustomHintWindow;
    FLastNode : TTreeNode;
    FOptionsChange : TNotifyEvent;
    FINIFileName: String;
    FSelectionChanging: Boolean;
    FRendering: Boolean;
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
    Procedure CMMouseLeave(var Msg : TMessage); Message CM_MOUSELEAVE;
    function FindTreeItem(strText: String): Integer;
    procedure GetExpandedNodes;
    function GetNodePath(Node: TTreeNode): String;
    procedure SetExpandedNodes;
    (**
      This property returns the indexed NodeInfo class for the collection.
      @precon  iIndex must eb a valid index.
      @postcon Returns the indexed NodeInfo class for the collection.
      @param   iIndex as an Integer
      @return  a TTreeNodeInfo
    **)
    Property NodeInfo[iIndex : Integer] : TTreeNodeInfo Read GetTreeNodeInfo;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure RenderContainers(RootNode : TTreenode; Container : TElementContainer);
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
    Property OnOptionsChange : TNotifyEvent Read FOptionsChange
      Write FOptionsChange;
  end;

implementation

Uses
  IniFiles, Types, Math, DGHLibrary;

Const
  (** A set of reserved word for the tree view to mark in bold @bug This needs to comes from the parser! **)
  strReservedWords : Array[1..109] Of String = (
    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
    'automated', 'begin', 'case', 'cdecl', 'class', 'const', 'constructor',
    'contains', 'default', 'destructor', 'dispid', 'dispinterface', 'div', 'do',
    'downto', 'dynamic', 'else', 'end', 'except', 'export', 'exports',
    'external', 'far', 'file', 'finalization', 'finally', 'for', 'forward',
    'function', 'goto', 'if', 'implementation', 'implements', 'in', 'index',
    'inherited', 'initialization', 'inline', 'interface', 'is', 'label',
    'library', 'local', 'message', 'mod', 'name', 'near', 'nil', 'nodefault',
    'not', 'object', 'of', 'on', 'or', 'out', 'overload', 'override', 'package',
    'packed', 'pascal', 'private', 'procedure', 'program', 'property',
    'protected', 'public', 'published', 'raise', 'read', 'readonly', 'record',
    'register', 'reintroduce', 'repeat', 'requires', 'resident',
    'resourcestring', 'safecall', 'sealed', 'set', 'shl', 'shr', 'static',
    'stdcall', 'stored', 'string', 'then', 'threadvar', 'to', 'try', 'type',
    'unit', 'until', 'uses', 'var', 'varargs', 'virtual', 'while', 'with',
    'write', 'writeonly', 'xor'
  );
{$R *.dfm}

(**

  This function returns the token type for a given character and last token
  type.

  @precon  Ch is the character for which the token type assessment needs to be
           taken for and LastToken os the type of the last token as this has an
           effect on some characters.
  @postcon Returns the token type for the given character.

  @param   Ch           as a Char
  @param   LastCharType as a TTokenType
  @return  a TTokenType

**)
Function GetTokenType(Ch : Char; LastCharType : TTokenType) : TTokenType;

Begin
  If ch In [#32, #9] Then
    Result := ttWhiteSpace
  Else If ch In ['#', '_', 'a'..'z', 'A'..'Z'] Then
    Begin
      If (LastCharType = ttNumber) And (Ch In ['A'..'F', 'a'..'f']) Then
        Result := ttNumber
      Else
        Result := ttIdentifier;
    End
  Else If ch In ['$', '0'..'9'] Then
    Begin
      Result := ttNumber;
      If LastCharType = ttIdentifier Then
        Result := ttIdentifier;
    End
  Else If ch In [#10, #13] Then
    Result := ttLineEnd
  Else If ch In [''''] Then
    Result := ttStringLiteral
  Else If ch In [#0..#255] - ['#', '_', 'a'..'z', 'A'..'Z', '$', '0'..'9'] Then
    Result := ttSymbol
  Else
    Result := ttUnknown;
End;

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon   strText si the line of text to be tokenised
  @postcon  Returns a new string list of the tokenized string
  @note     The string list returnsed must be destroyed be the calling method.

  @param   strText as a String
  @param   strReservedWords as an Array Of String
  @return  a TStringList

**)
Function Tokenize(strText : String;
  strReservedWords : Array Of String) : TStringList;

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
                Font.Style := [];
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

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment
           and This comment to be added to the node info object.
  @postcon Initialises the class.

  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @param   Comment    as a TComment
  @param   SelectType as a TSelectType

**)
Constructor TTreeNodeInfo.Create(iLine, iCol : Integer; Comment : TComment;
  SelectType : TSelectType);

Begin
  Inherited Create;
  FLine := ILine;
  FCol := iCol;
  FComment := Nil;
  FSelectType := SelectType;
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
  FExpandedNodes := TStringList.Create;
  FExpandedNodes.Sorted := True;
  FExpandedNodes.Duplicates := dupIgnore;
  LoadSettings;
  FHintWin := TCustomHintWindow.Create(Self);
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
  SaveSettings;
  FExpandedNodes.Free;
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

  This method loads the managed expanded nodes list from the registry.

  @precon  None.
  @postcon Loads the managed expanded nodes list from the registry.

**)
procedure TframeModuleExplorer.LoadSettings;

Var
  sl :  TStringList;
  i : Integer;
  iValue : Integer;

begin
  With TIniFile.Create(FINIFileName) Do
    Try
      sl := TStringList.Create;
      Try
        ReadSection('ManagedExpandedNodes', sl);
        For i := 0 To sl.Count - 1 Do
          Begin
            iValue := ReadInteger('ManagedExpandedNodes', sl[i], 0);
            FExpandedNodes.AddObject(sl[i], TObject(iValue));
          End;
      Finally
        sl.Free;
      End;
    Finally
      Free;
    End;
end;

(**

  This method saves the managed expand nodess list to the registry.

  @precon  None.
  @postcon Saves the managed expand nodess list to the registry.

**)
procedure TframeModuleExplorer.SaveSettings;

Var
  i : Integer;

begin
  With TIniFile.Create(FINIFileName) Do
    Try
      EraseSection('ManagedExpandedNodes');
      For i := 0 To FExpandedNodes.Count - 1 Do
        WriteInteger('ManagedExpandedNodes', FExpandedNodes[i],
          Integer(FExpandedNodes.Objects[i]));
    Finally
      Free;
    End;
end;

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
      str := StringReplace(GetNodePath(Node), '=', '|', [rfReplaceAll]);
      If Node.Expanded And (str <> '') Then
        Begin
          If Not FExpandedNodes.Find(str, iIndex) Then
            iIndex := FExpandedNodes.Add(str);
          FExpandedNodes.Objects[iIndex] := TObject(1);
        End Else
          If FExpandedNodes.Find(str, iIndex) Then
            FExpandedNodes.Delete(iIndex);
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
      str := StringReplace(GetNodePath(Node), '=', '|', [rfReplaceAll]);
      If FExpandedNodes.Find(str, j) Then
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

  @param   M                  as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.RenderModule(M : TBaseLanguageModule);

Var
  i : Integer;
  strTop : String;
  strSelection : String;

Begin
  If FRendering Then
    Exit;
  FRendering := True;
  Try
    FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
    With tvExplorer Do
      Begin
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
            If M = Nil Then
              Exit;
            // Create Root Tree Node
            FModule := AddNode(Nil, M.ModuleName, M.ModuleNameLine,
              M.ModuleNameCol, M.ImageIndexAdjustedForScope, M.Comment,
              stIdentifier);
            CreateSpecialTagNodes;
            OutputModuleInfo(M);
            SetExpandedNodes;
            ExpandNodes;
            // Restore top and selected items
            If M.FindElement(strErrorsAndWarnings) = Nil Then // Only if no errors.
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
          Items.EndUpdate;
        End;
      End;
    M.AddTickCount('Render');
    With stbStatusBar Do
      Begin
        SimpleText := '';
        For i := 1 To M.OpTickCounts - 1 Do
          Begin
            If SimpleText <> '' Then SimpleText := SimpleText + ', ';
            SimpleText := SimpleText + Format('%s: %d', [
              M.OpTickCountName[i],
              M.OpTickCountByIndex[i] - M.OpTickCountByIndex[i - 1]]);
          End;
        If SimpleText <> '' Then SimpleText := SimpleText + ', ';
        SimpleText := SimpleText + Format('%s: %d', ['Total',
          M.OpTickCountByIndex[M.OpTickCounts - 1] - M.OpTickCountByIndex[0]]);
      End;
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

begin
  RenderContainers(FModule, Container);
  GetBodyCommentTags(Container As TBaseLanguageModule);
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
  If doShowConflicts In BrowseAndDocItOptions.Options Then
    For i := 0 To FModule.Count -1 Do
      If AnsiCompareText(FModule.Item[i].Text, strDocumentationConflicts) = 0 Then
        Begin
          FModule.Item[i].Expand(True);
          Break;
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
      //: @bug ToDo node has module comment!
      FSpecialTagNodes[i].Node := tvExplorer.Items.AddChild(FModule,
        FSpecialTagNodes[i].strTagDesc);
      FSpecialTagNodes[i].Node.ImageIndex := Integer(iiToDoFolder) - 1;
      FSpecialTagNodes[i].Node.SelectedIndex := Integer(iiToDoFolder) - 1;
    End;
End;

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
        sl := Tokenize(Node.Text, strReservedWords);
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

Begin
  iLines := 1;
  iLine := 0;
  With Canvas Do
    Begin
      sl := Tokenize(Caption, strReservedWords);
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
            Inc(iPos, TextWidth(sl[i]));
          End;
      Finally
        sl.Free;
      End;
      If (FComment <> Nil) And ((FComment.TokenCount > 0) Or
        (FComment.TagCount > 0)) Then
        Begin
          FillRect(Rect(0, 15 + iLine, Width, Height));
          Pen.Color := clMaroon;
          MoveTo(0, 15 + iLine);
          Lineto(Width, 15 + iLine);
          Refresh;
          Font.Style := [];
          Font.Color := clNavy;
          str := FComment.AsString(0, MaxInt, False);
          R := Rect(2, 17 + iLine, Width - 2, Height);
          iPos := DrawText(Canvas.Handle, PChar(str), -1, R,
            DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly);
          R := Rect(2, 17 + iLine + iPos, Width - 4, Height);
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
                    DrawTextBiDiModeFlagsReadingOnly) + 1);
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
  Result := Node.DisplayRect(True);
  iMaxPos := Node.TreeView.ScreenToClient(Point(Screen.WorkAreaRect.Right, 0)).X -
    Result.Left;
  If SyntaxHighlight Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := 5;
      sl := Tokenize(Node.Text, strReservedWords);
      Try
        iLastmax := 0;
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, Node.Level, Canvas);
            Inc(iPos, Canvas.TextWidth(sl[i]));
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
                DrawTextBiDiModeFlagsReadingOnly) + 2);
              For j := 0 To Comment.TagCount - 1 Do
                If AnsiCompareText(SpecialTags.Names[i], Comment.Tag[j].TagName) = 0 Then
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
          N := NodeInfo[Integer(tvExplorer.Selected.Data)];
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
  @param   Key    as a Word as a reference
  @param   Shift  as a TShiftState

**)
procedure TframeModuleExplorer.tvExplorerKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  If Key = 13 Then
    Begin
      tvExplorerClick(Sender);
      If Assigned(OnFocus) Then
        FFocus(Sender);
    End;
end;

end.
