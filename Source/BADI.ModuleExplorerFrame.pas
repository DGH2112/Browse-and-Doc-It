﻿(**

  This module contains a frame which holds all the functionality of the
  module browser so that it can be independent of the application specifics.

  @Author  David Hoyle
  @Version 8.021
  @Date    10 Sep 2023

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit BADI.ModuleExplorerFrame;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  ImgList,
  ComCtrls,
  Contnrs,
  Generics.Collections,
  ActnList,
  ToolWin,
  VirtualTrees,
  StdCtrls,
  ExtCtrls,
  RegularExpressions,
  BADI.Base.Module,
  BADI.Interfaces,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.ModuleExplorer.VirtualStringTree,
  BADI.ModuleExplorer.CustomHintWindow,
  BADI.Comment.Tag,
  BADI.Types;

Type
  (** This is a procedure type for the positioning of the cursor in the
      current module. **)
  TSelectionChange = Procedure(Const iIdentLine, iIdentCol, iCommentLine : Integer) Of Object;

  (** This is a method signature for getting IDE errors for the module explorer to display. **)
  TBADIIDEErrors = Procedure(Const slErrors : TStringList) Of Object;

  (** This is a frame class to contain all the functionality of the module
      explorer so that it can be placed inside any container required and
      therefore does not need to know about things like BDS 2006 IDEs or
      application specifics. **)
  TframeModuleExplorer = class(TFrame)
    stbStatusBar: TStatusBar;
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
    tbtnSeparator: TToolButton;
    tbtnSyntaxHighlight: TToolButton;
    actSyntax: TAction;
    actShowHints: TAction;
    actConflicts: TAction;
    actErrors: TAction;
    actWarnings: TAction;
    actHints: TAction;
    actMethods: TAction;
    actProperties: TAction;
    tbtnShowHints: TToolButton;
    tbtnConflicts: TToolButton;
    tbtnErrors: TToolButton;
    tbtnWarnings: TToolButton;
    tbtnHints: TToolButton;
    tbtnMethods: TToolButton;
    tbtnProperties: TToolButton;
    tbtnSep2: TToolButton;
    tbtnSep3: TToolButton;
    tbtnConstants: TToolButton;
    tbtnVariables: TToolButton;
    tbtnTypes: TToolButton;
    actConstants: TAction;
    actVariables: TAction;
    actTypes: TAction;
    btnChecks: TToolButton;
    actChecks: TAction;
    actMetrics: TAction;
    tbtnMetrics: TToolButton;
    tmFilter: TTimer;
    pmExplorerContext: TPopupMenu;
    actAddToLocalDictionary: TAction;
    actIgnoreSpellingMistake: TAction;
    AddtoDictionary1: TMenuItem;
    IgnoreSpellingMistake1: TMenuItem;
    actAddToProjectDictionary: TAction;
    AddtoProjectDictionary1: TMenuItem;
    procedure actAddToLocalDictionaryExecute(Sender: TObject);
    procedure actAddToProjectDictionaryExecute(Sender: TObject);
    procedure actSpellingUpdate(Sender: TObject);
    procedure actIgnoreSpellingMistakeExecute(Sender: TObject);
    Procedure actLocalUpdate(Sender: TObject);
    Procedure actLocalExecute(Sender: TObject);
    Procedure FilterChange;
    Procedure FrameEnter(Sender: TObject);
    Procedure edtExplorerFilterChange(Sender: TObject);
    Procedure edtExplorerFilterMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer; Var MouseActivate: TMouseActivate);
    Procedure edtExplorerFilterKeyPress(Sender: TObject; Var Key: Char);
    procedure tmFilterTimer(Sender: TObject);
  Strict Private
    Type
      (** This record contains information about the special tag nodes. **)
      TSpecialTagNode = Record
        Node           : PVirtualNode;
        FTagName       : String;
        FTagDesc       : String;
        FTagProperties : TBADITagProperties;
        FFontStyles    : TFontStyles;
        FFontColour    : TColor;
        FBackColour    : TColor;
        FImageIndex    : TBADIImageIndex;
      End;
      (** An enumerate to define the position of the found text when filtering. **)
      TMatchType = (mtNone, mtStart, mtFull, mtEnd, mtMiddle);
      (** A record to define the filter search match information. **)
      TMatchResult = Record
        FMatchType : TMatchType;
        FStart     : Integer;
        FLength    : Integer;
      End;
      (** An enumerate to define the panels on the status bar. @nohints **)
      TStatusPanelIndex = (
        spiBytes,
        spiTokens,
        spiLines,
        spiFourth
      );
  Strict Private
    Const
      (** A constant to define text for checking the rendering height of text. **)
      strNodeHeightTest = 'Ag';
  Strict Private
    { Private declarations }
    FBADIOptions       : IBADIOptions;
    FModule            : PVirtualNode;
    FNodeInfo          : TObjectList;
    FSelectionChange   : TSelectionChange;
    FFocus             : TNotifyEvent;
    FSpecialTagNodes   : Array Of TSpecialTagNode;
    FHintWin           : TBADICustomHintWindow;
    FLastNode          : PVirtualNode;
    FINIFileName       : String;
    FSelectionChanging : Boolean;
    FRendering         : Boolean;
    FRefresh           : TNotifyEvent;
    FExplorer          : TBADIVirtualStringTree;
    FExplorerFilter    : String;
    FFilterRegEx       : TRegEx;
    FMouseEnter        : Boolean;
    FTargetCanvas      : TCanvas;
    FNode              : PVirtualNode;
    FNodeData          : PBADITreeData;
    FTokenFontInfo     : TBADITokenFontInfoTokenSet;
    FBGColour          : TColor;
    FFollowNode        : PVirtualNode;
    FFiltering         : Boolean;
    FLastFilterUpdate  : Int64;
    FLineDocIssues     : TDictionary<Integer,IBADILineDocIssues>;
    FDocIssueTotals    : IBADIDocIssueTotals;
    FIDEErrors         : TBADIIDEErrors;
  private
    procedure DoRefresh(Sender: TObject);
  Strict Protected
    Procedure GetBodyCommentTags(Const Module : TBaseLanguageModule);
    Function  AddNode(Const Parent : PVirtualNode; Const Element : TElementContainer;
      Const iLevel : Integer) : PVirtualNode; Overload;
    Function  AddNode(Const Parent : PVirtualNode; Const Tag : TTag; Const iLevel : Integer;
      Const iImageIndex : Integer; Const TagProperties : TBADITagProperties;
      Const FontStyles : TFontStyles; Const FontColour : TColor; Const BackColour : TColor;
      Const Comment : TComment) : PVirtualNode; Overload;
    Function  AddNode(Const Parent : PVirtualNode; Const strText, strName : String;
      Const iLevel : Integer; Const iImageIndex : Integer;
      Const boolTitle : Boolean = False) : PVirtualNode; Overload;
    Procedure CreateSpecialTagNodes();
    Procedure ExpandNodes;
    Procedure OutputModuleInfo(Const Container: TElementContainer);
    Function FindTreeItem(Const strText: String): PVirtualNode;
    procedure GetExpandedNodes(Const StartNode : PVirtualNode);
    function  GetNodePath(Const Node: PVirtualNode): String;
    procedure SetExpandedNodes(Const StartNode : PVirtualNode);
    Procedure RenderContainers(Const RootNode : PVirtualNode;
      Const Container : TElementContainer; Const iLevel : Integer);
    Procedure UpdateStatusBar(Const Module : TBaseLanguageModule);
    Procedure ManageExpandedNodes;
    Procedure SetStatusPanel(Const ePanel: TStatusPanelIndex; Const strStatusBarText: String;
      Const iValue: Integer);
    Procedure CMMouseLeave(Var Msg : TMessage); Message CM_MOUSELEAVE;
    Procedure DrawSelectedNode(Const sl : TStringList; Const ItemRect : TRect;
      Const iScopeImagesWidth : Integer; Var iPos : Integer); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawHighlightSelectedItem(Const iColour : TColor; Const ItemRect : TRect);
      {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawTree(Var R : TRect); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawVerticalTreeLine(Const iTreeColour : TColor; Const R : TRect);
      {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawTopHalfOfNodeConnector(Const iTreeColour : TColor; Const iCentre : Integer;
      Const R : TRect); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawNodeButton(Const iTreeColour : TColor; Const iCentre : Integer;
      Const R : TRect); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawImage(Var R : TRect); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawTreeText(Const sl : TStringList; Var R : TRect); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure DrawTextToCanvas(Const strText : String; Const R : TRect; Var iTextPos, iTop,
      iLeft : Integer); {$IFNDEF DEBUG} InLine; {$ENDIF}
    Function  InitMatchResult(Const eMatchType : TMatchType; Const iStart,
      iLength: Integer) : TMatchResult; {$IFNDEF DEBUG} InLine; {$ENDIF}
    Function  IsMatched(Const iIndex, iLength: Integer; Const MC: TMatchCollection): TMatchResult;
      {$IFNDEF DEBUG} InLine; {$ENDIF}
    Procedure tvExplorerAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    Procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure tvExplorerClick(Sender: TObject);
    Procedure tvExplorerKeyPress(Sender: TObject; Var Key: Char);
    Procedure tvExplorerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);
    Procedure tvExplorerGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      Var Ghosted: Boolean; Var ImageIndex: TImageIndex);
    Procedure tvExplorerBeforeItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      Var CustomDraw: Boolean);
    Procedure tvExplorerMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Var NodeHeight: Integer);
    Procedure FocusFollowedNode;
    Procedure tvExplorerNodeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Procedure SetExplorerFilter(Const strValue : String);
    Procedure tvExplorerChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Function  MaxLimit(Const Container: TElementContainer) : Integer;
    Function  GetLineDocIssue(Const iLine : Integer) : IBADILineDocIssues;
    Function  GetDocIssueTotals : IBADIDocIssueTotals;
    Procedure LogDocIssueConflict(Const Element : TElementContainer);
    Function  ExtractSpellingWord : String;
    Procedure CheckForIDEErrors(Const Container : TElementContainer);
    Function FollowMethodNodeData(Const iLine : Integer) : PBADITreeData;
    procedure PromoteLabels;
    (**
      This property gets and set the filter text for the explorer view.
      @precon  None.
      @postcon Gets and set the filter text for the explorer view.
      @return  a String
    **)
    Property ExplorerFilter : String Read FExplorerFilter Write SetExplorerFilter;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Procedure RenderModule(Const Module : TBaseLanguageModule);
    Procedure FollowEditorCursor(Const iLine : Integer);
    (**
      This is an event for the selection change in the browser tree.
      @precon  None.
      @postcon Hooks an event handler for the On Selection change event.
      @return  a TSelectionChange
    **)
    Property OnSelectionChange : TSelectionChange Read FSelectionChange Write FSelectionChange;
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
    (**
      This is an event handler for the IDE Errors.
      @precon  None.
      @postcon Implement a event handler for add errors to the module explorer.
      @return  a TBADIIDEErrors
    **)
    Property OnIDEErrors : TBADIIDEErrors Read FIDEErrors Write FIDEErrors;
    (**
      This property exposes the virtual tree view to outside sources.
      @precon  None.
      @postcon Exposes the virtual tree view to outside sources.
      @return  a TBADIVirtualStringTree
    **)
    Property Explorer : TBADIVirtualStringTree Read FExplorer;
    (**
      This property returns the limit types associated with the given line number.
      @precon  None.
      @postcon Returns the limit types associated with the given line number.
      @param   iLine as an Integer as a constant
      @return  a IBADILineDocIssues
    **)
    Property LineDocIssues[Const iLine : Integer] : IBADILineDocIssues Read GetLineDocIssue;
    (**
      This property returns the document issue totals for the parsed module.
      @precon  None.
      @postcon Returns the document issue totals for the parsed module.
      @return  an IBADIDocIssueTotals
    **)
    Property DocIssueTotals : IBADIDocIssueTotals Read GetDocIssueTotals;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Types,
  Math,
  BADI.Generic.Tokenizer,
  {$IFDEF DXE20}
  UITypes,
  {$ENDIF}
  RegularExpressionsCore,
  BADI.Options,
  BADI.Constants,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.ModuleExplorer.TreeNodeInfo,
  BADI.DocIssue,
  BADI.LineDocIssue,
  BADI.DocIssueTotals,
  BADI.SpellingIssue;

Resourcestring
  (** A format pattern for the bytes statusbar text. **)
  strStatusbarBytesText = '%1.0n Bytes';
  (** A format pattern for the tokens statusbar text. **)
  strStatusbarTokensText = '%1.0n Tokens';
  (** A format pattern for the lines status bar panel. **)
  strStatusbarLinesText = '%1.0n Lines.';
  (** A timing label for clearing the treeview. **)
  strClear = 'Clear';
  (** A timing label for building the treeview. **)
  strBuild = 'Build';
  (** A timing label for Setting up the treeview. **)
  strSetup = 'Setup';
  (** A timing label for rendering the treeview. **)
  strRender = 'Render';
  (** A timing label for the total rendering of the treeview. **)
  strTotal = 'Total';
  (** A message to show in the treeview for when BADI cannot parse a particular file. **)
  strBrowseAndDocItCannotParse = 'Browse and Doc It cannot parser this type of file!';

Const
  (** This default font name for the treeview. **)
  strTahomaFontName = 'Tahoma';
  (** This size of the node connector in the treeview. **)
  iNodeSize = 8;
  (** A divisor for calculating. **)
  iDivisor = 2;

{$R *.dfm}

(**

  This is an on execute event handler for the Add to Dictionary action.

  @precon  None.
  @postcon Adds the selected word to the local dictionary.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.actAddToLocalDictionaryExecute(Sender: TObject);

Begin
  TBADIOptions.BADIOptions.LocalDictionary.Add(ExtractSpellingWord);
  DoRefresh(Sender);
End;

(**

  This is an on execute event handler for the Add To Project Dictionary action.

  @precon  None.
  @postcon The selected spelling mistake is added to the project dictionary.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.actAddToProjectDictionaryExecute(Sender: TObject);

Begin
  TBADIOptions.BADIOptions.ProjectDictionary.Add(ExtractSpellingWord);
  DoRefresh(Sender);
End;

(**

  This is an on execute event handler for the Ignore Spelling Mistake action.

  @precon  None.
  @postcon Adds the selected word to the ignore dictionary.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.actIgnoreSpellingMistakeExecute(Sender: TObject);

Begin
  TBADIOptions.BADIOptions.IgnoreDictionary.Add(ExtractSpellingWord);
  DoRefresh(Sender);
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

    @param   AScope as a TScope as a constant

  **)
  Procedure UpdateScopes(Const AScope : TScope);

  Begin
    If AScope In FBADIOptions.ScopesToRender Then
      FBADIOptions.ScopesToRender := FBADIOptions.ScopesToRender - [AScope]
    Else
      FBADIOptions.ScopesToRender := FBADIOptions.ScopesToRender + [AScope];
  End;

  (**

    This procedure adds or removed the given option from the options set.

    @precon  None.
    @postcon Adds or removed the given option from the Options set.

    @param   Option as a TDocOption as a constant

  **)
  procedure UpdateOptions(Const Option : TDocOption);

  Begin
    If Option In FBADIOptions.Options Then
      FBADIOptions.Options := FBADIOptions.Options - [Option]
    Else
      FBADIOptions.Options := FBADIOptions.Options + [Option];
  End;

begin
  If Sender = actLocal Then
    UpdateScopes(scLocal)
  Else If Sender = actPrivate Then
    UpdateScopes(scPrivate)
  Else If Sender = actProtected Then
    UpdateScopes(scProtected)
  Else If Sender = actPublic Then
    UpdateScopes(scPublic)
  Else If Sender = actPublished Then
    UpdateScopes(scPublished)
  Else If Sender = actSyntax Then
    UpdateOptions(doCustomDrawing)
  Else If Sender = actShowHints Then
    UpdateOptions(doShowCommentHints)
  Else If Sender = actConflicts Then
    UpdateOptions(doShowConflicts)
  Else If Sender = actChecks Then
    UpdateOptions(doShowChecks)
  Else If Sender = actMetrics Then
    UpdateOptions(doShowMetrics)
  Else If Sender = actErrors Then
    UpdateOptions(doShowErrors)
  Else If Sender = actWarnings Then
    UpdateOptions(doShowWarnings)
  Else If Sender = actHints Then
    UpdateOptions(doShowHints)
  Else If Sender = actMethods Then
    UpdateOptions(doShowMethodMissingDocs)
  Else If Sender = actProperties Then
    UpdateOptions(doShowPropertyMissingDoc)
  Else If Sender = actConstants Then
    UpdateOptions(doShowUndocumentedConsts)
  Else If Sender = actVariables Then
    UpdateOptions(doShowUndocumentedVars)
  Else If Sender = actTypes Then
    Begin
      UpdateOptions(doShowUndocumentedTypes);
      UpdateOptions(doShowUndocumentedRecords);
      UpdateOptions(doShowUndocumentedObjects);
      UpdateOptions(doShowUndocumentedClasses);
      UpdateOptions(doShowUndocumentedInterfaces);
    End;
  DoRefresh(Sender);
end;

(**

  This is an on update event handler for the scope actions.

  @precon  None.
  @postcon Checks the action depending on the scopes in ScopesToRender.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.actLocalUpdate(Sender: TObject);

begin
  If Sender = actLocal Then
    (Sender As TAction).Checked := scLocal In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actPrivate Then
    (Sender As TAction).Checked := scPrivate In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actProtected Then
    (Sender As TAction).Checked := scProtected In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actPublic Then
    (Sender As TAction).Checked := scPublic In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actPublished Then
    (Sender As TAction).Checked := scPublished In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actSyntax Then
    (Sender As TAction).Checked := doCustomDrawing In TBADIOptions.BADIOptions.Options
  Else If Sender = actShowHints Then
    (Sender As TAction).Checked := doShowCommentHints In TBADIOptions.BADIOptions.Options
  Else If Sender = actConflicts Then
    (Sender As TAction).Checked := doShowConflicts In TBADIOptions.BADIOptions.Options
  Else If Sender = actChecks Then
    (Sender As TAction).Checked := doShowChecks In TBADIOptions.BADIOptions.Options
  Else If Sender = actMetrics Then
    (Sender As TAction).Checked := doShowMetrics In TBADIOptions.BADIOptions.Options
  Else If Sender = actErrors Then
    (Sender As TAction).Checked := doShowErrors In TBADIOptions.BADIOptions.Options
  Else If Sender = actWarnings Then
    (Sender As TAction).Checked := doShowWarnings In TBADIOptions.BADIOptions.Options
  Else If Sender = actHints Then
    (Sender As TAction).Checked := doShowHints In TBADIOptions.BADIOptions.Options
  Else If Sender = actMethods Then
    (Sender As TAction).Checked := doShowMethodMissingDocs In TBADIOptions.BADIOptions.Options
  Else If Sender = actProperties Then
    (Sender As TAction).Checked := doShowPropertyMissingDoc In TBADIOptions.BADIOptions.Options
  Else If Sender = actConstants Then
    (Sender As TAction).Checked := doShowUndocumentedConsts In TBADIOptions.BADIOptions.Options
  Else If Sender = actVariables Then
    (Sender As TAction).Checked := doShowUndocumentedVars In TBADIOptions.BADIOptions.Options
  Else If Sender = actTypes Then
    (Sender As TAction).Checked := doShowUndocumentedTypes In TBADIOptions.BADIOptions.Options
end;

(**

  This is an on update event handler for the Spelling actions.

  @precon  None.
  @postcon Enables the context menus for spelling actions.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.actSpellingUpdate(Sender: TObject);

Var
  A: TAction;
  boolEnabled : Boolean;
  NodeData : PBADITreeData;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      boolEnabled := A.Enabled;
      If Assigned(FExplorer.FocusedNode) Then
        Begin
          NodeData := FExplorer.GetNodeData(FExplorer.FocusedNode);
          boolEnabled := (NodeData.FNodeType = ntSpellingIssue)
        End;
      If boolEnabled <> A.Enabled Then
        A.Enabled := boolEnabled;
    End;
End;

(**

  This method adds a node to the treeview as a child of the give node. It assigns the line, column
  and comment information to the Node.

  @precon  P is the parent node to attach this new child too, Element is the parser node to render.
  @postcon Returns a instance of the newly add / created tree node.

  @param   Parent      as a PVirtualNode as a constant
  @param   strText     as a String as a constant
  @param   strName     as a String as a constant
  @param   iLevel      as an Integer as a constant
  @param   iImageIndex as an Integer as a constant
  @param   boolTitle   as a Boolean as a constant
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.AddNode(Const Parent : PVirtualNode; Const strText, strName : String;
  Const iLevel : Integer; Const iImageIndex : Integer;
  Const boolTitle : Boolean = False) : PVirtualNode;

Var
  NodeData : PBADITreeData;
  N : TBADITreeNodeInfo;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AddNode', tmoTiming);{$ENDIF}
  Result := FExplorer.AddChild(Parent);
  FExplorer.MultiLine[Result] := True;
  NodeData := FExplorer.GetNodeData(Result);
  N := TBADITreeNodeInfo.Create(strText, strName, iLevel, iImageIndex, boolTitle);
  FNodeInfo.Add(N);
  NodeData.FNode := N;
  FExplorer.InvalidateNode(Result); //: @note Used to recalc node height due to bug in VTV
End;

(**

  This method adds a node to the treeview as a child of the give node. It assigns the line, column
  and comment information to the Node.

  @precon  P is the parent node to attach this new child too, Element is the parser node to render.
  @postcon Returns a instance of the newly add / created tree node.

  @param   Parent  as a PVirtualNode as a constant
  @param   Element as a TElementContainer as a constant
  @param   iLevel  as an Integer as a constant
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.AddNode(Const Parent : PVirtualNode;
  Const Element : TElementContainer; Const iLevel : Integer) : PVirtualNode;

Var
  NodeData : PBADITreeData;
  N : TBADITreeNodeInfo;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AddNode', tmoTiming);{$ENDIF}
  Result := FExplorer.AddChild(Parent);
  FExplorer.MultiLine[Result] := True;
  NodeData := FExplorer.GetNodeData(Result);
  If Element Is TDocumentConflict Then
    NodeData.FNodeType := ntDocConflict
  Else If Element Is TDocIssue Then
    NodeData.FNodeType := ntDocIssue
  Else If Element Is TBADISpellingIssue Then
    NodeData.FNodeType := ntSpellingIssue
  Else
    NodeData.FNodeType := ntElement;
  N := TBADITreeNodeInfo.Create(Element, iLevel);
  FNodeInfo.Add(N);
  NodeData.FNode := N;
  FExplorer.InvalidateNode(Result); //: @note Used to recalc node height due to bug in VTV
end;

(**

  This method adds a node to the treeview as a child of the give node. It assigns the line, column and 
  comment information to the Node.

  @precon  P is the parent node to attach this new child too, Element is the parser node to render.
  @postcon Returns a instance of the newly add / created tree node.

  @param   Parent        as a PVirtualNode as a constant
  @param   Tag           as a TTag as a constant
  @param   iLevel        as an Integer as a constant
  @param   iImageIndex   as an Integer as a constant
  @param   TagProperties as a TBADITagProperties as a constant
  @param   FontStyles    as a TFontStyles as a constant
  @param   FontColour    as a TColor as a constant
  @param   BackColour    as a TColor as a constant
  @param   Comment       as a TComment as a constant
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.AddNode(Const Parent: PVirtualNode; Const Tag: TTag; Const iLevel,
  iImageIndex: Integer; Const TagProperties : TBADITagProperties; Const FontStyles : TFontStyles;
  Const FontColour : TColor; Const BackColour : TColor; Const Comment: TComment): PVirtualNode;

Var
  NodeData : PBADITreeData;
  N : TBADITreeNodeInfo;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AddNode', tmoTiming);{$ENDIF}
  Result := FExplorer.AddChild(Parent);
  FExplorer.MultiLine[Result] := True;
  NodeData := FExplorer.GetNodeData(Result);
  N := TBADITreeNodeInfo.Create(Tag, iLevel, iImageIndex, TagProperties, FontStyles, FontColour,
    BackColour, Comment);
  FNodeInfo.Add(N);
  NodeData.FNode := N;
  FExplorer.InvalidateNode(Result); //: @note Used to recalc node height due to bug in VTV
End;

(**

  This method checks the if the IDE has any error messages for the current module and adds them to the
  list of errors.

  @precon  Container must be a valid instance.
  @postcon Any IDE errors are added to the module.

  @param   Container as a TElementContainer as a constant

**)
Procedure TframeModuleExplorer.CheckForIDEErrors(Const Container: TElementContainer);

Type
  //: @nohints
  TBADIErrorFields = (efFilename, efMessage, efLine, efColumn);

ResourceString
  strMsg = '[DCC32] %s found at line %d column %d.';
  
Var
  Errors: TElementContainer;
  sl: TStringList;
  strError: String;
  astrError: TArray<String>;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckForIDEErrors', tmoTiming);{$ENDIF}
  Errors := Container.FindElement(strErrors);
  If Not Assigned(Errors) And (doShowIDEErrors In TBADIOptions.BADIOPtions.Options) Then
    If Assigned(FIDEErrors) Then
      Begin
        sl := TStringList.Create;
        Try
          FIDEErrors(sl);
          For strError In sl Do
            Begin
              astrError := strError.Split(['|']);
              Container.AddIssue(
                Format(strMsg, [
                  astrError[Integer(efMessage)], 
                  astrError[Integer(efLine)].ToInteger,
                  astrError[Integer(efColumn)].ToInteger + 1
                ]),
                scGlobal,
                astrError[Integer(efLine)].ToInteger,
                astrError[Integer(efColumn)].ToInteger + 1,
                etError,
                Container
              );
            End;
        Finally
          sl.Free;
        End;
      End;
End;

(**

  This is a message handler for the mouse leave message. It hides the hint
  window when the mouse leaves the control.

  @precon  Msg is the window message to handle.
  @postcon The is a mouse event event which hides the hint window.

  @nohint  Msg

  @param   Msg as a TMessage as a reference

**)
Procedure TframeModuleExplorer.CMMouseLeave(Var Msg : TMessage);

Begin
  FHintWin.ReleaseHandle;
End;

(**

  This is the constructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon Initialises the class.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeModuleExplorer.Create(AOwner: TComponent);

Const
  iDefaultFontHeight = -11;
  iTabOrder = 3;
  
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create(AOwner);
  {$IFDEF D2009}
  DoubleBuffered := True;
  {$ENDIF}
  FBADIOptions := TBADIOptions.BADIOptions;
  FExplorer := TBADIVirtualStringTree.Create(Self);
  FExplorer.Parent := Self;
  FExplorer.Align := alClient;
  FExplorer.Header.AutoSizeIndex := 0;
  FExplorer.Header.Font.Charset := DEFAULT_CHARSET;
  FExplorer.Header.Font.Color := clWindowText;
  FExplorer.Header.Font.Height := iDefaultFontHeight;
  FExplorer.Header.Font.Name := strTahomaFontName;
  FExplorer.Header.Font.Style := [];
  FExplorer.Header.MainColumn := -1;
  FExplorer.Header.Options := [hoColumnResize, hoDrag];
  FExplorer.Images := TBADIOptions.BADIOptions.ScopeImageList;
  FExplorer.TabOrder := iTabOrder;
  FExplorer.TreeOptions.MiscOptions := FExplorer.TreeOptions.MiscOptions + [toVariableNodeHeight];
  FExplorer.TreeOptions.SelectionOptions := FExplorer.TreeOptions.SelectionOptions + [toRightClickSelect];
  FExplorer.EmptyListMessage := strBrowseAndDocItCannotParse;
  FExplorer.OnAfterCellPaint := tvExplorerAfterCellPaint;
  FExplorer.OnBeforeItemPaint := tvExplorerBeforeItemPaint;
  FExplorer.OnClick := tvExplorerClick;
  FExplorer.OnGetImageIndex := tvExplorerGetImageIndex;
  FExplorer.OnKeyPress := tvExplorerKeyPress;
  FExplorer.OnMeasureItem := tvExplorerMeasureItem;
  FExplorer.OnMouseMove := tvExplorerMouseMove;
  FExplorer.OnExpanded := tvExplorerNodeExpanded;
  FExplorer.OnCollapsed := tvExplorerNodeExpanded;
  FExplorer.OnChange := tvExplorerChange;
  FExplorer.PopupMenu := pmExplorerContext;
  FMouseEnter := False;
  FFiltering := False;
  FExplorer.NodeDataSize := SizeOf(TBADITreeData);
  FINIFileName := TBADIOptions.BADIOptions.INIFileName;
  FNodeInfo := TObjectList.Create(True);
  FHintWin := TBADICustomHintWindow.Create(Self, FExplorer);
  FHintWin.Color := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours][ttExplorerHighlight].FBackColour;
  FHintWin.Canvas.Font.Assign(FExplorer.Font);
  FLastFilterUpdate := 0;
  FExplorer.OnGetText := tvExplorerGetText;
  FLineDocIssues := TDictionary<Integer,IBADILineDocIssues>.Create;
  FDocIssueTotals := TBADIDocIssueTotals.Create;
end;

(**

  This method create the special tags folder nodes and the document conflict folders in the treeview.

  @precon  None.
  @postcon Creates the special tag nodes.

**)
Procedure TframeModuleExplorer.CreateSpecialTagNodes();

Var
  i : Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CreateSpecialTagNodes', tmoTiming);{$ENDIF}
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    Begin
      FSpecialTagNodes[i].FTagName := TBADIOptions.BADIOptions.SpecialTags[i].FName;
      FSpecialTagNodes[i].FTagDesc := TBADIOptions.BADIOptions.SpecialTags[i].FDescription;
      FSpecialTagNodes[i].FTagProperties := TBADIOptions.BADIOptions.SpecialTags[i].FTagProperties;
      FSpecialTagNodes[i].FFontStyles := TBADIOptions.BADIOptions.SpecialTags[i].FFontStyles;
      FSpecialTagNodes[i].FFontColour := TBADIOptions.BADIOptions.SpecialTags[i].FFontColour;
      FSpecialTagNodes[i].FBackColour := TBADIOptions.BADIOptions.SpecialTags[i].FBackColour;
      FSpecialTagNodes[i].FImageIndex := TBADIOptions.BADIOptions.SpecialTags[i].FIconImage;
      FSpecialTagNodes[i].Node := AddNode(
        FModule,
        FSpecialTagNodes[i].FTagDesc,
        FSpecialTagNodes[i].FTagName,
        1,
        BADIImageIndex(TBADIOptions.BADIOptions.SpecialTags[i].FIconImage, scNone),
        True
      );
    End;
End;

(**

  This is the destructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon destroy the instance of the dockable form.

**)
Destructor TframeModuleExplorer.Destroy;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FLineDocIssues.Free;
  If FModule <> Nil Then
    GetExpandedNodes(FModule);
  FExplorer.Free;
  FSpecialTagNodes := Nil;
  ManageExpandedNodes;
  FHintWin.Free;
  FNodeInfo.Free;
  Inherited;
end;

(**

  This method refreshes the module explorer by asking for the module to be re-parsed.

  @precon  None.
  @postcon The module is re-parsed.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.DoRefresh(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DoRefresh', tmoTiming);{$ENDIF}
  If Assigned(FRefresh) Then
    FRefresh(Sender);
End;

(**

  This method highlights the selected item in the treeview.

  @precon  None.
  @postcon The item is highlighted if selected.

  @param   iColour      as a TColor as a constant
  @param   ItemRect     as a TRect as a constant

**)
Procedure TframeModuleExplorer.DrawHighlightSelectedItem(Const iColour : TColor;
  Const ItemRect : TRect);

Begin
  FTargetCanvas.Brush.Color := iColour;
  FTargetCanvas.FillRect(ItemRect);
End;

(**

  This method draws the icon for the current tree node.

  @precon  None.
  @postcon The nodes icon is drawn.

  @param   R            as a TRect as a reference

**)
Procedure TframeModuleExplorer.DrawImage(Var R : TRect);

Begin
  R.Left := R.Left + Integer(FExplorer.Indent) + FExplorer.Margin;
  Inc(R.Top);
  FExplorer.Images.Draw(
    FTargetCanvas,
    R.Left,
    ((R.Bottom - R.Top) - FExplorer.Images.Height) Div iDivisor,
    FNodeData.FNode.ImageIndex
  );
End;

(**

  This method draws the tree node button containing the + or - signs for expanding and collapsing .

  @precon  None.
  @postcon The button is drawn.

  @param   iTreeColour  as a TColor as a constant
  @param   iCentre      as an Integer as a constant
  @param   R            as a TRect as a constant

**)
Procedure TframeModuleExplorer.DrawNodeButton(Const iTreeColour : TColor; Const iCentre : Integer;
  Const R : TRect);

Const
  iButtonTop = -2;
  iButtonBottom = 3;
  iButtonLeft = 6;
  iBUttonRight = 11;

Begin
  If FNode.ChildCount > 0 Then
    Begin
      // Draw button
      FTargetCanvas.Pen.Color := iTreeColour;
      FTargetCanvas.Pen.Style := psSolid;
      FTargetCanvas.Rectangle(R.Left + iNodeSize Div iDivisor, iCentre - iNodeSize Div iDivisor,
        R.Left + iNodeSize + iNodeSize Div iDivisor + 1, iCentre + iNodeSize Div iDivisor + 1);
      // Draw negative side
      FTargetCanvas.Pen.Color := iTreeColour;
      FTargetCanvas.MoveTo(R.Left + iButtonLeft, iCentre);
      FTargetCanvas.LineTo(R.Left + iBUttonRight, iCentre);
      If Not FExplorer.Expanded[FNode] Then
        Begin
          // Make positive sign
          FTargetCanvas.MoveTo(R.Left + iNodeSize, iCentre + iButtonTop);
          FTargetCanvas.LineTo(R.Left + iNodeSize, iCentre + iButtonBottom);
        End;
    End;
End;

(**

  This method renders a selection rectangle around the selected note.

  @precon  sl must contain the string tokens to be rendered.
  @postcon The selection rectangle is rendered (accounting for extra width due to font
           preferences).

  @param   sl                as a TStringList as a constant
  @param   ItemRect          as a TRect as a constant
  @param   iScopeImagesWidth as an Integer as a constant
  @param   iPos              as an Integer as a reference

**)
Procedure TframeModuleExplorer.DrawSelectedNode(Const sl : TStringList;
  Const ItemRect : TRect; Const iScopeImagesWidth : Integer; Var iPos : Integer);

Const
  iLeft = 6;

Var
  R : TRect;
  HL: TTokenFontInfo;
  i: Integer;
  iRight : Integer;

Begin
  If FExplorer.Selected[FNode] Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := iLeft;
      iRight := iPos;
      InitCanvasFont(FTargetCanvas, tpFixed In FNodeData.FNode.TagProperties, FBADIOptions);
      For i := 0 To sl.Count - 1 Do
        Begin
          GetFontInfo(sl, i, FNodeData.FNode.Title, tpSyntax In FNodeData.FNode.TagProperties,
            FNodeData.FNode.ForeColour, FNodeData.FNode.BackColour, FNodeData.FNode.FontStyles,
            FTokenFontInfo, FBGColour, FTargetCanvas);
          If sl[i] = #13#10 Then
            iRight := iLeft
          Else
            Inc(iRight, FTargetCanvas.TextWidth(sl[i]));
          If iRight > iPos Then
            iPos := iRight;
        End;
      R := ItemRect;
      R.Left := (FNodeData.FNode.Level + 1) * Integer(FExplorer.Indent) -
        FExplorer.Left + iScopeImagesWidth + FExplorer.Margin + FExplorer.TextMargin - 1;
      R.Right := R.Left + iPos;
      FTargetCanvas.Pen.Color := clBlack;
      HL := FTokenFontInfo[ttExplorerHighlight];
      If FNode = FExplorer.FocusedNode Then
        Begin
          FTargetCanvas.Brush.Color := HL.FBackColour;
          FTargetCanvas.Pen.Color := HL.FForeColour;
          FTargetCanvas.FillRect(R);
        End;
      FTargetCanvas.Rectangle(R);
    End;
End;

(**

  This method draws the given text to the canvas using the current R rectangle and the current iPos
  for the left of the text.

  @precon  None.
  @postcon Draws the text and updates the iPos pixel position for the next token and the iTextPos
           to point to the net character in the string to draw.

  @param   strText      as a String as a constant
  @param   R            as a TRect as a constant
  @param   iTextPos     as an Integer as a reference
  @param   iTop         as an Integer as a reference
  @param   iLeft        as an Integer as a reference

**)
Procedure TframeModuleExplorer.DrawTextToCanvas(Const strText : String; Const R : TRect;
  Var iTextPos, iTop, iLeft : Integer);

Const
  iPadding = 2;
  
Var
  S : TRect;

Begin
  S := R;
  S.Top := iTop;
  S.Left := iLeft;
  Windows.DrawText(FTargetCanvas.Handle, PChar(strText), Length(strText), S,
    DT_LEFT Or DT_VCENTER);
  Inc(iLeft, FTargetCanvas.TextWidth(strText));
  Inc(iTextPos, Length(strText));
  If strText = #13#10 Then
    Begin
      iLeft := R.Left + iPadding;
      Inc(iTop, FTargetCanvas.TextHeight(strNodeHeightTest));
    End;
End;

(**

  This method draws the top half of the node connector.

  @precon  None.
  @postcon The top half of the node connector is drawn.

  @param   iTreeColour  as a TColor as a constant
  @param   iCentre      as an Integer as a constant
  @param   R            as a TRect as a constant

**)
Procedure TframeModuleExplorer.DrawTopHalfOfNodeConnector(Const iTreeColour : TColor;
  Const iCentre : Integer; Const R : TRect);

Begin
  FTargetCanvas.Pen.Color := iTreeColour;
  FTargetCanvas.Pen.Style := psSolid;
  FTargetCanvas.MoveTo(R.Left + iNodeSize, iCentre);
  FTargetCanvas.LineTo(R.Left + Integer(FExplorer.Indent), iCentre);
  If FNode.Parent <> Nil Then
    Begin
      // Draw connection to item
      FTargetCanvas.Pen.Color := iTreeColour;
      FTargetCanvas.Pen.Style := psSolid;
      FTargetCanvas.MoveTo(R.Left + iNodeSize, R.Top);
      FTargetCanvas.LineTo(R.Left + iNodeSize, iCentre);
      If FNode.Index < FNode.Parent.ChildCount - 1 Then
        Begin
          // Draw connector to next FNode.
          FTargetCanvas.Pen.Color := iTreeColour;
          FTargetCanvas.Pen.Style := psSolid;
          FTargetCanvas.MoveTo(R.Left + iNodeSize, iCentre);
          FTargetCanvas.LineTo(R.Left + iNodeSize, R.Bottom);
        End;
    End;
End;

(**

  This method draws the tree outline for the given node.

  @precon  None.
  @postcon The tree outline is drawn.

  @param   R        as a TRect as a reference

**)
Procedure TframeModuleExplorer.DrawTree(Var R : TRect);

Var
  iTreeColour: Integer;
  iCentre: Integer;

Begin
  R.Left := R.Left + (FNodeData.FNode.Level * Integer(FExplorer.Indent)) - FExplorer.Left;
  iTreeColour := TBADIOptions.BADIOptions.TreeColour;
  iCentre := (R.Top + R.Bottom) Div iDivisor;
  DrawVerticalTreeLine(iTreeColour, R);
  DrawTopHalfOfNodeConnector(iTreeColour, iCentre, R);
  DrawNodeButton(iTreeColour, iCentre, R);
End;

(**

  This procedure draws the text on the explorer tree view.

  @precon  NodeData and sl must be valid instance.
  @postcon The tree node text is drawn.

  @param   sl as a TStringList as a constant
  @param   R  as a TRect as a reference

**)
Procedure TframeModuleExplorer.DrawTreeText(Const sl : TStringList; Var R : TRect);

Const
  iPadding = 2;
  
Var
  iLeft, iTop : Integer;
  i: Integer;
  iTextPos: Integer;
  MC: TMatchCollection;
  MR: TMatchResult;
  iColour: TColor;

Begin
  R.Left := R.Left + FExplorer.Images.Width + FExplorer.TextMargin;
  iTop := R.Top;
  iLeft := R.Left + iPadding;
  iTextPos := 1;
  InitCanvasFont(FTargetCanvas, tpFixed In FNodeData.FNode.TagProperties, FBADIOptions);
  If ExplorerFilter <> '' Then
    MC := FFilterRegEx.Matches(FNodeData.FNode.Text);
  For i := 0 To sl.Count - 1 Do
    Begin
      GetFontInfo(sl, i, FNodeData.FNode.Title, tpSyntax In FNodeData.FNode.TagProperties,
        FNodeData.FNode.ForeColour, FNodeData.FNode.BackColour, FNodeData.FNode.FontStyles,
        FTokenFontInfo, FBGColour, FTargetCanvas);
      If FNode = FExplorer.FocusedNode Then
        If FTargetCanvas.Brush.Color = FBGColour Then
          FTargetCanvas.Brush.Color :=
            FTokenFontInfo[ttExplorerHighlight].FBackColour;
      If ExplorerFilter = '' Then
        DrawTextToCanvas(sl[i], R, iTextPos, iTop, iLeft)
      Else
        Begin
          MR := IsMatched(iTextPos, Length(sl[i]), MC);
          Case MR.FMatchType Of
            mtNone: DrawTextToCanvas(sl[i], R, iTextPos, iTop, iLeft);
            mtStart:
              Begin
                iColour := FTargetCanvas.Brush.Color;
                FTargetCanvas.Brush.Color := FTokenFontInfo[ttSearchHighlight].FForeColour;
                DrawTextToCanvas(Copy(sl[i], 1, MR.FLength), R, iTextPos, iTop, iLeft);
                FTargetCanvas.Brush.Color := iColour;
                DrawTextToCanvas(Copy(sl[i], MR.FLength + 1, Length(sl[i]) - MR.FLength), R,
                  iTextPos, iTop, iLeft);
              End;
            mtFull:
              Begin
                FTargetCanvas.Brush.Color := FTokenFontInfo[ttSearchHighlight].FForeColour;
                DrawTextToCanvas(sl[i], R, iTextPos, iTop, iLeft)
              End;
            mtEnd:
              Begin
                DrawTextToCanvas(Copy(sl[i], 1, MR.FStart - 1), R, iTextPos, iTop, iLeft);
                FTargetCanvas.Brush.Color := FTokenFontInfo[ttSearchHighlight].FForeColour;
                DrawTextToCanvas(Copy(sl[i], MR.FStart, MR.FLength), R, iTextPos, iTop, iLeft);
              End;
            mtMiddle:
              Begin
                DrawTextToCanvas(Copy(sl[i], 1, MR.FStart - 1), R, iTextPos, iTop, iLeft);
                iColour := FTargetCanvas.Brush.Color;
                FTargetCanvas.Brush.Color := FTokenFontInfo[ttSearchHighlight].FForeColour;
                DrawTextToCanvas(Copy(sl[i], MR.FStart, MR.FLength), R, iTextPos, iTop, iLeft);
                FTargetCanvas.Brush.Color := iColour;
                DrawTextToCanvas(Copy(sl[i], MR.FStart + MR.FLength,
                  Length(sl[i]) - MR.FStart - MR.FLength + 1), R,
                  iTextPos, iTop, iLeft);
              End;
          End;
        End;
    End;
End;

(**

  This method draw the vertical line between tree nodes.

  @precon  None.
  @postcon The vertical line between the nodes is drawn for different levels.

  @param   iTreeColour as a TColor as a constant
  @param   R           as a TRect as a constant

**)
Procedure TframeModuleExplorer.DrawVerticalTreeLine(Const iTreeColour : TColor; Const R : TRect);

Var
  P: PVirtualNode;
  i: Integer;

Begin
  P := FNode.Parent;
  For i := FNodeData.FNode.Level - 1 DownTo 0 Do
    Begin
      If (P <> Nil) And (P.Parent <> Nil) Then
        If P.Index < P.Parent.ChildCount - 1  Then
          Begin
            FTargetCanvas.Pen.Color := iTreeColour;
            FTargetCanvas.Pen.Style := psSolid;
            FTargetCanvas.MoveTo(Integer(FExplorer.Indent) * i + iNodeSize - FExplorer.Left, R.Top);
            FTargetCanvas.LineTo(Integer(FExplorer.Indent) * i + iNodeSize - FExplorer.Left, R.Bottom);
          End;
      P := P.Parent;
    End;
End;

(**

  This is an on change event handler for the Explorer Filter edit control.

  @precon  None.
  @postcon Notify that the filter has changed and the explorer should be filtered.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.edtExplorerFilterChange(Sender: TObject);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'edtExplorerFilterChange', tmoTiming);{$ENDIF}
  FilterChange;
End;

(**

  This is an on key press event handler for the Explorer Filter edit control.

  @precon  None.
  @postcon Clears the filter is escape is pressed.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
Procedure TframeModuleExplorer.edtExplorerFilterKeyPress(Sender: TObject; Var Key: Char);

Begin
  Case Key Of
    #27:
      Begin
        ExplorerFilter := '';
        Key := #0;
      End;
  End;
End;

(**

  This is a mouse activate event handler for the explorer filter.

  @precon  None.
  @postcon Makes FMouseEnter true to signify that the mouse was clicked on the filter.

  @param   Sender        as a TObject
  @param   Button        as a TMouseButton
  @param   Shift         as a TShiftState
  @param   X             as an Integer
  @param   Y             as an Integer
  @param   HitTest       as an Integer
  @param   MouseActivate as a TMouseActivate as a reference

**)
Procedure TframeModuleExplorer.edtExplorerFilterMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer; Var MouseActivate: TMouseActivate);

Begin
  FMouseEnter := True;
End;

(**

  This method expands, collapses or delete various nodes depending on the
  options and their contents.

  @precon  None.
  @postcon Expands, collapses or delete various nodes depending on the
           options and their contents.

**)
procedure TframeModuleExplorer.ExpandNodes;

  (**

    This method checks that the node text matches the text required (minus the optional number of
    children).

    @precon  None.
    @postcon Returns true if the node text matches the required text.

    @param   strNodeText  as a String as a constant
    @param   strMatchText as a String as a constant
    @return  a Boolean

  **)
  Function NodeTextMatch(Const strNodeText, strMatchText : String) : Boolean;

  Begin
    Result := CompareText(Copy(strNodeText, 1, Length(strMatchText)), strMatchText) = 0;
  End;

  (**

    This procedure expands the given node and all its children.

    @precon  Node must be a valid instance.
    @postcon The given node and all its children are expanded.

    @param   Node as a PVirtualNode as a constant

  **)
  Procedure ExpandNode(Const Node : PVirtualNode);

    (**

      This procedure recurses the child nodes expanding them and then their child nodes.

      @precon  ChildNode must be a valid instance.
      @postcon All child nodes are expanded.

      @param   ChildNode as a PVirtualNode as a constant

    **)
    Procedure RecurseNodes(Const ChildNode : PVirtualNode);

    Var
      N : PVirtualNode;

    Begin
      N := FExplorer.GetFirstChild(ChildNode);
      While Assigned(N) Do
        Begin
          FExplorer.Expanded[N] := True;
          RecurseNodes(N);
          N := FExplorer.GetNextSibling(N);
        End;
    End;

  Begin
    FExplorer.Expanded[Node] := True;
    RecurseNodes(Node);
  End;

Var
  iPromotedLabel : Integer;
  Node: PVirtualNode;
  NodeData: PBADITreeData;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ExpandNodes', tmoTiming);{$ENDIF}
  FExplorer.Expanded[FModule] := True;
  For iPromotedLabel := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[iPromotedLabel].Node.ChildCount = 0 Then
      Begin
        FExplorer.DeleteNode(FSpecialTagNodes[iPromotedLabel].Node);
        FSpecialTagNodes[iPromotedLabel].Node := Nil;
      End;
  For iPromotedLabel := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[iPromotedLabel].Node <> Nil Then
      If tpAutoExpand In FSpecialTagNodes[iPromotedLabel].FTagProperties Then
        FExplorer.Expanded[FSpecialTagNodes[iPromotedLabel].Node] := True;
  Node := FExplorer.GetFirstChild(FModule);
  While Node <> Nil Do
    Begin
      NodeData := FExplorer.GetNodeData(Node);
      If NodeTextMatch(NodeData.FNode.Text, strDocumentationConflicts) Then
        If doExpandDocConflicts In TBADIOptions.BADIOptions.Options Then
          ExpandNode(Node);
      If NodeTextMatch(NodeData.FNode.Text, strMetrics) Then
        If doExpandMetrics In TBADIOptions.BADIOptions.Options Then
          ExpandNode(Node);
      If NodeTextMatch(NodeData.FNode.Text, strChecks) Then
        If doExpandChecks In TBADIOptions.BADIOptions.Options Then
          ExpandNode(Node);
      If NodeTextMatch(NodeData.FNode.Text, strHints) Then
        If doExpandHints In TBADIOptions.BADIOptions.Options Then
          ExpandNode(Node);
      If NodeTextMatch(NodeData.FNode.Text, strWarnings) Then
        If doExpandWarnings In TBADIOptions.BADIOptions.Options Then
          ExpandNode(Node);
      If NodeTextMatch(NodeData.FNode.Text, strErrors) Then
        If doExpandErrors In TBADIOptions.BADIOptions.Options Then
          ExpandNode(Node);
      Node := FExplorer.GetNextSibling(Node);
    End;
end;

(**

  This method extracts the spelling mistake word from the current tree node text.

  @precon  The focused node is a spelling mistake.
  @postcon The spelling mistake word is returned.

  @return  a String

**)
Function TframeModuleExplorer.ExtractSpellingWord: String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ExtractSpellingWord', tmoTiming);{$ENDIF}
  Result := FExplorer.Text[FExplorer.FocusedNode, 0];
  Result := Copy(Result, 1, Pos('(', Result) - 1).Trim;
End;

(**

  This method updates the filtering of the treeview.

  @precon  None.
  @postcon The treeview filter is updated only showing node that match the filter text.

**)
Procedure TframeModuleExplorer.FilterChange;

Var
  N, P : PVirtualNode;
  NodeData : PBADITreeData;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FilterChange', tmoTiming);{$ENDIF}
  Try
    If ExplorerFilter <> '' Then
      FFilterRegEx := TRegEx.Create(ExplorerFilter, [roIgnoreCase, roCompiled, roSingleLine]);
  Except
    On E : ERegularExpressionError Do
      Begin
        stbStatusBar.SimpleText := E.Message;
        stbStatusBar.SimplePanel := True;
        Exit;
      End;
  End;
  FFollowNode := Nil;
  FExplorer.BeginUpdate;
  Try
    N := FExplorer.GetFirst;
    While Assigned(N) Do
      Begin
        If ExplorerFilter <> '' Then
          Begin
            NodeData := FExplorer.GetNodeData(N);
            FExplorer.IsVisible[N] := FFilterRegEx.IsMatch(NodeData.FNode.Text);
            If FExplorer.IsVisible[N] Then
              Begin
                If FExplorer.FullyVisible[N] Then // Override Follow Node
                  FFollowNode := N;
                P := FExplorer.NodeParent[N];
                While Assigned(P) Do
                  Begin
                    FExplorer.IsVisible[P] := True;
                    P := FExplorer.NodeParent[P];
                  End;
              End;
          End Else
            FExplorer.IsVisible[N] := True;
        N := FExplorer.GetNext(N);
      End;
  Finally
    FExplorer.EndUpdate;
  End;
  If Assigned(FFollowNode) Then
    Begin
      FExplorer.FocusedNode := FFollowNode;
      FExplorer.ScrollIntoView(FFollowNode, True);
    End;
End;

(**

  This function finds the tree node that has the path specified by the passed text.

  @precon  strText is the string representation of the node path to be found.
  @postcon Returns tree node index of the item corresponding to the given path.

  @param   strText as a String as a constant
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.FindTreeItem(Const strText : String) : PVirtualNode;

  (**

    This function recursively searches for the node path which matches the given text and returns
    the node is found.

    @precon  None.
    @postcon Recursively searches for the node path which matches the given text and returns the
             node is found.

    @param   Node as a PVirtualNode as a constant
    @return  a PVirtualNode

  **)
  Function FindNode(Const Node : PVirtualNode) : PVirtualNode;

  Var
    C : PVirtualNode;

  Begin
      Result := Nil;
      C := FExplorer.GetFirstChild(Node);
      While C <> Nil Do
        Begin
          If C.ChildCount > 0 Then
            Result := FindNode(C);
          If Result <> Nil Then
            Exit;
          If CompareText(GetNodePath(C), strText) = 0 Then
            Begin
              Result := C;
              Exit;
            End;
          C := FExplorer.GetNextSibling(C);
        End;
  End;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FindTreeItem', tmoTiming);{$ENDIF}
  Result := Nil;
  If strText <> '' Then
    Result := FindNode(FModule);
End;

(**

  This method attempts to focus the FFollowNode in the treeview.

  @precon  None.
  @postcon If the FFollowNode is not visible the first visible parent node is focused.

**)
Procedure TframeModuleExplorer.FocusFollowedNode;

Var
  Node: PVirtualNode;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FocusFollowedNode', tmoTiming);{$ENDIF}
  If Assigned(FFollowNode) And
    ((FFollowNode <> Explorer.FocusedNode) Or Not Explorer.FullyVisible[Explorer.FocusedNode]) Then
    Begin
      Node := FFollowNode;
      While Assigned(Node) And Not Explorer.FullyVisible[Node] Do
        Node := Explorer.NodeParent[Node];
      If Assigned(Node) Then
        Begin
          Explorer.FocusedNode := Node;
          Explorer.ScrollIntoView(Node, True);
        End;
    End;
End;

(**

  This method attempts to focus the node (element) which contains the cursor line provided.

  @precon  None.
  @postcon If found the elements or its first visible parent is focused.

  @param   iLine as an Integer as a constant

**)
Procedure TframeModuleExplorer.FollowEditorCursor(Const iLine: Integer);

Var
  Node : PVirtualNode;
  NodeData : PBADITreeData;
  iNodeLine, iFollowLine : Integer;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FollowEditorCursor', tmoTiming);{$ENDIF}
  If FDocIssueTotals.ContainsAny(TBADIOptions.BADIOptions.DoNotFollowEditor) Then
    Exit;
  iFollowLine := 0;
  Node := Explorer.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := Explorer.GetNodeData(Node);
      If NodeData.FNodeType = ntElement Then
        Begin
          iNodeLine := NodeData.FNode.Line;
          If Assigned(NodeData.FNode.Comment) And (NodeData.FNode.Comment.Line > 0) Then
            iNodeLine := NodeData.FNode.Comment.Line;
          If (iLine >= iNodeLine) And (iNodeLine > iFollowLine) Then
            Begin
              FFollowNode := Node;
              iFollowLine := iNodeLine;
            End;
        End;
      Node := Explorer.GetNext(Node);
    End;
  FocusFollowedNode;
End;

Function TframeModuleExplorer.FollowMethodNodeData(Const iLine : Integer) : PBADITreeData;

Var
  iFollowLine: Integer;
  iNodeLine: Integer;
  Node: PVirtualNode;
  NodeData: PBADITreeData;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FollowMethodNodeData', tmoTiming);{$ENDIF}
  Result := Nil;
  iFollowLine := 0;
  Node := Explorer.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := Explorer.GetNodeData(Node);
      If NodeData.FNodeType = ntElement Then
        Begin
          iNodeLine := NodeData.FNode.Line;
          If Assigned(NodeData.FNode.Comment) And (NodeData.FNode.Comment.Line > 0) Then
            iNodeLine := NodeData.FNode.Comment.Line;
          If (iLine >= iNodeLine) And (iNodeLine > iFollowLine) Then
            Begin
              Result := NodeData;
              iFollowLine := iNodeLine;
            End;
        End;
      Node := Explorer.GetNext(Node);
    End;
End;

(**

  This method sets the explorer frame as the focus when the form is activated.

  @precon  None.
  @postcon The explorer frame is focused.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.FrameEnter(Sender: TObject);

begin
  If Not FMouseEnter Then
    If Parent.Visible And Visible And FExplorer.Visible And FExplorer.CanFocus Then
      FExplorer.SetFocus;
  FMouseEnter := False;
end;

(**

  This method retrieves comments from the body comment collection and adds them to the comment node
  of the tree view.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon This method cycles through the body comments extracting special tags.

  @param   Module as a TBaseLanguageModule as a constant

**)
procedure TframeModuleExplorer.GetBodyCommentTags(Const Module : TBaseLanguageModule);

  (**

    This function attempts to find the index of the given tag name in the special tags array.

    @precon  None.
    @postcon Returns the index position of the tag in the special tags array else returns -1 if not
             found.

    @param   strTagName as a String as a constant
    @return  an Integer

  **)
  Function FindTag(Const strTagName : String) : Integer;

  Var
    iSpecialTag: Integer;

  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetBodyCommentTags/FindTag', tmoTiming);{$ENDIF}
    Result := -1;
    For iSpecialTag := Low(FSpecialTagNodes) To High(FSpecialTagNodes) Do
      If CompareText(strTagName, FSpecialTagNodes[iSpecialTag].FTagName) = 0 Then
        Begin
          Result := iSpecialTag;
          Break;
        End;
  End;

  (**

    This procedure adds a doc issue to the module based on a line and tag record.

    @precon  None.
    @postcon A doc issue is added to the module.

    @param   iLine as an Integer as a constant
    @param   Tag   as a TSpecialTagNode as a constant

  **)
  Procedure AddDocIssueInfo(Const iLine : Integer; Const Tag : TSpecialTagNode); OverLoad;

  Var
    recDocIssueInfo : TBADIDocIssueInfo;
    DocIssues: IBADILineDocIssues;
  
  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetBodyCommentTags/AddDocIssueInfo', tmoTiming);{$ENDIF}
    recDocIssueInfo.FName := Tag.FTagName;
    recDocIssueInfo.FImageIndex := Tag.FImageIndex;
    recDocIssueInfo.FForeColour := Tag.FFontColour;
    recDocIssueInfo.FBackColour := Tag.FBackColour;
    recDocIssueInfo.FMessage := '';
    If FLineDocIssues.TryGetValue(iLine, DocIssues) Then
      DocIssues.AddIssue('@' + Tag.FTagName, recDocIssueInfo)
    Else
      FLineDocIssues.Add(iLine, TBADILineDocIssue.Create('@' + Tag.FTagName, recDocIssueInfo));
  End;

  (**

    This procedure adds a doc issue to the module based on a line and tag name.

    @precon  None.
    @postcon A doc issue is added to the module.

    @param   iLine      as an Integer as a constant
    @param   strTagName as a String as a constant

  **)
  Procedure AddDocIssueInfo(Const iLine : Integer; Const strTagName : String); Overload;

  Var
    recDocIssueInfo : TBADIDocIssueInfo;
    DocIssues: IBADILineDocIssues;
  
  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetBodyCommentTags/AddDocIssueInfo', tmoTiming);{$ENDIF}
    recDocIssueInfo.FName := strTagName;
    recDocIssueInfo.FImageIndex := iiBadTag;
    recDocIssueInfo.FForeColour := clRed;
    recDocIssueInfo.FBackColour := clNone;
    recDocIssueInfo.FMessage := '';
    If FLineDocIssues.TryGetValue(iLine, DocIssues) Then
      DocIssues.AddIssue('@' + strTagName, recDocIssueInfo)
    Else
      FLineDocIssues.Add(iLine, TBADILineDocIssue.Create('@' + strTagName, recDocIssueInfo));
  End;

  (**

    This procedure outputs adds Total Info record based on a tag record.

    @precon  None.
    @postcon A Total Info record is added to the module.

    @param   Tag   as a TSpecialTagNode as a constant
    @param   iLine as an Integer as a constant
    @param   iCol  as an Integer as a constant

  **)
  Procedure AddTotalInfo(Const Tag : TSpecialTagNode; Const iLine, iCol : Integer); Overload;

  Var
    recTotalInfo : TBADITotalInfo;

  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetBodyCommentTags/AddTotalInfo', tmoTiming);{$ENDIF}
    recTotalInfo.FImageIndex := Tag.FImageIndex;
    recTotalInfo.FForeColour := Tag.FFontColour;
    recTotalInfo.FBackColour := Tag.FBackColour;
    recTotalInfo.FFontStyles := Tag.FFontStyles;
    recTotalInfo.FFirstLine := iLine;
    recTotalInfo.FFirstCol := iCol;
    FDocIssueTotals.IncDocIssue('@' + Tag.FTagName, recTotalInfo);
  End;

  (**

    This procedure outputs adds Total Info record based on a tag name.

    @precon  None.
    @postcon A Total Info record is added to the module.

    @param   strTagName as a String as a constant
    @param   iLine      as an Integer as a constant
    @param   iCol       as an Integer as a constant

  **)
  Procedure AddTotalInfo(Const strTagName : String; Const iLine, iCol : Integer); Overload;

  Var
    recTotalInfo : TBADITotalInfo;

  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetBodyCommentTags/AddTotalInfo', tmoTiming);{$ENDIF}
    recTotalInfo.FImageIndex := iiBadTag;
    recTotalInfo.FForeColour := clRed;
    recTotalInfo.FBackColour := clNone;
    recTotalInfo.FFontStyles := [];
    recTotalInfo.FFirstLine := iLine;
    recTotalInfo.FFirstCol := iCol;
    FDocIssueTotals.IncDocIssue('@' + strTagName, recTotalInfo);
  End;

ResourceString
  strBadTagFound = 'Bad Tag "%s" found!';

Const
  iTreeLevel = 2;
  astrTagsToIgnore : Array[0..9] Of String = (
    strnocheck,
    strnochecks,
    strnodocumentation,
    strnohint,
    strnohints,
    strnometric,
    strnometrics,
    strnospelling,
    strnospellings,
    strstopdocumentation
  );
  
Var
  iComment, iTag, iSpecialTag : Integer;
  Cmt: TComment;
  Tag: TTag;
  ST: TSpecialTagNode;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetBodyCommentTags', tmoTiming);{$ENDIF}
  For iComment := 0 To Module.BodyCommentCount - 1 Do
    Begin
      Cmt := Module.BodyComment[iComment];
      For iTag := 0 To Cmt.TagCount - 1 Do
        Begin
          iSpecialTag := FindTag(Cmt.Tag[iTag].TagName);
          If iSpecialTag >= 0 Then
            Begin
              If tpShowInTree In FSpecialTagNodes[iSpecialTag].FTagProperties Then
                Begin
                  Tag := Cmt.Tag[iTag];
                  ST := FSpecialTagNodes[iSpecialTag];
                  AddNode(FSpecialTagNodes[iSpecialTag].Node, Tag, iTreeLevel,
                    BADIImageIndex(ST.FImageIndex, scNone), ST.FTagProperties, ST.FFontStyles,
                    ST.FFontColour, ST.FBackColour, Cmt);
                  If tpShowInEditor In FSpecialTagNodes[iSpecialTag].FTagProperties Then
                    Begin
                      AddDocIssueInfo(Tag.Line, ST);
                      AddTotalInfo(ST, Tag.Line, Tag.Column);
                    End;
                End;
            End Else
            If Not IsKeyWord(LowerCase(Cmt.Tag[iTag].TagName), astrTagsToIgnore) Then
              Begin
                AddNode(
                  FModule,
                  Format(strBadTagFound, [Cmt.Tag[iTag].TagName]),
                  Cmt.Tag[iTag].TagName,
                  1,
                  BADIImageIndex(iiBadTag, scNone)
                );
                AddDocIssueInfo(Cmt.Tag[iTag].Line, Cmt.Tag[iTag].TagName);
                AddTotalInfo(Cmt.Tag[iTag].TagName, Cmt.Tag[iTag].Line, Cmt.Tag[iTag].Column);
              End;
        End;
    End;
End;

(**

  This is a getter method for the DocIssueTotals property.

  @precon  None.
  @postcon Returns the Document Issues Totals.

  @return  an IBADIDocIssueTotals

**)
Function TframeModuleExplorer.GetDocIssueTotals: IBADIDocIssueTotals;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetDocIssueTotals', tmoTiming);{$ENDIF}
  Result := FDocIssueTotals;
End;

(**

  This method gets the tree nodes that are currently expanded and stores them in a string list.

  @precon  Node is the tree node to be tested for expansion.
  @postcon Adds, update or deletes nodes from the expanded node list depending whether they are
           now expanded.

  @param   StartNode as a PVirtualNode as a constant

**)
Procedure TframeModuleExplorer.GetExpandedNodes(Const StartNode : PVirtualNode);

Var
  str : String;
  iIndex : Integer;
  Node : PVirtualNode;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetExpandedNodes', tmoTiming);{$ENDIF}
  Node := FExplorer.GetFirstChild(StartNode);
  While Node <> Nil Do
    Begin
      If Node.ChildCount > 0 Then
        Begin
          str := GetNodePath(Node);
          If FExplorer.Expanded[Node] And (str <> '') Then
            Begin
              If Not TBADIOptions.BADIOptions.ExpandedNodes.Find(str, iIndex) Then
                iIndex := TBADIOptions.BADIOptions.ExpandedNodes.Add(str);
              TBADIOptions.BADIOptions.ExpandedNodes.Objects[iIndex] := TObject(Trunc(Now));
            End Else
              If TBADIOptions.BADIOptions.ExpandedNodes.Find(str, iIndex) Then
                TBADIOptions.BADIOptions.ExpandedNodes.Delete(iIndex);
          GetExpandedNodes(Node);
        End;
      Node := FExplorer.GetNextSibling(Node);
    End;
End;

(**

  This is a getter method for the Line Doc Issue conflicts property.

  @precon  None.
  @postcon Returns a set of limit types for the given line number to indicate issues on that line.

  @param   iLine as an Integer as a constant
  @return  an IBADILineDocIssues

**)
Function TframeModuleExplorer.GetLineDocIssue(Const iLine: Integer): IBADILineDocIssues;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetLineDocIssue', tmoTiming);{$ENDIF}
  FLineDocIssues.TryGetValue(iLine, Result);
End;

(**

  This method returns the path of the specified tree node.

  @precon  Node is the tree node to be calculate the path to.
  @postcon Returns a string representation of the tree nodes path excluding the root item.

  @param   Node as a PVirtualNode as a constant
  @return  a String

**)
Function TframeModuleExplorer.GetNodePath(Const Node : PVirtualNode) : String;

Var
  P : PVirtualNode;
  str : String;
  NodeData : PBADITreeData;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetNodePath', tmoTiming);{$ENDIF}
  str := '';
  P := Node;
  While P <> Nil Do
    Begin
      If FExplorer.NodeParent[P] <> Nil Then
        Begin
          NodeData := FExplorer.GetNodeData(P);
          str := NodeData.FNode.Name + '.' + str;
        End;
      P := FExplorer.NodeParent[P];
    End;
  Result := str;
End;

(**

  This method initialises a TMatchResult record as we cannot define constructors for anonymous
  records.

  @precon  None.
  @postcon The record is initialised.

  @param   eMatchType as a TMatchType as a constant
  @param   iStart     as an Integer as a constant
  @param   iLength    as an Integer as a constant
  @return  a TMatchResult

**)
Function TframeModuleExplorer.InitMatchResult(Const eMatchType : TMatchType; Const iStart,
  iLength: Integer) : TMatchResult;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'InitMatchResult', tmoTiming);{$ENDIF}
  Result.FMatchType := eMatchType;
  Result.FStart := iStart;
  Result.FLength := iLength;
End;

(**

  This method searches the node text for matches and returns the match results.

  @precon  MC must be a valid instance.
  @postcon Searches the node text for matches and returns the match results.

  @param   iIndex  as an Integer as a constant
  @param   iLength as an Integer as a constant
  @param   MC      as a TMatchCollection as a constant
  @return  a TMatchResult

**)
Function TframeModuleExplorer.IsMatched(Const iIndex, iLength: Integer;
  Const MC: TMatchCollection): TMatchResult;

Var
  iMatch: Integer;
  M: TMatch;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'IsMatched', tmoTiming);{$ENDIF}
  Result.FMatchType := mtNone;
  For iMatch := 0 To MC.Count - 1 Do
    Begin
      M := MC.Item[iMatch];
      If (M.Index + M.Length >= iIndex) And (M.Index <= iIndex + iLength) Then
        Begin
          If (M.Index <= iIndex) And (M.Index + M.Length >= iIndex + iLength) Then
            Result := InitMatchResult(mtFull, 1, iLength)
          Else If (M.Index <= iIndex) And (M.Index + M.Length < iIndex + iLength) Then
            Result := InitMatchResult(mtStart, 1, M.Index + M.Length - iIndex)
          Else If (M.Index > iIndex) And (M.Index + M.Length >= iIndex + iLength) Then
            Result := InitMatchResult(mtEnd, M.Index - iIndex + 1, iIndex + iLength - M.Index)
          Else
            Result := InitMatchResult(mtMiddle, M.Index - iIndex + 1, M.Length);
          Break;
        End;
    End;
End;

(**

  This method logs the Doc Issue and Conflicts.

  @precon  None.
  @postcon If the Element is a Doc Issue or Conflict then it is logged.

  @param   Element as a TElementContainer as a constant

**)
Procedure TframeModuleExplorer.LogDocIssueConflict(Const Element: TElementContainer);

  (**

    This method converts an error type to a limit type.

    @precon  None.
    @postcon Returns the limit type corresponding to the given error type.

    @param   eErrorType as a TErrorType as a constant
    @return  a TLimitType

  **)
  Function ErrorTypeToLimitType(Const eErrorType : TErrorType) : TLimitType;

  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LogDocIssueConflict/ErrorTypeToLimitType', tmoTiming);{$ENDIF}
    Case eErrorType Of
      etHint:    Result := ltHints;
      etWarning: Result := ltWarnings;
      etError:   Result := ltErrors;
    Else
      Result := ltErrors;
    End;
  End;

  (**

    This method converts an conflict type to a limit type.

    @precon  None.
    @postcon Returns the limit type corresponding to the given conflict type.

    @param   eConflictType as a TBADIConflictType as a constant
    @return  a TLimitType

  **)
  Function ConflictTypeToLimitType(Const eConflictType : TBADIConflictType) : TLimitType;

  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LogDocIssueConflict/ConflictTypeToLimitType', tmoTiming);{$ENDIF}
    Case eConflictType Of
      ctDocumentation: Result := ltConflicts;
      ctMetric:        Result := ltMetrics;
      ctCheck:         Result := ltChecks;
      ctSpelling:      Result := ltSpelling;
    Else
      Result := ltErrors;
    End;
  End;

Const
  aLimitImageIndex : Array[TlimitType] Of TBADIImageIndex = (
    iiError,
    iiWarning,
    iiHint,
    iiDocConflictItem,
    iiCheckItem,
    iiMetricItem,
    iiSpellingItem
  );

Var
  DI: TDocIssue;
  eLimitType : TLimitType;
  LineDocIssues: IBADILineDocIssues;
  DC: TDocumentConflict;
  recTotalInfo : TBADITotalInfo;
  recDocIssueInfo : TBADIDocIssueInfo;
  SI: TBADISpellingIssue;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LogDocIssueConflict', tmoTiming);{$ENDIF}
  If Element Is TDocIssue Then
    Begin
      DI := Element As TDocIssue;
      eLimitType := ErrorTypeToLimitType(DI.ErrorType);
      recDocIssueInfo.FName := astrLimitType[eLimitType];
      recDocIssueInfo.FImageIndex := aLimitImageIndex[eLimitType];
      recDocIssueInfo.FForeColour := clNone;
      recDocIssueInfo.FBackColour := clNone;
      recDocIssueInfo.FMessage := Element.AsString(False, False);
      If FLineDocIssues.TryGetValue(DI.Line, LineDocIssues) Then
        LineDocIssues.AddIssue(astrLimitType[eLimitType], recDocIssueInfo)
      Else
        FLineDocIssues.Add(DI.Line, TBADILineDocIssue.Create(astrLimitType[eLimitType], recDocIssueInfo));
      recTotalInfo.FImageIndex := aLimitImageIndex[eLimitType];
      recTotalInfo.FForeColour := clNone;
      recTotalInfo.FBackColour := clNone;
      recTotalInfo.FFontStyles := [];
      recTotalInfo.FFirstLine := DI.Line;
      recTotalInfo.FFirstCol := DI.Column;
      FDocIssueTotals.IncDocIssue(astrLimitType[eLimitType], recTotalInfo);
    End
  Else If Element Is TDocumentConflict Then
    Begin
      DC := Element As TDocumentConflict;
      eLimitType := ConflictTypeToLimitType(DC.ConflictType);
      recDocIssueInfo.FName := astrLimitType[eLimitType];
      recDocIssueInfo.FImageIndex := aLimitImageIndex[eLimitType];
      recDocIssueInfo.FForeColour := clNone;
      recDocIssueInfo.FBackColour := clNone;
      recDocIssueInfo.FMessage := Element.AsString(False, False);
      If FLineDocIssues.TryGetValue(DC.Line, LineDocIssues) Then
        LineDocIssues.AddIssue(astrLimitType[eLimitType], recDocIssueInfo)
      Else
        FLineDocIssues.Add(DC.Line, TBADILineDocIssue.Create(astrLimitType[eLimitType], recDocIssueInfo));
      recTotalInfo.FImageIndex := aLimitImageIndex[eLimitType];
      recTotalInfo.FForeColour := clNone;
      recTotalInfo.FBackColour := clNone;
      recTotalInfo.FFontStyles := [];
      recTotalInfo.FFirstLine := DC.Line;
      recTotalInfo.FFirstCol := DC.Column;
      FDocIssueTotals.IncDocIssue(astrLimitType[eLimitType], recTotalInfo);    
    End
  Else If Element Is TBADISpellingIssue Then
    Begin
      SI := Element As TBADISpellingIssue;
      eLimitType := ConflictTypeToLimitType(ctSpelling);
      recDocIssueInfo.FName := astrLimitType[eLimitType];
      recDocIssueInfo.FImageIndex := aLimitImageIndex[eLimitType];
      recDocIssueInfo.FForeColour := clNone;
      recDocIssueInfo.FBackColour := clNone;
      recDocIssueInfo.FMessage := '';
      If FLineDocIssues.TryGetValue(SI.Line, LineDocIssues) Then
        LineDocIssues.AddSpellingMistake(SI.Identifier, SI.Column)
      Else
        Begin
         LineDocIssues := TBADILineDocIssue.Create(astrLimitType[eLimitType], recDocIssueInfo);
         LineDocissues.AddSpellingMistake(SI.Identifier, SI.Column);
         FLineDocIssues.Add(SI.Line, LineDocIssues);
        End;
      recTotalInfo.FImageIndex := aLimitImageIndex[eLimitType];
      recTotalInfo.FForeColour := clNone;
      recTotalInfo.FBackColour := clNone;
      recTotalInfo.FFontStyles := [];
      recTotalInfo.FFirstLine := SI.Line;
      recTotalInfo.FFirstCol := SI.Column;
      FDocIssueTotals.IncDocIssue(astrLimitType[eLimitType], recTotalInfo);    
    End;
End;

(**

  This method removed items from the list their date (TObject data) is more than
  a specific age in days.

  @precon  None.
  @postcon Removed items from the list their date (TObject data) is more than
           a specific age in days.

**)
procedure TframeModuleExplorer.ManageExpandedNodes;

Var
  i : Integer;
  dtDate: TDateTime;
  EN: TStringList;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ManageExpandedNodes', tmoTiming);{$ENDIF}
  EN := TBADIOptions.BADIOptions.ExpandedNodes;
  For i := EN.Count - 1 DownTo 0 Do
    Begin
      dtDate := Integer(EN.Objects[i]);
      If dtDate < Now - TBADIOptions.BADIOptions.ManagedNodesLife Then
        EN.Delete(i);
    End;
end;

(**

  This method returns the limit for adding child nodes to a node.

  @precon  Container must be a valid instance.
  @postcon Returns the limit for adding child nodes to a node.

  @param   Container as a TElementContainer as a constant
  @return  an Integer

**)
Function TframeModuleExplorer.MaxLimit(Const Container: TElementContainer) : Integer;

Const
  iDefaultMaxLimit = 9999;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'MaxLimit', tmoTiming);{$ENDIF}
  Result := iDefaultMaxLimit;
  If Container.ElementCount > 0 Then
    If Container.Elements[1] Is TDocumentConflict Then
      Case TDocumentConflict(Container.Elements[1]).ConflictType Of
        ctDocumentation: Result := TBADIOptions.BADIOptions.IssueLimits[ltConflicts];
        ctMetric:        Result := TBADIOptions.BADIOptions.IssueLimits[ltMetrics];
        ctCheck:         Result := TBADIOptions.BADIOptions.IssueLimits[ltChecks];
        ctSpelling:      Result := TBADIOptions.BADIOptions.IssueLimits[ltSpelling];
      End
    Else If Container.Elements[1] Is TDocIssue Then
      Case TDocIssue(Container.Elements[1]).ErrorType Of
        etHint:    Result := TBADIOptions.BADIOptions.IssueLimits[ltHints];
        etWarning: Result := TBADIOptions.BADIOptions.IssueLimits[ltWarnings];
        etError:   Result := TBADIOptions.BADIOptions.IssueLimits[ltErrors];  
      End
    Else If Container.Elements[1] Is TBADISpellingIssue Then
      Result := TBADIOptions.BADIOptions.IssueLimits[ltSpelling];
End;

(**

  This method outputs the modules information as items in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon Outputs the different parts of the module.

  @param   Container as a TElementContainer as a constant

**)
procedure TframeModuleExplorer.OutputModuleInfo(Const Container : TElementContainer);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'OutputModuleInfo', tmoTiming);{$ENDIF}
  RenderContainers(FModule, Container, 1);
  GetBodyCommentTags(Container As TBaseLanguageModule);
end;

Procedure TframeModuleExplorer.PromoteLabels;
Const
  strPromotedLabels: Array [1 .. 7] Of String = (
    strSpelling,
    strMetrics,
    strChecks,
    strDocumentationConflicts,
    strHints,
    strWarnings,
    strErrors    
    );

Var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PBADITreeData;

Begin
  For i := Low(strPromotedLabels) To High(strPromotedLabels) Do
    Begin
      Node := FExplorer.GetFirstChild(FModule);
      While Node <> Nil Do
        Begin
          NodeData := FExplorer.GetNodeData(Node);
          If Pos(strPromotedLabels[i], NodeData.FNode.Text) = 1 Then
            Begin
              FExplorer.MoveTo(Node, FModule, amAddChildFirst, False);
              Break;
            End;
          Node := FExplorer.GetNextSibling(Node);
        End;
    End;
End;

(**

  This method renders the modules sub-containers recursively.

  @precon  RootNode must be a valid tree node and Container must be a valid container.
  @postcon Renders the modules sub-containers recursively.

  @param   RootNode  as a PVirtualNode as a constant
  @param   Container as a TElementContainer as a constant
  @param   iLevel    as an Integer as a constant

**)
Procedure TframeModuleExplorer.RenderContainers(Const RootNode : PVirtualNode;
  Const Container: TElementContainer; Const iLevel : Integer);

Const
  strTooManyConflictsName = 'TooManyConflicts';

Var
  i : Integer;
  NewNode : PVirtualNode;
  iLimit : Integer;
  iCount : Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'RenderContainers', tmoTiming);{$ENDIF}
  iLimit := MaxLimit(Container);
  iCount := 0;
  For i := 1 To Container.ElementCount Do
    If Container.Elements[i].Scope In TBADIOptions.BADIOptions.ScopesToRender + [scNone, scGlobal] Then
      Begin
        LogDocIssueConflict(Container.Elements[i]);
        If iCount < iLimit Then
          Begin
            NewNode := AddNode(RootNode, Container.Elements[i], iLevel);
            RenderContainers(NewNode, Container[i], iLevel + 1);
          End
        Else If iCount = iLimit Then
          AddNode(RootNode, Format(strTooManyConflicts, [Container.ElementCount]),
            strTooManyConflictsName, iLevel, Container.Elements[i].ImageIndexAdjustedForScope);
        Inc(iCount);
      End;
End;

(**

  This method displays the specified module in the treeview.

  @precon  Module is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon Renders the module information for the given module.

  @param   Module as a TBaseLanguageModule as a constant

**)
procedure TframeModuleExplorer.RenderModule(Const Module : TBaseLanguageModule);

Var
  strTop : String;
  strSelection : String;
  N : PVirtualNode;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'RenderModule', tmoTiming);{$ENDIF}
  FTokenFontInfo := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours];
  FBGColour := FBADIOptions.BGColour[FBADIOptions.UseIDEEditorColours];
  If FBGColour = clNone Then
    FBGColour := FBADIOptions.BGColour[False];
  FExplorer.Color := FBGColour;
  If Module = Nil Then
    Begin
      strReservedWords := Nil;
      strDirectives := Nil;
    End Else
    Begin
      strReservedWords := Module.ReservedWords;
      strDirectives := Module.Directives;
    End;
  If FRendering Then
    Exit;
  FRendering := True;
  Try
    FLineDocIssues.Clear;
    FDocIssueTotals.Clear;
    FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
    FExplorer.Font.Name := TBADIOptions.BADIOptions.TreeFontName;
    FExplorer.Font.Size := TBADIOptions.BADIOptions.TreeFontSize;
    GetExpandedNodes(FModule);
    FModule := Nil;
    // Find and store the top item and the selected item in the tree view
    strTop := GetNodePath(FExplorer.TopNode);
    strSelection := GetNodePath(FExplorer.FocusedNode);
    FExplorer.BeginUpdate;
    Try
      ExplorerFilter := '';
      FFollowNode := Nil;
      FExplorer.Clear;
      FNodeInfo.Clear;
      If Module = Nil Then
        Exit;
      Module.AddTickCount(strClear);
      SetLength(FSpecialTagNodes, TBADIOptions.BADIOptions.SpecialTags.Count);
      // Create Root Tree Node
      FModule := AddNode(Nil, Module, 0);
      CreateSpecialTagNodes();
      CheckForIDEErrors(Module);
      OutputModuleInfo(Module);
      Module.AddTickCount(strBuild);
      SetExpandedNodes(FModule);
      ExpandNodes;
      // Restore top and selected items
      If Module.FindElement(strErrors) = Nil Then // Only if no errors.
        Begin
          N := FindTreeItem(strTop);
          If N <> Nil Then
            FExplorer.TopNode := N;
        End Else
          FExplorer.TopNode := FModule;
      N := FindTreeItem(strSelection);
      If N <> Nil Then
        Begin
          FExplorer.FocusedNode := N;
          FExplorer.Selected[FExplorer.FocusedNode] := True;
        End;
    Finally
      If Module <> Nil Then
        Module.AddTickCount(strSetup);
      FExplorer.EndUpdate;
    End;
    PromoteLabels;
    Module.AddTickCount(strRender);
    UpdateStatusBar(Module);
  Finally
    FRendering := False;
  End;
End;

(**

  This method expands the tree view nodes if they are found in the list.

  @precon  Node is the tree node to be expanded.
  @postcon Sets the node as expanded if it was in the expanded node list.

  @param   StartNode as a PVirtualNode as a constant

**)
Procedure TframeModuleExplorer.SetExpandedNodes(Const StartNode : PVirtualNode);

Var
  i : Integer;
  str : String;
  Node : PVirtualNode;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SetExpandedNodes', tmoTiming);{$ENDIF}
  Node := FExplorer.GetFirstChild(StartNode);
  While Node <> Nil Do
    Begin
      If Node.ChildCount > 0 Then
        Begin
          str := GetNodePath(Node);
          If TBADIOptions.BADIOptions.ExpandedNodes.Find(str, i) Then
            FExplorer.Expanded[Node] := True;
          SetExpandedNodes(Node);
        End;
      Node := FExplorer.GetNextSibling(Node);
    End;
End;

(**

  This is a setter method for the ExplorerFilter property.

  @precon  None.
  @postcon Sets the FExplorerFilter field and determines whether a simple panel is to be displayed for
           the filter text information.

  @param   strValue as a String as a constant

**)
Procedure TframeModuleExplorer.SetExplorerFilter(Const strValue: String);

ResourceString
  strFilteringFor = 'Filtering for "%s"...';

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SetExplorerFilter', tmoTiming);{$ENDIF}
  FExplorerFilter := strValue;
  stbStatusBar.SimplePanel := FExplorerFilter.Length > 0;
  If stbStatusBar.SimplePanel Then
    stbStatusBar.SimpleText := Format(strFilteringFor, [FExplorerFilter])
End;

(**

  This method sets the contents of a specific statusbar panel and sets its width accordingly.

  @precon  None.
  @postcon The status bar panel is updated.

  @param   ePanel           as a TStatusPanelIndex as a constant
  @param   strStatusBarText as a String as a constant
  @param   iValue           as an Integer as a constant

**)
Procedure TframeModuleExplorer.SetStatusPanel(Const ePanel: TStatusPanelIndex;
  Const strStatusBarText: String; Const iValue: Integer);

Const
  iPadding = 10;
  iPaddingMultiplier = 2;

Var
  strText : String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SetStatusPanel', tmoTiming);{$ENDIF}
  strText := Format(strStatusbarText, [Int(iValue)]);
  stbStatusBar.Panels[Byte(ePanel)].Text := strText;
  stbStatusBar.Panels[Byte(ePanel)].Width :=
    stbStatusBar.Canvas.TextWidth(strText) + iPaddingMultiplier * iPadding;
End;

(**

  This is an on timer event handler for the filter text.

  @precon  None.
  @postcon Attempts to filter the text in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.tmFilterTimer(Sender: TObject);

Const
  iUpdateInterval = 250;

Begin
  If (FLastFilterUpdate > 0) And (GetTickCount > FLastFilterUpdate + iUpdateInterval) Then
    Try
      tmFilter.Enabled := False;
      Try
        edtExplorerFilterChange(Self);
      Finally
        tmFilter.Enabled := True;
      End;
    Finally
      FLastFilterUpdate := 0;
    End;
End;

(**

  This is an on After Cell Paint event handler for the interfaces tree view.

  @precon  None.
  @postcon This method paints the highlighted text over the top of the tree text.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas
  @param   Node         as a PVirtualNode
  @param   Column       as a TcolumnIndex
  @param   CellRect     as a TRect

**)
Procedure TframeModuleExplorer.tvExplorerAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

  (**

    This local method highlights the text matches passed in the given colour.

    @precon  None.
    @postcon If there are matches these are highlighted.

    @param   MC      as a TMatchCollection as a constant
    @param   strText as a String as a constant
    @param   iColour as a TColor as a constant

  **)
  Procedure HighlightText(Const MC : TMatchCollection; Const strText : String; Const iColour : TColor);

  Const
    iHighlightTextOffset = 18 + 26;
    
  Var
    iStart: Integer;
    iMatch: Integer;
    M: TMatch;
    iLeft: Integer;
    R : TRect;

  Begin
    iStart := iHighlightTextOffset + Sender.GetNodeLevel(Node) * (Sender As TVirtualStringTree).Indent;
    For iMatch := 0 To MC.Count - 1 Do
      Begin
        M := MC[iMatch];
        TargetCanvas.Brush.Color := iColour;
        iLeft := TargetCanvas.TextWidth(Copy(strText, 1, M.Index - 1));
        R := CellRect;
        R.Left := iStart + iLeft;
        Inc(R.Top);
        DrawText(TargetCanvas.Handle, PChar(Copy(strText, M.Index, M.Length)), M.Length, R,
          DT_LEFT Or DT_VCENTER Or DT_NOPREFIX);
      End;
  End;

Var
  strText: String;

Begin
  If Not (doCustomDrawing In TBADIOptions.BADIOptions.Options) And (ExplorerFilter <> '') Then
    Begin
      strText := (Sender As TVirtualStringTree).Text[Node, 0];
      HighlightText(FFilterRegEx.Matches(strText), strText,
        FTokenFontInfo[ttSearchHighlight].FForeColour);
    End;
End;

(**

  This method is an OnDrawItem event handler for the treeview. Depending on the
  options it renders the syntax highlighted version of the tree.

  @precon  Sender is the control that invoked the event, Node is the node in
           the treeview to be drawn, State is the current drawing state of
           the node and DefaultDraw - this is change to false to allow custom
           drawing.
  @postcon This event handler draw a custom item in the tree view.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas
  @param   Node         as a PVirtualNode
  @param   ItemRect     as a TRect
  @param   CustomDraw   as a Boolean as a reference

**)
procedure TframeModuleExplorer.tvExplorerBeforeItemPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var CustomDraw: Boolean);

Var
  iPos: Integer;
  R : TRect;
  sl : TStringList;

begin
  FTargetCanvas := TargetCanvas;
  FNode := Node;
  CustomDraw := (doCustomDrawing In TBADIOptions.BADIOptions.Options);
  If Not CustomDraw Then
    Exit;
  FNodeData := Sender.GetNodeData(Node);
  sl := FNodeData.FNode.Tokens;
  DrawHighlightSelectedItem(FExplorer.Color, ItemRect);
  DrawSelectedNode(sl, ItemRect, FExplorer.Images.Width, iPos);
  R := ItemRect;
  DrawTree(R);
  DrawImage(R);
  DrawTreeText(sl, R);
end;

(**

  This is an on change event handler for the Explorer Module VTV control.

  @precon  None.
  @postcon Updates the Follow Node.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TframeModuleExplorer.tvExplorerChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

Begin
  FFollowNode := Node;
End;

(**

  This is an on click event handler for the explorer tree view.

  @precon  None.
  @postcon Fires a Selection Change event for the specifically selected item.

  @param   Sender as a TObject

 **)
Procedure TframeModuleExplorer.tvExplorerClick(Sender: TObject);

Var
  NodeData, ND: PBADITreeData;

Begin
  If FSelectionChanging Then
    Exit;
  FSelectionChanging := True;
  Try
    If Assigned(FExplorer.FocusedNode) Then
      If Assigned(FSelectionChange) And Not FRendering Then
        Begin
          NodeData := FExplorer.GetNodeData(FExplorer.FocusedNode);
          If Assigned(NodeData.FNode) Then
            If Not Assigned(NodeData.FNode.Comment) Then
              Begin
                ND := FollowMethodNodeData(NodeData.FNode.Line);
                //: @todo Check for method
                If Assigned(NodeData) And Assigned(ND) And Assigned(ND.FNode.Comment) Then
                  FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col, ND.FNode.Comment.Line)
                Else
                  FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col, NodeData.FNode.Line);
              End Else
              Begin
                ND := FollowMethodNodeData(NodeData.FNode.Line);
                //: @todo Check for method
                If Assigned(NodeData) And Assigned(ND) And Assigned(ND.FNode.Comment) Then
                  FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col, ND.FNode.Comment.Line)
                Else
                  FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col, NodeData.FNode.Comment.Line);
              End;
        End;
  Finally
    FSelectionChanging := False;
  End;
End;

(**

  This is an on get image index event handler for the virtual tree view.

  @precon  None.
  @postcon Sets the image index of the tree view item from the associated Tree Bode Info class.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as a TImageIndex as a reference

**)
procedure TframeModuleExplorer.tvExplorerGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);

Var
  NodeData : PBADITreeData;

begin
  NodeData := FExplorer.GetNodeData(Node);
  If Kind In [ikNormal, ikSelected] Then
    ImageIndex := NodeData.FNode.ImageIndex;
end;

(**

  This is an on get text event handler for the virtual tree view. It gets the
  text from the tree node info associated with the tree item.

  @precon  None.
  @postcon Gets the text for the tree node from the associated Tree Node Info
           class.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
procedure TframeModuleExplorer.tvExplorerGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);

Var
  NodeData : PBADITreeData;

begin
  NodeData := FExplorer.GetNodeData(Node);
  CellText := NodeData.FNode.Text;
end;

(**

  This method is an on key press event handler for the tree view.

  @precon  None.
  @postcon If an on focus event handler is assigned and enter is pressed it is fired else edits the
           explorer tree view filter.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
Procedure TframeModuleExplorer.tvExplorerKeyPress(Sender: TObject; Var Key: Char);

Var
  KeyStates: TKeyboardState;
  ShiftStates: TShiftState;

Begin
  If Not FFiltering Then
    Begin
      FFiltering := True;
      Try
        Case Key Of
          #08:
            Begin
              ExplorerFilter := Copy(ExplorerFilter, 1, Length(ExplorerFilter) - 1);
              FLastFilterUpdate := GetTickCount;
              Key := #0;
            End;
          #13:
            Begin
              tvExplorerClick(Sender);
              GetKeyboardState(KeyStates);
              ShiftStates := KeyboardStateToShiftState(KeyStates);
              If ShiftStates = [] Then
                If Assigned(OnFocus) Then
                  FFocus(Sender);
              Key := #0;
            End;
          #27:
            Begin
              ExplorerFilter := '';
              FLastFilterUpdate := GetTickCount;
              Key := #0;
            End;
          #32..#128:
            Begin
              ExplorerFilter := ExplorerFilter + Key;
              FLastFilterUpdate := GetTickCount;
              Key := #0;
            End;
        End;
      Finally
        FFiltering := False;
      End;
    End;
End;

(**

  This is an on measure item event handler for the tree view.

  @precon  None.
  @postcon Returns the height of the item based on the tree view font.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas
  @param   Node         as a PVirtualNode
  @param   NodeHeight   as an Integer as a reference

**)
procedure TframeModuleExplorer.tvExplorerMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);

Var
  NodeData : PBADITreeData;
  sl: TStringList;
  iToken: Integer;

Begin
  NodeData := Sender.GetNodeData(Node);
  If Assigned(NodeData.FNode) Then
    Begin
      InitCanvasFont(TargetCanvas, tpFixed In NodeData.FNode.TagProperties, FBADIOptions);
      sl := NodeData.FNode.Tokens;
      NodeHeight := 1 + TargetCanvas.TextHeight(strNodeHeightTest) + 1;
      For iToken := 0 To sl.Count - 1 Do
        If TBADITokenType(sl.Objects[iToken]) = ttLineEnd Then
          Inc(NodeHeight, TargetCanvas.TextHeight(strNodeHeightTest));
      If NodeHeight < Integer(FExplorer.DefaultNodeHeight) Then
        NodeHeight := Integer(FExplorer.DefaultNodeHeight);
    End;
End;

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
  Node : PVirtualNode;
  Rect : TRect;
  C : TComment;
  HitInfo: THitInfo;
  NodeData : PBADITreeData;

begin
  C := Nil;
  FExplorer.GetHitTestInfoAt(X, Y, True, HitInfo);
  If (hiOnItemLabel In HitInfo.HitPositions) Then
    Begin
      Node := HitInfo.HitNode;
      If (Node <> Nil) And (Node <> FLastNode) Then
        Begin
          FLastNode := Node;
          NodeData := FExplorer.GetNodeData(Node);
          If doShowCommentHints In TBADIOptions.BADIOptions.Options Then
            C := NodeData.FNode.Comment;
          Rect := FHintWin.CalcHintRect(FExplorer.ClientWidth, Screen.Width,
            Node, doCustomDrawing In TBADIOptions.BADIOptions.Options, C);
          If (Rect.Right <= FExplorer.ClientWidth) And ((C = Nil) Or
            ((C.TokenCount = 0) And (C.TagCount = 0))) Then
            Begin
              FHintWin.ReleaseHandle;
              Exit;
            End;
          Rect.TopLeft := FExplorer.ClientToScreen(Rect.TopLeft);
          Rect.BottomRight := FExplorer.ClientToScreen(Rect.BottomRight);
          FHintWin.ActivateHint(Rect, Node,
            doCustomDrawing In TBADIOptions.BADIOptions.Options, C);
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

  This is an on node expanded / collapsed event handler for the explorer view.

  @precon  None.
  @postcon Attempts to re-focus the followed node when nodes are expanded or collapsed.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TframeModuleExplorer.tvExplorerNodeExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);

Begin
  FocusFollowedNode;
End;

(**

  This method update the status of the module explorer statusbar.

  @precon  M must be a valid instance of a TBaseLanguageModule.
  @postcon Update the status of the module explorer statusbar.

  @param   Module as a TBaseLanguageModule as a constant

**)
procedure TframeModuleExplorer.UpdateStatusBar(Const Module: TBaseLanguageModule);

Const
  strOutputFmt = '%s: %1.1n';
  strCommaSep = ', ';
  dblTimingLimit = 1.0;

Var
  strTickLabel: String;
  dblTicks: Double;
  i : Integer;
  strText : String;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateStatusBar', tmoTiming);{$ENDIF}
  SetStatusPanel(spiBytes, strStatusbarBytesText, Module.Bytes);
  SetStatusPanel(spiTokens, strStatusbarTokensText, Module.TokenCount);
  SetStatusPanel(spiLines, strStatusbarLinesText, Module.Lines);
  If doShowPerformanceCountersInModuleExplorer In TBADIOptions.BADIOptions.Options Then
    Begin
      strText := '';
      For i := 1 To Module.OpTickCounts - 1 Do
        Begin
          strTickLabel := Module.OpTickCountName[i];
          dblTicks := Module.OpTickCountByIndex[i] - Module.OpTickCountByIndex[i - 1];
          If dblTicks > dblTimingLimit Then
            Begin
              If strText <> '' Then
                strText := strText + strCommaSep;
              strText := strText + Format(strOutputFmt, [strTickLabel, dblTicks]);
            End;
        End;
      If strText <> '' Then
        strText := strText + strCommaSep;
      strText := strText +
        Format(strOutputFmt, [strTotal, Module.OpTickCountByIndex[Module.OpTickCounts - 1] -
        Module.OpTickCountByIndex[0]]);
      stbStatusBar.Panels[Byte(spiFourth)].Text := strText;
      stbStatusBar.Hint := strText;
    End Else
    Begin
      stbStatusBar.Panels[Byte(spiFourth)].Text := '';
      stbStatusBar.Hint := '';
    End;
end;

End.

