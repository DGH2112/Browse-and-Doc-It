(**

  This module excapsulates the creation of menus in the IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    03 Jan 2018

**)
Unit BADI.IDEMenuInstaller;

Interface

Uses
  ToolsAPI,
  Menus,
  Classes,
  BADI.EditorNotifier,
  BADI.Base.Module,
  Windows,
  BADI.ProfilingForm,
  BADI.Types,
  Contnrs,
  ActnList,
  Graphics;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class manages the creation and destruction of the IDE menus. **)
  TBADIIDEMenuInstaller = Class
  Strict Private
    FBADIMenu       : TMenuItem;
    FBADIActions    : Array[Low(TBADIMenu)..High(TBADIMenu)] Of TAction;
    FEditorNotifier : TEditorNotifier;
  Strict Protected
    Function  CreateMenuItem(Const mmiParent: TMenuItem; Const eBADIMenu: TBADIMenu;
      Const ClickProc, UpdateProc: TNotifyEvent; Const iImageIndex : Integer) : TMenuItem;
    Procedure InsertCommentBlock(Const CommentStyle: TCommentStyle; Const CommentType: TCommentType);
    Function  IsTextSelected: Boolean;
    Function  SelectedText(Const boolDelete : Boolean): String;
    Procedure DeleteExistingComment(Const Source: IOTASourceEditor; Const iStartLine, iEndLine: Integer);
    Procedure InsertComment(Const strComment: String; Const Writer: IOTAEditWriter;
      Const iInsertLine: Integer; Const Source: IOTASourceEditor);
    Procedure PositionCursorInFunction(Const CursorDelta: TPoint; Const iInsertLine, iIndent: Integer;
      Const strComment: String);
    Procedure ProcessProfilingCode(Const Module : TBaseLanguageModule; Const SE: IOTASourceEditor;
      Const ProfileJob: TProfileJob; Const iTabs: Integer);
    Procedure InsertProfileCode(Const SE: IOTASourceEditor; Const ProfileJob: TProfileJob;
      Const strProlog, strEpilog: String);
    Procedure RemoveProfileCode(Const SE: IOTASourceEditor; Const ProfileJob: TProfileJob;
      Const slProlog, slEpilog: TStringList);
    Procedure DeleteProfileCode(Const SE: IOTASourceEditor; Const iStartLine, iEndLine: Integer);
    Procedure MethodCommentClick(Sender: TObject);
    Procedure PropertyCommentClick(Sender: TObject);
    Procedure BlockCommentClick(Sender: TObject);
    Procedure LineCommentClick(Sender: TObject);
    Procedure InSituCommentClick(Sender: TObject);
    Procedure ToDoCommentClick(Sender: TObject);
    Procedure DocumentationClick(Sender: TObject);
    Procedure DUnitClick(Sender: TObject);
    Procedure ProfilingClick(Sender: TObject);
    Procedure OptionsClick(Sender: TObject);
    Procedure ModuleExplorerClick(Sender: TObject);
    Procedure MetricsClick(Sender : TObject);
    Procedure ChecksClick(Sender : TObject);
    Procedure CreateBADIMainMenu;
    Procedure RemoveActionsFromToolbars;
    Function  AddImagesToIDE : Integer;
    Procedure NilActions;
    Procedure FreeActions;
    Procedure RefactorConstantClick(Sender : TObject);
  Public
    Constructor Create(Const EditorNotifier : TEditorNotifier);
    Destructor Destroy; Override;
    Procedure SelectionChange(Const iIdentLine, iIdentCol, iCommentLine, iCommentCol: Integer);
    Procedure Focus(Sender: TObject);
    Procedure OptionsChange(Sender: TObject);
    Procedure UpdateMenuShortcuts;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  BADI.Documentation.Dispatcher,
  BADI.ToolsAPIUtils,
  BADI.DocumentationOptionsForm,
  ShellAPI,
  Forms,
  BADI.DUnitCreator,
  BADI.DUnitForm,
  Dialogs,
  Controls,
  BADI.CommonIDEFunctions,
  BADI.DockableModuleExplorer,
  ProgressForm,
  BADI.Module.Dispatcher,
  BADI.ElementContainer,
  BADI.Generic.FunctionDecl,
  BADI.ResourceStrings,
  BADI.Generic.MethodDecl,
  BADI.Generic.PropertyDecl,
  BADI.Options,
  BADI.Functions,
  ComCtrls,
  BADI.Constants, 
  BADI.Base.Documentation, 
  BADI.Refactor.Constant, 
  BADI.Module.Metrics, 
  BADI.Module.Checks;

ResourceString
  (** This is a resource message to confirm whether the selected text should be
      moved. **)
  strThereIsSelectedText = 'There is selected text in the editor. Do you want to move this text ' +
    'within the new comment';

{ TBADIIDEMenuInstaller }

(**

  This method adds multiple images from the projects resource (bitmap) to the IDEs image list.
  The image name in the resource must end in Image as this is appended to the given name.
  An integer for the position of the first image in the IDEs image list is returned.

  @precon  None.
  @postcon The named image is loaded from the projects resource and put into the IDEs
           image list and its index returned.

  @return  an Integer

**)
Function TBADIIDEMenuInstaller.AddImagesToIDE : Integer;

Const
  strImage = 'Image';

Var
  NTAS : INTAServices;
  ilImages : TImageList;
  BM : TBitMap;
  iMenu: TBADIMenu;

begin
  NTAS := (BorlandIDEServices As INTAServices);
  ilImages := TImageList.Create(Nil);
  Try
    For iMenu := Low(TBADIMenu) To High(TBADIMenu) Do
      If FindResource(hInstance, PChar(BADIMenus[iMenu].FName + strImage), RT_BITMAP) > 0 Then
        Begin
          BM := TBitMap.Create;
          Try
            BM.LoadFromResourceName(hInstance, BADIMenus[iMenu].FName + strImage);
            ilImages.AddMasked(BM, BADIMenus[iMenu].FMaskColor);
          Finally
            BM.Free;
          End;
        End;
    Result := NTAS.AddImages(ilImages);
  Finally
    ilImages.Free;
  End;
end;

(**

  This method is a menu OnClick event for the insertion of a comment block at
  the cursor.

  This simple adds a comment block at the current cursor position in the active
  source code editor.

  @precon  Sender is the object initiating the event.
  @postcon Inserts a block comment into the editor.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.BlockCommentClick(Sender: TObject);

Var
  SE: IOTASourceEditor;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    InsertCommentBlock(csBlock, TBADIDispatcher.BADIDispatcher.GetCommentType(SE.FileName, csBlock));
End;

(**

  This is an on action event handler for the BADI Checks project editor view.

  @precon  None.
  @postcon Displays the editor view of module/method checks.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.ChecksClick(Sender: TObject);

Begin
  TBADIModuleChecksEditorView.CreateEditorView; 
End;

(**

  A constructor for the TBADIIDEMenuInstaller class.

  @precon  None.
  @postcon Creates the IDE menus.

  @param   EditorNotifier as a TEditorNotifier as a Constant

**)
Constructor TBADIIDEMenuInstaller.Create(Const EditorNotifier : TEditorNotifier);

Var
  iImageIndex: Integer;

Begin
  NilActions;
  FEditorNotifier := EditorNotifier;
  CreateBADIMainMenu;
  iImageIndex := AddImagesToIDE;
  CreateMenuItem(FBADIMenu, bmModuleExplorer, ModuleExplorerClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmDocumentation, DocumentationClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmDunit, DUnitClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmProfiling, ProfilingClick, Nil, iImageIndex);
  CreateMenuItem(FBADIMenu, bmSep1, Nil, Nil, 0);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmFocusEditor, Focus, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmMethodComment, MethodCommentClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmPropertyComment, PropertyCommentClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmBlockComment, BlockCommentClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmLineComment, LineCommentClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmInSituComment, InSituCommentClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmToDoComment, ToDoCommentClick, Nil, iImageIndex);
  CreateMenuItem(FBADIMenu, bmSep2, Nil, Nil, 0);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmRefactorConstant, RefactorConstantClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmBADIMetrics, MetricsClick, Nil, iImageIndex);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmBADIChecks, ChecksClick, Nil, iImageIndex);
  CreateMenuItem(FBADIMenu, bmSep3, Nil, Nil, 0);
  Inc(iImageIndex);
  CreateMenuItem(FBADIMenu, bmOptions, OptionsClick, Nil, iImageIndex);
End;

(**

  This method creates the main BADI menu item and stores it for later disposal.

  @precon  None.
  @postcon The main BADI menu it added to the IDE.

**)
Procedure TBADIIDEMenuInstaller.CreateBADIMainMenu;

ResourceString
  {$IFDEF DEBUG}
  strBrowseAndDocItDEBUG = '&Browse and Doc It %d.%d%s BETA (Build %d.%d.%d.%d)';
  {$ELSE}
  strBrowseAndDocIt = '&Browse and Doc It %d.%d%s';
  {$ENDIF}

  Const
  iMenuOffset = 2;

Var
  mmiMainMenu: TMainMenu;
  iMajor, iMinor, iBugFix, iBuild : Integer;

Begin
  mmiMainMenu := (BorlandIDEServices As INTAServices).MainMenu;
  FBADIMenu := TMenuItem.Create(mmiMainMenu);
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  {$IFDEF DEBUG}
  FBADIMenu.Caption := Format(strBrowseAndDocItDEBUG, [iMajor, iMinor, strRevision[Succ(iBugfix)],
    iMajor, iMinor, iBugfix, iBuild]);
  {$ELSE}
  FBADIMenu.Caption := Format(strBrowseAndDocIt, [iMajor, iMinor, strRevision[Succ(iBugfix)]]);
  {$ENDIF}
  mmiMainMenu.Items.Insert(mmiMainMenu.Items.Count - iMenuOffset, FBADIMenu);
End;

(**

  This method creates menu items using the passed information.

  @precon  mmiParent must be a valid parent menu item in the IDE .
  @postcon A Sub menu ite is created under mmiParent .

  @param   mmiParent   as a TMenuItem as a constant
  @param   eBADIMenu   as a TBADIMenu as a constant
  @param   ClickProc   as a TNotifyEvent as a constant
  @param   UpdateProc  as a TNotifyEvent as a constant
  @param   iImageIndex as an Integer as a constant
  @return  a TMenuItem

**)
Function TBADIIDEMenuInstaller.CreateMenuItem(Const mmiParent: TMenuItem; Const eBADIMenu : TBADIMenu;
  Const ClickProc, UpdateProc : TNotifyEvent; Const iImageIndex : Integer) : TMenuItem;

Const
  strAction = 'Action';
  strCategory = 'BADIActions';
  strMenu = 'Menu';
  
Var
  NTAS: INTAServices;
  Actn : TAction;

Begin
  NTAS := (BorlandIDEServices As INTAServices);
  // Create the IDE action (cached for removal later)
  Actn := Nil;
  Result := TMenuItem.Create(NTAS.MainMenu);
  If Assigned(ClickProc) Then
    Begin
      Actn := TAction.Create(NTAS.ActionList);
      Actn.ActionList := NTAS.ActionList;
      Actn.Name := BADIMenus[eBADIMenu].FName + strAction;
      Actn.Caption := BADIMenus[eBADIMenu].FCaption;
      Actn.OnExecute := ClickProc;
      Actn.OnUpdate := UpdateProc;
      Actn.ShortCut := TextToShortCut(TBADIOptions.BADIOptions.MenuShortcut[eBADIMenu]);
      Actn.ImageIndex := iImageIndex;
      Actn.Category := strCategory;
      FBADIActions[eBADIMenu] := Actn;
    End Else
  If BADIMenus[eBADIMenu].FCaption <> '' Then
    Begin
      Result.Caption := BADIMenus[eBADIMenu].FCaption;
      Result.ImageIndex := iImageIndex;
    End Else
      Result.Caption := '-';
  // Create menu (removed through parent menu)
  Result.Action := Actn;
  Result.Name := BADIMenus[eBADIMenu].FName + strMenu;
  // Create Action and Menu.
  mmiParent.Add(Result);
End;

(**

  This method deletes the comment between the start and end lines of the editor.

  @precon  None.
  @postcon Deletes the comment between the start and end lines of the editor.

  @param   Source     as an IOTASourceEditor as a constant
  @param   iStartLine as an Integer as a constant
  @param   iEndLine   as an Integer as a constant

**)
Procedure TBADIIDEMenuInstaller.DeleteExistingComment(Const Source: IOTASourceEditor; Const iStartLine,
  iEndLine: Integer);

Var
  Writer                  : IOTAEditWriter;
  ptStart, ptEnd          : TOTACharPos;
  iBufferStart, iBufferEnd: Integer;

Begin
  Writer := Source.CreateUndoableWriter;
  Try
    ptStart.Line      := iStartLine;
    ptStart.CharIndex := 0;
    iBufferStart      := Source.GetEditView(0).CharPosToPos(ptStart);
    Writer.CopyTo(iBufferStart);
    ptEnd.Line      := iEndLine;
    ptEnd.CharIndex := 0;
    iBufferEnd      := Source.GetEditView(0).CharPosToPos(ptEnd);
    Writer.DeleteTo(iBufferEnd);
  Finally
    Writer := Nil;
  End;
End;

(**

  This method deletes the profile code from the editor window.

  @precon  SE must be a valid instance.
  @postcon Deletes the profile text between the given line numbers.

  @param   SE         as an IOTASourceEditor as a constant
  @param   iStartLine as an Integer as a constant
  @param   iEndLine   as an Integer as a constant

**)
Procedure TBADIIDEMenuInstaller.DeleteProfileCode(Const SE: IOTASourceEditor; Const iStartLine,
  iEndLine: Integer);

Var
  Writer                  : IOTAEditWriter;
  C1, C2                  : TOTACharPos;
  iBufferPos1, iBufferPos2: Integer;

Begin
  Writer := SE.CreateUndoableWriter;
  Try
    C1.Line      := iStartLine;
    C1.CharIndex := 0;
    iBufferPos1  := SE.GetEditView(0).CharPosToPos(C1);
    C2.Line      := iEndLine + 1;
    C2.CharIndex := 0;
    iBufferPos2  := SE.GetEditView(0).CharPosToPos(C2);
    Writer.CopyTo(iBufferPos1);
    Writer.DeleteTo(iBufferPos2);
  Finally
    Writer := Nil;
  End;
End;

(**

  A destructor for the TBADIIDEMenuInstaller class.

  @precon  None.
  @postcon Uninstalls the menu from the IDE.

**)
Destructor TBADIIDEMenuInstaller.Destroy;

Begin
  If FBADIMenu <> Nil Then
    FBADIMenu.Free;
  RemoveActionsFromToolbars;
  FreeActions;
  Inherited Destroy;
End;

(**


  This is an on click event handler for the documentation menu.

  @precon  None.
  @postcon Invokes the documentation of the current active project.


  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.DocumentationClick(Sender: TObject);

Const
  strDocumentation = 'Documentation';
  strVerb = 'open';
  
Var
  ADocType: TDocType;
  AProject: IOTAProject;
  i       : Integer;
  DD : TBaseDocumentation;

Begin
  AProject := ActiveProject;
  If AProject <> Nil Then
    If TfrmDocumentationOptions.Execute(ADocType) Then
      Begin
        DD := DocumentDispatcher(
          ExtractFilePath(AProject.FileName) + strDocumentation,
          {$IFDEF D2005}
          ExtractFileName(AProject.ProjectOptions.TargetName), ADocType)
          {$ELSE}
          ExtractFileName(AProject.FileName), ADocType)
          {$ENDIF};
        Try
          DD.Add(AProject.FileName);
          For i := 0 To AProject.ModuleFileCount - 1 Do
            DD.Add(AProject.ModuleFileEditors[i].FileName);
          For i := 0 To AProject.GetModuleCount - 1 Do
            DD.Add(AProject.GetModule(i).FileName);
          DD.OutputDocumentation;
          ShellExecute(Application.Handle, strVerb, PChar(DD.MainDocument), '', '', SW_SHOWNORMAL);
        Finally
          Free;
        End;
      End;
End;

(**

  This method creates an instance of a DUnit form and passes a class instance
  that can create projects and units for the form.

  @precon  None.
  @postcon Creates an instance of a DUnit form and passes a class instance
           that can create projects and units for the form.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.DUnitClick(Sender: TObject);

ResourceString
  strSelectSourceCode = 'You must select a source code editor to create unit tests.';
  strNoSelectedProject = 'There is no active project in the project group.';

Var
  D: TDUnitCreator;

Begin
  If ActiveSourceEditor <> Nil Then
    Begin
      If ActiveProject <> Nil Then
        Begin
          D := TDUnitCreator.Create;
          Try
            TfrmDUnit.Execute(D);
          Finally
            D.Free;
          End;
        End
      Else
        MessageDlg(strNoSelectedProject, mtError, [mbOK], 0);
    End
  Else
    MessageDlg(strSelectSourceCode, mtError, [mbOK], 0);
End;

(**

  This method is an event handler for the On Focus event from the explorer
  frame.

  @precon  None.
  @postcon Focuses the active editor.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.Focus(Sender: TObject);

Const
  strTEditControl = 'TEditControl';
  
Var
  i  : Integer;
  frm: TCustomForm;

Begin
  If ActiveSourceEditor <> Nil Then
    Begin
      ActiveSourceEditor.Show;
      // IDE hack to focus the editor window because the above line doesn't do it
      frm   := ActiveSourceEditor.EditViews[0].GetEditWindow.Form;
      For i := 0 To frm.ComponentCount - 1 Do
        If frm.Components[i].ClassName = strTEditControl Then
          Begin
            If (frm.Components[i] As TWinControl).Visible Then
              (frm.Components[i] As TWinControl).SetFocus;
            Break;
          End;
    End;
End;

(**

  This method cycles through the BADI Menu Actions and frees any that have been created.

  @precon  None.
  @postcon All BADI Actions are freed and removed from the IDE.

**)
Procedure TBADIIDEMenuInstaller.FreeActions;

Var
  iBADIMenu: TBADIMenu;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    If Assigned(FBADIActions[iBADIMenu]) Then
      FBADIActions[iBADIMenu].Free;
End;

(**

  This method inserts the given comment into the editor at the given insert line.

  @precon  None.
  @postcon Inserts the given comment into the editor at the given insert line.

  @param   strComment  as a String as a constant
  @param   Writer      as an IOTAEditWriter as a constant
  @param   iInsertLine as an Integer as a constant
  @param   Source      as an IOTASourceEditor as a constant

**)
Procedure TBADIIDEMenuInstaller.InsertComment(Const strComment: String; Const Writer: IOTAEditWriter;
  Const iInsertLine: Integer; Const Source: IOTASourceEditor);

Var
  iBufferPos: Integer;
  C         : TOTACharPos;

Begin
  C.Line      := iInsertLine;
  C.CharIndex := 0;
  iBufferPos  := Source.GetEditView(0).CharPosToPos(C);
  Writer.CopyTo(iBufferPos);
  OutputText(Writer, strComment);
End;

(**

  This method inserts either a Block, Line or InSitu comment at the position of the curerent cursor 
  depending on the passed partameter.

  @precon  None.
  @postcon Inserts the specified comment.

  @param   CommentStyle as a TCommentStyle as a constant
  @param   CommentType  as a TCommentType as a constant

**)
Procedure TBADIIDEMenuInstaller.InsertCommentBlock(Const CommentStyle: TCommentStyle; Const CommentType: TCommentType);

Const
  iDefaultSpacignIndent = 2;
  iNonBlockIndent = 4;
  
Var
  SourceEditor   : IOTASourceEditor;
  EditPos        : TOTAEditPos;
  CharPos        : TOTACharPos;
  Writer         : IOTAEditWriter;
  iIndent        : Integer;
  strSelectedText: String;
  EV : IOTAEditView;

Begin
  If CommentType = ctNone Then
    Exit;
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  If IsTextSelected Then
    Case MessageDlg(strThereIsSelectedText, mtConfirmation, [mbYes, mbNo,
      mbCancel], 0) Of
      mrCancel: Exit;
      mrNo:     strSelectedText := '';
    Else
      strSelectedText := SelectedText(True);
    End;
  EV := SourceEditor.GetEditView(0);
  EditPos := EV.CursorPos;
  iIndent := EV.CursorPos.Col;
  Writer := SourceEditor.CreateUndoableWriter;
  Try
    CharPos.Line      := EditPos.Line;
    CharPos.CharIndex := EditPos.Col;
    Writer.CopyTo(SourceEditor.GetEditView(0).CharPosToPos(CharPos) - 1);
    OutputText(Writer, BuildBlockComment(CommentType, CommentStyle, iIndent,
      strSelectedText));
  Finally
    Writer := Nil;
  End;
  // Get header in view if not already
  EV := SourceEditor.GetEditView(0);
  Case CommentStyle Of
    csBlock: SelectionChange(EditPos.Line + 4 + 1, EditPos.Col, EditPos.Line, EditPos.Col);
  Else
    SelectionChange(EditPos.Line + 1, EditPos.Col, EditPos.Line, EditPos.Col);
  End;
  // Place cursor at start of comment
  Case CommentStyle Of
    csBlock:
      Begin
        EditPos.Line := EditPos.Line + iDefaultSpacignIndent;
        EditPos.Col  := EditPos.Col + iDefaultSpacignIndent;
      End;
  Else
    EditPos.Col := EditPos.Col + iNonBlockIndent;
  End;
  EV.CursorPos := EditPos;
  EV.Paint;
End;

(**

  This method inserts profiling code into the editor.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Inserts into the editor profiling code for the given profiling job.

  @param   SE         as an IOTASourceEditor as a constant
  @param   ProfileJob as a TProfileJob as a constant
  @param   strProlog  as a String as a constant
  @param   strEpilog  as a String as a constant

**)
Procedure TBADIIDEMenuInstaller.InsertProfileCode(Const SE: IOTASourceEditor;
  Const ProfileJob: TProfileJob; Const strProlog, strEpilog: String);

Var
  Writer    : IOTAEditWriter;
  iBufferPos: Integer;
  C         : TOTACharPos;

Begin
  Writer := SE.CreateUndoableWriter;
  Try
    C.Line      := ProfileJob.EndLine + 1;
    C.CharIndex := 0;
    iBufferPos  := SE.GetEditView(0).CharPosToPos(C);
    Writer.CopyTo(iBufferPos);
    OutputText(Writer, strEpilog);
  Finally
    Writer := Nil;
  End;
  Writer := SE.CreateUndoableWriter;
  Try
    C.Line      := ProfileJob.StartLine;
    C.CharIndex := 0;
    iBufferPos  := SE.GetEditView(0).CharPosToPos(C);
    Writer.CopyTo(iBufferPos);
    OutputText(Writer, strProlog);
  Finally
    Writer := Nil;
  End;
End;

(**

  This is an action for the Insert In Situ Comment event handler. It inserts an
  in sity comment at the current cursor position.

  @precon  None.
  @postcon Inserts an InSitu comment into the editor.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.InSituCommentClick(Sender: TObject);
Var
  SE: IOTASourceEditor;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    InsertCommentBlock(csInSitu, TBADIDispatcher.BADIDispatcher.GetCommentType(SE.FileName, csInSitu));
End;

(**

  This method test whether there is selected text in the editors current view.

  @precon  None.
  @postcon Returns true of there is selected text.

  @return  a Boolean

**)
Function TBADIIDEMenuInstaller.IsTextSelected: Boolean;

Var
  SE    : IOTASourceEditor;
  Reader: IOTAEditReader;

Begin
  Result := False;
  SE     := ActiveSourceEditor;
  If SE <> Nil Then
    Begin
      Reader := SE.CreateReader;
      Try
        Result := SE.EditViews[0].Block.Visible;
      Finally
        Reader := Nil;
      End;
    End;
End;

(**

  This method is an on click event handler for the Insert Line Comment action.

  @precon  None.
  @postcon Inserts a line comment at the cursor location in the editor.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.LineCommentClick(Sender: TObject);
Var
  SE: IOTASourceEditor;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    InsertCommentBlock(csLine, TBADIDispatcher.BADIDispatcher.GetCommentType(SE.FileName, csLine));
End;

(**

  This is a menu OnClick event for the insertion of a method comment. This method
  searches the IDE for the current module being edited and then creates a
  memory stream of the source and passes it to the Unit parser. It then finds
  the first method declaration prior to the cursor position, parses the
  declaration and output the information in as comment immediately above the
  method declaration. This comment block starts with '(**' to signify an
  ObjectPascalDoc comment that can be used by the documentation system.

  @precon  Sender is the object initiating the event .
  @postcon Inserts a Method comment into the editor avoid the current method .

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.MethodCommentClick(Sender: TObject);

Var
  Module          : TBaseLanguageModule;
  EditPos         : TOTAEditPos;
  T               : TElementContainer;
  F               : TGenericFunction;
  Writer          : IOTAEditWriter;
  Source          : IOTASourceEditor;
  CursorDelta     : TPoint;
  iIndent         : Integer;
  strComment      : String;
  iInsertLine     : Integer;
  iMaxCommentWidth: Integer;

Begin
  Source := ActiveSourceEditor;
  If Source = Nil Then
    Exit;
  Module := TBADIDispatcher.BADIDispatcher.Dispatcher(EditorAsString(Source), Source.FileName,
    Source.Modified, [moParse]);
  If Module <> Nil Then
    Try
      EditPos := Source.GetEditView(0).CursorPos;
      T       := Module.FindElement(strImplementedMethodsLabel);
      If T <> Nil Then
        Begin
          F := FindFunction(EditPos.Line, T, TGenericMethodDecl);
          If F <> Nil Then
            Begin
              iIndent := FindIndentOfFirstTokenOnLine(Module, F.Line) - 1;
              If F.Comment <> Nil Then
                Begin
                  If MessageDlg(Format(strMethodAlreadyExists, [F.QualifiedName]),
                    mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
                    Exit;
                  iInsertLine := F.Comment.Line;
                  DeleteExistingComment(Source, F.Comment.Line, F.Line);
                End
              Else
                iInsertLine    := F.Line;
              iMaxCommentWidth := Source.EditViews[0].Buffer.BufferOptions.RightMargin;
              Writer           := Source.CreateUndoableWriter;
              Try
                strComment := WriteComment(F,
                  TBADIDispatcher.BADIDispatcher.GetCommentType(Source.FileName, csBlock), iIndent,
                  True, CursorDelta, iMaxCommentWidth);
                InsertComment(strComment, Writer, iInsertLine, Source);
              Finally
                Writer := Nil;
              End;
              PositionCursorInFunction(CursorDelta, iInsertLine, iIndent, strComment);
            End
          Else
            MessageDlg(strNoMethodFound, mtWarning, [mbOK], 0);
        End;
    Finally
      Module.Free;
    End;
End;

(**

  This method displays the module statistics for the currentl visible module.

  @precon  None.
  @postcon The modulel statistics are displays with the current modules information.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.MetricsClick(Sender: TObject);

Begin
  TBADIModuleMetricsEditorView.CreateEditorView; 
End;

(**

  This is a TMenuItem on click event. If display the module explorer if its not
  visible else hide or focuses the explorer.

  @precon  Sender is the object initiating the event.
  @postcon Displays the Module Explorer.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.ModuleExplorerClick(Sender: TObject);

Begin
  TfrmDockableModuleExplorer.ShowDockableModuleExplorer;
End;

(**

  This method ensures all the BADI Menu Actions are initialised to NIL.

  @precon  None.
  @postcon All BADI menu actions are nil.

**)
Procedure TBADIIDEMenuInstaller.NilActions;

Var
  iBADIMenu: TBADIMenu;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    If Assigned(FBADIActions[iBADIMenu]) Then
      FBADIActions[iBADIMenu] := Nil;
End;

(**

  This is an event handler to be hooked the the Module Explorer Frames
  OnOptionsChange event handler.

  @precon  None.
  @postcon Refreshes the current module view.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.OptionsChange(Sender: TObject);

Begin
  FEditorNotifier.ResetLastupdateTickCount(1);
End;

(**

  This is a TMenuItem on click event. it invokes the Options dialogue.

  @precon  Sender is the object initiating the event.
  @postcon Displays the wizards Options dialogue.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.OptionsClick(Sender: TObject);

Const
  strBADI = 'Browse and Doc It';

Begin
  {$IFDEF DXE00}
  (BorlandIDEServices As IOTAServices).GetEnvironmentOptions.EditOptions('', strBADI);
  {$ELSE}
  If TfrmOptions.Execute([Low(TVisibleTab)..High(TVisibleTab)]) Then
    Begin
      FEditorNotifier.ResetLastupdateTickCount(1);
      UpdateMenuShortcuts;
    End;
  {$ENDIF}
End;

(**

  This method positions the comment and function according to the options and then places the cursor in 
  the appropriate position for editing.

  @precon  None.
  @postcon Positions the comment and function according to the options and then places the cursor in the
           appropriate position for editing.

  @param   CursorDelta as a TPoint as a constant
  @param   iInsertLine as an Integer as a constant
  @param   iIndent     as an Integer as a constant
  @param   strComment  as a String as a constant

**)
Procedure TBADIIDEMenuInstaller.PositionCursorInFunction(Const CursorDelta: TPoint;
  Const iInsertLine, iIndent: Integer; Const strComment: String);

Var
  Pt: TPoint;
  S : IOTASourceEditor;
  C : TOTAEditPos;

Begin
  SelectionChange(iInsertLine + CharCount(#13, strComment), 1, iInsertLine, 1);
  Pt.Y := iInsertLine;
  Pt.X := 1;
  Inc(Pt.Y, CursorDelta.Y);
  Inc(Pt.X, CursorDelta.X);
  C.Col  := Pt.X;
  C.Line := Pt.Y;
  S := ActiveSourceEditor;
  If S <> Nil Then
    S.GetEditView(0).CursorPos := C;
End;

(**

  This method processes each of the profiling jobs given determining whether they are insertions or 
  removals.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Processes each of the profiling jobs given determining whether they are insertions or 
           removals.

  @param   Module     as a TBaseLanguageModule as a constant
  @param   SE         as an IOTASourceEditor as a constant
  @param   ProfileJob as a TProfileJob as a constant
  @param   iTabs      as an Integer as a constant

**)
Procedure TBADIIDEMenuInstaller.ProcessProfilingCode(Const Module : TBaseLanguageModule;
  Const SE: IOTASourceEditor; Const ProfileJob: TProfileJob; Const iTabs: Integer);

Var
  strTemplate       : String;
  slProlog, slEpilog: TStringList;

Begin
  strTemplate := StringReplace(TBADIOptions.BADIOptions.ProfilingCode[Module.ClassName],
    '|', #13#10, [rfReplaceAll]);
  slProlog := PrologCode(strTemplate, ProfileJob.Method, ProfileJob.Indent + iTabs);
  Try
    slEpilog := EpilogCode(strTemplate, ProfileJob.Method, ProfileJob.Indent + iTabs);
    Try
      Case ProfileJob.CodeType Of
        pctInsert:
          InsertProfileCode(SE, ProfileJob, slProlog.Text, slEpilog.Text);
        pctRemove:
          RemoveProfileCode(SE, ProfileJob, slProlog, slEpilog);
      End;
    Finally
      slEpilog.Free;
    End;
  Finally
    slProlog.Free;
  End;
End;

(**

  This is an on click event handler for the Porfiling menu item.

  @precon  None.
  @postcon Invokes the profiling dialogue for selecting the methods to be
           profiled.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.ProfilingClick(Sender: TObject);

ResourceString
  strSelectSourceCode = 'You must select a source code editor to create unit tests.';
  strStartingToProcess = 'Starting to process profiling information...';
  strProfiling = 'Profiling...';
  strProfilingMethod = 'Processing method "%s"...';
  strNothingToDo = 'Nothing to do!';
  strTheEditorBufferIsReadOnly = 'The editor buffer is read only.';


Var
  SE         : IOTASourceEditor;
  M          : TBaseLanguageModule;
  ProfileJobs: TProfileJobs;
  i          : Integer;
  iTabs      : Integer;
  frm        : TfrmProgress;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    Begin
      If Not SE.EditViews[0].Buffer.IsReadOnly Then
        Begin
          M := TBADIDispatcher.BADIDispatcher.Dispatcher(EditorAsString(SE), SE.FileName,
            SE.Modified, [moParse, moProfiling]);
          Try
            ProfileJobs := TfrmProfiling.Execute(M);
            Try
              If ProfileJobs <> Nil Then
                Begin
                  If (ProfileJobs.Count > 0) Then
                    Begin
                      iTabs := (BorlandIDEServices As IOTAEditorServices)
                        .EditOptions.BlockIndent;
                      frm := TfrmProgress.Create(Nil);
                      Try
                        frm.Init(ProfileJobs.Count - 1, strProfiling, strStartingToProcess);
                        For i := 0 To ProfileJobs.Count - 1 Do
                          Begin
                            ProcessProfilingCode(M, SE, ProfileJobs.ProfileJob[i], iTabs);
                            frm.UpdateProgress(i, Format(strProfilingMethod, [
                              ProfileJobs.ProfileJob[i].Method]));
                          End;
                      Finally
                        frm.Free;
                      End;
                    End
                  Else
                    MessageDlg(strNothingToDo, mtError, [mbOK], 0);
                End;
            Finally
              ProfileJobs.Free;
            End;
          Finally
            M.Free;
          End;
        End
      Else
        MessageDlg(strTheEditorBufferIsReadOnly, mtError, [mbOK], 0);
    End
  Else
    MessageDlg(strSelectSourceCode, mtError, [mbOK], 0);
End;

(**

  This is a menu OnClick event for the insertion of a property comment. This
  method searches the IDE for the current module being edited and then
  creates a memory stream of the source and passes it to the Unit parser.

  It then finds the first preperty declaration prior to the cursor position,
  parses the declaration and output the information in as comment immediately
  above the property declaration.

  This comment block starts with '(**' to signify an ObjectPascalDoc comment
  that can be used by the documentation system.

  @precon  Sender is the object initiating the event.
  @postcon Inserts a property comment into the editor for the current property.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.PropertyCommentClick(Sender: TObject);

Var
  Module          : TBaseLanguageModule;
  EditPos         : TOTAEditPos;
  Source          : IOTASourceEditor;
  T               : TElementContainer;
  F               : TGenericFunction;
  Writer          : IOTAEditWriter;
  iInsertLine     : Integer;
  iIndent         : Integer;
  strComment      : String;
  CursorDelta     : TPoint;
  iMaxCommentWidth: Integer;

Begin
  Source := ActiveSourceEditor;
  If Source = Nil Then
    Exit;
  Module := TBADIDispatcher.BADIDispatcher.Dispatcher(EditorAsString(Source), Source.FileName,
    Source.Modified, [moParse]);
  If Module <> Nil Then
    Try
      EditPos := Source.GetEditView(0).CursorPos;
      T       := Module.FindElement(strTypesLabel);
      If T <> Nil Then
        Begin
          F := FindFunction(EditPos.Line, Module, TGenericProperty);
          If F <> Nil Then
            Begin
              iIndent := FindIndentOfFirstTokenOnLine(Module, F.Line) - 1;
              If F.Comment <> Nil Then
                Begin
                  If MessageDlg(Format(strPropertyAlreadyExists, [F.Identifier]),
                    mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
                    Exit;
                  iInsertLine := F.Comment.Line;
                  DeleteExistingComment(Source, F.Comment.Line, F.Line);
                End
              Else
                iInsertLine := F.Line;
              Writer        := Source.CreateUndoableWriter;
              Try
                iMaxCommentWidth := Source.EditViews[0].Buffer.BufferOptions.RightMargin;
                strComment       := WriteComment(F,
                  TBADIDispatcher.BADIDispatcher.GetCommentType(Source.FileName,
                  csBlock), iIndent, False, CursorDelta, iMaxCommentWidth);
                InsertComment(strComment, Writer, iInsertLine, Source);
              Finally
                Writer := Nil;
              End;
              PositionCursorInFunction(CursorDelta, iInsertLine, iIndent, strComment);
            End
          Else
            MessageDlg(strNoPropertyFound, mtWarning, [mbOK], 0);
        End;
    Finally
      Module.Free;
    End;
End;

(**

  This method invokes the refactor constant code.

  @precon  None.
  @postcon The refactor constant code is invoked.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.RefactorConstantClick(Sender: TObject);

ResourceString
  strMsg = 'Refactoring is not available in this editor view.';

Var
  EditSvrs : IOTAEditorServices;
  TopView : IOTAEditView;
  Cursor: TOTAEditPos;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorServices, EditSvrs) Then
    If Assigned(EditSvrs.TopView) Then
      Begin
        TopView := EditSvrs.TopView;
        Cursor := TopView.CursorPos;
        TBADIRefactorConstant.Refactor(ActiveSourceEditor, Cursor.Line, Cursor.Col);
      End Else
        MessageDlg(strMsg, mtError, [mbOK], 0);
End;

(**

  This method removes any BADI actions from any of the IDE toolbars to prevent AVs.

  @precon  None.
  @postcon All BADI actions are removed from the IDE toolbars.

**)
Procedure TBADIIDEMenuInstaller.RemoveActionsFromToolbars;

  (**

    This function checks to see whether the given action is in our action list and returns true if it is.

    @precon  None.
    @postcon Checks to see whether the given action is in our action list and returns true if it is.

    @param   Action as a TBasicAction as a constant
    @return  a Boolean

  **)
  Function IsCustomAction(Const Action : TBasicAction) : Boolean;

  Var
    iBADIMenu: TBADIMenu;

  Begin
    Result := False;
    For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
      If Action = FBADIActions[iBADIMenu] Then
        Begin
          Result := True;
          Break;
        End;
  End;

  (**

    This method iterates over the buttons on a toolbar and removed the button if its action corresponds 
    to an action from this expert.

    @precon  None.
    @postcon Iterates over the buttons on a toolbar and removed the button if its action corresponds to 
             an action from this expert.

    @param   TB as a TToolbar as a constant

  **)
  Procedure RemoveAction(Const TB : TToolbar);

  Var
    i: Integer;

  Begin
    If TB <> Nil Then
      For i := TB.ButtonCount - 1 DownTo 0 Do
        Begin
          If IsCustomAction(TB.Buttons[i].Action) Then
            TB.RemoveControl(TB.Buttons[i]);
        End;
  End;

Var
  NTAS : INTAServices;

Begin
  NTAS := (BorlandIDEServices As INTAServices);
  RemoveAction(NTAS.ToolBar[sCustomToolBar]);
  RemoveAction(NTAS.Toolbar[sStandardToolBar]);
  RemoveAction(NTAS.Toolbar[sDebugToolBar]);
  RemoveAction(NTAS.Toolbar[sViewToolBar]);
  RemoveAction(NTAS.Toolbar[sDesktopToolBar]);
  {$IFDEF D0006}
  RemoveAction(NTAS.Toolbar[sInternetToolBar]);
  RemoveAction(NTAS.Toolbar[sCORBAToolBar]);
  {$IFDEF D2009}
  RemoveAction(NTAS.Toolbar[sAlignToolbar]);
  RemoveAction(NTAS.Toolbar[sBrowserToolbar]);
  RemoveAction(NTAS.Toolbar[sHTMLDesignToolbar]);
  RemoveAction(NTAS.Toolbar[sHTMLFormatToolbar]);
  RemoveAction(NTAS.Toolbar[sHTMLTableToolbar]);
  RemoveAction(NTAS.Toolbar[sPersonalityToolBar]);
  RemoveAction(NTAS.Toolbar[sPositionToolbar]);
  RemoveAction(NTAS.Toolbar[sSpacingToolbar]);
  {$ENDIF}
  {$ENDIF}
End;

(**

  This method removes the profiling code from the editor after checking that the code is what is expected
  .

  @precon  SE and ProfileJob must be valid instances.
  @postcon Removes the profiling code from the editor after checking that the code is what is expected.

  @param   SE         as an IOTASourceEditor as a constant
  @param   ProfileJob as a TProfileJob as a constant
  @param   slProlog   as a TStringList as a constant
  @param   slEpilog   as a TStringList as a constant

**)
Procedure TBADIIDEMenuInstaller.RemoveProfileCode(Const SE: IOTASourceEditor;
  Const ProfileJob: TProfileJob; Const slProlog, slEpilog: TStringList);

ResourceString
  strRemoveMsg =
    'The current profiling does not match the %s code in the method "%s". ' +
    'Expected "%s" but found "%s". Profiling code has NOT been ' +
    'removed for this method. Do you want to continue processing?';

Const
  iPadding = 2;
  strEpilog = 'Epilog';
  strProlog = 'Prolog';

Var
  Reader     : IOTAEditReader;
  C1, C2     : TOTACharPos;
  iLine      : Integer;
  iBufferPos1: Integer;
  iBufferPos2: Integer;
  strBuffer  : AnsiString;
  iRead      : Integer;
  iBufferSize: Integer;
  strLine    : String;

Begin
  Reader := SE.CreateReader;
  Try
    For iLine := 0 To slEpilog.Count - 1 Do
      Begin
        C1.Line      := ProfileJob.EndLine - iLine;
        C1.CharIndex := 0;
        iBufferPos1  := SE.GetEditView(0).CharPosToPos(C1);
        C2.Line      := ProfileJob.EndLine - iLine + 1;
        C2.CharIndex := 0;
        iBufferPos2  := SE.GetEditView(0).CharPosToPos(C2);
        iBufferSize  := iBufferPos2 - iBufferPos1;
        SetLength(strBuffer, iBufferSize);
        iRead := Reader.GetText(iBufferPos1, PAnsiChar(strBuffer), iBufferSize);
        SetLength(strBuffer, iRead);
        strLine   := slEpilog[slEpilog.Count - 1 - iLine];
        strBuffer := Copy(strBuffer, 1, Length(strBuffer) - iPadding);
        If Not Like('*' + Trim(strLine) + '*', String(strBuffer)) Then
          Case MessageDlg(Format(strRemoveMsg, [strEpilog, ProfileJob.Method,
            strBuffer, strLine]), mtError, [mbYes, mbNo], 0) Of
            mrYes: Exit;
            mrNo:  Abort;
          End;
      End;
  Finally
    Reader := Nil;
  End;
  Reader := SE.CreateReader;
  Try
    For iLine := 0 To slProlog.Count - 1 Do
      Begin
        C1.Line      := ProfileJob.StartLine + iLine;
        C1.CharIndex := 0;
        iBufferPos1  := SE.GetEditView(0).CharPosToPos(C1);
        C2.Line      := ProfileJob.StartLine + iLine + 1;
        C2.CharIndex := 0;
        iBufferPos2  := SE.GetEditView(0).CharPosToPos(C2);
        iBufferSize  := iBufferPos2 - iBufferPos1;
        SetLength(strBuffer, iBufferSize);
        iRead := Reader.GetText(iBufferPos1, PAnsiChar(strBuffer), iBufferSize);
        SetLength(strBuffer, iRead);
        strLine   := Trim(slProlog[iLine]);
        strBuffer := Copy(strBuffer, 1, Length(strBuffer) - iPadding);
        If Not Like('*' + strLine + '*', String(strBuffer)) Then
          Case MessageDlg(Format(strRemoveMsg, [strProlog, ProfileJob.Method,
            strBuffer, strLine]), mtError, [mbYes, mbNo], 0) Of
            mrYes: Exit;
            mrNo:  Abort;
          End;
      End;
  Finally
    Reader := Nil;
  End;
  DeleteProfileCode(SE, ProfileJob.EndLine - slEpilog.Count + 1, ProfileJob.EndLine);
  DeleteProfileCode(SE, ProfileJob.StartLine, ProfileJob.StartLine + slProlog.Count - 1);
End;

(**

  This method returns the selected text in th active editor window and optionally deletes this text.

  @precon  None.
  @postcon Returns the selected text in th active editor window and optionally deletes this text.

  @param   boolDelete as a Boolean as a constant
  @return  a String

**)
Function TBADIIDEMenuInstaller.SelectedText(Const boolDelete: Boolean): String;

Var
  SE                            : IOTASourceEditor;
  Reader                        : IOTAEditReader;
  strBuffer                     : AnsiString;
  iRead                         : Integer;
  Block                         : IOTAEditBlock;
  cpStart, cpEnd                : TOTACharPos;
  Writer                        : IOTAEditWriter;
  iBufferPosStart, iBufferPosEnd: Integer;
  boolVisible                   : Boolean;

Begin
  Result          := '';
  boolVisible     := False;
  iBufferPosStart := 0;
  iBufferPosEnd   := 0;
  SE              := ActiveSourceEditor;
  If SE <> Nil Then
    Begin
      Reader := SE.CreateReader;
      Try
        Block := SE.EditViews[0].Block;
        If Block.Visible Then
          Begin
            boolVisible       := True;
            cpStart.Line      := Block.StartingRow;
            cpStart.CharIndex := Block.StartingColumn - 1;
            iBufferPosStart   := SE.EditViews[0].CharPosToPos(cpStart);
            cpEnd.Line        := Block.EndingRow;
            cpEnd.CharIndex   := Block.EndingColumn - 1;
            iBufferPosEnd     := SE.EditViews[0].CharPosToPos(cpEnd);
            SetLength(strBuffer, iBufferPosEnd - iBufferPosStart);
            iRead := Reader.GetText(iBufferPosStart, PAnsiChar(strBuffer),
              iBufferPosEnd - iBufferPosStart);
            SetLength(strBuffer, iRead);
            {$IFNDEF D2009}
            Result := strBuffer;
            {$ELSE}
            Result := String(strBuffer);
            {$ENDIF}
          End;
      Finally
        Reader := Nil;
      End;
      If boolVisible And boolDelete Then
        Begin
          Writer := SE.CreateUndoableWriter;
          Try
            Writer.CopyTo(iBufferPosStart);
            Writer.DeleteTo(iBufferPosEnd);
          Finally
            Writer := Nil;
          End;
        End;
    End;
End;

(**

  This method move the active editors cursor to the supplied position and centres the cursor on th screen
  .

  @precon  None.
  @postcon When a selection is made in the explorer the cursor is placed in the editor.

  @param   iIdentLine   as an Integer as a constant
  @param   iIdentCol    as an Integer as a constant
  @param   iCommentLine as an Integer as a constant
  @param   iCommentCol  as an Integer as a constant

**)
Procedure TBADIIDEMenuInstaller.SelectionChange(Const iIdentLine, iIdentCol, iCommentLine,
  iCommentCol: Integer);

  (**

    This method unfolders the method code at the nearest position to the cursor.

    @precon  EV must be a valid instance.
    @postcon The method code at the cursor is unfolded.

    @param   EV as an IOTAEditView as a constant

  **)
  Procedure UnfoldMethod(COnst EV : IOTAEditView);

  Var
    {$IFDEF D2006}
    EA: IOTAElideActions;
    {$ENDIF}
    
  Begin
    {$IFDEF D2006}
    If Supports(EV, IOTAElideActions, EA) Then
      EA.UnElideNearestBlock;
    {$ENDIF}
  End;

Var
  SourceEditor: IOTASourceEditor;
  C           : TOTAEditPos;
  EV          : IOTAEditView;
  iLine : Integer;

Begin
  SourceEditor := ActiveSourceEditor;
  If Assigned(SourceEditor) Then
    Begin
      If SourceEditor.EditViewCount > 0 Then
        Begin
          SourceEditor.Module.CurrentEditor.Show;
          If iIdentCol * iIdentLine > 0 Then
            Begin
              SourceEditor.Show;
              EV := (BorlandIDEServices As IOTAEditorServices).TopView;
              C.Col  := iIdentCol;
              C.Line := iIdentLine;
              UnfoldMethod(EV);
              EV.CursorPos := C;
              Case TBADIOptions.BADIOptions.BrowsePosition Of
                bpCommentTop:
                  Begin
                    iLine := iIdentLine;
                    If iCommentLine > 0 Then
                      iLine := iCommentLine;
                    EV.SetTopLeft(iLine, 1);
                  End;
                bpCommentCentre:
                  Begin
                    iLine := iIdentLine;
                    If iCommentLine > 0 Then
                      iLine := iCommentLine;
                    EV.Center(iLine, 1);
                  End;
                bpIdentifierTop: EV.SetTopLeft(C.Line, 1);
                bpIdentifierCentre: EV.Center(C.Line, 1);
                bpIdentifierCentreShowAllComment:
                  Begin
                    EV.Center(C.Line, 1);
                    If iCommentLine > 0 Then
                      If iCommentLine < EV.TopRow Then
                        EV.SetTopLeft(iCommentLine, 1);
                  End;
              End;
              If C.Line >= EV.TopRow + EV.ViewSize.Height - 1 Then
                EV.SetTopLeft(C.Line - EV.ViewSize.Height + 1 + 1, 1);  
            End;
        End;
    End;
End;

(**

  This in an on click event handler for the Insert ToDo Comment menu item.

  @precon  None.
  @postcon Inserts in the the menu at the cursor a todo line comment.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.ToDoCommentClick(Sender: TObject);

Const
  strAtToDo = '@todo ';

Var
  SE             : IOTASourceEditor;
  Writer         : IOTAEditWriter;
  EditPos        : TOTAEditPos;
  CharPos        : TOTACharPos;
  strSelectedText: String;
  strComment     : String;
  CommentType    : TCommentType;
  iIndent        : Integer;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    Begin
      If IsTextSelected Then
        Case MessageDlg(strThereIsSelectedText, mtConfirmation, [mbYes, mbNo,
          mbCancel], 0) Of
          mrCancel: Exit;
          mrNo:     strSelectedText := '';
        Else
          strSelectedText := SelectedText(True);
        End;
      EditPos := SE.EditViews[0].CursorPos;
      Writer  := SE.CreateUndoableWriter;
      Try
        CharPos.Line      := EditPos.Line;
        CharPos.CharIndex := EditPos.Col;
        Writer.CopyTo(SE.GetEditView(0).CharPosToPos(CharPos) - 1);
        CommentType := TBADIDispatcher.BADIDispatcher.GetCommentType(SE.FileName, csLine);
        iIndent     := EditPos.Col;
        strComment  := BuildBlockComment(CommentType, csLine, iIndent, strAtToDo + strSelectedText);
        OutputText(Writer, strComment);
        EditPos.Col := EditPos.Col + 10;
      Finally
        Writer := Nil;
      End;
      SE.EditViews[0].CursorPos := EditPos;
    End;
End;

(**

  This method cycles through the BADI Menu Actions and updates the shortcuts to pick up any changes
  to the shortcuts in the options.

  @precon  None.
  @postcon The BADI Menu Action shortcuts are updated.

**)
Procedure TBADIIDEMenuInstaller.UpdateMenuShortcuts;

Var
  iBADIMenu: TBADIMenu;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    If Assigned(FBADIActions[iBADIMenu]) Then
      FBADIActions[iBADIMenu].ShortCut :=
        TextToShortcut(TBADIOptions.BADIOptions.MenuShortcut[iBADIMenu]);
End;

End.
