(**

  This module excapsulates the creation of menus in the IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    11 Apr 2017

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
  {$IFDEF D2005} Strict {$ENDIF} Private
    FBADIMenu       : TMenuItem;
    FBADIActions    : Array[Low(TBADIMenu)..High(TBADIMenu)] Of TAction;
    FEditorNotifier : TEditorNotifier;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  CreateMenuItem(Const mmiParent: TMenuItem; Const eBADIMenu: TBADIMenu;
      Const ClickProc, UpdateProc: TNotifyEvent; iImageIndex : Integer) : TMenuItem;
    Procedure InsertCommentBlock(CommentStyle: TCommentStyle;
      CommentType: TCommentType);
    Function IsTextSelected: Boolean;
    Function SelectedText(boolDelete: Boolean): String;
    Procedure DeleteExistingComment(Source: IOTASourceEditor; iStartLine,
      iEndLine: Integer);
    Procedure InsertComment(Const strComment: String; Writer: IOTAEditWriter;
      iInsertLine: Integer; Source: IOTASourceEditor);
    Procedure PositionCursorInFunction(CursorDelta: TPoint; iInsertLine,
      iIndent: Integer; Const strComment: String);
    Procedure ProcessProfilingCode(Module : TBaseLanguageModule; SE: IOTASourceEditor;
      ProfileJob: TProfileJob; iTabs: Integer);
    Procedure InsertProfileCode(SE: IOTASourceEditor; ProfileJob: TProfileJob;
      Const strProlog, strEpilog: String);
    Procedure RemoveProfileCode(SE: IOTASourceEditor; ProfileJob: TProfileJob;
      Const slProlog, slEpilog: TStringList);
    Procedure DeleteProfileCode(SE: IOTASourceEditor; iStartLine,
      iEndLine: Integer);
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
    Procedure CreateBADIMainMenu;
    Procedure RemoveActionsFromToolbars;
    Function  AddImagesToIDE : Integer;
    Procedure NilActions;
    Procedure FreeActions;
  Public
    Constructor Create(Const EditorNotifier : TEditorNotifier);
    Destructor Destroy; Override;
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine, iCommentCol: Integer);
    Procedure Focus(Sender: TObject);
    Procedure OptionsChange(Sender: TObject);
    Procedure UpdateMenuShortcuts;
  End;

Implementation

Uses
  //CodeSiteLogging,
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
  BADI.Constants;

ResourceString
  (** This is a resource message to confirm whether the selected text should be
      moved. **)
  strThereIsSelectedText = 'There is selected text in the editor. Do you wan' +
    't to move this text within the new comment';

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

Var
  NTAS : INTAServices;
  ilImages : TImageList;
  BM : TBitMap;
  iMenu: TBADIMenu;

begin
  Result := -1;
  NTAS := (BorlandIDEServices As INTAServices);
  ilImages := TImageList.Create(Nil);
  Try
    For iMenu := Low(TBADIMenu) To High(TBADIMenu) Do
      If FindResource(hInstance, PChar(BADIMenus[iMenu].FName + 'Image'), RT_BITMAP) > 0 Then
        Begin
          BM := TBitMap.Create;
          Try
            BM.LoadFromResourceName(hInstance, BADIMenus[iMenu].FName + 'Image');
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
  CreateMenuItem(FBADIMenu, bmDocumentation, DocumentationClick, Nil, iImageIndex + 1);
  CreateMenuItem(FBADIMenu, bmDunit, DUnitClick, Nil, iImageIndex + 2);
  CreateMenuItem(FBADIMenu, bmProfiling, ProfilingClick, Nil, iImageIndex + 3);
  CreateMenuItem(FBADIMenu, bmSep1, Nil, Nil, 0);
  CreateMenuItem(FBADIMenu, bmFocusEditor, Focus, Nil, iImageIndex + 4);
  CreateMenuItem(FBADIMenu, bmMethodComment, MethodCommentClick, Nil, iImageIndex + 5);
  CreateMenuItem(FBADIMenu, bmPropertyComment, PropertyCommentClick, Nil, iImageIndex + 6);
  CreateMenuItem(FBADIMenu, bmBlockComment, BlockCommentClick, Nil, iImageIndex + 7);
  CreateMenuItem(FBADIMenu, bmLineComment, LineCommentClick, Nil, iImageIndex + 8);
  CreateMenuItem(FBADIMenu, bmInSituComment, InSituCommentClick, Nil, iImageIndex + 9);
  CreateMenuItem(FBADIMenu, bmToDoComment, ToDoCommentClick, Nil, iImageIndex + 10);
  CreateMenuItem(FBADIMenu, bmSep2, Nil, Nil, 0);
  CreateMenuItem(FBADIMenu, bmOptions, OptionsClick, Nil, iImageIndex + 11);
End;

(**

  This method creates the main BADI menu item and stores it for later disposal.

  @precon  None.
  @postcon The main BADI menu it added to the IDE.

**)
Procedure TBADIIDEMenuInstaller.CreateBADIMainMenu;

Var
  mmiMainMenu: TMainMenu;

Begin
  mmiMainMenu := (BorlandIDEServices As INTAServices).MainMenu;
  FBADIMenu := TMenuItem.Create(mmiMainMenu);
  FBADIMenu.Caption := '&Browse and Doc It';
  mmiMainMenu.Items.Insert(mmiMainMenu.Items.Count - 2, FBADIMenu);
End;

(**

  This method creates menu items using the passed information.

  @precon  mmiParent must be a valid parent menu item in the IDE .
  @postcon A Sub menu ite is created under mmiParent .

  @param   mmiParent   as a TMenuItem as a constant
  @param   EBADIMenu   as a TBADIMenu as a constant
  @param   ClickProc   as a TNotifyEvent as a constant
  @param   UpdateProc  as a TNotifyEvent as a constant
  @param   iImageIndex as an Integer
  @return  a TMenuItem

**)
Function TBADIIDEMenuInstaller.CreateMenuItem(Const mmiParent: TMenuItem;
  Const eBADIMenu : TBADIMenu; Const ClickProc, UpdateProc : TNotifyEvent;
  iImageIndex : Integer) : TMenuItem;

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
      Actn.Name := BADIMenus[eBADIMenu].FName + 'Action';
      Actn.Caption := BADIMenus[eBADIMenu].FCaption;
      Actn.OnExecute := ClickProc;
      Actn.OnUpdate := UpdateProc;
      Actn.ShortCut := TextToShortCut(TBADIOptions.BADIOptions.MenuShortcut[eBADIMenu]);
      Actn.ImageIndex := iImageIndex;
      Actn.Category := 'BADIActions';
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
  Result.Name := BADIMenus[eBADIMenu].FName + 'Menu';
  // Create Action and Menu.
  mmiParent.Add(Result);
End;

(**

  This method deletes the comment between the start and end lines of the editor.

  @precon  None.
  @postcon Deletes the comment between the start and end lines of the editor.

  @param   Source     as an IOTASourceEditor
  @param   iStartLine as an Integer
  @param   iEndLine   as an Integer

**)
Procedure TBADIIDEMenuInstaller.DeleteExistingComment(Source: IOTASourceEditor;
  iStartLine, iEndLine: Integer);

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

  @param   SE         as an IOTASourceEditor
  @param   iStartLine as an Integer
  @param   iEndLine   as an Integer

**)
Procedure TBADIIDEMenuInstaller.DeleteProfileCode(SE: IOTASourceEditor;
  iStartLine, iEndLine: Integer);

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

Var
  ADocType: TDocType;
  AProject: IOTAProject;
  i       : Integer;

Begin
  AProject := ActiveProject;
  If AProject <> Nil Then
    If TfrmDocumentationOptions.Execute(ADocType) Then
      With DocumentDispatcher(
        ExtractFilePath(AProject.FileName) + 'Documentation',
        {$IFDEF D2005}
        ExtractFileName(AProject.ProjectOptions.TargetName), ADocType)
        {$ELSE}
        ExtractFileName(AProject.FileName), ADocType)
        {$ENDIF} Do
        Try
          Add(AProject.FileName);
          For i := 0 To AProject.ModuleFileCount - 1 Do
            Add(AProject.ModuleFileEditors[i].FileName);
          For i := 0 To AProject.GetModuleCount - 1 Do
            Add(AProject.GetModule(i).FileName);
          OutputDocumentation;
          ShellExecute(Application.Handle, 'Open', PChar(MainDocument), '', '',
            SW_SHOWNORMAL);
        Finally
          Free;
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
        If frm.Components[i].ClassName = 'TEditControl' Then
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

  This method inserts the given comment into the editor at the given insert line.

  @precon  None.
  @postcon Inserts the given comment into the editor at the given insert line.

  @param   strComment  as a String as a constant
  @param   Writer      as an IOTAEditWriter
  @param   iInsertLine as an Integer
  @param   Source      as an IOTASourceEditor

**)
Procedure TBADIIDEMenuInstaller.InsertComment(Const strComment: String;
  Writer: IOTAEditWriter; iInsertLine: Integer; Source: IOTASourceEditor);

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

  This method inserts either a Block, Line or InSitu comment at the position of
  the curerent cursor depending on the passed partameter.

  @precon  None.
  @postcon Inserts the specified comment.

  @param   CommentStyle as a TCommentStyle
  @param   CommentType  as a TCommentType

**)
Procedure TBADIIDEMenuInstaller.InsertCommentBlock(CommentStyle: TCommentStyle;
  CommentType: TCommentType);

Var
  SourceEditor   : IOTASourceEditor;
  EditPos        : TOTAEditPos;
  CharPos        : TOTACharPos;
  Writer         : IOTAEditWriter;
  iIndent        : Integer;
  strSelectedText: String;

Begin
  If CommentType = ctNone Then
    Exit;
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  If IsTextSelected Then
    Case MessageDlg(strThereIsSelectedText, mtConfirmation, [mbYes, mbNo,
      mbCancel], 0) Of
      mrCancel:
        Exit;
      mrNo:
        strSelectedText := '';
    Else
      strSelectedText := SelectedText(True);
    End;
  With SourceEditor.GetEditView(0) Do
    Begin
      EditPos := CursorPos;
      iIndent := CursorPos.Col;
    End;
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
  With SourceEditor.GetEditView(0) Do
    Begin
      Case CommentStyle Of
        csBlock:
          SelectionChange(EditPos.Line + 5, EditPos.Col, EditPos.Line, EditPos.Col);
      Else
        SelectionChange(EditPos.Line + 1, EditPos.Col, EditPos.Line, EditPos.Col);
      End;
      // Place cursor at start of comment
      Case CommentStyle Of
        csBlock:
          Begin
            EditPos.Line := EditPos.Line + 2;
            EditPos.Col  := EditPos.Col + 2;
          End;
      Else
        EditPos.Col := EditPos.Col + 4;
      End;
      CursorPos := EditPos;
      Paint;
    End;
End;

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

  This method inserts profiling code into the editor.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Inserts into the editor profiling code for the given profiling job.

  @param   SE         as an IOTASourceEditor
  @param   ProfileJob as a TProfileJob
  @param   strProlog  as a String as a constant
  @param   strEpilog  as a String as a constant

**)
Procedure TBADIIDEMenuInstaller.InsertProfileCode(SE: IOTASourceEditor;
  ProfileJob: TProfileJob; Const strProlog, strEpilog: String);

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

  This in an on click event handler for the Insert ToDo Comment menu item.

  @precon  None.
  @postcon Inserts in the the menu at the cursor a todo line comment.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.ToDoCommentClick(Sender: TObject);

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
          mrCancel:
            Exit;
          mrNo:
            strSelectedText := '';
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
        strComment  := BuildBlockComment(CommentType, csLine, iIndent,
          '@todo ' + strSelectedText);
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

//{$IFNDEF D2005}
//(**
//
//  This is an on timer event handler for the menu timer.
//
//  @precon  None.
//  @postcon In Delphi 7 and below - it patches the shortcuts onto the menu items
//           as the Open Tools API "looses" the shortcuts.
//
//  @param   Sender as a TObject
//
//**)
//Procedure TBADIIDEMenuInstaller.MenuTimerEvent(Sender: TObject);
//
//Var
//  i: Integer;
//  M: TMenuItem;
//
//Begin
//  For i := 0 To FMenus.Count - 1 Do
//    Begin
//      M          := FMenus[i] As TMenuItem;
//      M.ShortCut := Integer(FMenuShortCuts[i]);
//    End;
//  FMenuTimer.Enabled := False;
//End;
//{$ENDIF}

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

  This is a TMenuItem on click event. it invokes the Options dialogue.

  @precon  Sender is the object initiating the event.
  @postcon Displays the wizards Options dialogue.

  @param   Sender as a TObject

**)
Procedure TBADIIDEMenuInstaller.OptionsClick(Sender: TObject);

Begin
  {$IFDEF DXE00}
  (BorlandIDEServices As IOTAServices).GetEnvironmentOptions.EditOptions('', 'Browse and Doc It');
  {$ELSE}
  If TfrmOptions.Execute([Low(TVisibleTab)..High(TVisibleTab)]) Then
    Begin
      FEditorNotifier.ResetLastupdateTickCount(1);
      UpdateMenuShortcuts;
    End;
  {$ENDIF}
End;

(**

  This method positions the comment and function according to the options and then places the cursor
  in the appropriate position for editing.

  @precon  None.
  @postcon Positions the comment and function according to the options and then places the cursor
           in the appropriate position for editing.

  @param   CursorDelta as a TPoint
  @param   iInsertLine as an Integer
  @param   iIndent     as an Integer
  @param   strComment  as a String as a constant

**)
Procedure TBADIIDEMenuInstaller.PositionCursorInFunction(CursorDelta: TPoint;
  iInsertLine: Integer; iIndent: Integer; Const strComment: String);

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
  S      := ActiveSourceEditor;
  If S <> Nil Then
    S.GetEditView(0).CursorPos := C;
End;

(**

  This method processes each of the profiling jobs given determining whether they are
  insertions or removals.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Processes each of the profiling jobs given determining whether they are
           insertions or removals.

  @param   Module     as a TBaseLanguageModule
  @param   SE         as an IOTASourceEditor
  @param   ProfileJob as a TProfileJob
  @param   iTabs      as an Integer

**)
Procedure TBADIIDEMenuInstaller.ProcessProfilingCode(Module : TBaseLanguageModule;
  SE: IOTASourceEditor; ProfileJob: TProfileJob; iTabs: Integer);

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
                        frm.Init(ProfileJobs.Count - 1, 'Profiling',
                          'Starting to processing profiling information...');
                        For i := 0 To ProfileJobs.Count - 1 Do
                          Begin
                            ProcessProfilingCode(M, SE, ProfileJobs.ProfileJob[i], iTabs);
                            frm.UpdateProgress(i, 'Processing method "' +
                              ProfileJobs.ProfileJob[i].Method + '"...');
                          End;
                      Finally
                        frm.Free;
                      End;
                    End
                  Else
                    MessageDlg('Nothing to do!', mtError, [mbOK], 0);
                End;
            Finally
              ProfileJobs.Free;
            End;
          Finally
            M.Free;
          End;
        End
      Else
        MessageDlg('The editor buffer is read only.', mtError, [mbOK], 0);
    End
  Else
    MessageDlg(strSelectSourceCode, mtError, [mbOK], 0);
End;

(**

  This method removes any BADI actions from any of the IDE toolbars to prevent AVs.

  @precon  None.
  @postcon All BADI actions are removed from the IDE toolbars.

**)
Procedure TBADIIDEMenuInstaller.RemoveActionsFromToolbars;

  (**

    This function checks to see whether the given action is in our action list
    and returns true if it is.

    @precon  None.
    @postcon Checks to see whether the given action is in our action list
             and returns true if it is.

    @param   Action as a TBasicAction
    @return  a Boolean

  **)
  Function IsCustomAction(Action : TBasicAction) : Boolean;

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

    This method iterates over the buttons on a toolbar and removed the button if
    its action corresponds to an action from this expert.

    @precon  None.
    @postcon Iterates over the buttons on a toolbar and removed the button if
             its action corresponds to an action from this expert.

    @param   TB as a TToolbar

  **)
  Procedure RemoveAction(TB : TToolbar);

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

  This method removes the profiling code from the editor after checking that the code is what is
  expected.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Removes the profiling code from the editor after checking that the code is what is
           expected.

  @param   SE         as an IOTASourceEditor
  @param   ProfileJob as a TProfileJob
  @param   slProlog   as a TStringList as a constant
  @param   slEpilog   as a TStringList as a constant

**)
Procedure TBADIIDEMenuInstaller.RemoveProfileCode(SE: IOTASourceEditor;
  ProfileJob: TProfileJob; Const slProlog, slEpilog: TStringList);

ResourceString
  strRemoveMsg =
    'The current profiling does not match the %s code in the method "%s". ' +
    'Expected "%s" but found "%s". Profiling code has NOT been ' +
    'removed for this method. Do you want to continue processing?';

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
        strBuffer := Copy(strBuffer, 1, Length(strBuffer) - 2);
        If Not Like('*' + Trim(strLine) + '*', String(strBuffer)) Then
          Case MessageDlg(Format(strRemoveMsg, ['Epilog', ProfileJob.Method,
            strBuffer, strLine]), mtError, [mbYes, mbNo], 0) Of
            mrYes:
              Exit;
            mrNo:
              Abort;
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
        strBuffer := Copy(strBuffer, 1, Length(strBuffer) - 2);
        If Not Like('*' + strLine + '*', String(strBuffer)) Then
          Case MessageDlg(Format(strRemoveMsg, ['Prolog', ProfileJob.Method,
            strBuffer, strLine]), mtError, [mbYes, mbNo], 0) Of
            mrYes:
              Exit;
            mrNo:
              Abort;
          End;
      End;
  Finally
    Reader := Nil;
  End;
  DeleteProfileCode(SE, ProfileJob.EndLine - slEpilog.Count + 1, ProfileJob.EndLine);
  DeleteProfileCode(SE, ProfileJob.StartLine, ProfileJob.StartLine + slProlog.Count - 1);
End;

(**

  This method returns the selected text in th active editor window and
  optionally deletes this text.

  @precon  None.
  @postcon Returns the selected text in th active editor window and
           optionally deletes this text.

  @param   boolDelete as a Boolean
  @return  a String

**)
Function TBADIIDEMenuInstaller.SelectedText(boolDelete: Boolean): String;

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

  This method move the active editors cursor to the supplied position and
  centres the cursor on th screen.

  @precon  None.
  @postcon When a selection is made in the explorer the cursor is placed in
           the editor.

  @param   iIdentLine as an Integer
  @param   iIdentCol  as an Integer
  @param   iCommentLine as an Integer
  @param   iCommentCol  as an Integer

**)
Procedure TBADIIDEMenuInstaller.SelectionChange(iIdentLine, iIdentCol,
  iCommentLine, iCommentCol: Integer);

Var
  SourceEditor: IOTASourceEditor;
  C           : TOTAEditPos;
  EV          : IOTAEditView;
  {$IFDEF D2006}
  EA: IOTAElideActions;
  {$ENDIF}

Begin
  SourceEditor := ActiveSourceEditor;
  If SourceEditor <> Nil Then
    Begin
      If SourceEditor.EditViewCount > 0 Then
        Begin
          SourceEditor.Module.CurrentEditor.Show;
          If iIdentCol * iIdentLine > 0 Then
            Begin
              SourceEditor.Show;
              EV := (BorlandIDEServices As IOTAEditorServices).TopView;
              {$IFDEF D2006}
              EV.QueryInterface(IOTAElideActions, EA);
              {$ENDIF}
              C.Col  := iIdentCol;
              C.Line := iIdentLine;
              {$IFDEF D2006}
              If EA <> Nil Then
                EA.UnElideNearestBlock;
              {$ENDIF}
              EV.CursorPos := C;
              Case TBADIOptions.BADIOptions.BrowsePosition Of
                bpCommentTop:
                  Begin
                    If iCommentLine > 0 Then
                      iIdentLine := iCommentLine;
                    EV.SetTopLeft(iIdentLine, 1);
                  End;
                bpCommentCentre:
                  Begin
                    If iCommentLine > 0 Then
                      iIdentLine := iCommentLine;
                    EV.Center(iIdentLine, 1);
                  End;
                bpIdentifierTop:
                  EV.SetTopLeft(iIdentLine, 1);
                bpIdentifierCentre:
                  EV.Center(iIdentLine, 1);
                bpIdentifierCentreShowAllComment:
                  Begin
                    EV.Center(iIdentLine, 1);
                    If iCommentLine > 0 Then
                      If iCommentLine < EV.TopRow Then
                        EV.SetTopLeft(iCommentLine, 1);
                  End;
              End;
            End;
        End;
    End;
End;

End.
