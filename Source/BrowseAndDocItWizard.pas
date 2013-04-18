(**

  This module contains the packages main wizard interface.

  @Author  David Hoyle
  @Date    18 Apr 2013
  @Version 1.0

**)
Unit BrowseAndDocItWizard;

Interface

Uses
  Classes,
  ToolsAPI,
  Menus,
  BaseLanguageModule,
  Types,
  CommonIDEFunctions,
  ModuleDispatcher
  {$IFNDEF D2005},
  ExtCtrls,
  Contnrs
  {$ENDIF},
  ProfilingForm,
  EditorNotifier;

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Type
  (** This is the class which defined the Wizard interface. **)
  TBrowseAndDocItWizard = Class(TNotifierObject, IOTANotifier, IOTAWizard)
    {$IFDEF D2005} Strict {$ENDIF} Private
    mmiPascalDocMenu: TMenuItem;
    FCounter        : Integer;
    FFileName       : String;
    FKeyBinding     : Integer;
    FINIFile        : String;
    {$IFNDEF D2005}
    FMenuTimer      : TTimer;
    FMenus          : TObjectList;
    FMenuShortCuts  : TList;
    {$ENDIF}
    FEditorNotifier : TEditorNotifier;
    Procedure InsertCommentBlock(CommentStyle: TCommentStyle;
      CommentType: TCommentType);
    Procedure OptionsClick(Sender: TObject);
    Procedure CheckForUpdatesClick(Sender: TObject);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol: Integer);
    Procedure Focus(Sender: TObject);
    Procedure OptionsChange(Sender: TObject);
    Procedure CreateMenuItem(mmiParent: TMenuItem; strCaption: String = '';
      ClickProc: TNotifyEvent = Nil; AShortCut: TShortCut = 0);
    Procedure InsertMethodCommentClick(Sender: TObject);
    Procedure InsertPropertyCommentClick(Sender: TObject);
    Procedure InsertBlockCommentClick(Sender: TObject);
    Procedure InsertLineCommentClick(Sender: TObject);
    Procedure InsertInSituCommentClick(Sender: TObject);
    Procedure InsertToDoCommentClick(Sender: TObject);
    Procedure DocumentationClick(Sender: TObject);
    Procedure DUnitClick(Sender: TObject);
    Procedure ProfilingClick(Sender: TObject);
    Procedure DeleteExistingComment(Source: IOTASourceEditor; iStartLine,
      iEndLine: Integer);
    Procedure PositionCursorInFunction(CursorDelta: TPoint; iInsertLine,
      iIndent: Integer; strComment: String);
    Procedure InsertComment(strComment: String; Writer: IOTAEditWriter;
      iInsertLine: Integer; Source: IOTASourceEditor);
    Function SelectedText(boolDelete: Boolean): String;
    Function IsTextSelected: Boolean;
    Procedure ProcessProfilingCode(SE: IOTASourceEditor; ProfileJob: TProfileJob;
      iTabs: Integer);
    Procedure InsertProfileCode(SE: IOTASourceEditor; ProfileJob: TProfileJob;
      strProlog, strEpilog: String);
    Procedure RemoveProfileCode(SE: IOTASourceEditor; ProfileJob: TProfileJob;
      slProlog, slEpilog: TStringList);
    Procedure DeleteProfileCode(SE: IOTASourceEditor; iStartLine,
      iEndLine: Integer);
    {$IFNDEF D2005}
    Procedure MenuTimerEvent(Sender: TObject);
    {$ENDIF}
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure ModuleExplorerClick(Sender: TObject);
    { IOTAWizard }
    Function GetIDString: String;
    Function GetName: String;
    Function GetState: TWizardState;
    Procedure Execute;
    { IOTAMenuWizard }
    {$HINTS OFF}
    Function GetMenuText: String;
    {$HINTS ON}
    Constructor Create(EditorNotifier : TEditorNotifier);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  SysUtils,
  CheckForUpdates,
  CheckForUpdatesForm,
  DockableModuleExplorer,
  DGHLibrary,
  ToolsAPIUtils,
  BaseDocumentation,
  DocumentationOptionsForm,
  DocumentationDispatcher,
  ShellAPI,
  Forms,
  Windows,
  DUnitCreator,
  DUnitForm,
  Dialogs,
  Controls, OptionsForm, ProgressForm;

ResourceString
  (** This is a resource message to confirm whether the selected text should be
      moved. **)
  strThereIsSelectedText = 'There is selected text in the editor. Do you wan' +
    't to move this text within the new comment';

Const
  (** This is the software ID for this module on the internet. **)
  strSoftwareID = 'BrowseAndDocIt2006';

(**

  This is an click event handler for the Check for Updates menu.

  @precon  None.
  @postcon Invokes the checking for updates.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.CheckForUpdatesClick(Sender: TObject);

Begin
  TCheckForUpdates.Execute(strSoftwareID, FINIFile, Sender <> Nil);
End;

(**

  This is the constructor method for the TPascalDocWizard class. This constructor create 
  the explorer form and menus.

  @precon  None.
  @postcon Initialises the wizard`s internal data structures and creates a menu
           interface in the IDE.

  @param   EditorNotifier as a TEditorNotifier

**)
Constructor TBrowseAndDocItWizard.Create(EditorNotifier : TEditorNotifier);

Var
  mmiMainMenu: TMainMenu;

Begin
  Inherited Create;
  FEditorNotifier := EditorNotifier;
  TfrmDockableModuleExplorer.HookEventHandlers(SelectionChange, Focus,
    OptionsChange);
  mmiMainMenu              := (BorlandIDEServices As INTAServices).MainMenu;
  mmiPascalDocMenu         := TMenuItem.Create(mmiMainMenu);
  mmiPascalDocMenu.Caption := '&Browse and Doc It';
  mmiMainMenu.Items.Insert(mmiMainMenu.Items.Count - 2, mmiPascalDocMenu);
  {$IFNDEF D2005}
  FMenus         := TObjectList.Create(False);
  FMenuShortCuts := TList.Create;
  {$ENDIF}
  CreateMenuItem(mmiPascalDocMenu, 'Module &Explorer', ModuleExplorerClick,
    Menus.ShortCut(13, [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, '&Documentation', DocumentationClick,
    Menus.ShortCut(Ord('D'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'D&Unit...', DUnitClick,
    Menus.ShortCut(Ord('U'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Pro&filing...', ProfilingClick,
    Menus.ShortCut(Ord('F'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, 'Focus Edi&tor', Focus,
    Menus.ShortCut(Ord('E'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Method Comment',
    InsertMethodCommentClick, Menus.ShortCut(Ord('M'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Property Comment',
    InsertPropertyCommentClick, Menus.ShortCut(Ord('P'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Comment Block',
    InsertBlockCommentClick, Menus.ShortCut(Ord('B'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Line Comment',
    InsertLineCommentClick, Menus.ShortCut(Ord('L'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Insert &In-Situ Comment',
    InsertInSituCommentClick, Menus.ShortCut(Ord('I'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'Insert &ToDo Comment',
    InsertToDoCommentClick, Menus.ShortCut(Ord('T'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, '&Options...', OptionsClick,
    Menus.ShortCut(Ord('O'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, 'Check for &Updates...', CheckForUpdatesClick);
  FKeyBinding := 0;
  FCounter    := 0;
  FFileName   := '';
  FINIFile    := BuildRootKey(Nil, Nil);
  CheckForUpdatesClick(Nil);
  {$IFNDEF D2005} // Code to patch shortcuts into the menus in D7 and below.
  FMenuTimer          := TTimer.Create(Nil);
  FMenuTimer.OnTimer  := MenuTimerEvent;
  FMenuTimer.Interval := 1000;
  FMenuTimer.Enabled  := True;
  {$ENDIF}
End;

(**

  This method creates menu items using the passed information.

  @precon  mmiParent must be a valid parent menu item in the IDE .
  @postcon A Sub menu ite is created under mmiParent .

  @param   mmiParent  as a TMenuItem
  @param   strCaption as a String
  @param   ClickProc  as a TNotifyEvent
  @param   AShortCut  as a TShortCut

**)
Procedure TBrowseAndDocItWizard.CreateMenuItem(mmiParent: TMenuItem;
  strCaption: String = ''; ClickProc: TNotifyEvent = Nil;
  AShortCut: TShortCut = 0);

Var
  mmiItem: TMenuItem;

Begin
  mmiItem := TMenuItem.Create(mmiParent);
  {$IFNDEF D2005}
  FMenus.Add(mmiItem); // For Delphi7 and below
  FMenuShortCuts.Add(Pointer(AShortCut));
  {$ENDIF}
  With mmiItem Do
    Begin
      If strCaption = '' Then
        Caption := '-'
      Else
        Caption        := strCaption;
      OnClick          := ClickProc;
      mmiItem.ShortCut := AShortCut;
      mmiParent.Add(mmiItem);
    End;
End;

(**

  This method deletes the comment between the start and end lines of the editor.

  @precon  None.
  @postcon Deletes the comment between the start and end lines of the editor.

  @param   Source     as an IOTASourceEditor
  @param   iStartLine as an Integer
  @param   iEndLine   as an Integer

**)
Procedure TBrowseAndDocItWizard.DeleteExistingComment(Source: IOTASourceEditor;
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

  This is the destructor method for the TBrowseAndDocItWizard class.

  @precon  None.
  @postcon Saves the wizards settings and frees memory for interval structures.

**)
Destructor TBrowseAndDocItWizard.Destroy;
Begin
  If mmiPascalDocMenu <> Nil Then
    mmiPascalDocMenu.Free;
  {$IFNDEF D2005}
  FMenuTimer.Enabled := False;
  FMenuTimer.OnTimer := Nil;
  FMenuShortCuts.Free;
  FMenus.Free;
  FMenuTimer.Free;
  {$ENDIF}
  Inherited;
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
End;

(**

  This method deletes the profile code from the editor window.

  @precon  SE must be a valid instance.
  @postcon Deletes the profile text between the given line numbers.

  @param   SE         as an IOTASourceEditor
  @param   iStartLine as an Integer
  @param   iEndLine   as an Integer

**)
Procedure TBrowseAndDocItWizard.DeleteProfileCode(SE: IOTASourceEditor;
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

  This method inserts the given comment into the editor at the given insert
  line.

  @precon  None.
  @postcon Inserts the given comment into the editor at the given insert line.

  @param   strComment  as a string
  @param   Writer      as an IOTAEditWriter
  @param   iInsertLine as an Integer
  @param   Source      as an IOTASourceEditor

**)
Procedure TBrowseAndDocItWizard.InsertComment(strComment: String;
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


  This is an on click event handler for the documentation menu.

  @precon  None.
  @postcon Invokes the documentation of the current active project.


  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.DocumentationClick(Sender: TObject);

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
Procedure TBrowseAndDocItWizard.DUnitClick(Sender: TObject);

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

  This is an exceute method for the wizard. Since this wizard is not implemented
  as a menu wizard this method has no code but is required for the interface.

  @precon  None.
  @postcon None.

**)
Procedure TBrowseAndDocItWizard.Execute;
Begin
  { Do nothing, this is not called }
End;

(**

  This is a getter method for the IDString property.

  @precon  None.
  @postcon Returns the ID string for the wizard.

  @return  a string

**)
Function TBrowseAndDocItWizard.GetIDString: String;
Begin
  Result := 'David Hoyle.Browse An Doc It';
End;

(**

  This is a getter method for the MenuText property.

  @precon  None.
  @postcon Reutns the Menu text for the wizard.

  @return  a string

**)
Function TBrowseAndDocItWizard.GetMenuText: String;
Begin
  Result := 'Browse and Doc It...';
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the wizard.

  @return  a string

**)
Function TBrowseAndDocItWizard.GetName: String;
Begin
  Result := 'David Hoyle.Browse An Doc It';
End;

(**

  This is a getter method for the State property.

  @precon  None.
  @postcon Enables the wizard.

  @return  a TWizardState

**)
Function TBrowseAndDocItWizard.GetState: TWizardState;
Begin
  Result := [wsEnabled];
End;

{$IFNDEF D2005}

(**

  This is an on timer event handler for the menu timer.

  @precon  None.
  @postcon In Delphi 7 and below - it patches the shortcuts onto the menu items
           as the Open Tools API "looses" the shortcuts.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.MenuTimerEvent(Sender: TObject);

Var
  i: Integer;
  M: TMenuItem;

Begin
  For i := 0 To FMenus.Count - 1 Do
    Begin
      M          := FMenus[i] As TMenuItem;
      M.ShortCut := Integer(FMenuShortCuts[i]);
    End;
  FMenuTimer.Enabled := False;
End;
{$ENDIF}


(**

  This is a TMenuItem on click event. If display the module explorer if its not
  visible else hide or focuses the explorer.

  @precon  Sender is the object initiating the event.
  @postcon Displays the Module Explorer.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.ModuleExplorerClick(Sender: TObject);
Begin
  TfrmDockableModuleExplorer.ShowDockableModuleExplorer;
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
Procedure TBrowseAndDocItWizard.InsertBlockCommentClick(Sender: TObject);

Var
  SE: IOTASourceEditor;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    InsertCommentBlock(csBlock, GetCommentType(SE.FileName, csBlock));
End;

(**

  This method inserts either a Block, Line or InSitu comment at the position of
  the curerent cursor depending on the passed partameter.

  @precon  None.
  @postcon Inserts the specified comment.

  @param   CommentStyle as a TCommentStyle
  @param   CommentType  as a TCommentType

**)
Procedure TBrowseAndDocItWizard.InsertCommentBlock(CommentStyle: TCommentStyle;
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
          SelectionChange(EditPos.Line + 5, EditPos.Col, EditPos.Line + 5,
            EditPos.Col);
      Else
        SelectionChange(EditPos.Line + 1, EditPos.Col, EditPos.Line + 1,
          EditPos.Col);
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

  This is an action for the Insert In Situ Comment event handler. It inserts an
  in sity comment at the current cursor position.

  @precon  None.
  @postcon Inserts an InSitu comment into the editor.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.InsertInSituCommentClick(Sender: TObject);
Var
  SE: IOTASourceEditor;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    InsertCommentBlock(csInSitu, GetCommentType(SE.FileName, csInSitu));
End;

(**

  This method is an on click event handler for the Insert Line Comment action.

  @precon  None.
  @postcon Inserts a line comment at the cursor location in the editor.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.InsertLineCommentClick(Sender: TObject);
Var
  SE: IOTASourceEditor;

Begin
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    InsertCommentBlock(csLine, GetCommentType(SE.FileName, csLine));
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
Procedure TBrowseAndDocItWizard.InsertMethodCommentClick(Sender: TObject);

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
  Module := Dispatcher(EditorAsString(Source), Source.FileName, Source.Modified,
    [moParse]);
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
                strComment := WriteComment(F, GetCommentType(Source.FileName,
                  csBlock), iIndent, True,
                  CursorDelta, iMaxCommentWidth);
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

  This method processes each of the profiling jobs given determining whether
  they are insertions or removals.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Processes each of the profiling jobs given determining whether they
           are insertions or removals.

  @param   SE         as an IOTASourceEditor
  @param   ProfileJob as a TProfileJob
  @param   iTabs      as an Integer

**)
Procedure TBrowseAndDocItWizard.ProcessProfilingCode(SE: IOTASourceEditor;
  ProfileJob: TProfileJob; iTabs: Integer);

Var
  strTemplate       : String;
  slProlog, slEpilog: TStringList;

Begin
  strTemplate := StringReplace(BrowseAndDocItOptions.ProfilingCode[SE.FileName],
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

  This method inserts profiling code into the editor.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Inserts into the editor profiling code for the given profiling job.

  @param   SE         as an IOTASourceEditor
  @param   ProfileJob as a TProfileJob
  @param   strProlog  as a String
  @param   strEpilog  as a String

**)
Procedure TBrowseAndDocItWizard.InsertProfileCode(SE: IOTASourceEditor;
  ProfileJob: TProfileJob; strProlog, strEpilog: String);

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

  This method removes the profiling code from the editor after checking that the
  code is what is expected.

  @precon  SE and ProfileJob must be valid instances.
  @postcon Removes the profiling code from the editor after checking that the
           code is what is expected.

  @param   SE         as an IOTASourceEditor
  @param   ProfileJob as a TProfileJob
  @param   slProlog   as a TStringList
  @param   slEpilog   as a TStringList

**)
Procedure TBrowseAndDocItWizard.RemoveProfileCode(SE: IOTASourceEditor;
  ProfileJob: TProfileJob; slProlog, slEpilog: TStringList);

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
Procedure TBrowseAndDocItWizard.InsertPropertyCommentClick(Sender: TObject);

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
  Module := Dispatcher(EditorAsString(Source), Source.FileName, Source.Modified,
    [moParse]);
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
                strComment       := WriteComment(F, GetCommentType(Source.FileName,
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
Procedure TBrowseAndDocItWizard.InsertToDoCommentClick(Sender: TObject);

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
        CommentType := GetCommentType(SE.FileName, csLine);
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

  This is a TMenuItem on click event. it invokes the Options dialogue.

  @precon  Sender is the object initiating the event.
  @postcon Displays the wizards Options dialogue.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.OptionsClick(Sender: TObject);

Begin
  If TfrmOptions.Execute([Low(TVisibleTab) .. High(TVisibleTab)]) Then
    FEditorNotifier.ResetLastupdateTickCount(1);
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
Function TBrowseAndDocItWizard.SelectedText(boolDelete: Boolean): String;

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

  This method test whether there is selected text in the editors current view.

  @precon  None.
  @postcon Returns true of there is selected text.

  @return  a Boolean

**)
Function TBrowseAndDocItWizard.IsTextSelected: Boolean;

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
Procedure TBrowseAndDocItWizard.SelectionChange(iIdentLine, iIdentCol,
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
              Case BrowseAndDocItOptions.BrowsePosition Of
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

(**

  This method is an event handler for the On Focus event from the explorer
  frame.

  @precon  None.
  @postcon Focuses the active editor.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.Focus(Sender: TObject);

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

  This is an event handler to be hooked the the Module Explorer Frames
  OnOptionsChange event handler.

  @precon  None.
  @postcon Refreshes the current module view.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.OptionsChange(Sender: TObject);

Begin
  FEditorNotifier.ResetLastupdateTickCount(1);
End;

(**

  This method positions the comment and function according to the options and
  then places the cursor in the appropriate position for editing.

  @precon  None.
  @postcon Positions the comment and function according to the options and
           then places the cursor in the appropriate position for editing.

  @param   CursorDelta as a TPoint
  @param   iInsertLine as an Integer
  @param   iIndent     as an Integer
  @param   strComment  as a string

**)
Procedure TBrowseAndDocItWizard.PositionCursorInFunction(CursorDelta: TPoint;
  iInsertLine: Integer; iIndent: Integer; strComment: String);

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

  This is an on click event handler for the Porfiling menu item.

  @precon  None.
  @postcon Invokes the profiling dialogue for selecting the methods to be
           profiled.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.ProfilingClick(Sender: TObject);

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
          M := Dispatcher(EditorAsString(SE), SE.FileName,
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
                            ProcessProfilingCode(SE, ProfileJobs.ProfileJob[i], iTabs);
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

End.
