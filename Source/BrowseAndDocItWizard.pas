(**

  This module contains the packages main wizard interface.

  @Author  David Hoyle
  @Date    17 Jul 2009
  @Version 1.0

**)
Unit BrowseAndDocItWizard;

Interface

Uses
  Classes, ToolsAPI, Menus, ExtCtrls, BaseLanguageModule, DockForm, Types,
  Contnrs;

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Type
  (** This emunerate descibed the different types of doc comment .**)
  TCommentType = (ctBlock, ctLine, ctInSitu);

  (** This is the class which defined the Wizard interface. **)
  TBrowseAndDocItWizard = Class(TNotifierObject, IOTAWizard)
  {$IFDEF D2005} Strict {$ENDIF} Private
    mmiPascalDocMenu : TMenuItem;
    FCounter : Integer;
    FFileName : String;
    FKeyBinding : Integer;
    FINIFile: String;
    {$IFNDEF D2005}
    FMenuTimer : TTimer;
    FMenus : TObjectList;
    FMenuShortCuts : TList;
    {$ENDIF}
    procedure InsertCommentBlock(CommentType: TCommentType);
    procedure OptionsClick(Sender: TObject);
    procedure CheckForUpdatesClick(Sender: TObject);
    procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer);
    Procedure Focus(Sender : TObject);
    Procedure OptionsChange(Sender : TObject);
    procedure CreateMenuItem(mmiParent: TMenuItem;  strCaption: String = '';
      ClickProc: TNotifyEvent = Nil; AShortCut : TShortCut = 0);
    Procedure InsertMethodCommentClick(Sender : TObject);
    Procedure InsertPropertyCommentClick(Sender : TObject);
    Procedure InsertBlockCommentClick(Sender : TObject);
    Procedure InsertLineCommentClick(Sender : TObject);
    Procedure InsertInSituCommentClick(Sender : TObject);
    Procedure DocumentationClick(Sender : TObject);
    Procedure DUnitClick(Sender : TObject);
    Procedure DeleteExistingComment(Source : IOTASourceEditor; iStartLine,
      iEndLine : Integer);
    procedure PositionCursorInFunction(CursorDelta: TPoint; iInsertLine,
      iIndent: Integer; strComment: string);
    procedure InsertComment(strComment: string; Writer: IOTAEditWriter;
      iInsertLine: Integer; Source: IOTASourceEditor);
    {$IFNDEF D2005}
    Procedure MenuTimerEvent(Sender : TObject);
    {$ENDIF}
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure ModuleExplorerClick(Sender : TObject);
    { IOTAWizard }
    Function GetIDString: string;
    Function GetName: string;
    Function GetState: TWizardState;
    Procedure Execute;
    { IOTAMenuWizard }
    {$HINTS OFF}
    Function GetMenuText: string;
    {$HINTS ON}
    Constructor Create;
    Destructor Destroy; Override;
  End;

  (** This class handles notifications from the editor so that changes in the
      editor can be displayed. **)
  TEditorNotifier = Class(TNotifierObject {$IFDEF D2005},
    INTAEditServicesNotifier {$ENDIF} )
  {$IFDEF D2005} Strict {$ENDIF} Private
    FUpdateTimer : TTimer;
    {$IFNDEF D2005}
    FLastSize : Int64;
    {$ENDIF}
    FLastEditorName : String;
    FLastCursorPos: TOTAEditPos;
    FLastParserResult : Boolean;
    Procedure EnableTimer(boolSuccessfulParse : Boolean);
    Procedure TimerEventHandler(Sender : TObject);
    Function EditorInfo(var strFileName : String;
      var boolModified : Boolean) : String;
    Procedure RenderDocument(Module : TBaseLanguageModule);
    Procedure ExceptionMsg(strExceptionMsg : String);
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    {$IFDEF D2005}
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    {$ENDIF}
    Constructor Create;
    Destructor Destroy; Override;
  End;

  (** This class represents a key binding notifier for installing and handling
      key bindings for this plugin. **)
  TKeyboardBinding = Class(TNotifierObject, IOTAKeyboardBinding)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FWizard : TBrowseAndDocItWizard;
    Procedure FocusModuleExplorer(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure ShowTokens(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    Constructor Create(Wizard : TBrowseAndDocItWizard);
  End;

  Procedure Register;
  Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
    RegisterProc : TWizardRegisterProc;
    var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Exports
  InitWizard Name WizardEntryPoint;

Implementation

Uses
  SysUtils, DockableModuleExplorer, IniFiles, ToolsAPIUtils, OptionsForm, Forms,
  Windows, ShellAPI, TokenForm, DGHLibrary, ModuleDispatcher, Dialogs, Controls,
  PsAPI, DocumentationOptionsForm, DocumentationDispatcher, BaseDocumentation,
  CheckForUpdates, CheckForUpdatesForm {$IFDEF EUREKALOG}, ExceptionLog {$ENDIF},
  DUnitForm, DUnitCreator, CommonIDEFunctions;

{$IFDEF D2005}
Resourcestring
  (** This is a text string of revision from nil and a to z. **)
  strRevision = ' abcdefghijklmnopqrstuvwxyz';
  {$IFDEF VER170}
  (** This is a message string to appear in the BDS 2005 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Borland Developer Studio 2005';
  {$ENDIF}
  {$IFDEF VER180}
  (** This is a message string to appear in the BDS 2006 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Borland Developer Studio 2006';
  {$ENDIF}
  {$IFDEF VER190}
  (** This is a message string to appear in the CDS 2007 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for CodeGear RAD Studio 2007';
  {$ENDIF}
  {$IFDEF VER200}
  (** This is a message string to appear in the CRS 2009 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for CodeGear RAD Studio 2009';
  {$ENDIF}
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'Freeware by David Hoyle (Build %d.%d.%d.%d)';
{$ENDIF}

Const
  (** This is the software ID for this module on the internet. **)
  strSoftwareID = 'BrowseAndDocIt2006';

Var
  (** This is an index for the wizard when register with the ide. Its required
      in order to remove it from memory. **)
  iWizardIndex : Integer;
  {$IFDEF D2005}
  (** This is an index for the editor notifier required when the package is
      unloaded **)
  iEditorIndex : Integer;
  {$ELSE}
  (** This is a private reference for the Editor Notifier class when not
      registered with the IDE. **)
  objEditorNotifier : TEditorNotifier;
  {$ENDIF}
  (** A private variable to hold the time of the last editor update. **)
  iLastUpdateTickCount : Cardinal;
  (** An index for the keybinding nofitier - required when the package is
      unloaded **)
  iKeyBinding: Integer;
  {boolCompiling : Boolean;}

(**

  This is the modules registry procedure so that the Delphi IDE can registry
  the wizard.

  @precon  None.
  @postcon Creates the following wizards and notifiers:
           1) Browse And Doc It Wizard
           2) Editor Notifier
           3) Keyboard Binding Notifier

**)
Procedure Register();

Var
  Wizard : TBrowseAndDocItWizard;

Begin
  Application.Handle := Application.MainForm.Handle;
  TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
  Wizard := TBrowseAndDocItWizard.Create;
  iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(Wizard);
  {iIDENotifier := (BorlandIDEServices As IOTAServices).AddNotifier(Wizard);}
  {$IFDEF D2005}
  iEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(
    TEditorNotifier.Create);
  {$ELSE}
  objEditorNotifier := TEditorNotifier.Create;
  {$ENDIF}
  iKeyBinding := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
    TKeyboardBinding.Create(Wizard))
End;

(**

  This is an click event handler for the Check for Updates menu.

  @precon  None.
  @postcon Invokes the checking for updates.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.CheckForUpdatesClick(Sender: TObject);
begin
  TCheckForUpdates.Execute(strSoftwareID, FINIFile, Sender <> Nil);
end;

(**

  This is the constructor method for the TPascalDocWizard class. This
  constructor create the explorer form and menus.

  @precon  None.
  @postcon Initialises the wizard's internal data structures and creates a menu
           interface in the IDE.

**)
Constructor TBrowseAndDocItWizard.Create;

Var
  mmiMainMenu : TMainMenu;

Begin
  Inherited Create;
  TfrmDockableModuleExplorer.HookEventHandlers(SelectionChange, Focus,
    OptionsChange);
  mmiMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  mmiPascalDocMenu := TMenuItem.Create(mmiMainMenu);
  mmiPascalDocMenu.Caption := '&Browse and Doc It';
  mmiMainMenu.Items.Insert(mmiMainMenu.Items.Count - 2, mmiPascalDocMenu);
  {$IFNDEF D2005}
  FMenus := TObjectList.Create(False);
  FMenuShortCuts := TList.Create;
  {$ENDIF}
  CreateMenuItem(mmiPascalDocMenu, 'Module &Explorer', ModuleExplorerClick,
    Menus.ShortCut(13, [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, '&Documentation', DocumentationClick,
    Menus.ShortCut(Ord('D'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu, 'D&Unit...', DUnitClick,
    Menus.ShortCut(Ord('U'), [ssCtrl, ssShift, ssAlt]));
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
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, '&Options...', OptionsClick,
    Menus.ShortCut(Ord('O'), [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, 'Check for &Updates...', CheckForUpdatesClick);
  FKeyBinding := 0;
  FCounter := 0;
  FFileName := '';
  FINIFile := BuildRootKey(Nil, Nil);
  CheckForUpdatesClick(Nil);
  {$IFNDEF D2005} // Code to patch shortcuts into the menus in D7 and below.
  FMenuTimer := TTimer.Create(Nil);
  FMenuTimer.OnTimer := MenuTimerEvent;
  FMenuTimer.Interval := 1000;
  FMenuTimer.Enabled := True;
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
Procedure TBrowseAndDocItWizard.CreateMenuItem(mmiParent : TMenuItem;
  strCaption : String = ''; ClickProc : TNotifyEvent = Nil;
  AShortCut : TShortCut = 0);

Var
  mmiItem : TMenuItem;

Begin
  mmiItem := TMenuItem.Create(mmiParent);
  {$IFNDEF D2005}
  FMenus.Add(mmiItem); // For Delphi7 and below
  FMenuShortCuts.Add(Pointer(AShortCut));
  {$ENDIF}
  With mmiItem Do
    Begin
      If strCaption = '' Then
        Caption  := '-'
      Else
        Caption := strCaption;
      OnClick := ClickProc;
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
procedure TBrowseAndDocItWizard.DeleteExistingComment(Source : IOTASourceEditor;
  iStartLine, iEndLine: Integer);

Var
  Writer : IOTAEditWriter;
  ptStart, ptEnd : TOTACharPos;
  iBufferStart, iBufferEnd : Integer;

begin
  Writer := Source.CreateUndoableWriter;
    Try
      ptStart.Line := iStartLine;
      ptStart.CharIndex := 0;
      iBufferStart := Source.GetEditView(0).CharPosToPos(ptStart);
      Writer.CopyTo(iBufferStart);
      ptEnd.Line := iEndLine;
      ptEnd.CharIndex := 0;
      iBufferEnd := Source.GetEditView(0).CharPosToPos(ptEnd);
      Writer.DeleteTo(iBufferEnd);
  Finally
    Writer := Nil;
  End;
end;

(**

  This is the destructor method for the TBrowseAndDocItWizard class.

  @precon  None.
  @postcon Saves the wizards settings and frees memory for interval structures.

**)
destructor TBrowseAndDocItWizard.Destroy;
begin
  If mmiPascalDocMenu <> Nil Then
    mmiPascalDocMenu.Free;
  {$IFNDEF D2005}
  FMenuShortCuts.Free;
  FMenus.Free;
  FMenuTimer.Free;
  {$ENDIF}
  Inherited;
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
end;

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
procedure TBrowseAndDocItWizard.InsertComment(strComment: string;
  Writer: IOTAEditWriter; iInsertLine: Integer; Source: IOTASourceEditor);

var
  iBufferPos: Integer;
  C: TOTACharPos;

begin
  C.Line := iInsertLine;
  C.CharIndex := 0;
  iBufferPos := Source.GetEditView(0).CharPosToPos(C);
  Writer.CopyTo(iBufferPos);
  OutputText(Writer, strComment);
end;

(**


  This is an on click event handler for the documentation menu.

  @precon  None.
  @postcon Invokes the documentation of the current active project.


  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.DocumentationClick(Sender: TObject);

var
  ADocType: TDocType;
  AProject: IOTAProject;
  i: Integer;

begin
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
end;

(**

  This method creates an instance of a DUnit form and passes a class instance
  that can create projects and units for the form.

  @precon  None.
  @postcon Creates an instance of a DUnit form and passes a class instance
           that can create projects and units for the form.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.DUnitClick(Sender: TObject);

ResourceString
  strSelectSourceCode = 'You must select a source code editor to create unit tests.';
  strNoSelectedProject = 'There is no active project in the project group.';

Var
  D: TDUnitCreator;

begin
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
        End Else
          MessageDlg(strNoSelectedProject, mtError, [mbOK], 0);
    End Else
      MessageDlg(strSelectSourceCode, mtError, [mbOK], 0);
End;

(**

  This is an exceute method for the wizard. Since this wizard is not implemented
  as a menu wizard this method has no code but is required for the interface.

  @precon  None.
  @postcon None.

**)
procedure TBrowseAndDocItWizard.Execute;
begin
  { Do nothing, this is not called }
end;

(**

  This is a getter method for the IDString property.

  @precon  None.
  @postcon Returns the ID string for the wizard.

  @return  a string

**)
function TBrowseAndDocItWizard.GetIDString: string;
begin
  Result := 'David Hoyle.Browse An Doc It';
end;

(**

  This is a getter method for the MenuText property.

  @precon  None.
  @postcon Reutns the Menu text for the wizard.

  @return  a string

**)
function TBrowseAndDocItWizard.GetMenuText: string;
begin
  Result := 'Browse and Doc It...';
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the wizard.

  @return  a string

**)
function TBrowseAndDocItWizard.GetName: string;
begin
  Result := 'David Hoyle.Browse An Doc It';
end;

(**

  This is a getter method for the State property.

  @precon  None.
  @postcon Enables the wizard.

  @return  a TWizardState

**)
function TBrowseAndDocItWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

{$IFNDEF D2005}
(**

  This is an on timer event handler for the menu timer.

  @precon  None.
  @postcon In Delphi 7 and below - it patches the shortcuts onto the menu items
           as the Open Tools API "looses" the shortcuts.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.MenuTimerEvent(Sender: TObject);

Var
  i : Integer;
  M : TMenuItem;

begin
  For i := 0 To FMenus.Count - 1 Do
    Begin
      M := FMenus[i] As TMenuItem;
      M.ShortCut := Integer(FMenuShortCuts[i]);
    End;
  FMenuTimer.Enabled:= False;
end;
{$ENDIF}

(**

  This is a TMenuItem on click event. If display the module explorer if its not
  visible else hide or focuses the explorer.

  @precon  Sender is the object initiating the event.
  @postcon Displays the Module Explorer.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.ModuleExplorerClick(Sender: TObject);
begin
  TfrmDockableModuleExplorer.ShowDockableModuleExplorer;
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
procedure TBrowseAndDocItWizard.InsertBlockCommentClick(Sender: TObject);
begin
  InsertCommentBlock(ctBlock);
end;

(**

  This method inserts either a Block, Line or InSitu comment at the position of
  the curerent cursor depending on the passed partameter.

  @precon  None.
  @postcon Inserts the specified comment.

  @param   CommentType as a TCommentType

**)
procedure TBrowseAndDocItWizard.InsertCommentBlock(CommentType : TCommentType);

Var
  SourceEditor : IOTASourceEditor;
  EditPos : TOTAEditPos;
  CharPos : TOTACharPos;
  Writer : IOTAEditWriter;
  iLen : Integer;

begin
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  With SourceEditor.GetEditView(0) Do
    Begin
      EditPos := CursorPos;
      iLen := CursorPos.Col;
    End;
  Writer := SourceEditor.CreateUndoableWriter;
  Try
    CharPos.Line := EditPos.Line;
    CharPos.CharIndex := EditPos.Col;
    Writer.CopyTo(SourceEditor.GetEditView(0).CharPosToPos(CharPos) - 1);
    Case CommentType Of
      ctBlock:
        Begin
          OutputText(Writer, '(**');
          OutputText(Writer, #13#10);
          OutputText(Writer, StringOfChar(#32, iLen - 1) + '  '#13#10);
          OutputText(Writer, StringOfChar(#32, iLen - 1) + '  '#13#10);
          OutputText(Writer, StringOfChar(#32, iLen - 1) + '  '#13#10);
          OutputText(Writer, StringOfChar(#32, iLen - 1));
          OutputText(Writer, '**)'#13#10);
          OutputText(Writer, StringOfChar(#32, iLen - 1));
        End;
      ctLine :
        Begin
          OutputText(Writer, '(**');
          OutputText(Writer, #32#32);
          OutputText(Writer, '**)'#13#10);
          OutputText(Writer, StringOfChar(#32, iLen - 1));
        End;
      ctInSitu :
        Begin
          OutputText(Writer, '(**  **) ');
        End;
    End;
  Finally
    Writer := Nil;
  End;
  // Get header in view if not already
  With SourceEditor.GetEditView(0) Do
    Begin
      Case CommentType Of
        ctBlock:
          SelectionChange(EditPos.Line + 5, EditPos.Col, EditPos.Line + 5,
            EditPos.Col);
      Else
        SelectionChange(EditPos.Line + 1, EditPos.Col, EditPos.Line + 1,
          EditPos.Col);
      End;
      // Place cursor at start of comment
      Case CommentType Of
        ctBlock:
          Begin
            EditPos.Line := EditPos.Line + 2;
            EditPos.Col := EditPos.Col + 2;
          End;
      Else
        EditPos.Col := EditPos.Col + 4;
      End;
      CursorPos := EditPos;
      Paint;
    End;
end;

(**

  This is an action for the Insert In Situ Comment event handler. It inserts an
  in sity comment at the current cursor position.

  @precon  None.
  @postcon Inserts an InSitu comment into the editor.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.InsertInSituCommentClick(Sender: TObject);
begin
  InsertCommentBlock(ctInSitu);
end;

(**

  This method is an on click event handler for the Insert Line Comment action.

  @precon  None.
  @postcon Inserts a line comment at the cursor location in the editor.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.InsertLineCommentClick(Sender: TObject);
begin
  InsertCommentBlock(ctLine);
end;

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
procedure TBrowseAndDocItWizard.InsertMethodCommentClick(Sender: TObject);

var
  Module : TBaseLanguageModule;
  EditPos: TOTAEditPos;
  T : TElementContainer;
  F : TGenericFunction;
  Writer: IOTAEditWriter;
  Source: IOTASourceEditor;
  CursorDelta: TPoint;
  iIndent: Integer;
  strComment: String;
  iInsertLine: Integer;

begin
  Source := ActiveSourceEditor;
  If Source = Nil Then
    Exit;
  Module := Dispatcher(EditorAsString(Source), Source.FileName, Source.Modified,
    [moParse]);
  If Module <> Nil Then
    Try
      EditPos :=  Source.GetEditView(0).CursorPos;
      T := Module.FindElement(strImplementedMethodsLabel);
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
                End Else
                  iInsertLine := F.Line;
              Writer := Source.CreateUndoableWriter;
              Try
                strComment := WriteComment(F, ctPascalBlock, iIndent, True,
                  CursorDelta);
                InsertComment(strComment, Writer, iInsertLine, Source);
              Finally
                Writer := Nil;
              End;
              PositionCursorInFunction(CursorDelta, iInsertLine, iIndent, strComment);
            End Else
              MessageDlg(strNoMethodFound, mtWarning, [mbOK], 0);
        End;
    Finally
      Module.Free;
    End;
end;

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
procedure TBrowseAndDocItWizard.InsertPropertyCommentClick(Sender: TObject);

var
  Module : TBaseLanguageModule;
  EditPos: TOTAEditPos;
  Source : IOTASourceEditor;
  T : TElementContainer;
  F : TGenericFunction;
  Writer: IOTAEditWriter;
  iInsertLine: Integer;
  iIndent: Integer;
  strComment: String;
  CursorDelta: TPoint;

begin
  Source := ActiveSourceEditor;
  If Source = Nil Then
    Exit;
  Module := Dispatcher(EditorAsString(Source), Source.FileName, Source.Modified,
    [moParse]);
  If Module <> Nil Then
    Try
      EditPos :=  Source.GetEditView(0).CursorPos;
      T := Module.FindElement(strTypesLabel);
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
                End Else
                  iInsertLine := F.Line;
              Writer := Source.CreateUndoableWriter;
              Try
                strComment := WriteComment(F, ctPascalBlock, iIndent, False,
                  CursorDelta);
                InsertComment(strComment, Writer, iInsertLine, Source);
              Finally
                Writer := Nil;
              End;
              PositionCursorInFunction(CursorDelta, iInsertLine, iIndent, strComment);
            End Else
              MessageDlg(strNoPropertyFound, mtWarning, [mbOK], 0);
        End;
    Finally
      Module.Free;
    End;
end;

(**

  This is a TMenuItem on click event. it invokes the Options dialogue.

  @precon  Sender is the object initiating the event.
  @postcon Displays the wizards Options dialogue.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.OptionsClick(Sender: TObject);

begin
  If TfrmOptions.Execute Then
    iLastUpdateTickCount := 1;
end;

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
procedure TBrowseAndDocItWizard.SelectionChange(iIdentLine, iIdentCol,
  iCommentLine, iCommentCol : Integer);

Var
  SourceEditor : IOTASourceEditor;
  C : TOTAEditPos;

begin
  {: @note although you can get the regions by Query an IOTAModule for
           IOTAModuleRegions, you can not do anything with them!}
  SourceEditor := ActiveSourceEditor;
  If SourceEditor <> Nil Then
    Begin
      If SourceEditor.EditViewCount > 0 Then
        Begin
          SourceEditor.Module.CurrentEditor.Show;
          If iIdentCol * iIdentLine > 0 Then
            Begin
              SourceEditor.Show;
              {$IFDEF D2005}
              If SourceEditor.GetSubViewCount > 0 Then
                SourceEditor.SwitchToView(0);
              {$ENDIF}
              C.Col := iIdentCol;
              C.Line := iIdentLine;
              SourceEditor.GetEditView(0).CursorPos := C;
              Case BrowseAndDocItOptions.BrowsePosition Of
                bpCommentTop:
                  Begin
                    If iCommentLine > 0 Then
                      iIdentLine := iCommentLine;
                    SourceEditor.GetEditView(0).SetTopLeft(iIdentLine, 1);
                  End;
                bpCommentCentre:
                  Begin
                    If iCommentLine > 0 Then
                      iIdentLine := iCommentLine;
                    SourceEditor.GetEditView(0).Center(iIdentLine, 1);
                  End;
                bpIdentifierTop:
                  SourceEditor.GetEditView(0).SetTopLeft(iIdentLine, 1);
                bpIdentifierCentre:
                  SourceEditor.GetEditView(0).Center(iIdentLine, 1);
                bpIdentifierCentreShowAllComment:
                  Begin
                    SourceEditor.GetEditView(0).Center(iIdentLine, 1);
                    If iCommentLine > 0 Then
                      If iCommentLine < SourceEditor.EditViews[0].TopRow Then
                        SourceEditor.GetEditView(0).SetTopLeft(iCommentLine, 1);
                  End;
              End;
            End;
        End;
      End;
end;

(**

  This method is an event handler for the On Focus event from the explorer
  frame.

  @precon  None.
  @postcon Focuses the active editor.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.Focus(Sender : TObject);

Var
  i : Integer;
  frm: TCustomForm;

Begin
  If ActiveSourceEditor <> Nil Then
    Begin
      ActiveSourceEditor.Show;
      // IDE hack to focus the editor window because the above line doesn't do it
      frm := ActiveSourceEditor.EditViews[0].GetEditWindow.Form;
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
Procedure TBrowseAndDocItWizard.OptionsChange(Sender : TObject);

Begin
  iLastUpdateTickCount := 1;
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
procedure TBrowseAndDocItWizard.PositionCursorInFunction(CursorDelta: TPoint;
  iInsertLine: Integer; iIndent: Integer; strComment: string);

Var
  Pt: TPoint;
  S: IOTASourceEditor;
  C: TOTAEditPos;

begin
  SelectionChange(iInsertLine + CharCount(#13, strComment), 1, iInsertLine, 1);
  Pt.Y := iInsertLine;
  Pt.X := 1;
  Inc(Pt.Y, CursorDelta.Y);
  Inc(Pt.X, CursorDelta.X);
  C.Col := Pt.X;
  C.Line := Pt.Y;
  S := ActiveSourceEditor;
  If S <> Nil Then
    S.GetEditView(0).CursorPos := C;
end;

(**

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Var
  Svcs : IOTAServices;
  Wizard : TBrowseAndDocItWizard;


Begin
  Application.Handle := Application.MainForm.Handle;
  TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
  Result := BorlandIDEServices <> Nil;
  If Result Then
    Begin
      Svcs := BorlandIDEServices As IOTAServices;
      ToolsAPI.BorlandIDEServices := BorlandIDEServices;
      Application.Handle := Svcs.GetParentHandle;
      Wizard := TBrowseAndDocItWizard.Create;
      RegisterProc(Wizard);
      {iIDENotifier := (BorlandIDEServices As IOTAServices).AddNotifier(Wizard);}
      {$IFDEF D2005}
      iEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(
        TEditorNotifier.Create);
      {$ELSE}
      objEditorNotifier := TEditorNotifier.Create;
      {$ENDIF}
      iKeyBinding := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
        TKeyboardBinding.Create(Wizard))
    End;
End;

(**

  This is the constructor method for the TEditorNotifier class.

  @precon  None.
  @postcon Initialise the class be creating a time for handling editor changes.

**)
constructor TEditorNotifier.Create;
begin
  FUpdateTimer := TTimer.Create(Nil);
  {$IFDEF D2005}
  FUpdateTimer.Interval := 100;
  {$ELSE}
  FUpdateTimer.Interval := 500;
  {$ENDIF}
  FUpdateTimer.OnTimer := TimerEventHandler;
  FLastParserResult := True;
  FUpdateTimer.Enabled := True;
end;

(**

  This is the destructor method for the TEditorNotifier class.

  @precon  None.
  @postcon Frees the timer control.

**)
destructor TEditorNotifier.Destroy;
begin
  FUpdateTimer.Free;
  inherited;
end;

(**

  This method extracts the filename, modified status and the editor stream of
  code for the BrowseAndDocItThread.

  @precon  None.
  @postcon Extracts the filename, modified status and the editor stream of
           code for the BrowseAndDocItThread.

  @param   strFileName  as a String as a reference
  @param   boolModified as a Boolean as a reference
  @return  a String

  @refactor Perhaps in hindsight, the compiler defines should be passed to the
            parsers create method rather than be part of the application options.

**)
function TEditorNotifier.EditorInfo(var strFileName: String;
  var boolModified: Boolean) : String;

Var
  SE : IOTASourceEditor;
  Options : IOTAProjectOptions;

begin
  Result := '';
  SE := ActiveSourceEditor;
  If SE <> Nil Then
    Begin
      strFileName := SE.FileName;
      boolModified := SE.Modified;
      Result := EditorAsString(SE);
      If ActiveProject <> Nil Then
        Begin
          Options := ActiveProject.ProjectOptions;
          BrowseAndDocItOptions.Defines.Text :=
            StringReplace(Options.Values['Defines'], ';', #13#10,
            [rfReplaceAll]);
        End;
      {$IFDEF VER120} // Delphi 4
      BrowseAndDocItOptions.Defines.Add('VER120');
      {$ENDIF}
      {$IFDEF VER130} // Delphi 5
      BrowseAndDocItOptions.Defines.Add('VER130');
      {$ENDIF}
      {$IFDEF VER140} // Delphi 6
      BrowseAndDocItOptions.Defines.Add('VER140');
      {$ENDIF}
      {$IFDEF VER150} // Delphi 7
      BrowseAndDocItOptions.Defines.Add('VER150');
      {$ENDIF}
      {$IFDEF VER160} // Delphi for .NET
      BrowseAndDocItOptions.Defines.Add('VER160');
      {$ENDIF}
      {$IFDEF VER170} // Delphi 2005
      BrowseAndDocItOptions.Defines.Add('VER170');
      {$ENDIF}
      {$IFDEF VER180} // Delphi 2006
      BrowseAndDocItOptions.Defines.Add('VER180');
      {$ENDIF}
      {$IFDEF VER190} // Delphi 2007
      BrowseAndDocItOptions.Defines.Add('VER190');
      {$ENDIF}
      {$IFDEF VER200} // Delphi 2009
      BrowseAndDocItOptions.Defines.Add('VER200');
      {$ENDIF}
      {$IFDEF WIN32}
      BrowseAndDocItOptions.Defines.Add('WIN32');
      BrowseAndDocItOptions.Defines.Add('MSWINDOWS');
      {$ELSE}
      BrowseAndDocItOptions.Defines.Add('LINUX');
      {$ENDIF}
    End;
end;

(**

  This method reenabled the timer and returns whether the parse failed or not.

  @precon  None.
  @postcon Reenabled the timer and returns whether the parse failed or not.

  @param   boolSuccessfulParse as a Boolean

**)
Procedure TEditorNotifier.EnableTimer(boolSuccessfulParse : Boolean);

begin
  FUpdateTimer.Enabled := True;
  FLastParserResult := boolSuccessfulParse;
end;

(**

  This method displays an exception message in a dialogue box.

  @precon  None.
  @postcon Displays an exception message in a dialogue box.

  @param   strExceptionMsg as a String

**)
procedure TEditorNotifier.ExceptionMsg(strExceptionMsg: String);
begin
  ShowMessage(strExceptionMsg);
end;

(**

  This method renders the given module in the module explorer window.

  @precon  None.
  @postcon Renders the given module in the module explorer window.

  @param   Module as a TBaseLanguageModule

**)
procedure TEditorNotifier.RenderDocument(Module: TBaseLanguageModule);
begin
  TfrmDockableModuleExplorer.RenderDocumentTree(Module);
end;

(**

  This is a TTimer on Timer event handler.

  @precon  None.
  @postcon Checks to see if the last time the editor was changes is beyond the
           wait time for the module to be updated. Creates an instance of the
           thread to render the module explorer.

  @param   Sender as a TObject

**)
procedure TEditorNotifier.TimerEventHandler(Sender: TObject);

  {$IFNDEF D2005}
  (**

    This function returns the size of the editor stream, i.e. size of the text
    buffer.

    @precon  If Editor is Nil 0 is returned.
    @postcon Returns the size of the editor stream, i.e. size of the text
             buffer.

    @param   Editor as an IOTASourceEditor
    @return  an Int64

  **)
  Function MemStreamSize(Editor : IOTASourceEditor) : Int64;

  Var
    strSource : String;

  Begin
    Result := 0;
    If Editor <> Nil Then
      Begin
        strSource := EditorAsString(Editor);
        Result := Length(strSource);
      End;
  End;
  {$ENDIF}

Var
  Editor : IOTASourceEditor;
  P : TOTAEditPos;

begin
  Editor := ActiveSourceEditor;
  If Editor <> Nil Then
    Begin
      If Editor.GetEditViewCount > 0 Then
        P := Editor.GetEditView(0).CursorPos;
      If iLastUpdateTickCount > 0 Then
        If (P.Col <> FLastCursorPos.Col) Or (P.Line <> FLastCursorPos.Line) Then
          Begin
            iLastUpdateTickCount := GetTickCount;
            FLastCursorPos := P;
          End;
      If Editor.FileName <> FLastEditorName Then
        Begin
          iLastUpdateTickCount := 1;
          FLastEditorName := Editor.FileName;
        End
      {$IFNDEF D2005}
        Else If FLastSize <> MemStreamSize(Editor) Then
            Begin
              iLastUpdateTickCount := GetTickCount;
              FLastSize := MemStreamSize(Editor);
            End
      {$ENDIF};
    End;
  If (iLastUpdateTickCount > 0) And
    (GetTickCount > iLastUpdateTickCount + BrowseAndDocItOptions.UpdateInterval) Then
    Begin
      iLastUpdateTickCount := 0;
      If (Application <> Nil) And (Application.MainForm <> Nil) And
        Application.MainForm.Visible Then
        Begin
      {$IFNDEF D2005}
      FLastSize := MemStreamSize(Editor);
      {$ENDIF}
          FUpdateTimer.Enabled := False;
          TBrowseAndDocItThread.CreateBrowseAndDocItThread(EnableTimer,
            EditorInfo, RenderDocument, ExceptionMsg);
        End;
    End;
end;

{$IFDEF D2005}
(**

  This an impementation of the DockFormRefresh method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow constant
  @param   DockForm   as a TDockableForm

**)
procedure TEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
end;

(**

  This an impementation of the DockFormUpdate method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow constant
  @param   DockForm   as a TDockableForm

**)
procedure TEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
end;

(**

  This an impementation of the DockFormVisibleChange method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow constant
  @param   DockForm   as a TDockableForm

**)
procedure TEditorNotifier.DockFormVisibleChanged(
  const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
end;

(**

  This an impementation of the EditorViewActivate method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Refreshes the module explorer IF the last parser was sucessful.

  @param   EditWindow as an INTAEditWindow constant
  @param   EditView   as an IOTAEditView constant

**)
procedure TEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  If FLastParserResult Then
    iLastUpdateTickCount := 1;
end;

(**

  This an impementation of the EditorViewModified method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Logs the last time the editor was updated.

  @param   EditWindow as an INTAEditWindow constant
  @param   EditView   as an IOTAEditView constant

**)
procedure TEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  iLastUpdateTickCount := GetTickCount;
end;

(**

  This an impementation of the WindowActivated method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow constant

**)
procedure TEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
end;

(**

  This an impementation of the WindowCommand method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow constant
  @param   Command    as an Integer
  @param   Param      as an Integer
  @param   Handled    as a Boolean as a reference

**)
procedure TEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow;
  Command, Param: Integer; var Handled: Boolean);
begin
end;

(**

  This method is not used by this class and is therefore not implemented.

  @precon  None.
  @postcon None.

  @param   EditWindow as an INTAEditWindow constant
  @param   Operation  as a TOperation

**)
procedure TEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
end;

(**

  This method is not used by this class and is therefore not implemented.

  @precon  None.
  @postcon None.

  @param   EditWindow        as an INTAEditWindow constant
  @param   Show              as a Boolean
  @param   LoadedFromDesktop as a Boolean

**)
procedure TEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
end;

(**

  This is a method which obtains information about the package from is
  version information with the package resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within
           the EXE file.

  @param   iMajor  as an Integer
  @param   iMinor  as an Integer
  @param   iBugFix as an Integer
  @param   iBuild  as an Integer

**)
Procedure BuildNumber(var iMajor, iMinor, iBugFix, iBuild : Integer);

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  strBuffer : Array[0..MAX_PATH] Of Char;

Begin
  { Build Number }
  GetModuleFilename(hInstance, strBuffer, MAX_PATH);
  VerInfoSize := GetFileVersionInfoSize(strBuffer, Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(strBuffer, 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        With VerValue^ Do
          Begin
            iMajor := dwFileVersionMS shr 16;
            iMinor := dwFileVersionMS and $FFFF;
            iBugFix := dwFileVersionLS shr 16;
            iBuild := dwFileVersionLS and $FFFF;
          End;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;

End;
{$ENDIF}

(**

  This method binds all the Browse and Doc It keybindings to the methods in this
  class.

  @precon  None.
  @postcon All the keybinding are bound.

  @param   BindingServices as an IOTAKeyBindingServices as a constant

**)
procedure TKeyboardBinding.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([Shortcut(13, [ssCtrl, ssShift, ssAlt])], FocusModuleExplorer, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('T'), [ssCtrl, ssShift, ssAlt])], ShowTokens, Nil);
end;

(**

  This is a keyboard binding event hanlder for showing the tokens in the parser.

  @precon  None.
  @postcon Displays a form containsing the tokens in the current editor.

  @param   Context       as an IOTAKeyContext as a constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.ShowTokens(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);

Var
  SourceEditor : IOTASourceEditor;
  Source : TBaseLanguageModule;

begin
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  Source := Dispatcher(EditorAsString(SourceEditor), SourceEditor.FileName,
    SourceEditor.Modified, []);
  If Source <> Nil Then
    Try
      TfrmTokenForm.Execute(Source);
    Finally
      Source.Free;
    End;
  BindingResult := krHandled;
end;

(**

  This is the constructor method for the TKeyboardBinding class.

  @precon  None.
  @postcon Initialises the internal wizard reference.

  @param   Wizard as a TBrowseAndDocItWizard

**)
constructor TKeyboardBinding.Create(Wizard: TBrowseAndDocItWizard);
begin
  FWizard := Wizard;
end;

(**

  This method is a handler for the Focus Modul Explorer keyboatf binding.

  @precon  None.
  @postcon Asks the main wizard for focus the module explorer.

  @param   Context       as an IOTAKeyContext as a constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.FocusModuleExplorer(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  FWizard.ModuleExplorerClick(Self);
  BindingResult := krHandled;
end;

(**

  This is a getter method for the BindingType property.

  @precon  None.
  @postcon Defines the keyboard binding as a partial binding set.

  @return  a TBindingType

**)
function TKeyboardBinding.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

(**

  This is a getter method for the DisplayName property.

  @precon  None.
  @postcon Returns the diosplay name for the set of Keyboard Bindings.

  @return  a string

**)
function TKeyboardBinding.GetDisplayName: string;
begin
  Result := 'Browse And Doc It Comment Bindings';
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of th keyboard binding set.

  @return  a string

**)
function TKeyboardBinding.GetName: string;
begin
  Result := 'BrowseAndDocItBindings';
end;

{$IFDEF D2005}
Var
  (** This is a handle for the splash screen bitmap resource **)
  bmSplashScreen : HBITMAP;
  (** This is a variable to hold the major version number for the package. **)
  iMajor : Integer;
  (** This is a variable to hold the minor version number for the package. **)
  iMinor : Integer;
  (** This is a variable to hold the bug fix version number for the package. **)
  iBugFix : Integer;
  (** This is a variable to hold the build number for the package. **)
  iBuild : Integer;
{$ENDIF}

(** This initialization section installs an IDE Splash Screen item. **)
Initialization
  {$IFDEF D2005}
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap');
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1)]),
    bmSplashScreen,
    False,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]), ''
    );
  {$ENDIF}
(** This finalization section removes this wizard from the IDE when the package
    is unloaded. **)
Finalization
  (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBinding);
  {$IFDEF D2005}
  (BorlandIDEServices As IOTAEditorServices).RemoveNotifier(iEditorIndex);
  {$ELSE}
  objEditorNotifier.Free;
  {$ENDIF}
  {(BorlandIDEServices As IOTAServices).RemoveNotifier(iIDENotifier);}
  (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
End.
