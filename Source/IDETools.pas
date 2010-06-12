(**

  This module defines the VBE IDE Tools interface between the IDE and the
  available tools.

  @Version 1.0
  @Date    12 Jun 2010
  @Author  David Hoyle

**)
Unit IDETools;

Interface

Uses
  Office2000_TLB, VBIDE_TLB, SysUtils, Windows, EventSink, ComObj,
  IniFiles, ExtCtrls, Dialogs, Classes, Forms, DocumentationDispatcher,
  BaseLanguageModule, Contnrs, ActnList, Messages, CommonIDEFunctions;

Type
  (** This is a record to keep together the menu item and the event sink. **)
  TIDEMenuItem = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMenu : CommandBarButton;
    FSink : TEventSink;
  Public
    Constructor Create(ClickProc: TClickProc);
    Destructor Destroy; Override;
    (**
      This property holds a reference to the IDE Menu item.
      @precon  None.
      @postcon Gets an sets the menu reference.
      @return  a CommandBarButton
    **)
    Property Menu : CommandBarButton Read FMenu Write FMenu;
    (**
      This property holds a reference to the IDE Sink item.
      @precon  None.
      @postcon Gets an sets the sink reference.
      @return  a TEventSink
    **)
    Property Sink : TEventSink Read FSink Write FSink;
  End;

  (** This is a record to describe a column and line cursor position. **)
  TEditPos = Packed record
    Col : Integer;
    Line: Integer;
  End;

  (** This class represents the VBE IDE Tools interface between the IDE and
      the available tools. **)
  TIDETools = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOldHandle : Integer;
    FCounter : Cardinal;
    FOldLength : Integer;
    (** Reference for the current VBE IDE **)
    FVBEIDE : VBE;
    FIDEMenu : CommandBarControl;
    FSinks : TObjectList;
    FCookies : LongInt;
    FTimer : TTimer;
    FVBProject : VBProject;
    FIgnoreVBAProject: Boolean;
    FPath : String;
    FCodeFragWidth : Integer;
    FDocType : TDocType;
    FActions : TActionList;
    FIdleTimer : TTimer;
    FMEVisible: Boolean;
    FVisible: Boolean;
    FOldWndProc : TFarProc;
    FNewWndProc : TFarProc;
    FLastCodePane: CodePane;
    FBADIThreadMgr : TBrowseAndDocItThreadManager;
    procedure CreateMenu;
    function GetProjectPath(strName: String): String;
    procedure SetProjectPath(strName: String; strValue: String);
    procedure SaveModules(Project: VBPRoject; slModules: TStringList);
    function GetModule(strProject, strModule: String): VBComponent;
    function GetWindowPosition(strName: String): TRect;
    procedure SetWindowPosition(strName: String; const Value: TRect);
    function GetModuleCode: String;
    function GetCursorPosition: TEditPos;
    Function GetCodePane : CodePane;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure ModuleExplorerClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure DocumentationClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure FocusEditorClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure InsertMethodCommentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure InsertPropertyCommentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure InsertBlockCommentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure InsertLineCommentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure InsertInSituCommentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure ExportClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure ImportClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure SaveCodeFragmentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure InsertCodeFragmentClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure ShowTokensClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure OptionsClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure CheckForUpdatesClick(Const Ctrl : CommandBarButton; Var CancelDefault : WordBool);
    Procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer);
    Procedure Focus(Sender : TObject);
    Procedure ScopeChange(Sender : TObject);
    Procedure TimerEvent(Sender : TObject);
    Procedure VBProjectChangeEvent(Project : VBProject);
    Procedure CodePaneChangeEvent(Pane : CodePane; Project : VBProject);
    Procedure Save;
    Function GetFileName(strProject, strModule : String; iType : Integer) : String;
    procedure PositionCursorInFunction(CursorDelta: TPoint; iInsertLine: Integer; iIndent: Integer; strComment: string);
    Procedure SuccessfulParse(boolSuccessfulParse : Boolean);
    Function  EditorInfo(var strFileName : String; var boolModified : Boolean) : String;
    Procedure RenderDocument(Module : TBaseLanguageModule);
    Procedure ExceptionMsg(strExceptionMsg : String);
    Procedure IdleTimerEvent(Sender : TObject);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure WndProc(var Msg : TMessage);
    Procedure MEFormClose(Sender : TObject; var CloseAction : TCloseAction);
    (**
      This property reads and write the project paths to and from the registry.
      @precon  None.
      @postcon Reads and write the project paths to and from the registry.
      @param   strName as       a String
      @return  a String
    **)
    Property ProjectPath[strName : String] : String Read GetProjectPath
      Write SetProjectPath;
    (**
      This property determines if the standard VBAProject should be ignored.
      @precon  None.
      @postcon Gets and sets whether the VBAProject should be ignored.
      @return  a Boolean
    **)
    Property IgnoreVBAProject : Boolean Read FIgnoreVBAProject Write FIgnoreVBAProject;
    (**
      This property attempts to get the specified module interface from the specified
      project.
      @precon  None.
      @postcon Attempts to get the specified module interface from the specified
               project.
      @param   strProject as       a String
      @param   strModule  as       a String
      @return  a VBComponent
    **)
    Property Modules[strProject, strModule : String] : VBComponent Read GetModule;
    (**
      This property allows a windows bounding rectangle to be read and written to the
      registry.
      @precon  None.
      @postcon Allows a windows bounding rectangle to be read and written to the
               registry.
      @param   strName as       a String
      @return  a TRect
    **)
    Property WindowPosition[strName : String] : TRect Read GetWindowPosition
      Write SetWindowPosition;
    (**
      This property returns the modules code as a string.
      @precon  None.
      @postcon Returns the module code as a String.
      @return  a String
    **)
    Property ModuleCode : String Read GetModuleCode;
    (**
      This property returns the current code panes cursor position.
      @precon  None.
      @postcon Returns the current code panes cursor position.
      @return  a TEditPos
    **)
    Property CursorPosition : TEditPos Read GetCursorPosition;
    (**
      This property returns the editor`s current code pane.
      @precon  None.
      @postcon Returns the editor`s current code pane.
      @return  a CodePane
    **)
    Property CurrentCodePane : CodePane Read GetCodePane;
  Public
    Constructor Create(VBEIDERef : VBE);
    Destructor Destroy; Override;
  End;

ResourceString
  (** This is a resource string for the main menu title for the tools. **)
  strMenuCaption = '&Browse and Doc It';
  (** This is the caption for the ModuleExplorer Menu Item. **)
  strModuleExplorer = '&Module Explorer';
  (** This is the caption for the Export Menu Item. **)
  strExport = '&Export...';
  (** This is the caption for the Import Menu Item. **)
  strImport = '&Import...';
  (** This is the caption for the Save Code Fragment Menu Item. **)
  strSaveCodeFragment = '&Save Code Fragment...';
  (** This is the caption for the Insert Code Fragment Menu Item. **)
  strInsertCodeFragment = 'Insert Code &Fragment...';
  (** This is the caption for the Documentation Menu Item. **)
  strDocumentation = '&Documentation...';
  (** This is the caption for the Show Tokens Menu Item. **)
  strShowTokens = '&Show Tokens...';
  (** This is the caption for the Options Menu Item. **)
  strOptions = '&Options...';
  (** This is the caption for the Check for Updates Menu Item. **)
  strCheckForUpdates = 'Check for &Updates...';
  (** This is the caption for the Focus Editor Menu Item. **)
  strFocusEditor = 'Focus &Editor';
  (** This is the caption for the Insert Method Comment Menu Item. **)
  strInsertMethodComment = 'Insert &Method Comment';
  (** This is the caption for the Insert Property Comment Menu Item. **)
  strInsertPropertyComment = 'Insert &Property Comment';
  (** This is the caption for the Insert Block Comment Menu Item. **)
  strInsertBlockComment = 'Insert &Block Comment';
  (** This is the caption for the Insert Line Comment Menu Item. **)
  strInsertLineComment = 'Insert &Line Comment';
  (** This is the caption for the Insert InSitu Comment Menu Item. **)
  strInsertInSituComment = 'Insert &InSitu Comment';
  (** This is a message for checking out a single file. **)
  strLockedMsg = 'The module "%s" in project "%s" is read only and therefore probably ' +
    'locked by the Version Control System?'#13#13 +
    'Pressing CANCEL will leave the module open for viewing - DO NOT MAKE CHANGES.';
  (** This is an Exception message warning the specified file is read only. **)
  strFileReadOnly = 'The file "%s" is read only.';
  (** This is a message for a code fragment that already exists. **)
  strFileExists = 'The code fragment "%s" already exists. Do you want to overwrite this fragment?';

Implementation

Uses
  ExportForm, ProgressForm, Controls, FileCtrl, Functions, TokenForm,
  ModuleDispatcher, OptionsForm, DocumentationOptionsForm, ShellAPI,
  CodeFragmentsForm, Variants, VBEIDEModuleExplorer,
  DGHLibrary, Math, VBModule, checkforupdates, Menus;

{ TIDEMenuItem }

(**

  This is a constructor for the TIDEMEnuItem class.

  @precon  None.
  @postcon Creates an instance of a event sink class.

  @param   ClickProc as a TClickProc

**)
constructor TIDEMenuItem.Create(ClickProc: TClickProc);
begin
  FSink := TEventSink.Create(ClickProc);
end;

(**

  This is a destructor for the TIDEMenuItem class.

  @precon  None.
  @postcon Frees the memory used by the event sink.

**)
destructor TIDEMenuItem.Destroy;
begin
  FSink.Free;
  Inherited Destroy;
end;

{ TIDETools }

(**

  This is the constructor method for the TIDETools class.

  @precon  None.
  @postcon Initialises the VBE IDE Tools class by:
             Opening the About Form
             Creating a message window.
             Creating the menu system.
             Creating and starting an event timer
             Loading the applications settings.

  @param   VBEIDERef as a VBE

**)
constructor TIDETools.Create(VBEIDERef : VBE);

begin
  Try
    FOldHandle := Application.Handle;
    Application.Handle := VBEIDERef.MainWindow.HWnd;
    TCheckForUpdates.Execute('BrowseAndDocIt2006',
      BrowseAndDocItOptions.INIFileName, False);
    FPath := ExtractFilePath(BrowseAndDocItOptions.INIFileName) + 'Code Fragments\';
    ForceDirectories(FPath);
    FVBEIDE := VBEIDERef;
    TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
    TfrmDockableModuleExplorer.HookEventHandlers(SelectionChange, Focus,
      ScopeChange, MEFormClose);
    TfrmDockableModuleExplorer.SetModuleExplorerPosition(WindowPosition['ModuleExplorer']);
    FVBProject := Nil;
    FSinks := TObjectList.Create(True);
    CreateMenu;
    FBADIThreadMgr := TBrowseAndDocItThreadManager.Create;
    FTimer := TTimer.Create(Nil);
    FTimer.Interval := 100;
    FTimer.OnTimer := TimerEvent;
    FCounter := 0;
    FVBEIDE.MainWindow.SetFocus;
    LoadSettings;
    FIdleTimer := TTimer.Create(Nil);
    FIdleTimer.Interval := 100;
    FIdleTimer.OnTimer := IdleTimerEvent;
    FOldWndProc := TFarProc(GetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC));
    FNewWndProc := Classes.MakeObjectInstance(WndProc);
    //: @debug SetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC, LongWord(FNewWndProc));
  Except
    On E : Exception Do DisplayException(E.Message);
  End;
end;

(**

  This is the destructor method for the TIDETools class.

  @precon  None.
  @postcon Frees all memory allocated by the class and removes the tools menu.

**)
destructor TIDETools.Destroy;
begin
  SaveSettings;
  //: @debug SetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC, LongWord(FOldWndProc));
  Classes.FreeObjectInstance(FNewWndProc);
  FActions.Free;
  WindowPosition['ModuleExplorer'] := TfrmDockableModuleExplorer.GetModuleExplorerPosition;
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer;
  FIdleTimer.Free;
  FTimer.Free;
  FBADIThreadMgr.Free;
  FSinks.Free;
  If (FVBEIDE <> Nil) And (FIDEMenu <> Nil) Then
    FVBEIDE.CommandBars.Item['Menu Bar'].Controls_[strMenuCaption].Delete(False);
  Application.Handle := FOldHandle;
  inherited Destroy;
end;

(**

  This method saves the applications settings to the ini file.

  @precon  None.
  @postcon Saves the applications settings to the ini file.

**)
procedure TIDETools.SaveSettings;
begin
  with TIniFile.Create(BrowseAndDocItOptions.INIFileName) do
    try
      WriteBool('ModuleExplorer', 'Visible', FMEVisible);
      WriteInteger('Documentation options', 'LastOption', Byte(FDocType));
      WriteInteger('Setup', 'CodeFragWidth', FCodeFragWidth);
    finally
      Free;
    end;
end;

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
procedure TIDETools.PositionCursorInFunction(CursorDelta: TPoint;
  iInsertLine: Integer; iIndent: Integer; strComment: string);

Var
  Pt: TPoint;
  CP: CodePane;

begin
  SelectionChange(iInsertLine + CharCount(#13, strComment) + 1, 1, iInsertLine, 1);
  Pt.Y := iInsertLine;
  Pt.X := iIndent + 3;
  Inc(Pt.Y, CursorDelta.Y);
  Inc(Pt.X, CursorDelta.X);
  CP := CurrentCodePane;
  If CP <> Nil Then
    CP.SetSelection(Pt.Y, Pt.X, Pt.Y, Pt.X);
end;

(**

  This method creates the menus required by the tools addin.

  @precon  None.
  @postcon Creates all the menus required by the tools addin.

**)
Procedure TIDETools.CreateMenu;

  (**

    This method creates a sub menu item to the main tools menu, sets the
    caption, creates an event sink and hooks an event handler for the menu.

    @precon  None.
    @postcon Creates a sub menu item to the main tools menu, sets the
             caption, creates an event sink and hooks an event handler for the
             menu.

    @param   strCaption     as a String
    @param   EventHandler   as a TClickProc
    @param   boolBeginGroup as a Boolean
    @param   strShortCut    as a String

  **)
  Procedure CreateMenuItem(strCaption : String; EventHandler : TClickProc;
    boolBeginGroup : Boolean; strShortCut : String = '');

  Var
    S : TIDEMenuItem;

  Begin
    S := TIDEMenuItem.Create(EventHandler);
    FSinks.Add(S);
    S.Menu := (FIDEMenu As CommandBarPopup).Controls_.Add(msoControlButton,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam) As CommandBarButton;
    S.Menu.Caption := strCaption;
    S.Menu.BeginGroup := boolBeginGroup;
    {: @bug Not implemented since I can not handle shortcuts. Subclassing
            of the main window fails on closedown IF there is another addin
            which subclasses the IDE.
    If strShortCut <> '' Then
      S.Menu.ShortCutText := 'Ctrl+Shift+Alt+' + strShortCut;
    }
    InterfaceConnect(S.Menu, DIID__CommandBarButtonEvents, S.Sink, FCookies);
  End;

Var
  MainMenu : CommandBar;

Begin
  If FVBEIDE <> Nil Then
    Begin
      MainMenu := FVBEIDE.CommandBars.Item['Menu Bar'];
      FIDEMenu := MainMenu.Controls_.Add(msoControlPopup, EmptyParam,
        EmptyParam, MainMenu.Controls_.Count - 1, EmptyParam);
      FIDEMenu.Set_Caption(strMenuCaption);
      CreateMenuItem(strModuleExplorer, ModuleExplorerClick, False, 'Enter');
      CreateMenuItem(strDocumentation, DocumentationClick, False);
      CreateMenuItem(strFocusEditor, FocusEditorClick, True, 'E');
      CreateMenuItem(strInsertMethodComment, InsertMethodCommentClick, False, 'M');
      CreateMenuItem(strInsertPropertyComment, InsertPropertyCommentClick, False, 'P');
      CreateMenuItem(strInsertBlockComment, InsertBlockCommentClick, False, 'B');
      CreateMenuItem(strInsertLineComment, InsertLineCommentClick, False, 'L');
      CreateMenuItem(strInsertInSituComment, InsertInSituCommentClick, False, 'I');
      CreateMenuItem(strExport, ExportClick, True, 'X');
      {: @debug CreateMenuItem(strImport, ImportClick, False, 'R'); }
      CreateMenuItem(strSaveCodeFragment, SaveCodeFragmentClick, True, 'S');
      CreateMenuItem(strInsertCodeFragment,InsertCodeFragmentClick, False, 'N');
      CreateMenuItem(strOptions, OptionsClick, True, 'O');
      CreateMenuItem(strCheckForUpdates, CheckForUpdatesClick, True, 'O');
    End;
End;

(**

  This is a timer event handler.

  @precon  None.
  @postcon Used to create events for the changing of the current VB Project and the
           Current Code Pane because I can not hook the events in the IDE - not sure you
           can in the VBE IDE.

  @param   Sender as a TObject

**)
procedure TIDETools.TimerEvent(Sender: TObject);

Var
  i : Integer;
  boolRefresh : Boolean;
  CP: CodePane;

begin
  Try
    If Not TfrmDockableModuleExplorer.IsVisible Then
      Exit;
    If FVBEIDE.CodePanes.Count = 0 Then
      Exit;
    If FVBProject <> FVBEIDE.ActiveVBProject Then
      Begin
        FVBProject := FVBEIDE.ActiveVBProject;
        If FVBPRoject <> Nil Then
          If FVBProject.Protection = vbext_pp_locked Then
            FVBProject := Nil;
        VBProjectChangeEvent(FVBProject);
      End;
    CP := CurrentCodePane;
    If FLastCodePane <> CP Then
      Begin
        FLastCodePane := CP;
        CodePaneChangeEvent(CP, FVBProject);
      End;
    Inc(FCounter, 100);
    boolRefresh := (FCounter >= BrowseAndDocItOptions.UpdateInterval) And
      (FCounter <= BrowseAndDocItOptions.UpdateInterval + 100);
    i := Length(ModuleCode);
    If i <> FOldLength Then
      Begin
        FCounter := 0;
        FOldLength := i;
      End;
    If boolRefresh Then
      Begin
        FTimer.Enabled := False;
        FBADIThreadMgr.Parse(SuccessfulParse, EditorInfo, RenderDocument,
          ExceptionMsg);
        FOldLength := Length(ModuleCode);
      End;
  Except
    On E: Exception Do DisplayException(E.Message);
  End;
end;

(**

  This is a getter method for the ProjectPath property.

  @precon  None.
  @postcon Gets the project path for the given project name.

  @param   strName as a String
  @return  a String

**)
function TIDETools.GetProjectPath(strName: String): String;
begin
  With TIniFile.Create(BrowseAndDocItOptions.INIFileName) Do
    Try
      Result := ReadString('ProjectPaths', strName, '');
    Finally
      Free;
    End;
end;

(**

  This is a setter method for the ProjectPath property.

  @precon  None.
  @postcon Sets the project path for the given project name.

  @param   strName  as a String
  @param   strValue as a String

**)
procedure TIDETools.SetProjectPath(strName : String; strValue: String);
begin
  If strValue[Length(strValue)] <> '\' Then
    strValue := strValue + '\';
  With TIniFile.Create(BrowseAndDocItOptions.INIFileName) Do
    Try
      WriteString('ProjectPaths', strName, strValue);
    Finally
      Free;
    End;
end;

(**

  This is a getter method for the Module property.

  @precon  None.
  @postcon Gets the module reference for the given project and module name else returns
           nil is they were not found.

  @param   strProject as a String
  @param   strModule  as a String
  @return  a VBComponent

**)
function TIDETools.GetModule(strProject, strModule: String): VBComponent;

Var
  i, j : Integer;
  Project : VBProject;

begin
  Result := Nil;
  For i := 1 To FVBEIDE.VBProjects.Count Do
    If CompareText(FVBEIDE.VBProjects.Item(i).Name, strProject) = 0 Then
      Begin
        Project := FVBEIDE.VBProjects.Item(i);
        For j := 1 To Project.VBComponents.Count Do
          If CompareText(Project.VBComponents.Item(j).Name, strModule) = 0 Then
            Begin
              Result := Project.VBComponents.Item(j);
              Exit;
            End;
      End;
  DisplayException(Format('The module "%s" was not found in project "%s".',
    [strModule, strProject]));
end;

(**

  This is a getter method for the CurrentCodePane property.

  @precon  None.
  @postcon Return the editors current code pane else returns nil.

  @return  a CodePane

**)
function TIDETools.GetCodePane: CodePane;
begin
  Result := Nil;
  If FVBEIDE <> Nil Then
    Result := FVBEIDE.ActiveCodePane;
end;

(**

  This is a getter method for the CursorPosition property.

  @precon  None.
  @postcon Returns the cursor position in the current code pane.

  @return  a TEditPos

**)
function TIDETools.GetCursorPosition : TEditPos;

Var
  i, j : Integer;
  CP: CodePane;

begin
  CP := CurrentCodePane;
  If CP <> Nil Then
    CP.GetSelection(Result.Line, Result.Col, i, j);
end;

(**

  This is a getter method for the FileName property.

  @precon  None.
  @postcon Returns the full path for the project module given its type.

  @param   strProject as a String
  @param   strModule  as a String
  @param   iType      as an Integer
  @return  a String

**)
function TIDETools.GetFileName(strProject, strModule: String; iType: Integer): String;
begin
  Result := ProjectPath[strProject] + strModule;
  Case iType Of
    1 :   Result := Format('%s.bas', [Result]);
    2 :   Result := Format('%s.cls', [Result]);
    3 :   Result := Format('%s.frm', [Result]);
    11 :  Result := Format('%s.txt', [Result]);
    100 : Result := Format('%s.cls', [Result]);
  Else
    Result := Format('%s.txt', [Result]);
  End;
end;

(**

  This is a getter method for the WindowPosition property.

  @precon  None.
  @postcon Gets the bounding rectangle for the named window from the registry.

  @param   strName as a String
  @return  a TRect

**)
function TIDETools.GetWindowPosition(strName: String): TRect;

begin
  With TIniFile.Create(BrowseAndDocItOptions.INIFileName) do
    Try
      Result.Left := ReadInteger(strName, 'Left', 0);
      Result.Top := ReadInteger(strName, 'Top', 0);
      Result.Right := ReadInteger(strName, 'Right', 0);
      Result.Bottom := ReadInteger(strName, 'Bottom', 0);
    Finally
      Free;
    End;
end;

(**

  This is a setter method for the WindowPosition property.

  @precon  None.
  @postcon Saves the bounding rectangle for the named window to the registry.

  @param   strName as a String
  @param   Value   as a TRect as a constant

**)
procedure TIDETools.SetWindowPosition(strName: String; const Value: TRect);
begin
  With TIniFile.Create(BrowseAndDocItOptions.INIFileName) do
    Try
      WriteInteger(strName, 'Left', Value.Left);
      WriteInteger(strName, 'Top', Value.Top);
      WriteInteger(strName, 'Right', Value.Right);
      WriteInteger(strName, 'Bottom', Value.Bottom);
    Finally
      Free;
    End;
end;

(**

  This is an on click event handler for the show tokens menu item.

  @precon  None.
  @postcon Displays the tokens for the currently selected code pane.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.ShowTokensClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  doc : TBaseLanguageModule;
  strFileName : String;
  strTemp : String;
  CP: CodePane;

begin
  Try
    CP := CurrentCodePane;
    If CP = Nil Then
      Exit;
    strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
      CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
    strTemp := ModuleCode;
    doc := Dispatcher(strTemp, strFileName, Not CP.CodeModule.Parent.Saved,
      [moParse]);
    Try
      TfrmTokenForm.Execute(doc);
    Finally
      doc.Free;
    End;
  Except
    On E: Exception Do
      DisplayException(E.Message);
  End;
end;

(**

  This method re-enables the timer for the parsing of code once the parser has
  parsed the code successfully.

  @precon  None.
  @postcon Re-enables the timer for the parsing of code once the parser has
           parsed the code successfully.

  @param   boolSuccessfulParse as a Boolean

**)
procedure TIDETools.SuccessfulParse(boolSuccessfulParse: Boolean);
begin
  FTimer.Enabled := True;
end;

(**

  This method outputs an exception message from the browse and doc it thread.

  @precon  None.
  @postcon Outputs an exception message from the browse and doc it thread.

  @param   strExceptionMsg as a String

**)
procedure TIDETools.ExceptionMsg(strExceptionMsg: String);
begin
  ShowMessage(strExceptionMsg);
end;

(**

  This is a button on click event handler for the Export button.

  @precon  None.
  @postcon Displays the export dialogue. If the dialogue is not cancelled then
           the selected modules from the current project are exported to the
           selected path.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.ExportClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  slModules : TStringList;
  vbp : VBProject;
  i : Integer;
  strPath : String;
  Module : VBComponent;
  State : TStatuses;
  strFileName : String;
  R : TRect;

begin
  Try
    vbp := FVBEIDE.ActiveVBProject;
    If vbp <> Nil Then
      Begin
        If vbp.Protection = vbext_pp_locked Then
          Begin
            MessageDlg(Format('The VBA Project "%s" is locked. You need to ' +
              'unlock it before you can continue.', [vbp.Name]), mtWarning,
              [mbOK], 0);
            Exit;
          End;
        slModules := TStringList.Create;
        Try
          slModules.Sorted := True;
          For i := 1 To vbp.VBComponents.Count Do
            Begin
              State := [];
              Module := vbp.VBComponents.Item(i);
              If Module.Saved Then Include(State, msSaved);
              strFileName := GetFileName(vbp.Name, Module.Name, Module.Type_);
              If FileExists(strFileName) Then
                If FileGetAttr(strFileName) And faReadOnly <> 0 Then
                  Include(State, msLocked);
              slModules.AddObject(Format('%s=%d', [Module.Name, Module.Type_]),
                TObject(Byte(State)));
            End;
          strPath := ProjectPath[vbp.Name];
          If strPath = '' Then strPath := GetCurrentDir;
          R := WindowPosition['Export'];
          If TfrmExport.Execute('Export Modules', vbp.Name, slModules, strPath,
            R) Then
            Begin
              WindowPosition['Export'] := R;
              ProjectPath[vbp.Name] := strPath;
              SaveModules(vbp, slModules);
              Save;
            End;
        Finally
          slModules.Free;
        End;
      End Else
        MessageDlg('There is no active Visual Basic Project.',
          mtWarning, [mbOK], 0);
  Except
    On E: Exception Do MessageDlg(E.Message, mtError, [mbOK], 0);
  End;
end;

(**

  This method exports the modules which are modified.

  @precon  None.
  @postcon exports the modules which are modified.

  @param   Project   as a VBPRoject
  @param   slModules as a TStringList

**)
Procedure TIDETools.SaveModules(Project : VBPRoject; slModules : TStringList);

Var
  i : Integer;
  Module : VBComponent;
  strFileName : String;

Begin
  With TfrmProgress.Create(Nil) Do
    Try
      Init(slModules.Count, 'Saving Modules', 'Exporting Modules');
      Show;
      For i := 0 To slModules.Count - 1 Do
        Begin
          Module := Modules[Project.Name, slModules[i]];
          If Module <> Nil Then
            Begin
              strFileName := GetFileName(Project.Name, Module.Name, Module.Type_);
              If FileExists(strFileName) Then
                If FileGetAttr(strFileName) And faReadOnly <> 0 Then
                  Begin
                    ShowMessage(Format(strFileReadOnly, [strFileName]));
                    Exit;
                  End;
              Module.Export(strFileName);
              UpdateProgress(i, slModules[i]);
            End Else
              DisplayException(Format('Module "%s" was not found in Project "%s".',
                [slModules[i], Project.Name]));
        End;
    Finally
      Free;
    End;
End;

(**

  This event handler refreshes the tree view is thew scope list changes.

  @precon  None.
  @postcon Refreshes the tree view is thew scope list changes.

  @param   Sender as a TObject

**)
procedure TIDETools.ScopeChange(Sender: TObject);
begin
  FCounter := BrowseAndDocItOptions.UpdateInterval;
end;

(**

  This method import a set of files into the current project creating or replace code
  as necessary.

  @precon  None.
  @postcon Import a set of files into the current project creating or replace code
           as necessary.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference
**)
procedure TIDETools.ImportClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);
begin
  //: @todo Implement the Import Menu.
end;


(**

  This is an on click event handler for the Check for Updates menu item.

  @precon  None.
  @postcon Invokes the checking for updates system.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.CheckForUpdatesClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);
begin
  TCheckForUpdates.Execute('BrowseAndDocIt2006',
    BrowseAndDocItOptions.INIFileName, True);
end;

(**

  This is an event handler that is fired when ever the current code pane changes.

  @precon  None.
  @postcon Contains code that needs to be processed when the current code pane changes.

  @param   Pane    as a CodePane
  @param   Project as a VBProject

**)
procedure TIDETools.CodePaneChangeEvent(Pane : CodePane; Project : VBProject);

Var
  strFileName : String;
  CP: CodePane;

begin
  FCounter := BrowseAndDocItOptions.UpdateInterval;
  If Pane = Nil Then
    Exit;
  CP := CurrentCodePane;
  strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
      CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
  If FileExists(strFileName) Then
    If FileGetAttr(strFileName) And faReadOnly <> 0 Then
      If MessageDlg(Format(strLockedMsg, [CP.CodeModule.Parent.Name,
        CP.CodeModule.Parent.Collection.Parent.Name]), mtWarning,
        [mbOK, mbCancel], 0) = mrOK Then
        Pane.Window_.Close;
end;

(**

  This is an event handler that is fired when ever the current VB project changes.

  @precon  None.
  @postcon Contains code that needs to be processed when the current VB Project changes.

  @param   Project as a VBProject

**)
procedure TIDETools.VBProjectChangeEvent(Project: VBProject);

begin
  If Project = Nil Then Exit;
  If IgnoreVBAProject And (Project.Name = 'VBAProject') Then
    Exit;
end;

(**

  This is a Windows procedure for handling keyboard shortcuts in the IDE.

  @precon  None.
  @postcon NOT CURRENTLY IMPLEMENTED PENDING A SOLUTION TO THE SUBCLASS OF THE
           IDE PROBLEM.

  @param   Msg as a TMessage as a reference

**)
procedure TIDETools.WndProc(var Msg: TMessage);
begin
  Case Msg.Msg Of
    WM_KEYDOWN:
      Begin
        OutputDebugString('Hello Dave.');
      End;
  End;
  Msg.Result := CallWindowProc(FOldWndProc, FVBEIDE.MainWindow.HWnd, Msg.Msg,
    Msg.WParam, Msg.LParam);
end;

(**

  This methods invokes the Save command under the File menu to save the contents
  of the project file.

  @precon  None.
  @postcon Invokes the Save command under the File menu to save the contents
           of the project file.

**)
procedure TIDETools.Save;

Var
  MainMenu : CommandBar;
  FileMenu : CommandBarPopup;
  i, j : Integer;

begin
  MainMenu := FVBEIDE.CommandBars.Item['Menu Bar'];
  For i := 1 To MainMenu.Controls_.Count Do
    If MainMenu.Controls_.Item[i].Caption = '&File' Then
      Begin
        FileMenu := MainMenu.Controls_.Item[i] As CommandBarPopup;
        For j := 1 To FileMenu.Controls_.Count Do
          If Copy(FileMenu.Controls_.Item[j].Caption, 1, 5) = '&Save' Then
            FileMenu.Controls_.Item[j].Execute;
      End;
end;

(**

  This is an on option click event handler.

  @precon  None.
  @postcon Displays the optins dialogue.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.OptionsClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

begin
  If TfrmOptions.Execute([Low(TVisibleTab)..High(TVisibleTab)]) Then
    Begin
      FOldLength := 0;
      FCounter := BrowseAndDocItOptions.UpdateInterval;
    End;
end;

(**

  This is an on focus event handler which focuses the current code window.

  @precon  None.
  @postcon The current edit window is focused.

  @param   Sender as a TObject

**)
procedure TIDETools.Focus(Sender : TObject);

var
  CP: CodePane;

begin
  CP := CurrentCodePane;
  If CP = Nil Then
    Exit;
  CP.Window_.SetFocus;
end;

(**

  This method focuses the editor window when this menu is selected.

  @precon  None.
  @postcon Focuses the editor window when this menu is selected.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.FocusEditorClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);
begin
  Focus(Self);
end;

(**

  This is an on selection change event handler for the module explorer.

  @precon  None.
  @postcon Positions the cursor in the current code window based on the browsing
           options.

  @param   iIdentLine   as an Integer
  @param   iIdentCol    as an Integer
  @param   iCommentLine as an Integer
  @param   iCommentCol  as an Integer

**)
procedure TIDETools.SelectionChange(iIdentLine, iIdentCol, iCommentLine,
    iCommentCol : Integer);

Var
  C : TEditPos;
  iLineInView: Integer;
  CP: CodePane;

begin
  Focus(Self);
  CP := CurrentCodePane;
  If CP <> Nil Then
    If iIdentCol * iIdentLine > 0 Then
      Begin
        iLineInView := CP.CountOfVisibleLines Div 2;
        C.Col := iIdentCol;
        C.Line := iIdentLine;
        CP.SetSelection(C.Line, C.Col, C.Line, C.Col);
        Case BrowseAndDocItOptions.BrowsePosition Of
          bpCommentTop:
            Begin
              If iCommentLine > 0 Then
                iIdentLine := iCommentLine;
              CP.TopLine := iIdentLine;
            End;
          bpCommentCentre:
            Begin
              If iCommentLine > 0 Then
                iIdentLine := iCommentLine;
              CP.TopLine := Max(iIdentLine - iLineInView, 1);
            End;
          bpIdentifierTop:
            CP.TopLine := iIdentLine;
          bpIdentifierCentre:
            CP.TopLine := Max(iIdentLine - iLineInView, 1);
          bpIdentifierCentreShowAllComment:
            Begin
              CP.TopLine := Max(iIdentLine - iLineInView, 1);
              If iCommentLine > 0 Then
                If iCommentLine < CP.TopLine Then
                  CP.TopLine := iCommentLine;
            End;
        End;
      End;
end;

(**

  This is an on form event handler for the module explorer.

  @precon  None.
  @postcon Sets the visible variable to false if the module explorer is closed.

  @param   Sender      as a TObject
  @param   CloseAction as a TCloseAction as a reference

**)
procedure TIDETools.MEFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FMEVisible := False;
end;

(**

  This is an on click event handler for the module explorer menu option.

  @precon  None.
  @postcon Shows the module explorer and refreshes the tree view.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.ModuleExplorerClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);
begin
  TfrmDockableModuleExplorer.ShowDockableModuleExplorer;
  FMEVisible := True;
  FCounter := BrowseAndDocItOptions.UpdateInterval;
end;

(**

  This method renders the passed module in the module explorer.

  @precon  None.
  @postcon Renders the passed module in the module explorer.

  @param   Module as a TBaseLanguageModule

**)
procedure TIDETools.RenderDocument(Module: TBaseLanguageModule);
begin
  TfrmDockableModuleExplorer.RenderDocumentTree(Module);
end;

(**

  This is an on click event handler for the Documenation menu.

  @precon  None.
  @postcon Invokes the dialogue to document the code in the current project.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.DocumentationClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);
var
  i: Integer;

begin
  Try
    If FVBProject <> Nil Then
      With TfrmDocumentationOptions.Create(Nil) Do
        Try
          If Execute(FDocType) Then
            With DocumentDispatcher(ProjectPath[FVBEIDE.ActiveVBProject.Name],
              FVBEIDE.ActiveVBProject.Description, FDocType) Do
              Try
                For i := 1 To FVBEIDE.ActiveVBProject.VBComponents.Count Do
                  Add(GetFileName(FVBEIDE.ActiveVBProject.Name,
                    FVBEIDE.ActiveVBProject.VBComponents.Item(i).Name,
                    FVBEIDE.ActiveVBProject.VBComponents.Item(i).Type_));
                OutputDocumentation;
                ShellExecute(Application.Handle, 'Open', PChar(MainDocument), '', '',
                  SW_SHOWNORMAL);
              Finally
                Free;
              End;
        Finally
          Free;
        End
    Else
      MessageDlg('There is no current active VB Project.', mtWarning, [mbOK], 0);
  Except
    On E: Exception Do DisplayException(E.Message);
  End;
end;

(**

  This is a getter method for the ModuleCode property.

  @precon  None.
  @postcon Returns the code which constitutes the code of the current editor.

  @return  a String

**)
Function TIDETools.GetModuleCode : String;

var
  CP: CodePane;

Begin
  Result := '';
  CP := CurrentCodePane;
  If CP <> Nil Then
    If CP.CodeModule.CountOfLines > 0 Then
      Result := CP.CodeModule.Lines[1,
        CP.CodeModule.CountOfLines] + #13#10;
End;

(**

  This is an on timer event handler for the idle timer.

  @precon  None.
  @postcon Ensures that there is an equivalent message loop for the delphi code.

  @param   Sender as a TObject

**)
procedure TIDETools.IdleTimerEvent(Sender: TObject);

Var
  recMainWndInfo, recModExplWndInfo : TWindowInfo;

begin
  CheckSynchronize;
  GetWindowInfo(FVBEIDE.MainWindow.HWnd, recMainWndInfo);
  GetWindowInfo(TfrmDockableModuleExplorer.GetWndHnd, recModExplWndInfo);
  If FVBEIDE.MainWindow.Visible And (
    (recMainWndInfo.dwOtherStuff And WS_ACTIVECAPTION > 0) Or
    (recModExplWndInfo.dwOtherStuff And WS_ACTIVECAPTION > 0)) Then
    Application.DoApplicationIdle;
  If FVisible <> FVBEIDE.MainWindow.Visible Then
    Begin
      If Not FVBEIDE.MainWindow.Visible Then
        TfrmDockableModuleExplorer.SetVisible(False)
      Else
        TfrmDockableModuleExplorer.SetVisible(FMEVisible);
      FVisible := FVBEIDE.MainWindow.Visible;
    End;
end;

(**

  This is an on click event handler for the insert block comment menu.

  @precon  None.
  @postcon Inserts a block documentation comment at the current line.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertBlockCommentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  iSL, iSC, iEL, iEC : Integer;
  strLine : String;
  CP: CodePane;

begin
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      CP.GetSelection(iSL, iSC, iEL, iEC);
      strLine :=
        StringOfChar(#32, iSC - 1) + ''':'#13#10 +
        StringOfChar(#32, iSC - 1) + ''':'#13#10 +
        StringOfChar(#32, iSC - 1) + ''':';
      CP.CodeModule.InsertLines(iSL, strLine);
      SelectionChange(iSL + 1, iSC + 2, iSL + 1, iSC + 2);
    End Else
      MessageDlg('There is no active Code Pane.', mtError, [mbOK], 0);
end;

(**

  This is an on click event handler for the insert code fragment event handler.

  @precon  None.
  @postcon Inserts the selected code fragment into the current code pane at the
           cursor position.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertCodeFragmentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  R : TRect;
  str : String;
  iStartLine, iStartColumn, iEndLine, iEndColumn : Integer;
  CP: CodePane;

begin
  Try
    CP := CurrentCodePane;
    If CP <> Nil Then
      Begin
        R := WindowPosition['CodeFragments'];
        str := TfrmInsertCodeFragments.Execute(FPath, R, FCodeFragWidth);
        If str <> '' Then
          Begin
            CP.GetSelection(iStartLine, iStartColumn, iEndLine, iEndColumn);
            CP.CodeModule.InsertLines(iStartLine, str);
            CP.SetSelection(iStartLine, iStartColumn, iEndLine, iEndColumn);
            CP.Set_TopLine(iStartLine + CP.CountOfVisibleLines Div 2);
          End;
        WindowPosition['CodeFragments'] := R;
      End Else
        MessageDlg('There is no active Code Pane.', mtError, [mbOK], 0);
  Except
    On E: Exception Do
      DisplayException(E.Message);
  End;
end;

(**

  This method is an on click event handler for the Insert InSitu Comment menu.

  @precon  None.
  @postcon Inserts a documentation comment at the cursor position.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertInSituCommentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  iSL, iSC, iEL, iEC : Integer;
  strLine : String;
  CP: CodePane;

begin
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      CP.GetSelection(iSL, iSC, iEL, iEC);
      strLine := CP.CodeModule.Lines[iSL, iSL];
      strLine := Copy(strLine, 1, iSC - 1) + ''': ' +
        Copy(strLine, iSC, Length(strLine) - iSC + 1);
      CP.CodeModule.ReplaceLine(iSL, strLine);
      SelectionChange(iSL, iSC + 2, iSL, iSC + 2);
    End Else
      MessageDlg('There is no active Code Pane.', mtError, [mbOK], 0);
end;

(**

  This is an on click event handler for the insert line comment menu.

  @precon  None.
  @postcon Inserts a line comment above the current line at the current column
           position.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertLineCommentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  iSL, iSC, iEL, iEC : Integer;
  strLine : String;
  CP: CodePane;

begin
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      CP.GetSelection(iSL, iSC, iEL, iEC);
      strLine := StringOfChar(#32, iSC - 1) + ''': ';
      CP.CodeModule.InsertLines(iSL, strLine);
      SelectionChange(iSL, iSC + 2, iSL, iSC + 2);
    End Else
      MessageDlg('There is no active Code Pane.', mtError, [mbOK], 0);
end;

(**

  This is a menu OnClick event for the insertion of a method comment. This method
  searches the IDE for the current module being edited and then creates a
  memory stream of the source and passes it to the Unit parser. It then finds
  the first method declaration prior to the cursor position, parses the
  declaration and output the information in as comment immediately above the
  method declaration.

  @precon  Sender is the object initiating the event .
  @postcon Inserts a Method comment into the editor avoid the current method .

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertMethodCommentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  Module: TBaseLanguageModule;
  strFileName: String;
  T: TElementContainer;
  N: TGenericFunction;
  iIndent: Integer;
  CursorDelta: TPoint;
  strComment: String;
  iInsertLine: Integer;
  CP: CodePane;
  strCode : String;

begin
  iInsertLine := 0;
  CP := CurrentCodePane;
    If CP = Nil Then
      Exit;
  strCode := ModuleCode;
  strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
    CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
  Module := Dispatcher(strCode, strFileName,
    Not CP.CodeModule.Parent.Saved, [moParse]);
  If Module <> Nil Then
    Try
      T := Module.FindElement(strImplementedMethodsLabel);
      If T <> Nil Then
        Begin
          N := FindFunction(CursorPosition.Line, T, TGenericMethodDecl);
          If N <> Nil Then
            Begin
              If N.Comment <> Nil Then
                Begin
                  If MessageDlg(Format(strMethodAlreadyExists, [N.QualifiedName]),
                    mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
                    Exit;
                  CP.CodeModule.DeleteLines(N.Comment.Line,
                    N.Line - N.Comment.Line);
                  iInsertLine := N.Comment.Line;
                End;
              If iInsertLine = 0 Then
                iInsertLine := N.Line;
              iIndent := FindIndentOfFirstTokenOnLine(Module, N.Line) - 1;
              strComment := WriteComment(N, ctVBLine, iIndent, True, CursorDelta);
              // Remove last #13#10 - not required as the IDE adds them
              strComment := Copy(strComment, 1, Length(strComment) - 2);
              CP.CodeModule.InsertLines(iInsertLine, strComment);
              PositionCursorInFunction(CursorDelta, iInsertLine, iIndent,
                strComment);
            End Else
              MessageDlg(strNoMethodFound, mtWarning, [mbOK], 0);
        End;
    Finally
      Module.Free;
    End;
end;

(**

  This is a menu OnClick event for the insertion of a property comment. This
  method searches the IDE for the current module being edited and then creates a
  memory stream of the source and passes it to the Unit parser. It then finds
  the first property declaration prior to the cursor position, parses the
  declaration and output the information in as comment immediately above the
  method declaration.

  @precon  Sender is the object initiating the event .
  @postcon Inserts a Porperty comment into the editor avoid the current method .

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertPropertyCommentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  Module: TBaseLanguageModule;
  strFileName: String;
  T: TElementContainer;
  N: TGenericFunction;
  iInsertLine: Integer;
  iIndent: Integer;
  strComment: String;
  CursorDelta: TPoint;
  CP: CodePane;
  strCode : String;

begin
  iInsertLine := 0;
  CP := CurrentCodePane;
  If CP = Nil Then
    Exit;
  strCode := ModuleCode;
  strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
    CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
  Module := Dispatcher(strCode, strFileName,
    Not CP.CodeModule.Parent.Saved, [moParse]);
  If Module <> Nil Then
    Try
      T := Module.FindElement(strImplementedPropertiesLabel);
      If T <> Nil Then
        Begin
          N := FindFunction(CursorPosition.Line, T, TGenericProperty);
          If N <> Nil Then
            Begin
              If N.Comment <> Nil Then
                Begin
                  If MessageDlg(Format(strPropertyAlreadyExists, [N.QualifiedName]),
                    mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
                    Exit;
                  CP.CodeModule.DeleteLines(N.Comment.Line,
                    N.Line - N.Comment.Line);
                  iInsertLine := N.Comment.Line;
                End;
              If iInsertLine = 0 Then
                iInsertLine := N.Line;
              iIndent := FindIndentOfFirstTokenOnLine(Module, N.Line) - 1;
              strComment := WriteComment(N, ctVBLine, iIndent, True, CursorDelta);
              // Remove last #13#10 - not required as the IDE adds them
              strComment := Copy(strComment, 1, Length(strComment) - 2);
              CP.CodeModule.InsertLines(iInsertLine, strComment);
              PositionCursorInFunction(CursorDelta, iInsertLine, iIndent,
                strComment);
            End Else
              MessageDlg(strNoPropertyFound, mtWarning, [mbOK], 0);
        End;
    Finally
      Module.Free;
    End;
end;

(**

  This method loads the applications settings from the ini file.

  @precon  None.
  @postcon Loads the applications settings from the ini file.

**)
procedure TIDETools.LoadSettings;
begin
  With TIniFile.Create(BrowseAndDocItOptions.INIFileName) Do
    Try
      FMEVisible := ReadBool('ModuleExplorer', 'Visible', True);
      FDocType := TDocType(ReadInteger('Documentation options', 'LastOption',
        Byte(dtHTML)));
      FCodeFragWidth := ReadInteger('Setup', 'CodeFragWidth', 100);
    Finally
      Free;
    End;
end;

(**

  This method is an on click event handler for the Save Code Fragement menu item.

  @precon  None.
  @postcon Saves the currently selected text as a code fragment.

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.SaveCodeFragmentClick(const Ctrl: CommandBarButton;
  var CancelDefault: WordBool);

Var
  strFileName : String;
  sl : TStringList;
  iStartLine, iStartColumn, iEndLine, iEndColumn : Integer;
  CP: CodePane;

begin
  Try
    sl := TStringList.Create;
    Try
      CP := CurrentCodePane;
      If CP <> Nil Then
        Begin
          CP.GetSelection(iStartLine, iStartColumn, iEndLine, iEndColumn);
          If (iStartLine = iEndLine) And (iStartColumn = iEndColumn) Then
            Begin
              MessageDlg('You have not selected any code.', mtWarning, [mbOK], 0);
              Exit;
            End;
          sl.Text := CP.CodeModule.Lines[iStartLine, iEndLine - iStartLine + 1];
        End;
      If InputQuery('Save Fragement', 'Fragment Name', strFileName) Then
        If strFileName <> '' Then
          Begin
            strFileName := FPath + CleanFileName(strFileName) + '.txt';
            If FileExists(strFileName) Then
              If MessageDlg(Format(strFileExists, [
                ExtractFilename(ChangeFileExt(strFileName, ''))]), mtConfirmation,
                [mbYes, mbNo, mbCancel], 0) <> mrYes Then Exit;
            sl.SaveToFile(strFileName);
          End Else
            MessageDlg('The code fragment must have a name.', mtWarning, [mbOK], 0);
    Finally
      sl.Free;
    End;
  Except
    On E: Exception Do DisplayException(E.Message);
  End;
end;

(**

  This method returns filename, modified and code information back to the Browse
  And Doc It thread for processing.

  @precon  None.
  @postcon Returns filename, modified and code information back to the Browse 
           And Doc It thread for processing.

  @param   strFileName  as a String as a reference
  @param   boolModified as a Boolean as a reference
  @return  a String

**)
Function TIDETools.EditorInfo(var strFileName: String;
  var boolModified: Boolean) : String;

var
  CP: CodePane;

begin
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
        CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
      boolModified := Not CP.CodeModule.Parent.Saved;
      Result := ModuleCode;
    End;
end;

End.
