(**

  This module defines the VBE IDE Tools interface between the IDE and the
  available tools.

  @Version 3.285
  @Date    17 Sep 2023
  @Author  David Hoyle

**)
Unit BADI.IDETools;

Interface

uses
  System.SysUtils,
  System.IniFiles,
  System.Classes,
  System.Contnrs,
  System.Win.ComObj,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.ActnList,
  Office2000_TLB,
  VBIDE_TLB,
  EventSink,
  BADI.Documentation.Dispatcher,
  BADI.Base.Module,
  BADI.CommonIDEFunctions,
  BADI.Thread.Manager,
  BADI.VBEIDE.ActiveForm,
  {$IFDEF WIN32}
  BrowseAndDocItVBEIDE_TLB,
  {$ELSE}
  BrowseAndDocItVBEIDE64_TLB,
  {$ENDIF}
  BADI.ModuleExplorerFrame;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a record to keep together the menu item and the event sink. **)
  TIDEMenuItem = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMenu : CommandBarButton;
    FSink : TEventSink;
  Public
    constructor Create(Const ClickProc: TClickProc);
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
  Strict Private
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
    //FOldWndProc : TFarProc;
    //FNewWndProc : TFarProc;
    FLastCodePane: CodePane;
    FBADIThreadMgr : TBADIThreadManager;
    FModuleExplorerFrame : TframeModuleExplorer;
    FToolWindow : VBIDE_TLB.Window_;
    FToolWindowForm : TTBADIActiveXToolWndForm;
  Strict Protected
    procedure CreateMenu;
    function  GetProjectPath(Const strName: String): String;
    procedure SetProjectPath(Const strName: String; Const strValue: String);
    procedure SaveModules(Const Project: VBPRoject; Const slModules: TStringList);
    function  GetModule(Const strProject, strModule: String): VBComponent;
    function  GetWindowPosition(Const strName: String): TRect;
    procedure SetWindowPosition(Const strName: String; Const Value: TRect);
    function GetModuleCode: String;
    function GetCursorPosition: TEditPos;
    Function GetCodePane : CodePane;
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
    Procedure SelectionChange(Const iIdentLine, iIdentCol, iCommentLine : Integer);
    Procedure Focus(Sender : TObject);
    Procedure ScopeChange(Sender : TObject);
    Procedure TimerEvent(Sender : TObject);
    procedure VBProjectChangeEvent(Const Project: VBProject);
    procedure CodePaneChangeEvent(Const Pane : CodePane; Const Project : VBProject);
    Procedure Save;
    Function  GetFileName(Const strProject, strModule : String; Const iType : Integer) : String;
    procedure PositionCursorInFunction(Const CursorDelta: TPoint; Const iInsertLine: Integer;
      Const iIndent: Integer; Const strComment: string);
    Procedure SuccessfulParse(Const boolSuccessfulParse : Boolean);
    Function  EditorInfo(Var strFileName : String; var boolModified : Boolean) : String;
    Procedure RenderDocument(Const Module : TBaseLanguageModule);
    Procedure ExceptionMsg(Const strExceptionMsg : String);
    Procedure IdleTimerEvent(Sender : TObject);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure MEFormClose(Sender : TObject; var CloseAction : TCloseAction);
    Function  VBFileExists(Const strFileName : String) : VBComponent;
    Procedure ResizeEvent(Sender : TObject);
    //: @debug Procedure WndProc(var Msg : TMessage);
    (**
      This property reads and write the project paths to and from the registry.
      @precon  None.
      @postcon Reads and write the project paths to and from the registry.
      @param   strName as a String as a constant
      @return  a String
    **)
    Property ProjectPath[Const strName : String] : String Read GetProjectPath Write SetProjectPath;
    (**
      This property determines if the standard VBAProject should be ignored.
      @precon  None.
      @postcon Gets and sets whether the VBAProject should be ignored.
      @return  a Boolean
    **)
    Property IgnoreVBAProject : Boolean Read FIgnoreVBAProject Write FIgnoreVBAProject;
    (**
      This property attempts to get the specified module interface from the specified project.
      @precon  None.
      @postcon Attempts to get the specified module interface from the specified project.
      @param   strModule  as a String as a constant
      @param   strProject as a String as a constant
      @return  a VBComponent
    **)
    Property Modules[Const strProject, strModule : String] : VBComponent Read GetModule;
    (**
      This property allows a windows bounding rectangle to be read and written to the
      registry.
      @precon  None.
      @postcon Allows a windows bounding rectangle to be read and written to the
               registry.
      @param   strName as a String as a constant
      @return  a TRect
    **)
    Property WindowPosition[Const strName : String] : TRect Read GetWindowPosition
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
    Constructor Create(Const VBEIDERef : VBE);
    Destructor Destroy; Override;
    Procedure CreateModuleExplorer(Const ToolWindowForm : ITBADIActiveXToolWndForm;
      Const ToolWindow : VBIDE_TLB.Window_);
    Procedure DestroyModuleExplorer();
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
  (** This is the caption for the Insert In-Situ Comment Menu Item. **)
  strInsertInSituComment = 'Insert &In-Situ Comment';
  (** This is a message for checking out a single file. **)
  strLockedMsg = 'The module "%s" in project "%s" is read only and therefore probably ' +
    'locked by the Version Control System?'#13#13 +
    'Pressing CANCEL will leave the module open for viewing - DO NOT MAKE CHANGES.';
  (** This is an Exception message warning the specified file is read only. **)
  strFileReadOnly = 'The file "%s" is read only.';
  (** This is a message for a code fragment that already exists. **)
  strFileExists = 'The code fragment "%s" already exists. Do you want to overwrite this fragment?';
  (** A resource string to prompt for overwriting an imported file. **)
  strTheFileAlreadyExists = 'The file "%s" already exists in this project. ' +
  'Do you want to overwrite it?';

Implementation

uses
  System.Variants,
  System.Math,
  System.UITypes,
  Winapi.ShellAPI,
  Vcl.Controls,
  Vcl.FileCtrl,
  Vcl.Menus,
  ExportForm,
  BADI.ProgressForm,
  BADI.Functions,
  BADI.TokenForm,
  BADI.OptionsForm,
  BADI.DocumentationOptionsForm,
  CodeFragmentsForm,
  BADI.VB.Module,
  BADI.Options,
  BADI.Module.Dispatcher,
  BADI.Types,
  BADI.ElementContainer,
  BADI.Generic.FunctionDecl,
  BADI.ResourceStrings,
  BADI.Generic.MethodDecl,
  BADI.Generic.PropertyDecl,
  BADI.VB.ResourceStrings,
  CodeSiteLogging;


(**

  This is a constructor for the TIDEMEnuItem class.

  @precon  None.
  @postcon Creates an instance of a event sink class.

  @param   ClickProc as a TClickProc as a constant

**)
constructor TIDEMenuItem.Create(Const ClickProc: TClickProc);

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

(**

  This is the constructor method for the TIDETools class.

  @precon  None.
  @postcon Initialises the VBE IDE Tools class by:
             Opening the About Form
             Creating a message window.
             Creating the menu system.
             Creating and starting an event timer
             Loading the applications settings.

  @nocheck ExceptionEating

  @param   VBEIDERef as a VBE as a constant

**)
constructor TIDETools.Create(Const VBEIDERef : VBE);

Const
  iTimerInterval = 100;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.Create', tmoTiming);{$ENDIF}
  Try
    FOldHandle := Application.Handle;
    Application.Handle := VBEIDERef.MainWindow.HWnd;
    FPath := ExtractFilePath(TBADIOptions.BADIOptions.INIFileName) + 'Code Fragments\';
    System.SysUtils.ForceDirectories(FPath);
    FVBEIDE := VBEIDERef;
    FVBProject := Nil;
    FSinks := TObjectList.Create(True);
    CreateMenu;
    FBADIThreadMgr := TBADIThreadManager.Create(SuccessfulParse, RenderDocument, ExceptionMsg);
    FTimer := TTimer.Create(Nil);
    FTimer.Interval := iTimerInterval;
    FTimer.OnTimer := TimerEvent;
    FCounter := 0;
    FVBEIDE.MainWindow.SetFocus;
    LoadSettings;
    FIdleTimer := TTimer.Create(Nil);
    FIdleTimer.Interval := iTimerInterval;
    FIdleTimer.OnTimer := IdleTimerEvent;
    //: @debug FOldWndProc := TFarProc(GetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC));
    //: FNewWndProc := Classes.MakeObjectInstance(WndProc);
    //: SetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC, LongWord(FNewWndProc));
  Except
    On E : Exception Do DisplayException(E);
  End;
end;

(**

  This is the destructor method for the TIDETools class.

  @precon  None.
  @postcon Frees all memory allocated by the class and removes the tools menu.

**)
destructor TIDETools.Destroy;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.Destroy', tmoTiming);{$ENDIF}
  SaveSettings;
  //: @debug Check that our WndProc is the current WndProc before removing.
  //: If TFarProc(GetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC)) = FNewWndProc Then
  //:   SetWindowLong(FVBEIDE.MainWindow.HWnd, GWL_WNDPROC, LongWord(FOldWndProc));
  //: Classes.FreeObjectInstance(FNewWndProc);
  FActions.Free;
  FIdleTimer.Free;
  FTimer.Free;
  FBADIThreadMgr.Free;
  FSinks.Free;
  If (FVBEIDE <> Nil) And (FIDEMenu <> Nil) Then
    FVBEIDE.CommandBars.Item['Menu Bar'].Controls_[strMenuCaption].Delete(False);
  Application.Handle := FOldHandle;
  inherited Destroy;
end;

Procedure TIDETools.DestroyModuleExplorer();

Begin
  FModuleExplorerFrame.Free;
End;

(**

  This method saves the applications settings to the INI file.

  @precon  None.
  @postcon Saves the applications settings to the INI file.

**)
procedure TIDETools.SaveSettings;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SaveSettings', tmoTiming);{$ENDIF}
  with TIniFile.Create(TBADIOPtions.BADIOptions.INIFileName) do
    try
      WriteBool('ModuleExplorer', 'Visible', FToolWindow.Visible);
      WriteInteger('Documentation options', 'LastOption', Byte(FDocType));
      WriteInteger('Setup', 'CodeFragWidth', FCodeFragWidth);
    finally
      Free;
    end;
end;

(**

  This method positions the comment and function according to the options and then places the cursor in 
  the appropriate position for editing.

  @precon  None.
  @postcon Positions the comment and function according to the options and then places the cursor in the
           appropriate position for editing.

  @param   CursorDelta as a TPoint as a constant
  @param   iInsertLine as an Integer as a constant
  @param   iIndent     as an Integer as a constant
  @param   strComment  as a string as a constant

**)
procedure TIDETools.PositionCursorInFunction(Const CursorDelta: TPoint;
  Const iInsertLine: Integer; Const iIndent: Integer; Const strComment: string);

Var
  Pt: TPoint;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.PositionCursorInFunction', tmoTiming);{$ENDIF}
  SelectionChange(iInsertLine + CharCount(#13, strComment) + 1, 1, iInsertLine);
  Pt.Y := iInsertLine;
  Pt.X := iIndent + 3;
  Inc(Pt.Y, CursorDelta.Y);
  Inc(Pt.X, CursorDelta.X);
  CP := CurrentCodePane;
  If CP <> Nil Then
    CP.SetSelection(Pt.Y, Pt.X, Pt.Y, Pt.X);
end;

(**

  This method creates the menus required by the tools add-in.

  @precon  None.
  @postcon Creates all the menus required by the tools add-in.

**)
Procedure TIDETools.CreateMenu;

  (**

    This method creates a sub menu item to the main tools menu, sets the caption, creates an event sink 
    and hooks an event handler for the menu.

    @precon  None.
    @postcon Creates a sub menu item to the main tools menu, sets the caption, creates an event sink and
             hooks an event handler for the menu.

    @nohints strShortcut

    @param   strCaption     as a String as a constant
    @param   EventHandler   as a TClickProc as a constant
    @param   boolBeginGroup as a Boolean as a constant
    @param   strShortCut    as a String as a constant

  **)
  Procedure CreateMenuItem(Const strCaption : String; Const EventHandler : TClickProc;
    Const boolBeginGroup : Boolean; Const strShortCut : String = '');

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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.CreateMenu', tmoTiming);{$ENDIF}
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
      CreateMenuItem(strImport, ImportClick, False, 'R');
      CreateMenuItem(strSaveCodeFragment, SaveCodeFragmentClick, True, 'S');
      CreateMenuItem(strInsertCodeFragment,InsertCodeFragmentClick, False, 'N');
      CreateMenuItem(strOptions, OptionsClick, True, 'O');
    End;
End;

Procedure TIDETools.CreateModuleExplorer(Const ToolWindowForm : ITBADIActiveXToolWndForm;
  Const ToolWindow : VBIDE_TLB.Window_);

Begin
  FToolWindow := ToolWindow;
  FToolwindow.Visible := FMEVisible;
  FToolWindowForm := (ToolWindowForm.VCLFormRef As TTBADIActiveXToolWndForm);
  FModuleExplorerFrame := TframeModuleExplorer.Create(FToolWindowForm);
  FModuleExplorerFrame.Parent := FToolWindowForm;
  FModuleExplorerFrame.Align := alClient;
  FModuleExplorerFrame.OnSelectionChange := SelectionChange;
  FModuleExplorerFrame.OnFocus := Focus;
  FModuleExplorerFrame.OnRefresh := ScopeChange;
End;

(**

  This is a timer event handler.

  @precon  None.
  @postcon Used to create events for the changing of the current VB Project and the
           Current Code Pane because I can not hook the events in the IDE - not sure you
           can in the VBE IDE.

  @nocheck ExceptionEating

  @param   Sender as a TObject

**)
procedure TIDETools.TimerEvent(Sender: TObject);

Var
  i : Integer;
  boolRefresh : Boolean;
  CP: CodePane;

begin
  Try
    If Not FModuleExplorerFrame.Visible Then
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
    boolRefresh := (FCounter >= TBADIOPtions.BADIOptions.UpdateInterval) And
      (FCounter <= TBADIOPtions.BADIOptions.UpdateInterval + 100);
    i := Length(ModuleCode);
    If i <> FOldLength Then
      Begin
        FCounter := 0;
        FOldLength := i;
      End;
    If boolRefresh Then
      Begin
        FTimer.Enabled := False;
        FBADIThreadMgr.Parse(EditorInfo);
        FOldLength := Length(ModuleCode);
      End;
  Except
    On E: Exception Do
      DisplayException(E);
  End;
end;

(**

  This is a getter method for the ProjectPath property.

  @precon  None.
  @postcon Gets the project path for the given project name.

  @param   strName as a String as a constant
  @return  a String

**)
function TIDETools.GetProjectPath(Const strName: String): String;

Const
  strProjectPaths = 'ProjectPaths';

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.GetProjectPath', tmoTiming);{$ENDIF}
  With TIniFile.Create(TBADIOPtions.BADIOptions.INIFileName) Do
    Try
      Result := ReadString(strProjectPaths, strName, '');
    Finally
      Free;
    End;
end;

(**

  This is a setter method for the ProjectPath property.

  @precon  None.
  @postcon Sets the project path for the given project name.

  @param   strName  as a String as a constant
  @param   strValue as a String as a constant

**)
procedure TIDETools.SetProjectPath(Const strName : String; Const strValue: String);

Const
  strProjectPaths = 'ProjectPaths';

Var
  iniFile: TMemIniFile;
  strPath : String;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SetProjectPath', tmoTiming);{$ENDIF}
  strPath := strValue;
  If strPath[Length(strPath)] <> '\' Then
    strPath := strPath + '\';
  iniFile := TMemIniFile.Create(TBADIOPtions.BADIOptions.INIFileName);
    Try
    iniFile.WriteString(strProjectPaths, strName, strPath);
    iniFile.UpdateFile;
    Finally
    iniFile.Free;
    End;
end;

(**

  This is a getter method for the Module property.

  @precon  None.
  @postcon Gets the module reference for the given project and module name else returns nil is they were
           not found.

  @param   strProject as a String as a constant
  @param   strModule  as a String as a constant
  @return  a VBComponent

**)
function TIDETools.GetModule(Const strProject, strModule: String): VBComponent;

Var
  i, j : Integer;
  Project : VBProject;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.GetModule', tmoTiming);{$ENDIF}
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
  DisplayException('The module "%s" was not found in project "%s".', [strModule, strProject]);
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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.GetCursorPosition', tmoTiming);{$ENDIF}
  CP := CurrentCodePane;
  If CP <> Nil Then
    CP.GetSelection(Result.Line, Result.Col, i, j);
end;

(**

  This is a getter method for the FileName property.

  @precon  None.
  @postcon Returns the full path for the project module given its type.

  @param   strProject as a String as a constant
  @param   strModule  as a String as a constant
  @param   iType      as an Integer as a constant
  @return  a String

**)
function TIDETools.GetFileName(Const strProject, strModule: String; Const iType: Integer): String;
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.GetFileName', tmoTiming);{$ENDIF}
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

  @param   strName as a String as a constant
  @return  a TRect

**)
function TIDETools.GetWindowPosition(Const strName: String): TRect;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.GetWindowPosition', tmoTiming);{$ENDIF}
  With TIniFile.Create(TBADIOPtions.BADIOptions.INIFileName) do
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

  @param   strName as a String as a constant
  @param   Value   as a TRect as a constant

**)
procedure TIDETools.SetWindowPosition(Const strName: String; Const Value: TRect);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SetWindowPosition', tmoTiming);{$ENDIF}
  With TIniFile.Create(TBADIOPtions.BADIOptions.INIFileName) do
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

  @nocheck ExceptionEating
  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.ShowTokensClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  doc : TBaseLanguageModule;
  strFileName : String;
  strTemp : String;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.ShowTokensClick', tmoTiming);{$ENDIF}
  Try
    CP := CurrentCodePane;
    If CP = Nil Then
      Exit;
    strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
      CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
    strTemp := ModuleCode;
    doc := TBADIDispatcher.BADIDispatcher.Dispatcher(
      strTemp,
      strFileName,
      Not CP.CodeModule.Parent.Saved,
      [moParse]
    );
    Try
      TfrmTokenForm.Execute(doc);
    Finally
      doc.Free;
    End;
  Except
    On E: Exception Do
      DisplayException(E);
  End;
end;

(**

  This method re-enables the timer for the parsing of code once the parser has
  parsed the code successfully.

  @precon  None.
  @postcon Re-enables the timer for the parsing of code once the parser has
           parsed the code successfully.

  @param   boolSuccessfulParse as a Boolean as a constant

**)
procedure TIDETools.SuccessfulParse(Const boolSuccessfulParse: Boolean);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SuccessfulParse', tmoTiming);{$ENDIF}
  FTimer.Enabled := True;
end;

(**

  This method outputs an exception message from the browse and doc it thread.

  @precon  None.
  @postcon Outputs an exception message from the browse and doc it thread.

  @param   strExceptionMsg as a String as a constant

**)
procedure TIDETools.ExceptionMsg(Const strExceptionMsg: String);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.ExceptionMsg', tmoTiming);{$ENDIF}
  ShowMessage(strExceptionMsg);
end;

(**

  This is a button on click event handler for the Export button.

  @precon  None.
  @postcon Displays the export dialogue. If the dialogue is not cancelled then
           the selected modules from the current project are exported to the
           selected path.

  @nocheck ExceptionEating
  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.ExportClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.ExportClick', tmoTiming);{$ENDIF}
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
              slModules.AddObject(Format('%s=%d', [Module.Name, Module.Type_]), TObject(Byte(State)));
            End;
          strPath := ProjectPath[vbp.Name];
          If strPath = '' Then strPath := GetCurrentDir;
          R := WindowPosition['Export'];
          If TfrmExport.Execute('Export Modules', vbp.Name, slModules, strPath, R) Then
            Begin
              WindowPosition['Export'] := R;
              ProjectPath[vbp.Name] := strPath;
              SaveModules(vbp, slModules);
              FVBEIDE.ActiveVBProject := vbp;
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

  @param   Project   as a VBPRoject as a constant
  @param   slModules as a TStringList as a constant

**)
Procedure TIDETools.SaveModules(Const Project : VBPRoject; Const slModules : TStringList);

Var
  i : Integer;
  Module : VBComponent;
  strFileName : String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SaveModules', tmoTiming);{$ENDIF}
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
              DisplayException('Module "%s" was not found in Project "%s".', [slModules[i], Project.Name]);
        End;
    Finally
      Free;
    End;
End;

(**

  This event handler refreshes the tree view if the scope list changes.

  @precon  None.
  @postcon Refreshes the tree view if the scope list changes.

  @param   Sender as a TObject

**)
procedure TIDETools.ScopeChange(Sender: TObject);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.ScopeChange', tmoTiming);{$ENDIF}
  FCounter := TBADIOPtions.BADIOptions.UpdateInterval;
end;

(**

  This method import a set of files into the current project creating or replace code
  as necessary.

  @precon  None.
  @postcon Import a set of files into the current project creating or replace code
           as necessary.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference
**)
procedure TIDETools.ImportClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  dlg : TOpenDialog;
  iFile: Integer;
  vbc: VBComponent;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.ImportClick', tmoTiming);{$ENDIF}
  If FVBEIDE.ActiveVBProject = Nil Then
    Exit;
  FVBProject := FVBEIDE.ActiveVBProject;
  dlg := TOpenDialog.Create(Nil);
  Try
    dlg.Filter := 'Visual Basic Files (*.cls;*.bas;*.frm)|*.cls;*.bas;*.frm|All Files|*.*';
    dlg.FilterIndex := 0;
    dlg.Options := dlg.Options + [ofFileMustExist, ofAllowMultiSelect];
    If dlg.Execute Then
      Begin
        For iFile := 0 To dlg.Files.Count - 1 Do
          Begin
            vbc := VBFileExists(dlg.Files[iFile]);
            If vbc <> Nil Then
              Case MessageDlg(Format(strTheFileAlreadyExists, [dlg.Files[iFile]]),
                mtConfirmation, [mbYes, mbNo, mbCancel], 0) Of
                mrYes   : FVBEIDE.ActiveVBProject.VBComponents.Remove(vbc);
                mrNo    : Break;
                mrCancel: Exit;
              End;
            vbc := FVBEIDE.ActiveVBProject.VBComponents.Import(dlg.Files[iFile]);
            vbc.Activate;
          End;
      End;
  Finally
    dlg.Free;
  End;
end;

(**

  This is an event handler that is fired when ever the current code pane changes.

  @precon  None.
  @postcon Contains code that needs to be processed when the current code pane changes.

  @nohints Project

  @param   Pane    as a CodePane as a constant
  @param   Project as a VBProject as a constant

**)
procedure TIDETools.CodePaneChangeEvent(Const Pane : CodePane; Const Project : VBProject);

Var
  strFileName : String;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.CodePaneChangeEvent', tmoTiming);{$ENDIF}
  FCounter := TBADIOPtions.BADIOptions.UpdateInterval;
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

  This method returns the VB component interface for the named file if found in the active VB Project 
  else returns nil.

  @precon  None.
  @postcon Returns the VB component interface for the named file if found in the active VB Project else 
           returns nil.

  @param   strFileName as a String as a constant
  @return  a VBComponent

**)
function TIDETools.VBFileExists(Const strFileName: String): VBComponent;

Var
  iFile : Integer;
  strVBCName : String;
  strLFileName : String;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.VBFileExists', tmoTiming);{$ENDIF}
  Result := Nil;
  If FVBProject = Nil Then
    Exit;
  strLFileName := strFileName;
  strLFileName := ChangeFileExt(ExtractFileName(strLFileName), '');
  For iFile := 1 To FVBProject.VBComponents.Count Do
    Begin
      strVBCName := FVBProject.VBComponents.Item(iFile).Name;
      If CompareText(strLFileName, strVBCName) = 0 Then
        Begin
          Result := FVBProject.VBComponents.Item(iFile);
          Exit;
        End;
    End;
end;

(**

  This is an event handler that is fired when ever the current VB project changes.

  @precon  None.
  @postcon Contains code that needs to be processed when the current VB Project changes.

  @param   Project as a VBProject as a constant

**)
procedure TIDETools.VBProjectChangeEvent(Const Project: VBProject);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.VBProjectChangeEvent', tmoTiming);{$ENDIF}
  If Project = Nil Then Exit;
  If IgnoreVBAProject And (Project.Name = 'VBAProject') Then
    Exit;
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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.Save', tmoTiming);{$ENDIF}
  MainMenu := FVBEIDE.CommandBars.Item['Menu Bar'];
  For i := 1 To MainMenu.Controls_.Count Do
    If MainMenu.Controls_.Item[i].Caption = '&File' Then
      Begin
        FileMenu := MainMenu.Controls_.Item[i] As CommandBarPopup;
        For j := 1 To FileMenu.Controls_.Count Do
          If Copy(FileMenu.Controls_.Item[j].Caption, 1, 5) = '&Save' Then
            Begin
            FileMenu.Controls_.Item[j].Execute;
              Exit;
            End;
      End;
end;

(**

  This is an on option click event handler.

  @precon  None.
  @postcon Displays the options dialogue.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.OptionsClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.OptionsClick', tmoTiming);{$ENDIF}
  If TfrmOptions.Execute([Low(TVisibleTab)..High(TVisibleTab)]) Then
    Begin
      FOldLength := 0;
      FCounter := TBADIOPtions.BADIOptions.UpdateInterval;
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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.Focus', tmoTiming);{$ENDIF}
  CP := CurrentCodePane;
  If CP = Nil Then
    Exit;
  CP.Show;
  CP.Window_.SetFocus;
end;

(**

  This method focuses the editor window when this menu is selected.

  @precon  None.
  @postcon Focuses the editor window when this menu is selected.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.FocusEditorClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.FocusEditorClick', tmoTiming);{$ENDIF}
  Focus(Self);
end;

(**

  This is an on selection change event handler for the module explorer.

  @precon  None.
  @postcon Positions the cursor in the current code window based on the browsing
           options.

  @param   iIdentLine   as an Integer as a constant
  @param   iIdentCol    as an Integer as a constant
  @param   iCommentLine as an Integer as a constant

**)
procedure TIDETools.SelectionChange(Const iIdentLine, iIdentCol, iCommentLine : Integer);

Var
  C : TEditPos;
  iLineInView: Integer;
  CP: CodePane;
  iLCommentLine : Integer;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SelectionChange', tmoTiming);{$ENDIF}
  Focus(Self);
  CP := CurrentCodePane;
  If CP <> Nil Then
    If iIdentCol * iIdentLine > 0 Then
      Begin
        iLineInView := CP.CountOfVisibleLines Div 2;
        C.Col := iIdentCol;
        C.Line := iIdentLine;
        CP.SetSelection(C.Line, C.Col, C.Line, C.Col);
        iLCommentLine := iCommentLine;
        If iLCommentLine = 0 Then
          iLCommentLine := iIdentLine;
        Case TBADIOPtions.BADIOptions.BrowsePosition Of
          bpCommentTop:
            CP.TopLine := iLCommentLine;
          bpCommentCentre:
            CP.TopLine := Max(iLCommentLine - iLineInView, 1);
          bpIdentifierTop:
            CP.TopLine := iIdentLine;
          bpIdentifierCentre:
            CP.TopLine := Max(iIdentLine - iLineInView, 1);
          bpIdentifierCentreShowAllComment:
            Begin
              CP.TopLine := Max(iIdentLine - iLineInView, 1);
              If iLCommentLine > 0 Then
                If iLCommentLine < CP.TopLine Then
                  CP.TopLine := iLCommentLine;
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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.MEFormClose', tmoTiming);{$ENDIF}
  FMEVisible := False;
end;

(**

  This is an on click event handler for the module explorer menu option.

  @precon  None.
  @postcon Shows the module explorer and refreshes the tree view.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.ModuleExplorerClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.ModuleExplorerClick', tmoTiming);{$ENDIF}
  FToolWindow.Visible := True;
  FToolWindow.SetFocus;
  FModuleExplorerFrame.SetFocus;
  FCounter := TBADIOptions.BADIOptions.UpdateInterval;
  FToolWindowForm.UpdateBounds;
end;

(**

  This method renders the passed module in the module explorer.

  @precon  None.
  @postcon Renders the passed module in the module explorer.

  @param   Module as a TBaseLanguageModule as a constant

**)
procedure TIDETools.RenderDocument(Const Module: TBaseLanguageModule);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.RenderDocument', tmoTiming);{$ENDIF}
  FModuleExplorerFrame.RenderModule(Module);
end;

Procedure TIDETools.ResizeEvent(Sender: TObject);

Begin
  CodeSite.Send('Resize');
End;

(**

  This is an on click event handler for the Documentation menu.

  @precon  None.
  @postcon Invokes the dialogue to document the code in the current project.

  @nocheck ExceptionEating
  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.DocumentationClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

var
  i: Integer;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.DocumentationClick', tmoTiming);{$ENDIF}
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
    On E: Exception Do DisplayException(E);
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
  If FVBEIDE.MainWindow.Visible And (
    (recMainWndInfo.dwOtherStuff And WS_ACTIVECAPTION > 0) Or
    (recModExplWndInfo.dwOtherStuff And WS_ACTIVECAPTION > 0)) Then
    Application.DoApplicationIdle;
end;

(**

  This is an on click event handler for the insert block comment menu.

  @precon  None.
  @postcon Inserts a block documentation comment at the current line.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertBlockCommentClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  iSL, iSC, iEL, iEC : Integer;
  strLine : String;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.InsertBlockCommentClick', tmoTiming);{$ENDIF}
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      CP.GetSelection(iSL, iSC, iEL, iEC);
      strLine :=
        StringOfChar(#32, iSC - 1) + ''':'#13#10 +
        StringOfChar(#32, iSC - 1) + ''':'#13#10 +
        StringOfChar(#32, iSC - 1) + ''':';
      CP.CodeModule.InsertLines(iSL, strLine);
      SelectionChange(iSL + 1, iSC + 2, iSL + 1);
    End Else
      MessageDlg('There is no active Code Pane.', mtError, [mbOK], 0);
end;

(**

  This is an on click event handler for the insert code fragment event handler.

  @precon  None.
  @postcon Inserts the selected code fragment into the current code pane at the
           cursor position.

  @nocheck ExceptionEating

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertCodeFragmentClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  R : TRect;
  str : String;
  iStartLine, iStartColumn, iEndLine, iEndColumn : Integer;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.InsertCodeFragmentClick', tmoTiming);{$ENDIF}
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
      DisplayException(E);
  End;
end;

(**

  This method is an on click event handler for the Insert In-Situ Comment menu.

  @precon  None.
  @postcon Inserts a documentation comment at the cursor position.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertInSituCommentClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  iSL, iSC, iEL, iEC : Integer;
  strLine : String;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.InsertInSituCommentClick', tmoTiming);{$ENDIF}
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      CP.GetSelection(iSL, iSC, iEL, iEC);
      strLine := CP.CodeModule.Lines[iSL, iSL];
      strLine := Copy(strLine, 1, iSC - 1) + ''': ' +
        Copy(strLine, iSC, Length(strLine) - iSC + 1);
      CP.CodeModule.ReplaceLine(iSL, strLine);
      SelectionChange(iSL, iSC + 2, iSL);
    End Else
      MessageDlg('There is no active Code Pane.', mtError, [mbOK], 0);
end;

(**

  This is an on click event handler for the insert line comment menu.

  @precon  None.
  @postcon Inserts a line comment above the current line at the current column
           position.

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertLineCommentClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  iSL, iSC, iEL, iEC : Integer;
  strLine : String;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.InsertLineCommentClick', tmoTiming);{$ENDIF}
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      CP.GetSelection(iSL, iSC, iEL, iEC);
      strLine := StringOfChar(#32, iSC - 1) + ''': ';
      CP.CodeModule.InsertLines(iSL, strLine);
      SelectionChange(iSL, iSC + 2, iSL);
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

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.InsertMethodCommentClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.InsertMethodCommentClick', tmoTiming);{$ENDIF}
  iInsertLine := 0;
  CP := CurrentCodePane;
    If CP = Nil Then
      Exit;
  strCode := ModuleCode;
  strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
    CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
  Module := TBADIDispatcher.BADIDispatcher.Dispatcher(strCode, strFileName,
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
              strComment := WriteComment(N, ctVBLine, iIndent, True, CursorDelta, 80);
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
  @postcon Inserts a Property comment into the editor avoid the current method .

  @nohint  Ctrl CancelDefault

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
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.InsertPropertyCommentClick', tmoTiming);{$ENDIF}
  iInsertLine := 0;
  CP := CurrentCodePane;
  If CP = Nil Then
    Exit;
  strCode := ModuleCode;
  strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
    CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
  Module := TBADIDispatcher.BADIDispatcher.Dispatcher(strCode, strFileName,
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
              strComment := WriteComment(N, ctVBLine, iIndent, True, CursorDelta, 80);
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

  This method loads the applications settings from the INI file.

  @precon  None.
  @postcon Loads the applications settings from the INI file.

**)
procedure TIDETools.LoadSettings;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.LoadSettings', tmoTiming);{$ENDIF}
  With TIniFile.Create(TBADIOptions.BADIOptions.INIFileName) Do
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

  This method is an on click event handler for the Save Code Fragment menu item.

  @precon  None.
  @postcon Saves the currently selected text as a code fragment.

  @nocheck ExceptionEating

  @nohint  Ctrl CancelDefault

  @param   Ctrl          as a CommandBarButton as a constant
  @param   CancelDefault as a WordBool as a reference

**)
procedure TIDETools.SaveCodeFragmentClick(const Ctrl: CommandBarButton; var CancelDefault: WordBool);

Var
  strFileName : String;
  sl : TStringList;
  iStartLine, iStartColumn, iEndLine, iEndColumn : Integer;
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.SaveCodeFragmentClick', tmoTiming);{$ENDIF}
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
    On E: Exception Do DisplayException(E);
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
Function TIDETools.EditorInfo(var strFileName: String; var boolModified: Boolean) : String;

var
  CP: CodePane;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TIDETools.EditorInfo', tmoTiming);{$ENDIF}
  Result := '';
  strFileName := '';
  boolModified := False;
  CP := CurrentCodePane;
  If CP <> Nil Then
    Begin
      strFileName := GetFileName(CP.CodeModule.Parent.Collection.Parent.Name,
        CP.CodeModule.Parent.Name, CP.CodeModule.Parent.Type_);
      boolModified := Not CP.CodeModule.Parent.Saved;
      Result := ModuleCode;
    End;
end;

(** @debug

  This is a Windows procedure for handling keyboard shortcuts in the IDE.

  @precon  None.
  @postcon

  @param   Msg as a TMessage as a reference


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
end;**)

End.
