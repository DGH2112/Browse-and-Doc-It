(**

  This module contains the packages main wizard interface.

  @Author  David Hoyle
  @Date    23 Aug 2008
  @Version 1.0

**)
Unit BrowseAndDocItWizard;

Interface

Uses
  Classes, ToolsAPI, Menus, ExtCtrls, BaseLanguageModule, ModuleExplorerFrame,
  DockForm;

Type
  (** This emunerate descibed the different types of doc comment .**)
  TCommentType = (ctBlock, ctLine, ctInSitu);

  (** This is the class which defined the Wizard interface. **)
  TBrowseAndDocItWizard = Class(TNotifierObject, IOTAWizard{, IOTAIDENotifier})
  Private
    mmiPascalDocMenu : TMenuItem;
    FCounter : Integer;
    FFileName : String;
    FKeyBinding : Integer;
    procedure InsertCommentBlock(CommentType: TCommentType);
    procedure OptionsClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer; SelectType : TSelectType);
    Procedure Focus(Sender : TObject);
    Procedure OptionsChange(Sender : TObject);
    function GetMethodDescription(Method : TGenericMethodDecl; AComment : TComment;
      iIndent : Integer) : String;
    Function WriteMethodComment(Method : TGenericMethodDecl;
      Source : IOTASourceEditor; Writer : IOTAEditWriter; AComment : TComment) : TOTAEditPos;
    Function WritePropertyComment(Prop : TGenericProperty;
      Source : IOTASourceEditor; Writer : IOTAEditWriter; AComment : TComment) : TOTAEditPos;
    procedure CreateMenuItem(mmiParent: TMenuItem;  strCaption: String = '';
      ClickProc: TNotifyEvent = Nil; AShortCut : TShortCut = 0);
    Procedure InsertMethodCommentClick(Sender : TObject);
    Procedure InsertPropertyCommentClick(Sender : TObject);
    Procedure InsertBlockCommentClick(Sender : TObject);
    Procedure InsertLineCommentClick(Sender : TObject);
    Procedure InsertInSituCommentClick(Sender : TObject);
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
    Function OutputTag(iIndent : Integer; Tag : TTag) : String;
    function GetPropertyDescription(Prop: TGenericProperty;
      AComment: TComment; iIndent: Integer): String;
    { IOTAIDENotifier
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified; }
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

  (** This class handles notifications from the editor so that changes in the
      editor can be displayed. **)
  TEditorNotifier = Class(TNotifierObject, INTAEditServicesNotifier)
  Private
    FUpdateTimer : TTimer;
    FLastEditorName : String;
    FLastCursorPos: TOTAEditPos;
    Procedure RefreshTree;
    Procedure TimerEventHandler(Sender : TObject);
    Procedure DetermineCompilerDefinitions(slDefines : TStringList);
  Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  End;

  (** This class represents a key binding notifier for installing and handling
      key bindings for this plugin. **)
  TKeyboardBinding = Class(TNotifierObject, IOTAKeyboardBinding)
  Private
    FWizard : TBrowseAndDocItWizard;
    Procedure FocusModuleExplorer(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure InsertMethodComment(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure InsertPropertyComment(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure InsertBlockComment(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure InsertLineComment(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure InsertInSituComment(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    Procedure ShowTokens(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  Protected
  Public
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
  PsAPI;

Resourcestring
  (** This is a text string of revision from nil and a to z. **)
  strRevision = ' abcdefghijklmnopqrstuvwxyz';
  (** This is a message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Borland Developer Studio 2006';
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'Open Source Freeware by David Hoyle (Build %d.%d.%d.%d)';
  (** This is a message for no methods to comment. **)
  strNoMethodFound = 'No method found on or above the current cursor position.';
  (** This is a message to confirm you wish to update the current comment. **)
  strMethodAlreadyExists = 'The method "%s" already has a comment. Do you' +
  ' want to update the comment with revised parameters and returns?';
  (** This is a message for no property to comment. **)
  strNoPropertyFound = 'No property found on or above the current cursor position.';
  (** This is a message to confirm you wish to update the current comment. **)
  strPropertyAlreadyExists = 'The property "%s" already has a comment. Do you' +
  ' want to continue?';

Const
  (** A simple array for outputting a or an. **)
  strAOrAn : Array[False..True] Of String = ('a', 'an');
  (** An array of parameter modifier phases. **)
  strModifier : Array[pamNone..pamOut] Of String = ('', ' as a reference',
    ' constant', ' as out');
  (** A list of vowels. **)
  strVowels : Set Of Char = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'];
  (** A constant array of outputs for the ArrayOf property. **)
  strArrayOf : Array[False..True] Of String = ('', 'Array Of ');

Var
  (** This is an index for the wizard when register with the ide. Its required
      in order to remove it from memory. **)
  iWizardIndex : Integer;
  (** This is an index for the editor notifier required when the package is
      unloaded **)
  iEditorIndex : Integer;
  {iIDENotifier : Integer;}
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
  TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
  Wizard := TBrowseAndDocItWizard.Create;
  iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(Wizard);
  {iIDENotifier := (BorlandIDEServices As IOTAServices).AddNotifier(Wizard);}
  iEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(
    TEditorNotifier.Create);
  iKeyBinding := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
    TKeyboardBinding.Create(Wizard))
End;

{procedure TBrowseAndDocItWizard.AfterCompile(Succeeded: Boolean);
begin
  boolCompiling := False;
end;

procedure TBrowseAndDocItWizard.AfterSave;
begin
end;

procedure TBrowseAndDocItWizard.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  boolCompiling := True;
end;

procedure TBrowseAndDocItWizard.BeforeSave;
begin
end;}

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
  With mmiPascalDocMenu Do
    Caption := '&Browse and Doc It';
  mmiMainMenu.Items.Insert(mmiMainMenu.Items.Count - 2, mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, 'Module &Explorer', ModuleExplorerClick,
    Menus.ShortCut(13, [ssCtrl, ssShift, ssAlt]));
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Method Comment', InsertMethodCommentClick);
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Property Comment', InsertPropertyCommentClick);
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Comment Block', InsertBlockCommentClick);
  CreateMenuItem(mmiPascalDocMenu, 'Insert &Line Comment', InsertLineCommentClick);
  CreateMenuItem(mmiPascalDocMenu, 'Insert &In-Situ Comment', InsertInSituCommentClick);
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, '&Options', OptionsClick);
  CreateMenuItem(mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, '&Help', HelpClick);
  FKeyBinding := 0;
  FCounter := 0;
  FFileName := '';
End;

(**


  This method creates menu items using the passed information.


  @precon  mmiParent must be a valid parent menu item in the IDE.

  @postcon A Sub menu ite is created under mmiParent.


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

  This is the destructor method for the TBrowseAndDocItWizard class.

  @precon  None.
  @postcon Saves the wizards settings and frees memory for interval structures.

**)
destructor TBrowseAndDocItWizard.Destroy;
begin
  If mmiPascalDocMenu <> Nil Then
    mmiPascalDocMenu.Free;
  Inherited;
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
end;

{procedure TBrowseAndDocItWizard.Destroyed;
begin
end;}

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

{procedure TBrowseAndDocItWizard.Modified;
begin
end;}

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
          Writer.Insert('(**');
          Writer.Insert(#10#13);
          Writer.Insert(PChar(StringOfChar(#32, iLen - 1) + '  '#10#13));
          Writer.Insert(PChar(StringOfChar(#32, iLen - 1) + '  '#10#13));
          Writer.Insert(PChar(StringOfChar(#32, iLen - 1) + '  '#10#13));
          Writer.Insert(PChar(StringOfChar(#32, iLen - 1)));
          Writer.Insert(PChar('**)'#10#13));
          Writer.Insert(PChar(StringOfChar(#32, iLen - 1)));
        End;
      ctLine :
        Begin
          Writer.Insert('(**');
          Writer.Insert(#32#32);
          Writer.Insert(PChar('**)'#10#13));
          Writer.Insert(PChar(StringOfChar(#32, iLen - 1)));
        End;
      ctInSitu :
        Begin
          Writer.Insert('(**  **) ');
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
            EditPos.Col, stIdentifier);
      Else
        SelectionChange(EditPos.Line + 1, EditPos.Col, EditPos.Line + 1,
          EditPos.Col,  stIdentifier);
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

  This is a menu OnClick event for the insertion of a method comment. This
  method searches the IDE for the current module being edited and then
  creates a memory stream of the source and passes it to the Unit parser.

  It then finds the first method declaration prior to the cursor position,
  parses the declaration and output the information in as comment immediately
  above the method declaration.

  This comment block starts with '(**' to signify an ObjectPascalDoc comment
  that can be used by the documentation system.

  @precon  Sender is the object initiating the event.
  @postcon Inserts a Method comment into the editor avoid the current method.

  @param   Sender  As a TObject

**)
procedure TBrowseAndDocItWizard.InsertMethodCommentClick(Sender: TObject);

var
  objMemStream: TStream;
  Module : TBaseLanguageModule;
  EditPos: TOTAEditPos;
  T : TElementContainer;
  N : TGenericMethodDecl;
  Writer: IOTAEditWriter;
  Source: IOTASourceEditor;

  (**

    This method recursively works throug the hierarchy of elements looking for
    the method which is closest to be on or just above the current cursor line.

    @precon  Container must be a valid TElementContainer instance.
    @postcon Recursively works throug the hierarchy of elements looking for
             the method which is closest to be on or just above the current
             cursor line.

    @param   Container as a TElementContainer

  **)
  Procedure FindMethod(Container : TElementContainer);

  Var
    i : Integer;
    M : TGenericMethodDecl;

  Begin
    For i := 1 To Container.ElementCount Do
      Begin
        If Container.Elements[i] Is TGenericMethodDecl Then
          Begin
            M := Container.Elements[i] As TGenericMethodDecl;
            If (M.Line <= EditPos.Line) Then
              Begin
                If N = Nil Then
                  N := M
                Else
                  If M.Line > N.Line Then
                    N := M;
              End;
          End;
        FindMethod(Container.Elements[i]);
      End;
  End;

begin
  N := Nil;
  Source := ActiveSourceEditor;
  If Source = Nil Then
    Exit;
  objMemStream := EditorAsMemoryStream(Source);
  Try
    Module := Dispatcher(objMemStream, Source.FileName, Source.Modified, [moParse]);
    If Module <> Nil Then
      Try
        EditPos :=  Source.GetEditView(0).CursorPos;
        T := Module.FindElement(strImplementedMethods);
        If T <> Nil Then
          Begin
            FindMethod(T);
            If N <> Nil Then
              Begin
                If N.Comment <> Nil Then
                  If MessageDlg(Format(strMethodAlreadyExists, [N.QualifiedName]),
                    mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
                    Exit;
                Writer := Source.CreateUndoableWriter;
                Try
                  EditPos := WriteMethodComment(N, Source, Writer, N.Comment);
                Finally
                  Writer := Nil;
                End;
                Source.GetEditView(0).CursorPos := EditPos;
              End Else
                MessageDlg(strNoMethodFound, mtWarning, [mbOK], 0);
          End;
      Finally
        Module.Free;
      End;
  Finally
   objMemStream.Free;
  End;
end;

(**


  This method writes the method comment to the active editor.


  @precon  Method is a valid instance of a method declaration to be commented,

           Writer is a valid instance of an open tools api writer, iBufferPos

           is the buffer position to insert the comment and iCol is the

           column to indent the comment by.

  @postcon A method comment is inserted into the editor.


  @param   Method   as a TGenericMethodDecl

  @param   Source   as an IOTASourceEditor
  @param   Writer   as an IOTAEditWriter
  @param   AComment as a TComment
  @return  a TOTAEditPos

**)
Function TBrowseAndDocItWizard.WriteMethodComment(Method : TGenericMethodDecl;
  Source : IOTASourceEditor; Writer : IOTAEditWriter; AComment : TComment) : TOTAEditPos;

Const
  strMethodTypes : Array[mtConstructor..mtFunction] Of String = (
    'Constructor', 'Destructor', 'Procedure', 'Function');

Var
  iLen : Integer;
  i : Integer;
  C, CharPos: TOTACharPos;
  iBufferPos: Integer;
  iLines : Integer;
  strType: String;

begin
  iLines := Method.Line;
  CharPos.Line := Method.Line;
  CharPos.CharIndex := Method.Column;
  If Method.ClsName <> '' Then
    Dec(CharPos.CharIndex, 1 + Length(Method.ClsName));
  Dec(CharPos.CharIndex, 1 + Length(strMethodTypes[Method.MethodType]));
  If Method.ClassMethod Then
    Dec(CharPos.CharIndex, 6);
  If AComment <> Nil Then // Delete existing comment.
    Begin
      C.CharIndex := AComment.Col;
      C.Line := AComment.Line;
      iBufferPos := Source.GetEditView(0).CharPosToPos(C);
      Writer.CopyTo(iBufferPos - 1);
      iBufferPos := Source.GetEditView(0).CharPosToPos(CharPos);
      Writer.DeleteTo(iBufferPos - 1);
    End Else
    Begin
      iBufferPos := Source.GetEditView(0).CharPosToPos(CharPos);
      Writer.CopyTo(iBufferPos - 1);
    End;
  // Block Header
  Writer.Insert('(**'#10#13#10#13);
  Writer.Insert(PChar(GetMethodDescription(Method, AComment, CharPos.CharIndex - 1 + 2)));
  // Output method information
  iLen := 0;
  For i := 0 To Method.ParameterCount - 1 Do
    If iLen < Length(Method.Parameters[i].Identifier) Then
      iLen := Length(Method.Parameters[i].Identifier);
  For i := 0 To Method.ParameterCount - 1 Do
    Begin
      Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1)));
      Writer.Insert(PChar(Format('  @param   %-*s as ', [
        iLen, Method.Parameters[i].Identifier])));
      If Method.Parameters[i].ParamType <> Nil Then
        Begin
          strType := Method.Parameters[i].ParamReturn;
          Writer.Insert(PChar(Format('%s %s%s%s'#10#13, [
            strAOrAn[(strType[1] In strVowels) Or Method.Parameters[i].ArrayOf],
            strArrayOf[Method.Parameters[i].ArrayOf], strType,
            strModifier[Method.Parameters[i].ParamModifier]])));
        End;
    End;
  If Method.ReturnType <> Nil Then
    Begin
      Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1)));
      Writer.Insert(PChar(Format('  @return  %s %-*s',
        [strAOrAn[Method.ReturnType.AsString[1] In strVowels], iLen,
        Method.ReturnType.AsString])));
      Writer.Insert(PChar(#10#13));
    End;
  // Block Footer
  If (Method.ParameterCount > 0) Or (Method.ReturnType <> Nil) Then
    Writer.Insert(PChar(#10#13));
  Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1) + '**)'#10#13));
  Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1)));
  iLines := Writer.CurrentPos.Line - iLines;
  // Get header in view if not already
  Result.Col := CharPos.CharIndex + 2;
  Result.Line := CharPos.Line + 2;
  SelectionChange(Result.Line + iLines, Result.Col, Result.Line - 2, Result.Col,
    stIdentifier);
end;

(**


  This method returns a description for the method if it is a constructor,
  destructor, getter or setter method, else it returns an empty string.


  @precon  Method is a valid instance of a method declatation to be described.

  @postcon Returns a description of the method is applicable.


  @param   Method   as a TGenericMethodDecl
  @param   AComment as a TComment
  @param   iIndent  as an Integer
  @return  a String

**)
function TBrowseAndDocItWizard.GetMethodDescription(Method : TGenericMethodDecl;
  AComment : TComment; iIndent : Integer) : String;

var
  i: Integer;
  boolCon: Boolean;
  strDescription : String;

begin
  With BrowseAndDocItOptions Do
    For i := 0 To MethodDescriptions.Count - 1 Do
      If Like(MethodDescriptions.Names[i], Method.Identifier) Then
        Begin
          strDescription := MethodDescriptions.ValueFromIndex[i];
          Break;
        End;
  If AComment = Nil Then
    Result := Format('%s%s'#10#13#10#13, [StringOfChar(#32, iIndent), strDescription])
  Else
    Begin
      Result := Format('%s'#10#13#10#13, [AComment.AsString(iIndent, 80, True)]);
      boolCon := False;
      i := AComment.FindTag('precon');
      If i > -1 Then
        Begin
          Result := Result + OutputTag(iIndent, AComment.Tag[i]);
          boolCon := True;
        End;
      i := AComment.FindTag('postcon');
      If i > -1 Then
        Begin
          Result := Result + OutputTag(iIndent, AComment.Tag[i]);
          boolCon := True;
        End;
      If boolCon Then
        Result := Result + #10#13;
      boolCon := False;
      For i := 0 To AComment.TagCount - 1 Do
        If Not IsKeyWord(AComment.Tag[i].TagName, ['param', 'postcon', 'precon', 'return']) Then
          Begin
            Result := Result + OutputTag(iIndent, AComment.Tag[i]);
            boolCon := True;
          End;
      If boolCon Then
        Result := Result + #10#13;
    End;
end;

(**


  This function returns the tag information indented and broken into line no
  wider than 80 characters.


  @precon  Tag must be a valid comment tag.

  @postcon Returns the tag information indented and broken into line no wider

           than 80 characters.


  @param   iIndent as an Integer
  @param   Tag     as a TTag
  @return  a String

**)
Function TBrowseAndDocItWizard.OutputTag(iIndent : Integer; Tag : TTag) : String;

Var
  str : String;
  i : Integer;

Begin
  str := Format('%s@%-8s', [StringOfChar(#32, iIndent), Tag.TagName]);
  For  i := 0 To Tag.TokenCount - 1 Do
    If Length(str + Tag.Token[i]) < 80 Then
      str := str + Tag.Token[i] + #32
    Else
      Begin
        Result := Result + str;
        str := #10#13 + StringOfChar(#32, iIndent + 9) + Tag.Token[i] + #32;
      End;
  Result := Result + str + #10#13;
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
procedure TBrowseAndDocItWizard.InsertPropertyCommentClick(Sender: TObject);

var
  objMemStream: TStream;
  Module : TBaseLanguageModule;
  EditPos: TOTAEditPos;
  Source : IOTASourceEditor;
  T : TElementContainer;
  Q : TGenericProperty;
  Writer: IOTAEditWriter;

  (**

    This method recursively works throug the hierarchy of elements looking for
    the properties which is closest to be on or just above the current cursor line.

    @precon  Container must be a valid TElementContainer instance.
    @postcon Recursively works throug the hierarchy of elements looking for
             the property which is closest to be on or just above the current
             cursor line.

    @param   Container as a TElementContainer

  **)
  Procedure FindProperty(Container : TElementContainer);

  Var
    i : Integer;
    P : TGenericProperty;

  Begin
    For i := 1 To Container.ElementCount Do
      Begin
        If Container.Elements[i] Is TGenericProperty Then
          Begin
            P := Container.Elements[i] As TGenericProperty;
            If (P.Line <= EditPos.Line) Then
              Begin
                If Q = Nil Then
                  Q := P
                Else
                  If P.Line > Q.Line Then
                    Q := P;
              End;
          End;
        FindProperty(Container.Elements[i]);
      End;
  End;

begin
  Source := ActiveSourceEditor;
  If Source = Nil Then
    Exit;
  objMemStream := EditorAsMemoryStream(Source);
  Try
    Module := Dispatcher(objMemStream, Source.FileName, Source.Modified, [moParse]);
    If Module <> Nil Then
      Try
        EditPos :=  Source.GetEditView(0).CursorPos;
        T := Module.FindElement(strTypesLabel);
        If T <> Nil Then
          Begin
            FindProperty(T);
            If Q <> Nil Then
              Begin
                If Q.Comment <> Nil Then
                  If MessageDlg(Format(strPropertyAlreadyExists, [Q.Identifier]),
                    mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
                    Exit;
                Writer := Source.CreateUndoableWriter;
                Try
                  EditPos := WritePropertyComment(Q, Source, Writer, Q.Comment);
                Finally
                  Writer := Nil;
                End;
                Source.GetEditView(0).CursorPos := EditPos;
              End Else
                MessageDlg(strNoPropertyFound, mtWarning, [mbOK], 0);
          End;
      Finally
        Module.Free;
      End;
  Finally
   objMemStream.Free;
  End;
end;

(**


  This method write the property comment to the active editor.


  @precon  P is a valid instance of a property declaration to be commented,

           Writer is a valid instance of an open tools api writer, iBufferPos

           is the buffer position to insert the comment and iCol is the

           column to indent the comment by.

  @postcon A property comment it inserted into the editor.


  @param   Prop     as a TGenericProperty

  @param   Source   as an IOTASourceEditor
  @param   Writer   as an IOTAEditWriter
  @param   AComment as a TComment
  @return  a TOTAEditPos

**)
Function TBrowseAndDocItWizard.WritePropertyComment(Prop : TGenericProperty;
  Source : IOTASourceEditor; Writer : IOTAEditWriter; AComment : TComment) : TOTAEditPos;

Var
  i : Integer;
  iLen : Integer;
  C, CharPos: TOTACharPos;
  iBufferPos: Integer;
  iLines : Integer;
  strType: String;

Begin
  iLines := Prop.Line;
  CharPos.Line := Prop.Line;
  CharPos.CharIndex := Prop.Column;
  Dec(CharPos.CharIndex, 9);
  If AComment <> Nil Then // Delete existing comment.
    Begin
      C.CharIndex := AComment.Col;
      C.Line := AComment.Line;
      iBufferPos := Source.GetEditView(0).CharPosToPos(C);
      Writer.CopyTo(iBufferPos - 1);
      iBufferPos := Source.GetEditView(0).CharPosToPos(CharPos);
      Writer.DeleteTo(iBufferPos - 1);
    End Else
    Begin
      iBufferPos := Source.GetEditView(0).CharPosToPos(CharPos);
      Writer.CopyTo(iBufferPos - 1);
    End;
  Writer.Insert(PChar('(**'#13#10));
  Writer.Insert(PChar(GetPropertyDescription(Prop, AComment, CharPos.CharIndex - 1 + 2)));
  iLen := 0;
  For i := 0 To Prop.ParameterCount - 1 Do
    If iLen < Length(Prop.Parameters[i].Identifier) Then
      iLen := Length(Prop.Parameters[i].Identifier);
  For i := 0 To Prop.ParameterCount - 1 Do
    Begin
      Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1) +
        Format('  @param   %-*s as ', [iLen,
          Prop.Parameters[i].Identifier])));
      If Prop.Parameters[i].ParamType <> Nil Then
        Begin
          strType := Prop.Parameters[i].ParamReturn;
          Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1) + '  ' +
          Format('%s %s%s%s'#13#10, [strAOrAn[(strType[1] In
              strVowels) Or Prop.Parameters[i].ArrayOf],
            strArrayOf[Prop.Parameters[i].ArrayOf], strType,
            strModifier[Prop.Parameters[i].ParamModifier]
            ])));
        End;
    End;
  If Prop.TypeId <> Nil Then
    Begin
      Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1)));
      Writer.Insert(PChar(Format('  @return  %s %-*s',
        [strAOrAn[Prop.TypeId.AsString[1] In strVowels], iLen,
        Prop.TypeId.AsString])));
      Writer.Insert(PChar(#10#13));
    End;
  Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1) + '**)'#13#10));
  Writer.Insert(PChar(StringOfChar(#32, CharPos.CharIndex - 1)));
  // Get header in view if not already
  iLines := Writer.CurrentPos.Line - iLines;
  Result.Col := CharPos.CharIndex + 2;
  Result.Line := CharPos.Line + 1;
  SelectionChange(Result.Line + iLines, Result.Col, Result.Line - 1, Result.Col,
    stIdentifier);
End;

(**


  This method returns a description for the method if it is a constructor,
  destructor, getter or setter method, else it returns an empty string.


  @precon  Method is a valid instance of a method declatation to be described.

  @postcon Returns a description of the method is applicable.


  @param   Prop     as a TGenericProperty
  @param   AComment as a TComment
  @param   iIndent  as an Integer
  @return  a String

**)
function TBrowseAndDocItWizard.GetPropertyDescription(Prop : TGenericProperty;
  AComment : TComment; iIndent : Integer) : String;

var
  i: Integer;

begin
  If AComment = Nil Then
    Begin
      Result := #32#32#10#13;
    End Else
    Begin
      Result := Format('%s'#10#13, [AComment.AsString(iIndent, 80, True)]);
      i := AComment.FindTag('precon');
      If i > -1 Then
        Result := Result + OutputTag(iIndent, AComment.Tag[i]);
      i := AComment.FindTag('postcon');
      If i > -1 Then
        Result := Result + OutputTag(iIndent, AComment.Tag[i]);
      For i := 0 To AComment.TagCount - 1 Do
        If Not IsKeyWord(AComment.Tag[i].TagName, ['param', 'postcon', 'precon', 'return']) Then
          Result := Result + OutputTag(iIndent, AComment.Tag[i]);
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
  @param   SelectType   as a TSelectType

**)
procedure TBrowseAndDocItWizard.SelectionChange(iIdentLine, iIdentCol,
  iCommentLine, iCommentCol : Integer; SelectType : TSelectType);

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
              If SourceEditor.GetSubViewCount > 0 Then
                SourceEditor.SwitchToView(0);
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

  This method invokes the HTML help page of the documentation to provide
  help for this addin.

  @precon  Sender is the menu item that invokes this event.
  @postcon Displays the Help file.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.HelpClick(Sender: TObject);

Const
  strHelpFile = '\BrowseAndDocIt.HLP';

begin
  If BrowseAndDocItOptions.DocHelpFile = '' Then
    Raise Exception.Create(strHelpFileNotDefined);
  If Not FileExists(BrowseAndDocItOptions.DocHelpFile + strHelpFile) Then
    Raise Exception.CreateFmt(strHelpFileNotFound, [BrowseAndDocItOptions.DocHelpFile + strHelpFile]);
  ShellExecute(HInstance, 'OPEN', PChar(BrowseAndDocItOptions.DocHelpFile + strHelpFile), '',
    PChar(BrowseAndDocItOptions.DocHelpFile), SW_NORMAL);
end;

{procedure TBrowseAndDocItWizard.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin

end;}

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

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @param   BorlandIDEServices as an IBorlandIDEServices constant
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
      iEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(
        TEditorNotifier.Create);
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
  FUpdateTimer.Interval := 100;
  FUpdateTimer.OnTimer := TimerEventHandler;
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

  This method get and parsers the current editor and display it in the module
  explorer.

  @precon  None.
  @postcon Creates an instance of the language module for the specified file
           extension and passes the language module to the Dockable Module
           Explorer for rendering.

**)
Procedure TEditorNotifier.RefreshTree;

Var
  objMemStream : TMemoryStream;
  SourceEditor : IOTASourceEditor;
  Reader : IOTAEditReader;
  iPosition : Integer;
  iRead : Integer;
  M : TBaseLanguageModule;
  Options : IOTAProjectOptions;
  {E : IOTAModuleErrors;
  i : Integer;
  Es : TOTAErrors;}

begin
  If Application = Nil Then
    Exit;
  If Application.MainForm = Nil Then
    Exit;
  If Application.MainForm.Visible Then
    Begin
      objMemStream := TMemoryStream.Create;
      Try
        SourceEditor := ActiveSourceEditor;
        If SourceEditor <> Nil Then
          Begin
            If ActiveProject <> Nil Then
              Begin
                Options := ActiveProject.ProjectOptions;
                BrowseAndDocItOptions.Defines.Text :=
                  StringReplace(Options.Values['Defines'], ';', #13#10,
                  [rfReplaceAll]);
              End;
            DetermineCompilerDefinitions(BrowseAndDocItOptions.Defines);
            Reader := SourceEditor.CreateReader;
            Try
              iPosition := 0;
              Repeat
                iRead := Reader.GetText(iPosition, @Buffer, iBufferSize);
                objMemStream.WriteBuffer(Buffer, iRead);
                Inc(iPosition, iRead);
              Until iRead < iBufferSize;
            Finally
              Reader := Nil;
            End;
            objMemStream.Position := 0;
            M := Dispatcher(objMemStream, SourceEditor.FileName,
              SourceEditor.Modified, [moParse, moCheckForDocumentConflicts]);
            Try
              {: @note Can not use this due to lock up in the IDE when calling
                       QueryInterface of the module when compiling is happening.
              If doShowIDEErrorsOnSuccessfulParse In BrowseAndDocItOptions.Options Then
                If M <> Nil Then
                  If M.FindElement(strErrors) = Nil Then
                    If Not boolCompiling And CheckChildProcesses Then
                      If SourceEditor.Module.QueryInterface(IOTAModuleErrors, E) = S_OK Then
                        Begin
                          Es := E.GetErrors;
                          For i := Low(Es) to High(Es) Do
                            M.AddIssue(Es[i].Text, scNone, 'IDE', Es[i].Start.Line,
                              Es[i].Start.CharIndex + 1, etError);
                        End;}
              TfrmDockableModuleExplorer.RenderDocumentTree(M);
            Finally
              M.Free;
            End;
          End;
      Finally
        objMemStream.Free;
      End;
    End;
end;

(**

  This method determines which sytem compiler definitions need to be put
  in the definition list.

  @precon  slDefines neds to be a valid TStringList
  @postcon Adds the relavent compiler definitions.

  @param   slDefines as a TStringList

**)
Procedure TEditorNotifier.DetermineCompilerDefinitions(slDefines : TStringList);

Begin
  // Delphi 4 - Starts here as this will be the earliest version that can run
  // this addin.
  {$IFDEF VER120} // Delphi 4
  slDefines.Add('VER120');
  {$ENDIF}
  {$IFDEF VER130} // Delphi 5
  slDefines.Add('VER130');
  {$ENDIF}
  {$IFDEF VER140} // Delphi 6
  slDefines.Add('VER140');
  {$ENDIF}
  {$IFDEF VER150} // Delphi 7
  slDefines.Add('VER150');
  {$ENDIF}
  {$IFDEF VER160} // Delphi for .NET
  slDefines.Add('VER160');
  {$ENDIF}
  {$IFDEF VER170} // Delphi 2005
  slDefines.Add('VER170');
  {$ENDIF}
  {$IFDEF VER180} // Delphi 2006
  slDefines.Add('VER180');
  {$ENDIF}
End;

(**

  This is a TTimer on Timer event handler.

  @precon  None.
  @postcon Checks to see if the last time the editor was changes is beyond the
           wait time for the module to be updated.

  @param   Sender as a TObject

**)
procedure TEditorNotifier.TimerEventHandler(Sender: TObject);

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
    End;
  If (iLastUpdateTickCount > 0) And
    (GetTickCount > iLastUpdateTickCount + BrowseAndDocItOptions.UpdateInterval) Then
    Begin
      iLastUpdateTickCount := 0;
      FUpdateTimer.Enabled := False;
      Try
        RefreshTree;
      Finally
        FUpdateTimer.Enabled := True;
      End;
    End;
end;

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
  @postcon Forces an immediate update of the module explorer.

  @param   EditWindow as an INTAEditWindow constant
  @param   EditView   as an IOTAEditView constant

**)
procedure TEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
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

(**

  This method binds all the Browse and Doc It keybindings to the methods in this
  class.

  @precon  None.
  @postcon All the keybinding are bound.

  @param   BindingServices as an IOTAKeyBindingServices constant

**)
procedure TKeyboardBinding.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([Shortcut(13, [ssCtrl, ssShift, ssAlt])], FocusModuleExplorer, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('M'), [ssCtrl, ssShift, ssAlt])], InsertMethodComment, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('P'), [ssCtrl, ssShift, ssAlt])], InsertPropertyComment, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('B'), [ssCtrl, ssShift, ssAlt])], InsertBlockComment, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('L'), [ssCtrl, ssShift, ssAlt])], InsertLineComment, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('I'), [ssCtrl, ssShift, ssAlt])], InsertInSituComment, Nil);
  BindingServices.AddKeyBinding([Shortcut(Ord('T'), [ssCtrl, ssShift, ssAlt])], ShowTokens, Nil);
end;

(**

  This method calls the main wizards Insert Block Comment method.

  @precon  None.
  @postcon A block comment should be inserted into the editor at the current
           cursor position.

  @param   Context       as an IOTAKeyContext constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.InsertBlockComment(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  FWizard.InsertBlockCommentClick(Self);
  BindingResult := krHandled;
end;

(**

  This method calls the main wizards Insert In Situ comment at the position of
  the current cursor in the editor.

  @precon  None.
  @postcon Should insert an insitu comment into the editor.

  @param   Context       as an IOTAKeyContext constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.InsertInSituComment(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  FWizard.InsertInSituCommentClick(Self);
  BindingResult := krHandled;
end;

(**

  This method calls the main wizards Insert Line Comment at the position of
  the current cursor in thr editor.

  @precon  None.
  @postcon Should insert a Line comment into the editor.

  @param   Context       as an IOTAKeyContext constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.InsertLineComment(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  FWizard.InsertLineCommentClick(Self);
  BindingResult := krHandled;
end;

(**

  This method calls the main wizards Insert Method Comment at the position of
  the current cursor in the editor.

  @precon  None.
  @postcon Should insert a method comment into the editor.

  @param   Context       as an IOTAKeyContext constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.InsertMethodComment(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  FWizard.InsertMethodCommentClick(Self);
  BindingResult := krHandled;
end;

(**

  This method calls the main wizards Insert Property Comment at the position of
  the current cursor in the editor.

  @precon  None.
  @postcon Should insert a property comment into the editor.

  @param   Context       as an IOTAKeyContext constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.InsertPropertyComment(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  FWizard.InsertPropertyCommentClick(Self);
  BindingResult := krHandled;
end;

(**

  This is a keyboard binding event hanlder for showing the tokens in the parser.

  @precon  None.
  @postcon Displays a form containsing the tokens in the current editor.

  @param   Context       as an IOTAKeyContext constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
procedure TKeyboardBinding.ShowTokens(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);

Var
  objMemStream : TMemoryStream;
  SourceEditor : IOTASourceEditor;
  Reader : IOTAEditReader;
  iPosition : Integer;
  iRead : Integer;
  Source : TBaseLanguageModule;

begin
  objMemStream := TMemoryStream.Create;
  Try
    SourceEditor := ActiveSourceEditor;
    If SourceEditor = Nil Then
      Exit;
    Reader := SourceEditor.CreateReader;
    Try
      iPosition := 0;
      Repeat
        iRead := Reader.GetText(iPosition, @Buffer, iBufferSize);
        objMemStream.WriteBuffer(Buffer, iRead);
        Inc(iPosition, iRead);
      Until iRead < iBufferSize;
    Finally
      Reader := Nil;
    End;
    objMemStream.Position := 0;
    Source := Dispatcher(objMemStream, SourceEditor.FileName,
      SourceEditor.Modified, []);
    If Source <> Nil Then
      Try
        TfrmTokenForm.Execute(Source);
      Finally
        Source.Free;
      End;
  Finally
    objMemStream.Free;
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

  @param   Context       as an IOTAKeyContext constant
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

(** This initialization section installs an IDE Splash Screen item. **)
Initialization
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap');
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1)]),
    bmSplashScreen,
    True,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]), ''
    );
(** This finalization section removes this wizard from the IDE when the package
    is unloaded. **)
Finalization
  (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBinding);
  (BorlandIDEServices As IOTAEditorServices).RemoveNotifier(iEditorIndex);
  {(BorlandIDEServices As IOTAServices).RemoveNotifier(iIDENotifier);}
  (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
End.
