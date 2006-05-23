(**

  This mmodule contains the packages main wizard interface.

  @Author  David Hoyle
  @Date    22 May 2006
  @Version 1.0

**)
Unit BrowseAndDocItWizard;

Interface

Uses
  Classes, ToolsAPI, DeskUtil, Menus, ExtCtrls, BaseLanguageModule,
  PascalDocModule, DockForm;

Type
  (** This emunerate descibed the different types of doc comment .**)
  TCommentType = (ctBlock, ctLine, ctInSitu);

  (** This enumerate define the position of the editor when an item is selected
      in the module explorer **)
  TBrowsePosition = (bpCommentTop, bpCommentCentre, bpIdentifierTop,
    bpIdentifierCentre);

  (** This is the clas which defined the Wizard interface. **)
  TBrowseAndDocItWizard = Class(TNotifierObject, IOTAWizard)
  Private
    mmiPascalDocMenu : TMenuItem;
    FCounter : Integer;
    FFileName : String;
    FKeyBinding : Integer;
    FDocOption: Integer;
    FDocHelpFile : String;
    FBrowsePosition : TBrowsePosition;
    procedure InsertCommentBlock(CommentType: TCommentType);
    procedure OptionsClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    Procedure LoadSettings;
    Procedure SaveSettings;
    procedure SelectionChange(iIdentLine, iIdentCol, iCommentLine,
      iCommentCol : Integer);
    function GetMethodDescription(Method : TMethodDecl) : String;
    procedure WriteMethodComment(Method : TMethodDecl; Writer : IOTAEditWriter;
      iBufferPos : TStreamPosition; iCol : Integer);
    Procedure WritePropertyComment(P : TProperty;
      Writer : IOTAEditWriter; iBufferPos : TStreamPosition; iCol : Integer);
    procedure CreateMenuItem(mmiParent: TMenuItem;  strCaption: String = '';
      ClickProc: TNotifyEvent = Nil);
    Procedure InsertMethodCommentClick(Sender : TObject);
    Procedure InsertPropertyCommentClick(Sender : TObject);
    Procedure InsertBlockCommentClick(Sender : TObject);
    Procedure InsertLineCommentClick(Sender : TObject);
    Procedure InsertInSituCommentClick(Sender : TObject);
    Procedure ModuleExplorerClick(Sender : TObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    { IOTAWizard }
    Function GetIDString: string;
    Function GetName: string;
    Function GetState: TWizardState;
    Procedure Execute;
    { IOTAMenuWizard }
    Function GetMenuText: string;
  End;

  (** This class handles notifications from the editor so that changes in the
      editor can be displayed. **)
  TEditorNotifier = Class(TNotifierObject, INTAEditServicesNotifier)
  Private
    FUpdateTimer : TTimer;
    Procedure RefreshTree;
    Procedure TimerEventHandler(Sender : TObject);
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
    Procedure FocusModlueExplorer(const Context: IOTAKeyContext;
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
  Protected
  Public
    Constructor Create(Wizard : TBrowseAndDocItWizard);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  End;

  Procedure Register;

Implementation

{$R SplashScreenIcon.res}

Uses
  SysUtils, DockableModuleExplorer, Registry, ToolsAPIUtils,
  OptionsForm, Forms, Windows, ShellAPI, TokenForm;

Resourcestring
  (** The registry key for the wizards settings. **)
  strKey = 'Software\Season''s Fall\Pascal Doc\';
  (** This is a text string of revision from nil and a to z. **)
  strRevision = ' abcdefghijklmnopqrstuvwxyz';
  (** This is a message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for Borland Developer Studio 2006';
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'Open Source Freeware by David Hoyle (Build %d.%d.%d.%d)';

Var
  (** This is an index for the wizard when register with the ide. Its required
      in order to remove it from memory. **)
  iWizardIndex : Integer;
  (** This is an index for the editor notifier required when the package is
      unloaded **)
  iEditorIndex : Integer;
  (** A private varaible to hold the Document Explorer Options **)
  FDocExplorerOptions : TDocOptions;
  (** A private variable to hold the update interval in ms between changes and
      the updating of the module explorer **)
  iUpdateInterval: Cardinal;
  (** A private variable to hold the time of the last editor update. **)
  iLastUpdateTickCount : Cardinal;
  (** An index for the keybinding nofitier - required when the package is
      unloaded **)
  iKeyBinding: Integer;

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
  iWizardIndex := (BorlandIDEServices As IOTAWizardServices).AddWizard(
    Wizard);
  iEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(
    TEditorNotifier.Create);
  iKeyBinding := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(
    TKeyboardBinding.Create(Wizard))
End;

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
  TfrmDockableModuleExplorer.HookEventHandlers(SelectionChange);
  mmiMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  mmiPascalDocMenu := TMenuItem.Create(mmiMainMenu);
  With mmiPascalDocMenu Do
    Caption := '&Browse and Doc It';
  mmiMainMenu.Items.Insert(mmiMainMenu.Items.Count - 2, mmiPascalDocMenu);
  CreateMenuItem(mmiPascalDocMenu, 'Module &Explorer', ModuleExplorerClick);
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
  iUpdateInterval := 0;
  SpecialTags := TStringList.Create;
  LoadSettings;
End;

(**

  This method creates menu items using the passed information.

  @precon  mmiParent must be a valid parent menu item in the IDE.
  @postcon A Sub menu ite is created under mmiParent.

  @param   mmiParent  as a TMenuItem as a reference
  @param   strCaption as a String
  @param   ClickProc  as a TNotifyEvent

**)
Procedure TBrowseAndDocItWizard.CreateMenuItem(mmiParent : TMenuItem;
  strCaption : String = ''; ClickProc : TNotifyEvent = Nil);

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
  SaveSettings;
  SpecialTags.Free;
  If mmiPascalDocMenu <> Nil Then
    mmiPascalDocMenu.Free;
  Inherited;
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
end;

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

  This method loads the wizards settings from the registry.

  @precon  None.
  @postcon Loads the wizards settings from the registry.

**)
Procedure TBrowseAndDocItWizard.LoadSettings;

Var
  j : TDocOption;
  iCount : Integer;
  k : Integer;

Begin
  With TRegIniFile.Create() Do
    Try
      For j := Low(TDocOption) To High(TDocOption) Do
        If ReadBool(strKey + 'DocExplorerOptions', DocOptionInfo[j].Description,
          DocOptionInfo[j].Enabled) Then
          FDocExplorerOptions := FDocExplorerOptions + [j];
      iUpdateInterval := ReadInteger(strKey + 'ModuleExplorer', 'UpdateInterval', 1000);
      FDocOption := ReadInteger(strKey + 'Setup', 'DocOption', 0);
      iCount := ReadInteger(strKey + 'Setup', 'SpecialTagCount', 0);
      If iCount = 0 Then
        Begin // Some default special tags
          SpecialTags.AddObject('todo=Things To Do', TObject(iShowInTree And iAutoExpand));
          SpecialTags.AddObject('precon=Pre-Conditions', TObject(0));
          SpecialTags.AddObject('postcon=Post-Conditions', TObject(0));
          SpecialTags.AddObject('param=Parameters', TObject(0));
          SpecialTags.AddObject('return=Returns', TObject(0));
          SpecialTags.AddObject('note=Notes', TObject(0));
          SpecialTags.AddObject('see=Also See', TObject(0));
          SpecialTags.AddObject('exception=Exception Raised', TObject(0));
        End Else
        Begin
          For k := 1 To iCount Do
            SpecialTags.AddObject(ReadString(strKey + 'SpecialTags',
              Format('SpecialTag%d', [k]), ''), TObject(ReadInteger(strKey + 'SpecialTags',
              Format('SpecialTagOptions%d', [k]), 0)));
        End;
      FDocHelpFile := ReadString(strKey + 'ModuleExplorer', 'HelpFile', '');
      FBrowsePosition := TBrowsePosition(ReadInteger(strKey + 'Setup', 'BrowsePosition',
        Integer(bpIdentifierCentre)));
    Finally
      Free;
    End;
End;

(**

  This method saves the wizards settings to the registry.

  @precon  None.
  @postcon Saves the wizards settings to the registry.

**)
Procedure TBrowseAndDocItWizard.SaveSettings;

Var
  j : TDocOption;
  k : Integer;

Begin
  With TRegIniFile.Create() Do
    Try
      For j := Low(TDocOption) To High(TDocOption) Do
        WriteBool(strKey + 'DocExplorerOptions', DocOptionInfo[j].Description,
          j In FDocExplorerOptions);
      WriteInteger(strKey + 'ModuleExplorer', 'UpdateInterval', iUpdateInterval);
      WriteInteger(strKey + 'Setup', 'DocOption', FDocOption);
      WriteInteger(strKey + 'Setup', 'SpecialTagCount', SpecialTags.Count);
      For k := 1 To SpecialTags.Count Do
        Begin
          WriteString(strKey + 'SpecialTags', Format('SpecialTag%d', [k]),
            SpecialTags[k - 1]);
          WriteInteger(strKey + 'SpecialTags', Format('SpecialTagOptions%d', [k]),
            Integer(SpecialTags.Objects[k - 1]));
        End;
      WriteString(strKey + 'ModuleExplorer', 'HelpFile', FDocHelpFile);
      WriteInteger(strKey + 'Setup', 'BrowsePosition', Integer(FBrowsePosition));
    Finally
      Free;
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
      SelectionChange(EditPos.Line, EditPos.Col, EditPos.Line, EditPos.Col);
      // Place cursor at start of comment
      Case CommentType Of
        ctBlock:
          Begin
            EditPos.Line := EditPos.Line + 2;
            EditPos.Col := EditPos.Col + 2;
          End;
        ctLine, ctInSitu:
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

Var
  objMemStream : TMemoryStream;
  SourceEditor: IOTASourceEditor;
  iStreamPos : TStreamPosition;
  EditPos : TOTAEditPos;
  CharPos : TOTACharPos;
  Writer : IOTAEditWriter;
  Method : TMethodDecl;
  recPosition : TTokenPosition;

begin
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  objMemStream := EditorAsMemoryStream(SourceEditor);
  Try
    With TPascalDocModule.Create(objMemStream, SourceEditor.FileName,
      SourceEditor.Modified, [], []) Do
      Try
        EditPos :=  SourceEditor.GetEditView(0).CursorPos;
        CharPos.Line := EditPos.Line;
        CharPos.CharIndex := EditPos.Col;
        iStreamPos := SourceEditor.GetEditView(0).CharPosToPos(CharPos);
        Method := FindMethodAtStreamPosition(iStreamPos, recPosition);
        Try
          If Method = Nil Then Exit;
          Writer := SourceEditor.CreateUndoableWriter;
          Try
            WriteMethodComment(Method, Writer, recPosition.BufferPos,
              recPosition.Column);
          Finally
            Writer := Nil;
          End;
          // Get header in view if not already
          With SourceEditor.GetEditView(0) Do
            Begin
              EditPos.Col := recPosition.Column + 2;
              EditPos.Line := recPosition.Line + 2;
              SelectionChange(EditPos.Line - 2, EditPos.Col, EditPos.Line - 2,
                EditPos.Col);
              CursorPos := EditPos;
            End;
        Finally
          If Method <> Nil Then Method.Free;
        End;
      Finally
        Free;
      End;
  Finally
   objMemStream.Free;
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

Var
  objMemStream : TMemoryStream;
  SourceEditor: IOTASourceEditor;
  iStreamPos : TStreamPosition;
  EditPos : TOTAEditPos;
  CharPos : TOTACharPos;
  Writer : IOTAEditWriter;
  Cls : TClassDecl;
  recPosition : TTokenPosition;

begin
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  objMemStream := EditorAsMemoryStream(SourceEditor);
  Try
    With TPascalDocModule.Create(objMemStream, SourceEditor.FileName,
      SourceEditor.Modified, [], []) Do
      Try
        EditPos :=  SourceEditor.GetEditView(0).CursorPos;
        CharPos.Line := EditPos.Line;
        CharPos.CharIndex := EditPos.Col;
        iStreamPos := SourceEditor.GetEditView(0).CharPosToPos(CharPos);
        Cls := FindPropertyAtStreamPosition(iStreamPos, recPosition);
        Try
          If Cls = Nil Then Exit;
          Writer := SourceEditor.CreateUndoableWriter;
          Try
            WritePropertyComment(Cls.Properties[0], Writer,
              recPosition.BufferPos, recPosition.Column);
          Finally
            Writer := Nil;
          End;
          // Get header in view if not already
          With SourceEditor.GetEditView(0) Do
            Begin
              EditPos.Line := recPosition.Line + 1;
              EditPos.Col := recPosition.Column + 2;
              SelectionChange(EditPos.Line - 1, EditPos.Col, EditPos.Line - 1,
                EditPos.Col);
              CursorPos := EditPos;
            End;
        Finally
          Cls.Free;
        End;
      Finally
        Free;
      End;
  Finally
   objMemStream.Free;
  End;
end;

(**

  This method write the property comment to the active editor.

  @precon  P is a valid instance of a property declaration to be commented,
           Writer is a valid instance of an open tools api writer, iBufferPos is
           the buffer position to insert the comment and iCol is the column to
           indent the comment by.
  @postcon A property comment it inserted into the editor.

  @param   P          as a TProperty
  @param   Writer     as an IOTAEditWriter
  @param   iBufferPos as a TStreamPosition
  @param   iCol       as an Integer

**)
Procedure TBrowseAndDocItWizard.WritePropertyComment(P : TProperty;
  Writer : IOTAEditWriter; iBufferPos : TStreamPosition; iCol : Integer);

Var
  i : Integer;
  iLen : Integer;

Begin
  Writer.CopyTo(iBufferPos - 1);
  Writer.Insert(PChar('(**'#13#10));
  Writer.Insert(PChar(StringOfChar(#32, iCol - 1) + '  '#13#10));
  iLen := 0;
  For i := 0 To P.ParameterCount - 1 Do
    If iLen < Length(P.Parameter[i].Identifier) Then
      iLen := Length(P.Parameter[i].Identifier);
  For i := 0 To P.ParameterCount - 1 Do
    Begin
      Writer.Insert(PChar(StringOfChar(#32, iCol - 1) +
        Format('  @param   %-*s as ', [iLen,
          P.Parameter[i].Identifier])));
      If P.Parameter[i].ParamType.AsString(True) <> '' Then
        Writer.Insert(PChar(StringOfChar(#32, iCol - 1) + '  ' +
        Format('%s %s%s%s'#13#10, [
          strAOrAn[(P.Parameter[i].ParamType.AsString(True)[1] In
            strVowels) Or P.Parameter[i].ArrayOf],
          strArrayOf[P.Parameter[i].ArrayOf],
          P.Parameter[i].ParamType.AsString(True),
          strModifier[P.Parameter[i].ParamModifier]
         ])));
    End;
  If P.TypeId <> '' Then
    Begin
      Writer.Insert(PChar(StringOfChar(#32, iCol - 1)));
      Writer.Insert(PChar(Format('  @return  %s %-*s',
        [strAOrAn[P.TypeId[1] In strVowels], iLen,
        P.TypeId])));
      Writer.Insert(PChar(#10#13));
    End;
  Writer.Insert(PChar(StringOfChar(#32, iCol - 1) + '**)'#13#10));
  Writer.Insert(PChar(StringOfChar(#32, iCol - 1)));
End;

(**

  This method write the method comment to the active editor.

  @precon  Method is a valid instance of a method declaration to be commented,
           Writer is a valid instance of an open tools api writer, iBufferPos is
           the buffer position to insert the comment and iCol is the column to
           indent the comment by.
  @postcon A method comment is inserted into the editor.

  @param   Method     as a TMethodDecl
  @param   Writer     as an IOTAEditWriter
  @param   iBufferPos as a TStreamPosition
  @param   iCol       as an Integer

**)
procedure TBrowseAndDocItWizard.WriteMethodComment(Method : TMethodDecl;
  Writer : IOTAEditWriter; iBufferPos : TStreamPosition; iCol : Integer);

Var
  iLen : Integer;
  i : Integer;

begin
  Writer.CopyTo(iBufferPos - 1);
  // Block Header
  Writer.Insert('(**'#10#13);
  Writer.Insert(PChar(#10#13));
  Writer.Insert(PChar(StringOfChar(#32, iCol - 1) +
    GetMethodDescription(Method)));
  Writer.Insert(PChar(#10#13));
  // Output method information
  iLen := 0;
  For i := 0 To Method.ParameterCount - 1 Do
    If iLen < Length(Method.Parameter[i].Identifier) Then
      iLen := Length(Method.Parameter[i].Identifier);
  For i := 0 To Method.ParameterCount - 1 Do
    Begin
      Writer.Insert(PChar(StringOfChar(#32, iCol - 1)));
      Writer.Insert(PChar(Format('  @param   %-*s as ', [
        iLen, Method.Parameter[i].Identifier])));
      If Method.Parameter[i].ParamType.AsString(True) <> '' Then
        Begin
          Writer.Insert(PChar(Format('%s %s%s%s'#10#13, [
            strAOrAn[(Method.Parameter[i].ParamType.AsString(True)[1] In strVowels)
              Or Method.Parameter[i].ArrayOf],
            strArrayOf[Method.Parameter[i].ArrayOf],
            Method.Parameter[i].ParamType.AsString(True),
            strModifier[Method.Parameter[i].ParamModifier]])));
        End;
    End;
  If Method.ReturnType <> '' Then
    Begin
      Writer.Insert(PChar(StringOfChar(#32, iCol - 1)));
      Writer.Insert(PChar(Format('  @return  %s %-*s',
      [strAOrAn[Method.ReturnType[1] In strVowels], iLen,
      Method.ReturnType])));
      Writer.Insert(PChar(#10#13));
    End;
  // Block Footer
  If (Method.ParameterCount > 0) Or (Method.ReturnType <> '') Then
    Writer.Insert(PChar(#10#13));
  Writer.Insert(PChar(StringOfChar(#32, iCol - 1) + '**)'#10#13));
  Writer.Insert(PChar(StringOfChar(#32, iCol - 1)));
end;


(**

  This method returns a description for the method if it is a constructor,
  destructor, getter or setter method, else it returns an empty string.

  @precon  Method is a valid instance of a method declatation to be described.
  @postcon Returns a description of the method isf applicable.
  
  @param   Method as a TMethodDecl
  @return  a String

**)
function TBrowseAndDocItWizard.GetMethodDescription(Method : TMethodDecl) : String;

begin
  If LowerCase(Copy(Method.Identifier, 1, 3)) = 'get' Then
    Result := Format('  This is a getter method for the %s property.'#10#13,
      [Copy(Method.Identifier, 4, Length(Method.Identifier) - 3)])
  Else If LowerCase(Copy(Method.Identifier, 1, 3)) = 'set' Then
    Result := Format('  This is a setter method for the %s property.'#10#13,
      [Copy(Method.Identifier, 4, Length(Method.Identifier) - 3)])
  Else If Method.MethodType = mtConstructor Then
    Result := Format('  This is the constructor method for the %s class.'#10#13,
      [Method.ClsName])
  Else If Method.MethodType = mtDestructor Then
    Result := Format('  This is the destructor method for the %s class.'#10#13,
      [Method.ClsName])
  Else
    Result := #32#32#10#13;
end;


(**

  This is a TMenuItem on click event. it invokes the Options dialogue.

  @precon  Sender is the object initiating the event.
  @postcon Displays the wizards Options dialogue.

  @param   Sender as a TObject

**)
procedure TBrowseAndDocItWizard.OptionsClick(Sender: TObject);

Var
  Opts : TDocOptions;
  i : Integer;
  HelpFile : String;
  iBrowsePos : Integer;

begin
  i := iUpdateInterval;
  Opts := FDocExplorerOptions;
  HelpFile := FDocHelpFile;
  iBrowsePos := Integer(FBrowsePosition);
  If TfrmOptions.Execute(Opts, i, HelpFile, iBrowsePos) Then
    Begin
      iUpdateInterval := i;
      FDocExplorerOptions := Opts;
      FDocHelpFile := HelpFile;
      iLastUpdateTickCount := 1;
      FBrowsePosition := TBrowsePosition(iBrowsePos);
    End;
end;

(**

  This method move the active editors cursor the the supplied position and
  centres the cursor on th screen.

  @precon  None.
  @postcon When a selection of made in the explorer the cursor is placed in
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
  (** @note There is no discurnable way of getting the code editor to handle
            folded code through the open tools API, therefore folded code
            will perform strangely when being browsed. **)
  SourceEditor := ActiveSourceEditor;
  If SourceEditor <> Nil Then
    Begin
      Case FBrowsePosition Of
        bpCommentTop, bpCommentCentre:
          Begin
            C.Line := iCommentLine;
            C.Col := iCommentCol;
            If C.Line * C.Col = 0 Then
              Begin
                C.Line := iIdentLine;
                C.Col := iIdentCol;
              End;
          End;
        bpIdentifierTop, bpIdentifierCentre:
          Begin
            C.Line := iIdentLine;
            C.Col := iIdentCol;
          End;
      End;
      If C.Line * C.Col > 0 Then
        Begin
          SourceEditor.GetEditView(0).CursorPos := C;
          If FBrowsePosition In [bpIdentifierCentre, bpCommentCentre] Then
            SourceEditor.GetEditView(0).Center(C.Line, 1)
          Else
            SourceEditor.GetEditView(0).SetTopLeft(C.Line, 1);
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
  strHelpFile = '\PASCALDOCHELP.HLP';

begin
  If FDocHelpFile = '' Then
    Raise Exception.Create(strHelpFileNotDefined);
  If Not FileExists(FDocHelpFile + strHelpFile) Then
    Raise Exception.CreateFmt(strHelpFileNotFound, [FDocHelpFile + strHelpFile]);
  ShellExecute(HInstance, 'OPEN', PChar(FDocHelpFile + strHelpFile), '',
    PChar(FDocHelpFile), SW_NORMAL);
end;

(**

  This is a procedure to initialising the wizard interface when loading the
  package as a DLL wizard.

  @precon  None.
  @postcon Initialises the wizard.

  @todo    This code needs to be updated to reflect the Register() procedure.

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

Begin
  Result := BorlandIDEServices <> Nil;
  If Result Then
    Begin
      Svcs := BorlandIDEServices As IOTAServices;
      ToolsAPI.BorlandIDEServices := BorlandIDEServices;
      Application.Handle := Svcs.GetParentHandle;
      RegisterProc(TBrowseAndDocItWizard.Create);
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
  M : TPascalDocModule;
  strExt : String;

begin
  If (Application = Nil) Or (Application.MainForm = Nil) Then Exit;
  If Not Application.MainForm.Visible Then Exit;
  objMemStream := TMemoryStream.Create;
  Try
    SourceEditor := ActiveSourceEditor;
    If SourceEditor = Nil Then
      Exit;
    strExt := UpperCase(ExtractFileExt(SourceEditor.FileName));
    If Not ((strExt = '.PAS') Or (strExt = '.DPR') Or (strExt = '.DPK')) Then
      Begin
        TfrmDockableModuleExplorer.RenderDocumentTree(Nil, FDocExplorerOptions);
        Exit;
      End;
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
    M := TPascalDocModule.Create(objMemStream, SourceEditor.FileName,
      SourceEditor.Modified, [moParse, moCheckForDocumentConflicts],
      FDocExplorerOptions);
    With M Do
      Try
        TfrmDockableModuleExplorer.RenderDocumentTree(M, FDocExplorerOptions);
      Finally
        Free;
      End;
  Finally
    objMemStream.Free;
  End;
end;

(**

  This is a TTimer on Timer event handler.

  @precon  None.
  @postcon Checks to see if the last time the editor was changes is beyond the
           wait time for the module to be updated.

  @param   Sender as a TObject

**)
procedure TEditorNotifier.TimerEventHandler(Sender: TObject);
begin
  If (iLastUpdateTickCount > 0) And (GetTickCount > iLastUpdateTickCount +
    iUpdateInterval) Then
    Begin
      iLastUpdateTickCount := 0;
      RefreshTree;
    End;
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

Begin
  { Build Number }
  VerInfoSize := GetFileVersionInfoSize(PChar('BrowseAndDocIt.BPL'), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(PChar('BrowseAndDocIt.BPL'), 0, VerInfoSize, VerInfo);
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
  BindingServices.AddKeyBinding([Shortcut(13, [ssCtrl, ssShift, ssAlt])], FocusModlueExplorer, Nil);
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
  OutputMessage('Method Comment');
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
  Source : TPascalDocModule;

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
    Source := TPascalDocModule.Create(objMemStream, SourceEditor.FileName,
      SourceEditor.Modified, [], []);
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
procedure TKeyboardBinding.FocusModlueExplorer(const Context: IOTAKeyContext;
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
  Result := 'Browse And Doc It Bindings';
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

{$IFDEF VER180}
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
{$IFDEF VER180}
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap');
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix, 1)]),
    bmSplashScreen,
    True,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]), ''
    );
{$ENDIF}
(** This finalization section removes this wizard from the IDE when the package
    is unloaded. **)
Finalization
  (BorlandIDEServices As IOTAKeyboardServices).RemoveKeyboardBinding(iKeyBinding);
  (BorlandIDEServices As IOTAEditorServices).RemoveNotifier(iEditorIndex);
  (BorlandIDEServices As IOTAWizardServices).RemoveWizard(iWizardIndex);
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer
End.
