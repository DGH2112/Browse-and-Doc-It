(**

  This module contains an editor notifier that monitors the coed for changes
  and in turn refreshes the module explorer.

  @Version 1.0
  @Date    20 Jul 2009
  @Author  David Hoyle

**)
Unit EditorNotifier;

Interface

Uses
  Classes, ToolsApi, ExtCtrls, BaseLanguageModule, DockForm;

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Type
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
    FLastUpdateTickCount : Cardinal;
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
    Procedure ResetLastUpdateTickCount(iNewValue : Integer = 0);
  End;

Implementation

Uses
  SysUtils, ToolsAPIUtils, Dialogs, DockableModuleExplorer, Windows, Forms,
  CommonIDEFunctions;

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

  This method resets the last update tick count with zero or an optional new
  value.

  @precon  None.
  @postcon Resets the last update tick count with zero or an optional new
           value.

  @param   iNewValue as an Integer

**)
procedure TEditorNotifier.ResetLastUpdateTickCount(iNewValue : Integer = 0);
begin
  FLastUpdateTickCount := 0;
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
      If FLastUpdateTickCount > 0 Then
        If (P.Col <> FLastCursorPos.Col) Or (P.Line <> FLastCursorPos.Line) Then
          Begin
            FLastUpdateTickCount := GetTickCount;
            FLastCursorPos := P;
          End;
      If Editor.FileName <> FLastEditorName Then
        Begin
          FLastUpdateTickCount := 1;
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
  If (FLastUpdateTickCount > 0) And
    (GetTickCount > FLastUpdateTickCount + BrowseAndDocItOptions.UpdateInterval) Then
    Begin
      FLastUpdateTickCount := 0;
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
    FLastUpdateTickCount := 1;
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
  FLastUpdateTickCount := GetTickCount;
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
{$ENDIF}

End.