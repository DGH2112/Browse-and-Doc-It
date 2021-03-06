(**

  This module contains an editor notifier that monitors the coed for changes
  and in turn refreshes the module explorer.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.EditorNotifier;

Interface

Uses
  Classes,
  ToolsApi,
  ExtCtrls,
  BADi.Base.Module,
  DockForm,
  BADI.CommonIDEFunctions;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class handles notifications from the editor so that changes in the
      editor can be displayed. **)
  TEditorNotifier = Class(TNotifierObject, IUnknown, IOTANotifier, INTAEditServicesNotifier)
  Strict Private
    FUpdateTimer : TTimer;
    FLastEditorName : String;
    FLastCursorPos: TOTAEditPos;
    FLastParserResult : Boolean;
    FLastUpdateTickCount : Cardinal;
    FBADIThreadMgr : TBrowseAndDocItThreadManager;
  Strict Protected
    Procedure EnableTimer(Const boolSuccessfulParse : Boolean);
    Procedure TimerEventHandler(Sender : TObject);
    Function  EditorInfo(var strFileName : String; var boolModified : Boolean) : String;
    Procedure RenderDocument(Const Module: TBaseLanguageModule);
    Procedure ExceptionMsg(Const strExceptionMsg : String);
    Procedure CheckForCursorMovement(Const Editor : IOTASourceEditor);
    Procedure CheckFileNameAndSize(Const Editor : IOTASourceEditor);
    // INTAEditServicesNotifier
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure ResetLastUpdateTickCount(Const iNewValue : Integer = 0);
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  BADI.ToolsAPIUtils,
  Dialogs,
  BADI.DockableModuleExplorer,
  Windows,
  Forms,
  BADI.Options,
  Controls;

(**

  This method checks the filename for changes.

  @precon  Editor must be a valid instance.
  @postcon Set the timer to update immediate if the file have changed.

  @param   Editor as an IOTASourceEditor as a constant

**)
Procedure TEditorNotifier.CheckFileNameAndSize(Const Editor : IOTASourceEditor);

Begin
  If Assigned(Editor) Then
    If Editor.FileName <> FLastEditorName Then
      Begin
        FLastUpdateTickCount := 1;
        FLastEditorName := Editor.FileName;
      End
End;

(**

  This method checks for cursor movement when the timer is called.

  @precon  Editor must be a valid instance.
  @postcon If the cursor has moved since last time the cursor position is updated and the
           FLastUpdateTickCount is also updated.

  @param   Editor as an IOTASourceEditor as a constant

**)
Procedure TEditorNotifier.CheckForCursorMovement(Const Editor : IOTASourceEditor);

Var
  P : TOTAEditPos;

Begin
  If Assigned(Editor) Then
    Begin
      If Editor.GetEditViewCount > 0 Then
        P := Editor.GetEditView(0).CursorPos;
      If FLastUpdateTickCount > 0 Then
        If (P.Col <> FLastCursorPos.Col) Or (P.Line <> FLastCursorPos.Line) Then
          Begin
            FLastUpdateTickCount := GetTickCount;
            FLastCursorPos := P;
          End;
    End;
End;

(**

  This is the constructor method for the TEditorNotifier class.

  @precon  None.
  @postcon Initialise the class be creating a time for handling editor changes.

**)
constructor TEditorNotifier.Create;

Const
  iUpdateInterval = 100;
  
begin
  Inherited Create;
  FBADIThreadMgr := TBrowseAndDocItThreadManager.Create;
  FUpdateTimer := TTimer.Create(Nil);
  FUpdateTimer.Interval := iUpdateInterval;
  FUpdateTimer.OnTimer := TimerEventHandler;
  FLastParserResult := True;
  FUpdateTimer.Enabled := True;
  FLastUpdateTickCount := 1; // Cause immediate parsing of the current file.
end;

(**

  This is the destructor method for the TEditorNotifier class.

  @precon  None.
  @postcon Frees the timer control.

**)
destructor TEditorNotifier.Destroy;
begin
  FupdateTimer.Enabled := False;
  FUpdateTimer.OnTimer := Nil;
  FUpdateTimer.Free;
  FBADIThreadMgr.Free;
  Inherited;
end;

(**

  This an impementation of the DockFormRefresh method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @nohint  EditWindow DockForm
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow as an INTAEditWindow as a constant
  @param   DockForm   as a TDockableForm

**)
procedure TEditorNotifier.DockFormRefresh(Const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
end;

(**

  This an impementation of the DockFormUpdate method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @nohint  EditWindow DockForm
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow as an INTAEditWindow as a constant
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

  @nohint  EditWindow DockForm
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow as an INTAEditWindow as a constant
  @param   DockForm   as a TDockableForm

**)
procedure TEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
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

Const
  strDefines = 'Defines';
  strWIN32 = 'WIN32';
  strWIN64 = 'WIN64';
  strMSWINDOWS = 'MSWINDOWS';
  {$IFDEF VER120} // Delphi 4
  strCompilerVersion = 'VER120');
  {$ENDIF}
  {$IFDEF VER130} // Delphi 5
  strCompilerVersion = 'VER130';
  {$ENDIF}
  {$IFDEF VER140} // Delphi 6
  strCompilerVersion = 'VER140';
  {$ENDIF}
  {$IFDEF VER150} // Delphi 7
  strCompilerVersion = 'VER150';
  {$ENDIF}
  {$IFDEF VER160} // Delphi for .NET
  strCompilerVersion = 'VER160';
  {$ENDIF}
  {$IFDEF VER170} // Delphi 2005
  strCompilerVersion = 'VER170';
  {$ENDIF}
  {$IFDEF VER180} // Delphi 2006
  strCompilerVersion = 'VER180';
  {$ENDIF}
  {$IFDEF VER190} // Delphi 2007
  strCompilerVersion = 'VER190';
  {$ENDIF}
  {$IFDEF VER200} // Delphi 2009
  strCompilerVersion = 'VER200';
  {$ENDIF}
  {$IFDEF VER210} // Delphi 2010
  strCompilerVersion = 'VER210';
  {$ENDIF}
  {$IFDEF VER220} // Delphi XE
  strCompilerVersion = 'VER220';
  {$ENDIF}
  {$IFDEF VER230} // Delphi XE2
  strCompilerVersion = 'VER230';
  {$ENDIF}
  {$IFDEF VER240} // Delphi XE3
  strCompilerVersion = 'VER240';
  {$ENDIF}
  {$IFDEF VER250} // Delphi XE4
  strCompilerVersion = 'VER250';
  {$ENDIF}
  {$IFDEF VER260} // Delphi XE5
  strCompilerVersion = 'VER260';
  {$ENDIF}
  {$IFDEF VER270} // Delphi XE6
  strCompilerVersion = 'VER270';
  {$ENDIF}
  {$IFDEF VER280} // Delphi XE7
  strCompilerVersion = 'VER280';
  {$ENDIF}
  {$IFDEF VER290} // Delphi XE8
  strCompilerVersion = 'VER290';
  {$ENDIF}
  {$IFDEF VER300} // Delphi XE10 Seattle
  strCompilerVersion = 'VER300';
  {$ENDIF}
  {$IFDEF VER310} // Delphi XE10.1 Berlin
  strCompilerVersion = 'VER310';
  {$ENDIF}
  {$IFDEF VER320} // Delphi XE10.2 Tokyo
  strCompilerVersion = 'VER320';
  {$ENDIF}
  {$IFDEF VER330} // Delphi XE10.3 Carnival
  strCompilerVersion = 'VER320';
  {$ENDIF}
  {$IFNDEF D0001}
    {$MESSAGE ERROR 'The Condition Definitions need to be updated!!!!!'}
  {$ENDIF}

  (**

    This procedure adds the standard compiler defintions to the definiton list for parsing files.

    @precon  Project must be a valid instance.
    @postcon The standard compiler definitions are added the definitions list for parsing.

    @param   Project as an IOTAProject as a constant

  **)
  Procedure SetStandardCompilerDefs(Const Project : IOTAProject);

  Var
    strPlatform: String;
    ProjOpsConfigs : IOTAProjectOptionsConfigurations;
    
  Begin
    TBADIOptions.BADIOptions.Defines.Add(strCompilerVersion);
    {$IFDEF DXE20}
    If Assigned(Project) Then
      If Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, ProjOpsConfigs) Then
        Begin
          strPlatform := UpperCase(ProjOpsConfigs.ActiveConfiguration.Platform);
          TBADIOptions.BADIOptions.Defines.Add(strPlatform);
          If (strPlatform = strWIN32) Or (strPlatform = strWIN64) Then
            TBADIOptions.BADIOptions.Defines.Add(strMSWINDOWS);
        End;
    {$ELSE}
    TBADIOptions.BADIOptions.Defines.Add(strWIN32);
    TBADIOptions.BADIOptions.Defines.Add(sreMSWINDOWS);
    {$ENDIF}
  
  End;
  
Var
  SE : IOTASourceEditor;
  Options : IOTAProjectOptions;
  Project : IOTAProject;

begin
  Result := '';
  strFileName := '';
  boolModified := False;
  SE := TBADIToolsAPIFunctions.ActiveSourceEditor;
  If Assigned(SE) Then
    Begin
      strFileName := SE.FileName;
      boolModified := SE.Modified;
      Result := TBADIToolsAPIFunctions.EditorAsString(SE);
      Project := TBADIToolsAPIFunctions.ActiveProject;
      If Assigned(Project) Then
        Begin
          Options := Project.ProjectOptions;
          TBADIOptions.BADIOptions.Defines.Text :=
            StringReplace(Options.Values[strDefines], ';', #13#10,
            [rfReplaceAll]);
          SetStandardCompilerDefs(Project);
        End;
    End;
end;

(**

  This an impementation of the EditorViewActivate method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Refreshes the module explorer IF the last parser was sucessful.

  @nohint  EditWindow EditView

  @param   EditWindow as an INTAEditWindow as a constant
  @param   EditView   as an IOTAEditView as a constant

**)
Procedure TEditorNotifier.EditorViewActivated(Const EditWindow: INTAEditWindow;
  Const EditView: IOTAEditView);

Begin
  FUpdateTimer.Enabled := True;
  FLastParserResult := True;
  FLastUpdateTickCount := 1;
End;

(**

  This an impementation of the EditorViewModified method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Logs the last time the editor was updated.

  @nohint  EditWindow EditView

  @param   EditWindow as an INTAEditWindow as a constant
  @param   EditView   as an IOTAEditView as a constant

**)
procedure TEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  FLastUpdateTickCount := GetTickCount;
end;

(**

  This method reenabled the timer and returns whether the parse failed or not.

  @precon  None.
  @postcon Reenabled the timer and returns whether the parse failed or not.

  @param   boolSuccessfulParse as a Boolean as a constant

**)
Procedure TEditorNotifier.EnableTimer(Const boolSuccessfulParse : Boolean);

begin
  FUpdateTimer.Enabled := True;
  FLastParserResult := boolSuccessfulParse;
end;

(**

  This method displays an exception message in a dialogue box.

  @precon  None.
  @postcon Displays an exception message in a dialogue box.

  @param   strExceptionMsg as a String as a Constant

**)
procedure TEditorNotifier.ExceptionMsg(Const strExceptionMsg: String);

begin
  ShowMessage(strExceptionMsg);
end;

(**

  This method renders the given module in the module explorer window.

  @precon  None.
  @postcon Renders the given module in the module explorer window.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TEditorNotifier.RenderDocument(Const Module: TBaseLanguageModule);
begin
  TfrmDockableModuleExplorer.RenderDocumentTree(Module);
end;

(**

  This method resets the last update tick count with zero or an optional new value.

  @precon  None.
  @postcon Resets the last update tick count with zero or an optional new value.

  @param   iNewValue as an Integer as a constant

**)
procedure TEditorNotifier.ResetLastUpdateTickCount(Const iNewValue : Integer = 0);

begin
  FLastUpdateTickCount := iNewValue;
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

ResourceString
  strMsg = 'The last parse of the source code failed. Do you want to re-parse the code?';

Var
  Editor : IOTASourceEditor;

begin
  Editor := TBADIToolsAPIFunctions.ActiveSourceEditor;
  CheckForCursorMovement(Editor);
  CheckFileNameAndSize(Editor);
  If (FLastUpdateTickCount > 0) And
    (GetTickCount > FLastUpdateTickCount + TBADIOptions.BADIOptions.UpdateInterval) Then
    Begin
      If Assigned(Application) And Assigned(Application.MainForm) And Application.MainForm.Visible Then
        Begin
          FLastUpdateTickCount := 0;
          FUpdateTimer.Enabled := False;
          If Not FLastParserResult Then
            If MessageDlg(strMsg, mtWarning, [mbYes, mbNo, mbCancel], 0) <> mrYes Then
              Exit;
          FBADIThreadMgr.Parse(EnableTimer, EditorInfo, RenderDocument, ExceptionMsg);
        End;
    End;
end;

(**

  This an impementation of the WindowActivated method for the Editor Notifier
  interface.

  @nohint  EditWindow
  @nocheck EmptyMethod

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow as a constant

**)
procedure TEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
end;

(**

  This an impementation of the WindowCommand method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @nohint  EditWindow Command Param Handled
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow as an INTAEditWindow as a constant
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

  @nohint  EditWindow Operation
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow as an INTAEditWindow as a constant
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

  @nohint  EditWindow Show LoadedFromDesktop
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow        as an INTAEditWindow as a constant
  @param   Show              as a Boolean
  @param   LoadedFromDesktop as a Boolean

**)
procedure TEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
end;

End.
