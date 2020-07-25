(**

  This module contains an editor notifier that monitors the code for changes
  and in turn refreshes the module explorer.

  @Author  David Hoyle
  @Version 2.259
  @Date    25 Jul 2020

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
  BADI.Interfaces,
  BADI.CommonIDEFunctions;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class handles notifications from the editor so that changes in the
      editor can be displayed. **)
  TEditorNotifier = Class(TNotifierObject, IUnknown, IOTANotifier, INTAEditServicesNotifier)
  Strict Private
    FUpdateTimer         : TTimer;
    FLastEditorName      : String;
    FLastCursorPos       : TOTAEditPos;
    FLastParserResult    : Boolean;
    FLastUpdateTickCount : Cardinal;
    FLastMoveTickCount   : Cardinal;
    FBADIThreadMgr       : TBrowseAndDocItThreadManager;
    FModuleStatsList     : IBADIModuleStatsList;
    FSource              : String;
  Strict Protected
    Procedure EnableTimer(Const boolSuccessfulParse : Boolean);
    Procedure TimerEventHandler(Sender : TObject);
    Function  EditorInfo(var strFileName : String; var boolModified : Boolean) : String;
    Procedure RenderDocument(Const Module: TBaseLanguageModule);
    Procedure ExceptionMsg(Const strExceptionMsg : String);
    Function CheckForCursorMovement(Const Editor : IOTASourceEditor) : TOTAEditPos;
    Procedure CheckFileNameAndSize(Const Editor : IOTASourceEditor);
    procedure CheckForChange(const iUpdateInterval: Cardinal);
    procedure CheckForMovement(const iUpdateInterval: Cardinal; const CP: TOTAEditPos);
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
    procedure UpdateProjectDictionary;
  Public
    Constructor Create(Const ModuleStatsList : IBADIModuleStatsList);
    Destructor Destroy; Override;
    Procedure ResetLastUpdateTickCount(Const iNewValue : Integer = 0);
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Dialogs,
  Windows,
  Forms,
  Controls,
  BADI.ToolsAPIUtils,
  BADI.DockableModuleExplorer,
  BADI.Options,
  BADI.Types,
  BADI.DocIssuesHintWindow,
  BADI.EditViewNotifier;

(**

  This method checks the filename for changes.

  @precon  Editor must be a valid instance.
  @postcon Set the timer to update immediate if the file have changed.

  @param   Editor as an IOTASourceEditor as a constant

**)
Procedure TEditorNotifier.CheckFileNameAndSize(Const Editor : IOTASourceEditor);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckFileNameAndSize', tmoTiming);{$ENDIF}
  If Assigned(Editor) Then
    If Editor.FileName <> FLastEditorName Then
      Begin
        FLastUpdateTickCount := 1;
        FLastEditorName := Editor.FileName;
      End
End;

(**

  This method checks for any change in the file.

  @precon  None.
  @postcon If there is change the module is parsed.

  @param   iUpdateInterval as a Cardinal as a constant

**)
Procedure TEditorNotifier.CheckForChange(Const iUpdateInterval: Cardinal);

ResourceString
  strMsg = 'The last parse of the source code failed. Do you want to re-parse the code?';

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckForChange', tmoTiming);{$ENDIF}
  If (FLastUpdateTickCount > 0) And (GetTickCount > FLastUpdateTickCount + iUpdateInterval) Then
    Begin
      If Assigned(Application) And Assigned(Application.MainForm) And Application.MainForm.Visible Then
        Begin
          FLastUpdateTickCount := 0;
          FUpdateTimer.Enabled := False;
          If Not FLastParserResult Then
            Case MessageDlg(strMsg, mtWarning, [mbYes, mbNo, mbCancel], 0) Of
              mrYes: FLastParserResult := True;
              mrNo: Exit;
              mrCancel: Abort;
            End;
          UpdateProjectDictionary;
          FBADIThreadMgr.Parse(EnableTimer, EditorInfo, RenderDocument, ExceptionMsg);
      End;
    End;
End;

(**

  This method checks for cursor movement when the timer is called.

  @precon  Editor must be a valid instance.
  @postcon If the cursor has moved since last time the cursor position is updated and the 
           FLastUpdateTickCount is also updated.

  @param   Editor as an IOTASourceEditor as a constant
  @return  a TOTAEditPos

**)
Function TEditorNotifier.CheckForCursorMovement(Const Editor : IOTASourceEditor) : TOTAEditPos;

Var
  EditorSvcs : IOTAEditorServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckForCursorMovement', tmoTiming);{$ENDIF}
  If Assigned(Editor) Then
    Begin
      If Editor.GetEditViewCount > 0 Then
        If Supports(BorlandIDEServices, IOTAEditorServices, EditorSvcs) Then
          Result := EditorSvcs.TopView.CursorPos;
      If (Result.Col <> FLastCursorPos.Col) Or (Result.Line <> FLastCursorPos.Line) Then
        Begin
          FLastCursorPos := Result;
          If FLastUpdateTickCount > 0 Then
            FLastUpdateTickCount := GetTickCount;
          FLastMoveTickCount := GetTickCount;
        End;
    End;
End;

(**

  This method checks to see of there has been movement of the cursor sand if so the follow cursor and
  update the display of the doc issue hint window.

  @precon  None.
  @postcon If there has been movement in the cursor the follow cursor is triggers and the hint window
           updated.

  @param   iUpdateInterval as a Cardinal as a constant
  @param   CP              as a TOTAEditPos as a constant

**)
Procedure TEditorNotifier.CheckForMovement(Const iUpdateInterval: Cardinal; Const CP: TOTAEditPos);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckForMovement', tmoTiming);{$ENDIF}
  If (FLastMoveTickCount > 0) And (GetTickCount > FLastMoveTickCount + iUpdateInterval) Then
    Begin
      If doFollowEditorCursor In TBADIOptions.BADIOptions.Options Then
        TfrmDockableModuleExplorer.FollowEditorCursor(CP.Line);
      TBADIDocIssueHintWindow.Display(TfrmDockableModuleExplorer.DocIssueTotals);
      FLastMoveTickCount := 0;
    End;
End;

(**

  This is the constructor method for the TEditorNotifier class.

  @precon  None.
  @postcon Initialise the class be creating a time for handling editor changes.

  @param   ModuleStatsList as an IBADIModuleStatsList as a constant

**)
Constructor TEditorNotifier.Create(Const ModuleStatsList : IBADIModuleStatsList);

Const
  iUpdateInterval = 100;
  
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create;
  FModuleStatsList := ModuleStatsList;
  FBADIThreadMgr := TBrowseAndDocItThreadManager.Create;
  FUpdateTimer := TTimer.Create(Nil);
  FUpdateTimer.Interval := iUpdateInterval;
  FUpdateTimer.OnTimer := TimerEventHandler;
  FLastParserResult := True;
  FUpdateTimer.Enabled := True;
  FLastUpdateTickCount := 1; // Cause immediate parsing of the current file.
  FLastMoveTickCount := 0;
end;

(**

  This is the destructor method for the TEditorNotifier class.

  @precon  None.
  @postcon Frees the timer control.

**)
Destructor TEditorNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FUpdateTimer.Enabled := False;
  FUpdateTimer.OnTimer := Nil;
  FUpdateTimer.Free;
  FBADIThreadMgr.Free;
  Inherited;
End;

(**

  This an implementation of the DockFormRefresh method for the Editor Notifier
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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DockFormRefresh', tmoTiming);{$ENDIF}
end;

(**

  This an implementation of the DockFormUpdated method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Not used.

  @nohint  EditWindow DockForm
  @nocheck MissingCONSTInParam EmptyMethod

  @param   EditWindow as an INTAEditWindow as a constant
  @param   DockForm   as a TDockableForm

**)
Procedure TEditorNotifier.DockFormUpdated(Const EditWindow: INTAEditWindow; DockForm: TDockableForm);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DockFormUpdated', tmoTiming);{$ENDIF}
  FUpdateTimer.Enabled := True;
  FLastParserResult := True;
  FLastUpdateTickCount := 1;
  FLastMoveTickCount := 1;
End;

(**

  This an implementation of the DockFormVisibleChanged method for the Editor Notifier
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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'DockFormVisibleChanged', tmoTiming);{$ENDIF}
end;

(**

  This method extracts the filename, modified status and the editor stream of
  code for the Browse And Doc It thread.

  @precon  None.
  @postcon Extracts the filename, modified status and the editor stream of
           code for the Browse And Doc It thread.

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
  {$IFDEF VER330} // Delphi XE10.3 Rio
  strCompilerVersion = 'VER330';
  {$ENDIF}
  {$IFDEF VER340} // Delphi XE10.4 Denali
  strCompilerVersion = 'VER340';
  {$ENDIF}
  {$IFNDEF D0001}
    {$MESSAGE ERROR 'The Condition Definitions need to be updated!!!!!'}
  {$ENDIF}

  (**

    This procedure adds the standard compiler definitions to the definition list for parsing files.

    @precon  Project must be a valid instance.
    @postcon The standard compiler definitions are added the definitions list for parsing.

    @param   Project as an IOTAProject as a constant

  **)
  Procedure SetStandardCompilerDefs(Const Project : IOTAProject);

  Var
    strPlatform: String;
    ProjOpsConfigs : IOTAProjectOptionsConfigurations;
    
  Begin
    {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EditorInfo/SetStandardCompilerDefs', tmoTiming);{$ENDIF}
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
    TBADIOptions.BADIOptions.Defines.Add(strMSWINDOWS);
    {$ENDIF}
  
  End;
  
Var
  SE : IOTASourceEditor;
  Options : IOTAProjectOptions;
  Project : IOTAProject;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EditorInfo', tmoTiming);{$ENDIF}
  Result := '';
  strFileName := '';
  boolModified := False;
  SE := TBADIToolsAPIFunctions.ActiveSourceEditor;
  If Assigned(SE) Then
    Begin
      strFileName := SE.FileName;
      boolModified := SE.Modified;
      Result := TBADIToolsAPIFunctions.EditorAsString(SE);
      FSource := Result;
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

  This an implementation of the EditorViewActivated method for the Editor Notifier
  interface.

  @precon  None.
  @postcon Refreshes the module explorer IF the last parser was successful.

  @nohint  EditWindow EditView

  @param   EditWindow as an INTAEditWindow as a constant
  @param   EditView   as an IOTAEditView as a constant

**)
Procedure TEditorNotifier.EditorViewActivated(Const EditWindow: INTAEditWindow;
  Const EditView: IOTAEditView);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EditorViewActivated', tmoTiming);{$ENDIF}
  FUpdateTimer.Enabled := True;
  FLastParserResult := True;
  FLastUpdateTickCount := 1;
  FLastMoveTickCount := 1;
End;

(**

  This an implementation of the EditorViewModified method for the Editor Notifier
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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EditorViewModified', tmoTiming);{$ENDIF}
  FLastUpdateTickCount := GetTickCount;
end;

(**

  This method re-enables the timer and returns whether the parse failed or not.

  @precon  None.
  @postcon Re-enables the timer and returns whether the parse failed or not.

  @param   boolSuccessfulParse as a Boolean as a constant

**)
Procedure TEditorNotifier.EnableTimer(Const boolSuccessfulParse : Boolean);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EnableTimer', tmoTiming);{$ENDIF}
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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ExceptionMsg', tmoTiming);{$ENDIF}
  ShowMessage(strExceptionMsg);
end;

(**

  This method renders the given module in the module explorer window.

  @precon  None.
  @postcon Renders the given module in the module explorer window.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TEditorNotifier.RenderDocument(Const Module: TBaseLanguageModule);

Var
  EditorSvcs : IOTAEditorServices;
  
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'RenderDocument', tmoTiming);{$ENDIF}
  TfrmDockableModuleExplorer.RenderDocumentTree(Module);
  If Supports(BorlandIDEServices, IOTAEditorServices, EditorSvcs) Then
    Begin
      If doFollowEditorCursor In TBADIOptions.BADIOptions.Options Then
          If Assigned(EditorSvcs.TopView) Then
            TfrmDockableModuleExplorer.FollowEditorCursor(EditorSvcs.TopView.CursorPos.Line);
      If Assigned(EditorSvcs.TopView) Then
        Begin
          {$IFDEF DXE100}
          TBADIEditViewNotifier.ForceFullRepaint;
          {$ENDIF DXE100}
          EditorSvcs.TopView.Paint;
        End;
    End;
end;

(**

  This method resets the last update tick count with zero or an optional new value.

  @precon  None.
  @postcon Resets the last update tick count with zero or an optional new value.

  @param   iNewValue as an Integer as a constant

**)
procedure TEditorNotifier.ResetLastUpdateTickCount(Const iNewValue : Integer = 0);

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ResetLastUpdateTickCount', tmoTiming);{$ENDIF}
  FLastUpdateTickCount := iNewValue;
end;

(**

  This is an on Timer event handler.

  @precon  None.
  @postcon Checks to see if the last time the editor was changes is beyond the
           wait time for the module to be updated. Creates an instance of the
           thread to render the module explorer.

  @param   Sender as a TObject

**)
procedure TEditorNotifier.TimerEventHandler(Sender: TObject);

Var
  Editor : IOTASourceEditor;
  iUpdateInterval: Cardinal;
  CP: TOTAEditPos;

begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'TimerEventHandler', tmoTiming);{$ENDIF}
  iUpdateInterval := TBADIOptions.BADIOptions.UpdateInterval;
  Editor := TBADIToolsAPIFunctions.ActiveSourceEditor;
  CP := CheckForCursorMovement(Editor);
  CheckFileNameAndSize(Editor);
  CheckForChange(iUpdateInterval);
  CheckForMovement(iUpdateInterval, CP);
  If Assigned(Application) And Assigned(Application.MainForm) And Application.MainForm.Visible Then
    If Not Application.Active Then
      TBADIDocIssueHintWindow.Disappear;
end;

(**

  This method updates the project dictionary filename.

  @precon  None.
  @postcon The project dictionary filename is updated.

**)
Procedure TEditorNotifier.UpdateProjectDictionary;

Const
  strDctExt = '.dct';

Var
  MS: IOTAModuleServices;
  P: IOTAProject;
  
Begin
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      P := MS.GetActiveProject;
      If Assigned(P) Then
        TBADIOptions.BADIOptions.ProjectDictionaryFile := ChangeFileExt(P.FileName, strDctExt);
    End;
End;

(**

  This an implementation of the WindowActivated method for the Editor Notifier
  interface.

  @nohint  EditWindow
  @nocheck EmptyMethod

  @precon  None.
  @postcon Not used.

  @param   EditWindow as an INTAEditWindow as a constant

**)
procedure TEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WindowActivated', tmoTiming);{$ENDIF}
end;

(**

  This an implementation of the WindowCommand method for the Editor Notifier
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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WindowCommand', tmoTiming);{$ENDIF}
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
Procedure TEditorNotifier.WindowNotification(Const EditWindow: INTAEditWindow; Operation: TOperation);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WindowNotification', tmoTiming);{$ENDIF}
End;

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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WindowShow', tmoTiming);{$ENDIF}
end;

End.
