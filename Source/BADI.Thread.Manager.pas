(**

  This module contains a custom thread for parsing the various modules along with a thread manager for
  creating the thread.

  @Author  David Hoyle
  @Version 1.179
  @Date    03 May 2021

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2021  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.Thread.Manager;

Interface

uses
  System.Classes,
  {$IFDEF EUREKALOG}
  EBase,
  {$ENDIF EUREKALOG}
  BADI.Interfaces,
  BADI.Base.Module,
  BADI.Types;

Type
  (** This is a procedure to return the success of the parse in the thread. **)
  TBADIParserNotify = Procedure(Const boolSuccessfulParse: Boolean) Of Object;
  (** This is a procedure to allow the thread to get information from the
      calling IDE. **)
  TBADIEditorInformation = Function(Var strFileName: String; Var boolModified: Boolean)
    : String Of Object;
  (** This is a procedure to allow the thread to render the module in the
      calling IDEs main thread. **)
  TBADIRenderDocumentTree = Procedure(Const Module: TBaseLanguageModule) Of Object;
  (** This is a procedure to allow the thread to display an error message in
      the calling IDEs main thread. **)
  TBADIThreadExceptionMsg = Procedure(Const strExceptionMsg: String) Of Object;
  (**

    This is a class to manage thread used to parse code. Its main aim is
    to ensure that only 1 thread is active at a time and provides a mechanism
    to wait for the working thread to finish.

  **)
  TBADIThreadManager = Class
  Strict Private
    FRenderDocumentTree   : TBADIRenderDocumentTree;
    FThread               : TThread;
    FSuccessfulParseProc  : TBADIParserNotify;
    FThreadExceptionMsg   : TBADIThreadExceptionMsg;
  Strict Protected
    Procedure TerminateThread(Sender: TObject);
  Public
    Constructor Create(
      Const SuccessfulParseProc: TBADIParserNotify;
      Const RenderDocumentTree: TBADIRenderDocumentTree;
      Const ThreadExceptionMsg: TBADIThreadExceptionMsg
    );
    Destructor Destroy; Override;
    Function Parse(Const EditorInfo: TBADIEditorInformation): Boolean;
    Procedure WaitForThreadToFinish;
  End;

  (** This class defines a thread in which the parsing of the code and
      rendering of the module explorer is done. **)
  TBADIThread = Class( {$IFDEF EUREKALOG} TThreadEx {$ELSE} TThread {$ENDIF EUREKALOG} )
  Strict Private
    FModule             : TBaseLanguageModule;
    FSource             : String;
    FFileName           : String;
    FModified           : Boolean;
    FRenderDocumentTree : TBADIRenderDocumentTree;
    FThreadExceptionMsg : TBADIThreadExceptionMsg;
    FSuccessfulParse    : Boolean;
  Strict Protected
    Procedure Execute; Override;
    Procedure RenderModuleExplorer;
    Procedure ShowException;
  Public
    Constructor CreateBADIThread(
      Const EditorInfo: TBADIEditorInformation;
      Const RenderDocumentTree: TBADIRenderDocumentTree;
      Const ThreadExceptionMsg: TBADIThreadExceptionMsg;
      Const TerminateThread: TNotifyEvent
    );
    Destructor Destroy; Override;
    (**
      This property gets and sets the SuccessfulParse variable of the thread.
      @precon  None.
      @postcon Gets and sets the SuccessfulParse variable of the thread.
      @return  a Boolean
    **)
    Property SuccessfulParse: Boolean Read FSuccessfulParse Write FSuccessfulParse;
  End;

Implementation

uses
  System.SysUtils,
  BADI.Module.Dispatcher;

(**

  A constructor for the TBADIThreadManager class.

  @precon  None.
  @postcon Initialises the thread variable to null.

  @param   SuccessfulParseProc as a TBADIParserNotify as a constant
  @param   RenderDocumentTree  as a TBADIRenderDocumentTree as a constant
  @param   ThreadExceptionMsg  as a TBADIThreadExceptionMsg as a constant

**)
Constructor TBADIThreadManager.Create(
              Const SuccessfulParseProc: TBADIParserNotify;
              Const RenderDocumentTree: TBADIRenderDocumentTree;
              Const ThreadExceptionMsg: TBADIThreadExceptionMsg
            );

Begin
  FThread := Nil;
  FSuccessfulParseProc := SuccessfulParseProc;
  FRenderDocumentTree := RenderDocumentTree;
  FThreadExceptionMsg := ThreadExceptionMsg;
End;

(**

  A destructor for the TBADIThreadManager class.

  @precon  None.
  @postcon Terminate any working thread.

**)
Destructor TBADIThreadManager.Destroy;

Begin
  If FThread <> Nil Then
    FThread.Terminate;
  Inherited Destroy;
End;

(**

  This method parses the given code reference ONLY IF there is no current parsing thread.

  @precon  None.
  @postcon Parses the given code reference ONLY IF there is no current parsing thread.

  @param   EditorInfo as a TBADIEditorInformation as a constant
  @return  a Boolean

**)
Function TBADIThreadManager.Parse(Const EditorInfo: TBADIEditorInformation): Boolean;

Begin
  Result := False;
  If FThread = Nil Then
    Begin
      FThread := TBADIThread.CreateBADIThread(
        EditorInfo,
        FRenderDocumentTree,
        FThreadExceptionMsg,
        TerminateThread
      );
      Result := True;
    End;
End;

(**

  This method is an on terminate event handler for threads.

  @precon  None.
  @postcon Called by the freeing thread which sets the thread variable to nil.

  @param   Sender as a TObject

**)
Procedure TBADIThreadManager.TerminateThread(Sender: TObject);

Begin
  If Assigned(FThread) Then
    If Assigned(FThread.FatalException) Then
      Begin
        {$IFDEF EUREKALOG}
        HandleException(FThread.FatalException);
        {$ELSE}
        If FThread.FatalException Is Exception Then
          ShowException(FThread.FatalException, Nil);
        {$ENDIF}
      End;
  FThread := Nil;
  If Assigned(FSuccessfulParseProc) Then
    If Sender Is TBADIThread Then
      FSuccessfulParseProc((Sender As TBADIThread).SuccessfulParse);
End;

(**

  This method waits for the thread to finish if a thread is active.

  @precon  None.
  @postcon Waits for the thread to finish.

**)
Procedure TBADIThreadManager.WaitForThreadToFinish;

Begin
  If Assigned(FThread) Then
    FThread.WaitFor;
End;

(**

  This is a constructor for the TBADIThread class.

  @precon  None.
  @postcon Creates a suspended thread and sets up a stream with the contents of the active editor and
           then resumed the thread in order to parse the contents.

  @param   EditorInfo         as a TBADIEditorInformation as a constant
  @param   RenderDocumentTree as a TBADIRenderDocumentTree as a constant
  @param   ThreadExceptionMsg as a TBADIThreadExceptionMsg as a constant
  @param   TerminateThread    as a TNotifyEvent as a constant

**)
Constructor TBADIThread.CreateBADIThread(
              Const EditorInfo: TBADIEditorInformation;
              Const RenderDocumentTree: TBADIRenderDocumentTree;
              Const ThreadExceptionMsg: TBADIThreadExceptionMsg;
              Const TerminateThread: TNotifyEvent
            );

Begin
  FSuccessfulParse    := False;
  FreeOnTerminate     := True; // Self Freeing...
  FRenderDocumentTree := RenderDocumentTree;
  FThreadExceptionMsg := ThreadExceptionMsg;
  OnTerminate         := TerminateThread;
  FSource             := '';
  If Assigned(EditorInfo) Then
    FSource := EditorInfo(FFileName, FModified);
  Inherited Create(False);
  //NameThreadForDebugging();
End;

(**

  This is a destructor for the TBADIThread class.

  @precon  None.
  @postcon Frees the stream memory.

**)
Destructor TBADIThread.Destroy;

Begin
  Inherited Destroy;
End;

(**

  This execute method parses the code of the active editor stored in the
  memory stream and render the information in the explorer module.

  @precon  None.
  @postcon Parses the code of the active editor stored in the memory stream and
           render the information in the explorer module.

  @nometric ExceptionEating

**)
Procedure TBADIThread.Execute;

{$IFDEF EUREKALOG}
Const
  strBrowseAndDocItParsingThread = 'BrowseAndDocItParsingThread';
{$ENDIF EUREKALOG}

Begin
  Try
    {$IFDEF EUREKALOG}
    NameThread(strBrowseAndDocItParsingThread);
    SetEurekaLogStateInThread(0, True);
    {$ENDIF EUREKALOG}
    If FFileName <> '' Then
      FModule := TBADIDispatcher.BADIDispatcher.Dispatcher(FSource, FFileName, FModified,
        [moParse, moCheckForDocumentConflicts])
    Else
      FModule := Nil;
    Try
      If Terminated Then
        Exit;
      Synchronize(RenderModuleExplorer);
      FSuccessfulParse := True;
    Finally
      FModule.Free;
    End;
  Except
    On E: EBADIParserAbort Do
      Exit;
  End;
End;

(**

  This method synchronises with the main IDE thread and renders the module
  explorer.

  @precon  FModule must be a valid TBaseLanguageModule instance.
  @postcon Synchronises with the main IDE thread and renders the module
           explorer.

**)
Procedure TBADIThread.RenderModuleExplorer;

Begin
  If Assigned(FRenderDocumentTree) Then
    FRenderDocumentTree(FModule);
End;

(**

  This method displays the raised exception message pass via the FFileName
  field.

  @precon  None.
  @postcon Displays the raised exception message pass via the FFileName
           field.

**)
Procedure TBADIThread.ShowException;

Const
  strMsg =
    'Exception in TBrowseAndDocItThread:'#13#10 +
    '  Exception: %s';
Begin
  If Assigned(FThreadExceptionMsg) Then
    FThreadExceptionMsg(Format(strMsg, [FFileName]));
End;

End.
