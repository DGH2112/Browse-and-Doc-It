(**

  This module provides a few Open Tools API general method that are used
  throughout this project.

  @Date    21 Oct 2008
  @Version 1.0
  @Author  David Hoyle

**)
unit ToolsAPIUtils;

interface

Uses
  SysUtils, Windows, ToolsAPI, Classes;

Type
  (** This is an enumerate for the types of messages that can be cleared. **)
  TClearMessage = (cmCompiler, cmSearch, cmTool);
  (** This is a set of messages that can be cleared. **)
  TClearMessages = Set of TClearMessage;

  Function ProjectGroup: IOTAProjectGroup;
  Function ActiveProject : IOTAProject;
  Function ProjectModule(Project : IOTAProject) : IOTAModule;
  Function ActiveSourceEditor : IOTASourceEditor;
  Function SourceEditor(Module : IOTAModule) : IOTASourceEditor;
  Procedure OutputMessage(strText : String); Overload;
  Procedure OutputMessage(strFileName, strText, strPrefix : String; iLine, iCol : Integer); Overload;
  Procedure ClearMessages(Msg : TClearMessages);
  Function BufferSize(SourceEditor : IOTASourceEditor) : Integer;
  Function EditorAsMemoryStream(SourceEditor : IOTASourceEditor) : TMemoryStream;

Const
  (** The buffer size for the copying of text from an editor to a memory
  stream. **)
  iBufferSize = 4096;

Var
  (** This is a character buffer for the transfer of text from the editor
      to the parser. **)
  Buffer : Array[1..iBufferSize] Of Char;

Implementation

(**

  This method returns the project group in the Delphi IDE.

  @precon  None.
  @postcon Returns the current project group.

  @return  an IOTAProjectGroup

**)
Function ProjectGroup: IOTAProjectGroup;

Var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProjectGroup: IOTAProjectGroup;

Begin
  Result := Nil;
  AModuleServices := (BorlandIDEServices as IOTAModuleServices);
  For i := 0 To AModuleServices.ModuleCount - 1 Do
    Begin
      AModule := AModuleServices.Modules[i];
      If (AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK) Then
       Break;
    End;
  Result := AProjectGroup;
  AModuleServices := Nil;
  AModule := Nil;
  AProjectGroup := Nil;
end;

(**

  This method returns the active project in the Delphi IDE.

  @precon  None.
  @postcon Returns the active project in the active project group.

  @return  an IOTAProject

**)
Function ActiveProject : IOTAProject;

var
  G : IOTAProjectGroup;

Begin
  Result := Nil;
  G := ProjectGroup;
  If G <> Nil Then
    Result := G.ActiveProject;
End;

(**

  This method returns the active source code editor in the Delphi IDE.

  @precon  None. 
  @postcon Returns the active source editor.

  @return  an IOTASourceEditor

**)
Function ActiveSourceEditor : IOTASourceEditor;

Begin
  Result := SourceEditor((BorlandIDEServices as IOTAModuleServices).CurrentModule);
End;

(**

  This method returns the source code editor for the passed module.

  @precon  Module is the module for which a source ditor interface is required.
  @postcon Returns the source editor interface for the given module.

  @param   Module as an IOTAMOdule
  @return  an IOTASourceEditor

**)
Function SourceEditor(Module : IOTAMOdule) : IOTASourceEditor;

Var
  iFileCount : Integer;
  i : Integer;

Begin
  Result := Nil;
  If Module = Nil Then Exit;
  With Module Do
    Begin
      iFileCount := GetModuleFileCount;
      For i := 0 To iFileCount - 1 Do
        If GetModuleFileEditor(i).QueryInterface(IOTASourceEditor,
          Result) = S_OK Then
          Break;
    End;
End;

(**

  This procedure provides a smiple procedural interface for sending a message
  to the IDE's message window.

  @precon  strText is the tool message to be displayed.
  @postcon Adds a simple message to the IDEs message window.

  @param   strText as a String

**)
Procedure OutputMessage(strText : String);

Begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(strText);
End;

(**

  This procedure provides a smiple procedural interface for sending a message
  to the IDE's message window.

  @precon  strFileName is the name of the file associated with the message,
           strText is the message to be displayed, strPrefix is the prefix text
           infront of the message, e.g. [Warning], iLine is the line in the file
           where the message applies and iCol is the column in the file where
           the message applies.
  @postcon Adds a tools message to the IDE message window.

  @param   strFileName as a String
  @param   strText     as a String
  @param   strPrefix   as a String
  @param   iLine       as an Integer
  @param   iCol        as an Integer

**)
Procedure OutputMessage(strFileName, strText, strPrefix : String;
  iLine, iCol : Integer);

Begin
  (BorlandIDEServices As IOTAMessageServices).AddToolMessage(strFileName,
    strText, strPrefix, iLine, iCol);
End;

(**

  This provides a simple procedural interface to clear messages from
  the message window. Supply a set containing the messages you want to
  clear.

  @precon  Msg is a set of clear message enumerates to define which messages
           from the IDE messge window are cleared.
  @postcon The messages in the IDE message window are clear in line with
           the passed enumerate.

  @param   Msg as a TClearMessages

**)
Procedure ClearMessages(Msg : TClearMessages);

Begin
  If cmCompiler In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearCompilerMessages;
  If cmSearch In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearSearchMessages;
  If cmTool In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearToolMessages;
End;

(**

  This function finds the open tools api module interface for the given project
  source.

  @precon  A valid open tools api project source.
  @postcon Returns the module interface for the given project source.

  @param   Project as an IOTAProject
  @return  an IOTAModule

**)
Function ProjectModule(Project : IOTAProject) : IOTAModule;

Var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProject: IOTAProject;

Begin
  Result := Nil;
  AModuleServices := (BorlandIDEServices as IOTAModuleServices);
  For i := 0 To AModuleServices.ModuleCount - 1 Do
    Begin
      AModule := AModuleServices.Modules[i];
      If (AModule.QueryInterface(IOTAProject, AProject) = S_OK) And
        (Project = AProject) Then
        Break;
    End;
  Result := AProject;
End;

(**

  This method returns the buffer size of the passed source editor.

  @precon  SourceEditor is a valid sourc editor to get the buffer size of
  @postcon Returns the size of the editors buffer.

  @param   SourceEditor as an IOTASourceEditor
  @return  an Integer

**)
Function BufferSize(SourceEditor : IOTASourceEditor) : Integer;

Var
  Reader : IOTAEditReader;
  iRead : Integer;

Begin
  Reader := SourceEditor.CreateReader;
  Try
    Result := 0;
    Repeat
      iRead := Reader.GetText(Result, @Buffer, iBufferSize);
      Inc(Result, iRead);
    Until iRead < iBufferSize;
  Finally
    Reader := Nil;
  End;
End;

(**

  This method returna memory stream of the source code editor.

  @precon  SourceEditor is the editor to get the source code from.
  @postcon Returns a memory stream of the file.

  @param   SourceEditor as an IOTASourceEditor
  @return  a TMemoryStream

**)
Function EditorAsMemoryStream(SourceEditor : IOTASourceEditor) : TMemoryStream;

Var
  Reader : IOTAEditReader;
  iRead : Integer;
  iPosition : Integer;

Begin
  Result := TMemoryStream.Create;
  Reader := SourceEditor.CreateReader;
  Try
    iPosition := 0;
    Repeat
      iRead := Reader.GetText(iPosition, @Buffer, iBufferSize);
      Result.WriteBuffer(Buffer, iRead);
      inc(iPosition, iRead);
    Until iRead < iBufferSize;
  Finally
    Reader := Nil;
  End;
  Result.Position := 0;
End;

End.
