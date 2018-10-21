(**

  This module provides a few Open Tools API general method that are used
  throughout this project.

  @Date    21 Oct 2018
  @Version 1.0
  @Author  David Hoyle

**)
unit BADI.ToolsAPIUtils;

interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  SysUtils,
  Windows,
  ToolsAPI,
  Classes,
  BADI.Types,
  BADI.ElementContainer;

Type
  (** This is an enumerate for the types of messages that can be cleared. **)
  TClearMessage = (cmCompiler, cmSearch, cmTool);
  (** This is a set of messages that can be cleared. **)
  TClearMessages = Set of TClearMessage;

  Function ProjectGroup: IOTAProjectGroup;
  Function ActiveProject : IOTAProject;
  Function ProjectModule(Const Project : IOTAProject) : IOTAModule;
  Function ActiveSourceEditor : IOTASourceEditor;
  Function SourceEditor(Const Module : IOTAModule) : IOTASourceEditor;
  Procedure OutputMessage(Const strText : String); Overload;
  Procedure OutputMessage(Const strFileName, strText, strPrefix : String; Const iLine,
    iCol : Integer); Overload;
  Procedure ClearMessages(Const Msg : TClearMessages);
  Function EditorAsString(Const SourceEditor : IOTASourceEditor) : String;
  Procedure OutputText(Const Writer : IOTAEditWriter; Const strText : String);
  Procedure PositionCursor(Const Container : TElementContainer; Const iIdentLine, iIdentColumn : Integer;
    Const BrowsePosition : TBrowsePosition); Overload;
  Procedure PositionCursor(Const iIdentLine, iIdentCol, iCommentLine: Integer;
    Const BrowsePosition : TBrowsePosition); Overload;

Implementation

{$IFDEF D2009}
Uses
  Character;
{$ENDIF}

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

Var
  CM : IOTAModule;

Begin
  Result := Nil;
  If BorlandIDEServices = Nil Then
    Exit;
  CM := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Result := SourceEditor(CM);
End;

(**

  This provides a simple procedural interface to clear messages from
  the message window. Supply a set containing the messages you want to
  clear.

  @precon  Msg is a set of clear message enumerates to define which messages
           from the IDE messge window are cleared.
  @postcon The messages in the IDE message window are clear in line with
           the passed enumerate.

  @param   Msg as a TClearMessages as a Constant

**)
Procedure ClearMessages(Const Msg : TClearMessages);

Begin
  If cmCompiler In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearCompilerMessages;
  If cmSearch In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearSearchMessages;
  If cmTool In Msg Then
    (BorlandIDEServices As IOTAMessageServices).ClearToolMessages;
End;

(**

  This method returna memory stream of the source code editor.

  @precon  SourceEditor is the editor to get the source code from.
  @postcon Returns a memory stream of the file.

  @param   SourceEditor as an IOTASourceEditor as a Constant
  @return  a String

**)
Function EditorAsString(Const SourceEditor : IOTASourceEditor) : String;

Const
  iBufferCapacity = 8 * 8 * 8;
  
Var
  Reader : IOTAEditReader;
  iRead : Integer;
  iPosition : Integer;
  strBuffer : AnsiString;
  strTmp : AnsiString;

Begin
  Result := '';
  Reader := SourceEditor.CreateReader;
  Try
    iPosition := 0;
    Repeat
      SetLength(strBuffer, iBufferCapacity);
      iRead := Reader.GetText(iPosition, PAnsiChar(strBuffer), iBufferCapacity);
      SetLength(strBuffer, iRead);
      Inc(iPosition, iRead);
      strTmp := strTmp + strBuffer;
    Until iRead < iBufferCapacity;
    Result := UTF8ToUnicodeString(strTmp);
  Finally
    Reader := Nil;
  End;
End;

(**

  This procedure provides a smiple procedural interface for sending a message
  to the IDE`s message window.

  @precon  strText is the tool message to be displayed.
  @postcon Adds a simple message to the IDEs message window.

  @param   strText as a String as a Constant

**)
Procedure OutputMessage(Const strText : String);

Begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(strText);
End;

(**

  This procedure provides a smiple procedural interface for sending a message to the IDE`s message
  window.

  @precon  strFileName is the name of the file associated with the message, strText is the message
           to be displayed, strPrefix is the prefix text infront of the message, e.g. [Warning],
           iLine is the line in the file where the message applies and iCol is the column in the
           file where the message applies.
  @postcon Adds a tools message to the IDE message window.

  @param   strFileName as a String as a constant
  @param   strText     as a String as a constant
  @param   strPrefix   as a String as a constant
  @param   iLine       as an Integer as a Constant
  @param   iCol        as an Integer as a Constant

**)
Procedure OutputMessage(Const strFileName, strText, strPrefix : String;
  Const iLine, iCol : Integer);

Begin
  (BorlandIDEServices As IOTAMessageServices).AddToolMessage(strFileName,
    strText, strPrefix, iLine, iCol);
End;

(**

  This procedure outputs the given text to the given edit writer.

  @precon  Writer must be a valid instance of an IOTAEditWriter interface.
  @postcon Outputs the given text to the given edit writer.

  @param   Writer  as an IOTAEditWriter as a Constant
  @param   strText as a String as a constant

**)
Procedure OutputText(Const Writer : IOTAEditWriter; Const strText : String);

Begin
  {$IFNDEF D2009}
  Writer.Insert(PAnsiChar(strText));
  {$ELSE}
  Writer.Insert(PAnsiChar(AnsiString(strText)));
  {$ENDIF}
End;

(**

  This method move the active editors cursor to the supplied position and centres the cursor on th screen
  .

  @precon  None.
  @postcon When a selection is made in the explorer the cursor is placed in the editor.

  @param   iIdentLine     as an Integer as a constant
  @param   iIdentCol      as an Integer as a constant
  @param   iCommentLine   as an Integer as a constant
  @param   BrowsePosition as a TBrowsePosition as a constant

**)
Procedure PositionCursor(Const iIdentLine, iIdentCol, iCommentLine: Integer;
  Const BrowsePosition : TBrowsePosition); Overload;

  (**

    This method unfolders the method code at the nearest position to the cursor.

    @precon  EV must be a valid instance.
    @postcon The method code at the cursor is unfolded.

    @param   EV as an IOTAEditView as a constant

  **)
  Procedure UnfoldMethod(COnst EV : IOTAEditView);

  Var
    {$IFDEF D2006}
    EA: IOTAElideActions;
    {$ENDIF}
    
  Begin
    {$IFDEF D2006}
    If Supports(EV, IOTAElideActions, EA) Then
      EA.UnElideNearestBlock;
    {$ENDIF}
  End;

Var
  SourceEditor: IOTASourceEditor;
  C           : TOTAEditPos;
  EV          : IOTAEditView;
  iLine : Integer;

Begin
  SourceEditor := ActiveSourceEditor;
  If Assigned(SourceEditor) Then
    Begin
      If SourceEditor.EditViewCount > 0 Then
        Begin
          SourceEditor.Module.CurrentEditor.Show;
          If iIdentCol * iIdentLine > 0 Then
            Begin
              SourceEditor.Show;
              EV := (BorlandIDEServices As IOTAEditorServices).TopView;
              C.Col  := iIdentCol;
              C.Line := iIdentLine;
              UnfoldMethod(EV);
              EV.CursorPos := C;
              Case BrowsePosition Of
                bpCommentTop:
                  Begin
                    iLine := iIdentLine;
                    If iCommentLine > 0 Then
                      iLine := iCommentLine;
                    EV.SetTopLeft(iLine, 1);
                  End;
                bpCommentCentre:
                  Begin
                    iLine := iIdentLine;
                    If iCommentLine > 0 Then
                      iLine := iCommentLine;
                    EV.Center(iLine, 1);
                  End;
                bpIdentifierTop: EV.SetTopLeft(C.Line, 1);
                bpIdentifierCentre: EV.Center(C.Line, 1);
                bpIdentifierCentreShowAllComment:
                  Begin
                    EV.Center(C.Line, 1);
                    If iCommentLine > 0 Then
                      If iCommentLine < EV.TopRow Then
                        EV.SetTopLeft(iCommentLine, 1);
                  End;
              End;
              If C.Line >= EV.TopRow + EV.ViewSize.Height - 1 Then
                EV.SetTopLeft(C.Line - EV.ViewSize.Height + 1 + 1, 1);  
            End;
        End;
    End;
End;

(**

  This method positions the cursor in the editor based on the container, comment and browse position.

  @precon  None.
  @postcon The cursor is positioned in the editor.

  @param   Container      as a TElementContainer as a constant
  @param   iIdentLine     as an Integer as a constant
  @param   iIdentColumn   as an Integer as a constant
  @param   BrowsePosition as a TBrowsePosition as a constant

**)
Procedure PositionCursor(Const Container : TElementContainer; Const iIdentLine, iIdentColumn : Integer;
  Const BrowsePosition : TBrowsePosition);

  (**

    This method unfolders the method code at the nearest position to the cursor.

    @precon  EV must be a valid instance.
    @postcon The method code at the cursor is unfolded.

    @param   EV as an IOTAEditView as a constant

  **)
  Procedure UnfoldMethod(Const EV : IOTAEditView);

  Var
    {$IFDEF D2006}
    EA: IOTAElideActions;
    {$ENDIF}
    
  Begin
    {$IFDEF D2006}
    If Supports(EV, IOTAElideActions, EA) Then
      EA.UnElideNearestBlock;
    {$ENDIF}
  End;
  
Var
  SourceEditor: IOTASourceEditor;
  C           : TOTAEditPos;
  EV          : IOTAEditView;
  iLine : Integer;

Begin
  SourceEditor := ActiveSourceEditor;
  If Assigned(SourceEditor) Then
    Begin
      If SourceEditor.EditViewCount > 0 Then
        Begin
          SourceEditor.Module.CurrentEditor.Show;
          If iIdentColumn * iIdentLine > 0 Then
            Begin
              SourceEditor.Show;
              EV := (BorlandIDEServices As IOTAEditorServices).TopView;
              C.Col  := iIdentColumn;
              C.Line := iIdentLine;
              iLine := iIdentLine;
              UnfoldMethod(EV);
              EV.CursorPos := C;
              Case BrowsePosition Of
                bpCommentTop:
                  Begin
                    iLine := iIdentLine;
                    If Assigned(Container) Then
                      Begin
                        iLine := Container.Line;
                        If Assigned(Container.Comment) Then
                           iLine := Container.Comment.Line;
                      End;
                    EV.SetTopLeft(iLine, 1);
                  End;
                bpCommentCentre:
                  Begin
                    iLine := iIdentLine;
                    If Assigned(Container) Then
                      Begin
                        iLine := Container.Line;
                        If Assigned(Container.Comment) Then
                           iLine := Container.Comment.Line;
                      End;
                    EV.Center(iLine, 1);
                  End;
                bpIdentifierTop: EV.SetTopLeft(iLine, 1);
                bpIdentifierCentre: EV.Center(iLine, 1);
                bpIdentifierCentreShowAllComment:
                  Begin
                    EV.Center(iLine, 1);
                    If Assigned(Container) Then
                      Begin
                        iLine := Container.Line;
                        If Assigned(Container.Comment) Then
                           iLine := Container.Comment.Line;
                        If iLine < EV.TopRow Then
                          EV.SetTopLeft(iLine, 1);
                      End;
                  End;
              End;
              If C.Line >= EV.TopRow + EV.ViewSize.Height - 1 Then
                EV.SetTopLeft(C.Line - EV.ViewSize.Height + 1 + 1, 1);  
            End;
        End;
    End;
End;

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

  This function finds the open tools api module interface for the given project
  source.

  @precon  A valid open tools api project source.
  @postcon Returns the module interface for the given project source.

  @param   Project as an IOTAProject as a Constant
  @return  an IOTAModule

**)
Function ProjectModule(Const Project : IOTAProject) : IOTAModule;

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

  This method returns the source code editor for the passed module.

  @precon  Module is the module for which a source ditor interface is required.
  @postcon Returns the source editor interface for the given module.

  @param   Module as an IOTAModule as a Constant
  @return  an IOTASourceEditor

**)
Function SourceEditor(Const Module : IOTAModule) : IOTASourceEditor;

Var
  iFileCount : Integer;
  i : Integer;

Begin
  Result := Nil;
  If Module = Nil Then
    Exit;
  iFileCount := Module.GetModuleFileCount;
  For i := 0 To iFileCount - 1 Do
    If Module.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, Result) = S_OK Then
      Break;
End;

//------------------------------------------------------------------------------------------------------
End.
