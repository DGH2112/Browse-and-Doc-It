(**

  This module provides a few Open Tools API general method that are used
  throughout this project.

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
unit BADI.ToolsAPIUtils;

interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  ToolsAPI,
  System.SysUtils,
  System.Classes,
  VCL.Forms,
  WinAPI.Windows,
  BADI.Types,
  BADI.ElementContainer;

Type
  (** This is an enumerate for the types of messages that can be cleared. **)
  TClearMessage = (cmCompiler, cmSearch, cmTool);
  (** This is a set of messages that can be cleared. **)
  TClearMessages = Set of TClearMessage;

  (** A record to encapsulate the custom Tools API functions **)
  TBADIToolsAPIFunctions = Record
    Class Function ProjectGroup: IOTAProjectGroup; Static;
    Class Function ActiveProject : IOTAProject; Static;
    Class Function ProjectModule(Const Project : IOTAProject) : IOTAModule; Static;
    Class Function ActiveSourceEditor : IOTASourceEditor; Static;
    Class Function SourceEditor(Const Module : IOTAModule) : IOTASourceEditor; Static;
    Class Procedure OutputMessage(Const strText : String); Overload; Static;
    Class Procedure OutputMessage(Const strFileName, strText, strPrefix : String; Const iLine,
      iCol : Integer); Overload; Static;
    Class Procedure ClearMessages(Const Msg : TClearMessages); Static;
    Class Function EditorAsString(Const SourceEditor : IOTASourceEditor) : String; Static;
    Class Procedure OutputText(Const Writer : IOTAEditWriter; Const strText : String); Static;
    Class Procedure PositionCursor(Const Container : TElementContainer; Const iIdentLine,
      iIdentColumn : Integer; Const BrowsePosition : TBrowsePosition); Overload; Static;
    Class Procedure PositionCursor(Const iIdentLine, iIdentCol, iCommentLine: Integer;
      Const BrowsePosition : TBrowsePosition); Overload; Static;
    {$IFDEF DXE102}
    Class Procedure RegisterFormClassForTheming(Const AFormClass : TCustomFormClass); Static;
    Class Procedure ApplyTheming(Const Component : TComponent); Static;
    {$ENDIF}
  End;

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
Class Function TBADIToolsAPIFunctions.ActiveProject : IOTAProject;

var
  G : IOTAProjectGroup;

Begin
  Result := Nil;
  G := ProjectGroup;
  If Assigned(G) Then
    Result := G.ActiveProject;
End;

(**

  This method returns the active source code editor in the Delphi IDE.

  @precon  None. 
  @postcon Returns the active source editor.

  @return  an IOTASourceEditor

**)
Class Function TBADIToolsAPIFunctions.ActiveSourceEditor : IOTASourceEditor;

Var
  MS : IOTAModuleServices;

Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    If Assigned(MS.CurrentModule) Then
      Result := SourceEditor(MS.CurrentModule);
End;

{$IFDEF DXE102}
(**

  This method apply theming to the given componentif theming is enabled and available.

  @precon  None.
  @postcon The component is themed if theming is available and enabled.

  @param   Component as a TComponent as a constant

**)
Class Procedure TBADIToolsAPIFunctions.ApplyTheming(Const Component: TComponent);

Var
  ITS : IOTAIDEThemingServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.ApplyTheme(Component);
End;
{$ENDIF}

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
Class Procedure TBADIToolsAPIFunctions.ClearMessages(Const Msg : TClearMessages);

Var
  MS : IOTAMessageServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    Begin
      If cmCompiler In Msg Then
        MS.ClearCompilerMessages;
      If cmSearch In Msg Then
        MS.ClearSearchMessages;
      If cmTool In Msg Then
        MS.ClearToolMessages;
    End;
End;

(**

  This method returna memory stream of the source code editor.

  @precon  SourceEditor is the editor to get the source code from.
  @postcon Returns a memory stream of the file.

  @param   SourceEditor as an IOTASourceEditor as a Constant
  @return  a String

**)
Class Function TBADIToolsAPIFunctions.EditorAsString(Const SourceEditor : IOTASourceEditor) : String;

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
Class Procedure TBADIToolsAPIFunctions.OutputMessage(Const strText : String);

Var
  MS : IOTAMessageServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    MS.AddTitleMessage(strText);
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
Class Procedure TBADIToolsAPIFunctions.OutputMessage(Const strFileName, strText, strPrefix : String;
  Const iLine, iCol : Integer);

Var
  MS : IOTAMessageServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAMessageServices, MS) Then
    MS.AddToolMessage(strFileName, strText, strPrefix, iLine, iCol);
End;

(**

  This procedure outputs the given text to the given edit writer.

  @precon  Writer must be a valid instance of an IOTAEditWriter interface.
  @postcon Outputs the given text to the given edit writer.

  @param   Writer  as an IOTAEditWriter as a Constant
  @param   strText as a String as a constant

**)
Class Procedure TBADIToolsAPIFunctions.OutputText(Const Writer : IOTAEditWriter; Const strText : String);

Begin
  {$IFNDEF D2009}
  Writer.Insert(PAnsiChar(strText));
  {$ELSE}
  Writer.Insert(PAnsiChar(UTF8Encode(strText)));
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
Class Procedure TBADIToolsAPIFunctions.PositionCursor(Const iIdentLine, iIdentCol, iCommentLine: Integer;
  Const BrowsePosition : TBrowsePosition);

  (**

    This method unfolders the method code at the nearest position to the cursor.

    @precon  EV must be a valid instance.
    @postcon The method code at the cursor is unfolded.

    @param   EV as an IOTAEditView as a constant

  **)
  Procedure UnfoldMethod(COnst EV : IOTAEditView);

  Var
    EA: IOTAElideActions;
    
  Begin
    If Supports(EV, IOTAElideActions, EA) Then
      EA.UnElideNearestBlock;
  End;

  (**

    This procedure aligns the method comment at the top of the editor.

    @precon  None.
    @postcon The code methods comment is aligned to the top of the editor.

    @param   EditView as an IOTAEditView as a constant

  **)
  Procedure CommentTop(Const EditView : IOTAEditView);

  Var
    iLine: Integer;

  Begin
    iLine := iIdentLine;
    If iCommentLine > 0 Then
      iLine := iCommentLine;
    EditView.SetTopLeft(iLine, 1);
  End;

  (**

    This procedure aligns the method comment in the centre of the editor.

    @precon  None.
    @postcon The code methods comment is aligned in the centre of the editor.

    @param   EditView as an IOTAEditView as a constant

  **)
  Procedure CommentCentre(Const EditView : IOTAEditView);

  Var
    iLine: Integer;

  Begin
    iLine := iIdentLine;
    If iCommentLine > 0 Then
      iLine := iCommentLine;
    EditView.Center(iLine, 1);
  End;

  (**

    This procedure aligns the method identifier in the centre of the editor but shows all the comment.

    @precon  None.
    @postcon The code method identifier in the centre of the editor but shows all the comment.

    @param   C        as a TOTAEditPos as a constant
    @param   EditView as an IOTAEditView as a constant

  **)
  Procedure IdentCentreShowAllComment(Const C : TOTAEditPos; Const EditView : IOTAEditView);

  Begin
    EditView.Center(C.Line, 1);
    If iCommentLine > 0 Then
      If iCommentLine < EditView.TopRow Then
        EditView.SetTopLeft(iCommentLine, 1);
  End;
  
Var
  SourceEditor: IOTASourceEditor;
  C           : TOTAEditPos;
  EV          : IOTAEditView;

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
                bpCommentTop: CommentTop(EV);
                bpCommentCentre: CommentCentre(EV);
                bpIdentifierTop: EV.SetTopLeft(C.Line, 1);
                bpIdentifierCentre: EV.Center(C.Line, 1);
                bpIdentifierCentreShowAllComment: IdentCentreShowAllComment(C, EV);
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
Class Procedure TBADIToolsAPIFunctions.PositionCursor(Const Container : TElementContainer;
  Const iIdentLine, iIdentColumn : Integer; Const BrowsePosition : TBrowsePosition);

  (**

    This method unfolders the method code at the nearest position to the cursor.

    @precon  EV must be a valid instance.
    @postcon The method code at the cursor is unfolded.

    @param   EV as an IOTAEditView as a constant

  **)
  Procedure UnfoldMethod(Const EV : IOTAEditView);

  Var
    EA: IOTAElideActions;
    
  Begin
    If Supports(EV, IOTAElideActions, EA) Then
      EA.UnElideNearestBlock;
  End;

  (**

    This procedure aligns the method comment at the top of the editor.

    @precon  None.
    @postcon The code methods comment is aligned to the top of the editor.

    @param   EditView as an IOTAEditView as a constant

  **)
  Procedure CommentTop(Const EditView : IOTAEditView);

  Var
    iLine: Integer;

  Begin
    iLine := iIdentLine;
    If Assigned(Container) Then
      Begin
        iLine := Container.Line;
        If Assigned(Container.Comment) Then
           iLine := Container.Comment.Line;
      End;
    EditView.SetTopLeft(iLine, 1);
  End;

  (**

    This procedure aligns the method comment in the centre of the editor.

    @precon  None.
    @postcon The code methods comment is aligned in the centre of the editor.

    @param   EditView as an IOTAEditView as a constant

  **)
  Procedure CommentCentre(Const EditView : IOTAEditView);

  Var
    iLine: Integer;

  Begin
    iLine := iIdentLine;
    If Assigned(Container) Then
      Begin
        iLine := Container.Line;
        If Assigned(Container.Comment) Then
           iLine := Container.Comment.Line;
      End;
    EditView.Center(iLine, 1);
  End;

  (**

    This procedure aligns the method identifier in the centre of the editor but shows all the comment.

    @precon  None.
    @postcon The code method identifier in the centre of the editor but shows all the comment.

    @param   EditView as an IOTAEditView as a constant

  **)
  Procedure IdentCentreShowAllComment(Const EditView : IOTAEditView);

  Var
    iLine: Integer;

  Begin
    iLine := iIdentLine;
    EditView.Center(iLine, 1);
    If Assigned(Container) Then
      Begin
        iLine := Container.Line;
        If Assigned(Container.Comment) Then
           iLine := Container.Comment.Line;
        If iLine < EditView.TopRow Then
          EditView.SetTopLeft(iLine, 1);
      End;
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
                bpCommentTop: CommentTop(EV);
                bpCommentCentre: CommentCentre(EV);
                bpIdentifierTop: EV.SetTopLeft(iLine, 1);
                bpIdentifierCentre: EV.Center(iLine, 1);
                bpIdentifierCentreShowAllComment: IdentCentreShowAllComment(EV);
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
Class Function TBADIToolsAPIFunctions.ProjectGroup: IOTAProjectGroup;

Var
  MS: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProjectGroup: IOTAProjectGroup;

Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      For i := 0 To MS.ModuleCount - 1 Do
        Begin
          AModule := MS.Modules[i];
          If (AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK) Then
           Break;
        End;
      Result := AProjectGroup;
    End;
end;

(**

  This function finds the open tools api module interface for the given project
  source.

  @precon  A valid open tools api project source.
  @postcon Returns the module interface for the given project source.

  @param   Project as an IOTAProject as a Constant
  @return  an IOTAModule

**)
Class Function TBADIToolsAPIFunctions.ProjectModule(Const Project : IOTAProject) : IOTAModule;

Var
  MS: IOTAModuleServices;
  AModule: IOTAModule;
  i: integer;
  AProject: IOTAProject;

Begin
  Result := Nil;
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      For i := 0 To MS.ModuleCount - 1 Do
        Begin
          AModule := MS.Modules[i];
          If (AModule.QueryInterface(IOTAProject, AProject) = S_OK) And (Project = AProject) Then
            Break;
        End;
      Result := AProject;
    End;
End;

{$IFDEF DXE102}
(**

  This method regsiters the given form class for theming is theming is enabled and available.

  @precon  None.
  @postcon The form is regsitered for theming is available and enabled.

  @param   AFormClass as a TCustomFormClass as a constant

**)
Class Procedure TBADIToolsAPIFunctions.RegisterFormClassForTheming(Const AFormClass : TCustomFormClass);

Var
  ITS : IOTAIDEThemingServices250;
  
Begin
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.RegisterFormClass(AFormClass);
End;
{$ENDIF}

(**

  This method returns the source code editor for the passed module.

  @precon  Module is the module for which a source ditor interface is required.
  @postcon Returns the source editor interface for the given module.

  @param   Module as an IOTAModule as a Constant
  @return  an IOTASourceEditor

**)
Class Function TBADIToolsAPIFunctions.SourceEditor(Const Module : IOTAModule) : IOTASourceEditor;

Var
  iFileCount : Integer;
  i : Integer;

Begin
  Result := Nil;
  If Not Assigned(Module) Then
    Exit;
  iFileCount := Module.GetModuleFileCount;
  For i := 0 To iFileCount - 1 Do
    If Module.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, Result) = S_OK Then
      Break;
End;

End.
