(**

  This is a derived class from TSynEdit allowing me to override some of
  the default properties and also add an number of extra WordStar style
  keybindings to the editors.

  @Version 1.0
  @Author  David Hoyle
  @Date    09 Jul 2008

**)

Unit DGHSynEdit;

Interface

Uses
  SysUtils, Classes, ComCtrls, SynEdit, SynEditTypes, SynEditKeyCmds,
  SynEditHighlighter, Controls;

Type
  (** A declaration for a call back proc to exceptions. **)
  TDGHSynEditExceptionProc = Procedure(strExceptionMsg : String) Of Object;

  (** Derived class of TSynEdit. Some of the methods assume that the editor is
      dymanically created as the first control of a TabSheet instance, hence
      this editor controls the TabSheet caption based on the file loaded. **)
  TDGHSynEdit = Class(TSynEdit)
  Private
    FFileName: String;
    FFileDateTime: TDateTime;
    FSGMLTag: Boolean;
    FInsertingTag : Boolean;
    FExceptionProc : TDGHSynEditExceptionProc;
    FSuspendTagCompletion : Boolean;
    FLastSize : Integer;
    function GetTabCaption: String;
    procedure SetTabCaption(const Value: String);
    function GetParentTabSheet: TTabSheet;
    procedure SetFileDateTime(const Value: TDateTime);
  Protected
    Procedure DoChange; Override;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Procedure LoadFromFile(strFileName : String);
    procedure SaveToFile(strFileName: String);
    Procedure Display;
    Procedure UpperCaseWord;
    procedure LowerCaseWord;
    Procedure LoadFromINIFile(strRootKey : String);
    Procedure SaveToINIFile(strRootKey : String);
    Procedure SuspendTagCompletion;
    { Properties }
    (**
      This property returns an appropriate captions for the editor tab within
      the main application.
      @precon  None.
      @postcon Returns an appropriate caption for the tab of the editor.
      @return  a String
    **)
    Property TabCaption : String Read GetTabCaption Write SetTabCaption;
    (**
      This property returns the filename for the editor.
      @precon  None.
      @postcon Returns the filename for the editor.
      @return  a String
    **)
    Property FileName : String Read FFileName;
    (**
      This property returns the tab sheet that the editor is bound within.
      @precon  None.
      @postcon Returns the tab sheet that the editor is bound within.
      @return  a TTabSheet
    **)
    Property ParentTabSheet : TTabSheet Read GetParentTabSheet;
    (**
      This property returns the current date and time of the editor file.
      @precon  None.
      @postcon Returns the current date and time of the editor file.
      @return  a TDateTime
    **)
    Property FileDateTime : TDateTime read FFileDateTime write SetFileDateTime;
    (**
      A property to determine if SMGL completion tags should be inserted.
      @precon  None.
      @postcon Returns whether SMGL completion tags should be inserted.
      @return  a Boolean
    **)
    Property SMGLTagCompletion : Boolean Read FSGMLTag Write FSGMLTag;
    (**
      A property to define a call back proc for exceptions.
      @precon  None.
      @postcon Sets or returns the call back proc.
      @return  a TDGHSynEditExceptionProc
    **)
    Property ExceptionProc : TDGHSynEditExceptionProc Read FExceptionProc
      Write FExceptionProc;
  Published
  End;

  (** This is a record to represent a top line and column of an editor
      window **)
  TWindowPosition = Record
    TopLine : Integer;
    LeftChar : Integer;
  End;

  (** This is a record to describe a caret position. **)
  TCaretPosition = Record
    CaretX : Integer;
    CaretY : Integer;
  End;

  (** This is a record to describe a block position with line and column
      informatiom. **)
  TBlockPosition = Record
    Line : Integer;
    Column : Integer;
  End;

  (** This is a record to describe the state of an editors window scroll
      position, caret position, and start and end positions of any highlighted
      blocks. **)
  TEditorState = Record
    Window : TWindowPosition;
    Caret : TCaretPosition;
    BlockBegin : TBlockPosition;
    BlockEnd : TBlockPosition;
  End;

  Function HighlighterName(Highlighter : TSynCustomHighlighter) : String;
  Function HighlighterExts(Highlighter : TSynCustomHighlighter) : String;

Implementation

{ TDGHSynEdit }

Uses
  Menus, Windows, IniFiles;

(**

  This function returns the Highlighter name from the first part of the
  Highlighter's Default Filter string.

  @precon  Highlighter must be a valid instance.
  @postcon Returns the Highlighter name from the first part of the
           Highlighter's Default Filter string.
           
  @param   Highlighter as a TSynCustomHighlighter
  @return  a String

**)
Function HighlighterName(Highlighter : TSynCustomHighlighter) : String;

Var
  iPos : Integer;

Begin
  Result := GetShortHint(Highlighter.DefaultFilter);
  iPos := Pos('(', Result);
  If iPos > 0 Then
    Delete(Result, iPos, Length(Result) - iPos + 1);
  Result := Trim(Result);
End;

(**

  This function returns the Highlighter extensions from the second part of the
  Highlighter's Default Filter string.

  @precon  Highlighter must be a valid instance.
  @postcon Returns the Highlighter extensions from the second part of the
           Highlighter's Default Filter string.

  @param   Highlighter as a TSynCustomHighlighter
  @return  a String

**)
Function HighlighterExts(Highlighter : TSynCustomHighlighter) : String;

Begin
  Result := GetLongHint(Highlighter.DefaultFilter);
End;

(**

  This is the constructor method for the TDGHSynEdit class.

  @precon  None.
  @postcon This is the constructor for the class that essentially override some
           of the default property values and then adds all the WordStar
           keybindings.

  @param   AOwner as a TComponent

**)
constructor TDGHSynEdit.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FLastSize := 0;
  TabWidth := 2;
  WantTabs := True;
  MaxScrollWidth := 8192;
  FSGMLTag := False;
  FSuspendTagCompletion := False;
  (* shortcuts.*)
  AddKey(ecDeleteChar, VK_DELETE, [ssCtrl], 0, []);
  AddKey(ecWordLeft, VK_LEFT, [ssCtrl], 0, []);
  AddKey(ecWordRight, VK_RIGHT, [ssCtrl], 0, []);
  AddKey(ecSelPageLeft, VK_LEFT, [ssAlt, ssShift], 0, []);
  AddKey(ecSelPageRight, VK_RIGHT, [ssAlt, ssShift], 0, []);
  AddKey(ecScrollLeft, VK_LEFT, [ssAlt], 0, []);
  AddKey(ecScrollRight, VK_RIGHT, [ssAlt], 0, []);
  AddKey(ecCommentBlock, Ord('C'), [ssAlt, ssCtrl], 0, []);
  AddKey(ecAutoCompletion, Ord('J'), [ssCtrl], 0, []);
  { Custom Commands }
  Options := [eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth,
    eoDragDropEditing, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo,
    eoScrollHintFollows, eoScrollPastEof, eoScrollPastEol, eoShowScrollHint,
    eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces];
end;

(**

  This method attempts to make the current editor the visible one on
  the PageControl.

  @precon  None.
  @postcon This method makes this editor appear in the page control as the
           active editor.

**)
procedure TDGHSynEdit.Display;
begin
  If Parent Is TTabSheet Then
    (Parent As TTabsheet).PageControl.ActivePage := (Parent As TTabsheet);
end;

(**

  This is an overridden DoChange to insert complletion tags for SMGL languages.

  @precon  None.
  @postcon Insert complletion tags for SMGL languages.

**)
procedure TDGHSynEdit.DoChange;

Var
  C : TBufferCoord;
  strLine : String;
  iLength : Integer;
  strTag : String;
  i : Integer;

begin
  If FSGMLTag And Not FInsertingTag And Not FSuspendTagCompletion And
    (FLastSize < Length(Text)) Then
    Begin
      FInsertingTag := True;
      Try
        C := Self.CaretXY;
        strLine := Lines[C.Line - 1];
        iLength := Length(strLine);
        strTag := '';
        If (iLength > 0) And (C.Char - 1 <= iLength) And
          (strLine[C.Char - 1]= '>') And (strLine[C.Char - 2] <> '/') Then
          Begin
            For i := C.Char - 2 DownTo 1 Do
              If strLine[i] = '<' Then
                Begin
                  If strLine[i + 1] <> '/' Then
                    strTag := GetWordAtRowCol(BufferCoord(i + 1, C.Line));
                  Break;
                End;
            If strTag <> '' Then
              Begin
                SelText := Format('</%s>', [strTag]);;
                SetCaretXY(C);
              End;
          End;
      Finally
        FInsertingTag := False;
      End;
    End;
  If FSuspendTagCompletion Then
    FSuspendTagCompletion := False;
  FLastSize := Length(Text);
  Inherited DoChange;
end;

(**

  This is a getter method for the ParentTabSheet property.

  @precon  None.
  @postcon Returns the tab sheet for this editor.

  @return  a TTabSheet

**)
function TDGHSynEdit.GetParentTabSheet: TTabSheet;
begin
  If Parent Is TTabSheet Then
    Result := Parent As TTabsheet
  Else
    Result := Nil;
end;

(**

  This is a getter method for the TabCaption property.

  @precon  None.
  @postcon Returns the caption of the parent tab sheet.

  @return  a String

**)
function TDGHSynEdit.GetTabCaption: String;
begin
  If Parent Is TTabSheet Then
    Result := (Parent As TTabsheet).Caption;
end;

(**

  This method loads a file from disk into the editor and update the file date
  and time, filename, and tab caption.

  @precon  None.
  @postcon This is the preferred method for loading files in this editor as it
           updates the parent TabSheet control with the files name as stores the
           Filename for later reference.

  @param   strFileName as a String

**)
procedure TDGHSynEdit.LoadFromFile(strFileName: String);
begin
  FFileName := strFileName;
  {$IFDEF VER180}
  FileAge(FFileName, FFileDateTime);
  {$ELSE}
  FFileDateTime := FileDateToDateTime(FileAge(FFileName));
  {$ENDIF}
  Try
    Lines.LoadFromFile(FFileName);
  Except
    On E : Exception Do
      If Assigned(FExceptionProc) Then
        FExceptionproc(E.Message)
      Else
        Raise;
  End;
  TabCaption := ExtractFileName(FFileName);
  FLastSize := Length(Text);
end;

(**

  This method loads the editors settings from the given INI File key.

  @precon  None.
  @postcon Loads the editors settings from the given INI File key.

  @param   strRootKey as a String

**)
procedure TDGHSynEdit.LoadFromINIFile(strRootKey: String);

Var
  strKey : String;

begin
  With TIniFile.Create(strRootKey) Do
    Try
      If Assigned(Highlighter) Then
        strKey := HighlighterName(Highlighter)
      Else
        strKey := 'General';
      Color := ReadInteger(strKey, 'Colour', Color);
      ActiveLineColor := ReadInteger(strKey, 'Active Line Colour',
        ActiveLineColor);
      Font.Name := ReadString(strKey, 'Font Name', Font.Name);
      Font.Size := ReadInteger(strKey, 'Font Size', Font.Size);
      Gutter.Font.Assign(Font);
      Gutter.ShowLineNumbers := ReadBool(strKey, 'Show Line Numbers',
        Gutter.ShowLineNumbers);
      Options := TSynEditorOptions(ReadInteger(strKey, 'Editor Options',
        Integer(Options)));
      RightEdge := ReadInteger(strKey, 'Right Edge', RightEdge);
      RightEdgeColor := ReadInteger(strKey, 'Right Edge Colour',
        RightEdgeColor);
      SelectedColor.Foreground := ReadInteger(strKey,
        'Selected Foreground', SelectedColor.Foreground);
      SelectedColor.Background := ReadInteger(strKey,
        'Selected Background', SelectedColor.Background);
      TabWidth := ReadInteger(strKey, 'Tab Width', TabWidth);
    Finally
      Free;
    End;
end;

(**

  This method saves the current editor edits to a disk file and updates the
  filename, date and time and tab caption.

  @precon  None.
  @postcon This is the preferred method for saving files in this editor as it
           updates the parent TabSheet control with the files name and stores the
           filename for later reference.

  @param   strFileName as a String

**)
procedure TDGHSynEdit.SaveToFile(strFileName: String);
begin
  FFileName := strFileName;
  Try
    Lines.SaveToFile(FFileName);
    {$IFDEF VER180}
    FileAge(FFileName, FFileDateTime);
    {$ELSE}
    FFileDateTime := FileDateToDateTime(FileAge(FFileName));
    {$ENDIF}
    Modified := False;
    TabCaption := ExtractFileName(FFileName);
  Except
    On E : Exception Do
      If Assigned(FExceptionProc) Then
        FExceptionproc(E.Message)
      Else
        Raise;
  End;
end;

(**

  This method saves the editors settings to the given Ini File key.

  @precon  None.
  @postcon Saves the editors settings to the given Ini File key.

  @param   strRootKey as a String

**)
procedure TDGHSynEdit.SaveToINIFile(strRootKey: String);

Var
  strKey : String;

begin
  With TIniFile.Create(strRootKey) Do
    Try
      If Assigned(Highlighter) Then
        strKey := HighlighterName(Highlighter)
      Else
        strKey := 'General';
      WriteInteger(strKey, 'Colour', Color);
      WriteInteger(strKey, 'Active Line Colour', ActiveLineColor);
      WriteString(strKey, 'Font Name', Font.Name);
      WriteInteger(strKey, 'Font Size', Font.Size);
      WriteBool(strKey, 'Show Line Numbers', Gutter.ShowLineNumbers);
      WriteInteger(strKey, 'Editor Options', Integer(Options));
      WriteInteger(strKey, 'Right Edge', RightEdge);
      WriteInteger(strKey, 'Right Edge Colour', RightEdgeColor);
      WriteInteger(strKey, 'Selected Foreground', SelectedColor.Foreground);
      WriteInteger(strKey, 'Selected Background', SelectedColor.Background);
      WriteInteger(strKey, 'Tab Width', TabWidth);
    Finally
      Free;
    End;
end;

(**

  This is a setter method for the TabCaption property.

  @precon  None.
  @postcon Sets the caption of the parent tan sheet control.

  @param   Value as a String constant

**)
procedure TDGHSynEdit.SetTabCaption(const Value: String);
begin
  If Parent Is TTabSheet Then
    (Parent As TTabsheet).Caption := Value;
end;

(**

  This method temporarily suspends the html/xml tag completion for 1 change
  event.

  @precon  None.
  @postcon Temporarily suspends the html/xml tag completion for 1 change event.

**)
procedure TDGHSynEdit.SuspendTagCompletion;
begin
  FSuspendTagCompletion := True;
end;

(**

  This method makes the word currently under the cursor UPPERCASE.

  @precon  None.
  @postcon This method makes the word currently under the cursor UPPERCASE.

**)
procedure TDGHSynEdit.UpperCaseWord;

Var
  ptCaret : TBufferCoord;

begin
  ptCaret := CaretXY;
  SetSelWord;
  If SelText <> '' Then
    SelText := Uppercase(SelText);
  CaretXY := ptCaret;
end;

(**

  This method makes the word currently under the cursor lowercase.

  @precon  None.
  @postcon This method makes the word currently under the cursor lowercase.

**)
procedure TDGHSynEdit.LowerCaseWord;

Var
  ptCaret : TBufferCoord;

begin
  ptCaret := CaretXY;
  SetSelWord;
  If SelText <> '' Then
    SelText := Lowercase(SelText);
  CaretXY := ptCaret;
end;

(**

  This is a setter method for the FileDateTime property.

  @precon  None.
  @postcon Sets the files date and time.

  @param   Value as a TDateTime constant

**)
procedure TDGHSynEdit.SetFileDateTime(const Value: TDateTime);
begin
  If FFileDateTime <> Value Then
    FFileDateTime := Value;
end;

End.
