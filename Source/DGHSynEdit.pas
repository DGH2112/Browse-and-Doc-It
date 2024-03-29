(**

  This is a derived class from TSynEdit allowing me to override some of
  the default properties and also add an number of extra WordStar style
  keybindings to the editors.

  @Version 1.0
  @Author  David Hoyle
  @Date    23 Oct 2004

**)

Unit DGHSynEdit;

Interface

Uses
  SysUtils, Classes, ComCtrls, SynEdit, SynEditKeyCmds;

Type

  (** Derived class of TSynEdit. Some of the methods assume that the editor is
      dymanically created as the first control of a TabSheet instance, hence
      this editor controls the TabSheet caption based on the file loaded. **)
  TDGHSynEdit = Class(TSynEdit)
  Private
    FFileName: String;
    FFileDateTime: TDateTime;
    function GetTabCaption: String;
    procedure SetTabCaption(const Value: String);
    procedure SetFileName(const Value: String);
    function GetParentTabSheet: TTabSheet;
    procedure SetFileDateTime(const Value: TDateTime);
  Protected
  Public
    Constructor Create(AOwner : TComponent); Override;
    Procedure LoadFromFile(strFileName : String);
    procedure SaveToFile(strFileName: String);
    Procedure Display;
    Procedure UpperCaseWord;
    procedure LowerCaseWord;
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
    Property FileName : String Read FFileName Write SetFileName;
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

Const
  (** A constant to define the Find Wordstar key sequence. **)
  ecFind          = ecUserFirst + 1;
  (** A constant to define the Find Again Wordstar key sequence. **)
  ecFindAgain     = ecUserFirst + 2;
  (** A constant to define the Replace Wordstar key sequence. **)
  ecReplace       = ecUserFirst + 3;
  (** A constant to define the Upper Word Wordstar key sequence. **)
  ecUppercaseWord = ecUserFirst + 4;
  (** A constant to define the Lower Word Wordstar key sequence. **)
  ecLowercaseWord = ecUserFirst + 5;
  (** A constant to define the Print Wordstar key sequence. **)
  ecPrint         = ecUserFirst + 6;

Implementation

{ TDGHSynEdit }

Uses
  Menus, Windows;

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
  TabWidth := 2;
  WantTabs := True;
  (*
    Add Wordstar shortcuts.
  *)
  AddKey(ecDeleteChar, VK_DELETE, [ssCtrl], 0, []);
  AddKey(ecBlockIndent, Ord('K'), [ssCtrl], Ord('I'), [ssCtrl]);
  AddKey(ecBlockIndent, Ord('K'), [ssCtrl], Ord('I'), []);
  AddKey(ecBlockUnindent, Ord('K'), [ssCtrl], Ord('U'), [ssCtrl]);
  AddKey(ecBlockUnindent, Ord('K'), [ssCtrl], Ord('U'), []);
  AddKey(ecCopy, Ord('K'), [ssCtrl], Ord('C'), [ssCtrl]);
  AddKey(ecCopy, Ord('K'), [ssCtrl], Ord('C'), []);
  AddKey(ecPaste, Ord('K'), [ssCtrl], Ord('V'), [ssCtrl]);
  AddKey(ecPaste, Ord('K'), [ssCtrl], Ord('V'), []);
  AddKey(ecLineSelect, Ord('K'), [ssCtrl], Ord('L'), [ssCtrl]);
  AddKey(ecLineSelect, Ord('K'), [ssCtrl], Ord('L'), []);
  AddKey(ecCut, Ord('K'), [ssCtrl], Ord('Y'), [ssCtrl]);
  AddKey(ecCut, Ord('K'), [ssCtrl], Ord('Y'), []);
  AddKey(ecColumnSelect, Ord('O'), [ssCtrl], Ord('C'), [ssCtrl]);
  AddKey(ecColumnSelect, Ord('O'), [ssCtrl], Ord('C'), []);
  AddKey(ecColumnSelect, Ord('O'), [ssCtrl], Ord('I'), [ssCtrl]);
  AddKey(ecColumnSelect, Ord('O'), [ssCtrl], Ord('I'), []);
  AddKey(ecNormalSelect, Ord('O'), [ssCtrl], Ord('K'), [ssCtrl]);
  AddKey(ecNormalSelect, Ord('O'), [ssCtrl], Ord('K'), []);
  AddKey(ecLineSelect, Ord('O'), [ssCtrl], Ord('L'), [ssCtrl]);
  AddKey(ecLineSelect, Ord('O'), [ssCtrl], Ord('L'), []);
  { Block Set Bookmarks }
  AddKey(ecSetMarker0, Ord('K'), [ssCtrl], Ord('0'), [ssCtrl]);
  AddKey(ecSetMarker1, Ord('K'), [ssCtrl], Ord('1'), [ssCtrl]);
  AddKey(ecSetMarker2, Ord('K'), [ssCtrl], Ord('2'), [ssCtrl]);
  AddKey(ecSetMarker3, Ord('K'), [ssCtrl], Ord('3'), [ssCtrl]);
  AddKey(ecSetMarker4, Ord('K'), [ssCtrl], Ord('4'), [ssCtrl]);
  AddKey(ecSetMarker5, Ord('K'), [ssCtrl], Ord('5'), [ssCtrl]);
  AddKey(ecSetMarker6, Ord('K'), [ssCtrl], Ord('6'), [ssCtrl]);
  AddKey(ecSetMarker7, Ord('K'), [ssCtrl], Ord('7'), [ssCtrl]);
  AddKey(ecSetMarker8, Ord('K'), [ssCtrl], Ord('8'), [ssCtrl]);
  AddKey(ecSetMarker9, Ord('K'), [ssCtrl], Ord('9'), [ssCtrl]);
  AddKey(ecSetMarker0, Ord('K'), [ssCtrl], Ord('0'), []);
  AddKey(ecSetMarker1, Ord('K'), [ssCtrl], Ord('1'), []);
  AddKey(ecSetMarker2, Ord('K'), [ssCtrl], Ord('2'), []);
  AddKey(ecSetMarker3, Ord('K'), [ssCtrl], Ord('3'), []);
  AddKey(ecSetMarker4, Ord('K'), [ssCtrl], Ord('4'), []);
  AddKey(ecSetMarker5, Ord('K'), [ssCtrl], Ord('5'), []);
  AddKey(ecSetMarker6, Ord('K'), [ssCtrl], Ord('6'), []);
  AddKey(ecSetMarker7, Ord('K'), [ssCtrl], Ord('7'), []);
  AddKey(ecSetMarker8, Ord('K'), [ssCtrl], Ord('8'), []);
  AddKey(ecSetMarker9, Ord('K'), [ssCtrl], Ord('9'), []);
  { Block Goto Bookmarks }
  AddKey(ecGotoMarker0, Ord('Q'), [ssCtrl], Ord('0'), [ssCtrl]);
  AddKey(ecGotoMarker1, Ord('Q'), [ssCtrl], Ord('1'), [ssCtrl]);
  AddKey(ecGotoMarker2, Ord('Q'), [ssCtrl], Ord('2'), [ssCtrl]);
  AddKey(ecGotoMarker3, Ord('Q'), [ssCtrl], Ord('3'), [ssCtrl]);
  AddKey(ecGotoMarker4, Ord('Q'), [ssCtrl], Ord('4'), [ssCtrl]);
  AddKey(ecGotoMarker5, Ord('Q'), [ssCtrl], Ord('5'), [ssCtrl]);
  AddKey(ecGotoMarker6, Ord('Q'), [ssCtrl], Ord('6'), [ssCtrl]);
  AddKey(ecGotoMarker7, Ord('Q'), [ssCtrl], Ord('7'), [ssCtrl]);
  AddKey(ecGotoMarker8, Ord('Q'), [ssCtrl], Ord('8'), [ssCtrl]);
  AddKey(ecGotoMarker9, Ord('Q'), [ssCtrl], Ord('9'), [ssCtrl]);
  AddKey(ecGotoMarker0, Ord('Q'), [ssCtrl], Ord('0'), []);
  AddKey(ecGotoMarker1, Ord('Q'), [ssCtrl], Ord('1'), []);
  AddKey(ecGotoMarker2, Ord('Q'), [ssCtrl], Ord('2'), []);
  AddKey(ecGotoMarker3, Ord('Q'), [ssCtrl], Ord('3'), []);
  AddKey(ecGotoMarker4, Ord('Q'), [ssCtrl], Ord('4'), []);
  AddKey(ecGotoMarker5, Ord('Q'), [ssCtrl], Ord('5'), []);
  AddKey(ecGotoMarker6, Ord('Q'), [ssCtrl], Ord('6'), []);
  AddKey(ecGotoMarker7, Ord('Q'), [ssCtrl], Ord('7'), []);
  AddKey(ecGotoMarker8, Ord('Q'), [ssCtrl], Ord('8'), []);
  AddKey(ecGotoMarker9, Ord('Q'), [ssCtrl], Ord('9'), []);
  { Custom Commands }
  AddKey(ecFind, Ord('Q'), [ssCtrl], Ord('F'), [ssCtrl]);     // Find
  AddKey(ecFind, Ord('Q'), [ssCtrl], Ord('F'), []);
  AddKey(ecFindAgain, Ord('L'), [ssCtrl], 0, []);             // Find Again
  AddKey(ecReplace, Ord('Q'), [ssCtrl], Ord('A'), [ssCtrl]);  // Replace
  AddKey(ecReplace, Ord('Q'), [ssCtrl], Ord('A'), []);
  AddKey(ecUppercaseWord, Ord('U'), [ssCtrl], 0, []);         // Uppercase Word
  AddKey(ecLowercaseWord, Ord('I'), [ssCtrl], 0, []);         // Lowercase Word
  AddKey(ecPrint, Ord('K'), [ssCtrl], Ord('P'), [ssCtrl]);    // Print
  AddKey(ecPrint, Ord('K'), [ssCtrl], Ord('P'), []);
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
  FileName := strFileName;
  FileDateTime := FileDateToDateTime(FileAge(strFileName));
  Lines.LoadFromFile(strFileName);
  TabCaption := ExtractFileName(strFileName);
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
  FileName := strFileName;
  Lines.SaveToFile(strFileName);
  FileDateTime := FileDateToDateTime(FileAge(strFileName));
  Modified := False;
  TabCaption := ExtractFileName(strFileName);
end;

(**

  This is a setter method for the FileName property.

  @precon  None.
  @postcon Sets the filename of the editor.

  @param   Value as a String constant

**)
procedure TDGHSynEdit.SetFileName(const Value: String);
begin
  If FFileName <> Value Then
    FFileName := Value;
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

  This method makes the word currently under the cursor UPPERCASE.

  @precon  None.
  @postcon This method makes the word currently under the cursor UPPERCASE.

**)
procedure TDGHSynEdit.UpperCaseWord;

Var
  ptCaret : TPoint;

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
  ptCaret : TPoint;

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
