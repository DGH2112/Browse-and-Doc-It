{ -------------------------------------------------------------------------

   This is a derived class from TSynEdit allowing me to override some of
   the default properties and also add an number of extra WordStar style
   keybindings to the editors.

  -------------------------------------------------------------------------- }

Unit DGHSynEdit;

Interface

Uses
  SysUtils, Classes, ComCtrls, SynEdit, SynEditKeyCmds;

Type

{ -------------------------------------------------------------------------

   Derived class of TSynEdit. Some of the methods assume that the editor is
   dymanically created as the first control of a TabSheet instance, hence
   this editor controls the TabSheet caption based on the file loaded.

  -------------------------------------------------------------------------- }

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
    Property TabCaption : String Read GetTabCaption Write SetTabCaption;
    Property FileName : String Read FFileName Write SetFileName;
    Property ParentTabSheet : TTabSheet Read GetParentTabSheet;
    Property FileDateTime : TDateTime read FFileDateTime write SetFileDateTime;
  Published
  End;

  TWindowPosition = Record
    TopLine : Integer;
    LeftChar : Integer;
  End;

  TCaretPosition = Record
    CaretX : Integer;
    CaretY : Integer;
  End;

  TBlockPosition = Record
    Line : Integer;
    Column : Integer;
  End;

  TEditorState = Record
    Window : TWindowPosition;
    Caret : TCaretPosition;
    BlockBegin : TBlockPosition;
    BlockEnd : TBlockPosition;
  End;

Const
  ecFind          = ecUserFirst + 1;
  ecFindAgain     = ecUserFirst + 2;
  ecReplace       = ecUserFirst + 3;
  ecUppercaseWord = ecUserFirst + 4;
  ecLowercaseWord = ecUserFirst + 5;
  ecPrint         = ecUserFirst + 6;

Implementation

{ TDGHSynEdit }

Uses
  Menus, Windows;

{ -------------------------------------------------------------------------

   This is the constructor for the class that essentially override some
   of the default property values and then adds all the WordStar
   keybindings.

   Create(
   );

  -------------------------------------------------------------------------- }

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

{ -------------------------------------------------------------------------

   This method attempts to make the current editor the visible one on
   the PageControl.

   Display(
   );

  -------------------------------------------------------------------------- }

procedure TDGHSynEdit.Display;
begin
  If Parent Is TTabSheet Then
    (Parent As TTabsheet).PageControl.ActivePage := (Parent As TTabsheet);
end;

{ -------------------------------------------------------------------------

   Getter method for the ParentTabSheet property.

  -------------------------------------------------------------------------- }

function TDGHSynEdit.GetParentTabSheet: TTabSheet;
begin
  If Parent Is TTabSheet Then
    Result := Parent As TTabsheet
  Else
    Result := Nil;
end;

{ -------------------------------------------------------------------------

   Getter method of the TabCaption property

  -------------------------------------------------------------------------- }

function TDGHSynEdit.GetTabCaption: String;
begin
  If Parent Is TTabSheet Then
    Result := (Parent As TTabsheet).Caption;
end;

{ -------------------------------------------------------------------------

   This is the preferred method for loading files in this editor as it
   updates the parent TabSheet control with the files name as stores the
   Filename for later reference.

   LoadFromFile(
     strFileName  // Name of the fies as a string
   );

  -------------------------------------------------------------------------- }

procedure TDGHSynEdit.LoadFromFile(strFileName: String);
begin
  FileName := strFileName;
  FileDateTime := FileDateToDateTime(FileAge(strFileName));
  Lines.LoadFromFile(strFileName);
  TabCaption := ExtractFileName(strFileName);
end;

{ -------------------------------------------------------------------------

   This is the preferred method for saving files in this editor as it
   updates the parent TabSheet control with the files name and stores the
   filename for later reference.

   SaveToFile(
     strFilename   // The name of the files as a string
   );

  -------------------------------------------------------------------------- }

procedure TDGHSynEdit.SaveToFile(strFileName: String);
begin
  FileName := strFileName;
  Lines.SaveToFile(strFileName);
  FileDateTime := FileDateToDateTime(FileAge(strFileName));
  Modified := False;
  TabCaption := ExtractFileName(strFileName);
end;

{ -------------------------------------------------------------------------

   Setter method for th FileName property

  -------------------------------------------------------------------------- }

procedure TDGHSynEdit.SetFileName(const Value: String);
begin
  If FFileName <> Value Then
    FFileName := Value;
end;

{ -------------------------------------------------------------------------

   Setter method for the TabCaption property.

  -------------------------------------------------------------------------- }

procedure TDGHSynEdit.SetTabCaption(const Value: String);
begin
  If Parent Is TTabSheet Then
    (Parent As TTabsheet).Caption := Value;
end;

{ -------------------------------------------------------------------------

   This method makes the word currently under the cursor UPPERCASE.

   UpperCaseWord(
   );

  -------------------------------------------------------------------------- }

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

{ -------------------------------------------------------------------------

   This method makes the word currently under the cursor lowercase.

   LowerCaseWord(
   );

  -------------------------------------------------------------------------- }

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

{ -------------------------------------------------------------------------

   Setter method for the FileDateTime property.

  -------------------------------------------------------------------------- }

procedure TDGHSynEdit.SetFileDateTime(const Value: TDateTime);
begin
  If FFileDateTime <> Value Then
    FFileDateTime := Value;
end;

End.
