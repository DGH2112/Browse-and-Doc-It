(**

  This module contains a custom syntax highlighter for the Eidolon MAP files.

  @Version 1.0
  @Date    05 Oct 2009
  @Author  David Hoyle

**)
Unit EidolonHighlighter;

Interface

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Uses
  ToolsAPI;

Type
  (** A class to define an new IDE Highlighter for Eidolon Map Files **)
  TEidolonHighlighter = Class(TNotifierObject, IOTAHighlighter, IOTAHighlighterPreview)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create;
    function GetIDString: string;
    function GetName: string;
    procedure Tokenize(StartClass: Byte; LineBuf: PAnsiChar; LineBufLen: Word;
      HighlightCodes: POTASyntaxCode);
    function TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar;
      LineBufLen: Word): Byte;
    function  GetBlockEndCol: Integer;
    function  GetBlockEndLine: Integer;
    function  GetBlockStartCol: Integer;
    function  GetBlockStartLine: Integer;
    function  GetCurrentInstructionLine: Integer;
    function  GetDisabledBreakpointLine: Integer;
    function  GetDisplayName: string;
    function  GetErrorLine: Integer;
    function  GetInvalidBreakpointLine: Integer;
    function  GetSampleSearchText: string;
    function  GetSampleText: string;
    function  GetValidBreakpointLine: Integer;
  End;

Implementation

Uses
  BaseLanguageModule;

Const
  (** A list of reserved word for the language. **)
  strReservedWords : Array[0..9] Of String = (
    'bar', 'class', 'dbtable', 'diamond', 'ellipse', 'line', 'rectangle',
      'texttable', 'timelocationtable', 'triangle'
    );

(**

  A constructor for the TEidolonHighlighter class.

  @precon  None.
  @postcon Should create a set of Eidolon Edit Options but currently throws an AV.

**)
constructor TEidolonHighlighter.Create;

Var
  EditOps : IOTAEditOptions;
  iEditOps : Integer;

begin
  EditOps := Nil;
  With (BorlandIDEServices As IOTAEditorServices) Do
    For iEditOps := 0 To EditOptionsCount - 1 Do
      If EditorOptions[iEditOps].IDString = 'Eidolon' Then
        EditOps := EditorOptions[iEditOps];
  If EditOps = Nil Then
    Begin
      // This causes an AV in the IDE - I think this is a bug in RAD Studio 2009.
      //EditOps := (BorlandIDEServices As IOTAEditorServices).AddEditOptions('Eidolon');
      //EditOps.Extensions := 'map';
      //EditOps.OptionsName := 'Eidolon MAP Files';
      //EditOps.SyntaxHighlighter := Self;
    End;
end;

(**

  This is a getter method for the BlockEndCol property.

  @precon  None.
  @postcon Returns the Block End Column number.

  @return  an Integer

**)
function TEidolonHighlighter.GetBlockEndCol: Integer;
begin
  Result := 39;
end;

(**

  This is a getter method for the BlockEndLine property.

  @precon  None.
  @postcon Returns the Block End Line number.

  @return  an Integer

**)
function TEidolonHighlighter.GetBlockEndLine: Integer;
begin
  Result := 12;
end;

(**

  This is a getter method for the BlockStartCol property.

  @precon  None.
  @postcon Returns the Block Start Column number.

  @return  an Integer

**)
function TEidolonHighlighter.GetBlockStartCol: Integer;
begin
  Result := 24;
end;

(**

  This is a getter method for the BlockStartLine property.

  @precon  None.
  @postcon Returns the Block Start Line number.

  @return  an Integer

**)
function TEidolonHighlighter.GetBlockStartLine: Integer;
begin
  Result := 12;
end;

(**

  This is a getter method for the CurrentInstructionLine property.

  @precon  None.
  @postcon Returns the current instruction line.

  @return  an Integer

**)
function TEidolonHighlighter.GetCurrentInstructionLine: Integer;
begin
  Result := -1;
end;

(**

  This is a getter method for the DisabledBreakPointLine property.

  @precon  None.
  @postcon Returns the disabled break point line.

  @return  an Integer

**)
function TEidolonHighlighter.GetDisabledBreakpointLine: Integer;
begin
  Result := -1;
end;

(**

  This is a getter method for the DisplayName property.

  @precon  None.
  @postcon Returns the display name.

  @return  a string

**)
function TEidolonHighlighter.GetDisplayName: string;
begin
  Result := 'Eidolon';
end;

(**

  This is a getter method for the ErrorLine property.

  @precon  None.
  @postcon Returns the error line number.

  @return  an Integer

**)
function TEidolonHighlighter.GetErrorLine: Integer;
begin
  Result := -1;
end;

(**

  This method returns a unique string ID for the highlighter.

  @precon  None.
  @postcon Returns a unique string ID for the highlighter.

  @return  a string

**)
function TEidolonHighlighter.GetIDString: string;
begin
  Result := 'DGH.Eidolon Highlighter';
end;

(**

  This is a getter method for the InvalidBreakPointLine property.

  @precon  None.
  @postcon Returns the invalid break point line number.

  @return  an Integer

**)
function TEidolonHighlighter.GetInvalidBreakpointLine: Integer;
begin
  Result := -1;
end;

(**

  This method returns the descriptive name for the highlighter.

  @precon  None.
  @postcon Returns the descriptive name for the highlighter.

  @return  a string

**)
function TEidolonHighlighter.GetName: string;
begin
  Result := 'Eidolon MAP Files';
end;

(**

  This is a getter method for the SampleSearchText property.

  @precon  None.
  @postcon Returns the sample search text.

  @return  a string

**)
function TEidolonHighlighter.GetSampleSearchText: string;
begin
  Result := 'Date';
end;

(**

  This is a getter method for the SampleText property.

  @precon  None.
  @postcon Returns the sample text.

  @return  a string

**)
function TEidolonHighlighter.GetSampleText: string;
begin
  Result :=
    '/**'#13#10 +
    ''#13#10 +
    '  Eidolon Map File'#13#10 +
    ''#13#10 +
    '**/'#13#10 +
    'This is a text file definition=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TableName=D:\Path\Text table.txt'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '  Activity Name:C(255)=Description'#13#10 +
    '  Start Date:D'#13#10 +
    '  Finish Date:D'#13#10 +
    '  Start Chainage:I'#13#10 +
    '  Start Chainage:I'#13#10 +
    '  Time Location Symbol:C(255)'#13#10 +
    '}'#13#10;
end;

(**

  This is a getter method for the ValidBreakPointLine property.

  @precon  None.
  @postcon Returns the valid break point line number.

  @return  an Integer

**)
function TEidolonHighlighter.GetValidBreakpointLine: Integer;
begin
  Result := -1;
end;

(**

  This method returns the higlighter mark up codes for the given Line Buffer.

  @precon  None.
  @postcon Returns the higlighter mark up codes for the given Line Buffer.

  @param   StartClass     as a Byte
  @param   LineBuf        as a PAnsiChar
  @param   LineBufLen     as a Word
  @param   HighlightCodes as a POTASyntaxCode

**)
procedure TEidolonHighlighter.Tokenize(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word; HighlightCodes: POTASyntaxCode);

Type
  TBlockType = (btNone, btIdentifier, btSingleLiteral, btDoubleLiteral,
     btTextDefinition, btLineComment, btBlockComment);

Const
  strAllSymbols = ([#33..#255]);
  strChars = (['a'..'z', 'A'..'Z', '-', '%']);
  strNumbers = (['0'..'9']);
  strSymbols = (strAllSymbols - strChars - strNumbers);

Var
  Codes : PAnsiChar;
  i : Integer;
  CurChar, LastCHar : AnsiChar;
  BlockType : TBlockType;
  iBlockStart : Integer;
  strToken: String;
  j: Integer;
  iToken: Integer;

begin
  CurChar := #0;
  SetLength(strToken, 100);
  iToken := 1;
  BlockType := btNone;
  iBlockStart := 0;
  Codes := PAnsiChar(HighlightCodes);
  FillChar(HighlightCodes^, LineBufLen, $E); // No highlighter
  For i := 0 To LineBufLen - 1 Do
    Begin
      If StartClass <> atComment Then
        Begin
          LastChar := CurChar;
          CurChar := LineBuf[i];
          If ((LastChar In ['*']) And (CurChar In ['/']) And (BlockType In [btBlockComment])) Then
            Begin
              Codes[i] := AnsiChar(atComment);
              //BlockType := btNone;
              //iBlockStart := 0;
              Break;
            End Else
          If ((LastChar In ['/']) And (CurChar In ['/'])) Or (BlockType In [btLineComment]) Then
            Begin
              Codes[i - 1] := AnsiChar(atComment);
              Codes[i] := AnsiChar(atComment);
              BlockType := btLineComment;
              If iBlockStart = 0 Then
                iBlockStart := i - 1;
            End Else
          If ((LastChar In ['/']) And (CurChar In ['*'])) Or (BlockType In [btBlockComment]) Then
            Begin
              Codes[i - 1] := AnsiChar(atComment);
              Codes[i] := AnsiChar(atComment);
              BlockType := btBlockComment;
              If iBlockStart = 0 Then
                iBlockStart := i - 1;
            End Else
          If CurChar In strChars Then
            Begin
              Codes[i] := AnsiChar(atIdentifier);
              strToken[iToken] := Char(curChar);
              Inc(iToken);
            End
          Else If CurChar In strNumbers Then
            Codes[i] := AnsiChar(atNumber)
          Else If CurChar In strSymbols Then
            Codes[i] := AnsiChar(atSymbol);
          If (i > 0) And (Codes[i] <> AnsiChar(atIdentifier)) And
            (Codes[i - 1] = AnsiChar(atIdentifier)) Then
            Begin
              SetLength(strToken, iToken - 1);
              If IsKeyWord(strToken, strReservedWords) Then
                Begin
                  For j := i - 1 DownTo i - Length(strToken) Do
                    Codes[j] := AnsiChar(atReservedWord);
                End;
              SetLength(strToken, 100);
              iToken := 1;
            End;
        End Else
          Codes[i] := Char(atComment);
    End;
end;

(**

  This method returns the highlighter code for the next line in the editor. Used
  for the block comment.

  @precon  None.
  @postcon Returns the highlighter code for the next line in the editor. Used
           for the block comment.

  @param   StartClass as a Byte
  @param   LineBuf    as a PAnsiChar
  @param   LineBufLen as a Word
  @return  a Byte

**)
function TEidolonHighlighter.TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word): Byte;

Var
  i : Integer;
  LastChar, CurChar: AnsiChar;

begin
  Result := StartClass;
  CurChar := #0;
  For i := 0 To LineBufLen - 1 Do
    Begin
      LastChar := CurChar;
      CurChar := LineBuf[i];
      If (LastChar In ['/']) And (CurChar In ['*']) Then
        Result := atComment
      Else If (LastChar In ['*']) And (CurChar In ['/']) Then
        Result := atWhiteSpace;
    End;
end;

End.
