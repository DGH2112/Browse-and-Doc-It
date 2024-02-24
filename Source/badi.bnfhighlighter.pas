(**

  This module contains a custom syntax highlighter for the RAD Studio
  IDE to show Backus-Naur grammar.

  @Author  David Hoyle
  @Version 1.005
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.BNFHighlighter;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  ToolsAPI;

Type
  (** A class to define an new IDE Highlighter for BNF Grammar **)
  TBNFHighlighter = Class(TNotifierObject, IUnknown, IOTANotifier, IOTAHighlighter {$IFDEF D2005},
    IOTAHighlighterPreview {$ENDIF})
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create;
    function GetIDString: string;
    function GetName: string;
    {$IFDEF RS120}
    Procedure Tokenize(StartClass: TOTALineClass; LineBuf: POTAEdChar; LineBufLen: TOTALineSize;
      HighlightCodes: POTASyntaxCode);
    Function TokenizeLineClass(StartClass: TOTALineClass; LineBuf: POTAEdChar;
      LineBufLen: TOTALineSize): TOTALineClass;
    {$ELSE}
    Procedure Tokenize(StartClass: Byte; LineBuf: PAnsiChar; LineBufLen: Word;
      HighlightCodes: POTASyntaxCode);
    Function TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar; LineBufLen: Word): Byte;
    {$ENDIF RS120}
  {$IFDEF D2005}
    Function  GetBlockEndCol: Integer;
    Function  GetBlockEndLine: Integer;
    Function  GetBlockStartCol: Integer;
    Function  GetBlockStartLine: Integer;
    Function  GetCurrentInstructionLine: Integer;
    Function  GetDisabledBreakpointLine: Integer;
    Function  GetDisplayName: String;
    Function  GetErrorLine: Integer;
    Function  GetInvalidBreakpointLine: Integer;
    Function  GetSampleSearchText: String;
    Function  GetSampleText: String;
    Function  GetValidBreakpointLine: Integer;
  {$ENDIF}
  End;

Implementation

(**

  A constructor for the TBNFHighlighter class.

  @precon  None.
  @postcon Should create a set of BNF Edit Options but currently throws an AV.

**)
constructor TBNFHighlighter.Create;

Var
  EditOps : IOTAEditOptions;
  iEditOps : Integer;

begin
  Inherited Create;
  EditOps := Nil;
  With (BorlandIDEServices As IOTAEditorServices) Do
    For iEditOps := 0 To EditOptionsCount - 1 Do
      If EditorOptions[iEditOps].IDString = 'Backus-Naur' Then
        EditOps := EditorOptions[iEditOps];
  If EditOps = Nil Then
    Begin
      //: @bug This causes an AV in the IDE - I think this is a bug in RAD Studio 2009.
      //EditOps := (BorlandIDEServices As IOTAEditorServices).AddEditOptions('Backus-Naur');
      //EditOps.Extensions := 'bnf';
      //EditOps.OptionsName := 'Backus-Naur Grammar';
      //EditOps.SyntaxHighlighter := Self;
    End;
end;

{$IFDEF D2005}
(**

  This method returns the end column of the highlighted block in the preview.

  @precon  None.
  @postcon Returns the end column of the highlighted block in the preview.

  @return  an Integer

**)
function TBNFHighlighter.GetBlockEndCol: Integer;
begin
  Result := 39;
end;

(**

  This method returns the end line of the highlighted block in the preview.

  @precon  None.
  @postcon Returns the end line of the highlighted block in the preview.

  @return  an Integer

**)
function TBNFHighlighter.GetBlockEndLine: Integer;
begin
  Result := 12;
end;

(**

  This method returns the start column of the highlighted block in the preview.

  @precon  None.
  @postcon Returns the start column of the highlighted block in the preview.

  @return  an Integer

**)
function TBNFHighlighter.GetBlockStartCol: Integer;
begin
  Result := 24;
end;

(**

  This method returns the start line of the highlighted block in the preview.

  @precon  None.
  @postcon Returns the start line of the highlighted block in the preview.

  @return  an Integer

**)
function TBNFHighlighter.GetBlockStartLine: Integer;
begin
  Result := 12;
end;

(**

  This method returns the line number for the Current Instruction.

  @precon  None.
  @postcon Returns -1 signifying this not to be displayed.

  @return  an Integer

**)
function TBNFHighlighter.GetCurrentInstructionLine: Integer;
begin
  Result := -1;
end;

(**

  This method returns the line number for the Disabled Breakpoint.

  @precon  None.
  @postcon Returns -1 signifying this not to be displayed.

  @return  an Integer

**)
function TBNFHighlighter.GetDisabledBreakpointLine: Integer;
begin
  Result := -1;
end;

(**

  This method returns the display name of the highlighter preview in the
  options dialogue.

  @precon  None.
  @postcon Returns the display name of the highlighter preview in the
           options dialogue.

  @return  a string

**)
function TBNFHighlighter.GetDisplayName: string;
begin
  Result := 'Backus-Naur';
end;

(**

  This method returns the line number for the Error Line.

  @precon  None.
  @postcon Returns -1 signifying this not to be displayed.

  @return  an Integer

**)
function TBNFHighlighter.GetErrorLine: Integer;
begin
  Result := -1;
end;

{$ENDIF}

(**

  This method returns a unique string ID for the highlighter.

  @precon  None.
  @postcon Returns a unique string ID for the highlighter.

  @return  a string

**)
function TBNFHighlighter.GetIDString: string;
begin
  Result := 'DGH.Backus-Naur Grammar Highlighter';
end;

{$IFDEF D2005}

(**

  This method returns the line number for the Invalid Breakpoint.

  @precon  None.
  @postcon Returns -1 signifying this not to be displayed.

  @return  an Integer

**)
function TBNFHighlighter.GetInvalidBreakpointLine: Integer;
begin
  Result := -1;
end;

{$ENDIF}

(**

  This method returns the descriptive name for the highlighter.

  @precon  None.
  @postcon Returns the descriptive name for the highlighter.

  @return  a string

**)
function TBNFHighlighter.GetName: string;
begin
  Result := 'Backus-Naur Grammar';
end;

{$IFDEF D2005}

(**

  This method returns the text to be highlighted in the preview as a search.

  @precon  None.
  @postcon Returns the text to be highlighted in the preview as a search.

  @return  a string

**)
function TBNFHighlighter.GetSampleSearchText: string;
begin
  Result := '<expression>';
end;

(**

  This method returns the text to be shown in the highlighter preview.

  @precon  None.
  @postcon Returns the text to be shown in the highlighter preview.

  @return  a string

**)
function TBNFHighlighter.GetSampleText: string;
begin
  Result :=
    '/**'#13#10 +
    ''#13#10 +
    '  Backus-Naur Language Grammar'#13#10 +
    ''#13#10 +
    '  @Version 1.0'#13#10 +
    '  @Date    21 Jul 2009'#13#10 +
    '  @Author  David Hoyle'#13#10 +
    ''#13#10 +
    '**/'#13#10 +
    '<syntax>           ::= <rule> | <rule> <syntax>'#13#10 +
    ''#13#10 +
    '<rule>             ::= "<" <rule-name> ">" "::=" <expression> <terminator>'#13#10 +
    ''#13#10 +
    '<expression>       ::= <list> | <list> "|" <expression> // A line comment'#13#10 +
    ''#13#10 +
    '<list>             ::= ''('' <expression> '')'' <RepeatOperator> | ''['' <expression> '']'' <RepeatOperator> | ( <term> | <term> <list> )'#13#10 +
    ''#13#10 +
    '<RepeatOperator>   ::= "*" | "+"'#13#10 +
    ''#13#10 +
    '<term>             ::= <literal> | "<" <rule-name> ">"'#13#10 +
    ''#13#10 +
    '<literal>          ::= ''"'' <text> ''"'' | "''" <text> "''"'#13#10 +
    ''#13#10 +
    '<rule-name>        ::= <text>'#13#10 +
    ''#13#10 +
    '<text>             ::= ? All visible ASCII characters - [#33..#128] ?'#13#10 +
    ''#13#10 +
    '<EOL>              ::= ? Carriage Return and Line Feed characters - [#13, #10] ?'#13#10 +
    ''#13#10 +
    '/** General termiantor is <EOL> except if the module header contains a tag'#13#10 +
    '    @@usesemicolon where upon the end of line is ignored and a semi-colon is'#13#10 +
    '    used. **/'#13#10 +
    '<terminator>       ::= '';'' | <EOL>'#13#10;
end;

(**

  This method returns the line number for the Valid Breakpoint.

  @precon  None.
  @postcon Returns -1 signifying this not to be displayed.

  @return  an Integer

**)
function TBNFHighlighter.GetValidBreakpointLine: Integer;
begin
  Result := -1;
end;

{$ENDIF}

(**

  This method returns the highlighter mark up codes for the given Line Buffer.

  @precon  None.
  @postcon Returns the highlighter mark up codes for the given Line Buffer.

  @param   StartClass     as a Byte
  @param   LineBuf        as a PAnsiChar
  @param   LineBufLen     as a Word
  @param   HighlightCodes as a POTASyntaxCode

**)
{$IFDEF RS120}
Procedure TBNFHighlighter.Tokenize(StartClass: TOTALineClass; LineBuf: POTAEdChar;
  LineBufLen: TOTALineSize; HighlightCodes: POTASyntaxCode);
{$ELSE}
Procedure TBNFHighlighter.Tokenize(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word; HighlightCodes: POTASyntaxCode);
{$ENDIF RS120}

Type
  TBlockType = (btNone, btIdentifier, btSingleLiteral, btDoubleLiteral,
     btTextDefinition, btLineComment, btBlockComment, btHexChar, btDecChar);

Const
  strValidSymbols = ([';', ':', '=', '(', ')', '[', ']', '-', '+', '*', '|',
    '''', '"', '.']);
  strAllSymbols = ([#33..#255] - ['A'..'Z'] - ['a'..'z'] - ['0'..'9']);
  strInvalidSymbols = (strAllSymbols - strValidSymbols);

Var
  Codes : PAnsiChar;
  i : Integer;
  CurChar, LastCHar : AnsiChar;
  BlockType : TBlockType;
  iBlockStart : Integer;

  (**

    This procedure checks for the end of a block type.

    @precon  None.
    @postcon Checks for the end of a block type and returns the block to None if found.

    @param   cChar          as an AnsiChar
    @param   CheckBlockType as a TBlockType

  **)
  Procedure CheckBlockEnd(cChar : AnsiChar; CheckBlockType : TBlockType);

  Begin
    If (LastChar = cChar) And (BlockType = CheckBlockType) And (i > iBlockStart + 1) Then
      Begin
        Codes[i] := AnsiChar($E);
        BlockType := btNone;
        iBlockStart := 0;
      End;
  End;

  (**

    This method checks to the start of a block section and set the block type.

    @precon  None.
    @postcon Checks to the start of a block section and set the block type.

    @param   cChar          as an AnsiChar
    @param   CheckBlockType as a TBlockType
    @param   iAttribute     as an Integer
    @return  a Boolean

  **)
  Function CheckBlockStart(cChar : AnsiChar; CheckBlockType : TBlockType;
    iAttribute : Integer) : Boolean;

  Begin
    Result := False;
    If ((CurChar = cChar) And (BlockType = btNone)) Or (BlockType = CheckBlockType) Then
      Begin
        Codes[i] := AnsiChar(iAttribute);
        BlockType := CheckBlockType;
        If iBlockStart = 0 Then
          iBlockStart := i;
        Result := True;
      End Else
  End;

begin
  CurChar := #0;
  LastChar := #0;
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
          If (LastChar In ['>']) And (BlockType = btIdentifier) Then
            Begin
              Codes[i] := AnsiChar($E);
              BlockType := btNone;
            End;
          CheckBlockEnd('''', btSingleLiteral);
          CheckBlockEnd('"', btDoubleLiteral);
          CheckBlockEnd('?', btTextDefinition);
          If ((LastChar In ['*']) And (CurChar In ['/']) And (BlockType In [btBlockComment])) Then
            Begin
              Codes[i] := AnsiChar(atComment);
              BlockType := btNone;
              iBlockStart := 0;
              Continue;
            End ;
          If (LastChar In ['$', '0'..'9', 'A'..'Z', 'a'..'z']) And
            Not (CurChar In ['$', '0'..'9', 'A'..'Z', 'a'..'z']) And (BlockType = btHexChar) Then
            Begin
              Codes[i] := AnsiChar(atWhiteSpace);
              BlockType := btNone;
              iBlockStart := 0;
            End;
          If (LastChar In ['#', '0'..'9']) And Not (CurChar In ['#', '0'..'9']) And (BlockType = btDecChar) Then
            Begin
              Codes[i] := AnsiChar(atWhiteSpace);
              BlockType := btNone;
              iBlockStart := 0;
            End;

          If CheckBlockStart('''', btSingleLiteral, atString) Then
          Else If CheckBlockStart('"', btDoubleLiteral, atCharacter) Then
          Else If CheckBlockStart('$', btHexChar, atNumber) Then
          Else If CheckBlockStart('#', btDecChar, atNumber) Then
          Else If CheckBlockStart('?', btTextDefinition, atPreproc) Then
          Else If ((CurChar In ['<']) And (BlockType = btNone)) Or (BlockType = btIdentifier) Then
            Begin
              Codes[i] := AnsiChar(atIdentifier);
              BlockType := btIdentifier;
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
          If CurChar In strValidSymbols Then
            Codes[i] := AnsiChar(atSymbol)
          Else If CurChar In strInvalidSymbols Then
            Codes[i] := AnsiChar(atIllegal);
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
{$IFDEF RS120}
Function TBNFHighlighter.TokenizeLineClass(StartClass: TOTALineClass; LineBuf: POTAEdChar;
  LineBufLen: TOTALineSize): TOTALineClass;
{$ELSE}
Function TBNFHighlighter.TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word): Byte;
{$ENDIF RS120}

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
