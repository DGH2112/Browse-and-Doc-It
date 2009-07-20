(**
  
  This module contains a custom syntax highlighter for the Borland/Codegear
  IDE to show Backus-Naur grammar.

  @Version 1.0
  @Date    20 Jul 2009
  @Author  David Hoyle
  
**)
Unit BNFHighlighter;

Interface

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Uses
  ToolsAPI;

Type
  (** A class to define an new IDE Highlighter for BNF Grammar **)
  TBNFHighlighter = Class(TNotifierObject, IOTAHighlighter)
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

(**

  This method returns the descriptive name for the highlighter.

  @precon  None.
  @postcon Returns the descriptive name for the highlighter.

  @return  a string

**)
function TBNFHighlighter.GetName: string;
begin
  Result := 'Backus-Naur Grammar Highlighter';
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
procedure TBNFHighlighter.Tokenize(StartClass: Byte; LineBuf: PAnsiChar;
  LineBufLen: Word; HighlightCodes: POTASyntaxCode);

Type
  TBlockType = (btNone, btIdentifier, btSingleLiteral, btDoubleLiteral,
     btTextDefinition, btLineComment, btBlockComment);

Const
  strValidSymbols = ([':', '=', '(', ')', '[', ']', '-', '+', '*', '|', '''', '"']);
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
    @postcon Checks for the end of a block type and returns the block to btNone
             if found.

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
          CheckBlockEnd('"', btSingleLiteral);
          If ((LastChar In ['*']) And (CurChar In ['/']) And (BlockType In [btBlockComment])) Then
            Begin
              Codes[i] := AnsiChar(atComment);
              BlockType := btNone;
              iBlockStart := 0;
              Break;
            End ;

          If CheckBlockStart('''', btSingleLiteral, atString) Then
          Else If CheckBlockStart('"', btDoubleLiteral, atString) Then
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
function TBNFHighlighter.TokenizeLineClass(StartClass: Byte; LineBuf: PAnsiChar;
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
