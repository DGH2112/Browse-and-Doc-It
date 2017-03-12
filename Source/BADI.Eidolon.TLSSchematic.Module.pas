(**

  This module contains a parser for the Eidolon Time Location Schematic Diagram
  Language.

  @Version    1.0
  @Date       12 Mar 2017
  @Author     David Hoyle

**)
Unit BADI.Eidolon.TLSSchematic.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.Eidolon.Types,
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo,
  BADI.Eidolon.TLSSchematic.TLSShape,
  BADI.Eidolon.TLSSchematic.TLSRoad,
  BADI.Eidolon.TLSSchematic.TLSStatic;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is the main class for dealing with backus-naur grammar files. **)
  TTLSSchematicModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource           : String;
    FRoad             : Integer;
    FObject           : Integer;
    FSettings         : Array[Low(TSetting)..High(TSetting)] Of Double;
    FMaxRoads         : Double;
    FDebug            : Boolean;
    FCurrentRoad      :  Double;
    FCurrentObject    : Double;
    FLineColour       : TColour;
    FLineStyle        : TLineStyle;
    FLineWeight       : TLineWeight;
    FTextOrientation  : TTextOrientation;
    FTextOrientations : TStringList;
    FTextPosition     : TTextPosition;
    FTextPositions    : TStringList;
    { Grammar Parsers }
    Procedure Goal;
    Function Road : Boolean;
    Function Object_ : Boolean;
    Function Objects : Boolean;
    Function Roads : Boolean;
    Function Margins : Boolean;
    Function Spacing : Boolean;
    Function StartChainage(R : TTLSShape) : Boolean;
    Function EndChainage(R : TTLSShape) : Boolean;
    Function StartOffset(R : TTLSRoad) : Boolean;
    Function EndOffset(R : TTLSRoad) : Boolean;
    Function Chainages(R : TTLSShape) : Boolean;
    Function Offsets(R : TTLSRoad) : Boolean;
    Function Percentage(var dblPercentage : Double) : Boolean;
    Function CentreLine : Boolean;
    Function Debugging : Boolean;
    Function UnknownToken : Boolean;
    Procedure RouteCode(S : TTLSShape);
    Function Lines: Boolean;
    Function LineStyle: Boolean;
    Function LineWeight: Boolean;
    Function LineColour : Boolean;
    Function NoText : Boolean;
    Function TextOrientationElement : Boolean;
    Function Orientation : TTextOrientation;
    Function Text : Boolean;
    Function TextPositionElement : TTextPosition;
    Function Static : Boolean;
    Function Location(S : TTLSShape) : Boolean;
    Function LocationEx(S : TTLSShape) : Boolean;
    Function Percentages(S : TTLSStatic) : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    function GetSettings(S: TSetting): Double;
    Function CheckLiteral(strLiteral, strMethod: String): Boolean;
    function GetSuppressedText(strName : String): Boolean;
    Function GetTextOrientation(strName : String) : TTextOrientation;
    Function GetTextPosition(strName : String) : TTextPosition;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Function GetModuleName : String; Override;
    procedure TidyUpEmptyElements;
  Public
    Constructor CreateParser(const Source, strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property returns the specific percentage setting as a value between
      0 and 1.
      @precon  None.
      @postcon Returns the specific percentage setting as a value between
               0 and 1.
      @param   S as a TSetting
      @return  a Double
    **)
    Property Settings[S : TSetting] : Double Read GetSettings;
    (**
      This property returns the maximum offset of the roads in the script.
      @precon  None.
      @postcon Returns the maximum offset of the roads in the script.
      @return  a Double
    **)
    Property MaxRoads : Double Read FMaxRoads;
    (**
      This property returns whether the schematic diagram should be drawn in
      debugging mode.
      @precon  None.
      @postcon Returns whether the schematic diagram should be drawn in
               debugging mode.
      @return  a Boolean
    **)
    Property Debug : Boolean Read FDebug;
    (**
      This property returns true if the name exists in the list of suppressed
      texts.
      @precon  None.
      @postcon Returns true if the name exists in the list of suppressed
               texts.
      @param   strName as a String
      @return  a Boolean
    **)
    Property SuppressedText[strName : String] : Boolean Read GetSuppressedText;
    (**
      This property gets the text orientation of the named symbol.
      @precon  None.
      @postcon Returns the text orientation of the named symbol.
      @param   strName as a String
      @return  a TTextOrientation
    **)
    Property TextOrientation[strName : String] : TTextOrientation
      Read GetTextOrientation;
    (**
      This property gets the text position of the named symbol.
      @precon  None.
      @postcon Returns the text position of the named symbol.
      @param   strName as a String
      @return  a TTextPosition
    **)
    Property TextPosition[strName : String] : TTextPosition
      Read GetTextPosition;
  End;

Implementation

Uses
  DGHLibrary,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.Eidolon.TLSSchematic.Comment, BADI.Eidolon.TLSSchematic.Constants,
  BADI.Eidolon.TLSSchematic.ResourceStrings, BADI.Eidolon.TLSSchematic.SchematicSetting,
  BADI.Eidolon.TLSSchematic.NoText, BADI.Eidolon.TLSSchematic.TLSObject, BADI.Eidolon.Constants;

(**

  This is the constructor method for the TTLSSchematicModule class.

  @precon  Source is a valid TStream descendant containing as stream of text,
           that is the contents of a source code module and Filename is the
           file name of the module being parsed and IsModified determines if
           the source code module has been modified since the last save to
           disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TTLSSchematicModule.CreateParser(const Source, strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Var
  boolCascade : Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BrowseAndDocItOptions.Defines);
  FSource := Source;
  FRoad := 1;
  FObject := 1;
  FMaxRoads := 1;
  FSettings[seMargins] := 2;
  FCurrentRoad := 5;
  FCurrentObject := 5;
  FSettings[seSpacing] := 2;
  FSettings[seCentreLine] := 50;
  FDebug := False;
  FLineColour := xlcBlack;
  FLineStyle := lsSOLID;
  FLineWeight := lw0_25;
  FTextOrientation := toHorizontal;
  FTextOrientations := TStringList.Create;
  FTextPosition := tpOutside;
  FTextPositions := TStringList.Create;
  AddTickCount('Start');
  CommentClass := TTLSSchematicComment;
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount('Parse');
      Add(strErrors, iiErrorFolder, scNone, Nil);
      Add(strWarnings, iiWarningFolder, scNone, Nil);
      Add(strHints, iiHintFolder, scNone, Nil);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone, Nil);
      If FindElement(strErrors).ElementCount = 0 Then
        CheckReferences;
      AddTickCount('Refs');
      boolCascade := True;
      If moCheckForDocumentConflicts In ModuleOptions Then
        CheckDocumentation(boolCascade);
      AddTickCount('Check');
      TidyUpEmptyElements;
    End;
End;

(**

  This method parses the debugging element of the grammar.

  @precon  None.
  @postcon Parses the debugging element of the grammar.

  @return  a Boolean

**)
function TTLSSchematicModule.Debugging: Boolean;

begin
  Result := False;
  If Token.UToken = 'DEBUG' Then
    Begin
      NextNonCommentToken;
      If CheckLiteral(';', 'Debugging') Then
        Begin
          FDebug := True;
          Result := True;
        End;
    End;
end;

(**

  This is a destructor for the TTLSSchematicModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.

**)
Destructor TTLSSchematicModule.Destroy;
begin
  FTextPositions.Free;
  FTextOrientations.Free;
  Inherited Destroy;
end;

(**

  This method parses the text element of the grammar.

  @precon  None.
  @postcon Returns true if the element was parsed correctly.

  @return  a Boolean

**)
function TTLSSchematicModule.Text: Boolean;

var
  O: TTextPosition;
  strName: String;

begin
  Result := False;
  If Token.UToken = 'TEXT' Then
    Begin
      NextNonCommentToken;
      O := TextPositionElement;
      If Token.Token = ';' Then
        FTextPosition := O
      Else
        Begin
          If Token.Token = ',' Then
            Begin
              NextNonCommentToken;
              If Token.TokenType In [ttSingleLiteral] Then
                Begin
                  strName := Copy(Token.Token, 2, Length(Token.Token) - 2);
                  FTextPositions.AddObject(strName, TObject(Integer(O)));
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strStringExpected, 'Objects', Token.Token,
                    strSeekableOnErrorTokens, stActual);
            End;
        End;
      If CheckLiteral(';', 'Objects') Then
        Result := True;
    End;
end;

(**

  This method parses the TextOrientation element of the grammar.

  @precon  None.
  @postcon Returns true if the element was parsed correctly.

  @return  a Boolean

**)
function TTLSSchematicModule.TextOrientationElement: Boolean;

var
  O: TTextOrientation;
  strName: String;

begin
  Result := False;
  If Token.UToken = 'TEXTORIENTATION' Then
    Begin
      NextNonCommentToken;
      O := Orientation;
      If Token.Token = ';' Then
        FTextOrientation := O
      Else
        Begin
          If Token.Token = ',' Then
            Begin
              NextNonCommentToken;
              If Token.TokenType In [ttSingleLiteral] Then
                Begin
                  strName := Copy(Token.Token, 2, Length(Token.Token) - 2);
                  FTextOrientations.AddObject(strName, TObject(Integer(O)));
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strStringExpected, 'Objects', Token.Token,
                    strSeekableOnErrorTokens, stActual);
            End;
        End;
      If CheckLiteral(';', 'Objects') Then
        Result := True;
    End;
end;

(**

  This method parses the TextPosition element of the grammar.

  @precon  None.
  @postcon Returns the text position specified.

  @return  a TTextPosition

**)
function TTLSSchematicModule.TextPositionElement: TTextPosition;
begin
  Result := tpOutside;
  If IsKeyWord(Token.Token, ['inside', 'outside']) Then
    Begin
      If CompareText(Token.Token, 'INSIDE') = 0 Then
        Result := tpInside;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strExpectedOUTSIDEINSIDE, 'TextPosition',
        Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method deletes any root elements which dont and items in them.

  @precon  None.
  @postcon Deletes any root elements which dont and items in them.

**)
procedure TTLSSchematicModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Elements[iElement] Is TLabelContainer Then
        DeleteElement(iElement);
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TTLSSchematicModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btLineComment, btFullComment, btSingleLiteral);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;

Var
  (** Token buffer. **)
  strToken : String;
  CurCharType : TBADITokenType;
  LastCharType : TBADITokenType;
  BlockType : TBlockType;
  (** Current line number **)
  iLine : Integer;
  (** Current column number **)
  iColumn : Integer;
  (** Token stream position. Fast to inc this than read the stream position. **)
  iStreamPos : Integer;
  (** Token line **)
  iTokenLine : Integer;
  (** Token column **)
  iTokenColumn : Integer;
  (** Current character position **)
  iStreamCount : Integer;
  Ch : Char;
  LastChar : Char;
  (** Token size **)
  iTokenLen : Integer;
  iChar: Integer;

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  CurCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For iChar := 1 To Length(FSource) Do
    Begin
      ch := FSource[iChar];
      Inc(iStreamCount);
      LastCharType := CurCharType;

      If IsInSet(ch, strWhiteSpace) Then
        CurCharType := ttWhiteSpace
      Else If IsInSet(ch, strLineEnd) Then
        CurCharType := ttLineEnd
      Else If IsInSet(ch, strSingleQuotes) Then
        CurCharType := ttSingleLiteral
      Else If IsInSet(ch, strDoubleQuotes) Then
        CurCharType := ttDoubleLiteral
      Else If IsInSet(ch, strSymbols) Then
        Begin
          If (ch = '.') And (LastCharType = ttNumber) Then
            CurCharType := ttNumber
          Else If CharInSet(ch, ['%', '-']) And (LastCharType = ttIdentifier) Then
            CurCharType := ttIdentifier
          Else
            CurCharType := ttSymbol;
        End
      Else If IsInSet(ch, strIdentifiers) Then
        Begin
          If LastCharType = ttNumber Then
            CurCharType := ttNumber
          Else
            CurCharType := ttIdentifier;
        End
      Else If IsInSet(ch, strNumbers) Then
        Begin
          If LastCharType = ttIdentifier Then
            CurCharType := ttIdentifier
          Else
            CurCharType := ttNumber;
        End
      Else
        CurCharType := ttUnknown;

      If (BlockType = btNoBlock) Then
        Begin
          // Check for full block comments
          If (LastChar = '/') And (Ch = '*') Then
            BlockType := btFullComment;
          // Check for line comments
          If (LastChar = '/') And (Ch = '/') Then
            BlockType := btLineComment;
        End;

      If (LastCharType <> CurCharType) Or ((BlockType In [btNoBlock]) And (CurCharType = ttLineEnd) And (Ch = #13)) Then
        Begin
          If Not (((BlockType In [btLineComment, btSingleLiteral]) And (CurCharType <> ttLineEnd)) Or
            (BlockType In [btFullComment])) Or
            ((BlockType In [btNoBlock]) And (CurCharType = ttLineEnd) And (Ch = #13)) Then
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
                  Begin
                    If BlockType = btLineComment Then
                      LastCharType := ttLineComment;
                    If IsKeyWord(strToken, strReservedWords) Then
                      LastCharType := ttReservedWord;
                    AddToken(TTokenInfo.Create(strToken, iStreamPos,
                      iTokenLine, iTokenColumn, Length(strToken), LastCharType));
                  End;
             // Store Stream position, line number and column of
             // token start
             iStreamPos := iStreamCount;
             iTokenLine := iLine;
             iTokenColumn := iColumn;
             BlockType := btNoBlock;
             iTokenLen := 1;
             SetLength(strToken, iTokenCapacity);
             strToken[iTokenLen] := Ch;
            End Else
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := Ch;
            End;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := Ch;
        End;

      // Check for the end of a block comment
      If (BlockType = btFullComment) And (LastChar = '*') And (Ch = '/') Then
        Begin
          BlockType := btNoBlock;
          CurCharType := ttBlockComment;
        End;

      // Check for single string literals
      If CurCharType = ttSingleLiteral Then
        If BlockType = btSingleLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btSingleLiteral;

      Inc(iColumn);
      If Ch = #10 Then
        Begin
          Inc(iLine);
          iColumn := 1;
          If BlockType In [btLineComment] Then
            BlockType := btNoBlock;
        End;
      LastChar := Ch;
    End;
    If iTokenLen > 0 Then
      Begin
        SetLength(strToken, iTokenLen);
        If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
          Begin
            If IsKeyWord(strToken, strReservedWords) Then
              CurCharType := ttReservedWord;
            AddToken(TTokenInfo.Create(strToken, iStreamPos,
              iTokenLine, iTokenColumn, Length(strToken), CurCharType));
          End;
      End;
  AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
    ttFileEnd));
End;

(**

  This method picks up tokens that have not been parsed and flags them as
  errors.

  @precon  None.
  @postcon Returns true ONLY if the end of files is found.

  @return  a Boolean

**)
function TTLSSchematicModule.UnknownToken: Boolean;
begin
  Result := False;
  If Not (Token.TokenType = ttfileEnd) Then
    Begin
      ErrorAndSeekToken(strUnDefinedToken, 'UnknownToken', Token.Token,
        strSeekableOnErrorTokens, stActual);
      Result := True;
    End;
end;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TTLSSchematicModule.ParseTokens;
begin
  Goal;
end;

(**

  This method parses the Percentage element of the Grammar.

  @precon  S must be a valid instance.
  @postcon Parses the Percentage element of the Grammar.

  @param   dblPercentage as a Double as a reference
  @return  a Boolean

**)
Function TTLSSchematicModule.Percentage(var dblPercentage : Double) : Boolean;

Var
  iErrorCode : Integer;

begin
  Result := False;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dblPercentage, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          NextNonCommentToken;
          CheckLiteral('%', 'Percentage');
          Result := True;
        End Else
          ErrorAndSeekToken(strIntegerExpected, 'Percentage', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strNumberExpected, 'Percentage', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the start and end percentages for the Ellipse command.

  @precon  S must be a valid instance of a TTLSShape.
  @postcon Returns true if the element was parsed successfully.

  @param   S as a TTLSStatic
  @return  a Boolean

**)
function TTLSSchematicModule.Percentages(S: TTLSStatic): Boolean;

Var
  dbl : Double;

begin
  Result := False;
  If Percentage(dbl) Then
    Begin
      S.StartOffset := dbl;
      If CheckLiteral(',', 'Percentages') Then
        If Percentage(dbl) Then
          Begin
            S.EndOffset := dbl;
            If CheckLiteral(',', 'Percentages') Then
              Result := True;
          End;
    End;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TTLSSchematicModule.ReservedWords: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
end;

(**

  This method parses the Road element of the grammar.

  @precon  None.
  @postcon Returns true IF the road element was parsed correctly.

  @return  a Boolean

**)
function TTLSSchematicModule.Road: Boolean;

var
  Rs: TElementContainer;
  R : TTLSRoad;
  iColour: TColour;
  boolFound: Boolean;

begin
  Result := False;
  If Token.UToken = 'ROAD' Then
    Begin
      Rs := FindElement(strRoads);
      If Rs = Nil Then
        Rs := Add(TLabelContainer.Create(strRoads, scNone, 0, 0, iiPublicClass, Nil));
      R := Rs.Add(TTLSRoad.Create(Format('Road%4.4d', [FRoad]), scPublic, Token.Line,
        Token.Column, iiPublicClass, GetComment)) As TTLSRoad;
      Inc(FRoad);
      R.Width := FCurrentRoad;
      R.LineColour := FLineColour;
      R.LineStyle := FLineStyle;
      R.LineWeight := FLineWeight;
      NextNonCommentToken;
      If Chainages(R) Then
        If Offsets(R) Then
          If Location(R) Then
            Begin
              boolFound := False;
              For iColour := Low(TColour) To High(TColour) Do
                If CompareText(Token.Token, strColours[iColour]) = 0 Then
                  Begin
                    R.Colour := iColour;
                    boolFound := True;
                    Break;
                  End;
              If boolFound Then
                Begin
                  NextNonCommentToken;
                  RouteCode(R);
                  If CheckLiteral(';', 'Road') Then
                    Result := True;
                End Else
                  ErrorAndSeekToken(strInvalidColourName, 'Road',
                    Token.Token, strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'Road', 'Left or Right',
                strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the roads element of the grammar.

  @precon  None.
  @postcon Returns true if the element was a roads element.

  @return  a Boolean

**)
function TTLSSchematicModule.Roads: Boolean;

Var
  dbl : Double;

begin
  Result := False;
  If Token.UToken = 'ROADS' Then
    Begin
      NextNonCommentToken;
      If Percentage(dbl) Then
        Begin
          FCurrentRoad := dbl;
          If CheckLiteral(';', 'Roads') Then
            Result := True;
        End;
    End;
end;

(**

  This method parses the route code element of the grammar.

  @precon  S must be a valid instance.
  @postcon Parses the route code element of the grammar.

  @param   S as a TTLSShape

**)
procedure TTLSSchematicModule.RouteCode(S : TTLSShape);
begin
  If Token.Token = ',' Then
    Begin
      NextNonCommentToken;
      If Token.TokenType In [ttSingleLiteral] Then
        Begin
          S.RouteCode := Copy(Token.Token, 2, Length(Token.Token) - 2);
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strStringExpected, 'RouteCode', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the spacing element of the grammar.

  @precon  None.
  @postcon Returns true if the element was a spacing element.

  @return  a Boolean

**)
function TTLSSchematicModule.Spacing: Boolean;

var
  Ss: TElementContainer;
  S: TSchematicSetting;
  dbl : Double;

begin
  Result := False;
  If Token.UToken = 'SPACING' Then
    Begin
      Ss := FindElement(strSettings);
      If Ss = Nil Then
        Ss := Add(TLabelContainer.Create(strSettings, scNone, 0, 0,
          iiPublicTypesLabel, Nil)) As TLabelContainer;
      S := Ss.Add(TSchematicSetting.Create('Spacing', scNone, Token.Line,
        Token.Column, iiPublicType, Nil)) As TSchematicSetting;
      NextNonCommentToken;
      If Percentage(dbl) Then
        Begin
          S.Percentage := dbl;
          FSettings[seSpacing] := S.Percentage;
          If CheckLiteral(';', 'Spacing') Then
            Result := True;
        End;
    End;
end;

(**

  This method parses the StartChainage element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if a number was parsed.

  @param   R as a TTLSShape
  @return  a Boolean

**)
function TTLSSchematicModule.StartChainage(R : TTLSShape): Boolean;

Var
  dbl : Double;
  iErrorCode : Integer;
  boolNeg : Boolean;

begin
  Result := False;
  boolNeg := Token.Token = '-';
  If boolNeg Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
    End;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dbl, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          If boolNeg Then
            R.StartChainage := -dbl
          Else
            R.StartChainage := dbl;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'StartChainage', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      If boolNeg Then
        PopTokenPosition;
end;

(**

  This method parses the StartOffset element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if an integer was parsed.

  @param   R as a TTLSRoad
  @return  a Boolean

**)
function TTLSSchematicModule.StartOffset(R : TTLSRoad): Boolean;

Var
  i : Integer;
  iErrorCode : Integer;
  boolNeg : Boolean;

begin
  Result := False;
  boolNeg := Token.Token = '-';
  If boolNeg Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
    End;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, i, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          If boolNeg Then
            R.StartOffset := -i
          Else
            R.StartOffset := i;
          If i > FMaxRoads Then
            FMaxRoads := i;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'StartOffset', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      If boolNeg Then
        PopTokenPosition;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TTLSSchematicModule.Directives: TKeyWords;

begin
  Result := Nil;
end;

(**

  This method parses the Ellipse element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true the element was parsed successfully.

  @return  a Boolean

**)
function TTLSSchematicModule.Static: Boolean;

var
  Os: TElementContainer;
  E : TTLSStatic;
  boolFound : Boolean;
  iColour : TColour;

begin
  Result := False;
  If IsKeyWord(Token.Token, ['staticdiamond', 'staticellipse', 'staticobject']) Then
    Begin
      Os := FindElement(strObjects);
      If Os = Nil Then
        Os := Add(TLabelContainer.Create(strObjects, scNone, 0, 0, iiPublicClass, Nil));
      E := Os.Add(TTLSStatic.Create(Format('Object%4.4d', [FObject]), scPublic,
        Token.Line, Token.Column, iiPublicClass, GetComment)) As TTLSStatic;
      Inc(FObject);
      E.Width := FCurrentObject;
      E.LineColour := FLineColour;
      E.LineStyle := FLineStyle;
      E.LineWeight := FLineWeight;
      E.TextOrientation := FTextOrientation;
      E.TextPosition := FTextPosition;
      E.Shape := tstRectangle;
      If Token.UToken = 'STATICELLIPSE' Then
        E.Shape := tstEllipse
      Else If Token.UToken = 'STATICDIAMOND' Then
        E.Shape := tstDiamond;
      NextNonCommentToken;
      If Chainages(E) Then
        If Percentages(E) Then
          If LocationEx(E) Then
            Begin
              boolFound := False;
              For iColour := Low(TColour) To High(TColour) Do
                If CompareText(Token.Token, strColours[iColour]) = 0 Then
                  Begin
                    E.Colour := iColour;
                    boolFound := True;
                    Break;
                  End;
              If boolFound Then
                Begin
                  NextNonCommentToken;
                  If CheckLiteral(',', 'Ellipse') Then
                    Begin
                      If Token.TokenType In [ttSingleLiteral] Then
                        Begin
                          E.Text := Copy(Token.Token, 2, Length(Token.Token) - 2);
                          NextNonCommentToken;
                          RouteCode(E);
                        End Else
                          ErrorAndSeekToken(strStringExpected, 'Ellipse', Token.Token,
                            strSeekableOnErrorTokens, stActual);
                    End;
                  If CheckLiteral(';', 'Ellipse') Then
                    Result := True;
                End Else
                  ErrorAndSeekToken(strInvalidColourName, 'Ellipse',
                    Token.Token, strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'Ellipse',
                'Left, Right, Over, OverLeft, OverRight, Under or Both',
                strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the EndChainage element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if an double was parsed.

  @param   R as a TTLSShape
  @return  a Boolean

**)
function TTLSSchematicModule.EndChainage(R : TTLSShape): Boolean;

Var
  dbl : Double;
  iErrorCode : Integer;
  boolNeg: Boolean;

begin
  Result := False;
  boolNeg := Token.Token = '-';
  If boolNeg Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
    End;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dbl, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          If boolNeg Then
            R.EndChainage := -dbl
          Else
            R.EndChainage := dbl;
          If R.StartChainage >= R.EndChainage Then
            ErrorAndSeekToken(strChainageError, 'EndChainage', Token.Token,
              strSeekableOnErrorTokens, stActual);
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'EndChainage', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      If boolNeg Then
        PopTokenPosition;
end;

(**

  This method parses the EndOffset element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if an integer was parsed.

  @param   R as a TTLSRoad
  @return  a Boolean

**)
function TTLSSchematicModule.EndOffset(R : TTLSRoad): Boolean;

Var
  i : Integer;
  iErrorCode : Integer;
  boolNeg : Boolean;

begin
  Result := False;
  boolNeg := Token.Token = '-';
  If boolNeg Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
    End;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, i, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          If boolNeg Then
            R.EndOffset := -i
          Else
            R.EndOffset := i;
          If i > FMaxRoads Then
            FMaxRoads := i;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'EndOffset', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      If boolNeg Then
        PopTokenPosition;
end;

(**

  This method tries to get a document comment from the previous token and return
  a TComment class to the calling routine.

  @note    All comments found are automatically added to the comment collection
           for disposal when the parser is destroyed.

  @precon  None.
  @postcon Returns the comment immediately before the current token else nil.

  @param   CommentPosition as a TCommentPosition
  @return  a TComment

**)
Function TTLSSchematicModule.GetComment(
  CommentPosition : TCommentPosition) : TComment;

Var
  T : TTokenInfo;
  iOffset : Integer;
  iToken: TTokenIndex;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  iToken := TokenIndex + iOffset;
  If iToken > -1 Then
    Begin
      While (iToken > -1) And ((Tokens[iToken] As TTokenInfo).TokenType In
        [ttLineEnd, ttLineContinuation]) Do
        Dec(iToken);
      If iToken > -1 Then
        Begin;
          T := Tokens[iToken] As TTokenInfo;
          If T.TokenType In [ttLineComment, ttBlockComment] Then
            Begin
              Result := TTLSSchematicComment.CreateComment(T.Token, T.Line, T.Column);
              OwnedItems.Add(Result);
            End;
        End;
    End;
End;

(**

  This method returns a string representing the name of the module.
  @precon  None.
  @postcon Returns a string representing the name of the module.

  @return  a String

**)
function TTLSSchematicModule.GetModuleName: String;
begin
  Result := ExtractFilename(FileName);
end;

(**

  This is a getter method for the Settings property.

  @precon  None.
  @postcon Returns the Setting percentage as a value between 0 and 1.

  @param   S as a TSetting
  @return  a Double

**)
function TTLSSchematicModule.GetSettings(S: TSetting): Double;
begin
  Result := FSettings[S] / 100.0;
end;

(**

  This is a getter method for the SuppressedText property.

  @precon  None.
  @postcon Returns true if the named text exists in the list of suppressed
           text.

  @param   strName as a String
  @return  a Boolean

**)
function TTLSSchematicModule.GetSuppressedText(strName : String): Boolean;

var
  N: TElementContainer;

begin
  Result := False;
  N := FindElement(strNoTexts);
  If N <> Nil Then
    Result := N.FindElement(strName) <> Nil;
end;

(**

  This is a getter method for the TextOrientation property.

  @precon  None.
  @postcon Returns the text orientation of the named symbol.

  @param   strName as a String
  @return  a TTextOrientation

**)
function TTLSSchematicModule.GetTextOrientation(strName: String): TTextOrientation;

var
  iIndex: Integer;

begin
  Result := toHorizontal;
  iIndex := FTextOrientations.IndexOf(strName);
  If iIndex > -1 Then
    Result := TTextOrientation(Integer(FTextOrientations.Objects[iIndex]));
end;

(**

  This is a getter method for the TextPosition property.

  @precon  None.
  @postcon Returns the text position for the named symbol.

  @param   strName as a String
  @return  a TTextPosition

**)
function TTLSSchematicModule.GetTextPosition(strName: String): TTextPosition;

var
  iIndex: Integer;

begin
  Result := tpOutside;
  iIndex := FTextPositions.IndexOf(strName);
  If iIndex > -1 Then
    Result := TTextPosition(Integer(FTextPositions.Objects[iIndex]));
end;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @param   iSkip as an Integer as a reference

**)
Procedure TTLSSchematicModule.ProcessCompilerDirective(var iSkip : Integer);

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method returns false and does not reference any tokens.

  @precon  None.
  @postcon Does nothing.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TTLSSchematicModule.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
End;

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TTLSSchematicModule.AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String;

Begin
  Result := ChangeFileExt(ExtractFileName(FileName), '') + ' Schematic Diagram';
End;

(**

  This method parses the centre Line element of the grammar.

  @precon  None.
  @postcon Parses the centre Line element of the grammar.

  @return  a Boolean

**)
function TTLSSchematicModule.CentreLine: Boolean;

var
  Ss: TElementContainer;
  S: TSchematicSetting;
  dbl : Double;

begin
  Result := False;
  If Token.UToken = 'CENTRELINE' Then
    Begin
      Ss := FindElement(strSettings);
      If Ss = Nil Then
        Ss := Add(TLabelContainer.Create(strSettings, scNone, 0, 0,
          iiPublicTypesLabel, Nil)) As TLabelContainer;
      S := Ss.Add(TSchematicSetting.Create('CentreLine', scNone, Token.Line,
        Token.Column, iiPublicType, Nil)) As TSchematicSetting;
      NextNonCommentToken;
      If Percentage(dbl) Then
        Begin
          S.Percentage := dbl;
          FSettings[seCentreLine] := S.Percentage;
          If CheckLiteral(';', 'CentreLine') Then
            Result := True;
        End;
    End;
end;

(**

  This method processes the chinages from the road element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if the chainages were successfully parsed.

  @param   R as a TTLSShape
  @return  a Boolean

**)
Function TTLSSchematicModule.Chainages(R : TTLSShape): Boolean;

Begin
  Result := False;
  If StartChainage(R) Then
    Begin
      If CheckLiteral(',', 'Chainage') Then
        If EndChainage(R) Then
          Begin
            If CheckLiteral(',', 'Chainage') Then
              Result := True;
          End Else
            ErrorAndSeekToken(strExpectedAnEndChainage, 'Chainages',
              Token.Token, strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strExpectedAStartChainage, 'Chainages', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method is the starting position for the parsing of an backus-naur
  module. It finds the first non comment token and begins the grammar checking
  from their by deligating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating Syntax.

**)
procedure TTLSSchematicModule.Goal;

var
  C: TComment;

begin
  Line := 1;
  Column := 1;
  Try
    If TokenCount > 0 Then
      Begin
        // Find first non comment token
        While (Token.TokenType In [ttLineComment, ttBlockComment, ttLineEnd]) And
          Not EndOfTokens Do
          Begin
            If Token.TokenType In [ttLineComment, ttBlockComment] Then
              Begin
                C := TTLSSchematicComment.CreateComment(Token.Token, Token.Line,
                  Token.Column);
                AddBodyComment(C);
                If Comment = Nil Then
                  Comment := C;
              End;
            NextToken;
          End;
        // Check for end of file else must be identifier
        If Not EndOfTokens Then
          While Road Or Object_ Or Objects Or Roads Or Margins Or Spacing Or
            Debugging Or CentreLine Or Lines Or NoText Or
            TextOrientationElement Or Text Or Static Or UnknownToken Do;
        If Not (Token.TokenType In [ttFileEnd]) Then
          Raise EBADIParserAbort.Create(strUnExpectedEndOfFile);
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 'Goal', 0, 0, etError);
  End;
end;

(**

  This method parses the margins element of the grammar.

  @precon  None.
  @postcon Returns true if the element was a margins element.

  @return  a Boolean

**)
function TTLSSchematicModule.Margins: Boolean;

var
  Ms: TElementContainer;
  M: TSchematicSetting;
  dbl : Double;

begin
  Result := False;
  If Token.UToken = 'MARGINS' Then
    Begin
      Ms := FindElement(strSettings);
      If Ms = Nil Then
        Ms := Add(TLabelContainer.Create(strSettings, scNone, 0, 0,
          iiPublicTypesLabel, Nil)) As TLabelContainer;
      M := Ms.Add(TSchematicSetting.Create('Margins', scNone, Token.Line,
        Token.Column, iiPublicType, Nil)) As TSchematicSetting;
      NextNonCommentToken;
      If Percentage(dbl) Then
        Begin
          M.Percentage := dbl;
          FSettings[seMargins] := M.Percentage;
          If CheckLiteral(';', 'Margins') Then
            Result := True;
        End;
    End;
end;

(**

  This method parses the NoText element of the grammar.

  @precon  None.
  @postcon Returns true if the grammar element was correctly parsed.

  @return  a Boolean

**)
function TTLSSchematicModule.NoText: Boolean;

var
  Ns: TElementContainer;
  T : TTokenInfo;

begin
  Result := False;
  If Token.UToken = 'NOTEXT' Then
    Begin
      T := Token;
      NextNonCommentToken;
      If Token.TokenType In [ttSingleLiteral] Then
        Begin
          Ns := FindElement(strNoTexts);
          If Ns = Nil Then
            Ns := Add(TLabelContainer.Create(strNoTexts, scNone, 0, 0,
              iiPublicThreadVarsLabel, Nil));
          Ns.Add(TNoText.Create(Copy(Token.Token, 2, Length(Token.Token) - 2),
            scPublic, T.Line, T.Column, iiPublicThreadVar, Nil));
          NextNonCommentToken;
          If CheckLiteral(';', 'NoText') Then
            Result := True;
        End Else
          ErrorAndSeekToken(strStringExpected, 'NoText', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the objects element of the grammar.

  @precon  None.
  @postcon Returns true if the element was a objects element.

  @return  a Boolean

**)
function TTLSSchematicModule.Objects: Boolean;

var
  strName: String;
  NWs: TElementContainer;
  T : TTokenInfo;
  NW: TSchematicSetting;
  dblPercentage : Double;

begin
  Result := False;
  If Token.UToken = 'OBJECTS' Then
    Begin
      T := Token;
      NextNonCommentToken;
      If Percentage(dblPercentage) Then
        Begin
          If Token.Token = ';' Then
            FCurrentObject := dblPercentage
          Else
            Begin
              If Token.Token = ',' Then
                Begin
                  NextNonCommentToken;
                  If Token.TokenType In [ttSingleLiteral] Then
                    Begin
                      strName := Copy(Token.Token, 2, Length(Token.Token) - 2);
                      NWs := FindElement(strNamedWidths);
                      If NWs = Nil Then
                        NWs := Add(TlabelContainer.Create(strNamedWidths, scNone,
                          0,  0, iiPublicVariablesLabel, Nil));
                      NW := Nws.Add(TSchematicSetting.Create(strName, scNone,
                        T.Line, T.Column, iiPublicVariable, Nil)) As TSchematicSetting;
                      NW.Percentage := dblPercentage;
                      NextNonCommentToken;
                    End Else
                      ErrorAndSeekToken(strStringExpected, 'Objects', Token.Token,
                        strSeekableOnErrorTokens, stActual);
                End;
            End;
        End;
      If CheckLiteral(';', 'Objects') Then
        Result := True;
    End;
end;

(**

  This method parses the Object grammar element of the language.

  @precon  None.
  @postcon Returns true if the language element was parsed correctly.

  @return  a Boolean

**)
function TTLSSchematicModule.Object_: Boolean;

var
  Os: TElementContainer;
  O: TTLSObject;
  boolFound: Boolean;
  iColour: TColour;

begin
  Result := False;
  If IsKeyWord(Token.Token, ['diamond', 'ellipse', 'object']) Then
    Begin
      Os := FindElement(strObjects);
      If Os = Nil Then
        Os := Add(TLabelContainer.Create(strObjects, scNone, 0, 0, iiPublicClass, Nil));
      O := Os.Add(TTLSObject.Create(Format('Object%4.4d', [FObject]), scPublic, Token.Line,
        Token.Column, iiPublicClass, GetComment)) As TTLSObject;
      Inc(FObject);
      O.Width := FCurrentObject;
      O.LineColour := FLineColour;
      O.LineStyle := FLineStyle;
      O.LineWeight := FLineWeight;
      O.TextOrientation := FTextOrientation;
      O.TextPosition := FTextPosition;
      O.Shape := tstRectangle;
      If Token.UToken = 'ELLIPSE' Then
        O.Shape := tstEllipse
      Else If Token.UToken = 'DIAMOND' Then
        O.Shape := tstDiamond;
      NextNonCommentToken;
      If Chainages(O) Then
        If LocationEx(O) Then
          Begin
            boolFound := False;
            For iColour := Low(TColour) To High(TColour) Do
              If CompareText(Token.Token, strColours[iColour]) = 0 Then
                Begin
                  O.Colour := iColour;
                  boolFound := True;
                  Break;
                End;
            If boolFound Then
              Begin
                NextNonCommentToken;
                If CheckLiteral(',', 'Object_') Then
                  Begin
                    If Token.TokenType In [ttSingleLiteral] Then
                      Begin
                        O.Text := Copy(Token.Token, 2, Length(Token.Token) - 2);
                        NextNonCommentToken;
                        RouteCode(O);
                      End Else
                        ErrorAndSeekToken(strStringExpected, 'Object', Token.Token,
                          strSeekableOnErrorTokens, stActual);
                  End;
                If CheckLiteral(';', 'Object_') Then
                  Result := True;
              End Else
                ErrorAndSeekToken(strInvalidColourName, 'Object',
                  Token.Token, strSeekableOnErrorTokens, stActual);
          End Else
            ErrorAndSeekToken(strReservedWordExpected, 'Object',
              'Left, Right, Over, OverLeft, OverRight, Under or Both',
              strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method processes the offsets portion of the road grammar element.

  @precon  R must be a valid instance.
  @postcon Returns true if the offsets were parsed correctly.

  @param   R as a TTLSRoad
  @return  a Boolean

**)
Function TTLSSchematicModule.Offsets(R: TTLSRoad): Boolean;

Begin
  Result := False;
  If StartOffset(R) Then
    Begin
      If CheckLiteral(',', 'Offsets') Then
        If EndOffset(R) Then
          Begin
            If CheckLiteral(',', 'Offsets') Then
              Result := True;
          End Else
            ErrorAndSeekToken(strExpectedAnEndOffset, 'Offsets',
              Token.Token, strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strExpectedAStartOffet, 'Offsets', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses the Orientation element of the grammar.

  @precon  None.
  @postcon Returns the text orienation requested.


  @return  a TTextOrientation

**)
function TTLSSchematicModule.Orientation: TTextOrientation;

begin
  Result := toHorizontal;
  If IsKeyWord(Token.Token, ['horizontal', 'vertical']) Then
    Begin
      If CompareText(Token.Token, 'VERTICAL') = 0 Then
        Result := toVertical;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strExpectedHORIZONTALVERTICAL, 'Orientation',
        Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the LineColour element of the grammar.

  @precon  None.
  @postcon Returns true if the element was parsed successfully and sets the
           FLineColour variable to the new line colour.

  @return  a Boolean

**)
function TTLSSchematicModule.LineColour: Boolean;

Var
  iColour : TColour;

begin
  Result := False;
  For iColour := Low(TColour) To High(TColour) Do
    If CompareText(Token.Token, strColours[iColour]) = 0 Then
      Begin
        FLineColour := iColour;
        Result := True;
        NextNonCommentToken;
        Break;
      End;
  If Not Result Then
    ErrorAndSeekToken(strInvalidColourName, 'LineColour', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the Lines element of the grammar.

  @precon  None.
  @postcon Returns true if the element was parsed successfully.

  @return  a Boolean

**)
Function TTLSSchematicModule.Lines : Boolean;

begin
  Result := False;
  If Token.UToken = 'LINES' Then
    Begin
      NextNonCommentToken;
      If LineColour Then
        If CheckLiteral(',', 'Lines') Then
          Begin
            If LineStyle Then;
              If CheckLiteral(',', 'Lines') Then
                If LineWeight Then
                  If CheckLiteral(';', 'Lines') Then
                    Result := True;
          End;
    End;
end;

(**

  This method parses the LineStyle element of the grammar.

  @precon  None.
  @postcon Returns true if the element was parsed successfully and sets the
           FLineStyle variable to the new line style.

  @return  a Boolean

**)
Function TTLSSchematicModule.LineStyle : Boolean;

var
  iLineStyle: TLineStyle;

begin
  Result := False;
  For iLineStyle := Low(TLineStyle) To High(TLineStyle) Do
    If CompareText(Token.Token, strLineStyles[iLineStyle]) = 0 Then
      Begin
        FLineStyle := iLineStyle;
        Result := True;
        Break;
      End;
  If Result Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineStyle, 'LineStyle', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the LineWeight element of the grammar.

  @precon  None.
  @postcon Returns true if the element was parsed successfully and sets the
           FLineWeight variable to the new line weight.

  @return  a Boolean

**)
Function TTLSSchematicModule.LineWeight : Boolean;

var
  iLineWeight: TLineWeight;

begin
  Result := False;
  For iLineWeight := Low(TLineWeight) To High(TLineWeight) Do
    If CompareText(Token.Token, strLineWeights[iLineWeight]) = 0 Then
      Begin
        FLineWeight := iLineWeight;
        Result := True;
        Break;
      End;
  If Result Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineWeight, 'LineWeight', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the Location element of the grammar.

  @precon  S must be a valid instance of a TTLSShape.
  @postcon Returns true if the element was parsed correctly;

  @param   S as a TTLSShape
  @return  a Boolean

**)
function TTLSSchematicModule.Location(S : TTLSShape): Boolean;

begin
  Result := False;
  If IsKeyWord(Token.Token, ['left', 'right']) Then
    Begin
      S.Location := loLeft;
      If CompareText(Token.Token, 'RIGHT') = 0 Then
        S.Location := loRight;
      NextNonCommentToken;
      If CheckLiteral(',', 'LocationEx') Then
        Result := True;
    End;
end;

(**

  This method parses the LocationEx element of the grammar.

  @precon  S must be a valid instance of a TTLSShape.
  @postcon Returns true if the element was parsed correctly;

  @param   S as a TTLSShape
  @return  a Boolean

**)
function TTLSSchematicModule.LocationEx(S : TTLSShape): Boolean;

begin
  Result := Location(S);
  If Not Result Then
    If IsKeyWord(Token.Token, ['both', 'over', 'overleft', 'overright',
      'under']) Then
      Begin
        If CompareText(Token.Token, 'BOTH') = 0 Then
          S.Location := loBoth
        Else If CompareText(Token.Token, 'OVER') = 0 Then
          S.Location := loOver
        Else If CompareText(Token.Token, 'OVERLEFT') = 0 Then
          S.Location := loOverLeft
        Else If CompareText(Token.Token, 'OVERRIGHT') = 0 Then
          S.Location := loOverRight
        Else If CompareText(Token.Token, 'UNDER') = 0 Then
          S.Location := loUnder;
        NextNonCommentToken;
        If CheckLiteral(',', 'LocationEx') Then
          Result := True;
      End;
end;

(**

  This method checks for the presents of a literal in the token stream and
  returns true if found and moves to the next non comment token.

  @precon  None.
  @postcon Checks for the presents of a literal in the token stream and
           returns true if found and moves to the next non comment token.

  @param   strLiteral as a String
  @param   strMethod  as a String
  @return  a Boolean

**)
function TTLSSchematicModule.CheckLiteral(strLiteral, strMethod: String): Boolean;
begin
  Result := False;
  If Token.Token = strLiteral Then
    Begin
      Result := True;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strLiteralExpected, strMethod, strLiteral,
        strSeekableOnErrorTokens, stActual);
end;

(** Register the file source code extensions that can be parsed by this module. **)
Initialization
  ModuleDispatcher.Add('.schematic', TTLSSchematicModule, False, ctCPPBlock, ctCPPLine,
    ctCPPLine);
End.
