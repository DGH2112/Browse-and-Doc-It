(**

  This module contains a parser for the Eidolon Time Location Schematic Diagram
  Language.

  @Version    1.0
  @Date       24 Aug 2010
  @Author     David Hoyle

**)
Unit EidolonTLSSchematicModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule, EidolonModule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** An enumerate to define the location for Roads. **)
  TLocation = (loLeft, loRight);

  (** An enumerate to define the mearsurement percentages for the diagrams. **)
  TSetting = (seMargins, seText, seObjects, seRoads, seSpacing);

  (** A class to represent the roads in the diagram. **)
  TTLSRoad = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FStartChainage : Double;
    FEndChainage : Double;
    FStartOffset : Integer;
    FEndOffset : Integer;
    FLocation : TLocation;
    FColour : TColour;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the Road Start Chainage.
      @precon  None.
      @postcon Gets and sets the Road Start Chainage.
      @return  a Double
    **)
    Property StartChainage : Double Read FStartChainage Write FStartChainage;
    (**
      This property gets and sets the Road End Chainage.
      @precon  None.
      @postcon Gets and sets the Road End Chainage.
      @return  a Double
    **)
    Property EndChainage : Double Read FEndChainage Write FEndChainage;
    (**
      This property gets and sets the Road end offset.
      @precon  None.
      @postcon Gets and sets the Road end offset.
      @return  a Integer
    **)
    Property StartOffset : Integer Read FStartOffset Write FStartOffset;
    (**
      This property gets and sets the Road start offset.
      @precon  None.
      @postcon Gets and sets the Road start offset.
      @return  a Integer
    **)
    Property EndOffset : Integer Read FEndOffset Write FEndOffset;
    (**
      This property gets and sets the Road location.
      @precon  None.
      @postcon Gets and sets the Road location.
      @return  a TLocation
    **)
    Property Location : TLocation Read FLocation Write FLocation;
    (**
      This property gets and sets the Road colour.
      @precon  None.
      @postcon Gets and sets the Road colour.
      @return  a TColour
    **)
    Property Colour : TColour Read FColour Write FColour;
  End;

  (** A class to represent a schematic setting. **)
  TSchematicSetting = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPercentage : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets ans sets the percentage of the settings.
      @precon  None.
      @postcon Gets ans sets the percentage of the settings.
      @return  an Integer
    **)
    Property Percentage : Integer Read FPercentage Write FPercentage;
  End;

  (** A pascal specific implementation of comments. **)
  TTLSSchematicComment = Class(TComment)
  Public
    Class Function CreateComment(strComment: String; iLine,
      iCol: Integer): TComment; Override;
  End;

  (** This is the main class for dealing with backus-naur grammar files. **)
  TTLSSchematicModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource        : String;
    FRoad          : Integer;
    FSettings      : Array[Low(TSetting)..High(TSetting)] Of Integer;
    FMaxRoads: Double;
    { Grammar Parsers }
    Procedure Goal;
    Function Road : Boolean;
    Function Objects : Boolean;
    Function Roads : Boolean;
    Function Margins : Boolean;
    Function Spacing : Boolean;
    Function Text_ : Boolean;
    Function StartChainage(R : TTLSRoad) : Boolean;
    Function EndChainage(R : TTLSRoad) : Boolean;
    Function StartOffset(R : TTLSRoad) : Boolean;
    Function EndOffset(R : TTLSRoad) : Boolean;
    Function Chainages(R : TTLSRoad) : Boolean;
    Function Offsets(R : TTLSRoad) : Boolean;
    Procedure Percentage(S : TSchematicSetting);
    Function UnknownToken : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    function GetSettings(S: TSetting): Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Function GetModuleName : String; Override;
    procedure TidyUpEmptyElements;
  Public
    Constructor CreateParser(Source : String; strFileName : String;
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
  End;

ResourceString
  (** A resource string for the Roads node in the tree. **)
  strRoads = 'Roads';

Implementation

Uses
  DGHLibrary;

Const
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', ',', '.', '/',
    ':', '=', '@', '[', ']', '^', '|', '%'];
  (** A set of characters for single quotes **)
  strSingleQuotes : Set Of AnsiChar = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes : Set Of AnsiChar = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers :  Set Of AnsiChar = ['-', '_', 'a'..'z', 'A'..'Z', '<', '>'];
  (** A set of number characters. **)
  strNumbers:  Set Of AnsiChar = ['#', '$', '0'..'9'];

  (** A set of reserved words (not used in this parser.) **)
  strReservedWords : Array[0..5] Of String = ('margins', 'objects', 'road',
    'roads', 'spacing', 'text');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..1] Of String = (';');
  (** A constant array of Locations. **)
  strLocations : Array[Low(TLocation)..High(TLocation)] Of String = (
    'Left', 'Right');

ResourceString
  (** A resource string for the settings node. **)
  strSettings = 'Settings';
  (** A resource string for an unexpected token instead of an integer. **)
  strIntegerExpected = 'An integer number is expected but ''%s'' found at line %d column %d.';
  (** A resource string for an invalid colour name. **)
  (** A resource string for an invalid colour. **)
  strInvalidColourName = 'Invalid colour name ''%s'' at line %d column %d.';
  (** A resource string for percentages that don`t add up to 100. **)
  strThePercentageSpacings = 'The Percentage spacings for elements does not ' +
    'add up to 100%% (Total: %d%%, ' +
    'Margins: %d%%, ' +
    'Text: %d%%, ' +
    'Roads: %d%%, ' +
    'Objects: %d%% and ' +
    'Spacing: %d%%';
  (** A resource string for an unexpected end chainage. **)
  strExpectedAnEndChainage = 'Expected an end chainage but ''%s'' found at l' +
  'ine %d column %d.';
  (** A resource string for an unexpected start chainage. **)
  strExpectedAStartChainage = 'Expected a start chainage but ''%s'' found at' +
  ' line %d column %d.';
  (** A resource string for an unexpected end offset. **)
  strExpectedAnEndOffset = 'Expected an end offset but found ''%s'' at line ' +
  '%d column %d.';
  (** A resource string for an unexpected start offset. **)
  strExpectedAStartOffet = 'Expected a start offset but found ''%s'' at line' +
  ' %d column %d.';

(**

  This method is a class method to first check the comment for being a
  documentation comment and then creating an instance of a TComment class and
  parsing the comment via the constructor.

  @precon  strComment is the full comment to be checked and parsed, iLine is
           the line number of the comment and iCol is the column number of
           the comment.

  @postcon Returns Nil if this is not a documentation comment or returns a
           valid TComment class.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
class function TTLSSchematicComment.CreateComment(strComment: String; iLine,
  iCol: Integer): TComment;

begin
  Result := Nil;
  If Length(strComment) > 0 Then
    Begin
      Case strComment[1] Of
        '/' : strComment := Copy(strComment, 2, Length(strComment) - 1);
      End;
      If Length(strComment) > 0 Then
        Begin
          If strComment[1] = '*' Then
            strComment := Copy(strComment, 2, Length(strComment) - 3);
          If Length(strComment) > 0 Then
            Begin
              If strComment[1] = '/' Then
                strComment := Copy(strComment, 2, Length(strComment) - 1);
              If Length(strComment) > 0 Then
                Begin
                  If strComment[1] = ':' Then
                    Begin;
                      strComment := Copy(strComment, 2, Length(strComment) - 1);
                      Result := Create(strComment, iLine, iCol);
                    End
                  Else If strComment[1] = '*' Then
                    Begin;
                      strComment := Copy(strComment, 2, Length(strComment) - 2);
                      Result := Create(strComment, iLine, iCol);
                    End;
                End;
            End;
        End;
    End;
end;

(**

  This is the constructor method for the TBackusnaurModule class.

  @precon  Source is a valid TStream descendant containing as stream of text,
           that is the contents of a source code module and Filename is the
           file name of the module being parsed and IsModified determines if
           the source code module has been modified since the last save to
           disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TTLSSchematicModule.CreateParser(Source : String; strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Var
  boolCascade : Boolean;
  i: Integer;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BrowseAndDocItOptions.Defines);
  FSource := Source;
  FRoad := 1;
  FmaxRoads := 1;
  FSettings[seMargins] := 2;
  FSettings[seRoads] := 20;
  FSettings[seObjects] := 20;
  FSettings[seSpacing] := 2;
  FSettings[seText] := 6;
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
      i := (FSettings[seMargins] + FSettings[seText] + FSettings[seObjects] +
        FSettings[seRoads] + FSettings[seSpacing]) * 2;
      If i <> 100 Then
        AddIssue(Format(strThePercentageSpacings, [i, FSettings[seMargins],
          FSettings[seText], FSettings[seRoads], FSettings[seObjects],
          FSettings[seSpacing]]), scNone, 'CreateParser', 0, 0, etWarning);
      AddTickCount('Check');
      TidyUpEmptyElements;
    End;
End;

(**


  This is a destructor for the TBackusNaurModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TTLSSchematicModule.Destroy;
begin
  Inherited Destroy;
end;

(**

  This method parses the text element of the grammar.

  @precon  None.
  @postcon Returns true if the element was a text element.

  @return  a Boolean

**)
function TTLSSchematicModule.Text_: Boolean;

var
  Ts: TElementContainer;
  T: TSchematicSetting;

begin
  Result := False;
  If Token.UToken = 'TEXT' Then
    Begin
      Ts := FindElement(strSettings);
      If Ts = Nil Then
        Ts := Add(TLabelContainer.Create(strSettings, scNone, 0, 0,
          iiPublicTypesLabel, Nil)) As TLabelContainer;
      T := Ts.Add(TSchematicSetting.Create('Text', scNone, Token.Line,
        Token.Column, iiPublicType, Nil)) As TSchematicSetting;
      NextNonCommentToken;
      Percentage(T);
      FSettings[seText] := T.Percentage;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Text_', ';',
            strSeekableOnErrorTokens, stActual);
    End;
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
  TBlockType = (btNoBlock, btLineComment, btFullComment);

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
  LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
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
            Else If (ch = '%') And (LastCharType = ttIdentifier) Then
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
            If Not (((BlockType In [btLineComment]) And (CurCharType <> ttLineEnd)) Or
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
                LastCharType := ttReservedWord;
              AddToken(TTokenInfo.Create(strToken, iStreamPos,
                iTokenLine, iTokenColumn, Length(strToken), LastCharType));
            End;
        End;
    AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
      ttFileEnd));
  Except
    On E : Exception Do
      AddIssue(E.Message, scGlobal, 'TokenizeStream', 0, 0, etError);
  End
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

  @param   S as a TSchematicSetting

**)
Procedure TTLSSchematicModule.Percentage(S: TSchematicSetting);

Var
  i : Integer;
  iErrorCode : Integer;

begin
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, i, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          S.Percentage := i;
          NextNonCommentToken;
          If Token.Token = '%' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'Percentage', '%',
              strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strIntegerExpected, 'Percentage', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strNumberExpected, 'Percentage', Token.Token,
        strSeekableOnErrorTokens, stActual);
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
      NextNonCommentToken;
      If Chainages(R) Then
        If Offsets(R) Then
          If IsKeyWord(Token.Token, ['left', 'right']) Then
            Begin
              R.Location := loLeft;
              If CompareText(Token.Token, 'RIGHT') = 0 Then
                R.Location := loRight;
              NextNonCommentToken;
              If Token.Token = ',' Then
                Begin
                  NextNonCommentToken;
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
                      If Token.Token = ';' Then
                        Begin
                          NextNonCommentToken;
                          Result := True;
                        End Else
                          ErrorAndSeekToken(strLiteralExpected, 'Road', ';',
                            strSeekableOnErrorTokens, stActual);
                    End Else
                      ErrorAndSeekToken(strInvalidColourName, 'Road',
                        Token.Token, strSeekableOnErrorTokens, stActual);
                End Else
                  ErrorAndSeekToken(strLiteralExpected, 'Road', ',',
                    strSeekableOnErrorTokens, stActual);
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

var
  Rs: TElementContainer;
  R: TSchematicSetting;

begin
  Result := False;
  If Token.UToken = 'ROADS' Then
    Begin
      Rs := FindElement(strSettings);
      If Rs = Nil Then
        Rs := Add(TLabelContainer.Create(strSettings, scNone, 0, 0,
          iiPublicTypesLabel, Nil)) As TLabelContainer;
      R := Rs.Add(TSchematicSetting.Create('Roads', scNone, Token.Line,
        Token.Column, iiPublicType, Nil)) As TSchematicSetting;
      NextNonCommentToken;
      Percentage(R);
      FSettings[seRoads] := R.Percentage;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Roads', ';',
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
      Percentage(S);
      FSettings[seSpacing] := S.Percentage;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Spacing', ';',
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the StartChainage element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if a number was parsed.

  @param   R as a TTLSRoad
  @return  a Boolean

**)
function TTLSSchematicModule.StartChainage(R : TTLSRoad): Boolean;

Var
  dbl : Double;
  iErrorCode : Integer;

begin
  Result := False;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dbl, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          R.StartChainage := dbl;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'StartChainage', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
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

begin
  Result := False;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, i, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          R.StartOffset := i;
          If i > FMaxRoads Then
            FMaxRoads := i;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'StartOffset', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
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

  This method parses the EndChainage element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if an double was parsed.

  @param   R as a TTLSRoad
  @return  a Boolean

**)
function TTLSSchematicModule.EndChainage(R : TTLSRoad): Boolean;

Var
  dbl : Double;
  iErrorCode : Integer;

begin
  Result := False;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dbl, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          R.EndChainage := dbl;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'EndChainage', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
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

begin
  Result := False;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, i, iErrorCode);
      If iErrorCode = 0 Then
        Begin
          R.EndOffset := i;
          If i > FMaxRoads Then
            FMaxRoads := i;
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'EndOffset', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
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

  This method processes the chinages from the road element of the grammar.

  @precon  R must be a valid instance.
  @postcon Returns true if the chainages were successfully parsed.

  @param   R as a TTLSRoad
  @return  a Boolean

**)
Function TTLSSchematicModule.Chainages(R : TTLSRoad): Boolean;

Begin
  Result := False;
  If StartChainage(R) Then
    Begin
      If Token.Token = ',' Then
        Begin
          NextNonCommentToken;
          If EndChainage(R) Then
            Begin
              If Token.Token = ',' Then
                Begin
                  NextNonCommentToken;
                  Result := True
                End Else
                  ErrorAndSeekToken(strLiteralExpected, 'Chainages', ';',
                    strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strExpectedAnEndChainage, 'Chainages',
                Token.Token, strSeekableOnErrorTokens, stActual);
        End
      Else
        ErrorAndSeekToken(strLiteralExpected, 'Chainages', ',',
          strSeekableOnErrorTokens, stActual);
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
          While Road or Objects Or Roads Or Margins Or Spacing Or Text_ Or
            UnknownToken Do;
        If Not (Token.TokenType In [ttFileEnd]) Then
          Raise EParserAbort.Create(strUnExpectedEndOfFile);
      End;
  Except
    On E : EParserAbort Do
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
      Percentage(M);
      FSettings[seMargins] := M.Percentage;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Margins', ';',
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
  Os: TElementContainer;
  O: TSchematicSetting;

begin
  Result := False;
  If Token.UToken = 'OBJECTS' Then
    Begin
      Os := FindElement(strSettings);
      If Os = Nil Then
        Os := Add(TLabelContainer.Create(strSettings, scNone, 0, 0,
          iiPublicTypesLabel, Nil)) As TLabelContainer;
      O := Os.Add(TSchematicSetting.Create('Objects', scNone, Token.Line,
        Token.Column, iiPublicType, Nil)) As TSchematicSetting;
      NextNonCommentToken;
      Percentage(O);
      FSettings[seObjects] := O.Percentage;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Result := True;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Objects', ';',
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
      If Token.Token = ',' Then
        Begin
          NextNonCommentToken;
          If EndOffset(R) Then
            Begin
              If Token.Token = ',' Then
                Begin
                  NextNonCommentToken;
                  Result := True
                End Else
                  ErrorAndSeekToken(strLiteralExpected, 'Offset', ',',
                    strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strExpectedAnEndOffset, 'Offsets',
                Token.Token, strSeekableOnErrorTokens, stActual);
        End
      Else
        ErrorAndSeekToken(strLiteralExpected, 'Offset', ',',
          strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strExpectedAStartOffet, 'Offsets', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

{ TTLSRoad }

(**

  This method returns a string representation of a road.

  @precon  None.
  @postcon Returns a string representation of a road.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TTLSRoad.AsString(boolShowIdentifier, boolForDocumentation: Boolean) : String;

Begin
  Result := Format('Road %1.1f, %1.1f, %d, %d, %s, %s', [FStartChainage,
    FEndChainage, FStartOffset, FEndOffset, strLocations[FLocation],
    strColours[FColour]]);
End;

{ TSchematicSetting }

(**

  This method returns a string representation of a schematic setting.

  @precon  None.
  @postcon Returns a string representation of a schematic setting.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TSchematicSetting.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

begin
  Result := Format('%s %d%%', [Identifier, FPercentage]);
end;

End.
