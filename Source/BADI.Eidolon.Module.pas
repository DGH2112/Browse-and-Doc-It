(**

  EidolonModule : A unit to parser Eidolon code. Please refer to the file
  "Eidolon Map File Grammar.bnf" for the complete grammar implemented.

  @Version    1.0
  @Date       15 Oct 2017
  @Author     David Hoyle

**)
Unit BADI.Eidolon.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.Eidolon.Types,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.Types,
  BADI.TokenInfo,
  BADI.Eidolon.BaseTable,
  BADI.Eidolon.FieldDef,
  BADI.Eidolon.DBTable,
  BADI.Eidolon.TimeLocationTable,
  BADI.Eidolon.Symbol,
  BADI.Eidolon.CustomFillSymbol,
  BADI.Eidolon.Line,
  BADI.Eidolon.Bar,
  BADI.Eidolon.Rectangle,
  BADI.Eidolon.SuperBar,
  BADI.Eidolon.Diamond,
  BADI.Eidolon.Triangle,
  BADI.Eidolon.Ellipse,
  BADI.Eidolon.RequirementsTable,
  BADI.Eidolon.TextTable;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is the main class for dealing with backus-naur grammar files. **)
  TEidolonModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource : String;
    FTextTableDefs: TLabelContainer;
    FDBTableDefs  : TLabelContainer;
    FTimeLocationTableDefs: TLabelContainer;
    FOutputTableDefs: TLabelContainer;
    FRequirementsTableDefs: TLabelContainer;
    { Grammar Parsers }
    Procedure Goal;
    Function  Table : Boolean;
    Function  TextTable(strName : String; StartToken : TTokenInfo;
      C : TComment) : Boolean;
    Function  DBTable(strName : String; StartToken : TTokenInfo;
      C : TComment) : Boolean;
    Function  TimeLocationTable(strName : String; StartToken : TTokenInfo;
      C : TComment) : Boolean;
    Procedure TextTableDef(TextTable : TTextTable);
    Function  FieldDef(Table : TBaseTable; var iSheetIndex : Integer): Boolean;
    Procedure TypeInfo(Field : TFieldDef);
    Function  DatabaseDef(DBTable : TDBTable; ConnectionType : TConnectionType) : Boolean;
    Procedure ConnectionDef(DBTable : TDBTable; ConnectionType : TConnectionType);
    Procedure TableNameDef(DBTable : TDBTable; ConnectionType : TConnectionType);
    Function  TimeLocationDef(Table : TTimeLocationTable;
      var iLayerIndex : Integer) : Boolean;
    Function  TLLine(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Procedure BorderDef(Symbol : TSymbol);
    Procedure BorderColour(Symbol : TSymbol);
    Function  ColourName : TColour;
    Procedure BorderLineStyle(Symbol : TSymbol);
    Procedure BorderWeight(Symbol : TSymbol);
    Function  TLRectangle(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Function  TLBar(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Function  TLSuperBar(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Procedure InteriorDef(CustomSymbol : TCustomFillSymbol);
    Procedure Transparency(CustomSymbol : TCustomFillSymbol);
    Procedure InteriorColour(CustomSymbol : TCustomFillSymbol);
    Procedure InteriorPattern(CustomSymbol : TCustomFillSymbol);
    Procedure InteriorPatternColour(CustomSymbol : TCustomFillSymbol);
    Procedure BarWidth(Bar : TBar);
    Procedure DateWidth(SuperBar : TSuperBar);
    Procedure LocationWidth(SuperBar : TSuperBar);
    Function  TLDiamond(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Function  TLTriangle(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Function  TLEllipse(strName : String; StartToken : TTokenInfo;
      TLT : TTimeLocationTable; var iLayerIndex : Integer) : Boolean;
    Procedure DiamondSize(Diamond : TDiamond);
    Procedure TriangleType(Triangle : TTriangle);
    Procedure EllipseSize(Ellipse : TEllipse);
    Function  OutputTable(strName : String; StartToken : TTokenInfo;
      C : TComment) : Boolean;
    Function  RequirementsTable(strName : String; StartToken : TTokenInfo;
      C : TComment) : Boolean;
    Function  AssociationDef(RequirementsTable : TRequirementsTable): Boolean;
    Procedure LineEndDefs(Line : TLine);
    Procedure StartType(Line : TLine);
    Procedure StartSize(Line : TLine);
    Procedure EndType(Line : TLine);
    Procedure EndSize(Line : TLine);
    Procedure LineEndType(Line : TLine; LineEnd : TLineEnd);
    Procedure LineEndSize(Line : TLine; LineEnd : TLineEnd);
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure EatLineEnds;
    Function  CheckLiteral(strLiteral : String) : Boolean;
    Function  CheckLineEnd() : Boolean;
    Procedure EatWhitespace;
    Function  EmptyLine : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    procedure TidyUpEmptyElements;
    Function GetModuleName : String; Override;
  Public
    Constructor CreateParser(const Source, strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.Eidolon.Association,
  BADI.Eidolon.ResourceStrings,
  BADI.Eidolon.Constants,
  BADI.Eidolon.ConnectionDef,
  BADI.Eidolon.Comment,
  BADI.Eidolon.DatabaseDef,
  BADI.Eidolon.OutputTable,
  BADI.Eidolon.TableNameDef,
  BADI.Eidolon.TextTableDef;

(**

  This method parses the Bar element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and TLT
           must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a BAR was parsed.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TLBar(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable; var iLayerIndex : Integer): Boolean;

Var
  B : TBar;

begin
  Result := False;
  If CompareText(Token.Token, 'BAR') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      B := TLT.AddSymbol(TBar.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TBar;
      B.SymbolType := tstBar;
      B.LayerIndex := iLayerIndex;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(B);
          EatWhitespace;
          If CheckLiteral(',') Then
            Begin
              InteriorDef(B);
              EatWhiteSpace;
              If CheckLiteral(',') Then
                Begin
                  BarWidth(B);
                  Transparency(B);
                  CheckLineEnd();
                End;
            End;
        End;
    End;
end;

(**

  This method parses the AssociationDef element of the grammar.

  @precon  RequirementsTable must be a valid instance of a TRequirementsTable
           class.
  @postcon parses the AssociationDef element of the grammar.

  @param   RequirementsTable as a TRequirementsTable
  @return  a Boolean

**)
function TEidolonModule.AssociationDef(
  RequirementsTable: TRequirementsTable): Boolean;
var
  strName: String;
  A: TAssociation;
  T : TTokenInfo;

begin
  EatWhitespace;
  Result := False;
  If Token.Token = '@' Then
    Begin
      Result := True;
      NextNonCommentToken;
      T := Token;
      strName := '';
      While Token.Token <> '=' Do
        Begin
          strName := strName + Token.Token;
          NextNonCommentToken;
        End;
      If strName <> '' Then
        Begin
          A := RequirementsTable.AddAssociation(TAssociation.Create(strName,
            scNone, T.Line, T.Column, iiPublicClass, Nil)) As TAssociation;
          If CheckLiteral('=') Then
            Begin
              strName := '';
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(A);
              CheckLineEnd();
            End;
        End Else
          ErrorAndSeekToken(strNullName, Token.Token, strSeekableOnErrorTokens, stActual);

    End;
end;

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TEidolonModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Begin
  Result := ChangeFileExt(Inherited AsString(boolShowIdentifier,
    boolForDocumentation), '');
End;

(**

  This method parses the BarWidth element of the grammar.

  @precon  Bar must be a valid instance of a TBar class.
  @postcon Sets the width of the bar.

  @param   Bar as a TBar

**)
procedure TEidolonModule.BarWidth(Bar: TBar);

Var
  iWidth, iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, iWidth, iErrorCode);
      If (iErrorCode > 0) Or (iWidth <= 0.0) Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, Token.Line, Token.Column, etError);
      Bar.BarWidth := iWidth;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the BorderColour element of the grammar.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon Assigns the border colour to the symbol.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderColour(Symbol: TSymbol);
begin
  Symbol.BorderColour := ColourName;
  //: @debug This does not seem to cause an error here!!!!
  {If (Symbol Is TLine) And (Symbol.BorderColour = xlcNONE)  Then
    AddIssue(Format(strLineColourOfNoneError, [Token.Line, Token.Column]), scPublic,
      'BorderColour', Token.Line, Token.Column, etError);}
end;

(**

  This method parses the BorderDef element of the grammar.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon Parses the BorderDef element of the grammar.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderDef(Symbol: TSymbol);
begin
  EatWhitespace;
  BorderColour(Symbol);
  EatWhitespace;
  If CheckLiteral(',') Then
    Begin
      EatWhitespace;
      BorderLineStyle(Symbol);
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          EatWhitespace;
          BorderWeight(Symbol);
        End;
    End;
end;

(**

  This method parses the BorderLineStyle element of the grammar.

  @precon  Symbol must be a valid instance of TSymbol.
  @postcon Assigns the line style to the symbol.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderLineStyle(Symbol: TSymbol);

var
  boolFound: Boolean;
  iLineStyle: TLineStyle;

begin
  Symbol.BorderLineStyle := lsSOLID;
  boolFound := False;
  For iLineStyle := Low(TLineStyle) To High(TLineStyle) Do
    If CompareText(Token.Token, strLineStyles[iLineStyle]) = 0 Then
      Begin
        Symbol.BorderLineStyle := iLineStyle;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineStyle, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the BorderWeight element of the grammar.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon Assigns the border weight to the symbol.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderWeight(Symbol: TSymbol);

var
  boolFound: Boolean;
  iLineWeight: TLineWeight;

begin
  Symbol.BorderWeight := lw0_25;
  boolFound := False;
  For iLineWeight := Low(TLineWeight) To High(TLineWeight) Do
    If CompareText(Token.Token, strLineWeights[iLineWeight]) = 0 Then
      Begin
        Symbol.BorderWeight := iLineWeight;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineWeight, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method checks for the presents of line end characters in the token stream
  and returns true if found and moves to the next non comment token after the
  line end characters.

  @precon  None.
  @postcon Checks for the presents of line end characters in the token stream
           and returns true if found and moves to the next non comment token
           after the line end characters.

  @return  a Boolean

**)
function TEidolonModule.CheckLineEnd(): Boolean;

begin
  Result := False;
  If Token.TokenType In [ttLineEnd] Then
    Begin
      Result := True;
      EatLineEnds;
    End Else
      ErrorAndSeekToken(strExpectedLineEnd, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method checks for the presents of a literal in the token stream and
  returns true if found and moves to the next non comment token.

  @precon  None.
  @postcon Checks for the presents of a literal in the token stream and
           returns true if found and moves to the next non comment token.

  @param   strLiteral as a String
  @return  a Boolean

**)
function TEidolonModule.CheckLiteral(strLiteral: String): Boolean;
begin
  Result := False;
  If Token.Token = strLiteral Then
    Begin
      Result := True;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strLiteralExpected, strLiteral, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the ColourName element of the grammar.

  @precon  None.
  @postcon Returns the enumerate associated with colour at the current token
           else returns xlcNone and registers an error.

  @return  a TColour

**)
function TEidolonModule.ColourName: TColour;

var
  iColour: TColour;
  boolFound: Boolean;

begin
  Result := xlcNone;
  boolFound := False;
  For iColour := Low(TColour) To High(TColour) Do
    If CompareText(Token.Token, strColours[iColour]) = 0 Then
      Begin
        Result := iColour;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidColourName, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the ConnectionDef element of the grammar.

  @precon  Table must be a valid instance of a TBaseTable class.
  @postcon Parses the ConnectionDef element of the grammar.

  @param   DBTable        as a TDBTable
  @param   ConnectionType as a TConnectionType

**)
procedure TEidolonModule.ConnectionDef(DBTable : TDBTable; ConnectionType : TConnectionType);

Const
  strValidConnections : Array[1..5] Of String = ('', 'dbase iv;', 'foxpro 2.6;',
    'paradox 4.x;', 'text;');

Var
  C : TConnectionDef;
  strConnection: String;
  iToken: Integer;

begin
  If Token.Token = strConnectionType[ConnectionType] Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, strConnectionLabel) = 0 Then
        Begin
          Case ConnectionType Of
            ctPrimary:
              C := DBTable.AddPrimary(TConnectionDef.Create('Connection', scNone,
                Token.Line, Token.Column, iiPublicType, Nil)) As TConnectionDef;
          Else
            C := DBTable.AddSecondary(TConnectionDef.Create('Connection', scNone,
              Token.Line, Token.Column, iiPublicType, Nil)) As TConnectionDef;
          End;
          NextNonCommentToken;
          If CheckLiteral('=') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(C);
              CheckLineEnd();
              strConnection := '';
              For iToken := 0 To C.TokenCount - 1 Do
                strConnection := strConnection + C.Tokens[iToken].Token;
              If Not IsKeyWord(strConnection, strValidConnections) Then
                AddIssue(Format(strIsNotAValidConnection, [strConnection, C.Line,
                  C.Column]), scNone, C.Line, C.Column, etWarning);
            End;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This is the constructor method for the TEidolonModule class.

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
Constructor TEidolonModule.CreateParser(const Source, strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FSource := Source;
  FTextTableDefs := AddUnique(TLabelContainer.Create(strTextTableDefsLabel, scNone,
    0, 0, iiPublicThreadVarsLabel, Nil)) As TLabelContainer;
  FDBTableDefs := AddUnique(TLabelContainer.Create(strDatabaseTableDefsLabel, scNone,
    0, 0, iiPublicConstantsLabel, Nil)) As TLabelContainer;
  FTimeLocationTableDefs := AddUnique(TLabelContainer.Create(strTimeLocationTableDefsLabel,
    scNone, 0, 0, iiPublicVariablesLabel, Nil)) As TLabelContainer;
  FOutputTableDefs := AddUnique(TLabelContainer.Create(strOutputTableDefsLabel, scNone,
    0, 0, iiInterfacesLabel, Nil)) As TLabelContainer;
  FRequirementsTableDefs := AddUnique(TLabelContainer.Create(strRequirementsTableDefsLabel, scNone,
    0, 0, iiDispInterfacesLabel, Nil)) As TLabelContainer;
  AddTickCount('Start');
  CommentClass := TEidolonComment;
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
      TidyUpEmptyElements;
    End;
End;

(**

  This method parses the DatabaseDef element of the grammar.

  @precon  DBTable must be a valid instance of a TDBTable class.
  @postcon Returns true of their was a database definition parsed.

  @param   DBTable        as a TDBTable
  @param   ConnectionType as a TConnectionType
  @return  a Boolean

**)
function TEidolonModule.DatabaseDef(DBTable : TDBTable; ConnectionType : TConnectionType): Boolean;

var
  D: TDatabaseDef;
  strFileName: String;
  iToken: Integer;

begin
  Result := False;
  If Token.Token = strConnectionType[ConnectionType] Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, strDatabaseLabel) =  0 Then
        Begin
          Result := True;
          Case ConnectionType Of
            ctPrimary:
              D := DBTable.AddPrimary(TDatabaseDef.Create('Database', scNone, Token.Line,
                Token.Column, iiPublicType, Nil)) As TDatabaseDef;
          Else
            D := DBTable.AddSecondary(TDatabaseDef.Create('Database', scNone, Token.Line,
              Token.Column, iiPublicType, Nil)) As TDatabaseDef;
          End;
          NextNonCommentToken;
          If CheckLiteral('=') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(D);
              CheckLineEnd();
              strFileName := '';
              For iToken := 0 To D.TokenCount - 1 Do
                strFileName := strFileName + D.Tokens[iToken].Token;
            End;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the DateWidth element of the grammar.

  @precon  SuperBar must be a valid instance.
  @postcon If the width is valid, it is assigned to the super bar else an exception is
           raised.

  @param   SuperBar as a TSuperBar

**)
Procedure TEidolonModule.DateWidth(SuperBar: TSuperBar);

Var
  dblWidth : Double;
  iErrorCode : Integer;

Begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dblWidth, iErrorCode);
      If (iErrorCode > 0) Or (dblWidth <= 0.0) Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, Token.Line, Token.Column, etError);
      SuperBar.DateWidth := dblWidth;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses the DBTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Parses the DBTable element of the grammar.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   C          as a TComment
  @return  a Boolean

**)
Function TEidolonModule.DBTable(strName : String; StartToken : TTokenInfo;
  C : TComment) : Boolean;
var
  DBT: TDBTable;
  iSheetIndex: Integer;

begin
  Result := False;
  If Token.UToken = 'DBTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          DBT := FDBTableDefs.AddUnique(TDBTable.Create(strName, scNone, StartToken.Line,
            StartToken.Column, iiPublicConstant, C)) As TDBTable;
          If CheckLiteral(')') Then
            If CheckLineEnd() Then
              Begin
                EatWhitespace;
                If CheckLiteral('{') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd() Then
                      Begin
                        While EmptyLine Do;
                        EatWhitespace;
                        If DatabaseDef(DBT, ctPrimary) Then
                          Begin
                            While EmptyLine Do;
                            EatWhitespace;
                            ConnectionDef(DBT, ctPrimary);
                            While EmptyLine Do;
                            EatWhitespace;
                            TableNameDef(DBT, ctPrimary);
                          End;
                        iSheetIndex := 1;
                        While FieldDef(DBT, iSheetIndex) Or EmptyLine Do;
                        If CheckLiteral('}') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd();
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**


  This is a destructor for the TEidolonModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TEidolonModule.Destroy;
begin
  Inherited Destroy;
end;

(**

  This method parses the DiamondSize element of the grammar.

  @precon  Diamond must be a valid instance of a TDiamond class.
  @postcon Parses the DiamondSize element of the grammar.

  @param   Diamond as a TDiamond

**)
procedure TEidolonModule.DiamondSize(Diamond : TDiamond);

Var
  iSize, iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, iSize, iErrorCode);
      If (iErrorCode > 0) Or (iSize <= 0)  Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, Token.Line, Token.Column, etError);
      Diamond.DiamondSize:= iSize;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This meothd parses the Diamond element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and TLT
           must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a DIAMOND was parsed.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TLDiamond(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable; var iLayerIndex : Integer): Boolean;

Var
  D : TDiamond;

begin
  Result := False;
  If CompareText(Token.Token, 'DIAMOND') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      D := TLT.AddSymbol(TDiamond.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TDiamond;
      D.SymbolType := tstDiamond;
      D.LayerIndex := iLayerIndex;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(D);
          EatWhitespace;
          If CheckLiteral(',') Then
            Begin
              InteriorDef(D);
              EatWhiteSpace;
              If CheckLiteral(',') Then
                Begin
                  DiamondSize(D);
                  Transparency(D);
                  CheckLineEnd();
                End;
            End;
        End;
    End;
end;

(**

  This method eats the line ends when found and puts the token at the first
  token on the next non-null line.

  @precon  None.
  @postcon Eats the line ends when found and puts the token at the first
           token on the next non-null line.

**)
procedure TEidolonModule.EatLineEnds;
begin
  While Token.TokenType In [ttLineEnd] Do
    NextNonCommentToken;
end;

(**

  This method eats the whitespace when found and puts the token at the next non
  whitespace token.

  @precon  None.
  @postcon Eats the whitespace when found and puts the token at the next non
           whitespace token.

**)
procedure TEidolonModule.EatWhitespace;
begin
  While Token.TokenType In [ttWhiteSpace] Do
    NextNonCommentToken;
end;

(**

  This method parses the EllipseSize element of the grammar.

  @precon  Ellipse must be a valid instance of a TEllipse class.
  @postcon Sets the EllipseSize property of the ellipse.

  @param   Ellipse as a TEllipse

**)
procedure TEidolonModule.EllipseSize(Ellipse: TEllipse);

Var
  iSize, iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, iSize, iErrorCode);
      If (iErrorCode > 0) Or (iSize <= 0)  Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, Token.Line, Token.Column,etError);
      Ellipse.EllipseSize:= iSize;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method eats both whitespace and line ends which make up empty lines. If
  whitespace or line ends where eaten the returns true.

  @precon  None.
  @postcon Eats both whitespace and line ends which make up empty lines. If
           whitespace or line ends where eaten the returns true.

  @return  a Boolean

**)
function TEidolonModule.EmptyLine: Boolean;
begin
  Result := False;
  While Token.TokenType In [ttLineEnd, ttWhiteSpace] Do
    Begin
      Result := True;
      NextNonCommentToken;
    End;
end;

(**

  This method parses the EndSize element of the grammar.

  @precon  Line must be a valid instance.
  @postcon Parses the EndSize element of the grammar.

  @param   Line as a TLine

**)
procedure TEidolonModule.EndSize(Line: TLine);
begin
  LineEndSize(Line, leEnd);
end;

(**

  This method parses the EndType element of the grammar.

  @precon  Line must be a valid instance.
  @postcon Parses the EndType element of the grammar.

  @param   Line as a TLine

**)
procedure TEidolonModule.EndType(Line: TLine);
begin
  LineEndType(Line, leEnd);
end;

(**

  This method parses the Ellipse element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and TLT
           must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a ELLIPSE was parsed.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TLEllipse(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable; var iLayerIndex : Integer): Boolean;

Var
  E : TEllipse;

begin
  Result := False;
  If CompareText(Token.Token, 'ELLIPSE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      E := TLT.AddSymbol(TEllipse.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TEllipse;
      E.SymbolType := tstEllipse;
      E.LayerIndex := iLayerIndex;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(E);
          EatWhitespace;
          If CheckLiteral(',') Then
            Begin
              InteriorDef(E);
              EatWhiteSpace;
              If CheckLiteral(',') Then
                Begin
                  EllipseSize(E);
                  Transparency(E);
                  CheckLineEnd();
                End;
            End;
        End;
    End;
end;

(**

  This method parses the FieldDef element of the grammar.

  @precon  Container must be a valid instance of a TElementContainer class.
  @postcon Returns true if a field def was parsed.

  @param   Table       as a TBaseTable
  @param   iSheetIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.FieldDef(Table : TBaseTable;
  var iSheetIndex : Integer): Boolean;

Var
  strName : String;
  F : TFieldDef;
  T : TTokenInfo;
  boolPrimaryKey: Boolean;

begin
  EatWhitespace;
  Result := False;
  boolPrimaryKey := False;
  If (Token.Token = '*') Or (Token.TokenType In [ttIdentifier]) Then
    Begin
      Result := True;
      If Token.Token = '*' Then
        Begin
          boolPrimaryKey := True;
          NextNonCommentToken;
        End;
      T := Nil;
      While Token.Token <> ':' Do
        Begin
          If T = Nil Then
            T := Token;
          If Token.TokenType In [ttLineEnd] Then
            Break;
          strName := strName + Token.Token;
          NextNonCommentToken;
        End;
      If strName <> '' Then
        Begin
          F := Table.AddField(TFieldDef.Create(strName, scNone, T.Line, T.Column,
            iiPublicField, Nil));
          F.PrimaryKey := boolPrimaryKey;
          F.SheetIndex := iSheetIndex;
          Inc(iSheetIndex);
          If CheckLiteral(':') Then
            Begin
              TypeInfo(F);
              If Token.Token = '=' Then
                Begin
                  NextNonCommentToken;
                  strName := '';
                  While Not (Token.TokenType In [ttLineEnd]) Do
                    Begin
                      strName := strName + Token.Token;
                      NextNonCommentToken;
                    End;
                  F.OutputName := strName;
                End;
              CheckLineEnd();
            End;
        End Else
          ErrorAndSeekToken(strNullName, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TEidolonModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral, btFullComment,
    btLineComment);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;
  strSingleSymbols = [#9, #10, #13, #32, ';', '(', ')', '*', '+', ',', '='];
  (** A set of characters for single quotes **)
  strSingleQuotes = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers = ['a'..'z', 'A'..'Z', '_', '-', '%', #192..#214,
    #216..#246, #248..#255];
  (** A set of number characters. **)
  strNumbers  = ['.', '0'..'9'];
  strAllChars = [#32..#255];
  (** A set of characters for general symbols **)
  strSymbols = (strAllChars - strIdentifiers - strNumbers - strSingleQuotes -
    strDoubleQuotes);

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

  (**

    This INLINE procedure changes the whitepace tokens for more human readable
    tokens.

    @precon  strToken must be a non-null string.
    @postcon Changes the whitepace tokens for more human readable
             tokens.

    @param   strToken as a String as a reference

  **)
  Procedure ProcessWhiteSpace(var strToken : String); {$IFDEF D2005} InLine; {$ENDIF}

  Begin
    If strToken = #13 Then
      strToken := '<LF>';
    If strToken = #10 Then
      strToken := '<CR>';
  End;

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  CurCharType := ttUnknown;
  //: @debug LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For iChar := 1 To Length(FSource) Do
    Begin
      Ch := FSource[iChar];
      Inc(iStreamCount);
      LastCharType := CurCharType;

      If IsInSet(ch, strWhiteSpace) Then
        CurCharType := ttWhiteSpace
      Else If isInSet(Ch, strLineEnd) Then
        CurCharType := ttLineEnd
      Else If IsInSet(ch, strSingleQuotes) Then
        CurCharType := ttSingleLiteral
      Else If IsInSet(ch, strDoubleQuotes) Then
        CurCharType := ttDoubleLiteral
      Else If IsInSet(ch, strSymbols) Then
        CurCharType := ttSymbol
      Else If IsInSet(ch, strIdentifiers) Then
        Begin
          If (LastCharType = ttNumber) And (IsInSet(Ch, ['A'..'F', 'a'..'f'])) Then
            CurCharType := ttNumber
          Else
            CurCharType := ttIdentifier;
        End
      Else If IsInSet(ch, strNumbers) Then
        Begin
          CurCharType := ttNumber;
          If LastCharType = ttIdentifier Then
            CurCharType := ttIdentifier;
        End
      Else
        CurCharType := ttUnknown;

      // Check for full block comments
      If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '*') Then
        BlockType := btFullComment;

      // Check for line comments
      If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '/') Then
        BlockType := btLineComment;

      If (LastCharType <> CurCharType) Or (IsInSet(Ch, strSingleSymbols)) Or
        (IsInSet(LastChar, strSingleSymbols)) Then
        Begin
          If ((BlockType In [btLineComment]) And (CurCharType <> ttLineEnd)) Or
            (BlockType In [btFullComment]) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := Ch;
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  If BlockType = btLineComment Then
                    LastCharType := ttLineComment;
                  ProcessWhiteSpace(strToken);
                  If LastCharType = ttIdentifier Then
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
      // Check for Double string literals
      If CurCharType = ttDoubleLiteral Then
        If BlockType = btDoubleLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btDoubleLiteral;

      Inc(iColumn);
      If Ch = #10 Then
        Begin
          Inc(iLine);
          iColumn := 1;
        End;
      LastChar := Ch;
    End;
    If iTokenLen > 0 Then
      Begin
        SetLength(strToken, iTokenLen);
        ProcessWhiteSpace(strToken);
        If CurCharType = ttIdentifier Then
          If IsKeyWord(strToken, strReservedWords) Then
            CurCharType := ttReservedWord;
        AddToken(TTokenInfo.Create(strToken, iStreamPos,
          iTokenLine, iTokenColumn, Length(strToken), CurCharType));
      End;
  AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
    ttFileEnd));
End;

(**

  This method parses the Transparency element of the grammar.

  @precon  CustomSymbol must be a valid instance of a TCustomFillSymbol.
  @postcon Sets the Transparency of the symbol.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.Transparency(CustomSymbol : TCustomFillSymbol);

Var
  iTransparency : Integer;
  iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.Token = ',' Then
    Begin
      NextNonCommentToken;
      EatWhitespace;
      If Token.TokenType In [ttNumber] Then
        Begin
          Val(Token.Token, iTransparency, iErrorCode);
          If (iErrorCode > 0) Or (Not (iTransparency In [0..100]))  Then
            AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
              Token.Column]), scNone, Token.Line, Token.Column, etError);
          CustomSymbol.Transparency := iTransparency;
          NextNonCommentToken;
          EatWhitespace;
        End Else
          ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the TriangleType element of the grammar.

  @precon  Triangle must be a valid instance of a TTriangle class.
  @postcon Parses the TriangleType element of the grammar.

  @param   Triangle as a TTriangle

**)
procedure TEidolonModule.TriangleType(Triangle: TTriangle);

var
  iTriangle: TTriangleType;
  boolFound: Boolean;

begin
  EatWhitespace;
  Triangle.TriangleType := ttStartAndEarly;
  boolFound := False;
  For iTriangle := Low(TTriangleType) To High(TTriangleType) Do
    If CompareText(Token.Token, strTriangleTypes[iTriangle]) = 0 Then
      Begin
        Triangle.TriangleType := iTriangle;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidPattern, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the Triangle element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and TLT
           must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a TRIANGLE was parsed.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TLTriangle(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable; var iLayerIndex : Integer): Boolean;

Var
  T : TTriangle;

begin
  Result := False;
  If CompareText(Token.Token, 'TRIANGLE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      T := TLT.AddSymbol(TTriangle.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TTriangle;
      T.SymbolType := tstTriangle;
      T.LayerIndex := iLayerIndex;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(T);
          EatWhitespace;
          If CheckLiteral(',') Then
            Begin
              InteriorDef(T);
              EatWhiteSpace;
              If CheckLiteral(',') Then
                Begin
                  TriangleType(T);
                  Transparency(T);
                  CheckLineEnd();
                End;
            End;
        End;
    End;
end;

(**

  This method parses the TypeInfo element of the grammar.

  @precon  Field must be a valid instance of a TFieldDef class.
  @postcon Parses the TypeInfo element of the grammar.

  @param   Field as a TFieldDef

**)
procedure TEidolonModule.TypeInfo(Field: TFieldDef);
var
  iWidth: Integer;
  iErrorCode: Integer;
begin
  If Token.Token = 'C' Then
    Begin
      Field.FieldType := ftText;
      NextNonCommentToken;
      If CheckLiteral('(') Then
        If Token.TokenType In [ttNumber] Then
          Begin
            Val(Token.Token, iWidth, iErrorCode);
            If iErrorCode = 0 Then
              Begin
                Field.FieldWidth := iWidth;
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strInvalidDataWidth, Token.Token, strSeekableOnErrorTokens, stActual);
            CheckLiteral(')');
          End Else
            ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
    End Else
  If Token.Token = 'B' Then
    Begin
      Field.FieldType := ftBoolean;
      NextNonCommentToken;
    End Else
  If Token.Token = 'Y' Then
    Begin
      Field.FieldType := ftByte;
      NextNonCommentToken;
    End Else
  If Token.Token = 'I' Then
    Begin
      Field.FieldType := ftInteger;
      NextNonCommentToken;
    End Else
  If Token.Token = 'L' Then
    Begin
      Field.FieldType := ftLong;
      NextNonCommentToken;
    End Else
  If Token.Token = 'S' Then
    Begin
      Field.FieldType := ftSingle;
      NextNonCommentToken;
    End Else
  If Token.Token = 'F' Then
    Begin
      Field.FieldType := ftDouble;
      NextNonCommentToken;
    End Else
  If Token.Token = 'U' Then
    Begin
      Field.FieldType := ftCurrency;
      NextNonCommentToken;
    End Else
  If Token.Token = 'D' Then
    Begin
      Field.FieldType := ftDate;
      NextNonCommentToken;
    End Else
  If Token.Token = 'O' Then
    Begin
      Field.FieldType := ftLongBinary;
      NextNonCommentToken;
    End Else
  If Token.Token = 'M' Then
    Begin
      Field.FieldType := ftMemo;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strInvalidDataType, Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TEidolonModule.ParseTokens;
begin
  Goal;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TEidolonModule.ReservedWords: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
end;

(**

  This method parses the StartSize element of the grammar.

  @precon  Line must be a valid instance.
  @postcon Parses the StartSize element of the grammar.

  @param   Line as a TLine

**)
procedure TEidolonModule.StartSize(Line: TLine);
begin
  LineEndSize(Line, leStart);
end;

(**

  This method parses the StartType element of the grammar.

  @precon  Line must be a valid instance.
  @postcon Parses the StartType element of the grammar.

  @param   Line as a TLine

**)
procedure TEidolonModule.StartType(Line: TLine);
begin
  LineEndType(Line, leStart);
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TEidolonModule.Directives: TKeyWords;

begin
  Result := Nil;
end;

(**

  This method parses the OutputTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Creates and parses a OutputTable element.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   C          as a TComment
  @return  a Boolean

**)
function TEidolonModule.OutputTable(strName: String; StartToken: TTokenInfo;
  C : TComment): Boolean;

var
  OT: TOutputTable;
  iSheetIndex: Integer;

begin
  Result := False;
  If Token.UToken = 'OUTPUTTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          OT := FOutputTableDefs.AddUnique(TOutputTable.Create(strName, scNone,
            StartToken.Line, StartToken.Column, iiPublicInterface, C)) As
            TOutputTable;
          If CheckLiteral(')') Then
            If CheckLineEnd() Then
              Begin
                EatWhitespace;
                If CheckLiteral('{') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd() Then
                      Begin
                        EatWhitespace;
                        If DatabaseDef(OT, ctPrimary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(OT, ctPrimary);
                            EatWhitespace;
                            TableNameDef(OT, ctPrimary);
                          End;
                        EatWhitespace;
                        If DatabaseDef(OT, ctSecondary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(OT, ctSecondary);
                            EatWhitespace;
                            TableNameDef(OT, ctSecondary);
                          End;
                        iSheetIndex := 1;
                        While FieldDef(OT, iSheetIndex) Or EmptyLine Do;
                        If CheckLiteral('}') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd();
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the Line element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and TLT
           must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a LINE symbol was parsed.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TLLine(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable; var iLayerIndex : Integer): Boolean;

Var
  L : TLine;

begin
  Result := False;
  If CompareText(Token.Token, 'LINE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      L := TLT.AddSymbol(TLine.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TLine;
      L.SymbolType := tstLine;
      L.LayerIndex := iLayerIndex;
      L.LineStartType := atNone;
      L.LineStartSize := asMediumMedium;
      L.LineEndType := atNone;
      L.LineEndSize := asMediumMedium;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(L);
          EatWhitespace;
          If Not (Token.TokenType In [ttLineEnd]) Then
            LineEndDefs(L);
          CheckLineEnd();
        End;
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
Function TEidolonModule.GetComment(
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
              Result := TEidolonComment.CreateComment(T.Token, T.Line, T.Column);
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
function TEidolonModule.GetModuleName: String;
begin
  Result := ExtractFilename(FileName);
end;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @param   iSkip as an Integer as a reference

**)
Procedure TEidolonModule.ProcessCompilerDirective(var iSkip : Integer);

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method parses the LineEndDefs element of the grammar.

  @precon  Line must be a valid instance.
  @postcon Parses the LineEndDefs element of the grammar.

  @param   Line as a TLine

**)
procedure TEidolonModule.LineEndDefs(Line : TLine);

begin
  If CheckLiteral(',') Then
    Begin
      StartType(Line);
      If CheckLiteral(',') Then
        Begin
          StartSize(Line);
          If CheckLiteral(',') Then
            Begin
              EndType(Line);
              If CheckLiteral(',') Then
                EndSize(Line);
            End;
        End;
    End;
end;

(**

  This method parses the LineEndSize element of the grammar.

  @precon  None.
  @postcon Parses the LineEndSize element of the grammar.

  @param   Line    as a TLine
  @param   LineEnd as a TLineEnd

**)
procedure TEidolonModule.LineEndSize(Line: TLine; LineEnd : TLineEnd);

Var
  boolFound : Boolean;
  i: TLineEndSize;

begin
  boolFound := False;
  EatWhitespace;
  For i := Low(TLineEndSize) To High(TLineEndSize) Do
    If CompareText(strLineEndSizes[i], Token.Token) = 0 Then
      Begin
        boolFound := True;
        Case LineEnd Of
          leStart: Line.LineStartSize := i;
          leEnd:   Line.LineEndSize := i;
        End;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineSize, Token.Token, strSeekableOnErrorTokens, stActual);
  EatWhitespace;
end;

(**

  This method parses the LineEndType element of the grammar.

  @precon  None.
  @postcon Parses the LineEndType element of the grammar.

  @param   Line    as a TLine
  @param   LineEnd as a TLineEnd

**)
procedure TEidolonModule.LineEndType(Line: TLine; LineEnd : TLineEnd);

Var
  boolFound : Boolean;
  i: TLineEndType;

begin
  boolFound := False;
  EatWhitespace;
  For i := Low(TLineEndType) To High(TLineEndType) Do
    If CompareText(strLineEndTypes[i], Token.Token) = 0 Then
      Begin
        boolFound := True;
        Case LineEnd Of
          leStart: Line.LineStartType := i;
          leEnd:   Line.LineEndType := i;
        End;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineType, Token.Token, strSeekableOnErrorTokens, stActual);
  EatWhitespace;
end;

(**

  This method parses the LocationWidth element of the grammar.

  @precon  SuperBar must be a valid instance.
  @postcon If the width is valid, it is assigned to the super bar else an exception is
           raised.

  @param   SuperBar as a TSuperBar

**)
Procedure TEidolonModule.LocationWidth(SuperBar: TSuperBar);

Var
  dblWidth : Double;
  iErrorCode : Integer;

Begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, dblWidth, iErrorCode);
      If (iErrorCode > 0) Or (dblWidth <= 0.0) Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, Token.Line, Token.Column, etError);
      SuperBar.LocationWidth := dblWidth;
      NextNonCommentToken;
    End
  Else
    ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
End;

(**

  This method does nothing as we are not referencing symbols in XML.

  @precon  None.
  @postcon Returns false always.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TEidolonModule.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
End;

(**

  This method parses the RequirementsTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Creates and parses a RequirementsTable element.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   C          as a TComment
  @return  a Boolean

**)
function TEidolonModule.RequirementsTable(strName: String; StartToken: TTokenInfo;
  C : TComment): Boolean;

Var
  RT : TRequirementsTable;
  iSheetIndex: Integer;

begin
  Result := False;
  If Token.UToken = 'REQUIREMENTSTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          RT := FRequirementsTableDefs.AddUnique(TRequirementsTable.Create(strName, scNone,
            StartToken.Line, StartToken.Column, iiPublicDispInterface, C)) As
            TRequirementsTable;
          If CheckLiteral(')') Then
            If CheckLineEnd() Then
              Begin
                EatWhitespace;
                If CheckLiteral('{') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd() Then
                      Begin
                        EatWhitespace;
                        If DatabaseDef(RT, ctPrimary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(RT, ctPrimary);
                            EatWhitespace;
                            TableNameDef(RT, ctPrimary);
                          End;
                        iSheetIndex := 1;
                        While FieldDef(RT, iSheetIndex) Or EmptyLine Do;
                        While AssociationDef(RT) Do;
                        EatWhitespace;
                        If DatabaseDef(RT, ctSecondary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(RT, ctSecondary);
                            EatWhitespace;
                            TableNameDef(RT, ctSecondary);
                          End;
                        iSheetIndex := 1;
                        While FieldDef(RT, iSheetIndex) Or EmptyLine Do;
                        While AssociationDef(RT) Do;
                        If CheckLiteral('}') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd();
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the common start elements of the xxxxTable definitions
  and passes the found name to the actual definitions.

  @precon  None.
  @postcon Parses the common start elements of the xxxxTable definitions
           and passes the found name to the actual definitions.

  @return  a Boolean

**)
function TEidolonModule.Table: Boolean;

var
  strName: String;
  StartToken : TTokenInfo;
  C : TComment;

begin
  C := GetComment;
  EatWhitespace;
  Result := False;
  strName := '';
  StartToken := Nil;
  While Token.Token <> '=' Do
    Begin
      If StartToken = Nil Then
        StartToken := Token;
      strName := strName + Token.Token;
      NextNonCommentToken;
    End;
  If Token.Token = '=' Then
    Begin
      NextNonCommentToken;
      If Token.UToken = 'CLASS' Then
        Begin
          NextNonCommentToken;
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              Result :=
                TextTable(strName, StartToken, C) Or
                DBTable(strName, StartToken, C) Or
                TimeLocationTable(strName, StartToken, C) Or
                OutputTable(strName, StartToken, C) Or
                RequirementsTable(strName, StartToken, C);
              If Not Result Then
                ErrorAndSeekToken(strInvalidTableType, Token.Token, strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '(', strSeekableOnErrorTokens, stActual);

        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'CLASS', strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the TableNameDef element of the grammar.

  @precon  Table must be a valid instance of a TBaseTable class.
  @postcon Parses the TableNameDef element of the grammar.

  @param   DBTable        as a TDBTable
  @param   ConnectionType as a TConnectionType

**)
procedure TEidolonModule.TableNameDef(DBTable : TDBTable; ConnectionType : TConnectionType);

Var
  T : TTableNameDef;

begin
  If Token.Token = strConnectionType[ConnectionType] Then
    Begin
      NextNonCommentToken;
      If CompareText(Token.Token, strTableNameLabel) = 0 Then
        Begin
          Case ConnectionType Of
            ctPrimary:
              T := DBTable.AddPrimary(TTableNameDef.Create('TableName', scNone,
                Token.Line, Token.Column, iiPublicType, Nil)) As TTableNameDef;
          Else
            T := DBTable.AddSecondary(TTableNameDef.Create('TableName', scNone,
              Token.Line, Token.Column, iiPublicType, Nil)) As TTableNameDef;
          End;
          NextNonCommentToken;
          If CheckLiteral('=') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(T);
              CheckLineEnd()
            End;
        End;
    End;
end;

(**

  This method parses the TextTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Parses the TextTable element of the grammar

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   C          as a TComment
  @return  a Boolean

**)
Function TEidolonModule.TextTable(strName : String; StartToken : TTokenInfo;
  C : TComment) : Boolean;

Var
  TT : TTextTable;
  iSheetIndex: Integer;

begin
  Result := False;
  If Token.UToken = 'TEXTTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          TT := FTextTableDefs.AddUnique(TTextTable.Create(strName, scNone,
            StartToken.Line, StartToken.Column, iiPublicThreadVar, C)) As TTextTable;
          If CheckLiteral(')') Then
            If CheckLineEnd() Then
              Begin
                EatWhitespace;
                If CheckLiteral('{') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd() Then
                      Begin
                        While EmptyLine Do;
                        EatWhiteSpace;
                        TextTableDef(TT);
                        iSheetIndex := 1;
                        While FieldDef(TT, iSheetIndex) Or EmptyLine Do;
                        If CheckLiteral('}') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd();
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the TextTableDef element of the grammar.

  @precon  TextTable must be a valid instance of a TTextTable class.
  @postcon Parses the TextTableDef element of the grammar.

  @param   TextTable as a TTextTable

**)
procedure TEidolonModule.TextTableDef(TextTable : TTextTable);

Var
  TTD: TTextTableDef;

begin
  If Token.Token = '#' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'TABLENAME') = 0 Then
        Begin
          TTD := TextTable.AddUnique(TTextTableDef.Create('TableName', scNone, Token.Line,
            Token.Column, iiPublicRecord, Nil)) As TTextTableDef;
          NextNonCommentToken;
          If CheckLiteral('=') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(TTD);
              TextTable.FileName := TTD.AsString(False, False);
              CheckLineEnd();
            End;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method deletes any root elements which dont and items in them.

  @precon  None.
  @postcon Deletes any root elements which dont and items in them.

**)
procedure TEidolonModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Elements[iElement] Is TLabelContainer Then
        If Pos('Definitions', Elements[iElement].Identifier)  = 0 Then
          DeleteElement(iElement);
end;

(**

  This method parses the TimeLocationDef elements of the grammar.

  @precon  Table must be a valid instance of a TTimeLocationTable class.
  @postcon Parses the TimeLocationDef elements of the grammar.

  @param   Table       as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TimeLocationDef(Table : TTimeLocationTable;
  var iLayerIndex : Integer): Boolean;

Var
  strName : String;
  StartToken : TTokenInfo;

begin
  Result := False;
  StartToken := Nil;
  EatWhitespace;
  If Token.Token = '&' Then
    Begin
      Result := True;
      NextNonCommentToken;
      While Token.Token <> '=' Do
        Begin
          If StartToken = Nil Then
            StartToken := Token;
          If Token.TokenType In [ttLineEnd] Then
            Break;
          strName := strName + Token.Token;
          NextNonCommentToken;
        End;
      If strName <> '' then
        Begin
          If CheckLiteral('=') Then
            Begin
              EatWhitespace;
              If TLRectangle(strName, StartToken, Table, iLayerIndex) Or
                TLBar(strName, StartToken, Table, iLayerIndex) Or
                TLLine(strName, StartToken, Table, iLayerIndex) Or
                TLTriangle(strName, StartToken, Table, iLayerIndex) Or
                TLEllipse(strName, StartToken, Table, iLayerIndex) Or
                TLDiamond(strName, StartToken, Table, iLayerIndex) Or
                TLSuperBar(strName, StartToken, Table, iLayerIndex) Then
                Inc(iLayerIndex);
            End;
        End Else
          ErrorAndSeekToken(strNullName, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the TimeLocationTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Parses the TimeLocationTable element of the grammar.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   C          as a TComment
  @return  a Boolean

**)
Function TEidolonModule.TimeLocationTable(strName : String; StartToken : TTokenInfo;
  C : TComment) : Boolean;

var
  TLT: TTimeLocationTable;
  iSheetIndex: Integer;
  iLayerIndex : Integer;

begin
  Result := False;
  If Token.UToken = 'TIMELOCATIONTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          TLT := FTimeLocationTableDefs.AddUnique(TTimeLocationTable.Create(strName,
            scNone, StartToken.Line, StartToken.Column, iiPublicVariable,
            C)) As TTimeLocationTable;
          If CheckLiteral(')') Then
            If CheckLineEnd() Then
              Begin
                EatWhitespace;
                If CheckLiteral('{') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd() Then
                      Begin
                        iSheetIndex := 1;
                        While FieldDef(TLT, iSheetIndex) Or EmptyLine Do;
                        iLayerIndex := 1;
                        While TimeLocationDef(TLT, iLayerIndex) Or EmptyLine Do;
                        If CheckLiteral('}') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd();
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the Rectangle element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and TLT
           must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true is a RECTANGLE was parsed.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
function TEidolonModule.TLRectangle(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable; var iLayerIndex : Integer): Boolean;

Var
  R : TRectangle;

begin
  Result := False;
  If CompareText(Token.Token, 'RECTANGLE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      R := TLT.AddSymbol(TRectangle.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TRectangle;
      R.SymbolType := tstRectangle;
      R.LayerIndex := iLayerIndex;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(R);
          EatWhitespace;
          If CheckLiteral(',') Then
            Begin
              InteriorDef(R);
              Transparency(R);
              CheckLineEnd();
            End;
        End;
    End;
end;

(**

  This method parses the SuperBar element of the time location grammar.

  @precon  TLT must be a valid instance.
  @postcon Parses and creates a super bar element if found in the AMP file.

  @param   strName     as a String
  @param   StartToken  as a TTokenInfo
  @param   TLT         as a TTimeLocationTable
  @param   iLayerIndex as an Integer as a reference
  @return  a Boolean

**)
Function TEidolonModule.TLSuperBar(strName: String; StartToken: TTokenInfo;
  TLT: TTimeLocationTable; var iLayerIndex: Integer): Boolean;

Var
  B: TSuperBar;

Begin
  Result := False;
  If CompareText(Token.Token, 'SUPERBAR') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      B := TLT.AddSymbol(TSuperBar.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TSuperBar;
      B.SymbolType := tstSuperBar;
      B.LayerIndex := iLayerIndex;
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          BorderDef(B);
          EatWhitespace;
          If CheckLiteral(',') Then
            Begin
              InteriorDef(B);
              EatWhiteSpace;
              If CheckLiteral(',') Then
                Begin
                  DateWidth(B);
                  If CheckLiteral(',') Then
                    Begin
                      LocationWidth(B);
                      Transparency(B);
                      CheckLineEnd();
                    End;
                End;
            End;
        End;
    End;
End;

(**

  This method is the starting position for the parsing of an Eidolon module. It
  finds the first non comment token and begins the grammar checking from their
  by deligating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating Syntax.

**)
procedure TEidolonModule.Goal;

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
                C := TEidolonComment.CreateComment(Token.Token, Token.Line,
                  Token.Column);
                AddBodyComment(C);
                If Comment = Nil Then
                  Comment := C;
              End;
            NextToken;
          End;
        // Check for end of file else must be identifier
        While Not EndOfTokens And (EmptyLine Or Table) Do;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError);
  End;
end;

(**

  This method parses the InteriorColour element of the grammar.

  @precon  Rectangle must be a valid instance of TRectangle class.
  @postcon Assigns the interior colour to the rectangle.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorColour(CustomSymbol : TCustomFillSymbol);
begin
  CustomSymbol.InteriorColour := ColourName;
end;

(**

  This method parses the InteriorDef element of the grammar.

  @precon  Rectangle must be a valid instance of a TRectangle.
  @postcon Parses the InteriorDef element of the grammar.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorDef(CustomSymbol: TCustomFillSymbol);
begin
  EatWhitespace;
  InteriorColour(CustomSymbol);
  EatWhitespace;
  If CheckLiteral(',') Then
    Begin
      EatWhitespace;
      InteriorPattern(CustomSymbol);
      EatWhitespace;
      If CheckLiteral(',') Then
        Begin
          EatWhitespace;
          InteriorPatternColour(CustomSymbol);
        End;
    End;
end;

(**

  This method parses the InteriorPattern element of the grammar.

  @precon  Rectangle
  @postcon Sets the InteriorPattern of the passed symbol.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorPattern(CustomSymbol : TCustomFillSymbol);

var
  boolFound: Boolean;
  iPattern: TInteriorPattern;
  strPattern : String;

begin
  CustomSymbol.InteriorPattern := ipNONE;
  boolFound := False;
  strPattern := '';
  While Token.TokenType In [ttNumber, ttIdentifier] Do
    Begin
      strPattern := strPattern + Token.Token;
      NextNonCommentToken;
    End;
  If strPattern = '' Then
    strPattern := Token.Token;
  For iPattern := Low(TInteriorPattern) To High(TInteriorPattern) Do
    If CompareText(strPattern, strInteriorPatterns[iPattern]) = 0 Then
      Begin
        CustomSymbol.InteriorPattern := iPattern;
        boolFound := True;
        Break;
      End;
  If Not boolFound Then
    ErrorAndSeekToken(strInvalidPattern, strPattern, strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the InteriorPatternColour element of the grammar.

  @precon  Rectangle must be a valid instance of TRectangle class.
  @postcon Assigns the interior pattern colour to the rectangle.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorPatternColour(CustomSymbol : TCustomFillSymbol);
begin
  CustomSymbol.InteriorPatternColour := ColourName;
  If (CustomSymbol.InteriorPattern = ipNONE) And
    (CustomSymbol.InteriorPatternColour <> xlcNONE) Then
    AddIssue(Format(strYouHaveSpecifiedAColour,
      [strColours[CustomSymbol.InteriorPatternColour]]), scNone, CustomSymbol.Line, CustomSymbol.Column,
        etWarning);
  If (CustomSymbol.InteriorPattern <> ipNONE) And
    (CustomSymbol.InteriorPatternColour = xlcNONE) Then
    AddIssue(Format(strYouHaveSpecifiedAPattern,
      [strInteriorPatterns[CustomSymbol.InteriorPattern]]), scNone, CustomSymbol.Line,
      CustomSymbol.Column, etWarning);
end;

End.
