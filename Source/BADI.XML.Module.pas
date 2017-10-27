(**

  XMLModule : A unit to parser XML. Please refer to the file
  "Extensible Markup Language (XML) 1.0.bnf" for the complete grammar
  implemented.

  @Version    1.0
  @Date       27 Oct 2017
  @Author     David Hoyle

**)
Unit BADI.XML.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.Types,
  BADI.TokenInfo,
  BADI.XML.XMLElement,
  BADI.XML.XMLPI;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** An enumerate to describe whether the module is XHTML or XML. **)
  TModuleType = (mtXHTML, mtXML);

  (** This is the main class for dealing with backus-naur grammar files. **)
  TXMLModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource : String;
    FModuleType : TModuleType;
    { Grammar Parsers }
    Procedure Goal;                                                    //  1
    Procedure Document;                                                //  2
    Procedure Prolog(xmlParent : TElementContainer; var iElements : Integer);                   //  3
    Function  Element(xmlParent : TElementContainer) : Boolean;        //  4
    Function  Misc(xmlParent : TElementContainer; var iElements : Integer) : Boolean; //  5
    Procedure XMLDecl(xmlParent : TElementContainer);                  //  6
    Function  DocTypeDecl(xmlParent : TElementContainer) : Boolean;    //  7
    Function  STag(xmlParent : TElementContainer) : TXMLElement;       //  8
    Function  XMLName : String;                                        //  9
    Procedure Content(xmlParent : TXMLElement);                        // 10
    Procedure ETag(xmlParent : TElementContainer);                     // 11
    Function  XMLComment : Boolean;                                    // 12
    Function  XMLPI(xmlParent : TElementContainer) : Boolean;          // 13
    Function  Whitespace(E : TXMLElement) : Boolean;                   // 14
    Function  VersionInfo(xmlParent : TElementContainer) : Boolean;    // 15
    Procedure EncodingDecl(xmlParent : TElementContainer);             // 16
    Procedure SDDecl(xmlParent : TElementContainer);                   // 17
    Function  ExternalID(xmlParent : TElementContainer) : Boolean;     // 18
    Procedure IntSubSet(xmlParent : TElementContainer);                // 19
    Function  Attribute(xmlElement : TXMLElement) : Boolean;           // 20
    Procedure Eq(xmlParent : TElementContainer);                       // 21
    Procedure VersionNum(xmlParent : TElementContainer);               // 22
    Procedure EncName(xmlParent : TElementContainer);                  // 23
    Function  PITarget(xmlParent : TElementContainer) : TXMLPI;        // 24
    Function  EntityValue(xmlParent : TElementContainer) : Boolean;    // 25
    Procedure NDataDecl(xmlParent : TElementContainer);                // 26
    Procedure SystemLiteral(xmlParent : TElementContainer);            // 27
    Procedure PubIDLiteral(xmlParent : TElementContainer);             // 28
    Function  MarkupDecl(xmlParent : TElementContainer) : Boolean;     // 29
    Function  DeclSep(xmlParent : TElementContainer) : Boolean;        // 30
    Function  ElementDecl(xmlParent : TElementContainer) : Boolean;    // 31
    Function  AttListDecl(xmlParent : TElementContainer) : Boolean;    // 32
    Function  EntityDecl(xmlParent : TElementContainer) : Boolean;     // 33
    Function  NotationDecl(xmlParent : TElementContainer) : Boolean;   // 34
    Procedure ContentSpec(xmlParent : TElementContainer);              // 35
    Function  Mixed(xmlParent : TElementContainer) : Boolean;          // 36
    Function  Children(xmlParent : TElementContainer) : Boolean;       // 37
    Function  ChoiceSeq(xmlParent : TElementContainer) : Boolean;      // 38
    Function  CharData(E : TXMLElement) : Boolean;               // 40
    Function  Reference(xmlParent: TElementContainer) : Boolean;       // 41
    Function  CDSect : Boolean;                                        // 42
    Procedure AttValue(xmlParent : TElementContainer);                 // 43
    Function  PEReference(xmlParent : TElementContainer) : Boolean;    // 44
    Function  AttDef(xmlParent : TElementContainer) : Boolean;         // 45
    Function  GEDecl(xmlParent : TElementContainer) : Boolean;         // 46
    Function  PEDecl(xmlParent : TElementContainer) : Boolean;         // 47
    Procedure PublicID(xmlParent : TElementContainer);                 // 48
    Function  EntityRef(xmlParent : TElementContainer) : Boolean;      // 50
    Function  CharRef(xmlParent : TElementContainer) : Boolean;        // 51
    Function  CDStart : Boolean;                                       // 52
    Procedure CDData;                                                  // 53
    Procedure CDEnd;                                                   // 54
    Procedure AttType(xmlParent : TElementContainer);                  // 55
    Procedure DefaultDecl(xmlParent : TElementContainer);              // 56
    Procedure EntityDef(xmlParent : TElementContainer);                // 57
    Function  StringType(xmlParent : TElementContainer) : Boolean;     // 58
    Function  TokenizedType(xmlParent : TElementContainer) : Boolean;  // 59
    Procedure EnumerateType(xmlParent : TElementContainer);            // 60
    Procedure Enumeration(xmlParent : TElementContainer);              // 61
    Function  NmToken(xmlParent : TElementContainer) : Boolean;        // 62
    Function  NameChar(xmlParent : TElementContainer) : Boolean;       // 63
    Function  CombiningChar : Boolean;                                 // 64
    Function  Extender : Boolean;                                      // 65
    Function  NotationType(xmlParent : TElementContainer) : Boolean;   // 66
    Function  ExtSubSet(xmlParent : TElementContainer) : Boolean;
    Function  TextDecl(xmlParent : TElementContainer) : Boolean;
    Function  ExtSubSetDecl(xmlParent : TElementContainer) : Boolean;
    Function  ConditionalSect(xmlParent : TElementContainer) : Boolean;
    Function  IncludeSect(xmlParent : TElementContainer) : Boolean;
    Function  IgnoreSect(xmlParent : TElementContainer) : Boolean;
    Procedure IgnoreSectContents;
    Procedure Ignore;
    // ----------------------------------------------------------------
    Function  EatCharData(E : TXMLElement) : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    procedure TidyUpEmptyElements;
    Function GetModuleName : String; Override;
  Public
    Constructor CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
      Const ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(Var boolCascade : Boolean); Override;
  End;

Implementation

Uses
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.XML.ResourceStrings,
  BADI.XML.XMLElemDecl,
  BADI.XML.Comment,
  BADI.XML.DocType,
  BADI.XML.XMLDecl,
  BADI.XML.XMLPERef,
  BADI.XML.XMLIgnoreElement,
  BADI.XML.XMLIncludeElement;

Const

  (** A set of reserved words (not used in this parser.) **)
  strReservedWords : Array[1..27] Of String = (
    'any', 'attlist', 'cdata', 'doctype', 'element', 'empty', 'encoding',
    'entities', 'entity', 'fixed', 'id', 'idref', 'idrefs', 'ignored', 'implied',
    'ndata', 'nmtoken', 'nmtokens', 'include', 'notation', 'pcdata', 'public',
    'required', 'standalone', 'system', 'version', 'xml'
  );


  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..1] Of String = ('<line-end>');

(**

  This method parses the AttDef element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the AttDef element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.AttDef(xmlParent: TElementContainer) : Boolean;

var
  strName: String;
  T : TTokenInfo;

begin
  Result := False;
  If Whitespace(Nil) Then
    Begin
      Result := True;
      T := Token;
      strName := XMLName;
      If strName = '' Then
        AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
          scNone, T.Line, T.Column, etError, Self)
      Else
        xmlParent.AddToken(strName);
      Whitespace(Nil);
      AttType(xmlParent);
      Whitespace(Nil);
      DefaultDecl(xmlParent);
    End;
end;

(**

  This method parses the AttListDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the AttListDecl element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.AttListDecl(xmlParent : TElementContainer): Boolean;
var
  strName: String;
  A: TXMLElemDecl;

begin
  Result := False;
  If Token.Token = '<!' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'ATTLIST') = 0 Then
        Begin
          Result := True;
          A := xmlParent.Add(TXMLElemDecl.Create('!' + Token.Token, scNone,
            Token.Line, Token.Column, iiPublicConstant, Nil)) As TXMLElemDecl;
          NextNonCommentToken;
          WhiteSpace(Nil);
          strName := XMLName;
          A.AddToken(strName);
          While AttDef(A) Do;
          Whitespace(Nil);
          If Token.Token = '>' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the Attribute element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Attribute element of the grammar.

  @param   xmlElement as a TXMLElement
  @return  a Boolean

**)
function TXMLModule.Attribute(xmlElement : TXMLElement): Boolean;

Var
  strName : String;

begin
  Result := False;
  strName := XMLName;
  If strName <> '' Then
    Begin
      If xmlElement.Attribute.IndexOf(strName) = -1 Then
        xmlElement.Attribute.Add(strName)
      Else
        ErrorAndSeekToken(strAttributeCanNotAppear, strName, strSeekableOnErrorTokens, stActual, Self);
      xmlElement.AddToken(strName);
      Result := True;
      Eq(xmlElement);
      AttValue(xmlElement);
    End;
end;

(**

  This method parses the AttType element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the AttType element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.AttType(xmlParent: TElementContainer);
begin
  If Not StringType(xmlParent) Then
    If Not TokenizedType(xmlParent) Then
      EnumerateType(xmlParent);
end;

(**

  This method parses the AttVakue element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the AttVakue element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.AttValue(xmlParent: TElementContainer);
begin
  If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
    AddToExpression(xmlParent)
  Else
    ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the CDData element of the grammar.

  @precon  None.
  @postcon Parses the CDData element of the grammar.

**)
procedure TXMLModule.CDData;
begin
  While Token.Token <> ']]' Do
    NextNonCommentToken;
end;

(**

  This method parses the CDEnd element of the grammar.

  @precon  None.
  @postcon Parses the CDEnd element of the grammar.

**)
procedure TXMLModule.CDEnd;
begin
  If Token.Token = ']]' Then
    Begin
      NextNonCommentToken;
      If Token.Token = '>' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, ']]>', strSeekableOnErrorTokens, stActual, Self);
    End Else
      ErrorAndSeekToken(strLiteralExpected, ']]>', strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the CDSect element of the grammar.

  @precon  None.
  @postcon Returns true if a CDSect element was parsed.

  @return  a Boolean

**)
function TXMLModule.CDSect: Boolean;
begin
  Result := False;
  If CDStart Then
    Begin
      Result := True;
      CDData;
      CDEnd;
    End;
end;

(**

  This method parses the CDStart element of the grammar.

  @precon  None.
  @postcon Returns true if the element was a CDStart element.

  @return  a Boolean

**)
function TXMLModule.CDStart: Boolean;
begin
  Result := False;
  If Token.Token = '<![' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If CompareText(Token.Token, 'CDATA') = 0 Then
        Begin
          NextNonCommentToken;
          If Token.Token = '[' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, '[', strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strExpectedWord, 'CDATA', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method eats up white space and token which do not start with an "<".

  @precon  None.
  @postcon Eats up white space and token which do not start with an "<".

  @param   E as a TXMLElement
  @return  a Boolean

**)
function TXMLModule.CharData(E : TXMLElement): Boolean;

begin
  Result := Whitespace(E) Or EatCharData(E);
end;

(**

  This method parses the CharRef element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the CharRef element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.CharRef(xmlParent: TElementContainer) : Boolean;
begin
  Result := False;
  If IsKeyWord(Token.Token, ['&#', '&#x']) Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      If Token.TokenType In [ttNumber] Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      If Token.Token = ';' Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method check the module for DOCTYPE or ?XML elements.

  @precon  None.
  @postcon Adds and documentation conflict IF either of these elements
           is not found.

  @param   boolCascade as a Boolean as a reference

**)
procedure TXMLModule.CheckDocumentation(var boolCascade: Boolean);

  (**

    This method searches for an element starting with the name passed.

    @precon  None.
    @postcon Returns true if the name was matched.

    @param   strName as a String
    @return  a Boolean

  **)
  Function FindMatch(strName : String) : Boolean;

  var
    i: Integer;

  Begin
    Result := False;
    For i := 1 To ElementCount Do
      If Like(strName, Elements[i].Name) Then
        Begin
          Result := True;
          Break;
        End;
  End;

Var
  recDocCon : TDocConflictTable;

begin
  recDocCon.FMessage := 'Module ''%s'' is missing a ''!DOCTYPE'' element.';
  recDocCon.FDescription := 'XHTML files should have a !DOCTYPE element as ' +
    'the first element.';
  recDocCon.FConflictType := dciMissing;
  If FModuleType = mtXHTML Then
    If Not FindMatch('!DOCTYPE*') Then
      AddDocumentConflict([ModuleName], 1, 1, Nil, strModuleDocumentation, recDocCon);
  recDocCon.FMessage := 'Module ''%s'' is missing a ''?xml'' element.';
  recDocCon.FDescription := 'XML files should have a ?xml element as ' +
    'the first element.';
  recDocCon.FConflictType := dciMissing;
  If FModuleType = mtXML Then
    If Not FindMatch('?xml*')  Then
      AddDocumentConflict([ModuleName], 1, 1, Nil, strModuleDocumentation, recDocCon);
end;

(**

  This method parses the Children element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Children element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.Children(xmlParent : TElementContainer): Boolean;

begin
  Result := ChoiceSeq(xmlParent);
end;

(**

  This method parses the Choice element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Choice element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.ChoiceSeq(xmlParent: TElementContainer): Boolean;

var
  strName: String;
  strDelimiter: String;

  (**

    This procedure processes either another Choice or Sequence or Name then
    an optional whitespace.

    @precon  None.
    @postcon Processes either another Choice or Sequence or Name then
             an optional whitespace.

  **)
  Procedure ProcessNames;

  var
    T: TTokenInfo;

  Begin
    If Token.Token = '(' Then
      ChoiceSeq(xmlParent)
    Else
      Begin
        T := Token;
        strName := XMLName;
        If strName = '' Then
          AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
            scNone, T.Line, T.Column, etError, Self)
        Else
          xmlParent.AddToken(strName);
        If IsKeyWord(Token.Token, ['*', '+', '?']) Then
          AddToExpression(xmlParent);
      End;
    Whitespace(Nil);
  End;

begin
  Result := False;
  If Token.Token = '(' Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      Whitespace(Nil);
      ProcessNames;
      If IsKeyWord(Token.Token, [',', '|']) Then
        Begin
          strDelimiter := Token.Token;
          AddToExpression(xmlParent);
          Repeat
            Whitespace(Nil);
            ProcessNames;
          Until Not IsToken(strDelimiter, xmlParent);
        End;
      If Token.Token = ')' Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
      If IsKeyWord(Token.Token, ['*', '+', '?']) Then
        AddToExpression(xmlParent);
    End;
end;

(**

  This method parses the CombiningChar element of the grammar.

  @precon  None.
  @postcon Always returns false in this implementation.

  @return  a Boolean

**)
function TXMLModule.CombiningChar: Boolean;
begin
  Result := False;
end;

(**

  This method parses the ConditionalSect element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true of a IncludeSect or IgnoreSect was found and parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.ConditionalSect(xmlParent: TElementContainer): Boolean;
begin
  Result := IncludeSect(xmlParent) or IgnoreSect(xmlParent);
end;

(**

  This method parses the Content element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Content element of the grammar.

  @param   xmlParent as a TXMLElement

**)
procedure TXMLModule.Content(xmlParent : TXMLElement);
begin
  Repeat
    // Do nothing
  Until Not (
    CharData(xmlParent) Or
    Element(xmlParent) Or
    Reference(xmlParent) Or
    CDSect Or
    XMLPI(xmlParent) Or
    XMLComment);
end;

(**

  This method parses the ContentSpec element of the grammar.

  @precon  xmlParent must be a valid instance of a container
  @postcon Parses the ContentSpec element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.ContentSpec(xmlParent : TElementContainer);

begin
  If CompareText(Token.Token, 'EMPTY') = 0 Then
    AddToExpression(xmlParent)
  Else If CompareText(Token.Token, 'ANY') = 0 Then
    AddToExpression(xmlParent)
  Else If Not Mixed(xmlParent) Then
    If Not Children(xmlParent) Then
      ErrorAndSeekToken(strInvalidContentSpec, '', strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This is the constructor method for the TXMLModule class.

  @precon  Source is a valid TStream descendant containing as stream of text, that is the contents of a 
           source code module and Filename is the file name of the module being parsed and IsModified 
           determines if the source code module has been modified since the last save to disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
Constructor TXMLModule.CreateParser(const Source, strFileName : String; Const IsModified : Boolean;
  Const ModuleOptions : TModuleOptions);

Var
  boolCascade : Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  Sorted := False;
  If IsKeyWord(ExtractFileExt(strFileName), ['.htm', '.html']) Then
    FModuletype := mtXHTML
  Else
    FModuleType := mtXML;
  CompilerDefines.Assign(BADIOptions.Defines);
  FSource := Source;
  AddTickCount('Start');
  CommentClass := TXMLComment;
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

  This method parses the DeclSep element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the DeclSep element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.DeclSep(xmlParent : TElementContainer) : Boolean;
begin
  Result := PEReference(xmlParent);
  If Not Result Then
    Whitespace(Nil);
end;

(**

  This method parses the DefaultDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the DefaultDecl element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.DefaultDecl(xmlParent: TElementContainer);
begin
  If CompareText(Token.Token, '#REQUIRED')= 0  Then
    AddToExpression(xmlParent)
  Else If CompareText(Token.Token, '#IMPLIED')= 0  Then
    AddToExpression(xmlParent)
  Else
    Begin
      If CompareText(Token.Token, '#FIXED')= 0 Then
        Begin
          AddToExpression(xmlParent);
          If Not Whitespace(Nil) Then
          ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strReservedWords, stActual, Self);
        End;
      AttValue(xmlParent);
    End;
end;

(**


  This is a destructor for the TXMLModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TXMLModule.Destroy;
begin
  Inherited Destroy;
end;

(**

  This method parses the doc type information at the top of the XML file as per
  the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the doc type information at the top of the XML file as per
           the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.DocTypeDecl(xmlParent : TElementContainer): Boolean;

Var
  D : TXMLDocType;

begin
  Result := False;
  If Token.Token = '<!' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'DOCTYPE') = 0 Then
        Begin
          Result := True;
          NextNonCommentToken;
          Whitespace(Nil);
          D := xmlParent.Add(TXMLDocType.Create('!DOCTYPE', scNone,
            Token.Line, Token.Column, iiPublicType, Nil)) As TXMLDocType;
          D.AddToken(XMLName);
          If Whitespace(Nil) Then
            ExternalID(D);
          WhiteSpace(Nil);
          If Token.Token = '[' Then
            Begin
              AddToExpression(D);
              IntSubSet(D);
              If Token.Token =  ']' Then
                Begin
                  AddToExpression(D);
                  WhiteSpace(Nil);
                End Else
                  ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
            End;
          If Token.Token = '>' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
      End;
end;

(**

  This method starts the parsing of the xml document as defined in the grammar.

  @precon  None.
  @postcon Starts the parsing of the xml document as defined in the grammar.

**)
procedure TXMLModule.Document;

var
  boolFound: Boolean;
  iElements : Integer;

begin
  iElements := 0;
  Prolog(Self, iElements);
  Repeat
    boolFound := Element(Self);
    If boolFound Then
      Begin
        Inc(iElements);
        While Misc(Self, iElements) Do;
      End;
  Until Not boolFound;
  If iElements = 0 Then
    ErrorAndSeekToken(strExpectedElement, Token.Token, strSeekableOnErrorTokens, stActual, Self);
  If Not (Token.TokenType In [ttFileEnd]) Then
    AddIssue(Format(strExpectedFileEnd, [Token.Token, Token.Line, Token.Column]),
      scNone, Token.Line, Token.Column, etError, Self);
end;

(**

  This method parses the elements of the xml document (tags).

  @precon  None.
  @postcon Parses the elements of the xml document (tags).

  @param   E as a TXMLElement
  @return  a Boolean

**)
function TXMLModule.EatCharData(E : TXMLElement): Boolean;

begin
  Result := (Token.Token[1] <> '<');
  If Result Then
    Begin
      E.AddContextText(Token.Token);
      NextNonCommentToken;
    End;
end;

(**

  This method parses the Element element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Element element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.Element(xmlParent : TElementContainer) : Boolean;

Var
  xmlChild : TXMLElement;

begin
  xmlChild := STag(xmlParent);
  Result := xmlChild <> Nil;
  If Result And (xmlChild <> Nil) And Not xmlChild.Referenced Then
    Begin
      Content(xmlChild);
      ETag(xmlChild);
    End;
end;

(**

  This method parses the ElementDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true of the element was parsed as a ElementDecl.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.ElementDecl(xmlParent : TElementContainer): Boolean;

var
  E: TXMLElemDecl;
  strName : String;
  T: TTokenInfo;

begin
  Result := False;
  If Token.Token = '<!' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'ELEMENT') = 0 Then
        Begin
          Result := True;
          E := xmlParent.Add(TXMLElemDecl.Create('!' + Token.Token,
            scNone, Token.Line, Token.Column, iiPublicClass, Nil)) As TXMLElemDecl;
          NextNonCommentToken;
          If WhiteSpace(Nil) Then
            Begin
              T := Token;
              strName := XMLName;
              If strName = '' then
                AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line,
                  T.Column]), scNone, T.Line, T.Column, etError, Self)
              Else
                E.AddToken(strName);
              If WhiteSpace(Nil) Then
                Begin
                  ContentSpec(E);
                  Whitespace(Nil);
                  If Token.Token = '>' Then
                    NextNonCommentToken
                  Else
                    ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
                End Else
                  ErrorAndSeekToken(strExpectedWhiteSpace, Token.Token, strSeekableOnErrorTokens,
                    stActual, Self);
            End Else
              ErrorAndSeekToken(strExpectedWhiteSpace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the EncName element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the EncName element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.EncName(xmlParent : TElementContainer);

var
  strToken: String;
  i: Integer;
  iValidchars: Integer;

begin
  If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
    Begin
      strToken := Token.Token;
      iValidchars := 0;
      If Length(strToken) > 1 Then
        If IsInSet(strToken[2], ['A'..'Z', 'a'..'z']) Then
          Inc(iValidchars, 2);
      For i := 2 To Length(strToken) - 1 Do
        If IsInSet(strToken[i], ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']) Then
          Inc(iValidchars);
      If iValidchars = Length(strToken) Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strEncNameContainsInvalidChars, Token.Token, strSeekableOnErrorTokens,
          stActual, Self);
    End Else
      ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);

end;

(**

  This method parses the EncodingDecl elements of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the EncodingDecl elements of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.EncodingDecl(xmlParent : TElementContainer);
begin
  If CompareText(Token.Token, 'encoding') = 0 Then
    Begin
      AddToExpression(xmlParent);
      Eq(xmlParent);
      EncName(xmlParent);
    End;
end;

(**

  This method parses the EntityDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the EntityDecl element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.EntityDecl(xmlParent : TElementContainer): Boolean;
begin
  Result := GEDecl(xmlParent);
end;

(**

  This method parses the EntityDef element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the EntityDef element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.EntityDef(xmlParent: TElementContainer);
begin
  If ExternalID(xmlParent) Then
    NDataDecl(xmlParent)
  Else
    EntityValue(xmlParent);
end;

(**

  This method parses the EntityRef element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the EntityRef element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.EntityRef(xmlParent: TElementContainer): Boolean;

Var
  strName  : String;
  T: TTokenInfo;

begin
  Result := False;
  If Token.Token = '&' Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      T := Token;
      strName := XMLName;
      If strName <> '' Then
        xmlParent.AddToken(strName)
      Else
        AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
          scNone, T.Line, T.Column, etError, Self);
      If Token.Token = ';' Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the EntityValue element of the grammar.

  @precon  None.
  @postcon Parses the EntityValue element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.EntityValue(xmlParent : TElementContainer): Boolean;

var
  strValue: String;

begin
  Result := False;
  If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
    Begin
      Result := True;
      strValue := Token.Token;
      strValue := Copy(strValue, 2, Length(strValue) - 2);
      If Length(strValue) > 1 Then
        Begin
          If strValue[1] = '%' Then
            Begin
              If Not Like('%*;', strValue) Then
                AddIssue(Format(strExpectedWord, ['<PEReference>', Token.Line,
                  Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
            End;
          //: @todo Should be either %Name; or &Name; or &#Number; or &#xHexNumber;
        End;
      AddToExpression(xmlParent);
    End Else
      ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the EnumeratedType element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the EnumeratedType element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.EnumerateType(xmlParent: TElementContainer);
begin
  If Not NotationType(xmlParent) Then
    Enumeration(xmlParent);
end;

(**

  This method parses the Numeration element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Numeration element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.Enumeration(xmlParent: TElementContainer);
begin
  If Token.Token = '(' Then
    Begin
      AddToExpression(xmlParent);
      Whitespace(Nil);
      NmToken(xmlParent);
      Whitespace(Nil);
      While Token.Token = '|' Do
        Begin
          AddToExpression(xmlParent);
          Whitespace(Nil);
          NmToken(xmlParent);
          Whitespace(Nil);
        End;
      Whitespace(Nil);
      If Token.Token = ')' Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
    End Else
      ErrorAndSeekToken(strLiteralExpected, '(', strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the Eq element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Eq element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.Eq(xmlParent : TElementContainer);
begin
  WhiteSpace(Nil);
  If Token.Token = '=' Then
    AddToExpression(xmlParent)
  Else
    ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual, Self);
  WhiteSpace(Nil);
end;

(**

  This method parses the ETag element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the ETag element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.ETag(xmlParent : TElementContainer);

Var
  strName : String;
  iLine, iColumn : Integer;

begin
  If Token.Token = '</' Then
    Begin
      NextNonCommentToken;
      iLine := Token.Line;
      iColumn := Token.Column;
      strName := XMLName;
      If strName <> xmlParent.Identifier Then
        AddIssue(Format(atrExpectedEndTagNamed, [xmlParent.Identifier, strName,
          iLine, iColumn]), scNone, iLine, iColumn, etWarning, Self);
      Whitespace(Nil);
      If Token.Token = '>' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
    End Else
      ErrorAndSeekToken(strExpectedEndTag, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the CombiningChar element of the grammar.

  @precon  None.
  @postcon Always returns false in this implementation.

  @return  a Boolean

**)
function TXMLModule.Extender: Boolean;
begin
  Result := False;
end;

(**

  This method parses the ExternalID element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the ExternalID element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.ExternalID(xmlParent : TElementContainer) : Boolean;
begin
  Result := False;
  If CompareText(Token.Token, 'SYSTEM') = 0 Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      If Not Whitespace(Nil) Then
        ErrorAndSeekToken(strExpectedWhiteSpace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      SystemLiteral(xmlParent);
    End Else
  If CompareText(Token.Token, 'PUBLIC') = 0 Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      If Not Whitespace(Nil) Then
        ErrorAndSeekToken(strExpectedWhiteSpace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      PubIDLiteral(xmlParent);
      If Not Whitespace(Nil) Then
        ErrorAndSeekToken(strExpectedWhiteSpace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      SystemLiteral(xmlParent);
    End;
end;

(**

  This method parses the ExtSubSet element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true of a TextDecl or ExtSubSetDecl was parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.ExtSubSet(xmlParent: TElementContainer): Boolean;

begin
  DocTypeDecl(xmlParent);
  Result := TextDecl(xmlParent);
  Result := Result Or ExtSubSetDecl(xmlParent);
end;

(**

  This method parses the ExtSubSetDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true of a Markup, ConditionalSect or DeclSep was found and
           parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.ExtSubSetDecl(xmlParent: TElementContainer) : Boolean;
begin
  Result := False;
  While (MarkupDecl(xmlParent) Or ConditionalSect(xmlParent) or DeclSep(xmlParent)) Do
    Begin
      Result := True;
      Whitespace(Nil);
    End;
end;

(**

  This method parses the TokenizedType element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true if the element was a tokenized type.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.TokenizedType(xmlParent: TElementContainer): Boolean;
begin
  Result := IskeyWord(Token.Token, ['entities', 'entity', 'id', 'idref',
    'idrefs', 'nmtoken', 'nmtokens']);
  If Result Then
    AddToExpression(xmlParent);
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TXMLModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;
  strSingleSymbols = [#9, #10, #13, #32, '%', ';', '(', ')', '*', '+', ','];
  (** A set of characters for single quotes **)
  strSingleQuotes = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers = ['#', 'a'..'z', 'A'..'Z', '_', '.', #192..#214, #216..#246,
    #248..#255];
  (** A set of number characters. **)
  strNumbers  = ['#', '$', '0'..'9'];
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
  boolInTag : Boolean;

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
    If strToken = #9 Then
      strToken := '<Tab>';
    If strToken = #32 Then
      strToken := '<Space>';
  End;

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
  boolInTag := False;

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

      If (LastCharType <> CurCharType) Or
        IsInSet(Ch, strSingleSymbols) Or
        IsInSet(LastChar, strSingleSymbols) Or
        (Not (BlockType In [btSingleLiteral, btDoubleLiteral]) And
        (
          (Not (IsInSet(Ch, ['>', '-'])) And (LastChar = '-')) Or
          ((Ch = '<') Or (LastChar = '>')) Or
          ((Ch = '>') And (LastChar = ']')) Or
          ((Ch = '>') And (LastChar = ')')) Or
          ((Ch = '>') And (LastChar = '?')) Or
          ((Ch = '>') And (LastChar = '<')) Or
          ((Ch = ']') And (LastChar = '[')) Or
          ((Ch = ']') And (LastChar <> ']'))
        )) Or
        ((BlockType In [btNoBlock]) And
        (CurCharType = ttLineEnd) And (Ch = #13)) Then
        Begin
          If Not (BlockType In [btSingleLiteral, btDoubleLiteral]) Or
            ((BlockType In [btNoBlock]) And (CurCharType = ttLineEnd) And (Ch = #13)) Or
            (Not (BlockType In [btSingleLiteral, btDoubleLiteral]) And
            (
              (Not (IsInSet(Ch, ['>', '-'])) And (LastChar = '-')) Or
              ((Ch = '<') OR (LastChar = '>')) Or
              ((Ch = '>') And (LastChar = ']')) Or
              ((Ch = '>') And (LastChar = ')')) Or
              ((Ch = '>') And (LastChar = '?')) Or
              ((Ch = '>') And (LastChar = '<')) Or
              ((Ch = ']') And (LastChar = '[')) Or
              ((Ch = ']') And (LastChar <> ']'))
            )) Then
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  ProcessWhiteSpace(strToken);
                  AddToken(TTokenInfo.Create(strToken, iStreamPos,
                    iTokenLine, iTokenColumn, Length(strToken), LastCharType));
                  If Not (LastCharType In [ttWhiteSpace, ttLineEnd]) Then
                    If strToken[1] = '<' Then
                      boolInTag := True
                    Else If strToken[Length(strToken)] = '>' Then
                      boolInTag := False;
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

      If boolInTag Then
        Begin
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
        End;

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
        AddToken(TTokenInfo.Create(strToken, iStreamPos,
          iTokenLine, iTokenColumn, Length(strToken), CurCharType));
      End;
  AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
    ttFileEnd));
End;

(**

  This method parses the VersionInfo element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the VersionInfo element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.VersionInfo(xmlParent : TElementContainer) : Boolean;
begin
  Result := False;
  WhiteSpace(Nil);
  If CompareText(Token.Token, 'version') = 0 Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      Eq(xmlParent);
      VersionNum(xmlParent);
    End;
end;

(**

  This method parses the VersionNum element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the VersionNum element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.VersionNum(xmlParent : TElementContainer);

var
  strNum: String;
  iNum: Integer;
  iErrorCode: Integer;
  strQuote: String;

begin
  If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
    Begin
      strNum := Copy(Token.Token, 2, Token.Length - 2);
      strQuote := Copy(Token.Token, 1, 1);
      If Copy(strNum, 1, 2) = '1.' Then
        Begin
          If Length(strNum) = 3 Then
            Begin
              Val(Copy(strNum, 3, 1), iNum, iErrorCode);
              If iErrorCode = 0 Then
                Begin
                  xmlParent.AddToken(Format('%s1.%d%s', [strQuote, iNum, strQuote]));
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strIsNotAValidVersionNum, Token.Token, strSeekableOnErrorTokens,
                    stActual, Self);
            End Else
              ErrorAndSeekToken(strExpectedVersionNum, Token.Token, strSeekableOnErrorTokens, stActual,
                Self);
        End Else
          ErrorAndSeekToken(strExpectedVersionNum, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End Else
      ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the Whitespace element in the grammar.

  @precon  None.
  @postcon Returns true if whitespace was encountered.

  @param   E as a TXMLElement
  @return  a Boolean

**)
function TXMLModule.Whitespace(E : TXMLElement): Boolean;

begin
  Result := False;
  While Token.TokenType In [ttWhiteSpace, ttLineEnd] Do
    Begin
      Result := True;
      If E <> Nil Then
        E.AddContextText(#32);
      NextNonCommentToken;
    End;
end;

(**

  This method parses the Comment element of the grammar.

  @precon  None.
  @postcon Return true if the method parsed a comment.

  @return  a Boolean

**)
function TXMLModule.XMLComment: Boolean;
begin
  Result := False;
  If Token.Token = '<!--' Then
    Begin
      Result := True;
      While Token.Token <> '-->' Do
        NextNonCommentToken;
      If Token.Token = '-->' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, '-->', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the start and end of the XML declarations as defined in the
  grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the start and end of the XML declarations as defined in the
           grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.XMLDecl(xmlParent : TElementContainer);
begin
  If Token.Token = '<?' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'xml') = 0 Then
        Begin
          xmlParent := xmlParent.Add(TXMLDecl.Create('?' + Token.Token, scNone,
            Token.Line, Token.Column, iiPublicObject, Nil));
          NextNonCommentToken;
          If Not VersionInfo(xmlParent) Then
            ErrorAndSeekToken(strExpectedWord, 'version', strSeekableOnErrorTokens, stActual, Self);
          Whitespace(Nil);
          EncodingDecl(xmlParent);
          Whitespace(Nil);
          SDDecl(xmlParent);
          Whitespace(Nil);
          If Token.Token = '?' Then
            Begin
              AddToExpression(xmlParent);
              If Token.Token = '>' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, '?>', strSeekableOnErrorTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '?>', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the Name element of the grammar.

  @precon  None.
  @postcon Returns an instance of a TXMLElement with the names as the xml
           element identifier. Additionally the element is created as a child
           of the given parent element.

  @return  a String

**)
Function TXMLModule.XMLName : String;

Var
  boolPRRef : Boolean;

begin
  Result := '';
  boolPRRef := False;
  If Token.Token = '%' Then
    Begin
      Result := Result + Token.Token;
      boolPRRef := True;
      NextNonCommentToken;
    End;
  If (Token.TokenType In [ttIdentifier]) Or (IsKeyWord(Token.Token, ['-', ':'])) Then
    Begin
      Result := Result + Token.Token;
      NextNonCommentToken;
      While (Token.TokenType In [ttIdentifier]) Or (IsKeyWord(Token.Token, ['-', '.', ':', '_'])) Do
        Begin
          Result := Result + Token.Token;
          NextNonCommentToken;
        End;
    End;
  If boolPRRef Then
    If Token.Token = ';' Then
      Begin
        Result := Result + Token.Token;
        NextNonCommentToken;
      End Else
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the PI element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true IF a PI element was parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.XMLPI(xmlParent : TElementContainer): Boolean;

Var
  pit : TXMLPI;

begin
  Result := False;
  If Token.Token = '<?' Then
    Begin
      Result := True;
      NextNonCommentToken;
      pit := PITarget(xmlParent);
      If pit <> Nil Then
        If WhiteSpace(Nil) Then
          Begin
            While (PrevToken.Token <> '?') And (Token.Token <> '>') Do
              If Token.TokenType In [ttWhiteSpace, ttLineEnd] Then
                NextNonCommentToken
              Else
                AddtoExpression(pit);
          End;
      If Token.Token = '?' Then
        AddToExpression(pit);
      If Token.Token = '>' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, '?>', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TXMLModule.ParseTokens;
begin
  Goal;
end;

(**

  This method parses the PEDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the PEDecl element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.PEDecl(xmlParent: TElementContainer) : Boolean;

Var
  strName : String;
  T: TTokenInfo;

begin
  Result := False;
  If Token.Token = '%' Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      If Not WhiteSpace(Nil) Then
        ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      T := Token;
      strName := XMLName;
      If strName = '' Then
        AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
          scNone, T.Line, T.Column, etError, Self)
      Else
        xmlParent.AddToken(strName);
      If Not WhiteSpace(Nil) Then
        ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      EntityDef(xmlParent);
      Whitespace(Nil);
      If Token.Token = '>' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End
end;

(**

  This method parses the PEReference element of the grammar.

  @precon  None.
  @postcon Parses the PEReference element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.PEReference(xmlParent : TElementContainer): Boolean;

Var
  strName : String;
  T: TTokenInfo;
  PERef: TXMLPERef;

begin
  Result := Token.Token = '%';
  If Result Then
    Begin
      NextNonCommentToken;
      T := Token;
      strName := XMLName;
      If strName = '' Then
        ErrorAndSeekToken(strExpectedWord, '<name>', strSeekableOnErrorTokens, stActual, Self);
      PERef := xmlParent.Add(TXMLPERef.Create(strName, scNone, T.Line,
        T.Column, iiPublicProperty, Nil)) As TXMLPERef;
      If Token.Token = ';' Then
        AddtoExpression(PERef)
      Else
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the PITarget element of the grammar.

  @precon  None.
  @postcon Parses the PITarget element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a TXMLPI

**)
Function TXMLModule.PITarget(xmlParent : TElementContainer) : TXMLPI;

Var
  strName : String;
  T: TTokenInfo;

begin
  Result := Nil;
  T := Token;
  strName := XMLName;
  If strName <> '' Then
    Result := xmlParent.Add(TXMLPI.Create('?' + strName, scPublic, T.Line, T.Column,
      iiPublicField, Nil)) As TXMLPI;
  If CompareText(strName, 'xml') = 0 Then
    AddIssue(Format(strPITargetCanNotBeNamed, [T.Line, T.Column]), scNone,
      T.Line, T.Column, etError, Self);
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TXMLModule.ReservedWords: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TXMLModule.Directives: TKeyWords;

begin
  Result := Nil;
end;

(**

  This method parses the MarkupDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the MarkupDecl element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.MarkupDecl(xmlParent : TElementContainer) : Boolean;
begin
  Result :=
    ElementDecl(xmlParent) Or
    AttListDecl(xmlParent) Or
    EntityDecl(xmlParent) Or
    NotationDecl(xmlParent) Or
    XMLPI(xmlParent) Or
    XMLComment;
end;

(**

  This method parses the miscellaneous elements as defined in the grammar.

  @precon  None.
  @postcon Parses the miscellaneous elements as defined in the grammar.

  @param   xmlParent as a TElementContainer
  @param   iElements as an Integer as a reference
  @return  a Boolean

**)
Function TXMLModule.Misc(xmlParent : TElementContainer; var iElements : Integer) : Boolean;

begin
  Result := XMLComment;
  If Not Result Then
    Result := XMLPI(xmlParent);
  If Result Then
    Inc(iElements);
  If Not Result Then
    Result := Whitespace(Nil);
end;

(**

  This method parses the Mixed element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the Mixed element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.Mixed(xmlParent : TElementContainer): Boolean;

var
  strName: String;
  iNames : Integer;
  T: TTokenInfo;

begin
  Result := False;
  If Token.Token = '(' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, '#PCDATA') = 0 Then
        Begin
          Result := True;
          xmlParent.AddToken('(', ttSymbol);
          AddToExpression(xmlParent);
          WhiteSpace(Nil);
          iNames := 0;
          While Token.Token = '|' Do
            Begin
              AddToExpression(xmlParent);
              Whitespace(Nil);
              T := Token;
              strName := XMLName;
              If strName <> '' Then
                Begin
                  xmlparent.AddToken(strName);
                  Inc(iNames);
                  Whitespace(Nil);
                End Else
                  AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line,
                    T.Column]), scNone, T.Line, T.Column, etError, Self);
            End;
          If Token.Token = ')' Then
            AddToExpression(xmlParent)
          Else
            ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
          If iNames > 0 Then
            If Token.Token = '*' Then
              AddToExpression(xmlParent)
            Else
              ErrorAndSeekToken(strLiteralExpected, '*', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the NameChar element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true if the element was a NameChar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.NameChar(xmlParent: TElementContainer): Boolean;
begin
  Result := False;
  If Token.TokenType In [ttIdentifier, ttNumber] Then
    Result := True;
  If Not Result Then
    Result := IsKeyWord(Token.Token, ['-', '.', ':', '_']);
  If Not Result Then
    Result := CombiningChar;
  If Not Result Then
    Result := Extender;
end;

(**

  This method parses the NDataDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the NDataDecl element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.NDataDecl(xmlParent : TElementContainer);

var
  strName: String;
  T: TTokenInfo;

begin
  If Whitespace(Nil) Then
    Begin
      If CompareText(Token.Token, 'NDATA') = 0 Then
        AddToExpression(xmlParent)
      Else
        ErrorAndSeekToken(strExpectedWord, 'NDATA', strSeekableOnErrorTokens, stActual, Self);
      If Not Whitespace(Nil) Then
        ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      T := Token;
      strName := XMLName;
      If strName <> '' Then
        xmlParent.AddToken(strName)
      Else
        AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
          scNone, T.Line, T.Column, etError, Self);
    End;
end;

(**

  This method parses the NmToken element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true if an NmToken element was parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.NmToken(xmlParent: TElementContainer): Boolean;
begin
  Result := False;
  While NameChar(xmlParent) Do
    Begin
      AddToExpression(xmlParent);
      If Not Result Then
        Result := True;
    End;
end;

(**

  This method parses the NotationDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the NotationDecl element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.NotationDecl(xmlParent : TElementContainer): Boolean;
var
  strName: String;
  N: TXMLElemDecl;
  T: TTokenInfo;

begin
  Result := False;
  If Token.Token = '<!' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'NOTATION') = 0 Then
        Begin
          Result := True;
          N := xmlParent.Add(TXMLElemDecl.Create('!' + Token.Token, scNone,
            Token.Line, Token.Column, iiPublicDispInterface, Nil)) As TXMLElemDecl;
          NextNonCommentToken;
          Whitespace(Nil);
          T := Token;
          strName := XMLName;
          If strName = '' Then
            AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
              scNone, T.Line, T.Column, etError, Self)
          Else
            N.AddToken(strName);
          Whitespace(Nil);
          If Not ExternalID(N) Then
            PublicID(N);
          Whitespace(Nil);
          If Token.Token = '>' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the NotationType element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Parses the NotationType element of the grammar.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.NotationType(xmlParent: TElementContainer): Boolean;

var
  strName: String;
  T: TTokenInfo;

begin
  Result := False;
  If CompareText(Token.Token, 'NOTATION') = 0 Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
      If Whitespace(Nil) Then
        Begin
          If Token.Token = '(' Then
            Begin
              AddToExpression(xmlParent);
              Whitespace(Nil);
              T := Token;
              strName := XMLName;
              If strName = '' Then
                AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line,
                  T.Column]), scNone, T.Line, T.Column, etError, Self)
              Else
                xmlParent.AddToken(strName);
              Whitespace(Nil);
              While Token.Token = '|' Do
                Begin
                  AddToExpression(xmlParent);
                  Whitespace(Nil);
                  T := Token;
                  strName := XMLName;
                  If strName = '' Then
                    AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line,
                      T.Column]), scNone, T.Line, T.Column, etError, Self)
                  Else
                    xmlParent.AddToken(strName);
                  Whitespace(Nil);
                End;
              Whitespace(Nil);
              If Token.Token = ')' Then
                AddToExpression(xmlParent)
              Else
                ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '(', strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens, stActual,
            Self);
    End;
end;

(**

  This method parses the GEDecl element of the grammar.

  @precon  xmlParent must be a valid instance of a container.
  @postcon Returns true if the grammar parsed was a GEDecl element.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.GEDecl(xmlParent: TElementContainer): Boolean;

var
  strName: String;
  E : TXMLElemDecl;
  T: TTokenInfo;

begin
  Result := False;
  If Token.Token = '<!' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'ENTITY') = 0 Then
        Begin
          Result := True;
          E := xmlParent.Add(TXMLElemDecl.Create('!' + Token.Token, scNone,
            Token.Line, Token.Column, iiPublicInterface, Nil)) As TXMLElemDecl;
          NextNonCommentToken;
          If Not WhiteSpace(Nil) Then
            ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens, stActual,
              Self);
          If Not PEDecl(E) Then
            Begin
              T := Token;
              strName := XMLName;
              If strName = '' Then
                AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line,
                  T.Column]), scNone, T.Line, T.Column, etError, Self)
              Else
                E.AddToken(strName);
              If Not WhiteSpace(Nil) Then
                ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens,
                  stActual, Self);
              EntityDef(E);
              Whitespace(Nil);
              If Token.Token = '>' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, Token.Token, strSeekableOnErrorTokens, stActual,
                  Self);
            End;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method tries to get a document comment from the previous token and return a TComment class to the 
  calling routine.

  @precon  None.
  @postcon Returns the comment immediately before the current token else nil.

  @note    All comments found are automatically added to the comment collection for disposal when the 
           parser is destroyed.

  @param   CommentPosition as a TCommentPosition as a constant
  @return  a TComment

**)
Function TXMLModule.GetComment(Const CommentPosition : TCommentPosition) : TComment;

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
              Result := TXMLComment.CreateComment(T.Token, T.Line, T.Column);
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
function TXMLModule.GetModuleName: String;
begin
  Result := ExtractFilename(FileName);
end;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @param   iSkip as an Integer as a reference

**)
Procedure TXMLModule.ProcessCompilerDirective(var iSkip : Integer);

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method prases the XML file prolog section as defined in the xml grammar.

  @precon  xmlParent must be a valid container.
  @postcon Parses the XML file prolog section as defined in the xml grammar.

  @param   xmlParent as a TElementContainer
  @param   iElements as an Integer as a reference

**)
procedure TXMLModule.Prolog(xmlParent : TElementContainer; var iElements : Integer);

begin
  XMLDecl(xmlParent);
  Repeat
    // Do nothing
  Until Not Misc(xmlParent, iElements);
  If DocTypeDecl(xmlParent) Then
    Begin
      Repeat
        // Do nothing
      Until Not Misc(xmlParent, iElements);
    End;
end;

(**

  This method parses the PubIDLiteral element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Parses the PubIDLiteral element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.PubIDLiteral(xmlParent : TElementContainer);

var
  i: Integer;
  strValue: String;

begin
  If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
    Begin
      strValue := Token.Token;
      strValue := Copy(strValue, 2, Length(strValue) - 2);
      For i := 2 To Length(strValue) - 1 Do
        If Not IsInSet(strValue[i], [#32, #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
          '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
          '#', '@', '$', '_', '%']) Then
          Begin
            ErrorAndSeekToken(strExpectedWord, '<PubIDLiteral>', strSeekableOnErrorTokens, stActual,
              Self);
            Exit;
          End;
      AddToExpression(xmlParent);
    End Else
      ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the PublicID element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Parses the PublicID element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.PublicID(xmlParent: TElementContainer);
begin
  If CompareText(Token.Token, 'PUBLIC') = 0 Then
    AddToExpression(xmlParent)
  Else
    ErrorAndSeekToken(strExpectedWord, Token.Token, strSeekableOnErrorTokens, stActual, Self);
  If Not WhiteSpace(Nil) Then
    ErrorAndSeekToken(strExpectedWhitespace, Token.Token, strSeekableOnErrorTokens, stActual, Self);
  PubidLiteral(xmlParent);
end;

(**

  This method parses the Reference element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Returns true is a reference was parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.Reference(xmlParent: TElementContainer): Boolean;
begin
  Result := False;
  If Not EntityRef(xmlParent) Then
    Result := CharRef(xmlParent);
end;

(**

  This method does nothing as we are not referencing symbols in XML.

  @precon  None.
  @postcon Returns false always.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TXMLModule.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
End;

(**

  This method parses the SCDecl element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Parses the SCDecl element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.SDDecl(xmlParent : TElementContainer);

Var
  strAnswer : String;

begin
  If CompareText(Token.Token, 'standalone') = 0 Then
    Begin
      AddToExpression(xmlParent);
      Eq(xmlParent);
      If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
        Begin
          strAnswer := Copy(Token.Token, 2, Token.Length - 2);
          If (CompareText(strAnswer, 'yes') = 0) Or
            (CompareText(strAnswer, 'no') = 0) Then
            AddToExpression(xmlParent)
          Else
            ErrorAndSeekToken(strExpectedWord, 'Yes or No', strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the start tag of an element defined the the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Returns true IF the tag has an ending forward slash.

  @param   xmlParent as a TElementContainer
  @return  a TXMLElement

**)
Function TXMLModule.STag(xmlParent : TElementContainer) : TXMLElement;

Var
  strXMLName: String;
  X: TXMLElement;
  T: TTokenInfo;

begin
  Result := Nil;
  If Token.Token = '<' Then
    Begin
      NextNonCommentToken;
      T := Token;
      strXMLName := XMLName;
      If strXMLName <> '' Then
        Begin
          X := xmlParent.Add(TXMLElement.Create(strXMLName, scNone,
            T.Line, T.Column, iiPublicRecord, Nil)) As TXMLElement;
          Result := X;
          If FModuleType In [mtXHTML] Then
            If LowerCase(strXMLName) <> strXMLName Then
              AddIssue(Format(strHTMLElementLowercase, [strXMLName, T.Line,
                T.Column]), scNone, T.Line, T.Column, etWarning, Self);
          Whitespace(Nil);
          While Attribute(X) Do
            WhiteSpace(Nil);
        End Else
          AddIssue(Format(strExpectedWord, ['<name>', T.Token, T.Line, T.Column]),
            scNone, T.Line, T.Column, etError, Self);
      If Token.Token = '/>' Then
        Result.Referenced := True;
      If IsKeyWord(Token.Token, ['/>', '>']) Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the StringType element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon returns true of the element was a string type.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.StringType(xmlParent: TElementContainer): Boolean;
begin
  Result := False;
  If CompareText(Token.Token, 'CDATA') = 0 Then
    Begin
      Result := True;
      AddToExpression(xmlParent);
    End;
end;

(**

  This method parses the SystemLiteral element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Parses the SystemLiteral element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.SystemLiteral(xmlParent : TElementContainer);
begin
  If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
    AddToExpression(xmlParent)
  Else
    ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parses the TextDecl element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Returns true of a TextDecl was found and parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
Function TXMLModule.TextDecl(xmlParent: TElementContainer) : Boolean;
begin
  Result := False;
  If Token.Token = '<?' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'xml') = 0 Then
        Begin
          Result := True;
          xmlParent := xmlParent.Add(TXMLDecl.Create('?' + Token.Token, scNone,
            Token.Line, Token.Column, iiPublicObject, Nil));
          NextNonCommentToken;
          VersionInfo(xmlParent);
          Whitespace(Nil);
          EncodingDecl(xmlParent);
          Whitespace(Nil);
          If Token.Token = '?' Then
            Begin
              AddToExpression(xmlParent);
              If Token.Token = '>' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, '?>', strSeekableOnErrorTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '?>', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method deletes any root elements which dont and items in them.

  @precon  None.
  @postcon Deletes any root elements which dont and items in them.

**)
procedure TXMLModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Elements[iElement] Is TLabelContainer Then
        DeleteElement(iElement);
end;

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TXMLModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Begin
  Result := ChangeFileExt(Inherited AsString(boolShowIdentifier,
    boolForDocumentation), '');
End;

(**

  This method is the starting position for the parsing of an XML module. It
  finds the first non comment token and begins the grammar checking from their
  by deligating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating Syntax.

**)
procedure TXMLModule.Goal;

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
                C := TXMLComment.CreateComment(Token.Token, Token.Line,
                  Token.Column);
                AddBodyComment(C);
                If Comment = Nil Then
                  Comment := C;
              End;
            NextToken;
          End;
        // Check for end of file else must be identifier
        If Not EndOfTokens Then
          If Like('*.dtd', FileName) Then
            ExtSubSet(Self)
          Else
            Document;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError, Self);
  End;
end;

(**

  This method parses the Ignore element of the grammar.

  @precon  None.
  @postcon Parses the Ignore element of the grammar.

**)
procedure TXMLModule.Ignore;
begin
  While (Token.Token <> '<![') And (Token.Token <> ']]') Do
    NextNonCommentToken;
end;

(**

  This method parses the IgnoreSect element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Returns true if an IgnoreSect element was found and parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.IgnoreSect(xmlParent: TElementContainer): Boolean;

Var
  I : TXMLIgnoreElement;

begin
  Result := False;
  If Token.Token = '<![' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'IGNORE') = 0 Then
        Begin
          I := xmlParent.Add(TXMLIgnoreElement.Create('IGNORE', scNone, Token.Line,
            Token.Column, iiPublicConstant, Nil)) As TXMLIgnoreElement;
          NextNonCommentToken;
          Whitespace(Nil);
          If Token.Token = '[' Then
            Begin
              AddToExpression(I);
              IgnoreSectContents;
              If Token.Token = ']]' Then
                Begin
                  I.AddToken(']', ttSymbol);
                  NextNonCommentToken;
                  If Token.Token = '>' Then
                    NextNonCommentToken
                  Else
                    ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
                End Else
                  ErrorAndSeekToken(strLiteralExpected, ']]>', strSeekableOnErrorTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '[', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the IgnoreSectContents element of the grammar.

  @precon  None.
  @postcon Parses the IgnoreSectContents element of the grammar.

**)
procedure TXMLModule.IgnoreSectContents;
begin
  Ignore;
  While Token.Token = '<![' Do
    Begin
      NextNonCommentToken;
      IgnoreSectContents;
      If Token.Token = ']]' Then
        Begin
          PushTokenPosition;
          NextNonCommentToken;
          If Token.Token = '>' Then
            NextNonCommentToken
          Else
            PopTokenPosition;
        End Else
          ErrorAndSeekToken(strLiteralExpected, ']]>', strSeekableOnErrorTokens, stActual, Self);
      Ignore;
    End;
end;

(**

  This method parses the IncludeSect element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Returns true IF an IncludeSect was found and parsed.

  @param   xmlParent as a TElementContainer
  @return  a Boolean

**)
function TXMLModule.IncludeSect(xmlParent: TElementContainer): Boolean;

Var
  I : TXMLIncludeElement;

begin
  Result := False;
  If Token.Token = '<![' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If CompareText(Token.Token, 'INCLUDE') = 0 Then
        Begin
          Result := True;
          I := xmlParent.Add(TXMLIncludeElement.Create('INCLUDE', scNone, Token.Line,
            Token.Column, iiPublicConstant, Nil)) As TXMLIncludeElement;
          NextNonCommentToken;
          Whitespace(Nil);
          If Token.Token = '[' Then
            Begin
              AddToExpression(I);
              ExtSubsetDecl(I);
              If Token.Token = ']]' Then
                Begin
                  I.AddToken(']', ttSymbol);
                  NextNonCommentToken;
                  If Token.Token = '>' Then
                    NextNonCommentToken
                  Else
                    ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
                End Else
                  ErrorAndSeekToken(strLiteralExpected, ']]>', strSeekableOnErrorTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '[', strSeekableOnErrorTokens, stActual, Self);
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the IntSubSet element of the grammar.

  @precon  xmlParent must be a valid container.
  @postcon Parses the IntSubSet element of the grammar.

  @param   xmlParent as a TElementContainer

**)
procedure TXMLModule.IntSubSet(xmlParent : TElementContainer);
begin
  If Not MarkupDecl(xmlParent) Then
    DeclSep(xmlParent);
end;

End.
