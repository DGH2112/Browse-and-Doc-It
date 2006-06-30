(**

  ObjectPascalModule : A unit to tokenize Pascal source code.

  @precon     Before this class con be constructed it requires an instance of a
              TStream decendant passed to the constructor which contains the
              source code text to be parsed.

  @bug        Comments which lie out side the method and not conisdered part of
              the tokens parsed are not picked up during as BodyComments, i.e.

              //: @@todo First comment
              //: @@todo Second comment

              The first comment is not picked up as it does not relate to a
              specific token.

  @Version    1.0
  @Date       30 Jun 2006
  @Author     David Hoyle

**)
Unit PascalDocModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule;

Type
  (** This is an enumerate to describe the type of constant expression. **)
  TExprType = (etUnknown, etConstExpr, etString, etNumeric);
  (** This is a set of TExprType enumerates. **)
  TExprTypes = Set of TExprType;

  (** A type to define the type of token search. **)
  TSeekToken = (stActual, stFirst);

  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TPascalDocModule = Class(TBaseLanguageModule)
  Private
    FSourceStream : TStream;
    { Grammar Parsers }
    Procedure Goal;
    Function OPProgram : Boolean;
    Function OPUnit : Boolean;
    Function OPPackage : Boolean;
    Function OPLibrary : Boolean;
    Procedure ProgramBlock;
    procedure UsesClause;
    Procedure PortabilityDirective;
    Procedure InterfaceSection;
    Procedure InterfaceDecl;
    Function ExportedHeading : Boolean;
    Procedure ImplementationSection;
    Procedure Block(Scope : TScope; Method : TMethodDecl);
    Function ExportedStmt : Boolean;
    procedure ExportsItem;
    Procedure DeclSection(Scope : TScope; Method : TMethodDecl);
    Function LabelDeclSection : Boolean;
    Function ConstSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function ConstantDecl(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function ResStringSection(Scope: TScope; Method : TMethodDecl): Boolean;
    Function ResourceStringDecl(Scope: TScope; Method : TMethodDecl): Boolean;
    Function TypeSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function TypeDecl(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function GetTypeDecl : TTypeDecl;
    Function TypedConstant(C: TGenericContainer; T : TTypeDecl) : Boolean;
    Function ArrayConstant(C: TGenericContainer; T : TTypeDecl) : Boolean;
    Function RecordConstant(C: TGenericContainer; T : TTypeDecl) : Boolean;
    Function RecordFieldConstant(C : TGenericContainer; T : TTypeDecl) : Boolean;
    function OPType : TTypeDecl;
    function RestrictedType : TRestrictedType;
    Function ClassRefType : TClassRefType;
    Function SimpleType : TSimpleType;
    function RealType : TRealType;
    function OrdinalType : TOrdinalType;
    function OrdIdent : TOrdIdent;
    Function VariantType : TVariantType;
    function SubRangeType : TSubRangeType;
    function EnumerateType : TEnumerateType;
    Procedure EnumerateElement(EnumerateType : TEnumerateType);
    Function StringType : TStringType;
    Function StrucType : TTypeDecl;
    function ArrayType(boolPacked : Boolean): TArrayType;
    Function RecType(boolPacked : Boolean) : TRecordDecl;
    Procedure FieldList(Rec : TRecordDecl);
    procedure FieldDecl(Rec: TRecordDecl);
    procedure RecVariant(Rec: TRecordDecl);
    function SetType(boolPacked: Boolean): TSetType;
    function FileType(boolPacked: Boolean): TFileType;
    Function VariantSection(Rec: TRecordDecl) : Boolean;
    Function PointerType : TPointerType;
    Function ProcedureType : TProcedureType;
    Function VarSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function ThreadVarSection(Scope : TScope) : Boolean;
    Function VarDecl(Scope : TScope; VarSection : TGenericContainerCollection) : Boolean;
    Procedure Expression(C : TGenericContainer; var ExprType : TExprTypes);
    Procedure SimpleExpression(C : TGenericContainer; var ExprType : TExprTypes);
    Procedure Term(C : TGenericContainer; var ExprType : TExprTypes);
    Procedure Factor(C : TGenericContainer; var ExprType : TExprTypes);
    Function RelOp(C : TGenericContainer; ExprType : TExprTypes) : Boolean;
    Function AddOp(C : TGenericContainer) : Boolean;
    Function MulOp(C : TGenericContainer; var ExprType : TExprTypes) : Boolean;
    Procedure Designator(C : TGenericContainer; var ExprType : TExprTypes);
    Procedure DesignatorSubElement(C : TGenericContainer;
      var ExprType : TExprTypes; strValidSymbols : Array of String);
    Function SetConstructor(C : TGenericContainer) : Boolean;
    Procedure SetElement(C : TGenericContainer);
    Procedure ExprList(C : TGenericContainer);
    Procedure Statement;
    Procedure StmtList;
    Procedure SimpleStatement;
    Function StructStmt : Boolean;
    Function CompoundStmt : Boolean;
    Function ConditionalStmt : Boolean;
    Function IfStmt : Boolean;
    Function CaseStmt : Boolean;
    Procedure CaseSelector;
    Procedure CaseLabel;
    Function LoopStmt : Boolean;
    Function RepeatStmt : Boolean;
    Function WhileStmt : Boolean;
    Function ForStmt : Boolean;
    Function WithStmt : Boolean;
    Function TryExceptAndFinallyStmt : Boolean;
    Function ExceptionBlock : Boolean;
    Function RaiseStmt : Boolean;
    Function AssemblerStatement : Boolean;
    Function ProcedureDeclSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Procedure ProcedureDecl;
    Procedure FunctionDecl;
    Function FunctionHeading(Scope :TScope) : TMethodDecl;
    Function ProcedureHeading(Scope : TScope) : TMethodDecl;
    Procedure FormalParameter(Method : TMethodDecl);
    Procedure FormalParam(Method : TMethodDecl);
    Procedure Parameter(Method : TMethodDecl; ParamMod : TParamModifier);
    Procedure Directive(M : TMethodDecl);
    Function ObjectType : TObjectDecl;
    Procedure ObjHeritage(ObjDecl : TObjectDecl);
    Function MethodList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    function MethodHeading(Cls: TObjectDecl; Scope: TScope): Boolean;
    Function ConstructorHeading(Scope :TScope) : TMethodDecl;
    Function DestructorHeading(Scope :TScope) : TMethodDecl;
    Function ObjFieldList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Procedure InitSection;
    Function ClassType : TClassDecl;
    Procedure ClassHeritage(Cls : TObjectDecl);
    procedure ClassVisibility(var Scope : TScope);
    Function ClassFieldList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Function ClassMethodList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Function ClassPropertyList(Cls : TClassDecl; var Scope : TScope) : Boolean;
    Function PropertyList(Cls : TClassDecl; var Scope : TScope) : Boolean;
    Procedure PropertyInterface(Prop : TProperty);
    Procedure PropertyParameterList(Prop : TProperty);
    Procedure PropertySpecifiers(Prop : TProperty);
    Function  InterfaceType : TInterfaceDecl;
    Procedure InterfaceHeritage(InterfaceDecl : TInterfaceDecl);
    Procedure RequiresClause;
    procedure ContainsClause;
    Function IdentList(OwnList : Boolean; SeekTokens : Array Of String): TIdentList;
    // Procedure QualId; // These can be iterated Form1.Control1.Click
    Function TypeId(C: TGenericContainer) : Boolean;
    // Procedure Ident;
    Function ConstExpr(C: TGenericContainer; var ExprType : TExprTypes) : Boolean;
    // Procedure UnitId;
    // Procedure LabelId;
    // Procedure Number;
    // Procedure OpString;
    (* Helper method to the grammar parsers *)
    Function ProcedureBit(ProcType : TMethodType; Scope : TScope) : TMethodDecl;
    Procedure ScopeImplementedMethods;
    procedure Sort;
    Procedure TokenizeStream;
    Procedure ParseTokens;
    procedure ErrorAndSeekToken(strMsg, strMethod, strExpected: String;
      SeekTokens: Array Of String; SeekToken : TSeekToken);
    Procedure AddToExpression(C : TGenericContainer);
    function IsToken(strToken: String; C: TGenericContainer): Boolean;
    procedure ArrayElement(C : TGenericContainer;
      iStartDimension: Integer; AT : TArrayType);
    Function UndefinedToken : Boolean;
  Public
    Constructor Create(Source : TStream; strFileName : String; IsModified : Boolean;
      ModuleOptions : TModuleOptions; DocOptions : TDocOptions);
    Destructor Destroy; Override;
    Function FindMethodAtStreamPosition(iStreamPos : TStreamPosition;
      var recPosition : TTokenPosition) : TMethodDecl;
    Function FindPropertyAtStreamPosition(iStreamPos: TStreamPosition;
      var recPosition : TTokenPosition): TClassDecl;
  End;

  (** This class is a collection class to contain and management the life time
      of multiple instances of TPascalDocModule classes.**)
  TPascalDocModuleList = Class
  Private
    FModules : TObjectList;
    function GetCount: Integer;
    function GetModule(iIndex: Integer): TPascalDocModule;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Source : TPascalDocModule);
    Procedure Delete(iIndex : Integer);
    (**
      This property provides access to the individual modules in the collection.
      @param   iIndex as       an Integer
      @return  a TPascalDocModule
    **)
    Property Module[iIndex : Integer] : TPascalDocModule Read GetModule; Default;
    (**
      This property returns the number of modules in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

Const
  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[1..64] Of String = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto', 'else',
    'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for',
    'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    {'nil', }'not', 'object', 'of', 'or', 'on', 'out', 'packed', 'procedure', 'program',
    'property', 'raise', 'record', 'repeat', 'resourcestring', 'set', 'shl',
    'shr', {'string', }'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'while', 'with', 'xor'
  );
  (** A sorted list of directives. Used for identifying tokens as
  directives. **)
  strDirectives : Array[1..40] Of String = (
    'absolute', 'abstract', 'assembler', 'automated', 'cdecl', 'contains',
    'default', 'dispid', 'dynamic', 'export', 'external', 'far', 'forward',
    'implements', 'index', 'message', 'name', 'near', 'nodefault', 'overload',
    'override', 'package', 'pascal', 'private', 'protected', 'public',
    'published', 'read', 'readonly', 'register', 'reintroduce', 'requires',
    'resident', 'safecall', 'stdcall', 'stored', 'varargs', 'virtual', 'write',
    'writeonly'
  );
  (** A sorted list of method directives. Used in identifying method
  directives. **)
  strMethodDirectives : Array[1..19] Of String = (
    'abstract', 'assembler', 'cdecl', 'dispid', 'dynamic', 'export',
    'external', 'far', 'forward',  'message', 'overload', 'override',
    'pascal', 'register', 'reintroduce', 'safecall', 'stdcall', 'varargs',
    'virtual'
  );
  (** A sorted list of property specifiers, includes a semi colon to end the
  property. **)
  strPropSpecs : Array[1..8] Of String = (
    ';', 'default', 'implements', 'index', 'nodefault', 'read', 'stored',
    'write'
  );
  (** A list of real types. **)
  strRealTypes : Array[1..7] Of String = ('comp', 'currency',
    'double', 'extended', 'real', 'real48', 'single');
  (** A list of ordinal idents **)
  strOrdIdents : Array[1..12] Of String = ('boolean', 'byte', 'char', 'int64',
    'integer', 'longint', 'longword', 'pchar', 'shortint', 'smallint',
    'widechar', 'word');
  (** A list of variants **)
  strVariants : Array[1..2] Of String = ('olevariant', 'variant');
  (** A list of string types. **)
  strStrings  : Array[1..3] Of String = ('ansistring', 'string', 'widestring');
  (** A string representing the Array Of parameter type. **)
  strArrayOf : Array[False..True] Of String = ('', 'Array Of ');
  (** This is a list of compound block statement start keywords. **)
  strBlockStarts : Array[1..4] Of String = ('asm', 'begin', 'case', 'try');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..30] Of String = (';', 'const', 'contains',
    'do', 'else', 'end', 'except', 'exports', 'finalization', 'finally',
    'function', 'implementation',
    'implements', 'initialization', 'interface', 'label', 'library', 'object',
    'package', 'procedure', 'program', 'record', 'requires', 'resourcestring',
    'then', 'type', 'unit', 'until', 'uses', 'var'
  );
//    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
//    'automated', 'begin', 'case', 'class', 'constructor',
//    'default', 'destructor', 'dispid', 'dispinterface', 'div', 'do', 'downto',
//    'else', 'except', 'file', 'finally',
//    'for', 'goto', 'if', 'in',
//    'index', 'inherited', 'inline', 'is',
//    'mod', 'name', 'near', 'nodefault', 'not',
//    'of', 'or', 'out', 'packed', 'private',
//    'property', 'protected', 'public', 'published', 'raise', 'read', 'readonly',
//    'repeat', 'resident', 'set', 'shl',
//    'shr', 'stored', 'then', 'threadvar', 'to', 'try', 'until',
//    'varargs', 'while', 'with', 'write', 'writeonly', 'xor'

  (** This is a list of functions which can be used in a const expression. **)
  strConstExprDesignators : Array[1..15] Of String = ('abs', 'chr', 'hi', 'high',
    'length', 'lo', 'low', 'odd', 'ord', 'pred', 'round', 'sizeof', 'succ',
    'swap', 'trunc');
  (** A list of Rel operators for expressions. **)
  strRelOps : Array[1..9] Of String = ('<', '<=', '<>', '=', '>', '>=', 'as',
    'in', 'is');
  (** A list of Add operators for expressions. **)
  strAddOps : Array[1..4] Of String = ('+', '-', 'or', 'xor');
  (** A list of Multiplier operators for expressions. **)
  strMulOps : Array[1..7] Of String = ('*', '/', 'and', 'div', 'mod', 'shl', 'shr');
  (** A list of expression concatenators which might appear after an identifier
      indicating that this might be an expression and not just a TypeId. **)
  strExpressionIndicators : Array[1..19] Of String = ('*', '+', '-', '/', '<',
    '<=', '<>', {'=', }'>', '>=', 'and', 'as', 'div', 'in', 'is', 'mod', 'or',
    'shl', 'shr', 'xor');
  (** A list of portability directives. **)
  strPortabilityDirective : Array[1..3] Of String = ('deprecated', 'library',
    'platform');

Implementation

Uses
  PascalDocChecker;

(**

  Thid method checks to see if the supplied tag is a special tag.

  @precon  strTagName is the name of a special tag to be checked.
  @postcon Returns the index of the special tag or -1 is not found.

  @param   strTagName as a String
  @return  a Integer

**)
Function IsSpecial(strTagName : String) : Integer;

Var
  i : Integer;

Begin
  Result := -1;
  For i := 0 To SpecialTags.Count - 1 Do
    If AnsiCompareText(SpecialTags.Names[i], strTagName) = 0 Then
      Begin
        Result := i;
        Exit;
      End;
End;

(**

  This is the constructor method for the TPascalDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text,
           that is the contents of a source code module and Filename is the file
           name of the module being parsed and IsModified determines if the
           source code module has been modified since the last save to disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a TStream
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions
  @param   DocOptions    as a TDocOptions

**)
Constructor TPascalDocModule.Create(Source : TStream; strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions; DocOptions : TDocOptions);

Begin
  Inherited Create(IsModified, strFileName);
  FSourceStream := Source;
  AddTickCount('Start');
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    ParseTokens;
  AddTickCount('Parse');
  Sort;
  AddTickCount('Sort');
  If (moParse In ModuleOptions) And
    (moCheckForDocumentConflicts In ModuleOptions) Then
    With TPascalDocChecker.Create(Self, DocOptions) Do
      Try
        CheckDocumentForConflicts;
        SortDocumentConflicts;
      Finally
        Free;
      End;
  AddTickCount('Check');
End;

(**

  This is the destructor method for the TPascalDocModule class.

  @precon  None.
  @postcon Destroy the class instance.

**)
Destructor TPascalDocModule.Destroy;

Begin
  Inherited Destroy;
End;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TPascalDocModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btLineComment, btBraceComment,
    btFullComment, btCompoundSymbol);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;
  test = 12.34;
  {: @todo Move to BaseLanguageModule }
  strSingleSymbols : Set Of Char = ['(', ')', ';', ',', '[', ']', '^', '-', '+', '/', '*'];

Var
  boolEOF : Boolean;
  (** Token buffer. **)
  strToken : String;
  CurCharType : TTokenType;
  LastCharType : TTokenType;
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
  LastToken : TTokenType;

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  boolEOF := False;
  CurCharType := ttUnknown;
  LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  Ch := #0;
  LastChar := #0;
  strToken := '';
  LastToken := ttUnknown;

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
    If FSourceStream <> Nil Then
      Begin
        Repeat
          If FSourceStream.Read(ch, 1) > 0 Then
            Begin
              Inc(iStreamCount);
              LastCharType := CurCharType;
              CurCharType := GetTokenType(Ch, LastCharType);

              // Check for full block comments
              If (BlockType = btNoBlock) And (LastChar = '(') And (Ch = '*') Then
                BlockType := btFullComment;

              // Check for line comments
              If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '/') Then
                BlockType := btLineComment;

              If (LastCharType <> CurCharType) Or (Ch In strSingleSymbols) Or
                (LastChar In strSingleSymbols)Then
                Begin
                  If ((BlockType In [btStringLiteral, btLineComment]) And
                    (CurCharType <> ttLineEnd)) Or
                    (BlockType In [btBraceComment, btFullComment,
                    btCompoundSymbol]) Then
                    Begin
                      Inc(iTokenLen);
                      If iTokenLen > Length(strToken) Then
                        SetLength(strToken, iTokenCapacity + Length(strToken));
                      strToken[iTokenLen] := Ch;
                    End Else
                    Begin
                      SetLength(strToken, iTokenLen);
                      If Not IsTokenWhiteSpace(strToken) Then
                        Begin
                          If LastCharType = ttIdentifier Then
                            Begin
                              If IsKeyWord(strToken, strReservedWords) Then
                                LastCharType := ttReservedWord;
                              If IsKeyWord(strToken, strDirectives) Then
                                LastCharType := ttDirective;
                              If strToken[1] = '#' Then
                                LastCharType := ttStringLiteral;
                            End;
                          If BlockType = btLineComment Then
                            LastCharType := ttComment;
                          If (LastCharType = ttComment) And (Length(strToken) > 2) Then
                            If (strToken[1] = '{') And (strToken[2] = '$') Then
                              LastCharType := ttCompilerDirective;
                          If ((LastToken = ttNumber) And ((strToken = '.') Or (LastCharType = ttNumber))) Or
                            ((LastToken = ttStringLiteral) And (strToken[1] = '#')) Or
                            ((LastToken = ttStringLiteral) And (LastCharType = ttStringLiteral)) Then
                            Begin
                              AppendToLastToken(strToken);
                              LastToken := LastToken;
                            End Else
                            Begin
                              AddToken(TTokenInfo.Create(strToken, iStreamPos,
                                iTokenLine, iTokenColumn, Length(strToken), LastCharType));
                              LastToken := LastCharType;
                            End;
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
              If (BlockType = btFullComment) And (LastChar = '*') And (Ch = ')') Then
                Begin
                  BlockType := btNoBlock;
                  CurCharType := ttComment;
                End;

              // Check for string literals
              If CurCharType = ttStringLiteral Then
                If BlockType = btStringLiteral Then
                  BlockType := btNoBlock
                Else If BlockType = btNoBlock Then
                  BlockType := btStringLiteral;

              // Check for block Comments
              If (BlockType = btNoBlock) And (Ch = '{') Then
                Begin
                  CurCharType := ttComment;
                  BlockType := btBraceComment;
                End;
              If (BlockType = btBraceComment) And (Ch = '}') Then
                Begin
                  CurCharType := ttComment;
                  BlockType := btNoBlock;
                End;
              If BlockType = btCompoundSymbol Then
                BlockType := btNoBlock;

              Inc(iColumn);
              If Ch = #10 Then
                Begin
                  Inc(iLine);
                  iColumn := 1;
                  If BlockType In [btLineComment, btStringLiteral] Then
                    BlockType := btNoBlock;
                End;
              LastChar := Ch;
            End Else
              boolEOF := True;
        Until boolEOF;
        If iTokenLen > 0 Then
          Begin
            SetLength(strToken, iTokenLen);
            If Not IsTokenWhiteSpace(strToken) Then
              AddToken(TTokenInfo.Create(strToken, iStreamPos,
                iTokenLine, iTokenColumn, Length(strToken), LastCharType));
          End;
      End;
  Except
    On E : Exception Do
      Errors.Add(E.Message, 'TokenizeStream', 0, 0, etError);
  End
End;

(**

  This method finds the method declaration that precedes the current cursor
  position and returns the token index for the delcaration start or -1 if
  no declaration was found.

  @precon  iStreamPos is the stream position where the current cursor is and
           where to start the search backwards.
  @postcon Returns a token index for the first method declaration found.

  @param   iStreamPos as a TStreamPosition
  @param   recPosition as a TTokenPosition as a reference.
  @return  a TMethodDecl

**)
Function TPascalDocModule.FindMethodAtStreamPosition(
  iStreamPos : TStreamPosition; var recPosition : TTokenPosition) : TMethodDecl;

Const
  strMethods : Array[1..4] of String = ('constructor', 'destructor', 'function',
    'procedure');

Var
  i, j : Integer;
  iTokenIndex : Integer;
  MethodType : TMethodType;

Begin
  Result := Nil;
  iTokenIndex := -1;
  // Find token at cursor
  j := -1;
  For i := TokenCount - 1 DownTo 0 Do
    If TokenInfo[i].BufferPos <= iStreamPos Then
      Begin
        j := i;
        Break;
      End;
  // Find method before this point
  For i := j DownTo 0 Do
    If IsKeyWord(TokenInfo[i].Token, strMethods) Then
      Begin
        iTokenIndex := i;
        Break;
      End;
  If iTokenIndex = -1 Then
    Exit;
  SetTokenIndex(iTokenIndex);
  recPosition.Line := Token.Line;
  recPosition.Column := Token.Column;
  recPosition.BufferPos := Token.BufferPos;
  If (PrevToken <> Nil) And (PrevToken.UToken = 'CLASS') Then
    Begin
      recPosition.BufferPos := PrevToken.BufferPos;
      recPosition.Column := PrevToken.Column;
    End;
  If Token.UToken = 'CONSTRUCTOR' Then
    MethodType := mtConstructor
  Else If Token.UToken = 'DESTRUCTOR' Then
    MethodType := mtDestructor
  Else If Token.UToken = 'FUNCTION' Then
    MethodType := mtFunction
  Else
    MethodType := mtProcedure;
  Case MethodType Of
    mtConstructor: Result := ConstructorHeading(scPublic);
    mtDestructor: Result := DestructorHeading(scPublic);
    mtFunction: Result := FunctionHeading(scPublic);
    mtProcedure: Result := ProcedureHeading(scPublic);
  End;
End;

(**

  This method finds the property declaration that precedes the current cursor
  position and returns the token index for the delcaration start or -1 if
  no declaration was found.

  @precon  iStreamPos is the stream position where the current cursor is and
           where to start the search backwards.
  @postcon Returns a token index for the first property declaration found.

  @param   iStreamPos as a TStreamPosition
  @param   recPosition as a TTokenPosition as a Reference
  @return  a TClassDecl

**)
Function TPascalDocModule.FindPropertyAtStreamPosition(
  iStreamPos : TStreamPosition; var recPosition : TTokenPosition) : TClassDecl;

Var
  i, j : Integer;
  iTokenIndex : Integer;
  Scope : TScope;

Begin
  Result := Nil;
  iTokenIndex := -1;
  // Find token at cursor
  j := -1;
  For i := TokenCount - 1 DownTo 0 Do
    If TokenInfo[i].BufferPos <= iStreamPos Then
      Begin
        j := i;
        Break;
      End;
  // Find method before this point
  For i := j DownTo 0 Do
    If TokenInfo[i].UToken = 'PROPERTY' Then
      Begin
        iTokenIndex := i;
        Break;
      End;
  If iTokenIndex = -1 Then Exit;
  SetTokenIndex(iTokenIndex);
  recPosition.Line := Token.Line;
  recPosition.Column := Token.Column;
  recPosition.BufferPos := Token.BufferPos;
  Result := TClassDecl.Create('Temp', scPrivate, 0, 0);
  If Not (PropertyList(Result, Scope) And (Result.Properties.Count > 0)) Then
    FreeAndNil(Result);
End;


(**

  This method sorts the structures items in alphanumeric order.

  @precon  None.
  @postcon Sorts all the collection held be the this class.

**)
Procedure TPascalDocModule.Sort;

Begin
  If UsesCls <> Nil Then UsesCls.Sort;
  If Requires <> Nil Then Requires.Sort;
  If Contains <> Nil Then Contains.Sort;
  Types.Sort;
  Vars.Sort;
  ThreadVars.Sort;
  Constants.Sort;
  ResourceStrings.Sort;
  ExportedHeadings.Sort;
  ImplementedMethods.Sort;
  ExportsClause.Sort;
  ScopeImplementedMethods;
End;

(**

  This method looks up the scope of the class method in the classes and updates
  the implemented method with the scope of the classes method.

  @precon  None.
  @postcon Looks up the scope of the class method in the classes and updates
           the implemented method with the scope of the classes method.

**)
procedure TPascalDocModule.ScopeImplementedMethods;

Var
  i, j, k : Integer;

begin
  Types.RemoveForwardDecls;
  // Check implemented methods for declarations
  For i := 0 To ImplementedMethods.Count - 1 Do
    If ImplementedMethods[i].ClsName <> '' Then
      Begin
        j := Types.Find(ImplementedMethods[i].ClsName);
        If j <> - 1 Then
          Begin
            k := (Types[j] As TObjectDecl).Methods.Find('',
              ImplementedMethods[i].Identifier);
            If k <> -1 Then
              ImplementedMethods[i].Scope := (Types[j] As TObjectDecl).Methods[k].Scope;
          End Else
            Errors.Add(Format(strUndeclaredClassMethod,
              [ImplementedMethods[i].ClsName, ImplementedMethods[i].Identifier]),
              'ScopeImplementedMethods', ImplementedMethods[i].Line,
              ImplementedMethods[i].Line, etError);
      End Else
      Begin
        k := ExportedHeadings.Find('', ImplementedMethods[i].Identifier);
        If k <> -1 Then
          ImplementedMethods[i].Scope := ExportedHeadings[k].Scope;
      End;
  // Check class method for implementations.
  For i := 0 To Types.Count - 1 Do
    If Types[i] Is TObjectDecl Then
      If Not (Types[i] Is TInterfaceDecl) Then
        With Types[i] As TObjectDecl Do
          For j := 0 To Methods.Count -1 Do
            If ImplementedMethods.Find(Identifier, Methods[j].Identifier) = -1 Then
              If Not Methods[j].HasDirective('ABSTRACT') Then
                Errors.Add(Format(strUnsatisfiedForwardReference,
                  [Identifier, Methods[j].Identifier]), 'ScopeImplementedMethods',
                  Methods[j].Line, Methods[j].Col, etError);
end;

(**

  This method seeks the first non-comment token in the source code which match
  one of the passed tokens.

  @precon  The Tokens passed MUST be sorted in lowercase and in ascending order.
  @postcon Seeks the first non-comment token in the source code which match
           one of the passed tokens.

  @param   strMsg      as a String
  @param   strMethod   as a String
  @param   strExpected as a String
  @param   SeekTokens  as an Array Of string
  @param   SeekToken   as a TSeekToken

**)
Procedure TPascalDocModule.ErrorAndSeekToken(strMsg, strMethod, strExpected : String;
  SeekTokens: Array Of String; SeekToken : TSeekToken);

  (**

    This method counts the number of occurrances of "%s" in the string and
    returns that number.

    @precon  None.
    @postcon Returns the number of string parameters in the text.

    @param   strText as a String
    @return  an Integer

  **)
  Function StringCount(strText : String) : Integer;

  Var
    i : Integer;

  Begin
    Result := 0;
    For i := 1 To Length(strText) - 1 Do
      If Copy(strText, i, 2) = '%s' Then Inc(Result);
  End;

Begin
  Case StringCount(strMsg) Of
    1: Errors.Add(Format(strMsg, [strExpected, Token.Line, Token.Column]),
         strMethod, Token.Line, Token.Column, etError);
    2: Errors.Add(Format(strMsg, [strExpected, Token.Token, Token.Line,
         Token.Column]), strMethod, Token.Line, Token.Column, etError);
  Else
    Errors.Add('Not enough strings passed to ErrorAndSeekToken().', strMethod,
      Token.Line, Token.Column, etError);
  End;
  NextNonCommentToken;
  While Not IsKeyWord(Token.Token, SeekTokens) Do
    NextNonCommentToken;
  If SeekToken = stFirst Then
    NextNonCommentToken;
End;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TPascalDocModule.ParseTokens;
begin
  Goal;
end;

(**

  This method adds the current toen to the passed generic container if it is not
  nil and moves to the next non comment token.

  @precon  None.
  @postcon Adds the current toen to the passed generic container if it is not
           nil and moves to the next non comment token.

  @param   C as a TGenericContainer

**)
Procedure TPascalDocModule.AddToExpression(C : TGenericContainer);

Begin
  If C <> Nil Then
    C.Add(Token.Token);
  NextNonCommentToken;
End;

(**

  This method check the current token against the passed string and if true
  returns true and addeds the token to the generic container.

  @precon  None.
  @postcon Check the current token against the passed string and if true
           returns true and addeds the token to the generic container.

  @param   strToken as a String
  @param   C        as a TGenericContainer
  @return  a Boolean 

**)
Function TPascalDocModule.IsToken(strToken : String; C : TGenericContainer): Boolean;

Begin
  Result := strToken = Token.Token;
  If Result Then
    AddToExpression(C);
End;

(**

  This method is the starting position for the parsing of an object pascal
  module. It finds the first non comment token and begins the grammar checking
  from their by deligating to the program, library, unit and package methods.

  @grammar Goal -> ( Program | Package | Library | Unit )

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating to the program, library, unit and package
           methods.

**)
procedure TPascalDocModule.Goal;

Var
  boolHasProcessed : Boolean;

begin
  Try
    If TokenCount = 0 Then
      Exit;
    // Find first non comment token
    While (Token.TokenType In [ttComment, ttCompilerDirective]) And Not EndOfTokens Do
      NextNonCommentToken;
    // Check for end of file else must be identifier
    If Not EndOfTokens Then
      Begin
        ModuleComment := GetComment;
        boolHasProcessed := OPProgram;
        If Not boolHasProcessed  Then
          boolHasProcessed := OPLibrary;
        If Not boolHasProcessed  Then
          boolHasProcessed := OPPackage;
        If Not boolHasProcessed  Then
          boolHasProcessed := OPUnit;
        If Not boolHasProcessed  Then
          ErrorAndSeekToken(strModuleKeyWordNotfound, 'Goal', Token.Token,
            strSeekableOnErrorTokens, stActual);
      End Else
        Raise EDocException.Create(strUnExpectedEndOfFile);
  Except
    On E : EDocException Do
      Errors.Add(E.Message, E.ExceptionMethod, 0, 0, etError);
    On E : Exception Do
      Errors.Add(E.Message, 'Goal', 0, 0, etError);
  End;
end;

(**

  This method parses a Program declaration from the current token
  position using the following object pascal grammar.

  @grammar Program -> [ PROGRAM Ident [ '(' IdentList ')' ';' ]
                      ProgramBlock '.'

  @precon  None.
  @postcon Returns true is a program section was parsed.

  @return  a Boolean

**)
Function TPascalDocModule.OPProgram : Boolean;

begin
  Result := Token.UToken = 'PROGRAM';
  If Not Result Then
    Exit;
  ModuleType := mtProgram;
  NextNonCommentToken;
  If Token.TokenType = ttIdentifier Then
    Begin
      ModuleName := Token.Token;
      ModuleNameLine := Token.Line;
      ModuleNameCol := Token.Column;
      NextNonCommentToken;
      // In the Program module we need to check for '(' Ident List ')' but discard
      If Token.Token = '(' Then
        Begin
          NextNonCommentToken;
          IdentList(True, strSeekableOnErrorTokens); // get ident list
          // Check for closing parenthesis
          If Token.Token <> ')' Then
            ErrorAndSeekToken(strLiteralExpected, 'OPProgram', ')',
              strSeekableOnErrorTokens, stActual)
          Else
            NextNonCommentToken;
        End;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPProgram', ';',
          strSeekableOnErrorTokens, stActual)
      Else
        NextNonCommentToken;
      ProgramBlock;
      // Check for '.'
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPProgram', '.',
          strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strIdentExpected, 'OPProgram', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses a unit declaration from the current token position using
  the following object pascal grammar.

  @grammar Unit -> UNIT Ident ';'
                   InterfaceSection
                   ImplementationSection
                   InitSection '.'

  @precon  None.
  @postcon Returns true if a unit section was parsed.

  @return  a Boolean

**)
Function TPascalDocModule.OPUnit : Boolean;

Begin
  Result := Token.UToken = 'UNIT';
  If Result Then
    Begin
      ModuleType := mtUnit;
      NextNonCommentToken;
      If Token.TokenType <> ttIdentifier Then
        ErrorAndSeekToken(strIdentExpected, 'OPUnit', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        Begin;
          ModuleName := Token.Token;
          ModuleNameLine := Token.Line;
          ModuleNameCol := Token.Column;
          NextNonCommentToken;
        End;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPUnit', ';',
          strSeekableOnErrorTokens, stActual)
      Else
        NextNonCommentToken;
      InterfaceSection;
      ImplementationSection;
      InitSection;
      // Check for '.'
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPUnit', '.',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a package declaration from the current token position
  using the following object pascal grammar.

  @grammar Package -> PACKAGE Ident ';'
                      [ RequiresClause ]
                      [ ContainsClause ]
                      END '.'

  @precon  None.
  @postcon Returns true is a package section was parsed.

  @return  a Boolean

**)
Function TPascalDocModule.OPPackage : Boolean;

begin
  Result := Token.UToken = 'PACKAGE';
  If Result Then
    Begin;
      ModuleType := mtPackage;
      NextNonCommentToken;
      If Token.TokenType <> ttIdentifier Then
        ErrorAndSeekToken(strIdentExpected, 'OPPackage', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        Begin
          ModuleName := Token.Token;
          ModuleNameLine := Token.Line;
          ModuleNameCol := Token.Column;
          NextNonCommentToken;
        End;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPPackage', ';',
          strSeekableOnErrorTokens, stActual)
      Else
        NextNonCommentToken;
      // Look for requires and contains clauses
      RequiresClause;
      ContainsClause;
      If Token.UToken <> 'END' Then
        ErrorAndSeekToken(strReservedWordExpected, 'OPPackage', 'END',
          strSeekableOnErrorTokens, stActual)
      Else
        NextNonCommentToken;
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPPackage', '.',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the Library declaration from the current token using the
  following object pascal grammar.

  @grammar Library -> LIBRARY Ident ';'
                      ProgramBlock '.'

  @precon  None.
  @postcon Returns true is a library section was parsed.

  @return  a Boolean

**)
Function TPascalDocModule.OPLibrary : Boolean;

begin
  Result := Token.UToken = 'LIBRARY';
  If Result Then
    Begin
      ModuleType := mtLibrary;
      NextNonCommentToken;
      If Token.TokenType <> ttIdentifier Then
        ErrorAndSeekToken(strIdentExpected, 'OPLibrary', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        Begin
          ModuleName := Token.Token;
          ModuleNameLine := Token.Line;
          ModuleNameCol := Token.Column;
          NextNonCommentToken;
        End;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPLibrary', ';',
          strSeekableOnErrorTokens, stActual)
      Else
        NextNonCommentToken;
      ProgramBlock;
      // Check for '.'
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPLibrary', '.',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses a program block from the current token position using
  the following object pascal grammar.

  @grammar ProgramBlock -> [ UsesClause ]
                           Block

  @precon  None.
  @postcon Parses a program block from the current token position using the
           following object pascal grammar.

**)
procedure TPascalDocModule.ProgramBlock;
begin
  UsesClause;
  Block(scPublic, Nil);
end;

(**

  This method parses the Uses clause declaration from the current token
  position using the following object pascal grammar.

  @grammar Uses -> USES IdentList ';'

  @precon  None.
  @postcon Parses the Uses clause declaration from the current token position
           using the following object pascal grammar.

**)
Procedure TPascalDocModule.UsesClause;

Var
  Comment : TComment;

Begin
  If Token.UToken = 'USES' Then
    Begin
      Comment := GetComment;
      NextNonCommentToken;
      If UsesCls = Nil Then
        Begin
          UsesCls := IdentList(True, strSeekableOnErrorTokens);
          UsesCls.Comment := Comment;
        End Else
        Begin
          UsesCls.Assign(IdentList(True, strSeekableOnErrorTokens));
          If UsesCls.Comment <> Nil Then
            UsesCls.Comment.Assign(Comment)
          Else
            UsesCls.Comment := Comment;
        End;
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'UsesClause', ';',
          strSeekableOnErrorTokens, stActual)
      Else
        NextNonCommentToken;
    End;
End;

(**

  This method attempts to parse the current token position as a Portability
  directive.

  @grammar PortabilityDirective -> platform
                                -> deprecated
                                -> library

  @precon  None.
  @postcon Attempts to parse the current token position as a Portability
           directive.

**)
Procedure TPascalDocModule.PortabilityDirective;

Begin
  If IsKeyWord(Token.Token, strPortabilityDirective) Then
    NextNonCommentToken; //: @todo Add to symbol.
End;

(**

  This method parses an interface section from the current token position using
  the following object pascal grammar.

  @grammar  InterfaceClause -> INTERFACE
                               [ UsesClause ]
                               [ InterfaceDecl ] ...

  @precon  None.
  @postcon Parses an interface section from the current token position using
           the following object pascal grammar.

**)
Procedure TPascalDocModule.InterfaceSection;

Begin
  If Token.UToken <> 'INTERFACE' Then
    ErrorAndSeekToken(strReservedWordExpected, 'InterfaceSection', 'INTERFACE',
      strSeekableOnErrorTokens, stActual)
  Else
    NextNonCommentToken;
  UsesClause;
  InterfaceDecl;
End;

(**

  This method parses an interface declaration from the current token position
  using the following object pascal grammar.

  @grammar InterfaceDecl -> ConstSection
                            ResStringSection
                            TypeSection
                            VarSection
                            ThreadvarSection
                            ExportedHeading
                            ExportedProcs

  @precon  None.
  @postcon Parses an interface declaration from the current token position
           using the following object pascal grammar.

**)
Procedure TPascalDocModule.InterfaceDecl;

Begin
  Repeat
    {Loop doing nothing};
  Until Not (
    ConstSection(scPublic, Nil) Or
    ResStringSection(scPublic, Nil) Or
    TypeSection(scPublic, Nil) Or
    VarSection(scPublic, Nil) Or
    ThreadVarSection(scPublic) Or
    ExportedHeading Or
    ExportedStmt Or
    UndefinedToken
  );
End;

(**

  This method parses a exported heading declaration section from the current
  token position using the following object pascal grammar.

  @grammar ExportedHeading -> ProcedureHeading ';' [ Directive ]
                              FunctionHeading ';' [ Directive ]

  @precon  None.
  @postcon This method returns true if the current section was found to be
           an exported heading section.
  @return  a Boolean

**)
Function TPascalDocModule.ExportedHeading : Boolean;

Var
  M : TMethodDecl;

Begin
  Result := False;
  Repeat
    M := ProcedureHeading(scPublic);
    If M = Nil Then
      M := FunctionHeading(scPublic);
    If M <> Nil Then
      Begin
        Result := True;
        If Token.Token = ';' Then
          Begin
            NextNonCommentToken;
            Directive(M);
            ExportedHeadings.Add(M);
          End Else
            ErrorAndSeekToken(strLiteralExpected, 'ExportedHeading', ';',
              strSeekableOnErrorTokens, stFirst);
      End;
  Until M = Nil;
End;

(**

  This method parses an implementation section from the current token position
  using the following object pascal grammar.

  @grammar ImplementationSection -> IMPLEMENTATION
                                    [ UsesClause ]
                                    [ DeclSection ] ...

  @precon  None.
  @postcon Parses an implementation section from the current token position.

**)
Procedure TPascalDocModule.ImplementationSection;

Begin
  If Token.UToken <> 'IMPLEMENTATION' Then
    ErrorAndSeekToken(strReservedWordExpected, 'ImplementationSection',
      'IMPLEMENTATION', strSeekableOnErrorTokens, stActual)
  Else
    NextNonCommentToken;
  UsesClause;
  DeclSection(scPrivate, Nil);
End;

(**

  This method parses a block section from the current token position using the
  following object pascal grammar.

  @grammar Block -> [ DeclSection ]
                    CompoundStmt

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon Parses a block section from the current token position

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl

**)
procedure TPascalDocModule.Block(Scope : TScope; Method : TMethodDecl);

Var
  bool : Boolean;

Begin
  DeclSection(Scope, Method);
  bool := CompoundStmt;
  If Not bool Then
    AssemblerStatement;
End;

(**

  This method parses an exported procedure section from the current token
  position.

  @grammar ExportedStmt -> EXPORTS ExportsItem [, ExportsItem]...

  @precon  none.
  @postcon Returns true if an exported procedure was found.

  @return  a Boolean

**)
Function TPascalDocModule.ExportedStmt : Boolean;

Begin
  Result := Token.UToken = 'EXPORTS';
  If Result Then
    Begin
      NextNonCommentToken;
      Repeat
        ExportsItem;
      Until Not IsToken(',', Nil);
      If Token.Token = ';' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'ExportedStmt', ';',
          strSeekableOnErrorTokens, stFirst);
    End;
End;

(**

  This method parses an exports entry from the current token position using the
  following object pascal grammar.

  @grammar ExportsEntry -> Ident [ INDEX IntegerConstant [ NAME StringConstant ]
             [ RESIDENT ] ]</TD>

  @precon  None.
  @postcon Parses an exports entry from the current token position.

**)
Procedure TPascalDocModule.ExportsItem;

Var
  E : TGenericContainer;

Begin
  If (Token.TokenType In [ttIdentifier]) Then
    Begin
      E := TGenericContainer.Create(Token.Token, scPublic, Token.Line, Token.Column);
      ExportsClause.Add(E);
      E.Comment := GetComment;
      NextNonCommentToken;
      // Check INDEX
      If Token.UToken = 'INDEX' Then
        Begin
          E.Add(Token.Token);
          NextNonCommentToken;
          E.Add(Token.Token);
          NextNonCommentToken;
        End;
      // Check NAME
      If Token.UToken = 'NAME' Then
        Begin
          E.Add(Token.Token);
          NextNonCommentToken;
          E.Add(Token.Token);
          NextNonCommentToken;
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'ExportsItem', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a declaration section from the current token position using
  the following object pascal grammar.

  @grammar DeclSection -> LabelDeclSection
                       -> ConstSection
                       -> ResStringSection
                       -> TypeSection
                       -> VarSection
                       -> ThreadVarSection
                       -> ProcedureDeclSection
                       -> ExportedProcs

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon Parses a declaration section from the current token position.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl

**)
Procedure TPascalDocModule.DeclSection(Scope : TScope; Method : TMethodDecl);

Begin
  Repeat
    {Do nothing}
  Until Not (
    LabelDeclSection Or
    ConstSection(Scope, Method) Or
    ResStringSection(Scope, Nil) Or
    TypeSection(Scope, Method) Or
    VarSection(Scope, Method) Or
    ThreadVarSection(Scope) Or
    ProcedureDeclSection(Scope, Method) Or
    ExportedStmt Or
    UndefinedToken
  );
End;

(**

  This method parses a label declaration section from the current token
  position using the following object pascal grammar.

  @grammar LabelDeclSection -> LABEL LabelId

  @precon  None.
  @postcon This method dicards the labels found and returns True if this method
           handles a label declaration section.
  @return  a Boolean

**)
Function TPascalDocModule.LabelDeclSection : Boolean;

Begin
  Result := Token.UToken = 'LABEL';
  If Result Then
    Begin
      NextNonCommentToken;
      // We will ignore labels but treat them as IdentLists
      IdentList(True, strSeekableOnErrorTokens);
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'LabelDeclSection', ';',
          strSeekableOnErrorTokens, stFirst)
      Else
        NextNonCommentToken;
    End;
End;

(**

  This method parses a constant section declaration from the current token
  position using the following object pascal grammar.

  @grammar ConstSection -> CONST ( ConstantDecl ';' ) ...

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.ConstSection(Scope : TScope; Method : TMethodDecl) : Boolean;

Var
  Com : TComment;
  C : TGenericContainerCollection;

Begin
  Result := Token.UToken = 'CONST';
  If Result Then
    Begin
      // Get const comment
      C := Constants;
      If Method <> Nil Then
        C := Method.Consts;
      If C.Comment = Nil Then
        C.Comment := GetComment
      Else
        Begin
          Com := GetComment; // Added to the ownedlist for disposal
          C.Comment.Assign(Com);
        End;
      NextNonCommentToken;
      While ConstantDecl(Scope, Method) Do
        If Token.Token = ';' Then
          NextNonCommentToken
        Else
          ErrorAndSeekToken(strLiteralExpected, 'ConstSection', ';',
            strSeekableOnErrorTokens, stFirst);
    End;
End;

(**

  This method parses a constant declaration from the current token position
  using the following object pascal grammar.

  @grammar ConstantDecl -> Ident '=' ConstExpr
                        -> Ident ':' TypeId '=' TypedConstant

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.ConstantDecl(Scope : TScope; Method : TMethodDecl) : Boolean;

Var
  C : TGenericContainer;
  T : TTypeDecl;
  ExprType : TExprTypes;

Begin
  Result := False;
  // If not identifier then there is a new section
  If Token.TokenType = ttIdentifier Then
    Begin
      // Create constant and add to the collection, then get comment
      C := TConstant.Create(Token.Token, Scope, Token.Line, Token.Column);
      If Method = Nil Then
        Constants.Add(C)
      Else
        Method.Consts.Add(C);
      Result := True;
      C.Comment := GetComment;
      NextNonCommentToken;
      If Token.Token = '=' Then        // ConstExpr
        Begin
          C.Add(Token.Token);
          NextNonCommentToken;
          ExprType := [etUnknown, etConstExpr];
          ConstExpr(C, ExprType)
        End
      Else If Token.Token = ':' Then   // TypedConstant
        Begin
          C.Add(':');
          NextNonCommentToken;
          T := GetTypeDecl;
          If T <> Nil Then
            C.Append(T);
          If Token.Token = '=' Then
            Begin
              C.Add('=');
              NextNonCommentToken;
              TypedConstant(C, T);
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'ConstantDecl', '=',
                strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ConstantDecl', '= or :',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a resource string declaration section from the current
  token position.

  @grammar ConstSection -> RESOURCESTRING ( ResourceStringDecl ';' ) ...
           Also see {@link TPascalDocModule.ConstantSection}.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.ResStringSection(Scope : TScope;
  Method : TMethodDecl) : Boolean;

Var
  C : TComment;
  R : TGenericContainerCollection;

Begin
  Result := Token.UToken = 'RESOURCESTRING';
  If Not Result Then
    Exit;
  R := ResourceStrings;
  If Method <> Nil Then
    R := Method.ResStrings;
  If R.Comment = Nil Then
    R.Comment := GetComment
  Else
    Begin
      C := GetComment;
      R.Comment.Assign(C);
    End;
  NextNonCommentToken;
  Repeat
    {Loop do nothing}
  Until Not ResourceStringDecl(Scope, Method);
End;

(**

  This method parses a resource string declaration section from the current
  token position.

  @grammar ConstantDecl -> Ident '=' ConstExpr
           Also see {@link TPascalDocModule.VarDecl}.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.ResourceStringDecl(Scope : TScope;
  Method : TMethodDecl) : Boolean;

Var
  C : TGenericContainer;
  ExprType : TExprTypes;

Begin
  Result := False;
  ExprType := [etConstExpr, etString];
  // If not identifier then there is a new section
  If Token.TokenType = ttIdentifier Then
    Begin
      // Create constant and add to the collection, then get comment
      C := TResourceString.Create(Token.Token, Scope ,Token.Line, Token.Column);
      If Method = Nil Then
        ResourceStrings.Add(C)
      Else
        Method.ResStrings.Add(C);
      Result := True;
      C.Comment := GetComment;
      NextNonCommentToken;
      If Token.Token = '=' then
        Begin
          C.Add(Token.Token);
          NextNonCommentToken;
          ConstExpr(C, ExprType);
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ResourceStringDecl', '=',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a type section from the current token position using the
  following object pascal grammar.

  @grammar Typesection -> TYPE ( TypeDecl ';' ) ...

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.TypeSection(Scope : TScope; Method : TMethodDecl) : Boolean;

Var
  C : TComment;
  T : TGenericContainerCollection;

Begin
  Result := Token.UToken = 'TYPE';
  If Result Then
    Begin
      C := GetComment;
      If C <> Nil Then
        Begin
          T := Types;
          If Method <> Nil Then
            T := Method.Types;
          If T.Comment = Nil Then
            T.Comment := C
          Else
            T.Comment.Assign(C);
        End;
      NextNonCommentToken;
      While TypeDecl(Scope, Method) Do
        If Token.Token = ';' Then
          NextNonCommentToken
        Else
          ErrorAndSeekToken(strLiteralExpected, 'TypeSection', ';',
            strSeekableOnErrorTokens, stFirst);
    End;
End;

(**

  This method parses a type declaration section from the current token position
  using the following object pascal grammar.

  @grammar TypeDecl -> Ident '=' [TYPE] Type
                    -> Ident '=' [TYPE] RestrictedType

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.TypeDecl(Scope : TScope;
  Method : TMethodDecl) : Boolean;

Var
  Ident : TIdentInfo;
  Ty : TGenericContainer;
  C : TComment;

Begin
  Result := False;
  If Token.TokenType = ttIdentifier Then
    Begin
      C := GetComment; // Added to the ownedlist for disposal
      Ident.Ident := Token.Token;
      Ident.Line := Token.Line;
      Ident.Col := Token.Column;
      Ident.Scope := Scope;
      Ident.Method := Method;
      Result := True;
      NextNonCommentToken;
      If Token.Token = '=' Then
        Begin
          NextNonCommentToken;
          If Token.UToken = 'TYPE' Then
            NextNonCommentToken;
          Ty := GetTypeDecl;
          If Ty <> Nil Then
            Begin
              Ty.Insert('=', 0);
              Ty.Identifier := Ident.Ident;
              Ty.Scope := Ident.Scope;
              Ty.Line := Ident.Line;
              Ty.Col := Ident.Col;
              Ty.Comment := C;
              If Method <> Nil Then
                Method.Types.Add(Ty)
              Else
                Types.Add(Ty);
            End Else
              ErrorAndSeekToken(strTypeNotFound, 'TypeDecl', Token.Token,
                strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'TypeDecl', '=',
            strSeekableOnErrorTokens, stActual)
    End;
End;

(**

  This method attempt to get a type declaration from the current token
  position.

  @precon  None.
  @postcon If a type is found it is returned as the result else nil.

  @return  a TTypeDecl

**)
Function TPascalDocModule.GetTypeDecl : TTypeDecl;

Begin
  Result := RestrictedType;
  If Result = Nil Then
    Result := OPType;
End;

(**

  This method parses a typed constant from the current token position using
  the following object pascal grammar.

  @grammar TypedConstant -> ( ConstExpr | ArrayConstant | RecordConstant )

  @precon  C is a valid instance of the constant to be populated with tokens.
  @postcon Returns false if this was not a typed constant an not handled.

  @param   C as a TGenericContainer
  @param   T as a TTypeDecl
  @return  a Boolean

**)
Function TPascalDocModule.TypedConstant(C : TGenericContainer;
  T : TTypeDecl) : Boolean;

Var
  ExprType : TExprTypes;

Begin
  ExprType := [etUnknown, etConstExpr];
  Result := ArrayConstant(C, T) Or RecordConstant(C, T) Or ConstExpr(C, ExprType);
End;

(**

  This method test whether the typed constant is an Array Constant (starts with
  ARRAY.

  @grammar ArrayConstant -> '(' TypedConstant ','... ')'

  @precon  C must be a valid generic container.
  @postcon If ARRAY is found processes the constant as an array constant.

  @param  C as a TGenericContainer
  @param   T as a TTypeDecl
  @return a Boolean

**)
Function TPascalDocModule.ArrayConstant(C : TGenericContainer;
  T : TTypeDecl) : Boolean;

Begin
  Result := T Is TArrayType;
  If Result Then
    ArrayElement(C, 1, T As TArrayType);
end;

(**

  This method parses the current token position are an element of an array
  constant.

  @precon  C must be a valid generic container and AT must be an instance of the
           array type associated with the array constant.
  @postcon Parses the current token position are an element of an array
           constant.

  @param   C               as a TGenericContainer
  @param   iStartDimension as an Integer
  @param   AT              as a TArrayType

**)
Procedure TPascalDocModule.ArrayElement(C : TGenericContainer;
  iStartDimension : Integer; AT : TArrayType);
Begin
  If iStartDimension <= AT.Dimensions Then
    If Token.Token = '(' Then
      Begin
        AddToExpression(C);
        Repeat
          If iStartDimension < AT.Dimensions Then
            ArrayElement(C, iStartDimension + 1, AT)
          Else
            TypedConstant(C, Nil)
        Until Not IsToken(',', C);
        If Token.Token = ')' Then
          AddToExpression(C)
        Else
          ErrorAndSeekToken(strLiteralExpected, 'ProcessArrayDimension', ')',
            strSeekableOnErrorTokens, stActual);
      End Else
        ErrorAndSeekToken(strLiteralExpected, 'ProcessArrayDimension', '(',
          strSeekableOnErrorTokens, stActual);
End;

(**

  This method attempts to parser the current token position as an RecordConstant.

  @grammar RecordConstant -> '(' RecordFieldConstant ';'... ')'

  @precon  C must be a valid generic container.
  @postcon Attempts to parser the current token position as an RecordConstant.

  @param   C as a TGenericContainer
  @param   T as a TTypeDecl
  @return  a Boolean

**)
Function TPascalDocModule.RecordConstant(C : TGenericContainer;
  T : TTypeDecl) : Boolean;

Begin
  Result := Token.Token = '(';
  If Result Then
    Begin
      AddToExpression(C);
      Repeat
        RecordFieldConstant(C, T);
      Until Not IsToken(';', C);
      If Token.Token = ')' Then
        AddToExpression(C)
      Else
        ErrorAndSeekToken(strLiteralExpected, 'RecordConstant', ')',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a record field
  constant.

  @grammar RecordFieldConstant -> Ident ':' TypedConstant

  @precon  C must be a valid generic container.
  @postcon Attempts to parse the current token position as a record field
           constant.

  @param   C as a TGenericContainer
  @param   T as a TTypeDecl
  @return  a Boolean

**)
Function TPascalDocModule.RecordFieldConstant(C : TGenericContainer;
  T : TTypeDecl) : Boolean;

Begin
  Result := False;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
    Begin
      NextNonCommentToken;
      If Token.Token = ':' Then
        Begin
          Result := True;
          RollBackToken;
          AddToExpression(C);
          AddToExpression(C);
          TypedConstant(C, T)
        End Else
          RollBackToken;
    End;
End;

(**

  This method parses a type from the current token position using the following
  object pascal grammar.

  @grammar Type -> TypeId
                -> SimpleType
                -> StrucType
                -> PointerType
                -> StringType
                -> ProcedureType
                -> VariantType
                -> ClassRefType

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypeDecl

**)
Function TPascalDocModule.OPType : TTypeDecl;

Begin
  Result := StrucType;
  If Result = Nil Then
    Result := PointerType;
  If Result = Nil Then
    Result := StringType;
  If Result = Nil Then
    Result := ProcedureType;
  If Result = Nil Then
    Result := VariantType;
  If Result = Nil Then
    Result := ClassRefType;
  If Result = Nil Then
    Result := SimpleType;
End;

(**

  This method parses a restricted type from the current token position using
  the following object pascal grammar.

  @grammar RestrictedType -> ObjectType
                          -> ClassType
                          -> InterfaceType

  @note    The simpleType() method is here to act as a catch all for types that
           have note been previously handled.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TRestrictedType

**)
Function TPascalDocModule.RestrictedType : TRestrictedType;

Begin
  Result := ObjectType;
  If Result = Nil Then
    Result := ClassType;
  If Result = Nil Then
    Result := InterfaceType;
End;

(**

  This method parses a class reference type declaration from the current token
  position using the following object pascal grammar.

  @grammar ClassRefType -> CLASS OF TypeId

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TClassRefType

**)
Function TPascalDocModule.ClassRefType : TClassRefType;

Begin
  Result := Nil;
  If Not (Token.UToken = 'CLASS') Then
    Exit;
  NextNonCommentToken;
  If Token.UToken <> 'OF' Then
    Begin
      RollBackToken;
      Exit;
    End;
  NextNonCommentToken;
  Result := TClassRefType.Create;
  SymbolTable.Add(Result);
  Result.Add('Class');
  Result.Add('Of');
  If Not TypeId(Result) Then
    ErrorAndSeekToken(strTypeIdExpected, 'PointerType', '',
      strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a simple type declaration from the current token
  position using the following object pascal grammar.

  @grammar SimpleType -> ( OrdinalType | RealType )

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TSimpleType

**)
function TPascalDocModule.SimpleType : TSimpleType;

begin
  Result := RealType;
  If Result = Nil Then
    Result := OrdinalType;
end;

(**

  This method determines if the token represents a real type using the following
  object pascal grammar.

  @grammar RealType -> REAL48
                    -> REAL
                    -> SINGLE
                    -> DOUBLE
                    -> EXTENDED
                    -> CURRENCY
                    -> COMP

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TRealType

**)
Function TPascalDocModule.RealType : TRealType;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strRealTypes) Then
    Exit;
  Result := TRealType.Create;
  SymbolTable.Add(Result);
  Result.Add(Token.Token);
  NextNonCommentToken;
End;

(**

  This method determines if the type is an ordinal type using the folowing
  object pascal grammar.

  @grammar OrdinalType -> ( SubrangeType | EnumerateType | OrdIndent )

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TOrdinalType

**)
Function TPascalDocModule.OrdinalType : TOrdinalType;

Begin
  Result := OrdIdent;
  If Result = Nil Then
    Result := EnumerateType;
  If Result = Nil Then
    Result := SubRangeType;
End;

(**

  This method determines if the current token is an ordinal ident using the
  following object pascal grammar.

  @grammar OrdIndent -> SHORTINT
                     -> SMALLINT
                     -> INTEGER
                     -> BYTE
                     -> LONGINT
                     -> INT64
                     -> WORD
                     -> BOOLEAN
                     -> CHAR
                     -> WIDECHAR
                     -> LONGWORD
                     -> PCHAR

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TOrdIdent

**)
Function TPascalDocModule.OrdIdent : TOrdIdent;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strOrdIdents) Then
    Exit;
  Result := TOrdIdent.Create;
  SymbolTable.Add(Result);
  Result.Add(Token.Token);
  NextNonCommentToken;
End;

(**

  This method parses a variant type declaration section using the following
  object pascal grammar.

  @grammar VariantType -> VARIANT
                       -> OLEVARIANT

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TVariantType

**)
Function TPascalDocModule.VariantType : TVariantType;

begin
  Result := Nil;
  If Not IsKeyWord(Token.UToken, strVariants) Then
    Exit;
  Result := TVariantType.Create;
  SymbolTable.Add(Result);
  Result.Add('=');
  Result.Add(Token.Token);
  NextNonCommentToken;
end;

(**

  This method parses  a sub range type from the current token position using
  the following object pascal grammar. This method also currently acts as a type
  CATCH ALL if nothing else works.

  @grammar SubrangeType -> ConstExpr .. ConstExpr

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @return  a TSubRangeType

**)
Function TPascalDocModule.SubRangeType : TSubRangeType;

Var
  ExprType : TExprTypes;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strReservedWords) Then
    Begin
      Result := TSubRangeType.Create;
      SymbolTable.Add(Result);
      ExprType := [etUnknown, etConstExpr];
      ConstExpr(Result, ExprType);
      If Token.Token = '..' Then // Handle simple expressions
        Begin
          AddToExpression(Result);
          ConstExpr(Result, ExprType);
        End;
    End;
End;

(**

  This method parses an enumerate type from the current token position using
  the following object pascal grammar.

  @grammar EnumeratedType -> '(' EnumerateTypeElement, ... ')'

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @return  a TEnumerateType

**)
Function TPascalDocModule.EnumerateType : TEnumerateType;

Begin
  Result := Nil;
  If Token.Token = '(' Then
    Begin
      Result := TEnumerateType.Create;
      SymbolTable.Add(Result);
      AddToExpression(Result);
      Repeat
        EnumerateElement(Result);
      Until Not IsToken(',', Result);
      If Token.Token = ')' Then
        AddToExpression(Result)
      Else
        ErrorAndSeekToken(strLiteralExpected, 'EnumerateType', ')',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses the current token position as an Enumerate Element.

  @grammar EnumerateTypeElement -> Ident [ ‘=’ ConstExpr ]

  @precon  None.
  @postcon Parses the current token position as an Enumerate Element.

  @param   EnumerateType as a TEnumerateType

**)
Procedure TPascalDocModule.EnumerateElement(EnumerateType : TEnumerateType);

Var
  ExprType : TExprTypes;

Begin
  If Token.TokenType = ttIdentifier Then
    Begin
      AddToExpression(EnumerateType);
      If Token.Token = '=' Then
        Begin
          AddToExpression(EnumerateType);
          ExprType := [etUnknown, etConstExpr];
          ConstExpr(EnumerateType, ExprType);
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'EnumerateElement', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a sring type declaration from the current token position
  using the following object pascal grammar.

  @grammar StringType -> STRING
                      -> ANSISTRING
                      -> WIDESTRING
                      -> STRING '[' ConstExpr ']'

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TStringType

**)
Function TPascalDocModule.StringType : TStringType;

Var
  ExprType : TExprTypes;

begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strStrings) Then
    Exit;
  Result := TStringType.Create;
  SymbolTable.Add(Result);
  Result.Add(Token.Token);
  NextNonCommentToken;
  // Check for '[' ConstExpr ']'
  If Token.Token = '[' Then
    Begin
      Result.Add('[');
      NextNonCommentToken;
      ExprType := [etNumeric, etConstExpr];
      ConstExpr(Result, ExprType);
      If Token.Token = ']' Then
        Begin
          Result.Add(Token.Token);
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'StringType', ']',
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses an Array, Set of File type declaration from the current
  token position using the following object pascal grammar.

  @grammar StrucType -> [ PACKED ] ( ArrayType | SetType | FileType | RecType )

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypeDecl

**)
Function TPascalDocModule.StrucType : TTypeDecl;

Var
  boolPacked : Boolean;

begin
  boolPacked := False;
  If Token.UToken = 'PACKED' Then
    Begin
      boolPacked := True;
      NextNonCommentToken;
    End;
  Result := ArrayType(boolPacked);
  If Result = Nil Then
    Result := SetType(boolPacked);
  If Result = Nil Then
    Result := FileType(boolPacked);
  If Result = Nil Then
    Result := RecType(boolPacked);
end;

(**

  This method parses an array type declaration from the current token position
  using the following object pascal grammar.

  @grammar ArrayType -> ARRAY [ '[' OrdinalType / ',' ... ']' ] OF Type
  
  @precon  boolPacked determines if the array type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @return  a TArrayType

**)
Function TPascalDocModule.ArrayType(boolPacked : Boolean) : TArrayType;

Var
  E : TOrdinalType;
  T : TTypeDecl;

Begin
  Result := Nil;
  If Token.UToken = 'ARRAY' Then
    Begin
      Result := TArrayType.Create;
      SymbolTable.Add(Result);
      If boolPacked Then
        Result.Add('Packed');
      Result.Add(Token.Token);
      NextNonCommentToken;
      If Token.Token = '[' Then
        Begin
          Repeat
            AddToExpression(Result);
            Result.AddDimension;
            E := OrdinalType;
            If E <> Nil Then
              Result.Append(E);
          Until Not IsToken(',', Result);
          If Token.Token = ']' Then
            AddToExpression(Result)
          Else
            ErrorAndSeekToken(strLiteralExpected, 'ArrayType', ']',
              strSeekableOnErrorTokens, stActual);
        End;
      If Token.UToken = 'OF' Then
        Begin
          Result.Add(Token.Token);
          NextNonCommentToken;
          T := GetTypeDecl;
          If T <> Nil Then
            Result.Add(T.AsString(False)); //: @todo Remove '=' and update recorddecl.asstring()
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'ArrayType', 'OF',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  Method parses a set type declaration from the current token position using
  following object pascal grammar.

  @grammar SetType -> SET OF OrdinalType

  @precon  boolPacked determines if the set type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @return  a TSetType

**)
Function TPascalDocModule.SetType(boolPacked : Boolean) : TSetType;

Begin
  Result := Nil;
  If Not (Token.UToken = 'SET') Then Exit;
  Result := TSetType.Create;
  SymbolTable.Add(Result);
  If boolPacked Then
    Result.Add('Packed');
  Result.Add(Token.Token);
  NextNonCommentToken;
  If Token.UToken = 'OF' Then
    Begin
      Result.Add(Token.Token);
      NextNonCommentToken;
      While Not (Token.Token[1] In [';', '=']) Do
        Begin
          Result.Add(Token.Token);
          NextNonCommentToken;
        End;
    End Else
      ErrorAndSeekToken(strReservedWordExpected, 'SetType', 'OF',
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a file type declaration from the current token position
  using the following object pascal grammar.

  @grammar FileType -> FILE OF Type

  @precon  boolPacked determines if the file type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @return  a TFileType

**)
Function TPascalDocModule.FileType(boolPacked : Boolean) : TFileType;

Begin
  Result := Nil;
  If Not (Token.UToken = 'FILE') Then Exit;
  NextNonCommentToken;
  If Token.UToken <> 'OF' Then
    Begin
      RollBackToken;
      Exit;
    End;
  Result := TFileType.Create;
  SymbolTable.Add(Result);
  If boolPacked Then
    Result.Add('Packed');
  Result.Add('File');
  Result.Add(Token.Token);
  NextNonCommentToken;
  While Token.Token <> ';' Do
    Begin
      Result.Add(Token.Token);
      NextNonCommentToken;
    End;
End;

(**

  This method parses a record type declaration from the current token position.

  @grammar RecType -> RECORD [ FieldList ] END

  @precon  boolPacked detmerines if the record is packed for not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @return  a TRecordDecl

**)
Function TPascalDocModule.RecType(boolPacked : Boolean): TRecordDecl;

begin
  Result := Nil;
  If Not (Token.UToken = 'RECORD') Then Exit;
  Result := TRecordDecl.Create;
  SymbolTable.Add(Result);
  Result.IsPacked := boolPacked;
  NextNonCommentToken;
  FieldList(Result);
  If Token.UToken = 'END' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strReservedWordExpected, 'RecType', 'END',
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses a field list for classes, records and object declarations
  from the current token position.

  @grammar FieldList -> FieldDecl / ';' ... [ VariantSection ] [ ';' ]

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses a field list for classes, records and object declarations
           from the current token position.

  @param   Rec as a TRecordDecl

**)
Procedure TPascalDocModule.FieldList(Rec: TRecordDecl);

begin
  While (Token.UToken <> 'END') And (Token.Token <> ')') Do
    Begin
      If VariantSection(Rec) Then Exit;
      FieldDecl(Rec);
      // Check for ';'
      If Token.Token = ';' Then NextNonCommentToken;
    End;
end;

(**

  This method parses a records field declarations from the current token
  position using the following object pascal grammar.

  @grammar FieldDecl -> IdentList ':' Type

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses a records field declarations from the current token position

  @param   Rec as a TRecordDecl

**)
Procedure TPascalDocModule.FieldDecl(Rec: TRecordDecl);

Var
  I : TIdentList;
  j : Integer;
  P : TParameter;
  T : TTypeDecl;

Begin
  I := IdentList(False, strSeekableOnErrorTokens);
  Try
    If Token.Token = ':' Then
      Begin
        NextNonCommentToken;
        T := GetTypeDecl;
        // Create record fields
        For j := 0 To I.Count - 1 Do
          Begin
            P :=  TParameter.Create(pmNone, I[j].Ident, False, T, '',
              scPublic, I[j].Line, I[j].Col);
            P.Comment := I[j].Comment;
            Rec.AddParameter(P);
          End;
      End Else
        ErrorAndSeekToken(strLiteralExpected, 'FieldList', ':',
          strSeekableOnErrorTokens, stActual);
  Finally
    I.Free;
  End;
End;

(**

  This method parses the variant section of a record from the current token
  position using the following object pascal grammar.

  @grammar VariantSection -> CASE [ Ident ':' ] TypeId OF RecVariant / ';' ...

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Returns true is a variant section of a record was parsed.

  @param   Rec as a TRecordDecl
  @return  a Boolean

**)
Function TPascalDocModule.VariantSection(Rec: TRecordDecl) : Boolean;

Begin
  Result := Token.UToken = 'CASE';
  If Result Then
    Begin
      //: @bug Skip CASE declaration to RecVariant section
      While Token.UToken <> 'OF' Do
        NextNonCommentToken;
      Repeat
        NextNonCommentToken;
        If (Token.UToken = 'END') Or (Token.Token = ')') Then
          Exit;
        RecVariant(Rec);
      Until Token.Token <> ';';
      If Token.Token = ';' Then
        NextNonCommentToken;
    End;
End;

(**

  This method parses the record variant section of a record from the current
  token position using the following object pascal grammar.

  @grammar RecVariant -> ConstExpr / ',' ... ':' '(' [ FieldList ] ')'

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses the record variant section of a record from the current
           token position

  @param   Rec as a TRecordDecl

**)
Procedure TPascalDocModule.RecVariant(Rec : TRecordDecl);

Var
  C : TGenericContainer;
  ExprType : TExprTypes;

Begin
  C := TGenericContainer.Create('tmp', scPrivate, 0, 0);
  Try
    Repeat
      ExprType := [etUnknown, etConstExpr];
      ConstExpr(C, ExprType);
    Until Not IsToken(',', C);
    If Token.Token <> ':' Then
      ErrorAndSeekToken(strLiteralExpected, 'RecVariant', ':',
        strSeekableOnErrorTokens, stActual)
    Else
      NextNonCommentToken;
    If Token.Token <> '(' Then
      ErrorAndSeekToken(strLiteralExpected, 'RecVariant', '(',
        strSeekableOnErrorTokens, stActual)
    Else
      NextNonCommentToken;
    FieldList(Rec);
    If Token.Token <> ')' Then
      ErrorAndSeekToken(strLiteralExpected, 'RecVariant', ')',
        strSeekableOnErrorTokens, stActual)
    Else;
      NextNonCommentToken;
  Finally
    C.Free;
  End;
End;

(**

  This method parses a pointer type declaration from the current token position
  using the following object pascal grammar.

  @grammar PointerType -> '^' TypeId

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TPointerType

**)
Function TPascalDocModule.PointerType : TPointerType;

Begin
  Result := Nil;
  If Not (Token.Token = '^') Then
    Exit;
  Result := TPointerType.Create;
  SymbolTable.Add(Result);
  Result.Add('^');
  NextNonCommentToken;
  If Not TypeId(Result) Then
    ErrorAndSeekToken(strTypeIdExpected, 'PointerType', '',
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses a procedure type declaration from the current token
  position using the following object pascal grammar.

  @grammar ProceduralType -> ( ProcedureHeading | FunctionHeading ) [ OF OBJECT ]

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TProcedureType

**)
Function TPascalDocModule.ProcedureType : TProcedureType;

Var
  M : TMethodDecl;
  MethodType : TMethodType;
  boolOfObject : Boolean;

begin
  boolOfObject := False;
  Result := Nil;
  If Not ((Token.UToken = 'FUNCTION') Or (Token.UToken = 'PROCEDURE')) Then
    Exit;
  If Token.UToken = 'FUNCTION' Then
    MethodType := mtFunction
  Else
    MethodType := mtProcedure;
  M := TMethodDecl.Create(MethodType, scPublic, 0, 0);
  Try
    NextNonCommentToken;
    FormalParameter(M);
    If Token.Token = ':' Then
      Begin
        NextNonCommentToken;
        M.ReturnType := Token.Token;
        NextNonCommentToken;
      End;
    If Token.UToken = 'OF' Then
      Begin
        NextNonCommentToken;
        If Token.UToken <> 'OBJECT' Then
          ErrorAndSeekToken(strReservedWordExpected, 'ProcedureType', 'OBJECT',
            strSeekableOnErrorTokens, stActual)
        Else
          Begin
            boolOfObject := True;
            NextNonCommentToken;
          End;
      End;
    //: @bug Directive(M);
    Result := TProcedureType.Create;
    SymbolTable.Add(Result);
    Result.Add(M.GetAsString(True, True));
    If boolOfObject Then
      Result.Add('Of Object');
  Finally
    M.Free;
  End;
end;

(**

  This method check and parses a var section declaration from the current token
  position using the following object pascal grammar.

  @grammar VarSection -> VAR ( VarDecl ';' ) ...

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.VarSection(Scope : TScope; Method : TMethodDecl) : Boolean;

Var
  C : TComment;
  V : TGenericContainerCollection;

Begin
  Result := Token.UToken = 'VAR';
  If Not Result Then
    Exit;
  V := Vars;
  If Method <> Nil Then
    V := Method.Vars;
  If V.Comment = Nil Then
    V.Comment := GetComment
  Else
    Begin
      C := GetComment;
      V.Comment.Assign(C);
    End;
  NextNonCommentToken;
  While VarDecl(Scope, V) Do
    Begin
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'VarSection', ';',
          strSeekableOnErrorTokens, stFirst)
      Else
        NextNonCommentToken;
    End;
End;

(**

  This method parses a Thread var section declatation from the current token
  position.

  @see     For object pascal grammar see {@link TPascalDocModule.VarSection}.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.ThreadVarSection(Scope : TScope) : Boolean;

Var
  C : TComment;

Begin
  Result := Token.UToken = 'THREADVAR';
  If Not Result Then
    Exit;
  If ThreadVars.Comment = Nil Then
    ThreadVars.Comment := GetComment
  Else
    Begin
      C := GetComment;
      ThreadVars.Comment.Assign(C);
    End;
  NextNonCommentToken;
  While VarDecl(Scope, ThreadVars) Do
    Begin
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'ThreadVarSection', ';',
          strSeekableOnErrorTokens, stFirst)
      Else
        NextNonCommentToken;
    End;
End;

(**

  This method parses a variable declaration from the current token position.

  @grammar VarDecl -> IdentList ':' Type [ ( ABSOLUTE ( Ident | ConstExpr ) ) | '=' ConstExpr ]

  @precon  Scope defines the current scope of the variable and VarSection is a
           valid variable container for the storage of the variable declared.
  @postcon Returns true if a variable declaration was handled.

  @param   Scope      as a TScope
  @param   VarSection as a TGenericContainerCollection
  @return  a Boolean

**)
Function TPascalDocModule.VarDecl(Scope : TScope;
  VarSection : TGenericContainerCollection) : Boolean;

Var
  I  :TIdentList;
  j : Integer;
  V : TGenericContainer;
  T : TTypeDecl;
  C : TGenericContainer;
  ExprType : TExprTypes;

Begin
  Result := False;
  If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
    Exit;
  // Get ident list line and column
  I := IdentList(False, strSeekableOnErrorTokens);
  Try
    If Token.Token <> ':' Then
      ErrorAndSeekToken(strLiteralExpected, 'VarDecl', ':',
        strSeekableOnErrorTokens, stActual)
    Else
      Begin
        NextNonCommentToken;
        T := GetTypeDecl;
        If T = Nil Then
          Raise Exception.Create('Can''t get type in vardecl.');
        If Token.UToken = 'ABSOLUTE' Then
          Begin
            C := TGenericContainer.Create;
            Try
              C.Add(Token.Token);
              NextNonCommentToken;
              ExprType := [etUnknown, etConstExpr];
              ConstExpr(C, ExprType);
            Finally
              T.Append(C);
              C.Free;
            End;
          End;
        If Token.Token = '=' Then
          Begin
            C := TGenericContainer.Create;
            Try
              C.Add(Token.Token);
              NextNonCommentToken;
              ExprType := [etUnknown, etConstExpr];
              ConstExpr(C, ExprType);
            Finally
              T.Append(C);
              C.Free;
            End;
          End;
        For j := 0 To I.Count - 1 Do
          Begin
            V := TVar.Create(I[j].Ident, Scope, I[j].Line, I[j].Col);
            V.Add(':');
            V.Append(T);
            If I[j].Comment <> Nil Then
              Begin
                V.Comment := TComment.Create(I[j].Comment); //: @bug Surely I.Comment gets destroyed!
              End Else
                If I[0].Comment <> Nil Then
                  Begin
                    V.Comment := TComment.Create(I[0].Comment);
                    V.Comment.AddToken('(Copy)', ttIdentifier);
                  End;
            VarSection.Add(V);
          End;
        Result := True;
      End;
  Finally
    I.Free;
  End;
End;

(**

  This method attempts to parse the next series of tokens as an expression.

  @grammer Expression -> SimpleExpression [RelOp SimpleExpression]

  @precon  None.
  @postcon Attempts to parse the next series of tokens as an expression.

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes

**)
Procedure TPascalDocModule.Expression(C : TGenericContainer;
  var ExprType : TExprTypes);

Begin
  Repeat
    SimpleExpression(C, ExprType);
  Until Not RelOp(C, ExprType);
End;

(**

  This method attempts to parse the next series of tokens as a Simple
  Expression.

  @grammar SimpleExpression -> ['+' | '-'] Term [AddOp Term]...

  @precon  none.
  @postcon Attempts to parse the next series of tokens as a Simple Expression.

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes

**)
Procedure TPascalDocModule.SimpleExpression(C : TGenericContainer;
  var ExprType : TExprTypes);

Begin
  If IsKeyWord(Token.Token, ['+', '-']) Then
    AddToExpression(C);
  Repeat
    Term(C, ExprType);
  Until Not AddOp(C);
End;

(**

  This method attempts to parse a term from the current token position.

  @grammar Term -> Factor [MulOp Factor]...

  @precon  None.
  @postcon Attempts to parse a term from the current token position.

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes

**)
Procedure TPascalDocModule.Term(C : TGenericContainer; var ExprType : TExprTypes);

Begin
  Repeat
    Factor(C, ExprType);
  Until Not MulOp(C, ExprType)
End;

(**

  This method attempts to parse a factor from the current token position.

  @grammar Factor -> Designator ['(' ExprList ')']
                  -> '@' Designator
                  -> Number
                  -> String
                  -> NIL
                  -> '(' Expression ')'
                  -> NOT Factor
                  -> SetConstructor
                  -> TypeId '(' Expression ')'

  @precon  None.
  @postcon Attempts to parse a factor from the current token position.

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes

**)
Procedure TPascalDocModule.Factor(C : TGenericContainer;
  var ExprType : TExprTypes);

Var
  SubExprType : TExprTypes;

  (**

    This method sets up the ExprType variable accounting for Constant
    Expressions.

    @precon  None.
    @postcon Sets up the ExprType variable accounting for Constant
             Expressions.

  **)
  Procedure SetupSubExprType;

  Begin
    SubExprType := [etUnknown];
    If etConstExpr In ExprType Then
      Include(SubExprType, etConstExpr); // Make sure const expr is propogated
  End;

Begin
  If Token.TokenType In [ttStringLiteral] Then
    Begin
      If (etUnknown In ExprType) Then
        Begin
          Exclude(ExprType, etUnknown);
          Include(ExprType, etString);
          AddToExpression(C);
        End
      Else If Not (etString In ExprType) Then
        ErrorAndSeekToken(strExprConflict, 'Factor', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        AddToExpression(C);
    End
  Else If Token.TokenType In [ttNumber] Then
    Begin
      If (etUnknown In ExprType) Then
        Begin
          Exclude(ExprType, etUnknown);
          Include(ExprType, etNumeric);
          AddToExpression(C);
        End
      Else If Not (etNumeric In ExprType) Then
        ErrorAndSeekToken(strExprConflict, 'Factor', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        AddToExpression(C);
    End
  Else If Token.UToken = 'NIL' Then
    AddToExpression(C)
  Else If Token.Token = '@' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Expression(C, SubExprType);
    End
  Else If Token.UToken = 'NOT' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Factor(C, SubExprType);
    End
  Else If Token.UToken = 'INHERITED' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Designator(C, SubExprType)
    End
  Else If Token.Token = '(' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Expression(C, SubExprType);
      If Token.Token = ')' Then
        Begin
          AddToExpression(C);
          DesignatorSubElement(C, SubExprType, ['.', '^']); // Type cast handler
        End
      Else
        ErrorAndSeekToken(strLiteralExpected, 'Factor', ')',
          strSeekableOnErrorTokens, stActual);
    End
  Else If SetConstructor(C) Then
    // Do nothing block...
  //: @debug Else If TypeId(C) Then
    // Do nothing block...
  Else
    Begin
      SetupSubExprType;
      Designator(C, SubExprType);
    End;
End;

(**

  This method check for the presence of a RelOp token at the current position
  and returns true if found and advances the token position else returns false

  @grammar RelOp  -> '>'
                  -> '<'
                  -> '<='
                  -> '>='
                  -> '<>'
                  -> IN
                  -> IS
                  -> AS
                  -> = (this is not in the original grammar.)

  @precon  None.
  @postcon Check for the presence of a RelOp token at the current position
           and returns true if found and advances the token position else
           returns false

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes
  @return  a Boolean

**)
Function TPascalDocModule.RelOp(C : TGenericContainer; ExprType : TExprTypes) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strRelOps) And Not
    ((Token.Token = '=') And (etConstExpr In ExprType));
  If Result Then
    AddToExpression(C);
End;

(**

  This method check for the presence of an AddOp token at the current position
  and returns true if found and advances the token position else returns false

  @grammar AddOp 	-> '+'
                  -> '-'
                  -> OR
                  -> XOR

  @precon  None.
  @postcon Check for the presence of an AddOp token at the current position
           and returns true if found and advances the token position else
           returns false

  @param   C as a TGenericContainer
  @return  a Boolean

**)
Function TPascalDocModule.AddOp(C : TGenericContainer) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strAddOps);
  If Result Then
    AddToExpression(C);
End;

(**

  This method check for the presence of a MulOp token at the current position
  and returns true if found and advances the token position else returns false

  @grammar MulOp 	-> '*'
                  -> '/'
                  -> DIV
                  -> MOD
                  -> AND
                  -> SHL
                  -> SHR

  @precon  None.
  @postcon Check for the presence of a MulOp token at the current position
           and returns true if found and advances the token position else
           returns false

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes
  @return  a Boolean

**)
Function TPascalDocModule.MulOp(C : TGenericContainer;
  var ExprType : TExprTypes) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strMulOps);
  If Result Then
    Begin
      If Not (etString In ExprType) Then
        AddToExpression(C)
      Else
        ErrorAndSeekToken(strExprConflict, 'MulOp', Token.Token,
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a Designator.

  @grammar Designator -> QualId ['.' Ident | '[' ExprList ']' | '^']...

  @precon  None
  @postcon Attempts to parse the current token position as a Designator.

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes
  
  @bug     Create a function IsIdentifier which allows all the following token
           types: ttIdentifier, ttDirective

**)
Procedure TPascalDocModule.Designator(C : TGenericContainer;
  var ExprType : TExprTypes);

Begin
  If Token.TokenType In [ttIdentifier, ttDirective] Then
    Begin
      AddToExpression(C);
      DesignatorSubElement(C, ExprType, ['.', '[', '^', '(']);
    End;
End;

(**

  This method handles the sub elements of a designator, i.e. period, [, ( and ^.

  @precon  None.
  @postcon Handles the sub elements of a designator, i.e. period, [, ( and ^.

  @param   C               as a TGenericContainer
  @param   ExprType        as a TExprTypes as a reference
  @param   strValidSymbols as an Array Of String

**)
Procedure TPascalDocModule.DesignatorSubElement(C : TGenericContainer;
  var ExprType : TExprTypes; strValidSymbols : Array of String);

Begin
  While IsKeyWord(Token.Token, strValidSymbols) Or (IsKeyWord(Token.Token, ['(', '['])) Do // Always check for proc/func
    If Token.Token = '.' Then
      Begin
        AddToExpression(C);
        If Token.TokenType In [ttIdentifier, ttDirective] Then
          AddToExpression(C)
        Else
          ErrorAndSeekToken(strIdentExpected, 'DesignatorSubElement', Token.Token,
            strSeekableOnErrorTokens, stActual);
      End
    Else If Token.Token = '[' Then
      Begin
        AddToExpression(C);
        ExprList(C);
        If Token.Token = ']' Then
          AddToExpression(C)
        Else
          ErrorAndSeekToken(strLiteralExpected, 'DesignatorSubElement', ']',
            strSeekableOnErrorTokens, stActual);
      End
    Else If Token.Token = '^' Then
      AddToExpression(C)
    Else If Token.Token = '(' Then
      Begin
        If etConstExpr In ExprType Then
          If Not IsKeyWord(PrevToken.Token, strConstExprDesignators) Then
            Begin
              ErrorAndSeekToken(strConstExprDesignator, 'DesignatorSubElement',
                PrevToken.Token, strSeekableOnErrorTokens, stActual);
              Exit;
            End;
        AddToExpression(C);
        ExprList(C);
        If Token.Token = ')' Then
          AddToExpression(C)
        Else
          ErrorAndSeekToken(strLiteralExpected, 'DesignatorSubElement', ')',
            strSeekableOnErrorTokens, stActual);
      End;
End;

(**

  This method attempts to parse the current token position as a Set Constructor.

  @grammar SetConstructor -> '[' [SetElement/','...] ']'

  @precon  None.
  @postcon Attempts to parse the current token position as a Set Constructor.

  @param   C as a TGenericContainer
  @return  a Boolean

**)
Function TPascalDocModule.SetConstructor(C : TGenericContainer) : Boolean;

Begin
  Result := Token.Token = '[';
  If Result Then
    Begin
      AddToExpression(C);
      SetElement(C);
      If Token.Token = ']' Then
        AddToExpression(C);
    End;
End;

(**

  This method attempts to parse the current token position as a set element.

  @grammar SetElement -> Expression ['..' Expression]
  
  @precon  None.
  @postcon Attempts to parse the current token position as a set element.

  @param   C as a TGenericContainer

**)
Procedure TPascalDocModule.SetElement(C : TGenericContainer);

Var
  ExprType : TExprTypes;

Begin
  Repeat
    ExprType := [etUnknown];
    Expression(C, ExprType);
  Until Not (IsToken('..', C) Or IsToken(',', C));
End;

(**

  This method attempts to parse the current token position as an Expression
  List.
  
  @grammar ExprList -> Expression/','...
  
  @precon  None.
  @postcon Attempts to parse the current token position as an Expression List.
  
  @param   C as a TGenericContainer

**)
Procedure TPascalDocModule.ExprList(C : TGenericContainer);

Var
  ExprType : TExprTypes;

Begin
  Repeat
    ExprType := [etUnknown];
    Expression(C, ExprType);
  Until Not IsToken(',', C);
End;

(**

  This method attempts to parse the current token position as a statement.

  @grammar Statement -> [LabelId ':'] [SimpleStatement | StructStmt]

  @precon  None.
  @postcon Attempts to parse the current token position as a statement.

**)
Procedure TPascalDocModule.Statement;

Begin
  // Check for label
  NextNonCommentToken;
  If Token.Token = ':' Then
    NextNonCommentToken
  Else
    RollBackToken;
  If Not StructStmt Then
    SimpleStatement;
End;

(**

  This method attempts to parse the current token as a list of statements.

  @grammar StmtList -> Statement ';'...

  @precon  None.
  @postcon Attempts to parse the current token as a list of statements.

**)
Procedure TPascalDocModule.StmtList;

Begin
  Repeat
    Statement;
  Until Not IsToken(';', Nil);
End;

(**

  This method attempts to evaluate the current token position as a Simple
  Statement.

  @grammar SimpleStatement -> Designator ['(' ExprList ')']
                           -> Designator ':=' Expression
                           -> INHERITED
                           -> GOTO LabelId

  @precon  None.
  @postcon Attempts to evaluate the current token position as a Simple
           Statement.

**)
Procedure TPascalDocModule.SimpleStatement;

Var
  ExprType : TExprTypes;

Begin
  If Token.Token = 'GOTO' Then
    Begin
      NextNonCommentToken;
      NextNonCommentToken;
    End
  Else
    Begin
      If Token.UToken = 'INHERITED' Then
        NextNonCommentToken;
      ExprType := [etUnknown];
      Designator(Nil, ExprType);
      If Token.Token = '(' Then
        Begin
          NextNonCommentToken;
          ExprType := [etUnknown];
          Expression(Nil, ExprType);
          If Token.Token = ')' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'SimpleStatement', ')',
              strSeekableOnErrorTokens, stActual);
          DesignatorSubElement(Nil, ExprType, ['.', '^']);
        End;
      If Token.Token = ':=' Then
        Begin
          NextNonCommentToken;
          ExprType := [etUnknown];
          Expression(Nil, ExprType);
        End;
    End;
End;

(**

  This method attempts to parse the current token position as a structured
  statement.

  @grammar StructStmt -> CompoundStmt
                      -> ConditionalStmt
                      -> LoopStmt
                      -> WithStmt
                      -> TryExceptStmt
                      -> TryFinallyStmt
                      -> RaiseStmt
                      -> AssemblerStmt


  @precon  None.
  @postcon Attempts to parse the current token position as a structured
           statement.

  @return  a Boolean

**)
Function TPascalDocModule.StructStmt : Boolean;

Begin
  Result := CompoundStmt Or ConditionalStmt Or LoopStmt Or WithStmt Or
    TryExceptAndFinallyStmt Or // <= Combined together as the type can not be
    RaiseStmt Or               //    determined until the Except or Finally
    AssemblerStatement;        //    key work is found.
End;

(**

  This method parses the compound statement section of a procedure implementation
  from the current token position using the following object pascal grammar.

  @grammar CompoundStmt -> BEGIN StmtList END

  @precon  None.
  @postcon Parses the compound statement section of a procedure implementation
           from the current token position

  @return  a Boolean

**)
Function TPascalDocModule.CompoundStmt : Boolean;

begin
  Result := Token.UToken = 'BEGIN';
  If Result Then
    Begin
      NextNonCommentToken;
      StmtList;
      If Token.UToken = 'END' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'CompoundStmt', 'END',
          ['end']{strSeekableOnErrorTokens}, stActual);
    End;
end;

(**

  This method attempts to parse the current token position as a ConditionalStmt.

  @grammar ConditionalStmt -> IfStmt
                           -> CaseStmt

  @precon  None.
  @postcon Attempts to parse the current token position as a ConditionalStmt.

  @return  a Boolean

**)
Function TPascalDocModule.ConditionalStmt : Boolean;

Begin
  Result := IfStmt Or CaseStmt;
End;

(**

  This method attempts to parse the current token position as an IF statement.

  @grammar IfStmt -> IF Expression THEN Statement [ELSE Statement]

  @precon  None.
  @postcon Attempts to parse the current token position as an IF statement.

  @return  a Boolean

**)
Function TPascalDocModule.IfStmt : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.UToken = 'IF';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [etUnknown];
      Expression(Nil, ExprType);
      If Token.UToken = 'THEN' Then
        Begin
          NextNonCommentToken;
          Statement;
          If Token.UToken = 'ELSE' Then
            Begin
              NextNonCommentToken;
              Statement;
            End;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'IfStmt', 'THEN',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a CASE statement.

  @grammar CaseStmt -> CASE Expression OF CaseSelector ';'... [ELSE Statement] [';'] END

  @precon  None.
  @postcon Attempts to parse the current token position as a CASE statement.

  @return  a Boolean

**)
Function TPascalDocModule.CaseStmt : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.UToken = 'CASE';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [etUnknown];
      Expression(Nil, ExprType);
      If Token.UToken = 'OF' Then
        Begin
          NextNonCommentToken;
          Repeat
            CaseSelector
          Until Not IsToken(';', Nil);
          If Token.UToken = 'ELSE' Then
            Begin
              NextNonCommentToken;
              StmtList;
             End;
          If Token.UToken = 'END' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strReservedWordExpected, 'CaseStmt', 'END',
              strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'CaseStmt', 'OF',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a case selector.

  @grammar CaseSelector	-> CaseLabel ','... ':' Statement

  @precon  None.
  @postcon Attempts to parse the current token position as a case selector.

  @return  a Boolean

**)
Procedure TPascalDocModule.CaseSelector;

Begin
  Repeat
    CaseLabel;
  Until Not IsToken(',', Nil);
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      Statement;
    End Else
      If Not IsKeyWord(Token.Token, ['else', 'end']) Then
        ErrorAndSeekToken(strLiteralExpected, 'CaseSelector', ':',
          strSeekableOnErrorTokens, stActual);
End;

(**

  This method attempts to parse the current token position as a Case Label.

  @grammar CaseLabel -> ConstExpr ['..' ConstExpr]

  @precon  None.
  @postcon Attempts to parse the current token position as a Case Label.

**)
Procedure TPascalDocModule.CaseLabel;

Var
  ExprType : TExprTypes;

Begin
  ExprType := [etUnknown, etConstExpr];
  ConstExpr(Nil, ExprType);
  If Token.Token = '..' Then
    Begin
      NextNonCommentToken;
      ConstExpr(Nil, ExprType);
    End;
End;

(**

  This method attempts to parse the current token position as a Loop statement.

  @grammar LoopStmt -> RepeatStmt
                    -> WhileStmt
                    -> ForStmt

  @precon  None.
  @postcon Attempts to parse the current token position as a Loop statement.

  @return  a Boolean

**)
Function TPascalDocModule.LoopStmt : Boolean;

Begin
  Result := RepeatStmt Or WhileStmt Or ForStmt;
End;

(**

  This method attempts to parse the current token position as a Repeat Statement.

  @grammar RepeatStmt -> REPEAT StmtList UNTIL Expression

  @precon  None.
  @postcon Attempts to parse the current token position as a Repeat Statement.

  @return  a Boolean

**)
Function TPascalDocModule.RepeatStmt : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.UToken = 'REPEAT';
  If Result Then
    Begin
      NextNonCommentToken;
      StmtList;
      If Token.UToken = 'UNTIL' Then
        Begin
          NextNonCommentToken;
          ExprType := [etUnknown];
          Expression(Nil, ExprType);
        End
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'RepeatStmt', 'UNTIL',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a While Statement.

  @grammar WhileStmt -> WHILE Expression DO Statement

  @precon  None.
  @postcon Attempts to parse the current token position as a While Statement.

  @return  a Boolean

**)
Function TPascalDocModule.WhileStmt : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.UToken = 'WHILE';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [etUnknown];
      Expression(Nil, ExprType);
      If Token.UToken = 'DO' Then
        Begin
          NextNonCommentToken;
          Statement;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'WhileStmt', 'DO',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempt to parse the current token position as a For statement.

  @grammar ForStmt -> FOR QualId ':=' Expression (TO | DOWNTO) Expression DO Statement

  @precon  None.
  @postcon Attempt to parse the current token position as a For statement.

  @return  a Boolean

**)
Function TPascalDocModule.ForStmt : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.UToken = 'FOR';
  If Result Then
    Begin
      NextNonCommentToken;
      If Token.TokenType = ttIdentifier Then
        Begin
          NextNonCommentToken;
          If Token.Token = ':=' Then
            Begin
              NextNonCommentToken;
              ExprType := [etUnknown];
              Expression(Nil, ExprType);
              If IsKeyWord(Token.Token, ['downto', 'to']) Then
                Begin
                  NextNonCommentToken;
                  Expression(Nil, ExprType);
                  If Token.UToken = 'DO' Then
                    Begin
                      NextNonCommentToken;
                      Statement;
                    End Else
                      ErrorAndSeekToken(strReservedWordExpected, 'ForStmt', 'DO',
                        strSeekableOnErrorTokens, stActual);
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, 'ForStmt',
                    'TO or DOWNTO', strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'ForStmt', ':=',
                strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strIdentExpected, 'ForStmt', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a With Statement.

  @grammar WithStmt -> WITH IdentList DO Statement

  @precon  None.
  @postcon Attempts to parse the current token position as a With Statement.

  @return  a Boolean

**)
Function TPascalDocModule.WithStmt : Boolean;

Begin
  Result := Token.UToken = 'WITH';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprList(Nil);
      If Token.UToken = 'DO' Then
        Begin
          NextNonCommentToken;
          Statement;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'WithStmt', 'DO',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a Try Except or
  Try Finally block.

  @grammar TryExceptStmt -> TRY
                              StmtList
                            EXCEPT
                              ExceptionBlock
                            END

                            ...or...

                            TRY
                              StmtList
                            FINALLY
                              StmtList
                            END

  @precon  None.
  @postcon Attempts to parse the current token position as a Try Except or
           Try Finally block.

  @return  a Boolean

**)
Function TPascalDocModule.TryExceptAndFinallyStmt : Boolean;

Begin
  Result := Token.UToken = 'TRY';
  If Result Then
    Begin
      NextNonCommentToken;
      StmtList;
      If IsKeyWord(Token.UToken, ['except', 'finally']) Then
        Begin
          If Token.UToken = 'EXCEPT' Then
            Begin
              NextNonCommentToken;
              If Not ExceptionBlock Then
                StmtList;
            End Else
            Begin
              NextNonCommentToken;
              StmtList;
            End;
          If Token.UToken = 'END' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strReservedWordExpected, 'TryExceptAndFinallyStmt',
              'END', strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'TryExceptAndFinallyStmt',
            'EXCEPT or FINALLY', strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempt to parse the current token position as an Exception Block.

  @grammar ExceptionBlock -> [ON [Ident ‘:’] TypeID DO Statement]...
		             [ELSE Statement]


  @precon  None.
  @postcon Attempt to parse the current token position as an Exception Block.

  @return  a Boolean

**)
Function TPascalDocModule.ExceptionBlock : Boolean;

Var
  C : TGenericContainer;

Begin
  Result := False;
  While Token.UToken = 'ON' Do
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType = ttIdentifier Then
        Begin
          NextNonCommentToken;
          If Token.Token = ':' Then
            NextNonCommentToken
          Else
            RollBackToken;
        End;
      C := TGenericContainer.Create;
      Try
        TypeId(C);
        If Token.UToken = 'DO' Then
          Begin
            NextNonCommentToken;
            Statement;
            If Token.Token = ';' Then
              Begin
                NextNonCommentToken;
                If Token.UToken = 'ELSE' Then
                  Begin
                    NextNonCommentToken;
                    StmtList;
                  End;
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, 'ExceptionBlock', 'DO',
                    strSeekableOnErrorTokens, stActual);
              End Else
                ErrorAndSeekToken(strLiteralExpected, 'ExceptionBlock', ';',
                  strSeekableOnErrorTokens, stActual);
      Finally
        C.Free;
      End;
    End;
End;

(**

  This method attempts to parse the current token position as a Raise Statement.

  @grammar RaiseStmt -> RAISE [object] [AT address]

  @precon  None.
  @postcon Attempts to parse the current token position as a Raise Statement.

  @return  a Boolean

**)
Function TPascalDocModule.RaiseStmt : Boolean;

Begin
  Result := Token.UToken = 'RAISE';
  If Result Then
    Begin
      NextNonCommentToken;
      SimpleStatement;
      If Token.UToken = 'AT' Then
        Begin
          NextNonCommentToken;
          If Token.TokenType = ttNumber Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strNumberExpected, 'RaiseStmt', '',
              strSeekableOnErrorTokens, stActual);
        End;
    End;
End;

(**

  This method attempts to parse the current token position as an assembler
  statement.

  @grammar AssmeblerStatement -> ASM
                                   <assemblerlanguage>
                                 END


  @precon  None.
  @postcon Attempts to parse the current token position as an assembler
           statement.

  @return  a Boolean

**)
Function TPascalDocModule.AssemblerStatement : Boolean;

Begin
  Result := Token.UToken = 'ASM';
  If Result Then
    Begin
      Repeat
        NextNonCommentToken;
      Until Token.UToken = 'END';
      NextNonCommentToken;
    End;
End;

(**

  This method parses a procedure declaration section from the current token
  position using the following object pascal grammar.

  @grammar ProcedureDeclSection -> ProcedureDecl
                                -> FunctionDecl
                                -> ConstructorDecl
                                -> DestructorDecl

  @note    This method is not implemented as per the grammar as I think the
           grammar is incorrect. The grammar dictates a block call after each
           procedure and function declaration BUT there are no constructor and
           destructor declarations only headings. I have places the Block() call
           here in the ProceduralDeclSection() instead and called all
           ####Heading() methods.

  @precon  Scope is the current scope of the procedure declaration and Method
           is the current method scoped else nil.
  @postcon Returns true is a procedure declaration was parsed.

  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.ProcedureDeclSection(Scope : TScope;
  Method : TMethodDecl) : Boolean;

Var
  M : TMethodDecl;
  Cls : Boolean;

Begin
  Result := False;
  Repeat
    Cls := False;
    If Token.UToken = 'CLASS' Then
      Begin
        NextNonCommentToken;
        Cls := True;
      End;
    M := ProcedureHeading(Scope);
    If M = Nil Then
      M := FunctionHeading(Scope);
    If M = Nil Then
      M := ConstructorHeading(Scope);
    If M = Nil Then
      M := DestructorHeading(Scope);
    If M <> Nil Then
      Begin
        Result := True;
        NextNonCommentToken;
        Directive(M);
        M.ClassMethod := Cls;
        If Method = Nil Then
          ImplementedMethods.Add(M)
        Else
          Method.LocalMethods.Add(M);
        // Only call Block not an external procedure.
        If (Length(M.Ext) = 0) And Not M.HasDirective('forward') Then
          Begin
            Block(scLocal, M);
            If Token.Token = ';' Then
              NextNonCommentToken
            Else
              ErrorAndSeekToken(strLiteralExpected, 'ProcedureDeclSection', ';',
                strSeekableOnErrorTokens, stActual);
          End;
      End;
  Until M = Nil;
End;

(**

  This method attempts to parse the current token position as a ProcedureDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a ProcedureDecl;

  @grammar ProcedureDecl -> ProcedureHeading ';' [Directive] [PortabilityDirective]
                            Block ';'

**)
Procedure TPascalDocModule.ProcedureDecl;

Begin
End;

(**

  This method attempts to parse the current token position as a FunctionDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a FunctionDecl;

  @grammar FunctionDecl -> FunctionHeading ';' [Directive] [PortabilityDirective]
                           Block ';'

**)
Procedure TPascalDocModule.FunctionDecl;

Begin
End;

(**

  This method parses a function declaration from the current token position
  using the following object pascal grammar.

  @grammar FunctionHeading -> FUNCTION Ident [ FormalParameters ] ':' ( SimpleType | STRING )

  @precon  Scope is the current scope of the function declaration.
  @postcon Returns a method declaration is a function was parsed else nil.

  @param   Scope as a TScope
  @return  a TMethodDecl

**)
Function TPascalDocModule.FunctionHeading(Scope :TScope) : TMethodDecl;

Begin
  Result := ProcedureBit(mtFunction, Scope);
  // Exit if not a function
  If Result = Nil Then
    Exit;
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      Result.ReturnType := Token.Token;
      NextNonCommentToken;
    End Else
      Errors.Add(Format(strFunctionWarning, [Result.QualifiedName]),
        'FunctionHeading', Token.Line, Token.Column, etWarning);
End;

(**

  This method parse a procedure declaration from the current token position
  using the following object pascal grammar.

  @grammar ProcedureHeading -> PROCEDURE Ident [ FormalParameters ]

  @precon  Scope is the current scope of the procedure declaration.
  @postcon Returns a method declaration is a procedure was parsed else nil.

  @param   Scope    as a TScope
  @return  a TMethodDecl

**)
Function TPascalDocModule.ProcedureHeading(Scope : TScope) : TMethodDecl;

Begin
  Result := ProcedureBit(mtProcedure, Scope);
End;

(**

  This method does the donkey work for parsing the main portion of a method
  declaration on behalf of the ####Heading functions.

  @precon  ProcType is the type if method the be handled, proedure, function,
           etc and Scope is the scope of th method.
  @postcon Returns a method declaration object if parsed else returns nil.

  @param   ProcType as a TMethodType
  @param   Scope    as a TScope
  @return  a TMethodDecl

**)
Function TPascalDocModule.ProcedureBit(ProcType : TMethodType;
  Scope : TScope) : TMethodDecl;

Var
  C : TComment;

Begin
  Result := Nil;
  If Token.UToken <> UpperCase(strMethodTypes[ProcType]) Then
    Exit;
  If PrevToken.UToken = 'CLASS' Then
    Begin
      RollBackToken;
      C := GetComment;
      NextNonCommentToken;
    End Else
      C := GetComment;
  NextNonCommentToken;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
    Begin
      // Create method and store in collection and get comment
      Result := TMethodDecl.Create(ProcType, Scope, Token.Line, Token.Column);
      Result.Comment := C;
      Result.Identifier := Token.Token;
      NextNonCommentToken;
      // Check for '.' to signify a class method
      If Token.Token = '.' Then
        Begin
          NextNonCommentToken;
          Result.ClsName := Result.Identifier;
          If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
            ErrorAndSeekToken(strIdentExpected, 'ProcedureHeading', Token.Token,
              strSeekableOnErrorTokens, stActual);
          Result.Identifier := Token.Token;
          NextNonCommentToken;
        End;
      FormalParameter(Result);
      // Check for an alias
      If Token.Token = '=' Then
        Begin
          NextNonCommentToken;
          Result.Alias := Token.Token;
          NextNonCommentToken;
          If Token.Token = '.' Then
            Begin
              Result.Alias := Result.Alias + Token.Token;
              NextNonCommentToken;
              Result.Alias := Result.Alias + Token.Token;
              NextNonCommentToken;
            End;
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'ProcedureHeading', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a methods formal parameters from the current token
  position using the following object psacal grammar.

  @grammar FormalParameter -> '(' FormalParm / ';' ... ')'

  @precon  Method is a valid method to which the formal parameters are to be
           added.
  @postcon Parses a methods formal parameters from the current token position

  @param   Method as a TMethodDecl

**)
Procedure TPascalDocModule.FormalParameter(Method : TMethodDecl);

Begin
  If Token.Token = '(' Then
    Begin
      Repeat
        NextNonCommentToken;
        If Token.Token = ')' Then
          Break;
        FormalParam(Method);
      Until Token.Token <> ';';
      If Token.Token = ')' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'FormalParameters', ')',
          strSeekableOnErrorTokens, stActual);
  End;
End;

(**

  This method parses a formal parameter for a method from the current token
  position using the following object pascal grammar.

  @grammar FormalParam -> [ VAR | CONST | OUT ] Parameter

  @precon  Method is a valid method to which the formal parameters are to be
           added.
  @postcon Parses a formal parameter for a method from the current token
           position

  @param   Method as a TMethodDecl

**)
Procedure TPascalDocModule.FormalParam(Method : TMethodDecl);

Var
  pmMod : TParamModifier;

Begin
  pmMod := pmNone;
  // Get modifiers
  If Token.UToken = 'VAR' Then
    pmMod := pmVar
  Else If Token.UToken = 'CONST' Then
    pmMod := pmConst
  Else If Token.UToken = 'OUT' Then
    pmMod := pmOut;
  If pmMod <> pmNone Then
    NextNonCommentToken;
  Parameter(Method, pmMod);
End;

(**

  This method parses a parameter list for a method from the current token
  position using the following object pascal grammar.

  @grammar Parameter -> IdentList [ ':' ( [ ARRAY OF ] SimpleType | STRING | FILE ) ]
                     -> Ident ':' SimpleType '=' ConstExpr

  @precon  Method is a valid method to add a parameter too and ParamMod is a
           parameter modifier for the parameter to signify a const, var or out
           paramemter.
  @postcon Parses a parameter list for a method from the current token position

  @param   Method   as a TMethodDecl
  @param   ParamMod as a TParamModifier

**)
Procedure TPascalDocModule.Parameter(Method : TMethodDecl;
  ParamMod : TParamModifier);

Var
  I : TIdentList;
  boolArrayOf : Boolean;
  strValue : String;
  j : Integer;
  T : TTypeDecl;

Begin
  // Get ident list
  T := Nil;
  boolArrayOf := False;
  strValue := '';
  I := IdentList(False, strSeekableOnErrorTokens);
  Try
    If Token.Token = ':' Then
      Begin
        NextNonCommentToken;
        // Check Array Of
        If Token.UToken = 'ARRAY' Then
          Begin
            NextNonCommentToken;
            IF Token.UToken = 'OF' Then
              Begin;
                boolArrayOf := True;
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strReservedWordExpected, 'FormalParameter', 'OF',
                  strSeekableOnErrorTokens, stActual);
          End;
        T := GetTypeDecl;
        If T = Nil Then
          If Token.UToken = 'CONST' Then
            Begin
              T := TTypeDecl.Create(Token.Token, scPrivate, Token.Line,
                Token.Column);
              NextNonCommentToken;
            End;
        // Get default value
        If Token.Token = '=' Then
          Begin
            NextNonCommentToken;
            While (Token.Token <> ';') And (Token.Token <> ')') Do
              Begin
                If strValue <> '' Then
                  strValue := strValue + #32;
                strValue := Token.Token;
                NextNonCommentToken;
              End;
          End;
      End;
    // Create the parameters using the ident list
    For j := 0 To I.Count - 1 Do
      Method.Parameters.Add(TParameter.Create(ParamMod, I[j].Ident, boolArrayOf, T,
        strValue, scPublic, I[j].Line, I[j].Col));
  Finally
    I.Free;
  End;
End;


(**

  This method retrives the method directives after the method declaration from
  the current token position using the followong object pascal grammar.

  @grammar Directive -> REGISTER    ';'
                        DYNAMIC     ';'
                        VIRTUAL     ';'
                        EXPORT      ';'
                        EXTERNAL    ';'
                        FAR         ';'
                        FORWARD     ';'
                        MESSAGE     ';'
                        OVERRIDE    ';'
                        OVERLOAD    ';'
                        PASCAL      ';'
                        REINTRODUCE ';'
                        SAFECALL    ';'
                        STDCALL     ';'

  @precon  M is a valid method declaration to add directives too.
  @postcon Retrives the method directives after the method declaration from
           the current token position

  @param   M as a TMethodDecl

**)
Procedure TPascalDocModule.Directive(M : TMethodDecl);

Begin
  // Check for method directives
  While IsKeyWord(Token.Token, strMethodDirectives) Do
    Begin
      M.AddDirectives(Token.Token);
      If Token.UToken = 'MESSAGE' Then
        Begin
          NextNonCommentToken;
          While Token.Token <> ';' Do
            Begin
              If M.Msg <> '' Then
                M.Msg := M.Msg + #32;
              M.Msg := M.Msg + Token.Token;
              NextNonCommentToken;
            End;
          RollBackToken;
        End;
      If Token.UToken = 'EXTERNAL' Then
        Begin
          NextNonCommentToken;
          While Token.Token <> ';' Do
            Begin
              If M.Ext <> '' Then
                M.Ext := M.Ext + #32;
              M.Ext := M.Ext + Token.Token;
              NextNonCommentToken;
            End;
          RollBackToken;
        End;
      If Token.UToken = 'DISPID' Then
        Begin
          NextNonCommentToken;
          If Token.Token = '-' Then
            Begin
              M.AddDirectives(Token.Token);
              NextNonCommentToken;
            End;
          If Token.TokenType = ttNumber Then
            M.AddDirectives(Token.Token)
          Else
            ErrorAndSeekToken(strNumberExpected, 'Directive', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End;
      NextNonCommentToken;
      If Token.Token = ';' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'Directive', ';',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses an Object type declaration from the current token position
  using the followong object pascal grammar.

  @grammar ObjectType -> OBJECT [ ObjHertiage ] [ ObjectFieldList ] [ MethodList ]

  @precon  None.
  @postcon Returns an object declaration if one was parsed else nil.

  @return  a TObjectDecl

**)
function TPascalDocModule.ObjectType : TObjectDecl;

Var
  InternalScope : TScope;

begin
  InternalScope := scPublic;
  Result := Nil;
  If Not (Token.UToken = 'OBJECT') Then
    Exit;
  Result := TObjectDecl.Create;
  SymbolTable.Add(Result);
  NextNonCommentToken;
  // Get the classes heritage
  ObjHeritage(Result);
  // If this class has not body then return
  If Token.Token = ';' Then
    Begin
      NextNonCommentToken;
      Exit;
    End;
  Repeat
    ClassVisibility(InternalScope);
    If Token.UToken = 'END' Then
      Break;
  Until Not (
    ClassMethodList(Result, InternalScope) Or
    ClassFieldList(Result, InternalScope)
  );
  // Check for 'END' and ';'
  If Token.UToken = 'END' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strReservedWordExpected, 'ObjectType', 'END',
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method attempts to parse the current token position as an Object Heritage
  list.

  @grammar ObjHeritage -> '(' QualId ')'

  @precon  None.
  @postcon Attempts to parse the current token position as an Object Heritage
           list.

  @param   ObjDecl as a TObjectDecl

**)
Procedure TPascalDocModule.ObjHeritage(ObjDecl : TObjectDecl);

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      If Token.TokenType = ttIdentifier Then
        Begin
          NextNonCommentToken;
          If Token.Token = ')' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'ObjHeritage', ')',
              strSeekableOnErrorTokens, stActual);
        End Else
            ErrorAndSeekToken(strIdentExpected, 'ObjHeritage', Token.Token,
              strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parse a method list from the current token position using the
  following object pascal grammar.

  @grammar MethodList -> ( MethodHeading [ ';' VIRTUAL ] ) / ';' ...

  @precon  Cls is an object declaration to add methods too and Scopeis the
           current internal scope of the object.
  @postcon Returns true is a method declaration was parsed.

  @param   Cls   as a TObjectDecl
  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.MethodList(Cls: TObjectDecl; Scope: TScope): Boolean;

Begin
  Result := MethodHeading(Cls, Scope);
End;

(**

  This method checks for and parses a method declaration in a class from the
  current token position.

  @grammar MethodHeading -> ProcedureHeading
                         -> FunctionHeading
                         -> ConstructorHeading
                         -> DestructorHeading

  @precon  Cls is an object declaration to add method declarations too and
           Scope is the current scope inside the object declaration.
  @postcon Returns true if a method declaration was parsed.

  @param   Cls   as a TObjectDecl
  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.MethodHeading(Cls: TObjectDecl; Scope: TScope): Boolean;

Var
  M :TMethodDecl;
  boolClassMethod : Boolean;

begin
  Result := False;
  // Check for class method
  boolClassMethod := False;
  If Token.UToken = 'CLASS' Then
    Begin
      boolClassMethod := True;
      NextNonCommentToken;
    End;
  // Check for method
  M := ProcedureHeading(Scope);
  If M = Nil Then
    M := FunctionHeading(Scope);
  If M = Nil Then
    M := ConstructorHeading(Scope);
  If M = Nil Then
    M := DestructorHeading(Scope);
  If M <> Nil Then
    Begin
      M.ClassMethod := boolClassMethod;
      Result := True;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(M);
          Cls.Methods.Add(M);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ExportedHeading', ';',
            strSeekableOnErrorTokens, stActual);
    End Else
      If boolClassMethod Then
        RollBackToken;
end;

(**

  This method parses a constructor declaration from the current token position
  using the following object pascal grammar.

  @grammar ConstructorHeading -> CONSTRUCTOR Ident [ FormalParameters ]

  @precon  Scope is the current scope of the constructor declaration.
  @postcon Returns a method declaration is a constructor was parsed else nil.

  @param   Scope as a TScope
  @return  a TMethodDecl

**)
function TPascalDocModule.ConstructorHeading(Scope: TScope): TMethodDecl;
begin
  Result := ProcedureBit(mtConstructor, Scope);
end;

(**

  This method parses a destructor declaration from the current token position
  using the following object pascal grammar.

  @grammar DestructorHeading -> DESTRUCTOR Ident [ FormalParameters ]

  @precon  Scope is the current scope of the destructor declaration.
  @postcon Returns a method declaration is a destructor was parsed else nil.

  @param   Scope as a TScope
  @return  a TMethodDecl

**)
function TPascalDocModule.DestructorHeading(Scope: TScope): TMethodDecl;
begin
  Result := ProcedureBit(mtDestructor, Scope);
end;

(**

  This method parses a classes / interfaces field list from the current token
  position using the following object pascal grammar.

  @grammar ObjFieldList -> ( IndentList ':' Type ) / ';' ...

  @precon  Cls is an ibject delcaration to add fields too and Scope is the
           current internal scope of the object.
  @postcon Returns true is a field was parsed.

  @param   Cls   as a TObjectDecl
  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.ObjFieldList(Cls: TObjectDecl; Scope: TScope): Boolean;

Var
  I : TIdentList;
  j : Integer;
  P : TParameter;
  T : TTypeDecl;

begin
  Result := False;
  I := IdentList(False, strSeekableOnErrorTokens);
  Try
    If Token.Token = ':' Then
      Begin
        Result := True;
        NextNonCommentToken;
        T := GetTypeDecl;
        For j := 0 To I.Count - 1 Do
          Begin
            P := TParameter.Create(pmNone, I[j].Ident, False, T, '', Scope,
              I[j].Line, I[j].Col);
            P.Comment := I[j].Comment;
            Cls.AddParameter(P);
          End;
      End Else
        ErrorAndSeekToken(strLiteralExpected, 'ObjFieldList', ':',
          strSeekableOnErrorTokens, stActual);
  Finally
    I.Free;
  End;
end;

(**

  This method parses the modules initialisation / finalisation section from the
  current token position using the following object pascal grammar.

  @grammar InitSection -> INITIALIZATION StmtList [ FINALIZATION StmtList ] END
                       -> BEGIN StmtList End
                       -> END

  @precon  None.
  @postcon Parses the modules initialisation / finalisation section from the
           current token position

**)
Procedure TPascalDocModule.InitSection;

Begin
  If Token.UToken = 'INITIALIZATION' Then
    Begin
      InitComment := GetComment;
      NextNonCommentToken;
      StmtList;
      If Token.UToken = 'FINALIZATION' Then
        Begin
          FinalComment := GetComment;
          NextNonCommentToken;
          StmtList;
        End;
      If Token.UToken = 'END' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'Initsection',
          'END', strSeekableOnErrorTokens, stActual);
    End
  Else If Token.UToken = 'BEGIN' Then
    CompoundStmt
  Else If Token.UToken = 'END' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strReservedWordExpected, 'Initsection',
      'INITIALIZATION, BEGIN or END', strSeekableOnErrorTokens, stActual);
End;

(**

  This method parse a class declaration from the current token position
  deligating field, property and method declarations using the following
  object pascal grammar.

  @grammar ClassType -> CLASS [ ClassHeritage ]
                              [ ClassFieldList ]
                              [ ClassMethodList ]
                              [ ClassPropertyList ]
                        END

  @precon  None.
  @postcon Returns a class declaration is a class was parsed else nil.

  @return  a TClassDecl

**)
function TPascalDocModule.ClassType : TClassDecl;

Var
  InternalScope : TScope;

begin
  InternalScope := scPublished;
  Result := Nil;
  If Not (Token.UToken = 'CLASS') Then
    Exit;
  NextNonCommentToken;
  // Check for 'OF'
  If Token.UToken = 'OF' Then
    Begin
      RollBackToken;
      Exit;
    End;
  Result := TClassDecl.Create;
  SymbolTable.Add(Result);
  Result.AbstractClass := (Token.UToken = 'ABSTRACT');
  If Result.AbstractClass Then
    NextNonCommentToken;
  // Get the classes heritage
  ClassHeritage(Result);
  // If this class has no body then return
  If Token.Token = ';' Then
    Exit;
  Repeat
    ClassVisibility(InternalScope);
    If Token.UToken = 'END' Then
      Break;
  Until Not (
    ClassMethodList(Result, InternalScope) Or
    ClassPropertyList(Result, InternalScope) Or
    ClassFieldList(Result, InternalScope)
  );
  // Check for 'END' and ';'
  If Token.UToken = 'END' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strReservedWordExpected, 'ClassType', 'END',
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses a class heriage ist from the current token
  position using the following object pascal grammar.

  @grammar ClassHeritage -> '(' IdentList ')'

  @precon  Cls is a valid object declaration to get a heritage for.
  @postcon Parses a class heriage ist from the current token position

  @param   Cls as a TObjectDecl

**)
procedure TPascalDocModule.ClassHeritage(Cls: TObjectDecl);

Var
  I : TIdentList;

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      I := IdentList(False, strSeekableOnErrorTokens);
      Try
        Cls.Heritage.Assign(I);
      Finally
        I.Free;
      End;
      If Token.Token = ')' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'ClassHeritage', ')',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parse the class visibility from the current token
  using the following object pascal grammar.

  @grammar ClassVisibility -> [ PUBLIC | PROTECTED | PRIVATE | PUBLISHED ]

  @precon  Scope is the current internal scope of the class.
  @postcon Parse the class visibility from the current token

  @param   Scope as a TScope as a reference

**)
procedure TPascalDocModule.ClassVisibility(var Scope : TScope);
begin
  While IsKeyWord(Token.Token, strScope) Do
    Begin
      If Token.UToken = 'PRIVATE' Then
        Scope := scPrivate
      Else If Token.UToken = 'PROTECTED' Then
        Scope := scProtected
      Else If Token.UToken = 'PUBLIC' Then
        Scope := scPublic
      Else
        Scope := scPublished;
      NextNonCommentToken;
    End;
end;

(**

  This method parses a class field list from the current token position using
  the following object pascal grammar.

  @grammar ObjFieldList -> ( ClassVisibility ObjFieldList ) / ';' ...

  @precon  Cls is a valid object declaration to add fields too and Scope is the
           current scope of the class.
  @postcon Returns true is field where handled and parsed.

  @param   Cls   as a TObjectDecl
  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.ClassFieldList(Cls: TObjectDecl; Scope: TScope): Boolean;

Begin
  ClassVisibility(Scope);
  Result := ObjFieldList(Cls, Scope);
  If Not Result Then
    Exit;
  If Token.Token = ';' Then
    NextNonCommentToken;
End;

(**

  This method parses a class method list from the current token position using
  the following object pascal grammar.

  @grammar MethodList -> ( ClassVisibility MethodList ) / ';' ...

  @precon  Cls is a valid object declaration to get method for and Scope is the
           current scope of the class.
  @postcon Returns true is method were parsed.

  @param   Cls   as a TObjectDecl
  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.ClassMethodList(Cls: TObjectDecl; Scope: TScope): Boolean;

Begin
  ClassVisibility(Scope);
  Result := MethodList(Cls, Scope);
  If Not Result Then
    Exit;
End;

(**

  This method parses a class property list frmo the current token position
  using the following object pascal grammar.

  @grammar ClassPropertyList -> ( ClassVisibility PropertyList ';' ) ...

  @precon  Cls is a valid class declaration to get method for and Scope is the
           current scope of the class.
  @postcon Returns true is properties were parsed.

  @param   Cls   as a TClassDecl
  @param   Scope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalDocModule.ClassPropertyList(Cls: TClassDecl;
  var Scope: TScope): Boolean;

Begin
  ClassVisibility(Scope);
  Result :=  PropertyList(Cls, Scope);
  If Not Result Then
    Exit;
  If Token.Token = ';' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strLiteralExpected, 'ClassPropertyList', ';',
      strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a class property list from the current
  token position using the following object pascal grammar.

  @grammar PropertyList -> PROPERTY Ident [ PropertyInterface ] PropertySpecifiers

  @precon  Cls is a valid class declaration to get method for and Scope is the
           current scope of the class.
  @postcon Returns true is properties were parsed.

  @param   Cls   as a TClassDecl
  @param   Scope as a TScope
  @return  a Boolean

**)
Function TPascalDocModule.PropertyList(Cls: TClassDecl; var Scope: TScope): Boolean;

Var
  P : TProperty;
  C : TComment;

begin
  ClassVisibility(Scope);
  Result := Token.UToken = 'PROPERTY';
  If Not Result Then
    Exit;
  C := GetComment;
  NextNonCommentToken;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
    Begin
      P := TProperty.Create(Token.Token, Scope, Token.Line, Token.Column);
      Cls.Properties.Add(P);
      P.Comment := C;
      NextNonCommentToken;
      PropertyInterface(P);
      PropertySpecifiers(P);
    End Else
      ErrorAndSeekToken(strIdentExpected, 'PropertyList', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the property interface from the current token position
  using the following object pascal grammar.

  @grammar PropertyInterface -> [ PropertyParameterList ] ':' Ident

  @precon  Prop is a property to parse an interface for.
  @postcon Parses the property interface from the current token position

  @param   Prop as a TProperty

**)
Procedure TPascalDocModule.PropertyInterface(Prop : TProperty);

Begin
  PropertyParameterList(Prop);
  // Check for property type id
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      Prop.TypeId := Token.Token;
      NextNonCommentToken;
    End;

End;

(**

  This method parses a properties parameter list from the current token using
  the following object pascal grammar.

  @grammar PropertyParameterList -> '[' ( IdentList ':' TypeId ) / ';' ... ']'

  @precon  Prop is a property to parse a parameter list for.
  @postcon Parses a properties parameter list from the current token

  @param   Prop as a TProperty

**)
Procedure TPascalDocModule.PropertyParameterList(Prop : TProperty);

Var
  ParamMod : TParamModifier;
  I : TIdentList;
  j : Integer;
  T : TTypeDecl;

Begin
  If Token.Token = '[' Then
    Begin
      Repeat
        NextNonCommentToken;
        ParamMod := pmNone;
        If Token.UToken = 'VAR' Then
          ParamMod := pmVar;
        If Token.UToken = 'CONST' Then
          ParamMod := pmConst;
        If Token.UToken = 'OUT' Then
          ParamMod := pmOut;
        If ParamMod <> pmNone Then
          NextNonCommentToken;
        I := IdentList(False, strSeekableOnErrorTokens);
        Try
          If Token.Token = ':' Then
            Begin
              NextNonCommentToken;
              T := GetTypeDecl;
              For j := 0 To I.Count - 1 Do
                Prop.Parameters.Add(TParameter.Create(ParamMod, I[j].Ident, False,
                  T, '', scPublic, I[j].Line, I[j].Col));
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'PropertyParameterList', ':',
                strSeekableOnErrorTokens, stActual);
        Finally
          I.Free;
        End;
      Until Token.Token <> ';';
      If Token.Token = ']' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'PropertyParameterList', ']',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses the property specifiers from the current token position
  using the following object pascal grammar.

  @grammar PropertySpecifiers -> [ INDEX ConstExpr ]
                                 [ READ Ident ]
                                 [ WRITE Ident ]
                                 [ STORED ( Ident | Constant) ]
                                 [ ( DEFAULT ConstExpr ) | NODEFAULT ]
                                 [ IMPLEMENTS TypeId ]

  @precon  Prop is a property to parse specifiers for.
  @postcon Parses the property specifiers from the current token position

  @param   Prop as a TProperty

**)
procedure TPascalDocModule.PropertySpecifiers(Prop: TProperty);
begin
  // Check for index
  If Token.UToken = 'INDEX' Then
    Begin
      NextNonCommentToken;
      While Not IsKeyWord(Token.UToken, strMethodDirectives) And (Token.Token <> ';') Do
        Begin
          Prop.IndexSpec := Token.Token;
          NextNonCommentToken;
        End;
    End;
  // Check for read
  If Token.UToken = 'READ' Then
    Begin
      NextNonCommentToken;
      While Not IsKeyWord(Token.Token, strPropSpecs) Do
        Begin
          If Prop.ReadSpec <> '' Then
            Prop.ReadSpec:= Prop.ReadSpec + #32;
          Prop.ReadSpec := Prop.ReadSpec + Token.Token;
          NextNonCommentToken;
        End;
    End;
  // Check for write
  If Token.UToken = 'WRITE' Then
    Begin
      NextNonCommentToken;
      While Not IsKeyWord(Token.Token, strPropSpecs) Do
        Begin
          If Prop.WriteSpec <> '' Then
            Prop.WriteSpec:= Prop.WriteSpec + #32;
          Prop.WriteSpec := Prop.WriteSpec + Token.Token;
          NextNonCommentToken;
        End;
    End;
  // Check for stored
  If Token.UToken = 'STORED' Then
    Begin
      NextNonCommentToken;
      While Not IsKeyWord(Token.Token, strPropSpecs) Do
        Begin
          If Prop.StoredSpec <> '' Then
            Prop.StoredSpec:= Prop.StoredSpec + #32;
          Prop.StoredSpec := Prop.StoredSpec + Token.Token;
          NextNonCommentToken;
        End;
    End;
  // Check for default
  If Token.UToken = 'DEFAULT' Then
    Begin
      NextNonCommentToken;
      While Not IsKeyWord(Token.Token, strPropSpecs) Do
        Begin
          If Prop.DefaultSpec <> '' Then
            Prop.DefaultSpec:= Prop.DefaultSpec + #32;
          Prop.DefaultSpec := Prop.DefaultSpec + Token.Token;
          NextNonCommentToken;
        End;
    End;
  If Token.UToken = 'NODEFAULT' Then
    NextNonCommentToken;
  // Check for implements
  If Token.UToken = 'IMPLEMENTS' Then
    Begin
      NextNonCommentToken;
      Prop.ImplementsSpec := Token.Token;
      NextNonCommentToken;
    End;
  If Token.UToken = 'READONLY' Then
    Begin
      Prop.ReadOnlySpec := True;
      NextNonCommentToken;
    End;
  If Token.UToken = 'WRITEONLY' Then
    Begin
      Prop.WriteOnlySpec := True;
      NextNonCommentToken;
    End;
  If Token.UToken = 'DISPID' Then
    Begin
      NextNonCommentToken;
      Prop.DispIDSpec := Token.Token;
      If Token.Token = '-' Then
        Begin
          NextNonCommentToken;
          Prop.DispIDSpec := Prop.DispIDSpec + Token.Token;
        End;
      NextNonCommentToken;
    End;
  // Check for ';'
  If Token.Token = ';' Then
    Begin
      NextNonCommentToken;
     // Check for default property
      If Token.UToken = 'DEFAULT' Then
        Begin
          Prop.DefaultProperty := True;
          NextNonCommentToken;
        End Else
          RollBackToken;
    End;
end;

(**

  This method parses an Interface declaration from the current token position
  using the following object pascal grammar.

  @grammar InterfaceType -> INTERFACE [ InterfaceHeritage ]
                            [ ClassMethodList ]
                            [ ClassPropertyList ]
                            END

  @precon  None.
  @postcon Returns an interface declaration if one was parsed else nil.

  @return  a TInterfaceDecl

**)
function TPascalDocModule.InterfaceType : TInterfaceDecl;

Var
  InternalScope : TScope;

begin
  InternalScope := scPublic;
  Result := Nil;
  If Not ((Token.UToken = 'INTERFACE') Or (Token.UToken = 'DISPINTERFACE')) Then
    Exit;
  If Token.UToken = 'INTERFACE' Then
    Result := TInterfaceDecl.Create
  Else
    Result := TDispInterfaceDecl.Create;
  SymbolTable.Add(Result);
  NextNonCommentToken;
  // If this class has not body then return
  If Token.Token = ';' Then
    Exit;
  // Get the classes heritage
  InterfaceHeritage(Result);
  // Get GUID if there is one
  If Token.Token = '[' Then
    Begin
      NextNonCommentToken;
      If Token.TokenType = ttStringLiteral Then
        Begin
          Result.GUID := Token.Token;
          NextNonCommentToken;
          If Token.Token = ']' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'InterfaceType', ']',
              strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strStringExpected, 'InterfaceType', '',
            strSeekableOnErrorTokens, stActual);
    End;
  Repeat
    ClassVisibility(InternalScope);
    If Token.UToken = 'END' Then
      Break;
  Until Not (
    ClassMethodList(Result, InternalScope) Or
    ClassPropertyList(Result, InternalScope)
  );
  // Check for 'END' and ';'
  If Token.UToken = 'END' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strReservedWordExpected, 'InterfaceType', 'END',
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method attempts to parse the current token position as a heritage list.

  @grammar InterfaceHeritage -> '(' IdentList ')'

  @precon  None.
  @postcon Attempts to parse the current token position as a heritage list.

  @param   InterfaceDecl as a TInterfaceDecl

**)
Procedure TPascalDocModule.InterfaceHeritage(InterfaceDecl : TInterfaceDecl);

begin
  ClassHeritage(InterfaceDecl); // Same as ClassHeritage
End;

(**

  This method parses a requires clause from the current token position usnig
  the following object pascal grammar.

  @grammar RequiresClause -> REQUIRES IdentList ... ';'

  @precon  None.
  @postcon Parses a requires clause from the current token position usnig

**)
Procedure TPascalDocModule.RequiresClause;

Var
  Comment : TComment;

Begin
  If Not (Token.UToken = 'REQUIRES') Then
    Exit;
  Comment := GetComment;
  NextNonCommentToken;
  Requires := IdentList(True, strSeekableOnErrorTokens);
  Requires.Comment := Comment;
  If Token.Token <> ';' Then
    ErrorAndSeekToken(strLiteralExpected, 'RequiresClause', ';',
      strSeekableOnErrorTokens, stActual);
  NextNonCommentToken;
End;

(**

  This method parses a contains clause fro the cutrrent token position using
  the following object pascal grammar.

  @grammar ContainsClause -> CONTAINS IdentList ... ';'

  @precon  None.
  @postcon Parses a contains clause fro the cutrrent token position

**)
Procedure TPascalDocModule.ContainsClause;

Var
  Comment : TComment;

Begin
  If Not (Token.UToken = 'CONTAINS') Then
    Exit;
  Comment := GetComment;
  NextNonCommentToken;
  Contains := IdentList(True, strSeekableOnErrorTokens);
  Contains.Comment := Comment;
  If Token.Token <> ';' Then
    ErrorAndSeekToken(strLiteralExpected, 'ContainsClause', ';',
      strSeekableOnErrorTokens, stActual);
  NextNonCommentToken;
End;

(**

  This method creates a identifier list starting at the current token and
  return the list to the calling function. If OwnList is true then the identlist
  is added to the classes owned items list for automatic disposal, else it the
  responsibliity of the calling function to disposal of the class.

  @grammar IdentList -> Ident / ',' ...

  @precon  OwnList determines if the identlist should be disposed of be the
           parser or be the caller. SeekTokens is a sorted lowercase list of
           token to find if an error is found.
  @postcon Returns an ident list.

  @param   OwnList    as a Boolean
  @param   SeekTokens as an Array of String
  @return  a TIdentList

**)
Function TPascalDocModule.IdentList(OwnList : Boolean;
  SeekTokens : Array Of String) : TIdentList;

Begin
  Result := TIdentList.Create;
  // Add to owned list immediately if required to ensure that is an exception
  // is raised the memory is released in the destructor
  If OwnList Then
    OwnedItems.Add(Result);
  Repeat
    Result.Add(Token.Token, Token.Line, Token.Column, GetComment);
    NextNonCommentToken;
    If Token.UToken = 'IN' Then
      Begin
        NextNonCommentToken;
        If Token.TokenType <> ttStringLiteral Then
          ErrorAndSeekToken(strStringExpected, 'IdentList', Token.Token,
            SeekTokens, stActual)
        Else
          NextNonCommentToken;
      End;
  Until Not IsToken(',', Nil);
End;

(**

  This method returns a type id at the current token position using the
  following object pascal grammar.

  @grammar TypeId -> [ UnitId '.' ] <type-identifier>

  @precon  C must be a valid generic container.
  @postcon Returns a type id as a string of text.

  @param   C as a TGenericContainer
  @return  a Boolean

**)
Function TPascalDocModule.TypeId(C: TGenericContainer) : Boolean;

Begin
  Result := Token.TokenType = ttIdentifier;
  If Result Then
    Begin
      AddToExpression(C);
      If Token.Token = '.' Then
        Begin
          AddToExpression(C);
          If Token.TokenType = ttIdentifier Then
            AddToExpression(C)
          Else
            ErrorAndSeekToken(strIdentExpected, 'TypeId', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End;
    End;
End;

(**

  This method parses a constant expression from the current token position
  using the following object pascal grammar.

  @grammar ConstExpr -> <constant-expression>

  @precon  C is a generic container to add tokens too.
  @postcon Returns true if a constant expression was parsed.

  @param   C as a TGenericContainer
  @param   ExprType as a TExprTypes
  @return  a Boolean

**)
Function TPascalDocModule.ConstExpr(C : TGenericContainer;
  var ExprType : TExprTypes) : Boolean;

Begin
  Result := True;
  Expression(C, ExprType); // ConstExpr is a subset of Expression
End;

(**

  This method is a catch all handler for declarations so that the code tries
  to find the next declaration in the event of a code error.

  @precon  None.
  @postcon A catch all handler for declarations so that the code tries
           to find the next declaration in the event of a code error.

  @return  a Boolean

**)
Function TPascalDocModule.UndefinedToken : Boolean;

Begin
  Result := Not IsKeyWord(Token.Token, strReservedWords);
  If Result Then
    Begin
      ErrorAndSeekToken(strUndefinedToken, 'UndefinedToken', Token.Token,
        strSeekableOnErrorTokens, stActual);
      If Token.Token = ';' Then
        NextNonCommentToken;
    End;
End;

{ TPascalDocModuleList }

(**

  This method add the source pascal doc module to the collection.

  @precon  Source is a valid instance of a TPascalDocModule class.
  @postcon Add the source pascal doc module to the collection.

  @param   Source as a TPascalDocModule

**)
procedure TPascalDocModuleList.Add(Source: TPascalDocModule);
begin
  If Source <> Nil Then FModules.Add(Source);
end;

(**

  This is the constructor method for the TPascalDocModuleList class.

  @precon  None.
  @postcon Initialises the collection.

**)
constructor TPascalDocModuleList.Create;
begin
  FModules := TObjectList.Create(True);
end;

(**

  This method deletes the indexed module from the collection.

  @precon  iIndex is the index of the module to delete.
  @postcon Deletes the indexed module from the collection.

  @param   iIndex as an Integer

**)
procedure TPascalDocModuleList.Delete(iIndex: Integer);
begin
  FModules.Delete(iIndex);
end;

(**

  This is the destructor method for the TPascalDocModuleList class.

  @precon  None.
  @postcon Destroys the collection.

**)
destructor TPascalDocModuleList.Destroy;
begin
  FModules.Free;
  inherited;
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of modules in the collection.

  @return  an Integer

**)
function TPascalDocModuleList.GetCount: Integer;
begin
  Result := FModules.Count;
end;

(**

  This is a getter method for the Module property.

  @precon  iIndex is the index of the module required.
  @postcon Returns the modules as an instance of a TPascalDocModule.

  @param   iIndex as an Integer
  @return  a TPascalDocModule

**)
function TPascalDocModuleList.GetModule(iIndex: Integer): TPascalDocModule;
begin
  Result := FModules[iIndex] As TPascalDocModule;
end;

End.
