(**

  ObjectPascalModule : A unit to tokenize Pascal source code.

  @todo       Handle conditional compilation. Now there's a dilema here do I
              handle it at the token level or during the parsing of the source?
  @todo       Allow for the handling of multiple errors - Whats the best way to
              do this?

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
  @Date       05 Jun 2006
  @Author     David Hoyle

**)
Unit PascalDocModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule;

Type
  (** This enumerate described the different type of ident list that can be
      found in the language which have different behaviours. **)
  TIdentListType = (iltStandIdentList, iltUsesClause, iltIndexedEnumerates);
  
  TConstExprType = (cetUnknown, cetString, cetNumeric);

  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TPascalDocModule = Class(TBaseLanguageModule)
  Private
    FConstExprType : TConstExprType;
    { Grammer Parsers }
    Procedure Goal;
    Function OPProgram : Boolean;
    Function OPUnit : Boolean;
    Function OPPackage : Boolean;
    Function OPLibrary : Boolean;
    Procedure ProgramBlock;
    procedure UsesClause;
    Procedure InterfaceSection;
    Procedure InterfaceDecl;
    Function ExportedHeading : Boolean;
    Function ExportedProcs : Boolean;
    Procedure ImplementationSection;
    Procedure Block(Scope : TScope; Method : TMethodDecl);
    Procedure DeclSection(Scope : TScope; Method : TMethodDecl);
    Function LabelDeclSection : Boolean;
    Function ConstSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function ConstantDecl(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function ResStringSection(Scope: TScope; Method : TMethodDecl): Boolean;
    Function ResourceStringDecl(Scope: TScope; Method : TMethodDecl): Boolean;
    Function TypeSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function TypeDecl(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function GetTypeDecl : TTypes;
    Function TypedConstant(C: TGenericContainer) : Boolean;
    Function ArrayConstant(C: TGenericContainer) : Boolean;
    Function RecordConstant(C: TGenericContainer) : Boolean;
    Procedure RecordFieldConstant;
    function OPType : TTypes;
    function RestrictedType : TTypes;
    Function ClassRefType : TClassRefType;
    Function SimpleType : TTypes;
    function RealType : TRealType;
    function OrdinalType : TOrdinalType;
    function OrdIdent : TOrdinalType ;
    Function VariantType : TVariantType;
    function SubRangeType : TOrdinalType;
    function EnumerateType : TOrdinalType;
    Function StringType : TStringType;
    Function StrucType : TTypes;
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
    Procedure Expression;
    Procedure SimpleExpression;
    Procedure Term;
    Procedure Factor;
    Procedure RelOp;
    Procedure AddOp;
    Procedure MulOp;
    Procedure Designator;
    Procedure SetConstructor;
    Procedure SetElement;
    Procedure ExprList;
    Procedure Statement;
    Procedure StmtList;
    Procedure SimpleStatement;
    Procedure StructStmt;
    Procedure CompoundStmt;
    Procedure ConditionalStmt;
    Procedure IfStmt;
    Procedure CastStmt;
    Procedure CaseSelector;
    Procedure CaseLabel;
    Procedure LoopStmt;
    Procedure RepeatStmt;
    Procedure WhileStmt;
    Procedure ForStmt;
    Procedure WithStmt;
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
    Procedure ObjHeritage;
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
    Procedure InterfaceHeritage;
    Procedure RequiresClause;
    procedure ContainsClause;
    Function IdentList(OwnList : Boolean; IdentListType : TIdentListType;
      SeekTokens : Array Of String): TIdentList;
    // Procedure QualId;
    function TypeId: String;
    // Procedure Ident;
    Function ConstExpr(C: TGenericContainer) : Boolean;
    // Procedure UnitId;
    // Procedure LabelId;
    // Procedure Number;
    // Procedure OpString;
    (* Helper method to the grammer parsers *)
    Function ProcedureBit(ProcType : TMethodType; Scope : TScope) : TMethodDecl;
    procedure ExportsEntry;
    procedure ExportsList;
    Procedure SkipStatements(var iBlockCount : Integer);
    Procedure ScopeImplementedMethods;
    procedure Sort;
    Procedure ParseTokens;
    procedure ErrorAndSeekToken(strMsg, strMethod, strExpected: String;
      SeekTokens: array of string);
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
  strReservedWords : Array[1..63] Of String = (
    'and', 'array', 'as', 'asm', 'begin', 'case',	'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto', 'else',
    'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for',
    'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    {'nil', }'not', 'object', 'of', 'or', 'out', 'packed', 'procedure', 'program',
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

  (** An array of parameter modifier phases. **)
  strModifier : Array[pmNone..pmOut] Of String = ('', ' as a reference',
    ' constant', ' as out');
  (** A simple array for outputting a or an. **)
  strAOrAn : Array[False..True] Of String = ('a', 'an');
  (** A list of vowels. **)
  strVowels : Set Of Char = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'];

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..89] Of String = (';',
    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
    'automated', 'begin', 'case', 'class', 'const', 'constructor', 'contains',
    'default', 'destructor', 'dispid', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'implements', 'in',
    'index', 'inherited', 'initialization', 'inline', 'interface', 'is',
    'label', 'library', 'mod', 'name', 'near', 'nodefault', 'not', 'object',
    'of', 'or', 'out', 'package', 'packed', 'private', 'procedure', 'program',
    'property', 'protected', 'public', 'published', 'raise', 'read', 'readonly',
    'record', 'repeat', 'requires', 'resident', 'resourcestring', 'set', 'shl',
    'shr', 'stored', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'varargs', 'while', 'with', 'write', 'writeonly', 'xor'
  );
  
  strConstExprFuncs : Array[1..15] Of String = ('Abs', 'Chr', 'Hi', 'High',
    'Length', 'Lo', 'Low', 'Odd', 'Ord', 'Pred', 'Round', 'SizeOf', 'Succ',
    'Swap', 'Trunc');
    
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
  Inherited Create(Source, strReservedWords, strDirectives, IsModified,
    strFileName);
  If moParse In ModuleOptions Then ParseTokens;
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
  If Not (PropertyList(Result, Scope) And (Result.PropertyCount > 0)) Then
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
      With Types[i] As TObjectDecl Do
        For j := 0 To Methods.Count -1 Do
          If ImplementedMethods.Find(Identifier, Methods[j].Identifier) = -1 Then
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

**)
Procedure TPascalDocModule.ErrorAndSeekToken(strMsg, strMethod, strExpected : String;
  SeekTokens: Array of string);

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
  While Not IsKeyWord(Token.Token, SeekTokens) Do
    Begin
      NextNonCommentToken;
      If (Token.UToken = 'CLASS') And (PrevToken.Token = '=') Then
        Begin
          While Not IsKeyWord(Token.Token, [';']) Do
            NextNonCommentToken;
          Exit;
        End;
    End;
End;

(**

  This method is the starting position for the parsing of an object pascal
  module. It finds the first non comment token and begins the grammer checking
  from their by deligating to the program, library, unit and package methods.

  @grammar Goal -> ( Program | Package | Library | Unit )

  @precon  None.
  @postcon It finds the first non comment token and begins the grammer checking
           from their by deligating to the program, library, unit and package
           methods.

**)
procedure TPascalDocModule.Goal;

begin
  Try
    If TokenCount = 0 Then
      Exit;
    // Find first non comment token
    While (Token.TokenType In [ttComment, ttCompilerDirective]) And
      Not EndOfTokens Do
      NextToken;
    // Check for end of file else must be identifier
    If Not EndOfTokens Then
      Begin
        ModuleComment := GetComment;
        Repeat
          {Do Nothing}
        Until Not (OPProgram Or OPLibrary Or OPPackage Or OPUnit);
      End Else
        Raise EDocException.Create(strUnExpectedEndOfFile);
  Except
    On E : Exception Do
      Errors.Add(E.Message, 'Goal', 0, 0, etError);
  End;
end;

(**

  This method parses a Program declaration from the current token
  position using the following object pascal grammer.

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
  If Token.TokenType <> ttIdentifier Then
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'OPProgram');
  ModuleName := Token.Token;
  ModuleNameLine := Token.Line;
  ModuleNameCol := Token.Column;
  NextNonCommentToken;
  // In the Program module we need to check for '(' Ident List ')' but discard
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      IdentList(True, iltStandIdentList, strSeekableOnErrorTokens); // get ident list
      // Check for closing parenthesis
      If Token.Token <> ')' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPProgram', ')',
          strSeekableOnErrorTokens)
      Else
        NextNonCommentToken;
    End;
  // Check for ';'
  If Token.Token <> ';' Then
    ErrorAndSeekToken(strLiteralExpected, 'OPProgram', ';',
      strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  ProgramBlock;
  // Check for '.'
  If Token.Token <> '.' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, '.', Token,
      'OPPRogram');
end;

(**

  This method parses a unit declaration from the current token position using
  the following object pascal grammer.

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
  If Not Result Then
    Exit;
  ModuleType := mtUnit;
  NextNonCommentToken;
  If Token.TokenType <> ttIdentifier Then
    ErrorAndSeekToken(strIdentExpected, 'OPUnit', Token.Token,
      strSeekableOnErrorTokens)
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
      strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  InterfaceSection;
  ImplementationSection;
  InitSection;
  // Check for '.'
  If Token.Token <> '.' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, '.', Token, 'OPUnit');
End;

(**

  This method parses a package declaration from the current token position
  using the following object pascal grammer.

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
  If Not Result Then
    Exit;
  ModuleType := mtPackage;
  NextNonCommentToken;
  If Token.TokenType <> ttIdentifier Then
    ErrorAndSeekToken(strIdentExpected, 'OPPackage', Token.Token,
      strSeekableOnErrorTokens)
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
      strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  // Look for requires and contains clauses
  RequiresClause;
  ContainsClause;
  If Token.UToken <> 'END' Then
    ErrorAndSeekToken(strReservedWordExpected, 'OPPackage', 'END',
      strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  If Token.Token <> '.' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, '.', Token,
      'OPPackage');
end;

(**

  This method parses the Library declaration from the current token using the
  following object pascal grammer.

  @grammar Library -> LIBRARY Ident ';'
                      ProgramBlock '.'

  @precon  None.
  @postcon Returns true is a library section was parsed.

  @return  a Boolean

**)
Function TPascalDocModule.OPLibrary : Boolean;

begin
  Result := Token.UToken = 'LIBRARY';
  If Not Result Then
    Exit;
  ModuleType := mtLibrary;
  NextNonCommentToken;
  If Token.TokenType <> ttIdentifier Then
    ErrorAndSeekToken(strIdentExpected, 'OPLibrary', Token.Token,
      strSeekableOnErrorTokens)
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
      strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  ProgramBlock;
  // Check for '.'
  If Token.Token <> '.' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, '.', Token,
      'OPLibrary');
end;

(**

  This method parses a program block from the current token position using
  the following object pascal grammer.

  @grammar ProgramBlock -> [ UsesClause ]
                           Block

  @precon  None.
  @postcon Parses a program block from the current token position using the
           following object pascal grammer.

**)
procedure TPascalDocModule.ProgramBlock;
begin
  UsesClause;
  Block(scPublic, Nil);
end;

(**

  This method parses the Uses clause declaration from the current token
  position using the following object pascal grammer.

  @grammer Uses -> USES IdentList ';'

  @precon  None.
  @postcon Parses the Uses clause declaration from the current token position
           using the following object pascal grammer.

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
          UsesCls := IdentList(True, iltUsesClause, strSeekableOnErrorTokens);
          UsesCls.Comment := Comment;
        End Else
        Begin
          UsesCls.Assign(IdentList(True, iltUsesClause, strSeekableOnErrorTokens));
          If UsesCls.Comment <> Nil Then
            UsesCls.Comment.Assign(Comment)
          Else
            UsesCls.Comment := Comment;
        End;
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'UsesClause', ';',
          strSeekableOnErrorTokens)
      Else
        NextNonCommentToken;
    End;
End;

(**

  This method parses an interface section from the current token position using
  the following object pascal grammer.

  @grammar  InterfaceClause -> INTERFACE
                               [ UsesClause ]
                               [ InterfaceDecl ] ...

  @precon  None.
  @postcon Parses an interface section from the current token position using
           the following object pascal grammer.

**)
Procedure TPascalDocModule.InterfaceSection;

Begin
  If Token.UToken <> 'INTERFACE' Then
    ErrorAndSeekToken(strReservedWordExpected, 'InterfaceSection', 'INTERFACE',
      strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  UsesClause;
  InterfaceDecl;
End;

(**

  This method parses an interface declaration from the current token position
  using the following object pascal grammer.

  @grammar InterfaceDecl -> ConstSection
                            ResStringSection
                            TypeSection
                            VarSection
                            ThreadvarSection
                            ExportedHeading
                            ExportedProcs

  @precon  None.
  @postcon Parses an interface declaration from the current token position
           using the following object pascal grammer.

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
    ExportedProcs
  );
End;

(**

  This method parses a exported heading declaration section from the current
  token position using the following object pascal grammer.

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
        ExportedHeadings.Add(M);
        Result := True;
        // Check for ';'
        If Token.Token = ';' Then
          Begin
            NextNonCommentToken;
            Directive(M);
          End Else
            ErrorAndSeekToken(strLiteralExpected, 'ExportedHeading', ';',
              strSeekableOnErrorTokens);
      End;
  Until M = Nil;
End;

(**

  This method parses an exported procedure section from the current token
  position.

  @grammar ExportedProcs -> EXPORTS ExportsList ';'

  @precon  none.
  @postcon Returns true if an exported procedure was found.

  @return  a Boolean

**)
Function TPascalDocModule.ExportedProcs : Boolean;

Begin
  Result := Token.UToken = 'EXPORTS';
  If Result Then
    Begin
      ExportsList;
      If Token.Token = ';' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'ExportedProcs', ';',
          strSeekableOnErrorTokens);
    End;
End;

(**

  This method parses an exports list from the current token position using the
  following object pascal grammer.

  @grammar ExportsList -> ExportsEntry / ',' ...

  @precon  None.
  @postcon Parses an exports list from the current token position.

**)
Procedure TPascalDocModule.ExportsList();

Begin
  Repeat
    NextNonCommentToken;
    ExportsEntry;
  Until Token.Token <> ',';
End;

(**

  This method parses an exports entry from the current token position using the
  following object pascal grammer.

  @grammar ExportsEntry -> Ident [ INDEX IntegerConstant [ NAME StringConstant ]
             [ RESIDENT ] ]</TD>

  @precon  None.
  @postcon Parses an exports entry from the current token position.

**)
Procedure TPascalDocModule.ExportsEntry;

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
      // Check RESIDENT
      If Token.UToken = 'RESIDENT' Then
        Begin
          E.Add(Token.Token);
          NextNonCommentToken;
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'ExportsEntry', Token.Token,
        strSeekableOnErrorTokens);
End;

(**

  This method parses an implementation section from the current token position
  using the following object pascal grammer.

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
      'IMPLEMENTATION', strSeekableOnErrorTokens)
  Else
    NextNonCommentToken;
  UsesClause;
  DeclSection(scPrivate, Nil);
End;

(**

  This method parses a block section from the current token position using the
  following object pascal grammer.

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

Begin
  DeclSection(Scope, Method);
  CompoundStmt;
End;

(**

  This method parses a declaration section from the current token position using
  the following object pascal grammer.

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
    ExportedProcs
  );
End;

(**

  This method parses a label declaration section from the current token
  position using the following object pascal grammer.

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
      IdentList(True, iltStandIdentList, strSeekableOnErrorTokens);
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'LabelDeclSection', ';',
          strSeekableOnErrorTokens)
      Else
        NextNonCommentToken;
    End;
End;

(**

  This method parses a constant section declaration from the current token
  position using the following object pascal grammer.

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
            strSeekableOnErrorTokens);
    End;
End;

(**

  This method parses a constant declaration from the current token position
  using the following object pascal grammer.

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
  T : TTypes;

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
      //: @todo Need to return to this later to implement ConstExpr properly!
      If Token.Token = '=' Then        // ConstExpr
        Begin
          C.Add(Token.Token);
          NextNonCommentToken;
          ConstExpr(C)
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
              TypedConstant(C);
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'ConstantDecl', '=',
                strSeekableOnErrorTokens);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ConstantDecl', '= or :',
            strSeekableOnErrorTokens);
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

Begin
  Result := False;
  FConstExprType := cetString;
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
          //: @todo Implement this as a constant expression of TYPE String.
          ConstExpr(C);
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ResourceStringDecl', '=',
            strSeekableOnErrorTokens);
    End;
End;

(**

  This method parses a type section from the current token position using the
  following object pascal grammer.

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
            strSeekableOnErrorTokens);
    End;
End;

(**

  This method parses a type declaration section from the current token position
  using the following object pascal grammer.

  @grammar TypeDecl -> Ident '=' Type
                    -> Ident '=' RestrictedType

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypes

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
              ErrorAndSeekToken(strTypeNotFound, 'TypeDecl', '',
                strSeekableOnErrorTokens);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'TypeDecl', '=',
            strSeekableOnErrorTokens)
    End;
End;

Function TPascalDocModule.GetTypeDecl : TTypes;

Begin
  Result := OPType;
  If Result = Nil Then
    Result := RestrictedType;
End;

(**

  This method parses a typed constant from the current token position using
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>TypedConstant</B></TD>
      <TD>-></TD>
      <TD>( ConstExpr | ArrayConstant | RecordConstant )</TD>
    </TR>
    <TR>
      <TD><B>ArrayConstant</B></TD>
      <TD>-></TD>
      <TD>'(' TypedConstant / ',' ... ')'</TD>
    </TR>
    <TR>
      <TD><B>RecordConstant</B></TD>
      <TD>-></TD>
      <TD>'(' RecordFieldConstant / ';' ... ')'</TD>
    </TR>
    <TR>
      <TD><B>RecordFieldConstant</B></TD>
      <TD>-></TD>
      <TD>Ident ':' TypedConstant</TD>
    </TR>
  </TABLE>

  @precon  C is a valid instance of the constant to be populated with tokens.
  @postcon Returns false if this was not a typed constant an not handled.

  @param   C as a TGenericContainer
  @return  a Boolean

**)
Function TPascalDocModule.TypedConstant(C : TGenericContainer) : Boolean;

Begin
  Result := ArrayConstant(C) Or RecordConstant(C) Or ConstExpr(C);
End;

Function TPascalDocModule.ArrayConstant(C : TGenericContainer) : Boolean;

Begin
  Result := False;
  //: @todo Requires implementing at the same time as ConstExpr
end;

Function TPascalDocModule.RecordConstant(C : TGenericContainer) : Boolean;

Begin
  Result := False;
  //: @todo Requires implementing at the same time as ConstExpr
End;

Procedure TPascalDocModule.RecordFieldConstant;

Begin
End;

(**

  This method parses a type from the current token position using the following
  object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Type</B></TD>
      <TD>-></TD>
      <TD>TypeId</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>SimpleType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>StrucType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>PointerType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>StringType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ProcedureType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>VariantType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ClassRefType</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypes

**)
Function TPascalDocModule.OPType : TTypes;

Begin
  Result := ClassRefType;
  If Result = Nil Then Result := VariantType;
  If Result = Nil Then Result := ProcedureType;
  If Result = Nil Then Result := StringType;
  If Result = Nil Then Result := PointerType;
  If Result = Nil Then Result := StrucType;
End;

(**

  This method parses a restricted type from the current token position using
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>RestrictedType</B></TD>
      <TD>-></TD>
      <TD>ObjectType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ClassType</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>InterfaceType</TD>
    </TR>
  </TABLE>

  @note    The simpleType() method is here to act as a catch all for types that
           have note been previously handled.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypes

**)
Function TPascalDocModule.RestrictedType : TTypes;

Begin
  Result := ObjectType;
  If Result = Nil Then Result := ClassType;
  If Result = Nil Then Result := InterfaceType;
  If Result = Nil Then Result := SimpleType;
End;

(**

  This method parses a class reference type declaration from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ClassRefType</B></TD>
      <TD>-></TD>
      <TD>CLASS OF TypeId</TD>
    </TR>
  </TABLE>

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
  Result.Add(TypeId);
End;

(**

  This method parses a simple type declaration from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>SimpleType</B></TD>
      <TD>-></TD>
      <TD>( OrdinalType | RealType )</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypes

**)
function TPascalDocModule.SimpleType : TTypes;

begin
  Result := RealType;
  If result = Nil Then Result := OrdinalType;
end;

(**

  This method determines if the token represents a real type using the following
  object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>RealType</B></TD>
      <TD>-></TD>
      <TD>REAL48</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>REAL</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>SINGLE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>DOUBLE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>EXTENDED</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>CURRENCY</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>COMP</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TRealType

**)
Function TPascalDocModule.RealType : TRealType;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strRealTypes) Then Exit;
  Result := TRealType.Create;
  SymbolTable.Add(Result);
  Result.Add(Token.Token);
  NextNonCommentToken;
End;

(**

  This method determines if the type is an ordinal type using the folowing
  object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>OrdinalType</B></TD>
      <TD>-></TD>
      <TD>( SubrangeType | EnumerateType | OrdIndent )</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TOrdinalType

**)
Function TPascalDocModule.OrdinalType : TOrdinalType;

Begin
  Result := OrdIdent;
  If Result = Nil Then Result := EnumerateType;
  If Result = Nil Then Result := SubRangeType;
End;

(**

  This method determines if the current token is an ordinal ident using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>OrdIndent</B></TD>
      <TD>-></TD>
      <TD>SHORTINT</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>SMALLINT</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>INTEGER</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>BYTE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>LONGINT</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>INT64</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>WORD</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>BOOLEAN</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>CHAR</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>WIDECHAR</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>LONGWORD</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>PCHAR</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TOrdinalType

**)
Function TPascalDocModule.OrdIdent : TOrdinalType;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strOrdIdents) Then
    Exit;
  Result := TOrdinalType.Create;
  SymbolTable.Add(Result);
  Result.Add(Token.Token);
  NextNonCommentToken;
End;

(**

  This method parses a variant type declaration section using the following
  object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>VariantType</B></TD>
      <TD>-></TD>
      <TD>VARIANT</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>OLEVARIANT</TD>
    </TR>
  </TABLE>

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
  the following object pascal grammer. This method also currently acts as a type
  CATCH ALL if nothing else works.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>SubrangeType</B></TD>
      <TD>-></TD>
      <TD>ConstExpr .. ConstExpr</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @return  a TOrdinalType

**)
Function TPascalDocModule.SubRangeType : TOrdinalType;

Begin
  //: @todo This needs implementing properly.
  Result := TOrdinalType.Create;
  SymbolTable.Add(Result);
  ConstExpr(Result);
End;

(**

  This method parses an enumerate type from the current token position using
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>EnumerateType</B></TD>
      <TD>-></TD>
      <TD>'(' IdentList ')'</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @return  a TOrdinalType

**)
Function TPascalDocModule.EnumerateType : TOrdinalType;

Var
  I : TIdentList;

Begin
  Result := Nil;
  If Not (Token.Token = '(') Then
    Exit;
  Result := TOrdinalType.Create;
  SymbolTable.Add(Result);
  NextNonCommentToken;
  Result.Add('(');
  I := IdentList(False, iltIndexedEnumerates, []); //: @todo SeekTokens(EnumerateType)
  Try
    Result.Add(I.AsString);
  Finally
    I.Free;
  End;
  If Token.Token <> ')' Then
    ErrorAndSeekToken(strLiteralExpected, 'EnumerateType', ')',
      strSeekableOnErrorTokens)
  Else
    Begin
      Result.Add(')');
      NextNonCommentToken;
    End;
End;

(**

  This method parses a sring type declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>StringType</B></TD>
      <TD>-></TD>
      <TD>STRING</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ANSISTRING</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>WIDESTRING</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>STRING '[' ConstExpr ']'</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TStringType

**)
Function TPascalDocModule.StringType : TStringType;

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
      ConstExpr(Result);
      If Token.Token = ']' Then
        Begin
          Result.Add(Token.Token);
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'StringType', ']',
            strSeekableOnErrorTokens);
    End;
end;

(**

  This method parses an Array, Set of File type declaration from the current
  token position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>StrucType</B></TD>
      <TD>-></TD>
      <TD>[ PACKED ] ( ArrayType | SetType | FileType | RecType )</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypes

**)
Function TPascalDocModule.StrucType : TTypes;

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
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ArrayType</B></TD>
      <TD>-></TD>
      <TD>ARRAY [ '[' OrdinalType / ',' ... ']' ] OF Type</TD>
    </TR>
  </TABLE>

  @precon  boolPacked determines if the array type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @return  a TArrayType

**)
Function TPascalDocModule.ArrayType(boolPacked : Boolean) : TArrayType;

Begin
  Result := Nil;
  If Not (Token.UToken = 'ARRAY') Then Exit;
  Result := TArrayType.Create;
  SymbolTable.Add(Result);
  If boolPacked Then
    Result.Add('Packed');
  Result.Add(Token.Token);
  NextNonCommentToken;
  If Token.Token = '[' Then
    Begin
      Result.Add(Token.Token);
      NextNonCommentToken;
      While Token.Token <> ']' Do
        Begin
          Result.Add(Token.Token);
          NextNonCommentToken;
        End;
      Result.Add(Token.Token);
      NextNonCommentToken;
    End;
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
      ErrorAndSeekToken(strReservedWordExpected, 'ArrayType', 'OF',
        strSeekableOnErrorTokens);
End;

(**

  Method parses a set type declaration from the current token position using
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>SetType</B></TD>
      <TD>-></TD>
      <TD>SET OF OrdinalType</TD>
    </TR>
  </TABLE>

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
        strSeekableOnErrorTokens);
End;

(**

  This method parses a file type declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>FileType</B></TD>
      <TD>-></TD>
      <TD>FILE OF Type</TD>
    </TR>
  </TABLE>

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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>RecType</B></TD>
      <TD>-></TD>
      <TD>RECORD [ FieldList ] END</TD>
    </TR>
  </TABLE>

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
      strSeekableOnErrorTokens);
end;

(**

  This method parses a field list for classes, records and object declarations
  from the current token position.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>FieldList</B></TD>
      <TD>-></TD>
      <TD>FieldDecl / ';' ... [ VariantSection ] [ ';' ]</TD>
    </TR>
  </TABLE>

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
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>FieldDecl</B></TD>
      <TD>-></TD>
      <TD>IdentList ':' Type</TD>
    </TR>
  </TABLE>

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
  T : TTypes;

Begin
  I := IdentList(False, iltStandIdentList, []); //: @todo SeekTokens(FieldDecl)
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
          strSeekableOnErrorTokens);
  Finally
    I.Free;
  End;
End;

(**

  This method parses the variant section of a record from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>VariantSection</B></TD>
      <TD>-></TD>
      <TD>CASE [ Ident ':' ] TypeId OF RecVariant / ';' ...</TD>
    </TR>
  </TABLE>

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
  token position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>RecVariant</B></TD>
      <TD>-></TD>
      <TD>ConstExpr / ',' ... ':' '(' [ FieldList ] ')'</TD>
    </TR>
  </TABLE>

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses the record variant section of a record from the current
           token position

  @param   Rec as a TRecordDecl

**)
Procedure TPascalDocModule.RecVariant(Rec : TRecordDecl);

Var
  C : TGenericContainer;

Begin
  C := TGenericContainer.Create('tmp', scPrivate, 0, 0);
  Try
    Repeat
      If Token.Token = ',' Then NextNonCommentToken;
      ConstExpr(C);
    Until Token.Token <> ',';
    If Token.Token <> ':' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token,
        'RecVariant');
    NextNonCommentToken;
    If Token.Token <> '(' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, '(', Token,
        'RecVariant');
    NextNonCommentToken;
    FieldList(Rec);
    If Token.Token <> ')' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ')', Token,
        'RecVariant');
    NextNonCommentToken;
  Finally
    C.Free;
  End;
End;

(**

  This method parses a pointer type declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>PointerType</B></TD>
      <TD>-></TD>
      <TD>'^' TypeId</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TPointerType

**)
Function TPascalDocModule.PointerType : TPointerType;

begin
  Result := Nil;
  If Not (Token.Token = '^') Then
    Exit;
  Result := TPointerType.Create;
  SymbolTable.Add(Result);
  Result.Add('^');
  NextNonCommentToken;
  Result.Add(TypeId);
end;

(**

  This method parses a procedure type declaration from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ProceduralType</B></TD>
      <TD>-></TD>
      <TD>( ProcedureHeading | FunctionHeading ) [ OF OBJECT ]</TD>
    </TR>
  </TABLE>

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
          Raise EDocException.CreateLiteral(strReservedWordExpected, 'OBJECT',
            Token, 'ProcedureType');
        boolOfObject := True;
        NextNonCommentToken;
      End;
    Directive(M);
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
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>VarSection</B></TD>
      <TD>-></TD>
      <TD>VAR ( VarDecl ';' ) ...</TD>
    </TR>
  </TABLE>

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
        Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
          'VarSection');
      NextNonCommentToken;
    End;
End;

(**

  This method parses a Thread var section declatation from the current token
  position.

  @see     For object pascal grammer see {@link TPascalDocModule.VarSection}.

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
        Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
          'ThreadVarSection');
      NextNonCommentToken;
    End;
End;

(**

  This method parses a variable declaration from the current token position.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>VarDecl</B></TD>
      <TD>-></TD>
      <TD>IdentList ':' Type [ ( ABSOLUTE ( Ident | ConstExpr ) ) | '=' ConstExpr ] </TD>
    </TR>
  </TABLE>

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
  T : TTypes;
  C : TGenericContainer;

Begin
  Result := False;
  If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
    Exit;
  // Get ident list line and column
  I := IdentList(False, iltStandIdentList, []); //: @todo SeekTokens(VarDecl)
  Try
    If Token.Token <> ':' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token, 'VarDecl');
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
          ConstExpr(C);
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
          ConstExpr(C);
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
        V.Comment := I[j].Comment;
        VarSection.Add(V);
      End;
    Result := True;
  Finally
    I.Free;
  End;
End;

Procedure TPascalDocModule.Expression;

Begin
End;

Procedure TPascalDocModule.SimpleExpression;

Begin
End;

Procedure TPascalDocModule.Term;

Begin
End;

Procedure TPascalDocModule.Factor;

Begin
End;

Procedure TPascalDocModule.RelOp;

Begin
End;

Procedure TPascalDocModule.AddOp;

Begin
End;

Procedure TPascalDocModule.MulOp;

Begin
End;

Procedure TPascalDocModule.Designator;

Begin
End;

Procedure TPascalDocModule.SetConstructor;

Begin
End;

Procedure TPascalDocModule.SetElement;

Begin
End;

Procedure TPascalDocModule.ExprList;

Begin
End;

Procedure TPascalDocModule.Statement;

Begin
End;

Procedure TPascalDocModule.StmtList;

Begin
End;

Procedure TPascalDocModule.SimpleStatement;

Begin
End;

Procedure TPascalDocModule.StructStmt;

Begin
End;

(**

  This method parses the compound statement section of a procedure implementation
  from the current token position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>CompoundStmt</B></TD>
      <TD>-></TD>
      <TD></TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon Parses the compound statement section of a procedure implementation
           from the current token position

**)
procedure TPascalDocModule.CompoundStmt();

Var
  iBlockCount : Integer;

begin
  iBlockCount := 0;
  GetBodyCmt;
  If Not IsKeyWord(Token.Token, strBlockStarts) Then Exit;
  Inc(iBlockCount);
  // The finding of the next token is handled manually so we can find todos
  NextToken;
  While (Token.TokenType In [ttComment, ttCompilerDirective]) And
    Not EndOfTokens Do
    Begin
      GetBodyCmt;
      NextToken;
    End;
  SkipStatements(iBlockCount);
  GetBodyCmt;
end;

Procedure TPascalDocModule.ConditionalStmt;

Begin
End;

Procedure TPascalDocModule.IfStmt;

Begin
End;

Procedure TPascalDocModule.CastStmt;

Begin
End;

Procedure TPascalDocModule.CaseSelector;

Begin
End;

Procedure TPascalDocModule.CaseLabel;

Begin
End;

Procedure TPascalDocModule.LoopStmt;

Begin
End;

Procedure TPascalDocModule.RepeatStmt;

Begin
End;

Procedure TPascalDocModule.WhileStmt;

Begin
End;

Procedure TPascalDocModule.ForStmt;

Begin
End;

Procedure TPascalDocModule.WithStmt;

Begin
End;

(**

  This method skips statements with in the implementation section of
  procedures and programs.

  @precon  iBlockCount is a referenced count er to the current level of
           nested statement block.
  @postcon Skips statements with in the implementation section of
           procedures and programs.

  @param   iBlockCount as an Integer as a reference

**)
procedure TPascalDocModule.SkipStatements(var iBlockCount : Integer);

begin
  // Skip over compound statements
  While iBlockCount > 0 Do
    Begin
      GetBodyCmt;
      If IsKeyWord(Token.Token, strBlockStarts) Then
        Inc(iBlockCount);
      If (Token.UToken = 'END') Then
        Dec(iBlockCount);
      If Token.UToken = 'FINALIZATION' Then
        Exit;
      // The finding of the next token is handled manually so we can find todos
      NextToken;
      While (Token.TokenType In [ttComment, ttCompilerDirective]) And
        Not EndOfTokens Do
        Begin
          GetBodyCmt;
          NextToken;
        End;
    End;
end;

(**

  This method parses a procedure declaration section from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ProcedureDeclSection</B></TD>
      <TD>-></TD>
      <TD>ProcedureDecl</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>FunctionDecl</TD>
    </TR>
  </TABLE>
  <TABLE>
    <TR>
      <TD><B>ProcedureDecl</B></TD>
      <TD>-></TD>
      <TD>ProcedureHeading ';' [ Directive ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>Block ';'</TD>
    </TR>
  </TABLE>
  <TABLE>
    <TR>
      <TD><B>FunctionDecl</B></TD>
      <TD>-></TD>
      <TD>FunctionHeading ';' [ Directive ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>Block ';'</TD>
    </TR>
  </TABLE>

  @note    This method is not implemented as per the grammer as I think the
           grammer is incorrect. The grammer dictates a block call after each
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
        M.ClassMethod := Cls;
        If Method = Nil Then
          ImplementedMethods.Add(M)
        Else
          Method.LocalMethods.Add(M);
        Result := True;
        NextNonCommentToken;
        // Only call Block not an external procedure.
        If (Length(M.Ext) = 0) And Not M.HasDirective('forward') Then
          Begin
            Block(scLocal, M);
            If Token.Token <> ';' Then
              Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
                'ProcedureDeclSection');
            NextNonCommentToken;
          End;
      End;
  Until M = Nil;
End;

Procedure TPascalDocModule.ProcedureDecl;

Begin
End;

Procedure TPascalDocModule.FunctionDecl;

Begin
End;

(**

  This method parses a function declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>FunctionHeading</B></TD>
      <TD>-></TD>
      <TD>FUNCTION Ident [ FormalParameters ] ':' ( SimpleType | STRING )</TD>
    </TR>
  </TABLE>

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
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ProcedureHeading</B></TD>
      <TD>-></TD>
      <TD>PROCEDURE Ident [ FormalParameters ]</TD>
    </TR>
  </TABLE>

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
  If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'ProcedureHeading');
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
        Raise EDocException.CreateNormal(strIdentExpected, Token,
          'ProcedureHeading');
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
End;

(**

  This method parses a methods formal parameters from the current token
  position using the following object psacal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>FormalParameter</B></TD>
      <TD>-></TD>
      <TD>'(' FormalParm / ';' ... ')'</TD>
    </TR>
  </TABLE>

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
      If Token.Token <> ')' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, ')', Token,
          'FormalParameters');
      NextNonCommentToken;
  End;
End;

(**

  This method parses a formal parameter for a method from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>FormalParam</B></TD>
      <TD>-></TD>
      <TD>[ VAR | CONST | OUT ] Parameter</TD>
    </TR>
  </TABLE>

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
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Parameter</B></TD>
      <TD>-></TD>
      <TD>IdentList [ ':' ( [ ARRAY OF ] SimpleType | STRING | FILE ) ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>Ident ':' SimpleType '=' ConstExpr</TD>
    </TR>
  </TABLE>

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
  T : TTypes;

Begin
  // Get ident list
  T := Nil;
  boolArrayOf := False;
  strValue := '';
  I := IdentList(False, iltStandIdentList, []); //: @todo SeekTokens(Paramter)
  Try
    If Token.Token = ':' Then
      Begin
        NextNonCommentToken;
        // Check Array Of
        If Token.UToken = 'ARRAY' Then
          Begin
            NextNonCommentToken;
            IF Token.UToken <> 'OF' Then
              Raise EDocException.CreateLiteral(strReservedWordExpected, 'OF',
                Token, 'FormalParameter');
              boolArrayOf := True;
              NextNonCommentToken;
          End;
        T := GetTypeDecl;
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
      Method.Add(TParameter.Create(ParamMod, I[j].Ident, boolArrayOf, T,
        strValue, scPublic, I[j].Line, I[j].Col));
  Finally
    I.Free;
  End;
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

  This method retrives the method directives after the method declaration from
  the current token position using the followong object pascal grammer.

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
          M.AddDirectives(Token.Token);
          NextNonCommentToken;
        End;
      NextNonCommentToken;
      If Token.Token <> ';' Then
        Begin
          ErrorAndSeekToken(strLiteralExpected, 'Directive', ';',
            strSeekableOnErrorTokens);
          Exit;
        End Else
          NextNonCommentToken;
    End;
End;

(**

  This method parses an Object type declaration from the current token position
  using the followong object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ObjectType</B></TD>
      <TD>-></TD>
      <TD>OBJECT [ ObjHertiage ] [ ObjectFieldList ] [ MethodList ]</TD>
    </TR>
  </TABLE>

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
  ClassHeritage(Result);
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
  If Token.UToken <> 'END' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'END', Token,
      'ObjectType');
  NextNonCommentToken;
end;

Procedure TPascalDocModule.ObjHeritage;

Begin
End;

(**

  This method parse a method list from the current token position using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>MethodList</B></TD>
      <TD>-></TD>
      <TD>( MethodHeading [ ';' VIRTUAL ] ) / ';' ...</TD>
    </TR>
  </TABLE>

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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>MethodHeading</B></TD>
      <TD>-></TD>
      <TD>ProcedureHeading</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>FunctionHeading</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ConstructorHeading</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>DestructorHeading</TD>
    </TR>
  </TABLE>

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
      Cls.Methods.Add(M);
      Result := True;
    End Else
      If boolClassMethod Then
        RollBackToken;
end;

(**

  This method parses a constructor declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ConstructorHeading</B></TD>
      <TD>-></TD>
      <TD>CONSTRUCTOR Ident [ FormalParameters ]</TD>
    </TR>
  </TABLE>

  @precon  Scope is the current scope of the constructor declaration.
  @postcon Returns a method declaration is a constructor was parsed else nil.

  @param   Scope as a TScope
  @return  a TMethodDecl

**)
function TPascalDocModule.ConstructorHeading(Scope: TScope): TMethodDecl;
begin
  Result := ProcedureBit(mtConstructor, Scope);
  If Result <> Nil Then
    Directive(Result);
end;

(**

  This method parses a destructor declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>DestructorHeading</B></TD>
      <TD>-></TD>
      <TD>DESTRUCTOR Ident [ FormalParameters ]</TD>
    </TR>
  </TABLE>

  @precon  Scope is the current scope of the destructor declaration.
  @postcon Returns a method declaration is a destructor was parsed else nil.

  @param   Scope as a TScope
  @return  a TMethodDecl

**)
function TPascalDocModule.DestructorHeading(Scope: TScope): TMethodDecl;
begin
  Result := ProcedureBit(mtDestructor, Scope);
  If Result <> Nil Then
    Directive(Result);
end;

(**

  This method parses a classes / interfaces field list from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ObjFieldList</B></TD>
      <TD>-></TD>
      <TD>( IndentList ':' Type ) / ';' ...</TD>
    </TR>
  </TABLE>

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
  T : TTypes;

begin
  Result := False;
  I := IdentList(False, iltStandIdentList, []); //: @todo SeekTokens(ObjFieldList)
  Try
    If Token.Token <> ':' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token,
        'ObjFieldList');
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
  Finally
    I.Free;
  End;
end;

(**

  This method parses the modules initialisation / finalisation section from the
  current token position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>InitSection</B></TD>
      <TD>-></TD>
      <TD>INITIALIZATION StmtList [ FINALIZATION StmtList ] END</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>BEGIN StmtList End</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>END</TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon Parses the modules initialisation / finalisation section from the
           current token position

**)
Procedure TPascalDocModule.InitSection;

Var
  i : Integer;

Begin
  If Token.UToken = 'INITIALIZATION' Then
    Begin
      InitComment := GetComment;
      NextNonCommentToken;
      i := 1;
      SkipStatements(i);
      If Token.UToken = 'FINALIZATION' Then
        Begin
          FinalComment := GetComment;
          NextNonCommentToken;
          i := 1;
          SkipStatements(i);
        End;
    End
  Else If Token.UToken = 'BEGIN' Then
    CompoundStmt
  Else If Token.UToken = 'END' Then
    NextNonCommentToken
  Else
    Raise EDocException.CreateLiteral(strReservedWordExpected,
      'INITIALIZATION, BEGIN or END', Token, 'Initsection');
End;

(**

  This method parse a class declaration from the current token position
  deligating field, property and method declarations using the following
  object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ClassType</B></TD>
      <TD>-></TD>
      <TD>CLASS [ ClassHeritage ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ClassFieldList ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ClassMethodList ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ClassPropertyList ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>END</TD>
    </TR>
  </TABLE>

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
  Result := TClassDecl.Create;
  SymbolTable.Add(Result);
  NextNonCommentToken;
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
  If Token.UToken <> 'END' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'END', Token,
      'ClassType');
  NextNonCommentToken;
end;

(**

  This method parses a class heriage ist from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ClassHeritage</B></TD>
      <TD>-></TD>
      <TD>'(' IdentList ')'</TD>
    </TR>
  </TABLE>

  @precon  Cls is a valid object declaration to get a heritage for.
  @postcon Parses a class heriage ist from the current token position

  @param   Cls as a TObjectDecl

**)
procedure TPascalDocModule.ClassHeritage(Cls: TObjectDecl);

Var
  I : TIdentList;

begin
  If Token.Token <> '(' Then
    Exit;
  NextNonCommentToken;
  I := IdentList(False, iltStandIdentList, []); //: @todo SeekTokens(ClassHeritage)
  Try
    Cls.Heritage.Assign(I);
  Finally
    I.Free;
  End;
  If Token.Token <> ')' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ')', Token,
      'ClassHeritage');
  NextNonCommentToken;
end;

(**

  This method parse the class visibility from the current token
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ClassVisibility</B></TD>
      <TD>-></TD>
      <TD>[ PUBLIC | PROTECTED | PRIVATE | PUBLISHED ]</TD>
    </TR>
  </TABLE>

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
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ObjFieldList</B></TD>
      <TD>-></TD>
      <TD>( ClassVisibility ObjFieldList ) / ';' ...</TD>
    </TR>
  </TABLE>

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
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>MethodList</B></TD>
      <TD>-></TD>
      <TD>( ClassVisibility MethodList ) / ';' ...</TD>
    </TR>
  </TABLE>

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
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'MethodList');
  NextNonCommentToken;
End;

(**

  This method parses a class property list frmo the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ClassPropertyList</B></TD>
      <TD>-></TD>
      <TD>( ClassVisibility PropertyList ';' ) ...</TD>
    </TR>
  </TABLE>

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
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'ClassPropertyList');
  NextNonCommentToken;
End;

(**

  This method parses a class property list from the current
  token position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>PropertyList</B></TD>
      <TD>-></TD>
      <TD>PROPERTY Ident [ PropertyInterface ] PropertySpecifiers</TD>
    </TR>
  </TABLE>

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
  If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'PropertyList');
  P := TProperty.Create(Token.Token, Scope, Token.Line, Token.Column);
  Cls.AddProperty(P);
  P.Comment := C;
  NextNonCommentToken;
  PropertyInterface(P);
  PropertySpecifiers(P);
end;

(**

  This method parses the property interface from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>PropertyInterface</B></TD>
      <TD>-></TD>
      <TD>[ PropertyParameterList ] ':' Ident</TD>
    </TR>
  </TABLE>

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
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>PropertyParameterList</B></TD>
      <TD>-></TD>
      <TD>'[' ( IdentList ':' TypeId ) / ';' ... ']'</TD>
    </TR>
  </TABLE>

  @precon  Prop is a property to parse a parameter list for.
  @postcon Parses a properties parameter list from the current token

  @param   Prop as a TProperty

**)
Procedure TPascalDocModule.PropertyParameterList(Prop : TProperty);

Var
  ParamMod : TParamModifier;
  I : TIdentList;
  j : Integer;
  T : TTypes;

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
        I := IdentList(False, iltStandIdentList, []); //: @todo SeekTokens(PropertyParameterList)
        Try
          If Token.Token <> ':' Then
            Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token,
              'PropertyParameterList');
          NextNonCommentToken;
          T := GetTypeDecl;
          For j := 0 To I.Count - 1 Do
            Prop.AddParameter(TParameter.Create(ParamMod, I[j].Ident, False,
              T, '', scPublic, I[j].Line, I[j].Col));
        Finally
          I.Free;
        End;
      Until Token.Token <> ';';
      If Token.Token <> ']' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, ']', Token,
          'PropertyParameterList');
      NextNonCommentToken;
    End;
End;


(**

  This method parses the property specifiers from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>PropertySpecifiers</B></TD>
      <TD>-></TD>
      <TD>[ INDEX ConstExpr ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ READ Ident ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ WRITE Ident ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ STORED ( Ident | Constant) ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ( DEFAULT ConstExpr ) | NODEFAULT ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ IMPLEMENTS TypeId ]</TD>
    </TR>
  </TABLE>

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
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>InterfaceType</B></TD>
      <TD>-></TD>
      <TD>INTERFACE [ InterfaceHeritage ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ClassMethodList ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ClassPropertyList ]</TD>
    </TR>
  </TABLE>

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
  ClassHeritage(Result);
  // Get GUID if there is one
  If Token.Token = '[' Then
    Begin
      NextNonCommentToken;
      If Token.TokenType <> ttStringLiteral Then
        Raise EDocException.CreateNormal(strStringExpected, Token, 'InterfaceType');
      Result.GUID := Token.Token;
      NextNonCommentToken;
      If Token.Token <> ']' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, ']', Token,
          'InterfaceType');
      NextNonCommentToken;
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
  If Token.UToken <> 'END' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'END', Token,
      'InterfaceType');
  NextNonCommentToken;
end;

Procedure TPascalDocModule.InterfaceHeritage;

begin
End;

(**

  This method parses a requires clause from the current token position usnig
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>RequiresClause</B></TD>
      <TD>-></TD>
      <TD>REQUIRES IdentList ... ';'</TD>
    </TR>
  </TABLE>

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
  Requires := IdentList(True, iltUsesClause, []); //: @todo SeekTokens(RequiresClause);
  Requires.Comment := Comment;
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'RequiresClause');
  NextNonCommentToken;
End;

(**

  This method parses a contains clause fro the cutrrent token position using
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ContainsClause</B></TD>
      <TD>-></TD>
      <TD>CONTAINS IdentList ... ';'</TD>
    </TR>
  </TABLE>

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
  Contains := IdentList(True, iltUsesClause, []); //: @todo SeekTokens(ContainsClause)
  Contains.Comment := Comment;
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'ContainsClause');
  NextNonCommentToken;
End;

(**

  This method creates a identifier list starting at the current token and
  return the list to the calling function. If OwnList is true then the identlist
  is added to the classes owned items list for automatic disposal, else it the
  responsibliity of the calling function to disposal of the class.

  @grammer IdentList -> Ident / ',' ...

  @precon  OwnList determines if the identlist should be disposed of be the
           parser or be the caller. SeekTokens is a sorted lowercase list of
           token to find if an error is found.
  @postcon Returns an ident list.

  @param   OwnList    as a Boolean
  @param   IdentListType as a TIdentListType
  @param   SeekTokens as an Array of String
  @return  a TIdentList

**)
Function TPascalDocModule.IdentList(OwnList : Boolean;
  IdentListType : TIdentListType; SeekTokens : Array Of String) : TIdentList;
var
  C: TGenericContainer;

Begin
  If IdentListType = iltIndexedEnumerates Then
    Result := TIndexedEnumerateList.Create
  Else
    Result := TIdentList.Create;
  // Add to owned list immediately if required to ensure that is an exception
  // is raised the memory is released in the destructor
  If OwnList Then
    OwnedItems.Add(Result);
  While Not EndOfTokens Do
    Begin
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'IdentList', Token.Token,
            SeekTokens);
          Exit;
        End;
      Result.Add(Token.Token, Token.Line, Token.Column, GetComment);
      NextNonCommentToken;
      If IdentListType In [iltUsesClause] Then
        If Token.UToken = 'IN' Then
          Begin
            NextNonCommentToken;
            If Token.TokenType <> ttStringLiteral Then
              ErrorAndSeekToken(strStringExpected, 'IdentList', Token.Token,
                SeekTokens)
            Else
              NextNonCommentToken;
          End;
      If IdentListType In [iltIndexedEnumerates] Then
        If Token.Token = '=' Then
          Begin
            NextNonCommentToken;
            C := TGenericContainer.Create;
            Try
              With (Result As TIndexedEnumerateList) Do
                ConstExpr(IndexInfo[Count - 1]);
            Finally
              C.Free;
            End;
          End;
      If Token.Token <> ',' Then
        Exit;
      NextNonCommentToken;
    End;
End;

(**

  This method returns a type id at the current token position using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>TypeId</B></TD>
      <TD>-></TD>
      <TD>[ UnitId '.' ] &#060;type-identifier></TD>
    </TR>
  </TABLE>

  @precon  None.
  @postcon Returns a type id as a string of text.

  @return  a String

**)
Function TPascalDocModule.TypeId : String;

Begin
  Result := Token.Token;
  NextNonCommentToken;
  If Token.Token <> '.' Then
    Exit;
  Result := Result + Token.Token;
  NextNonCommentToken;
  Result := Result + Token.Token;
  NextNonCommentToken;
End;

(**

  This method parses a constant expression from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ConstExpr</B></TD>
      <TD>-></TD>
      <TD>&#060;constant-expression></TD>
    </TR>
  </TABLE>

  @precon  C is a generic container to add tokens too.
  @postcon Returns true if a constant expression was parsed.

  @param   C as a TGenericContainer
  @return  a Boolean

**)
Function TPascalDocModule.ConstExpr(C : TGenericContainer) : Boolean;

Var
  iCounter : Integer;
  iTokens : Integer;

Begin
  {: @todo Need to impleent this correctly using Expression() and pass and check
           the type during processing. }
  Result := True;
  iCounter := 0;
  ITokens := 0;
  While (Not (Token.Token[1] In strConstExprTerminals) Or (iCounter > 0)) And
    Not IsKeyWord(Token.Token, strSeekableOnErrorTokens) Do
    Begin
      If Token.Token[1] In ['(', '['] Then
        Inc(iCounter);
      If Token.Token[1] In [')', ']'] Then
        Begin
          Dec(iCounter);
          If iCounter < 0 Then
            Exit;
        End;
      C.Add(Token.Token);
      NextNonCommentToken;
      Inc(iTokens);
    End;
  If iTokens = 0 Then
    Errors.Add('Loop detected.', 'ConstExpr', Token.Line, Token.Column, etError);
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
