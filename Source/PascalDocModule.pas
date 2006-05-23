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
  @Date       23 May 2006
  @Author     David Hoyle

**)
Unit PascalDocModule;

Interface

Uses
  Contnrs, Classes, Windows, BaseLanguageModule;

Type
  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TPascalDocModule = Class(TBaseLanguageModule)
  Private
    FOwnedItems : TObjectList;
    FSourceStream : TStream;
    FTokens : TObjectList;
    FExportedHeadings : TMethodCollection;
    FImplementedMethods : TMethodCollection;
    FConstantsCollection : TGenericContainerCollection;
    FVarsCollection : TGenericContainerCollection;
    FThreadVarsCollection : TGenericContainerCollection;
    FTypeCollection : TGenericContainerCollection;
    FTokenIndex : TTokenIndex;
    FModuleName : String;
    FModuleType : TModuleType;
    FModuleComment : TComment;
    FRequiresClause : TIdentList;
    FContainsClause : TIdentList;
    FUsesClause : TIdentList;
    FInitComment : TComment;
    FFinalComment : TComment;
    FResStrCollection: TGenericContainerCollection;
    FExportsCollection : TGenericContainerCollection;
    FBodyComment : TObjectList;
    FModuleNameCol: Integer;
    FModuleNameLine: Integer;
    FDocErrors: TDocErrorCollection;
    FSymbolTable: TGenericContainerCollection;
    FFileName: String;
    FModified : Boolean;
    FDocumentConflicts: TObjectList;
    FTickList : TStringList;
    function GetOpTickCountName(iIndex: Integer): String;
    function GetOpTickCountByIndex(iIndex: Integer): Integer;
    function GetOpTickCounts: Integer;
    function GetOpTickCount(strStart, strFinish : String): Integer;
    { Token methods }
    Function GetTokenCount : Integer;
    Function GetTokenInfo(iIndex : TTokenIndex) : TTokenInfo;
    Function GetToken : TTokenInfo;
    Procedure ParseStream;
    Function GetBodyComment(iIndex : Integer) : TComment;
    Function GetBodyCommentCount : Integer;
    Function GetDocumentConflict(iIndex : Integer) : TDocumentConflict;
    Function GetDocumentConflictCount : Integer;
    (**
      Returns a refernce the to owned items collection. This is used to manage
      the life time of all the ident lists and comments found in the module.
      @return  a TObjectList
    **)
    Property OwnedItems : TObjectList Read FOwnedItems;
    (**
      Returns a reference to the body comments collection.
      @return  a TObjectList
    **)
    Property BodyComments : TObjectList Read FBodyComment;
    { Grammer Parsers }
    Function OPProgram : Boolean;
    Function OPUnit : Boolean;
    Function OPPackage : Boolean;
    Function OPLibrary : Boolean;
    Procedure ProgramBlock;
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
    Function ResourceStringDecl(Scope: TScope; Method : TMethodDecl): Boolean;
    Function TypeSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function TypeDecl : TTypes;
    Function ClassRefType : TClassRefType;
    Function SimpleType : TTypes;
    Function VariantType : TVariantType;
    Function StringType : TStringType;
    Function StrucType : TTypes;
    Function RecType(boolPacked : Boolean) : TRecordDecl;
    Procedure FieldList(Rec : TRecordDecl);
    Function PointerType : TPointerType;
    Function ProcedureType : TProcedureType;
    Function VarSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Function ThreadVarSection(Scope : TScope) : Boolean;
    Function ResStringSection(Scope: TScope; Method : TMethodDecl): Boolean;
    Function VarDecl(Scope : TScope; VarSection : TGenericContainerCollection) : Boolean;
    Procedure CompoundStmt;
    Procedure SkipStatements(var iBlockCount : Integer);
    Function ProcedureDeclSection(Scope : TScope; Method : TMethodDecl) : Boolean;
    Procedure FormalParameter(Method : TMethodDecl);
    Procedure FormalParam(Method : TMethodDecl);
    Procedure Parameter(Method : TMethodDecl; ParamMod : TParamModifier);
    Function ObjectType : TObjectDecl;
    Function ObjFieldList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Function MethodList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Procedure InitSection;
    Function ClassType : TClassDecl;
    Procedure ClassHeritage(Cls : TObjectDecl);
    Function ClassFieldList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Function ClassMethodList(Cls : TObjectDecl; Scope : TScope) : Boolean;
    Function ClassPropertyList(Cls : TClassDecl; var Scope : TScope) : Boolean;
    Procedure PropertyInterface(Prop : TProperty);
    Procedure PropertyParameterList(Prop : TProperty);
    Procedure PropertySpecifiers(Prop : TProperty);
    Function InterfaceType : TInterfaceDecl;
    procedure ClassVisibility(var Scope : TScope);
    Procedure RequiresClause;
    Function IdentList(OwnList : Boolean): TIdentList;
    procedure ContainsClause;
    procedure UsesClause;
    Function ConstExpr(C: TGenericContainer) : Boolean;
    Function TypedConstant(C: TGenericContainer) : Boolean;
    function TypeId: String;
    function OPType : TTypes;
    function RestrictedType : TTypes;
    function RealType : TRealType;
    function OrdinalType : TOrdinalType;
    function EnumerateType : TOrdinalType;
    function OrdIdent : TOrdinalType ;
    function SubRangeType : TOrdinalType;
    function ArrayType(boolPacked : Boolean): TArrayType;
    function FileType(boolPacked: Boolean): TFileType;
    function SetType(boolPacked: Boolean): TSetType;
    Function VariantSection(Rec: TRecordDecl) : Boolean;
    procedure FieldDecl(Rec: TRecordDecl);
    procedure RecVariant(Rec: TRecordDecl);
    function MethodHeading(Cls: TObjectDecl; Scope: TScope): Boolean;
    Function ProcedureBit(ProcType : TMethodType; Scope : TScope) : TMethodDecl;
    procedure ExportsEntry;
    procedure ExportsList;
    Function PrevToken : TTokenInfo;
    Procedure NextToken;
    Procedure NextNonCommentToken;
    Procedure RollBackToken;
    Function EndOfTokens : Boolean;
    Function GetComment : TComment;
    Procedure SetTokenIndex(iIndex : TTokenIndex);
    Procedure GetBodyCmt;
    Function ConstructorHeading(Scope :TScope) : TMethodDecl;
    Function DestructorHeading(Scope :TScope) : TMethodDecl;
    Function FunctionHeading(Scope :TScope) : TMethodDecl;
    Function ProcedureHeading(Scope : TScope) : TMethodDecl;
    Procedure Directive(M : TMethodDecl);
    Procedure ScopeImplementedMethods;
    Procedure Goal;
    procedure Sort;
    Function PropertyList(Cls : TClassDecl; var Scope : TScope) : Boolean;
  Public
    Constructor Create(Source : TStream; FileName : String; IsModified : Boolean;
      ModuleOptions : TModuleOptions; DocOptions : TDocOptions);
    Destructor Destroy; Override;
    Procedure AddDocumentConflict(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol  : Integer;
      DocConflictType : TDocConflictType);
    Function FindMethodAtStreamPosition(iStreamPos : TStreamPosition;
      var recPosition : TTokenPosition) : TMethodDecl;
    Function FindPropertyAtStreamPosition(iStreamPos: TStreamPosition;
      var recPosition : TTokenPosition): TClassDecl;
    Procedure AddTickCount(strLabel : String);
    { Properties }
    Property OpTickCount[strStart, strFinish : String] : Integer Read GetOpTickCount;
    Property OpTickCounts : Integer Read GetOpTickCounts;
    Property OpTickCountByIndex[iIndex : Integer] : Integer Read GetOpTickCountByIndex;
    Property OpTickCountName[iIndex : Integer] : String Read GetOpTickCountName;
    (**
      Returns the number of token within the module after tokenizing.
      @return  an Integer
    **)
    Property TokenCount : Integer Read GetTokenCount;
    (**
      Returns the token information for the specifically indexed token within
      the module.
      @param   iIndex as       a TTokenIndex
      @return  a TTokenInfo
    **)
    Property TokenInfo[iIndex : TTokenIndex] : TTokenInfo Read GetTokenInfo;
    (**
      Returns the current token with in the module. Also see
      {@link TPascalDocModule.SetTokenIndex SetTokenIndex}.
      @return  a TTokenInfo
    **)
    Property Token : TTokenInfo Read GetToken;
    (**
      Returns the module name as a string.
      @return  a String
    **)
    Property ModuleName : String Read FModuleName Write FModuleName;
    (**
      Returns the type of the modules, Program, Unit, Package, etc.
      @return  a TModuleType
    **)
    Property ModuleType : TModuleType Read FModuleType Write FModuleType;
    (**
      Returns a reference to the modules comment.
      @return  a TComment
    **)
    Property ModuleComment : TComment Read FModuleComment;
    (**
      Returns a reference to the requires clause collection.
      @return  a TIdentList
    **)
    Property Requires : TIdentList Read FRequiresClause Write FRequiresClause;
    (**
      Returns a reference to the contains clause collection.
      @return  a TIdentList
    **)
    Property Contains : TIdentList Read FContainsClause Write FContainsClause;
    (**
      Returns a reference to the uses clause collection.
      @return  a TIdentList
    **)
    Property UsesCls : TIdentList Read FUsesClause Write FUsesClause;
    (**
      Returns a reference to the exported headings collection.
      @return  a TMethodCollection
    **)
    Property ExportedHeadings : TMethodCollection Read FExportedHeadings;
    (**
      Returns a reference to the implemented methods collection.
      @return  a TMethodCollection
    **)
    Property ImplementedMethods : TMethodCollection Read FImplementedMethods;
    (**
      Returns a reference to the constants clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Constants : TGenericContainerCollection Read FConstantsCollection;
    (**
      Returns a reference to the resource string clause collection.
      @return  a TGenericContainerCollection
    **)
    Property ResourceStrings : TGenericContainerCollection Read FResStrCollection;
    (**
      Returns a reference to the variables clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Vars : TGenericContainerCollection Read FVarsCollection;
    (**
      Returns a reference to the ThreadVar clause collection.
      @return  a TGenericContainerCollection
    **)
    Property ThreadVars : TGenericContainerCollection Read FThreadVarsCollection;
    (**
      Returns a reference to the modules types clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Types : TGenericContainerCollection Read FTypeCollection;
    (**
      Returns a reference to the modules Initialization comment.
      @return  a TComment
    **)
    Property InitComment : TComment Read FInitComment Write FInitComment;
    (**
      Returns a reference to the modules Finalization comment.
      @return  a TComment
    **)
    Property FinalComment : TComment Read FFinalComment Write FFinalComment;
    (**
      Returns a refernce to the modules exports collection.
      @return  a TGenericContainerCollection
    **)
    Property ExportsClause : TGenericContainerCollection Read FExportsCollection
      Write FExportsCollection;
    (**
      Returns the specific indexed body comment from the collection.
      @param   iIndex as       an Integer
      @return  a TComment
    **)
    Property BodyComment[iIndex : Integer] : TComment Read GetBodyComment;
    (**
      Returns a reference to the modules body comments collection.
      @return  an Integer
    **)
    Property BodyCommentCount : Integer Read GetBodyCommentCount;
    (**
      Returns the line number of the modules name.
      @return  an Integer
    **)
    Property ModuleNameLine : Integer Read FModuleNameLine Write FModuleNameLine;
    (**
      Returns the column number of the module name.
      @return  an Integer
    **)
    Property ModuleNameCol : Integer Read FModuleNameCol Write FModuleNameCol;
    (**
      Returns a reference to the modules error collection.
      @return  a TDocErrorCollection
    **)
    Property Errors : TDocErrorCollection Read FDocErrors;
    (**
      Returns a reference to the modules symbol table. All symbol in the module
      are stored here and disposed of from here. Reference from other collections
      like Types are purely reference only and those collection will not manage
      the symbols life time.
      @return  a TGenericContainerCollection
    **)
    Property SymbolTable : TGenericContainerCollection Read FSymbolTable;
    (**
      This property returns the file name of the module as passed to the
      constructor.
      @return  a String
    **)
    Property FileName : String Read FFileName;
    (**
      This property returns whether the source code is modified or not.
      @return  a Boolean
    **)
    Property Modified : Boolean Read FModified;
    (**
      This property returns a reference to the classes document conflict list.
      @return  a TStringList
    **)
    Property DocumentConflict[iIndex : Integer] : TDocumentConflict
      Read GetDocumentConflict;
    (**

      @return  an Integer
    **)
    Property DocumentConflictCount : Integer Read GetDocumentConflictCount;
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
  (** A list of string representing the types of modules. **)
  strModuleTypes : Array[mtProgram..mtUnit] Of String = ('Program', 'Package',
    'Library', 'Unit');
  (** This is a list of constant expression terminating reserved words. **)
  strTerminalWords : Array[1..5] Of String = ('end', 'private', 'protected',
    'public', 'published');
  (** This is a list of compound block statement start keywords. **)
  strBlockStarts : Array[1..4] Of String = ('asm', 'begin', 'case', 'try');
  (** This is a constant for special tag items to show in the tree **)
  iShowInTree = $0001;
  (** This is a constant for special tag items to auto expand in the tree **)
  iAutoExpand = $0002;

  (** This is a string array representing the TDocOption enumerates. **)
  DocOptionInfo : Array[Low(TDocOption)..High(TDocOption)] Of TDocOptionRec = (
    (Description : 'Draw Syntax Highlighted Module Explorer'; Enabled : False),
    (Description : 'Show comments in the hints'; Enabled : False),
    (Description : 'Show local declarations in methods'; Enabled : False),
    (Description : 'Show private declarations'; Enabled : True),
    (Description : 'Show protected declarations'; Enabled : True),
    (Description : 'Show public declarations'; Enabled : True),
    (Description : 'Show published declarations'; Enabled : True),
    (Description : 'Show local procedures and functions'; Enabled : True),
    (Description : 'Show Documentation Conflicts'; Enabled : False),
    (Description : 'Show Missing Method Documentation'; Enabled : True),
    (Description : 'Show Missing Method Documentation Description'; Enabled : True),
    (Description : 'Show Different Method Parameter Count'; Enabled : True),
    (Description : 'Show Undocumented Method Parameters'; Enabled : True),
    (Description : 'Show Incorrect Method Parameter Type'; Enabled : True),
    (Description : 'Show Undocumented Method Return'; Enabled : True),
    (Description : 'Show Incorrect Method Return Type'; Enabled : True),
    (Description : 'Show Undocumented Types'; Enabled : False),
    (Description : 'Show Undocumented Records'; Enabled : False),
    (Description : 'Show Undocumented Objects'; Enabled : False),
    (Description : 'Show Undocumented Classes'; Enabled : False),
    (Description : 'Show Undocumented Interfaces'; Enabled : False),
    (Description : 'Show Undocumented Variables'; Enabled : False),
    (Description : 'Show Undocumented Constants'; Enabled : False),
    (Description : 'Show Undocumented Module'; Enabled : True),
    (Description : 'Show Missing Module Date'; Enabled : False),
    (Description : 'Show Check Module Date'; Enabled : False),
    (Description : 'Show Missing Module Version'; Enabled : False),
    (Description : 'Show Missing Module Author'; Enabled : False),
    (Description : 'Show Missing Method Pre-Conditions'; Enabled : False),
    (Description : 'Show Missing Method Post-Conditions'; Enabled : False),
    (Description : 'Use Single Pre and Post Method Conditions'; Enabled : True),
    (Description : 'Show Missing Property Documentation'; Enabled : False),
    (Description : 'Show Missing Property Documentation Description'; Enabled : False),
    (Description : 'Show Different Property Parameter Count'; Enabled : False),
    (Description : 'Show Undocumented Property Parameter'; Enabled : False),
    (Description : 'Show Incorrect Property Parameter Type'; Enabled : False),
    (Description : 'Show Undocumented Property Return Type'; Enabled : False),
    (Description : 'Show Incorrect Property Return Type'; Enabled : False),
    (Description : 'Show Missing Property Pre-Conditions'; Enabled : False),
    (Description : 'Show Missing Property Post-Conditions'; Enabled : False),
    (Description : 'Use Single Pre and Post Property Conditions'; Enabled : True),
    (Description : 'Categories Documentation Conflicts'; Enabled : False)
  );

  (** An array of parameter modifier phases. **)
  strModifier : Array[pmNone..pmOut] Of String = ('', ' as a reference',
    ' constant', ' as out');
  (** A simple array for outputting a or an. **)
  strAOrAn : Array[False..True] Of String = ('a', 'an');
  (** A list of vowels. **)
  strVowels : Set Of Char = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'];

Var
  (** This is a global string list containing the special tags list. **)
  SpecialTags : TStringList;

  Function Tokenize(strText : String) : TStringList;
  Function ConvertDate(Const strDate : String) : TDateTime;

  Implementation

Uses
  PascalDocChecker, SysUtils;

(**

  This function returns true if the given word is in the supplied word list. It
  uses a binary search, so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list.
  @precon  strWordList is a static array of words in lowercase and alphabetical
           order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String
  @param   strWordList as an Array Of String
  @return  a Boolean

**)
function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;

Var
  l, m, r : Integer;
  str : String;

begin
  Result := False;
  str := LowerCase(strWord);
  l := 0;
  r := High(strWordList);
  While l <= r Do
    Begin
      m := (l + r) Div 2;
      If strWordList[m] < str Then
        l := Succ(m)
      Else If strWordList[m] > str Then
        r:= Pred(m)
      Else
        Begin
          Result := True;
          Exit;
        End;
    End;
end;

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

  This function returns the token type for a given character and last token
  type.

  @precon  Ch is the character for which the token type assessment needs to be
           taken for.
  @precon  LastToken os the type of the last token as this has an effect on some
           characters.
  @postcon Returns the token type for the given character.

  @param   Ch        as a Char
  @param   LastToken as a TTokenType
  @return  a TTokenType

**)
Function GetTokenType(Ch : Char; LastToken : TTokenType) : TTokenType;

Begin
  If ch In strWhiteSpace Then
    Result := ttWhiteSpace
  Else If ch In strTokenChars Then
    Result := ttIdentifier
  Else If ch In strNumbers Then
    Begin
      Result := ttNumber;
      If LastToken = ttIdentifier Then
        Result := ttIdentifier;
    End
  Else If ch In strLineEnd Then
    Result := ttLineEnd
  Else If ch In strQuote Then
    Result := ttStringLiteral
  Else If ch In strSymbols Then
    Begin
      Result := ttSymbol;
      If (Ch = '.') And (LastToken = ttNumber) Then
        Result := ttNumber;
    End
  Else
    Result := ttUnknown;
End;

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon   strText si the line of text to be tokenised
  @postcon  Returns a new string list of the tokenized string
  @resource The string list returnsed must be destroyed be the calling method.

  @param   strText as a String
  @return  a TStringList

**)
Function Tokenize(strText : String) : TStringList;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  (** Token buffer. **)
  strToken : String;
  CurToken : TTokenType;
  LastToken : TTokenType;
  BlockType : TBlockType;
  (** Token size **)
  iTokenLen : Integer;
  i : Integer;

Begin
  Result := TStringList.Create;
  BlockType := btNoBlock;
  strToken := '';
  CurToken := ttUnknown;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For i := 1 To Length(strText) Do
    Begin
      LastToken := CurToken;
      CurToken := GetTokenType(strText[i], LastToken);

      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If ((BlockType In [btStringLiteral]) And (CurToken <> ttLineEnd)) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := strText[i];
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  If IsKeyWord(strToken, strReservedWords) Then
                    LastToken := ttReservedWord;
                  If IsKeyWord(strToken, strDirectives) Then
                    LastToken := ttDirective;
                  Result.AddObject(strToken, TObject(LastToken));
                End;
             BlockType := btNoBlock;
             iTokenLen := 1;
             SetLength(strToken, iTokenCapacity);
             strToken[iTokenLen] := strText[i];
            End;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strText[i];
        End;

      // Check for string literals
      If CurToken = ttStringLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If IsKeyWord(strToken, strReservedWords) Then
        CurToken := ttReservedWord;
      If IsKeyWord(strToken, strDirectives) Then
        CurToken := ttDirective;
      Result.AddObject(strToken, TObject(CurToken));
    End;
End;

(**

  This function converts a freeform text string representing dates and times
  in standard formats in to a TDateTime value.

  @precon  strDate is the string to convert into a date.
  @postcon Returns a valid TDateTime value.

  @param   strDate as a String
  @return  a TDateTime

**)
Function ConvertDate(Const strDate : String) : TDateTime;

Type
  TDateRec = Record
    iDay, iMonth, iYear, iHour, iMinute, iSecond : Word;
  End;

Const
  strErrMsg = 'Can not convert the date "%s" to a valid TDateTime value.';
  Delimiters : Set Of Char = ['-', ' ', '\', '/', ':'];
  Days : Array[1..7] Of String = ('fri', 'mon', 'sat', 'sun', 'thu', 'tue', 'wed');
  Months : Array[1..24] Of String = (
    'apr', 'april',
    'aug', 'august',
    'dec', 'december',
    'feb', 'february',
    'jan', 'january',
    'jul', 'july',
    'jun', 'june',
    'mar', 'march',
    'may', 'may',
    'nov', 'november',
    'oct', 'october',
    'sep', 'september'
    );
  MonthIndexes : Array[1..24] Of Word = (
    4, 4,
    8, 8,
    12, 12,
    2, 2,
    1, 1,
    7, 7,
    6, 6,
    3, 3,
    5, 5,
    11, 11,
    10, 10,
    9, 9
  );

Var
  i : Integer;
  sl : TStringList;
  strToken : String;
  iTime : Integer;
  recDate : TDateRec;
  tmp : Word;
  iIndex0, iIndex1, iIndex2 : Integer;

  (**

    This procedure adds the token to the specified string list and clears the
    token.

    @precon  StringList is the string list to add the token too.
    @precon  strToken is the token to add to the list.

    @param   StringList as a TStringList
    @param   strToken   as a String as a reference

  **)
  Procedure AddToken(StringList : TStringList; var strToken  : String);

  Begin
    If strToken <> '' Then
      Begin
        StringList.Add(strToken);
        strToken := '';
      End;
  End;

  (**

    This procedure tries to extract the value from the indexed string list
    item into the passed variable reference. It delete is true it remove the
    item from the string list.

    @precon  iIndex is the index of the item from the string list to extract.
    @precon  iValue is a word variable to place the converted item into.
    @precon  Delete determines whether the item is removed from the string list.

    @param   iIndex as an Integer
    @param   iValue as a Word as a reference
    @param   Delete as a Boolean

  **)
  Procedure ProcessValue(iIndex : Integer; var iValue : Word; Delete : Boolean);

  Begin
    If iIndex > sl.Count - 1 Then Exit;
    Val(sl[iIndex], iValue, i);
    If i <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
    If Delete Then sl.Delete(iIndex);
  End;

  (**

    This procedure assigns string list indexes to the three index values
    according to the short date format and what information is supplied.

  **)
  Procedure AssignIndexes();

  Var
    slFormat : TStringList;
    str : String;
    j : Integer;

  Begin
    iIndex0 := 0; // Default Day / Month / Year
    iIndex1 := 1;
    iIndex2 := 2;
    slFormat := TstringList.Create;
    Try
      str := '';
      For j := 1 To Length(ShortDateFormat) Do
        If ShortDateFormat[j] In Delimiters Then
          AddToken(slFormat, str)
        Else
          str := str + ShortDateFormat[j];
      AddToken(slFormat, str);
      // Remove day of week
      For j := slFormat.Count - 1 DownTo 0 Do
        If (slFormat[j][1] In ['d', 'D']) And (Length(slFormat[j]) > 2) Then
          slFormat.Delete(j);
      For j := 0 To slFormat.Count - 1 Do
        Begin
          If slFormat[j][1] In ['d', 'D'] Then iIndex0 := j;
          If slFormat[j][1] In ['m', 'M'] Then iIndex1 := j;
          If slFormat[j][1] In ['y', 'Y'] Then iIndex2 := j;
        End;
    Finally
      slFormat.Free;
    End;
  End;

Begin
  Result := 0;
  sl := TStringList.Create;
  Try
    strToken := '';
    iTime := -1;
    For i := 1 To Length(strDate) Do
      If strDate[i] In Delimiters Then
        Begin
          AddToken(sl, strToken);
          If (strDate[i] = ':') And (iTime = -1) Then iTime := sl.Count - 1;
        End Else
          strToken := strToken + strDate[i];
    AddToken(sl, strToken);
    FillChar(recDate, SizeOf(recDate), 0);
    // Decode time
    If iTime > -1 Then
      Begin
        ProcessValue(iTime,recDate.iHour, True);
        ProcessValue(iTime,recDate.iMinute, True);
        ProcessValue(iTime,recDate.iSecond, True);
      End;
    // Remove day value if present
    For i := sl.Count - 1 DownTo 0 Do
      If IsKeyWord(sl[i], Days) Then
        sl.Delete(i);
    // Decode date
    Case sl.Count Of
      1 :
        Begin
          DecodeDate(Now, recDate.iYear, recDate.iMonth, tmp);
          ProcessValue(0, recDate.iDay, False); // Day only
        End;
      2, 3 : // Day and Month (Year)
        Begin
          DecodeDate(Now, recDate.iYear, tmp, tmp);
          AssignIndexes;
          ProcessValue(iIndex0, recDate.iDay, False); // Get day
          If IsKeyWord(sl[iIndex1], Months) Then
            Begin
              For i := Low(Months) To High(Months) Do
                If AnsiCompareText(Months[i], sl[iIndex1]) = 0 Then
                  Begin
                    recDate.iMonth := MonthIndexes[i];
                    Break;
                  End;
            End Else
              ProcessValue(iIndex1, recDate.iMonth, False); // Get Month
            If sl.Count = 3 Then
              Begin
                ProcessValue(iIndex2, recDate.iYear, False); // Get Year
                If recDate.iYear < 1900 Then Inc(recDate.iYear, 2000);
              End;
        End;
    Else
      If sl.Count <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
    End;
    // Output result.
    With recDate Do
      Begin
        If Not (iHour In [0..23]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Not (iMinute In [0..59]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Not (iSecond In [0..59]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        Result := EncodeTime(iHour, iMinute, iSecond, 0);
        If iYear * iMonth * iDay <> 0 Then
          Begin
            If Not (iDay In [1..31]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
            If Not (iMonth In [1..12]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
            Result := Result + EncodeDate(iYear, iMonth, iDay);
          End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This is a custom sort method for the DocConflictClass which allows the
  internal sort method to sort the documentation conflicts.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function CompareDocConflicts(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(
    TDocumentConflict(Item1).Category + '.' + TDocumentConflict(Item1).Message,
    TDocumentConflict(Item2).Category + '.' + TDocumentConflict(Item2).Message)

End;

(**

  This is the constructor method for the TPascalDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text
           that is the contents of a source code module.
  @precon  Filename is the file name of the module being parsed.
  @precon  IsModified determines if the source code module has been modified
           since the last save to disk.

  @param   Source        as a TStream
  @param   FileName      as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions
  @param   DocOptions    as a TDocOptions

**)
Constructor TPascalDocModule.Create(Source : TStream; FileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions; DocOptions : TDocOptions);

Begin
  FTickList := TStringList.Create;
  FSourceStream := Source;
  FFileName := FileName;
  FModified := IsModified;
  FTokens := TObjectList.Create(True);
  FOwnedItems := TObjectList.Create(True);
  FExportedHeadings := TMethodCollection.Create;
  FImplementedMethods := TMethodCollection.Create;
  FConstantsCollection := TGenericContainerCollection.Create(True);
  FResStrCollection := TGenericContainerCollection.Create(True);
  FVarsCollection := TGenericContainerCollection.Create(True);
  FThreadVarsCollection := TGenericContainerCollection.Create(True);
  FTypeCollection := TGenericContainerCollection.Create(False);
  FExportsCollection := TGenericContainerCollection.Create(True);
  FBodyComment := TObjectList.Create(True);
  FDocErrors := TDocErrorCollection.Create;
  FSymbolTable := TGenericContainerCollection.Create(True);
  FDocumentConflicts :=  TObjectList.Create(True);
  FContainsClause := Nil;
  FFinalComment := Nil;
  FInitComment := Nil;
  FModuleComment := Nil;
  FModuleName := '';
  FModuleNameCol := 0;
  FModuleNameLine := 0;
  FModuleType := mtUnit;
  FRequiresClause := Nil;
  FUsesClause := Nil;
  FTokenIndex := 0;
  AddTickCount('Start');
  ParseStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then Goal;
  AddTickCount('Parse');
  Sort;
  AddTickCount('Sort');
  If (moParse In ModuleOptions) And
    (moCheckForDocumentConflicts In ModuleOptions) Then
    With TPascalDocChecker.Create(Self, DocOptions) Do
      Try
        CheckDocumentForConflicts;
        FDocumentConflicts.Sort(CompareDocConflicts);
      Finally
        Free;
      End;
  AddTickCount('Check');
End;

(**

  This is the destructor method for the TPascalDocModule class.

**)
Destructor TPascalDocModule.Destroy;

Begin
  FDocumentConflicts.Free;
  FSymbolTable.Free;
  FDocErrors.Free;
  FBodyComment.Free;
  FExportsCollection.Free;
  FTypeCollection.Free;
  FThreadVarsCollection.Free;
  FVarsCollection.Free;
  FResStrCollection.Free;
  FConstantsCollection.Free;
  FImplementedMethods.Free;
  FExportedHeadings.Free;
  FOwnedItems.Free;
  FTokens.Free;
  FTickList.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the TokenCount property.

  @postcon Returns the number of tokens in the collection.

  @return  an Integer

**)
Function TPascalDocModule.GetTokenCount : Integer;

Begin
  Result := FTokens.Count;
End;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into object pascal tokens.

**)
Procedure TPascalDocModule.ParseStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btLineComment, btBraceComment,
    btFullComment);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;
  test = 12.34;

Var
  boolEOF : Boolean;
  (** Token buffer. **)
  strToken : String;
  CurToken : TTokenType;
  LastToken : TTokenType;
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

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  boolEOF := False;
  CurToken := ttUnknown;
  LastToken := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  Ch := #0;
  LastChar := #0;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
    If FSourceStream <> Nil Then
      Begin
        Repeat
          If FSourceStream.Read(ch, 1) > 0 Then
            Begin
              Inc(iStreamCount);
              LastToken := CurToken;
              CurToken := GetTokenType(Ch, LastToken);

              // Check for full block comments
              If (BlockType = btNoBlock) And (LastChar = '(') And (Ch = '*') Then
                BlockType := btFullComment;

              // Check for line comments
              If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '/') Then
                BlockType := btLineComment;

              If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
                Begin
                  If ((BlockType In [btStringLiteral, btLineComment]) And
                    (CurToken <> ttLineEnd)) Or
                    (BlockType In [btBraceComment, btFullComment]) Then
                    Begin
                      Inc(iTokenLen);
                      If iTokenLen > Length(strToken) Then
                        SetLength(strToken, iTokenCapacity + Length(strToken));
                      strToken[iTokenLen] := Ch;
                    End Else
                    Begin
                      SetLength(strToken, iTokenLen);
                      If Not TBaseLanguageModule.IsTokenWhiteSpace(strToken) Then
                        Begin
                          If LastToken = ttIdentifier Then
                            Begin
                              If IsKeyWord(strToken, strReservedWords) Then
                                LastToken := ttReservedWord;
                              If IsKeyWord(strToken, strDirectives) Then
                                LastToken := ttDirective;
                            End;
                          If BlockType = btLineComment Then
                            LastToken := ttComment;
                          If (LastToken = ttComment) And (Length(strToken) > 2) Then
                            If (strToken[1] = '{') And (strToken[2] = '$') Then
                              LastToken := ttCompilerDirective;
                          FTokens.Add(TTokenInfo.Create(strToken, iStreamPos,
                            iTokenLine, iTokenColumn, Length(strToken), LastToken));
                          //Inc(iCounter);
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
                  CurToken := ttComment;
                End;

              // Check for string literals
              If CurToken = ttStringLiteral Then
                If BlockType = btStringLiteral Then
                  BlockType := btNoBlock
                Else If BlockType = btNoBlock Then
                  BlockType := btStringLiteral;

              // Check for block Comments
              If (BlockType = btNoBlock) And (Ch = '{') Then
                Begin
                  CurToken := ttComment;
                  BlockType := btBraceComment;
                End;
              If (BlockType = btBraceComment) And (Ch = '}') Then
                Begin
                  CurToken := ttComment;
                  BlockType := btNoBlock;
                End;

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
              FTokens.Add(TTokenInfo.Create(strToken, iStreamPos,
                iTokenLine, iTokenColumn, Length(strToken), LastToken));
          End;
      End;
  Except
    On E : Exception Do
      Errors.Add(E.Message, 0, 0, 'Exception during tokenizing.', etError);
  End
End;

(**

  This is a getter method for the TokenInfo property.

  @precon  iIndex is the index of the token info object required.
  @postcon Returns the token info object requested.

  @param   iIndex as a TTokenIndex
  @return  a TTokenInfo

**)
function TPascalDocModule.GetTokenInfo(iIndex: TTokenIndex): TTokenInfo;

begin
  Result := FTokens[iIndex] As TTokenInfo;
end;

(**

  This is a getter method for the Token property.

  @postcon Returns a token info object for the current token.

  @return  a TTokenInfo

**)
Function TPascalDocModule.GetToken : TTokenInfo;

Begin
  If FTokenIndex >= FTokens.Count Then
    Raise EDocException.Create(strUnExpectedEndOfFile);
  Result := FTokens[FTokenIndex] As TTokenInfo;
End;

(**

  This method returns the previous token in the token list, else returns nil.

  @postcon Returns a token info object for the previous non comment token.

  @return  a TTokenInfo

**)
Function TPascalDocModule.PrevToken : TTokenInfo;

Var
  i : Integer;

begin
  Result := Nil;
  For i := FTokenIndex - 1 DownTo 0 Do
    If Not ((FTokens[i] As TTokenInfo).TokenType In [ttComment,
      ttCompilerDirective]) Then
      Begin
        Result := FTokens[i] As TTokenInfo;
        Exit;
      End;
end;

(**

  This method moves the toke to the next token in the token list or raises an
  EDocException.

**)
Procedure TPascalDocModule.NextToken;

begin
  Inc(FTokenIndex);
end;

(**

  This method checks for the end of the token list and returns true if it is
  found.

  @postcon Returns true is we are beyond the end of the token collection.

  @return  a Boolean

**)
Function TPascalDocModule.EndOfTokens : Boolean;

Begin
  Result := FTokenIndex >= FTokens.Count;
End;

(**

  This method tries to get a document comment from the previous token and return
  a TComment class to the calling routine.

  @note    All comments found are automatically added to the comment collection
           for disposal when the parser is destroyed.

  @postcon Returns the comment immediately before the current token else nil.

  @return  a TComment

**)
Function TPascalDocModule.GetComment : TComment;

Var
  T : TTokenInfo;

Begin
  Result := Nil;
  If FTokenIndex - 1 > -1 Then
    Begin
      T := FTokens[FTokenIndex - 1] As TTokenInfo;
      If T.TokenType = ttComment Then
        Begin
          Result := TComment.CreateComment(T.Token, T.Line, T.Column);
          OwnedItems.Add(Result);
        End;
    End;
End;

(**

  This is a getter method for the DocumentConflict property.

  @precon  iIndex must be a valid integer index.
  @postcon Returns the documentation conflict references by the passed index.

  @param   iIndex as an Integer
  @return  a TDocumentConflict

**)
function TPascalDocModule.GetDocumentConflict(
  iIndex: Integer): TDocumentConflict;
begin
  Result := FDocumentConflicts.Items[iIndex] As TDocumentConflict;
end;

(**

  This is a getter method for the DocumentConflictCount property.

  @precon  None.
  @postcon Returns the number of documenation conflicts in the collection.

  @return  an Integer

**)
function TPascalDocModule.GetDocumentConflictCount: Integer;
begin
  Result := FDocumentConflicts.Count;
end;

(**

  This is a getter method for the OpTickCount property.

  @precon  None.
  @postcon If both the start and end token are found in the collection of Tick
           Counts then the number of Tick Counts between them are returned.

  @param   strStart  as a String
  @param   strFinish as a String
  @return  an Integer  

**)
function TPascalDocModule.GetOpTickCount(strStart, strFinish : String): Integer;

Var
  i : Integer;
  iStart, iFinish : Integer;

begin
  Result := -1;
  iStart := 0;
  iFinish := 0;
  For i := 0 To FTickList.Count - 1 Do
    Begin
      If AnsiComparetext(FTickList[i], strStart) = 0 Then iStart := i;
      If AnsiComparetext(FTickList[i], strFinish) = 0 Then iFinish := i;
    End;
  If iStart * iFinish > 0 Then
    Result := Integer(FTickList.Objects[iFinish]) - Integer(FTickList.Objects[iStart]);
end;

(**

  This is a getter method for the OpTickCountByIndex property.

  @precon  iIndex must be a valid index.
  @postcon Returns the tick count associated with the passed index.

  @param   iIndex as an Integer
  @return  an Integer

**)
function TPascalDocModule.GetOpTickCountByIndex(iIndex: Integer): Integer;
begin
  Result := Integer(FTickList.Objects[iIndex]);
end;

(**

  This is a getter method for the OpTickCountName property.

  @precon  iIndex must be a valid integer index.
  @postcon Returns the name of the OpTickCount references by the index passed.

  @param   iIndex as an Integer
  @return  a String

**)
function TPascalDocModule.GetOpTickCountName(iIndex: Integer): String;
begin
  Result := FTickList[iIndex];
end;

(**

  This is a getter method for the OpTickCounts property.

  @precon  None.
  @postcon Returns the number of items in the OpTickCount collection.

  @return  an Integer

**)
function TPascalDocModule.GetOpTickCounts: Integer;
begin
  Result := FTickList.Count;
end;

(**

  This method tries to get a body comment from the previous token in the token
  list and add it to the body comment list.

**)
Procedure TPascalDocModule.GetBodyCmt;

Var
  T : TTokenInfo;
  C : TComment;

Begin
  If FTokenIndex - 1 > -1 Then
    Begin
      T := FTokens[FTokenIndex - 1] As TTokenInfo;
      If T.TokenType = ttComment Then
        Begin
          C := TComment.CreateComment(T.Token, T.Line, T.Column);
          If C <> Nil Then BodyComments.Add(C);
        End;
    End;
End;

(**

  This method move the token position to the next non comment token.

**)
procedure TPascalDocModule.NextNonCommentToken;
begin
  // Go to the next token
  NextToken;
  // Keep going if a comment
  While (Token.TokenType In [ttComment, ttCompilerDirective]) And Not EndOfTokens Do
    NextToken;
end;

(**

  This method rolls back to the previous token in the token list skipping
  comment tokens.

**)
Procedure TPascalDocModule.RollBackToken;

Begin
  Dec(FTokenIndex);
  While (FTokenIndex > 0) And (TokenInfo[FTokenIndex].TokenType In [ttComment,
    ttCompilerDirective]) Do
    Dec(FTokenIndex);
  If FTokenIndex < 0 Then
    Raise EDocException.Create(strUnExpectedStartOfFile);
End;

(**

  This is a getter method for the BodyComment property.

  @precon  iIndex is the index of the body comment required.
  @postcon Return the requested comment object.

  @param   iIndex as an Integer
  @return  a TComment

**)
Function TPascalDocModule.GetBodyComment(iIndex : Integer) : TComment;

Begin
  Result := FBodyComment[iIndex] As TComment;
End;

(**

  This is a getter method for the BodyCommentCount property.

  @postcon Returns the number of body comment in the collection.

  @return  an Integer

**)
Function TPascalDocModule.GetBodyCommentCount : Integer;

Begin
  Result := FBodyComment.Count;
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

  This is a setter method for the TokenIndex property.

  @precon  iIndex is the token index to set the parse to start at.

  @param   iIndex as a TTokenIndex

**)
Procedure TPascalDocModule.SetTokenIndex(iIndex : TTokenIndex);

Begin
  FTokenIndex := iIndex;
End;

(**

  This method sorts the structures items in alphanumeric order.

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

**)
procedure TPascalDocModule.ScopeImplementedMethods;

Var
  i, j, k : Integer;

begin
  For i := 0 To ImplementedMethods.Count - 1 Do
    If ImplementedMethods[i].ClsName <> '' Then
      Begin
        j := Types.Find(ImplementedMethods[i].ClsName);
        If j <> - 1 Then
          Begin
            k := (Types[j] As TObjectDecl).FindMethod(ImplementedMethods[i].Identifier);
            If k <> -1 Then
              ImplementedMethods[i].Scope := (Types[j] As TObjectDecl).Method[k].Scope;
          End;
      End Else
      Begin
        k := ExportedHeadings.Find(ImplementedMethods[i].Identifier);
        If k <> -1 Then
          ImplementedMethods[i].Scope := ExportedHeadings[k].Scope;
      End;
end;

(**

  This method is the starting position for the parsing of an object pascal
  module. It finds the first non comment token and begins the grammer checking
  from their by deligating to the program, library, unit and package methods.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Goal</B></TD>
      <TD>-></TD>
      <TD>( Program | Package | Library | Unit )</TD>
    </TR>
  </TABLE>

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
    If EndOfTokens Then
      Raise EDocException.Create(strUnExpectedEndOfFile);
    // See if there is a module comment
    // Store comment for auto removal if not nil
    FModuleComment := GetComment;
    Repeat
      {Do Nothing}
    Until Not (OPProgram Or OPLibrary Or OPPackage Or OPUnit);
    Types.RemoveForwardDecls; // remove forward declarations from the types
  Except
    On E : EDocException Do
      Begin
        Errors.Add(E.Message, E.Line, E.Col, E.ExceptionMethod, etError);
      End;
  End;
end;

(**

  This method parses a Program declaration from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Program</B></TD>
      <TD>-></TD>
      <TD>[ PROGRAM Ident [ '(' IdentList ')' ';' ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>ProgramBlock '.'</TD>
    </TR>
  </TABLE>

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
      IdentList(True); // get ident list
      // Check for closing parenthesis
      If Token.Token <> ')' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, ')', Token,
          'OPProgram');
      NextNonCommentToken;
    End;
  // Check for ';'
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'OPProgram');
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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Unit</B></TD>
      <TD>-></TD>
      <TD>UNIT Ident ';'</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>InterfaceSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>ImplementationSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>InitSection '.'</TD>
    </TR>
  </TABLE>

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
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'OPUnit');
  ModuleName := Token.Token;
  ModuleNameLine := Token.Line;
  ModuleNameCol := Token.Column;
  NextNonCommentToken;
  // Check for ';'
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token, 'OPUnit');
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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Package</B></TD>
      <TD>-></TD>
      <TD>PACKAGE Ident ';'</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ RequiresClause ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ ContainsClause ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>END '.'</TD>
    </TR>
  </TABLE>

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
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'OPPackage');
  ModuleName := Token.Token;
  ModuleNameLine := Token.Line;
  ModuleNameCol := Token.Column;
  NextNonCommentToken;
  // Check for ';'
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token, 'OPPackage');
  NextNonCommentToken;
  // Look for requires and contains clauses
  RequiresClause;
  ContainsClause;
  If Token.UToken <> 'END' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'END', Token,
      'OPPackage');
  NextNonCommentToken;
  If Token.Token <> '.' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, '.', Token,
      'OPPackage');
end;

(**

  This method parses the Library declaration from the current token using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Library</B></TD>
      <TD>-></TD>
      <TD>LIBRARY Ident ';'</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>END '.'</TD>
    </TR>
  </TABLE>

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
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'OPLibrary');
  ModuleName := Token.Token;
  ModuleNameLine := Token.Line;
  ModuleNameCol := Token.Column;
  NextNonCommentToken;
  // Check for ';'
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token, 'OPLibrary');
  NextNonCommentToken;
  ProgramBlock;
  // Check for '.'
  If Token.Token <> '.' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, '.', Token, 'OPLibrary');
end;

(**

  This method parses a program block from the current token position using
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ProgramBlock</B></TD>
      <TD>-></TD>
      <TD>[ UsesClause ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>Block</TD>
    </TR>
  </TABLE>

**)
procedure TPascalDocModule.ProgramBlock;
begin
  UsesClause;
  Block(scPublic, Nil);
end;

(**

  This method parses the Uses clause declaration from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Uses</B></TD>
      <TD>-></TD>
      <TD>USES IdentList ';'</TD>
    </TR>
  </TABLE>

**)
Procedure TPascalDocModule.UsesClause;

Var
  Comment : TComment;

Begin
  If Not (Token.UToken = 'USES') Then
    Exit;
  Comment := GetComment;
  NextNonCommentToken;
  If UsesCls = Nil Then
    Begin
      UsesCls := IdentList(True);
      UsesCls.Comment := Comment;
    End Else
    Begin
      UsesCls.Assign(IdentList(True));
      If UsesCls.Comment <> Nil Then
        UsesCls.Comment.Assign(Comment)
      Else
        UsesCls.Comment := Comment;
    End;
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'UsesClause');
  NextNonCommentToken;
End;

(**

  This method parses an interface section from the current token position using
  the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>InterfaceClause</B></TD>
      <TD>-></TD>
      <TD>INTERFACE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ UsesClause ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ InterfaceDecl ] ... </TD>
    </TR>
  </TABLE>

**)
Procedure TPascalDocModule.InterfaceSection;

Begin
  If Token.UToken <> 'INTERFACE' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'INTERFACE',
      Token, 'InterfaceSection');
  NextNonCommentToken;
  UsesClause;
  InterfaceDecl;
End;

(**

  This method parses an interface declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>InterfaceDecl</B></TD>
      <TD>-></TD>
      <TD>ConstSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>TypeSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>VarSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ExportedHeading</TD>
    </TR>
  </TABLE>

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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ExportedHeading</B></TD>
      <TD>-></TD>
      <TD>ProcedureHeading ';' [ Directive ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>FunctionHeading ';' [ Directive ]</TD>
    </TR>
  </TABLE>

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
        If Token.Token <> ';' Then
          Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
            'ExportedHeading');
        NextNonCommentToken;
      End;
  Until M = Nil;
End;

(**

  This method parses an exported procedure section from the current token
  position.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ExportedProcs</B></TD>
      <TD>-></TD>
      <TD>EXPORTS ExportsList ';'</TD>
    </TR>
  </TABLE>

  @postcon Returns true if an exported procedure was found.

  @return  a Boolean

**)
Function TPascalDocModule.ExportedProcs : Boolean;

Begin
  Result := Token.UToken = 'EXPORTS';
  If Not Result Then
    Exit;
  ExportsClause.Comment := GetComment;
  ExportsList;
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'ExportedProcs');
  NextNonCommentToken;
End;

(**

  This method parses an exports list from the current token position using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ExportsList</B></TD>
      <TD>-></TD>
      <TD>ExportsEnter / ',' ...</TD>
    </TR>
  </TABLE>

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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ExportsEntry</B></TD>
      <TD>-></TD>
      <TD>Ident [ INDEX IntegerConstant [ NAME StringConstant ] [ RESIDENT ] ]</TD>
    </TR>
  </TABLE>

**)
Procedure TPascalDocModule.ExportsEntry;

Var
  E : TGenericContainer;

Begin
  If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
    Raise EDocException.CreateNormal(strIdentExpected, Token, 'ExportsEntry');
  E := TGenericContainer.Create(Token.Token, scPublic, Token.Line, Token.Column);
  ExportsClause.Add(E);
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
End;

(**

  This method parses an implementation section from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ImplementationSection</B></TD>
      <TD>-></TD>
      <TD>IMPLEMENTATION</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ UsesClause ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD></TD>
      <TD>[ DeclSection ] ...</TD>
    </TR>
  </TABLE>

**)
Procedure TPascalDocModule.ImplementationSection;

Begin
  If Token.UToken <> 'IMPLEMENTATION' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'IMPLEMENTATION',
      Token, 'ImplementationSection');
  NextNonCommentToken;
  UsesClause;
  DeclSection(scPrivate, Nil);
End;

(**

  This method parses a block section from the current token position using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Block</B></TD>
      <TD>-></TD>
      <TD>[ DeclSection ]</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>CompoundStmt</TD>
    </TR>
  </TABLE>

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>DeclSection</B></TD>
      <TD>-></TD>
      <TD>LabelDeclSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ConstSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>TypeSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>VarSection</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>ProcedureDeclSection</TD>
    </TR>
  </TABLE>

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>LabelDeclSection</B></TD>
      <TD>-></TD>
      <TD>LABEL LabelId</TD>
    </TR>
  </TABLE>

  @postcon This method dicards the labels found and returns True if this method
           handles a label declaration section.
  @return  a Boolean

**)
Function TPascalDocModule.LabelDeclSection : Boolean;

Begin
  Result := Token.UToken = 'LABEL';
  If Not Result Then
    Exit;
  NextNonCommentToken;
  // We will ignore labels but treat them as IdentLists
  IdentList(True);
  // Check for ';'
  If Token.Token <> ';' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
      'LabelDeclSection');
  NextNonCommentToken;
End;

(**

  This method parses a constant section declaration from the current token
  position using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ConstSection</B></TD>
      <TD>-></TD>
      <TD>CONST ( ConstantDecl ';' ) ... </TD>
    </TR>
  </TABLE>

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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
  If Not Result Then
    Exit;
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
    Begin
      If Token.Token <> ';' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
          'ConstSection');
      NextNonCommentToken;
    End;
End;

(**

  This method parses a constant declaration from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>ConstantDecl</B></TD>
      <TD>-></TD>
      <TD>Ident '=' ConstExpr</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>Ident ':' TypeId '=' TypedConstant</TD>
    </TR>
  </TABLE>

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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
  If Token.TokenType <> ttIdentifier Then
    Exit;
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
      ConstExpr(C)
    End
  Else If Token.Token = ':' Then   // TypedConstant
    Begin
      C.Add(':');
      NextNonCommentToken;
      T := TypeDecl;
      If T <> Nil Then
        C.Append(T);
      If Token.Token <> '=' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, '=', Token,
          'ConstantDecl');
      C.Add('=');
      NextNonCommentToken;
      TypedConstant(C);
    End Else
      Raise EDocException.CreateLiteral(strLiteralExpected, '= or :', Token,
        'ConstantDecl');
End;

(**

  This method parses a resource string declaration section from the current
  token position.

  @see     For object pascal grammer see {@link TPascalDocModule.VarDecl}.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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
  // If not identifier then there is a new section
  If Token.TokenType <> ttIdentifier Then
    Exit;
  // Create constant and add to the collection, then get comment
  C := TResourceString.Create(Token.Token, Scope ,Token.Line, Token.Column);
  If Method = Nil Then
    ResourceStrings.Add(C)
  Else
    Method.ResStrings.Add(C);
  Result := True;
  C.Comment := GetComment;
  NextNonCommentToken;
  If Token.Token <> '=' then
    Raise EDocException.CreateLiteral(strLiteralExpected, '=', Token,
      'ResourceStringDecl');
  C.Add(Token.Token);
  NextNonCommentToken;
  ConstExpr(C);
  NextNonCommentToken;
End;

(**

  This method parses a type section from the current token position using the
  following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Typesection</B></TD>
      <TD>-></TD>
      <TD>TYPE ( TypeDecl ';' ) ... </TD>
    </TR>
  </TABLE>

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.
  @param   Scope  as a TScope
  @param   Method as a TMethodDecl
  @return  a Boolean

**)
Function TPascalDocModule.TypeSection(Scope : TScope; Method : TMethodDecl) : Boolean;

Var
  Ident : TIdentInfo;
  C : TComment;
  T : TGenericContainerCollection;
  Ty : TGenericContainer;

Begin
  Result := Token.UToken = 'TYPE';
  If Not Result Then
    Exit;
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
  Repeat
    // Get identifier
    If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
      Exit;
    C := GetComment; // Added to the ownedlist for disposal
    Ident.Ident := Token.Token;
    Ident.Line := Token.Line;
    Ident.Col := Token.Column;
    Ident.Scope := Scope;
    Ident.Method := Method;
    NextNonCommentToken;
    // Check for '='
    If Token.Token <> '=' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, '=', Token,
        'TypeSection');
    NextNonCommentToken;
    Ty := TypeDecl;
    If Ty = Nil Then
      Exit
    Else
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
      End;
    If Token.Token <> ';' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ';', Token,
        'TypeSection');
    NextNonCommentToken;
  Until False; // Infinate loop
End;

(**

  This method parses a type declaration section from the current token position
  using the following object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>TypeDecl</B></TD>
      <TD>-></TD>
      <TD>Ident '=' Type</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>Ident '=' RestrictedType</TD>
    </TR>
  </TABLE>

  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TTypes

**)
Function TPascalDocModule.TypeDecl : TTypes;

Begin
  Result := OPType;
  If Result = Nil Then Result := RestrictedType;
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
  Result := True;
  ConstExpr(C);
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

  @postcon This method returns True if this method handles a constant
           declaration section.
  @return  a TOrdinalType

**)
Function TPascalDocModule.OrdIdent : TOrdinalType;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strOrdIdents) Then Exit;
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

  @postcon Returns an ordinal type if one was parsed else returns nil.

  @return  a TOrdinalType

**)
Function TPascalDocModule.SubRangeType : TOrdinalType;

Begin
  // This is implemented as a CATCH ALL.
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

  @postcon Returns an ordinal type if one was parsed else returns nil.

  @return  a TOrdinalType

**)
Function TPascalDocModule.EnumerateType : TOrdinalType;

Var
  I : TIdentList;
  j : Integer;

Begin
  Result := Nil;
  If Not (Token.Token = '(') Then
    Exit;
  Result := TOrdinalType.Create;
  SymbolTable.Add(Result);
  NextNonCommentToken;
  Result.Add('(');
  I := IdentList(False);
  Try
    For j := 0 To I.Count - 1 Do
      Begin
        If j <> 0 Then
          Result.Add(',');
        Result.Add(I[j].Ident);
      End;
  Finally
    I.Free;
  End;
  If Token.Token <> ')' Then
    Raise EDocException.CreateLiteral(strLiteralExpected, ')', Token,
      'EnumerateType');
  Result.Add(')');
  NextNonCommentToken;
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
      If Token.Token <> ']' Then
        Raise EDocException.CreateLiteral(strLiteralExpected, ']', Token,
          'StringType');
      Result.Add(Token.Token);
      NextNonCommentToken;
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
  If Result = Nil Then Result := SetType(boolPacked);
  If Result = Nil Then Result := FileType(boolPacked);
  If Result = Nil Then Result := RecType(boolPacked);
end;

(**

  This method adds a specific documentation conflict to the Docuemntation
  conflict collection.

  @precon  None.
  @postcon Adds a specific documentation conflict to the Docuemntation
           conflict collection.

  @param   Args            as an Array Of TVarRec constant
  @param   iIdentLine      as an Integer
  @param   iIdentColumn    as an Integer
  @param   iCommentLine    as an Integer
  @param   iCommentCol     as an Integer
  @param   DocConflictType as a TDocConflictType

**)
procedure TPascalDocModule.AddDocumentConflict(Const Args: Array of TVarRec;
  iIdentLine, iIdentColumn, iCommentLine, iCommentCol : Integer; DocConflictType: TDocConflictType);
begin
  FDocumentConflicts.Add(TDocumentConflict.Create(Args, iIdentLine,
    iIdentColumn, iCommentLine, iCommentCol, DocConflictType));
end;

(**

  This method adds a timer count to the modules OpTickCount collection. This
  can be used to provide timing / profiling information on operations.

  @precon  None.
  @postcon Adds a timer count to the modules OpTickCount collection. This
           can be used to provide timing / profiling information on operations.

  @param   strLabel as a String

**)
procedure TPascalDocModule.AddTickCount(strLabel: String);
begin
  FTickList.AddObject(strLabel, TObject(GetTickCount));
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

  @postcon This method returns True if this method handles a constant
           declaration section.
  @precon  boolPacked determines if the array type is packed or not.

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
  If Token.UToken <> 'OF' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'OF', Token,
      'ArrayType');
  Result.Add(Token.Token);
  NextNonCommentToken;
  While Not (Token.Token[1] In [';', '=']) Do
    Begin
      Result.Add(Token.Token);
      NextNonCommentToken;
    End;
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
  If Token.UToken <> 'OF' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'OF', Token,
      'SetType');
  Result.Add(Token.Token);
  NextNonCommentToken;
  While Not (Token.Token[1] In [';', '=']) Do
    Begin
      Result.Add(Token.Token);
      NextNonCommentToken;
    End;
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
  If Token.UToken <> 'END' Then
    Raise EDocException.CreateLiteral(strReservedWordExpected, 'END', Token,
      'RecType');
  NextNonCommentToken;
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
  @param   Rec as a TRecordDecl

**)
Procedure TPascalDocModule.FieldDecl(Rec: TRecordDecl);

Var
  I : TIdentList;
  j : Integer;
  P : TParameter;
  T : TTypes;


Begin
  I := IdentList(False);
  Try
    If Token.Token <> ':' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token,
        'FieldList');
    NextNonCommentToken;
    T := TypeDecl;
    // Create record fields
    For j := 0 To I.Count - 1 Do
      Begin
        P :=  TParameter.Create(pmNone, I[j].Ident, False, T, '',
          scPublic, I[j].Line, I[j].Col);
        P.Comment := I[j].Comment;
        Rec.AddParameter(P);
      End;
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
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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

  This method parses a resource string declaration section from the current
  token position.

  @see     For object pascal grammer see {@link TPascalDocModule.ConstantSection}.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section.
  @precon  The Method parameter is nil for methods in the implementation
           section or a reference to a method for a local declaration section
           with in a method.
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

  This method parses a variable declaration from the current token position.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>VarDecl</B></TD>
      <TD>-></TD>
      <TD>IdentList ':' Type [ ( ABSOLUTE ( Ident | ConstExpr ) ) | '=' ConstExpr ] </TD>
    </TR>
  </TABLE>

  @precon  Scope defines the current scope of the variable.
  @precon  VarSection is a valid variable container for the storage of the
           variable declared.
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
  I := IdentList(False);
  Try
    If Token.Token <> ':' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token, 'VarDecl');
    NextNonCommentToken;
    T := TypeDecl;
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

(**

  This method skips statements with in the implementation section of
  procedures and programs.

  @precon  iBlockCount is a referenced count er to the current level of
           nested statement block. 

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

  @precon  Scope is the current scope of the procedure declaration.
  @precon  Method is the current method scoped else nil.
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
        Token.Line, Token.Column, 'FunctionHeading', etWarning);
  If Result <> Nil Then
    Directive(Result);
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
  If Result <> Nil Then
    Directive(Result);
End;

(**

  This method does the donkey work for parsing the main portion of a method
  declaration on behalf of the ####Heading functions.

  @precon  ProcType is the type if method the be handled, proedure, function, etc.
  @precon  Scope is the scope of th method.
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

  @precon  Method is a valid method to add a parameter too.
  @precon  ParamMod is a parameter modifier for the parameter to signify a
           const, var or out paramemter.

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
  I := IdentList(False);
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
        T := TypeDecl;
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

  This method retrives the method directives after the method declaration from
  the current token position using the followong object pascal grammer.

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>Directive</B></TD>
      <TD>-></TD>
      <TD>REGISTER</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>DYNAMIC</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>VIRTUAL</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>EXPORT</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>EXTERNAL</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>FAR</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>FORWARD</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>MESSAGE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>OVERRIDE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>OVERLOAD</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>PASCAL</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>REINTRODUCE</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>SAFECALL</TD>
    </TR>
    <TR>
      <TD></TD>
      <TD>-></TD>
      <TD>STDCALL</TD>
    </TR>
  </TABLE>

  @precon  M is a valid method declaration to add directives too.

  @param   M as a TMethodDecl

**)
Procedure TPascalDocModule.Directive(M : TMethodDecl);

Begin
  If Token.Token = ';' Then
    NextNonCommentToken;
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
      If Token.Token = ';' Then
        NextNonCommentToken;
    End;
  If PrevToken.Token = ';' Then
    RollBackToken;
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

  @precon  Cls is an object declaration to add methods too.
  @precon  Scopeis the current internal scope of the object.
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

  @precon  Cls is an object declaration to add method declarations too.
  @precon  Scope is the current scope inside the object declaration.
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
      Cls.AddMethod(M);
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

  @precon  Cls is an ibject delcaration to add fields too.
  @precon  Scope is the current internal scope of the object.
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
  I := IdentList(False);
  Try
    If Token.Token <> ':' Then
      Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token,
        'ObjFieldList');
    Result := True;
    NextNonCommentToken;
    T := TypeDecl;
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

  @param   Cls as a TObjectDecl

**)
procedure TPascalDocModule.ClassHeritage(Cls: TObjectDecl);

Var
  I : TIdentList;

begin
  If Token.Token <> '(' Then
    Exit;
  NextNonCommentToken;
  I := IdentList(False);
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

  @precon  Cls is a valid object declaration to add fields too.
  @precon  Scope is the current scope of the class.
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

  @precon  Cls is a valid object declaration to get method for.
  @precon  Scope is the current scope of the class.
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

  @precon  Cls is a valid class declaration to get method for.
  @precon  Scope is the current scope of the class.
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

  @precon  Cls is a valid class declaration to get method for.
  @precon  Scope is the current scope of the class.
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
        I := IdentList(False);
        Try
          If Token.Token <> ':' Then
            Raise EDocException.CreateLiteral(strLiteralExpected, ':', Token,
              'PropertyParameterList');
          NextNonCommentToken;
          T := TypeDecl;
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

**)
Procedure TPascalDocModule.RequiresClause;

Var
  Comment : TComment;

Begin
  If Not (Token.UToken = 'REQUIRES') Then
    Exit;
  Comment := GetComment;
  NextNonCommentToken;
  Requires := IdentList(True);
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

**)
Procedure TPascalDocModule.ContainsClause;

Var
  Comment : TComment;

Begin
  If Not (Token.UToken = 'CONTAINS') Then
    Exit;
  Comment := GetComment;
  NextNonCommentToken;
  Contains := IdentList(True);
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

  <P><B><U>Object Pascal Grammer</U></B></P>
  <TABLE>
    <TR>
      <TD><B>IdentList</B></TD>
      <TD>-></TD>
      <TD>Ident / ',' ...</TD>
    </TR>
  </TABLE>

  @precon  OwnList determines if the identlist should be disposed of be the
           parser or be the caller.
  @postcon Returns an ident list.

  @param   OwnList as a Boolean
  @return  a TIdentList

**)
Function TPascalDocModule.IdentList(OwnList : Boolean) : TIdentList;

Begin
  Result := TIdentList.Create;
  // Add to owned list immediately if required to ensure that is an expcetion
  // is raised the memory is released in the destructor
  If OwnList Then
    OwnedItems.Add(Result);
  While Not EndOfTokens Do
    Begin
      // This has been disable because delphi will allow the use of reserved
      // word as identifiers
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
        Raise EDocException.CreateNormal(strIdentExpected, Token, 'IdentList');
      Result.Add(Token.Token, Token.Line, Token.Column, GetComment);
      NextNonCommentToken;
      (* This is a change to the grammer to handle "in 'something.pas'" *)
      If Token.UToken = 'IN' Then
        Begin
          NextNonCommentToken;
          If Token.TokenType <> ttStringLiteral Then
            Raise EDocException.CreateNormal(strStringExpected, Token,
              'IdentList');
          NextNonCommentToken;
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
  Result := True;
  iCounter := 0;
  ITokens := 0;
  While (Not (Token.Token[1] In strConstExprTerminals) Or (iCounter > 0)) And
    Not IsKeyWord(Token.Token, strTerminalWords) Do
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
    Raise EDocException.CreateNormal('Loop detected.', Token, 'ConstExpr');
End;

{ TPascalDocModuleList }

(**

  This method add the source pascal doc module to the collection.

  @precon  Source is a valid instance of a TPascalDocModule class.

  @param   Source as a TPascalDocModule

**)
procedure TPascalDocModuleList.Add(Source: TPascalDocModule);
begin
  If Source <> Nil Then FModules.Add(Source);
end;

(**

  This is the constructor method for the TPascalDocModuleList class.

**)
constructor TPascalDocModuleList.Create;
begin
  FModules := TObjectList.Create(True);
end;

(**

  This method deletes the indexed module from the collection.

  @precon  iIndex is the index of the module to delete.

  @param   iIndex as an Integer

**)
procedure TPascalDocModuleList.Delete(iIndex: Integer);
begin
  FModules.Delete(iIndex);
end;

(**

  This is the destructor method for the TPascalDocModuleList class.

**)
destructor TPascalDocModuleList.Destroy;
begin
  FModules.Free;
  inherited;
end;

(**

  This is a getter method for the Count property.

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
