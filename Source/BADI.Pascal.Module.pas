(**

  ObjectPascalModule : A unit to tokenize Pascal source code.

  @Version    2.0
  @Date       24 Feb 2017
  @Author     David Hoyle

  @grammar    For the grammar to this parser pleaser see the "Object Pascal Grammar.bnf".

  @todo       Implement an expression parser for the above compiler defines.
              Needs to be able to evaluate constants in the code and use the
              two functions Defined() and Declared().

**)
Unit BADI.Pascal.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.ElementContainer,
  BADI.Types,
  BADI.Pascal.MethodDecl,
  BADI.Pascal.TypeDecl,
  BADI.Generic.TypeDecl,
  BADI.Pascal.RecordDecl,
  BADI.Generic.FunctionDecl,
  BADI.Pascal.ObjectDecl,
  BADI.Pascal.ClassDecl,
  BADI.Pascal.PropertyDecl,
  BADI.Pascal.InterfaceDecl,
  BADI.Comment,
  BADI.TokenInfo, BADI.Pascal.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  //: @debug TObjectDecl = Class;

  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TPascalModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource                  : String;
    FMethodStack             : TObjectList;
    FTypesLabel              : TLabelContainer;
    FConstantsLabel          : TLabelContainer;
    FResourceStringsLabel    : TLabelContainer;
    FVariablesLabel          : TLabelContainer;
    FThreadVarsLabel         : TLabelContainer;
    FExportedHeadingsLabel   : TLabelContainer;
    FExportsHeadingsLabel    : TLabelContainer;
    FImplementedMethodsLabel : TLabelContainer;
    FExternalSyms            : TStringList;
    FModuleType              : TModuleType;
    FSourceCodeForProfiling  : TStringList;
    { Grammar Parsers }
    Procedure Goal;
    Function  OPProgram : Boolean;
    Function  OPUnit : Boolean;
    Function  OPPackage : Boolean;
    Function  OPLibrary : Boolean;
    Procedure ProgramBlock;
    procedure UsesClause;
    Function  PortabilityDirective : Boolean;
    Procedure InterfaceSection;
    Procedure InterfaceDecl;
    Function  ExportedHeading(Container : TElementContainer) : Boolean;
    Procedure ImplementationSection;
    Procedure Block(AScope : TScope; Method : TPascalMethod);
    Function  ExportsStmt : Boolean;
    procedure ExportsItem(Container : TElementContainer);
    Procedure DeclSection(AScope : TScope; Container : TElementContainer);
    Function  LabelDeclSection(Container : TElementContainer) : Boolean;
    Function  ConstSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ConstantDecl(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ResStringSection(AScope: TScope): Boolean;
    Function  ResourceStringDecl(AScope: TScope; Container : TElementContainer): Boolean;
    Function  TypeSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  TypeDecl(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  GetTypeDecl(AToken : TTypeToken) : TTypes;
    Function  TypedConstant(C: TElementContainer; T : TGenericTypeDecl) : Boolean;
    Function  ArrayConstant(C: TElementContainer; T : TGenericTypeDecl) : Boolean;
    Function  RecordConstant(C: TElementContainer; T : TGenericTypeDecl) : Boolean;
    Function  RecordFieldConstant(C : TElementContainer; T : TGenericTypeDecl) : Boolean;
    function  OPType(AToken : TTypeToken) : TTypes;
    function  RestrictedType(AToken : TTypeToken) : TRestrictedType;
    Function  ClassRefType(AToken : TTypeToken) : TClassRefType;
    Function  SimpleType(AToken : TTypeToken) : TSimpleType;
    function  RealType(AToken : TTypeToken) : TRealType;
    function  OrdinalType(AToken : TTypeToken) : TOrdinalType;
    function  OrdIdent(AToken : TTypeToken) : TOrdIdent;
    Function  VariantType(AToken : TTypeToken) : TVariantType;
    function  SubRangeType(AToken : TTypeToken) : TSubRangeType;
    function  EnumerateType(AToken : TTypeToken) : TEnumerateType;
    Procedure EnumerateElement(EnumerateType : TEnumerateType);
    Function  StringType(AToken : TTypeToken) : TStringType;
    Function  StrucType(AToken : TTypeToken) : TTypes;
    function  ArrayType(boolPacked : Boolean; AToken : TTypeToken): TArrayType;
    Function  RecType(boolPacked : Boolean; AToken : TTypeToken) : TRecordDecl;
    Procedure RecordVisibility(var AScope : TScope);
    Function  RecordTypeSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  RecordConstSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  RecordVarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  RecordClassVarSection(AScope : TScope; Cls : TRecordDecl) : Boolean;
    Function  RecordMethodList(Cls : TRecordDecl; AScope : TScope;
      PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  RecordPropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Function  RecordFieldList(Rec : TRecordDecl; InternalScope : TScope) : Boolean;
    Function  FieldDecl(Rec: TRecordDecl; InteralScope : TScope) : Boolean;
    procedure RecVariant(Rec: TRecordDecl; InternalScope : TScope);
    function  SetType(boolPacked: Boolean; AToken : TTypeToken): TSetType;
    function  FileType(boolPacked: Boolean; AToken : TTypeToken): TFileType;
    Function  RecordVariantSection(Rec: TRecordDecl; InternalScope : TScope) : Boolean;
    Function  PointerType(AToken : TTypeToken) : TPointerType;
    Function  ProcedureType(AToken : TTypeToken) : TProcedureType;
    Function  AnonymousReferenceType(AToken : TTypeToken) : TAnonymousReferenceType;
    Function  VarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ThreadVarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  VarDecl(AScope : TScope; VarSection : TElementContainer;
      AImageIndex : TBADIImageIndex) : Boolean;
    Function  ThreadVarDecl(AScope : TScope; VarSection : TElementContainer) : Boolean;
    Procedure Expression(C : TElementContainer; var ExprType : TPascalExprTypes);
    Procedure SimpleExpression(C : TElementContainer; var ExprType : TPascalExprTypes);
    Procedure Term(C : TElementContainer; var ExprType : TPascalExprTypes);
    Procedure Factor(C : TElementContainer; var ExprType : TPascalExprTypes);
    Function  RelOp(C : TElementContainer; ExprType : TPascalExprTypes) : Boolean;
    Function  AddOp(C : TElementContainer) : Boolean;
    Function  MulOp(C : TElementContainer; var ExprType : TPascalExprTypes) : Boolean;
    Function  Designator(C : TElementContainer; var ExprType : TPascalExprTypes) : Boolean;
    Procedure DesignatorSubElement(C : TElementContainer; var ExprType : TPascalExprTypes;
      strValidSymbols : Array of String);
    Function  SetConstructor(C : TElementContainer) : Boolean;
    Procedure SetElement(C : TElementContainer);
    Procedure ExprList(C : TElementContainer);
    Procedure MethodExprList(C : TElementContainer);
    Procedure Statement;
    Procedure StmtList;
    Procedure SimpleStatement;
    Function  StructStmt : Boolean;
    Function  CompoundStmt(Method : TGenericFunction) : Boolean;
    Function  ConditionalStmt : Boolean;
    Function  IfStmt : Boolean;
    Function  CaseStmt : Boolean;
    Procedure CaseSelector;
    Procedure CaseLabel;
    Function  LoopStmt : Boolean;
    Function  RepeatStmt : Boolean;
    Function  WhileStmt : Boolean;
    Function  ForStmt : Boolean;
    Function  WithStmt : Boolean;
    Function  TryExceptAndFinallyStmt : Boolean;
    Function  ExceptionBlock : Boolean;
    Function  RaiseStmt : Boolean;
    Function  AssemblerStatement : Boolean;
    Function  ProcedureDeclSection(AScope : TScope) : Boolean;
    Function  ProcedureDecl(AScope : TScope) : TPascalMethod;
    Function  FunctionDecl(AScope : TScope) : TPascalMethod;
    Function  ConstructorDecl(AScope : TScope) : TPascalMethod;
    Function  DestructorDecl(AScope : TScope) : TPascalMethod;
    Function  OperatorDecl(AScope : TScope) : TPascalMethod;
    Function  FunctionHeading(AScope :TScope; Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;
    Function  ProcedureHeading(AScope : TScope; Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;
    Function  OperatorHeading(AScope :TScope; Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;
    Procedure FormalParameter(Method : TPascalMethod);
    Procedure FormalParam(Method : TPascalMethod);
    Procedure Parameter(Method : TPascalMethod; ParamMod : TParamModifier);
    Procedure Directive(M : TPascalMethod; boolGrammarFix : Boolean = False);
    Function  ObjectType(AToken : TTypeToken) : TObjectDecl;
    Procedure ObjHeritage(ObjDecl : TObjectDecl);
    Procedure ObjVisibility(var AScope : TScope);
    Function  ObjTypeSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ObjConstSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ObjVarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ObjClassVarSection(AScope : TScope; Cls : TRecordDecl) : Boolean;
    Function  ObjMethodList(Cls : TRecordDecl; AScope : TScope;
      PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  ObjPropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Function  ObjFieldList(Rec : TRecordDecl; InternalScope : TScope) : Boolean;
    Function  MethodList(Cls : TRecordDecl; AScope : TScope;
      PermissibleMethods : TPermissibleMethods) : Boolean;
    function  MethodHeading(Cls: TRecordDecl; AScope: TScope;
      PermissibleMethods : TPermissibleMethods): Boolean;
    Function  ConstructorHeading(AScope :TScope; Container : TElementContainer) : TPascalMethod;
    Function  DestructorHeading(AScope :TScope; Container : TElementContainer) : TPascalMethod;
    Procedure InitSection;
    Function  ClassType(AToken : TTypeToken) : TClassDecl;
    Procedure RecObjClsIntHeritage(RecObjClsInt : TRecordDecl);
    procedure ClassVisibility(var AScope : TScope);
    Function  ClassTypeSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ClassConstSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ClassVarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function  ClassClassVarSection(AScope : TScope; Cls : TRecordDecl) : Boolean;
    Function  ClassMethodList(Cls : TRecordDecl; AScope : TScope;
      PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  ClassPropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Function  ClassFieldList(Cls : TObjectDecl; AScope : TScope) : Boolean;
    Function  FieldList(Cls : TObjectDecl; AScope : TScope) : Boolean;
    Function  PropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Procedure PropertyInterface(Prop : TPascalProperty);
    Procedure PropertyParameterList(Prop : TPascalProperty);
    Procedure PropertySpecifiers(Prop : TPascalProperty);
    Function  InterfaceType(AToken : TTypeToken) : TInterfaceDecl;
    Procedure InterfaceHeritage(InterfaceDecl : TInterfaceDecl);
    Function  InterfaceMethodList(Cls : TRecordDecl; AScope : TScope;
      PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  InterfacePropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Procedure RequiresClause;
    procedure ContainsClause;
    Procedure IdentList(Container : TElementContainer; SeekTokens : Array Of String;
      iImageIndex : TBADIImageIndex = iiNone);
    Function  TypeId(Container: TElementContainer) : Boolean;
    Function  ConstExpr(Container: TElementContainer; var ExprType : TPascalExprTypes) : Boolean;
    Procedure RTTIAttributes;
    Procedure AttributeDeclaration;
    Procedure TypeParams(var strIdentifier : String);
    Procedure TypeParamDeclList(var strIdentifier : String);
    Procedure TypeParamDecl(var strIdentifier : String);
    Procedure TypeParamList(var strIdentifier : String);
    Procedure ConstraintList;
    Procedure Constraint;
    Procedure FormalTypeParamList(var strIdentifier : String);
    Function  MethodQualifiers(AScope :TScope;
      Container : TElementContainer; iMethodType : TMethodType; boolClassMethod : Boolean;
      C : TComment; boolIdent : Boolean = True) : TPascalMethod;
    Procedure TypeArgs(Container : TElementContainer);
    Function  AnonymousMethod(Container : TElementContainer) : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    procedure ArrayElement(C : TElementContainer; iStartDimension: Integer; AT : TArrayType);
    Function  CheckReturnValue(Method : TGenericFunction) : Boolean;
    Procedure CheckAlias(Method : TPascalMethod);
    Function  CheckNumberType(ExprType : TPascalExprTypes) : Boolean;
    Procedure UpdateTypeToken(var AToken : TTypeToken); {$IFDEF D2005} InLine; {$ENDIF}
    Procedure AddToContainer(Container : TElementContainer; var Method : TPascalMethod);
    Procedure TidyUpEmptyElements;
    Procedure CheckUnResolvedMethods;
    procedure ResolveScopeOfImplementedMethods(StartLabel : TLabelContainer);
    procedure ResolveScopeOfImplementedExportedMethods;
    procedure ResolveScopeOfImplementedExportsMethods;
    procedure FindUnresolvedRecordObjectAndClassMethods(TypeLabel : TLabelContainer);
    procedure FindUnresolvedExportedMethods;
    {procedure FindUnresolvedExportsMethods;}
    procedure FindUnresolvedImplementedClassMethods(StartLabel : TLabelContainer);
    Function  FindRecObjClsInt(slClassNames : TStringList) : TRecordDecl;
    Function  IsIdentifier(AToken : TTokenInfo) : Boolean;
    Procedure ProcessIncludeDirective(strToken : String);
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function  GetCurrentMethod: TPascalMethod;
    Function  GetModuleName : String; Override;
    Function  GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Procedure CheckFunctionReturn(Func : TPascalMethod);
    (**
      This property returns the method on top of the method stack.
      @precon  None.
      @postcon Returns the method on top of the method stack else returns nil.
      @return  a TPascalMethod
    **)
    Property CurrentMethod : TPascalMethod Read GetCurrentMethod;
    (**
      Returns the type of the modules, Program, Unit, Package, etc.
      @precon  None.
      @postcon Returns the type of the modules, Program, Unit, Package, etc.
      @return  a TModuleType
    **)
    Property ModuleType : TModuleType Read FModuleType Write FModuleType;
  Public
    Constructor CreateParser(const Source, strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function DefaultProfilingTemplate : String; Override;
  End;

Implementation

Uses
  DGHLibrary,
  BADI.Functions,
  BADI.Options,
  BADI.Constants,
  BADI.ResourceStrings,
  BADI.CompilerConditionData,
  BADI.Module.Dispatcher,
  BADI.Pascal.Comment,
  BADI.Pascal.Constants,
  BADI.Pascal.ExportsItem,
  BADI.Pascal.ConstantDecl,
  BADI.Pascal.TempCntr,
  BADI.Pascal.ResourceStrings,
  BADI.Pascal.Functions,
  BADI.Pascal.ResourceStringDecl, BADI.Pascal.IdentList, BADI.Pascal.FieldDecl,
  BADI.Pascal.InitializationDecl, BADI.Pascal.FinalizationDecl, BADI.Pascal.VariableDecl,
  BADI.Pascal.ParameterDecl, BADI.Pascal.PropertySpec, BADI.Pascal.DispInterfaceDecl,
  BADI.Pascal.ThreadVariableDecl;

(**

  This is the constructor method for the TPascalDocModule class.

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
Constructor TPascalModule.CreateParser(const Source, strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);
var
  boolCascade: Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FTypesLabel              := Nil;
  FConstantsLabel          := Nil;
  FResourceStringsLabel    := Nil;
  FVariablesLabel          := Nil;
  FThreadVarsLabel         := Nil;
  FExportedHeadingsLabel   := Nil;
  FExportsHeadingsLabel    := Nil;
  FImplementedMethodsLabel := Nil;
  FModuleType := mtUnit;
  FExternalSyms := TStringList.Create;
  FExternalSyms.Sorted := True;
  {$IFDEF D0006}
  FExternalSyms.CaseSensitive := False;
  {$ENDIF}
  FMethodStack := TObjectList.Create(False);
  FSourceCodeForProfiling := TStringList.Create;
  If moProfiling In ModuleOptions Then
    FSourceCodeForProfiling.Text := Source;
  CompilerDefines.Assign(BrowseAndDocItOptions.Defines);
  FSource := Source;
  AddTickCount('Start');
  CommentClass := TPascalComment;
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount('Parse');
      CheckUnResolvedMethods;
      AddTickCount('Resolve');
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


  This is a destructor for the TPascalModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TPascalModule.Destroy;
begin
  FSourceCodeForProfiling.Free;
  FExternalSyms.Free;
  FMethodStack.Free;
  Inherited Destroy;
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TPascalModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btLineComment, btBraceComment,
    btFullComment, btCompoundSymbol);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;
  strSingleSymbols : Set Of AnsiChar = ['(', ')', ';', ',', '[', ']', '^',
    '-', '+', '/', '*', '<', '>'];

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
  LastToken : TBADITokenType;
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
  LastToken := ttUnknown;

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For iChar := 1 To Length(FSource) Do
    Begin
      ch := FSource[iChar];
      Inc(iStreamCount);
      LastCharType := CurCharType;

      If IsInSet(ch, strWhiteSpace) Then
        CurCharType := ttWhiteSpace
      Else If isInSet(ch, strTokenChars) Then
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
      Else If IsInSet(ch, strLineEnd) Then
        CurCharType := ttLineEnd
      Else If IsInSet(ch, strQuote) Then
        CurCharType := ttSingleLiteral
      Else If IsInSet(ch, strSymbols) Then
        Begin
          If (LastCharType = ttNumber) And (IsInSet(LastChar, ['e', 'E'])) And
            (IsInSet(Ch, ['-', '+'])) Then
            CurCharType := ttNumber
          Else
            CurCharType := ttSymbol
        End
      Else
        CurCharType := ttUnknown;

      // Check for full block comments
      If (BlockType = btNoBlock) And (LastChar = '(') And (Ch = '*') Then
        BlockType := btFullComment;

      // Check for line comments
      If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '/') Then
        BlockType := btLineComment;

      If (LastCharType <> CurCharType) Or (IsInSet(Ch, strSingleSymbols)) Or
        (IsInSet(LastChar, strSingleSymbols)) Then
        Begin
          If ((BlockType In [btStringLiteral, btLineComment]) And
            (CurCharType <> ttLineEnd)) Or
            (BlockType In [btBraceComment, btFullComment, btCompoundSymbol]) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := Ch;
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
                  Begin
                    If LastCharType = ttIdentifier Then
                      Begin
                        If IsKeyWord(strToken, strReservedWords) Then
                          LastCharType := ttReservedWord;
                        If IsKeyWord(strToken, strDirectives) Then
                          LastCharType := ttDirective;
                        If strToken[1] = '#' Then
                          LastCharType := ttSingleLiteral;
                      End;
                    If BlockType = btLineComment Then
                      LastCharType := ttLineComment;
                    If (LastCharType = ttBlockComment) And (Length(strToken) > 2) Then
                      If (strToken[1] = '{') And (strToken[2] = '$') Then
                        LastCharType := ttCompilerDirective;
                    If ((LastToken = ttNumber) And ((strToken = '.') Or (LastCharType = ttNumber))) Or
                      ((LastToken = ttSingleLiteral) And (strToken[1] = '#')) Or
                      ((LastToken = ttSingleLiteral) And (LastCharType = ttSingleLiteral)) Then
                      Begin
                        AppendToLastToken(strToken);
                        LastToken := LastToken; //: @bug What was I thinking??????
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
          CurCharType := ttBlockComment;
        End;

      // Check for string literals
      If CurCharType = ttSingleLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

      // Check for block Comments
      If (BlockType = btNoBlock) And (Ch = '{') Then
        Begin
          CurCharType := ttBlockComment;
          BlockType := btBraceComment;
        End;
      If (BlockType = btBraceComment) And (Ch = '}') Then
        Begin
          CurCharType := ttBlockComment;
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
    End;
    If iTokenLen > 0 Then
      Begin
        SetLength(strToken, iTokenLen);
        If CurCharType = ttIdentifier Then
          Begin
            If IsKeyWord(strToken, strReservedWords) Then
              CurCharType := ttReservedWord;
            If IsKeyWord(strToken, strDirectives) Then
              CurCharType := ttDirective;
          End;
        If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
          AddToken(TTokenInfo.Create(strToken, iStreamPos,
            iTokenLine, iTokenColumn, Length(strToken), CurCharType));
      End;
End;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TPascalModule.ParseTokens;
begin
  Goal;
end;

(**

  This method adds a pascal method is the given container. If Nil adds to the
  implemented methods.

  @precon  Method must be a valid TPascalMethod.
  @postcon Adds a pascal method is the given container. If Nil adds to the
           implemented methods.

  @param   Container as a TElementContainer
  @param   Method    as a TPascalMethod as a reference

**)
procedure TPascalModule.AddToContainer(Container: TElementContainer;
  var Method: TPascalMethod);

Var
  iIcon : TBADIImageIndex;
  AScope : TScope;
  E : TElementContainer;
  tmpMethod : TPascalMethod;
  iCls: Integer;
  MethodsLabel : TLabelContainer;

begin
  If Method <> Nil Then
    Begin
      If Container = Nil Then
        Begin
          If Method.Identifier <> '' Then
            Begin
              If FImplementedMethodsLabel = Nil Then
                FImplementedMethodsLabel := Add(strImplementedMethodsLabel,
                  iiImplementedMethods, scNone, Nil) As TLabelContainer;
              Container := FImplementedMethodsLabel;
            End else
            Begin
              E := CurrentMethod.FindElement(strAnonymousMethods);
              If E = Nil Then
                E := CurrentMethod.Add(strAnonymousMethods, iiMethodsLabel, scNone, Nil);
              Container := E;
            End;
          iIcon := iiUnknownClsObj;
          AScope := scNone;
          E := FTypesLabel;
          If E <> Nil Then
            For iCls := 0 To Method.ClassNames.Count - 1 Do
              If E <> Nil Then
                Begin
                  E := E.FindElement(Method.ClassNames[iCls]);
                  If E <> Nil Then
                    Begin
                      iIcon := E.ImageIndex;
                      AScope := E.Scope;
                      E := E.FindElement('Types');
                    End;
                  Container := Container.Add(TLabelContainer.Create(
                    Method.ClassNames[iCls], AScope, 0, 0, iIcon, Nil));
                  // Need to add a Types container to the E.FindElement search before
                  // looking for another class.
                End;
        End;
      If Container Is TRecordDecl Then
        Begin
          MethodsLabel := (Container As TRecordDecl).FindElement(strMethodsLabel) As TLabelContainer;
          If MethodsLabel = Nil Then
            MethodsLabel := Container.Add(
              strMethodsLabel, iiMethodsLabel, scNone, Nil) As TLabelContainer;
          Container := MethodsLabel;
        End;
      tmpMethod := Method;
      Method := Container.Add(tmpMethod) As TPascalMethod;
      If tmpMethod <> Method Then
        AddIssue(Format(strDuplicateIdentifierFound, [Method.Identifier,
          Method.Line, Method.Column]), scNone,  'AddToContainer',
          tmpMethod.Line, tmpMethod.Column, etError);
    End;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TPascalModule.ReservedWords: TKeyWords;

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
function TPascalModule.Directives: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strDirectives)));
  For i := Low(strDirectives) To High(strDirectives) Do
    Result[i] := strDirectives[i];
end;

(**


  This is a getter method for the CurrentMethod property.

  @precon  None.
  @postcon Returns the method on top of the method stack else returns nil.


  @return  a TPascalMethod

**)
function TPascalModule.GetCurrentMethod: TPascalMethod;
begin
  If FMethodStack.Count = 0 Then
    Result := Nil
  Else
    Result := FMethodStack[FMethodStack.Count - 1] As TPascalMethod;
end;

(**

  This is a getter method for the ModuleName property.

  @precon  None.
  @postcon Overrides the inherited method to place the module type in front of
           the module name.

  @return  a String

**)
Function TPascalModule.GetModuleName : String;

Begin
  Result := Inherited GetModuleName;
  Result := strModuleTypes[ModuleType] + #32 + Result;
End;

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
Function TPascalModule.GetComment(
  CommentPosition : TCommentPosition) : TComment;

Var
  T : TTokenInfo;
  iOffset : Integer;
  iToken: TTokenIndex;
  iBracket: Integer;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  iToken := TokenIndex + iOffset;
  // If there is an RTTI Attribute, walk backwards through the tokens to just before.
  If iToken > -1 Then
    While Tokens[iToken].Token = ']' Do
      Begin
        iBracket := 1;
        Dec(iToken);
        While (iToken > -1) And (iBracket > 0) Do
          Begin
            If Tokens[iToken].Token = ']' Then
              Inc(iBracket);
            If Tokens[iToken].Token = '[' Then
              Dec(iBracket);
            Dec(iToken);
          End;
      End;
  If iToken > -1 Then
    Begin
      T := Tokens[iToken] As TTokenInfo;
      If T.TokenType In [ttLineComment, ttBlockComment] Then
        Begin
          Result := TPascalComment.CreateComment(T.Token, T.Line, T.Column);
          OwnedItems.Add(Result);
        End;
    End;
End;

(**

  This method is the starting position for the parsing of an object pascal
  module. It finds the first non comment token and begins the grammar checking
  from their by deligating to the program, library, unit and package methods.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating to the program, library, unit and package
           methods.

**)
procedure TPascalModule.Goal;

Var
  boolHasProcessed : Boolean;

begin
  Try
    If TokenCount > 0 Then
      Begin
        // Find first non comment token
        While (Token.TokenType In [ttLineComment, ttBlockComment,
          ttCompilerDirective]) And Not EndOfTokens Do
          NextNonCommentToken;
        // Check for end of file else must be identifier
        If Not EndOfTokens Then
          Begin
            Comment := GetComment;
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
          Begin
            AddIssue(strUnExpectedEndOfFile, scNone, 'Goal', 0, 0, etError);
            Raise EBADIParserAbort.Create('Parsing Aborted!');
          End;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 'Goal', 0, 0, etError);
  End;
end;

(**

  This method parses a Program declaration from the current token
  position using the following object pascal grammar.

  @precon  None.
  @postcon Returns true is a program section was parsed.

  @return  a Boolean

**)
Function TPascalModule.OPProgram : Boolean;

begin
  Result := Token.UToken = 'PROGRAM';
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtProgram;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          ModuleName := Token.Token;
          ModuleNameLine := Token.Line;
          ModuleNameCol := Token.Column;
          NextNonCommentToken;
          // In the Program module we need to check for '(' Ident List ')' but
          // discard
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              IdentList(Nil, strSeekableOnErrorTokens); // get ident list
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
    End;
end;

(**

  This method parses a unit declaration from the current token position using
  the following object pascal grammar.

  @precon  None.
  @postcon Returns true if a unit section was parsed.

  @return  a Boolean

**)
Function TPascalModule.OPUnit : Boolean;

Begin
  Result := Token.UToken = 'UNIT';
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtUnit;
      NextNonCommentToken;
      If Not IsIdentifier(Token) Then
        ErrorAndSeekToken(strIdentExpected, 'OPUnit', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        Begin;
          ModuleName := Token.Token;
          ModuleNameLine := Token.Line;
          ModuleNameCol := Token.Column;
          NextNonCommentToken;
        End;
      While Token.Token = '.' Do
        Begin
          ModuleName := ModuleName + '.';
          NextNonCommentToken;
          If IsIdentifier(Token) Then
            Begin
              ModuleName := ModuleName + '.';
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strIdentExpected, 'OPUnit', Token.Token,
                strSeekableOnErrorTokens, stActual)
        End;
      PortabilityDirective;
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

  @precon  None.
  @postcon Returns true is a package section was parsed.

  @return  a Boolean

**)
Function TPascalModule.OPPackage : Boolean;

begin
  Result := Token.UToken = 'PACKAGE';
  If Result Then
    Begin;
      Comment := GetComment;
      ModuleType := mtPackage;
      NextNonCommentToken;
      If Not IsIdentifier(Token) Then
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

  @precon  None.
  @postcon Returns true is a library section was parsed.

  @return  a Boolean

**)
Function TPascalModule.OPLibrary : Boolean;

begin
  Result := Token.UToken = 'LIBRARY';
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtLibrary;
      NextNonCommentToken;
      If Not IsIdentifier(Token) Then
        ErrorAndSeekToken(strIdentExpected, 'OPLibary', Token.Token,
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
      // Fix for Compiler accepting non-standard grammer (i.e. no begin)
      If Token.UToken = 'END' Then
        NextNonCommentToken;
      // Check for '.'
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, 'OPLibrary', '.',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses a program block from the current token position using
  the following object pascal grammar.

  @precon  None.
  @postcon Parses a program block from the current token position using the
           following object pascal grammar.

**)
procedure TPascalModule.ProgramBlock;
begin
  UsesClause;
  Block(scPrivate, Nil);
end;

(**

  This method parses the Uses clause declaration from the current token
  position using the following object pascal grammar.

  @precon  None.
  @postcon Parses the Uses clause declaration from the current token position
           using the following object pascal grammar.

**)
Procedure TPascalModule.UsesClause;

Var
  U : TElementContainer;
  AComment : TComment;

Begin
  If Token.UToken = 'USES' Then
    Begin
      AComment := GetComment;
      U := Add(strUses, iiUsesLabel, scNone, AComment);
      NextNonCommentToken;
      IdentList(U, strSeekableOnErrorTokens, iiUsesItem);
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

  @precon  None.
  @postcon Attempts to parse the current token position as a Portability
           directive.

  @return  a Boolean

**)
Function TPascalModule.PortabilityDirective : Boolean;

Begin
  Result := False;
  While IsKeyWord(Token.Token, strPortabilityDirective) Do
    Begin
      Result := True;
      If CompareText(Token.Token, 'deprecated') = 0  Then
        Begin
          NextNonCommentToken;
          // Skip optional new Depreciate message.
          If Token.TokenType In [ttSingleLiteral] Then
            NextNonCommentToken;
        End Else
          NextNonCommentToken; //: @note Does not get added to any symbols.
      If Token.Token = ';' Then
        Begin
          PushTokenPosition;
          NextNonCommentToken;
          If Not IsKeyWord(Token.Token, strPortabilityDirective) Then
            PopTokenPosition;
        End;
    End;
End;

(**

  This method parses an interface section from the current token position using
  the following object pascal grammar.

  @precon  None.
  @postcon Parses an interface section from the current token position using
           the following object pascal grammar.

**)
Procedure TPascalModule.InterfaceSection;

Begin
  If Token.UToken = 'INTERFACE' Then
    Begin
      NextNonCommentToken;
      UsesClause;
      InterfaceDecl;
    End Else
    ErrorAndSeekToken(strReservedWordExpected, 'InterfaceSection', 'INTERFACE',
      strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses an interface declaration from the current token position
  using the following object pascal grammar.

  @precon  None.
  @postcon Parses an interface declaration from the current token position
           using the following object pascal grammar.

**)
Procedure TPascalModule.InterfaceDecl;

Begin
  If FExportedHeadingsLabel = Nil Then
    FExportedHeadingsLabel := Add(strExportedHeadingsLabel,
      iiExportedHeadingslabel, scNone, Nil) As TLabelContainer;
  Repeat
    {Loop doing nothing};
  Until Not (
    ConstSection(scPublic, Self) Or
    ResStringSection(scPublic) Or
    TypeSection(scPublic, Self) Or
    VarSection(scPublic, Self) Or
    ThreadVarSection(scPublic, Self) Or
    ExportedHeading(FExportedHeadingsLabel) Or
    ExportsStmt
  );
End;

(**

  This method parses a exported heading declaration section from the current
  token position using the following object pascal grammar.

  @precon  None.
  @postcon This method returns true if the current section was found to be
           an exported heading section.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ExportedHeading(Container : TElementContainer) : Boolean;

Var
  M : TPascalMethod;

Begin
  Result := False;
  Repeat
    M := ProcedureHeading(scPublic, Container);
    If M = Nil Then
      M := FunctionHeading(scPublic, Container);
    If M <> Nil Then
      Begin
        Result := True;
        M.ForwardDecl := True;
        If Token.Token = ';' Then
          Begin
            NextNonCommentToken;
            Directive(M);
            CheckFunctionReturn(M);
            If PortabilityDirective Then
              If Token.Token = ';' Then
                NextNonCommentToken;
          End Else
            ErrorAndSeekToken(strLiteralExpected, 'ExportedHeading', ';',
              strSeekableOnErrorTokens, stFirst);
      End;
  Until M = Nil;
End;

(**

  This method parses an implementation section from the current token position
  using the following object pascal grammar.

  @precon  None.
  @postcon Parses an implementation section from the current token position.

**)
Procedure TPascalModule.ImplementationSection;

Begin
  If Token.UToken <> 'IMPLEMENTATION' Then
    ErrorAndSeekToken(strReservedWordExpected, 'ImplementationSection',
      'IMPLEMENTATION', strSeekableOnErrorTokens, stActual)
  Else
    NextNonCommentToken;
  UsesClause;
  DeclSection(scPrivate, Self);
  ExportsStmt;
End;

(**

  This method parses a block section from the current token position using the
  following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon Parses a block section from the current token position

  @param   AScope as a TScope
  @param   Method as a TPascalMethod

**)
procedure TPascalModule.Block(AScope : TScope; Method : TPascalMethod);

Var
  bool : Boolean;

Begin
  FMethodStack.Add(Method);
  Try
    DeclSection(AScope, Method);
    bool := CompoundStmt(Method);
    If Not bool Then
      AssemblerStatement;
    ExportsStmt;
  Finally
    FMethodStack.Delete(FMethodStack.Count - 1);
  End;
End;

(**

  This method parses an exported procedure section from the current token
  position.

  @precon  None.
  @postcon Returns true if an exported procedure was found.

  @return  a Boolean

**)
Function TPascalModule.ExportsStmt : Boolean;

Begin
  Result := Token.UToken = 'EXPORTS';
  If Result Then
    Begin
      If FExportsHeadingsLabel = Nil Then
        FExportsHeadingsLabel := Add(strExportsLabel,
          iiExportedFunctionsLabel, scNone, Nil) As TLabelContainer;
      NextNonCommentToken;
      Repeat
        ExportsItem(FExportsHeadingsLabel);
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

  @precon  None .
  @postcon Parses an exports entry from the current token position .

  @param   Container as a TElementContainer

**)
Procedure TPascalModule.ExportsItem(Container : TElementContainer);

Var
  E : TElementContainer;
  ExprType : TPascalExprTypes;

Begin
  If IsIdentifier(Token) Then
    Begin
      E := TExportsItem.Create(Token.Token, scPublic, Token.Line,
        Token.Column, iiPublicExportedFunction, GetComment);
      E := Container.Add(E);
      NextNonCommentToken;
      While IsKeyWord(Token.Token, ['index', 'name']) Do
        Begin
          // Check INDEX
          If Token.UToken = 'INDEX' Then
            Begin
              AddToExpression(E);
              ExprType := [petInteger, petConstExpr];
              ConstExpr(E, ExprType);
            End;
          // Check NAME
          If Token.UToken = 'NAME' Then
            Begin
              AddToExpression(E);
              ExprType := [petString, petConstExpr];
              ConstExpr(E, ExprType);
            End;
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'ExportsItem', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a declaration section from the current token position using
  the following object pascal grammar.

  @precon  On entry to this method , Scope defines the current scope of the
           block i . e . private in in the implemenation section or public if
           in the interface section and The Method parameter is nil for
           methods in the implementation section or a reference to a method
           for a local declaration section with in a method .
  @postcon Parses a declaration section from the current token position .

  @param   AScope    as a TScope
  @param   Container as a TElementContainer

**)
Procedure TPascalModule.DeclSection(AScope : TScope; Container : TElementContainer);

Begin
  Repeat
    {Do nothing}
  Until Not (
    LabelDeclSection(Container) Or
    ConstSection(AScope, Container) Or
    ResStringSection(AScope) Or
    TypeSection(AScope, Container) Or
    VarSection(AScope, Container) Or
    ThreadVarSection(AScope, Container) Or
    ProcedureDeclSection(AScope) Or
    ExportsStmt
  );
End;

(**

  This method returns the pascal profiling code for the module.

  @precon  None.
  @postcon Returns the pascal profiling code for the module.

  @return  a String

**)
Function TPascalModule.DefaultProfilingTemplate: String;

Begin
  Result :=
    '{$IFDEF PROFILECODE}'#13#10 +
    'CodeProfiler.Start(''$METHODNAME$'');'#13#10 +
    'Try'#13#10 +
    '{$ENDIF}'#13#10 +
    '$METHODCODE$'#13#10 +
    '{$IFDEF PROFILECODE}'#13#10 +
    'Finally'#13#10 +
    '  CodeProfiler.Stop;'#13#10 +
    'End;'#13#10 +
    '{$ENDIF}';
End;

(**

  This method parses a label declaration section from the current token position
  using the following object pascal grammar.

  @precon  None .
  @postcon This method dicards the labels found and returns True if this method
           handles a label declaration section .

  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.LabelDeclSection(Container : TElementContainer) : Boolean;

Begin
  Result := False;
  If Container <> Nil Then
    Begin
      Result := Token.UToken = 'LABEL';
      If Result Then
        Begin
          Assert(CurrentMethod <> Nil, 'Method in LabelDeclSection is NULL!');
          NextNonCommentToken;
          Repeat
            If IsIdentifier(Token) Then
              Begin
                CurrentMethod.LabelsLabel := CurrentMethod.Add(strLabelsLabel,
                  iiPublicLabelsLabel, scNone, Nil) As TLabelContainer;
                CurrentMethod.LabelsLabel.Add(Token, scLocal, iiPublicLabel,
                  GetComment);
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strNumberExpected, 'LabelDeclSection', Token.Token,
                  strSeekableOnErrorTokens, stFirst);
          Until Not IsToken(',', Nil);
          // Check for ';'
          If Token.Token = ';' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'LabelDeclSection', ';',
              strSeekableOnErrorTokens, stFirst);
        End;
    End;
End;

(**

  This method parses a constant section declaration from the current token
  position using the following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods
           in the implementation section or a reference to a method for a
           local declaration section with in a method.

  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ConstSection(AScope : TScope; Container : TElementContainer) : Boolean;

Var
  C : TElementContainer;
  LabelScope: TScope;
  ConstantsLabel: TLabelContainer;

Begin
  Result := Token.UToken = 'CONST';
  If Result Then
    Begin
      LabelScope := AScope;
      If LabelScope <> scLocal Then
        LabelScope := scPublic;
      If Container Is TPascalMethod then
        Begin
          If (Container As TPascalMethod).ConstantsLabel = Nil Then
            (Container As TPascalMethod).ConstantsLabel := CurrentMethod.Add(
              strConstantsLabel, iiPublicConstantsLabel, LabelScope,
              GetComment) As TLabelContainer;
          C := (Container As TPascalMethod).ConstantsLabel;
        End Else
      If Container Is TRecordDecl then
        Begin
          ConstantsLabel := (Container As TRecordDecl).FindElement(strConstantsLabel) As TLabelContainer;
          If ConstantsLabel = Nil Then
            ConstantsLabel := Container.Add(
              strConstantsLabel, iiPublicConstantsLabel, LabelScope,
              GetComment) As TLabelContainer;
          C := ConstantsLabel;
        End Else
        Begin
          If FConstantsLabel = Nil Then
            FConstantsLabel := Add(strConstantsLabel, iiPublicConstantsLabel,
              LabelScope, GetComment) As TLabelContainer;
          C := FConstantsLabel;
        End;
      NextNonCommentToken;
      RTTIAttributes;
      While ConstantDecl(AScope, C) Do
        If Token.Token = ';' Then
          Begin
            NextNonCommentToken;
            RTTIAttributes;
          End Else
            ErrorAndSeekToken(strLiteralExpected, 'ConstSection', ';',
              strSeekableOnErrorTokens, stFirst);
    End;
End;

(**

  This method parses a constant declaration from the current token position
  using the following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods
           in the implementation section or a reference to a method for a
           local declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ConstantDecl(AScope : TScope;
  Container : TElementContainer) : Boolean;

Var
  T : TGenericTypeDecl;
  ExprType : TPascalExprTypes;
  C, tmpC : TConstant;
  FTemporaryElements: TElementContainer;

Begin
  Result := False;
  // If not identifier then there is a new section
  If IsIdentifier(Token) Then
    Begin
      // Create constant and add to the collection, then get comment
      tmpC := TConstant.Create(Token.Token, AScope, Token.Line, Token.Column,
        iiPublicConstant, GetComment);
      C := Container.Add(tmpC) As TConstant;
      Result := True;
      If tmpC <> C Then
        AddIssue(Format(strDuplicateIdentifierFound, [Token.Token, Token.Line,
          Token.Column]), scNone, 'ConstantDecl', Token.Line, Token.Column,
          etError);
      NextNonCommentToken;
      If Token.Token = '=' Then        // ConstExpr
        Begin
          NextNonCommentToken;
          ExprType := [petUnknown, petConstExpr];
          ConstExpr(C, ExprType);
        End
      Else If Token.Token = ':' Then   // TypedConstant
        Begin
          C.Typed := True;
          NextNonCommentToken;
          FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              C.AddTokens(T);
            If Token.Token = '=' Then
              Begin
                C.AppendToken(Token);
                NextNonCommentToken;
                TypedConstant(C, T);
              End Else
                ErrorAndSeekToken(strLiteralExpected, 'ConstantDecl', '=',
                  strSeekableOnErrorTokens, stActual);
          Finally
            FTemporaryElements.Free;
          End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ConstantDecl', '= or :',
            strSeekableOnErrorTokens, stActual);
      PortabilityDirective;
    End;
End;

(**


  This method parses a resource string declaration section from the current
  token position.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods
           in the implementation section or a reference to a method for a
           local declaration section with in a method.

  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.ResStringSection(AScope : TScope) : Boolean;

Var
  R : TElementContainer;

Begin
  Result := Token.UToken = 'RESOURCESTRING';
  If Result Then
    Begin
      If CurrentMethod <> Nil Then
        Begin
          If CurrentMethod.ResourceStringsLabel = Nil Then
            CurrentMethod.ResourceStringsLabel := CurrentMethod.Add(
              strResourceStringsLabel, iiPublicResourceStringsLabel, scLocal,
              GetComment) As TLabelContainer;
          R := CurrentMethod.ResourceStringsLabel;
        End Else
        Begin
          If FResourceStringsLabel = Nil Then
            FResourceStringsLabel := Add(strResourceStringsLabel,
              iiPublicResourceStringsLabel, scPublic, GetComment) As TLabelContainer;
          R := FResourceStringsLabel;
        End;
      NextNonCommentToken;
      Repeat
        {Loop do nothing}
      Until Not ResourceStringDecl(AScope, R);
    End;
End;

(**

  This method parses a resource string declaration section from the current
  token position.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods in
           the implementation section or a reference to a method for a local
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ResourceStringDecl(AScope : TScope;
  Container : TElementContainer) : Boolean;

Var
  C, tmpC : TElementContainer;
  ExprType : TPascalExprTypes;

Begin
  Result := False;
  ExprType := [petConstExpr, petString];
  // If not identifier then there is a new section
  If IsIdentifier(Token) Then
    Begin
      // Create constant and add to the collection, then get comment
      tmpC := TResourceString.Create(Token.Token, AScope ,Token.Line, Token.Column,
        iiPublicResourceString, GetComment);
      C := Container.Add(tmpC);
      If tmpC <> C Then
        AddIssue(Format(strDuplicateIdentifierFound, [Token.Token, Token.Line,
          Token.Column]), scNone, 'ResourceStringDecl', Token.Line,
          Token.Column, etError);
      Result := True;
      NextNonCommentToken;
      If Token.Token = '=' then
        Begin
          NextNonCommentToken;
          ConstExpr(C, ExprType);
          PortabilityDirective;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ResourceStringDecl', '=',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**


  This method parses a type section from the current token position using the
  following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods
           in the implementation section or a reference to a method for a
           local declaration section with in a method.

  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.TypeSection(AScope : TScope; Container : TElementContainer) : Boolean;

Var
  LabelScope: TScope;
  TL : TLabelContainer;
  TypesLabel: TLabelContainer;

Begin
  Result := Token.UToken = 'TYPE';
  If Result Then
    Begin
      LabelScope := AScope;
      If LabelScope <> scLocal Then
        LabelScope := scPublic;
      If Container Is TPascalMethod Then
        Begin
          If (Container As TPascalMethod).TypesLabel = Nil Then
            (Container As TPascalMethod).TypesLabel := Container.Add(strTypesLabel,
              iiPublicTypesLabel, LabelScope, GetComment) As TLabelContainer;
          TL := (Container As TPascalMethod).TypesLabel;
        End Else
      If Container Is TRecordDecl Then
        Begin
          TypesLabel := (Container As TRecordDecl).FindElement(strTypesLabel) As TLabelContainer;
          If TypesLabel = Nil Then
            TypesLabel := Container.Add(strTypesLabel,
              iiPublicTypesLabel, LabelScope, GetComment) As TLabelContainer;
          TL := TypesLabel;
        End Else
        Begin
          If FTypesLabel = Nil Then
            FTypesLabel := Add(strTypesLabel, iiPublicTypesLabel, LabelScope,
              GetComment) As TLabelContainer;
          TL := FTypesLabel;
        End;
      NextNonCommentToken;
      While TypeDecl(AScope, TL) Do
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

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.TypeDecl(AScope : TScope; Container : TElementContainer) : Boolean;

Var
  AToken : TTypeToken;
  T: TTypes;
  boolIsType: Boolean;

Begin
  Result := False;
  RTTIAttributes;
  If IsIdentifier(Token) Then
    Begin
      AToken := TypeToken(Token, AScope, GetComment, Container);
      NextNonCommentToken;
      TypeParams(AToken.FIdentifier);
      If Token.Token = '=' Then
        Begin
          NextNonCommentToken;
          boolIsType := False;
          If Token.UToken = 'TYPE' Then
            Begin
              NextNonCommentToken;
              boolIsType := True;
            End;
          Result := True;
          T := GetTypeDecl(AToken);
          If T <> NIl Then
            T.IsTyped := boolIsType;
          If T = Nil Then
            ErrorAndSeekToken(strTypeNotFound, 'TypeDecl', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'TypeDecl', '=',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempt to get a type declaration from the current token
  position.

  @precon  None.
  @postcon If a type is found it is returned as the result else nil.

  @param   AToken as a TTypeToken
  @return  a TTypes

**)
Function TPascalModule.GetTypeDecl(AToken : TTypeToken) : TTypes;

Begin
  Result := RestrictedType(AToken);
  If Result = Nil Then
    Result := OPType(AToken);
  PortabilityDirective;
End;

(**

  This method parses the TypeArgs element of the grammar for handling generics on type
  arguments.

  @precon  None.
  @postcon Parses the TypeArgs element of the grammar for handling generics on type
           arguments.

  @param   Container as a TElementContainer

**)
Procedure TPascalModule.TypeArgs(Container : TElementContainer);

Begin
  PushTokenPosition;
  If Token.Token = '<' Then
    Begin
      Container.AddToken(Token.Token, Token.TokenType);
      Repeat
        NextNonCommentToken;
        If IsIdentifier(Token) Or (Token.UToken = 'STRING') Then
          Begin
            Container.AddToken(Token.Token, Token.TokenType);
            ReferenceSymbol(Token); //: @bug DOESNT WORK!!!!
            NextNonCommentToken;
          End Else
          Begin
            PopTokenPosition;
            Exit;
          End;
        If Token.Token = ',' Then
          Container.AddToken(Token.Token, Token.TokenType);
      Until Token.Token <> ',';
      If Token.Token = '>' Then
        Begin
          Container.AddToken(Token.Token, Token.TokenType);
          NextNonCommentToken;
        End Else
          PopTokenPosition;
    End;
End;

(**

  This method parses a typed constant from the current token position using
  the following object pascal grammar.

  @precon  C is a valid instance of the constant to be populated with tokens.
  @postcon Returns false if this was not a typed constant an not handled.

  @param   C as a TElementContainer
  @param   T as a TGenericTypeDecl
  @return  a Boolean

**)
Function TPascalModule.TypedConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  ExprType := [petUnknown, petConstExpr];
  Result := ArrayConstant(C, T) Or RecordConstant(C, T) Or ConstExpr(C, ExprType);
End;

(**

  This method test whether the typed constant is an Array Constant (starts with
  ARRAY.

  @precon  C must be a valid generic container.
  @postcon If ARRAY is found processes the constant as an array constant.

  @param  C as a TElementContainer
  @param  T as a TGenericTypeDecl
  @return a Boolean

**)
Function TPascalModule.ArrayConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

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

  @param   C               as a TElementContainer
  @param   iStartDimension as an Integer
  @param   AT              as a TArrayType

**)
Procedure TPascalModule.ArrayElement(C : TElementContainer;
  iStartDimension : Integer; AT : TArrayType);

Var
  ExprType : TPascalExprTypes;

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
          ErrorAndSeekToken(strLiteralExpected, 'ArrayElement', ')',
            strSeekableOnErrorTokens, stActual);
      End Else
      Begin // If not '(' handle as ConstExpr
        ExprType := [petUnknown, petConstExpr];
        ConstExpr(C, ExprType);
      End;
End;

(**

  This method parses a class var section within a record definition.

  @precon  Cls must be a valid instance.
  @postcon A class var sectino is parsed if found and true returned.

  @param   AScope as a TScope
  @param   Cls    as a TRecordDecl
  @return  a Boolean

**)
Function TPascalModule.RecordClassVarSection(AScope: TScope; Cls : TRecordDecl): Boolean;

Begin
  Result := ClassClassVarSection(AScope, Cls)
End;

(**

  This method attempts to parser the current token position as an RecordConstant.

  @precon  C must be a valid generic container.
  @postcon Attempts to parser the current token position as an RecordConstant.

  @param   C as a TElementContainer
  @param   T as a TGenericTypeDecl
  @return  a Boolean

**)
Function TPascalModule.RecordConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.Token = '(';
  If Result Then
    Begin
      AddToExpression(C);
      Repeat
        If Not RecordFieldConstant(C, T) Then
          If Token.Token <> ')' Then
            Begin // If not handled treat as ConstExpr
              ExprType := [petUnknown, petConstExpr];
              ConstExpr(C, ExprType);
            End;
      Until Not IsToken(';', C) And Not IsToken(',', C);
      If Token.Token = ')' Then
        AddToExpression(C)
      Else
        ErrorAndSeekToken(strLiteralExpected, 'RecordConstant', ')',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a const section within a record definition.

  @precon  Container must be a valid container.
  @postcon Parses a const section of it exists and returns true.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.RecordConstSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := ConstSection(AScope, Container);
End;

(**

  This method attempts to parse the current token position as a record field
  constant.

  @precon  C must be a valid generic container.
  @postcon Attempts to parse the current token position as a record field
           constant.

  @param   C as a TElementContainer
  @param   T as a TGenericTypeDecl
  @return  a Boolean

**)
Function TPascalModule.RecordFieldConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

Var
  strIdentifier: String;

Begin
  Result := False;
  If IsIdentifier(Token) Then
    Begin
      strIdentifier := Token.Token;
      PushTokenPosition;
      NextNonCommentToken;
      If Token.Token = ':' Then
        Begin
          Result := True;
          C.AddToken(strIdentifier);
          AddToExpression(C);
          TypedConstant(C, T)
        End Else
          PopTokenPosition;
    End;
End;

(**

  This method parses a type from the current token position using the following
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TTypes

**)
Function TPascalModule.OPType(AToken : TTypeToken) : TTypes;

Begin
  Result := StrucType(AToken);
  If Result = Nil Then
    Result := PointerType(AToken);
  If Result = Nil Then
    Result := StringType(AToken);
  If Result = Nil Then
    Result := ProcedureType(AToken);
  If Result = Nil Then
    Result := AnonymousReferenceType(AToken);
  If Result = Nil Then
    Result := VariantType(AToken);
  If Result = Nil Then
  Result := ClassRefType(AToken);
  If Result = Nil Then
    Result := SimpleType(AToken);
End;

(**

  This method parses a restricted type from the current token position using
  the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TRestrictedType

**)
Function TPascalModule.RestrictedType(AToken : TTypeToken) : TRestrictedType;

Begin
  Result := ObjectType(AToken);
  If Result = Nil Then
    Result := ClassType(AToken);
  If Result = Nil Then
    Result := InterfaceType(AToken);
End;

(**

  This method parses RTTI Attributes at the current token  position if they exist.

  @precon  None.
  @postcon Any RTTI attributes at the current position are parsed.

**)
Procedure TPascalModule.RTTIAttributes;

Begin
  While Token.Token = '[' Do
    Begin
      Repeat
        NextNonCommentToken;
        AttributeDeclaration;
      Until Token.Token <> ',';
      If Token.Token = ']' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'RTTIAttributes', ']',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a class reference type declaration from the current token
  position using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TClassRefType

**)
Function TPascalModule.ClassRefType(AToken : TTypeToken) : TClassRefType;

Begin
  Result := Nil;
  If Token.UToken = 'CLASS' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If Token.UToken = 'OF' Then
        Begin
          NextNonCommentToken;
          UpdateTypeToken(AToken);
          With AToken Do
            Result := TClassRefType.Create(FIdentifier, FScope, FLine,
              FColumn, iiPublicType, FComment);
          Result := AToken.FContainer.Add(Result) As TClassRefType;
          Result.AddToken('Class');
          Result.AddToken('Of');
          If Not TypeId(Result) Then
            ErrorAndSeekToken(strTypeIdExpected, 'ClassRefType', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End Else
          PopTokenPosition;
    End;
End;

(**

  This method updates the AToken record depending on the initial information
  passed.

  @precon  None.
  @postcon Updates the AToken record depending on the initial information
           passed.

  @param   AToken as a TTypeToken as a reference

**)
Procedure TPascalModule.UpdateTypeToken(var AToken: TTypeToken);

begin
  If AToken.FIdentifier = '' Then
    Begin
      AToken.FIdentifier := Token.Token;
      AToken.FLine := Token.Line;
      AToken.FColumn := Token.Column;
    End;
  If AToken.FComment = Nil Then
    AToken.FComment := GetComment;
end;

(**

  This method parses a simple type declaration from the current token
  position using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TSimpleType

**)
function TPascalModule.SimpleType(AToken : TTypeToken) : TSimpleType;

begin
  Result := RealType(AToken);
  If Result = Nil Then
    Result := OrdinalType(AToken);
end;

(**

  This method determines if the token represents a real type using the following
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TRealType

**)
Function TPascalModule.RealType(AToken : TTypeToken) : TRealType;

Begin
  Result := Nil;
  If IsKeyWord(Token.Token, strRealTypes) Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TRealType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TRealType;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
End;

(**

  This method determines if the type is an ordinal type using the folowing
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TOrdinalType

**)
Function TPascalModule.OrdinalType(AToken : TTypeToken) : TOrdinalType;

Begin
  Result := OrdIdent(AToken);
  If Result = Nil Then
  Result := EnumerateType(AToken);
  If Result = Nil Then
  Result := SubRangeType(AToken);
End;

(**

  This method determines if the current token is an ordinal ident using the
  following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TOrdIdent

**)
Function TPascalModule.OrdIdent(AToken : TTypeToken) : TOrdIdent;

Begin
  Result := Nil;
  If IsKeyWord(Token.Token, strOrdIdents) Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TOrdIdent.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TOrdIdent;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
End;

(**

  This method parses a variant type declaration section using the following
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TVariantType

**)
Function TPascalModule.VariantType(AToken : TTypeToken) : TVariantType;

begin
  Result := Nil;
  If IsKeyWord(Token.Token, strVariants) Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TVariantType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TVariantType;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
end;

(**

  This method parses  a sub range type from the current token position using
  the following object pascal grammar. This method also currently acts as a type
  CATCH ALL if nothing else works.

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @param   AToken as a TTypeToken
  @return  a TSubRangeType

**)
Function TPascalModule.SubRangeType(AToken : TTypeToken) : TSubRangeType;

Var
  ExprType : TPascalExprTypes;
  Container: TElementContainer;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strReservedWords) Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TSubRangeType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TSubRangeType;
      ExprType := [petUnknown, petConstExpr];
      ConstExpr(Result, ExprType);
      If Token.Token = '..' Then // Handle simple expressions
        Begin
          AddToExpression(Result);
          ConstExpr(Result, ExprType);
        End;
      Container := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        TypeArgs(Container);
        If Container.TokenCount > 0 Then
          Result.AddTokens(Container);
      Finally
        Container.Free;
      End;
    End;
End;

(**

  This method parses an enumerate type from the current token position using
  the following object pascal grammar.

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @param   AToken as a TTypeToken
  @return  a TEnumerateType

**)
Function TPascalModule.EnumerateType(AToken : TTypeToken) : TEnumerateType;

Begin
  Result := Nil;
  If Token.Token = '(' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TEnumerateType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TEnumerateType;
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

  @precon  None.
  @postcon Parses the current token position as an Enumerate Element.

  @param   EnumerateType as a TEnumerateType

**)
Procedure TPascalModule.EnumerateElement(EnumerateType : TEnumerateType);

Var
  ExprType : TPascalExprTypes;

Begin
  If IsIdentifier(Token) Then
    Begin
      AddToExpression(EnumerateType);
      If Token.Token = '=' Then
        Begin
          AddToExpression(EnumerateType);
          ExprType := [petUnknown, petConstExpr];
          ConstExpr(EnumerateType, ExprType);
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'EnumerateElement', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a sring type declaration from the current token position
  using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TStringType

**)
Function TPascalModule.StringType(AToken : TTypeToken) : TStringType;

Var
  ExprType : TPascalExprTypes;

begin
  Result := Nil;
  If IsKeyWord(Token.Token, strStrings) Then
    Begin
      UpdateTypeToken(AToken);
      Result := TStringType.Create(AToken.FIdentifier, AToken.FScope,
        AToken.FLine, AToken.FColumn, iiPublicType, AToken.FComment);
      Result := AToken.FContainer.Add(Result) As TStringType;
      Result.AppendToken(Token);
      NextNonCommentToken;
      // Check for '[' ConstExpr ']'
      If Token.Token = '[' Then
        Begin
          Result.AppendToken(Token);
          NextNonCommentToken;
          ExprType := [petInteger, petConstExpr];
          ConstExpr(Result, ExprType);
          If Token.Token = ']' Then
            Begin
              Result.AddToken(Token.Token);
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'StringType', ']',
                strSeekableOnErrorTokens, stActual);
        End;
    End;
end;

(**

  This method parses an Array, Set of File type declaration from the current
  token position using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TTypes

**)
Function TPascalModule.StrucType(AToken : TTypeToken) : TTypes;

Var
  boolPacked : Boolean;

begin
  boolPacked := False;
  If Token.UToken = 'PACKED' Then
    Begin
      boolPacked := True;
      NextNonCommentToken;
    End;
  Result := ArrayType(boolPacked, AToken);
  If Result = Nil Then
    Result := SetType(boolPacked, AToken);
  If Result = Nil Then
    Result := FileType(boolPacked, AToken);
  If Result = Nil Then
    Result := RecType(boolPacked, AToken);
  If Token.UToken = 'PACKED' Then
    Begin
      Result.InsertToken(Token.Token, 0);
      NextNonCommentToken;
    End;
end;

(**

  This method parses an array type declaration from the current token position
  using the following object pascal grammar.

  @precon  boolPacked determines if the array type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @param   AToken     as a TTypeToken
  @return  a TArrayType

**)
Function TPascalModule.ArrayType(boolPacked : Boolean; AToken : TTypeToken) : TArrayType;

var
  T: TGenericTypeDecl;
  FTemporaryElements: TElementContainer;

Begin
  Result := Nil;
  If Token.UToken = 'ARRAY' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TArrayType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TArrayType;
      If boolPacked Then
        Result.AddToken('Packed');
      Result.AppendToken(Token);
      NextNonCommentToken;
      FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        If Token.Token = '[' Then
          Begin
            AddToExpression(Result);
            Repeat
              Result.AddDimension;
              T := OrdinalType(TypeToken(Nil, scNone, Nil,
                FTemporaryElements.Add(Format('%d', [Result.Dimensions]), iiNone,
                scNone, Nil)));
              If T <> Nil Then
                Result.AddTokens(T);
            Until Not IsToken(',', Result);
            If Token.Token = ']' Then
              AddToExpression(Result)
            Else
              ErrorAndSeekToken(strLiteralExpected, 'ArrayType', ']',
                strSeekableOnErrorTokens, stActual);
          End;
        If Token.UToken = 'OF' Then
          Begin
            Result.AppendToken(Token);
            NextNonCommentToken;
            T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              Result.AddTokens(T);
          End Else
            ErrorAndSeekToken(strReservedWordExpected, 'ArrayType', 'OF',
              strSeekableOnErrorTokens, stActual);
      Finally
        FTemporaryElements.Free;
      End;
      PortabilityDirective;
    End;
End;

(**

  This method parses a record type declaration from the current token position.

  @precon  boolPacked detmerines if the record is packed for not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @param   AToken     as a TTypeToken
  @return  a TRecordDecl

**)
Function TPascalModule.RecType(boolPacked : Boolean; AToken : TTypeToken): TRecordDecl;

var
  InternalScope: TScope;
  boolFieldAllowed : Boolean;

begin
  Result := Nil;
  If Token.UToken = 'RECORD' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TRecordDecl.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicRecord, FComment);
      Result := AToken.FContainer.Add(Result) As TRecordDecl;
      Result.Line := AToken.FLine;
      Result.Column := AToken.FColumn;
      Result.Comment := AToken.FComment ;
      Result.IsPacked := boolPacked;
      NextNonCommentToken;
      InternalScope := scPublic;
      boolFieldAllowed := True;
      Result.HelperClass := (Token.UToken = 'HELPER');
      If Result.HelperClass Then
        Begin
          NextNonCommentToken;
          boolFieldAllowed := False;
        End;
      // Get the classes heritage
      RecObjClsIntHeritage(Result);
      If Result.HelperClass Then
        If Token.UToken = 'FOR' Then
          Begin
            NextNonCommentToken;
            If IsIdentifier(Token) Then
              Begin
                Result.HelperClassName := Token.Token;
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strIdentExpected, 'ClassType', Token.Token,
                  strSeekableOnErrorTokens, stActual);
          End Else
            ErrorAndSeekToken(strReservedWordExpected, 'ClassType', 'FOR',
              strSeekableOnErrorTokens, stActual);

      Repeat
        RecordVisibility(InternalScope);
        If Token.UToken = 'END' Then
          Break;
      Until Not (
        RecordTypeSection(InternalScope, Result) Or
        RecordConstSection(InternalScope, Result) Or
        RecordVarSection(InternalScope, Result) Or
        RecordClassVarSection(InternalScope, Result) Or
        RecordMethodList(Result, InternalScope, [mtConstructor..mtOperator]) Or
        RecordPropertyList(Result, InternalScope) Or
        (boolFieldAllowed And RecordFieldList(Result, InternalScope))
      );
      If Token.UToken = 'END' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'RecType', 'END',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the field list element of the record definition.

  @precon  Rec must be a valid instance.
  @postcon Parses a field list if at the current token and returnd true if so.

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.RecordFieldList(Rec : TRecordDecl; InternalScope : TScope) : Boolean;

Var
  boolFoundField : Boolean;

Begin
  Result := False;
  Repeat
    RTTIAttributes;
    boolFoundField := FieldDecl(Rec, InternalScope);
    If boolFoundField Then
      Result := True;
  Until Not IsToken(';', Nil);
  RecordVariantSection(Rec, InternalScope);
End;

(**

  This method parses the method list for a record definition.

  @precon  Cls must be a valid instance.
  @postcon A list of methods is parsed if found at the current cursor position.

  @param   Cls                as a TRecordDecl
  @param   AScope             as a TScope
  @param   PermissibleMethods as a TPermissibleMethods
  @return  a Boolean

**)
Function TPascalModule.RecordMethodList(Cls: TRecordDecl; AScope: TScope;
  PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses a property list in a record definition.

  @precon  Cls must be a valid instance.
  @postcon A property list is parsed if found at the current position.

  @param   Cls    as a TRecordDecl
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.RecordPropertyList(Cls: TRecordDecl; Var AScope: TScope): Boolean;

Begin
  Result := ClassPropertyList(Cls, AScope);
End;

(**

  This method parses a type section defined within a record definition.

  @precon  Cls must be a valid instance.
  @postcon A type section is parsed if found at the current position.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.RecordTypeSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := TypeSection(AScope, Container);
End;

(**

  This method parses record visibility keywords in a record definition.

  @precon  None.
  @postcon If record visibility keyword are found the AScope variable is updated.

  @param   AScope as a TScope as a reference

**)
Procedure TPascalModule.RecordVisibility(Var AScope: TScope);

Begin
  While (Token.UToken = 'STRICT') Or IsKeyWord(Token.Token, ['private', 'public']) Do
    Begin
      While Token.UToken = 'STRICT' Do
        Begin
          NextNonCommentToken;
          If Token.UToken = 'PRIVATE' Then
            Begin
              AScope := scPrivate;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'RecordVisibility', 'PRIVATE',
                strSeekableOnErrorTokens, stActual);
        End;
      While IsKeyWord(Token.Token, ['private', 'public']) Do
        Begin
          If Token.UToken = 'PRIVATE' Then
            Begin
              AScope := scPrivate;
              NextNonCommentToken;
            End
          Else If Token.UToken = 'PUBLIC' Then
            Begin
              AScope := scPublic;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'RecordVisibility',
                'PRIVATE or PUBLIC', strSeekableOnErrorTokens, stActual);
        End;
    End;
End;

(**

  This method parses a field list for classes, records and object declarations
  from the current token position.

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses a field list for classes, records and object declarations
           from the current token position.

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.ObjFieldList(Rec: TRecordDecl; InternalScope : TScope) : Boolean;

Begin
  Result := False;
  Repeat
    If Not Result Then
      Begin
        RTTIAttributes;
        Result := Result Or FieldDecl(Rec, InternalScope);
      End;
  Until Not IsToken(';', Nil);
End;

(**

  This method parses a records field declarations from the current token
  position using the following object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses a records field declarations from the current token position

  @param   Rec          as a TRecordDecl
  @param   InteralScope as a TScope
  @return  a Boolean

**)
Function  TPascalModule.FieldDecl(Rec: TRecordDecl; InteralScope : TScope) : Boolean;

Var
  I : TIdentList;
  j : Integer;
  P, tmpP : TField;
  T : TGenericTypeDecl;
  FTemporaryElements: TElementContainer;
  Fields: TElementContainer;

Begin
  Result := False;
  I := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    IdentList(I, strSeekableOnErrorTokens);
    If I.ElementCount > 0 Then
      Begin
        Result := True;
        If Token.Token = ':' Then
          Begin
            NextNonCommentToken;
            FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
            Try
              T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
              // Create record fields
              Fields := Rec.FindElement(strFieldsLabel);
              If Fields = Nil Then
                Fields := Rec.Add(TLabelContainer.Create(strFieldsLabel, scNone,
                  0, 0, iiFieldsLabel, Nil)) As TLabelContainer;
              For j := 1 To I.ElementCount Do
                Begin
                  tmpP :=  TField.Create(I[j].Name, InteralScope, I[j].Line, I[j].Column,
                    iiPublicField, I[j].Comment);
                  P := Fields.Add(tmpP) As TField;
                  If P <> tmpP Then
                    AddIssue(Format(strDuplicateIdentifierFound, [I[j].Name,
                      I[j].Line, I[j].Column]), scNone, 'FieldDecl', I[j].Line,
                      I[j].Column, etError);
                  If T <> Nil Then
                    P.AddTokens(T)
                  Else
                    ErrorAndSeekToken(strTypeNotFound, 'FieldDecl', '',
                      strSeekableOnErrorTokens, stFirst);
                End;
              PortabilityDirective;
            Finally
              FTemporaryElements.Free;
            End;
          End Else
            ErrorAndSeekToken(strLiteralExpected, 'FieldDecl', ':',
              strSeekableOnErrorTokens, stActual);
        End;
  Finally
    I.Free;
  End;
End;

(**

  This method parses the variant section of a record from the current token
  position using the following object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Returns true is a variant section of a record was parsed.

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.RecordVariantSection(Rec: TRecordDecl; InternalScope : TScope) : Boolean;

Var
  C : TElementContainer;

Begin
  Result := Token.UToken = 'CASE';
  If Result Then
    Begin
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          PushTokenPosition;
          NextNonCommentToken;
          If Token.Token = ':' Then
            NextNonCommentToken
          Else
            PopTokenPosition;
        End;
      C := TTempCntr.Create(Token.Token, scPrivate, Token.Line,
        Token.Column, iiNone, Nil);
      Try
        If TypeId(C) Then
          Begin
            If Token.UToken = 'OF' Then
              Begin
                NextNonCommentToken;
                Repeat
                  If IsKeyWord(Token.Token, [')', 'else', 'end']) Then
                    Break;
                  RecVariant(Rec, InternalScope);
                Until Not IsToken(';', Nil);
              End Else
                ErrorAndSeekToken(strReservedWordExpected, 'VariantSection',
                  'OF', strSeekableOnErrorTokens, stActual);
          End Else
            ErrorAndSeekToken(strTypeIDExpected, 'VariantSection',
              Token.Token, strSeekableOnErrorTokens, stActual);
      Finally
        C.Free;
      End;
    End;
End;

(**

  This method parses a var section within a record definition.

  @precon  Container must be a valid instance.
  @postcon A var section is parsed if found at the current position.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.RecordVarSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := VarSection(AScope, Container);
End;

(**

  This method parses the record variant section of a record from the current
  token position using the following object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses the record variant section of a record from the current token
           position

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope

**)
Procedure TPascalModule.RecVariant(Rec : TRecordDecl; InternalScope : TScope);

Var
  C : TElementContainer;
  ExprType : TPascalExprTypes;

Begin
  C := TTempCntr.Create('', scPrivate, 0, 0, iiNone, Nil);
  Try
    Repeat
      ExprType := [petUnknown, petConstExpr];
      ConstExpr(C, ExprType);
    Until Not IsToken(',', C);
    If Token.Token = ':' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, 'RecVariant', ':',
        strSeekableOnErrorTokens, stActual);
    If Token.Token = '(' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, 'RecVariant', '(',
        strSeekableOnErrorTokens, stActual);
    RecordFieldList(Rec, InternalScope);
    If Token.Token = ')' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, 'RecVariant', ')',
        strSeekableOnErrorTokens, stActual);
  Finally
    C.Free;
  End;
End;

(**

  This method tries to find the symbol with its scope as mark it as referenced.

  @precon  None.
  @postcon Tries to find the symbol with its scope as mark it as referenced.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TPascalModule.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Var
  i: Integer;
  E: TElementContainer;
  M: TPascalMethod;
  boolFound: Boolean;

begin
  Result := ReferenceSection(AToken, FVariablesLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FConstantsLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FResourceStringsLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FTypesLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FThreadVarsLabel);
  If Result Then
    Exit;
  // Check Module Local Methods
  boolFound := False;
  E := FImplementedMethodsLabel;
  If E <> Nil Then
    For i := 1 To E.ElementCount Do
      If CompareText(E[i].Identifier, AToken.Token) = 0 Then
        Begin
          E[i].Referenced := True;
          AToken.Reference := trResolved;
          boolFound := True;
        End;
  Result := boolFound;
  If Result Then
    Exit;
  // Check Methods in Same Class.
  M := CurrentMethod;
  If M <> Nil Then
    Begin
      If M.RecObjClsInt <> Nil Then
        Begin
          E := M.RecObjClsInt.FindElement(strMethodsLabel);
          If E <> Nil Then
            For i := 1 To E.ElementCount Do
              If CompareText(E[i].Identifier, AToken.Token) = 0 Then
                Begin
                  E[i].Referenced := True;
                  AToken.Reference := trResolved;
                End;
        End;
    End;
end;

(**

  Method parses a set type declaration from the current token position using
  following object pascal grammar.

  @precon  boolPacked determines if the set type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @param   AToken     as a TTypeToken
  @return  a TSetType

**)
Function TPascalModule.SetType(boolPacked : Boolean; AToken : TTypeToken) : TSetType;

Var
  T : TOrdinalType;
  FTemporaryElements: TElementContainer;

Begin
  Result := Nil;
  If Token.UToken = 'SET' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TSetType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TSetType;
      If boolPacked Then
        Result.AddToken('Packed');
      Result.AppendToken(Token);
      NextNonCommentToken;
      If Token.UToken = 'OF' Then
        Begin
          AddToExpression(Result);
          FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            T := OrdinalType(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              Result.AddTokens(T)
            Else
              ErrorAndSeekToken(strOrdinalTypeExpected, 'SetType', Token.Token,
                strSeekableOnErrorTokens, stActual);
          Finally
            FTemporaryElements.Free;
          End;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'SetType', 'OF',
            strSeekableOnErrorTokens, stActual);
      PortabilityDirective;
    End;
End;

(**

  This method parses a file type declaration from the current token position
  using the following object pascal grammar.

  @precon  boolPacked determines if the file type is packed or not.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   boolPacked as a Boolean
  @param   AToken     as a TTypeToken
  @return  a TFileType

**)
Function TPascalModule.FileType(boolPacked : Boolean; AToken : TTypeToken) : TFileType;

Var
  T : TGenericTypeDecl;
  FTemporaryElements: TElementContainer;

Begin
  Result := Nil;
  If Token.UToken = 'FILE' Then
    Begin
      NextNonCommentToken;
      If Token.UToken = 'OF' Then
        Begin
          UpdateTypeToken(AToken);
          With AToken Do
            Result := TFileType.Create(FIdentifier, FScope, FLine,
              FColumn, iiPublicType, FComment);
          Result := AToken.FContainer.Add(Result) As TFileType;
          If boolPacked Then
            Result.AddToken('Packed');
          Result.AddToken('File');
          AddToExpression(Result);
          FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              Result.AddTokens(T)
            Else
              ErrorAndSeekToken(strTypeDeclExpected, 'FileType', Token.Token,
                strSeekableOnErrorTokens, stActual);
          Finally
            FTemporaryElements.Free;
          End;
        End Else
        Begin
          UpdateTypeToken(AToken);
          With AToken Do
            Result := TFileType.Create(FIdentifier, FScope, FLine,
              FColumn, iiPublicType, FComment);
          Result := AToken.FContainer.Add(Result) As TFileType;
          If boolPacked Then
            Result.AddToken('Packed');
          Result.AddToken('File');
        End;
    End;
End;

(**

  This method parses a pointer type declaration from the current token position
  using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TPointerType

**)
Function TPascalModule.PointerType(AToken : TTypeToken) : TPointerType;

Begin
  Result := Nil;
  If Token.Token = '^' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TPointerType.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TPointerType;
      Result.AppendToken(Token);
      NextNonCommentToken;
      If Not TypeId(Result) Then
        ErrorAndSeekToken(strTypeIdExpected, 'PointerType', Token.Token,
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses a procedure type declaration from the current token
  position using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TProcedureType

**)
Function TPascalModule.ProcedureType(AToken : TTypeToken) : TProcedureType;

Var
  M : TPascalMethod;
  TemporaryContainer: TElementContainer;
  boolMethodDirective : Boolean;

begin
  Result := Nil;
  TemporaryContainer := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    M := ProcedureHeading(scPrivate, TemporaryContainer, False);
    If M = Nil Then
      M := FunctionHeading(scPrivate, TemporaryContainer, False);
    If M <> Nil Then
      Begin
        UpdateTypeToken(AToken);
        With AToken Do
          Result := TProcedureType.Create(FIdentifier, FScope, FLine,
            FColumn, iiPublicType, FComment);
        Result := AToken.FContainer.Add(Result) As TProcedureType;
        Result.AddToken(M.AsString(True, False));
        If Token.UToken = 'OF' Then
          Begin
            AddToExpression(Result);
            If Token.UToken = 'OBJECT' Then
              AddToExpression(Result)
            Else
              ErrorAndSeekToken(strReservedWordExpected, 'ProcedureType', 'OBJECT',
                strSeekableOnErrorTokens, stActual);
          End;
        boolMethodDirective := True;
        While (Token.Token = ';') And boolMethodDirective Do
          Begin
            boolMethodDirective := False;
            PushTokenPosition;
            NextNonCommentToken;
            If IsKeyWord(Token.Token, strMethodDirectives) Then
              Begin
                Result.AppendToken(Token);
                NextNonCommentToken;
                boolMethodDirective := True;
              End Else
                PopTokenPosition;
          End;
      End;
  Finally
    TemporaryContainer.Free;
  End;
end;

(**

  This method parses the AnonymousMethod element of the grammar.

  @precon  None.
  @postcon Parses an anonymous method at the current token position and returns true if
           the token position is an anonymous method else returns false.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.AnonymousMethod(Container : TElementContainer) : Boolean;

Var
  M : TPascalMethod;

Begin
    M := FunctionHeading(scLocal, Container, False);
    If M = Nil Then
      M := ProcedureHeading(scLocal, Container, False);
    Result := M <> Nil;
    If M <> Nil Then
      Begin
        PortabilityDirective;
        Block(scLocal, M);
      End;
End;

(**

  This method parses an anonymous method reference type declaration at the current token
  position.

  @precon  AToken must be a valid set of data.
  @postcon An anonymous method type is parsed if found and returned else the return is
           nil.

  @param   AToken as a TTypeToken
  @return  a TAnonymousReferenceType

**)
Function TPascalModule.AnonymousReferenceType(AToken : TTypeToken) : TAnonymousReferenceType;

Var
  M : TPascalMethod;
  TemporaryContainer: TElementContainer;
  boolMethodDirective :Boolean;

begin
  Result := Nil;
  If Token.UToken = 'REFERENCE' Then
    Begin
      NextNonCommentToken;
      If Token.UToken = 'TO' Then
        Begin
          NextNonCommentToken;
          TemporaryContainer := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            M := ProcedureHeading(scPrivate, TemporaryContainer, False);
            If M = Nil Then
              M := FunctionHeading(scPrivate, TemporaryContainer, False);
            If M <> Nil Then
              Begin
                UpdateTypeToken(AToken);
                With AToken Do
                  Result := TAnonymousReferenceType.Create(FIdentifier, FScope, FLine,
                    FColumn, iiPublicType, FComment);
                Result := AToken.FContainer.Add(Result) As TAnonymousReferenceType;
                Result.AddToken(M.AsString(True, False));
                boolMethodDirective := True;
                While (Token.Token = ';') And boolMethodDirective Do
                  Begin
                    boolMethodDirective := False;
                    PushTokenPosition;
                    NextNonCommentToken;
                    If IsKeyWord(Token.Token, strMethodDirectives) Then
                      Begin
                        Result.AppendToken(Token);
                        NextNonCommentToken;
                        boolMethodDirective := True;
                      End Else
                        PopTokenPosition;
                  End;
              End;
          Finally
            TemporaryContainer.Free;
          End;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'AnonymousReferenceType', 'TO',
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method check and parses a var section declaration from the current token
  position using the following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods
           in the implementation section or a reference to a method for a
           local declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.VarSection(AScope : TScope; Container : TElementContainer) : Boolean;

Var
  V : TLabelContainer;
  LabelScope : TScope;
  VariablesLabel: TLabelContainer;

Begin
  Result := Token.UToken = 'VAR';
  If Result Then
    Begin
      LabelScope := AScope;
      //If LabelScope <> scLocal Then
      //  LabelScope := scPublic;
      If Container Is TPascalMethod Then
        Begin
          If (Container As TPascalMethod).VariablesLabel = Nil Then
            (Container As TPascalMethod).VariablesLabel := Container.Add(
              strVarsLabel, iiPublicVariablesLabel, LabelScope,
              GetComment) As TLabelContainer;
          V := (Container As TPascalMethod).VariablesLabel;
        End Else
      If Container Is TRecordDecl Then
        Begin
          VariablesLabel := (Container As TRecordDecl).FindElement(strVarsLabel) As TLabelContainer;
          If VariablesLabel = Nil Then
            VariablesLabel := Container.Add(
              strVarsLabel, iiPublicVariablesLabel, LabelScope,
              GetComment) As TLabelContainer;
          V := VariablesLabel;
        End Else
        Begin
          If FVariablesLabel = Nil Then
            FVariablesLabel := Add(strVarsLabel, iiPublicVariablesLabel, LabelScope,
              GetComment) As TLabelContainer;
          V := FVariablesLabel;
        End;
      NextNonCommentToken;
      While VarDecl(AScope, V, iiPublicVariable) Do
        Begin
          If Token.Token <> ';' Then
            ErrorAndSeekToken(strLiteralExpected, 'VarSection', ';',
              strSeekableOnErrorTokens, stFirst)
          Else
            NextNonCommentToken;
        End;
    End;
End;

(**

  This method checks and parses a class var section declaration from the current
  token position using the following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the
           block i.e. private in in the implemenation section or public if in
           the interface section and The Method parameter is nil for methods
           in the implementation section or a reference to a method for a
           local declaration section with in a method.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AScope as a TScope
  @param   Cls    as a TRecordDecl
  @return  a Boolean

**)
Function TPascalModule.ClassClassVarSection(AScope : TScope;
  Cls : TRecordDecl) : Boolean;

Var
  V : TElementContainer;
  LabelScope : TScope;
  ClassVarsLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'CLASS' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      Result := Token.UToken = 'VAR';
      If Result Then
        Begin
          LabelScope := AScope;
          If LabelScope <> scLocal Then
            LabelScope := scPublic;
          ClassVarsLabel := Cls.FindElement(strClassVarsLabel) As TLabelContainer;
          If ClassVarsLabel = Nil Then
            ClassVarsLabel := Cls.Add(strClassVarsLabel,
              iiPublicClassVariablesLabel, LabelScope,
              GetComment) As TLabelContainer;
          V := ClassVarsLabel;
          NextNonCommentToken;
          While VarDecl(AScope, V, iiPublicClassVariable) Do
            Begin
              If Token.Token <> ';' Then
                ErrorAndSeekToken(strLiteralExpected, 'ClassVarSection', ';',
                  strSeekableOnErrorTokens, stFirst)
              Else
                NextNonCommentToken;
            End;
        End Else
          PopTokenPosition;
    End;
End;

(**

  This method parses the grammar associated with a constant section within a class
  declaration.

  @precon  None.
  @postcon Parses the grammar associated with a constant section within a class
           declaration.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ClassConstSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := ConstSection(AScope, Container);
End;

(**

  This method parses a Thread var section declatation from the current token
  position.

  @precon  On entry to this method , Scope defines the current scope of the
           block i . e . private in in the implemenation section or public if
           in the interface section .
  @postcon This method returns True if this method handles a constant
           declaration section .

  @see     For object pascal grammar see {@link TPascalDocModule.VarSection} .

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ThreadVarSection(AScope : TScope; Container : TElementContainer) : Boolean;

Begin
  Result := False;
  If (Container = Nil) Or (Container = Self) Then // Not allowed in methods.
    Begin
      Result := Token.UToken = 'THREADVAR';
      If Result Then
        Begin
          If FThreadVarsLabel = Nil Then
            FThreadVarsLabel := Add(strThreadVarsLabel, iiPublicThreadVarsLabel,
              scNone, GetComment) As TLabelContainer;
          NextNonCommentToken;
          While ThreadVarDecl(AScope, FThreadVarsLabel) Do
            Begin
              If Token.Token <> ';' Then
                ErrorAndSeekToken(strLiteralExpected, 'ThreadVarSection', ';',
                  strSeekableOnErrorTokens, stFirst)
              Else
                NextNonCommentToken;
            End;
        End;
    End;
End;

(**

  This method remove the Implement Methods and Exported Headings IF they have
  no elements.

  @precon  None.
  @postcon Remove the Implement Methods and Exported Headings IF they have
           no elements.

**)
procedure TPascalModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    Begin
      If Elements[iElement] Is TFinalizationSection Then
        Continue;
      If Elements[iElement] Is TInitializationSection Then
        Continue;
      If Elements[iElement].ElementCount = 0 Then
        DeleteElement(iElement);
    End;
end;

(**


  This method parses a variable declaration from the current token position.

  @precon  AScope defines the current scope of the variable and VarSection is a
           valid variable container for the storage of the variable declared.
  @postcon Returns true if a variable declaration was handled.

  @param   AScope      as a TScope
  @param   VarSection  as a TElementContainer
  @param   AImageIndex as a TBADIImageIndex
  @return  a Boolean

**)
Function TPascalModule.VarDecl(AScope : TScope; VarSection : TElementContainer;
  AImageIndex : TBADIImageIndex) : Boolean;

Var
  I  :TIdentList;
  j : Integer;
  V, tmpV : TElementContainer;
  T : TGenericTypeDecl;
  C : TElementContainer;
  ExprType : TPascalExprTypes;
  FTemporaryElements: TElementContainer;
  AToken: TTokenInfo;

Begin
  Result := False;
  RTTIAttributes;
  If IsIdentifier(Token) Then
    Begin
      // Get ident list line and column
      I := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        IdentList(I, strSeekableOnErrorTokens);
        If Token.Token <> ':' Then
          ErrorAndSeekToken(strLiteralExpected, 'VarDecl', ':',
            strSeekableOnErrorTokens, stActual)
        Else
          Begin
            FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
            Try
              NextNonCommentToken;
              AToken := Token;
              T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
              If T <> Nil Then
                VarSection.ReferenceSymbol(AToken);
              If Token.UToken = 'ABSOLUTE' Then
                Begin
                  C := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
                  Try
                    C.AddToken(Token.Token);
                    NextNonCommentToken;
                    ExprType := [petUnknown, petConstExpr];
                    ConstExpr(C, ExprType);
                  Finally
                    If T <> Nil Then
                      T.AddTokens(C);
                    C.Free;
                  End;
                End;
              If Token.Token = '=' Then
                Begin
                  C := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
                  Try
                    C.AddToken(Token.Token);
                    NextNonCommentToken;
                    If Not TypedConstant(C, T) Then
                      Begin
                        ExprType := [petUnknown, petConstExpr];
                        ConstExpr(C, ExprType);
                      End;
                    If Token.UToken = 'NIL' Then
                      AddToExpression(C);
                  Finally
                    If T <> Nil Then
                      T.AddTokens(C);
                    C.Free;
                  End;
                End;
              PortabilityDirective;
              If T <> Nil Then
                For j := 1 To I.ElementCount Do
                  Begin
                    tmpV := TVar.Create(I[j].Identifier, AScope, I[j].Line, I[j].Column,
                      AImageIndex, I[j].Comment);
                    V := VarSection.Add(tmpV);
                    If tmpV <> V Then
                      AddIssue(Format(strDuplicateIdentifierFound,
                        [I[j].Identifier, I[j].Line, I[j].Column]),
                        scNone, 'VarDecl', I[j].Line, I[j].Column, etError);
                    V.AddTokens(T);
                    If I[j].Comment <> Nil Then
                      Begin
                        V.Comment := TPascalComment.Create(I[j].Comment);
                        OwnedItems.Add(V.Comment);
                      End;
                  End;
              Result := True;
            Finally
              FTemporaryElements.Free;
            End;
          End;
      Finally
        I.Free;
      End;
    End;
End;

(**

  This method parses a variable declaration from the current token position.

  @precon  AScope defines the current scope of the variable and VarSection is a
           valid variable container for the storage of the variable declared.
  @postcon Returns true if a variable declaration was handled.

  @param   AScope      as a TScope
  @param   VarSection as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ThreadVarDecl(AScope : TScope;
  VarSection : TElementContainer) : Boolean;

Var
  I  :TIdentList;
  j : Integer;
  V, tmpV : TElementContainer;
  T : TGenericTypeDecl;
  C : TElementContainer;
  ExprType : TPascalExprTypes;
  FTemporaryElements: TElementContainer;

Begin
  Result := False;
  If IsIdentifier(Token) Then
    Begin
      // Get ident list line and column
      I := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        IdentList(I, strSeekableOnErrorTokens);
        If Token.Token <> ':' Then
          ErrorAndSeekToken(strLiteralExpected, 'VarDecl', ':',
            strSeekableOnErrorTokens, stActual)
        Else
          Begin
            NextNonCommentToken;
            FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
            Try
              T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
              If Token.Token = '=' Then
                Begin
                  C := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
                  Try
                    C.AppendToken(Token);
                    NextNonCommentToken;
                    ExprType := [petUnknown, petConstExpr];
                    ConstExpr(C, ExprType);
                  Finally
                    If T <> Nil Then
                      T.AddTokens(C);
                    C.Free;
                  End;
                End;
              PortabilityDirective;
              For j := 1 To I.ElementCount Do
                Begin
                  tmpV := TThreadVar.Create(I[j].Identifier, AScope, I[j].Line, I[j].Column,
                    iiPublicThreadVar, I[j].Comment);
                  V := VarSection.Add(tmpV);
                  If tmpV <> V Then
                    AddIssue(Format(strDuplicateIdentifierFound,
                      [I[j].Identifier, I[j].Line, I[j].Column]),
                      scNone, 'VarDecl', I[j].Line, I[j].Column, etError);
                  V.AddTokens(T);
                  If I[j].Comment <> Nil Then
                    Begin
                      V.Comment := TPascalComment.Create(I[j].Comment);
                      OwnedItems.Add(V.Comment);
                    End;
                End;
              Finally
                FTemporaryElements.Free;
              End;
            Result := True;
          End;
      Finally
        I.Free;
      End;
    End;
End;

(**

  This method attempts to parse the next series of tokens as an expression.

  @precon  None.
  @postcon Attempts to parse the next series of tokens as an expression.

  @grammer Expression -> SimpleExpression [RelOp SimpleExpression]

  @param   C        as a TElementContainer
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.Expression(C : TElementContainer; var ExprType : TPascalExprTypes);

Begin
  Repeat
    SimpleExpression(C, ExprType);
  Until Not RelOp(C, ExprType);
End;

(**


  This method attempts to parse the next series of tokens as a Simple
  Expression.

  @precon  none.
  @postcon Attempts to parse the next series of tokens as a Simple Expression.

  @param   C        as a TElementContainer
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.SimpleExpression(C : TElementContainer; var ExprType : TPascalExprTypes);

Begin
  If IsKeyWord(Token.Token, ['+', '-']) Then
    AddToExpression(C);
  Repeat
    Term(C, ExprType);
  Until Not AddOp(C);
End;

(**


  This method attempts to parse a term from the current token position.

  @precon  None.
  @postcon Attempts to parse a term from the current token position.

  @param   C        as a TElementContainer
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.Term(C : TElementContainer; var ExprType : TPascalExprTypes);

Begin
  Repeat
    Factor(C, ExprType);
  Until Not MulOp(C, ExprType)
End;

(**


  This method attempts to parse a factor from the current token position.

  @precon  None.
  @postcon Attempts to parse a factor from the current token position.

  @param   C        as a TElementContainer
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.Factor(C : TElementContainer; var ExprType : TPascalExprTypes);

Var
  SubExprType : TPascalExprTypes;

  (**

    This method sets up the ExprType variable accounting for Constant
    Expressions.

    @precon  None.
    @postcon Sets up the ExprType variable accounting for Constant
             Expressions.

  **)
  Procedure SetupSubExprType;

  Begin
    SubExprType := [petUnknown];
    If petConstExpr In ExprType Then
      Include(SubExprType, petConstExpr); // Make sure const expr is propogated
  End;

Begin
  If Token.TokenType In [ttSingleLiteral] Then
    Begin
      If (petUnknown In ExprType) Then
        Begin
          Exclude(ExprType, petUnknown);
          Include(ExprType, petString);
          AddToExpression(C);
        End
      Else If Not (petString In ExprType) Then
        ErrorAndSeekToken(strExprConflict, 'Factor', Token.Token,
          strSeekableOnErrorTokens, stActual)
      Else
        AddToExpression(C);
    End
  Else If Token.TokenType In [ttNumber] Then
    Begin
      If (petUnknown In ExprType) Then
        Begin
          Exclude(ExprType, petUnknown);
          If Pos('.', Token.Token) > 0 Then
            Include(ExprType, petFloat)
          Else
            Include(ExprType, petInteger);
          AddToExpression(C);
        End
      Else
        Begin
          If Not CheckNumberType(ExprType) Then
            AddIssue(Format(strExprConflict, [Token.Token, Token.Line,
              Token.Column]), scNone, 'Factor', Token.Line, Token.Column,
              etWarning);
          AddToExpression(C);
        End;
    End
  { Else If Token.UToken = 'NIL' Then
    AddToExpression(C) }
  Else If Token.Token = '@' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Expression(C, SubExprType);
    End
  Else If Token.Token = '&' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Expression(C, SubExprType);
    End
  Else If Token.Token = '^' Then
    Begin
      AddToExpression(C);
      SetupSubExprType;
      Expression(C, SubExprType);
    End
  Else If Token.Token = '@@' Then
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
      Designator(C, SubExprType);
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
  Else If AnonymousMethod(C) Then
    // Do nothing block...
  Else
    Begin
      SetupSubExprType;
      Designator(C, SubExprType);
    End;
End;

(**

  This method checks the type of number in the expression to make sure Integers
  and Floating point number aren`t mixed.

  @precon  None.
  @postcon Checks the type of number in the expression to make sure Integers
           and Floating point number aren`t mixed.

  @param   ExprType as a TPascalExprTypes
  @return  a Boolean

  @note    This may have problems with expression that allow integers and
           floats to be added, etc.

**)
Function TPascalModule.CheckNumberType(ExprType : TPascalExprTypes) : Boolean;

Begin
  If Pos('.', Token.Token) > 0 Then
    Result := petFloat In ExprType
  Else
    Result := petInteger In ExprType;
End;

(**

  This method check for the presence of a RelOp token at the current position
  and returns true if found and advances the token position else returns false

  @precon  None.
  @postcon Check for the presence of a RelOp token at the current position
           and returns true if found and advances the token position else
           returns false

  @param   C as a TElementContainer
  @param   ExprType as a TPascalExprTypes
  @return  a Boolean

**)
Function TPascalModule.RelOp(C : TElementContainer; ExprType : TPascalExprTypes) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strRelOps) And Not
    ((Token.Token = '=') And (petConstExpr In ExprType));
  If Result Then
    AddToExpression(C);
End;

(**

  This method check for the presence of an AddOp token at the current position
  and returns true if found and advances the token position else returns false

  @precon  None.
  @postcon Check for the presence of an AddOp token at the current position
           and returns true if found and advances the token position else
           returns false

  @param   C as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.AddOp(C : TElementContainer) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strAddOps);
  If Result Then
    AddToExpression(C);
End;

(**

  This method check for the presence of a MulOp token at the current position
  and returns true if found and advances the token position else returns false

  @precon  None.
  @postcon Check for the presence of a MulOp token at the current position
           and returns true if found and advances the token position else
           returns false

  @param   C as a TElementContainer
  @param   ExprType as a TPascalExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.MulOp(C : TElementContainer; var ExprType : TPascalExprTypes) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strMulOps);
  If Result Then
    Begin
      If Not (petString In ExprType) Then
        AddToExpression(C)
      Else
        ErrorAndSeekToken(strExprConflict, 'MulOp', Token.Token,
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**


  This method attempts to parse the current token position as a Designator.

  @precon  None
  @postcon Attempts to parse the current token position as a Designator.

  @param   C        as a TElementContainer
  @param   ExprType as a TPascalExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.Designator(C : TElementContainer; var ExprType : TPascalExprTypes) : Boolean;

Var
  M : TPascalMethod;
  Container : TElementContainer;

Begin
  Result := IsIdentifier(Token) Or
    (Token.UToken = 'NIL') Or (Token.UToken = 'STRING');
  If Result Then
    Begin
      M := CurrentMethod As TPascalMethod;
      If Token.Reference In [trUnknown] Then
        If M <> Nil Then
          Begin
            If Not M.ReferenceSymbol(Token) Then
              Token.Reference := trUnresolved
          End Else
            If Not ReferenceSymbol(Token) Then
              Token.Reference := trUnresolved;
      AddToExpression(C);
      Container := TTempCntr.Create('', Scope, 0, 0, iiNone, Nil);
      Try
        TypeArgs(Container);
        If (C <> Nil) And (Container.TokenCount > 0) Then
          C.AddTokens(Container);
      Finally
        Container.Free;
      End;
      DesignatorSubElement(C, ExprType, ['.', '[', '^', '(']);
    End;
End;

(**

  This method handles the sub elements of a designator, i. e. period,[,(and ^.

  @precon  None .
  @postcon Handles the sub elements of a designator , i . e . period , [, ( and
           ^.

  @param   C               as a TElementContainer
  @param   ExprType        as a TPascalExprTypes as a reference
  @param   strValidSymbols as an Array Of String

**)
Procedure TPascalModule.DesignatorSubElement(C : TElementContainer;
  var ExprType : TPascalExprTypes; strValidSymbols : Array of String);

var
  M: TPascalMethod;

Begin
  M := CurrentMethod As TPascalMethod;
  While IsKeyWord(Token.Token, strValidSymbols) Or (IsKeyWord(Token.Token, ['(', '['])) Do // Always check for proc/func
    If Token.Token = '.' Then
      Begin
        AddToExpression(C);
        If IsIdentifier(Token) Then
          Begin
            If Token.Reference In [trUnknown] Then
              If M <> Nil Then
                Begin
                  If Not M.ReferenceSymbol(Token) Then
                    Token.Reference := trUnresolved;
                End Else
                  If Not ReferenceSymbol(Token) Then
                    Token.Reference := trUnresolved;
            AddToExpression(C);
          End
        Else
          ErrorAndSeekToken(strIdentExpected, 'DesignatorSubElement', Token.Token,
            strSeekableOnErrorTokens, stActual);
      End
    Else If Token.Token = '[' Then
      Begin
        If Token.Reference In [trUnknown] Then
          If M <> Nil Then
            Begin
              If Not M.ReferenceSymbol(Token) Then
                Token.Reference := trUnresolved;
            End Else
              If Not ReferenceSymbol(Token) Then
                Token.Reference := trUnresolved;
        AddToExpression(C);
        ExprList(C);
        If Token.Token = ']' Then
          AddToExpression(C)
        Else
          ErrorAndSeekToken(strLiteralExpected, 'DesignatorSubElement', ']',
            strSeekableOnErrorTokens, stActual);
      End
    Else If Token.Token = '^' Then
      Begin
        If Token.Reference In [trUnknown] Then
          If M <> Nil Then
            Begin
              If Not M.ReferenceSymbol(Token) Then
                Token.Reference := trUnresolved;
            End Else
              If Not ReferenceSymbol(Token) Then
                Token.Reference := trUnresolved;
        AddToExpression(C);
      End
    Else If (Token.Token = '(') Then
      Begin
        If doStrictConstantExpressions In BrowseAndDocItOptions.Options Then
          If petConstExpr In ExprType Then
            If Not IsKeyWord(PrevToken.Token, strConstExprDesignators) Then
              Begin
                ErrorAndSeekToken(strConstExprDesignator, 'DesignatorSubElement',
                  PrevToken.Token, strSeekableOnErrorTokens, stActual);
                Exit;
              End;
        AddToExpression(C);
        If Token.Token <> ')' Then
          Begin
            MethodExprList(C);
            While Token.Token = ':' Do
              Begin
                AddToExpression(C);
                ExprList(C);
              End;
          End;
        If Token.Token = ')' Then
          AddToExpression(C)
        Else
          ErrorAndSeekToken(strLiteralExpected, 'DesignatorSubElement', ')',
            strSeekableOnErrorTokens, stActual);
      End;
End;

(**

  This method attempts to parse the current token position as a Set
  Constructor.

  @precon  None.
  @postcon Attempts to parse the current token position as a Set Constructor.

  @param   C as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.SetConstructor(C : TElementContainer) : Boolean;

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

  @precon  None.
  @postcon Attempts to parse the current token position as a set element.

  @param   C as a TElementContainer

**)
Procedure TPascalModule.SetElement(C : TElementContainer);

Var
  ExprType : TPascalExprTypes;

Begin
  Repeat
    ExprType := [petUnknown];
    Expression(C, ExprType);
  Until Not (IsToken('..', C) Or IsToken(',', C));
End;

(**

  This method attempts to parse the current token position as an Expression
  List.

  @precon  None.
  @postcon Attempts to parse the current token position as an Expression List.

  @param   C as a TElementContainer

**)
Procedure TPascalModule.ExprList(C : TElementContainer);

Var
  ExprType : TPascalExprTypes;

Begin
  Repeat
    ExprType := [petUnknown];
    Expression(C, ExprType);
  Until Not IsToken(',', C);
End;

(**

  This method parses an expression list which can contain anonymous methods.

  @precon  C must be a valid container.
  @postcon The expression list is parsed if valid.

  @param   C as a TElementContainer

**)
Procedure TPascalModule.MethodExprList(C : TElementContainer);

Var
  ExprType : TPascalExprTypes;
  Cntr : TTempCntr;

Begin
  Repeat
    If Not AnonymousMethod(C) Then
      Begin
        ExprType := [petUnknown];
        Cntr := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
        Try
          Expression(Cntr, ExprType);
          If (Cntr.TokenCount = 0) And (Cntr.ElementCount = 0) Then
            AddIssue(Format('The method implementation on line %d at column %d is missing a parameter!', [Token.Line, Token.Column]), scNone, 'MethofExprList', Token.Line, Token.Column, etError);
        Finally
          Cntr.Free;
        End;
      End;
  Until Not IsToken(',', C);
End;

(**


  This method attempts to parse the current token position as a statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a statement.

**)
Procedure TPascalModule.Statement;

Begin
  If CurrentMethod <> Nil Then
    Begin
      If CurrentMethod.LabelsLabel <> Nil Then // Check for label
        If CurrentMethod.LabelsLabel.FindElement(Token.Token) <> Nil Then
          Begin
            NextNonCommentToken;
            If Token.Token = ':' Then
              NextNonCommentToken
            Else
              ErrorAndSeekToken(strLiteralExpected, 'Statement', ':',
                strSeekableOnErrorTokens, stActual);
          End;
    End;
  If Not StructStmt Then
    SimpleStatement;
End;

(**


  This method attempts to parse the current token as a list of statements.

  @precon  None.
  @postcon Attempts to parse the current token as a list of statements.

**)
Procedure TPascalModule.StmtList;

Const
  strStatementTerminals : Array[1..6] Of String = ('else', 'end',
    'except', 'finalization', 'finally', 'until');

Var
  boolEnd : Boolean;

Begin
  Repeat
    Statement;
    boolEnd := Not IsToken(';', Nil);
    If boolEnd Then
      Begin
        boolEnd := IsKeyWord(Token.Token, strStatementTerminals);
        If Not boolEnd Then
          ErrorAndSeekToken(strLiteralExpected, 'StmtList', ';',
            strSeekableOnErrorTokens, stFirst);
      End
  Until boolEnd;
End;

(**


  This method attempts to evaluate the current token position as a Simple
  Statement.

  @precon  None.
  @postcon Attempts to evaluate the current token position as a Simple
           Statement.

**)
Procedure TPascalModule.SimpleStatement;

Var
  ExprType : TPascalExprTypes;
  C : TTempCntr;

Begin
  If Token.UToken = 'GOTO' Then
    Begin
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLabelExpected, 'SimpleStatement', Token.Token,
          strSeekableOnErrorTokens, stActual);
    End
  Else
    Begin
      If Token.UToken = 'INHERITED' Then
        NextNonCommentToken;
      If Token.Token = '(' Then
        Begin
          NextNonCommentToken;
          ExprType := [petUnknown];
          Expression(Nil, ExprType);
          If Token.Token = ')' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'SimpleStatement', ')',
              strSeekableOnErrorTokens, stActual);
          DesignatorSubElement(Nil, ExprType, ['.', '^']);
        End Else
      If Token.Token = '@' Then
        Begin
          NextNonCommentToken;
          ExprType := [petUnknown];
          Designator(Nil, ExprType);
        End Else
        Begin
          ExprType := [petUnknown];
          Designator(Nil, ExprType);
        End;
      If Token.Token = ':=' Then
        Begin
          NextNonCommentToken;
          C := TTempCntr.Create('', Scope, 0, 0, iiNone, Nil);
          Try
          ExprType := [petUnknown];
            Expression(C, ExprType);
            If (C.TokenCount = 0) And (C.ElementCount = 0) Then
              AddIssue(Format('Assignment missing at line %d column %d!', [Token.Line,
                Token.Column]), scNone, 'SimpleStatement', Token.Line, Token.Column,
                etError);
          Finally
            C.Free;
          End;
        End;
    End;
End;

(**

  This method attempts to parse the current token position as a structured
  statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a structured
           statement.

  @return  a Boolean

**)
Function TPascalModule.StructStmt : Boolean;

Begin
  Result :=
    CompoundStmt(Nil) Or
    ConditionalStmt Or
    LoopStmt Or
    WithStmt Or
    TryExceptAndFinallyStmt Or // <= Combined together as the type can not be
    RaiseStmt Or               //    determined until the Except or Finally
    AssemblerStatement;        //    key work is found.
End;

(**

  This method parses the compound statement section of a procedure
  implementation from the current token position using the following object
  pascal grammar.

  @precon  None.
  @postcon Parses the compound statement section of a procedure implementation
           from the current token position

  @param   Method as a TGenericFunction
  @return  a Boolean

**)
Function TPascalModule.CompoundStmt(Method : TGenericFunction) : Boolean;

Var
  strTemplate: String;
  slProlog: TStringList;
  i: Integer;

begin
  Result := Token.UToken = 'BEGIN';
  If Result Then
    Begin
      If moProfiling In ModOptions Then
        If Method <> Nil Then
          Begin
            Method.StartLine := Token.Line + 1;
            Method.Indent := Token.Column - 1;
            NextToken;
            // Check Profiling Prolog Code for a match
            strTemplate := StringReplace(
              BrowseAndDocItOptions.ProfilingCode[Self],
              '|', #13#10, [rfReplaceAll]);
            slProlog := PrologCode(strTemplate, Method.QualifiedName, 0);
            Try
              Method.HasProfiling := slProlog.Count > 0;
              For i := 0 To slProlog.Count - 1 Do
                Method.HasProfiling := Method.HasProfiling And
                  (CompareText(Trim(slProlog[i]),
                  Trim(FSourceCodeForProfiling[Method.StartLine + i - 1])) = 0);
            Finally
              slProlog.Free;
            End;
            PreviousToken;
          End;
      NextNonCommentToken;
      StmtList;
      If Token.UToken = 'END' Then
        Begin
          If moProfiling In ModOptions Then
            If Method <> Nil Then
              Method.EndLine := Token.Line - 1;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'CompoundStmt', 'END',
            ['end']{strSeekableOnErrorTokens}, stActual);
    End;
end;

(**

  This method attempts to parse the current token position as a
  ConditionalStmt.

  @precon  None.
  @postcon Attempts to parse the current token position as a ConditionalStmt.

  @return  a Boolean

**)
Function TPascalModule.ConditionalStmt : Boolean;

Begin
  Result := IfStmt Or CaseStmt;
End;

(**

  This method attempts to parse the current token position as an IF statement.

  @precon  None.
  @postcon Attempts to parse the current token position as an IF statement.

  @return  a Boolean

**)
Function TPascalModule.IfStmt : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.UToken = 'IF';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown];
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

  @precon  None.
  @postcon Attempts to parse the current token position as a CASE statement.

  @return  a Boolean

**)
Function TPascalModule.CaseStmt : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.UToken = 'CASE';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown];
      Expression(Nil, ExprType);
      If Token.UToken = 'OF' Then
        Begin
          NextNonCommentToken;
          Repeat
            If IsKeyWord(Token.Token, ['else', 'end']) Then
              Break;
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

  @precon  None.
  @postcon Attempts to parse the current token position as a case selector.

**)
Procedure TPascalModule.CaseSelector;

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

  @precon  None.
  @postcon Attempts to parse the current token position as a Case Label.

**)
Procedure TPascalModule.CaseLabel;

Var
  ExprType : TPascalExprTypes;

Begin
  ExprType := [petUnknown, petConstExpr];
  ConstExpr(Nil, ExprType);
  If Token.Token = '..' Then
    Begin
      NextNonCommentToken;
      ConstExpr(Nil, ExprType);
    End;
End;

(**

  This method attempts to parse the current token position as a Loop statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a Loop statement.

  @return  a Boolean

**)
Function TPascalModule.LoopStmt : Boolean;

Begin
  Result := RepeatStmt Or WhileStmt Or ForStmt;
End;

(**

  This method attempts to parse the current token position as a Repeat
  Statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a Repeat Statement.

  @return  a Boolean

**)
Function TPascalModule.RepeatStmt : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.UToken = 'REPEAT';
  If Result Then
    Begin
      NextNonCommentToken;
      StmtList;
      If Token.UToken = 'UNTIL' Then
        Begin
          NextNonCommentToken;
          ExprType := [petUnknown];
          Expression(Nil, ExprType);
        End
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'RepeatStmt', 'UNTIL',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a While
  Statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a While Statement.

  @return  a Boolean

**)
Function TPascalModule.WhileStmt : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.UToken = 'WHILE';
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown];
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

  @precon  None.
  @postcon Attempt to parse the current token position as a For statement.

  @return  a Boolean

**)
Function TPascalModule.ForStmt : Boolean;

Var
  ExprType : TPascalExprTypes;
  M: TPascalMethod;

Begin
  Result := Token.UToken = 'FOR';
  If Result Then
    Begin
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          M := CurrentMethod As TPascalMethod;
          If Token.Reference In [trUnknown] Then
            If M <> Nil Then
              Begin
                If Not M.ReferenceSymbol(Token) Then
                  Token.Reference := trUnresolved;
              End Else
                If Not ReferenceSymbol(Token) then
                  Token.Reference := trUnresolved;
          NextNonCommentToken;
          If Token.Token = ':=' Then
            Begin
              NextNonCommentToken;
              ExprType := [petUnknown];
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
            End
          Else If Token.UToken = 'IN' Then
            Begin
              NextNonCommentToken;
              ExprType := [petUnknown];
              Expression(Nil, ExprType);
              If Token.UToken = 'DO' Then
                Begin
                  NextNonCommentToken;
                  Statement;
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, 'ForStmt', 'DO',
                    strSeekableOnErrorTokens, stActual);
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

  @precon  None.
  @postcon Attempts to parse the current token position as a With Statement.

  @return  a Boolean

**)
Function TPascalModule.WithStmt : Boolean;

Begin
  Result := Token.UToken = 'WITH';
  If Result Then
    Begin
      NextNonCommentToken;
      Repeat
        ExprList(Nil);
      Until Not IsToken(',', Nil);
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

  @precon  None.
  @postcon Attempts to parse the current token position as a Try Except or Try
           Finally block.

  @return  a Boolean

**)
Function TPascalModule.TryExceptAndFinallyStmt : Boolean;

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

  This method attempt to parse the current token position as an Exception
  Block.

  @precon  None.
  @postcon Attempt to parse the current token position as an Exception Block.

  @return  a Boolean

**)
Function TPascalModule.ExceptionBlock : Boolean;

Var
  Con : TElementContainer;

Begin
  Result := False;
  While Token.UToken = 'ON' Do
    Begin
      Result := True;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          PushTokenPosition;
          NextNonCommentToken;
          If Token.Token = ':' Then
            NextNonCommentToken
          Else
            PopTokenPosition;
        End;
      Con := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        TypeId(Con);
        If Token.UToken = 'DO' Then
          Begin
            NextNonCommentToken;
            If Not CompoundStmt(Nil) Then
               Statement;
            If Token.Token = ';' Then
              NextNonCommentToken;
            If Token.UToken = 'ELSE' Then
              Begin
                NextNonCommentToken;
                StmtList;
              End;
          End Else
            If Token.UToken <> 'END' Then
              ErrorAndSeekToken(strReservedWordExpected, 'ExceptionBlock', 'DO',
                strSeekableOnErrorTokens, stActual);
      Finally
        Con.Free;
      End;
    End;
End;

(**

  This method attempts to parse the current token position as a Raise
  Statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a Raise Statement.

  @return  a Boolean

**)
Function TPascalModule.RaiseStmt : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.UToken = 'RAISE';
  If Result Then
    Begin
      NextNonCommentToken;
      SimpleStatement;
      If Uppercase(Token.Token) = 'AT' Then
        Begin
          NextNonCommentToken;
          ExprType := [petUnknown, petConstExpr];
          ConstExpr(Nil, ExprType);
        End;
    End;
End;

(**

  This method attempts to parse the current token position as an assembler
  statement.

  @precon  None.
  @postcon Attempts to parse the current token position as an assembler
           statement.

  @return  a Boolean

**)
Function TPascalModule.AssemblerStatement : Boolean;

Begin
  Result := Token.UToken = 'ASM';
  If Result Then
    Begin
      Repeat
        NextNonCommentToken;
      Until (Token.UToken = 'END') And (PrevToken.Token <> '@@');
      NextNonCommentToken;
    End;
End;

(**

  This function returns a string repreentation of the unit.

  @precon  None .
  @postcon Returns a string repreentation of the unit .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TPascalModule.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;

begin
  Result := strModuleTypes[ModuleType];
  If boolShowIdentifier Then
    Result := Result + #32 + ChangeFileExt(ExtractFileName(Identifier), '');
end;

(**

  This method parses a single RTTI Attribute declaration at the current token position.

  @precon  None.
  @postcon The current RTTI Attribute at the current token postiion is parsed else an
           error is raised.

**)
Procedure TPascalModule.AttributeDeclaration;

Var
  iExprType : TPascalExprTypes;

Begin
  If IsIdentifier(Token) Then
    Begin
      NextNonCommentToken;
      If Token.Token = '(' Then
        Begin
          Repeat
            NextNonCommentToken;
            If Token.Token = ')' Then
              Break;
            iExprType := [petUnknown];
            ConstExpr(Nil, iExprType);
          Until Token.Token <> ',';
          If Token.Token = ')' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'AttributeDeclaration', ')',
              strSeekableOnErrorTokens, stActual);
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, 'AttributeDeclaration', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a procedure declaration section from the current token
  position using the following object pascal grammar.

  @precon  AScope is the current scope of the procedure declaration and Method
           is the current method scoped else nil.
  @postcon Returns true is a procedure declaration was parsed.

  @param   AScope    as a TScope
  @return  a Boolean

**)
Function TPascalModule.ProcedureDeclSection(AScope : TScope) : Boolean;

Var
  M : TPascalMethod;
  Cls : Boolean;

Begin
  Result := False;
  Repeat
    RTTIAttributes;
    Cls := False;
    If Token.UToken = 'CLASS' Then
      Begin
        NextNonCommentToken;
        Cls := True;
      End;
    M := ProcedureDecl(AScope);
    If M = Nil Then
      M := FunctionDecl(AScope);
    If M = Nil Then
      M := ConstructorDecl(AScope);
    If M = Nil Then
      M := DestructorDecl(AScope);
    If M = Nil Then
      M := OperatorDecl(AScope);
    If M <> Nil Then
      Begin
        Result := True;
        M.ClassMethod := Cls;
      End;
  Until M = Nil;
End;

(**

  This method attempts to parse the current token position as a ProcedureDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a ProcedureDecl;

  @param   AScope    as a TScope
  @return  a TPascalMethod

**)
Function TPascalModule.ProcedureDecl(AScope : TScope) : TPascalMethod;

Begin
  Result := ProcedureHeading(AScope, CurrentMethod);
  If Result <> Nil Then
    Begin
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(Result);
          If PortabilityDirective Then
            If Token.Token = ';' Then
              NextNonCommentToken;
          If Not Result.ForwardDecl Then
            Begin
              Block(scLocal, Result);
              If Token.Token = ';' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, 'ProcedureDecl', ';',
                  strSeekableOnErrorTokens, stActual);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ProcedureDecl', ';',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a FunctionDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a FunctionDecl;

  @param   AScope    as a TScope
  @return  a TPascalMethod

**)
Function TPascalModule.FunctionDecl(AScope : TScope) : TPascalMethod;

Begin
  Result := FunctionHeading(AScope, CurrentMethod);
  If Result <> Nil Then
    Begin
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(Result);
          CheckFunctionReturn(Result);
          PortabilityDirective;
          If Not Result.ForwardDecl Then
            Begin
              Block(scLocal, Result);
              If Token.Token = ';' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, 'FunctionDecl', ';',
                  strSeekableOnErrorTokens, stActual);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'FunctionDecl', ';',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses the constraint element of the grammar for within a generic parameter
  definition.

  @precon  None.
  @postcon Parses the constraint element of the grammar for within a generic parameter
           definition.

**)
Procedure TPascalModule.Constraint;

Begin
  If IsKeyWord(Token.Token, ['class', 'constructor', 'record']) Then
    NextNonCommentToken
  Else
    If IsIdentifier(Token) Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strIdentExpected, 'Constraint', Token.Token,
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses the constraint list element of the generic parameters declaration.

  @precon  None.
  @postcon Parses the constraint list element of the generic parameters declaration.

**)
Procedure TPascalModule.ConstraintList;

Var
  boolComma : Boolean;

Begin
  Repeat
    Constraint;
    boolComma := Token.Token = ',';
    If boolComma Then
      NextNonCommentToken;
  Until Not boolComma;
End;

(**

  This method attempts to parse the current token position as a ConstructorDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a ConstructorDecl;

  @param   AScope    as a TScope
  @return  a TPascalMethod

**)
Function TPascalModule.ConstructorDecl(AScope : TScope) : TPascalMethod;

Begin
  Result := ConstructorHeading(AScope, CurrentMethod);
  If Result <> Nil Then
    Begin
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(Result);
          PortabilityDirective;
          If Not Result.ForwardDecl Then
            Begin
              Block(scLocal, Result);
              If Token.Token = ';' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, 'ConstructorDecl', ';',
                  strSeekableOnErrorTokens, stActual);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'ConstructorDecl', ';',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a DestructorDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a DestructorDecl;

  @param   AScope    as a TScope
  @return  a TPascalMethod

**)
Function TPascalModule.DestructorDecl(AScope : TScope) : TPascalMethod;

Begin
  Result := DestructorHeading(AScope, CurrentMethod);
  If Result <> Nil Then
    Begin
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(Result);
          PortabilityDirective;
          If Not Result.ForwardDecl Then
            Begin
              Block(scLocal, Result);
              If Token.Token = ';' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, 'DestructorDecl', ';',
                  strSeekableOnErrorTokens, stActual);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'DestructorDecl', ';',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method attempts to parse the current token position as a OperatorDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a OperatorDecl;

  @param   AScope    as a TScope
  @return  a TPascalMethod

**)
Function TPascalModule.OperatorDecl(AScope : TScope) : TPascalMethod;

Begin
  Result := OperatorHeading(AScope, CurrentMethod);
  If Result <> Nil Then
    Begin
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(Result);
          CheckFunctionReturn(Result);
          PortabilityDirective;
          If Not Result.ForwardDecl Then
            Begin
              Block(scLocal, Result);
              If Token.Token = ';' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, 'OperatorDecl', ';',
                  strSeekableOnErrorTokens, stActual);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'OperatorDecl', ';',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a function declaration from the current token position
  using the following object pascal grammar.

  @precon  AScope is the current scope of the function declaration.
  @postcon Returns a method declaration is a function was parsed else nil.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @param   boolIdent as a Boolean
  @return  a TPascalMethod

**)
Function TPascalModule.FunctionHeading(AScope :TScope;
  Container : TElementContainer; boolIdent : Boolean = true) : TPascalMethod;

Var
  C : TComment;
  boolClassMethod : Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtFunction]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = 'CLASS' Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken);
        End Else
          C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Xor boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'FunctionHeading', Token.Token,
            strSeekableOnErrorTokens, stActual);
          Exit;
        End;
      Result := MethodQualifiers(AScope, Container, mtFunction, boolClassMethod, C,
        boolIdent);
      FormalParameter(Result);
      If boolIdent Then
        CheckAlias(Result);
      CheckReturnValue(Result);
      Directive(Result, True);
    Finally
      AddToContainer(Container, Result);
    End;
End;

(**

  This method parses a operator declaration from the current token position
  using the following object pascal grammar.

  @precon  AScope is the current scope of the operator declaration.
  @postcon Returns a method declaration is a function was parsed else nil.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @param   boolIdent as a Boolean
  @return  a TPascalMethod

**)
Function TPascalModule.OperatorHeading(AScope :TScope;
  Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;

Var
  C : TComment;
  boolRequiresReturn: Boolean;
  boolClassMethod : Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtOperator]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = 'CLASS' Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken);
        End Else
          C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Xor boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'OperatorHeading', Token.Token,
            strSeekableOnErrorTokens, stActual);
          Exit;
        End;
      Result := MethodQualifiers(AScope, Container, mtOperator, boolClassMethod, C,
        boolIdent);
      If Not IsKeyWord(Result.Identifier, strOperatorList) Then
        AddIssue(Format(strInvalidOperator, [Result.Identifier, Result.Line,
          Result.Column]), AScope, 'OperatorHeading', Result.Line, Result.Column, etError);
      FormalParameter(Result);
      If boolIdent Then
        CheckAlias(Result);
      boolRequiresReturn := CheckReturnValue(Result);
      Directive(Result, True);
      If boolRequiresReturn Then
        If Result.Alias = '' Then
          AddIssue(Format(strFunctionWarning, [Result.QualifiedName]), scNone,
            'CheckReturnValue', Token.Line, Token.Column, etWarning);
    Finally
      AddToContainer(Container, Result);
    End;
End;

(**

  This method checks the alias (if one exists) of the procedure / function.

  @precon  Method must be a valid TPascalMethod instance.
  @postcon Checks the alias (if one exists) of the procedure / function.

  @param   Method as a TPascalMethod

**)
procedure TPascalModule.CheckAlias(Method : TPascalMethod);

Begin
  If Token.Token = '=' Then
    Begin
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          Method.Alias := Token.Token;
          NextNonCommentToken;
          If Token.Token = '.' Then
            Begin
              Method.Alias := Method.Alias + Token.Token;
              NextNonCommentToken;
              If IsIdentifier(Token) Then
                Begin
                  Method.Alias := Method.Alias + Token.Token;
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strIdentExpected, 'CheckAlias', Token.Token,
                    strSeekableOnErrorTokens, stActual);
            End;
        End Else
          ErrorAndSeekToken(strIdentExpected, 'CheckAlias', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method checks that function returns are present for non-aliased or
  external functions. If not present adds an issue.

  @precon  Func must be a valid instance.
  @postcon Checks that function returns are present for non-aliased or
           external functions. If not present adds an issue.

  @param   Func as a TPascalMethod

**)
procedure TPascalModule.CheckFunctionReturn(Func : TPascalMethod);

begin
  If Func.MethodType = mtFunction Then
    Begin
      If Func.ReturnType.ElementCount = 0 Then
        If Func.Alias = '' Then
          If Not Func.HasDirective('external') Then
            AddIssue(Format(strFunctionWarning, [Func.QualifiedName]), scNone,
              'CheckFunctionReturns', Token.Line, Token.Column, etWarning);
    End;
end;

(**

  This method checks the returns value of the function.

  @precon  Method must be a valid TPascalMethod instance .
  @postcon Checks the returns value of the function .

  @param   Method as a TGenericFunction
  @return  a Boolean

**)
Function TPascalModule.CheckReturnValue(Method : TGenericFunction) : Boolean;

Begin
  Result := False;
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      GetTypeDecl(TypeToken(Token, scNone, Nil, Method.ReturnType));
    End Else
      Result := True;
End;

(**

  This method cross reference the methods in class, exported and implemented
  and marsk the as resolved and output error messages for those that are still
  unresolved.

  @precon  None.
  @postcon Cross reference the methods in class, exported and implemented
           and marsk the as resolved and output error messages for those that
           are still unresolved.

**)
procedure TPascalModule.CheckUnResolvedMethods;

Var
  Errors: TLabelContainer;

begin
  ResolveScopeOfImplementedExportedMethods;
  ResolveScopeOfImplementedExportsMethods;
  ResolveScopeOfImplementedMethods(FImplementedMethodsLabel);
  // Only resolved methods IF there are no other errors.
  Errors := FindElement(strErrors) As TLabelContainer;
  If Errors <> Nil Then
    If Errors.ElementCount > 0 Then
      Exit;
  FindUnresolvedRecordObjectAndClassMethods(FTypesLabel);
  FindUnresolvedExportedMethods;
  {FindUnresolvedExportsMethods;}
  FindUnresolvedImplementedClassMethods(FImplementedMethodsLabel);
end;

(**

  This method parse a procedure declaration from the current token position
  using the following object pascal grammar.

  @precon  AScope is the current scope of the procedure declaration.
  @postcon Returns a method declaration is a procedure was parsed else nil.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @param   boolIdent as a Boolean
  @return  a TPascalMethod

**)
Function TPascalModule.ProcedureHeading(AScope : TScope;
  Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;

Var
  C : TComment;
  boolClassMethod : Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtProcedure]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = 'CLASS' Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken)
        End Else
          C := GetComment;
      NextNonCommentToken;
      If Not IsIdentifier(Token) And boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'ProcedureHeading', Token.Token,
            strSeekableOnErrorTokens, stActual);
          Exit;
        End;
      Result := MethodQualifiers(AScope, Container, mtProcedure, boolClassMethod, C,
        boolIdent);
      FormalParameter(Result);
      If boolIdent Then
        CheckAlias(Result);
      Directive(Result, True);
    Finally
      AddToContainer(Container, Result);
    End;
End;

(**

  This method parses a methods formal parameters from the current token
  position using the following object psacal grammar.

  @precon  Method is a valid method to which the formal parameters are to be
           added.
  @postcon Parses a methods formal parameters from the current token position

  @param   Method as a TPascalMethod

**)
Procedure TPascalModule.FormalParameter(Method : TPascalMethod);

Begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      Repeat
        FormalParam(Method);
      Until Not IsToken(';', Nil);
      If Token.Token = ')' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'FormalParameters', ')',
          strSeekableOnErrorTokens, stActual);
  End;
End;

(**

  This method parses the formal type parameter list element of the generic grammar.

  @precon  None.
  @postcon The formal type parameter list is parsed.

  @param   strIdentifier as a String as a Reference

**)
Procedure TPascalModule.FormalTypeParamList(var strIdentifier : String);

Begin
  If Token.Token = '<' Then
    Begin
      strIdentifier := strIdentifier + Token.Token;
      TypeParamDeclList(strIdentifier);
      If Token.Token = '>' Then
        Begin
          strIdentifier := strIdentifier + Token.Token;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'FormalTypeParamList', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a formal parameter for a method from the current token
  position using the following object pascal grammar.

  @precon  Method is a valid method to which the formal parameters are to be
           added.
  @postcon Parses a formal parameter for a method from the current token
           position

  @param   Method as a TPascalMethod

**)
Procedure TPascalModule.FormalParam(Method : TPascalMethod);

Var
  pmMod : TParamModifier;

Begin
  pmMod := pamNone;
  // Get modifiers
  If Token.UToken = 'VAR' Then
    pmMod := pamVar
  Else If Token.UToken = 'CONST' Then
    pmMod := pamConst
  Else If Token.UToken = 'OUT' Then
    pmMod := pamOut;
  If pmMod <> pamNone Then
    NextNonCommentToken;
  Parameter(Method, pmMod);
End;

(**

  This method parses a parameter list for a method from the current token
  position using the following object pascal grammar.

  @precon  Method is a valid method to add a parameter too and ParamMod is a
           parameter modifier for the parameter to signify a const, var or out
           paramemter.
  @postcon Parses a parameter list for a method from the current token position

  @param   Method   as a TPascalMethod
  @param   ParamMod as a TParamModifier

**)
Procedure TPascalModule.Parameter(Method : TPascalMethod;
  ParamMod : TParamModifier);

Var
  boolArrayOf : Boolean;
  strValue : String;
  j : Integer;
  T : TGenericTypeDecl;
  P, C : TElementContainer;
  ExprType : TPascalExprTypes;
  FTemporaryElements: TElementContainer;

Begin
  // Get ident list
  T := Nil;
  boolArrayOf := False;
  strValue := '';
  P := TTempCntr.Create('', scLocal, 0, 0, iiNone, Nil);
  Try
    P.Sorted:= False;
    FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
    Try
      IdentList(P, strSeekableOnErrorTokens);
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
          T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
          If T = Nil Then
            If Token.UToken = 'CONST' Then
              Begin
                T := FTemporaryElements.Add(TTypes.Create('tmp', scPrivate,
                  Token.Line, Token.Column, iiNone, Nil)) As TTypes;
                T.AddToken(Token.Token);
                NextNonCommentToken;
              End;
          // Get default value
          If Token.Token = '=' Then
            Begin
              NextNonCommentToken;
              C := TConstant.Create('', scLocal, 0, 0, iiNone, Nil);
              Try
                ExprType := [petConstExpr, petUnknown];
                ConstExpr(C, ExprType);
                strValue := C.AsString(True, False);
              Finally
                C.Free;
              End;
            End;
        End;
      // Create the parameters using the ident list
      For j := 1 To P.ElementCount Do
        Method.AddParameter(TPascalParameter.Create(ParamMod, P[j].Identifier,
          boolArrayOf, T, strValue, scPublic, P[j].Line, P[j].Column));
    Finally
      FTemporaryElements.Free;
    End;
  Finally
    P.Free;
  End;
End;

(**

  This method retrives the method directives after the method declaration from
  the current token position using the followong object pascal grammar.

  @precon  M is a valid method declaration to add directives too.
  @postcon Retrives the method directives after the method declaration from the
           current token position

  @param   M              as a TPascalMethod
  @param   boolGrammarFix as a Boolean

**)
Procedure TPascalModule.Directive(M : TPascalMethod; boolGrammarFix : Boolean = False);

Var
  C : TElementContainer;
  ExprType : TPascalExprTypes;
  strExternal: String;

Begin
  // Check for method directives
  While IsKeyWord(Token.Token, strMethodDirectives) Do
    Begin
      If Token.UToken = 'FORWARD' THEN
        M.ForwardDecl := True;
      If Token.UToken = 'ABSTRACT' THEN
        M.ForwardDecl := True;
      C := TIdentList.Create('', scLocal, 0, 0, iiNone, Nil);
      Try
        If Token.UToken = 'MESSAGE' Then
          Begin
            NextNonCommentToken;
            ExprType := [petConstExpr, petInteger];
            ConstExpr(C, ExprType);
            M.AddDirectives('Message ' + C.AsString(True, False));
          End
        Else If Token.UToken = 'EXTERNAL' Then
          Begin
            M.ForwardDecl := True;
            NextNonCommentToken;
            If Token.Token <> ';' Then
              Begin
                ExprType := [petConstExpr, petString];
                ConstExpr(C, ExprType);
                strExternal := C.AsString(True, False);
                If strExternal = '' Then
                  M.AddDirectives('External')
                Else
                  M.AddDirectives('External ' + strExternal);
                If Token.UToken = 'NAME' Then
                  Begin
                    NextNonCommentToken;
                    ExprType := [petConstExpr, petString];
                    ConstExpr(C, ExprType);
                    M.AddDirectives('Name ' + C.AsString(True, False));
                  End;
                If Token.UToken = 'INDEX' Then
                  Begin
                    NextNonCommentToken;
                    ExprType := [petConstExpr, petInteger];
                    ConstExpr(C, ExprType);
                    M.AddDirectives('Index ' + C.AsString(True, False));
                  End;
              End;
            M.Referenced := True;
          End
        Else If Token.UToken = 'DISPID' Then
          Begin
            NextNonCommentToken;
            ExprType := [petConstExpr, petInteger];
            ConstExpr(C, ExprType);
            M.AddDirectives('DispID ' + C.AsString(True, False));
          End
        Else
          Begin
            M.AddDirectives(Token.Token);
            NextNonCommentToken;
          End;
        If Not IsKeyWord(Token.Token, strMethodDirectives) Then
          If Token.Token = ';' Then      // no semi-colon         |
            Begin                        //                       v
              If Not boolGrammarFix Then // function X() : Integer stdcall;
                NextNonCommentToken
            End Else
              If Token.Token <> '=' Then
                ErrorAndSeekToken(strLiteralExpected, 'Directive', ';',
                  strSeekableOnErrorTokens, stActual);
      Finally
        C.Free;
      End;
    End;
End;

(**

  This method parses a class var section for an object declaration by delegating this to
  the ClassClassVarSection.

  @precon  Cls must be a valid instance
  @postcon Parses a class var section defined within an object delcaration.

  @param   AScope as a TScope
  @param   Cls    as a TRecordDecl
  @return  a Boolean

**)
Function TPascalModule.ObjClassVarSection(AScope: TScope; Cls: TRecordDecl): Boolean;

Begin
  Result := ClassClassVarSection(AScope, Cls);
End;

(**

  This method parses a constant section defined in an object delcaration by delegating
  this to the ConstSection method.

  @precon  None.
  @postcon Parses a constant section in an object definition.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ObjConstSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := ConstSection(AScope, Container);
End;

(**

  This method parses an Object type declaration from the current token position
  using the followong object pascal grammar.

  @precon  None.
  @postcon Returns an object declaration if one was parsed else nil.

  @param   AToken as a TTypeToken
  @return  a TObjectDecl

**)
function TPascalModule.ObjectType(AToken : TTypeToken) : TObjectDecl;

Var
  InternalScope : TScope;

begin
  InternalScope := scPublic;
  Result := Nil;
  If Token.UToken = 'OBJECT' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TObjectDecl.Create(FIdentifier, FScope, FLine,
          FColumn, iiPublicObject, FComment);
      Result := AToken.FContainer.Add(Result) As TObjectDecl;
      Result.Line := AToken.FLine;
      Result.Column := AToken.FColumn;
      Result.Comment := AToken.FComment ;
      NextNonCommentToken;
      // Get the classes heritage
      ObjHeritage(Result);
      // If this class has not body then return
      If Token.Token <> ';' Then
        Begin
          Repeat
            ObjVisibility(InternalScope);
            If Token.UToken = 'END' Then
              Break;
          Until Not (
            ObjTypeSection(InternalScope, Result) Or
            ObjConstSection(InternalScope, Result) Or
            ObjVarSection(InternalScope, Result) Or
            ObjClassVarSection(InternalScope, Result) Or
            ObjMethodList(Result, InternalScope, [mtConstructor..mtFunction]) Or
            ObjPropertyList(Result, InternalScope) Or
            ObjFieldList(Result, InternalScope)
          );
          // Check for 'END'
          If Token.UToken = 'END' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strReservedWordExpected, 'ObjectType', 'END',
              strSeekableOnErrorTokens, stActual);
        End Else
          NextNonCommentToken;
    End;
end;

(**

  This method attempts to parse the current token position as an Object Heritage
  list.

  @precon  None.
  @postcon Attempts to parse the current token position as an Object Heritage
           list.

  @param   ObjDecl as a TObjectDecl

**)
Procedure TPascalModule.ObjHeritage(ObjDecl : TObjectDecl);

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          ObjDecl.Heritage.AppendToken(Token);
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

  This method parses a list of methods defined in an object by delegating this to the
  MethodList method.

  @precon  Cls must be a valid instance
  @postcon Method within the oject declarations are parsed.

  @param   Cls                as a TRecordDecl
  @param   AScope             as a TScope
  @param   PermissibleMethods as a TPermissibleMethods
  @return  a Boolean

**)
Function TPascalModule.ObjMethodList(Cls: TRecordDecl; AScope: TScope;
  PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses the Property List element of the grammar within an object definition
  by delegating this to the ClassPropertyList method.

  @precon  Cls must be a valid instance
  @postcon Parse the property list element of the grammar within the object definition.

  @param   Cls    as a TRecordDecl
  @param   AScope as a TScope as a Reference
  @return  a Boolean

**)
function TPascalModule.ObjPropertyList(Cls: TRecordDecl; Var AScope: TScope): Boolean;

Begin
  Result := ClassPropertyList(Cls, AScope);
End;

(**

  This method parses a type section within an object declaration by delegating this to the
  TypeSection method.

  @precon  Cls must be a valid instance
  @postcon A type section is parsed which is defined within an object definition.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ObjTypeSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := TypeSection(AScope, Container);
End;

(**

  This method parses the var section declared with an object definition.

  @precon  Container must be a valid container.
  @postcon A var section is parsed if found.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ObjVarSection(AScope: TScope;
  Container: TElementContainer): Boolean;

Begin
  Result := VarSection(AScope, Container);
End;

(**

  This method parses the visibility elements of an object definiton.

  @precon  None.
  @postcon Parses an visibility elements of the object definiton return the visibility in
           the var parameter.

  @param   AScope as a TScope as a Reference

**)
Procedure TPascalModule.ObjVisibility(Var AScope: TScope);

Const
  strObjScope : Array[1..3] Of String = ('private', 'protected', 'public');

Begin
  While (Token.UToken = 'STRICT') Or IsKeyWord(Token.Token, strObjScope) Do
    Begin
      While Token.UToken = 'STRICT' Do
        Begin
          NextNonCommentToken;
          If IsKeyWord(Token.Token, strStrictedScope) Then
            Begin
              If Token.UToken = 'PRIVATE' Then
                Begin
                  AScope := scPrivate;
                  NextNonCommentToken;
                End
              Else If Token.UToken = 'PROTECTED' Then
                Begin
                  AScope := scProtected;
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, 'ObjVisibility',
                    'PRIVATE or PROTECTED', strSeekableOnErrorTokens, stActual);
            End;
        End;
      While IsKeyWord(Token.Token, strObjScope) Do
        Begin
          If Token.UToken = 'PRIVATE' Then
            Begin
              AScope := scPrivate;
              NextNonCommentToken;
            End
          Else If Token.UToken = 'PROTECTED' Then
            Begin
              AScope := scProtected;
              NextNonCommentToken;
            End
          Else If Token.UToken = 'PUBLIC' Then
            Begin
              AScope := scPublic;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'ObjVisibility',
                'PRIVATE, PROTECTED or PUBLIC', strSeekableOnErrorTokens, stActual);
        End;
    End;
End;

(**

  This method parse a method list from the current token position using the following
  object pascal grammar.

  @precon  Cls is an object declaration to add methods too and Scopeis the current
           internal scope of the object.
  @postcon Returns true is a method declaration was parsed.

  @param   Cls                as a TRecordDecl
  @param   AScope             as a TScope
  @param   PermissibleMethods as a TPermissibleMethods
  @return  a Boolean

**)
Function TPascalModule.MethodList(Cls: TRecordDecl; AScope: TScope;
  PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  RTTIAttributes;
  Result := MethodHeading(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses the quantified record / object / class names that preceed an
  implemented method.

  @precon  None.
  @postcon The method builds a list of the record / object / class hierarchy that
           qualifies the implemented method.

  @param   AScope          as a TScope
  @param   Container       as a TElementContainer
  @param   iMethodType     as a TMethodType
  @param   boolClassMethod as a Boolean
  @param   C               as a TComment
  @param   boolIdent       as a Boolean
  @return  a TPascalMethod

**)
Function TPascalModule.MethodQualifiers(AScope :TScope;
  Container : TElementContainer; iMethodType : TMethodType; boolClassMethod : Boolean;
  C : TComment; boolIdent : Boolean = True) : TPascalMethod;

Var
  slClassNames: TStringList;
  iLine: Integer;
  iColumn: Integer;
  strIdentifier: String;

Begin
  iLine := 0;
  iColumn := 0;
  slClassNames := TStringList.Create;
  Try
    Try
      iLine := Token.Line;
      iColumn := Token.Column;
      If boolIdent Then
        Begin
          strIdentifier := Token.Token;
          NextNonCommentToken;
          FormalTypeParamList(strIdentifier);
          While Token.Token = '.' Do
            Begin
              NextNonCommentToken;
              If Not IsIdentifier(Token) Then
                ErrorAndSeekToken(strIdentExpected, 'ProcessClsIdents', Token.Token,
                  strSeekableOnErrorTokens, stActual);
              If Not (Container Is TClassDecl) Then
                Begin
                  slClassNames.Add(strIdentifier);
                  strIdentifier := Token.Token;
                  iLine := Token.Line;
                  iColumn := Token.Column;
                End Else
                Begin
                  If strIdentifier <> '' Then
                    strIdentifier := strIdentifier + '.';
                  strIdentifier := strIdentifier + Token.Token;
                End;
              NextNonCommentToken;
              FormalTypeParamList(strIdentifier);
            End;
        End;
    Finally
      Result := TPascalMethod.Create(iMethodType, strIdentifier, AScope,
        iLine, iColumn);
      Result.ClassNames.Assign(slClassNames);
      Result.RecObjClsInt := FindRecObjClsInt(slClassNames);
      Result.Comment := C;
      Result.ClassMethod := boolClassMethod;
    End;
  Finally
    slClassNames.Free;
  End;
End;

(**

  This method checks for and parses a method declaration in a class from the current token
  position.

  @precon  Cls is an object declaration to add method declarations too and Scope is the
           current scope inside the object declaration.
  @postcon Returns true if a method declaration was parsed.

  @param   Cls                as a TRecordDecl
  @param   AScope             as a TScope
  @param   PermissibleMethods as a TPermissibleMethods
  @return  a Boolean

**)
Function TPascalModule.MethodHeading(Cls: TRecordDecl; AScope: TScope;
  PermissibleMethods : TPermissibleMethods): Boolean;

Var
  M :TPascalMethod;
  boolClassMethod : Boolean;

begin
  Result := False;
  // Check for class method
  boolClassMethod := False;
  PushTokenPosition;
  If Token.UToken = 'CLASS' Then
    Begin
      boolClassMethod := True;
      NextNonCommentToken;
    End;
  // Check for method
  M := ProcedureHeading(AScope, Cls);
  If M = Nil Then
    M := FunctionHeading(AScope, Cls);
  If M = Nil Then
    M := ConstructorHeading(AScope, Cls);
  If M = Nil Then
    M := DestructorHeading(AScope, Cls);
  If M = Nil Then
    M := OperatorHeading(AScope, Cls);
  If M <> Nil Then
    Begin
      M.ClassMethod := boolClassMethod;
      M.ForwardDecl := True;
      Result := True;
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          Directive(M);
          CheckFunctionReturn(M);
          If PortabilityDirective Then
            If Token.Token = ';' Then
              NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'MethodHeading', ';',
            strSeekableOnErrorTokens, stActual);
      If Not (M.MethodType In PermissibleMethods) Then
        AddIssue(strMethodNotPermitted, AScope, 'MethodHeading', M.Line, M.Column,
          etError);
    End Else
      If boolClassMethod Then
        PopTokenPosition;
end;

(**

  This method parses a constructor declaration from the current token position
  using the following object pascal grammar.

  @precon  Scope is the current scope of the constructor declaration.
  @postcon Returns a method declaration is a constructor was parsed else nil.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a TPascalMethod

**)
function TPascalModule.ConstructorHeading(AScope: TScope;
  Container : TElementContainer): TPascalMethod;

Var
  C : TComment;
  boolClassMethod : Boolean;

begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtConstructor]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = 'CLASS' Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken)
        End Else
          C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          Result := MethodQualifiers(AScope, Container, mtConstructor, boolClassMethod, C);
          FormalParameter(Result);
        End;
    Finally
      AddToContainer(Container, Result);
    End;
end;

(**

  This method parses a destructor declaration from the current token position
  using the following object pascal grammar.

  @precon  Scope is the current scope of the destructor declaration.
  @postcon Returns a method declaration is a destructor was parsed else nil.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a TPascalMethod

**)
function TPascalModule.DestructorHeading(AScope: TScope;
  Container : TElementContainer): TPascalMethod;

Var
  C : TComment;
  boolClassMethod: Boolean;

begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtDestructor]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = 'CLASS' Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken)
        End Else
          C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          Result := MethodQualifiers(AScope, Container, mtDestructor, boolClassMethod, C);
          FormalParameter(Result);
        End;
    Finally
      AddToContainer(Container, Result);
    End;
end;

(**


  This method parses a classes / interfaces field list from the current token
  position using the following object pascal grammar.

  @precon  Cls is an ibject delcaration to add fields too and Scope is the
           current internal scope of the object.
  @postcon Returns true is a field was parsed.

  @param   Cls    as a TObjectDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.FieldList(Cls: TObjectDecl; AScope: TScope): Boolean;

Var
  I : TIdentList;
  j : Integer;
  P, tmpP : TField;
  T : TGenericTypeDecl;
  FTemporaryElements: TElementContainer;

begin
  Result := False;
  I := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    IdentList(I, strSeekableOnErrorTokens);
    If Token.Token = ':' Then
      Begin
        Result := True;
        NextNonCommentToken;
        FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
        Try
          T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
          For j := 1 To I.ElementCount Do
            Begin
              tmpP := TField.Create(I[j].Name, AScope, I[j].Line, I[j].Column,
                iiPublicField, I[j].Comment);
              If Cls.FieldsLabel = Nil Then
                Cls.FieldsLabel := Cls.Add(strFieldsLabel, iiFieldsLabel, scNone,
                  Nil) As TLabelContainer;
              P := Cls.FieldsLabel.Add(tmpP) As TField;
              If P <> tmpP Then
                AddIssue(Format(strDuplicateIdentifierFound, [I[j].Name,
                  I[j].Line, I[j].Column]), scNone, 'ObjFieldDecl', I[j].Line,
                  I[j].Column, etError);
              If T <> Nil Then
                P.AddTokens(T)
              Else
                ErrorAndSeekToken(strTypeDeclExpected, 'ObjFieldList', '',
                  strSeekableOnErrorTokens, stFirst);
            End;
        Finally
          FTemporaryElements.Free;
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

  @precon  None.
  @postcon Parses the modules initialisation / finalisation section from the
           current token position

**)
Procedure TPascalModule.InitSection;

Begin
  If Token.UToken = 'INITIALIZATION' Then
    Begin
      Add(TInitializationSection.Create(Token.Token, scNone, Token.Line,
        Token.Column, iiInitialization, GetComment));
      NextNonCommentToken;
      StmtList;
      If Token.UToken = 'FINALIZATION' Then
        Begin
          Add(TFinalizationSection.Create(Token.Token, scNone, Token.Line,
            Token.Column, iiFinalization, GetComment));
          NextNonCommentToken;
          StmtList;
        End;
      If Token.UToken = 'END' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'Initsection',
          'END', strSeekableOnErrorTokens, stActual);
    End
  Else If CompoundStmt(Nil) Then
    Begin
      // Do Nothing...
    End
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

  @precon  None.
  @postcon Returns a class declaration is a class was parsed else nil.

  @param   AToken as a TTypeToken
  @return  a TClassDecl

**)
function TPascalModule.ClassType(AToken : TTypeToken) : TClassDecl;

Var
  InternalScope : TScope;
  boolFieldAllowed: Boolean;

begin
  boolFieldAllowed := True;
  InternalScope := scPublished;
  Result := Nil;
  If Token.UToken = 'CLASS' Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      // Check for 'OF'
      If Token.UToken <> 'OF' Then
        Begin
          UpdateTypeToken(AToken);
          With AToken Do
            Result := TClassDecl.Create(FIdentifier, FScope, FLine,
              FColumn, iiPublicClass, FComment);
          Result := AToken.FContainer.Add(Result) As TClassDecl;
          Result.Line := AToken.FLine;
          Result.Column := AToken.FColumn;
          Result.Comment := AToken.FComment;
          Result.AbstractClass := (Token.UToken = 'ABSTRACT');
          If Result.AbstractClass Then
            NextNonCommentToken;
          Result.SealedClass := (Token.UToken = 'SEALED');
          If Result.SealedClass Then
            NextNonCommentToken;
          Result.HelperClass := (Token.UToken = 'HELPER');
          If Result.HelperClass Then
            Begin
              NextNonCommentToken;
              boolFieldAllowed := False;
            End;
          // Get the classes heritage
          RecObjClsIntHeritage(Result);
          If Result.HelperClass Then
            If Token.UToken = 'FOR' Then
              Begin
                NextNonCommentToken;
                If IsIdentifier(Token) Then
                  Begin
                    Result.HelperClassName := Token.Token;
                    NextNonCommentToken;
                  End Else
                    ErrorAndSeekToken(strIdentExpected, 'ClassType', Token.Token,
                      strSeekableOnErrorTokens, stActual);
              End Else
                ErrorAndSeekToken(strReservedWordExpected, 'ClassType', 'FOR',
                  strSeekableOnErrorTokens, stActual);
          // If this class has no body then return
          If Token.Token <> ';' Then
            Begin
              Repeat
                ClassVisibility(InternalScope);
                If Token.UToken = 'END' Then
                  Break;
              Until Not (
                ClassTypeSection(InternalScope, Result) Or
                ClassConstSection(InternalScope, Result) Or
                ClassVarSection(InternalScope, Result) Or
                ClassClassVarSection(InternalScope, Result) Or
                ClassMethodList(Result, InternalScope, [mtConstructor..mtFunction]) Or
                ClassPropertyList(Result, InternalScope) Or
                (boolFieldAllowed And ClassFieldList(Result, InternalScope))
              );
              // Check for 'END'
              If Token.UToken = 'END' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strReservedWordExpected, 'ClassType', 'END',
                  strSeekableOnErrorTokens, stActual);
            End;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the grammar associated with a type declaration with in a class
  declaration.

  @precon  None.
  @postcon Parses the grammar associated with a type declaration with in a class
           declaration.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TPascalModule.ClassTypeSection(AScope: TScope;
      Container: TElementContainer): Boolean;

Begin
  Result := TypeSection(AScope, Container);
End;

(**

  This method parses a class heriage ist from the current token position using the
  following object pascal grammar.

  @precon  Cls is a valid object declaration to get a heritage for.
  @postcon Parses a class heriage ist from the current token position

  @param   RecObjClsInt as a TRecordDecl

**)
procedure TPascalModule.RecObjClsIntHeritage(RecObjClsInt: TRecordDecl);

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      IdentList(RecObjClsInt.Heritage, strSeekableOnErrorTokens);
      If Token.Token = ')' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'ClassHeritage', ')',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the grammar associated with a var declaration with in a class
  declaration.

  @precon  None.
  @postcon Parses the grammar associated with a var declaration with in a class
           declaration.

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ClassVarSection(AScope: TScope;
      Container: TElementContainer): Boolean;

Begin
  Result := VarSection(AScope, Container);
End;

(**

  This method parse the class visibility from the current token
  using the following object pascal grammar.

  @precon  Scope is the current internal scope of the class.
  @postcon Parse the class visibility from the current token

  @param   AScope as a TScope as a reference

**)
Procedure TPascalModule.ClassVisibility(Var AScope: TScope);

Begin
  While (Token.UToken = 'STRICT') Or IsKeyWord(Token.Token, strScope) Do
    Begin
      While Token.UToken = 'STRICT' Do
        Begin
          NextNonCommentToken;
          If IsKeyWord(Token.Token, strStrictedScope) Then
            Begin
              If Token.UToken = 'PRIVATE' Then
                AScope := scPrivate
              Else If Token.UToken = 'PROTECTED' Then
                AScope := scProtected;
              NextNonCommentToken;
            End;
        End;
      While IsKeyWord(Token.Token, strScope) Do
        Begin
          If Token.UToken = 'PRIVATE' Then
            AScope := scPrivate
          Else If Token.UToken = 'PROTECTED' Then
            AScope := scProtected
          Else If Token.UToken = 'PUBLIC' Then
            AScope := scPublic
          Else
            AScope := scPublished;
          NextNonCommentToken;
        End;
    End;
End;

(**


  This method parses a class field list from the current token position using
  the following object pascal grammar.

  @precon  Cls is a valid object declaration to add fields too and Scope is the
           current scope of the class.
  @postcon Returns true is field where handled and parsed.

  @param   Cls    as a TObjectDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.ClassFieldList(Cls: TObjectDecl; AScope: TScope): Boolean;

Begin
  RTTIAttributes;
  Result := FieldList(Cls, AScope);
  If Result Then
    If Token.Token = ';' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, 'ClassFieldList', ';',
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a class method list from the current token position using the
  following object pascal grammar.

  @precon  Cls is a valid object declaration to get method for and Scope is the current
           scope of the class.
  @postcon Returns true is method were parsed.

  @param   Cls                as a TRecordDecl
  @param   AScope             as a TScope
  @param   PermissibleMethods as a TPermissibleMethods
  @return  a Boolean

**)
Function TPascalModule.ClassMethodList(Cls: TRecordDecl; AScope: TScope;
  PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses a class property list frmo the current token position using
  the following object pascal grammar.

  @precon  Cls is a valid class declaration to get method for and Scope is the
           current scope of the class.
  @postcon Returns true is properties were parsed.

  @param   Cls    as a TRecordDecl
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.ClassPropertyList(Cls: TRecordDecl; var AScope: TScope): Boolean;

Begin
  RTTIAttributes;
  Result :=  PropertyList(Cls, AScope);
  If Result Then
    Begin
      If Token.Token = ';' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, 'ClassPropertyList', ';',
          strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a class property list from the current token position using
  the following object pascal grammar.

  @precon  Cls is a valid class declaration to get method for and Scope is the
           current scope of the class.
  @postcon Returns true is properties were parsed.

  @param   Cls    as a TRecordDecl
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.PropertyList(Cls: TRecordDecl; var AScope: TScope): Boolean;

Var
  tmpP : TPascalProperty;
  C : TComment;
  P: TPascalProperty;
  PropertiesLabel: TLabelContainer;
  boolIsClassProp : Boolean;

begin
  boolIsClassProp := False;
  If Token.UToken = 'CLASS' THEN
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      boolIsClassProp := True;
    End;
  Result := Token.UToken = 'PROPERTY';
  If Result Then
    Begin
      C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          PropertiesLabel := Cls.FindElement(strPropertiesLabel) As TLabelContainer;
          If PropertiesLabel = Nil Then
            PropertiesLabel := Cls.Add(strPropertiesLabel, iiPropertiesLabel,
              scNone, Nil) As TLabelContainer;
          tmpP := TPascalProperty.Create(Token.Token, AScope, Token.Line,
            Token.Column, iiPublicProperty, C);
          P := PropertiesLabel.Add(tmpP) As TPascalProperty;
          If P <> tmpP Then
            AddIssue(Format(strDuplicateIdentifierFound, [Token.Token,
              Token.Line, Token.Column]), scNone,  'AddToContainer', Token.Line,
              Token.Column, etError);
          NextNonCommentToken;
          PropertyInterface(P);
          PropertySpecifiers(P);
          P.IsClassProperty := boolIsClassProp;
          PortabilityDirective;
        End Else
          ErrorAndSeekToken(strIdentExpected, 'PropertyList', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End Else
      If boolIsClassProp Then
        PopTokenPosition;
end;

(**


  This method parses the property interface from the current token position
  using the following object pascal grammar.

  @precon  Prop is a property to parse an interface for.
  @postcon Parses the property interface from the current token position

  @param   Prop   as a TPascalProperty

**)
Procedure TPascalModule.PropertyInterface(Prop : TPascalProperty);

Begin
  PropertyParameterList(Prop);
  CheckReturnValue(Prop);
End;

(**


  This method parses a properties parameter list from the current token using
  the following object pascal grammar.

  @precon  Prop is a property to parse a parameter list for.
  @postcon Parses a properties parameter list from the current token

  @param   Prop   as a TPascalProperty

**)
Procedure TPascalModule.PropertyParameterList(Prop : TPascalProperty);

Var
  ParamMod : TParamModifier;
  I : TIdentList;
  j : Integer;
  T : TGenericTypeDecl;
  FTemporaryElements: TElementContainer;

Begin
  If Token.Token = '[' Then
    Begin
      Repeat
        NextNonCommentToken;
        ParamMod := pamNone;
        If Token.UToken = 'VAR' Then
          ParamMod := pamVar;
        If Token.UToken = 'CONST' Then
          ParamMod := pamConst;
        If Token.UToken = 'OUT' Then
          ParamMod := pamOut;
        If ParamMod <> pamNone Then
          NextNonCommentToken;
        I := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
        Try
          IdentList(I, strSeekableOnErrorTokens);
          If Token.Token = ':' Then
            Begin
              NextNonCommentToken;
              FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
              Try
                T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
                If T <> Nil Then
                  For j := 1 To I.ElementCount Do
                    Prop.AddParameter(TPascalParameter.Create(
                      ParamMod, I[j].Identifier, False, T, '', scPublic, I[j].Line,
                      I[j].Column));
              Finally
                FTemporaryElements.Free;
              End;
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

  @precon  Prop is a property to parse specifiers for.
  @postcon Parses the property specifiers from the current token position

  @param   Prop as a TPascalProperty

**)
procedure TPascalModule.PropertySpecifiers(Prop: TPascalProperty);

Var
  C : TPropertySpec;
  ExprType : TPascalExprTypes;

begin
  // Check for index
  If Token.UToken = 'INDEX' Then
    Begin
      NextNonCommentToken;
      ExprType := [petInteger, petConstExpr];
      C := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        ConstExpr(C, ExprType);
        Prop.IndexSpec := C.AsString(True, False);
      Finally
        C.Free;
      End;
    End;
  // Check for read
  If Token.UToken = 'READ' Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown];
      C := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        Designator(C, ExprType);
        Prop.ReadSpec := C.AsString(True, False);
        If C.TokenCount > 0 Then
          Prop.ReferenceSymbol(C.Tokens[0]);
      Finally
        C.Free;
      End;
    End;
  // Check for write
  If Token.UToken = 'WRITE' Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown];
      C := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        Designator(C, ExprType);
        Prop.WriteSpec := C.AsString(True, False);
        If C.TokenCount > 0 Then
          Prop.ReferenceSymbol(C.Tokens[0]);
      Finally
        C.Free;
      End;
    End;
  // Check for stored
  If Token.UToken = 'STORED' Then
    Begin
      NextNonCommentToken;
      ExprType := [petInteger, petConstExpr];
      C := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        ConstExpr(C, ExprType);
        Prop.StoredSpec := C.AsString(True, False);
      Finally
        C.Free;
      End;
    End;
  // Check for default
  If Token.UToken = 'DEFAULT' Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown, petConstExpr];
      C := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        ConstExpr(C, ExprType);
        Prop.DefaultSpec := C.AsString(True, False);
      Finally
        C.Free;
      End;
    End;
  If Token.UToken = 'NODEFAULT' Then
    NextNonCommentToken;
  // Check for implements
  If Token.UToken = 'IMPLEMENTS' Then
    Begin
      NextNonCommentToken;
      IdentList(Prop.ImplementsSpec, strSeekableOnErrorTokens);
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
      ExprType := [petInteger, petConstExpr];
      C := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        ConstExpr(C, ExprType);
        Prop.DispIdSpec := C.AsString(True, False);
      Finally
        C.Free;
      End;
    End;
  // Check for ';'
  If Token.Token = ';' Then
    Begin
      //PushTokenPosition;
      NextNonCommentToken;
      // Check for default property
      If Token.UToken = 'DEFAULT' Then
        Begin
          Prop.DefaultProperty := True;
          NextNonCommentToken;
        End Else
          //PopTokenPosition;
          RollBackToken;
    End;
end;

(**

  This method parses an Interface declaration from the current token position
  using the following object pascal grammar.

  @precon  None.
  @postcon Returns an interface declaration if one was parsed else nil.

  @param   AToken as a TTypeToken
  @return  a TInterfaceDecl

**)
function TPascalModule.InterfaceType(AToken : TTypeToken) : TInterfaceDecl;

Var
  InternalScope : TScope;

begin
  InternalScope := scPublic;
  Result := Nil;
  If (Token.UToken = 'INTERFACE') Or (Token.UToken = 'DISPINTERFACE') Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        If Token.UToken = 'INTERFACE' Then
          Begin
            Result := TInterfaceDecl.Create(FIdentifier, FScope, FLine,
              FColumn, iiPublicInterface, FComment);
            Result := AToken.FContainer.Add(Result) as TInterfaceDecl;
            Result.Line := AToken.FLine;
            Result.Column := AToken.FColumn;
            Result.Comment := AToken.FComment ;
          End Else
          Begin
            Result := TDispInterfaceDecl.Create(FIdentifier, FScope, FLine,
              FColumn, iiPublicDispInterface, FComment);
            Result := AToken.FContainer.Add(Result) as TDispInterfaceDecl;
            Result.Line := AToken.FLine;
            Result.Column := AToken.FColumn;
            Result.Comment := AToken.FComment ;
          End;
      NextNonCommentToken;
      // If this class has not body then return
      If Token.Token <> ';' Then
        Begin
          // Get the classes heritage
          InterfaceHeritage(Result);
          // Get GUID if there is one
          If Token.Token = '[' Then
            Begin
              PushTokenPosition;
              NextNonCommentToken;
              If Token.TokenType In [ttSingleLiteral] Then
                Begin
                  Result.GUID := Token.Token;
                  NextNonCommentToken;
                  If Token.Token = ']' Then
                    NextNonCommentToken
                  Else
                    ErrorAndSeekToken(strLiteralExpected, 'InterfaceType', ']',
                      strSeekableOnErrorTokens, stActual);
                End Else
                  PopTokenPosition;
            End;
          Repeat
            If Token.UToken = 'END' Then
              Break;
          Until Not (
            InterfaceMethodList(Result, InternalScope, [mtProcedure..mtFunction]) Or
            InterfacePropertyList(Result, InternalScope)
          );
          // Check for 'END' and ';'
          If Token.UToken = 'END' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strReservedWordExpected, 'InterfaceType', 'END',
              strSeekableOnErrorTokens, stActual);
        End;
    End;
end;

(**

  This method attempts to parse the current token position as a heritage list.

  @precon  None.
  @postcon Attempts to parse the current token position as a heritage list.

  @param   InterfaceDecl as a TInterfaceDecl

**)
Procedure TPascalModule.InterfaceHeritage(InterfaceDecl : TInterfaceDecl);

begin
  RecObjClsIntHeritage(InterfaceDecl); // Same as ClassHeritage
End;

(**

  This method parses the gramar for the method list of an interface.

  @precon  Cls must be a valid instance.
  @postcon The grammar element is parsed.

  @param   Cls                as a TRecordDecl
  @param   AScope             as a TScope
  @param   PermissibleMethods as a TPermissibleMethods
  @return  a Boolean

**)
function TPascalModule.InterfaceMethodList(Cls: TRecordDecl; AScope: TScope;
  PermissibleMethods: TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses the grammar for an interace property by delegating the process to the
  ClassPropertyList method.

  @precon  Cls must be a valid reference.
  @postcon The property list element of the grammar is parsed.

  @param   Cls as a TRecordDecl
  @param   AScope as a TScope as a Reference
  @return  a Boolean

**)
Function TPascalModule.InterfacePropertyList(Cls: TRecordDecl;
  Var AScope: TScope): Boolean;

Begin
  Result := ClassPropertyList(Cls, AScope);
End;

(**

  This method parses a requires clause from the current token position usnig
  the following object pascal grammar.

  @precon  None.
  @postcon Parses a requires clause from the current token position usnig

**)
Procedure TPascalModule.RequiresClause;

Var
  R : TElementContainer;

Begin
  If Token.UToken = 'REQUIRES' Then
    Begin
      R := Add(strRequiresLabel, iiUsesLabel, scNone, GetComment);
      NextNonCommentToken;
      IdentList(R, strSeekableOnErrorTokens, iiUsesItem);
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'RequiresClause', ';',
          strSeekableOnErrorTokens, stActual);
      NextNonCommentToken;
    End;
End;

(**

  This method parses a contains clause fro the cutrrent token position using
  the following object pascal grammar.

  @precon  None.
  @postcon Parses a contains clause fro the cutrrent token position

**)
Procedure TPascalModule.ContainsClause;

Var
  C : TElementContainer;

Begin
  If Token.UToken = 'CONTAINS' Then
    Begin
      C := Add(strContainsLabel, iiUsesLabel, scNone, GetComment);
      NextNonCommentToken;
      IdentList(C, strSeekableOnErrorTokens, iiUsesItem);
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, 'ContainsClause', ';',
          strSeekableOnErrorTokens, stActual);
      NextNonCommentToken;
    End;
End;

(**

  This method creates a identifier list starting at the current token and
  return the list to the calling function. If OwnList is true then the identlist
  is added to the classes owned items list for automatic disposal, else it the
  responsibliity of the calling function to disposal of the class.

  @precon  OwnList determines if the identlist should be disposed of be the
           parser or be the caller. SeekTokens is a sorted lowercase list of
           token to find if an error is found.
  @postcon Returns an ident list.

  @param   Container   as a TElementContainer
  @param   SeekTokens  as an Array Of String
  @param   iImageIndex as a TBADIImageIndex

**)
Procedure TPascalModule.IdentList(Container : TElementContainer;
  SeekTokens : Array Of String; iImageIndex : TBADIImageIndex = iiNone);

Var
  C, AComment : TComment;
  I: TIdentList;
  strUnit : String;
  iLine: Integer;
  iColumn: Integer;
  TempContainer: TElementContainer;
  iToken: Integer;

Begin
  AComment := Nil;
  If IsIdentifier(Token) Then
    Repeat
      If IsIdentifier(Token) Then
        Begin
          C := GetComment;
          If C <> Nil then
            Begin
              AComment := TPascalComment.Create(C);
              OwnedItems.Add(AComment);
            End;
          strUnit := Token.Token;
          iLine := Token.Line;
          iColumn := Token.Column;
          NextNonCommentToken;
          TempContainer := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            TypeArgs(TempContainer);
            For iToken := 0 To TempContainer.TokenCount -1 Do
              strUnit := strUnit + TempContainer.Tokens[iToken].Token;
          Finally
            TempContainer.Free;
          End;
          While Token.Token = '.' Do
            Begin
              strUnit := strUnit + Token.Token;
              NextNonCommentToken;
              If IsIdentifier(Token) Then
                Begin
                  strUnit := strUnit + Token.Token;
                  NextNonCommentToken;
                End
              Else
                ErrorAndSeekToken(strIdentExpected, 'IdentList', Token.Token,
                  strSeekableOnErrorTokens, stFirst);
            End;
          I := Nil;
          If Container <> Nil Then
            I := Container.Add(TIdentList.Create(strUnit, scNone, iLine,
              iColumn, iImageIndex, AComment)) As TIdentList;
          If Token.UToken = 'IN' Then
            Begin
              If I <> Nil Then
                I.AddToken(Token.Token);
              NextNonCommentToken;
              If Token.TokenType <> ttSingleLiteral Then
                ErrorAndSeekToken(strStringExpected, 'IdentList', Token.Token,
                  SeekTokens, stActual)
              Else
                Begin
                  If I <> Nil Then
                    I.AddToken(Token.Token);
                  NextNonCommentToken;
                End;
            End;
        End Else
          ErrorAndSeekToken(strIdentExpected, 'IdentList', Token.Token,
            strSeekableOnErrorTokens, stFirst);
    Until Not IsToken(',', Nil);
End;

(**

  This method returns a type id at the current token position using the
  following object pascal grammar.

  @precon  C must be a valid generic container.
  @postcon Returns a type id as a string of text.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.TypeId(Container: TElementContainer) : Boolean;

Begin
  Result := IsIdentifier(Token) Or (Token.UToken = 'STRING');
  If Result Then
    Begin
      AddToExpression(Container);
      If Token.Token = '.' Then
        Begin
          AddToExpression(Container);
          If IsIdentifier(Token) Then
            AddToExpression(Container)
          Else
            ErrorAndSeekToken(strIdentExpected, 'TypeId', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End;
    End;
End;

(**

  This method parses a parameter declaration for a generic type definition.

  @precon  None.
  @postcon A generic type parameter definition is parsed.

  @param   strIdentifier as a String as a reference

**)
procedure TPascalModule.TypeParamDecl(var strIdentifier : String);

Begin
  TypeParamList(strIdentifier);
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      ConstraintList;
    End;
End;

(**

  This method parses the TypeParamDeclList element of the grammar.

  @precon  None.
  @postcon The type param declaration list is parses and the identifier modified.

  @param   strIdentifier as a String as a reference

**)
Procedure TPascalModule.TypeParamDeclList(var strIdentifier : String);

Begin
  Repeat
    NextNonCommentToken;
    TypeParamDecl(strIdentifier);
    If Token.Token = ';' Then
      strIdentifier := strIdentifier + ', '; // This is deliberate
  Until Token.Token <> ';';
End;

(**

  This method parses the TypeParamList element of the generics grammar.

  @precon  None.
  @postcon the updated identifier is returned.

  @param   strIdentifier as a String as a reference

**)
Procedure TPascalModule.TypeParamList(var strIdentifier : String);

Var
  boolComma: Boolean;
  iParams : Integer;

Begin
  iParams := 0;
  Repeat
    RTTIAttributes;
    If IsKeyWord(Token.Token, ['+', '-']) Then
      RTTIAttributes;
    If IsIdentifier(Token) Then
      Begin
        If iParams > 0 Then
          strIdentifier := strIdentifier + ', ';
        strIdentifier := strIdentifier + Token.Token;
        //: @debug QQQQQ
        ReferenceSymbol(Token);
        NextNonCommentToken;
        Inc(iParams);
      End Else
        ErrorAndSeekToken(strIdentExpected, 'TypeParamList', Token.Token,
          strSeekableOnErrorTokens, stActual);
    boolComma := Token.Token = ',';
    If boolComma Then
      NextNonCommentToken;
  Until Not boolComma;
End;

(**

  This method parses the TypeParam element of the grammar.

  @precon  None.
  @postcon The updated identifier is returned if it contains a generic definition.

  @param   strIdentifier as a String as a reference

**)
Procedure TPascalModule.TypeParams(var strIdentifier : String);

Begin
  If Token.Token = '<' Then
    Begin
      strIdentifier := strIdentifier + Token.Token;
      TypeParamDeclList(strIdentifier);
      If Token.Token = '>' Then
        Begin
          strIdentifier := strIdentifier + Token.Token;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'TypeParams', '>',
            strSeekableOnErrorTokens, stActual);
    End;
End;

(**

  This method parses a constant expression from the current token position
  using the following object pascal grammar.

  @precon  C is a generic container to add tokens too.
  @postcon Returns true if a constant expression was parsed.

  @param   Container as a TElementContainer
  @param   ExprType  as a TPascalExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.ConstExpr(Container : TElementContainer;
  var ExprType : TPascalExprTypes) : Boolean;

Var
  iStartIndex : Integer;

Begin
  Result := True;
  iStartIndex := Token.BufferPos;
  Expression(Container, ExprType); // ConstExpr is a subset of Expression
  If iStartIndex = Token.BufferPos Then
    ErrorAndSeekToken(strConstExprExpected, 'ConstExpr', Token.Token,
      strSeekableOnErrorTokens, stActual);
End;

(**

  This method processes a compiler directive looking for conditional statements.

  @precon  None.
  @postcon Processes a compiler directive looking for conditional statements.

  @param   iSkip as an Integer as a reference

**)
procedure TPascalModule.ProcessCompilerDirective(var iSkip : Integer);

  (**

    This function returns the definition string from the current compiler
    directive.

    @precon  None.
    @postcon Returns the definition as a string.

    @return  a String

  **)
  Function GetDef : String;

  Var
    iPos : Integer;
    strToken : String;

  Begin
    strToken := Token.Token;
    iPos := Pos(#32, strToken);
    Result := Trim(Copy(strToken, iPos + 1, Length(strToken) - iPos - 1));
  End;

  (**

    This function checks to see if the string of text starts with the passed
    start string.

    @precon  None.
    @postcon Returns true if the string starts match.

    @param   strText  as a String
    @param   strStart as a String
    @return  a Boolean

  **)
  Function Like(strText, strStart : String) : Boolean;

  Begin
    Result := False;
    If Length(strText) >= Length(strStart) Then
      Result := CompareText(Copy(strText, 1, Length(strStart)), strStart) = 0;
  End;

  (**

    This method adds the number to the stack and increments the iSkip variable by the
    value passed.

    @precon  None.
    @postcon Adds the number to the stack and increments the iSkip variable by the value
             passed.

    @param   iCompilerCondition as a TCompilerCondition

  **)
  Procedure IncSkip(iCompilerCondition : TCompilerCondition);

  Begin
    CompilerConditionStack.Push(iCompilerCondition, TokenIndex);
    If iCompilerCondition = ccIncludeCode Then
      Inc(iSkip);
  End;

  (**

    This function removes the number from the stack and decrements the iSkip
    variable by 1. Note this also added the removed value to the UNDO stack.

    @precon  None.
    @postcon Removes the number from the stack and decrements the iSkip
             variable by 1.

    @return  a Boolean

  **)
  Function DecSkip : Boolean;

  Var
    CompilerCondition: TCompilerConditionData;

  Begin
    Result := False;
    If CompilerConditionStack.CanPop Then
      Begin
        CompilerCondition := CompilerConditionStack.Peek;
        CompilerConditionUndoStack.Push(CompilerCondition);
        If CompilerCondition.CompilerCondition = ccIncludeCode Then
          Dec(iSkip);
        CompilerConditionStack.Pop;
      End Else
        Result := True;
  End;

Var
  CompilerCondition : TCompilerConditionData;

begin
  If Like(Token.Token, '{$DEFINE ') Then
    AddDef(GetDef)
  Else If Like(Token.Token, '{$UNDEF ') Then
    DeleteDef(GetDef)
  Else If Like(Token.Token, '{$IFDEF ') Then
    Begin
      If Not IfDef(GetDef) Then
        IncSkip(ccIncludeCode)
      Else
        IncSkip(ccExcludeCode);
    End
  Else If Like(Token.Token, '{$IFOPT ') Then
    Begin
      If Not IfDef(GetDef) Then
        IncSkip(ccIncludeCode)
      Else
        IncSkip(ccExcludeCode);
    End
  Else If Like(Token.Token, '{$IF ') Then
    IncSkip(ccExcludeCode) // FAKE $IF by defaulting to TRUE
  Else If Like(Token.Token, '{$IFNDEF ') Then
    Begin
      If Not IfNotDef(GetDef) Then
        IncSkip(ccIncludeCode)
      Else
        IncSkip(ccExcludeCode);
    End
  Else If Like(Token.Token, '{$ELSE') Then
    Begin
      If CompilerConditionStack.CanPop Then
        Begin
          CompilerCondition := CompilerConditionStack.Peek;
          //: @debug Should the else not add to the stack rather than manipulate it?
          If CompilerCondition.CompilerCondition = ccIncludeCode Then
            Begin
              CompilerConditionStack.Poke(ccExcludeCode, CompilerCondition.TokenIndex);
              Dec(iSkip);
            End Else
            Begin
              CompilerConditionStack.Poke(ccIncludeCode, CompilerCondition.TokenIndex);
              Inc(iSkip);
            End;
        End Else
          AddIssue(Format(strElseIfMissingIfDef, [Token.Line, Token.Column]),
              scGlobal, 'ProcessCompilerDirective', Token.Line, Token.Column,
              etError);
    End
  Else If Like(Token.Token, '{$ENDIF') Then
    Begin
      If DecSkip Then
        AddIssue(Format(strEndIfMissingIfDef, [Token.Line, Token.Column]),
            scGlobal, 'ProcessCompilerDirective', Token.Line, Token.Column, etError);
    End
  Else If Like(Token.Token, '{$IFEND') Then
    Begin
      If DecSkip Then
        AddIssue(Format(strEndIfMissingIfDef, [Token.Line, Token.Column]),
            scGlobal, 'ProcessCompilerDirective', Token.Line, Token.Column, etError);
    End
  Else If Like(Token.Token, '{$INCLUDE') Then
    ProcessIncludeDirective(Token.Token)
  Else If Like(Token.Token, '{$EXTERNALSYM') Then
    FExternalSyms.Add(GetDef);
  If iSkip < 0 Then
    iSkip := 0;
end;

(**

  This method processes an INCLUDE directive and injects the INC file code into the parser
  after the current location.

  @precon  None.
  @postcon The parser tokens from the include file are inserted after the current token.

  @param   strToken as a String

**)
Procedure TPascalModule.ProcessIncludeDirective(strToken: String);

Var
  strFileName : String;
  sl : TStringList;
  M : TPascalModule;
  iToken: Integer;
  T : TTokenInfo;

Begin
  strFileName := strToken;
  Delete(strToken, 1, Length('{$INCLUDE'));
  strFileName := Trim(strToken);
  If (Length(strFileName) > 0) And (strFileName[Length(strFileName)] = '}') Then
    strFileName := Copy(strFileName, 1, Length(strFileName) - 1);
  If (Length(strFileName) > 0) And (strFileName[1] = '''') Then
    strFileName := Copy(strFileName, 2, Length(strFileName) - 2);
  If Pos(':', strFileName) = 0 Then
    strFileName := ExpandFileName(ExtractFilePath(FileName) + strFileName);
  If Not FileExists(strFileName) Then
    AddIssue(Format('The INCLUDE file "%s" does not exist!', [strFileName]), scNone,
      'ProcessIncludeDirective', Token.Line, Token.Column, etError)
  Else
    Begin
      sl := TStringList.Create;
      Try
        sl.LoadFromFile(strFileName);
        M := TPascalModule.CreateParser(sl.Text, strFileName, False, []);
        Try
          for iToken := M.TokenCount - 1 downto 0 Do
            Begin
              T := M.Tokens[iToken];
              InsertToken(T.Token, Succ(TokenIndex), T.TokenType);
            End;
        Finally
          M.Free;
        End;
      Finally
        sl.Free;
      End;
    End;
End;

(**


  This method find unresolved implemented methods, both within objects and
  classes and simple procedures and function and outputs an error if they are
  unresolved.

  @precon  None.
  @postcon Find unresolved implemented methods, both within objects and
           classes and simple procedures and function and outputs an error if
           they are unresolved.


  @param   StartLabel as a TLabelContainer

**)
procedure TPascalModule.FindUnresolvedImplementedClassMethods(
  StartLabel : TLabelContainer);

var
  Method: TPascalMethod;
  ClassLabel: TLabelContainer;
  k: Integer;

begin
  If StartLabel <> Nil Then
    For k := 1 To StartLabel.ElementCount Do
      Begin
        If StartLabel.Elements[k] Is TPascalMethod Then
          Begin
            Method := StartLabel.Elements[k] As TPascalMethod;
            If (Method.RecObjClsInt <> Nil) And Not Method.Resolved Then
              AddIssue(Format(strUndeclaredClassMethod, [Method.QualifiedName]),
                  scNone, 'FindUnresolvedImplementedClassMethods', Method.Line,
                  Method.Column, etWarning);
          End Else
          Begin
            ClassLabel := StartLabel.Elements[k] as TLabelContainer;
            FindUnresolvedImplementedClassMethods(ClassLabel);
          End;
      End;
end;

(**

  This method tries to find the class that the class names string list refers to.

  @precon  slClassNames must be a valid instance of a string list class.
  @postcon Returns the class reference the string list refers to else returns nil;

  @param   slClassNames as a TStringList
  @return  a TRecordDecl

**)
Function TPascalModule.FindRecObjClsInt(slClassNames: TStringList): TRecordDecl;

Var
  i: Integer;
  E: TElementContainer;

Begin
  Result := Nil;
  E := Self;
  If E <> Nil Then
    For i := 0 To slClassNames.Count -  1 Do
      Begin
        E := E.FindElement(strTypesLabel);
        If E <> Nil Then
          E := E.FindElement(slClassNames[i]);
        If E = Nil Then
          Break;
      End;
  If E <> Nil Then
    If E Is TRecordDecl Then
      Result := E As TRecordDecl;
End;

(**


  This method find unresolved exported headings and outputs them as errors.

  @precon  None.
  @postcon Find unresolved exported headings and outputs them as errors.


**)
procedure TPascalModule.FindUnresolvedExportedMethods;

var
  Method: TPascalMethod;
  k: Integer;
  iIndex : Integer;

begin
  If FExportedHeadingsLabel <> Nil Then
    For k := 1 To FExportedHeadingsLabel.ElementCount Do
      If FExportedHeadingsLabel.Elements[k] Is TPascalMethod Then
        Begin
          Method := FExportedHeadingsLabel.Elements[k] As TPascalMethod;
          If Not Method.Resolved And Not FExternalSyms.Find(Method.Identifier, iIndex) Then
            AddIssue(Format(strUnSatisfiedForwardReference, [Method.Identifier]),
              scNone, 'FindUnresolvedExportedMethods', Method.Line, Method.Column,
              etWarning);
          End;
end;

{procedure TPascalModule.FindUnresolvedExportsMethods;

var
  Method: TExportsItem;
  k: Integer;
  iIndex : Integer;

begin
  If FExportsHeadingsLabel <> Nil Then
    For k := 1 To FExportsHeadingsLabel.ElementCount Do
      If FExportsHeadingsLabel.Elements[k] Is TExportsItem Then
        Begin
          Method := FExportsHeadingsLabel.Elements[k] As TExportsItem;
          If Not Method.Resolved And Not FExternalSyms.Find(Method.Identifier, iIndex) Then
            AddIssue(Format(strUnSatisfiedForwardReference, [Method.Identifier]),
              scNone, 'FindUnresolvedExportedMethods', Method.Line, Method.Column, etError);
          End;
end;}

(**


  This method finds all the unresolved object and class methods in a recursive
  manner to capture any private classes of classes.

  @precon  None.
  @postcon Finds all the unresolved object and class methods in a recursive
           manner to capture any private classes of classes.


  @param   TypeLabel as a TLabelContainer

**)
procedure TPascalModule.FindUnresolvedRecordObjectAndClassMethods(TypeLabel : TLabelContainer);

  (**


    This function walks backwards through the heirarchy to find all the
    qualifying objects and classes.

    @precon  None.
    @postcon Walks backwards through the heirarchy to find all the
             qualifying objects and classes.


    @param   RecObjOrCls as a TRecordDecl
    @return  a String

  **)
  Function GetClassQualification(RecObjOrCls : TRecordDecl) : String;

  Var
    P : TElementContainer;

  Begin
    Result := '';
    P := RecObjOrCls;
    While P <> Nil Do
      Begin
        If P Is TRecordDecl Then
          Result := P.Identifier + '.' + Result;
        P := P.Parent;
      End;
  End;

  (**

    This method determines if the method should have an implementation.

    @precon  Method must be a valid TPascalMethod instance.
    @postcon Determines if the method should have an implementation.

    @param   Method as a TPascalMethod
    @return  a Boolean

  **)
  Function ShouldMethodHaveImplementation(Method : TPascalMethod) : Boolean;

  Begin
    Result := Not Method.Resolved;
    Result := Result And Not Method.HasDirective('abstract');
    Result := Result And (Method.Alias = '');
  End;

var
  Method: TPascalMethod;
  j: Integer;
  MethodsLabel: TElementContainer;
  RecordObjectOrClass: TRecordDecl;
  k: Integer;
  ClassTypeLabel: TLabelContainer;

begin
  If TypeLabel <> Nil Then
    For k := 1 To TypeLabel.ElementCount Do
      Begin
        If (TypeLabel.Elements[k] Is TRecordDecl) And Not
          (TypeLabel.Elements[k] Is TInterfaceDecl) Then
          Begin
            RecordObjectOrClass := TypeLabel.Elements[k] As TRecordDecl;
            MethodsLabel := RecordObjectOrClass.FindElement(strMethodsLabel);
            If MethodsLabel <> Nil Then
              For j := 1 To MethodsLabel.ElementCount Do
                If MethodsLabel.Elements[j] Is TPascalMethod Then
                  Begin
                    Method := MethodsLabel.Elements[j] As TPascalMethod;
                    If ShouldMethodHaveImplementation(Method) Then
                      AddIssue(Format(strUnSatisfiedForwardReference,
                        [GetClassQualification(RecordObjectOrClass) + Method.Identifier]),
                        scNone, 'FindUnresolvedRecordObjectAndClassMethods', Method.Line,
                        Method.Column, etWarning);
                  End;
            ClassTypeLabel := RecordObjectOrClass.FindElement(strTypesLabel) As TLabelContainer;
            If ClassTypeLabel <> Nil Then
              FindUnresolvedRecordObjectAndClassMethods(ClassTypeLabel);
          End;
      end;
end;

(**


  This method Resolved the scope of implemented exported headings.

  @precon  None.
  @postcon Resolved the scope of implemented exported headings.


**)
procedure TPascalModule.ResolveScopeOfImplementedExportedMethods;

var
  ImplementedMethod: TElementContainer;
  Method: TPascalMethod;
  k: Integer;

begin
  If (FExportedHeadingsLabel <> Nil) And (FImplementedMethodsLabel <> Nil) Then
    For k := 1 To FExportedHeadingsLabel.ElementCount Do
      If FExportedHeadingsLabel.Elements[k] Is TPascalMethod Then
        Begin
          Method := FExportedHeadingsLabel.Elements[k] As TPascalMethod;
          ImplementedMethod := FImplementedMethodsLabel.FindElement(Method.Name);
          If (ImplementedMethod <> Nil) And (ImplementedMethod Is TPascalMethod) Then
            Begin
              Method.Resolved := True;
              (ImplementedMethod As TPascalMethod).Resolved := True;
              (ImplementedMethod As TPascalMethod).Scope := Method.Scope;
            End;
        End;
end;

(**

  This method resolved the references between the exports methods and
  implemented methods marking each as resovled where a match is found.

  @precon  None.
  @postcon Resolved the references between the exports methods and
           implemented methods marking each as resovled where a match is found.

**)
procedure TPascalModule.ResolveScopeOfImplementedExportsMethods;

var
  ImplementedMethod: TElementContainer;
  Method: TExportsItem;
  k: Integer;

begin
  If (FExportsHeadingsLabel <> Nil) And (FImplementedMethodsLabel <> Nil) Then
    For k := 1 To FExportsHeadingsLabel.ElementCount Do
      If FExportsHeadingsLabel.Elements[k] Is TExportsItem Then
        Begin
          Method := FExportsHeadingsLabel.Elements[k] As TExportsItem;
          ImplementedMethod := FImplementedMethodsLabel.FindElement(
            Method.Identifier, ftIdentifier);
          If (ImplementedMethod <> Nil) And (ImplementedMethod Is TPascalMethod) Then
            Begin
              Method.Resolved := True;
              (ImplementedMethod As TPascalMethod).Resolved := True;
              (ImplementedMethod As TPascalMethod).Scope := Method.Scope;
            End;
        End;
end;

(**


  This method searches the types tree for the declarations of the methods found
  in the implemented methods element and marks elements as resolved.

  @precon  None.
  @postcon Searches the types tree for the declarations of the methods found
           in the implemented methods element and marks elements as resolved.


  @param   StartLabel as a TLabelContainer

**)
procedure TPascalModule.ResolveScopeOfImplementedMethods(
  StartLabel : TLabelContainer);

var
  Element: TElementContainer;
  Method: TPascalMethod;
  i: Integer;

begin
  If StartLabel <> Nil Then
    For i := 1 To StartLabel.ElementCount Do
      If StartLabel.Elements[i] Is TPascalMethod Then
        Begin
          Method := StartLabel.Elements[i] As TPascalMethod;
          If Method.RecObjClsInt <> Nil Then
            Begin
              Element := Method.RecObjClsInt.FindElement(strMethodsLabel);
              If Element <> Nil Then
                Begin
                  Element := Element.FindElement(Method.Name);
                  If Element Is TPascalMethod Then
                    Begin
                      Method.Scope := Element.Scope;
                      Method.Resolved := True;
                      Method.Referenced := Element.Referenced;
                      Element.Referenced := True;
                      (Element As TPascalMethod).Resolved := True;
                    End;
                End;
            End Else
              If Pos('.', Method.QualifiedName) > 0 Then
                AddIssue(Format(strUnSatisfiedDeclaration,
                  [Method.QualifiedName]),
                  scNone, 'ResolveScopeOfImplementedMethods', Method.Line,
                  Method.Column, etWarning);
        End Else
          ResolveScopeOfImplementedMethods(
            StartLabel.Elements[i] As TLabelContainer);
end;

(**

  This method determines if the given token is a valid identifier.

  @precon  Token must be a valid instance.
  @postcon Returns true if the given token is a valid Object Pascal identifier.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function  TPascalModule.IsIdentifier(AToken : TTokenInfo) : Boolean;

Begin
  Result := (AToken.TokenType In [ttIdentifier, ttDirective]) Or
    ((AToken.TokenType In [ttReservedWord]) And
      IsKeyWord(AToken.Token, strIdentifierReservedWords));
End;

(** Register the file source code extensions that can be parsed by this module. **)
Initialization
  ModuleDispatcher.Add('.dpk', TPascalModule, True, ctPascalBlock, ctPascalBlock,
    ctPascalBlock);
  ModuleDispatcher.Add('.dpr', TPascalModule, True, ctPascalBlock, ctPascalBlock,
    ctPascalBlock);
  ModuleDispatcher.Add('.pas', TPascalModule, True, ctPascalBlock, ctPascalBlock,
    ctPascalBlock);
End.
