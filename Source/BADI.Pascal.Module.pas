(**

  ObjectPascalModule : A unit to tokenize Pascal source code.

  @Version    2.0
  @Date       03 Jan 2018
  @Author     David Hoyle

  @todo       Implement an expression parser for the above compiler defines.
              Needs to be able to evaluate constants in the code and use the
              two functions Defined() and Declared().
  @todo       There are some metrics and checks whihc are overridden.
  @nometric   UnsortedModule Toxicity

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
  BADI.TokenInfo,
  BADI.Pascal.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
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
    procedure UsesClause(Const eScope : TScope);
    Function  PortabilityDirective : Boolean;
    Procedure InterfaceSection;
    Procedure InterfaceDecl;
    Function  ExportedHeading(Const Container : TElementContainer) : Boolean;
    Procedure ImplementationSection;
    Procedure Block(Const AScope : TScope; Const Method : TPascalMethod);
    Function  ExportsStmt : Boolean;
    procedure ExportsItem(Const Container : TElementContainer);
    Procedure DeclSection(Const AScope : TScope; Const Container : TElementContainer);
    Function  LabelDeclSection(Const Container : TElementContainer) : Boolean;
    Function  ConstSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ConstantDecl(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ResStringSection(Const AScope: TScope): Boolean;
    Function  ResourceStringDecl(Const AScope: TScope; Const Container : TElementContainer): Boolean;
    Function  TypeSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  TypeDecl(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  GetTypeDecl(Const AToken : TTypeToken) : TTypes;
    Function  TypedConstant(Const C: TElementContainer; Const T : TGenericTypeDecl) : Boolean;
    Function  ArrayConstant(Const C: TElementContainer; Const T : TGenericTypeDecl) : Boolean;
    Function  RecordConstant(Const C: TElementContainer; Const T : TGenericTypeDecl) : Boolean;
    Function  RecordFieldConstant(Const C : TElementContainer; Const T : TGenericTypeDecl) : Boolean;
    function  OPType(Const AToken : TTypeToken) : TTypes;
    function  RestrictedType(Const AToken : TTypeToken) : TRestrictedType;
    Function  ClassRefType(Const AToken : TTypeToken) : TClassRefType;
    Function  SimpleType(Const AToken : TTypeToken) : TSimpleType;
    function  RealType(Const AToken : TTypeToken) : TRealType;
    function  OrdinalType(Const AToken : TTypeToken) : TOrdinalType;
    function  OrdIdent(Const AToken : TTypeToken) : TOrdIdent;
    Function  VariantType(Const AToken : TTypeToken) : TVariantType;
    function  SubRangeType(Const AToken : TTypeToken) : TSubRangeType;
    function  EnumerateType(Const AToken : TTypeToken) : TEnumerateType;
    Procedure EnumerateElement(Const EnumerateType : TEnumerateType);
    Function  StringType(Const AToken : TTypeToken) : TStringType;
    Function  StrucType(Const AToken : TTypeToken) : TTypes;
    function  ArrayType(Const boolPacked : Boolean; Const AToken : TTypeToken): TArrayType;
    Function  RecType(Const boolPacked : Boolean; Const AToken : TTypeToken) : TRecordDecl;
    Procedure RecordVisibility(Var AScope : TScope);
    Function  RecordTypeSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  RecordConstSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  RecordVarSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  RecordClassVarSection(Const AScope : TScope; Const Cls : TRecordDecl) : Boolean;
    Function  RecordMethodList(Const Cls : TRecordDecl; Const AScope : TScope;
      Const PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  RecordPropertyList(Const Cls : TRecordDecl; Var AScope : TScope) : Boolean;
    Function  RecordFieldList(Const Rec : TRecordDecl; Const InternalScope : TScope) : Boolean;
    Function  FieldDecl(Const Rec: TRecordDecl; Const InteralScope : TScope) : Boolean;
    procedure RecVariant(Const Rec: TRecordDecl; Const InternalScope : TScope);
    function  SetType(Const boolPacked: Boolean; Const AToken : TTypeToken): TSetType;
    function  FileType(Const boolPacked: Boolean; Const AToken : TTypeToken): TFileType;
    Function  RecordVariantSection(Const Rec: TRecordDecl; Const InternalScope : TScope) : Boolean;
    Function  PointerType(Const AToken : TTypeToken) : TPointerType;
    Function  ProcedureType(Const AToken : TTypeToken) : TProcedureType;
    Function  AnonymousReferenceType(Const AToken : TTypeToken) : TAnonymousReferenceType;
    Function  VarSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ThreadVarSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  VarDecl(Const AScope : TScope; Const VarSection : TElementContainer;
      Const AImageIndex : TBADIImageIndex) : Boolean;
    Function  ThreadVarDecl(Const AScope : TScope; Const VarSection : TElementContainer) : Boolean;
    Procedure Expression(Const C : TElementContainer; Var ExprType : TPascalExprTypes);
    Procedure SimpleExpression(Const C : TElementContainer; Var ExprType : TPascalExprTypes);
    Procedure Term(Const C : TElementContainer; Var ExprType : TPascalExprTypes);
    Procedure Factor(Const Container : TElementContainer; Var ExprType : TPascalExprTypes);
    Function  RelOp(Const C : TElementContainer; Const ExprType : TPascalExprTypes) : Boolean;
    Function  AddOp(Const C : TElementContainer) : Boolean;
    Function  MulOp(Const C : TElementContainer; Var ExprType : TPascalExprTypes) : Boolean;
    Function  Designator(Const C : TElementContainer; Var ExprType : TPascalExprTypes) : Boolean;
    Procedure DesignatorSubElement(Const C : TElementContainer; Var ExprType : TPascalExprTypes;
      Const strValidSymbols : Array of String);
    Function  SetConstructor(Const C : TElementContainer) : Boolean;
    Procedure SetElement(Const C : TElementContainer);
    Procedure ExprList(Const C : TElementContainer);
    Procedure MethodExprList(Const C : TElementContainer);
    Procedure Statement;
    Function  StmtList : Integer;
    Procedure SimpleStatement;
    Function  StructStmt : Boolean;
    Function  CompoundStmt(Const Method : TGenericFunction) : Boolean;
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
    Function  ProcedureDeclSection(Const AScope : TScope) : Boolean;
    Function  ProcedureDecl(Const AScope : TScope) : TPascalMethod;
    Function  FunctionDecl(Const AScope : TScope) : TPascalMethod;
    Function  ConstructorDecl(Const AScope : TScope) : TPascalMethod;
    Function  DestructorDecl(Const AScope : TScope) : TPascalMethod;
    Function  OperatorDecl(Const AScope : TScope) : TPascalMethod;
    Function  FunctionHeading(Const AScope :TScope; Const Container : TElementContainer;
      Const boolIdent : Boolean = True) : TPascalMethod;
    Function  ProcedureHeading(Const AScope : TScope; Const Container : TElementContainer;
      Const boolIdent : Boolean = True) : TPascalMethod;
    Function  OperatorHeading(Const AScope :TScope; Const Container : TElementContainer;
      Const boolIdent : Boolean = True) : TPascalMethod;
    Procedure FormalParameter(Const Method : TPascalMethod);
    Procedure FormalParam(Const Method : TPascalMethod);
    Procedure Parameter(Const Method : TPascalMethod; Const ParamMod : TParamModifier);
    Procedure Directive(Const M : TPascalMethod; Const boolGrammarFix : Boolean = False);
    Function  ObjectType(Const AToken : TTypeToken) : TObjectDecl;
    Procedure ObjHeritage(Const ObjDecl : TObjectDecl);
    Procedure ObjVisibility(Var AScope : TScope);
    Function  ObjTypeSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ObjConstSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ObjVarSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ObjClassVarSection(Const AScope : TScope; Const Cls : TRecordDecl) : Boolean;
    Function  ObjMethodList(Const Cls : TRecordDecl; Const AScope : TScope;
      Const PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  ObjPropertyList(Const Cls : TRecordDecl; Var AScope : TScope) : Boolean;
    Function  ObjFieldList(Const Rec : TRecordDecl; Const InternalScope : TScope) : Boolean;
    Function  MethodList(Const Cls : TRecordDecl; Const AScope : TScope;
      Const PermissibleMethods : TPermissibleMethods) : Boolean;
    function  MethodHeading(Const Cls: TRecordDecl; Const AScope: TScope;
      Const PermissibleMethods : TPermissibleMethods): Boolean;
    Function  ConstructorHeading(Const AScope :TScope; Const Container : TElementContainer) : TPascalMethod;
    Function  DestructorHeading(Const AScope :TScope; Const Container : TElementContainer) : TPascalMethod;
    Procedure InitSection;
    Function  ClassType(Const AToken : TTypeToken) : TClassDecl;
    Procedure RecObjClsIntHeritage(Const RecObjClsInt : TRecordDecl);
    procedure ClassVisibility(Var AScope : TScope);
    Function  ClassTypeSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ClassConstSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ClassVarSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;
    Function  ClassClassVarSection(Const AScope : TScope; Const Cls : TRecordDecl) : Boolean;
    Function  ClassMethodList(Const Cls : TRecordDecl; Const AScope : TScope;
      Const PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  ClassPropertyList(Const Cls : TRecordDecl; Var AScope : TScope) : Boolean;
    Function  ClassFieldList(Const Cls : TObjectDecl; Const AScope : TScope) : Boolean;
    Function  FieldList(Const Cls : TObjectDecl; Const AScope : TScope) : Boolean;
    Function  PropertyList(Const Cls : TRecordDecl; Var AScope : TScope) : Boolean;
    Procedure PropertyInterface(Const Prop : TPascalProperty);
    Procedure PropertyParameterList(Const Prop : TPascalProperty);
    Procedure PropertySpecifiers(Const Prop : TPascalProperty);
    Function  InterfaceType(Const AToken : TTypeToken) : TInterfaceDecl;
    Procedure InterfaceHeritage(Const InterfaceDecl : TInterfaceDecl);
    Function  InterfaceMethodList(Const Cls : TRecordDecl; Const AScope : TScope;
      Const PermissibleMethods : TPermissibleMethods) : Boolean;
    Function  InterfacePropertyList(Const Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Procedure RequiresClause;
    procedure ContainsClause;
    Procedure IdentList(Const Container : TElementContainer;  Const eScope : TScope;
      Const ContainerClass : TElementContainerClass; Const SeekTokens : Array Of String;
      Const iImageIndex : TBADIImageIndex = iiNone);
    Function  TypeId(Const Container: TElementContainer) : Boolean;
    Function  ConstExpr(Const Container: TElementContainer; Var ExprType : TPascalExprTypes) : Boolean;
    Procedure RTTIAttributes;
    Procedure AttributeDeclaration;
    Procedure TypeParams(Var strIdentifier : String);
    Procedure TypeParamDeclList(Var strIdentifier : String);
    Procedure TypeParamDecl(Var strIdentifier : String);
    Procedure TypeParamList(Var strIdentifier : String);
    Procedure ConstraintList;
    Procedure Constraint;
    Procedure FormalTypeParamList(Var strIdentifier : String);
    Function  MethodQualifiers(Const AScope :TScope; Const Container : TElementContainer;
      Const iMethodType : TMethodType; Const boolClassMethod : Boolean; Const C : TComment;
      Const boolIdent : Boolean = True) : TPascalMethod;
    Procedure TypeArgs(Const Container : TElementContainer);
    Function  AnonymousMethod(Const Container : TElementContainer) : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    procedure ArrayElement(Const C : TElementContainer; Const iStartDimension: Integer;
      Const AT : TArrayType);
    Function  CheckReturnValue(Const Method : TGenericFunction) : Boolean;
    Procedure CheckAlias(Const Method : TPascalMethod);
    Function  CheckNumberType(Const ExprType : TPascalExprTypes) : Boolean;
    Procedure UpdateTypeToken(var AToken : TTypeToken); {$IFDEF D2005} InLine; {$ENDIF}
    Procedure AddToContainer(Const Container : TElementContainer; Var Method : TPascalMethod);
    Procedure TidyUpEmptyElements;
    Procedure CheckUnResolvedMethods;
    procedure ResolveScopeOfImplementedMethods(Const StartLabel : TLabelContainer);
    procedure ResolveScopeOfImplementedExportedMethods;
    procedure ResolveScopeOfImplementedExportsMethods;
    procedure ResolveForwardImplementedMethods;
    procedure FindUnresolvedRecordObjectAndClassMethods(Const TypeLabel : TLabelContainer);
    procedure FindUnresolvedExportedMethods;
    {procedure FindUnresolvedExportsMethods;}
    procedure FindUnresolvedImplementedClassMethods(Const StartLabel : TLabelContainer);
    Function  FindRecObjClsInt(Const slClassNames : TStringList) : TRecordDecl;
    Function  IsIdentifier(Const AToken : TTokenInfo) : Boolean;
    Procedure ProcessIncludeDirective(Const strToken : String);
    Procedure MetricsUnsortedMethods;
    Procedure MetricsHardCodedStrings(Const Container : TElementContainer);
    Procedure MetricsHardCodedNumbers(Const Container : TElementContainer);
    Procedure MetricsLongOrEmptyMethods(Const Method : TGenericFunction);
    Procedure MetricsNestedIFDepth(Const Method : TGenericFunction);
    Procedure MetricsLongParameterList(Const Method : TGenericFunction);
    Procedure MetricMissingConstInParamList(Const Method : TGenericFunction);
    Procedure MetricsExceptionEating(Const Container : TElementContainer);
    Procedure MetricsEmptyBlockAtToken(Const eCheck : TBADIModuleCheck);
    Procedure MetricsCyclometricComplexity(Const Method : TGenericFunction);
    Procedure MetricsMethodToxicity(Const Method : TGenericFunction);
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function  GetCurrentMethod: TPascalMethod;
    Function  GetModuleName : String; Override;
    Function  GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Procedure CheckFunctionReturn(Const Func : TPascalMethod);
    (**
      This property returns the method on top of the method stack.
      @precon  None.
      @postcon Returns the method on top of the method stack else returns nil.
      @return  a TPascalMethod
    **)
    Property CurrentMethod : TPascalMethod Read GetCurrentMethod;
  Public
    Constructor CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
      Const ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(Var iSkip : Integer); Override;
    Function ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Class Function DefaultProfilingTemplate : String; Override;
    (**
      Returns the type of the modules, Program, Unit, Package, etc.
      @precon  None.
      @postcon Returns the type of the modules, Program, Unit, Package, etc.
      @return  a TModuleType
    **)
    Property ModuleType : TModuleType Read FModuleType Write FModuleType;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  {$IFDEF PROFILECODE}
  Profiler,
  {$ENDIF}
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
  BADI.Pascal.ResourceStringDecl,
  BADI.Pascal.IdentList,
  BADI.Pascal.FieldDecl,
  BADI.Pascal.InitializationDecl,
  BADI.Pascal.FinalizationDecl,
  BADI.Pascal.VariableDecl,
  BADI.Pascal.ParameterDecl,
  BADI.Pascal.PropertySpec,
  BADI.Pascal.DispInterfaceDecl,
  BADI.Pascal.ThreadVariableDecl,
  BADI.Pascal.UsesList,
  BADI.Generic.Parameter,
  Generics.Collections,
  System.Character;

Const
  (** Constant for the keyword ABSTRACT. **)
  strABSTRACT = 'ABSTRACT';
  (** Constant for the keyword ARRAY. **)
  strARRAY = 'ARRAY';
  (** Constant for the keyword CASE. **)
  strCASE = 'CASE';
  (** Constant for the keyword CLASS. **)
  strCLASS = 'CLASS';
  (** Constant for the keyword CONST. **)
  strCONST = 'CONST';
  (** Constant for the keyword DISPID. **)
  strDISPID = 'DISPID';
  (** Constant for the keyword DO. **)
  strDO = 'DO';
  (** Constant for the keyword ELSE. **)
  strELSE = 'ELSE';
  (** Constant for the keyword END. **)
  strEND = 'END';
  (** Constant for the keyword FOR. **)
  strFOR = 'FOR';
  (** Constant for the keyword HELPER. **)
  strHELPER = 'HELPER';
  (** Constant for the keyword IN. **)
  strIN = 'IN';
  (** Constant for the keyword INDEX. **)
  strINDEX = 'INDEX';
  (** Constant for the keyword INHERITED. **)
  strINHERITED = 'INHERITED';
  (** Constant for the keyword INTERFACE. **)
  strINTERFACE = 'INTERFACE';
  (** Constant for the keyword NAME. **)
  strNAME = 'NAME';
  (** Constant for the keyword NIL. **)
  strNIL = 'NIL';
  (** Constant for the keyword OBJECT. **)
  strOBJECT = 'OBJECT';
  (** Constant for the keyword OF. **)
  strOF = 'OF';
  (** Constant for the keyword OUT. **)
  strOUT = 'OUT';
  (** Constant for the keyword PRIVATE. **)
  strPRIVATE = 'PRIVATE';
  (** Constant for the keyword PROTECTED. **)
  strPROTECTED = 'PROTECTED';
  (** Constant for the keyword PUBLIC. **)
  strPUBLIC = 'PUBLIC';
  (** Constant for the keyword STRICT. **)
  strSTRICT = 'STRICT';
  (** Constant for the keyword STRING. **)
  strSTRING = 'STRING';
  (** Constant for the keyword TYPE. **)
  strTYPE = 'TYPE';
  (** Constant for the keyword TYPE. **)
  strVAR = 'VAR';
  (** Constant for the keyword VAR. **)
  strCDINCLUDE = '{$INCLUDE';
  
  (** A lower case keyword else. **)
  strLCElse = 'else';
  (** A lower case keyword end. **)
  strLCEnd = 'end';
  
  (** A list of cyclo metric complexity oerators which increase the complexity. **)
  strCycloMetricComplexityOperators : Array[0..2] Of String = ('and', 'or', 'xor');

(**

  This is the constructor method for the TPascalDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text, that is the contents of a 
           source code module and Filename is the file name of the module being parsed and IsModified 
           determines if the source code module has been modified since the last save to disk.
  @postcon Creates an instance of the module parser.

  @nometricHardString

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
Constructor TPascalModule.CreateParser(Const Source, strFileName : String;
  Const IsModified : Boolean; Const ModuleOptions : TModuleOptions);

Const
  strStart = 'Start';
  strTokenize = 'Tokenize';
  strParse = 'Parse';
  strResolve = 'Resolve';
  strRefs = 'Refs';
  strChecks = 'Check';
  
Var
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
  CompilerDefines.Assign(BADIOptions.Defines);
  FSource := Source;
  AddTickCount(strStart);
  CommentClass := TPascalComment;
  TokenizeStream;
  AddTickCount(strTokenize);
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount(strParse);
      CheckUnResolvedMethods;
      AddTickCount(strResolve);
      Add(strErrors, iiErrorFolder, scNone, Nil);
      Add(strWarnings, iiWarningFolder, scNone, Nil);
      Add(strHints, iiHintFolder, scNone, Nil);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone, Nil);
      If FindElement(strErrors).ElementCount = 0 Then
        CheckReferences;
      AddTickCount(strRefs);
      boolCascade := True;
      If moCheckForDocumentConflicts In ModuleOptions Then
        CheckDocumentation(boolCascade);
      AddTickCount(strChecks);
      TidyUpEmptyElements;
      MetricsUnsortedMethods;
    End;
End;

(**


  This is a destructor for the TPascalModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TPascalModule.Destroy;

Begin
  FSourceCodeForProfiling.Free;
  FExternalSyms.Free;
  FMethodStack.Free;
  Inherited Destroy;
End;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @nometrics

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
  iCommentPadding = 2;
  iDirectiveSignPos = 2;

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
      {$IFDEF DXE40}
      Else If IsInSet(ch, strTokenChars) Or ch.IsLetter Then
      {$ELSE}
      Else If IsInSet(ch, strTokenChars) Or IsLetter(ch) Then
      {$ENDIF}
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
                    If (LastCharType = ttBlockComment) And (Length(strToken) > iCommentPadding) Then
                      If (strToken[1] = '{') And (strToken[iDirectiveSignPos] = '$') Then
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
Procedure TPascalModule.ParseTokens;

Begin
  Goal;
End;

(**

  This method adds a pascal method is the given container. If Nil adds to the implemented methods.

  @precon  Method must be a valid TPascalMethod.
  @postcon Adds a pascal method is the given container. If Nil adds to the implemented methods.

  @nometrics

  @param   Container as a TElementContainer as a constant
  @param   Method    as a TPascalMethod as a reference

**)
procedure TPascalModule.AddToContainer(Const Container: TElementContainer; Var Method: TPascalMethod);

Const 
  strTypes = 'Types';

Var
  iIcon : TBADIImageIndex;
  AScope : TScope;
  E : TElementContainer;
  tmpMethod : TPascalMethod;
  iCls: Integer;
  MethodsLabel : TLabelContainer;
  C: TElementContainer;

begin
  C := Container;
  If Method <> Nil Then
    Begin
      If C = Nil Then
        Begin
          If Method.Identifier <> '' Then
            Begin
              If FImplementedMethodsLabel = Nil Then
                FImplementedMethodsLabel := Add(strImplementedMethodsLabel,
                  iiImplementedMethods, scNone, Nil) As TLabelContainer;
              C := FImplementedMethodsLabel;
            End else
            Begin
              E := CurrentMethod.FindElement(strAnonymousMethods);
              If E = Nil Then
                E := CurrentMethod.Add(strAnonymousMethods, iiMethodsLabel, scNone, Nil);
              C := E;
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
                      E := E.FindElement(strTypes);
                    End;
                  C := C.Add(TLabelContainer.Create(
                    Method.ClassNames[iCls], AScope, 0, 0, iIcon, Nil));
                  // Need to add a Types C to the E.FindElement search before
                  // looking for another class.
                End;
        End;
      If C Is TRecordDecl Then
        Begin
          MethodsLabel := (C As TRecordDecl).FindElement(strMethodsLabel) As TLabelContainer;
          If MethodsLabel = Nil Then
            MethodsLabel := C.Add(
              strMethodsLabel, iiMethodsLabel, scNone, Nil) As TLabelContainer;
          C := MethodsLabel;
        End;
      tmpMethod := Method;
      Method := C.Add(tmpMethod) As TPascalMethod;
      If tmpMethod <> Method Then
        AddIssue(Format(strDuplicateIdentifierFound, [Method.Identifier,
          Method.Line, Method.Column]), scNone, tmpMethod.Line, tmpMethod.Column, etError, Self);
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
End;

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
Function TPascalModule.GetComment(Const CommentPosition : TCommentPosition) : TComment;

Const 
  iBeforeOffset = -1;
  iBeforePrevOffset = -2;

Var
  T : TTokenInfo;
  iOffset : Integer;
  iToken: TTokenIndex;
  iBracket: Integer;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := iBeforeOffset
  Else
    iOffset := iBeforePrevOffset;
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
              ErrorAndSeekToken(strModuleKeyWordNotfound, Token.Token, strSeekableOnErrorTokens,
                stActual, Self);
          End Else
          Begin
            AddIssue(strUnExpectedEndOfFile, scNone, 0, 0, etError, Self);
            Raise EBADIParserAbort.Create(strParsingAborted);
          End;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError, Self);
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

Const
  strPROGRAM = 'PROGRAM';
  
begin
  Result := Token.UToken = strPROGRAM;
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtProgram;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        Begin
          ModuleName := Token.Token;
          Line := Token.Line;
          Column := Token.Column;
          NextNonCommentToken;
          // In the Program module we need to check for '(' Ident List ')' but
          // discard
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              IdentList(Nil, scNone, TIdentList, strSeekableOnErrorTokens); // get ident list
              // Check for closing parenthesis
              If Token.Token <> ')' Then
                ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self)
              Else
                NextNonCommentToken;
            End;
          // Check for ';'
          If Token.Token <> ';' Then
            ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self)
          Else
            NextNonCommentToken;
          ProgramBlock;
          // Check for '.'
          If Token.Token <> '.' Then
            ErrorAndSeekToken(strLiteralExpected, '.', strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
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

Const
  strUNIT = 'UNIT';

Begin
  Result := Token.UToken = strUNIT;
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtUnit;
      NextNonCommentToken;
      If Not IsIdentifier(Token) Then
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self)
      Else
        Begin;
          ModuleName := Token.Token;
          Line := Token.Line;
          Column := Token.Column;
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
              ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self)
        End;
      PortabilityDirective;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self)
      Else
        NextNonCommentToken;
      InterfaceSection;
      ImplementationSection;
      InitSection;
      // Check for '.'
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, '.', strSeekableOnErrorTokens, stActual, Self);
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

Const
  strPACKAGE = 'PACKAGE';
  
begin
  Result := Token.UToken = strPACKAGE;
  If Result Then
    Begin;
      Comment := GetComment;
      ModuleType := mtPackage;
      NextNonCommentToken;
      If Not IsIdentifier(Token) Then
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self)
      Else
        Begin
          ModuleName := Token.Token;
          Line := Token.Line;
          Column := Token.Column;
          NextNonCommentToken;
        End;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self)
      Else
        NextNonCommentToken;
      // Look for requires and contains clauses
      RequiresClause;
      ContainsClause;
      If Token.UToken <> strEND Then
        ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual, Self)
      Else
        NextNonCommentToken;
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, '.', strSeekableOnErrorTokens, stActual, Self);
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

Const
  strLIBRARY = 'LIBRARY';
  
begin
  Result := Token.UToken = strLIBRARY;
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtLibrary;
      NextNonCommentToken;
      If Not IsIdentifier(Token) Then
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self)
      Else
        Begin
          ModuleName := Token.Token;
          Line := Token.Line;
          Column := Token.Column;
          NextNonCommentToken;
        End;
      // Check for ';'
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self)
      Else
        NextNonCommentToken;
      ProgramBlock;
      // Fix for Compiler accepting non-standard grammer (i.e. no begin)
      If Token.UToken = strEND Then
        NextNonCommentToken;
      // Check for '.'
      If Token.Token <> '.' Then
        ErrorAndSeekToken(strLiteralExpected, '.', strSeekableOnErrorTokens, stActual, Self);
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
  UsesClause(scPublic);
  Block(scPrivate, Nil);
end;

(**

  This method parses the Uses clause declaration from the current token position using the following 
  object pascal grammar.

  @precon  None.
  @postcon Parses the Uses clause declaration from the current token position using the following object
           pascal grammar.

  @param   eScope as a TScope as a constant

**)
Procedure TPascalModule.UsesClause(Const eScope : TScope);

Const
  strUCUSES = 'USES';
  strInterfaceLabel = 'Interface';
  strImplementationLabel = 'Implementation';
  
Var
  U : TElementContainer;
  AComment : TComment;
  i: Integer;

Begin
  If Token.UToken = strUCUSES Then
    Begin
      AComment := GetComment;
      U := Add(strUses, iiUsesLabel, scNone, AComment);
      If ModuleType = mtUnit Then
        If eScope = scPublic Then
          U := U.Add(strInterfaceLabel, iiUsesLabel, scNone, Nil)
        Else
          U := U.Add(strImplementationLabel, iiUsesLabel, scNone, Nil);
      NextNonCommentToken;
      IdentList(U, eScope, TUsesList, strSeekableOnErrorTokens, iiUsesItem);
      // Stop Implementation units from being hinted
      If eScope In [scPrivate] Then
        For i := 1 To U.ElementCount Do
          U.Elements[i].Referenced := True;
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self)
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

Const
  strDeprecated = 'deprecated';
  
Begin
  Result := False;
  While IsKeyWord(Token.Token, strPortabilityDirective) Do
    Begin
      Result := True;
      If CompareText(Token.Token, strDeprecated) = 0  Then
        Begin
          NextNonCommentToken;
          // Skip optional new Deprecate message.
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
  If Token.UToken = strINTERFACE Then
    Begin
      NextNonCommentToken;
      UsesClause(scPublic);
      InterfaceDecl;
    End Else
      ErrorAndSeekToken(strReservedWordExpected, strINTERFACE, strSeekableOnErrorTokens, stActual, Self);
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

  This method parses a exported heading declaration section from the current token position using the 
  following object pascal grammar.

  @precon  None.
  @postcon This method returns true if the current section was found to be an exported heading section.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ExportedHeading(Const Container : TElementContainer) : Boolean;

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
            ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self);
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

Const
  strIMPLEMENTATION = 'IMPLEMENTATION';
  
Begin
  If Token.UToken <> strIMPLEMENTATION Then
    ErrorAndSeekToken(strReservedWordExpected, strIMPLEMENTATION, strSeekableOnErrorTokens, stActual,
      Self)
  Else
    NextNonCommentToken;
  UsesClause(scPrivate);
  DeclSection(scPrivate, Self);
  ExportsStmt;
End;

(**

  This method parses a block section from the current token position using the following object pascal 
  grammar.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon Parses a block section from the current token position

  @param   AScope as a TScope as a constant
  @param   Method as a TPascalMethod as a constant

**)
procedure TPascalModule.Block(Const AScope : TScope; Const Method : TPascalMethod);

Var
  bool : Boolean;

Begin
  FMethodStack.Add(Method);
  Try
    DeclSection(AScope, Method);
    bool := CompoundStmt(Method);
    If Assigned(Method) Then
      Method.IsDeclarationOnly := Not bool;
    If Not bool Then
      AssemblerStatement;
    ExportsStmt;
    MetricsLongOrEmptyMethods(Method);
    MetricsNestedIFDepth(Method);
    MetricsCyclometricComplexity(Method);
    MetricsMethodToxicity(Method);
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

Const
  strEXPORTS = 'EXPORTS';
  
Begin
  Result := Token.UToken = strEXPORTS;
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
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self);
    End;
End;

(**

  This method parses an exports entry from the current token position using the following object pascal 
  grammar.

  @precon  None .
  @postcon Parses an exports entry from the current token position .

  @param   Container as a TElementContainer as a constant

**)
Procedure TPascalModule.ExportsItem(Const Container : TElementContainer);

Const
  strLCIndex = 'index';
  strLCName = 'name';
  
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
      While IsKeyWord(Token.Token, [strLCIndex, strLCName]) Do
        Begin
          // Check INDEX
          If Token.UToken = strINDEX Then
            Begin
              AddToExpression(E);
              ExprType := [petInteger, petConstExpr];
              ConstExpr(E, ExprType);
            End;
          // Check NAME
          If Token.UToken = strNAME Then
            Begin
              AddToExpression(E);
              ExprType := [petString, petConstExpr];
              ConstExpr(E, ExprType);
            End;
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
End;

(**

  This method parses a declaration section from the current token position using the following object 
  pascal grammar.

  @precon  On entry to this method , Scope defines the current scope of the block i . e . private in in 
           the implemenation section or public if in the interface section and The Method parameter is
           nil for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method .
  @postcon Parses a declaration section from the current token position .

  @nocheck EmptyREPEAT

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant

**)
Procedure TPascalModule.DeclSection(Const AScope : TScope; Const Container : TElementContainer);

Var
  M: TGenericFunction;
  eIssueState : TBADIIssueState;

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
  If Container Is TGenericFunction Then
    Begin
      M := Container As TGenericFunction;
      MetricsLongParameterList(M);
      MetricMissingConstInParamList(M);
      If M.Metric[mmLongMethodVariableLists] > BADIOptions.ModuleMetric[mmLongMethodVariableLists].FLimit Then
        Begin
          eIssueState := AddMetric([M.QualifiedName, M.Metric[mmLongMethodVariableLists],
            BADIOptions.ModuleMetric[mmLongMethodVariableLists].FLimit], M.Line, M.Column, M,
            mmLongMethodVariableLists);
          If eIssueState In [isOverride] Then
            M.MetricOverrides := M.MetricOverrides + [mmLongMethodVariableLists];
        End;
    End;
End;

(**

  This method returns the pascal profiling code for the module.

  @precon  None.
  @postcon Returns the pascal profiling code for the module.

  @return  a String

**)
Class Function TPascalModule.DefaultProfilingTemplate: String;

Const
  strProfileCode = '{$IFDEF PROFILECODE}'#13#10 +
    'CodeProfiler.Start(''$METHODNAME$'');'#13#10 +
    'Try'#13#10 +
    '{$ENDIF}'#13#10 +
    '$METHODCODE$'#13#10 +
    '{$IFDEF PROFILECODE}'#13#10 +
    'Finally'#13#10 +
    '  CodeProfiler.Stop;'#13#10 +
    'End;'#13#10 +
    '{$ENDIF}';
    
Begin
  Result := strProfileCode;
End;

(**

  This method checks the given method for missing CONST key words for each parameter and outputs a
  modue metric message if not found.

  @precon  None.
  @postcon Checks the given method for missing CONST key words for each parameter and outputs a
           modue metric message if not found.

  @param   Method as a TGenericFunction as a constant

**)
Procedure TPascalModule.MetricMissingConstInParamList(Const Method : TGenericFunction);

Const
  strSender = 'sender';
  
Var
  P : TGenericParameter;
  iParam: Integer;
  eIssueState: TBADIIssueState;

Begin
  If Assigned(Method) And (Method.ParameterCount > 0) Then
    Begin
      P := Method.Parameters[0];
      If mcsoMCParmListIgnoreEvents In BADIOptions.ModuleCheckSubOptions Then
        If CompareText(P.Identifier, strSender) = 0 Then
          Exit;
      For iParam := 0 To Method.ParameterCount - 1 Do
        If Method.Parameters[iParam].ParamModifier = pamNone Then
          Begin
            eIssuestate := AddCheck([Method.Parameters[iParam].Identifier,
              Method.QualifiedName], Method.Parameters[iParam].Line, Method.Parameters[iParam].Column,
              Method, mcMissingCONSTInParemterList);
            If eIssueState In [isAdded, isOverride] Then
              Method.IncrementCheck(mcMissingCONSTInParemterList, eIssueState = isOverride);
          End;
    End;
End;

(**

  This method checks the given metyhod cyclometric complexity and if found to be above the limit a module
  metric message is output.

  @precon  None.
  @postcon Checks the given metyhod cyclometric complexity and if found to be above the limit a module
           metric message is output.

  @param   Method as a TGenericFunction as a constant

**)
Procedure TPascalModule.MetricsCyclometricComplexity(Const Method : TGenericFunction);

Var
  eIssueState: TBADIIssueState;

Begin
  If Assigned(Method) Then
    If Method.Metric[mmCyclometricComplexity] >
      BADIOptions.ModuleMetric[mmCyclometricComplexity].FLimit Then
      Begin
        eIssueState := AddMetric([Method.QualifiedName, Method.Metric[mmCyclometricComplexity],
          BADIOptions.ModuleMetric[mmCyclometricComplexity].FLimit],
          Method.Line, Method.Column, Method, mmCyclometricComplexity);
        If eIssueState In [isOverride] Then
          Method.MetricOverrides := Method.MetricOverrides + [mmCyclometricComplexity];
      End;
End;

(**

  This method outputs a metric message for a missing block.

  @precon  None.
  @postcon A metric messsage is output for an empty block.

  @param   eCheck as a TBADIModuleCheck as a constant

**)
Procedure TPascalModule.MetricsEmptyBlockAtToken(Const eCheck : TBADIModuleCheck);

ResourceString
  strMethod = 'method';
  
Var
  eIssueState: TBADIIssueState;

Begin
  If Assigned(CurrentMethod) Then
    Begin
      eIssueState := AddCheck([strMethod, CurrentMethod.QualifiedName], Token.Line, Token.Column,
        CurrentMethod, eCheck);
      If eIssueState In [isAdded, isOverride] Then
        CurrentMethod.IncrementCheck(eCheck, eIssueState = isOverride);
    End Else
      AddCheck([strModuleTypes[ModuleType], ModuleName], Token.Line, Token.Column, Self, eCheck);
End;

(**

  This method outputs a module metric message for an On Exceptino clause thst will eat all exceptions.

  @precon  Container must be a valid container.
  @postcon Outputs a module metric message for an On Exceptino clause thst will eat all exceptions.

  @param   Container as a TElementContainer as a constant

**)
Procedure TPascalModule.MetricsExceptionEating(Const Container : TElementContainer);

Const
  strMethod = 'method';
  
Var
  eIssueState: TBADIIssueState;
  
Begin
  If Assigned(CurrentMethod) Then
    Begin
      eIssueState := AddCheck([strMethod, CurrentMethod.QualifiedName], Container.Line,
        Container.Column, CurrentMethod, mcExceptionEating);
      If eIssueState In [isAdded, isOverride] Then
        CurrentMethod.IncrementCheck(mcExceptionEating, eIssueState = isOverride);
    End Else
      AddCheck([strModuleTypes[ModuleType], ModuleName], Container.Line, Container.Column,
        Self, mcExceptionEating)
End;

(**

  This method checks the current token for a hard coded number (Int or Dec) and adds a metric message of 
  a magic number is in use.

  @precon  Container must be a valid instance.
  @postcon A metrci message is raised if the current token is a magic number.

  @param   Container as a TElementContainer as a constant

**)
Procedure TPascalModule.MetricsHardCodedNumbers(Const Container: TElementContainer);

Const
  strMethod = 'method';

Var
  M: TGenericFunction;
  dbl: Double;
  i : Integer;
  iErrorCode: Integer;
  eIssueState: TBADIIssueState;

Begin
  If Not (Container Is TConstant) Then
    Begin
      M := CurrentMethod;
      If Pos('.', Token.Token) = 0 Then
        Begin
          Val(Token.Token, i, iErrorCode);
          If (Abs(i) = 0) And (mcsoHCIntIgnoreZero In BADIOptions.ModuleCheckSubOptions) Then
            Exit;
          If (Abs(i) = 1) And (mcsoHCIntIgnoreOne In BADIOptions.ModuleCheckSubOptions) Then
            Exit;
          If Assigned(M) Then
            Begin
              eIssueState := AddCheck([Token.Token, strMethod, M.QualifiedName], Token.Line,
                Token.Column, M, mcHardCodedIntegers);
              If eIssueState In [isAdded, isOverride] Then
                M.IncrementCheck(mcHardCodedIntegers, eIssueState = isOverride);
            End Else
              AddCheck([Token.Token, strModuleTypes[ModuleType], ModuleName], Token.Line,
                Token.Column, Self, mcHardCodedIntegers);
        End Else
        Begin
          Val(Token.Token, dbl,  iErrorCode);
          If (Abs(dbl) = 0.0) And (mcsoHCNumIgmoreZero In BADIOptions.ModuleCheckSubOptions) Then
            Exit;
          If Assigned(M) Then
            Begin
              eIssueState := AddCheck([Token.Token, strMethod, M.QualifiedName], Token.Line,
                Token.Column, M, mcHardCodedNumbers);
              If eIssueState In [isAdded, isOverride] Then
                M.IncrementCheck(mcHardCodedNumbers, eIssueState = isOverride);
            End Else
              AddCheck([Token.Token, strModuleTypes[ModuleType], ModuleName], Token.Line,
                Token.Column, Self, mcHardCodedNumbers);         
        End;
    End;
End;

(**

  This method checks the current string literal token for alpha text and if so outputs a module metric 
  message for a hard coded string.

  @precon  None.
  @postcon Checks the current string literal token for alpha text and if so outputs a module metric 
           message for a hard coded string.

  @param   Container as a TElementContainer as a constant

**)
Procedure TPascalModule.MetricsHardCodedStrings(Const Container : TElementContainer);

Const
  strAlpha = ['a'..'z', 'A'..'Z'];
  iMinAlphaLength = 3;
  strMethod = 'method';

Var
  i : Integer;
  iAlphaLen : Integer;
  iMaxAlphaLen : Integer;
  eIssueState: TBADIIssueState;

Begin
  If (Length(Token.Token) > iMinAlphaLength) And Not (Container Is TConstant) Then
    Begin
      iAlphaLen := 0;
      iMaxAlphaLen := 0;
      For i := 1 To Length(Token.Token) Do
        If CharInSet(Token.Token[i], strAlpha) Then
          Begin
            Inc(iAlphaLen);
            If iMaxAlphaLen < iAlphaLen Then
              iMaxAlphaLen := iAlphaLen;
          End Else
            iAlphaLen := 0;
      If iMaxAlphaLen > 1 Then
        If Assigned(CurrentMethod) Then
          Begin
            eIssueState := AddCheck([Token.Token, strMethod, CurrentMethod.QualifiedName], Token.Line,
              Token.Column, CurrentMethod, mcHardCodedStrings);
            If eIssueState In [isAdded, isOverride] Then
              CurrentMethod.IncrementCheck(mcHardCodedStrings, eIssueState = isOverride);
          End Else
            AddCheck([Token.Token, strModuleTypes[ModuleType], ModuleName], Token.Line,
              Token.Column, Self, mcHardCodedStrings);
    ENd;
End;

(**

  This method checks the given method for either an empty implementation or an implementation that is
  too long and outputs a module metric message accordingly.

  @precon  None.
  @postcon Checks the given method for either an empty implementation or an implementation that is
           too long and outputs a module metric message accordingly.

  @param   Method as a TGenericFunction as a constant

**)
Procedure TPascalModule.MetricsLongOrEmptyMethods(Const Method : TGenericFunction);

Var
  eIssueState: TBADIIssueState;

Begin
  If Assigned(Method) Then
    Begin
      If BADIOptions.ModuleCheck[mcEmptyMethod].FEnabled Then
        If Method.StmtCount = 0 Then
          Begin
            eIssueState := AddCheck([Method.QualifiedName], Method.Line, Method.Column, Method,
              mcEmptyMethod);
            If eIssueState In [isAdded, isOverride] Then
              Method.IncrementCheck(mcEmptyMethod, eIssueState = isOverride);
          End;
      If BADIOptions.ModuleMetric[mmLongMethods].FEnabled Then
        If (Method.EndLine > Method.StartLine + BADIOptions.ModuleMetric[mmLongMethods].FLimit) Then
          Begin
            eIssueState := AddMetric([Method.QualifiedName, Method.Metric[mmLongMethods],
              BADIOptions.ModuleMetric[mmLongMethods].FLimit], Method.Line, Method.Column, Method,
              mmLongMethods);
            If eIssueState In [isOverride] Then
              Method.MetricOverrides := Method.MetricOverrides + [mmLongMethods];
          End;
    End;
End;

(**

  This method checks the given method for a long parmaeter list and if found outputs a module metric
  messge accordingly.

  @precon  Method must be a valid instance.
  @postcon Checks the given method for a long parmaeter list and if found outputs a module metric
           messge accordingly.

  @param   Method as a TGenericFunction as a constant

**)
Procedure TPascalModule.MetricsLongParameterList(Const Method : TGenericFunction);

Var
  eIssueState: TBADIIssueState;

Begin
  If Method.ParameterCount > BADIOptions.ModuleMetric[mmLongParameterLists].FLimit Then
    Begin
      eIssueState := AddMetric([Method.QualifiedName, Method.Metric[mmLongParameterLists],
        BADIOptions.ModuleMetric[mmLongParameterLists].FLimit], Method.Line, Method.Column, Method,
        mmLongParameterLists);
      If eIssueState In [isOverride] Then
        Method.MetricOverrides := Method.MetricOverrides + [mmLongParameterLists];
    End;
End;

(**

  This method outputs the toxicity of a method if it exceeds a specific limit.

  @precon  None.
  @postcon Outputs a toxicity message for the method if it exceeds a specific limit.

  @param   Method as a TGenericFunction as a constant

**)
Procedure TPascalModule.MetricsMethodToxicity(Const Method: TGenericFunction);

Var
  eIssueState: TBADIIssueState;

Begin
  If Assigned(Method) Then
    If Method.Metric[mmToxicity] > BADIOptions.ModuleMetric[mmToxicity].FLimit Then
      Begin
        eIssueState := AddMetric([Method.QualifiedName, Method.Metric[mmToxicity],
          BADIOptions.ModuleMetric[mmToxicity].FLimit], Method.Line, Method.Column, Method,
          mmToxicity);
        If eIssueState In [isOverride] Then
          Method.MetricOverrides := Method.MetricOverrides + [mmToxicity];
      End;
End;

(**

  This method outputs the NestedIFDepth of a method if it exceeds a specific limit.

  @precon  None.
  @postcon Outputs a NestedIFDepth message for the method if it exceeds a specific limit.

  @param   Method as a TGenericFunction as a constant

**)
Procedure TPascalModule.MetricsNestedIFDepth(Const Method : TGenericFunction);

Var
  eIssueState: TBADIIssueState;

Begin
  If Assigned(Method) Then
    If Method.Metric[mmNestedIFDepth] > BADIOptions.ModuleMetric[mmNestedIFDepth].FLimit Then
      Begin
        eIssueState := AddMetric([Method.QualifiedName, Method.Metric[mmNestedIFDepth],
          BADIOptions.ModuleMetric[mmNestedIFDepth].FLimit], Method.Line, Method.Column, Method,
          mmNestedIFDepth);
        If eIssueState In [isOverride] Then
          Method.MetricOverrides := Method.MetricOverrides + [mmNestedIFDepth];
      End;
End;

(**

  This method checks the sort order of the methods and output a metric / check message each method that
  is not in the correct position.

  @precon  None.
  @postcon A message is otput for each method that is not in the correct position.

**)
Procedure TPascalModule.MetricsUnsortedMethods;

  (**

    This method collects a list of pascal methods at a specific level in the treeview and check to see if
    the method sort order is correct and if not outputs a message.

    @precon  Container must be a valid instance
    @postcon Methods that are nnot in the correct sort order generate messages.

    @param   Container as a TElementContainer as a constant

  **)
  Procedure CheckForUnsortedMethods(Const Container : TElementContainer);

  Var
    olMethods : TList<TPascalMethod>;
    iMethod: Integer;
    M: TPascalMethod;
    iPrevLine, iCurrLine, iNextLine : Integer;
    boolOkay: Boolean;
    eIssueState: TBADIIssueState;

  Begin
    olMethods := TList<TPascalMethod>.Create();
    Try
      For iMethod := 1 To Container.ElementCount Do
        If Container.Elements[iMethod] Is TPascalMethod Then
          Begin
            M := Container.Elements[iMethod] As TPascalMethod;
            If Not M.ForwardDecl Then
              olMethods.Add(M);
          End;
      iPrevLine := 0;
      iNextLine := MaxInt;
      For iMethod := 0 To olMethods.Count - 1 Do
        Begin
          boolOkay := True;
          If iMethod = 0 Then
            Begin
              iCurrLine := olMethods[iMethod].StartLine;
              If olMethods.Count > 1 Then
                iNextLine := olMethods[Succ(iMethod)].StartLine;
              boolOkay := (iPrevLine < iCurrLine) And (iCurrLine < iNextLine)
            End Else
          If iMethod < olMethods.Count - 1 Then
            Begin
              iPrevLine := olMethods[Pred(iMethod)].StartLine;
              iCurrLine := olMethods[iMethod].StartLine;
              iNextLine := olMethods[Succ(iMethod)].StartLine;
              boolOkay := (iPrevLine < iCurrLine) And (iCurrLine < iNextLine)
            End Else
          If iMethod = olMethods.Count - 1 Then
            Begin
              iPrevLine := olMethods[Pred(iMethod)].StartLine;
              iCurrLine := olMethods[iMethod].StartLine;
              iNextLine := MaxInt;
              boolOkay := (iPrevLine < iCurrLine) And (iCurrLine < iNextLine)
            End;
          If Not boolOkay Then
            Begin
              eIssueState := AddCheck([olMethods[iMethod].QualifiedName], olMethods[iMethod].Line,
                olMethods[iMethod].Column, olMethods[iMethod], mcUnsortedMethod);
              If eIssueState In [isAdded, isOverride] Then
                olMethods[iMethod].IncrementCheck(mcUnsortedMethod, eIssueState = isOverride);
            End;
        End;
    Finally
      olMethods.Free;
    End;
  End;

Var
  iContainer: Integer;

Begin
  If BADIOptions.ModuleCheck[mcUnsortedMethod].FEnabled And Assigned(FImplementedMethodsLabel) Then
    Begin
      CheckForUnsortedMethods(FImplementedMethodsLabel);
      For iContainer := 1 To FImplementedMethodsLabel.ElementCount Do
        If Not (FImplementedMethodsLabel.Elements[iContainer] Is TGenericFunction) Then
          CheckForUnsortedMethods(FImplementedMethodsLabel.Elements[iContainer])
        Else
          CheckForUnsortedMethods(FImplementedMethodsLabel.Elements[iContainer]);
    End;
End;

(**

  This method parses a label declaration section from the current token position using the following 
  object pascal grammar.

  @precon  None .
  @postcon This method dicards the labels found and returns True if this method handles a label 
           declaration section .

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.LabelDeclSection(Const Container : TElementContainer) : Boolean;

Const
  strMsg = 'Method in LabelDeclSection is NULL!';
  strLABEL = 'LABEL';
  
Begin
  Result := False;
  If Container <> Nil Then
    Begin
      Result := Token.UToken = strLABEL;
      If Result Then
        Begin
          Assert(CurrentMethod <> Nil, strMsg);
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
                ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stFirst, Self);
          Until Not IsToken(',', Nil);
          // Check for ';'
          If Token.Token = ';' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self);
        End;
    End;
End;

(**

  This method parses a constant section declaration from the current token position using the following 
  object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ConstSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;

Var
  C : TElementContainer;
  LabelScope: TScope;
  ConstantsLabel: TLabelContainer;

Begin
  Result := Token.UToken = strCONST;
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
            ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self);
    End;
End;

(**

  This method parses a constant declaration from the current token position using the following object 
  pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ConstantDecl(Const AScope : TScope;
  Const Container : TElementContainer) : Boolean;

Const
  strMsg = '= or :';

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
          Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
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
                ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual, Self);
          Finally
            FTemporaryElements.Free;
          End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, strMsg, strSeekableOnErrorTokens, stActual, Self);
      PortabilityDirective;
    End;
End;

(**

  This method parses a resource string declaration section from the current token position.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @nocheck EmptyREPEAT

  @param   AScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.ResStringSection(Const AScope : TScope) : Boolean;

Const
   strRESOURCESTRING = 'RESOURCESTRING';
   
Var
  R : TElementContainer;

Begin
  Result := Token.UToken = strRESOURCESTRING;
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

  This method parses a resource string declaration section from the current token position.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ResourceStringDecl(Const AScope : TScope;
  Const Container : TElementContainer) : Boolean;

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
          Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
      Result := True;
      NextNonCommentToken;
      If Token.Token = '=' then
        Begin
          NextNonCommentToken;
          ConstExpr(C, ExprType);
          PortabilityDirective;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a type section from the current token position using the following object pascal 
  grammar.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.TypeSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;

Var
  LabelScope: TScope;
  TL : TLabelContainer;
  TypesLabel: TLabelContainer;

Begin
  Result := Token.UToken = strTYPE;
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
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self);
    End;
End;

(**

  This method parses a type declaration section from the current token position using the following 
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.TypeDecl(Const AScope : TScope; Const Container : TElementContainer) : Boolean;

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
          If Token.UToken = strTYPE Then
            Begin
              NextNonCommentToken;
              boolIsType := True;
            End;
          Result := True;
          T := GetTypeDecl(AToken);
          If T <> NIl Then
            T.IsTyped := boolIsType;
          If T = Nil Then
            ErrorAndSeekToken(strTypeNotFound, Token.Token, strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempt to get a type declaration from the current token position.

  @precon  None.
  @postcon If a type is found it is returned as the result else nil.

  @param   AToken as a TTypeToken as a constant
  @return  a TTypes

**)
Function TPascalModule.GetTypeDecl(Const AToken : TTypeToken) : TTypes;

Begin
  Result := RestrictedType(AToken);
  If Result = Nil Then
    Result := OPType(AToken);
  PortabilityDirective;
End;

(**

  This method parses the TypeArgs element of the grammar for handling generics on type arguments.

  @precon  None.
  @postcon Parses the TypeArgs element of the grammar for handling generics on type arguments.

  @param   Container as a TElementContainer as a constant

**)
Procedure TPascalModule.TypeArgs(Const Container : TElementContainer);

Begin
  PushTokenPosition;
  If Token.Token = '<' Then
    Begin
      Container.AddToken(Token.Token, Token.TokenType);
      Repeat
        NextNonCommentToken;
        If IsIdentifier(Token) Or (Token.UToken = strSTRING) Then
          Begin
            Container.AddToken(Token.Token, Token.TokenType);
            ReferenceSymbol(Token);
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

  This method parses a typed constant from the current token position using the following object pascal 
  grammar.

  @precon  C is a valid instance of the constant to be populated with tokens.
  @postcon Returns false if this was not a typed constant an not handled.

  @param   C as a TElementContainer as a constant
  @param   T as a TGenericTypeDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.TypedConstant(Const C : TElementContainer;
  Const T : TGenericTypeDecl) : Boolean;

Var
  ExprType : TPascalExprTypes;

Begin
  ExprType := [petUnknown, petConstExpr];
  Result := ArrayConstant(C, T) Or RecordConstant(C, T) Or ConstExpr(C, ExprType);
End;

(**

  This method test whether the typed constant is an Array Constant (starts with ARRAY.

  @precon  C must be a valid generic container.
  @postcon If ARRAY is found processes the constant as an array constant.

  @param   C as a TElementContainer as a constant
  @param   T as a TGenericTypeDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.ArrayConstant(Const C : TElementContainer;
  Const T : TGenericTypeDecl) : Boolean;

Begin
  Result := T Is TArrayType;
  If Result Then
    ArrayElement(C, 1, T As TArrayType);
end;

(**

  This method parses the current token position are an element of an array constant.

  @precon  C must be a valid generic container and AT must be an instance of the array type associated 
           with the array constant.
  @postcon Parses the current token position are an element of an array constant.

  @param   C               as a TElementContainer as a constant
  @param   iStartDimension as an Integer as a constant
  @param   AT              as a TArrayType as a constant

**)
Procedure TPascalModule.ArrayElement(Const C : TElementContainer;
  Const iStartDimension : Integer; Const AT : TArrayType);

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
          ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
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

  @param   AScope as a TScope as a constant
  @param   Cls    as a TRecordDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordClassVarSection(Const AScope: TScope; Const Cls : TRecordDecl): Boolean;

Begin
  Result := ClassClassVarSection(AScope, Cls)
End;

(**

  This method attempts to parser the current token position as an RecordConstant.

  @precon  C must be a valid generic container.
  @postcon Attempts to parser the current token position as an RecordConstant.

  @param   C as a TElementContainer as a constant
  @param   T as a TGenericTypeDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordConstant(Const C : TElementContainer;
  Const T : TGenericTypeDecl) : Boolean;

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
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a const section within a record definition.

  @precon  Container must be a valid container.
  @postcon Parses a const section of it exists and returns true.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordConstSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := ConstSection(AScope, Container);
End;

(**

  This method attempts to parse the current token position as a record field constant.

  @precon  C must be a valid generic container.
  @postcon Attempts to parse the current token position as a record field constant.

  @param   C as a TElementContainer as a constant
  @param   T as a TGenericTypeDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordFieldConstant(Const C : TElementContainer;
  Const T : TGenericTypeDecl) : Boolean;

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

  This method parses a type from the current token position using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TTypes

**)
Function TPascalModule.OPType(Const AToken : TTypeToken) : TTypes;

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

  This method parses a restricted type from the current token position using the following object pascal 
  grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TRestrictedType

**)
Function TPascalModule.RestrictedType(Const AToken : TTypeToken) : TRestrictedType;

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
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a class reference type declaration from the current token position using the 
  following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TClassRefType

**)
Function TPascalModule.ClassRefType(Const AToken : TTypeToken) : TClassRefType;

Const
  strClassLabel = 'Class';
  strOfLabel = 'Of';
  
Var
  T : TTypeToken;

Begin
  Result := Nil;
  If Token.UToken = strCLASS Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      If Token.UToken = strOF Then
        Begin
          NextNonCommentToken;
          T := AToken;
          UpdateTypeToken(T);
          Result := TClassRefType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
            T.FComment);
          Result := AToken.FContainer.Add(Result) As TClassRefType;
          Result.AddToken(strClassLabel);
          Result.AddToken(strOfLabel);
          If Not TypeId(Result) Then
            ErrorAndSeekToken(strTypeIdExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
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
Procedure TPascalModule.UpdateTypeToken(Var AToken: TTypeToken);

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

  This method parses a simple type declaration from the current token position using the following object
  pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TSimpleType

**)
function TPascalModule.SimpleType(Const AToken : TTypeToken) : TSimpleType;

begin
  Result := RealType(AToken);
  If Result = Nil Then
    Result := OrdinalType(AToken);
end;

(**

  This method determines if the token represents a real type using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TRealType

**)
Function TPascalModule.RealType(Const AToken : TTypeToken) : TRealType;

Var
  T : TTypeToken;

Begin
  Result := Nil;
  If IsKeyWord(Token.Token, strRealTypes) Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TRealType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
      Result := AToken.FContainer.Add(Result) As TRealType;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
End;

(**

  This method determines if the type is an ordinal type using the folowing object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TOrdinalType

**)
Function TPascalModule.OrdinalType(Const AToken : TTypeToken) : TOrdinalType;

Begin
  Result := OrdIdent(AToken);
  If Result = Nil Then
  Result := EnumerateType(AToken);
  If Result = Nil Then
  Result := SubRangeType(AToken);
End;

(**

  This method determines if the current token is an ordinal ident using the following object pascal 
  grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TOrdIdent

**)
Function TPascalModule.OrdIdent(Const AToken : TTypeToken) : TOrdIdent;

Var
  T : TTypeToken;

Begin
  Result := Nil;
  If IsKeyWord(Token.Token, strOrdIdents) Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TOrdIdent.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
      Result := AToken.FContainer.Add(Result) As TOrdIdent;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
End;

(**

  This method parses a variant type declaration section using the following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TVariantType

**)
Function TPascalModule.VariantType(Const AToken : TTypeToken) : TVariantType;

Var
  T : TTypeToken;

begin
  Result := Nil;
  If IsKeyWord(Token.Token, strVariants) Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TVariantType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
      Result := AToken.FContainer.Add(Result) As TVariantType;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
end;

(**

  This method parses a sub range type from the current token position using the following object pascal
  grammar. This method also currently acts as a type CATCH ALL if nothing else works.

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @param   AToken as a TTypeToken as a constant
  @return  a TSubRangeType

**)
Function TPascalModule.SubRangeType(Const AToken : TTypeToken) : TSubRangeType;

Var
  ExprType : TPascalExprTypes;
  Container: TElementContainer;
  T : TTypeToken;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strReservedWords) Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TSubRangeType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
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

  This method parses an enumerate type from the current token position using the following object pascal 
  grammar.

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @param   AToken as a TTypeToken as a constant
  @return  a TEnumerateType

**)
Function TPascalModule.EnumerateType(Const AToken : TTypeToken) : TEnumerateType;

Var
  T : TTypeToken;

Begin
  Result := Nil;
  If Token.Token = '(' Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TEnumerateType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
      Result := AToken.FContainer.Add(Result) As TEnumerateType;
      AddToExpression(Result);
      Repeat
        EnumerateElement(Result);
      Until Not IsToken(',', Result);
      If Token.Token = ')' Then
        AddToExpression(Result)
      Else
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses the current token position as an Enumerate Element.

  @precon  None.
  @postcon Parses the current token position as an Enumerate Element.

  @param   EnumerateType as a TEnumerateType as a constant

**)
Procedure TPascalModule.EnumerateElement(Const EnumerateType : TEnumerateType);

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
      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
End;

(**

  This method parses a sring type declaration from the current token position using the following object 
  pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TStringType

**)
Function TPascalModule.StringType(Const AToken : TTypeToken) : TStringType;

Var
  ExprType : TPascalExprTypes;
  T : TTypeToken;

begin
  Result := Nil;
  If IsKeyWord(Token.Token, strStrings) Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TStringType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
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
              ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
        End;
    End;
end;

(**

  This method parses an Array, Set of File type declaration from the current token position using the 
  following object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TTypes

**)
Function TPascalModule.StrucType(Const AToken : TTypeToken) : TTypes;

Const
  strPACKED = 'PACKED';
  
Var
  boolPacked : Boolean;

begin
  boolPacked := False;
  If Token.UToken = strPACKED Then
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
  If Token.UToken = strPACKED Then
    Begin
      Result.InsertToken(Token.Token, 0);
      NextNonCommentToken;
    End;
end;

(**

  This method parses an array type declaration from the current token position using the following object
  pascal grammar.

  @precon  boolPacked determines if the array type is packed or not.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   boolPacked as a Boolean as a constant
  @param   AToken     as a TTypeToken as a constant
  @return  a TArrayType

**)
Function TPascalModule.ArrayType(Const boolPacked : Boolean; Const AToken : TTypeToken) : TArrayType;

Const
  strPacked = 'Packed';
  
var
  T: TGenericTypeDecl;
  FTemporaryElements: TElementContainer;
  Tkn : TTypeToken;

Begin
  Result := Nil;
  If Token.UToken = strARRAY Then
    Begin
      Tkn := AToken;
      UpdateTypeToken(Tkn);
      Result := TArrayType.Create(Tkn.FIdentifier, Tkn.FScope, Tkn.FLine, Tkn.FColumn, iiPublicType,
        Tkn.FComment);
      Result := AToken.FContainer.Add(Result) As TArrayType;
      If boolPacked Then
        Result.AddToken(strPacked);
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
              ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
          End;
        If Token.UToken = strOF Then
          Begin
            Result.AppendToken(Token);
            NextNonCommentToken;
            T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              Result.AddTokens(T);
          End Else
            ErrorAndSeekToken(strReservedWordExpected, strOF, strSeekableOnErrorTokens, stActual, Self);
      Finally
        FTemporaryElements.Free;
      End;
      PortabilityDirective;
    End;
End;

(**

  This method parses a record type declaration from the current token position.

  @precon  boolPacked detmerines if the record is packed for not.
  @postcon This method returns True if this method handles a constant declaration section.

  @nometrics

  @param   boolPacked as a Boolean as a constant
  @param   AToken     as a TTypeToken as a constant
  @return  a TRecordDecl

**)
Function TPascalModule.RecType(Const boolPacked : Boolean; Const AToken : TTypeToken): TRecordDecl;

Const
  strRECORD = 'RECORD';
  
var
  InternalScope: TScope;
  boolFieldAllowed : Boolean;
  T : TTypeToken;

begin
  Result := Nil;
  If Token.UToken = strRECORD Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TRecordDecl.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicRecord,
        T.FComment);
      Result := AToken.FContainer.Add(Result) As TRecordDecl;
      Result.Line := AToken.FLine;
      Result.Column := AToken.FColumn;
      Result.Comment := AToken.FComment ;
      Result.IsPacked := boolPacked;
      NextNonCommentToken;
      InternalScope := scPublic;
      boolFieldAllowed := True;
      Result.HelperClass := (Token.UToken = strHELPER);
      If Result.HelperClass Then
        Begin
          NextNonCommentToken;
          boolFieldAllowed := False;
        End;
      // Get the classes heritage
      RecObjClsIntHeritage(Result);
      If Result.HelperClass Then
        If Token.UToken = strFOR Then
          Begin
            NextNonCommentToken;
            If IsIdentifier(Token) Then
              Begin
                Result.HelperClassName := Token.Token;
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual,
                  Self);
          End Else
            ErrorAndSeekToken(strReservedWordExpected, strFOR, strSeekableOnErrorTokens, stActual, Self);

      Repeat
        RecordVisibility(InternalScope);
        If Token.UToken = strEND Then
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
      If Token.UToken = strEND Then
        Begin
          Result.EndLine := Token.Line;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the field list element of the record definition.

  @precon  Rec must be a valid instance.
  @postcon Parses a field list if at the current token and returnd true if so.

  @param   Rec           as a TRecordDecl as a constant
  @param   InternalScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordFieldList(Const Rec : TRecordDecl; Const InternalScope : TScope) : Boolean;

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

  @param   Cls                as a TRecordDecl as a constant
  @param   AScope             as a TScope as a constant
  @param   PermissibleMethods as a TPermissibleMethods as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordMethodList(Const Cls: TRecordDecl; Const AScope: TScope;
  Const PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses a property list in a record definition.

  @precon  Cls must be a valid instance.
  @postcon A property list is parsed if found at the current position.

  @param   Cls    as a TRecordDecl as a constant
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.RecordPropertyList(Const Cls: TRecordDecl; Var AScope: TScope): Boolean;

Begin
  Result := ClassPropertyList(Cls, AScope);
End;

(**

  This method parses a type section defined within a record definition.

  @precon  Cls must be a valid instance.
  @postcon A type section is parsed if found at the current position.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordTypeSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

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

Const
  strMsg = 'PRIVATE or PUBLIC';
  strLCPrivate = 'private';
  strLCPublic = 'public';
  
Begin
  While (Token.UToken = strSTRICT) Or IsKeyWord(Token.Token, [strLCPrivate, strLCPublic]) Do
    Begin
      While Token.UToken = strSTRICT Do
        Begin
          NextNonCommentToken;
          If Token.UToken = strPRIVATE Then
            Begin
              AScope := scPrivate;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, strPRIVATE, strSeekableOnErrorTokens, stActual,
                Self);
        End;
      While IsKeyWord(Token.Token, [strLCPrivate, strLCPublic]) Do
        Begin
          If Token.UToken = strPRIVATE Then
            Begin
              AScope := scPrivate;
              NextNonCommentToken;
            End
          Else If Token.UToken = strPUBLIC Then
            Begin
              AScope := scPublic;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, strMsg, strSeekableOnErrorTokens,
                stActual, Self);
        End;
    End;
End;

(**

  This method parses a field list for classes, records and object declarations from the current token 
  position.

  @precon  Rec in a valid instance of a record type to add fields / parameters too.
  @postcon Parses a field list for classes, records and object declarations from the current token 
           position.

  @param   Rec           as a TRecordDecl as a constant
  @param   InternalScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.ObjFieldList(Const Rec: TRecordDecl; Const InternalScope : TScope) : Boolean;

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

  This method parses a records field declarations from the current token position using the following 
  object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters too.
  @postcon Parses a records field declarations from the current token position

  @param   Rec          as a TRecordDecl as a constant
  @param   InteralScope as a TScope as a constant
  @return  a Boolean

**)
Function  TPascalModule.FieldDecl(Const Rec: TRecordDecl; Const InteralScope : TScope) : Boolean;

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
    IdentList(I, InteralScope, TIdentList, strSeekableOnErrorTokens);
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
                      I[j].Line, I[j].Column]), scNone, I[j].Line, I[j].Column, etError, Self);
                  If T <> Nil Then
                    P.AddTokens(T)
                  Else
                    ErrorAndSeekToken(strTypeNotFound, '', strSeekableOnErrorTokens, stFirst, Self);
                End;
              PortabilityDirective;
            Finally
              FTemporaryElements.Free;
            End;
          End Else
            ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
        End;
  Finally
    I.Free;
  End;
End;

(**

  This method parses the variant section of a record from the current token position using the following 
  object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters too.
  @postcon Returns true is a variant section of a record was parsed.

  @param   Rec           as a TRecordDecl as a constant
  @param   InternalScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordVariantSection(Const Rec: TRecordDecl; Const InternalScope : TScope) : Boolean;

Var
  C : TElementContainer;

Begin
  Result := Token.UToken = strCASE;
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
            If Token.UToken = strOF Then
              Begin
                NextNonCommentToken;
                Repeat
                  If IsKeyWord(Token.Token, [')', strLCElse, strLCEnd]) Then
                    Break;
                  RecVariant(Rec, InternalScope);
                Until Not IsToken(';', Nil);
              End Else
                ErrorAndSeekToken(strReservedWordExpected, strOF, strSeekableOnErrorTokens, stActual, 
                  Self);
          End Else
            ErrorAndSeekToken(strTypeIDExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
      Finally
        C.Free;
      End;
    End;
End;

(**

  This method parses a var section within a record definition.

  @precon  Container must be a valid instance.
  @postcon A var section is parsed if found at the current position.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.RecordVarSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := VarSection(AScope, Container);
End;

(**

  This method parses the record variant section of a record from the current token position using the 
  following object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters too.
  @postcon Parses the record variant section of a record from the current token position

  @param   Rec           as a TRecordDecl as a constant
  @param   InternalScope as a TScope as a constant

**)
Procedure TPascalModule.RecVariant(Const Rec : TRecordDecl; Const InternalScope : TScope);

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
      ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
    If Token.Token = '(' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, '(', strSeekableOnErrorTokens, stActual, Self);
    RecordFieldList(Rec, InternalScope);
    If Token.Token = ')' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
  Finally
    C.Free;
  End;
End;

(**

  This method tries to find the symbol with its scope as mark it as referenced.

  @precon  None.
  @postcon Tries to find the symbol with its scope as mark it as referenced.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TPascalModule.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

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

  Method parses a set type declaration from the current token position using following object pascal 
  grammar.

  @precon  boolPacked determines if the set type is packed or not.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   boolPacked as a Boolean as a constant
  @param   AToken     as a TTypeToken as a constant
  @return  a TSetType

**)
Function TPascalModule.SetType(Const boolPacked : Boolean; Const AToken : TTypeToken) : TSetType;

Const
  strSET = 'SET';
  strPacked = 'Packed';
  
Var
  T : TOrdinalType;
  FTemporaryElements: TElementContainer;
  Tkn : TTypeToken;

Begin
  Result := Nil;
  If Token.UToken = strSET Then
    Begin
      Tkn := AToken;
      UpdateTypeToken(Tkn);
      Result := TSetType.Create(Tkn.FIdentifier, Tkn.FScope, Tkn.FLine, Tkn.FColumn, iiPublicType,
        Tkn.FComment);
      Result := AToken.FContainer.Add(Result) As TSetType;
      If boolPacked Then
        Result.AddToken(strPacked);
      Result.AppendToken(Token);
      NextNonCommentToken;
      If Token.UToken = strOF Then
        Begin
          AddToExpression(Result);
          FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            T := OrdinalType(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              Result.AddTokens(T)
            Else
              ErrorAndSeekToken(strOrdinalTypeExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
          Finally
            FTemporaryElements.Free;
          End;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strOF, strSeekableOnErrorTokens, stActual, Self);
      PortabilityDirective;
    End;
End;

(**

  This method parses a file type declaration from the current token position using the following object 
  pascal grammar.

  @precon  boolPacked determines if the file type is packed or not.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   boolPacked as a Boolean as a constant
  @param   AToken     as a TTypeToken as a constant
  @return  a TFileType

**)
Function TPascalModule.FileType(Const boolPacked : Boolean; Const AToken : TTypeToken) : TFileType;

Const
  strFILE = 'FILE';
  strPacked = 'Packed';
  strFileLable = 'File';
  
Var
  T : TGenericTypeDecl;
  FTemporaryElements: TElementContainer;
  Tkn : TTypeToken;

Begin
  Result := Nil;
  If Token.UToken = strFILE Then
    Begin
      NextNonCommentToken;
      If Token.UToken = strOF Then
        Begin
          Tkn := AToken;
          UpdateTypeToken(Tkn);
          Result := TFileType.Create(Tkn.FIdentifier, Tkn.FScope, Tkn.FLine, Tkn.FColumn,
            iiPublicType, Tkn.FComment);
          Result := AToken.FContainer.Add(Result) As TFileType;
          If boolPacked Then
            Result.AddToken(strPacked);
          Result.AddToken(strFileLable);
          AddToExpression(Result);
          FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
            If T <> Nil Then
              Result.AddTokens(T)
            Else
              ErrorAndSeekToken(strTypeDeclExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
          Finally
            FTemporaryElements.Free;
          End;
        End Else
        Begin
          Tkn := AToken;
          UpdateTypeToken(Tkn);
          Result := TFileType.Create(Tkn.FIdentifier, Tkn.FScope, Tkn.FLine, Tkn.FColumn,
            iiPublicType, Tkn.FComment);
          Result := AToken.FContainer.Add(Result) As TFileType;
          If boolPacked Then
            Result.AddToken(strPacked);
          Result.AddToken(strFileLable);
        End;
    End;
End;

(**

  This method parses a pointer type declaration from the current token position using the following 
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TPointerType

**)
Function TPascalModule.PointerType(Const AToken : TTypeToken) : TPointerType;

Var
  T : TTypeToken;

Begin
  Result := Nil;
  If Token.Token = '^' Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TPointerType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
        T.FComment);
      Result := AToken.FContainer.Add(Result) As TPointerType;
      Result.AppendToken(Token);
      NextNonCommentToken;
      If Not TypeId(Result) Then
        ErrorAndSeekToken(strTypeIdExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses a procedure type declaration from the current token position using the following 
  object pascal grammar.

  @precon  None.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AToken as a TTypeToken as a constant
  @return  a TProcedureType

**)
Function TPascalModule.ProcedureType(Const AToken : TTypeToken) : TProcedureType;

Var
  M : TPascalMethod;
  TemporaryContainer: TElementContainer;
  boolMethodDirective : Boolean;
  T : TTypeToken;

begin
  Result := Nil;
  TemporaryContainer := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    M := ProcedureHeading(scPrivate, TemporaryContainer, False);
    If M = Nil Then
      M := FunctionHeading(scPrivate, TemporaryContainer, False);
    If M <> Nil Then
      Begin
        T := AToken;
        UpdateTypeToken(T);
        Result := TProcedureType.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicType,
          T.FComment);
        Result := AToken.FContainer.Add(Result) As TProcedureType;
        Result.AddToken(M.AsString(True, False));
        If Token.UToken = strOF Then
          Begin
            AddToExpression(Result);
            If Token.UToken = strOBJECT Then
              AddToExpression(Result)
            Else
              ErrorAndSeekToken(strReservedWordExpected, strOBJECT, strSeekableOnErrorTokens, stActual, 
                Self);
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
  @postcon Parses an anonymous method at the current token position and returns true if the token 
           position is an anonymous method else returns false.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.AnonymousMethod(Const Container : TElementContainer) : Boolean;

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

  This method parses an anonymous method reference type declaration at the current token position.

  @precon  AToken must be a valid set of data.
  @postcon An anonymous method type is parsed if found and returned else the return is nil.

  @param   AToken as a TTypeToken as a constant
  @return  a TAnonymousReferenceType

**)
Function TPascalModule.AnonymousReferenceType(Const AToken : TTypeToken) : TAnonymousReferenceType;

Const
  strREFERENCE = 'REFERENCE';
  strTO = 'TO';
  
Var
  M : TPascalMethod;
  TemporaryContainer: TElementContainer;
  boolMethodDirective :Boolean;
  T : TTypeToken;

begin
  Result := Nil;
  If Token.UToken = strREFERENCE Then
    Begin
      NextNonCommentToken;
      If Token.UToken = strTO Then
        Begin
          NextNonCommentToken;
          TemporaryContainer := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            M := ProcedureHeading(scPrivate, TemporaryContainer, False);
            If M = Nil Then
              M := FunctionHeading(scPrivate, TemporaryContainer, False);
            If M <> Nil Then
              Begin
                T := AToken;
                UpdateTypeToken(T);
                Result := TAnonymousReferenceType.Create(T.FIdentifier, T.FScope, T.FLine,
                  T.FColumn, iiPublicType, T.FComment);
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
          ErrorAndSeekToken(strReservedWordExpected, strTO, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method check and parses a var section declaration from the current token position using the 
  following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.VarSection(Const AScope : TScope; Const Container : TElementContainer) : Boolean;

Var
  V : TLabelContainer;
  LabelScope : TScope;
  VariablesLabel: TLabelContainer;

Begin
  Result := Token.UToken = strVAR;
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
            ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self)
          Else
            NextNonCommentToken;
        End;
    End;
End;

(**

  This method checks and parses a class var section declaration from the current token position using the
  following object pascal grammar.

  @precon  On entry to this method, Scope defines the current scope of the block i.e. private in in the 
           implemenation section or public if in the interface section and The Method parameter is nil
           for methods in the implementation section or a reference to a method for a local 
           declaration section with in a method.
  @postcon This method returns True if this method handles a constant declaration section.

  @param   AScope as a TScope as a constant
  @param   Cls    as a TRecordDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.ClassClassVarSection(Const AScope : TScope;
  Const Cls : TRecordDecl) : Boolean;

Var
  V : TElementContainer;
  LabelScope : TScope;
  ClassVarsLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = strCLASS Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      Result := Token.UToken = strVAR;
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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self)
              Else
                NextNonCommentToken;
            End;
        End Else
          PopTokenPosition;
    End;
End;

(**

  This method parses the grammar associated with a constant section within a class declaration.

  @precon  None.
  @postcon Parses the grammar associated with a constant section within a class declaration.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ClassConstSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := ConstSection(AScope, Container);
End;

(**

  This method parses a Thread var section declatation from the current token position.

  @precon  On entry to this method , Scope defines the current scope of the block i . e . private in in 
           the implemenation section or public if in the interface section .
  @postcon This method returns True if this method handles a constant declaration section .

  @see     For object pascal grammar see {@link TPascalDocModule.VarSection} .

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ThreadVarSection(Const AScope : TScope;
  Const Container : TElementContainer) : Boolean;

Const
  strTHREADVAR = 'THREADVAR';

Begin
  Result := False;
  If (Container = Nil) Or (Container = Self) Then // Not allowed in methods.
    Begin
      Result := Token.UToken = strTHREADVAR;
      If Result Then
        Begin
          If FThreadVarsLabel = Nil Then
            FThreadVarsLabel := Add(strThreadVarsLabel, iiPublicThreadVarsLabel,
              scNone, GetComment) As TLabelContainer;
          NextNonCommentToken;
          While ThreadVarDecl(AScope, FThreadVarsLabel) Do
            Begin
              If Token.Token <> ';' Then
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self)
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

  @precon  AScope defines the current scope of the variable and VarSection is a valid variable container
           for the storage of the variable declared.
  @postcon Returns true if a variable declaration was handled.

  @nometrics

  @param   AScope      as a TScope as a constant
  @param   VarSection  as a TElementContainer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @return  a Boolean

**)
Function TPascalModule.VarDecl(Const AScope : TScope; Const VarSection : TElementContainer;
  Const AImageIndex : TBADIImageIndex) : Boolean;

Const
  strABSOLUTE = 'ABSOLUTE';
  
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
        IdentList(I, AScope, TIdentList, strSeekableOnErrorTokens);
        If Token.Token <> ':' Then
          ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self)
        Else
          Begin
            FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
            Try
              NextNonCommentToken;
              AToken := Token;
              T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
              If T <> Nil Then
                VarSection.ReferenceSymbol(AToken);
              If Token.UToken = strABSOLUTE Then
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
                    If Token.UToken = strNIL Then
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
                        scNone, I[j].Line, I[j].Column, etError, Self);
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

  @precon  AScope defines the current scope of the variable and VarSection is a valid variable container
           for the storage of the variable declared.
  @postcon Returns true if a variable declaration was handled.

  @param   AScope     as a TScope as a constant
  @param   VarSection as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ThreadVarDecl(Const AScope : TScope;
  Const VarSection : TElementContainer) : Boolean;

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
        IdentList(I, AScope, TIdentList, strSeekableOnErrorTokens);
        If Token.Token <> ':' Then
          ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self)
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
                      scNone, I[j].Line, I[j].Column, etError, Self);
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

  @param   C        as a TElementContainer as a constant
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.Expression(Const C : TElementContainer; Var ExprType : TPascalExprTypes);

Begin
  Repeat
    SimpleExpression(C, ExprType);
  Until Not RelOp(C, ExprType);
End;

(**

  This method attempts to parse the next series of tokens as a Simple Expression.

  @precon  none.
  @postcon Attempts to parse the next series of tokens as a Simple Expression.

  @param   C        as a TElementContainer as a constant
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.SimpleExpression(Const C : TElementContainer; Var ExprType : TPascalExprTypes);

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

  @param   C        as a TElementContainer as a constant
  @param   ExprType as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.Term(Const C : TElementContainer; Var ExprType : TPascalExprTypes);

Begin
  Repeat
    Factor(C, ExprType);
  Until Not MulOp(C, ExprType)
End;

(**

  This method attempts to parse a factor from the current token position.

  @precon  None.
  @postcon Attempts to parse a factor from the current token position.

  @nocheck EmptyBEGINEND

  @param   Container as a TElementContainer as a constant
  @param   ExprType  as a TPascalExprTypes as a reference

**)
Procedure TPascalModule.Factor(Const Container : TElementContainer; Var ExprType : TPascalExprTypes);

Const
  strNOT = 'NOT';
  
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
      MetricsHardCodedStrings(Container);
      If (petUnknown In ExprType) Then
        Begin
          Exclude(ExprType, petUnknown);
          Include(ExprType, petString);
          AddToExpression(Container);
        End
      Else If Not (petString In ExprType) Then
        ErrorAndSeekToken(strExprConflict, Token.Token, strSeekableOnErrorTokens, stActual, Self)
      Else
        AddToExpression(Container);
    End
  Else If Token.TokenType In [ttNumber] Then
    Begin
      If (petUnknown In ExprType) Then
        Begin
          MetricsHardCodedNumbers(Container);
          Exclude(ExprType, petUnknown);
          If Pos('.', Token.Token) > 0 Then
            Include(ExprType, petFloat)
          Else
            Include(ExprType, petInteger);
          AddToExpression(Container);
        End Else
        Begin
          If Not CheckNumberType(ExprType) Then
            AddIssue(Format(strExprConflict, [Token.Token, Token.Line,
              Token.Column]), scNone, Token.Line, Token.Column, etWarning, Self);
          AddToExpression(Container);
        End;
    End
  { Else If Token.UToken = 'NIL' Then
    AddToExpression(C) }
  Else If Token.Token = '@' Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Expression(Container, SubExprType);
    End
  Else If Token.Token = '&' Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Expression(Container, SubExprType);
    End
  Else If Token.Token = '^' Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Expression(Container, SubExprType);
    End
  Else If Token.Token = '@@' Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Expression(Container, SubExprType);
    End
  Else If Token.UToken = strNOT Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Factor(Container, SubExprType);
    End
  Else If Token.UToken = strINHERITED Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Designator(Container, SubExprType);
    End
  Else If Token.Token = '(' Then
    Begin
      AddToExpression(Container);
      SetupSubExprType;
      Expression(Container, SubExprType);
      If Token.Token = ')' Then
        Begin
          AddToExpression(Container);
          DesignatorSubElement(Container, SubExprType, ['.', '^']); // Type cast handler
        End
      Else
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
    End
  Else If SetConstructor(Container) Then
    Begin
      // Do nothing block...
    End
  Else If AnonymousMethod(Container) Then
    Begin
      // Do nothing block...
    End
  Else
    Begin
      SetupSubExprType;
      Designator(Container, SubExprType);
    End;
End;

(**

  This method checks the type of number in the expression to make sure Integers and Floating point number
  aren`t mixed.

  @precon  None.
  @postcon Checks the type of number in the expression to make sure Integers and Floating point number 
           aren`t mixed.

  @note    This may have problems with expression that allow integers and floats to be added, etc.

  @param   ExprType as a TPascalExprTypes as a constant
  @return  a Boolean

**)
Function TPascalModule.CheckNumberType(Const ExprType : TPascalExprTypes) : Boolean;

Begin
  If Pos('.', Token.Token) > 0 Then
    Result := petFloat In ExprType
  Else
    Result := petInteger In ExprType;
End;

(**

  This method check for the presence of a RelOp token at the current position and returns true if found 
  and advances the token position else returns false

  @precon  None.
  @postcon Check for the presence of a RelOp token at the current position and returns true if found and
           advances the token position else returns false

  @param   C        as a TElementContainer as a constant
  @param   ExprType as a TPascalExprTypes as a constant
  @return  a Boolean

**)
Function TPascalModule.RelOp(Const C : TElementContainer; Const ExprType : TPascalExprTypes) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strRelOps) And Not
    ((Token.Token = '=') And (petConstExpr In ExprType));
  If Result Then
    AddToExpression(C);
End;

(**

  This method check for the presence of an AddOp token at the current position and returns true if found 
  and advances the token position else returns false

  @precon  None.
  @postcon Check for the presence of an AddOp token at the current position and returns true if found 
           and advances the token position else returns false

  @param   C as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.AddOp(Const C : TElementContainer) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strAddOps);
  If Result Then
    AddToExpression(C);
End;

(**

  This method check for the presence of a MulOp token at the current position and returns true if found 
  and advances the token position else returns false

  @precon  None.
  @postcon Check for the presence of a MulOp token at the current position and returns true if found and
           advances the token position else returns false

  @param   C        as a TElementContainer as a constant
  @param   ExprType as a TPascalExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.MulOp(Const C : TElementContainer; Var ExprType : TPascalExprTypes) : Boolean;

Begin
  Result := IsKeyWord(Token.Token, strMulOps);
  If Result Then
    Begin
      If Not (petString In ExprType) Then
        AddToExpression(C)
      Else
        ErrorAndSeekToken(strExprConflict, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempts to parse the current token position as a Designator.

  @precon  None
  @postcon Attempts to parse the current token position as a Designator.

  @param   C        as a TElementContainer as a constant
  @param   ExprType as a TPascalExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.Designator(Const C : TElementContainer; Var ExprType : TPascalExprTypes) : Boolean;

Var
  M : TPascalMethod;
  Container : TElementContainer;

Begin
  Result := IsIdentifier(Token) Or
    (Token.UToken = strNIL) Or (Token.UToken = strSTRING);
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
  @postcon Handles the sub elements of a designator , i . e . period , [, ( and ^.

  @nometrics

  @param   C               as a TElementContainer as a constant
  @param   ExprType        as a TPascalExprTypes as a reference
  @param   strValidSymbols as an Array Of String as a constant

**)
Procedure TPascalModule.DesignatorSubElement(Const C : TElementContainer;
  var ExprType : TPascalExprTypes; Const strValidSymbols : Array of String);

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
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
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
          ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
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
        If doStrictConstantExpressions In BADIOptions.Options Then
          If petConstExpr In ExprType Then
            If Not IsKeyWord(PrevToken.Token, strConstExprDesignators) Then
              Begin
                ErrorAndSeekToken(strConstExprDesignator, PrevToken.Token, strSeekableOnErrorTokens,
                  stActual, Self);
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
          ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
      End;
End;

(**

  This method attempts to parse the current token position as a Set Constructor.

  @precon  None.
  @postcon Attempts to parse the current token position as a Set Constructor.

  @param   C as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.SetConstructor(Const C : TElementContainer) : Boolean;

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

  @param   C as a TElementContainer as a constant

**)
Procedure TPascalModule.SetElement(Const C : TElementContainer);

Var
  ExprType : TPascalExprTypes;

Begin
  Repeat
    ExprType := [petUnknown];
    Expression(C, ExprType);
  Until Not (IsToken('..', C) Or IsToken(',', C));
End;

(**

  This method attempts to parse the current token position as an Expression List.

  @precon  None.
  @postcon Attempts to parse the current token position as an Expression List.

  @param   C as a TElementContainer as a constant

**)
Procedure TPascalModule.ExprList(Const C : TElementContainer);

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

  @param   C as a TElementContainer as a constant

**)
Procedure TPascalModule.MethodExprList(Const C : TElementContainer);

Var
  ExprType : TPascalExprTypes;
  Cntr : TTempCntr;

Begin
  Repeat
    If Not AnonymousMethod(CurrentMethod) Then
      Begin
        ExprType := [petUnknown];
        Cntr := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
        Try
          Expression(Cntr, ExprType);
          If (Cntr.TokenCount = 0) And (Cntr.ElementCount = 0) Then
            AddIssue(Format(strTheMethodImplementMissingParam,
              [Token.Line, Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
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
              ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
          End;
    End;
  If Not StructStmt Then
    SimpleStatement;
End;

(**

  This method attempts to parse the current token as a list of statements.

  @precon  None.
  @postcon Attempts to parse the current token as a list of statements.

  @return  an Integer

**)
Function TPascalModule.StmtList : Integer;

Const
  strStatementTerminals : Array[1..6] Of String = ('else', 'end',
    'except', 'finalization', 'finally', 'until');

Var
  boolEnd : Boolean;
  iTokenPos: TTokenIndex;

Begin
  Result := 0;
  Repeat
    iTokenPos := TokenIndex;
    Statement;
    If iTokenPos < TokenIndex Then
      Inc(Result);
    boolEnd := Not IsToken(';', Nil);
    If boolEnd Then
      Begin
        boolEnd := IsKeyWord(Token.Token, strStatementTerminals);
        If Not boolEnd Then
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stFirst, Self);
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

Const
  strGOTO = 'GOTO';
  
Var
  ExprType : TPascalExprTypes;
  C : TTempCntr;
  eIssueState: TBADIIssueState;

Begin
  If Token.UToken = strGOTO Then
    Begin
      If Assigned(CurrentMethod) Then
        Begin
          eIssueState := AddCheck([CurrentMethod.QualifiedName], Token.Line, Token.Column,
            CurrentMethod, mcUseOfGOTOStatements);
          If eIssueState In [isAdded, isOverride] Then
            CurrentMethod.IncrementCheck(mcUseOfGOTOStatements, eIssueState = isOverride);
        End;
      NextNonCommentToken;
      If IsIdentifier(Token) Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLabelExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End
  Else
    Begin
      If Token.UToken = strINHERITED Then
        NextNonCommentToken;
      If Token.Token = '(' Then
        Begin
          NextNonCommentToken;
          ExprType := [petUnknown];
          Expression(Nil, ExprType);
          If Token.Token = ')' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
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
              AddIssue(Format(strAssignmentMissing, [Token.Line,
                Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
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

  This method parses the compound statement section of a procedure implementation from the current token 
  position using the following object pascal grammar.

  @precon  None.
  @postcon Parses the compound statement section of a procedure implementation from the current token 
           position

  @param   Method as a TGenericFunction as a constant
  @return  a Boolean

**)
Function TPascalModule.CompoundStmt(Const Method : TGenericFunction) : Boolean;

Const
  strBEGIN = 'BEGIN';
  
Var
  strTemplate: String;
  slProlog: TStringList;
  i: Integer;

begin
  Result := Token.UToken = strBEGIN;
  If Result Then
    Begin
      If Assigned(Method) Then
        Begin
          Method.StartLine := Token.Line + 1;
          If moProfiling In ModOptions Then
            Begin
              Method.Indent := Token.Column - 1;
              NextToken;
              // Check Profiling Prolog Code for a match
              strTemplate := StringReplace(
                BADIOptions.ProfilingCode[Self.ClassName],
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
        End;
      NextNonCommentToken;
      If Assigned(Method) Then
        Method.StmtCount := StmtList
      Else
        If StmtList = 0 Then
          MetricsEmptyBlockAtToken(mcEmptyBEGINEND);
      If Token.UToken = strEND Then
        Begin
          If Assigned(Method) Then
            Method.EndLine := Token.Line - 1;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strEND, [strLCend]{strSeekableOnErrorTokens},
            stActual, Self);
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

Const
  strTHEN = 'THEN';
  
Const
  strIF = 'IF';
  
Var
  ExprType : TPascalExprTypes;
  iTokenIndex : TTokenIndex;
  IFExpr: TTempCntr;
  iToken: Integer;

Begin
  Result := Token.UToken = strIF;
  If Result Then
    Begin
      If Assigned(CurrentMethod) Then
        If mmsoMethodCCIncIF In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
          Begin
            CurrentMethod.IncIFDepth;
            CurrentMethod.IncCyclometricComplexity;
          End;
      NextNonCommentToken;
      ExprType := [petUnknown];
      IFExpr := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        Expression(IFExpr, ExprType);
        If mmsoMethodCCIncIF In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
          If mmsoMethodCCIncludeExpression In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
            For iToken := 0 To IFExpr.TokenCount - 1 Do
              If IsKeyWord(IFExpr.Tokens[iToken].Token, strCycloMetricComplexityOperators) Then
                If Assigned(CurrentMethod) Then
                  CurrentMethod.IncCyclometricComplexity;
      Finally
        IFExpr.Free;
      End;
      If Token.UToken = strTHEN Then
        Begin
          NextNonCommentToken;
          iTokenIndex := TokenIndex;
          Statement;
          If iTokenIndex = TokenIndex Then
            MetricsEmptyBlockAtToken(mcEmptyTHEN);
          If Token.UToken = strELSE Then
            Begin
              NextNonCommentToken;
              iTokenIndex := TokenIndex;
              Statement;
              If iTokenIndex = TokenIndex Then
                MetricsEmptyBlockAtToken(mcEmptyELSE);
            End;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strTHEN, strSeekableOnErrorTokens, stActual, Self);
      If Assigned(CurrentMethod) Then
        CurrentMethod.DecIFDepth;
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
  Result := Token.UToken = strCASE;
  If Result Then
    Begin
      NextNonCommentToken;
      ExprType := [petUnknown];
      Expression(Nil, ExprType);
      If Token.UToken = strOF Then
        Begin
          NextNonCommentToken;
          Repeat
            If IsKeyWord(Token.Token, [strLCElse, strLCEnd]) Then
              Break;
            CaseSelector
          Until Not IsToken(';', Nil);
          If Token.UToken = strELSE Then
            Begin
              NextNonCommentToken;
              If StmtList = 0 Then
                MetricsEmptyBlockAtToken(mcEmptyELSE);
             End;
          If Token.UToken = strEND Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strOF, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempts to parse the current token position as a case selector.

  @precon  None.
  @postcon Attempts to parse the current token position as a case selector.

**)
Procedure TPascalModule.CaseSelector;

Var
  iTokenIndex: TTokenIndex;

Begin
  Repeat
    CaseLabel;
  Until Not IsToken(',', Nil);
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      iTokenIndex := TokenIndex;
      Statement;
      If TokenIndex = iTokenIndex Then
        MetricsEmptyBlockAtToken(mcEmptyCASE);
    End Else
      If Not IsKeyWord(Token.Token, [strLCElse, strLCEnd]) Then
        ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
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

Const
  strREPEAT = 'REPEAT';
  strUNTIL = 'UNTIL';
  
Var
  ExprType : TPascalExprTypes;
  UntilExpr: TTempCntr;
  iToken: Integer;
  iTokenIndex: TTokenIndex;

Begin
  Result := Token.UToken = strREPEAT;
  If Result Then
    Begin
      NextNonCommentToken;
      iTokenIndex := TokenIndex;
      StmtList;
      If TokenIndex = iTokenIndex Then
        MetricsEmptyBlockAtToken(mcEmptyREPEAT);
      If Token.UToken = strUNTIL Then
        Begin
          If Assigned(CurrentMethod) Then
            If mmsoMethodCCIncREPEAT In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
              CurrentMethod.IncCyclometricComplexity;
          NextNonCommentToken;
          ExprType := [petUnknown];
          UntilExpr := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
          Try
            Expression(UntilExpr, ExprType);
            If mmsoMethodCCIncREPEAT In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
              For iToken := 0 To UntilExpr.TokenCount - 1 Do
                If IsKeyWord(UntilExpr.Tokens[iToken].Token, strCycloMetricComplexityOperators) Then
                  If Assigned(CurrentMethod) Then
                    CurrentMethod.IncCyclometricComplexity;
          Finally
            UntilExpr.Free;
          End;
        End
      Else
        ErrorAndSeekToken(strReservedWordExpected, strUNTIL, strSeekableOnErrorTokens, stActual, Self);
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

Const
  strWHILE = 'WHILE';
  
Var
  ExprType : TPascalExprTypes;
  WhileExpr: TTempCntr;
  iToken: Integer;
  iTokenIndex: TTokenIndex;

Begin
  Result := Token.UToken = strWHILE;
  If Result Then
    Begin
      If Assigned(CurrentMethod) Then
        If mmsoMethodCCIncWHILE In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
          CurrentMethod.IncCyclometricComplexity;
      NextNonCommentToken;
      ExprType := [petUnknown];
      WhileExpr := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        Expression(WhileExpr, ExprType);
        If mmsoMethodCCIncWHILE In BADIOptions.BADIOptions.ModuleMetricSubOptions Then
          For iToken := 0 To WhileExpr.TokenCount - 1 Do
            If IsKeyWord(WhileExpr.Tokens[iToken].Token, strCycloMetricComplexityOperators) Then
              If Assigned(CurrentMethod) Then
                CurrentMethod.IncCyclometricComplexity;
      Finally
        WhileExpr.Free;
      End;
      If Token.UToken = strDO Then
        Begin
          NextNonCommentToken;
          iTokenIndex := TokenIndex;
          Statement;
          If TokenIndex = iTokenIndex Then
            MetricsEmptyBlockAtToken(mcEmptyWHILE);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strDO, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempt to parse the current token position as a For statement.

  @precon  None.
  @postcon Attempt to parse the current token position as a For statement.

  @return  a Boolean

**)
Function TPascalModule.ForStmt : Boolean;

Const
  strToOrDownTo = 'TO or DOWNTO';
  strForExprTerminators : Array[0..1] Of String = ('downto', 'to');

Var
  ExprType : TPascalExprTypes;
  M: TPascalMethod;
  iTokenIndex: TTokenIndex;

Begin
  Result := Token.UToken = strFOR;
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
              If IsKeyWord(Token.Token, strForExprTerminators) Then
                Begin
                  NextNonCommentToken;
                  Expression(Nil, ExprType);
                  If Token.UToken = strDO Then
                    Begin
                      NextNonCommentToken;
                      iTokenIndex := TokenIndex;
                      Statement;
                      If TokenIndex = iTokenIndex Then
                        MetricsEmptyBlockAtToken(mcEmptyFOR);
                    End Else
                      ErrorAndSeekToken(strReservedWordExpected, strDO, strSeekableOnErrorTokens,
                        stActual, Self);
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, strToOrDownTO, strSeekableOnErrorTokens,
                    stActual, Self);
            End
          Else If Token.UToken = strIN Then
            Begin
              NextNonCommentToken;
              ExprType := [petUnknown];
              Expression(Nil, ExprType);
              If Token.UToken = strDO Then
                Begin
                  NextNonCommentToken;
                  iTokenIndex := TokenIndex;
                  Statement;
                  If TokenIndex = iTokenIndex Then
                    MetricsEmptyBlockAtToken(mcEmptyFOR);
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, strDO, strSeekableOnErrorTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, ':=', strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempts to parse the current token position as a With Statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a With Statement.

  @return  a Boolean

**)
Function TPascalModule.WithStmt : Boolean;

Const
  strWITH = 'WITH';
var
  eIssueState: TBADIIssueState;
  
Begin
  Result := Token.UToken = strWITH;
  If Result Then
    Begin
      If Assigned(CurrentMethod) Then
        Begin
          eIssueState := AddCheck([CurrentMethod.QualifiedName], Token.Line, Token.Column,
            CurrentMethod, mcUseOfWithStatements);
          If eIssueState In [isAdded, isOverride] Then
            CurrentMethod.IncrementCheck(mcUseOfWithStatements, eIssueState = isOverride);
        End;
      NextNonCommentToken;
      Repeat
        ExprList(Nil);
      Until Not IsToken(',', Nil);
      If Token.UToken = strDO Then
        Begin
          NextNonCommentToken;
          Statement;
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strDO, strSeekableOnErrorTokens, stActual, Self);
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

Const
  strTRY = 'TRY';
  strEXCEPT = 'EXCEPT';
  strTryBlockOperators : Array[0..1] Of String = ('except', 'finally');
  strMSg = 'EXCEPT or FINALLY';
  
Begin
  Result := Token.UToken = strTRY;
  If Result Then
    Begin
      NextNonCommentToken;
      StmtList;
      If IsKeyWord(Token.UToken, strTryBlockOperators) Then
        Begin
          If Token.UToken = strEXCEPT Then
            Begin
              NextNonCommentToken;
              If Not ExceptionBlock Then
                Begin
                  If StmtList = 0 Then
                    MetricsEmptyBlockAtToken(mcEmptyEXCEPT);
                End;
            End Else
            Begin
              NextNonCommentToken;
              If StmtList = 0 Then
                MetricsEmptyBlockAtToken(mcEmptyFINALLY);
            End;
          If Token.UToken = strEND Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, strMsg, strSeekableOnErrorTokens,
            stActual, Self);
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

Const
  strON = 'ON';
  strLCException = 'exception';
  
Var
  Con : TElementContainer;

Begin
  Result := False;
  While Token.UToken = strON Do
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
      Con := TTempCntr.Create('', scNone, Token.Line, Token.Column, iiNone, Nil);
      Try
        TypeId(Con);
        If Con.TokenCount > 0 Then
          If CompareText(Con.Tokens[0].Token, strLCException) = 0 Then
            MetricsExceptionEating(Con);
        If Token.UToken = strDO Then
          Begin
            NextNonCommentToken;
            If Not CompoundStmt(Nil) Then
               Statement;
            If Token.Token = ';' Then
              NextNonCommentToken;
            If Token.UToken = strELSE Then
              Begin
                NextNonCommentToken;
                StmtList;
              End;
          End Else
            If Token.UToken <> strEND Then
              ErrorAndSeekToken(strReservedWordExpected, strDO, strSeekableOnErrorTokens, stActual, Self);
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

Const
  strRAISE = 'RAISE';
  strAT = 'AT';
  
Var
  ExprType : TPascalExprTypes;

Begin
  Result := Token.UToken = strRAISE;
  If Result Then
    Begin
      NextNonCommentToken;
      SimpleStatement;
      If Uppercase(Token.Token) = strAT Then
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

Const
  strASM = 'ASM';
  
Begin
  Result := Token.UToken = strASM;
  If Result Then
    Begin
      Repeat
        NextNonCommentToken;
      Until (Token.UToken = strEND) And (PrevToken.Token <> '@@');
      NextNonCommentToken;
    End;
End;

(**

  This function returns a string repreentation of the unit.

  @precon  None .
  @postcon Returns a string repreentation of the unit.

  @nohints

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TPascalModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := strModuleTypes[ModuleType];
  If boolShowIdentifier Then
    Result := Result + #32 + ChangeFileExt(ExtractFileName(Identifier), '');
end;

(**

  This method parses a single RTTI Attribute declaration at the current token position.

  @precon  None.
  @postcon The current RTTI Attribute at the current token postiion is parsed else an error is raised.

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
            ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
        End;
    End Else
      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
End;

(**

  This method parses a procedure declaration section from the current token position using the following 
  object pascal grammar.

  @precon  AScope is the current scope of the procedure declaration and Method is the current method 
           scoped else nil.
  @postcon Returns true is a procedure declaration was parsed.

  @param   AScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.ProcedureDeclSection(Const AScope : TScope) : Boolean;

Var
  M : TPascalMethod;
  Cls : Boolean;

Begin
  Result := False;
  Repeat
    RTTIAttributes;
    Cls := False;
    If Token.UToken = strCLASS Then
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

  @param   AScope as a TScope as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.ProcedureDecl(Const AScope : TScope) : TPascalMethod;

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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempts to parse the current token position as a FunctionDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a FunctionDecl;

  @param   AScope as a TScope as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.FunctionDecl(Const AScope : TScope) : TPascalMethod;

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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses the constraint element of the grammar for within a generic parameter definition.

  @precon  None.
  @postcon Parses the constraint element of the grammar for within a generic parameter definition.

**)
Procedure TPascalModule.Constraint;

Const
  strConstraints : Array[0..2] Of String = ('class', 'constructor', 'record');

Begin
  If IsKeyWord(Token.Token, strConstraints) Then
    NextNonCommentToken
  Else
    If IsIdentifier(Token) Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
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

  @param   AScope as a TScope as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.ConstructorDecl(Const AScope : TScope) : TPascalMethod;

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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempts to parse the current token position as a DestructorDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a DestructorDecl;

  @param   AScope as a TScope as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.DestructorDecl(Const AScope : TScope) : TPascalMethod;

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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method attempts to parse the current token position as a OperatorDecl;

  @precon  None.
  @postcon Attempts to parse the current token position as a OperatorDecl;

  @param   AScope as a TScope as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.OperatorDecl(Const AScope : TScope) : TPascalMethod;

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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a function declaration from the current token position using the following object 
  pascal grammar.

  @precon  AScope is the current scope of the function declaration.
  @postcon Returns a method declaration is a function was parsed else nil.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @param   boolIdent as a Boolean as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.FunctionHeading(Const AScope :TScope;
  Const Container : TElementContainer; Const boolIdent : Boolean = true) : TPascalMethod;

Var
  C : TComment;
  boolClassMethod : Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtFunction]) Then
    Begin
      boolClassMethod := False;
      If PrevToken.UToken = strCLASS Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken);
        End Else
          C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Xor boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
          Exit;
        End;
      Result := MethodQualifiers(AScope, Container, mtFunction, boolClassMethod, C, boolIdent);
      Try
        FormalParameter(Result);
        If boolIdent Then
          CheckAlias(Result);
        CheckReturnValue(Result);
        Directive(Result, True);
      Finally
        AddToContainer(Container, Result);
      End;
    End;
End;

(**

  This method parses a operator declaration from the current token position using the following object 
  pascal grammar.

  @precon  AScope is the current scope of the operator declaration.
  @postcon Returns a method declaration is a function was parsed else nil.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @param   boolIdent as a Boolean as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.OperatorHeading(Const AScope :TScope;
  Const Container : TElementContainer; Const boolIdent : Boolean = True) : TPascalMethod;

Var
  C : TComment;
  boolRequiresReturn: Boolean;
  boolClassMethod : Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtOperator]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = strCLASS Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken);
        End Else
          C := GetComment;
      NextNonCommentToken;
      If IsIdentifier(Token) Xor boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
          Exit;
        End;
      Result := MethodQualifiers(AScope, Container, mtOperator, boolClassMethod, C,
        boolIdent);
      If Not IsKeyWord(Result.Identifier, strOperatorList) Then
        AddIssue(Format(strInvalidOperator, [Result.Identifier, Result.Line,
          Result.Column]), AScope, Result.Line, Result.Column, etError, Self);
      FormalParameter(Result);
      If boolIdent Then
        CheckAlias(Result);
      boolRequiresReturn := CheckReturnValue(Result);
      Directive(Result, True);
      If boolRequiresReturn Then
        If Result.Alias = '' Then
          AddIssue(Format(strFunctionWarning, [Result.QualifiedName]), scNone,
            Token.Line, Token.Column, etWarning, Self);
    Finally
      AddToContainer(Container, Result);
    End;
End;

(**

  This method checks the alias (if one exists) of the procedure / function.

  @precon  Method must be a valid TPascalMethod instance.
  @postcon Checks the alias (if one exists) of the procedure / function.

  @param   Method as a TPascalMethod as a constant

**)
procedure TPascalModule.CheckAlias(Const Method : TPascalMethod);

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
                  ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method checks that function returns are present for non-aliased or external functions. If not 
  present adds an issue.

  @precon  Func must be a valid instance.
  @postcon Checks that function returns are present for non-aliased or external functions. If not 
           present adds an issue.

  @param   Func as a TPascalMethod as a constant

**)
procedure TPascalModule.CheckFunctionReturn(Const Func : TPascalMethod);

Const
  strLCexternal = 'external';
  
begin
  If Func.MethodType = mtFunction Then
    Begin
      If Func.ReturnType.ElementCount = 0 Then
        If Func.Alias = '' Then
          If Not Func.HasDirective(strLCexternal) Then
            AddIssue(Format(strFunctionWarning, [Func.QualifiedName]), scNone,
              Token.Line, Token.Column, etWarning, Self);
    End;
end;

(**

  This method checks the returns value of the function.

  @precon  Method must be a valid TPascalMethod instance .
  @postcon Checks the returns value of the function .

  @param   Method as a TGenericFunction as a constant
  @return  a Boolean

**)
Function TPascalModule.CheckReturnValue(Const Method : TGenericFunction) : Boolean;

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

  This method cross reference the methods in class, exported and implemented and marsk the as resolved 
  and output error messages for those that are still unresolved.

  @precon  None.
  @postcon Cross reference the methods in class, exported and implemented and marsk the as resolved and 
           output error messages for those that are still unresolved.

**)
procedure TPascalModule.CheckUnResolvedMethods;

Var
  Errors: TLabelContainer;

begin
  ResolveScopeOfImplementedExportedMethods;
  ResolveScopeOfImplementedExportsMethods;
  ResolveScopeOfImplementedMethods(FImplementedMethodsLabel);
  ResolveForwardImplementedMethods;
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

  This method parse a procedure declaration from the current token position using the following object 
  pascal grammar.

  @precon  AScope is the current scope of the procedure declaration.
  @postcon Returns a method declaration is a procedure was parsed else nil.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @param   boolIdent as a Boolean as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.ProcedureHeading(Const AScope : TScope;
  Const Container : TElementContainer; Const boolIdent : Boolean = True) : TPascalMethod;

Var
  C : TComment;
  boolClassMethod : Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtProcedure]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = strCLASS Then
        Begin
          boolClassMethod := True;
          C := GetComment(cpBeforePreviousToken)
        End Else
          C := GetComment;
      NextNonCommentToken;
      If Not IsIdentifier(Token) And boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
          Exit;
        End;
      Result := MethodQualifiers(AScope, Container, mtProcedure, boolClassMethod, C, boolIdent);
      FormalParameter(Result);
      If boolIdent Then
        CheckAlias(Result);
      Directive(Result, True);
    Finally
      AddToContainer(Container, Result);
    End;
End;

(**

  This method parses a methods formal parameters from the current token position using the following 
  object psacal grammar.

  @precon  Method is a valid method to which the formal parameters are to be added.
  @postcon Parses a methods formal parameters from the current token position

  @param   Method as a TPascalMethod as a constant

**)
Procedure TPascalModule.FormalParameter(Const Method : TPascalMethod);

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
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
  End;
End;

(**

  This method parses the formal type parameter list element of the generic grammar.

  @precon  None.
  @postcon The formal type parameter list is parsed.

  @param   strIdentifier as a String as a reference

**)
Procedure TPascalModule.FormalTypeParamList(Var strIdentifier : String);

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
          ErrorAndSeekToken(strLiteralExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a formal parameter for a method from the current token position using the following 
  object pascal grammar.

  @precon  Method is a valid method to which the formal parameters are to be added.
  @postcon Parses a formal parameter for a method from the current token position

  @param   Method as a TPascalMethod as a constant

**)
Procedure TPascalModule.FormalParam(Const Method : TPascalMethod);

Var
  pmMod : TParamModifier;

Begin
  pmMod := pamNone;
  // Get modifiers
  If Token.UToken = strVAR Then
    pmMod := pamVar
  Else If Token.UToken = strCONST Then
    pmMod := pamConst
  Else If Token.UToken = strOUT Then
    pmMod := pamOut;
  If pmMod <> pamNone Then
    NextNonCommentToken;
  Parameter(Method, pmMod);
End;

(**

  This method parses a parameter list for a method from the current token position using the following 
  object pascal grammar.

  @precon  Method is a valid method to add a parameter too and ParamMod is a parameter modifier for the 
           parameter to signify a const, var or out paramemter.
  @postcon Parses a parameter list for a method from the current token position

  @param   Method   as a TPascalMethod as a constant
  @param   ParamMod as a TParamModifier as a constant

**)
Procedure TPascalModule.Parameter(Const Method : TPascalMethod; Const ParamMod : TParamModifier);

Const
  strTmpName = 'TmpName';

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
      IdentList(P, scNone, TIdentList, strSeekableOnErrorTokens);
      If Token.Token = ':' Then
        Begin
          NextNonCommentToken;
          // Check Array Of
          If Token.UToken = strARRAY Then
            Begin
              NextNonCommentToken;
              IF Token.UToken = strOF Then
                Begin;
                  boolArrayOf := True;
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, strOF, strSeekableOnErrorTokens, stActual, Self);
            End;
          T := GetTypeDecl(TypeToken(Nil, scNone, Nil, FTemporaryElements));
          If T = Nil Then
            If Token.UToken = strCONST Then
              Begin
                T := FTemporaryElements.Add(TTypes.Create(strTmpName, scPrivate,
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

  This method retrives the method directives after the method declaration from the current token position
  using the followong object pascal grammar.

  @precon  M is a valid method declaration to add directives too.
  @postcon Retrives the method directives after the method declaration from the current token position

  @param   M              as a TPascalMethod as a constant
  @param   boolGrammarFix as a Boolean as a constant

**)
Procedure TPascalModule.Directive(Const M : TPascalMethod; Const boolGrammarFix : Boolean = False);

Const
  strFORWARD = 'FORWARD';
  strMESSAGE = 'MESSAGE';
  strEXTERNAL = 'EXTERNAL';
  strMessageLabel = 'Message';
  strExternalLabel = 'External';
  strNameLabel = 'Name';
  strIndexLabel = 'Index';
  strDispIDLabel = 'DispID';
  
Var
  C : TElementContainer;
  ExprType : TPascalExprTypes;
  strExternalDecl: String;

Begin
  // Check for method directives
  While IsKeyWord(Token.Token, strMethodDirectives) Do
    Begin
      If Token.UToken = strFORWARD THEN
        M.ForwardDecl := True;
      If Token.UToken = strABSTRACT THEN
        M.ForwardDecl := True;
      C := TIdentList.Create('', scLocal, 0, 0, iiNone, Nil);
      Try
        If Token.UToken = strMESSAGE Then
          Begin
            NextNonCommentToken;
            ExprType := [petConstExpr, petInteger];
            ConstExpr(C, ExprType);
            M.AddDirectives(strMessageLabel + #32 + C.AsString(True, False));
          End
        Else If Token.UToken = strEXTERNAL Then
          Begin
            M.ForwardDecl := True;
            NextNonCommentToken;
            If Token.Token <> ';' Then
              Begin
                ExprType := [petConstExpr, petString];
                ConstExpr(C, ExprType);
                strExternalDecl := C.AsString(True, False);
                If strExternalDecl = '' Then
                  M.AddDirectives(strExternalLabel)
                Else
                  M.AddDirectives(strExternalLabel+ #32 + strExternalDecl);
                If Token.UToken = strNAME Then
                  Begin
                    NextNonCommentToken;
                    ExprType := [petConstExpr, petString];
                    ConstExpr(C, ExprType);
                    M.AddDirectives(strNameLabel + #32 + C.AsString(True, False));
                  End;
                If Token.UToken = strINDEX Then
                  Begin
                    NextNonCommentToken;
                    ExprType := [petConstExpr, petInteger];
                    ConstExpr(C, ExprType);
                    M.AddDirectives(strIndexLabel + #32 + C.AsString(True, False));
                  End;
              End;
            M.Referenced := True;
          End
        Else If Token.UToken = strDISPID Then
          Begin
            NextNonCommentToken;
            ExprType := [petConstExpr, petInteger];
            ConstExpr(C, ExprType);
            M.AddDirectives(strDispIDLabel + #32 + C.AsString(True, False));
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
                ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
      Finally
        C.Free;
      End;
    End;
End;

(**

  This method parses a class var section for an object declaration by delegating this to the 
  ClassClassVarSection.

  @precon  Cls must be a valid instance
  @postcon Parses a class var section defined within an object delcaration.

  @param   AScope as a TScope as a constant
  @param   Cls    as a TRecordDecl as a constant
  @return  a Boolean

**)
Function TPascalModule.ObjClassVarSection(Const AScope: TScope; Const Cls: TRecordDecl): Boolean;

Begin
  Result := ClassClassVarSection(AScope, Cls);
End;

(**

  This method parses a constant section defined in an object delcaration by delegating this to the 
  ConstSection method.

  @precon  None.
  @postcon Parses a constant section in an object definition.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ObjConstSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := ConstSection(AScope, Container);
End;

(**

  This method parses an Object type declaration from the current token position using the followong 
  object pascal grammar.

  @precon  None.
  @postcon Returns an object declaration if one was parsed else nil.

  @param   AToken as a TTypeToken as a constant
  @return  a TObjectDecl

**)
function TPascalModule.ObjectType(Const AToken : TTypeToken) : TObjectDecl;

Var
  InternalScope : TScope;
  T : TTypeToken;

begin
  InternalScope := scPublic;
  Result := Nil;
  If Token.UToken = strOBJECT Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      Result := TObjectDecl.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicObject,
        T.FComment);
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
            If Token.UToken = strEND Then
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
          If Token.UToken = strEND Then
            Begin
              Result.EndLine := Token.Line;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual,
                Self);
        End Else
          NextNonCommentToken;
    End;
end;

(**

  This method attempts to parse the current token position as an Object Heritage list.

  @precon  None.
  @postcon Attempts to parse the current token position as an Object Heritage list.

  @param   ObjDecl as a TObjectDecl as a constant

**)
Procedure TPascalModule.ObjHeritage(Const ObjDecl : TObjectDecl);

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
            ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
        End Else
            ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a list of methods defined in an object by delegating this to the MethodList method.

  @precon  Cls must be a valid instance
  @postcon Method within the oject declarations are parsed.

  @param   Cls                as a TRecordDecl as a constant
  @param   AScope             as a TScope as a constant
  @param   PermissibleMethods as a TPermissibleMethods as a constant
  @return  a Boolean

**)
Function TPascalModule.ObjMethodList(Const Cls: TRecordDecl; Const AScope: TScope;
  Const PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses the Property List element of the grammar within an object definition by delegating 
  this to the ClassPropertyList method.

  @precon  Cls must be a valid instance
  @postcon Parse the property list element of the grammar within the object definition.

  @param   Cls    as a TRecordDecl as a constant
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
function TPascalModule.ObjPropertyList(Const Cls: TRecordDecl; Var AScope: TScope): Boolean;

Begin
  Result := ClassPropertyList(Cls, AScope);
End;

(**

  This method parses a type section within an object declaration by delegating this to the TypeSection 
  method.

  @precon  Cls must be a valid instance
  @postcon A type section is parsed which is defined within an object definition.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ObjTypeSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := TypeSection(AScope, Container);
End;

(**

  This method parses the var section declared with an object definition.

  @precon  Container must be a valid container.
  @postcon A var section is parsed if found.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ObjVarSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := VarSection(AScope, Container);
End;

(**

  This method parses the visibility elements of an object definiton.

  @precon  None.
  @postcon Parses an visibility elements of the object definiton return the visibility in the var 
           parameter.

  @param   AScope as a TScope as a reference

**)
Procedure TPascalModule.ObjVisibility(Var AScope: TScope);

Const
  strObjScope : Array[1..3] Of String = ('private', 'protected', 'public');
  strPrivateOrProtected = 'PRIVATE or PROTECTED';
  strPrivateProtectedOrPublic = 'PRIVATE, PROTECTED or PUBLIC';

Begin
  While (Token.UToken = strSTRICT) Or IsKeyWord(Token.Token, strObjScope) Do
    Begin
      While Token.UToken = strSTRICT Do
        Begin
          NextNonCommentToken;
          If IsKeyWord(Token.Token, strStrictedScope) Then
            Begin
              If Token.UToken = strPRIVATE Then
                Begin
                  AScope := scPrivate;
                  NextNonCommentToken;
                End
              Else If Token.UToken = strPROTECTED Then
                Begin
                  AScope := scProtected;
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, strPrivateOrProtected,
                    strSeekableOnErrorTokens, stActual, Self);
            End;
        End;
      While IsKeyWord(Token.Token, strObjScope) Do
        Begin
          If Token.UToken = strPRIVATE Then
            Begin
              AScope := scPrivate;
              NextNonCommentToken;
            End
          Else If Token.UToken = strPROTECTED Then
            Begin
              AScope := scProtected;
              NextNonCommentToken;
            End
          Else If Token.UToken = strPUBLIC Then
            Begin
              AScope := scPublic;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, strPrivateProtectedOrPublic,
                strSeekableOnErrorTokens, stActual, Self);
        End;
    End;
End;

(**

  This method parse a method list from the current token position using the following object pascal 
  grammar.

  @precon  Cls is an object declaration to add methods too and Scopeis the current internal scope of the
           object.
  @postcon Returns true is a method declaration was parsed.

  @param   Cls                as a TRecordDecl as a constant
  @param   AScope             as a TScope as a constant
  @param   PermissibleMethods as a TPermissibleMethods as a constant
  @return  a Boolean

**)
Function TPascalModule.MethodList(Const Cls: TRecordDecl; Const AScope: TScope;
  Const PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  RTTIAttributes;
  Result := MethodHeading(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses the quantified record / object / class names that preceed an implemented method.

  @precon  None.
  @postcon The method builds a list of the record / object / class hierarchy that qualifies the 
           implemented method.

  @param   AScope          as a TScope as a constant
  @param   Container       as a TElementContainer as a constant
  @param   iMethodType     as a TMethodType as a constant
  @param   boolClassMethod as a Boolean as a constant
  @param   C               as a TComment as a constant
  @param   boolIdent       as a Boolean as a constant
  @return  a TPascalMethod

**)
Function TPascalModule.MethodQualifiers(Const AScope :TScope; Const Container : TElementContainer;
  Const iMethodType : TMethodType; Const boolClassMethod : Boolean; Const C : TComment;
  Const boolIdent : Boolean = True) : TPascalMethod;

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
                ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
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
      Result := TPascalMethod.Create(iMethodType, strIdentifier, AScope, iLine, iColumn);
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

  This method checks for and parses a method declaration in a class from the current token position.

  @precon  Cls is an object declaration to add method declarations too and Scope is the current scope 
           inside the object declaration.
  @postcon Returns true if a method declaration was parsed.

  @param   Cls                as a TRecordDecl as a constant
  @param   AScope             as a TScope as a constant
  @param   PermissibleMethods as a TPermissibleMethods as a constant
  @return  a Boolean

**)
Function TPascalModule.MethodHeading(Const Cls: TRecordDecl; Const AScope: TScope;
  Const PermissibleMethods : TPermissibleMethods): Boolean;

Var
  M :TPascalMethod;
  boolClassMethod : Boolean;

begin
  Result := False;
  // Check for class method
  boolClassMethod := False;
  PushTokenPosition;
  If Token.UToken = strCLASS Then
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
          ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
      If Not (M.MethodType In PermissibleMethods) Then
        AddIssue(strMethodNotPermitted, AScope, M.Line, M.Column, etError, Self);
      If Cls Is TInterfaceDecl Then
        Begin
          MetricMissingConstInParamList(M);
          MetricsLongParameterList(M);
        End;
    End Else
      If boolClassMethod Then
        PopTokenPosition;
end;

(**

  This method parses a constructor declaration from the current token position using the following object
  pascal grammar.

  @precon  Scope is the current scope of the constructor declaration.
  @postcon Returns a method declaration is a constructor was parsed else nil.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a TPascalMethod

**)
function TPascalModule.ConstructorHeading(Const AScope: TScope;
  Const Container : TElementContainer): TPascalMethod;

Var
  C : TComment;
  boolClassMethod : Boolean;

begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtConstructor]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = strCLASS Then
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

  This method parses a destructor declaration from the current token position using the following object 
  pascal grammar.

  @precon  Scope is the current scope of the destructor declaration.
  @postcon Returns a method declaration is a destructor was parsed else nil.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a TPascalMethod

**)
function TPascalModule.DestructorHeading(Const AScope: TScope;
  Const Container : TElementContainer): TPascalMethod;

Var
  C : TComment;
  boolClassMethod: Boolean;

begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtDestructor]) Then
    Try
      boolClassMethod := False;
      If PrevToken.UToken = strCLASS Then
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

  This method parses a classes / interfaces field list from the current token position using the 
  following object pascal grammar.

  @precon  Cls is an ibject delcaration to add fields too and Scope is the current internal scope of the
           object.
  @postcon Returns true is a field was parsed.

  @param   Cls    as a TObjectDecl as a constant
  @param   AScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.FieldList(Const Cls: TObjectDecl; Const AScope: TScope): Boolean;

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
    IdentList(I, AScope, TIdentList, strSeekableOnErrorTokens);
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
                  I[j].Line, I[j].Column]), scNone, I[j].Line, I[j].Column, etError, Self);
              If T <> Nil Then
                P.AddTokens(T)
              Else
                ErrorAndSeekToken(strTypeDeclExpected, '', strSeekableOnErrorTokens, stFirst, Self);
            End;
        Finally
          FTemporaryElements.Free;
        End;
      End Else
        ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
  Finally
    I.Free;
  End;
end;

(**

  This method parses the modules initialisation / finalisation section from the current token position 
  using the following object pascal grammar.

  @precon  None.
  @postcon Parses the modules initialisation / finalisation section from the current token position.

  @nocheck EmptyBEGINEND

**)
Procedure TPascalModule.InitSection;

Const
  strINITIALIZATION = 'INITIALIZATION';
  strFINALIZATION = 'FINALIZATION';
  strInitBeginOrEnd = 'INITIALIZATION, BEGIN or END';
  
Var
  I, F : TElementContainer;
  iFinal : Integer;
  iInitial : Integer;

Begin
  If Token.UToken = strINITIALIZATION Then
    Begin
      I := Add(TInitializationSection.Create(Token.Token, scNone, Token.Line,
        Token.Column, iiInitialization, GetComment));
      NextNonCommentToken;
      iInitial := StmtList;
      If Token.UToken = strFINALIZATION Then
        Begin
          F := Add(TFinalizationSection.Create(Token.Token, scNone, Token.Line,
            Token.Column, iiFinalization, GetComment));
          NextNonCommentToken;
          iFinal := StmtList;
          If iFinal = 0 Then
            Begin
              AddCheck([], F.Line, F.Column, F, mcEmptyFinalization);
            End;
        End Else
          iFinal := 0;
      If (iInitial = 0) And (iFinal = 0) Then
        Begin
          AddCheck([], I.Line, I.Column, I, mcEmptyIntialization);
        End;
      If Token.UToken = strEND Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual, Self);
    End
  Else If CompoundStmt(Nil) Then
    Begin
      // Do Nothing...
    End
  Else If Token.UToken = strEND Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strReservedWordExpected, strInitBeginOrEnd, strSeekableOnErrorTokens,
      stActual, Self);
End;

(**

  This method parse a class declaration from the current token position deligating field, property and 
  method declarations using the following object pascal grammar.

  @precon  None.
  @postcon Returns a class declaration is a class was parsed else nil.

  @nometrics

  @param   AToken as a TTypeToken as a constant
  @return  a TClassDecl

**)
function TPascalModule.ClassType(Const AToken : TTypeToken) : TClassDecl;

Const
  strSEALED = 'SEALED';
  
Var
  InternalScope : TScope;
  boolFieldAllowed: Boolean;
  T : TTypeToken;

begin
  boolFieldAllowed := True;
  InternalScope := scPublished;
  Result := Nil;
  If Token.UToken = strCLASS Then
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      // Check for 'OF'
      If Token.UToken <> strOF Then
        Begin
          T := AToken;
          UpdateTypeToken(T);
          Result := TClassDecl.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn, iiPublicClass,
            T.FComment);
          Result := AToken.FContainer.Add(Result) As TClassDecl;
          Result.Line := AToken.FLine;
          Result.Column := AToken.FColumn;
          Result.Comment := AToken.FComment;
          Result.AbstractClass := (Token.UToken = strABSTRACT);
          If Result.AbstractClass Then
            NextNonCommentToken;
          Result.SealedClass := (Token.UToken = strSEALED);
          If Result.SealedClass Then
            NextNonCommentToken;
          Result.HelperClass := (Token.UToken = strHELPER);
          If Result.HelperClass Then
            Begin
              NextNonCommentToken;
              boolFieldAllowed := False;
            End;
          // Get the classes heritage
          RecObjClsIntHeritage(Result);
          If Result.HelperClass Then
            If Token.UToken = strFOR Then
              Begin
                NextNonCommentToken;
                If IsIdentifier(Token) Then
                  Begin
                    Result.HelperClassName := Token.Token;
                    NextNonCommentToken;
                  End Else
                    ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
              End Else
                ErrorAndSeekToken(strReservedWordExpected, strFOR, strSeekableOnErrorTokens, stActual, Self);
          // If this class has no body then return
          If Token.Token <> ';' Then
            Begin
              Repeat
                ClassVisibility(InternalScope);
                If Token.UToken = strEND Then
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
              If Token.UToken = strEND Then
                Begin
                  Result.EndLine := Token.Line;
                  NextNonCommentToken;
                End Else
                  ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual,
                    Self);
            End;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses the grammar associated with a type declaration with in a class declaration.

  @precon  None.
  @postcon Parses the grammar associated with a type declaration with in a class declaration.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TPascalModule.ClassTypeSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := TypeSection(AScope, Container);
End;

(**

  This method parses a class heriage ist from the current token position using the following object 
  pascal grammar.

  @precon  Cls is a valid object declaration to get a heritage for.
  @postcon Parses a class heriage ist from the current token position

  @param   RecObjClsInt as a TRecordDecl as a constant

**)
procedure TPascalModule.RecObjClsIntHeritage(Const RecObjClsInt: TRecordDecl);

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      IdentList(RecObjClsInt.Heritage, scNone, TIdentList, strSeekableOnErrorTokens);
      If Token.Token = ')' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the grammar associated with a var declaration with in a class declaration.

  @precon  None.
  @postcon Parses the grammar associated with a var declaration with in a class declaration.

  @param   AScope    as a TScope as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.ClassVarSection(Const AScope: TScope;
  Const Container: TElementContainer): Boolean;

Begin
  Result := VarSection(AScope, Container);
End;

(**

  This method parse the class visibility from the current token using the following object pascal grammar
  .

  @precon  Scope is the current internal scope of the class.
  @postcon Parse the class visibility from the current token

  @param   AScope as a TScope as a reference

**)
Procedure TPascalModule.ClassVisibility(Var AScope: TScope);

Begin
  While (Token.UToken = strSTRICT) Or IsKeyWord(Token.Token, strScope) Do
    Begin
      While Token.UToken = strSTRICT Do
        Begin
          NextNonCommentToken;
          If IsKeyWord(Token.Token, strStrictedScope) Then
            Begin
              If Token.UToken = strPRIVATE Then
                AScope := scPrivate
              Else If Token.UToken = strPROTECTED Then
                AScope := scProtected;
              NextNonCommentToken;
            End;
        End;
      While IsKeyWord(Token.Token, strScope) Do
        Begin
          If Token.UToken = strPRIVATE Then
            AScope := scPrivate
          Else If Token.UToken = strPROTECTED Then
            AScope := scProtected
          Else If Token.UToken = strPUBLIC Then
            AScope := scPublic
          Else
            AScope := scPublished;
          NextNonCommentToken;
        End;
    End;
End;

(**

  This method parses a class field list from the current token position using the following object pascal
  grammar.

  @precon  Cls is a valid object declaration to add fields too and Scope is the current scope of the 
           class.
  @postcon Returns true is field where handled and parsed.

  @param   Cls    as a TObjectDecl as a constant
  @param   AScope as a TScope as a constant
  @return  a Boolean

**)
Function TPascalModule.ClassFieldList(Const Cls: TObjectDecl; Const AScope: TScope): Boolean;

Begin
  RTTIAttributes;
  Result := FieldList(Cls, AScope);
  If Result Then
    If Token.Token = ';' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
End;

(**

  This method parses a class method list from the current token position using the following object 
  pascal grammar.

  @precon  Cls is a valid object declaration to get method for and Scope is the current scope of the 
           class.
  @postcon Returns true is method were parsed.

  @param   Cls                as a TRecordDecl as a constant
  @param   AScope             as a TScope as a constant
  @param   PermissibleMethods as a TPermissibleMethods as a constant
  @return  a Boolean

**)
Function TPascalModule.ClassMethodList(Const Cls: TRecordDecl; Const AScope: TScope;
  Const PermissibleMethods : TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses a class property list frmo the current token position using the following object 
  pascal grammar.

  @precon  Cls is a valid class declaration to get method for and Scope is the current scope of the 
           class.
  @postcon Returns true is properties were parsed.

  @param   Cls    as a TRecordDecl as a constant
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.ClassPropertyList(Const Cls: TRecordDecl; Var AScope: TScope): Boolean;

Begin
  RTTIAttributes;
  Result :=  PropertyList(Cls, AScope);
  If Result Then
    Begin
      If Token.Token = ';' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a class property list from the current token position using the following object 
  pascal grammar.

  @precon  Cls is a valid class declaration to get method for and Scope is the current scope of the 
           class.
  @postcon Returns true is properties were parsed.

  @param   Cls    as a TRecordDecl as a constant
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.PropertyList(Const Cls: TRecordDecl; Var AScope: TScope): Boolean;

Const
  strPROPERTY = 'PROPERTY';
  
Var
  tmpP : TPascalProperty;
  C : TComment;
  P: TPascalProperty;
  PropertiesLabel: TLabelContainer;
  boolIsClassProp : Boolean;

begin
  boolIsClassProp := False;
  If Token.UToken = strCLASS THEN
    Begin
      PushTokenPosition;
      NextNonCommentToken;
      boolIsClassProp := True;
    End;
  Result := Token.UToken = strPROPERTY;
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
              Token.Line, Token.Column]), scNone,  Token.Line, Token.Column, etError, Self);
          NextNonCommentToken;
          PropertyInterface(P);
          PropertySpecifiers(P);
          P.IsClassProperty := boolIsClassProp;
          PortabilityDirective;
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End Else
      If boolIsClassProp Then
        PopTokenPosition;
end;

(**

  This method parses the property interface from the current token position using the following object 
  pascal grammar.

  @precon  Prop is a property to parse an interface for.
  @postcon Parses the property interface from the current token position

  @param   Prop as a TPascalProperty as a constant

**)
Procedure TPascalModule.PropertyInterface(Const Prop : TPascalProperty);

Begin
  PropertyParameterList(Prop);
  CheckReturnValue(Prop);
End;

(**

  This method parses a properties parameter list from the current token using the following object pascal
  grammar.

  @precon  Prop is a property to parse a parameter list for.
  @postcon Parses a properties parameter list from the current token

  @param   Prop as a TPascalProperty as a constant

**)
Procedure TPascalModule.PropertyParameterList(Const Prop : TPascalProperty);

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
        If Token.UToken = strVAR Then
          ParamMod := pamVar;
        If Token.UToken = strCONST Then
          ParamMod := pamConst;
        If Token.UToken = strOUT Then
          ParamMod := pamOut;
        If ParamMod <> pamNone Then
          NextNonCommentToken;
        I := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
        Try
          IdentList(I, scNone, TIdentList, strSeekableOnErrorTokens);
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
              ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
        Finally
          I.Free;
        End;
      Until Token.Token <> ';';
      If Token.Token = ']' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses the property specifiers from the current token position using the following object 
  pascal grammar.

  @precon  Prop is a property to parse specifiers for.
  @postcon Parses the property specifiers from the current token position

  @nometrics

  @param   Prop as a TPascalProperty as a constant

**)
procedure TPascalModule.PropertySpecifiers(Const Prop: TPascalProperty);

Const
  strREAD = 'READ';
  strWRITE = 'WRITE';
  strSTORED = 'STORED';
  strDEFAULT = 'DEFAULT';
  strNODEFAULT = 'NODEFAULT';
  strIMPLEMENTS = 'IMPLEMENTS';
  strREADONLY = 'READONLY';
  strWRITEONLY = 'WRITEONLY';
  
Var
  C : TPropertySpec;
  ExprType : TPascalExprTypes;

begin
  // Check for index
  If Token.UToken = strINDEX Then
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
  If Token.UToken = strREAD Then
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
  If Token.UToken = strWRITE Then
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
  If Token.UToken = strSTORED Then
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
  If Token.UToken = strDEFAULT Then
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
  If Token.UToken = strNODEFAULT Then
    NextNonCommentToken;
  // Check for implements
  If Token.UToken = strIMPLEMENTS Then
    Begin
      NextNonCommentToken;
      IdentList(Prop.ImplementsSpec, scNone, TIdentList, strSeekableOnErrorTokens);
    End;
  If Token.UToken = strREADONLY Then
    Begin
      Prop.ReadOnlySpec := True;
      NextNonCommentToken;
    End;
  If Token.UToken = strWRITEONLY Then
    Begin
      Prop.WriteOnlySpec := True;
      NextNonCommentToken;
    End;
  If Token.UToken = strDISPID Then
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
      PushTokenPosition;
      NextNonCommentToken;
      // Check for default property
      If Token.UToken = strDEFAULT Then
        Begin
          Prop.DefaultProperty := True;
          NextNonCommentToken;
        End Else
          PopTokenPosition;
    End;
end;

(**

  This method parses an Interface declaration from the current token position using the following object 
  pascal grammar.

  @precon  None.
  @postcon Returns an interface declaration if one was parsed else nil.

  @param   AToken as a TTypeToken as a constant
  @return  a TInterfaceDecl

**)
function TPascalModule.InterfaceType(Const AToken : TTypeToken) : TInterfaceDecl;

Const
  strDISPINTERFACE = 'DISPINTERFACE';
  
Var
  InternalScope : TScope;
  T : TTypeToken;

begin
  InternalScope := scPublic;
  Result := Nil;
  If (Token.UToken = strINTERFACE) Or (Token.UToken = strDISPINTERFACE) Then
    Begin
      T := AToken;
      UpdateTypeToken(T);
      If Token.UToken = strINTERFACE Then
        Begin
          Result := TInterfaceDecl.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn,
            iiPublicInterface, T.FComment);
          Result := T.FContainer.Add(Result) as TInterfaceDecl;
          Result.Line := T.FLine;
          Result.Column := T.FColumn;
          Result.Comment := T.FComment ;
        End Else
        Begin
          Result := TDispInterfaceDecl.Create(T.FIdentifier, T.FScope, T.FLine, T.FColumn,
            iiPublicDispInterface, T.FComment);
          Result := T.FContainer.Add(Result) as TDispInterfaceDecl;
          Result.Line := T.FLine;
          Result.Column := T.FColumn;
          Result.Comment := T.FComment;
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
                    ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
                End Else
                  PopTokenPosition;
            End;
          Repeat
            If Token.UToken = strEND Then
              Break;
          Until Not (
            InterfaceMethodList(Result, InternalScope, [mtProcedure..mtFunction]) Or
            InterfacePropertyList(Result, InternalScope)
          );
          // Check for 'END' and ';'
          If Token.UToken = strEND Then
            Begin
              Result.EndLine := Token.Line;
              NextNonCommentToken;
            End Else
              ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual,
                Self);
        End;
    End;
end;

(**

  This method attempts to parse the current token position as a heritage list.

  @precon  None.
  @postcon Attempts to parse the current token position as a heritage list.

  @param   InterfaceDecl as a TInterfaceDecl as a constant

**)
Procedure TPascalModule.InterfaceHeritage(Const InterfaceDecl : TInterfaceDecl);

begin
  RecObjClsIntHeritage(InterfaceDecl); // Same as ClassHeritage
End;

(**

  This method parses the gramar for the method list of an interface.

  @precon  Cls must be a valid instance.
  @postcon The grammar element is parsed.

  @param   Cls                as a TRecordDecl as a constant
  @param   AScope             as a TScope as a constant
  @param   PermissibleMethods as a TPermissibleMethods as a constant
  @return  a Boolean

**)
function TPascalModule.InterfaceMethodList(Const Cls: TRecordDecl; Const AScope: TScope;
  Const PermissibleMethods: TPermissibleMethods): Boolean;

Begin
  Result := MethodList(Cls, AScope, PermissibleMethods);
End;

(**

  This method parses the grammar for an interace property by delegating the process to the 
  ClassPropertyList method.

  @precon  Cls must be a valid reference.
  @postcon The property list element of the grammar is parsed.

  @param   Cls    as a TRecordDecl as a constant
  @param   AScope as a TScope as a reference
  @return  a Boolean

**)
Function TPascalModule.InterfacePropertyList(Const Cls: TRecordDecl;
  Var AScope: TScope): Boolean;

Begin
  Result := ClassPropertyList(Cls, AScope);
End;

(**

  This method parses a requires clause from the current token position usnig the following object pascal 
  grammar.

  @precon  None.
  @postcon Parses a requires clause from the current token position usnig

**)
Procedure TPascalModule.RequiresClause;

Const
  strREQUIRES = 'REQUIRES';
  
Var
  R : TElementContainer;

Begin
  If Token.UToken = strREQUIRES Then
    Begin
      R := Add(strRequiresLabel, iiUsesLabel, scNone, GetComment);
      NextNonCommentToken;
      IdentList(R, scNone, TUsesList, strSeekableOnErrorTokens, iiUsesItem);
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
      NextNonCommentToken;
    End;
End;

(**

  This method parses a contains clause fro the cutrrent token position using the following object pascal 
  grammar.

  @precon  None.
  @postcon Parses a contains clause fro the cutrrent token position

**)
Procedure TPascalModule.ContainsClause;

Const
  strCONTAINS = 'CONTAINS';
  
Var
  C : TElementContainer;

Begin
  If Token.UToken = strCONTAINS Then
    Begin
      C := Add(strContainsLabel, iiUsesLabel, scNone, GetComment);
      NextNonCommentToken;
      IdentList(C, scNone, TUsesList, strSeekableOnErrorTokens, iiUsesItem);
      If Token.Token <> ';' Then
        ErrorAndSeekToken(strLiteralExpected, ';', strSeekableOnErrorTokens, stActual, Self);
      NextNonCommentToken;
    End;
End;

(**

  This method creates a identifier list starting at the current token and return the list to the calling 
  function. If OwnList is true then the identlist is added to the classes owned items list for automatic 
  disposal, else it the responsibliity of the calling function to disposal of the class.

  @precon  OwnList determines if the identlist should be disposed of be the parser or be the caller . 
           SeekTokens is a sorted lowercase list of token to find if an error is found.
  @postcon Returns an ident list.

  @param   Container      as a TElementContainer as a constant
  @param   eScope         as a TScope as a constant
  @param   ContainerClass as a TElementContainerClass as a constant
  @param   SeekTokens     as an Array Of String as a constant
  @param   iImageIndex    as a TBADIImageIndex as a constant

**)
Procedure TPascalModule.IdentList(Const Container : TElementContainer; Const eScope : TScope;
  Const ContainerClass : TElementContainerClass; Const SeekTokens : Array Of String;
  Const iImageIndex : TBADIImageIndex = iiNone);

Var
  C, AComment : TComment;
  I: TElementContainer;
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
                ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stFirst, Self);
            End;
          I := Nil;
          If Container <> Nil Then
            I := Container.Add(ContainerClass.Create(strUnit, eScope, iLine,
              iColumn, iImageIndex, AComment)) As TElementContainer;
          If Token.UToken = strIN Then
            Begin
              If I <> Nil Then
                I.AddToken(Token.Token);
              NextNonCommentToken;
              If Token.TokenType <> ttSingleLiteral Then
                ErrorAndSeekToken(strStringExpected, Token.Token, SeekTokens, stActual, Self)
              Else
                Begin
                  If I <> Nil Then
                    I.AddToken(Token.Token);
                  NextNonCommentToken;
                End;
            End;
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stFirst, Self);
    Until Not IsToken(',', Nil);
End;

(**

  This method returns a type id at the current token position using the following object pascal grammar.

  @precon  C must be a valid generic container.
  @postcon Returns a type id as a string of text.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TPascalModule.TypeId(Const Container: TElementContainer) : Boolean;

Begin
  Result := IsIdentifier(Token) Or (Token.UToken = strSTRING);
  If Result Then
    Begin
      AddToExpression(Container);
      If Token.Token = '.' Then
        Begin
          AddToExpression(Container);
          If IsIdentifier(Token) Then
            AddToExpression(Container)
          Else
            ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
        End;
    End;
End;

(**

  This method parses a parameter declaration for a generic type definition.

  @precon  None.
  @postcon A generic type parameter definition is parsed.

  @param   strIdentifier as a String as a reference

**)
procedure TPascalModule.TypeParamDecl(Var strIdentifier : String);

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
Procedure TPascalModule.TypeParamDeclList(Var strIdentifier : String);

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
Procedure TPascalModule.TypeParamList(Var strIdentifier : String);

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
        ReferenceSymbol(Token);
        NextNonCommentToken;
        Inc(iParams);
      End Else
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
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
Procedure TPascalModule.TypeParams(Var strIdentifier : String);

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
          ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses a constant expression from the current token position using the following object 
  pascal grammar.

  @precon  C is a generic container to add tokens too.
  @postcon Returns true if a constant expression was parsed.

  @param   Container as a TElementContainer as a constant
  @param   ExprType  as a TPascalExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.ConstExpr(Const Container : TElementContainer;
  Var ExprType : TPascalExprTypes) : Boolean;

Var
  iStartIndex : Integer;

Begin
  Result := True;
  iStartIndex := Token.BufferPos;
  Expression(Container, ExprType); // ConstExpr is a subset of Expression
  If iStartIndex = Token.BufferPos Then
    ErrorAndSeekToken(strConstExprExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
End;

(**

  This method processes a compiler directive looking for conditional statements.

  @precon  None.
  @postcon Processes a compiler directive looking for conditional statements.

  @nometrics

  @param   iSkip as an Integer as a reference

**)
procedure TPascalModule.ProcessCompilerDirective(var iSkip : Integer);

  (**

    This function returns the definition string from the current compiler directive.

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

    This function checks to see if the string of text starts with the passed start string.

    @precon  None.
    @postcon Returns true if the string starts match.

    @param   strText  as a String as a constant
    @param   strStart as a String as a constant
    @return  a Boolean

  **)
  Function Like(Const strText, strStart : String) : Boolean;

  Begin
    Result := False;
    If Length(strText) >= Length(strStart) Then
      Result := CompareText(Copy(strText, 1, Length(strStart)), strStart) = 0;
  End;

  (**

    This method adds the number to the stack and increments the iSkip variable by the value passed.

    @precon  None.
    @postcon Adds the number to the stack and increments the iSkip variable by the value passed.

    @param   iCompilerDefType   as a TCompilerDefType as a constant
    @param   iCompilerCondition as a TCompilerCondition as a constant

  **)
  Procedure IncSkip(Const iCompilerDefType : TCompilerDefType;
    Const iCompilerCondition : TCompilerCondition);

  Begin
    CompilerConditionStack.Push(iCompilerDefType, iCompilerCondition, TokenIndex);
    If iCompilerCondition = ccIncludeCode Then
      Inc(iSkip);
  End;

  (**

    This function removes the number from the stack and decrements the iSkip variable by 1. Note this 
    also added the removed value to the UNDO stack.

    @precon  None.
    @postcon Removes the number from the stack and decrements the iSkip variable by 1.

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

Const
  strCDDEFINE = '{$DEFINE ';
  strCDUNDEF = '{$UNDEF ';
  strCDIFDEF = '{$IFDEF ';
  strCDIFOPT = '{$IFOPT ';
  strCDIF = '{$IF ';
  strCDIFNDEF = '{$IFNDEF ';
  strCDELSE = '{$ELSE';
  strCDENDIF = '{$ENDIF';
  strCDIFEND = '{$IFEND';
  strCDEXTERNALSYM = '{$EXTERNALSYM';

Var
  CompilerCondition : TCompilerConditionData;

begin
  If Like(Token.Token, strCDDEFINE) Then
    AddDef(GetDef)
  Else If Like(Token.Token, strCDUNDEF) Then
    DeleteDef(GetDef)
  Else If Like(Token.Token, strCDIFDEF) Then
    Begin
      If Not IfDef(GetDef) Then
        IncSkip(cdtIFDEF, ccIncludeCode)
      Else
        IncSkip(cdtIFDEF, ccExcludeCode);
    End
  Else If Like(Token.Token, strCDIFOPT) Then
    Begin
      If Not IfDef(GetDef) Then
        IncSkip(cdtIFDEF, ccIncludeCode)
      Else
        IncSkip(cdtIFDEF, ccExcludeCode);
    End
  Else If Like(Token.Token, strCDIF) Then
    IncSkip(cdtIFDEF, ccExcludeCode) // FAKE $IF by defaulting to TRUE
  Else If Like(Token.Token, strCDIFNDEF) Then
    Begin
      If Not IfNotDef(GetDef) Then
        IncSkip(cdtIFNDEF, ccIncludeCode)
      Else
        IncSkip(cdtIFNDEF, ccExcludeCode);
    End
  Else If Like(Token.Token, strCDELSE) Then
    Begin
      If CompilerConditionStack.CanPop Then
        Begin
          CompilerCondition := CompilerConditionStack.Peek;
          If CompilerCondition.CompilerCondition = ccIncludeCode Then
            Begin
              CompilerConditionStack.Push(cdtELSE, ccExcludeCode, CompilerCondition.TokenIndex);
              Dec(iSkip);
            End Else
            Begin
              CompilerConditionStack.Push(cdtELSE, ccIncludeCode, CompilerCondition.TokenIndex);
              Inc(iSkip);
            End;
        End Else
          AddIssue(Format(strElseIfMissingIfDef, [Token.Line, Token.Column]),
              scGlobal, Token.Line, Token.Column, etError, Self);
    End
  Else If Like(Token.Token, strCDENDIF) Then
    Begin
      If TokenStackTop > 0 Then
        Begin
          CompilerConditionStack.Push(cdtENDIF, ccIncludeCode, TokenIndex);
          Dec(iSkip);
        End Else
          If DecSkip Then
            AddIssue(Format(strEndIfMissingIfDef, [Token.Line, Token.Column]),
              scGlobal, Token.Line, Token.Column, etError, Self);

    End
  Else If Like(Token.Token, strCDIFEND) Then
    Begin
      If DecSkip Then
        AddIssue(Format(strEndIfMissingIfDef, [Token.Line, Token.Column]),
            scGlobal, Token.Line, Token.Column, etError, Self);
    End
  Else If Like(Token.Token, strCDINCLUDE) Then
    ProcessIncludeDirective(Token.Token)
  Else If Like(Token.Token, strCDEXTERNALSYM) Then
    FExternalSyms.Add(GetDef);
  If iSkip < 0 Then
    iSkip := 0;
end;

(**

  This method processes an INCLUDE directive and injects the INC file code into the parser after the 
  current location.

  @precon  None.
  @postcon The parser tokens from the include file are inserted after the current token.

  @param   strToken as a String as a constant

**)
Procedure TPascalModule.ProcessIncludeDirective(Const strToken: String);

ResourceString
  strINCLUDEFileDoesNotExist = 'The INCLUDE file "%s" does not exist!';

Const
  iQuotePadding = 2;

Var
  strFileName : String;
  sl : TStringList;
  M : TPascalModule;
  iToken: Integer;
  T : TTokenInfo;

Begin
  strFileName := strToken;
  Delete(strFileName, 1, Length(strCDINCLUDE));
  strFileName := Trim(strFileName);
  If (Length(strFileName) > 0) And (strFileName[Length(strFileName)] = '}') Then
    strFileName := Copy(strFileName, 1, Length(strFileName) - 1);
  If (Length(strFileName) > 0) And (strFileName[1] = '''') Then
    strFileName := Copy(strFileName, iQuotePadding, Length(strFileName) - iQuotePadding);
  If Pos(':', strFileName) = 0 Then
    strFileName := ExpandFileName(ExtractFilePath(FileName) + strFileName);
  If Not FileExists(strFileName) Then
    AddIssue(Format(strINCLUDEFileDoesNotExist, [strFileName]), scNone,
      Token.Line, Token.Column, etError, Self)
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

  This method find unresolved implemented methods, both within objects and classes and simple procedures 
  and function and outputs an error if they are unresolved.

  @precon  None.
  @postcon Find unresolved implemented methods, both within objects and classes and simple procedures 
           and function and outputs an error if they are unresolved.

  @param   StartLabel as a TLabelContainer as a constant

**)
procedure TPascalModule.FindUnresolvedImplementedClassMethods(
  Const StartLabel : TLabelContainer);

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
                  scNone, Method.Line, Method.Column, etWarning, Method);
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

  @param   slClassNames as a TStringList as a constant
  @return  a TRecordDecl

**)
Function TPascalModule.FindRecObjClsInt(Const slClassNames: TStringList): TRecordDecl;

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
              scNone, Method.Line, Method.Column, etWarning, Method);
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

  This method finds all the unresolved object and class methods in a recursive manner to capture any 
  private classes of classes.

  @precon  None.
  @postcon Finds all the unresolved object and class methods in a recursive manner to capture any 
           private classes of classes.

  @param   TypeLabel as a TLabelContainer as a constant

**)
procedure TPascalModule.FindUnresolvedRecordObjectAndClassMethods(Const TypeLabel : TLabelContainer);

  (**

    This function walks backwards through the heirarchy to find all the qualifying objects and classes.

    @precon  None.
    @postcon Walks backwards through the heirarchy to find all the qualifying objects and classes.

    @param   RecObjOrCls as a TRecordDecl as a constant
    @return  a String

  **)
  Function GetClassQualification(Const RecObjOrCls : TRecordDecl) : String;

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

    @param   Method as a TPascalMethod as a constant
    @return  a Boolean

  **)
  Function ShouldMethodHaveImplementation(Const Method : TPascalMethod) : Boolean;

  Const
    strLCAbstract = 'abstract';
    
  Begin
    Result := Not Method.Resolved;
    Result := Result And Not Method.HasDirective(strLCAbstract);
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
                        scNone, Method.Line, Method.Column, etWarning, Method);
                  End;
            ClassTypeLabel := RecordObjectOrClass.FindElement(strTypesLabel) As TLabelContainer;
            If ClassTypeLabel <> Nil Then
              FindUnresolvedRecordObjectAndClassMethods(ClassTypeLabel);
          End;
      end;
end;

(**

  This method resolved method and procedures that have been forward referenced.

  @precon  None.
  @postcon The implementation of a forward referenced method is updates wih the refernece information
           for the forward declaration.

  @nometrics

**)
Procedure TPascalModule.ResolveForwardImplementedMethods;

Var
  iElement: Integer;
  ForwardMethod: TPascalMethod;
  iImplementation: Integer;
  ImplementedMethod: TPascalMethod;

Begin
  If Assigned(FImplementedMethodsLabel) Then
    For iElement := 1 To FImplementedMethodsLabel.ElementCount Do
      If FImplementedMethodsLabel.Elements[iElement] Is TPascalMethod Then
        Begin
          ForwardMethod := FImplementedMethodsLabel.Elements[iElement] As TPascalMethod;
          If ForwardMethod.ForwardDecl Then
            For iImplementation := 1 To FImplementedMethodsLabel.ElementCount Do
              If FImplementedMethodsLabel.Elements[iImplementation] Is TPascalMethod Then
                Begin
                  ImplementedMethod := FImplementedMethodsLabel.Elements[iImplementation] As TPascalMethod;
                  If ForwardMethod <> ImplementedMethod Then
                    If CompareText(Copy(ForwardMethod.Name, 1, Length(ImplementedMethod.Name)),
                      ImplementedMethod.Name) = 0 Then
                      Begin
                        If Not ImplementedMethod.Referenced And ForwardMethod.Referenced Then
                          ImplementedMethod.Referenced := True;
                      End;
                End;
        End;
End;

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

  This method resolved the references between the exports methods and implemented methods marking each as
  resovled where a match is found.

  @precon  None.
  @postcon Resolved the references between the exports methods and implemented methods marking each as 
           resovled where a match is found.

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

  This method searches the types tree for the declarations of the methods found in the implemented 
  methods element and marks elements as resolved.

  @precon  None.
  @postcon Searches the types tree for the declarations of the methods found in the implemented methods 
           element and marks elements as resolved.

  @param   StartLabel as a TLabelContainer as a constant

**)
procedure TPascalModule.ResolveScopeOfImplementedMethods(
  Const StartLabel : TLabelContainer);

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
                  [Method.QualifiedName]), scNone, Method.Line, Method.Column, etWarning, Method);
        End Else
          ResolveScopeOfImplementedMethods(
            StartLabel.Elements[i] As TLabelContainer);
end;

(**

  This method determines if the given token is a valid identifier.

  @precon  Token must be a valid instance.
  @postcon Returns true if the given token is a valid Object Pascal identifier.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function  TPascalModule.IsIdentifier(Const AToken : TTokenInfo) : Boolean;

Begin
  Result := (AToken.TokenType In [ttIdentifier, ttDirective]) Or
    ((AToken.TokenType In [ttReservedWord]) And
      IsKeyWord(AToken.Token, strIdentifierReservedWords));
End;

End.
