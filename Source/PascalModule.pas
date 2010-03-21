(**

  ObjectPascalModule : A unit to tokenize Pascal source code.

  @Version    1.0
  @Date       21 Mar 2010
  @Author     David Hoyle

  @todo       Implement an expression parser for the above compiler defines.
              Needs to be able to evaluate constants in the code and use the
              two functions Defined() and Declared().

**)
Unit PascalModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A pascal specific implementation of comments. **)
  TPascalComment = Class(TComment)
  Public
    Class Function CreateComment(strComment: String; iLine,
      iCol: Integer): TComment; Override;
  End;

  (** This is an enumerate to describe the type of constant expression. **)
  TExprType = (etUnknown, etConstExpr, etString, etInteger, etFloat);
  (** This is a set of TExprType enumerates. **)
  TExprTypes = Set of TExprType;

  (** This class represents a list of identifiers **)
  TIdentList = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This class represents a temporary list / collection **)
  TTempCntr = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a sub class for general type types **)
  TTypes = Class(TGenericTypeDecl)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a sub class for restricted type types **)
  TRestrictedType = Class(TGenericTypeDecl);

  (** This is a sub class for typeid types **)
  TTypeID = Class(TTypes);
  (** This is a sub class for Simple Type types **)
  TSimpleType = Class(TTypes);
  (** This is a sub class for Structured Type types **)
  TStrucType = Class(TTypes);
  (** This is a sub class for String types **)
  TStringType = Class(TTypes);
  (** This is a sub class for Procedure types **)
  TProcedureType = Class(TTypes);
  (** This is a sub class for Variant types **)
  TVariantType = Class(TTypes);
  (** This is a sub class for Class Ref types **)
  TClassRefType = Class(TTypes);

  (** This is a sub class for Ordinal types **)
  TOrdinalType = Class(TSimpleType);
  (** This is a sub class for Real types **)
  TRealType = Class(TSimpleType);

  (** This is a sub class for the Real48 type. **)
  TReal48 = Class(TRealType);
  (** This is a sub class for the Real type. **)
  TReal = Class(TRealType);
  (** This is a sub class for the Single type. **)
  TSingle = Class(TRealType);
  (** This is a sub class for the Double type. **)
  TDouble = Class(TRealType);
  (** This is a sub class for the Extended type. **)
  TExtended = Class(TRealType);
  (** This is a sub class for the Currency type. **)
  TCurrency = Class(TRealType);
  (** This is a sub class for the Complex type. **)
  TComp = Class(TRealType);

  (** This is a sub class for the SubRange type. **)
  TSubRangeType = Class(TOrdinalType);
  (** This is a sub class for the Enumerate type. **)
  TEnumerateType = Class(TOrdinalType);
  (** This is a sub class for the OrdIdent type. **)
  TOrdIdent = Class(TOrdinalType);

  (** This is a sub class for the ShortInt type. **)
  TShortInt = Class(TOrdIdent);
  (** This is a sub class for the SmallInt type. **)
  TSmallInt = Class(TOrdIdent);
  (** This is a sub class for the Integer type. **)
  TInteger = Class(TOrdIdent);
  (** This is a sub class for the Byte type. **)
  TByte = Class(TOrdIdent);
  (** This is a sub class for the LongInt type. **)
  TLongInt = Class(TOrdIdent);
  (** This is a sub class for the Int64 type. **)
  TInt64 = Class(TOrdIdent);
  (** This is a sub class for the Word type. **)
  TWord = Class(TOrdIdent);
  (** This is a sub class for the Boolean type. **)
  TBoolean = Class(TOrdIdent);
  (** This is a sub class for the Char type. **)
  TChar = Class(TOrdIdent);
  (** This is a sub class for the WideChar type. **)
  TWideChar = Class(TOrdIdent);
  (** This is a sub class for the LongWord type. **)
  TLongWord = Class(TOrdIdent);
  (** This is a sub class for the PChar type. **)
  TPChar = Class(TOrdIdent);

  (** This is a sub class for the Variant type. **)
  TVariant = Class(TVariantType);
  (** This is a sub class for the OLEVariant type. **)
  TOLEVariant = Class(TVariantType);

  (** This is a sub class for the String type. **)
  TString = Class(TStringType);
  (** This is a sub class for the AnsiString type. **)
  TAnsiString = Class(TStringType);
  (** This is a sub class for the WideString type. **)
  TWideString = Class(TStringType);
  (** This is a sub class for the ShortString type. **)
  TShortString = Class(TStringType);

  (** This is a sub class for Array types **)
  TArrayType = Class(TStrucType)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDimensions : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure AddDimension;
    (**
      This property defines the number of dmiensions that the array contains.
      @precon  None.
      @postcon Returns the number of dimension that the array contains.
      @return  an Integer
    **)
    Property Dimensions : Integer Read FDimensions;
  End;

  (** This is a sub class for Set types **)
  TSetType = Class(TStrucType);
  (** This is a sub class for File types **)
  TFileType = Class(TStrucType);
  (** This is a sub class for Pointer types **)
  TPointerType = Class(TTypes);

  (** This is a class that represents a record definition. **)
  TRecordDecl = Class(TRestrictedType)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPacked      : Boolean;
    FFieldsLabel : TLabelContainer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; iImageIndex : TImageIndex; AComment : TComment); Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function  AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function  ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    (**
      Returns whether the record is packed or not.
      @precon  None.
      @postcon Returns whether the record is packed or not.
      @return  a Boolean
    **)
    Property IsPacked : Boolean Read FPacked Write FPacked;
    (**
      This property caches the fields label IF it exists.
      @precon  None.
      @postcon Returns the reference to the fields label.
      @return  a TLabelContainer
    **)
    Property FieldsLabel : TLabelContainer Read FFieldsLabel Write FFieldsLabel;
  End;

  (** A class to represent a Object Pascal Parameter. **)
  TPascalParameter = Class(TGenericParameter)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TObjectDecl = Class;

  (** This is a class that defines method within Object Pascal code. **)
  TPascalMethod = Class(TGenericMethodDecl)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDirectives           : TStringList;
    FResolved             : Boolean;
    FObjClsInt            : TObjectDecl;
    FTypesLabel           : TLabelContainer;
    FVariablesLabel       : TLabelContainer;
    FConstantsLabel       : TLabelContainer;
    FResourceStringsLabel : TLabelContainer;
    FLabelsLabel          : TLabelContainer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Constructor Create(MethodType : TMethodType; strName : String; AScope : TScope;
      iLine, iCol : Integer); Override;
    Destructor Destroy; Override;
    Procedure AddDirectives(strDirective : String);
    Function HasDirective(strDirective : String) : Boolean;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    (**
      Returns the string list of directives associated with the method.
      @precon  None.
      @postcon Returns the string list of directives associated with the method.
      @return  a TStringList
    **)
    Property Directives : TStringList Read FDirectives;
    (**
      This property returns whether the method is resovled forward or not.
      @precon  None.
      @postcon Returns whether the method is resovled forward or not.
      @return  a Boolean
    **)
    Property Resolved : Boolean Read FResolved Write FResolved;
    (**
      This property returns the object/class that the method belongs to.
      @precon  None.
      @postcon Returns the object/class that the method belongs to.
      @return  a TObjectDecl
    **)
    Property ObjClsInt : TObjectDecl Read FObjClsInt Write FObjClsInt;
    (**
      This property gets or sets the types label for the module.
      @precon  None.
      @postcon Gets or sets the types label for the module.
      @return  a TLabelContainer
    **)
    Property TypesLabel  : TLabelContainer Read FTypesLabel Write FTypesLabel;
    (**
      This property gets or sets the Variables label for the module.
      @precon  None.
      @postcon Gets or sets the Variables label for the module.
      @return  a TLabelContainer
    **)
    Property VariablesLabel : TLabelContainer Read FVariablesLabel
      Write FVariablesLabel;
    (**
      This property gets or sets the Constants label for the module.
      @precon  None.
      @postcon Gets or sets the Constants label for the module.
      @return  a TLabelContainer
    **)
    Property ConstantsLabel : TLabelContainer Read FConstantsLabel
      Write FConstantsLabel;
    (**
      This property gets or sets the Resource Strings label for the module.
      @precon  None.
      @postcon Gets or sets the Resource Strings label  for the module.
      @return  a TLabelContainer
    **)
    Property ResourceStringsLabel : TLabelContainer Read FResourceStringsLabel
      Write FResourceStringsLabel;
    (**
      This property gets or sets the Labels label for the module.
      @precon  None.
      @postcon Gets or sets the Labels label for the module.
      @return  a TLabelContainer
    **)
    Property LabelsLabel : TLabelContainer Read FLabelsLabel Write FLabelsLabel;
  End;

  (** This is a class that defines properties with Object Pascal code. **)
  TPascalProperty = Class(TGenericProperty)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FIndexSpec : String;
    FWriteSpec: String;
    FImplementsSpec: TIdentList;
    FStoredSpec: String;
    FDefaultSpec: String;
    FReadSpec: String;
    FDefaultProperty: Boolean;
    FDispIDSpec: String;
    FReadOnlySpec: Boolean;
    FWriteOnlySpec: Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Constructor Create(strIdent: String; AScope: TScope; iLine, iCol : Integer;
      AImageIndex : TImageIndex; AComment : TComment); Override;
    Destructor Destroy; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      Returns the
      @precon  None.
      @postcon Returns the
      @return  a String
    **)
    Property IndexSpec : String Read FIndexSpec Write FIndexSpec;
    (**
      Returns the properties Read specification.
      @precon  None.
      @postcon Returns the properties Read specification.
      @return  a String
    **)
    Property ReadSpec : String Read FReadSpec Write FReadSpec;
    (**
      Returns the properties write specification.
      @precon  None.
      @postcon Returns the properties write specification.
      @return  a String
    **)
    Property WriteSpec : String Read FWriteSpec Write FWriteSpec;
    (**
      Returns the properties Stored specification.
      @precon  None.
      @postcon Returns the properties Stored specification.
      @return  a String
    **)
    Property StoredSpec : String Read FStoredSpec Write FStoredSpec;
    (**
      Returns the property default value.
      @precon  None.
      @postcon Returns the property default value.
      @return  a String
    **)
    Property DefaultSpec : String Read FDefaultSpec Write FDefaultSpec;
    (**
      Returns whether this property is the classes / interfaces default
      @precon  None.
      @postcon Returns whether this property is the classes / interfaces default
      property.
      @return  a Boolean
    **)
    Property DefaultProperty : Boolean Read FDefaultProperty Write FDefaultProperty;
    (**
      Returns the implements specification for the property.
      @precon  None.
      @postcon Returns the implements specification for the property.
      @return  a TIdentList
    **)
    Property ImplementsSpec : TIdentList Read FImplementsSpec Write FImplementsSpec;
    (**
      Returns the properties DispID reference.
      @precon  None.
      @postcon Returns the properties DispID reference.
      @return  a String
    **)
    Property DispIdSpec : String Read FDispIDSpec Write FDispIDSpec;
    (**
      Returns whether the property has a ReadOnly specification.
      @precon  None.
      @postcon Returns whether the property has a ReadOnly specification.
      @return  a Boolean
    **)
    Property ReadOnlySpec : Boolean Read FReadOnlySpec Write FReadOnlySpec;
    (**
      Returns whether the property has a WriteOnly specification.
      @precon  None.
      @postcon Returns whether the property has a WriteOnly specification.
      @return  a Boolean
    **)
    Property WriteOnlySpec : Boolean Read FWriteOnlySpec Write FWriteOnlySpec;
  End;

  (** This class defines a property specifier. **)
  TPropertySpec = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a class the extends the record definition to handle an object
  definition **)
  TObjectDecl = Class(TRecordDecl)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FHeritage : TIdentList;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; iImageIndex : TImageIndex; AComment : TComment); Override;
    Destructor Destroy; Override;
    procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    (**
      Returns a reference to the object class heritage.
      @precon  None.
      @postcon Returns a reference to the object class heritage.
      @return  a TIdentList
     **)
    Property Heritage : TIdentList Read FHeritage;
  End;

  (** This is a class the extends the object definition to handle an class
  definition **)
  TClassDecl = Class(TObjectDecl)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FAbstractClass: Boolean;
    FSealedClass  : Boolean;
    FHelper       : Boolean;
    FHelperClass  : String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    (**
      This property defined whether the class is abstract or not.
      @precon  None.
      @postcon None.
      @return  a Boolean
    **)
    Property AbstractClass : Boolean Read FAbstractClass Write FAbstractClass;
    (**
      This property defines whether the class is sealed or not.
      @precon  None.
      @postcon None.
      @return  a Boolean
    **)
    Property SealedClass : Boolean Read FSealedClass Write FSealedClass;
    (**
      This property gets or sets whether the class is a helper class.
      @precon  None.
      @postcon Gets or sets whether the class is a helper class.
      @return  a Boolean
    **)
    Property HelperClass : Boolean Read FHelper Write FHelper;
    (**
      This property gets or sets the class name of the class to be helped.
      @precon  None.
      @postcon Gets or sets the class name of the class to be helped.
      @return  a String
    **)
    Property HelperClassName : String Read FHelperClass Write FHelperClass;
  End;

  (** This is a class the extends the class definition to handle an interface
  definition **)
  TInterfaceDecl = Class(TClassDecl)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FGUID : String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      Returns the GUID for the interface.
      @precon  None.
      @postcon Returns the GUID for the interface.
      @return  a String
    **)
    Property GUID : String Read FGUID Write FGUID;
  End;

  (** This is a class the extends the class definition to handle an interface
  definition **)
  TDispInterfaceDecl = Class(TInterfaceDecl)
  {$IFDEF D2005} Strict {$ENDIF} private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a sub class for all constants. **)
  TConstant = Class(TGenericConstant)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FTyped : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property determines it the constant is typed or simple.
      @precon  None.
      @postcon Sets or gets whether the constant is typed or simple.
      @return  a Boolean
    **)
    Property Typed : Boolean Read FTyped Write FTyped;
  End;

  (** This is a sub class for all resource strings. **)
  TResourceString = Class(TConstant)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a sub class for all variables. **)
  TVar = Class(TGenericVariable)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;
  (** This is a sub class for all thread variables. **)
  TThreadVar = Class(TVar)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This class presents a field in a record, object, or class. **)
  TField = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This class represents an exported method. **)
  TExportsItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FResolved : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets whether the symbol is resolved.
      @precon  None.
      @postcon Gets and sets whether the symbol is resolved.
      @return  a Boolean
    **)
    Property Resolved : Boolean Read FResolved Write FResolved;
  End;

  (** A class to represent the initialization section **)
  TInitializationSection = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** A class to represent the finalization section **)
  TFinalizationSection = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a record to represent the token information for type declaration. **)
  TTypeToken = Record
    FToken       : TTokenInfo;
    FScope       : TScope;
    FComment     : TComment;
    FContainer   : TElementContainer;
  End;

  (** An enumerate for the types of reference changes that can be done. **)
  TRefType = (rtFields, rtVariables, rtConstants, rtResourceStrings, rtTypes,
    rtClassVars);
  (** A set of reference checks that need to be undertaken. **)
  TRefTypes = Set of TRefType;

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
    FModuleType : TModuleType;
    { Grammar Parsers }
    Procedure Goal;
    Function OPProgram : Boolean;
    Function OPUnit : Boolean;
    Function OPPackage : Boolean;
    Function OPLibrary : Boolean;
    Procedure ProgramBlock;
    procedure UsesClause;
    Function PortabilityDirective : Boolean;
    Procedure InterfaceSection;
    Procedure InterfaceDecl;
    Function ExportedHeading(Container : TElementContainer) : Boolean;
    Procedure ImplementationSection;
    Procedure Block(AScope : TScope; Method : TPascalMethod);
    Function ExportsStmt : Boolean;
    procedure ExportsItem(Container : TElementContainer);
    Procedure DeclSection(AScope : TScope; Container : TElementContainer);
    Function LabelDeclSection(Container : TElementContainer) : Boolean;
    Function ConstSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function ConstantDecl(AScope : TScope; Container : TElementContainer) : Boolean;
    Function ResStringSection(AScope: TScope): Boolean;
    Function ResourceStringDecl(AScope: TScope; Container : TElementContainer): Boolean;
    Function TypeSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function TypeDecl(AScope : TScope; Container : TElementContainer) : Boolean;
    Function GetTypeDecl(AToken : TTypeToken) : TGenericTypeDecl;
    Function TypedConstant(C: TElementContainer; T : TGenericTypeDecl) : Boolean;
    Function ArrayConstant(C: TElementContainer; T : TGenericTypeDecl) : Boolean;
    Function RecordConstant(C: TElementContainer; T : TGenericTypeDecl) : Boolean;
    Function RecordFieldConstant(C : TElementContainer; T : TGenericTypeDecl) : Boolean;
    function OPType(AToken : TTypeToken) : TGenericTypeDecl;
    function RestrictedType(AToken : TTypeToken) : TRestrictedType;
    Function ClassRefType(AToken : TTypeToken) : TClassRefType;
    Function SimpleType(AToken : TTypeToken) : TSimpleType;
    function RealType(AToken : TTypeToken) : TRealType;
    function OrdinalType(AToken : TTypeToken) : TOrdinalType;
    function OrdIdent(AToken : TTypeToken) : TOrdIdent;
    Function VariantType(AToken : TTypeToken) : TVariantType;
    function SubRangeType(AToken : TTypeToken) : TSubRangeType;
    function EnumerateType(AToken : TTypeToken) : TEnumerateType;
    Procedure EnumerateElement(EnumerateType : TEnumerateType);
    Function StringType(AToken : TTypeToken) : TStringType;
    Function StrucType(AToken : TTypeToken) : TGenericTypeDecl;
    function ArrayType(boolPacked : Boolean; AToken : TTypeToken): TArrayType;
    Function RecType(boolPacked : Boolean; AToken : TTypeToken) : TRecordDecl;
    Function FieldList(Rec : TRecordDecl; InternalScope : TScope) : Boolean;
    Function FieldDecl(Rec: TRecordDecl; InteralScope : TScope) : Boolean;
    procedure RecVariant(Rec: TRecordDecl; InternalScope : TScope);
    function SetType(boolPacked: Boolean; AToken : TTypeToken): TSetType;
    function FileType(boolPacked: Boolean; AToken : TTypeToken): TFileType;
    Function VariantSection(Rec: TRecordDecl; InternalScope : TScope) : Boolean;
    Function PointerType(AToken : TTypeToken) : TPointerType;
    Function ProcedureType(AToken : TTypeToken) : TProcedureType;
    Function VarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function ClassVarSection(AScope : TScope; Cls : TRecordDecl) : Boolean;
    Function ThreadVarSection(AScope : TScope; Container : TElementContainer) : Boolean;
    Function VarDecl(AScope : TScope; VarSection : TElementContainer;
      AImageIndex : TImageIndex) : Boolean;
    Function ThreadVarDecl(AScope : TScope; VarSection : TElementContainer) : Boolean;
    Procedure Expression(C : TElementContainer; var ExprType : TExprTypes);
    Procedure SimpleExpression(C : TElementContainer; var ExprType : TExprTypes);
    Procedure Term(C : TElementContainer; var ExprType : TExprTypes);
    Procedure Factor(C : TElementContainer; var ExprType : TExprTypes);
    Function RelOp(C : TElementContainer; ExprType : TExprTypes) : Boolean;
    Function AddOp(C : TElementContainer) : Boolean;
    Function MulOp(C : TElementContainer; var ExprType : TExprTypes) : Boolean;
    Function Designator(C : TElementContainer; var ExprType : TExprTypes) : Boolean;
    Procedure DesignatorSubElement(C : TElementContainer; var ExprType : TExprTypes;
      strValidSymbols : Array of String);
    Function SetConstructor(C : TElementContainer) : Boolean;
    Procedure SetElement(C : TElementContainer);
    Procedure ExprList(C : TElementContainer);
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
    Function ProcedureDeclSection(AScope : TScope) : Boolean;
    Function ProcedureDecl(AScope : TScope) : TPascalMethod;
    Function FunctionDecl(AScope : TScope) : TPascalMethod;
    Function ConstructorDecl(AScope : TScope) : TPascalMethod;
    Function DestructorDecl(AScope : TScope) : TPascalMethod;
    Function OperatorDecl(AScope : TScope) : TPascalMethod;
    Function FunctionHeading(AScope :TScope; Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;
    Function ProcedureHeading(AScope : TScope; Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;
    Function OperatorHeading(AScope :TScope; Container : TElementContainer; boolIdent : Boolean = True) : TPascalMethod;
    Procedure FormalParameter(Method : TPascalMethod);
    Procedure FormalParam(Method : TPascalMethod);
    Procedure Parameter(Method : TPascalMethod; ParamMod : TParamModifier);
    Procedure Directive(M : TPascalMethod; boolGrammarFix : Boolean = False);
    Function ObjectType(AToken : TTypeToken) : TObjectDecl;
    Procedure ObjHeritage(ObjDecl : TObjectDecl);
    Function MethodList(Cls : TRecordDecl; AScope : TScope) : Boolean;
    function MethodHeading(Cls: TRecordDecl; AScope: TScope): Boolean;
    Function ConstructorHeading(AScope :TScope; Container : TElementContainer) : TPascalMethod;
    Function DestructorHeading(AScope :TScope; Container : TElementContainer) : TPascalMethod;
    Function ObjFieldList(Cls : TObjectDecl; AScope : TScope) : Boolean;
    Procedure InitSection;
    Function ClassType(AToken : TTypeToken) : TClassDecl;
    Procedure ClassHeritage(Cls : TObjectDecl);
    procedure ClassVisibility(var AScope : TScope);
    Function ClassFieldList(Cls : TObjectDecl; AScope : TScope) : Boolean;
    Function ClassMethodList(Cls : TRecordDecl; AScope : TScope) : Boolean;
    Function ClassPropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Function PropertyList(Cls : TRecordDecl; var AScope : TScope) : Boolean;
    Procedure PropertyInterface(Prop : TPascalProperty);
    Procedure PropertyParameterList(Prop : TPascalProperty);
    Procedure PropertySpecifiers(Prop : TPascalProperty);
    Function  InterfaceType(AToken : TTypeToken) : TInterfaceDecl;
    Procedure InterfaceHeritage(InterfaceDecl : TInterfaceDecl);
    Procedure RequiresClause;
    procedure ContainsClause;
    Procedure IdentList(Container : TElementContainer; SeekTokens : Array Of String;
      iImageIndex : TImageIndex = iiNone);
    Function TypeId(Container: TElementContainer) : Boolean;
    Function ConstExpr(Container: TElementContainer; var ExprType : TExprTypes) : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    procedure ArrayElement(C : TElementContainer; iStartDimension: Integer; AT : TArrayType);
    Function CheckReturnValue(Method : TPascalMethod) : Boolean;
    Procedure CheckAlias(Method : TPascalMethod);
    Function CheckNumberType(ExprType : TExprTypes) : Boolean;
    Procedure UpdateTypeToken(var AToken : TTypeToken); {$IFDEF D2005} InLine; {$ENDIF}
    Procedure AddToContainer(Container : TElementContainer; var Method : TPascalMethod);
    Procedure TidyUpEmptyElements;
    Procedure CheckUnResolvedMethods;
    procedure ResolveScopeOfImplementedClassMethods(StartLabel : TLabelContainer);
    procedure ResolveScopeOfImplementedExportedMethods;
    procedure ResolveScopeOfImplementedExportsMethods;
    procedure FindUnresolvedObjectAndClassMethods(TypeLabel : TLabelContainer);
    procedure FindUnresolvedExportedMethods;
    {procedure FindUnresolvedExportsMethods;}
    procedure FindUnresolvedImplementedClassMethods(StartLabel : TLabelContainer);
    Function FindObjClsInt(slClassNames : TStringList) : TObjectDecl;
    procedure ProcessClsIdents(Container : TElementContainer;
      slClassNames : TStringList; var strIdentifier : String; var iLine,
      iColumn : Integer);
    Procedure EatDotNETAttribute;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function GetCurrentMethod: TPascalMethod;
    Function GetModuleName : String; Override;
    Function GetComment(
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
    Constructor CreateParser(Source : String; strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function KeyWords : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Resourcestring
  (** This is an error message for rendering a temporay container - SHOULDN'T do this **)
  strTriedToRenderTmpCntr = 'Tried to Render a Temporary Container!';
  (** This is an error message for duplicate identifiers. **)
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';

Const
  (** A set of characters for alpha characaters **)
  strTokenChars : Set Of AnsiChar = ['#', '_', 'a'..'z', 'A'..'Z'];
  (** A set of numbers **)
  strNumbers : Set Of AnsiChar = ['$', '0'..'9'];
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', ',', '-', '.', '/', ':',
    ';', '<', '=', '>', '@', '[', ']', '^'];
  (** A set of characters for quotes **)
  strQuote : Set Of AnsiChar = [''''];
  (** A string representing the Array Of parameter type. **)
  strArrayOf : Array[False..True] Of String = ('', 'Array Of ');

  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[0..73] Of String = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto', 'else',
    'end', 'except', 'exports', 'file', 'finalization', 'finally', 'for',
    'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library', 'mod',
    'nil', 'not', 'object', 'of', 'on', 'or', 'out', 'packed',
    'private', 'procedure', 'program', 'property', 'protected', 'public', 'published', 'raise', 'record', 'repeat',
    'resourcestring', 'sealed', 'set', 'shl', 'shr', 'static', 'strict', 'string',
    'then', 'threadvar', 'to', 'try', 'type', 'unit', 'unsafe', 'until', 'uses',
    'var', 'while', 'with', 'xor'
  );
  (** A sorted list of directives. Used for identifying tokens as
  directives. **)
  strDirectives : Array[0..44] Of String = (
    'absolute', 'abstract', 'assembler', 'automated', 'cdecl', 'contains',
    'default', 'deprecated', 'dispid', 'dynamic', 'export', 'external', 'far',
    'final', 'forward', 'helper', 'implements', 'index', 'inline', 'library', 'local',
    'message', 'name', 'near', 'nodefault', 'operator', 'overload', 'override', 'package',
    'pascal', 'platform', 'read',
    'readonly', 'register', 'reintroduce', 'requires', 'resident', 'safecall',
    'static', 'stdcall', 'stored', 'varargs', 'virtual', 'write', 'writeonly'
  );

  (** A list of string representing the types of modules. **)
  strModuleTypes : Array[mtProgram..mtUnit] Of String = ('Program', 'Package',
    'Library', 'Unit');
  (** A list of strings representing the stricted scope types. **)
  strStrictedScope : Array[scPrivate..scProtected] Of String = ('private',
    'protected');
  (** A list of strings representing the scope types. **)
  strScope : Array[scGlobal..scPublished] Of String = ('global', 'local',
    'private', 'protected', 'public', 'published');
  (** A sorted list of method directives. Used in identifying method
  directives. **)
  strMethodDirectives : Array[1..24] Of String = (
    'abstract', 'assembler', 'cdecl', 'dispid', 'dynamic', 'export',
    'external', 'far', 'final', 'forward',  'inline', 'local', 'message',
    'near', 'overload', 'override', 'pascal', 'register', 'reintroduce',
    'safecall', 'static', 'stdcall',  'varargs', 'virtual'
  );
  (** A list of real types. **)
  strRealTypes : Array[1..7] Of String = ('comp', 'currency',
    'double', 'extended', 'real', 'real48', 'single');
  (** A list of ordinal idents **)
  strOrdIdents : Array[1..13] Of String = ('boolean', 'byte', 'cardinal', 'char',
    'int64', 'integer', 'longint', 'longword', 'pchar', 'shortint', 'smallint',
    'widechar', 'word');
  (** A list of variants **)
  strVariants : Array[1..2] Of String = ('olevariant', 'variant');
  (** A list of string types. **)
  strStrings  : Array[1..4] Of String = ('ansistring', 'shortstring', 'string',
    'widestring');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..31] Of String = (';', 'const', 'contains',
    'do', 'else', 'end', 'except', 'exports', 'finalization', 'finally',
    'function', 'implementation',
    'implements', 'initialization', 'interface', 'label', 'library', 'object',
    'package', 'procedure', 'program', 'record', 'requires', 'resourcestring',
    'static', 'then', 'type', 'unit', 'until', 'uses', 'var'
  );

  (** This is a list of functions which can be used in a const expression. **)
  strConstExprDesignators : Array[1..17] Of String = ('abs', 'byte', 'chr', 'hi',
    'high', 'integer', 'length', 'lo', 'low', 'odd', 'ord', 'pred', 'round',
    'sizeof', 'succ', 'swap', 'trunc');
  (** A list of Rel operators for expressions. **)
  strRelOps : Array[1..9] Of String = ('<', '<=', '<>', '=', '>', '>=', 'as',
    'in', 'is');
  (** A list of Add operators for expressions. **)
  strAddOps : Array[1..4] Of String = ('+', '-', 'or', 'xor');
  (** A list of Multiplier operators for expressions. **)
  strMulOps : Array[1..7] Of String = ('*', '/', 'and', 'div', 'mod', 'shl', 'shr');
  (** A list of portability directives. **)
  strPortabilityDirective : Array[1..3] Of String = ('deprecated', 'library',
    'platform');
  (** A list of strings representing the types of methods. **)
  strMethodTypes : Array[Low(TMethodType)..High(TMethodType)] Of String = (
    'Constructor', 'Destructor', 'Procedure', 'Function', 'Operator');
  (** A list of strings representing the parameter modifiers for methods. **)
  strParamModifier : Array[pamNone..pamOut] Of String = ('', 'var ', 'const ',
    'out ');

(**


  This function creates a TTypeToken negating the need for a temporary
  variable.


  @precon  None.

  @postcon Creates a TTypeToken negating the need for a temporary variable.


  @param   AToken    as a TTokenInfo
  @param   AScope    as a TScope
  @param   AComment  as a TComment
  @param   Container as a TElementContainer
  @return  a TTypeToken

**)
Function TypeToken(AToken : TTokenInfo; AScope : TScope; AComment : TComment;
  Container : TElementContainer) : TTypeToken;

Begin
  Result.FToken := AToken;
  Result.FScope := AScope;
  Result.FComment := AComment;
  Result.FContainer := Container;
  Assert(Container <> Nil, 'Conatiner in TTypeToken is NULL!');
End;

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
class function TPascalComment.CreateComment(strComment: String; iLine,
  iCol: Integer): TComment;

begin
  Result := Nil;
  If Length(strComment) > 0 Then
    Begin
      Case strComment[1] Of
        '/' : strComment := Copy(strComment, 3, Length(strComment) - 2);
        '{' : strComment := Copy(strComment, 2, Length(strComment) - 2);
        '(' : strComment := Copy(strComment, 3, Length(strComment) - 4);
      End;
      If Length(strComment) > 0 Then
        Begin
          If strComment[Length(strComment)] = '*' Then
            SetLength(strComment, Length(strComment) - 1);
          If Length(strComment) > 0 Then
            Begin
              If (IsInSet(strComment[1], [':', '*'])) Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 1);
                  Result := Create(strComment, iLine, iCol);
                End;
            End;
        End;
    End;
end;

(**

  This method increments the internal count of the number of dimensions of the
  array.

  @precon  None.
  @postcon Increments the internal count of the number of dimensions of the
           array.

**)
Procedure TArrayType.AddDimension;

Begin
  Inc(FDimensions);
End;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Returns the name of the record + '= Record '. 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TRecordDecl.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;
begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'='#32;
  Result := Result + 'Record';
end;

(**

  This is the constructor method for the TRecordDecl class.

  @precon  None.
  @postcon Initialises the class.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   iImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TRecordDecl.Create(strName : String; AScope : TScope; iLine,
  iColumn : Integer; iImageIndex : TImageIndex; AComment : TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, iImageIndex, AComment);
  FPacked := False;
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the Interface declaration with the heritage.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TInterfaceDecl.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;
var
  iToken: Integer;
begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'='#32;
  Result := Result + 'Interface';
  If Heritage.ElementCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 1 To Heritage.ElementCount Do
        Begin
          Result := Result + Heritage.Elements[iToken].AsString(boolShowIdentifier,
            boolForDocumentation);
          If iToken < Heritage.ElementCount  Then
            Result := Result + #32',';
        End;
      Result := Result + ')';
    End;
  If boolForDocumentation Then
    If FGUID <> '' Then
      Result := Result + #13#10 + FGUID;
end;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the DispInterface declaration with the heritage.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDispInterfaceDecl.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;
var
  iToken: Integer;
begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'='#32;
  Result := Result + 'DispInterface';
  If Heritage.ElementCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 1 To Heritage.ElementCount Do
        Begin
          Result := Result + Heritage.Elements[iToken].AsString(boolShowIdentifier,
            boolForDocumentation);
          If iToken < Heritage.ElementCount  Then
            Result := Result + #32',';
        End;
      Result := Result + ')';
    End;
end;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns a type formatted with an equals sign between the name and
           the definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTypes.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    '=', BrowseAndDocItOptions.MaxDocOutputWidth);
end;

(**

  This is the constructor method for the TConstant class.

  @precon  None.
  @postcon Creates an instance of a TConstant declaration.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TConstant.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FTyped := False;
end;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Formats the constant information depending on whether its a simple
           constant or a typed constant.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TConstant.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  If FTyped Then
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
      ':', BrowseAndDocItOptions.MaxDocOutputWidth)
   Else
     Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
       '=', BrowseAndDocItOptions.MaxDocOutputWidth);
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Outputs the parameter information in the style of object pascal
           code.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TPascalParameter.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean) : String;

begin
  Result := strParamModifier[ParamModifier];
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If ParamType <> Nil Then
    Begin
      If boolShowIdentifier Then
        Result := Result + #32':'#32;
      Result := Result + strArrayOf[ArrayOf];
      Result := Result + ParamType.AsString(False, boolForDocumentation);
    End;
  If DefaultValue <> '' Then
    Result := Result + #32'='#32 + DefaultValue;
end;

(**

  This method adds a directive to the directives list.

  @precon  strDirective is a directive token to be added to the directives
           collection.
  @postcon Adds a directive to the directives list.

  @param   strDirective as a String

**)
Procedure TPascalMethod.AddDirectives(strDirective : String);

Begin
  FDirectives.Add(strDirective);
End;

(**

  This is the constructor method for the TPascalMethod class.

  @precon  None.
  @postcon Initialises the class and creates a string list for the directives.

  @param   MethodType as a TMethodType
  @param   strName    as a String
  @param   AScope     as a TScope
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
constructor TPascalMethod.Create(MethodType: TMethodType; strName : String;
  AScope: TScope; iLine, iCol: Integer);
begin
  Inherited Create(MethodType, strName, AScope, iLine, iCol);
  FTypesLabel           := Nil;
  FVariablesLabel       := Nil;
  FConstantsLabel       := Nil;
  FResourceStringsLabel := Nil;
  FLabelsLabel          := Nil;
  FDirectives           := TStringList.Create;
  FResolved             := False;
  FObjClsInt            := Nil;
end;

(**

  This is the destructor method for the TPascalMethod class.

  @precon  None.
  @postcon Frees the memory used for the directives string list.

**)
destructor TPascalMethod.Destroy;
begin
  FDirectives.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Outputs the pascal method declaration.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TPascalMethod.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;

Var
  i : Integer;

begin
  Result := strMethodTypes[MethodType];
  If Name <> '' Then
    If boolShowIdentifier Then
      Result := Result + #32 + Identifier;
  If ParameterCount > 0 Then
    Begin
      If boolForDocumentation Then
        Result := Result + '('#13#10
      Else
        Result := Result + '(';
      For i := 0 To ParameterCount - 1 Do
        Begin
          If boolForDocumentation Then
            Result := Result + #32#32;
          Result := Result + Parameters[i].AsString(boolShowIdentifier,
            boolForDocumentation);
          If i < ParameterCount - 1 Then
            Begin
              If boolForDocumentation Then
                Result := Result + ';'#13#10
              Else
                Result := Result + '; ';
            End;
        End;
      If boolForDocumentation Then
        Result := Result + #13#10;
      Result := Result + ')';
    End;
  If ReturnType <> Nil Then
      Result := Result + #32':'#32 + ReturnType.AsString(boolShowIdentifier,
        boolForDocumentation);
  For i := 0 To FDirectives.Count - 1 Do
    Result := Result + '; ' + FDirectives[i];
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns a combination of the Identifier + parameters so that
           overloaded methods can be accepted by the container.

  @return  a String

**)
function TPascalMethod.GetName: String;

Var
  i : Integer;

begin
  Result := Identifier;
  If Result = '' Then
    Result := Format('PROC%4.4d', [Random(9999)]);
  For i := 0 To ParameterCount - 1 Do
    Begin
      Result := Result + '.' + strParamModifier[Parameters[i].ParamModifier];
      If Parameters[i].ParamType <> Nil Then
        Begin
          Result := Result + strArrayOf[Parameters[i].ArrayOf];
          Result := Result + Parameters[i].ParamType.AsString(False, False);
        End;
      If ReturnType <> Nil Then
        Result := Result + ReturnType.AsString(False, False);
    End;
  If HasDirective('forward') Then
    Result := Result + '.Forward';
end;

(**

  This method test the directive for a specified directive and returns true if
  found.

  @precon  strDirective is the directive to search for.
  @postcon Returns true if the directive was found.

  @param   strDirective as a String
  @return  a Boolean

**)
function TPascalMethod.HasDirective(strDirective: String): Boolean;

Var
  i : Integer;

begin
  Result := False;
  For i := 0 To Directives.Count - 1 Do
    If CompareText(strDirective,
      Copy(Directives[i], 1, Length(strDirective))) = 0 Then
      Begin
        Result := True;
        Break;
      End;
end;

(**

  This method tries to find the symbol with its scope as mark it as referenced.

  @precon  None.
  @postcon Tries to find the symbol with its scope as mark it as referenced.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TPascalMethod.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Var
  i: Integer;
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
  // Local Methods
  boolFound := False;
  For i := 1 To ElementCount Do
    If Elements[i] Is TPascalMethod Then
      Begin
        M := Elements[i] As TPascalMethod;
        If CompareText(AToken.Token, M.Identifier) = 0 Then
          Begin
            M.Referenced := True;
            AToken.Reference := trResolved;
            boolFound := True;
          End;
      End;
  If boolFound Then
    Begin
      Result := True;
      Exit;
    End;
  If ObjClsInt <> Nil Then
    Result := ObjClsInt.ReferenceSymbol(AToken);
  If Result Then
    Exit;
  Result := Inherited ReferenceSymbol(AToken); //: bug IS this a duplicate?
end;

(**

  This is the constructor method for the TPascalProperty class.

  @precon  None.
  @postcon Initialises the property specifiers.

  @param   strIdent    as a String
  @param   AScope       as a TScope
  @param   iLine       as an Integer
  @param   iCol        as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TPascalProperty.Create(strIdent: String; AScope: TScope;
  iLine, iCol : Integer; AImageIndex : TImageIndex; AComment : TComment);
begin
  Inherited Create(strIdent, AScope, iLine, iCol, AImageIndex, AComment);
  FDefaultProperty := False;
  FDefaultSpec := '';
  FDispIDSpec := '';
  FImplementsSpec := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
  FIndexSpec := '';
  FReadOnlySpec := False;
  FWriteOnlySpec := False;
  FReadSpec := '';
  FStoredSpec := '';
  FWriteSpec := '';
end;

(**

  This is a destructor for the TPascalProperty class.

  @precon  None.
  @postcon Frees any memory used by the Imlpemented Specifications.

**)
destructor TPascalProperty.Destroy;
begin
  FImplementsSpec.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns a unique name for properties which are overloaded.

  @return  a String

**)
function TPascalProperty.GetName: String;

Var
  i : Integer;

begin
  Result := Identifier;
  If Result = '' Then
    Result := Format('PROP%4.4d', [Random(9999)]);
  For i := 0 To ParameterCount - 1 Do
    Begin
      Result := Result + '.' + strParamModifier[Parameters[i].ParamModifier];
      If Parameters[i].ParamType <> Nil Then
        Begin
          Result := Result + strArrayOf[Parameters[i].ArrayOf];
          Result := Result + Parameters[i].ParamType.AsString(False, False);
        End;
      If ReturnType <> Nil Then
        Result := Result + ReturnType.AsString(False, False);
    End;
end;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Outputs the pascal property declaration . 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TPascalProperty.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;

  (**

    This is a shorthand routine for output the string specs to the result.

    @precon  None .
    @postcon Output the string specs to the result .

    @param   strName  as a String
    @param   strValue as a String

  **)
  Procedure OutputSpec(strName, strValue : String);

  Begin
    If strValue <> '' Then
      Begin
        If boolForDocumentation Then
          Result := Result + #32#32;
        If strName <> '' Then
          Begin
            If Not boolForDocumentation Then
              Result := Result + #32;
            Result := Result + strName;
          End;
        Result := Result + #32 + strValue;
        If boolForDocumentation Then
          Result := Result + #13#10;
      End;
  End;

Var
  i : Integer;

begin
  Result := 'Property ';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If ParameterCount > 0 Then
    Begin
      Result := Result + '[';
      If boolForDocumentation Then
        Result := Result + #13#10;
      For i := 0 To ParameterCount - 1 Do
        Begin
          If boolForDocumentation Then
            Result := Result + #32#32;
            Result := Result + Parameters[i].AsString(boolShowIdentifier,
              boolForDocumentation);
            If i < ParameterCount - 1 Then
              Result := Result + '; ';
          If boolForDocumentation Then
            Result := Result + #13#10;
          End;
      Result := Result + ']';
    End;
  Result := Result + #32':'#32;
  If ReturnType <> Nil Then
    Begin
      For i := 0 To ReturnType.TokenCount - 1 Do
        Result := Result + ReturnType.AsString(boolShowIdentifier, boolForDocumentation);
    End;
  If boolForDocumentation Then
    Result := Result + #13#10;
  OutputSpec('Index', FIndexSpec);
  OutputSpec('Read', FReadSpec);
  OutputSpec('Write', FWriteSpec);
  OutputSpec('Stored', FStoredSpec);
  OutputSpec('Default', FDefaultSpec);
  If FImplementsSpec.ElementCount > 0 Then
    Begin
      If boolForDocumentation Then
        Result := Result + #32#32
      Else
        Result := Result + #32;
      Result := Result + 'Implements ';
      For i := 1 To FImplementsSpec.ElementCount Do
        Begin
          If i > 1 Then
            Result := Result + ', ';
          Result := Result + FImplementsSpec.Elements[i].Identifier;
        End;
      If boolForDocumentation Then
        Result := Result + #13#10;
    End;
  If FReadOnlySpec Then
    OutputSpec('', 'ReadOnly');
  If FWriteOnlySpec Then
    OutputSpec('', 'WriteOnly');
  OutputSpec('DispID', FDispIDSpec);
  If FDefaultProperty Then
    OutputSpec('', 'Default');
end;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the property specifier, Name = key word, tokens = value.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TPropertySpec.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;

var
  iToken: Integer;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  For iToken := 0 To TokenCount - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + Tokens[iToken].Token;
    End;
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the name of the field and = sign and then the definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TField.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

Var
  iToken : Integer;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32':';
  For iToken := 0 To TokenCount - 1 Do
    Result := Result + #32 + Tokens[iToken].Token;
end;

(**


  This method check whether the field has been documented correctly.

  @precon  None.
  @postcon Check whether the field has been documented correctly.


  @param   boolCascade as a Boolean as a reference

**)
procedure TField.CheckDocumentation(var boolCascade: Boolean);
begin
  If doShowUndocumentedFields In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strFieldDocumentation, DocConflictTable[dctFieldClauseUndocumented]);
  Inherited CheckDocumentation(boolCascade);
end;

(**

  This is a constructor for the TExportsItem class.

  @precon  None.
  @postcon Initialises the class.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
Constructor TExportsItem.Create(strName : String; AScope : TScope; iLine,
  iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FResolved := False;
End;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Returns the Exported item declaration . 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TExportsItem.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

Var
  iToken : Integer;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  For iToken := 0 To TokenCount - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + Tokens[iToken].Token;
    End;
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the variable declaration.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TVar.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    ':', BrowseAndDocItOptions.MaxDocOutputWidth);
end;

(**

  This is the constructor method for the TObjectDecl class.

  @precon  None.
  @postcon Creates an internal Heritage list.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   iImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TObjectDecl.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; iImageIndex: TImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, iImageIndex, AComment);
  FHeritage := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
  FHeritage.Sorted := False;
end;

(**

  This is the destructor method for the TObjectDecl class.

  @precon  None.
  @postcon Destroy the Heritage list.

**)
destructor TObjectDecl.Destroy;
begin
  FHeritage.Free;
  Inherited Destroy;
end;

(**

  This method references symbols with the scope of the object / class.

  @precon  None.
  @postcon References symbols with the scope of the object / class.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TObjectDecl.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Var
  i: Integer;
  boolFound: Boolean;
  MethodsLabel : TLabelContainer;

begin
  Result := Inherited ReferenceSymbol(AToken);
  If Result Then
    Exit;
  boolFound := False;
  MethodsLabel := FindElement(strMethodsLabel) As TLabelContainer;
  If MethodsLabel <> Nil Then
    Begin
      For i := 1 To Methodslabel.ElementCount Do
        If CompareText(AToken.Token, Methodslabel[i].Identifier) = 0 Then
          Begin
            Methodslabel[i].Referenced := True;
            AToken.Reference := trResolved;
            boolFound := True;
          End;
    End;
  Result := boolFound;
End;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Output the name of the Object = '= Object (" HeritageList ")' 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TObjectDecl.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

Var
  iToken: Integer;

begin
  Result := Identifier + #32'='#32'Object';
  If FHeritage.TokenCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 0 To FHeritage.TokenCount - 1 Do
        Begin
          Result := Result + FHeritage.Tokens[iToken].Token;
          If iToken < FHeritage.TokenCount - 1  Then
            Result := Result + #32',';
        End;
      Result := Result + ')';
    End;
end;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Output the name of the Class = '= Class (" HeritageList ")' 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TClassDecl.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

Var
  iToken: Integer;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'='#32;
  Result := Result + 'Class';
  If FAbstractClass Then
    Result := Result + #32'Abstract';
  If FSealedClass Then
    Result := Result + #32'Sealed';
  If FHelper Then
    Result := Result + #32'Helper';
  If Heritage.ElementCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 1 To Heritage.ElementCount Do
        Begin
          Result := Result + Heritage.Elements[iToken].AsString(boolShowIdentifier,
            boolForDocumentation);
          If iToken < Heritage.ElementCount  Then
            Result := Result + ','#32;
        End;
      Result := Result + ')';
    End;
  If FHelper Then
    Result := Result + Format(' For %s', [FHelperClass]);
end;

(**

  This is the constructor method for the TPascalDocModule class.

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
Constructor TPascalModule.CreateParser(Source : String; strFileName : String;
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
    '-', '+', '/', '*'];

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
  LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';
  LastToken := ttUnknown;

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
          If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
            AddToken(TTokenInfo.Create(strToken, iStreamPos,
              iTokenLine, iTokenColumn, Length(strToken), LastCharType));
        End;
  Except
    On E : Exception Do
      AddIssue(E.Message, scGlobal, 'TokenizeStream', 0, 0, etError);
  End
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
  iIcon : TImageIndex;
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
          If FImplementedMethodsLabel = Nil Then
            FImplementedMethodsLabel := Add(strImplementedMethodsLabel,
              iiImplementedMethods, scNone, Nil) As TLabelContainer;
          Container := FImplementedMethodsLabel;
          iIcon := iiUnknownClsObj;
          AScope := scNone;
          E := FTypesLabel;
          If E <> Nil Then
            For iCls := 0 To Method.ClassNames.Count - 1 Do
              Begin
                E := E.FindElement(Method.ClassNames[iCls]);
                If E <> Nil Then
                  Begin
                    iIcon := E.ImageIndex;
                    AScope := E.Scope;
                  End;
                Container := Container.Add(TLabelContainer.Create(
                  Method.ClassNames[iCls], AScope, 0, 0, iIcon, Nil));
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
function TPascalModule.KeyWords: TKeyWords;

Var
  i, j : Integer;
  str : String;

begin
  SetLength(Result, Succ(High(strReservedWords)) + Succ(High(strDirectives)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
  For i := Low(strDirectives) To High(strDirectives) Do
    Result[High(strReservedWords) + i] := strDirectives[i];
  For i := Low(Result) To Pred(High(Result)) Do
    For j := i + 1 To High(Result) Do
      If Result[i] > Result[j] Then
        Begin
          str := Result[i];
          Result[i] := Result[j];
          Result[j] := str;
        End;
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

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  If TokenIndex + iOffset > -1 Then
    Begin
      T := Tokens[TokenIndex + iOffset] As TTokenInfo;
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

  @grammar Goal -> ( Program | Package | Library | Unit )

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
            Raise EParserAbort.Create('Parsing Aborted!');
          End;
      End;
  Except
    On E : EParserAbort Do
      AddIssue(E.Message, scNone, 'Goal', 0, 0, etError);
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
Function TPascalModule.OPProgram : Boolean;

begin
  Result := Token.UToken = 'PROGRAM';
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtProgram;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier, ttDirective] Then
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

  @grammar Unit -> UNIT Ident ';'
                   InterfaceSection
                   ImplementationSection
                   InitSection '.'

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
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
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
          If Token.TokenType In [ttIdentifier, ttDirective] Then
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

  @grammar Package -> PACKAGE Ident ';'
                      [ RequiresClause ]
                      [ ContainsClause ]
                      END '.'

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
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
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
Function TPascalModule.OPLibrary : Boolean;

begin
  Result := Token.UToken = 'LIBRARY';
  If Result Then
    Begin
      Comment := GetComment;
      ModuleType := mtLibrary;
      NextNonCommentToken;
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
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

  @grammar ProgramBlock -> [ UsesClause ]
                           Block

  @precon  None.
  @postcon Parses a program block from the current token position using the
           following object pascal grammar.

**)
procedure TPascalModule.ProgramBlock;
begin
  UsesClause;
  EatDotNETAttribute;
  Block(scPrivate, Nil);
end;

(**

  This method parses the Uses clause declaration from the current token
  position using the following object pascal grammar.

  @grammar Uses -> USES IdentList ';'

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

  @grammar PortabilityDirective -> platform
                                -> deprecated
                                -> library

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
      NextNonCommentToken; //: @note Does not get added to any symbols.
      If Token.Token = ';' Then
        Begin
          NextNonCommentToken;
          If Not IsKeyWord(Token.Token, strPortabilityDirective) Then
            RollBackToken;
        End;
    End;
End;

(**

  This method parses an interface section from the current token position using
  the following object pascal grammar.

  @grammar  InterfaceSection -> INTERFACE
                               [ UsesClause ]
                               [ InterfaceDecl ] ...

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

  @grammar ExportedHeading -> ProcedureHeading ';' [ Directive ]
                              FunctionHeading ';' [ Directive ]

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

  @grammar ImplementationSection -> IMPLEMENTATION
                                    [ UsesClause ]
                                    [ DeclSection ] ...

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

  @grammar Block -> [ DeclSection ]
                    CompoundStmt

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
    bool := CompoundStmt;
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

  @grammar ExportedStmt -> EXPORTS ExportsItem [, ExportsItem]...

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

  @grammar ExportsEntry -> Ident [ INDEX IntegerConstant [ NAME StringConstant
           ] [ RESIDENT ] ] </TD>

  @param   Container as a TElementContainer

**)
Procedure TPascalModule.ExportsItem(Container : TElementContainer);

Var
  E : TElementContainer;
  ExprType : TExprTypes;

Begin
  If (Token.TokenType In [ttIdentifier, ttDirective]) Then
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
              ExprType := [etInteger, etConstExpr];
              ConstExpr(E, ExprType);
            End;
          // Check NAME
          If Token.UToken = 'NAME' Then
            Begin
              AddToExpression(E);
              ExprType := [etString, etConstExpr];
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

  @grammar DeclSection -> LabelDeclSection -> ConstSection -> ResStringSection
           -> TypeSection -> VarSection -> ThreadVarSection ->
           ProcedureDeclSection -> ExportedProcs

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

  This method parses a label declaration section from the current token position
  using the following object pascal grammar.

  @precon  None . 
  @postcon This method dicards the labels found and returns True if this method 
           handles a label declaration section . 

  @grammar LabelDeclSection -> LABEL LabelId 

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
            If Token.TokenType In [ttIdentifier, ttDirective] Then
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


  @grammar ConstSection -> CONST ( ConstantDecl ';' ) ...


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
      If Container Is TClassDecl then
        Begin
          ConstantsLabel := (Container As TClassDecl).FindElement(strConstantsLabel) As TLabelContainer;
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
      While ConstantDecl(AScope, C) Do
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


  @precon  On entry to this method, Scope defines the current scope of the

           block i.e. private in in the implemenation section or public if in

           the interface section and The Method parameter is nil for methods

           in the implementation section or a reference to a method for a

           local declaration section with in a method.

  @postcon This method returns True if this method handles a constant

           declaration section.


  @grammar ConstantDecl -> Ident '=' ConstExpr [PortabilityDirective] -> Ident

           ':' TypeId '=' TypedConstant [PortabilityDirective]


  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.ConstantDecl(AScope : TScope;
  Container : TElementContainer) : Boolean;

Var
  T : TGenericTypeDecl;
  ExprType : TExprTypes;
  C, tmpC : TConstant;
  FTemporaryElements: TElementContainer;

Begin
  Result := False;
  // If not identifier then there is a new section
  If Token.TokenType In [ttIdentifier, ttDirective] Then
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
          ExprType := [etUnknown, etConstExpr];
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

  @grammar ConstSection -> RESOURCESTRING ( ResourceStringDecl ';' ) ... Also
           see {@link TPascalDocModule.ConstantSection} .

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

  @grammar ConstantDecl -> Ident '=' ConstExpr
           Also see {@link TPascalDocModule.VarDecl}.

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
  ExprType : TExprTypes;

Begin
  Result := False;
  ExprType := [etConstExpr, etString];
  // If not identifier then there is a new section
  If Token.TokenType In [ttIdentifier, ttDirective] Then
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

  @grammar Typesection -> TYPE ( TypeDecl ';' ) ...

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
      If Container Is TClassDecl Then
        Begin
          TypesLabel := (Container As TClassDecl).FindElement(strTypesLabel) As TLabelContainer;
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

  @grammar TypeDecl -> Ident '=' [TYPE] Type -> Ident '=' [TYPE] RestrictedType

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.TypeDecl(AScope : TScope; Container : TElementContainer) : Boolean;

Var
  AToken : TTypeToken;

Begin
  Result := False;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
    Begin
      AToken := TypeToken(Token, AScope, GetComment, Container);
      NextNonCommentToken;
      If Token.Token = '=' Then
        Begin
          NextNonCommentToken;
          If Token.UToken = 'TYPE' Then
            NextNonCommentToken;
          Result := True;
          If GetTypeDecl(AToken) = Nil Then
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

  @param   AToken as a TTypeToken
  @return  a TGenericTypeDecl

**)
Function TPascalModule.GetTypeDecl(AToken : TTypeToken) : TGenericTypeDecl;

Begin
  Result := RestrictedType(AToken);
  If Result = Nil Then
    Result := OPType(AToken);
  PortabilityDirective;
End;

(**

  This method parses a typed constant from the current token position using
  the following object pascal grammar.

  @grammar TypedConstant -> ( ConstExpr | ArrayConstant | RecordConstant )

  @precon  C is a valid instance of the constant to be populated with tokens.
  @postcon Returns false if this was not a typed constant an not handled.

  @param   C as a TElementContainer
  @param   T as a TGenericTypeDecl
  @return  a Boolean

**)
Function TPascalModule.TypedConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

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
  ExprType : TExprTypes;

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
        ExprType := [etUnknown, etConstExpr];
        ConstExpr(C, ExprType);
      End;
End;

(**

  This method attempts to parser the current token position as an RecordConstant.

  @grammar RecordConstant -> '(' RecordFieldConstant ';'... ')'

  @precon  C must be a valid generic container.
  @postcon Attempts to parser the current token position as an RecordConstant.

  @param   C as a TElementContainer
  @param   T as a TGenericTypeDecl
  @return  a Boolean

**)
Function TPascalModule.RecordConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.Token = '(';
  If Result Then
    Begin
      AddToExpression(C);
      Repeat
        If Not RecordFieldConstant(C, T) Then
          If Token.Token <> ')' Then
            Begin // If not handled treat as ConstExpr
              ExprType := [etUnknown, etConstExpr];
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

  This method attempts to parse the current token position as a record field
  constant.

  @grammar RecordFieldConstant -> Ident ':' TypedConstant

  @precon  C must be a valid generic container.
  @postcon Attempts to parse the current token position as a record field
           constant.

  @param   C as a TElementContainer
  @param   T as a TGenericTypeDecl
  @return  a Boolean

**)
Function TPascalModule.RecordFieldConstant(C : TElementContainer;
  T : TGenericTypeDecl) : Boolean;

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

  @param   AToken as a TTypeToken
  @return  a TGenericTypeDecl

**)
Function TPascalModule.OPType(AToken : TTypeToken) : TGenericTypeDecl;

Begin
  Result := StrucType(AToken);
  If Result = Nil Then
    Result := PointerType(AToken);
  If Result = Nil Then
    Result := StringType(AToken);
  If Result = Nil Then
    Result := ProcedureType(AToken);
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

  @grammar RestrictedType -> ObjectType
                          -> ClassType
                          -> InterfaceType

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

  This method parses a class reference type declaration from the current token
  position using the following object pascal grammar.

  @grammar ClassRefType -> CLASS OF TypeId

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
      NextNonCommentToken;
      If Token.UToken = 'OF' Then
        Begin
          NextNonCommentToken;
          UpdateTypeToken(AToken);
          With AToken Do
            Result := TClassRefType.Create(FToken.Token, FScope, FToken.Line,
              FToken.Column, iiPublicType, FComment);
          Result := AToken.FContainer.Add(Result) As TClassRefType;
          Result.AddToken('Class');
          Result.AddToken('Of');
          If Not TypeId(Result) Then
            ErrorAndSeekToken(strTypeIdExpected, 'ClassRefType', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End Else
          RollBackToken;
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
  If AToken.FToken = Nil Then
    AToken.FToken := Token;
  If AToken.FComment = Nil Then
    AToken.FComment := GetComment;
end;

(**

  This method parses a simple type declaration from the current token
  position using the following object pascal grammar.

  @grammar SimpleType -> ( OrdinalType | RealType )

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
        Result := TRealType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TRealType;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
End;

(**

  This method determines if the type is an ordinal type using the folowing
  object pascal grammar.

  @grammar OrdinalType -> ( SubrangeType | EnumerateType | OrdIndent )

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
                     -> CARDINAL

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
        Result := TOrdIdent.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TOrdIdent;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
End;

(**

  This method parses a variant type declaration section using the following
  object pascal grammar.

  @grammar VariantType -> VARIANT
                       -> OLEVARIANT

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
        Result := TVariantType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TVariantType;
      Result.AppendToken(Token);
      NextNonCommentToken;
    End;
end;

(**

  This method parses  a sub range type from the current token position using
  the following object pascal grammar. This method also currently acts as a type
  CATCH ALL if nothing else works.

  @grammar SubrangeType -> ConstExpr .. ConstExpr

  @precon  None.
  @postcon Returns an ordinal type if one was parsed else returns nil.

  @param   AToken as a TTypeToken
  @return  a TSubRangeType

**)
Function TPascalModule.SubRangeType(AToken : TTypeToken) : TSubRangeType;

Var
  ExprType : TExprTypes;

Begin
  Result := Nil;
  If Not IsKeyWord(Token.Token, strReservedWords) Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TSubRangeType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
      Result := AToken.FContainer.Add(Result) As TSubRangeType;
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
        Result := TEnumerateType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
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

  This method eats .NET attributes.

  @precon  None.
  @postcon Eats .NET attributes.

**)
procedure TPascalModule.EatDotNETAttribute;

begin
  If Token.Token = '[' Then
    Begin
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          NextNonCommentToken;
          If Token.Token = ']' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, 'EatDotNETAttribute', ']',
              strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'EatDotNETAttribute', '[',
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the current token position as an Enumerate Element.

  @grammar EnumerateTypeElement -> Ident [ â€˜=â€™ ConstExpr ]

  @precon  None.
  @postcon Parses the current token position as an Enumerate Element.

  @param   EnumerateType as a TEnumerateType

**)
Procedure TPascalModule.EnumerateElement(EnumerateType : TEnumerateType);

Var
  ExprType : TExprTypes;

Begin
  If Token.TokenType In [ttIdentifier, ttDirective] Then
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

  @param   AToken as a TTypeToken
  @return  a TStringType

**)
Function TPascalModule.StringType(AToken : TTypeToken) : TStringType;

Var
  ExprType : TExprTypes;

begin
  Result := Nil;
  If IsKeyWord(Token.Token, strStrings) Then
    Begin
      UpdateTypeToken(AToken);
      Result := TStringType.Create(AToken.FToken.Token, AToken.FScope,
        AToken.FToken.Line, AToken.FToken.Column, iiPublicType, AToken.FComment);
      Result := AToken.FContainer.Add(Result) As TStringType;
      Result.AppendToken(Token);
      NextNonCommentToken;
      // Check for '[' ConstExpr ']'
      If Token.Token = '[' Then
        Begin
          Result.AppendToken(Token);
          NextNonCommentToken;
          ExprType := [etInteger, etConstExpr];
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

  @grammar StrucType -> [ PACKED ] ( ArrayType | SetType | FileType | RecType )

  @precon  None.
  @postcon This method returns True if this method handles a constant
           declaration section.

  @param   AToken as a TTypeToken
  @return  a TGenericTypeDecl

**)
Function TPascalModule.StrucType(AToken : TTypeToken) : TGenericTypeDecl;

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

  @grammar ArrayType -> ARRAY [ '[' OrdinalType / ',' ... ']' ] OF Type

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
        Result := TArrayType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
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

  @grammar RecType -> RECORD [ FieldList ] END

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

begin
  Result := Nil;
  If Token.UToken = 'RECORD' Then
    Begin
      UpdateTypeToken(AToken);
      With AToken Do
        Result := TRecordDecl.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicRecord, FComment);
      Result := AToken.FContainer.Add(Result) As TRecordDecl;
      Result.Line := AToken.FToken.Line;
      Result.Column := AToken.FToken.Column;
      Result.Comment := AToken.FComment ;
      Result.IsPacked := boolPacked;
      NextNonCommentToken;

      Repeat
        ClassVisibility(InternalScope);
        If Token.UToken = 'END' Then
          Break;
      Until Not (
        FieldList(Result, InternalScope) Or
        ClassMethodList(Result, InternalScope) Or
        ClassPropertyList(Result, InternalScope) Or
        TypeSection(InternalScope, Result) Or
        ConstSection(InternalScope, Result) Or
        VarSection(InternalScope, Result) Or
        ClassVarSection(InternalScope, Result)
      );

      If Token.UToken = 'END' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'RecType', 'END',
          strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses a field list for classes, records and object declarations
  from the current token position.

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses a field list for classes, records and object declarations
           from the current token position.

  @grammar FieldList -> FieldDecl / ';' ... [ VariantSection ] [ ';' ]

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.FieldList(Rec: TRecordDecl; InternalScope : TScope) : Boolean;

begin
  Repeat
    Result := VariantSection(Rec, InternalScope);
    If Not Result Then
      Result := FieldDecl(Rec, InternalScope);
  Until Not IsToken(';', Nil);
end;

(**

  This method parses a records field declarations from the current token 
  position using the following object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters
           too.
  @postcon Parses a records field declarations from the current token position

  @grammar FieldDecl -> IdentList ':' Type

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

  @grammar VariantSection -> CASE [ Ident ':' ] TypeId OF RecVariant / ';' ...

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.VariantSection(Rec: TRecordDecl; InternalScope : TScope) : Boolean;

Var
  C : TElementContainer;

Begin
  Result := Token.UToken = 'CASE';
  If Result Then
    Begin
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier, ttDirective] Then
        Begin
          NextNonCommentToken;
          If Token.Token = ':' Then
            NextNonCommentToken
          Else
            RollBackToken;
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

  This method parses the record variant section of a record from the current
  token position using the following object pascal grammar.

  @precon  Rec in a valid instance of a record type to add fields / parameters 
           too.
  @postcon Parses the record variant section of a record from the current token
           position

  @grammar RecVariant -> ConstExpr / ',' ... ':' '(' [ FieldList ] ')'

  @param   Rec           as a TRecordDecl
  @param   InternalScope as a TScope

**)
Procedure TPascalModule.RecVariant(Rec : TRecordDecl; InternalScope : TScope);

Var
  C : TElementContainer;
  ExprType : TExprTypes;

Begin
  C := TTempCntr.Create('', scPrivate, 0, 0, iiNone, Nil);
  Try
    Repeat
      ExprType := [etUnknown, etConstExpr];
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
    FieldList(Rec, InternalScope);
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
      If M.ObjClsInt <> Nil Then
        Begin
          E := M.ObjClsInt.FindElement(strMethodsLabel);
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

  @grammar SetType -> SET OF OrdinalType

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
        Result := TSetType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
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

  @grammar FileType -> FILE OF Type

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
            Result := TFileType.Create(FToken.Token, FScope, FToken.Line,
              FToken.Column, iiPublicType, FComment);
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
            Result := TFileType.Create(FToken.Token, FScope, FToken.Line,
              FToken.Column, iiPublicType, FComment);
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

  @grammar PointerType -> '^' TypeId

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
        Result := TPointerType.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicType, FComment);
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

  @grammar ProceduralType -> ( ProcedureHeading | FunctionHeading ) [ OF OBJECT ]

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
          Result := TProcedureType.Create(FToken.Token, FScope, FToken.Line,
            FToken.Column, iiPublicType, FComment);
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
        If Token.Token = ';' Then
          Begin
            NextNonCommentToken;
            If IsKeyWord(Token.Token, strMethodDirectives) Then
              Begin
                Result.AppendToken(Token);
                NextNonCommentToken;
              End Else
                RollBackToken;
          End;
      End;
  Finally
    TemporaryContainer.Free;
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

  @grammar VarSection -> VAR ( VarDecl ';' ) ...

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
      If LabelScope <> scLocal Then
        LabelScope := scPublic;
      If Container Is TPascalMethod Then
        Begin
          If (Container As TPascalMethod).VariablesLabel = Nil Then
            (Container As TPascalMethod).VariablesLabel := Container.Add(
              strVarsLabel, iiPublicVariablesLabel, LabelScope,
              GetComment) As TLabelContainer;
          V := (Container As TPascalMethod).VariablesLabel;
        End Else
      If Container Is TClassDecl Then
        Begin
          VariablesLabel := (Container As TClassDecl).FindElement(strVarsLabel) As TLabelContainer;
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
Function TPascalModule.ClassVarSection(AScope : TScope;
  Cls : TRecordDecl) : Boolean;

Var
  V : TElementContainer;
  LabelScope : TScope;
  ClassVarsLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'CLASS' Then
    Begin
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
          RollBackToken;
    End;
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

  @grammar VarDecl -> IdentList ':' Type [ ( ABSOLUTE ( Ident | ConstExpr ) ) |
           '=' ConstExpr ]

  @param   AScope      as a TScope
  @param   VarSection  as a TElementContainer
  @param   AImageIndex as a TImageIndex
  @return  a Boolean

**)
Function TPascalModule.VarDecl(AScope : TScope; VarSection : TElementContainer;
  AImageIndex : TImageIndex) : Boolean;

Var
  I  :TIdentList;
  j : Integer;
  V, tmpV : TElementContainer;
  T : TGenericTypeDecl;
  C : TElementContainer;
  ExprType : TExprTypes;
  FTemporaryElements: TElementContainer;
  AToken: TTokenInfo;

Begin
  Result := False;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
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
                    ExprType := [etUnknown, etConstExpr];
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
                        ExprType := [etUnknown, etConstExpr];
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

  @grammar ThreadVarDecl -> IdentList ':' Type [ ( ABSOLUTE ( Ident | ConstExpr ) ) | '=' ConstExpr ]

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
  ExprType : TExprTypes;
  FTemporaryElements: TElementContainer;

Begin
  Result := False;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
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
                    ExprType := [etUnknown, etConstExpr];
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
  @param   ExprType as a TExprTypes as a reference

**)
Procedure TPascalModule.Expression(C : TElementContainer; var ExprType : TExprTypes);

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


  @grammar SimpleExpression -> ['+' | '-'] Term [AddOp Term]...


  @param   C        as a TElementContainer
  @param   ExprType as a TExprTypes as a reference

**)
Procedure TPascalModule.SimpleExpression(C : TElementContainer; var ExprType : TExprTypes);

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


  @grammar Term -> Factor [MulOp Factor]...


  @param   C        as a TElementContainer
  @param   ExprType as a TExprTypes as a reference

**)
Procedure TPascalModule.Term(C : TElementContainer; var ExprType : TExprTypes);

Begin
  Repeat
    Factor(C, ExprType);
  Until Not MulOp(C, ExprType)
End;

(**


  This method attempts to parse a factor from the current token position.


  @precon  None.

  @postcon Attempts to parse a factor from the current token position.


  @grammar Factor -> Designator ['(' ExprList ')'] -> '@' Designator -> Number

           -> String -> NIL -> '(' Expression ')' -> NOT Factor ->

           SetConstructor -> TypeId '(' Expression ')' // NOT USED


  @param   C        as a TElementContainer
  @param   ExprType as a TExprTypes as a reference

**)
Procedure TPascalModule.Factor(C : TElementContainer; var ExprType : TExprTypes);

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
  If Token.TokenType In [ttSingleLiteral] Then
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
          If Pos('.', Token.Token) > 0 Then
            Include(ExprType, etFloat)
          Else
            Include(ExprType, etInteger);
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

  @param   ExprType as a TExprTypes
  @return  a Boolean

  @note    This may have problems with expression that allow integers and
           floats to be added, etc.

**)
Function TPascalModule.CheckNumberType(ExprType : TExprTypes) : Boolean;

Begin
  If Pos('.', Token.Token) > 0 Then
    Result := etFloat In ExprType
  Else
    Result := etInteger In ExprType;
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

  @param   C as a TElementContainer
  @param   ExprType as a TExprTypes
  @return  a Boolean

**)
Function TPascalModule.RelOp(C : TElementContainer; ExprType : TExprTypes) : Boolean;

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

  @param   C as a TElementContainer
  @param   ExprType as a TExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.MulOp(C : TElementContainer; var ExprType : TExprTypes) : Boolean;

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


  @precon  None

  @postcon Attempts to parse the current token position as a Designator.


  @grammar Designator -> QualId ['.' Ident | '[' ExprList ']' | '^']...


  @param   C        as a TElementContainer
  @param   ExprType as a TExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.Designator(C : TElementContainer; var ExprType : TExprTypes) : Boolean;

var
  M : TPascalMethod;

Begin
  Result := (Token.TokenType In [ttIdentifier, ttDirective]) Or
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
      DesignatorSubElement(C, ExprType, ['.', '[', '^', '(']);
    End;
End;

(**

  This method handles the sub elements of a designator, i. e. period,[,(and ^.

  @precon  None .
  @postcon Handles the sub elements of a designator , i . e . period , [, ( and
           ^.

  @param   C               as a TElementContainer
  @param   ExprType        as a TExprTypes as a reference
  @param   strValidSymbols as an Array Of String

**)
Procedure TPascalModule.DesignatorSubElement(C : TElementContainer;
  var ExprType : TExprTypes; strValidSymbols : Array of String);

var
  M: TPascalMethod;

Begin
  M := CurrentMethod As TPascalMethod;
  While IsKeyWord(Token.Token, strValidSymbols) Or (IsKeyWord(Token.Token, ['(', '['])) Do // Always check for proc/func
    If Token.Token = '.' Then
      Begin
        AddToExpression(C);
        If Token.TokenType In [ttIdentifier, ttDirective, ttReservedWord] Then
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
          If etConstExpr In ExprType Then
            If Not IsKeyWord(PrevToken.Token, strConstExprDesignators) Then
              Begin
                ErrorAndSeekToken(strConstExprDesignator, 'DesignatorSubElement',
                  PrevToken.Token, strSeekableOnErrorTokens, stActual);
                Exit;
              End;
        AddToExpression(C);
        ExprList(C);
        While Token.Token = ':' Do
          Begin
            AddToExpression(C);
            ExprList(C);
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


  @grammar SetConstructor -> '[' [SetElement/','...] ']'


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


  @grammar SetElement -> Expression ['..' Expression]


  @param   C as a TElementContainer

**)
Procedure TPascalModule.SetElement(C : TElementContainer);

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


  @precon  None.

  @postcon Attempts to parse the current token position as an Expression List.


  @grammar ExprList -> Expression/','...


  @param   C as a TElementContainer

**)
Procedure TPascalModule.ExprList(C : TElementContainer);

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

  @precon  None.
  @postcon Attempts to parse the current token position as a statement.

  @grammar Statement -> [LabelId ':'] [SimpleStatement | StructStmt]

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


  @grammar StmtList -> Statement ';'...


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

  @grammar SimpleStatement -> Designator ['(' ExprList ')'] -> Designator ':='
           Expression -> INHERITED -> GOTO LabelId

**)
Procedure TPascalModule.SimpleStatement;

Var
  ExprType : TExprTypes;

Begin
  If Token.UToken = 'GOTO' Then
    Begin
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier, ttDirective] Then
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
          ExprType := [etUnknown];
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
          ExprType := [etUnknown];
          Designator(Nil, ExprType);
        End Else
        Begin
          ExprType := [etUnknown];
          Designator(Nil, ExprType);
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

  @precon  None.
  @postcon Attempts to parse the current token position as a structured
           statement.

  @grammar StructStmt -> CompoundStmt -> ConditionalStmt -> LoopStmt ->
           WithStmt -> TryExceptStmt -> TryFinallyStmt -> RaiseStmt ->
           AssemblerStmt

  @return  a Boolean

**)
Function TPascalModule.StructStmt : Boolean;

Begin
  Result :=
    CompoundStmt Or
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

  @grammar CompoundStmt -> BEGIN StmtList END

  @return  a Boolean

**)
Function TPascalModule.CompoundStmt : Boolean;

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


  This method attempts to parse the current token position as a
  ConditionalStmt.


  @precon  None.

  @postcon Attempts to parse the current token position as a ConditionalStmt.


  @grammar ConditionalStmt -> IfStmt -> CaseStmt


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

  @grammar IfStmt -> IF Expression THEN Statement [ELSE Statement]

  @return  a Boolean

**)
Function TPascalModule.IfStmt : Boolean;

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

  @precon  None.
  @postcon Attempts to parse the current token position as a CASE statement.

  @grammar CaseStmt -> CASE Expression OF CaseSelector ';'... [ELSE Statement]
                      [';'] END

  @return  a Boolean

**)
Function TPascalModule.CaseStmt : Boolean;

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

  @grammar CaseSelector -> CaseLabel ','... ':' Statement

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

  @grammar CaseLabel -> ConstExpr ['..' ConstExpr]

**)
Procedure TPascalModule.CaseLabel;

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

  @precon  None.
  @postcon Attempts to parse the current token position as a Loop statement.

  @grammar LoopStmt -> RepeatStmt -> WhileStmt -> ForStmt

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

  @grammar RepeatStmt -> REPEAT StmtList UNTIL Expression

  @return  a Boolean

**)
Function TPascalModule.RepeatStmt : Boolean;

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

  This method attempts to parse the current token position as a While
  Statement.

  @precon  None.
  @postcon Attempts to parse the current token position as a While Statement.

  @grammar WhileStmt -> WHILE Expression DO Statement

  @return  a Boolean

**)
Function TPascalModule.WhileStmt : Boolean;

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

  @precon  None.
  @postcon Attempt to parse the current token position as a For statement.

  @grammar ForStmt -> FOR QualId ':=' Expression (TO | DOWNTO) Expression DO
           Statement

  @return  a Boolean

**)
Function TPascalModule.ForStmt : Boolean;

Var
  ExprType : TExprTypes;
  M: TPascalMethod;

Begin
  Result := Token.UToken = 'FOR';
  If Result Then
    Begin
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier, ttDirective] Then
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
            End
          Else If Token.UToken = 'IN' Then
            Begin
              NextNonCommentToken;
              ExprType := [etUnknown];
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

  @grammar WithStmt -> WITH IdentList DO Statement

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

  @grammar TryExceptStmt -> TRY StmtList EXCEPT ExceptionBlock END ...or... TRY
           StmtList FINALLY StmtList END

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

  @grammar ExceptionBlock -> [ON [Ident â€˜:â€™]
           TypeID DO Statement]... [ELSE Statement]

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
      If Token.TokenType In [ttIdentifier, ttDirective] Then
        Begin
          NextNonCommentToken;
          If Token.Token = ':' Then
            NextNonCommentToken
          Else
            RollBackToken;
        End;
      Con := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        TypeId(Con);
        If Token.UToken = 'DO' Then
          Begin
            NextNonCommentToken;
            If Not CompoundStmt Then
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

  @grammar RaiseStmt -> RAISE [object] [AT address]

  @return  a Boolean

**)
Function TPascalModule.RaiseStmt : Boolean;

Var
  ExprType : TExprTypes;

Begin
  Result := Token.UToken = 'RAISE';
  If Result Then
    Begin
      NextNonCommentToken;
      SimpleStatement;
      If Uppercase(Token.Token) = 'AT' Then
        Begin
          NextNonCommentToken;
          ExprType := [etUnknown, etConstExpr];
          ConstExpr(Nil, ExprType);
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

  This method parses a procedure declaration section from the current token
  position using the following object pascal grammar.

  @grammar ProcedureDeclSection -> ProcedureDecl
                                -> FunctionDecl
                                -> ConstructorDecl
                                -> DestructorDecl

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

  @grammar ProcedureDecl -> ProcedureHeading ';' [Directive] [PortabilityDirective]
                            Block ';'

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
  strIdentifier: String;
  slClassNames: TStringList;
  iLine: Integer;
  iColumn: Integer;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtFunction]) Then
    Try
      If PrevToken.UToken = 'CLASS' Then
        C := GetComment(cpBeforePreviousToken)
      Else
        C := GetComment;
      NextNonCommentToken;
      If (Token.TokenType In [ttIdentifier, ttDirective]) Xor boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'FunctionHeading', Token.Token,
            strSeekableOnErrorTokens, stActual);
          Exit;
        End;
      // Create method and store in collection and get comment
      iLine := 0;
      iColumn := 0;
      slClassNames := TStringList.Create;
      Try
        Try
          If boolIdent Then
            Begin
              strIdentifier := Token.Token;
              iLine := Token.Line;
              iColumn := Token.Column;
              NextNonCommentToken;
              ProcessClsIdents(Container, slClassNames, strIdentifier, iLine,
                iColumn);
            End;
        Finally
          Result := TPascalMethod.Create(mtFunction, strIdentifier, AScope,
            iLine, iColumn);
          Result.ClassNames.Assign(slClassNames);
          Result.ObjClsInt := FindObjClsInt(slClassNames);
          Result.Comment := C;
        End;
      Finally
        slClassNames.Free;
      End;
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
  Container : TElementContainer; boolIdent : Boolean = true) : TPascalMethod;

Var
  C : TComment;
  strIdentifier: String;
  slClassNames: TStringList;
  iLine: Integer;
  iColumn: Integer;
  boolRequiresReturn: Boolean;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtOperator]) Then
    Try
      If PrevToken.UToken = 'CLASS' Then
        C := GetComment(cpBeforePreviousToken)
      Else
        C := GetComment;
      NextNonCommentToken;
      If (Token.TokenType In [ttIdentifier, ttDirective]) Xor boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'OperatorHeading', Token.Token,
            strSeekableOnErrorTokens, stActual);
          Exit;
        End;
      // Create method and store in collection and get comment
      iLine := 0;
      iColumn := 0;
      slClassNames := TStringList.Create;
      Try
        Try
          If boolIdent Then
            Begin
              strIdentifier := Token.Token;
              iLine := Token.Line;
              iColumn := Token.Column;
              NextNonCommentToken;
              ProcessClsIdents(Container, slClassNames, strIdentifier, iLine,
                iColumn);
            End;
        Finally
          Result := TPascalMethod.Create(mtOperator, strIdentifier, AScope,
            iLine, iColumn);
          Result.ClassNames.Assign(slClassNames);
          Result.ObjClsInt := FindObjClsInt(slClassNames);
          Result.Comment := C;
        End;
      Finally
        slClassNames.Free;
      End;
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

  This method processes the class names and Identifier for the Procedure,
  Function, Constructor or Destructor.

  @precon  Container and slClassNames must be a valid instances.
  @postcon Processes the class names and Identifier for the Procedure,
           Function, Constructor or Destructor.

  @param   Container     as a TElementContainer
  @param   slClassNames  as a TStringList
  @param   strIdentifier as a String as a reference
  @param   iLine         as an Integer as a reference
  @param   iColumn       as an Integer as a reference

**)
Procedure TPascalModule.ProcessClsIdents(Container : TElementContainer;
  slClassNames : TStringList; var strIdentifier : String; var iLine,
  iColumn : Integer);

Begin
  // Check for '.' to signify a class method or alias
  While Token.Token = '.' Do
    Begin
      NextNonCommentToken;
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) Then
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
      If Token.TokenType In [ttIdentifier, ttDirective] Then
        Begin
          Method.Alias := Token.Token;
          NextNonCommentToken;
          If Token.Token = '.' Then
            Begin
              Method.Alias := Method.Alias + Token.Token;
              NextNonCommentToken;
              If Token.TokenType In [ttIdentifier, ttDirective] Then
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
      If Func.ReturnType = Nil Then
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

  @param   Method as a TPascalMethod
  @return  a Boolean

**)
Function TPascalModule.CheckReturnValue(Method : TPascalMethod) : Boolean;

Begin
  Result := False;
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      If (Token.TokenType In [ttIdentifier, ttDirective]) Or (Token.UToken = 'STRING') Then
        Begin
          Method.ReturnType := TTypes.Create('', scNone, 0, 0, iiNone, Nil);
          ReferenceSymbol(Token);
          TypeId(Method.ReturnType);
          ReferenceSymbol(Token);
        End Else
          ErrorAndSeekToken(strIdentExpected, 'CheckReturnValue',
            Token.Token, strSeekableOnErrorTokens, stActual);
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
  ResolveScopeOfImplementedClassMethods(FImplementedMethodsLabel);
  ResolveScopeOfImplementedExportedMethods;
  ResolveScopeOfImplementedExportsMethods;
  // Only resolved methods IF there are no other errors.
  Errors := FindElement(strErrors) As TLabelContainer;
  If Errors <> Nil Then
    If Errors.ElementCount > 0 Then
      Exit;
  FindUnresolvedObjectAndClassMethods(FTypesLabel);
  FindUnresolvedExportedMethods;
  {FindUnresolvedExportsMethods;}
  FindUnresolvedImplementedClassMethods(FImplementedMethodsLabel);
end;

(**

  This method parse a procedure declaration from the current token position
  using the following object pascal grammar.

  @grammar ProcedureHeading -> PROCEDURE Ident [ FormalParameters ]

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
  strIdentifier: String;
  slClassNames: TStringList;
  iLine, iColumn : Integer;

Begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtProcedure]) Then
    Try
      If PrevToken.UToken = 'CLASS' Then
        C := GetComment(cpBeforePreviousToken)
      Else
        C := GetComment;
      NextNonCommentToken;
      If Not (Token.TokenType In [ttIdentifier, ttDirective]) And boolIdent Then
        Begin
          ErrorAndSeekToken(strIdentExpected, 'ProcedureHeading', Token.Token,
            strSeekableOnErrorTokens, stActual);
          Exit;
        End;
      // Create method and store in collection and get comment
      iLine := 0;
      iColumn := 0;
      slClassNames := TStringList.Create;
      Try
        Try
          If boolIdent Then
            Begin
              strIdentifier := Token.Token;
              iLine := Token.Line;
              iColumn := Token.Column;
              NextNonCommentToken;
              ProcessClsIdents(Container, slClassNames, strIdentifier, iLine,
                iColumn);
              End;
        Finally
          Result := TPascalMethod.Create(mtProcedure, strIdentifier, AScope,
            iLine, iColumn);
          Result.ClassNames.Assign(slClassNames);
          Result.ObjClsInt := FindObjClsInt(slClassNames);
          Result.Comment := C;
        End;
      Finally
        slClassNames.Free;
      End;
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

  @grammar FormalParameter -> '(' FormalParm / ';' ... ')'

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

  This method parses a formal parameter for a method from the current token
  position using the following object pascal grammar.

  @grammar FormalParam -> [ VAR | CONST | OUT ] Parameter

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

  @grammar Parameter -> IdentList [ ':' ( [ ARRAY OF ] SimpleType | STRING | FILE ) ]
                     -> Ident ':' SimpleType '=' ConstExpr

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
  ExprType : TExprTypes;
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
                ExprType := [etConstExpr, etUnknown];
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

  @grammar Directive -> REGISTER ';' DYNAMIC ';' VIRTUAL ';' EXPORT ';'
           EXTERNAL ConstExpr [NAME ConstExpr ';' ] ';' DISPID ConstExpr ';'
           FAR ';' FORWARD ';' MESSAGE ConstExpr ';' OVERRIDE ';' OVERLOAD
           ';' PASCAL ';' REINTRODUCE ';' SAFECALL ';' STDCALL ';'

  @param   M              as a TPascalMethod
  @param   boolGrammarFix as a Boolean

**)
Procedure TPascalModule.Directive(M : TPascalMethod; boolGrammarFix : Boolean = False);

Var
  C : TElementContainer;
  ExprType : TExprTypes;

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
            ExprType := [etConstExpr, etInteger];
            ConstExpr(C, ExprType);
            M.AddDirectives('Message ' + C.AsString(True, False));
          End
        Else If Token.UToken = 'EXTERNAL' Then
          Begin
            M.ForwardDecl := True;
            NextNonCommentToken;
            If Token.Token <> ';' Then
              Begin
                ExprType := [etConstExpr, etString];
                ConstExpr(C, ExprType);
                M.AddDirectives('External');
                M.AddDirectives(C.AsString(True, False));
                If Token.UToken = 'NAME' Then
                  Begin
                    NextNonCommentToken;
                    ExprType := [etConstExpr, etString];
                    ConstExpr(C, ExprType);
                    M.AddDirectives('Name ' + C.AsString(True, False));
                  End;
              End;
          End
        Else If Token.UToken = 'DISPID' Then
          Begin
            NextNonCommentToken;
            ExprType := [etConstExpr, etInteger];
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

  This method parses an Object type declaration from the current token position
  using the followong object pascal grammar.

  @grammar ObjectType -> OBJECT [ ObjHertiage ] [ ObjectFieldList ] [ MethodList ]

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
        Result := TObjectDecl.Create(FToken.Token, FScope, FToken.Line,
          FToken.Column, iiPublicObject, FComment);
      Result := AToken.FContainer.Add(Result) As TObjectDecl;
      Result.Line := AToken.FToken.Line;
      Result.Column := AToken.FToken.Column;
      Result.Comment := AToken.FComment ;
      NextNonCommentToken;
      // Get the classes heritage
      ObjHeritage(Result);
      // If this class has not body then return
      If Token.Token <> ';' Then
        Begin
          Repeat
            ClassVisibility(InternalScope);
            If Token.UToken = 'END' Then
              Break;
          Until Not (
            ClassMethodList(Result, InternalScope) Or
            ClassFieldList(Result, InternalScope)
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
      If Token.TokenType In [ttIdentifier, ttDirective] Then
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

  This method parse a method list from the current token position using the
  following object pascal grammar.

  @precon  Cls is an object declaration to add methods too and Scopeis the 
           current internal scope of the object.
  @postcon Returns true is a method declaration was parsed.

  @param   Cls    as a TRecordDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.MethodList(Cls: TRecordDecl; AScope: TScope): Boolean;

Begin
  Result := MethodHeading(Cls, AScope);
End;

(**

  This method checks for and parses a method declaration in a class from the
  current token position.

  @precon  Cls is an object declaration to add method declarations too and
           Scope is the current scope inside the object declaration.
  @postcon Returns true if a method declaration was parsed.

  @param   Cls    as a TRecordDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.MethodHeading(Cls: TRecordDecl; AScope: TScope): Boolean;

Var
  M :TPascalMethod;
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

  @param   AScope    as a TScope
  @param   Container as a TElementContainer
  @return  a TPascalMethod

**)
function TPascalModule.ConstructorHeading(AScope: TScope;
  Container : TElementContainer): TPascalMethod;

Var
  C : TComment;
  strIdentifier: String;
  slClassNames: TStringList;
  iLine: Integer;
  iColumn: Integer;

begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtConstructor]) Then
    Try
      C := GetComment;
      NextNonCommentToken;
      If (Token.TokenType In [ttIdentifier, ttDirective]) Then
        Begin
          // Create method and store in collection and get comment
          iLine := 0;
          iColumn := 0;
          slClassNames := TStringList.Create;
          Try
            Try
              strIdentifier := Token.Token;
              iLine := Token.Line;
              iColumn := Token.Column;
              NextNonCommentToken;
              ProcessClsIdents(Container, slClassNames, strIdentifier, iLine,
                iColumn);
            Finally
              Result := TPascalMethod.Create(mtConstructor, strIdentifier, AScope,
                iLine, iColumn);
              Result.ClassNames.Assign(slClassNames);
              Result.ObjClsInt := FindObjClsInt(slClassNames);
              Result.Comment := C;
            End;
          Finally
            slClassNames.Free;
          End;
          FormalParameter(Result);
        End;
    Finally
      AddToContainer(Container, Result);
    End;
end;

(**

  This method parses a destructor declaration from the current token position
  using the following object pascal grammar.

  @grammar DestructorHeading -> DESTRUCTOR Ident [ FormalParameters ]

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
  strIdentifier: String;
  slClassNames: TStringList;
  iLine: Integer;
  iColumn: Integer;

begin
  Result := Nil;
  If Token.UToken = UpperCase(strMethodTypes[mtDestructor]) Then
    Try
      C := GetComment;
      NextNonCommentToken;
      If (Token.TokenType In [ttIdentifier, ttDirective]) Then
        Begin
          // Create method and store in collection and get comment
          iLine := 0;
          iColumn := 0;
          slClassNames := TStringList.Create;
          Try
            Try
              strIdentifier := Token.Token;
              iLine := Token.Line;
              iColumn := Token.Column;
              NextNonCommentToken;
              ProcessClsIdents(Container, slClassNames, strIdentifier, iLine,
                iColumn);
            Finally
              Result := TPascalMethod.Create(mtDestructor, strIdentifier, AScope,
                iLine, iColumn);
              Result.ClassNames.Assign(slClassNames);
              Result.ObjClsInt := FindObjClsInt(slClassNames);
              Result.Comment := C;
            End;
          Finally
            slClassNames.Free;
          End;
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


  @grammar ObjFieldList -> ( IndentList ':' Type ) / ';' ...


  @param   Cls    as a TObjectDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.ObjFieldList(Cls: TObjectDecl; AScope: TScope): Boolean;

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

  @grammar InitSection -> INITIALIZATION StmtList [ FINALIZATION StmtList ] END
                       -> BEGIN StmtList End
                       -> END

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
  Else If CompoundStmt Then
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

  @grammar ClassType -> CLASS [ ABSTRACT | SEALED] [ ClassHeritage ]
                          [ ClassFieldList ]
                          [ ClassMethodList ]
                          [ ClassPropertyList ]
                        END

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
      NextNonCommentToken;
      // Check for 'OF'
      If Token.UToken <> 'OF' Then
        Begin
          UpdateTypeToken(AToken);
          With AToken Do
            Result := TClassDecl.Create(FToken.Token, FScope, FToken.Line,
              FToken.Column, iiPublicClass, FComment);
          Result := AToken.FContainer.Add(Result) As TClassDecl;
          Result.Line := AToken.FToken.Line;
          Result.Column := AToken.FToken.Column;
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
          ClassHeritage(Result);
          If Result.HelperClass Then
            If Token.UToken = 'FOR' Then
              Begin
                NextNonCommentToken;
                If Token.TokenType In [ttIdentifier, ttDirective] Then
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
                ClassMethodList(Result, InternalScope) Or
                ClassPropertyList(Result, InternalScope) Or
                TypeSection(InternalScope, Result) Or
                ConstSection(InternalScope, Result) Or
                VarSection(InternalScope, Result) Or
                ClassVarSection(InternalScope, Result) Or
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
          RollBackToken;
    End;
end;

(**

  This method parses a class heriage ist from the current token
  position using the following object pascal grammar.

  @grammar ClassHeritage -> '(' IdentList ')'

  @precon  Cls is a valid object declaration to get a heritage for.
  @postcon Parses a class heriage ist from the current token position

  @param   Cls as a TObjectDecl

**)
procedure TPascalModule.ClassHeritage(Cls: TObjectDecl);

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      IdentList(Cls.Heritage, strSeekableOnErrorTokens);
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

  @grammar ClassVisibility -> [ STATIC ] [ PUBLIC | PROTECTED | PRIVATE | PUBLISHED ]

  @precon  Scope is the current internal scope of the class.
  @postcon Parse the class visibility from the current token

  @param   AScope as a TScope as a reference

**)
procedure TPascalModule.ClassVisibility(var AScope : TScope);
begin
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
end;

(**


  This method parses a class field list from the current token position using
  the following object pascal grammar.


  @precon  Cls is a valid object declaration to add fields too and Scope is the

           current scope of the class.

  @postcon Returns true is field where handled and parsed.


  @grammar ObjFieldList -> ( ClassVisibility ObjFieldList ) / ';' ...


  @param   Cls    as a TObjectDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.ClassFieldList(Cls: TObjectDecl; AScope: TScope): Boolean;

Begin
  Result := ObjFieldList(Cls, AScope);
  If Result Then
    If Token.Token = ';' Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLiteralExpected, 'ClassFieldList', ';',
        strSeekableOnErrorTokens, stActual);
End;

(**

  This method parses a class method list from the current token position using
  the following object pascal grammar.

  @grammar MethodList -> ( ClassVisibility MethodList ) / ';' ...

  @precon  Cls is a valid object declaration to get method for and Scope is the
           current scope of the class.
  @postcon Returns true is method were parsed.

  @param   Cls   as a TRecordDecl
  @param   AScope as a TScope
  @return  a Boolean

**)
Function TPascalModule.ClassMethodList(Cls: TRecordDecl; AScope: TScope): Boolean;

Begin
  Result := MethodList(Cls, AScope);
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

begin
  ClassVisibility(AScope);
  Result := Token.UToken = 'PROPERTY';
  If Result Then
    Begin
      C := GetComment;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier, ttDirective, ttReservedWord] Then
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
          PortabilityDirective;
        End Else
          ErrorAndSeekToken(strIdentExpected, 'PropertyList', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**


  This method parses the property interface from the current token position
  using the following object pascal grammar.


  @precon  Prop is a property to parse an interface for.

  @postcon Parses the property interface from the current token position


  @grammar PropertyInterface -> [ PropertyParameterList ] ':' Ident


  @param   Prop   as a TPascalProperty

**)
Procedure TPascalModule.PropertyInterface(Prop : TPascalProperty);
var
  FTemporaryElements: TElementContainer;

Begin
  PropertyParameterList(Prop);
  // Check for property type id
  If Token.Token = ':' Then
    Begin
      NextNonCommentToken;
      FTemporaryElements := TTempCntr.Create('', scNone, 0, 0, iiNone, Nil);
      Try
        Prop.ReturnType := TTypes.Create('', scNone, 0, 0, iiNone, Nil);
        TypeId(Prop.ReturnType);
      Finally
        FTemporaryElements.Free;
      End;
    End;
End;

(**


  This method parses a properties parameter list from the current token using
  the following object pascal grammar.


  @precon  Prop is a property to parse a parameter list for.

  @postcon Parses a properties parameter list from the current token


  @grammar PropertyParameterList -> '[' ( IdentList ':' TypeId ) / ';' ... ']'


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

  @grammar PropertySpecifiers -> [ INDEX ConstExpr ]
                                 [ READ Ident ]
                                 [ WRITE Ident ]
                                 [ STORED ( Ident | Constant) ]
                                 [ ( DEFAULT ConstExpr ) | NODEFAULT ]
                                 [ IMPLEMENTS TypeId ]

  @precon  Prop is a property to parse specifiers for.
  @postcon Parses the property specifiers from the current token position

  @param   Prop as a TPascalProperty

**)
procedure TPascalModule.PropertySpecifiers(Prop: TPascalProperty);

Var
  C : TPropertySpec;
  ExprType : TExprTypes;

begin
  // Check for index
  If Token.UToken = 'INDEX' Then
    Begin
      NextNonCommentToken;
      ExprType := [etInteger, etConstExpr];
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
      ExprType := [etUnknown];
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
      ExprType := [etUnknown];
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
      ExprType := [etInteger, etConstExpr];
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
      ExprType := [etUnknown, etConstExpr];
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
      ExprType := [etInteger, etConstExpr];
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
            Result := TInterfaceDecl.Create(FToken.Token, FScope, FToken.Line,
              FToken.Column, iiPublicInterface, FComment);
            Result := AToken.FContainer.Add(Result) as TInterfaceDecl;
            Result.Line := AToken.FToken.Line;
            Result.Column := AToken.FToken.Column;
            Result.Comment := AToken.FComment ;
          End Else
          Begin
            Result := TDispInterfaceDecl.Create(FToken.Token, FScope, FToken.Line,
              FToken.Column, iiPublicDispInterface, FComment);
            Result := AToken.FContainer.Add(Result) as TDispInterfaceDecl;
            Result.Line := AToken.FToken.Line;
            Result.Column := AToken.FToken.Column;
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
              NextNonCommentToken;
              If Token.TokenType In [ttSingleLiteral, ttIdentifier] Then
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
        End;
    End;
end;

(**

  This method attempts to parse the current token position as a heritage list.

  @grammar InterfaceHeritage -> '(' IdentList ')'

  @precon  None.
  @postcon Attempts to parse the current token position as a heritage list.

  @param   InterfaceDecl as a TInterfaceDecl

**)
Procedure TPascalModule.InterfaceHeritage(InterfaceDecl : TInterfaceDecl);

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

  @grammar ContainsClause -> CONTAINS IdentList ... ';'

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

  @grammar IdentList -> Ident / ',' ...

  @precon  OwnList determines if the identlist should be disposed of be the
           parser or be the caller. SeekTokens is a sorted lowercase list of
           token to find if an error is found.
  @postcon Returns an ident list.

  @param   Container   as a TElementContainer
  @param   SeekTokens  as an Array Of String
  @param   iImageIndex as a TImageIndex

**)
Procedure TPascalModule.IdentList(Container : TElementContainer;
  SeekTokens : Array Of String; iImageIndex : TImageIndex = iiNone);

Var
  C, AComment : TComment;
  I: TIdentList;
  strUnit : String;
  iLine: Integer;
  iColumn: Integer;

Begin
  AComment := Nil;
  If Token.TokenType In [ttIdentifier, ttDirective] Then
    Repeat
      If Token.TokenType In [ttIdentifier, ttDirective] Then
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
          While Token.Token = '.' Do
            Begin
              strUnit := strUnit + Token.Token;
              NextNonCommentToken;
              If Token.TokenType In [ttIdentifier, ttDirective] Then
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

  @grammar TypeId -> [ UnitId '.' ] <type-identifier>

  @precon  C must be a valid generic container.
  @postcon Returns a type id as a string of text.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TPascalModule.TypeId(Container: TElementContainer) : Boolean;

Begin
  Result := (Token.TokenType In [ttIdentifier, ttDirective]) Or (Token.UToken = 'STRING');
  If Result Then
    Begin
      AddToExpression(Container);
      If Token.Token = '.' Then
        Begin
          AddToExpression(Container);
          If Token.TokenType In [ttIdentifier, ttDirective] Then
            AddToExpression(Container)
          Else
            ErrorAndSeekToken(strIdentExpected, 'TypeId', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End;
    End;
End;

(**


  This method parses a constant expression from the current token position
  using the following object pascal grammar.


  @precon  C is a generic container to add tokens too.

  @postcon Returns true if a constant expression was parsed.


  @grammar ConstExpr -> <constant-expression>


  @param   Container as a TElementContainer
  @param   ExprType  as a TExprTypes as a reference
  @return  a Boolean

**)
Function TPascalModule.ConstExpr(Container : TElementContainer;
  var ExprType : TExprTypes) : Boolean;

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

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TResourceString.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowUndocumentedConsts In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strResourceStringDocumentation,
        DocConflictTable[dctResourceStringClauseUndocumented]);
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TThreadVar.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowUndocumentedVars In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strThreadVarDocumentation, DocConflictTable[dctThreadVarClauseUndocumented]);
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TRecordDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedRecords In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strRecordDocumentation, DocConflictTable[dctRecordClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method searches for reference to the passed symbol in the records
  fields.

  @precon  None.
  @postcon Returns true if the symbol is found.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TRecordDecl.ReferenceSymbol(AToken : TTokenInfo) : Boolean;
begin
  Result := ReferenceSection(AToken, FFieldsLabel);
end;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TObjectDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedObjects In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strObjectDocumentation, DocConflictTable[dctObjectClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TClassDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedClasses In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strClassDocumentation, DocConflictTable[dctClassClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method searches for reference to trhe passed symbol in the classes
  various section.

  @precon  None.
  @postcon Returns true if the symbol is found.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TClassDecl.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

begin
  Result := Inherited ReferenceSymbol(AToken);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strVarsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strConstantsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strTypesLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strClassVarsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strPropertiesLabel) As TLabelContainer);
end;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TInterfaceDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedInterfaces In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strInterfaceDocumentation, DocConflictTable[dctInterfaceClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TDispInterfaceDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedInterfaces In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strDispInterfaceDocumentation, DocConflictTable[dctDispInterfaceClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method check the module`s initialisation sections for comments.

  @precon  None.
  @postcon Check the module`s initialisation sections for comments.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TInitializationSection.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowMissingInitComment In BrowseAndDocItOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([strInitializationLabel], Line, Column , Comment,
        strModuleInitSection, DocConflictTable[dctMissingInitComment]);
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the name of the Initialisation section as a String .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TInitializationSection.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;
begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
end;

(**

  This method check the module`s finalisation sections for comments.

  @precon  None.
  @postcon Check the module`s finalisation sections for comments.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TFinalizationSection.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowMissingFinalComment In BrowseAndDocItOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([strFinalizationLabel], Line, Column, Comment,
        strModuleFinalSection, DocConflictTable[dctMissingFinalComment]);
End;

(**

This is a getter method for the AsString property.

  @precon  None . 
  @postcon Returns the name of the Finalisation section as a String . 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TFinalizationSection.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;
begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
end;

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

    This method adds the number to the stack and increments the iSkip variable
    by the value passed.

    @precon  None.
    @postcon Adds the number to the stack and increments the iSkip variable
             by the value passed.

    @param   iValue as an Integer

  **)
  Procedure IncSkip(iValue : Integer);

  Begin
    CompilerConditionStack.Add(Pointer(iValue));
    Inc(iSkip, iValue);
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
    iStackTop : Integer;
    iStack: Integer;

  Begin
    Result := False;
    iStackTop := CompilerConditionStack.Count - 1;
    If iStackTop >= 0 Then
      Begin
        iStack := Integer(CompilerConditionStack[iStackTop]);
        CompilerConditionUndoStack.Add(Pointer(iStack));
        If iStack = 1 Then
          Dec(iSkip);
        CompilerConditionStack.Delete(iStackTop);
      End Else
        Result := True;
  End;

Var
  iStack, iStackTop : Integer;

begin
  If Like(Token.Token, '{$DEFINE ') Then
    AddDef(GetDef)
  Else If Like(Token.Token, '{$UNDEF ') Then
    DeleteDef(GetDef)
  Else If Like(Token.Token, '{$IFDEF ') Then
    Begin
      If Not IfDef(GetDef) Then
        IncSkip(1)
      Else
        IncSkip(0);
    End
  Else If Like(Token.Token, '{$IFOPT ') Then
    Begin
      If Not IfDef(GetDef) Then
        IncSkip(1)
      Else
        IncSkip(0);
    End
  Else If Like(Token.Token, '{$IF ') Then
    IncSkip(0) // FAKE $IF by defaulting to TRUE
  Else If Like(Token.Token, '{$IFNDEF ') Then
    Begin
      If Not IfNotDef(GetDef) Then
        IncSkip(1)
      Else
        IncSkip(0);
    End
  Else If Like(Token.Token, '{$ELSE') Then
    Begin
      iStackTop := CompilerConditionStack.Count - 1;
      If iStackTop >= 0 Then
        Begin
          iStack := Integer(CompilerConditionStack[iStackTop]);
          If iStack = 1 Then
            Begin
              CompilerConditionStack[iStackTop] := Pointer(0);
              Dec(iSkip)
            End Else
            Begin
              CompilerConditionStack[iStackTop] := Pointer(1);
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
  Else If Like(Token.Token, '{$EXTERNALSYM') Then
    FExternalSyms.Add(GetDef);
  If iSkip < 0 Then
    iSkip := 0;
end;

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
            If (Method.ObjClsInt <> Nil) And Not Method.Resolved Then
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
  @return  a TObjectDecl

**)
Function TPascalModule.FindObjClsInt(slClassNames: TStringList): TObjectDecl;

Var
  i: Integer;
  E: TElementContainer;

Begin
  Result := Nil;
  E := Self;
  For i := 0 To slClassNames.Count -  1 Do
    Begin
      E := E.FindElement(strTypesLabel);
      If E <> Nil Then
        E := E.FindElement(slClassNames[i]);
    End;
  If E <> Nil Then
    If E Is TObjectDecl Then
      Result := E As TObjectDecl;
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
procedure TPascalModule.FindUnresolvedObjectAndClassMethods(TypeLabel : TLabelContainer);

  (**


    This function walks backwards through the heirarchy to find all the
    qualifying objects and classes.

    @precon  None.
    @postcon Walks backwards through the heirarchy to find all the
             qualifying objects and classes.


    @param   ObjOrCls as a TObjectDecl
    @return  a String

  **)
  Function GetClassQualification(ObjOrCls : TObjectDecl) : String;

  Var
    P : TElementContainer;

  Begin
    Result := '';
    P := ObjOrCls;
    While P <> Nil Do
      Begin
        If P Is TObjectDecl Then
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
  ObjectOrClass: TObjectDecl;
  k: Integer;
  ClassTypeLabel: TLabelContainer;

begin
  If TypeLabel <> Nil Then
    For k := 1 To TypeLabel.ElementCount Do
      Begin
        If (TypeLabel.Elements[k] Is TObjectDecl) And Not
          (TypeLabel.Elements[k] Is TInterfaceDecl) Then
          Begin
            ObjectOrClass := TypeLabel.Elements[k] As TObjectDecl;
            MethodsLabel := ObjectOrClass.FindElement(strMethodsLabel);
            If MethodsLabel <> Nil Then
              For j := 1 To MethodsLabel.ElementCount Do
                If MethodsLabel.Elements[j] Is TPascalMethod Then
                  Begin
                    Method := MethodsLabel.Elements[j] As TPascalMethod;
                    If ShouldMethodHaveImplementation(Method) Then
                      AddIssue(Format(strUnSatisfiedForwardReference,
                        [GetClassQualification(ObjectOrClass) + Method.Identifier]),
                        scNone, 'FindUnresolvedObjectAndClassMethods', Method.Line,
                        Method.Column, etWarning);
                  End;
            ClassTypeLabel := ObjectOrClass.FindElement(strTypesLabel) As TLabelContainer;
            If ClassTypeLabel <> Nil Then
              FindUnresolvedObjectAndClassMethods(ClassTypeLabel);
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
procedure TPascalModule.ResolveScopeOfImplementedClassMethods(
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
          If Method.ObjClsInt <> Nil Then
            Begin
              Element := Method.ObjClsInt.FindElement(strMethodsLabel);
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
            End;
        End Else
          ResolveScopeOfImplementedClassMethods(
            StartLabel.Elements[i] As TLabelContainer);
end;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Returns a string representation of the class information . 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TIdentList.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    '', BrowseAndDocItOptions.MaxDocOutputWidth)
end;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Returns a string representation of the class information . 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TTempCntr.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;
begin
  Result := '';
  Raise Exception.Create(strTriedToRenderTmpCntr);
end;

(**

  This is a getter method for the AsString property.

  @precon  None . 
  @postcon Returns a string representation of the type . 

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String              

**)
function TArrayType.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := BuildStringRepresentation(boolShowIdentifier And (Identifier <> ''),
    boolForDocumentation, '=', BrowseAndDocItOptions.MaxDocOutputWidth);
end;

End.
