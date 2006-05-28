(**

  This module contains the base class for all language module to derived from
  and all standard constants across which all language modules have in common.

  @Date    28 May 2006
  @Version 1.0
  @Author  David Hoyle

**)

Unit BaseLanguageModule;

Interface
Uses
  SysUtils, Classes, Contnrs;

Type

  (** This is a record that contains the description and the default for a
      TDocOption enumerate. **)
  TDocOptionRec = Record
    Description : String;
    Enabled : Boolean;
  End;

  (** Type to distinguish Stream position from token index. **)
  TStreamPosition = Integer;
  (** Type to distinguish Stream position from token index. **)
  TTokenIndex = Integer;
  (** An enumerate type to define the stream status and token types. **)
  TTokenType = (ttUnknown, ttWhiteSpace, ttReservedWord, ttIdentifier, ttNumber,
    ttSymbol, ttLineEnd, ttArrayElement, ttStatementEnd, ttStringLiteral,
    ttComment, ttHTMLTag, ttDirective, ttCompilerDirective, ttLinkTag);
  (** An enumerate for the scoping of identifiers. **)
  TScope = (scGlobal, scLocal, scPrivate, scProtected, scPublic, scPublished);
  (** A set to represent combinations of scopes. **)
  TScopes = Set Of TScope;
  (** An enumerate for the parameter modifiers of methods. **)
  TParamModifier = (pmNone, pmVar, pmConst, pmOut);
  (** An enumerate for the types of modules that can be parsed. **)
  TModuleType = (mtProgram, mtPackage, mtLibrary, mtUnit);
  (** An enumerate for the different methods. **)
  TMethodType = (mtConstructor, mtDestructor, mtProcedure, mtFunction);
  (** An enumerate for warning and errors. **)
  TErrorType = (etWarning, etError);

  (** This is a list of options valable for the display of module information
      with in the module explorer.
      @bug At the moment these option are not passed early enough for the
           doManageExpandedNodes option to be available to the LoadSettings
           and SaveSettings methods.
  **)
  TDocOption = (
    doCustomDrawing,
    doShowCommentHints,
    doShowLocals,
    doShowPrivates,
    doShowProtecteds,
    doShowPublics,
    doShowPublisheds,
    doShowLocalProcs,
    doShowConflicts,
    doManageExpandedNodes,
    doShowMissingProcDocs,
    doShowMissingDocDesc,
    doShowDiffParamCount,
    doShowUndocumentedParams,
    doShowIncorrectParamType,
    doShowUndocumentedReturn,
    doShowIncorrectReturnType,
    doShowUndocumentedTypes,
    doShowUndocumentedRecords,
    doShowUndocumentedObjects,
    doShowUndocumentedClasses,
    doShowUndocumentedInterfaces,
    doShowUndocumentedVars,
    doShowUndocumentedConsts,
    doShowUndocumentedModule,
    doShowMissingModuleDate,
    doShowCheckModuleDate,
    doShowMissingModuleVersion,
    doShowMissingModuleAuthor,
    doShowMissingPreCons,
    doShowMissingPostCons,
    doShowMissingPropertyDoc,
    doShowMissingPropDocDesc,
    doShowDiffPropParamCount,
    doShowUndocPropParam,
    doShowIncorrectPropParamType,
    doShowUndocPropReturn,
    doShowIncorrectPropReturnType,
    doShowMissingPropPreCons,
    doShowMissingPropPostCons,
    doCategoriesConflicts
  );

  (** This is a set of display options. **)
  TDocOptions = Set of TDocOption;

  (** A class to hold text about a single tag **)
  TTag = Class
  Private
    FTokens : TStringList;
    FTagName: String;
    FLine: Integer;
    FColumn: Integer;
    function GetToken(iTokenIndex: Integer): String;
    procedure SetTagName(const Value: String);
    function GetTokenCount : Integer;
    function GetTokenType(iTokenIndex: Integer): TTokenType;
  Public
    Constructor Create(strName : String; iLine, iColumn : Integer); Overload;
    Destructor Destroy; Override;
    Procedure AddToken(strToken : String; iType : TTokenType);
    Function AsString(ShowHTML : Boolean) : String;
    (**
      Returns the tag name as a string.
      @return  a String
    **)
    Property TagName : String read FTagName write SetTagName;
    (**
      Returns the specifically index token from the tags token collection.
      @param   iTokenIndex as       an Integer
      @return  a String
    **)
    Property Token[iTokenIndex : Integer] : String read GetToken; Default;
    (**
      Returns the specifically index tokens type from the tags token collection.
      @param   iTokenIndex as       an Integer
      @return  a TTokenType
    **)
    Property TokenType[iTokenIndex : Integer] : TTokenType read GetTokenType;
    (**
      Returns the number of token in the tag.
      @return  an Integer
    **)
    Property TokenCount : Integer read GetTokenCount;
    (**
      Returns the line number of the tag.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column position of the tag.
      @return  an Integer
    **)
    Property Column : Integer Read FColumn;
  End;

  (** A class the handles and stores all the comment information **)
  TComment = Class
  Private
    FTokens : TStringList;
    FTags : TObjectList;
    FTagMode : Boolean;
    FLastTag : TTag;
    FLine : Integer;
    FCol : Integer;
    FTagLine : Integer;
    FTagColumn : Integer;
    function GetTag(iTagIndex: Integer): TTag;
    function GetTagCount: Integer;
    function GetToken(iTokenIndex: Integer): String;
    function GetTokenCount: Integer;
    function GetTokenType(iTokenIndex: Integer): TTokenType;
    Function GetLine : Integer;
    Function GetCol : Integer;
  Public
    Constructor Create(strComment : String; iLine, iCol : Integer); Overload;
    Destructor Destroy; Override;
    Class Function CreateComment(strComment : String; iLine, iCol : Integer) : TComment;
    Procedure AddToken(strToken : String; iType : TTokenType);
    Procedure ParseComment(strComment : String);
    Procedure Assign(srcComment : TComment); Overload;
    Procedure Assign(strComment : String); Overload;
    procedure ResetTagMode;
    Function AsString(iIndent, iMaxWidth : Integer; ShowHTML : Boolean) : String;
    Function FindTag(strTagName : String) : Integer;
    (**
      Returns the specifically indexed token from the collection.
      @param   iTokenIndex as       an Integer
      @return  a String
    **)
    Property Token[iTokenIndex : Integer] : String read GetToken; Default;
    (**
      Returns the number of tokens found in the comment.
      @return  an Integer
    **)
    Property TokenCount : Integer read GetTokenCount;
    (**
      Returns the specifically indexed tokens type from the token collection.
      @param   iTokenIndex as       an Integer
      @return  a TTokenType
    **)
    Property TokenType[iTokenIndex : Integer] : TTokenType read GetTokenType;
    (**
      Returns the specifically indexed tag from the comments tag collection.
      @param   iTagIndex as       an Integer
      @return  a TTag
    **)
    Property Tag[iTagIndex : Integer] : TTag Read GetTag;
    (**
      Returns the number of tags found within the comment.
      @return  an Integer
    **)
    Property TagCount : Integer Read GetTagCount;
    (**
      Returns the line number of the comment.
      @return  an Integer
    **)
    Property Line : Integer Read GetLine;
    (**
      Returns the column number of the comment.
      @return  an Integer
    **)
    Property Col : Integer Read GetCol;
  End;

  (** Forward declaration. **)
  TMethodDecl = Class;

  (** A record type to contain a token and its line and column in the editor. **)
  TIdentInfo = Record
    Ident : String;
    Line : Integer;
    Col : Integer;
    Scope : TScope;
    Method : TMethodDecl;
  End;

  (** This is a class the store information about each token **)
  TTokenInfo = Class
  private
    FToken : String;
    FColumn : Integer;
    FBufferPos: Integer;
    FLine: Integer;
    FLength : Integer;
    FTokenType: TTokenType;
    FUToken : String;
  Public
    Constructor Create(strToken : String; iPos, iLine, iCol,
      iLength : Integer; TType : TTokenType); Overload;
    (**
      Returns the token as a string.
      @return  a String
    **)
    Property Token : String read FToken;
    (**
      Returns the uppercase version of the token. Used for keyword comparisons.
      @return  a String
    **)
    Property UToken : String read FUToken;
    (**
      Returns the buffer position of the token start point.
      @return  an Integer
    **)
    Property BufferPos : Integer read FBufferPos;
    (**
      Returns the line number of the token start point.
      @return  an Integer
    **)
    Property Line : Integer read FLine;
    (**
      Returns the column number of the token start point.
      @return  an Integer
    **)
    Property Column : Integer read FColumn;
    (**
      Returns the length of the token.
      @return  an Integer
    **)
    Property Length : Integer read FLength;
    (**
      Returns the token type for the token.
      @return  a TTokenType
    **)
    Property TokenType : TTokenType read FTokenType;
  End;

  (** This class represents a single identifier with line, col and comment
      attributes. **)
  TIdent = Class
  Private
    FLine: Integer;
    FCol: Integer;
    FIdent: String;
    FComment: TComment;
  Public
    Constructor Create(strIdent : String; iLine, iCol : Integer; Comment : TComment);
    (**
      Returns the identifiers name as a string.
      @return  a String
    **)
    Property Ident : String Read FIdent;
    (**
      Returns the line number of the idenifier.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the identifier.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns the comment associated with the identifier.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment;
  End;

  (** This class represents a list of identifiers **)
  TIdentList = Class
  Private
    FIdents : TObjectList;
    FComment : TComment;
    Function GetIdentInfo(iIndex : Integer) : TIdent;
    Function GetCount : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(strIdent : String; iLine, iCol : Integer; Comment : TComment);
    Procedure Assign(src : TIdentList);
    Procedure Sort;
    Function AsString : String;
    (**
      Returns the specifically indexed identifier in the list.
      @param   iIndex as       an Integer
      @return  a TIdent
    **)
    Property Idents[iIndex : Integer] : TIdent Read GetIdentInfo; Default;
    (**
      Returns the number of identifiers in the list.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      Returns the comment associated with the identifier list.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
  End;

  (** This class holds the information for a constant. **)
  TGenericContainer = Class
  Private
    FTokens : TStringList;
    FComment : TComment;
    FIdentifier : String;
    FScope : TScope;
    FLine : Integer;
    FCol : Integer;
    Function GetToken(iIndex : Integer) : String;
    Function GetTokenCount : Integer;
  Public
    Constructor Create; Overload; Virtual;
    Constructor Create(strIdentifier : String; Scope : TScope;
      iLine, iCol : Integer); Overload; Virtual;
    Constructor Create(Ident : TIdentInfo); Overload; Virtual;
    Destructor Destroy; Override;
    Procedure Add(strToken : String);
    Procedure Assign(src : TPersistent);
    Procedure Append(src : TGenericContainer);
    Procedure Insert(strText : String; iIndex : Integer);
    Procedure Sort; Virtual;
    Function AsString(ShowFirstToken : Boolean) : String; Virtual;
    (**
      Returns the identifier for the generic container.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier Write FIdentifier;
    (**
      Returns the specifically indexed token in the container.
      @param   iIndex as       an Integer
      @return  a String
    **)
    Property Token[iIndex : Integer] : String Read GetToken; Default;
    (**
      Returns the number of tokens in the generic container.
      @return  an Integer
    **)
    Property Count : Integer Read GetTokenCount;
    (**
      Returns the comment associated with the generic container.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the scope of the generic container.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
    (**
      Returns the line number for the generic container.
      @return  an Integer
    **)
    Property Line : Integer Read FLine Write FLine;
    (**
      Returns the column number of the generic container.
      @return  an Integer
    **)
    Property Col : Integer Read FCol Write FCol;
  End;

  (** This is a sub class for all types **)
  TTypes = Class(TGenericContainer);
  (** This is a sub class for Ordinal types **)
  TOrdinalType = Class(TTypes);
  (** This is a sub class for Real types **)
  TRealType = Class(TTypes);
  (** This is a sub class for Pointer types **)
  TPointerType = Class(TTypes);
  (** This is a sub class for String types **)
  TStringType = Class(TTypes);
  (** This is a sub class for Procedure types **)
  TProcedureType = Class(TTypes);
  (** This is a sub class for Variant types **)
  TVariantType = Class(TTypes);
  (** This is a sub class for Class Ref types **)
  TClassRefType = Class(TTypes);
  (** This is a sub class for Array types **)
  TArrayType = Class(TTypes);
  (** This is a sub class for Set types **)
  TSetType = Class(TTypes);
  (** This is a sub class for File types **)
  TFileType = Class(TTypes);

  (** This is a sub class for all constants. **)
  TConstant = Class(TGenericContainer);
  (** This is a sub class for all resource strings. **)
  TResourceString = Class(TConstant);
  (** This is a sub class for all variables. **)
  TVar = Class(TGenericContainer);
  (** This is a sub class for all thread variables. **)
  TThreadVar = Class(TVar);

  (** This is a collaboration class for storing the constants. **)
  TGenericContainerCollection = Class
  Private
    FItems : TObjectList;
    FComment : TComment;
    Function GetCount : Integer;
    Function GetItem(iIndex : Integer) : TGenericContainer;
  Public
    Constructor Create(OwnItems : Boolean);
    Destructor Destroy; Override;
    procedure RemoveForwardDecls;
    Procedure Add(AItem : TGenericContainer);
    Function Find(strClassName : String) : Integer;
    Procedure Sort;
    (**
      Returns the comment associcate with this collection.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the specifically indexed item from the generic collection.
      @param   iIndex as       an Integer
      @return  a TGenericContainer
    **)
    Property Items[iIndex : Integer] : TGenericContainer Read GetItem; Default;
    (**
      Returns the number of items in the generic collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** This class represents a parameter of a method declaration. **)
  TParameter = Class
  Private
    FParamModifier : TParamModifier;
    FIdentifier : String;
    FArrayOf : Boolean;
    FParamType : TTypes;
    FDefaultValue : String;
    FComment : TComment;
    FScope : TScope;
    FLine: Integer;
    FCol: Integer;
  Public
    Constructor Create(ParamMod : TParamModifier; Ident : String;
      boolArrayOf : Boolean; AType : TTypes; Value : String;
      Scope : TScope; iLine, iCol : Integer); Overload;
    Procedure Assign(Parameter : TParameter);
    Destructor Destroy; Override;
    (**
      Returns the parameter modifier : const, var or out.
      @return  a TParamModifier
    **)
    Property ParamModifier : TParamModifier Read FParamModifier;
    (**
      Returns the parameters idenitfier.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier;
    (**
      Returns whether the parameter is an array parameter.
      @return  a Boolean
    **)
    Property ArrayOf : Boolean Read FArrayOf;
    (**
      Returns the parameter type identifier for the parameter.
      @return  a TTypes
    **)
    Property ParamType : TTypes Read FParamType;
    (**
      Returns the default value of the parameter is there is one.
      @return  a String
    **)
    Property DefaultValue : String Read FDefaultValue;
    (**
      Returns the comment associcated with the parameter (field).
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the parameters scope with in the record / object / class etc.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope;
    (**
      Returns the parameters line number.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the parameters column number.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
  End;

  (** This is a forward declaration so that a method declaration can contain
  other methods. **)
  TMethodCollection = Class;

  (** This class represents a method declaration. **)
  TMethodDecl = Class
  Private
    FComment : TComment;
    FMethodType : TMethodType;
    FClsName : String;
    FIdentifier : String;
    FParameter : TObjectList;
    FReturnType : String;
    FDirectives : TStringList;
    FScope : TScope;
    FMsg: String;
    FExt: String;
    FClassMethod : Boolean;
    FLine: Integer;
    FCol: Integer;
    FLocalMethods : TMethodCollection;
    FTypes : TGenericContainerCollection;
    FVars : TGenericContainerCollection;
    FConsts : TGenericContainerCollection;
    FResStrings : TGenericContainerCollection;
    FAlias: String;
    Procedure SetClsName(Value : String);
    Procedure SetIdentifier(Value : String);
    Function GetParameter(iIndex : Integer) : TParameter;
    Function GetCount : Integer;
    Procedure SetReturnType(Value : String);
    procedure SetMsg(const Value: String);
    procedure SetExt(const Value: String);
    Function GetQualifiedName : String;
  Public
    Constructor Create(MethodType : TMethodType; Scope : TScope;
      iLine, iCol : Integer);
    Destructor Destroy; Override;
    Procedure Add(Parameter : TParameter);
    Procedure AddDirectives(strDirective : String);
    Function GetAsString(ShowClassName, ShowMethodType : Boolean): String;
    Function HasDirective(strDirective : String) : Boolean;
    Procedure Assign(Method : TMethodDecl);
    (**
      Returns the comment associated with the method.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the methods types, procedure, function, constructor, destructor.
      @return  a TMethodType
    **)
    Property MethodType : TMethodType Read FMethodType;
    (**
      Returns the methods class name.
      @return  a String
    **)
    Property ClsName : String Read FClsName Write SetClsName;
    (**
      Returns the methods idenitifier.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier Write SetIdentifier;
    (**
      Returns the specifically indexed parameter of the method.
      @param   iIndex as       an Integer
      @return  a TParameter
    **)
    Property Parameter[iIndex : Integer] : TParameter Read GetParameter; Default;
    (**
      Returns the number of parameter the method has.
      @return  an Integer
    **)
    Property ParameterCount : Integer Read GetCount;
    (**
      Returns the return type of the method if it is a function.
      @return  a String
    **)
    Property ReturnType : String Read FReturnType Write SetReturnType;
    (**
      Returns the string list of directives associated with the method.
      @return  a TStringList
    **)
    Property Directives : TStringList Read FDirectives;
    (**
      Returns the internal scope of the method within the class / interface /
      module etc.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
    (**
      Returns the associated message for the method if the method is a message
      handler.
      @return  a String
    **)
    Property Msg : String Read FMsg Write SetMsg;
    (**
      Returns the external reference for the method if there is one.
      @return  a String
    **)
    Property Ext : String Read FExt Write SetExt;
    (**
      Return whether the method is a class method.
      @return  a Boolean
    **)
    Property ClassMethod : Boolean Read FClassMethod Write FClassMethod;
    (**
      Returns the line number of the method.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the method.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns a reference to the local methods collection of this method.
      @return  a TMethodCollection
    **)
    Property LocalMethods : TMethodCollection Read FLocalMethods;
    (**
      Returns a reference to the types collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property Types : TGenericContainerCollection Read FTypes;
    (**
      Returns a reference to the variables collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property Vars : TGenericContainerCollection Read FVars;
    (**
      Returns a reference to the constants collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property Consts : TGenericContainerCollection Read FConsts;
    (**
      Returns a reference to the resource string collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property ResStrings : TGenericContainerCollection Read FResStrings;
    (**
      Returns the Qualified name of the method.
      @return  a String
    **)
    Property QualifiedName : String Read GetQualifiedName;
    (**
      Returns the method alias name.
      @return  a String
    **)
    Property Alias : String Read FAlias Write FAlias;
  End;

  (** This is a collaboration class to hold instances of method declarations. **)
  TMethodCollection = Class
  Private
    FMethods : TObjectList;
    Function GetMethod(iIndex : Integer) : TMethodDecl;
    Function GetCount : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Method : TMethodDecl);
    Function Find(strMethodName : String) : Integer;
    Procedure Sort;
    (**
      Returns the specifically indexed method from the methods collection.
      @param   iIndex as       an Integer
      @return  a TMethodDecl
    **)
    Property Method[iIndex : Integer] : TMethodDecl Read GetMethod; Default;
    (**
      Returns the number of methods in the methods collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** This is a class that represents properties of a class or interface. **)
  TProperty = Class
  Private
    FParameter : TObjectList;
    FIdentifier : String;
    FTypeId : String;
    FIndexSpec : String;
    FScope : TScope;
    FWriteSpec: String;
    FImplementsSpec: String;
    FStoredSpec: String;
    FDefaultSpec: String;
    FReadSpec: String;
    FComment: TComment;
    FDefaultProperty: Boolean;
    FLine: Integer;
    FCol: Integer;
    FDispIDSpec: String;
    FReadOnlySpec: Boolean;
    FWriteOnlySpec: Boolean;
    Function GetAsString : String;
    function GetParameter(iIndex: Integer): TParameter;
    procedure SetDefaultSpec(const Value: String);
    procedure SetImplementsSpec(const Value: String);
    procedure SetIndexSpec(const Value: String);
    procedure SetReadSpec(const Value: String);
    procedure SetStoredSpec(const Value: String);
    procedure SetTypeId(const Value: String);
    procedure SetWriteSpec(const Value: String);
    function GetParameterCount: Integer;
    procedure SetDefaultProperty(const Value: Boolean);
  Public
    Constructor Create(strIdent : String; Scope : TScope; iLine, iCol : Integer);
    Destructor Destroy; Override;
    Procedure AddParameter(Parameter : TParameter);
    Procedure Assign(Prop : TProperty);
    (**
      Returns the identifier of the property.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier;
    (**
      Returns the specifically indexed parameter of the property.
      @param   iIndex as       an Integer
      @return  a TParameter
    **)
    Property Parameter[iIndex : Integer] : TParameter Read GetParameter;
    (**
      Returns the number of parameter the property has.
      @return  an Integer
    **)
    Property ParameterCount : Integer Read GetParameterCount;
    (**
      Returns the type identifier of the property.
      @return  a String
    **)
    Property TypeId : String Read FTypeId Write SetTypeId;
    (**
      Returns the
      @return  a String
    **)
    Property IndexSpec : String Read FIndexSpec Write SetIndexSpec;
    (**
      Returns the properties Read specification.
      @return  a String
    **)
    Property ReadSpec : String Read FReadSpec Write SetReadSpec;
    (**
      Returns the properties write specification.
      @return  a String
    **)
    Property WriteSpec : String Read FWriteSpec Write SetWriteSpec;
    (**
      Returns the properties Stored specification.
      @return  a String
    **)
    Property StoredSpec : String Read FStoredSpec Write SetStoredSpec;
    (**
      Returns the property default value.
      @return  a String
    **)
    Property DefaultSpec : String Read FDefaultSpec Write SetDefaultSpec;
    (**
      Returns whether this property is the classes / interfaces default
      property.
      @return  a Boolean
    **)
    Property DefaultProperty : Boolean Read FDefaultProperty Write SetDefaultProperty;
    (**
      Returns the implements specification for the property.
      @return  a String
    **)
    Property ImplementsSpec : String Read FImplementsSpec Write SetImplementsSpec;
    (**
      Returns a string represnetation of the property with all the appropriate
      specifiers.
      @return  a String
    **)
    Property AsString : String Read GetAsString;
    (**
      Returns the comment associated with the property.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the scope with in the class / interface of the property.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope;
    (**
      Returns the line number of the property.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the property.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns the properties DispID reference.
      @return  a String
    **)
    Property DispIdSpec : String Read FDispIDSpec Write FDispIDSpec;
    (**
      Returns whether the property has a ReadOnly specification.
      @return  a Boolean
    **)
    Property ReadOnlySpec : Boolean Read FReadOnlySpec Write FReadOnlySpec;
    (**
      Returns whether the property has a WriteOnly specification.
      @return  a Boolean
    **)
    Property WriteOnlySpec : Boolean Read FWriteOnlySpec Write FWriteOnlySpec;
  End;

  (** This is a class that represents a record definition. **)
  TRecordDecl = Class(TTypes)
  Private
    FPacked : Boolean;
    FParameter : TObjectList;
    Function GetParameter(iIndex : Integer) : TParameter;
    Function GetParameterCount : Integer;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure AddParameter(Parameter : TParameter);
    Procedure Sort; Override;
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    (**
      Returns the specifically indexed parameter in the record.
      @param   iIndex as       an Integer
      @return  a TParameter
    **)
    Property Parameter[iIndex : Integer] : TParameter Read GetParameter;
    (**
      Returns the number of parameter (fields) in the record.
      @return  an Integer
    **)
    Property ParameterCount : Integer Read GetParameterCount;
    (**
      Returns whether the record is packed or not.
      @return  a Boolean
    **)
    Property IsPacked : Boolean Read FPacked Write FPacked;
  End;

  (** This is a class the extends the record definition to handle an object
  definition **)
  TObjectDecl = Class(TRecordDecl)
  Private
    FMethod : TObjectList;
    FHeritage : TIdentList;
    Function GetMethodDecl(iIndex : Integer) : TMethodDecl;
    Function GetMethodCount : Integer;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure AddMethod(Method : TMethodDecl);
    Function FindMethod(strMethodName : String) : Integer;
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    Procedure Assign(AObject : TObjectDecl); Virtual;
    Procedure Sort; Override;
    (**
      Returns the indexed method in the objects methods collection.
      @param   iIndex as       an Integer
      @return  a TMethodDecl
    **)
    Property Method[iIndex : Integer] : TMethodDecl Read GetMethodDecl;
    (**
      Returns the number of method in the object methods collection.
      @return  an Integer
    **)
    Property MethodCount : Integer Read GetMethodCount;
    (**
      Returns a reference to the object class heritage.
      @return  a TIdentList
    **)
    Property Heritage : TIdentList Read FHeritage;
  End;

  (** This is a class the extends the object definition to handle an class
  definition **)
  TClassDecl = Class(TObjectDecl)
  Private
    FProperty : TObjectList;
    FAbstractClass: Boolean;
    Function GetProperty(iIndex : Integer) : TProperty;
    Function GetPropertyCount : Integer;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure AddProperty(Prop : TProperty);
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    Procedure Assign(AObject : TObjectDecl); Override;
    Procedure Sort; Override;
    (**
      Returns the indexed property for the class.
      @param   iIndex as       an Integer
      @return  a TProperty
    **)
    Property Properties[iIndex : Integer] : TProperty Read GetProperty;
    (**
      Returns the number of properties in the classes properties collection.
      @return  an Integer
    **)
    Property PropertyCount : Integer Read GetPropertyCount;
    Property AbstractClass : Boolean Read FAbstractClass Write FAbstractClass;
  End;

  (** This is a class the extends the class definition to handle an interface
  definition **)
  TInterfaceDecl = Class(TClassDecl)
  Private
    FGUID : String;
    Procedure SetGUID(Value : String);
  Public
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    Procedure Assign(AObject : TObjectDecl); Override;
    (**
      Returns the GUID for the interface.
      @return  a String
    **)
    Property GUID : String Read FGUID Write SetGUID;
  End;

  (** This is a class the extends the class definition to handle an interface
  definition **)
  TDispInterfaceDecl = Class(TInterfaceDecl)
  Public
    Function AsString(ShowFirstToken : Boolean) : String; Override;
  End;

  (** This class defines a parsing error. **)
  TDocError = Class
  Private
    FLine: Integer;
    FCol: Integer;
    FMsg: String;
    FExceptionMethod: String;
    FErrorType : TErrorType;
  Public
    Constructor Create(strMsg : String; iLine, iCol : Integer;
      strExceptionMethod : String; ErrType : TErrorType); Overload;
    (**
      Returns the exception message.
      @return  a String
    **)
    Property Msg : String Read FMsg;
    (**
      Returns the line number where the exception occurred.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number where the exception occurred.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns the Exception method of the exception stored.
      @return  a String
    **)
    Property ExceptionMethod : String Read FExceptionMethod;
    (**
      Returns the error type for the error.
      @return  a TErrorType
    **)
    Property ErrorType : TErrorType Read FErrorType Write FErrorType;
  End;

  (** This class provides a collection for the parsing errors. **)
  TDocErrorCollection = Class
  Private
    FErrors : TObjectList;
    function GetCount: Integer;
    function GetError(iIndex: Integer): TDocError;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(strMsg : String; iLine, iCol : Integer;
      strMethodName : String; ErrType : TErrorType);
    (**
      Returns the number of errors in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      Returns the specifically indexed error from the collection.
      @param   iIndex as       an Integer
      @return  a TDocError
    **)
    Property Errors[iIndex : Integer] : TDocError Read GetError; Default;
  End;

  (**

    This is an augmented Exception class to hold information about the
    line number and column number of the error.

  **)
  EDocException = Class(Exception)
  Private
    FLine : Integer;
    FCol : Integer;
    FExceptionMethod: String;
  Public
    Constructor CreateNormal(strMsg : String; Token : TTokenInfo;
      strMethodName : String);
    Constructor CreateLiteral(strMsg, strLiteral : String; Token : TTokenInfo;
      strMethodName : String);
    (**
      Returns the line number of the documentation exception.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the documentation exception.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns a string representing the classes ExceptionMethod.
      @return  a String
    **)
    Property ExceptionMethod : String Read FExceptionMethod;
  End;

  (** This is an enumerate to define the different type of documentation
      conflict. **)
  TDocConflictType = (
    dctModuleMissingDocumentation,
    dctModuleMissingDate,
    dctModuleIncorrectDate,
    dctModuleCheckDateError,
    dctModuleMissingVersion,
    dctModuleMissingAuthor,
    dctTypeClauseUndocumented,
    dctConstantClauseUndocumented,
    dctResourceStringClauseUndocumented,
    dctVariableClauseUndocumented,
    dctThreadVarClauseUndocumented,
    dctRecordClauseUndocumented,
    dctObjectClauseUndocumented,
    dctClassClauseUndocumented,
    dctInterfaceClauseUndocumented,
    dctDispinterfaceClauseUndocumented,
    dctMethodUndocumented,
    dctMethodHasNoDesc,
    dctMethodDiffParamCount,
    dctMethodUndocumentedParam,
    dctMethodIncorrectParamType,
    dctMethodUndocumentedReturn,
    dctMethodIncorrectReturnType,
    dctMethodPreconNotDocumented,
    dctMethodMissingPrecon,
    dctMethodTooManyPrecons,
    dctMethodPostconNotDocumented,
    dctMethodMissingPostcon,
    dctMethodTooManyPostcons,
    dctPropertyUndocumented,
    dctPropertyHasNoDesc,
    dctPropertyDiffParamCount,
    dctPropertyUndocumentedParam,
    dctPropertyIncorrectParamType,
    dctPropertyUndocumentedReturn,
    dctPropertyIncorrectReturnType,
    dctPropertyPreconNotDocumented,
    dctPropertyMissingPrecon,
    dctPropertyTooManyPrecons,
    dctPropertyPostconNotDocumented,
    dctPropertyMissingPostcon,
    dctPropertyTooManyPostcons
  );

  (** A record to describe the information to be associated with a
      DocConflictType **)
  TDocConflictTypeRec = Record
    Category : String;
    MessageMask : String;
    Description : String;
  End;

  (** This is a class to represent a module documentation conflict. **)
  TDocumentConflict = Class
  Private
    FMessage : String;
    FIdentLine : Integer;
    FIdentColumn : Integer;
    FCommentLine : Integer;
    FCommentColumn : Integer;
    FDocConflictType: TDocConflictType;
  Protected
    Function GetCategory: String;
    Function GetDescription: String;
  Public
    Constructor Create(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      DocConflictType : TDocConflictType);
    Property Category : String Read GetCategory;
    Property Message : String Read FMessage;
    Property Description : String Read GetDescription;
    Property IdentLine : Integer Read FIdentLine;
    Property IdentColumn : Integer Read FIdentColumn;
    Property CommentLine : Integer Read FCommentLine;
    Property CommentColumn : Integer Read FCommentColumn;
    Property DocConflictType : TDocConflictType Read FDocConflictType;
  End;

  (** This is a record to describe the position of a token in the editor. **)
  TTokenPosition = Record
    Line : Integer;
    Column : Integer;
    BufferPos : Integer;
  End;

  (** This is an enumerate to define the options for the parsing of a module. **)
  TModuleOption = (moParse, moCheckForDocumentConflicts);
  (** This is a set of Module Option enumerates. **)
  TModuleOptions = Set Of TModuleOption;

  (** This is an abtract class from which all language modules should be
      derived. **)
  TBaseLanguageModule = Class Abstract
  Private
    FOwnedItems : TObjectList;
    FTokens : TObjectList;
    FTokenIndex : TTokenIndex;
    FSourceStream : TStream;
    FDocErrors: TDocErrorCollection;
    FTickList : TStringList;
    FModuleName : String;
    FModuleType : TModuleType;
    FModuleComment : TComment;
    FExportedHeadings : TMethodCollection;
    FImplementedMethods : TMethodCollection;
    FConstantsCollection : TGenericContainerCollection;
    FVarsCollection : TGenericContainerCollection;
    FThreadVarsCollection : TGenericContainerCollection;
    FTypeCollection : TGenericContainerCollection;
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
    FSymbolTable: TGenericContainerCollection;
    FFileName: String;
    FModified : Boolean;
    FDocumentConflicts: TObjectList;
    Function GetTokenCount : Integer;
    Function GetTokenInfo(iIndex : TTokenIndex) : TTokenInfo;
    Function GetToken : TTokenInfo;
    function GetOpTickCountName(iIndex: Integer): String;
    function GetOpTickCountByIndex(iIndex: Integer): Integer;
    function GetOpTickCounts: Integer;
    function GetOpTickCount(strStart, strFinish : String): Integer;
    Function GetBodyComment(iIndex : Integer) : TComment;
    Function GetBodyCommentCount : Integer;
    Function GetDocumentConflict(iIndex : Integer) : TDocumentConflict;
    Function GetDocumentConflictCount : Integer;
    Procedure TokenizeStream(strReservedWords, strDirectives : Array of String);
  Protected
    Function PrevToken : TTokenInfo;
    Procedure NextToken;
    Function EndOfTokens : Boolean;
    Procedure NextNonCommentToken;
    Procedure RollBackToken;
    Function GetComment : TComment;
    Procedure SetTokenIndex(iIndex : TTokenIndex);
    Procedure SortDocumentConflicts;
    Procedure GetBodyCmt;
    (**
      Returns a refernce the to owned items collection. This is used to manage
      the life time of all the ident lists and comments found in the module.
      @return  a TObjectList
    **)
    Property OwnedItems : TObjectList Read FOwnedItems;
    (**
      Returns the current token with in the module. Also see
      {@link TPascalDocModule.SetTokenIndex SetTokenIndex}.
      @return  a TTokenInfo
    **)
    Property Token : TTokenInfo Read GetToken;
    (**
      Returns a reference to the body comments collection.
      @return  a TObjectList
    **)
    Property BodyComments : TObjectList Read FBodyComment;
  Public
    Constructor Create(Source : TStream; strReservedWords,
      strDirectives : Array of String; IsModified : Boolean; strFileName : String);
    Destructor Destroy; Override;
    Procedure AddTickCount(strLabel : String);
    Procedure AddDocumentConflict(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol  : Integer;
      DocConflictType : TDocConflictType);
    Function ConvertDate(Const strDate : String) : TDateTime;
    { Properties }
    (**
      Returns a reference to the modules error collection.
      @return  a TDocErrorCollection
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
      Returns the number of token within the module after tokenizing.
      @return  an Integer
    **)
    Property Errors : TDocErrorCollection Read FDocErrors;
    Property OpTickCount[strStart, strFinish : String] : Integer Read GetOpTickCount;
    Property OpTickCounts : Integer Read GetOpTickCounts;
    Property OpTickCountByIndex[iIndex : Integer] : Integer Read GetOpTickCountByIndex;
    Property OpTickCountName[iIndex : Integer] : String Read GetOpTickCountName;
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
    Property ModuleComment : TComment Read FModuleComment Write FModuleComment;
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
      Returns a reference to the variables clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Vars : TGenericContainerCollection Read FVarsCollection;
    (**
      Returns a reference to the modules types clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Types : TGenericContainerCollection Read FTypeCollection;
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
      Returns a reference to the resource string clause collection.
      @return  a TGenericContainerCollection
    **)
    Property ResourceStrings : TGenericContainerCollection Read FResStrCollection;
    (**
      Returns a reference to the ThreadVar clause collection.
      @return  a TGenericContainerCollection
    **)
    Property ThreadVars : TGenericContainerCollection Read FThreadVarsCollection;
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
  End;

ResourceString
  (** The registry key for the wizards settings. **)
  strRegRootKey = 'Software\Season''s Fall\Pascal Doc\';

  (** Label for Documentation Conflicts **)
  strDocumentationConflicts = 'Documentation Conflicts';

  (** This is the tree branch under which module documentation error appear **)
  strModuleDocumentation = 'Module Documentation';
  (** This is a documentation error for a missing module description **)
  strModuleMissingDocumentation = 'This module has no document comment.';
  (** This is a documentation error for a missing module description **)
  strModuleMissingDocumentationDesc = 'Each module should have a comment ' +
    'before the PROGRAM, UNIT, PACKAGE or LIBARY key work describing the ' +
    'contents of the module.';
  (** This is a documentation error for a missing documentation date **)
  strModuleMissingDate = 'This module is missing a documentation date.';
  (** This is a documentation error for an incorrect documenation date **)
  strModuleIncorrectDate = 'The module documentation date ''%s'' is incorrect (''%s'').';
  (** This is a documentation error for an invalid documenation date **)
  strModuleCheckDateError = 'The module documentation date ''%s'' is not valid (''%s'').';
  (** This is a documentation error for a missing documentation version **)
  strModuleMissingVersion = 'This module is missing a documentation version.';
  (** This is a documentation error for a missing documentation author **)
  strModuleMissingAuthor = 'This module is missing a documentation author.';

  (** This is the tree branch under which type documentation error appear **)
  strTypeDocumentation = 'Type Documentation';
  (** Document conflict message for an undocumented type clause item. **)
  strTypeClauseUndocumented = 'Type ''%s'' is undocumented.';

  (** This is the tree branch under which constant documentation error appear **)
  strConstantDocumentation = 'Constant Documentation';
  (** Document conflict message for an undocumented constant clause item. **)
  strConstantClauseUndocumented = 'Constant ''%s'' is undocumented.';

  (** This is the tree branch under which resource string documentation error appear **)
  strResourceStringDocumentation = 'Resource String Documentation';
  (** Document conflict message for an undocumented resource string clause item. **)
  strResourceStringClauseUndocumented = 'Resource string ''%s'' is undocumented.';

  (** This is the tree branch under which variable documentation error appear **)
  strVariableDocumentation = 'Variable Documentation';
  (** Document conflict message for an undocumented variable clause item. **)
  strVariableClauseUndocumented = 'Variable ''%s'' is undocumented.';

  (** This is the tree branch under which thread variable documentation error appear **)
  strThreadVarDocumentation = 'Thread Variable Documentation';
  (** Document conflict message for an undocumented thread variable clause item. **)
  strThreadVarClauseUndocumented = 'Thread variable ''%s'' is undocumented.';

  (** This is the tree branch under which record documentation error appear **)
  strRecordDocumentation = 'Record Documentation';
  (** Document conflict message for an undocumented record clause item. **)
  strRecordClauseUndocumented = 'Record variable ''%s'' is undocumented.';

  (** This is the tree branch under which object documentation error appear **)
  strObjectDocumentation = 'Object Documentation';
  (** Document conflict message for an undocumented object clause item. **)
  strObjectClauseUndocumented = 'Object variable ''%s'' is undocumented.';

  (** This is the tree branch under which class documentation error appear **)
  strClassDocumentation = 'Class Documentation';
  (** Document conflict message for an undocumented class variable clause item. **)
  strClassClauseUndocumented = 'Class variable ''%s'' is undocumented.';

  (** This is the tree branch under which interface documentation error appear **)
  strInterfaceDocumentation = 'Interface Documentation';
  (** Document conflict message for an undocumented interface variable clause item. **)
  strInterfaceClauseUndocumented = 'Interface variable ''%s'' is undocumented.';

  (** This is the tree branch under which dispinterface documentation error appear **)
  strDispInterfaceDocumentation = 'DispInterface Documentation';
  (** Document conflict message for an undocumented dispinterface variable clause item. **)
  strDispInterfaceClauseUndocumented = 'DispInterface variable ''%s'' is undocumented.';

  (** Label for Method Documentation Conflicts **)
  strMethodDocumentation = 'Method Documentation';
  (** Document conflict message for missing method documentation. **)
  strMethodUndocumented = 'Method ''%s'' has not been documented.';
  (** Document conflict message for missing method description. **)
  strMethodHasNoDesc = 'Method ''%s'' has no description.';

  (** Label for Method Parameter Documentation Conflicts **)
  strMethodParamDocumentation = 'Method Parameter Documentation';
  (** Document conflict message for different number of parameters and tags. **)
  strMethodDiffParamCount = 'Method ''%s'' has a different parameter count.';
  (** Document conflict message for an undocumented parameter. **)
  strMethodUndocumentedParam = 'Parameter ''%s'' in method ''%s'' is not documented.';
  (** Document conflict message for an incorrect parameter type. **)
  strMethodIncorrectParamType = 'The parameter type for ''%s'' in method ''%s'' is incorrect.';

  (** Label for Method Return Documentation Conflicts **)
  strMethodReturnDocumentation = 'Method Return Documentation';
  (** Document conflict message for an undocumented return type. **)
  strMethodUndocumentedReturn = 'Method ''%s''`s return type is not documented.';
  (** Document conflict message for an incorrect return type. **)
  strMethodIncorrectReturnType = 'Method ''%s''`s return type is incorrect.';

  (** Label for Method Pre-Condition Documentation Conflicts **)
  strMethodPreConDocumentation = 'Method Pre-Condition Documentation';
  (** A documentation message for missing precondition text. **)
  strMethodPreConNotDocumented = 'A Pre-condition in Method ''%s'' is not documented.';
  (** Document conflict message for a missing pre-condition tag. **)
  strMethodMissingPreCon = 'Method ''%s'' has missing pre-condition tags.';
  (** Document conflict message for too many pre-condition tag. **)
  strMethodTooManyPreCons = 'Method ''%s'' has too many pre-condition tags.';

  (** Label for Method Post-Condition Documentation Conflicts **)
  strMethodPostConDocumentation = 'Method Post-Condition Documentation';
  (** A documentation message for missing postcondition text. **)
  strMethodPostConNotDocumented = 'A Post-condition in Method ''%s'' is not documented.';
  (** Document conflict message for a missing post-condition tag. **)
  strMethodMissingPostCon = 'Method ''%s'' has a missing post-condition tag.';
  (** Document conflict message for too many post-condition tag. **)
  strMethodTooManyPostCons = 'Method ''%s'' has too many post-condition tags.';

  (** Label for Property Documentation Conflicts **)
  strPropertyDocumentation = 'Property Documentation';
  (** Document conflict message for missing method documentation. **)
  strPropertyUndocumented = 'Property ''%s'' has not been documented.';
  (** Document conflict message for missing property description. **)
  strPropertyHasNoDesc = 'Property ''%s'' has no description.';

  (** Label for Property Parameter Documentation Conflicts **)
  strPropertyParamDocumentation = 'Property Parameter Documentation';
  (** Document conflict message for different number of parameters and tags. **)
  strPropertyDiffParamCount = 'Property ''%s'' has a different parameter count.';
  (** Document conflict message for an undocumented parameter. **)
  strPropertyUndocumentedParam = 'Parameter ''%s'' in property ''%s'' is not documented.';
  (** Document conflict message for an incorrect parameter type. **)
  strPropertyIncorrectParamType = 'The parameter type for ''%s'' in property ''%s'' is incorrect.';

  (** Label for Property Return Documentation Conflicts **)
  strPropertyReturnDocumentation = 'Property Return Documentation';
  (** Document conflict message for an undocumented return type. **)
  strPropertyUndocumentedReturn = 'Property ''%s''`s return type is not documented.';
  (** Document conflict message for an incorrect return type. **)
  strPropertyIncorrectReturnType = 'Property ''%s''`s return type is incorrect.';

  (** Label for Property Pre-Condition Documentation Conflicts **)
  strPropertyPreConDocumentation = 'Property Pre-Condition Documentation';
  (** A documentation message for missing precondition text. **)
  strPropertyPreConNotDocumented = 'A Pre-condition in Property ''%s'' is not documented.';
  (** Document conflict message for a missing pre-condition tag. **)
  strPropertyMissingPreCon = 'Property ''%s'' has missing pre-condition tags.';
  (** Document conflict message for too many pre-condition tag. **)
  strPropertyTooManyPreCons = 'Property ''%s'' has too many pre-condition tags.';

  (** Label for Property Post-Condition Documentation Conflicts **)
  strPropertyPostConDocumentation = 'Property Post-Condition Documentation';
  (** A documentation message for missing postcondition text. **)
  strPropertyPostConNotDocumented = 'A Post-condition in Property ''%s'' is not documented.';
  (** Document conflict message for a missing post-condition tag. **)
  strPropertyMissingPostCon = 'Property ''%s'' has a missing post-condition tag.';
  (** Document conflict message for too many post-condition tag. **)
  strPropertyTooManyPostCons = 'Property ''%s'' has too many post-condition tags.';

  (** Errors and warnings label **)
  strErrorsAndWarnings = 'Errors and Warnings';
  (** Label for Uses Clause **)
  strUses = 'Uses';
  (** Label for Types Clause **)
  strTypesLabel = 'Types';
  (** Label for Class Clause **)
  strClass = 'Class';
  (** Label for Classes Clause **)
  strClasses = 'Classes';
  (** Label for Interfaces Clause **)
  strInterfaces = 'Interfaces';
  (** Label for Interface Clause **)
  strInterface = 'Interface';
  (** Label for DispInterface Clause **)
  strDispInterface = 'DispInterface';
  (** Label for DispInterfaces Clause **)
  strDispInterfaces = 'DispInterfaces';
  (** Label for Object Clause **)
  strObject = 'Object';
  (** Label for Objects Clause **)
  strObjects = 'Objects';
  (** Label for Record Clause **)
  strRecord = 'Record';
  (** Label for Records Clause **)
  strRecords = 'Records';
  (** Label for Constants Clause **)
  strConstants = 'Constants';
  (** Label for Resource Strings Clause **)
  strResourceStrings = 'Resource Strings';
  (** Label for Variables Clause **)
  strVars = 'Variables';
  (** Label for Thread Variables Clause **)
  strThreadVars = 'Thread Variables';
  (** Label for Exported Headings **)
  strExportedHeadings = 'Exported Headings';
  (** Label for Exports Clause **)
  strExports = 'Exports';
  (** Label for Implemented Methods **)
  strImplementedMethods = 'Implemented Methods';
  (** Label for Requires Clause **)
  strRequires = 'Requires';
  (** Label for Contains Clause **)
  strContains = 'Contains';
  (** Label for Modules Section **)
  strModules = 'Modules';
  (** Label for Initialization Clause **)
  strInitialization = 'Initialization';
  (** Label for Finalization Clause **)
  strFinalization = 'Finalization';
  (** Label for Functions and Procedures label **)
  strFunctionsAndProcedures = 'Function & Procedures';
  (** Resource string for saving a file. **)

  strSaveFile = 'Do you want to save the file "%s"?';
  (** Resource string for overwriting a file. **)
  strOverwriteFile = 'Do you want to overwrite the file "%s"?';
  (** Resource string for a class not found. **)
  strUnExpectedStartOfFile = 'Unexpected start-of-file.';
  (** Exception message for an unexpected end of file. **)
  strUnExpectedEndOfFile = 'Unexpected end-of-file.';
  (** Exception message when an identifier is expected but something else is found. **)
  strIdentExpected = 'Identifier expected but "%s" found at line %d column %d.';
  (** Exception message when an string is expected but something else is found. **)
  strStringExpected = 'String literal expected but "%s" found at line %d column %d.';
  (** Exception message when an reserved word is expected but something else is
      found. **)
  strReservedWordExpected = 'Expected "%s" but "%s" found at line %d column %d.';
  (** Exception message when an literal character is expected but something else
      is found. **)
  strLiteralExpected = '"%s" expected but "%s" found at line %d column %d.';
  (** Warning for a function not having a return parameter. **)
  strFunctionWarning = 'Function "%s" does not have a return type specified.';
  (** An exception message for a non defined help file option. **)
  strHelpFileNotDefined = 'There is no help file specified. Please specified a ' +
    'help file in the options dialogue.';
  (** An exception message for a missing help file **)
  strHelpFileNotFound = 'The help file "%s" was not found.';

Const
  (** A set of characters for whitespace **)
  strWhiteSpace : Set Of Char = [#32, #9];
  (** A set of characters for line feed and carriage return **)
  strLineEnd : Set of Char = [#10, #13];
  (** A set of characters for alpha characaters **)
  strTokenChars : Set Of Char = ['#', '_', 'a'..'z', 'A'..'Z'];
  (** A set of numbers **)
  strNumbers : Set Of Char = ['$', '0'..'9'];
  (** A set of characters for general symbols **)
  strSymbols : Set Of Char = ['&', '(', ')', '*', '+',
    ',', '-', '.', '/', ':', ';', '<', '=', '>', '@', '[', ']', '^', '{', '}'];
  (** A set of characters for quotes **)
  strQuote : Set Of Char = [''''];
  (** A set of characters that terminate a constant expression. **)
  strConstExprTerminals : Set Of Char = ['&', ',', ':', ';', '<', '=', '>',
    ']', '^', '{', '}'];
  (** A list of strings representing the different types of token. **)
  strTypes : Array[ttUnknown..ttLinkTag] Of String = ('Unknown',
    'WhiteSpace', 'Keyword', 'Identifier', 'Number', 'Symbol', 'LineEnd',
    'ArrayElement', 'StatementEnd', 'StringLiteral', 'Comment', 'HTMLTag',
    'Directive', 'CompilerDirective', 'LinkTag');
  (** A list of strings representing the scope types. **)
  strScope : Array[scGlobal..scPublished] Of String = ('global', 'local',
    'private', 'protected', 'public', 'published');
  (** A list of strings representing the types of methods. **)
  strMethodTypes : Array[mtConstructor..mtFunction] Of String = (
    'Constructor', 'Destructor', 'Procedure', 'Function');
  (** A list of strings representing the parameter modifiers for methods. **)
  strParamModifier : Array[pmNone..pmOut] Of String = ('', 'var ', 'const ',
    'out ');

  (** This is a string array representing the TDocOption enumerates.
      @todo Moves the descriptions to resource strings and add some context. **)
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
    (Description : 'Manage Expanded Nodes'; Enabled : True),
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
    (Description : 'Show Missing Property Documentation'; Enabled : False),
    (Description : 'Show Missing Property Documentation Description'; Enabled : False),
    (Description : 'Show Different Property Parameter Count'; Enabled : False),
    (Description : 'Show Undocumented Property Parameter'; Enabled : False),
    (Description : 'Show Incorrect Property Parameter Type'; Enabled : False),
    (Description : 'Show Undocumented Property Return Type'; Enabled : False),
    (Description : 'Show Incorrect Property Return Type'; Enabled : False),
    (Description : 'Show Missing Property Pre-Conditions'; Enabled : False),
    (Description : 'Show Missing Property Post-Conditions'; Enabled : False),
    (Description : 'Categories Documentation Conflicts'; Enabled : False)
  );

  (** This is a string array representing the TDocOption enumerates.
      @todo This needs to be expanded with descriptions for all the documentation conflicts.
  **)
  DocConflictInfo : Array[Low(TDocConflictType)..High(TDocConflictType)] Of TDocConflictTypeRec = (
    (Category: strModuleDocumentation;
      MessageMask: strModuleMissingDocumentation;
      Description: strModuleMissingDocumentationDesc),
    (Category: strModuleDocumentation;
      MessageMask: strModuleMissingDate; Description: ''),
    (Category: strModuleDocumentation;
      MessageMask: strModuleIncorrectDate; Description: ''),
    (Category: strModuleDocumentation;
      MessageMask: strModuleCheckDateError; Description: ''),
    (Category: strModuleDocumentation;
      MessageMask: strModuleMissingVersion; Description: ''),
    (Category: strModuleDocumentation;
      MessageMask: strModuleMissingAuthor; Description: ''),
    (Category: strTypedocumentation;
      MessageMask: strTypeClauseUndocumented; Description: ''),
    (Category: strConstantdocumentation;
      MessageMask: strConstantClauseUndocumented; Description: ''),
    (Category: strResourceStringdocumentation;
      MessageMask: strResourceStringClauseUndocumented; Description: ''),
    (Category: strVariabledocumentation;
      MessageMask: strVariableClauseUndocumented; Description: ''),
    (Category: strThreadVardocumentation;
      MessageMask: strThreadVarClauseUndocumented; Description: ''),
    (Category: strRecorddocumentation;
      MessageMask: strRecordClauseUndocumented; Description: ''),
    (Category: strObjectdocumentation;
      MessageMask: strObjectClauseUndocumented; Description: ''),
    (Category: strClassdocumentation;
      MessageMask: strClassClauseUndocumented; Description: ''),
    (Category: strInterfacedocumentation;
      MessageMask: strInterfaceClauseUndocumented; Description: ''),
    (Category: strDispinterfacedocumentation;
      MessageMask: strDispinterfaceClauseUndocumented; Description: ''),
    (Category: strMethodDocumentation;
      MessageMask: strMethodUndocumented; Description: ''),
    (Category: strMethodDocumentation;
      MessageMask: strMethodHasNoDesc; Description: ''),
    (Category: strMethodParamDocumentation;
      MessageMask: strMethodDiffParamCount; Description: ''),
    (Category: strMethodParamDocumentation;
      MessageMask: strMethodUndocumentedParam; Description: ''),
    (Category: strMethodParamDocumentation;
      MessageMask: strMethodIncorrectParamType; Description: ''),
    (Category: strMethodReturnDocumentation;
      MessageMask: strMethodUndocumentedReturn; Description: ''),
    (Category: strMethodReturnDocumentation;
      MessageMask: strMethodIncorrectReturnType; Description: ''),
    (Category: strMethodPreconDocumentation;
      MessageMask: strMethodPreconNotDocumented; Description: ''),
    (Category: strMethodPreconDocumentation;
      MessageMask: strMethodMissingPrecon; Description: ''),
    (Category: strMethodPreconDocumentation;
      MessageMask: strMethodTooManyPrecons; Description: ''),
    (Category: strMethodPostconDocumentation;
      MessageMask: strMethodPostconNotDocumented; Description: ''),
    (Category: strMethodPostconDocumentation;
      MessageMask: strMethodMissingPostcon; Description: ''),
    (Category: strMethodPostconDocumentation;
      MessageMask: strMethodTooManyPostcons; Description: ''),
    (Category: strPropertyDocumentation;
      MessageMask: strPropertyUndocumented; Description: ''),
    (Category: strPropertyDocumentation;
      MessageMask: strPropertyHasNoDesc; Description: ''),
    (Category: strPropertyParamDocumentation;
      MessageMask: strPropertyDiffParamCount; Description: ''),
    (Category: strPropertyParamDocumentation;
      MessageMask: strPropertyUndocumentedParam; Description: ''),
    (Category: strPropertyParamDocumentation;
      MessageMask: strPropertyIncorrectParamType; Description: ''),
    (Category: strPropertyReturnDocumentation;
      MessageMask: strPropertyUndocumentedReturn; Description: ''),
    (Category: strPropertyReturnDocumentation;
      MessageMask: strPropertyIncorrectReturnType; Description: ''),
    (Category: strPropertyPreconDocumentation;
      MessageMask: strPropertyPreconNotDocumented; Description: ''),
    (Category: strPropertyPreconDocumentation;
      MessageMask: strPropertyMissingPrecon; Description: ''),
    (Category: strPropertyPreconDocumentation;
      MessageMask: strPropertyTooManyPrecons; Description: ''),
    (Category: strPropertyPostconDocumentation;
      MessageMask: strPropertyPostconNotDocumented; Description: ''),
    (Category: strPropertyPostconDocumentation;
      MessageMask: strPropertyMissingPostcon; Description: ''),
    (Category: strPropertyPostconDocumentation;
      MessageMask: strPropertyTooManyPostcons; Description: '' )
  );

  (** A list of string representing the types of modules. **)
  strModuleTypes : Array[mtProgram..mtUnit] Of String = ('Program', 'Package',
    'Library', 'Unit');
  (** This is a constant for special tag items to show in the tree **)
  iShowInTree = $0001;
  (** This is a constant for special tag items to auto expand in the tree **)
  iAutoExpand = $0002;

Var
  (** This is a global string list containing the special tags list. **)
  SpecialTags : TStringList;

  Function GetTokenType(Ch : Char; LastToken : TTokenType) : TTokenType;
  function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Function IsTokenWhiteSpace(strToken : String) : Boolean;

Implementation

Uses
  Windows;

(**

  This function returns the token type for a given character and last token
  type.

  @precon  Ch is the character for which the token type assessment needs to be
           taken for and LastToken os the type of the last token as this has an
           effect on some characters.
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

  This function returns true if the given word is in the supplied word list. It
  uses a binary search, so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and
           strWordList is a static array of words in lowercase and alphabetical
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

  This function converts a freeform text string representing dates and times
  in standard formats in to a TDateTime value.

  @precon  strDate is the string to convert into a date.
  @postcon Returns a valid TDateTime value.

  @param   strDate as a String
  @return  a TDateTime

**)
Function TBaseLanguageModule.ConvertDate(Const strDate : String) : TDateTime;

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

    @precon  StringList is the string list to add the token too and strToken is
             the token to add to the list.
    @postcon Adds the token to the specified string list and clears the
             token.

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

    @precon  iIndex is the index of the item from the string list to extract,
             iValue is a word variable to place the converted item into and
             Delete determines whether the item is removed from the string list.
    @postcon Tries to extract the value from the indexed string list
             item into the passed variable reference.

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

    @precon  None.
    @postcon Assigns string list indexes to the three index values according to
             the short date format and what information is supplied.

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

  This is a TObjectList custom sort procedure for the TIdentList class.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortIdentList(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(TIdent(Item1).Ident, TIdent(Item2).Ident);
End;

(**

  This is a TObjectList custom sort procedure for the
  TGenericContainerCollection class.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortGenericContainerCollection(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(TGenericContainer(Item1).Identifier,
    TGenericContainer(Item2).Identifier);
End;

(**

  This is a TObjectList custom sort procedure for the TMethodCollection class.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortMethodCollection(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(TMethodDecl(Item1).QualifiedName,
    TMethodDecl(Item2).QualifiedName);
End;

(**

  This is a TObjectList custom sort procedure for the TRecordDecl class.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortRecordDecl(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(TParameter(Item1).Identifier,
    TParameter(Item2).Identifier);
End;

(**

  This is a TObjectList custom sort procedure for the TMethodDecl class.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortMethodDecl(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(TMethodDecl(Item1).Identifier,
    TMethodDecl(Item2).Identifier);
End;

(**

  This is a TObjectList custom sort procedure for the TClassDecl class.

  @precon  None.
  @postcon Compares the 2 items and returns the comparison, If item12 < item2
           then -1, if the same then 0, and if item1 > item 2 then +1.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortClassDecl(Item1, Item2 : Pointer) : Integer;

Begin
  Result := AnsiCompareText(TProperty(Item1).Identifier,
    TProperty(Item2).Identifier);
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
procedure TBaseLanguageModule.AddDocumentConflict(Const Args: Array of TVarRec;
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
procedure TBaseLanguageModule.AddTickCount(strLabel: String);
begin
  FTickList.AddObject(strLabel, TObject(GetTickCount));
end;

(**

  This is the constructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Initialise this base class and Tokensizes the passed stream of
           characters.

  @param   Source           as a TStream
  @param   strReservedWords as an Array Of String
  @param   strDirectives    as an Array Of String
  @param   IsModified       as a Boolean
  @param   strFileName      as a String

**)
constructor TBaseLanguageModule.Create(Source : TStream; strReservedWords,
  strDirectives : Array of String; IsModified : Boolean; strFileName : String);
begin
  Inherited Create;
  FFileName := strFileName;
  FModified := IsModified;
  FOwnedItems := TObjectList.Create(True);
  FTokens := TObjectList.Create(True);
  FTokenIndex := 0;
  FSourceStream := Source;
  FDocErrors := TDocErrorCollection.Create;
  FTickList := TStringList.Create;
  FExportedHeadings := TMethodCollection.Create;
  FImplementedMethods := TMethodCollection.Create;
  FConstantsCollection := TGenericContainerCollection.Create(True);
  FResStrCollection := TGenericContainerCollection.Create(True);
  FVarsCollection := TGenericContainerCollection.Create(True);
  FThreadVarsCollection := TGenericContainerCollection.Create(True);
  FTypeCollection := TGenericContainerCollection.Create(False);
  FExportsCollection := TGenericContainerCollection.Create(True);
  FBodyComment := TObjectList.Create(True);
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
  AddTickCount('Start');
  TokenizeStream(strReservedWords, strDirectives);
  AddTickCount('Tokenize');
end;

(**

  This is the destructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Frees the memory used by all the collections.

**)
destructor TBaseLanguageModule.Destroy;
begin
  FDocumentConflicts.Free;
  FSymbolTable.Free;
  FBodyComment.Free;
  FExportsCollection.Free;
  FTypeCollection.Free;
  FThreadVarsCollection.Free;
  FVarsCollection.Free;
  FResStrCollection.Free;
  FConstantsCollection.Free;
  FImplementedMethods.Free;
  FExportedHeadings.Free;
  FTickList.Free;
  FDocErrors.Free;
  FTokens.Free;
  FOwnedItems.Free;
  inherited;
end;

(**

  This method tries to get a document comment from the previous token and return
  a TComment class to the calling routine.

  @note    All comments found are automatically added to the comment collection
           for disposal when the parser is destroyed.

  @precon  None.
  @postcon Returns the comment immediately before the current token else nil.

  @return  a TComment

**)
Function TBaseLanguageModule.GetComment : TComment;

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

  This is a setter method for the TokenIndex property.

  @precon  iIndex is the token index to set the parse to start at.
  @postcon Sets the TokenIndex position.

  @param   iIndex as a TTokenIndex

**)
Procedure TBaseLanguageModule.SetTokenIndex(iIndex : TTokenIndex);

Begin
  FTokenIndex := iIndex;
End;

(**

  This method is a sort procedure for the Documentation Conflicts
  collection.

  @precon  None.
  @postcon Sorts the documentation conflicts first by Category and then by
           message.

**)
procedure TBaseLanguageModule.SortDocumentConflicts;
begin
  FDocumentConflicts.Sort(CompareDocConflicts);
end;

(**

  This method tries to get a body comment from the previous token in the token
  list and add it to the body comment list.

  @precon  None.
  @postcon Tries to get a body comment from the previous token in the token
           list and add it to the body comment list.

**)
Procedure TBaseLanguageModule.GetBodyCmt;

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

  This is a getter method for the BodyComment property.

  @precon  iIndex is the index of the body comment required.
  @postcon Return the requested comment object.

  @param   iIndex as an Integer
  @return  a TComment

**)
Function TBaseLanguageModule.GetBodyComment(iIndex : Integer) : TComment;

Begin
  Result := FBodyComment[iIndex] As TComment;
End;

(**

  This is a getter method for the BodyCommentCount property.

  @precon  None.
  @postcon Returns the number of body comment in the collection.

  @return  an Integer

**)
Function TBaseLanguageModule.GetBodyCommentCount : Integer;

Begin
  Result := FBodyComment.Count;
End;

(**

  This is a getter method for the DocumentConflict property.

  @precon  iIndex must be a valid integer index.
  @postcon Returns the documentation conflict references by the passed index.

  @param   iIndex as an Integer
  @return  a TDocumentConflict

**)
function TBaseLanguageModule.GetDocumentConflict(
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
function TBaseLanguageModule.GetDocumentConflictCount: Integer;
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
function TBaseLanguageModule.GetOpTickCount(strStart, strFinish : String): Integer;

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
function TBaseLanguageModule.GetOpTickCountByIndex(iIndex: Integer): Integer;
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
function TBaseLanguageModule.GetOpTickCountName(iIndex: Integer): String;
begin
  Result := FTickList[iIndex];
end;

(**

  This is a getter method for the OpTickCounts property.

  @precon  None.
  @postcon Returns the number of items in the OpTickCount collection.

  @return  an Integer

**)
function TBaseLanguageModule.GetOpTickCounts: Integer;
begin
  Result := FTickList.Count;
end;

(**

  This is a getter method for the TokenCount property.

  @precon  None.
  @postcon Returns the number of tokens in the collection.

  @return  an Integer

**)
Function TBaseLanguageModule.GetTokenCount : Integer;

Begin
  Result := FTokens.Count;
End;
(**

  This is a getter method for the TokenInfo property.

  @precon  iIndex is the index of the token info object required.
  @postcon Returns the token info object requested.

  @param   iIndex as a TTokenIndex
  @return  a TTokenInfo

**)
function TBaseLanguageModule.GetTokenInfo(iIndex: TTokenIndex): TTokenInfo;

begin
  Result := FTokens[iIndex] As TTokenInfo;
end;

(**

  This is a getter method for the Token property.

  @precon  None.
  @postcon Returns a token info object for the current token.

  @return  a TTokenInfo

**)
Function TBaseLanguageModule.GetToken : TTokenInfo;

Begin
  If FTokenIndex >= FTokens.Count Then
    Raise EDocException.Create(strUnExpectedEndOfFile);
  Result := FTokens[FTokenIndex] As TTokenInfo;
End;

(**

  This method checks to see if the passed token if white space, if so returns
  true.

  @precon  None.
  @postcon Checks to see if the passed token if white space, if so returns
           true.

  @param   strToken as a String
  @return  a Boolean

**)
Function IsTokenWhiteSpace(strToken : String) : Boolean;

Var
  i : Integer;

Begin
  Result := True;
  For i := 1 To Length(strToken) Do
    If Not (strToken[i] In strWhiteSpace) And
      Not (strToken[i] In strLineEnd)Then
      Result := False;
End;

(**

  This method moves the toke to the next token in the token list or raises an
  EDocException.

  @precon  None.
  @postcon Moves the token to the next token in the token list or raises an
           EDocException.

**)
Procedure TBaseLanguageModule.NextToken;

begin
  Inc(FTokenIndex);
end;

(**

  This method checks for the end of the token list and returns true if it is
  found.

  @precon  None.
  @postcon Returns true is we are beyond the end of the token collection.

  @return  a Boolean

**)
Function TBaseLanguageModule.EndOfTokens : Boolean;

Begin
  Result := FTokenIndex >= FTokens.Count;
End;

(**

  This method move the token position to the next non comment token.

  @precon  None.
  @postcon Move the token position to the next non comment token.

**)
procedure TBaseLanguageModule.NextNonCommentToken;
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

  @precon  None.
  @postcon Rolls back to the previous token in the token list skipping
           comment tokens.

**)
Procedure TBaseLanguageModule.RollBackToken;

Begin
  Dec(FTokenIndex);
  While (FTokenIndex > 0) And (TokenInfo[FTokenIndex].TokenType In [ttComment,
    ttCompilerDirective]) Do
    Dec(FTokenIndex);
  If FTokenIndex < 0 Then
    Raise EDocException.Create(strUnExpectedStartOfFile);
End;

(**

  This method returns the previous token in the token list, else returns nil.

  @precon  None.
  @postcon Returns a token info object for the previous non comment token.

  @return  a TTokenInfo

**)
Function TBaseLanguageModule.PrevToken : TTokenInfo;

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

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

  @param   strReservedWords as an Array of String
  @param   strDirectives    as an Array of String

**)
Procedure TBaseLanguageModule.TokenizeStream(strReservedWords,
  strDirectives : Array of String);

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
                      If Not IsTokenWhiteSpace(strToken) Then
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

  This method added the strToken to the tags token list with type iType.

  @precon  strToken is a string to be added as a token and iType is the token
           type of the token.
  @postcon Adds the token to the internal list.

  @param   strToken as a String
  @param   iType    as a TTokenType

**)
procedure TTag.AddToken(strToken: String; iType: TTokenType);
begin
  FTokens.AddObject(strToken, TObject(iType));
end;

(**

  This is the TTag class's constructor method. It creates the token list.

  @precon  strName is the name of the new tag to be created, iLine is the line
           number of the tag and iColumn is the column position of the tag.
  @postcon Initialises the comment tag class.

  @param   strName as a String
  @param   iLine   as an Integer
  @param   iColumn as an Integer

**)
constructor TTag.Create(strName: String; iLine, iColumn : Integer);
begin
  Inherited Create;
  FTagName := strName;
  FTokens := TStringList.Create;
  FLine := iLine;
  FColumn := iColumn;
end;

(**

  This is the TTag class Destructor method. It disploses of the token list.

  @precon  None.
  @postcon Frees the tags tokens.

**)
destructor TTag.Destroy;
begin
  FTokens.Free;
  Inherited Destroy;
end;

(**

  This method returns all the tags tokens as a string with spaces in between.

  @precon  ShowHTML determines of the routine output the HTML tags in the
           resulting string.
  @postcon Returns a string representation of the tag.

  @param   ShowHTML as a Boolean
  @return  a String

**)
function TTag.AsString(ShowHTML : Boolean): String;

Var
  i : Integer;

begin
  Result := '';
  For i := 0 To FTokens.Count - 1 Do
    If (TokenType[i] <> ttHTMLTag) Or ((TokenType[i] = ttHTMLTag) And (ShowHTML)) Then
    Result := Result + FTokens[i] + #32;
end;

(**

  This is a getter method for the Token array property of the TTag class.
  Return the token specified by the token index.

  @precon  iTokenIndex is the index of the token required.
  @postcon Returns the tokwen as a string.

  @param   iTokenIndex as an Integer
  @return  a String

**)
function TTag.GetToken(iTokenIndex: Integer): String;

begin
  Result := FTokens[iTokenIndex];
end;

(**

  This is a getter method for the TokenCount property of the TTag class.
  It returns the number of tokens in the token list.

  @precon  None.
  @postcon Returns the number of tokens in the collection.

  @return  an Integer

**)
function TTag.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

(**

  This is a getter method for the TokenType array property of the TComment
  class. It returns the type of type indexed.

  @precon  iTokenIndex is the index of the token required.
  @postcon Returns a token type for the specified token.

  @param   iTokenIndex as an Integer
  @return  a TTokenType

**)
function TTag.GetTokenType(iTokenIndex: Integer): TTokenType;
begin
  Result := TTokenType(FTokens.Objects[iTokenIndex]);
end;

(**

  This is a setter method for the TagName property of the TTag class.

  @precon  Value is the new value of the TagName property.
  @postcon Sets the TagName property.

  @param   Value as a String constant

**)
procedure TTag.SetTagName(const Value: String);
begin
  If FTagName <> Value Then
    FTagName := Value;
end;

(** --------------------------------------------------------------------------

  TComment methods

 -------------------------------------------------------------------------- **)

(**

  This method added a token and its type to the token list.

  @precon  strToken is a string to be added as a token and iType is the token's
           type.
  @postcon Added a token and its type to the token list.

  @param   strToken as a String
  @param   iType    as a TTokenType

**)
procedure TComment.AddToken(strToken: String; iType: TTokenType);

begin
  If (strToken[1] = '@') And (Copy(strToken, 1, 2) <> '@@') Then
    Begin
      FTagMode := True;
      FLastTag := TTag.Create(Copy(strToken, 2, Length(strToken) - 1),
        FTagLine, FTagColumn - Length(strToken));
      FTags.Add(FLastTag);
      Exit; // Don't add token identifier to token list
    End;
  If Not FTagMode Then
    Begin
      If (strToken[1] = '<') And (strToken[Length(strToken)] = '>') Then
        iType := ttHTMLTag;
      FTokens.AddObject(strToken, TObject(iType))
    End Else
      FLastTag.AddToken(strToken, iType);
end;

(**

  This method appends all the tokens and tags from the source comment to this
  comment.

  @precon  srcComment is a source comment to be assign to this comment.
  @postcon Appends all the tokens and tags from the source comment to this
           comment.

  @param   srcComment as a TComment

**)
procedure TComment.Assign(srcComment: TComment);

Var
  i, j : Integer;

begin
  If srcComment <> Nil Then
    Begin
      ResetTagMode;
      // Add tokens from one to the next.
      For i := 0 To srcComment.TokenCount - 1 Do
        AddToken(srcComment.Token[i], srcComment.TokenType[i]);
      For i := 0 To srcComment.TagCount - 1 Do
        Begin
          AddToken('@' + srcComment.Tag[i].TagName, ttIdentifier);
          For j := 0 To srcComment.Tag[i].TokenCount - 1 Do
            AddToken(srcComment.Tag[i].Token[j],
              srcComment.Tag[i].TokenType[j]);
          End;
    End;
end;

(**

  This method assigns the str passed to the end of the token list. The string
  has a pre and post fix added so that the ParseComment() method will accept it
  as a valid comment.

  @precon  strComment is a string of text to be parsed as a comment.
  @postcon Assigns the str passed to the end of the token list. The string
           has a pre and post fix added so that the ParseComment() method will
           accept it as a valid comment.

  @param   strComment as a String

**)
procedure TComment.Assign(strComment: String);
begin
  ResetTagMode;
  ParseComment(strComment);
end;

(**

  This method returns a string representation of the comment tokens with the
  specified indent and broken into lines by the max width parameter.

  @precon  iIndent is the indent in space required of the comment, iMaxWidth is
           the maximum width before the comment is broken onto another line and
           ShowHTML determines if the routine outputs the HTML Tags in the
           resulting string.
  @postcon Returns a string representation of the comment indented and broken
           into lines.

  @param   iIndent   as an Integer
  @param   iMaxWidth as an Integer
  @param   ShowHTML  as a Boolean
  @return  a String

**)
function TComment.AsString(iIndent, iMaxWidth: Integer; ShowHTML : Boolean): String;

Var
  str : String;
  i : Integer;

begin
  Result := '';
  str := StringOfChar(#32, iIndent);
  For i := 0 To TokenCount - 1 Do
    If (TokenType[i] <> ttHTMLtag) Or ((TokenType[i] = ttHTMLtag) And ShowHTML) Then
    Begin
      str := str + Token[i] + #32;
      If Length(str) > iMaxWidth Then
        Begin
          If Result <> '' Then
            Result := Result + #13#10;
          Result := Result + str;
          str := StringOfChar(#32, iIndent);
        End;
    End;
  If Result <> '' Then
    Result := Result + #13#10;
  Result := Result + str;
end;

(**

  This is the TComment constructor. It create a token list and a tag list.
  Then it passes the comment to the comment parser.

  @precon  strComment is a string of text to be parsed as a comment, iLine is
           the line number of the comment and iCol is the column number of the
           comment.
  @postcon It create a token list and a tag list. Then it passes the comment to
           the comment parser.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
constructor TComment.Create(strComment : String; iLine, iCol : Integer);
begin
  Inherited Create;
  FLastTag := Nil;
  FTokens := TStringList.Create;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  FLine := iLine;
  FCol := iCol;
  ParseComment(strComment);
end;

(**

  This method is a class method to first check the comment for being a
  documentation comment and then creating an instance of a TComment class and
  parsing the comment via the constructor.

  @precon  strComment is the full comment to be checked and parsed, iLine is the
           line number of the comment and iCol is the column number of the
           comment.
  @postcon Returns Nil if this is not a documentation comment or returns a valid
           TComment class.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
class function TComment.CreateComment(strComment: String; iLine,
  iCol: Integer): TComment;

begin
  Result := Nil;
  If Length(strComment) = 0 Then Exit;
  Case strComment[1] Of
    '/' : strComment := Copy(strComment, 3, Length(strComment) - 2);
    '{' : strComment := Copy(strComment, 2, Length(strComment) - 2);
    '(' : strComment := Copy(strComment, 3, Length(strComment) - 4);
  End;
  If Length(strComment) = 0 Then Exit;
  If strComment[Length(strComment)] = '*' Then
    SetLength(strComment, Length(strComment) - 1);
  If Length(strComment) = 0 Then Exit;
  If Not (strComment[1] In [':', '*']) Then Exit;
  strComment := Copy(strComment, 2, Length(strComment) - 1);
  Result := Create(strComment, iLine, iCol);
end;

(**

  This is the TComment class's destructor. It disposes of the token list and
  the tag list.

  @precon  None.
  @postcon Frees the classes internal lists.

**)
destructor TComment.Destroy;
begin
  FTags.Free;
  FTokens.Free;
  inherited;
end;

(**

  This is a getter method for the Tag array property of the TComment class.
  It returns a TTag reference to the indexed tag item.

  @precon  iTagIndex is the index of the tag required.
  @postcon Returns an instance of the specified tag.

  @param   iTagIndex as an Integer
  @return  a TTag

**)
function TComment.GetTag(iTagIndex: Integer): TTag;
begin
  Result := (FTags[iTagIndex] As TTag);
end;

(**

  This is a getter method for the TagCount property of the TComment class.
  It return the number of tag in the list.

  @precon  None.
  @postcon Returns the number of tags in the collection.

  @return  an Integer

**)
function TComment.GetTagCount: Integer;
begin
  Result := FTags.Count;
end;

(**

  This is a getter method for the Token array property of the TComment class.
  it return  the indexed Token.

  @precon  iTokenIndex is the index of the token required.
  @postcon Returns the token as a string.

  @param   iTokenIndex as an Integer
  @return  a String

**)
function TComment.GetToken(iTokenIndex: Integer): String;
begin
  Result := FTokens[iTokenIndex];
end;

(**

  This is a getter method for the TokenCount property of the TComment class.
  It returns the number of tokens in the token list.

  @precon  None.
  @postcon Returns the number of tokens in the collection.

  @return  an Integer

**)
function TComment.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

(**

  This is a getter method for the TokenType array property of the TComment
  class. It returns the type of the token indexed.

  @precon  iTokenIndex is the index of the token required.
  @postcon Returns a token type for the specified token.

  @param   iTokenIndex as an Integer
  @return  a TTokenType

**)
function TComment.GetTokenType(iTokenIndex: Integer): TTokenType;
begin
  Result := TTokenType(FTokens.Objects[iTokenIndex]);
end;

(**

  This method takes the given comment and parses it into tokens. It pulls out
  all the tags at the same time. Tag should be at the end of the comment.

  @precon  strComment is a string of text to be parsed as a comment.
  @postcon Takes the given comment and parses it into tokens. It pulls out
           all the tags at the same time. Tag should be at the end of the comment.

  @param   strComment as a String

**)
procedure TComment.ParseComment(strComment: String);

Type
  TBlockType = (btNone, btHTML, btLink);

Const
  iTokenCapacity = 25;

Var
  i : Integer;
  CurToken : TTokenType;
  LastToken : TTokenType;
  strToken : String;
  BlockType : TBlockType;
  iTokenLen : Integer;

begin
  CurToken := ttUnknown;
  LastToken := ttUnknown;
  strToken := '';
  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);
  BlockType := btNone;
  FTagLine := FLine;
  FTagColumn := FCol + 3;
  For i := 1 To Length(strComment) Do
    Begin
      LastToken := CurToken;
      If strComment[i] In strWhiteSpace Then
        CurToken := ttWhiteSpace
      Else If strComment[i] In strLineEnd Then
        CurToken := ttLineEnd
      Else
        CurToken := ttIdentifier;

      If ((CurToken <> LastToken) And (BlockType = btNone)) Or
        (strComment[i] = '<') Then
        Begin
          SetLength(strToken, iTokenLen);
          If Not IsTokenWhiteSpace(strToken) Then
            AddToken(strToken, LastToken);
          iTokenLen := 1;
          SetLength(strToken, iTokenCapacity);
          strToken[iTokenLen] := strComment[i];;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strComment[i];
        End;

      If (BlockType = btNone) And (strToken[1] = '{') Then
        BlockType := btLink;

      If (BlockType = btLink) And (strToken[iTokenLen] = '}') Then
        Begin
          BlockType := btNone;
          CurToken := ttLinkTag;
        End;

      If strComment[i] = #10 Then
        Begin
          FTagColumn := 1;
          Inc(FTagLine);
        End Else
          Inc(FTagColumn);
    End;
  If (iTokenLen > 0) Then
    Begin
     SetLength(strToken, iTokenLen);
      If Not IsTokenWhiteSpace(strToken) Then
        AddToken(strToken, LastToken);
    End;
end;

(**

  This method resets the comment tag mode, i.e. the comment will accept text as
  tokens and not tag tokens.

  @precon  None.
  @postcon Resets the comment tag mode, i.e. the comment will accept text as
           tokens and not tag tokens.

**)
procedure TComment.ResetTagMode;
begin
  FTagMode := False;
end;

(**

  This method is a getter method for the Line property.

  @precon  None.
  @postcon Returns the line property value.

  @return  an Integer

**)
Function TComment.GetLine : Integer;

Begin
  Result := FLine;
End;

(**

  This method is a getter method for the Column property.

  @precon  None.
  @postcon Returns the column property value.

  @return  an Integer

**)
Function TComment.GetCol : Integer;

Begin
  Result := FCol;
End;

(**

  This method tries to find the given tag in the tag collection. It returns
  the index, else -1.

  @precon  strTagName is the name of the tag to search for.
  @postcon Returns the tags index if found else -1.

  @param   strTagName as a String
  @return  an Integer

**)
function TComment.FindTag(strTagName: String): Integer;

Var
  i : Integer;

begin
  Result := -1;
  For i := 0 To TagCount - 1 Do
    If AnsiCompareText(Tag[i].TagName, strTagName) = 0 Then
      Begin
        Result := i;
        Exit;
      End;
end;

(**

  This is a constructor for the TTokenInfo class. It assigns values to all the
  properties.

  @precon  strToken is a text token to create a token info object for, iPos is
           the module buffer position of the token, iLine is the line number of
           the token, iCol is the column number of the token and iLength is the
           length of the token and TType is the enumerate type of the token
           (reserved word, identifier).
  @postcon Initialises the class.

  @param   strToken as a String
  @param   iPos     as an Integer
  @param   iLine    as an Integer
  @param   iCol     as an Integer
  @param   iLength  as an Integer
  @param   TType    as a TTokenType

**)
Constructor TTokenInfo.Create(strToken : String; iPos, iLine, iCol,
  iLength: Integer; TType : TTokenType);

begin
  FToken := strToken;
  FBufferPos := iPos;
  FLine := iLine;
  FColumn := iCol;
  FLength := iLength;
  FTokenType := TType;
  FUToken := '';
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := UpperCase(strToken);
end;

(**

  This is the constructor method for the TIdent class.

  @precon  strIdent is the name of the new identifier, iLine is the line number
           of the identifier, iCol is the column number of the identifier and
           Comment is the comment associated with the identifier.
  @postcon Create the Ident class.

  @param   strIdent as a String
  @param   iLine    as an Integer
  @param   iCol     as an Integer
  @param   Comment  as a TComment

**)
constructor TIdent.Create(strIdent: String; iLine, iCol: Integer;
  Comment: TComment);
begin
  FIdent := strIdent;
  FLine := iLine;
  FCol := iCol;
  FComment := Comment;
end;

(** --------------------------------------------------------------------------

  TIdentList Methods

 -------------------------------------------------------------------------- **)

(**

  This is the constructor for the TIdentList class. It creates an internal
  TStringlist for storing the identifiers.

  @precon  None.
  @postcon Initialises the class.

**)
Constructor TIdentList.Create;

Begin
  Inherited Create;
  FComment := Nil;
  FIdents := TObjectList.Create;
End;

(**

  This is the destructor for the TIdentList class and it free the internal
  StringList.

  @precon  None.
  @postcon Frees the memory allocated for the idents.

**)
destructor TIdentList.Destroy;
begin
  FIdents.Free;
  inherited;
end;

(**

  This method is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of identifiers in the collection.

  @return  an Integer

**)
Function TIdentList.GetCount : Integer;

Begin
  Result := FIdents.Count;
End;

(**

  This is a getter method for the IndentInfo property.

  @precon  iIndex is the index of the item in the collection required.
  @postcon Returns an identifier class for the identifier requested.

  @param   iIndex as an Integer
  @return  a TIdent

**)
Function TIdentList.GetIdentInfo(iIndex : Integer) : TIdent;

Begin
  Result := FIdents[iIndex] As TIdent;
End;

(**

  This method sort the identity list.

  @precon  None.
  @postcon Sort yhe identity list.

**)
procedure TIdentList.Sort;

begin
  Inherited;
  FIdents.Sort(SortIdentList);
end;

(**

  This method adds an identifier to the internal string list and stores the
  line number in the lower 16 bits of the Data pointer and the column number
  in the upper 16 bits of the Data pointer.

  @precon  strIdent is a text identifier to add to the collection, iLine is the
           line number of the identifier, iCol is the column number of the
           collection and Comment is the comment to be associated with the
           identifier.
  @postcon Adds an identifier to the internal string list and stores the
           line number in the lower 16 bits of the Data pointer and the column
           number in the upper 16 bits of the Data pointer.

  @param   strIdent as a String
  @param   iLine    as an Integer
  @param   iCol     as an Integer
  @param   Comment  as a TComment

**)
Procedure TIdentList.Add(strIdent : String; iLine, iCol : Integer;
  Comment : TComment);

Begin
  FIdents.Add(TIdent.Create(strIdent, iLine, iCol, Comment));
End;

(**

  This method added the contents of the source ident list to this ident list.

  @precon  src is another valid identifier object to get identifiers from.
  @postcon Added the contents of the source ident list to this ident list.

  @param   src as a TIdentList

**)
Procedure TIdentList.Assign(src : TIdentList);

Var
  i : Integer;

Begin
  For i := 0 To src.Count - 1 Do
    Add(src[i].Ident, src[i].Line, src[i].Col, src[i].Comment);
End;

(**

  This method returns the ident list as a comma separated list of tokens.

  @precon  None.
  @postcon Returns a string representation of the identifiers comma separated.

  @return  a String

**)
function TIdentList.AsString: String;

Var
  I : Integer;

begin
  Result := '';
  For i := 0 To FIdents.Count - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + ', ';
      Result := Result + Idents[i].Ident;
    End;
end;

(** --------------------------------------------------------------------------

  TConstant Methods

 ------------------------------------------------------------------------- **)

(**

  This is the constructor for the TConstant class. It creates an internal
  TStringList for the constants text.

  @precon  None
  @postcon It creates an internal StringList for the constants text.

**)
Constructor TGenericContainer.Create;

Begin
  Inherited Create;
  FComment := Nil;
  FTokens := TStringList.Create;
  FIdentifier := '';
  FScope := scPrivate;
  FLine := 0;
  FCol := 0;
End;

(**

  This is the constructor for the TConstant class. It creates an internal
  TStringList for the constants text and initialises the identifier, scope
  line and column attributes.

  @precon  strIdentifier is a text identifier for the item to be created,
           Scope is the scope of the item, iLine is the line number of the
           item and iCol is the column number of the item.
  @postcon It creates an internal StringList for the constants text and
           initialises the identifier, scope line and column attributes.

  @param   strIdentifier as a String
  @param   Scope         as a TScope
  @param   iLine         as an Integer
  @param   iCol          as an Integer

**)
Constructor TGenericContainer.Create(strIdentifier  :String; Scope : TScope;
  iLine, iCol : Integer);

Begin
  Self.Create;
  FIdentifier := strIdentifier;
  FScope := Scope;
  FLine := iLine;
  FCol := iCol;
End;

(**

  This is the constructor for the TConstant class. It creates an internal
  TStringList for the constants text and initialises the identifier, scope
  line and column attributes.

  @precon  Ident is a identifier info create to create the item from.
  @postcon It creates an internal TStringList for the constants text and
           initialises the identifier, scope line and column attributes.

  @param   Ident as a TIdentInfo

**)
constructor TGenericContainer.Create(Ident: TIdentInfo);
begin
  Self.Create;
  FIdentifier := Ident.Ident;
  FScope := Ident.Scope;
  FLine := Ident.Line;
  FCol := Ident.Col;
end;

(**

  This is the TConstant classes destructor. It frees the internal TStringList.

  @precon  None.
  @postcon It frees the internal TStringList.

**)
Destructor TGenericContainer.Destroy;

Begin
  FTokens.Free;
  Inherited Destroy;
End;

(**

  This method adds the passed token to the internal string list.

  @precon  strToken is a text token to add to the collection.
  @postcon Adds the passed token to the internal string list.

  @param   strToken as a String

**)
Procedure TGenericContainer.Add(strToken : String);

Begin
  FTokens.Add(strToken);
End;

(**

  This is a getter method for the Token property.

  @precon  iIndex is the index of the token required.
  @postcon Returns the required token as a string.

  @param   iIndex as an Integer
  @return  a String

**)
Function TGenericContainer.GetToken(iIndex : Integer) : String;

Begin
  Result := FTokens[iIndex];
End;

(**

  This is a getter method for the TokenCount property.

  @precon  None.
  @postcon returns the number of tokens in the collection.

  @return  an Integer

**)
Function TGenericContainer.GetTokenCount : Integer;

Begin
  Result := FTokens.Count;
End;

(**

  This method allows the contents of a source string list to be assigned to
  the internal string list.

  @precon  src is another generic container collection to get tokens from.
  @postcon Allows the contents of a source string list to be assigned to
           the internal string list.

  @param   src as a TPersistent

**)
Procedure TGenericContainer.Assign(src : TPersistent);

Begin
  If FTokens <> Nil Then
    FTokens.Assign(src);
End;

(**

  This method appends the tokens in the src container on to the end of this
  container.

  @precon  src is another generic container collection to append tokens from.
  @postcon Appends the tokens in the src container on to the end of this
           container.

  @param   src as a TGenericContainer

**)
procedure TGenericContainer.Append(src: TGenericContainer);

Var
  i : Integer;

begin
  If FTokens <> Nil Then
    For i := 0 To src.Count - 1 Do
      FTokens.Add(src[i]);
end;

(**

  This method return the value of the constant as a string.

  @precon  ShowFirstToken tells the method to display the first token in the
           returned string.
  @postcon Returns a string representation of the item.

  @param   ShowFirstToken as a Boolean
  @return  a String

**)
function TGenericContainer.AsString(ShowFirstToken : Boolean): String;

Var
  i : Integer;

begin
  Result := '';
  For i := (1 - Integer(ShowFirstToken)) To FTokens.Count - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + FTokens[i];
    End;
end;

(**

  This method allows the insertion of the given text in to the token collection
  at the given index.

  @precon  strText is a string token to insert into the container and iIndex is
           the position where the token should be inserted.
  @postcon Allows the insertion of the given text in to the token collection
           at the given index.

  @param   strText as a String
  @param   iIndex  as an Integer

**)
procedure TGenericContainer.Insert(strText: String; iIndex: Integer);

begin
  FTokens.Insert(iIndex, strText);
end;

(**

  This method is a virtual sort method not implemented, but should be
  overridden by descendants.

  @precon  None.
  @postcon None.

**)
procedure TGenericContainer.Sort;
begin
  // Do nothing - all descendants need to implement and override this method.
end;

(** --------------------------------------------------------------------------

  TGenericContainerCollection Methods

 -------------------------------------------------------------------------- **)

(**

  This is the ConstantCollections constructor. It creates a object list to hold
  and manage all the constants that are added to the collection.

  @precon  OwnItems tells the collection to dispose of the memory belonging to
           the items in the collection, else its the responsibility if the
           creator.
  @postcon It creates a object list to hold and manage all the constants that
           are added to the collection.

  @param   OwnItems as a Boolean

**)
Constructor TGenericContainerCollection.Create(OwnItems : Boolean);

Begin
  Inherited Create;
  FComment := Nil;
  FItems := TObjectList.Create(OwnItems);
End;

(**

  This is the constant collection destructor method. It frees the object list
  and in turn frees all the constants added.

  @precon  None.
  @postcon It frees the object list and in turn frees all the constants added.

**)
Destructor TGenericContainerCollection.Destroy;

Begin
  FItems.Free;
  Inherited Destroy;
End;

(**

  This method adds the specified constant to the internal collection.

  @precon  AItem is generic container to add to the collection.
  @postcon Adds the specified constant to the internal collection.

  @param   AItem as a TGenericContainer

**)
Procedure TGenericContainerCollection.Add(AItem : TGenericContainer);

Begin
  FItems.Add(AItem);
End;

(**

  This is a getter method for the Constant array property.

  @precon  iIndex is the index of the item in the collection required.
  @postcon Returns a generic container for the item requested.

  @param   iIndex as an Integer
  @return  a TGenericContainer

**)
Function TGenericContainerCollection.GetItem(iIndex : Integer) : TGenericContainer;

Begin
  Result := FItems[iIndex] As TGenericContainer;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon returns the number of items in the collection.

  @return  an Integer

**)
Function TGenericContainerCollection.GetCount : Integer;

Begin
  Result := FItems.Count;
End;

(**

  This method removes any forward declarations from the types collection.

  @precon  None.
  @postcon Removes any forward declarations from the types collection.

**)
Procedure TGenericContainerCollection.RemoveForwardDecls;

Var
  i : Integer;

Begin
  // Search for forward record, object, class and interface declarations and
  // remove
  For i := FItems.Count - 1 DownTo 0 Do
    Begin
      If Items[i] is TClassDecl Then
        With Items[i] As TClassDecl Do
          If Heritage.Count + ParameterCount + PropertyCount + MethodCount = 0 Then
            FItems.Delete(i);
    End;
End;

(**

  This method sorts the classes Items list and then calls each item in term and
  asks them to sort themselves.

  @precon  None.
  @postcon Sorts the classes Items list and then calls each item in term and
           asks them to sort themselves.

**)
procedure TGenericContainerCollection.Sort;

Var
  i : Integer;

begin
  Inherited;
  FItems.Sort(SortGenericContainerCollection);
  For i := 0 To Count - 1 Do
    Items[i].Sort;
end;

(**

  This method finds th index in the collection of the specified class name else
  returns -1.

  @precon  strClassName is the name of the class of items to be found in the
           collection.
  @postcon Returns the index of the found item else returns -1.

  @param   strClassName as a String
  @return  an Integer

**)
function TGenericContainerCollection.Find(strClassName: String): Integer;

Var
  i : Integer;

begin
  Result := -1;
  For i := 0 To Count - 1 Do
    If FItems[i] Is TObjectDecl Then
      If strClassName = (FItems[i] As TObjectDecl).Identifier Then
        Begin
          Result := i;
          Exit;
        End;
end;


(** --------------------------------------------------------------------------

  TParameter Methods

 -------------------------------------------------------------------------- **)

(**

  This method is a virtual assign procedure so that the properties of a
  Parameter can be assigned to another instance of the same class.

  @precon  Parameter is the parameter declaration to get information from.
  @postcon A virtual assign procedure so that the properties of a Parameter can
           be assigned to another instance of the same class.

  @param   Parameter as a TParameter

**)
procedure TParameter.Assign(Parameter: TParameter);
begin
  FArrayOf := Parameter.ArrayOf;
  FCol := Parameter.Col;
  //FComment
  FDefaultValue := Parameter.DefaultValue;
  FIdentifier := Parameter.Identifier;
  FLine := Parameter.Line;
  FParamModifier := Parameter.ParamModifier;
  FParamType.Append(Parameter.ParamType);
  FScope := Parameter.Scope;
end;

(**

  This is the constructor for the TParameter class. The constructor initialises
  all the attributes of the classes on construction. It creates a string list
  to store the parameter type.

  @precon  ParamMod is an enumerate identifying a const, var or out parameter,
           Ident is the parameters identifier, boolArrayOf indicate whether the
           parameter is an array, AType is the type of the parameter, Value is
           the constant value for the parameter is applicable, Scope is the
           scope of the parameter, iLine is the line number of the parameter
           and iCol is the column number of the icon.
  @postcon The constructor initialises all the attributes of the classes on
           construction. It creates a string list to store the parameter type.

  @param   ParamMod    as a TParamModifier
  @param   Ident       as a String
  @param   boolArrayOf as a Boolean
  @param   AType       as a TTypes
  @param   Value       as a String
  @param   Scope       as a TScope
  @param   iLine       as an Integer
  @param   iCol        as an Integer

**)
Constructor TParameter.Create(ParamMod : TParamModifier; Ident : String;
  boolArrayOf : Boolean; AType : TTypes;
  Value : String; Scope : TScope; iLine, iCol : Integer);

Begin
  FParamType := TTypes.Create;
  If AType <> Nil Then
    FParamType.Append(AType);
  FParamModifier := ParamMod;
  FIdentifier := Ident;
  FArrayOf := boolArrayOf;
  FDefaultValue := Value;
  FScope := Scope;
  FLine := iLine;
  FCol := iCol;
  FComment := Nil;
End;

(**

  This is the TParameters destructor method. If frees the parameter type string
  list.

  @precon  None.
  @postcon If frees the parameter type string list.

**)
destructor TParameter.Destroy;
begin
  FParamType.Free;
  inherited;
end;

(** --------------------------------------------------------------------------

  TProperty Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds a parameter to the parameter collection.

  @precon  Parameter is a parameter to be added to the property.
  @postcon Adds a parameter to the parameter collection.

  @param   Parameter as a TParameter

**)
procedure TProperty.AddParameter(Parameter: TParameter);
begin
  FParameter.Add(Parameter);
end;

(**

  This method is a virtual assign procedure so that the properties of a
  Property can be assigned to another instance of the same class.

  @precon  Prop is the property declaration to get information from.
  @postcon Virtual assign procedure so that the properties of a Property can be
           assigned to another instance of the same class.

  @param   Prop as a TProperty

**)
procedure TProperty.Assign(Prop: TProperty);

Var
  i : Integer;
  P : TParameter;

begin
  FCol := Prop.Col;
  //FComment
  FDefaultProperty := Prop.DefaultProperty;
  FDefaultSpec := Prop.DefaultSpec;
  FDispIDSpec := Prop.DispIdSpec;
  FIdentifier := Prop.Identifier;
  FImplementsSpec := Prop.ImplementsSpec;
  FIndexSpec := Prop.IndexSpec;
  FLine := Prop.Line;
  For i := 0 To Prop.ParameterCount - 1 Do
    Begin
      P := TParameter.Create(pmNone, '', False, Nil, '', scPrivate, 0, 0);
      P.Assign(Prop.Parameter[i]);
      AddParameter(P);
    End;
  FReadOnlySpec := Prop.ReadOnlySpec;
  FReadSpec := Prop.ReadSpec;
  FScope := Prop.Scope;
  FStoredSpec := Prop.StoredSpec;
  FTypeId := Prop.TypeId;
  FWriteOnlySpec := Prop.WriteOnlySpec;
  FWriteSpec := Prop.WriteSpec;
end;

(**

  This is the TProperty classes constructor. It initialises the basic
  information for the class. If also creates a parameter collection for the
  storage of parameters.

  @precon  strIdent is a text identifier for the property, Scope is the scope of
           the property, iLine is the line number of the icon and iCol is the
           column number of the icon.
  @postcon It initialises the basic information for the class. If also creates a
           parameter collection for the storage of parameters.

  @param   strIdent as a String
  @param   Scope    as a TScope
  @param   iLine    as an Integer
  @param   iCol     as an Integer

**)
constructor TProperty.Create(strIdent: String; Scope: TScope;
  iLine, iCol : Integer);
begin
  FComment := Nil;
  FDefaultProperty := False;
  FDefaultSpec := '';
  FDispIDSpec := '';
  FImplementsSpec := '';
  FIndexSpec := '';
  FReadOnlySpec := False;
  FWriteOnlySpec := False;
  FReadSpec := '';
  FStoredSpec := '';
  FTypeId := '';
  FWriteSpec := '';
  FScope := Scope;
  FParameter := TObjectList.Create(True);
  FIdentifier := strIdent;
  FLine := iLine;
  FCol := iCol;
end;

(**

  This is the classes destructor. It frees the parameter collection and the
  parameters.

  @precon  None.
  @postcon It frees the parameter collection and the parameters.

**)
destructor TProperty.Destroy;
begin
  FParameter.Free;
  inherited;
end;

(**

  This is a getter method for the AsString property. It returns a string
  representation of the property.

  @precon  None.
  @postcon Returns a string representation of the property.

  @return  a String

**)
function TProperty.GetAsString: String;

Var
  I : Integer;

begin
  Result := Identifier;
  If ParameterCount > 0 Then
    Begin
      Result := Result + '[';
      For i := 0 To ParameterCount - 1 Do
        Begin
          Case Parameter[i].ParamModifier Of
            pmConst : Result := Result + 'Const ';
            pmVar: Result := Result + 'Var ';
            pmOut: Result := Result + 'Out ';
          End;
          If i <> 0 Then
            Result := Result + '; ';
          Result := Result + Parameter[i].Identifier + ' : ' +
            Parameter[i].ParamType.AsString(True);
        End;
      Result := Result + ']';
    End;
  If TypeId <> '' Then
    Result := Result + ' : ' + TypeId;
  // Get specifiers
  If IndexSpec <> '' Then
    Result := Result + ' Index ' + IndexSpec;
  If ReadSpec <> '' Then
    Result := Result + ' Read ' + ReadSpec;
  If WriteSpec <> '' Then
    Result := Result + ' Write ' + WriteSpec;
  If StoredSpec <> '' Then
    Result := Result + ' Stored ' + StoredSpec;
  If DefaultSpec <> '' Then
    Result := Result + ' Default ' + DefaultSpec;
  If ImplementsSpec <> '' Then
    Result := Result + ' Implements ' + ImplementsSpec;
  If ReadOnlySpec Then
    Result := Result + ' ReadOnly';
  If DispIdSpec <> '' Then
    Result := Result + ' DispId ' + DispIdSpec;
  If DefaultProperty Then
    Result := Result + '; Default';
end;

(**

  This is a getter method for the Parameter array property.

  @precon  iIndex is the index of the parameter of the property required.
  @postcon Returns a parameter object for the requested item.

  @param   iIndex as an Integer
  @return  a TParameter

**)
function TProperty.GetParameter(iIndex: Integer): TParameter;
begin
  Result := FParameter[iIndex] As TParameter;
end;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of parameters in the property.

  @return  an Integer

**)
function TProperty.GetParameterCount: Integer;
begin
  Result := FParameter.Count;
end;

(**

  This is a setter method for the DefaultProperty property.

  @precon  Value is the new value to assign to the DefaultProperty property.
  @postcon Sets the default property.

  @param   Value as a Boolean constant

**)
procedure TProperty.SetDefaultProperty(const Value: Boolean);
begin
  If FDefaultProperty <> Value Then
    FDefaultProperty := Value;
end;

(**

  This is a setter method for the DefaultSpec property.

  @precon  Value is the new value to assign to the DefaultSpec property.
  @postcon Sets the default Spec property.

  @param   Value as a String constant

**)
procedure TProperty.SetDefaultSpec(const Value: String);
begin
  If FDefaultSpec <> Value Then
    FDefaultSpec := Value;
end;

(**

  This is a setter method for the ImplementsSpec property.

  @precon  Value is the new value to assign to the ImplementsSpec property.
  @postcon Sets the implementsSpec property.

  @param   Value as a String constant

**)
procedure TProperty.SetImplementsSpec(const Value: String);
begin
  If FImplementsSpec <> Value Then
    FImplementsSpec := Value;
end;

(**

  This is a setter method for the IndexSpec property.

  @precon  Value is the new value to assign to the IndexSpec property.
  @postcon Setst the indexspec property.

  @param   Value as a String constant

**)
procedure TProperty.SetIndexSpec(const Value: String);
begin
  If FIndexSpec <> Value Then
    FIndexSpec := Value;
end;

(**

  This is a setter method for the ReadSpec property.

  @precon  Value is the new value to assign to the ReadSpec property.
  @postcon Sets the readspec property.

  @param   Value as a String constant

**)
procedure TProperty.SetReadSpec(const Value: String);
begin
  If FReadSpec <> Value Then
    FReadSpec := Value;
end;

(**

  This is a setter method for the StoredSpec property.

  @precon  Value is the new value to assign to the StoredSpec property.
  @postcon Sets the stored spec property.

  @param   Value as a String constant

**)
procedure TProperty.SetStoredSpec(const Value: String);
begin
  If FStoredSpec <> Value Then
    FStoredSpec := Value;
end;

(**

  This is a setter method for the TypeId property.

  @precon  Value is the new value to assign to the TyprId property.
  @postcon Sets type id property.

  @param   Value as a String constant

**)
procedure TProperty.SetTypeId(const Value: String);
begin
  If FTypeId <> Value Then
    FTypeId := Value;
end;

(**

  This is a setter method for the WriteSpec property.

  @precon  Value is the new value to assign to the WriteSpec property.
  @postcon Sets the write spec property.

  @param   Value as a String constant

**)
procedure TProperty.SetWriteSpec(const Value: String);
begin
  If FWriteSpec <> Value Then
    FWriteSpec := Value;
end;

(** --------------------------------------------------------------------------

  TMethodDecl Methods

 -------------------------------------------------------------------------- **)

(**

  This is the constructor for the TMethodDecl class. It initialises the method
  type, scope and line and col information. If also creates a colection to
  store the parameter objects and a string list for the method directives.

  @precon  MethodType is an enumerate indocating the type of the method, Scope
           is the scope of the method, iLine is the line number of the method,
           and iCol is the column number of the method.
  @postcon It initialises the method type, scope and line and col information.

  @param   MethodType as a TMethodType
  @param   Scope      as a TScope
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
Constructor TMethodDecl.Create(MethodType : TMethodType; Scope : TScope;
  iLine, iCol : Integer);

Begin
  Inherited Create;
  FAlias := '';
  FClassMethod := False;
  FClsName := '';
  FComment := Nil;
  FExt := '';
  FIdentifier := '';
  FMsg := '';
  FReturnType := '';
  FParameter := TObjectList.Create(True);
  FDirectives := TStringList.Create;
  FMethodType := MethodType;
  FScope := Scope;
  FLine := iLine;
  FCol := iCol;
  FLocalMethods := TMethodCollection.Create;
  FTypes := TGenericContainerCollection.Create(False);
  FVars := TGenericContainerCollection.Create(True);
  FConsts := TGenericContainerCollection.Create(True);
  FResStrings := TGenericContainerCollection.Create(True);
End;

(**

  This is the destructor method for the TMethodDecl class. It frees the
  parameters collection, the parameter and the directives.

  @precon  None.
  @postcon It frees the parameters collection, the parameter and the directives.

**)
Destructor TMethodDecl.Destroy;

Begin
  FResStrings.Free;
  FConsts.Free;
  FVars.Free;
  FTypes.Free;
  FLocalMethods.Free;
  FDirectives.Free;
  FParameter.Free;
  Inherited Destroy;
End;

(**

  This method is a virtual assign procedure so that the properties of a
  method can be assigned to another instance of the same class.

  @precon  Method is the method declaration to get information from.
  @postcon Virtual assign procedure so that the properties of a method can be
           assigned to another instance of the same class.

  @param   Method as a TMethodDecl

**)
procedure TMethodDecl.Assign(Method: TMethodDecl);

Var
  i : Integer;
  P : TParameter;

begin
  FAlias := Method.Alias;
  FClassMethod := Method.ClassMethod;
  FClsName := Method.ClsName;
  FCol := Method.Col;
  //FComment
  //FConsts
  FDirectives.Assign(Method.Directives);
  FExt := Method.Ext;
  FIdentifier := Method.Identifier;
  FLine := Method.Line;
  //FLocalMethods
  FMethodType := Method.MethodType;
  FMsg := Method.Msg;
  For i := 0 To Method.ParameterCount - 1 Do
    Begin
      P := TParameter.Create(pmNone, '', False, Nil, '', scPrivate, 0, 0);
      P.Assign(Method.Parameter[i]);
      Add(P);
    End;
  //FResStrings
  FReturnType := Method.ReturnType;
  FScope := Method.Scope;
  //FTypes
  //FVars
end;

(**

  This is a setter method for the ClsName property.

  @precon  Value is the new value to assign to the ClsName property.
  @postcon Setst the class name property.

  @param   Value as a String

**)
Procedure TMethodDecl.SetClsName(Value : String);

Begin
  If FClsName <> Value Then
    FClsName := Value;
End;

(**

  This is a setter method for the Identifier property.

  @precon  Value is the new value to assign to the Identifier property.
  @postcon Sets the identifier property.

  @param   Value as a String

**)
Procedure TMethodDecl.SetIdentifier(Value : String);

Begin
  If FIdentifier <> Value Then
    FIdentifier := Value;
End;

(**

  This is a setter method for the ReturnType property.

  @precon  Value is the new value to assign to the ReturnType property.
  @postcon Sets the return type property.

  @param   Value as a String

**)
Procedure TMethodDecl.SetReturnType(Value : String);

Begin
  If FReturnType <> Value Then
    FReturnType := Value;
End;

(**

  This method adds a parameter class to the parameters collection.

  @precon  Paremeter is a parameter to add to the method.
  @postcon Adds a parameter class to the parameters collection.

  @param   Parameter as a TParameter

**)
Procedure TMethodDecl.Add(Parameter : TParameter);

Begin
  FParameter.Add(Parameter);
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of parameters in the method.

  @return  an Integer

**)
Function TMethodDecl.GetCount : Integer;

Begin
  Result := FParameter.Count;
End;

(**

  This is a getter method for the Parameter array property.

  @precon  iIndex is th eindex of the parameter required.
  @postcon Returns a paramemter object for the specified item.

  @param   iIndex as an Integer
  @return  a TParameter

**)
Function TMethodDecl.GetParameter(iIndex : Integer) : TParameter;

Begin
  Result := FParameter[iIndex] As TParameter;
End;

(**

  This method returns a fully qualified name for the method.

  @precon  None.
  @postcon Returns a fully qualified name for the method.

  @return  a String

**)
Function TMethodDecl.GetQualifiedName : String;

Begin
  Result := Identifier;
  If ClsName <> '' Then
    Result := ClsName + '.' + Result;
End;


(**

  This method adds a directive to the directives list.

  @precon  strDirective is a directive token to be added to the directives
           collection.
  @postcon Adds a directive to the directives list.

  @param   strDirective as a String

**)
Procedure TMethodDecl.AddDirectives(strDirective : String);

Begin
  FDirectives.Add(strDirective);
End;

(**

  This is a getter method for the AsString property.

  @precon  ShowClassName tell the routine to returns the class name in the
           resultant string and ShowMethodType tell the routine to returns the
           methods type in the resultant string.
  @postcon Returns a string representation of the method.

  @param   ShowClassName  as a Boolean
  @param   ShowMethodType as a Boolean
  @return  a String

**)
Function TMethodDecl.GetAsString(ShowClassName, ShowMethodType : Boolean) : String;

Var
  i : Integer;

Begin
  Result := '';
  If ShowMethodType Then
    Result := strMethodTypes[MethodType];
  If ClassMethod Then
    Result := Result + 'Class ';
  If (ClsName <> '') And ShowClassName Then
    Result := Result + ClsName + '.';
  Result := Result + Identifier;
  // Get parameters
  If ParameterCount > 0 Then Result := Result + '(';
  For i := 0 To ParameterCount - 1 Do
    Begin
      Result := Result + strParamModifier[Parameter[i].ParamModifier];
      Result := Result + Parameter[i].Identifier;
      If (i <> ParameterCount - 1) And (Parameter[i].ParamType.AsString(True) =
        Parameter[i + 1].ParamType.AsString(True)) Then
        Result := Result + ', '
      Else
        If Parameter[i].ParamType.Count > 0 Then
          Begin
            Result := Result + ' :';
            If Parameter[i].ArrayOf Then Result := Result + ' Array Of';
            Result := Result + #32 + Parameter[i].ParamType.AsString(True);
            If i <> ParameterCount - 1 Then Result := Result + '; ';
          End;
    End;
  If ParameterCount > 0 Then Result := Result + ')';
  // Get return type
  If ReturnType <> '' Then Result := Result + ' : ' + ReturnType;
  // Get directives
  If Directives.Count > 0 Then
    For i := 0 To Directives.Count - 1 Do
      result := Result + '; ' + Directives[i];
  If Msg <> '' Then
    Result := Result + #32 + Msg;
  If Ext <> '' Then
    Result := Result + #32 + Ext;
  If Alias <> '' Then
    Result := Result + ' = ' + Alias;
End;

(**

  This method test the directive for a specified directive and returns true if
  found.

  @precon  strDirective is the directive to search for.
  @postcon Returns true if the directive was found.

  @param   strDirective as a String
  @return  a Boolean

**)
function TMethodDecl.HasDirective(strDirective: String): Boolean;

Var
  i : Integer;

begin
  Result := False;
  For i := 0 To Directives.Count - 1 Do
    If AnsiCompareText(strDirective, Directives[i]) = 0 Then
      Begin
        Result := True;
        Exit;
      End;
end;

(**

  This is a setter method for the Msg property.

  @precon  Value is the new value to assign to the Msg property.
  @postcon Sets the Message property for the method.

  @param   Value as a String constant

**)
procedure TMethodDecl.SetMsg(const Value: String);
begin
  If FMsg <> Value Then
    FMsg := Value;
end;

(**

  This is a setter method for the Ext property.

  @precon  Value is the new value to assign to the Ext property.
  @postcon Setst the extension property for the method.

  @param   Value as a String constant

**)
procedure TMethodDecl.SetExt(const Value: String);
begin
  If FExt <> Value Then
    FExt := Value;
end;

(** --------------------------------------------------------------------------

  TMethodCollection Methods

 -------------------------------------------------------------------------- **)

(**

  This is the constructor method for the TMethodCollection class. It creates
  a collection object to hold all the methods.

  @precon  None.
  @postcon It creates a collection object to hold all the methods.

**)
Constructor TMethodCollection.Create;

Begin
  Inherited Create;
  FMethods := TObjectList.Create(True);
End;

(**

  This is the destructor method for the TMethodCollection class. It frees the
  method collection and all the methods stored.

  @precon  None.
  @postcon It frees the method collection and all the methods stored.

**)
Destructor TMethodCollection.Destroy;

Begin
  FMethods.Free;
  Inherited Destroy;
End;

(**

  This method adds a TMethodDecl class to the methods collection.

  @precon  Method is a method declaration to be added to the collection.
  @postcon Adds a TMethodDecl class to the methods collection.

  @param   Method as a TMethodDecl

**)
Procedure TMethodCollection.Add(Method : TMethodDecl);

Begin
  FMethods.Add(Method);
End;

(**

  This is a getter method for the Method property.

  @precon  iIndex is the index of the method required.
  @postcon Returns a method declaration object for the requested item.

  @param   iIndex as an Integer
  @return  a TMethodDecl

**)
Function TMethodCollection.GetMethod(iIndex : Integer) : TMethodDecl;

Begin
  Result := FMethods[iIndex] As TMethodDecl;
End;

(**

  Thos method sorts the methods in the class.

  @precon  None.
  @postcon Sorts the methods in the class.

**)
procedure TMethodCollection.Sort;
begin
  Inherited;
  FMethods.Sort(SortMethodCollection);
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of method in the collection.

  @return  an Integer

**)
Function TMethodCollection.GetCount : Integer;

Begin
  Result := FMethods.Count;
End;

(**

  This method returns the index of the specified method else returns -1.

  @precon  strMethodName is the name of a method to find.
  @postcon Returns the index of the method if found else -1.

  @param   strMethodName as a String
  @return  an Integer

**)
function TMethodCollection.Find(strMethodName: String): Integer;

Var
  i : Integer;

begin
  Result := -1;
  For i := 0 To Count - 1 Do
    If strMethodName = Method[i].Identifier Then
      Begin
        Result := i;
        Exit;
      End;
end;

(** ---------------------------------------------------------------------------

  TRecordDecl Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds a parameter to the classes parameter collection.

  @precon  Parameter is a parameter to be added to the records field collection.
  @postcon Adds a parameter to the classes parameter collection.

  @param   Parameter as a TParameter

**)
procedure TRecordDecl.AddParameter(Parameter: TParameter);
begin
  FParameter.Add(Parameter);
end;

(**

  This method returns a string representation of the record excluding fields.

  @precon  ShowFirstToken tells the method whether to show the first token in
           the resultant string.
  @postcon Returns a string representation of the records declaration only.

  @param   ShowFirstToken as a Boolean
  @return  a String

**)
Function TRecordDecl.AsString(ShowFirstToken : Boolean) : String;

Begin
  Result := Identifier + ' = Record';
  If IsPacked Then
    Result := 'Packed ' + Result;
End;

(**

  This is the constructor method for the TRecordDecl class.

  @precon  None.
  @postcon Initialises the class.

**)
constructor TRecordDecl.Create;
begin
  inherited;
  FPacked := False;
  FParameter := TObjectList.Create(True);
end;

(**

  This is the destructor method for the TRecordDecl class.

  @precon  None.
  @postcon Destroy the instance of the class.

**)
destructor TRecordDecl.Destroy;
begin
  FParameter.Free;
  inherited;
end;

(**

  This is a getter method for the Parameter array property.

  @precon  iIndex is the index of the paramemter required.
  @postcon Returns the parameter object requested.

  @param   iIndex as an Integer
  @return  a TParameter

**)
function TRecordDecl.GetParameter(iIndex: Integer): TParameter;
begin
  Result := FParameter[iIndex] As TParameter;
end;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of fields in the record.

  @return  an Integer

**)
function TRecordDecl.GetParameterCount: Integer;
begin
  Result := FParameter.Count;
end;

(**

  This method sorts the record classes paramters.

  @precon  None.
  @postcon Sorts the record classes paramters.

**)
procedure TRecordDecl.Sort;
begin
  inherited;
  FParameter.Sort(SortRecordDecl);
end;

(** ---------------------------------------------------------------------------

  TObjectDecl Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds a method to the classes methods collection.

  @precon  Method is a method declaration to be added to the object.
  @postcon Adds a method to the classes methods collection.

  @param   Method as a TMethodDecl

**)
procedure TObjectDecl.AddMethod(Method: TMethodDecl);
begin
  FMethod.Add(Method);
end;

(**

  This method is a virtual assign procedure so that the properties of a
  TObjectDecl can be assigned to another instance of the same class.

  @precon  AObject is the object declaration to get information from.
  @postcon Virtual assign procedure so that the properties of a TObjectDecl can
           be assigned to another instance of the same class.

  @param   AObject as a TObjectDecl

**)
procedure TObjectDecl.Assign(AObject: TObjectDecl);

Var
  i : Integer;
  P : TParameter;
  M : TMethodDecl;

begin
  Identifier := AObject.Identifier;
  Heritage.Assign(AObject.Heritage);
  Scope := AObject.Scope;
  For i := 0 To AObject.ParameterCount - 1 Do
    Begin
      P := TParameter.Create(pmNone, '', False, Nil, '', scPrivate, 0, 0);
      P.Assign(AObject.Parameter[i]);
      AddParameter(P);
    End;
  For i := 0 To AObject.MethodCount - 1 Do
    Begin
      M := TMethodDecl.Create(mtProcedure, scPrivate, 0, 0);
      M.Assign(AObject.Method[i]);
      AddMethod(M);
    End;
end;

(**

  This method returns a string representation of the object exlcluding fields
  and methods.

  @precon  ShowFirstToken tells the method to show the first token in the
           resultant string.
  @postcon Returns a string representation of the object declaration.

  @param   ShowFirstToken as a Boolean
  @return  a String

**)
Function TObjectDecl.AsString(ShowFirstToken : Boolean) : String;

Begin
  Result := Identifier + ' = Object';
  If Heritage.Count > 0 Then
    Result := Result + '(' + Heritage.AsString + ')';
End;


(**

  This is the constructor method for the TObjectDecl class.

  @precon  None.
  @postcon Initialises the class.

**)
constructor TObjectDecl.Create;
begin
  inherited;
  FHeritage := TIdentList.Create;
  FMethod := TObjectList.Create(True);
end;

(**

  This is the destructor method for the TObjectDecl class.

  @precon  None.
  @postcon Destroy the instance of the class.

**)
destructor TObjectDecl.Destroy;
begin
  FMethod.Free;
  FHeritage.Free;
  inherited;
end;

(**

  This method find the index of the specified method in the method collection
  else returns -1.

  @precon  strMethodName is the method name to find the in the object.
  @postcon Returns the index of the method if found else -1.

  @param   strMethodName as a String
  @return  an Integer

**)
function TObjectDecl.FindMethod(strMethodName: String): Integer;

Var
  i : Integer;

begin
  Result := -1;
  For i := 0 To MethodCount - 1 Do
    If strMethodName = Method[i].Identifier Then
      Begin
        Result := i;
        Exit;
      End;
end;

(**

  This is a getter method for the MethodCount property.

  @precon  None.
  @postcon Returns the number of method in the object.

  @return  an Integer

**)
function TObjectDecl.GetMethodCount: Integer;
begin
  Result := FMethod.Count;
end;

(**

  This is a getter method for the MethodDecl array property.

  @precon  iIndex is the index of the method required.
  @postcon returns the method declaration object requested.

  @param   iIndex as an Integer
  @return  a TMethodDecl

**)
function TObjectDecl.GetMethodDecl(iIndex: Integer): TMethodDecl;
begin
  Result := FMethod[iIndex] As TMethodDecl;
end;

(**

  This method sorts the object class's methods.

  @precon  None.
  @postcon Sorts the object classes methods.

**)
procedure TObjectDecl.Sort;
begin
  inherited;
  FMethod.Sort(SortMethodDecl);
end;

(** ---------------------------------------------------------------------------

  TClassDecl Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds a property to the classes property collection.

  @precon  Prop is a property to be added to the class.
  @postcon Adds a property to the classes property collection.

  @param   Prop as a TProperty

**)
procedure TClassDecl.AddProperty(Prop: TProperty);
begin
  FProperty.Add(Prop);
end;

(**

  This method is a virtual assign procedure so that the properties of a
  TClassDecl can be assigned to another instance of the same class.

  @precon  AObject is the object declaration to get information from.
  @postcon Virtual assign procedure so that the properties of a TClassDecl can
           be assigned to another instance of the same class.

  @param   AObject as a TObjectDecl

**)
procedure TClassDecl.Assign(AObject: TObjectDecl);

Var
  i : Integer;
  P : TProperty;
  A : TClassDecl;

begin
  Inherited Assign(AObject);
  A := AObject As TClassDecl;
  For i := 0 To A.PropertyCount - 1 Do
    Begin
      P := TProperty.Create('', scPrivate, 0, 0);
      P.Assign(A.Properties[i]);
      AddProperty(P);
    End;
end;

(**

  This method returns a string represenmtation of the classes declaration
  excluding fields, properties, and methods.

  @precon  ShowFirstToken tells the method to show the first token in the
           resultant string.
  @postcon Returns a string representation of the object declaration.

  @param   ShowFirstToken as a Boolean
  @return  a String

**)
Function TClassDecl.AsString(ShowFirstToken : Boolean) : String;

Begin
  Result := Identifier + ' = Class';
  If AbstractClass Then
    Result := Result + ' Abstract';
  If Heritage.Count > 0 Then
    Result := Result + '(' + Heritage.AsString + ')';
End;

(**

  This is the constructor method for the TClassDecl class. This initialise the
  classes identifier, scope and line and col information. It also creates
  collections for storing parameter (fields), properties and methods.

  @precon  None.
  @postcon This initialise the classes identifier, scope and line and col
           information.

**)
constructor TClassDecl.Create;
begin
  Inherited Create;
  FProperty := TObjectList.Create(True);
end;

(**

  This is the destructor method for the TClassDecl class. It frees the fields,
  properties and methods collections and in turns al the fields (parameters),
  properties and methods held by the class.

  @precon  None.
  @postcon It frees the fields, properties and methods collections and in turns
           al the fields (parameters), properties and methods held by the class.

**)
destructor TClassDecl.Destroy;
begin
  FProperty.Free;
  inherited Destroy;
end;

(**

  This is a getter method for the Property array property.

  @precon  iIndex is the index of the property required.
  @postcon Returns the property object requested.

  @param   iIndex as an Integer
  @return  a TProperty

**)
function TClassDecl.GetProperty(iIndex: Integer): TProperty;
begin
  Result := FProperty[iIndex] As TProperty;
end;

(**

  This is a getter method for the PropertyCount property.

  @precon  None.
  @postcon Returns the number of properties in the class.

  @return  an Integer

**)
function TClassDecl.GetPropertyCount: Integer;
begin
  Result := FProperty.Count;
end;

(**

  This method corts the classes property list.

  @precon  None.
  @postcon Sorts the proerty list.

**)
procedure TClassDecl.Sort;
begin
  inherited;
  FProperty.Sort(SortClassDecl);
end;

(**

  This method is a virtual assign procedure so that the properties of a
  TInterfaceDecl can be assigned to another instance of the same class.

  @precon  AObject is the object declaration to get information from.
  @postcon Virtual assign procedure so that the properties of a TInterfaceDecl
           can be assigned to another instance of the same class.

  @param   AObject as a TObjectDecl

**)
procedure TInterfaceDecl.Assign(AObject: TObjectDecl);
begin
  inherited Assign(AObject);
end;

(**

  This method returns a string representation of the interface excluding
  properties and methods.

  @precon  ShowFirstToken tells the method to show the first token in the
           resultant string.
  @postcon Returns a string representation of the object declaration.

  @param   ShowFirstToken as a Boolean
  @return  a String

**)
Function TInterfaceDecl.AsString(ShowFirstToken : Boolean) : String;

Begin
  Result := Identifier + ' = Interface';
  If Heritage.Count > 0 Then
    Result := Result + '(' + Heritage.AsString + ')';
End;

(**

  This is a setter method for the GUID property.

  @precon  Value is the value to be assign to the GUID property.
  @postcon Setst the GUID property.

  @param   Value as a String

**)
procedure TInterfaceDecl.SetGUID(Value : String);

Begin
  If FGUID <> Value Then
    FGUID := Value;
End;

{ TDispInterfaceDecl }

(**

  This method returns a string representation if a dispinterface.

  @precon ShowFirstToken identifies if the first token it to be displayed.
  @postcon Returns a string representation of the dispiterface declaration

  @param   ShowFirstToken as a Boolean
  @return  a String

**)
function TDispInterfaceDecl.AsString(ShowFirstToken: Boolean): String;
begin
  Result := Identifier + ' = DispInterface';
  If Heritage.Count > 0 Then
    Result := Result + '(' + Heritage.AsString + ')';
end;

(**

  This is the constructor method for the TDocError class.

  @precon  strMsg is the error message to create a doc error for, iLine is the
           line number of the error, iCol is the column number for the message,
           strExceptionMethod is the name of the method the exception occurred
           in and ErrType determines if the mesage is a warning or an error.
  @postcon Initialises the class.

  @param   strMsg             as a String
  @param   iLine              as an Integer
  @param   iCol               as an Integer
  @param   strExceptionMethod as a String
  @param   ErrType            as a TErrorType

**)
constructor TDocError.Create(strMsg: String; iLine, iCol: Integer;
  strExceptionMethod : String; ErrType : TErrorType);

begin
  FMsg := strMsg;
  FLine := iLine;
  FCol := iCol;
  FExceptionMethod := strExceptionMethod;
  ErrorType := ErrType;
End;

{ TDocErrorCollection }

(**

  This method adds an error to the error collection.

  @precon  strMsg is the error message to add to the collection, iLine is the
           line number of the error, iCol is the column number for the message.
           strExceptionMethod is the name of the method the exception occurred
           in and ErrType determines if the message is a warning or an error.
  @postcon Adds an error to the error collection.

  @param   strMsg        as a String
  @param   iLine         as an Integer
  @param   iCol          as an Integer
  @param   strMethodName as a String
  @param   ErrType       as a TErrorType

**)
procedure TDocErrorCollection.Add(strMsg: String; iLine, iCol: Integer;
  strMethodName : String; ErrType : TErrorType);
begin
  FErrors.Add(TDocError.Create(strMsg, iLine, iCol, strMethodName, ErrType));
end;

(**

  This is the constructor method for the TDocErrorCollection class.

  @precon  None.
  @postcon Creates the collection.

**)
constructor TDocErrorCollection.Create;
begin
  FErrors := TObjectList.Create(True);
end;

(**

  This is the destructor method for the TDocErrorCollection class.

  @precon  None.
  @postcon Destroy the collection.

**)
destructor TDocErrorCollection.Destroy;
begin
  FErrors.Free;
  inherited;
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of errors in the collection.

  @return  an Integer

**)
function TDocErrorCollection.GetCount: Integer;
begin
  Result := FErrors.Count;
end;

(**

  This is a getter method for the Error property.

  @precon  iIndex is the index of the error required.
  @postcon Returns the doc error object requested/

  @param   iIndex as an Integer
  @return  a TDocError

**)
function TDocErrorCollection.GetError(iIndex: Integer): TDocError;
begin
  Result := FErrors[iIndex] As TDocError;
end;

(**

  This is the constructor method for the EDocException class.

  @precon  strMsg is the text message of the exception, Token is the token where
           the error occurred and strMethodName is the name of the method where
           the exception occurred.
  @postcon Creates the exception.

  @param   strMsg        as a String
  @param   Token         as a TTokenInfo
  @param   strMethodName as a String

**)
Constructor EDocException.CreateNormal(strMsg : String; Token : TTokenInfo;
  strMethodName : String);

Begin
  Inherited Create(Format(strMsg, [Token.Token, Token.Line, Token.Column]));
  FLine := Token.Line;
  FCol := Token.Column;
  FExceptionMethod := strMethodName;
End;

(**

  This is the constructor method for the EDocException class.

  @precon  strMsg is the text message of the exception, strLiteral is the
           literal text that was expected when the exception occurred, Token is
           the token where the error occurred and strMethodName is the name of
           the method where the exception occurred.
  @postcon Creates the exception.

  @param   strMsg        as a String
  @param   strLiteral    as a String
  @param   Token         as a TTokenInfo
  @param   strMethodName as a String

**)
Constructor EDocException.CreateLiteral(strMsg, strLiteral : String;
  Token : TTokenInfo; strMethodName : String);

Begin
  Inherited Create(Format(strMsg, [strLiteral, Token.Token, Token.Line,
    Token.Column]));
  FLine := Token.Line;
  FCol := Token.Column;
  FExceptionMethod := strMethodName;
End;

{ TDocumentConflict }

(**

  This is the constructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Initialises the Conflict class.

  @param   Args            as an Array Of TVarRec constant
  @param   iIdentLine      as an Integer
  @param   iIdentColumn    as an Integer
  @param   iCommentLine    as an Integer
  @param   iCommentCol     as an Integer
  @param   DocConflictType as a TDocConflictType

**)
constructor TDocumentConflict.Create(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      DocConflictType : TDocConflictType);
begin
  FMessage := Format(DocConflictInfo[DocConflictType].MessageMask , Args);
  FIdentLine := iIdentLine;
  FIdentColumn := iIdentColumn;
  FCommentLine := iCommentLine;
  FCommentColumn := iCommentCol;
  FDocConflictType := DocConflictType;
end;

(**

  This is a getter method for the Category property.

  @precon  None.
  @postcon Returns the category of the document conflict.

  @return  a String

**)
function TDocumentConflict.GetCategory: String;
begin
  Result := DocConflictInfo[DocConflictType].Category;
end;

(**

  This is a getter method for the Description property.

  @precon  None.
  @postcon Returns a description of the document conflict.

  @return  a String

**)
function TDocumentConflict.GetDescription: String;
begin
  Result := DocConflictInfo[DocConflictType].Description;
end;

End.
