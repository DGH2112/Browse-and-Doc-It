(**

  This module contains the base class for all language module to derived from
  and all standard constants across which all language modules have in common.

  @Date    06 Aug 2008
  @Version 1.0
  @Author  David Hoyle

**)

Unit BaseLanguageModule;

Interface
Uses
  SysUtils, Classes, Contnrs;

Type

  (** Type to distinguish Stream position from token index. **)
  TStreamPosition = Integer;
  (** Type to distinguish Stream position from token index. **)
  TTokenIndex = Integer;
  (** An enumerate type to define the stream status and token types. **)
  TTokenType = (ttUnknown, ttWhiteSpace, ttReservedWord, ttIdentifier, ttNumber,
    ttSymbol, ttLineEnd, ttArrayElement, ttStatementEnd, ttStringLiteral,
    ttComment, ttHTMLTag, ttDirective, ttCompilerDirective, ttLinkTag);
  (** An enumerate for the scoping of identifiers. **)
  TScope = (scNone, scGlobal, scLocal, scPrivate, scProtected, scPublic, scPublished);
  (** A set to represent combinations of scopes. **)
  TScopes = Set Of TScope;
  (** An enumerate for the parameter modifiers of methods. **)
  TParamModifier = (pamNone, pamVar, pamConst, pamOut);
  (** An enumerate for the types of modules that can be parsed. **)
  TModuleType = (mtProgram, mtPackage, mtLibrary, mtUnit);
  (** An enumerate for the different methods. **)
  TMethodType = (mtConstructor, mtDestructor, mtProcedure, mtFunction);
  (** An enumerate for warning and errors. **)
  TErrorType = (etWarning, etError);

  (** This is a list of options valable for the display of module information
      **)
  TDocOption = (
    doCustomDrawing,
    doShowCommentHints,
    doShowLocals,
    doShowPrivates,
    doShowProtecteds,
    doShowPublics,
    doShowPublisheds,
    doShowConflicts,
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
    doShowMissingInitComment,
    doShowMissingFinalComment
  );

  (** An enumerate to associate images with different types of Elements. **)
  TImageIndex = (
    iiNone,

    iiModule,

    iiDocConflictFolder,
    iiDocConflictIncorrect,
    iiDocConflictItem,
    iiDocConflictMissing,

    iiErrorFolder,
    iiError,
    iiWarning,

    iiUsesLabel,
    iiUsesItem,

    iiTypesLabel,
    iiPublicType,
    iiPrivateType,
    iiPublishedType,
    iiProtectedType,
    iiLocalType,

    iiRecordsLabel,
    iiPublicRecord,
    iiPrivateRecord,
    iiPublishedRecord,
    iiProtectedRecord,
    iiLocalRecord,

    iiFieldsLabel,
    iiPublicField,
    iiPrivateField,
    iiPublishedField,
    iiProtectedField,

    iiObjectsLabel,
    iiPublicObject,
    iiPrivateObject,
    iiPublishedObject,
    iiProtectedObject,
    iiLocalObject,

    iiPublicConstructor,
    iiPrivateConstructor,
    iiPublishedConstructor,
    iiProtectedConstructor,
    iiLocalConstructor,

    iiPublicDestructor,
    iiPrivateDestructor,
    iiPublishedDestructor,
    iiProtectedDestructor,
    iiLocalDestructor,

    iiPublicProcedure,
    iiPrivateProcedure,
    iiPublishedProcedure,
    iiProtectedProcedure,
    iiLocalProcedure,

    iiPublicFunction,
    iiPrivateFunction,
    iiPublishedFunction,
    iiProtectedFunction,
    iiLocalFunction,

    iiClassesLabel,
    iiPublicClass,
    iiPrivateClass,
    iiPublishedClass,
    iiProtectedClass,
    iiLocalClass,

    iiPropertiesLabel,
    iiPublicProperty,
    iiPrivateProperty,
    iiPublishedProperty,
    iiProtectedProperty,
    iiLocalProperty,

    iiInterfacesLabel,
    iiPublicInterface,
    iiPrivateInterface,
    iiPublishedInterface,
    iiProtectedInterface,
    iiLocalInterface,

    iiDispInterfacesLabel,
    iiPublicDispInterface,
    iiPrivateDispInterface,
    iiPublishedDispInterface,
    iiProtectedDispInterface,
    iiLocalDispInterface,

    iiConstantsLabel,
    iiPublicConstant,
    iiPrivateConstant,
    iiPublishedConstant,
    iiProtectedConstant,
    iiLocalConstant,

    iiResourceStringsLabel,
    iiPublicResourceString,
    iiPrivateResourceString,
    iiPublishedResourceString,
    iiProtectedResourceString,
    iiLocalResourceString,

    iiVariablesLabel,
    iiPublicVariable,
    iiPrivateVariable,
    iiPublishedVariable,
    iiProtectedVariable,
    iiLocalVariable,

    iiThreadVarsLabel,
    iiPublicThreadVar,
    iiPrivateThreadVar,
    iiPublishedThreadVar,
    iiProtectedThreadVar,
    iiLocalThreadVar,

    iiExportedHeadingsLabel,

    iiExportedFunctionsLabel,
    iiPublicExportedFunction,
    iiPrivateExportedFunction,
    iiPublishedExportedFunction,
    iiProtectedExportedFunction,
    iiLocalExportedFunction,

    iiLabelsLabel,
    iiPublicLabel,

    iiImplementedMethods,

    iiInitialization,
    iiFinalization,

    iiToDoFolder,
    iiToDoItem,

    iiUnknownClsObj
  );

  (** This is a set of display options. **)
  TDocOptions = Set of TDocOption;

  (** This is an enumerate to define the options for the parsing of a module. **)
  TModuleOption = (moParse, moCheckForDocumentConflicts);
  (** This is a set of Module Option enumerates. **)
  TModuleOptions = Set Of TModuleOption;

  (** A type to define the position before a token of the comment to be
      associated with the identifier. **)
  TCommentPosition = (cpBeforeCurrentToken, cpBeforePreviousToken);

  (** An enumerate to define the type of conflict and hence icon. **)
  TDocConflictIcon = (dciItem, dciIncorrect, dciMissing);

  (** An enumerate to index document conflict information. **)
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

    dctClassClauseUndocumented,

    dctPropertyUndocumented,
    dctPropertyHasNoDesc,
    dctPropertyPreconNotDocumented,
    dctPropertyDiffParamCount,
    dctPropertyMissingPreCon,
    dctPropertyTooManyPreCons,
    dctPropertyUndocumentedParam,
    dctPropertyIncorrectParamType,
    dctPropertyPostconNotDocumented,
    dctPropertyUndocumentedReturn,
    dctPropertyIncorrectReturnType,
    dctPropertyMissingPostCon,
    dctPropertyTooManyPostCons,

    dctRecordClauseUndocumented,

    dctObjectClauseUndocumented,

    dctInterfaceClauseUndocumented,

    dctDispInterfaceClauseUndocumented,

    dctMethodUndocumented,
    dctMethodHasNoDesc,
    dctMethodPreconNotDocumented,
    dctMethodDiffParamCount,
    dctMethodMissingPreCon,
    dctMethodTooManyPrecons,
    dctMethodUndocumentedParam,
    dctMethodIncorrectParamType,
    dctMethodPostconNotDocumented,
    dctMethodUndocumentedReturn,
    dctMethodIncorrectReturntype,
    dctMethodMissingPostCon,
    dctMethodTooManyPostCons,

    dctMissingInitComment,
    dctMissingFinalComment
  );

  (** This record refined a pairing of Resource Name and Image Mask Colour for
      the imported images from the Executable File associated with the
      Image Index enumerate. **)
  TImageIndexInfo = Record
    FResourcename : String;
    FMaskColour : Integer;
  End;

  (** This is a record that contains the description and the default for a
      TDocOption enumerate. **)
  TDocOptionRec = Record
    FDescription : String;
    FEnabled : Boolean;
  End;

  (** A record type to contain a token and its line and column in the editor. **)
  TIdentInfo = Record
    FIdent : String;
    FLine : Integer;
    FCol : Integer;
    FScope : TScope;
  End;

  (** This is a record to describe the position of a token in the editor. **)
  TTokenPosition = Record
    FLine : Integer;
    FColumn : Integer;
    FBufferPos : Integer;
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
    Procedure Append(strToken : String);
    (**
      Returns the token as a string.
      @precon  None.
      @postcon Returns the token as a string.
      @return  a String
    **)
    Property Token : String read FToken;
    (**
      Returns the uppercase version of the token. Used for keyword comparisons.
      @precon  None.
      @postcon Returns the uppercase version of the token. Used for keyword comparisons.
      @return  a String
    **)
    Property UToken : String read FUToken;
    (**
      Returns the buffer position of the token start point.
      @precon  None.
      @postcon Returns the buffer position of the token start point.
      @return  an Integer
    **)
    Property BufferPos : Integer read FBufferPos;
    (**
      Returns the line number of the token start point.
      @precon  None.
      @postcon Returns the line number of the token start point.
      @return  an Integer
    **)
    Property Line : Integer read FLine;
    (**
      Returns the column number of the token start point.
      @precon  None.
      @postcon Returns the column number of the token start point.
      @return  an Integer
    **)
    Property Column : Integer read FColumn;
    (**
      Returns the length of the token.
      @precon  None.
      @postcon Returns the length of the token.
      @return  an Integer
    **)
    Property Length : Integer read FLength;
    (**
      Returns the token type for the token.
      @precon  None.
      @postcon Returns the token type for the token.
      @return  a TTokenType
    **)
    Property TokenType : TTokenType read FTokenType;
  End;

  (** A class to hold text about a single tag **)
  TTag = Class
  Private
    FTokens : TStringList;
    FTagName: String;
    FLine: Integer;
    FColumn: Integer;
  Protected
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
      @precon  None.
      @postcon Returns the tag name as a string.
      @return  a String
    **)
    Property TagName : String read FTagName write SetTagName;
    (**
      Returns the specifically index token from the tags token collection.
      @precon  iTokenIndex must be a valid index between 0 and TokenCount - 1.
      @postcon Returns the specifically index token from the tags token collection.
      @param   iTokenIndex as       an Integer
      @return  a String
    **)
    Property Token[iTokenIndex : Integer] : String read GetToken; Default;
    (**
      Returns the specifically index tokens type from the tags token collection.
      @precon  iTokenIndex must be a valid index between 0 and TokenCount - 1.
      @postcon Returns the specifically index tokens type from the tags token collection.
      @param   iTokenIndex as       an Integer
      @return  a TTokenType
    **)
    Property TokenType[iTokenIndex : Integer] : TTokenType read GetTokenType;
    (**
      Returns the number of token in the tag.
      @precon  None.
      @postcon Returns the number of token in the tag.
      @return  an Integer
    **)
    Property TokenCount : Integer read GetTokenCount;
    (**
      Returns the line number of the tag.
      @precon  None.
      @postcon Returns the line number of the tag.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column position of the tag.
      @precon  None.
      @postcon Returns the column position of the tag.
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
  Protected
    function GetTag(iTagIndex: Integer): TTag;
    function GetTagCount: Integer;
    function GetToken(iTokenIndex: Integer): String;
    function GetTokenCount: Integer;
    function GetTokenType(iTokenIndex: Integer): TTokenType;
    Function GetLine : Integer;
    Function GetCol : Integer;
  Public
    Constructor Create(srcComment : TComment); Overload;
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
      @precon  iTokenIndex must a valid index between 0 and TokenCount - 1.
      @postcon Returns the specifically indexed token from the collection.
      @param   iTokenIndex as       an Integer
      @return  a String
    **)
    Property Token[iTokenIndex : Integer] : String read GetToken; Default;
    (**
      Returns the number of tokens found in the comment.
      @precon  None.
      @postcon Returns the number of tokens found in the comment.
      @return  an Integer
    **)
    Property TokenCount : Integer read GetTokenCount;
    (**
      Returns the specifically indexed tokens type from the token collection.
      @precon  iTokenIndex must be a valid index between 0 and TokenCount - 1.
      @postcon Returns the specifically indexed tokens type from the token collection.
      @param   iTokenIndex as       an Integer
      @return  a TTokenType
    **)
    Property TokenType[iTokenIndex : Integer] : TTokenType read GetTokenType;
    (**
      Returns the specifically indexed tag from the comments tag collection.
      @precon  iTagIndex must eb a valid index between 0 and TagCount - 1.
      @postcon Returns the specifically indexed tag from the comments tag collection.
      @param   iTagIndex as       an Integer
      @return  a TTag
    **)
    Property Tag[iTagIndex : Integer] : TTag Read GetTag;
    (**
      Returns the number of tags found within the comment.
      @precon  None.
      @postcon Returns the number of tags found within the comment.
      @return  an Integer
    **)
    Property TagCount : Integer Read GetTagCount;
    (**
      Returns the line number of the comment.
      @precon  None.
      @postcon Returns the line number of the comment.
      @return  an Integer
    **)
    Property Line : Integer Read GetLine;
    (**
      Returns the column number of the comment.
      @precon  None.
      @postcon Returns the column number of the comment.
      @return  an Integer
    **)
    Property Col : Integer Read GetCol;
  End;

  (** A record to describe document conflict information. **)
  TDocConflictTable = Record
    FCategory     : String;
    FMessage      : String;
    FDescription  : String;
    FConflictType : TDocConflictIcon;
  End;

  (** This is a class reference for the TElementContainer class such that the
      descendant classes can clone themselves. **)
  TElementContainerClass = Class Of TElementContainer;

  (** This class implements the IElementCollection interface so that this
      element container can be rendered with the module browser. **)
  TElementContainer = Class {$IFDEF VER180} Abstract {$ENDIF}
  Private
    FElements : TObjectList;
    FTokens : TObjectList;
    FName : String;
    FLine : Integer;
    FColumn : Integer;
    FComment : TComment;
    FScope : TScope;
    FImageIndex : TImageIndex;
    FSorted  : Boolean;
  Protected
    Function GetElementCount : Integer;
    Function GetElements(iIndex : Integer) : TElementContainer;
    Function GetTokenCount : Integer;
    Function GetTokens(iIndex : Integer) : TTokenInfo;
    Function GetAsString : String; Virtual;
    Function GetImageIndexAdjustedForScope : Integer;
    Function Find(strName : String) : Integer;
    Function GetName: String; Virtual;
    Procedure SetSorted(boolValue : Boolean);
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Virtual;
    Destructor Destroy; Override;
    Function Add(AElement : TElementContainer) : TElementContainer; Overload; Virtual;
    Function Add(Token : TTokenInfo; AScope : TScope; AImageIndex : TImageIndex;
      AComment : TComment) : TElementContainer; Overload; Virtual;
    Function Add(strToken : String; AImageIndex : TImageIndex;
      AComment : TComment) : TElementContainer; Overload; Virtual;
    Procedure AppendToken(AToken : TTokenInfo); Overload; Virtual;
    Procedure AppendToken(strToken : String); Overload; Virtual;
    Procedure AddTokens(AElement : TElementContainer); Virtual;
    Function FindElement(strName : String) : TElementContainer;
    Procedure Assign(Source : TElementContainer); Virtual;
    Function FindToken(strToken : String) : Integer;
    Procedure DeleteElement(iIndex : Integer);
    Procedure CheckDocumentation(var boolCascade : Boolean); Virtual;
    Procedure AddDocumentConflict(Const Args: Array of TVarRec;
      iIdentLine, iIdentColumn : Integer; AComment : TComment;
      DocConflictRec : TDocConflictTable);
    (**
      This property returns the number of elements in the collection.
      @precon  None.
      @postcon Returns the number of elements in the collection.
      @return  an Integer
    **)
    Property ElementCount : Integer Read GetElementCount;
    (**
      This property returns an instance of the indexed token from the
      collection.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed token from the collection.
      @param   iIndex as       an Integer
      @return  a TTokenInfo
    **)
    Property Tokens[iIndex : Integer] : TTokenInfo Read GetTokens;
    (**
      This property returns the number of tokens in the collection.
      @precon  None.
      @postcon Returns the number of tokens in the collection.
      @return  an Integer
    **)
    Property TokenCount : Integer Read GetTokenCount;
    (**
      This property returns an instance of the indexed element from the
      collection.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed element from the collection.
      @param   iIndex as       an Integer
      @return  a TElementContainer
    **)
    Property Elements[iIndex : Integer] : TElementContainer Read GetElements; Default;
    (**
      This property returns the name of the element.
      @precon  None.
      @postcon Returns the name of the element.
      @return  a String
    **)
    Property Name : String Read GetName;
    (**
      This property returns the identifier name (same as name) of the element.
      @precon  None.
      @postcon Returns the identifier name (same as name) of the element.
      @return  a String
    **)
    Property Identifier : String read FName Write FName;
    (**
      This property returns the line number associated with this element.
      @precon  None.
      @postcon Returns the line number associated with this element.
      @return  an Integer
    **)
    Property Line : Integer Read FLine Write FLine;
    (**
      This property returns the column number associated with this element.
      @precon  None.
      @postcon Returns the column number associated with this element.
      @return  an Integer
    **)
    Property Column : Integer Read FColumn Write FColumn;
    (**
      This property returns the comment that is associated with this element.
      @precon  None.
      @postcon Returns the comment that is associated with this element.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      This property returns a string representation of the element.
      @precon  None.
      @postcon Returns a string representation of the element.
      @return  a String
    **)
    Property AsString : String Read GetAsString;
    (**
      This property returns the Scope of the element.
      @precon  None.
      @postcon Returns the Scope of the element.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
    (**
      This property returns the Image Index of the element.
      @precon  None.
      @postcon Returns the Image Index of the element.
      @return  a TImageIndex
    **)
    Property ImageIndex : TImageIndex Read FImageIndex Write FImageIndex;
    (**
      This property returns the image index associated with the element based on
      its Image Index and Scope.
      @precon  None.
      @postcon Returns the image index associated with the element based on
               its Image Index and Scope.
      @return  an Integer
    **)
    Property ImageIndexAdjustedForScope : Integer Read GetImageIndexAdjustedForScope;
    (**
      This property determines if the collection is sorted or sequential.
      @precon  None.
      @postcon Sets or gets whether the collection is sorted.
      @return  a Boolean
    **)
    Property Sorted : Boolean Read FSorted Write SetSorted;
  End;

  (** This class represents a single identifier with line, col and comment
      attributes. **)
  TIdent = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer);

  (** This class represents a list of identifiers **)
  TIdentList = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer);

  (** This is a sub class for all types **)
  TGenericTypeDecl = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a sub class for all constants **)
  TGenericConstant = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a sub class for all variables **)
  TGenericVariable = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This class represents a parameter of a method declaration. **)
  TGenericParameter = Class (TElementContainer)
  Private
    FParamModifier : TParamModifier;
    FArrayOf : Boolean;
    FParamType : TGenericTypeDecl;
    FDefaultValue : String;
  Public
    Constructor Create(ParamMod : TParamModifier; Ident : String;
      boolArrayOf : Boolean; AType : TGenericTypeDecl; Value : String;
      AScope : TScope; iLine, iCol : Integer); ReIntroduce; Overload;
    Destructor Destroy; Override;
    Function ParamReturn : String;
    (**
      Returns the parameter modifier : const, var or out.
      @precon  None.
      @postcon Returns the parameter modifier : const, var or out.
      @return  a TParamModifier
    **)
    Property ParamModifier : TParamModifier Read FParamModifier;
    (**
      Returns whether the parameter is an array parameter.
      @precon  None.
      @postcon Returns whether the parameter is an array parameter.
      @return  a Boolean
    **)
    Property ArrayOf : Boolean Read FArrayOf;
    (**
      Returns the parameter type identifier for the parameter.
      @precon  None.
      @postcon Returns the parameter type identifier for the parameter.
      @return  a TgenericTypeDecl
    **)
    Property ParamType : TGenericTypeDecl Read FParamType;
    (**
      Returns the default value of the parameter is there is one.
      @precon  None.
      @postcon Returns the default value of the parameter is there is one.
      @return  a String
    **)
    Property DefaultValue : String Read FDefaultValue;
    (**
      Returns the parameters scope with in the record / object / class etc.
      @precon  None.
      @postcon Returns the parameters scope with in the record / object / class etc.
      @return  a TScope
    **)
  End;

  (** This class represents a method declaration. **)
  TGenericMethodDecl = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer)
  Private
    FParameters : TObjectList;
    FMethodType : TMethodType;
    FClsName : String;
    FReturnType : TGenericTypeDecl;
    FMsg: String;
    FExt: String;
    FClassMethod : Boolean;
    FAlias: String;
    FForwardDecl : Boolean;
  Protected
    Procedure SetClsName(Value : String);
    procedure SetMsg(const Value: String);
    procedure SetExt(const Value: String);
    Function GetQualifiedName : String;
    Function GetParameterCount : Integer;
    Function GetParameters(iIndex : Integer) : TGenericParameter;
    procedure CheckMethodDocumentation;
    procedure CheckMethodParamCount;
    Procedure CheckMethodParameters;
    Procedure CheckMethodReturns;
  Public
    Constructor Create(MethodType : TMethodType; strName : String; AScope : TScope;
      iLine, iCol : Integer); ReIntroduce; Virtual;
    Destructor Destroy; Override;
    Procedure AddParameter(AParameter : TGenericParameter);
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    (**
      Returns the methods types, procedure, function, constructor, destructor.
      @precon  None.
      @postcon Returns the methods types, procedure, function, constructor, destructor.
      @return  a TMethodType
    **)
    Property MethodType : TMethodType Read FMethodType;
    (**
      Returns the methods class name.
      @precon  None.
      @postcon Returns the methods class name.
      @return  a String
    **)
    Property ClsName : String Read FClsName Write SetClsName;
    (**
      This property returns the number of parameter in the parameter collection.
      @precon  None.
      @postcon Returns the number of parameter in the parameter collection.
      @return  an Integer
    **)
    Property ParameterCount : Integer Read GetParameterCount;
    (**
      This property returns an instance of the indexed parameter.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed parameter.
      @param   iIndex as       an Integer
      @return  a TGenericParameter
    **)
    Property Parameters[iIndex : Integer] : TGenericParameter Read GetParameters;
    (**
      This property returns the type corresponding to the method return.
      @precon  None.
      @postcon Returns the type corresponding to the method return.
      @return  a TGenericTypeDecl
    **)
    Property ReturnType : TGenericTypeDecl Read FReturnType Write FReturnType;
    (**
      Returns the associated message for the method if the method is a message
      @precon  None.
      @postcon Returns the associated message for the method if the method is a message
      handler.
      @return  a String
    **)
    Property Msg : String Read FMsg Write SetMsg;
    (**
      Returns the external reference for the method if there is one.
      @precon  None.
      @postcon Returns the external reference for the method if there is one.
      @return  a String
    **)
    Property Ext : String Read FExt Write SetExt;
    (**
      Return whether the method is a class method.
      @precon  None.
      @postcon Return whether the method is a class method.
      @return  a Boolean
    **)
    Property ClassMethod : Boolean Read FClassMethod Write FClassMethod;
    (**
      Returns the Qualified name of the method.
      @precon  None.
      @postcon Returns the Qualified name of the method.
      @return  a String
    **)
    Property QualifiedName : String Read GetQualifiedName;
    (**
      Returns the method alias name.
      @precon  None.
      @postcon Returns the method alias name.
      @return  a String
    **)
    Property Alias : String Read FAlias Write FAlias;
    (**
      This property returns whether the method is a forward declaration or not.
      @precon  None.
      @postcon Returns whether the method is a forward declaration or not.
      @return  a Boolean
    **)
    Property ForwardDecl : Boolean Read FForwardDecl Write FForwardDecl;
  End;

  (** This is a class that represents properties of a class or interface. **)
  TGenericProperty = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer)
  Private
    FParameters : TObjectList;
    FTypeID : TGenericTypeDecl;
  Protected
    Function GetParameterCount : Integer;
    Function GetParameters(iIndex : Integer) : TGenericParameter;
    procedure CheckPropertyDocumentation;
    procedure CheckPropertyParamCount;
    procedure CheckPropertyParameters;
    procedure CheckPropertyReturns;
  Public
    Constructor Create(strIdent : String; AScope : TScope; iLine, iCol : Integer;
      AImageIndex : TImageIndex; AComment : TComment); Override;
    Destructor Destroy; Override;
    Procedure AddParameter(AParameter : TGenericParameter);
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    (**
      This property returns the number of parameter in the parameter collection.
      @precon  None.
      @postcon Returns the number of parameter in the parameter collection.
      @return  an Integer
    **)
    Property ParameterCount : Integer Read GetParameterCount;
    (**
      This property returns an instance of the indexed parameter.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed parameter.
      @param   iIndex as       an Integer
      @return  a TGenericParameter
    **)
    Property Parameters[iIndex : Integer] : TGenericParameter Read GetParameters;
    (**
      Returns the type identifier of the property.
      @precon  None.
      @postcon Returns the type identifier of the property.
      @return  a TGenericTypeDecl
    **)
    Property TypeId : TGenericTypeDecl Read FTypeID Write FTypeID;
  End;

  (** This class defines a parsing error. **)
  TDocError = Class(TElementContainer)
  Private
    FMsg: String;
    FErrorType : TErrorType;
    FMethod : String;
  Protected
    Function GetAsString : String; Override;
  Public
    Constructor Create(strMsg : String; AScope : TScope; strMethod : String; iLine,
      iCol : Integer; ErrType : TErrorType); Reintroduce; Overload;
    (**
      Returns the exception message.
      @precon  None.
      @postcon Returns the exception message.
      @return  a String
    **)
    Property Msg : String Read FMsg;
    (**
      Returns the type of exception stored.
      @precon  None.
      @postcon Returns the type of exception stored.
      @return  a TErrorType
    **)
    Property ErrorType : TErrorType Read FErrorType;
    (**
      Returns the exception method of the exception stored.
      @precon  None.
      @postcon Returns the exception method of the exception stored.
      @return  a String
    **)
    Property Method : String Read FMethod;
  End;

  (**

    This is an augmented Exception class to hold information about the
    line number and column number of the error.

  **)
  EDocException = Class(TElementContainer)
  Private
    FExceptionMethod: String;
  Protected
    function GetMessage: String;
  Public
    Constructor Create(strMsg : String; Token : TTokenInfo;
      strMethodName : String); ReIntroduce; Overload;
    Constructor Create(strMsg, strLiteral : String; Token : TTokenInfo;
      strMethodName : String); ReIntroduce; Overload;
    Constructor Create(strMsg : String; strMethodName : String); ReIntroduce;
      Overload;
    (**
      Returns a string representing the classes ExceptionMethod.
      @precon  None.
      @postcon Returns a string representing the classes ExceptionMethod.
      @return  a String
    **)
    Property ExceptionMethod : String Read FExceptionMethod;
    (**
      This property returns the message of the Document Error as a String.
      @precon  None.
      @postcon Returns the message of the Document Error as a String.
      @return  a String
    **)
    Property Message : String Read GetMessage;
  End;

  (** This is a class to represent a module documentation conflict. **)
  TDocumentConflict = Class(TElementContainer)
  Private
    FMessage       : String;
    FCommentLine   : Integer;
    FCommentColumn : Integer;
  Protected
    Function GetAsString : String; Override;
  Public
    Constructor Create(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      strDocConflictMsg, strDocConflictDesc : String;
      AImageIndex : TImageIndex); ReIntroduce;
    Destructor Destroy; Override;
    (**
      This property defines the line where the comment associated with the
      conflict starts.
      @precon  None.
      @postcon Return the line where the comment associated with the conflict
               starts.
      @return  an Integer
    **)
    Property CommentLine : Integer Read FCommentLine;
    (**
      This property defines the column where the comment associated with the
      conflict starts.
      @precon  None.
      @postcon Return the column where the comment associated with the conflict
               starts.
      @return  an Integer
    **)
    Property CommentColumn : Integer Read FCommentColumn;
  End;

  (** This is a type for a set of characters and the return type of several
      properties. **)
  TCharSet = Set of Char;

//  (** This is a type to define an array of string. **)
//  TArrayOfString = Array Of String;

  (** This is an abtract class from which all language modules should be
      derived. **)
  TBaseLanguageModule = Class {$IFDEF VER180} Abstract {$ENDIF} (TElementContainer)
  Private
    FOwnedItems : TObjectList;
    FTokens : TObjectList;
    FTokenIndex : TTokenIndex;
    FDocErrors: TElementContainer;
    FTickList : TStringList;
    FModuleName : String;
    FModuleType : TModuleType;
    FModuleComment : TComment;
    FBodyComment : TObjectList;
    FModuleNameCol: Integer;
    FModuleNameLine: Integer;
    FFileName: String;
    FModified : Boolean;
    FCompilerDefs : TStringList;
    FPreviousTokenIndex : TTokenIndex;
    FCompilerConditionStack : TList;
  Protected
    Function GetTokenCount : Integer;
    Function GetTokenInfo(iIndex : TTokenIndex) : TTokenInfo;
    Function GetToken : TTokenInfo;
    function GetOpTickCountName(iIndex: Integer): String;
    function GetOpTickCountByIndex(iIndex: Integer): Integer;
    function GetOpTickCounts: Integer;
    function GetOpTickCount(strStart, strFinish : String): Integer;
    Function GetBodyComment(iIndex : Integer) : TComment;
    Function GetBodyCommentCount : Integer;
    Function GetAsString : String; Override;
    Function PrevToken : TTokenInfo;
    Procedure NextToken;
    Function EndOfTokens : Boolean;
    Procedure NextNonCommentToken;
    Procedure RollBackToken;
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
    Procedure SetTokenIndex(iIndex : TTokenIndex);
    Procedure GetBodyCmt;
    Procedure AddToken(AToken : TTokenInfo);
    procedure AppendToLastToken(strToken : String);
    procedure ProcessCompilerDirective(var iSkip : Integer);
    Function GetModuleName : String; Virtual;
    (**
      Returns a refernce the to owned items collection. This is used to manage
      the life time of all the ident lists and comments found in the module.
      @precon  None.
      @postcon Returns a refernce the to owned items collection. This is used to
               manage the life time of all the ident lists and comments found in
               the module.
      @return  a TObjectList
    **)
    Property OwnedItems : TObjectList Read FOwnedItems;
    (**
      Returns the current token with in the module. Also see
      {@link TPascalDocModule.SetTokenIndex SetTokenIndex}.
      @precon  None.
      @postcon Returns the current token with in the module. Also see
               {@link TPascalDocModule.SetTokenIndex SetTokenIndex}.
      @return  a TTokenInfo
    **)
    Property Token : TTokenInfo Read GetToken;
    (**
      Returns a reference to the body comments collection.
      @precon  None.
      @postcon Returns a reference to the body comments collection.
      @return  a TObjectList
    **)
    Property BodyComments : TObjectList Read FBodyComment;
    (**
      This property provide access to a list of compiler defines as a string
      list.
      @precon  None.
      @postcon Provide a string list of compiler defines {$DEFINE xxxxx}
      @return  a TStringList
    **)
    Property CompilerDefines : TStringList Read FCompilerDefs;
  Public
    Constructor Create(IsModified : Boolean; strFileName : String); Reintroduce;
      Overload;
    Destructor Destroy; Override;
    Procedure AddTickCount(strLabel : String);
    Procedure AddDef(strDef : String);
    Procedure DeleteDef(strDef : String);
    Function IfDef(strDef : String) : Boolean;
    Function IfNotDef(strDef : String) : Boolean;
    Procedure AddError(Error : TElementContainer);
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    { Properties }
    (**
      Returns a reference to the modules error collection.
      @precon  None.
      @postcon Returns a reference to the modules error collection.
      @return  an Integer
    **)
    Property TokenCount : Integer Read GetTokenCount;
    (**
      Returns the token information for the specifically indexed token within
      the module.
      @precon  None.
      @postcon Returns the token information for the specifically indexed token within
      the module.
      @param   iIndex as       a TTokenIndex
      @return  a TTokenInfo
    **)
    Property TokenInfo[iIndex : TTokenIndex] : TTokenInfo Read GetTokenInfo;
    (**
      This property returns the tick count time between the 2 named tick counts
      previously stored using the AddTickCount() method.
      @precon  None.
      @postcon Returns the time between two counter if both the names are found.
      @param   strStart  as       a String
      @param   strFinish as       a String
      @return  an Integer
    **)
    Property OpTickCount[strStart, strFinish : String] : Integer Read GetOpTickCount;
    (**
      Thie property returns the number of operation tick counter storeed in the
      collection.
      @precon  None.
      @postcon Returns the number of operation tick counter storeed in the
               collection.
      @return  an Integer
    **)
    Property OpTickCounts : Integer Read GetOpTickCounts;
    (**
      This property returns the tick count associated with the indexed item.
      @precon  iIndex must be a valid index between 0 and OpTickCount - 1.
      @postcon Returns the tick count associated with the indexed item.
      @param   iIndex as       an Integer
      @return  an Integer
    **)
    Property OpTickCountByIndex[iIndex : Integer] : Integer Read GetOpTickCountByIndex;
    (**
      This property returns the name associated with the indexed item.
      @precon  iIndex must be a valid index between 0 and OpTickCount - 1.
      @postcon Returns the name associated with the indexed item.
      @param   iIndex as       an Integer
      @return  a String
    **)
    Property OpTickCountName[iIndex : Integer] : String Read GetOpTickCountName;
    (**
      Returns the module name as a string.
      @precon  None.
      @postcon Returns the module name as a string.
      @return  a String
    **)
    Property ModuleName : String Read GetModuleName Write FModuleName;
    (**
      Returns the type of the modules, Program, Unit, Package, etc.
      @precon  None.
      @postcon Returns the type of the modules, Program, Unit, Package, etc.
      @return  a TModuleType
    **)
    Property ModuleType : TModuleType Read FModuleType Write FModuleType;
    (**
      Returns a reference to the modules comment.
      @precon  None.
      @postcon Returns a reference to the modules comment.
      @return  a TComment
    **)
    Property ModuleComment : TComment Read FModuleComment Write FModuleComment;
    (**
      Returns the specific indexed body comment from the collection.
      @precon  None.
      @postcon Returns the specific indexed body comment from the collection.
      @param   iIndex as       an Integer
      @return  a TComment
    **)
    Property BodyComment[iIndex : Integer] : TComment Read GetBodyComment;
    (**
      Returns a reference to the modules body comments collection.
      @precon  None.
      @postcon Returns a reference to the modules body comments collection.
      @return  an Integer
    **)
    Property BodyCommentCount : Integer Read GetBodyCommentCount;
    (**
      Returns the line number of the modules name.
      @precon  None.
      @postcon Returns the line number of the modules name.
      @return  an Integer
    **)
    Property ModuleNameLine : Integer Read FModuleNameLine Write FModuleNameLine;
    (**
      Returns the column number of the module name.
      @precon  None.
      @postcon Returns the column number of the module name.
      @return  an Integer
    **)
    Property ModuleNameCol : Integer Read FModuleNameCol Write FModuleNameCol;
    (**
      This property returns the file name of the module as passed to the
      constructor.
      @precon  None.
      @postcon This property returns the file name of the module as passed to the
      constructor.
      @return  a String
    **)
    Property FileName : String Read FFileName;
    (**
      This property returns whether the source code is modified or not.
      @precon  None.
      @postcon This property returns whether the source code is modified or not.
      @return  a Boolean
    **)
    Property Modified : Boolean Read FModified;
  End;

  (** This is a class to define a set of options for the application. **)
  TBrowseAndDocItOptions = Class
  Private
    FOptions : TDocOptions;
    FDefines : TStringList;
  Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    (**
      This property contains the basic toggleable options for the application.
      @precon  None.
      @postcon Contains the basic toggleable options for the application.
      @return  a TDocOptions
    **)
    Property Options : TDocOptions Read FOptions Write FOptions;
    (**
      This property provides a list of Compiler Conditional Defines.
      @precon  None.
      @postcon Provides a list of Compiler Conditional Defines.
      @return  a TStringList
    **)
    Property Defines : TStringList Read FDefines;
  End;

ResourceString
  (** Options text for Draw Syntax Highlighted Module Explorer **)
  strDrawSynHighModuleExplorer = 'Draw Syntax Highlighted Module Explorer';
  (** Options text for Show comments in the hints **)
  strShowCommentsInTheHints = 'Show comments in the hints';
  (** Options text for Show local declarations in methods **)
  strShowLocalDeclarationsInMethods = 'Show local declarations in methods';
  (** Options text for Show private declarations **)
  strShowPrivateDeclarations = 'Show private declarations';
  (** Options text for Show protected declarations **)
  strShowProtectedDeclarations = 'Show protected declarations';
  (** Options text for Show public declarations **)
  strShowPublicDeclarations = 'Show public declarations';
  (** Options text for Show published declarations **)
  strShowPublishedDeclarations = 'Show published declarations';
  (** Options text for Show Documentation Conflicts **)
  strShowDocumentationConflicts = 'Show Documentation Conflicts';
  (** Options text for Show Missing Method Documentation **)
  strShowMissingMethodDocumentation = 'Show Missing Method Documentation';
  (** Options text for Show Missing Method Documentation Description **)
  strShowMissingMethodDocDesc = 'Show Missing Method Documentation Description';
  (** Options text for Show Different Method Parameter Count **)
  strShowDiffMethodParameterCount = 'Show Different Method Parameter Count';
  (** Options text for Show Undocumented Method Parameters **)
  strShowUndocumentedMethodParameters = 'Show Undocumented Method Parameters';
  (** Options text for Show Incorrect Method Parameter Type **)
  strShowIncorrectMethodParameterType = 'Show Incorrect Method Parameter Type';
  (** Options text for Show Undocumented Method Return **)
  strShowUndocumentedMethodReturn = 'Show Undocumented Method Return';
  (** Options text for Show Incorrect Method Return Type **)
  strShowIncorrectMethodReturnType = 'Show Incorrect Method Return Type';
  (** Options text for Show Undocumented Types **)
  strShowUndocumentedTypes = 'Show Undocumented Types';
  (** Options text for Show Undocumented Records **)
  strShowUndocumentedRecords = 'Show Undocumented Records';
  (** Options text for Show Undocumented Objects **)
  strShowUndocumentedObjects = 'Show Undocumented Objects';
  (** Options text for Show Undocumented Classes **)
  strShowUndocumentedClasses = 'Show Undocumented Classes';
  (** Options text for Show Undocumented Interfaces **)
  strShowUndocumentedInterfaces = 'Show Undocumented Interfaces';
  (** Options text for Show Undocumented Variables **)
  strShowUndocumentedVariables = 'Show Undocumented Variables';
  (** Options text for Show Undocumented Constants **)
  strShowUndocumentedConstants = 'Show Undocumented Constants';
  (** Options text for Show Undocumented Module **)
  strShowUndocumentedModule = 'Show Undocumented Module';
  (** Options text for Show Missing Module Date **)
  strShowMissingModuleDate = 'Show Missing Module Date';
  (** Options text for Show Check Module Date **)
  strShowCheckModuleDate = 'Show Check Module Date';
  (** Options text for Show Missing Module Version **)
  strShowMissingModuleVersion = 'Show Missing Module Version';
  (** Options text for Show Missing Module Author **)
  strShowMissingModuleAuthor = 'Show Missing Module Author';
  (** Options text for Show Missing Method Pre-Conditions **)
  strShowMissingMethodPreConditions = 'Show Missing Method Pre-Conditions';
  (** Options text for Show Missing Method Post-Conditions **)
  strShowMissingMethodPostConditions = 'Show Missing Method Post-Conditions';
  (** Options text for Show Missing Property Documentation **)
  strShowMissingPropertyDocumentation = 'Show Missing Property Documentation';
  (** Options text for Show Missing Property Documentation Description **)
  strShowMissingPropertyDocuDesc = 'Show Missing Property Documentation Description';
  (** Options text for Show Different Property Parameter Count **)
  strShowDiffPropertyParameterCount = 'Show Different Property Parameter Count';
  (** Options text for Show Undocumented Property Parameter **)
  strShowUndocumentedPropertyParameter = 'Show Undocumented Property Parameter';
  (** Options text for Show Incorrect Property Parameter Type **)
  strShowIncorrectPropertyParameterType = 'Show Incorrect Property Parameter Type';
  (** Options text for Show Undocumented Property Return Type **)
  strShowUndocumentedPropertyReturnType = 'Show Undocumented Property Return Type';
  (** Options text for Show Incorrect Property Return Type **)
  strShowIncorrectPropertyReturnType = 'Show Incorrect Property Return Type';
  (** Options text for Show Missing Property Pre-Conditions **)
  strShowMissingPropertyPreConditions = 'Show Missing Property Pre-Conditions';
  (** Options text for Show Missing Property Post-Conditions **)
  strShowMissingPropertyPostConditions = 'Show Missing Property Post-Conditions';
  (** Options text for Show Missing Initialization Comment **)
  strShowMissingInitComment = 'Show Missing Initialization Comments';
  (** Options text for Show Missing Finalization Comment **)
  strShowMissingFinalComment = 'Show Missing Finalization Comments';

  (** Label for Documentation Conflicts **)
  strDocumentationConflicts = 'Documentation Conflicts';
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
  (** Label for Labels **)
  strLabel = 'Labels';

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
  (** Exception message when an number is expected but something else is found. **)
  strNumberExpected = 'Number expected but "%s" found at line %d column %d.';
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
  (** An exception message for an undeclared class method. **)
  strUndeclaredClassMethod = 'Method "%s" has not been declared.';
  (** An exception message for an unsatisfied forward reference. **)
  strUnSatisfiedForwardReference = 'Method "%s" has an unsatisfied ' +
    'forward reference.';
  (** An exception message for a type not found. **)
  strTypeNotFound = 'Type declaration missing but found "%s" at line %d column %d.';
  (** An exception message when a TypeID is expected. **)
  strTypeIDExpected = 'A TypeID was expected but found "%s" at line %d column %d.';
  (** An execption message when a Expr conflict occurs in an expression **)
  strExprConflict = 'The token "%s" conflicts with the TYPE of the preceeding ' +
    'expression at line %d column %d.';
  (** An exception message if a function is used in a constant expression **)
  strConstExprDesignator = 'The token "%s" at line %d column %d is not allowed ' +
    'in a Constant Expression.';
  (** An exception message if the first none comment token is not Program,
      Package, Unit or Library. **)
  strModuleKeyWordNotfound = '"%s" found but module starting keyword PROGRAM, ' +
    'PACKAGE, UNIT or LIBRARY not found.';
  (** An exception message for an undefined token in the stream. **)
  strUnDefinedToken = 'The token "%s" at line %d column %d is not defined.';
  (** An exception message for an $ELSE without a string $IFDEF / $FIFNDEF **)
  strElseIfMissingIfDef = '$ELSE is missing a starting $IFDEF or $IFNDEF at ' +
    'line %d column %d.';
  (** An exception message for an $ENDIF without a string $IFDEF / $FIFNDEF **)
  strEndIfMissingIfDef = '$ENDIF is missing a starting $IFDEF or $IFNDEF at ' +
    'line %d column %d.';
  (** An exception message for an Ordinal Type not found. **)
  strOrdinalTypeExpected = 'Ordinal type expected but "%s" found at line %d ' +
    'column %d.';
  (** An exception message for a Type Declaration not found. **)
  strTypeDeclExpected = 'Type Declaration expected but "%s" found at line %s ' +
    'column %d.';
  (** An exception message for a Label not found. **)
  strLabelExpected = 'Label expected but "%s" found at line %s column %d.';
  (** An exception message for a Constant Expression found. **)
  strConstExprExpected = 'Constant Expression expected but "%s" found at ' +
    'line %s column %d.';

  (** This is the tree branch under which module documentation error appear **)
  strModuleDocumentation = 'Module Documentation';
  (** This is a documentation error for a missing module description **)
  strModuleMissingDocumentation = 'This module has no document comment.';
  (** This is a documentation error description for a missing module
      description **)
  strModuleMissingDocumentationDesc = 'Each module should have a comment ' +
    'before the PROGRAM, UNIT, PACKAGE or LIBARY key work describing the ' +
    'contents of the module. #Example: #(** #  description #  @@Author David Hoyle ' +
    '#  @@Version 1.0 #  @@Date 07/Jan/2006 #**)';
  (** This is a documentation error for a missing documentation date **)
  strModuleMissingDate = 'This module is missing a documentation date.';
  (** This is a documentation error description for a missing documentation
      date **)
  strModuleMissingDateDesc = 'Each module comment required an @@Date tag to ' +
    'describe the date on which the module was last edited. #Example: ' +
    '@@Date 07 Jan 1970';
  (** This is a documentation error for an incorrect documenation date **)
  strModuleIncorrectDate = 'The module documentation date ''%s'' is incorrect (''%s'').';
  (** This is a documentation error description for an incorrect documenation
      date **)
  strModuleIncorrectDateDesc = 'The module date must be either the date of the ' +
    'file saved to disk for the current date if the module is being edited. ' +
    '#Example: @@Date 12/Jan/2006';
  (** This is a documentation error for an invalid documenation date **)
  strModuleCheckDateError = 'The module documentation date ''%s'' is not valid (''%s'').';
  (** This is a documentation error description for an invalid documenation date **)
  strModuleCheckDateErrorDesc = 'The module date must be a valid date and be ' +
    'either the date of the file saved to disk for the current date if the ' +
    'module is being edited. #Example: @@Date 12/Jan/2006';
  (** This is a documentation error for a missing documentation version **)
  strModuleMissingVersion = 'This module is missing a documentation version.';
  (** This is a documentation error description for a missing documentation
      version **)
  strModuleMissingVersionDesc = 'Each module comment requires an @@Version tag ' +
    'which should be incremented when major and minor changes. #Example: ' +
    '@@Version 1.0.';
  (** This is a documentation error for a missing documentation author **)
  strModuleMissingAuthor = 'This module is missing a documentation author.';
  (** This is a documentation error description for a missing documentation
      author **)
  strModuleMissingAuthorDesc = 'Each module comment should have an @@Author tag ' +
    'to describe who has written the module. #Example: @@Author David Hoyle';

  (** This is the tree branch under which type documentation error appear **)
  strTypeDocumentation = 'Type Documentation';
  (** Document conflict message for an undocumented type clause item. **)
  strTypeClauseUndocumented = 'Type ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented type clause
      item. **)
  strTypeClauseUndocumentedDesc = 'Each Type declaration should have a short ' +
    'description which attempts to decribed what the type should be used for.';

  (** This is the tree branch under which constant documentation error appear **)
  strConstantDocumentation = 'Constant Documentation';
  (** Document conflict message for an undocumented constant clause item. **)
  strConstantClauseUndocumented = 'Constant ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented constant clause
      item. **)
  strConstantClauseUndocumentedDesc = 'Each Constant declaration should have ' +
    'a short description which attempts to decribed what the constant ' +
    'represents.';

  (** This is the tree branch under which resource string documentation error appear **)
  strResourceStringDocumentation = 'Resource String Documentation';
  (** Document conflict message for an undocumented resource string clause item. **)
  strResourceStringClauseUndocumented = 'Resource string ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented resource string
      clause item. **)
  strResourceStringClauseUndocumentedDesc = 'Each Resource String declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'resource string represents.';

  (** This is the tree branch under which variable documentation error appear **)
  strVariableDocumentation = 'Variable Documentation';
  (** Document conflict message for an undocumented variable clause item. **)
  strVariableClauseUndocumented = 'Variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented variable clause
      item. **)
  strVariableClauseUndocumentedDesc = 'Each Variable declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'variable is used for.';

  (** This is the tree branch under which thread variable documentation error appear **)
  strThreadVarDocumentation = 'Thread Variable Documentation';
  (** Document conflict message for an undocumented thread variable clause item. **)
  strThreadVarClauseUndocumented = 'Thread variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented thread variable
      clause item. **)
  strThreadVarClauseUndocumentedDesc = 'Each Thread Variable declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'thread variable is used for.';

  (** This is the tree branch under which record documentation error appear **)
  strRecordDocumentation = 'Record Documentation';
  (** Document conflict message for an undocumented record clause item. **)
  strRecordClauseUndocumented = 'Record variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented record clause
      item. **)
  strRecordClauseUndocumentedDesc = 'Each Record declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'record represents.';

  (** This is the tree branch under which object documentation error appear **)
  strObjectDocumentation = 'Object Documentation';
  (** Document conflict message for an undocumented object clause item. **)
  strObjectClauseUndocumented = 'Object variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented object clause
      item. **)
  strObjectClauseUndocumentedDesc = 'Each Object declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'object represents.';

  (** This is the tree branch under which class documentation error appear **)
  strClassDocumentation = 'Class Documentation';
  (** Document conflict message for an undocumented class variable clause item. **)
  strClassClauseUndocumented = 'Class variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented class variable
      clause item. **)
  strClassClauseUndocumentedDesc = 'Each Class declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'class represents.';

  (** This is the tree branch under which interface documentation error appear **)
  strInterfaceDocumentation = 'Interface Documentation';
  (** Document conflict message for an undocumented interface variable clause item. **)
  strInterfaceClauseUndocumented = 'Interface variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented interface
      variable clause item. **)
  strInterfaceClauseUndocumentedDesc = 'Each Interface declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'interface represents.';

  (** This is the tree branch under which dispinterface documentation error appear **)
  strDispInterfaceDocumentation = 'DispInterface Documentation';
  (** Document conflict message for an undocumented dispinterface variable clause item. **)
  strDispInterfaceClauseUndocumented = 'DispInterface variable ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented dispinterface
      variable clause item. **)
  strDispInterfaceClauseUndocumentedDesc = 'Each DispInterface declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'dispinterface represents.';

  (** Label for Method Documentation Conflicts **)
  strMethodDocumentation = 'Method Documentation';
  (** Document conflict message for missing method documentation. **)
  strMethodUndocumented = 'Method ''%s'' has not been documented.';
  (** Document conflict message description for missing method documentation. **)
  strMethodUndocumentedDesc = 'Each method declaration in the implementation ' +
    'section should have a description which should provide information to ' +
    'future developers regarding the purpose of the method. # #In addition to ' +
    'the descrition each method should have a pre-condition statement ' +
    '(@precon) and a post-condition statement (@postcon). # #Along with these ' +
    'there should be a list of the parameters and any return types.';
  (** Document conflict message for missing method description. **)
  strMethodHasNoDesc = 'Method ''%s'' has no description.';
  (** Document conflict message descritpion for missing method description. **)
  strMethodHasNoDescDesc = 'Each method declaration in the implementation ' +
    'section should have a description which should provide information to ' +
    'furture developers regarding the purpose of the method.';

  (** Document conflict message for different number of parameters and tags. **)
  strMethodDiffParamCount = 'Method ''%s'' has a different parameter count.';
  (** Document conflict message description for different number of parameters
      and tags. **)
  strMethodDiffParamCountDesc = 'There are a different number of @@param tags ' +
    'in the comment compared to the prameters passed to the method.';
  (** Document conflict message for an undocumented parameter. **)
  strMethodUndocumentedParam = 'Parameter ''%s'' in method ''%s'' is not documented.';
  (** Document conflict message description for an undocumented parameter. **)
  strMethodUndocumentedParamDesc = 'The specified parameter in the documented ' +
    'method does not have a corresponding @@param tag in the comment header.';
  (** Document conflict message for an incorrect parameter type. **)
  strMethodIncorrectParamType = 'The parameter type for ''%s'' in method ''%s'' is incorrect.';
  (** Document conflict message description for an incorrect parameter type. **)
  strMethodIncorrectParamTypeDesc = 'The type of the specified parameter ' +
    'differents from the type provided in the @@param tag of the method comment.';

  (** Document conflict message for an undocumented return type. **)
  strMethodUndocumentedReturn = 'Method ''%s''`s return type is not documented.';
  (** Document conflict message descritpion for an undocumented return type. **)
  strMethodUndocumentedReturnDesc = 'A methods return type required an ' +
    '@@return tag in the method comment.';
  (** Document conflict message for an incorrect return type. **)
  strMethodIncorrectReturnType = 'Method ''%s''`s return type is incorrect.';
  (** Document conflict message description for an incorrect return type. **)
  strMethodIncorrectReturnTypeDesc = 'The type of the method return is not the ' +
    'same as the type defined in the method.';

  (** A documentation message for missing precondition text. **)
  strMethodPreConNotDocumented = 'A Pre-condition in Method ''%s'' is not documented.';
  (** A documentation message description for missing precondition text. **)
  strMethodPreConNotDocumentedDesc = 'The @@precon tag in the specified method ' +
    'is either not present or does not contain a statement. A pre-condition ' +
    'statement says something about the status of the input parameters for the ' +
    'method which must be valid for the method to function correctly.';
  (** Document conflict message for a missing pre-condition tag. **)
  strMethodMissingPreCon = 'Method ''%s'' has missing pre-condition tags.';
  (** Document conflict message description for a missing pre-condition tag. **)
  strMethodMissingPreConDesc = 'The method comment expected an @@precon tag ' +
    'which says something about the status of the input parameters for the ' +
    'method which must be valid for the method to function correctly.';
  (** Document conflict message for too many pre-condition tag. **)
  strMethodTooManyPreCons = 'Method ''%s'' has too many pre-condition tags.';
  (** Document conflict message description for too many pre-condition tag. **)
  strMethodTooManyPreConsDesc = 'The method comment has too many pre-condition ' +
    'tags (@precon).';

  (** A documentation message for missing postcondition text. **)
  strMethodPostConNotDocumented = 'A Post-condition in Method ''%s'' is not ' +
    'documented.';
  (** A documentation message description for missing postcondition text. **)
  strMethodPostConNotDocumentedDesc = 'The @@prepost tag in the specified method ' +
    'is either not present or does not contain a statement. A post-condition ' +
    'statement says something about the status of the output from the method ' +
    'which will be valid for the method after being called.';
  (** Document conflict message for a missing post-condition tag. **)
  strMethodMissingPostCon = 'Method ''%s'' has a missing post-condition tag.';
  (** Document conflict message description for a missing post-condition tag. **)
  strMethodMissingPostConDesc = 'The method comment expected an @@postcon tag ' +
    'which says something about the status of the out of the method which ' +
    'will be valid after the method is called.';
  (** Document conflict message for too many post-condition tag. **)
  strMethodTooManyPostCons = 'Method ''%s'' has too many post-condition tags.';
  (** Document conflict message description for too many post-condition tag. **)
  strMethodTooManyPostConsDesc = 'The method comment has too many post-condition ' +
    'tags (@postcon).';

  (** Label for Property Documentation Conflicts **)
  strPropertyDocumentation = 'Property Documentation';
  (** Document conflict message for missing method documentation. **)
  strPropertyUndocumented = 'Property ''%s'' has not been documented.';
  (** Document conflict message description for missing method documentation. **)
  strPropertyUndocumentedDesc = 'Each property declaration in the class or' +
    'interface should have a description which should provide information to ' +
    'future developers regarding the purpose of the property. # #In addition to ' +
    'the descrition each property should have a pre-condition statement ' +
    '(@precon) and a post-condition statement (@postcon). # #Along with these ' +
    'there should be a list of the parameters and any return types.';
  (** Document conflict message for missing property description. **)
  strPropertyHasNoDesc = 'Property ''%s'' has no description.';
  (** Document conflict message description for missing property description. **)
  strPropertyHasNoDescDesc = 'Each property declaration in the class or ' +
    'interface should have a description which should provide information to ' +
    'furture developers regarding the purpose of the method.';

  (** Document conflict message for different number of parameters and tags. **)
  strPropertyDiffParamCount = 'Property ''%s'' has a different parameter count.';
  (** Document conflict message description for different number of parameters
      and tags. **)
  strPropertyDiffParamCountDesc = 'There are a different number of @@param tags ' +
    'in the comment compared to the prameters passed to the property.';
  (** Document conflict message for an undocumented parameter. **)
  strPropertyUndocumentedParam = 'Parameter ''%s'' in property ''%s'' is not documented.';
  (** Document conflict message description for an undocumented parameter. **)
  strPropertyUndocumentedParamDesc = 'The specified parameter in the documented ' +
    'property does not have a corresponding @@param tag in the comment header.';
  (** Document conflict message for an incorrect parameter type. **)
  strPropertyIncorrectParamType = 'The parameter type for ''%s'' in property ''%s'' is incorrect.';
  (** Document conflict message description for an incorrect parameter type. **)
  strPropertyIncorrectParamTypeDesc = 'The type of the specified parameter ' +
    'differents from the type provided in the @@param tag of the property comment.';

  (** Document conflict message for an undocumented return type. **)
  strPropertyUndocumentedReturn = 'Property ''%s''`s return type is not documented.';
  (** Document conflict message description for an undocumented return type. **)
  strPropertyUndocumentedReturnDesc = 'A property return type required an ' +
    '@@return tag in the property comment.';
  (** Document conflict message for an incorrect return type. **)
  strPropertyIncorrectReturnType = 'Property ''%s''`s return type is incorrect.';
  (** Document conflict message description for an incorrect return type. **)
  strPropertyIncorrectReturnTypeDesc = 'The type of the property return is not ' +
    'the same as the type defined in the property.';

  (** A documentation message for missing precondition text. **)
  strPropertyPreConNotDocumented = 'A Pre-condition in Property ''%s'' is not documented.';
  (** A documentation message descritpion for missing precondition text. **)
  strPropertyPreConNotDocumentedDesc = 'The @@precon tag in the specified property ' +
    'is either not present or does not contain a statement. A pre-condition ' +
    'statement says something about the status of the input parameters for the ' +
    'property which must be valid for the property to function correctly.';
  (** Document conflict message for a missing pre-condition tag. **)
  strPropertyMissingPreCon = 'Property ''%s'' has missing pre-condition tags.';
  (** Document conflict message description for a missing pre-condition tag. **)
  strPropertyMissingPreConDesc = 'The property comment expected an @@precon tag ' +
    'which says something about the status of the input parameters for the ' +
    'property which must be valid for the property to function correctly.';
  (** Document conflict message for too many pre-condition tag. **)
  strPropertyTooManyPreCons = 'Property ''%s'' has too many pre-condition tags.';
  (** Document conflict message description for too many pre-condition tag. **)
  strPropertyTooManyPreConsDesc = 'The property comment has too many pre-condition ' +
    'tags (@precon).';

  (** A documentation message for missing postcondition text. **)
  strPropertyPostConNotDocumented = 'A Post-condition in Property ''%s'' is not documented.';
  (** A documentation message description for missing postcondition text. **)
  strPropertyPostConNotDocumentedDesc = 'The @@prepost tag in the specified property ' +
    'is either not present or does not contain a statement. A post-condition ' +
    'statement says something about the status of the output from the property ' +
    'which will be valid for the property after being called.';
  (** Document conflict message for a missing post-condition tag. **)
  strPropertyMissingPostCon = 'Property ''%s'' has a missing post-condition tag.';
  (** Document conflict message description for a missing post-condition tag. **)
  strPropertyMissingPostConDesc = 'The property comment expected an @@postcon tag ' +
    'which says something about the status of the out of the property which ' +
    'will be valid after the property is called.';
  (** Document conflict message for too many post-condition tag. **)
  strPropertyTooManyPostCons = 'Property ''%s'' has too many post-condition tags.';
  (** Document conflict message description for too many post-condition tag. **)
  strPropertyTooManyPostConsDesc = 'The property comment has too many post-condition ' +
    'tags (@postcon).';

  (** Label for Finalization Documentation Conflicts **)
  strModuleInitSection = 'Module Initialization Section';
  (** Document conflict message for a missing Finalialization Comment. **)
  strMissingInitComment = 'The module is missing an Initialization Comment.';
  (** Document conflict message description a missing Finalialization Comment. **)
  strMissingInitCommentDesc = 'It is usually advised able to document the ' +
    'code contain in the Initialization section of the module so that ' +
    'developers known which portion of the module are automatically ' +
    'created.';

  (** Label for Initialization Documentation Conflicts **)
  strModuleFinalSection = 'Module Finalization Section';
  (** Document conflict message for a missing Initialization Comment. **)
  strMissingFinalComment = 'The module is missing an Finalization Comment.';
  (** Document conflict message description a missing Initialization Comment. **)
  strMissingFinalCommentDesc = 'It is usually advised able to document the ' +
    'code contain in the Finalization section of the module so that ' +
    'developers known which portion of the module are automatically ' +
    'destroyed.';

Const
  (** A set of characters for whitespace **)
  strWhiteSpace : Set Of Char = [#32, #9];
  (** A set of characters for line feed and carriage return **)
  strLineEnd : Set of Char = [#10, #13];
  (** A list of strings representing the different types of token. **)
  strTypes : Array[ttUnknown..ttLinkTag] Of String = ('Unknown',
    'WhiteSpace', 'Keyword', 'Identifier', 'Number', 'Symbol', 'LineEnd',
    'ArrayElement', 'StatementEnd', 'StringLiteral', 'Comment', 'HTMLTag',
    'Directive', 'CompilerDirective', 'LinkTag');
  (** This is a string array representing the TDocOption enumerates. **)
  DocOptionInfo : Array[Low(TDocOption)..High(TDocOption)] Of TDocOptionRec = (
    (FDescription : strDrawSynHighModuleExplorer;          FEnabled : False),
    (FDescription : strShowCommentsInTheHints;             FEnabled : False),
    (FDescription : strShowLocalDeclarationsInMethods;     FEnabled : False),
    (FDescription : strShowPrivateDeclarations;            FEnabled : True),
    (FDescription : strShowProtectedDeclarations;          FEnabled : True),
    (FDescription : strShowPublicDeclarations;             FEnabled : True),
    (FDescription : strShowPublishedDeclarations;          FEnabled : True),
    (FDescription : strShowDocumentationConflicts;         FEnabled : False),
    (FDescription : strShowMissingMethodDocumentation;     FEnabled : True),
    (FDescription : strShowMissingMethodDocDesc;           FEnabled : True),
    (FDescription : strShowDiffMethodParameterCount;       FEnabled : True),
    (FDescription : strShowUndocumentedMethodParameters;   FEnabled : True),
    (FDescription : strShowIncorrectMethodParameterType;   FEnabled : True),
    (FDescription : strShowUndocumentedMethodReturn;       FEnabled : True),
    (FDescription : strShowIncorrectMethodReturnType;      FEnabled : True),
    (FDescription : strShowUndocumentedTypes;              FEnabled : False),
    (FDescription : strShowUndocumentedRecords;            FEnabled : False),
    (FDescription : strShowUndocumentedObjects;            FEnabled : False),
    (FDescription : strShowUndocumentedClasses;            FEnabled : False),
    (FDescription : strShowUndocumentedInterfaces;         FEnabled : False),
    (FDescription : strShowUndocumentedVariables;          FEnabled : False),
    (FDescription : strShowUndocumentedConstants;          FEnabled : False),
    (FDescription : strShowUndocumentedModule;             FEnabled : True),
    (FDescription : strShowMissingModuleDate;              FEnabled : False),
    (FDescription : strShowCheckModuleDate;                FEnabled : False),
    (FDescription : strShowMissingModuleVersion;           FEnabled : False),
    (FDescription : strShowMissingModuleAuthor;            FEnabled : False),
    (FDescription : strShowMissingMethodPreConditions;     FEnabled : False),
    (FDescription : strShowMissingMethodPostConditions;    FEnabled : False),
    (FDescription : strShowMissingPropertyDocumentation;   FEnabled : False),
    (FDescription : strShowMissingPropertyDocuDesc;        FEnabled : False),
    (FDescription : strShowDiffPropertyParameterCount;     FEnabled : False),
    (FDescription : strShowUndocumentedPropertyParameter;  FEnabled : False),
    (FDescription : strShowIncorrectPropertyParameterType; FEnabled : False),
    (FDescription : strShowUndocumentedPropertyReturnType; FEnabled : False),
    (FDescription : strShowIncorrectPropertyReturnType;    FEnabled : False),
    (FDescription : strShowMissingPropertyPreConditions;   FEnabled : False),
    (FDescription : strShowMissingPropertyPostConditions;  FEnabled : False),
    (FDescription : strShowMissingInitComment;             FEnabled : False),
    (FDescription : strShowMissingFinalComment;            FEnabled : False)
  );

  (** This is a constant for special tag items to show in the tree **)
  iShowInTree = $0001;
  (** This is a constant for special tag items to auto expand in the tree **)
  iAutoExpand = $0002;

  (** This is a list of Image Resource name to be loaded fom the executable. **)
  ImageList : Array[Succ(Low(TImageIndex))..High(TImageIndex)] Of TImageIndexInfo = (
    (FResourceName : 'Module';                    FMaskColour: $0000FF00),

    (FResourceName : 'DocConflictFolder';         FMaskColour: $0000FF00),
    (FResourceName : 'DocConflictIncorrect';      FMaskColour: $0000FF00),
    (FResourceName : 'DocConflictItem';           FMaskColour: $0000FF00),
    (FResourceName : 'DocConflictMissing';        FMaskColour: $0000FF00),

    (FResourceName : 'ErrorFolder';               FMaskColour: $0000FF00),
    (FResourceName : 'Error';                     FMaskColour: $0000FF00),
    (FResourceName : 'Warning';                   FMaskColour: $0000FF00),

    (FResourceName : 'UsesLabel';                 FMaskColour: $0000FF00),
    (FResourceName : 'UsesItem';                  FMaskColour: $0000FF00),

    (FResourceName : 'TypesLabel';                FMaskColour: $0000FF00),
    (FResourceName : 'PublicType';                FMaskColour: $0000FF00),
    (FResourceName : 'PrivateType';               FMaskColour: $0000FF00),
    (FResourceName : 'PublishedType';             FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedType';             FMaskColour: $0000FF00),
    (FResourceName : 'LocalType';                 FMaskColour: $0000FF00),

    (FResourceName : 'RecordsLabel';              FMaskColour: $0000FF00),
    (FResourceName : 'PublicRecord';              FMaskColour: $0000FF00),
    (FResourceName : 'PrivateRecord';             FMaskColour: $0000FF00),
    (FResourceName : 'PublishedRecord';           FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedRecord';           FMaskColour: $0000FF00),
    (FResourceName : 'LocalRecord';               FMaskColour: $0000FF00),

    (FResourceName : 'FieldsLabel';               FMaskColour: $0000FF00),
    (FResourceName : 'PublicField';               FMaskColour: $0000FF00),
    (FResourceName : 'PrivateField';              FMaskColour: $0000FF00),
    (FResourceName : 'PublishedField';            FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedField';            FMaskColour: $0000FF00),

    (FResourceName : 'ObjectsLabel';              FMaskColour: $0000FF00),
    (FResourceName : 'PublicObject';              FMaskColour: $0000FF00),
    (FResourceName : 'PrivateObject';             FMaskColour: $0000FF00),
    (FResourceName : 'PublishedObject';           FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedObject';           FMaskColour: $0000FF00),
    (FResourceName : 'LocalObject';               FMaskColour: $0000FF00),

    (FResourceName : 'PublicConstructor';         FMaskColour: $0000FF00),
    (FResourceName : 'PrivateConstructor';        FMaskColour: $0000FF00),
    (FResourceName : 'PublishedConstructor';      FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedConstructor';      FMaskColour: $0000FF00),
    (FResourceName : 'LocalConstructor';          FMaskColour: $0000FF00),

    (FResourceName : 'PublicDestructor';          FMaskColour: $00FF00FF),
    (FResourceName : 'PrivateDestructor';         FMaskColour: $00FF00FF),
    (FResourceName : 'PublishedDestructor';       FMaskColour: $00FF00FF),
    (FResourceName : 'ProtectedDestructor';       FMaskColour: $00FF00FF),
    (FResourceName : 'LocalDestructor';           FMaskColour: $00FF00FF),

    (FResourceName : 'PublicProcedure';           FMaskColour: $0000FF00),
    (FResourceName : 'PrivateProcedure';          FMaskColour: $0000FF00),
    (FResourceName : 'PublishedProcedure';        FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedProcedure';        FMaskColour: $0000FF00),
    (FResourceName : 'LocalProcedure';            FMaskColour: $0000FF00),

    (FResourceName : 'PublicFunction';            FMaskColour: $0000FF00),
    (FResourceName : 'PrivateFunction';           FMaskColour: $0000FF00),
    (FResourceName : 'PublishedFunction';         FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedFunction';         FMaskColour: $0000FF00),
    (FResourceName : 'LocalFunction';             FMaskColour: $0000FF00),

    (FResourceName : 'ClassesLabel';              FMaskColour: $0000FF00),
    (FResourceName : 'PublicClass';               FMaskColour: $0000FF00),
    (FResourceName : 'PrivateClass';              FMaskColour: $0000FF00),
    (FResourceName : 'PublishedClass';            FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedClass';            FMaskColour: $0000FF00),
    (FResourceName : 'LocalClass';                FMaskColour: $0000FF00),

    (FResourceName : 'PropertyLabel';             FMaskColour: $00FF00FF),
    (FResourceName : 'PublicProperty';            FMaskColour: $00FF00FF),
    (FResourceName : 'PrivateProperty';           FMaskColour: $00FF00FF),
    (FResourceName : 'PublishedProperty';         FMaskColour: $00FF00FF),
    (FResourceName : 'ProtectedProperty';         FMaskColour: $00FF00FF),
    (FResourceName : 'LocalProperty';             FMaskColour: $00FF00FF),

    (FResourceName : 'InterfacesLabel';           FMaskColour: $0000FF00),
    (FResourceName : 'PublicInterface';           FMaskColour: $0000FF00),
    (FResourceName : 'PrivateInterface';          FMaskColour: $0000FF00),
    (FResourceName : 'PublishedInterface';        FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedInterface';        FMaskColour: $0000FF00),
    (FResourceName : 'LocalInterface';            FMaskColour: $0000FF00),

    (FResourceName : 'DispInterfacesLabel';       FMaskColour: $0000FF00),
    (FResourceName : 'PublicDispInterface';       FMaskColour: $0000FF00),
    (FResourceName : 'PrivateDispInterface';      FMaskColour: $0000FF00),
    (FResourceName : 'PublishedDispInterface';    FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedDispInterface';    FMaskColour: $0000FF00),
    (FResourceName : 'LocalDispInterface';        FMaskColour: $0000FF00),

    (FResourceName : 'ConstantsLabel';            FMaskColour: $0000FF00),
    (FResourceName : 'PublicConst';               FMaskColour: $0000FF00),
    (FResourceName : 'PrivateConst';              FMaskColour: $0000FF00),
    (FResourceName : 'PublishedConst';            FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedConst';            FMaskColour: $0000FF00),
    (FResourceName : 'LocalConst';                FMaskColour: $0000FF00),

    (FResourceName : 'ResourceStringsLabel';      FMaskColour: $0000FF00),
    (FResourceName : 'PublicResourceString';      FMaskColour: $0000FF00),
    (FResourceName : 'PrivateResourceString';     FMaskColour: $0000FF00),
    (FResourceName : 'PublishedResourceString';   FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedResourceString';   FMaskColour: $0000FF00),
    (FResourceName : 'LocalResourceString';       FMaskColour: $0000FF00),

    (FResourceName : 'VariablesLabel';            FMaskColour: $0000FF00),
    (FResourceName : 'PublicVariable';            FMaskColour: $0000FF00),
    (FResourceName : 'PrivateVariable';           FMaskColour: $0000FF00),
    (FResourceName : 'PublishedVariable';         FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedVariable';         FMaskColour: $0000FF00),
    (FResourceName : 'LocalVariable';             FMaskColour: $0000FF00),

    (FResourceName : 'ThreadVarsLabel';           FMaskColour: $0000FF00),
    (FResourceName : 'PublicThreadVar';           FMaskColour: $0000FF00),
    (FResourceName : 'PrivateThreadVar';          FMaskColour: $0000FF00),
    (FResourceName : 'PublishedThreadVar';        FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedThreadVar';        FMaskColour: $0000FF00),
    (FResourceName : 'LocalThreadVar';            FMaskColour: $0000FF00),

    (FResourceName : 'ExportedHeadingsLabel';     FMaskColour: $0000FF00),

    (FResourceName : 'ExportedFunctionsLabel';    FMaskColour: $0000FF00),
    (FResourceName : 'PublicExportedFunction';    FMaskColour: $0000FF00),
    (FResourceName : 'PrivateExportedFunction';   FMaskColour: $0000FF00),
    (FResourceName : 'PublishedExportedFunction'; FMaskColour: $0000FF00),
    (FResourceName : 'ProtectedExportedFunction'; FMaskColour: $0000FF00),
    (FResourceName : 'LocalExportedFunction';     FMaskColour: $0000FF00),

    (FResourceName : 'LabelsLabel';               FMaskColour: $0000FF00),
    (FResourceName : 'PublicLabel';               FMaskColour: $0000FF00),

    (FResourceName : 'ImplementedMethodsLabel';   FMaskColour: $0000FF00),

    (FResourceName : 'InitializationLabel';       FMaskColour: $00FF00FF),
    (FResourceName : 'FinalizationLabel';         FMaskColour: $0000FF00),

    (FResourceName : 'ToDoFolder';                FMaskColour: $0000FF00),
    (FResourceName : 'ToDoItem';                  FMaskColour: $0000FF00),

    (FResourceName : 'UnknownClsObj';             FMaskColour: $0000FF00)
  );

  (** A table of information for document conflicts. **)
  DocConflictTable : Array[Low(TDocConflictType)..High(TDocConflictType)] Of TDocConflictTable = (
    (FCategory: strModuleDocumentation;
      FMessage: strModuleMissingDocumentation;
      FDescription: strModuleMissingDocumentationDesc;
      FConflictType: dciMissing),
    (FCategory: strModuleDocumentation;
      FMessage: strModuleMissingDate;
      FDescription: strModuleMissingDateDesc;
      FConflictType: dciMissing),
    (FCategory: strModuleDocumentation;
      FMessage: strModuleIncorrectDate;
      FDescription: strModuleIncorrectDateDesc;
      FConflictType: dciIncorrect),
    (FCategory: strModuleDocumentation;
      FMessage: strModuleCheckDateError;
      FDescription: strModuleCheckDateErrorDesc;
      FConflictType: dciIncorrect),
    (FCategory: strModuleDocumentation;
      FMessage: strModuleMissingVersion;
      FDescription: strModuleMissingVersionDesc;
      FConflictType: dciMissing),
    (FCategory: strModuleDocumentation;
      FMessage: strModuleMissingAuthor;
      FDescription: strModuleMissingAuthorDesc;
      FConflictType: dciMissing),

    (FCategory: strTypeDocumentation;
      FMessage: strTypeClauseUndocumented;
      FDescription: strTypeClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strConstantDocumentation;
      FMessage: strConstantClauseUndocumented;
      FDescription: strConstantClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strResourceStringDocumentation;
      FMessage: strResourceStringClauseUndocumented;
      FDescription: strResourceStringClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strVariableDocumentation;
      FMessage: strVariableClauseUndocumented;
      FDescription: strVariableClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strThreadVarDocumentation;
      FMessage: strThreadVarClauseUndocumented;
      FDescription: strThreadVarClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FCategory: strClassDocumentation;
      FMessage: strClassClauseUndocumented;
      FDescription: strClassClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyUndocumented;
      FDescription: strPropertyUndocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyHasNoDesc;
      FDescription: strPropertyHasNoDescDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyPreconNotDocumented;
      FDescription: strPropertyPreconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyDiffParamCount;
      FDescription: strPropertyPreconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyMissingPreCon;
      FDescription: strPropertyMissingPreConDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyTooManyPreCons;
      FDescription: strPropertyTooManyPreConsDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyUndocumentedParam;
      FDescription: strPropertyUndocumentedParamDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyIncorrectParamType;
      FDescription: strPropertyIncorrectParamTypeDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyPostconNotDocumented;
      FDescription: strPropertyPostconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyUndocumentedReturn;
      FDescription: strPropertyUndocumentedReturnDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyIncorrectReturnType;
      FDescription: strPropertyIncorrectReturnTypeDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyMissingPostCon;
      FDescription: strPropertyMissingPostConDesc;
      FConflictType: dciMissing),
    (FCategory: strPropertyDocumentation;
      FMessage: strPropertyTooManyPostCons;
      FDescription: strPropertyTooManyPostConsDesc;
      FConflictType: dciMissing),

    (FCategory: strRecordDocumentation;
      FMessage: strRecordClauseUndocumented;
      FDescription: strRecordClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FCategory: strObjectDocumentation;
      FMessage: strObjectClauseUndocumented;
      FDescription: strObjectClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FCategory: strInterfaceDocumentation;
      FMessage: strInterfaceClauseUndocumented;
      FDescription: strInterfaceClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FCategory: strDispInterfaceDocumentation;
      FMessage: strDispInterfaceClauseUndocumented;
      FDescription: strDispInterfaceClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FCategory: strMethodDocumentation;
      FMessage: strMethodUndocumented;
      FDescription: strMethodUndocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodHasNoDesc;
      FDescription: strMethodHasNoDescDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodPreconNotDocumented;
      FDescription: strMethodPreconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodDiffParamCount;
      FDescription: strMethodDiffParamCountDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodMissingPreCon;
      FDescription: strMethodMissingPreConDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodTooManyPrecons;
      FDescription: strMethodTooManyPreconsDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodUndocumentedParam;
      FDescription: strMethodUndocumentedParamDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodIncorrectParamType;
      FDescription: strMethodIncorrectParamTypeDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodPostconNotDocumented;
      FDescription: strMethodPostconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodUndocumentedReturn;
      FDescription: strMethodUndocumentedReturnDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodIncorrectReturntype;
      FDescription: strMethodIncorrectReturntypeDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodMissingPostCon;
      FDescription: strMethodMissingPostConDesc;
      FConflictType: dciMissing),
    (FCategory: strMethodDocumentation;
      FMessage: strMethodTooManyPostCons;
      FDescription: strMethodTooManyPostConsDesc;
      FConflictType: dciMissing),

    (FCategory: strModuleInitSection;
      FMessage: strMissingInitComment;
      FDescription: strMissingInitCommentDesc;
      FConflictType: dciMissing),
    (FCategory: strModuleFinalSection;
      FMessage: strMissingFinalComment;
      FDescription: strMissingFinalComment;
      FConflictType: dciMissing)
  );

Var
  (** This is a global string list containing the special tags list. It is
      constructed and destroyed in the Initialization and Finalizations
      sections respectively. **)
  SpecialTags : TStringList;
  (** This is a global variable for the Browse and Doc It options that need to
      be available throughout the application. **)
  BrowseAndDocItOptions : TBrowseAndDocItOptions;

  Function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Function IsTokenWhiteSpace(strToken : String) : Boolean;

Implementation

Uses
  Windows, StrUtils, DGHLibrary;

Var
  (** This variable provides an incremental number of making doc conflict
      messages unique. **)
  iDocConflictCounter: Integer;
  (** This private variable holds the root of the module for the insertion
      of the documentation conflicts. It is set in TBaseLanguageModule.Create **)
  objModuleRootElement : TElementContainer;

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
          Break;
        End;
    End;
end;

(***

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
    End
  Else If Not FTagMode Then
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
      If Copy(Token[i], 1, 2) = '@@' Then
        str := str + Copy(Token[i], 2, Length(Token[i]) - 1) + #32
      Else If Copy(Token[i], 1, 1) = '#' Then
        str := str + #13 + Copy(Token[i], 2, Length(Token[i]) - 1) + #32
      Else
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

  This is the constructor method for the TComment class.

  @precon  None.
  @postcon Allows a comment to be constructed from another comment (clone).

  @param   srcComment as a TComment

**)
Constructor TComment.Create(srcComment : TComment);

Begin
  Inherited Create;
  FLastTag := Nil;
  FTokens := TStringList.Create;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  FLine := srcComment.Line;
  FCol := srcComment.Col;
  Assign(srcComment);
End;

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
              If (strComment[1] In [':', '*']) Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 1);
                  Result := Create(strComment, iLine, iCol);
                End;
            End;
        End;
    End;
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
        Break;
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

  This method adds and passed elemtn container to this classes element
  collection.

  @precon  AElement must be a valid TElementContainer.
  @postcon Adds and passed elemtn container to this classes element
           collection.

  @param   AElement as a TElementContainer
  @return  a TElementContainer

**)
Function TElementContainer.Add(AElement: TElementContainer) : TElementContainer;

Var
  i : Integer;

begin
  Result := AElement;
  Assert(AElement.Name <> '', 'Can not add a null element to the collection!');
  i := Find(AElement.Name);
  If i < 0 Then
    FElements.Insert(Abs(i) - 1, AElement)
  Else
    Begin
      Result := FElements[i -1] As TElementContainer;
      Result.Comment.Assign(AElement.Comment);
      (** Free AElement after getting the comment as it will leak otherwise. **)
      AElement.Free;
    End;
end;

(**

  This method adds and passed Token to this classes element collection.

  @precon  Token must be a valid TTokenInfo and AComment must be either nil or
           a valid TComment instance.
  @postcon Adds and passed elemtn container to this classes element
           collection.

  @param   Token       as a TTokenInfo
  @param   AScope      as a TScope
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment
  @return  a TElementContainer

**)
Function TElementContainer.Add(Token: TTokenInfo; AScope : TScope;
  AImageIndex : TImageIndex; AComment: TComment) : TElementContainer;

Var
  i : Integer;

begin
  Assert(Token.Token <> '', 'Can not add a null token to the collection!');
  i := Find(Token.Token);
  If i < 0 Then
    Begin
      Result := TElementContainer.Create(Token.Token, AScope, Token.Line,
        Token.Column, AImageIndex, AComment);
      FElements.Insert(Abs(i) - 1, Result);
    End Else
    Begin
      Result := FElements[i - 1] As TElementContainer;
      Result.Comment.Assign(AComment);
    End;
end;

(**

  This method adds a string token to the container as a sub container NOT a
  token.

  @precon  None.
  @postcon Returns an instance of the sub container created around the token.

  @param   strToken    as a String
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment
  @return  a TElementContainer

**)
function TElementContainer.Add(strToken: String; AImageIndex: TImageIndex;
  AComment: TComment): TElementContainer;

Var
  i : Integer;

begin
  Assert(strToken <> '', 'Can not add a null string to the collection!');
  i := Find(strToken);
  If i < 0 Then
    Begin
      Result := TElementContainer.Create(strToken, scNone, 0, 0, AImageIndex, AComment);
      FElements.Insert(Abs(i) - 1, Result);
    End Else
    Begin
      Result := FElements[i - 1] As TElementContainer;
      Result.Comment.Assign(AComment);
    End;
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
  @param   AComment        as a TComment
  @param   DocConflictRec  as a TDocConflictTable

**)
procedure TElementContainer.AddDocumentConflict(Const Args: Array of TVarRec;
  iIdentLine, iIdentColumn : Integer; AComment : TComment;
  DocConflictRec : TDocConflictTable);

Var
  E, I : TElementContainer;
  iL, iC : Integer;
  iIcon : TImageIndex;

begin
  iL := 0;
  iC := 0;
  If AComment <> Nil Then
    Begin
      iL := AComment.Line;
      iC := AComment.Col;
    End;
  Case DocConflictRec.FConflictType Of
    dciMissing : iIcon := iiDocConflictMissing;
    dciIncorrect : iIcon := iiDocConflictIncorrect;
  Else
    iIcon := iiDocConflictItem;
  End;
  Assert(objModuleRootElement <> Nil, 'objModuleRootElement can not be null!');
  E := objModuleRootElement.FindElement(strDocumentationConflicts);
  If E = Nil Then
    Begin
      E := TElementContainer.Create(strDocumentationConflicts, scGlobal, 0, 0,
        iiDocConflictFolder, Nil);
      E := objModuleRootElement.Add(E);
    End;
  I := E.FindElement(DocConflictrec.FCategory);
  If I = Nil Then
    Begin
      I := TElementContainer.Create(DocConflictRec.FCategory,
        scGlobal, 0, 0, iiDocConflictFolder, Nil);
      I := E.Add(I);
    End;
  I.Add(TDocumentConflict.Create(Args, iIdentLine, iIdentColumn, iL, iC,
    DocConflictRec.FMessage, DocConflictRec.FDescription, iIcon));
end;

(**

  This methof adds the given elements tokens to the current containers tokens.

  @precon  None.
  @postcon Adds the given elements tokens to the current containers tokens.

  @param   AElement as a TElementContainer

**)
procedure TElementContainer.AddTokens(AElement: TElementContainer);

Var
  i : Integer;

begin
  Assert(AElement <> Nil, 'Can not add a null element to the collection!');
  For i := 0 To AElement.TokenCount - 1 Do
    AppendToken(AElement.Tokens[i]);
end;

(**

  This method appends the given string to the end of the token.

  @precon  None.
  @postcon Appends the given string to the end of the token.

  @param   strToken as a String

**)
Procedure TTokenInfo.Append(strToken : String);

Begin
  FToken := FToken + strToken;
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := FUtoken + UpperCase(strToken);
End;

(**

  This method adds a TTokenInfo instance to the Token collection.

  @precon  AToken must be a valid instance of a TTokenInfo class.
  @postcon Adds a TTokenInfo instance to the Token collection.

  @param   AToken as a TTokenInfo

**)
procedure TElementContainer.AppendToken(AToken: TTokenInfo);
begin
  FTokens.Add(TTokenInfo.Create(AToken.Token, AToken.BufferPos, AToken.Line,
    AToken.Column, AToken.Length, AToken.TokenType));
end;

(**

  This method appends the given string as a token in the containers token list.

  @precon  None.
  @postcon Appends the given string as a token in the containers token list.

  @param   strToken as a String

**)
procedure TElementContainer.AppendToken(strToken: String);
begin
  FTokens.Add(TTokenInfo.Create(strToken, 0, 0, 0, 0, ttUnknown));
end;

(**

  This method copies the tokens from the source into this element replacing any
  token that already exist.

  @precon  Source must be a valid TElementContainer.
  @postcon Copies the tokens from the source into this element replacing any
           token that already exist.

  @param   Source as a TElementContainer

**)
procedure TElementContainer.Assign(Source: TElementContainer);

Var
  iToken : Integer;

begin
  FName := Source.FName;
  FScope := Source.FScope;
  FLine := Source.FLine;
  FColumn := Source.FColumn;
  FComment := Nil;
  If Source.Comment <> Nil Then
    FComment := TComment.Create(Source.Comment);
  FImageIndex := Source.FImageIndex;
  FTokens.Clear;
  For iToken :=  0 To Source.TokenCount - 1 Do
    AppendToken(Source.Tokens[iToken]);
end;

(**

  This method recrusively checks the documentation of the module. Descendants
  need to override this to implement document checking.

  @precon  None.
  @postcon Recrusively checks the documentation of the module.

  @param   boolCascade as a Boolean

**)
procedure TElementContainer.CheckDocumentation(var boolCascade : Boolean);

Var
  i : Integer;

begin
  If boolCascade Then
    For i := 1 To ElementCount Do
      Elements[i].CheckDocumentation(boolCascade);
end;

(**

  This is the constructor method for the TElementContainer class.

  @precon  AComment must be either nil or a valid TComment instance.
  @postcon Creates an instance of the element container.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TElementContainer.Create(strName: String; AScope : TScope;
  iLine, iColumn : Integer; AImageIndex : TImageIndex; AComment: TComment);

begin
  FElements := TObjectList.Create(True);
  FTokens := TObjectList.Create(True);
  FName := strName;
  FLine := iLine;
  FColumn := iColumn;
  FComment := AComment;
  FScope := AScope;
  FImageIndex := AImageIndex;
  FSorted := True;
end;

(**

  This method deletes the indexed element from the collection.

  @precon  iIndex must be a valid index between 1 and ElementCount.
  @postcon Deletes the indexed element from the collection.

  @param   iIndex as an Integer

**)
procedure TElementContainer.DeleteElement(iIndex: Integer);

begin
  FElements.Delete(iIndex - 1);
end;

(**

  This is the destructor method for the TElementContainer class.

  @precon  None.
  @postcon Destroys the instance of the class.

**)
destructor TElementContainer.Destroy;
begin
  FTokens.Free;
  FElements.Free;
  inherited;
end;

(**

  This method returns the position of the named container in the current
  containers collection if found else returns the position (as a negative)
  where the item should be inserted in the collection.

  @precon  None.
  @postcon Returns the position of the named container in the current
           containers collection if found else returns the position (as a
           negative) where the item should be inserted in the collection.

  @param   strName as a String
  @return  an Integer

**)
function TElementContainer.Find(strName: String): Integer;

Var
  iFirst : Integer;
  iMid : Integer;
  iLast : Integer;

begin
  Result := -1;
  If FSorted Then
    Begin // Binary search...
      iFirst := 1;
      iLast := FElements.Count;
      While iFirst <= iLast Do
        Begin
          iMid := (iFirst + iLast) Div 2;
          If AnsiCompareText(Elements[iMid].Name, strName) = 0 Then
            Begin
              Result := iMid;
              Break;
            End Else
          If AnsiCompareText(Elements[iMid].Name, strName) > 0 Then
            iLast := iMid - 1
          Else
            iFirst := iMid + 1;
        End;
    End Else
    Begin // Sequential search...
      For iFirst := 1 To ElementCount Do
        If AnsiCompareText(Elements[iFirst].Name, strName) = 0 Then
          Begin
            Result := iFirst;
            Break;
          End;
      iFirst := ElementCount + 1;
    End;
  If Result < 0 Then
    Result := -iFirst;
end;

(**

  This method searches the elements collection of and instance matching the
  given name.

  @precon  None.
  @postcon Returns either instance of the found item or returns nil.

  @param   strName as a String
  @return  a TElementContainer

**)
function TElementContainer.FindElement(strName: String): TElementContainer;

Var
  i : Integer;

begin
  Result := Nil;
  i := Find(strName);
  If i > 0 Then
    Result := Elements[i];
end;

(**

  This function finds the occurance of the token and returns its index if found
  else returns -1.

  @precon  None.
  @postcon Finds the occurance of the token and returns its index if found
           else returns -1.

  @param   strToken as a String
  @return  an Integer

**)
function TElementContainer.FindToken(strToken: String): Integer;

var
  i: Integer;

begin
  Result := -1;
  For i := 0 To TokenCount - 1 Do
    If strToken = Tokens[i].Token Then
      Begin
        Result := i;
        Break;
      End;
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the name of the element followed by the list of tokens.

  @return  a String

**)
function TElementContainer.GetAsString: String;

begin
  Result := FName;
end;

(**

  This method returns the adjusted image index the element based on the scope.

  @precon  None.
  @postcon Returns the adjusted image index the element based on the scope.

  @return  an Integer

**)
Function TElementContainer.GetImageIndexAdjustedForScope : Integer;

Begin
  Case FScope Of
    scPrivate   : Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 1;
    scPublished : Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 2;
    scProtected : Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 3;
    scLocal     : Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 4;
  Else
    // scPublic, scGlobal, scNone
    Result := Integer(ImageIndex) - Integer(Succ(iiNone));
  End;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the element. This can be override for the
           purposes of find / sorting the elements. Identifier still returns
           the FName variable.

  @return  a String

**)
function TElementContainer.GetName: String;
begin
  Result := FName;
end;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of Items in the Elements Collection.

  @return  an Integer

**)
function TElementContainer.GetElementCount: Integer;
begin
  Result := FElements.Count;
end;

(**

  This is a getter method for the Elements property.

  @precon  iIndex must be a valid index into the elements collection.
  @postcon Returns the instance of the indexed element.

  @param   iIndex as an Integer
  @return  a TElementContainer

**)
function TElementContainer.GetElements(iIndex: Integer): TElementContainer;
begin
  Result := FElements[iIndex - 1] As TElementContainer;
end;

(**

  This is a getter method for the TokenCount property.

  @precon  None.
  @postcon Returns the number of Tokens in the elements collection.

  @return  an Integer

**)
function TElementContainer.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

(**

  This is a getter method for the Tokens property.

  @precon  iIndex must be a valid index into the Token collection.
  @postcon Returns the instance of the indexed token.

  @param   iIndex as an Integer
  @return  a TTokenInfo

**)
function TElementContainer.GetTokens(iIndex: Integer): TTokenInfo;
begin
  Result := FTokens[iIndex] As TTokenInfo;
end;

(**

  This is a setter method for the Sorted property.

  @precon  None.
  @postcon Sets the class to be either sorted or not sorted. Must be set before
           adding elements to the collection.

  @param   boolValue as a Boolean

**)
procedure TElementContainer.SetSorted(boolValue: Boolean);
begin
  Assert(ElementCount = 0, 'Can not set sorted after adding elements.');
  FSorted := boolValue;
end;

(** --------------------------------------------------------------------------

  TParameter Methods

 -------------------------------------------------------------------------- **)

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
  @param   AType       as a TGenericTypeDecl
  @param   Value       as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iCol        as an Integer

**)
Constructor TGenericParameter.Create(ParamMod : TParamModifier; Ident : String;
  boolArrayOf : Boolean; AType : TGenericTypeDecl;
  Value : String; AScope : TScope; iLine, iCol : Integer);

Begin
  Inherited Create(Ident, AScope, iLine, iCol, iiNone, Nil);
  If AType <> Nil Then
    Begin
      FParamType := TElementContainerClass(AType.ClassType).Create('', AScope,
        0, 0, iiNone, Nil) As TGenericTypeDecl;
      FParamType.Assign(AType);
    End;
  FParamModifier := ParamMod;
  FArrayOf := boolArrayOf;
  FDefaultValue := Value;
  Assert(Ident <> '', 'Ident in TGenericParameter IS NULL!');
End;

(**

  This is the TParameters destructor method. If frees the parameter type string
  list.

  @precon  None.
  @postcon If frees the parameter type string list.

**)
destructor TGenericParameter.Destroy;
begin
  FParamType.Free;
  inherited;
end;

(**

  This method returns the type information for the parameter only.

  @precon  None.
  @postcon Returns the type information for the parameter only.

  @return  a String

**)
function TGenericParameter.ParamReturn: String;

Var
  i : Integer;

begin
  Result := '';
  If ParamType <> Nil Then
    For i := 0 To ParamType.TokenCount - 1 Do
      Result := Result + ParamType.Tokens[i].Token;
end;

(** --------------------------------------------------------------------------

  TProperty Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds a parameter to the property.

  @precon  AParameter must be a valid TGenericParameter descendant.
  @postcon Adds a parameter to the property.

  @param   AParameter as a TGenericParameter

**)
procedure TGenericProperty.AddParameter(AParameter: TGenericParameter);
begin
  FParameters.Add(AParameter);
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

  @param   strIdent    as a String
  @param   AScope       as a TScope
  @param   iLine       as an Integer
  @param   iCol        as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TGenericProperty.Create(strIdent: String; AScope: TScope;
  iLine, iCol : Integer; AImageIndex : TImageIndex; AComment : TComment);
begin
  Inherited Create(strIdent, AScope, iLine, iCol, AImageIndex, AComment);
  FParameters := TObjectList.Create(True);
  FTypeId := Nil;
end;

(**

  This is the classes destructor. It frees the parameter collection and the
  parameters.

  @precon  None.
  @postcon It frees the parameter collection and the parameters.

**)
destructor TGenericProperty.Destroy;
begin
  FParameters.Free;
  FTypeID.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of parameters in the property.

  @return  an Integer

**)
function TGenericProperty.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

(**

  This is a getter method for the Parameters property.

  @precon  iIndex must be between 0 and ParameterCount - 1.
  @postcon Returns an instance of the indexed parameter.

  @param   iIndex as an Integer
  @return  a TGenericParameter

**)
function TGenericProperty.GetParameters(iIndex: Integer): TGenericParameter;
begin
  Result := FParameters[iIndex] As TGenericParameter;
end;

(** --------------------------------------------------------------------------

  TMethodDecl Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds the given parameter to the internal list.

  @precon  AParameter must be a valid instance.
  @postcon Adds the given parameter to the internal list.

  @param   AParameter as a TGenericParameter

**)
procedure TGenericMethodDecl.AddParameter(AParameter: TGenericParameter);
begin
  FParameters.Add(AParameter);
end;

(**

  This is the constructor for the TMethodDecl class. It initialises the method
  type, scope and line and col information. If also creates a colection to
  store the parameter objects and a string list for the method directives.

  @precon  MethodType is an enumerate indocating the type of the method, Scope
           is the scope of the method, iLine is the line number of the method,
           and iCol is the column number of the method.
  @postcon It initialises the method type, scope and line and col information.

  @param   MethodType as a TMethodType
  @param   strName    as a String
  @param   AScope     as a TScope
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
Constructor TGenericMethodDecl.Create(MethodType : TMethodType; strName : String;
  AScope : TScope; iLine, iCol : Integer);

Var
  AImageIndex : TImageIndex;

Begin
  FParameters := TObjectList.Create(True);
  Case MethodType Of
    mtConstructor : AImageIndex := iiPublicConstructor;
    mtDestructor  : AImageIndex := iiPublicDestructor;
    mtProcedure   : AImageIndex := iiPublicProcedure;
    mtFunction    : AImageIndex := iiPublicFunction;
  Else
    AImageIndex := iiPublicProcedure;
  End;
  Inherited Create(strName, AScope, iLine, iCol, AImageIndex, Nil);
  FAlias := '';
  FClassMethod := False;
  FClsName := '';
  FComment := Nil;
  FExt := '';
  FMsg := '';
  FReturnType := Nil;
  FMethodType := MethodType;
  FForwardDecl := False;
End;

(**

  This is the destructor method for the TMethodDecl class. It frees the
  parameters collection, the parameter and the directives.

  @precon  None.
  @postcon It frees the parameters collection, the parameter and the directives.

**)
Destructor TGenericMethodDecl.Destroy;

Begin
  FReturnType.Free;
  FParameters.Free;
  Inherited Destroy;
End;

(**

  This is a setter method for the ClsName property.

  @precon  Value is the new value to assign to the ClsName property.
  @postcon Setst the class name property.

  @param   Value as a String

**)
Procedure TGenericMethodDecl.SetClsName(Value : String);

Begin
  If FClsName <> Value Then
    FClsName := Value;
End;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of parameters associated with the method.

  @return  an Integer

**)
function TGenericMethodDecl.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

(**

  This is a getter method for the Parameters property.

  @precon  iIndex must be a valid index.
  @postcon Returns the index instance of the paramter.

  @param   iIndex as an Integer
  @return  a TGenericParameter

**)
function TGenericMethodDecl.GetParameters(iIndex: Integer): TGenericParameter;
begin
  Result := FParameters[iIndex] As TGenericParameter;
end;

(**

  This method returns a fully qualified name for the method.

  @precon  None.
  @postcon Returns a fully qualified name for the method.

  @return  a String

**)
Function TGenericMethodDecl.GetQualifiedName : String;

Begin
  Result := Identifier;
  If ClsName <> '' Then
    Result := ClsName + '.' + Result;
End;

(**

  This is a setter method for the Msg property.

  @precon  Value is the new value to assign to the Msg property.
  @postcon Sets the Message property for the method.

  @param   Value as a String constant

**)
procedure TGenericMethodDecl.SetMsg(const Value: String);
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
procedure TGenericMethodDecl.SetExt(const Value: String);
begin
  If FExt <> Value Then
    FExt := Value;
end;

(**

  This is the constructor method for the TDocError class.

  @precon  strMsg is the error message to create a doc error for, iLine is the
           line number of the error, iCol is the column number for the message,
           strExceptionMethod is the name of the method the exception occurred
           in and ErrType determines if the mesage is a warning or an error.
  @postcon Initialises the class.

  @param   strMsg             as a String
  @param   AScope             as a TScope
  @param   strMethod          as a String
  @param   iLine              as an Integer
  @param   iCol               as an Integer
  @param   ErrType            as a TErrorType

**)
constructor TDocError.Create(strMsg : String; AScope : TScope; strMethod: String; iLine,
  iCol: Integer; ErrType : TErrorType);

Var
  AImageIndex : TImageIndex;

Begin
  Case ErrType Of
    etWarning: AImageIndex := iiWarning;
  Else
    AImageIndex := iiError;
  End;
  Inherited Create(Format('%d@%d:%d', [GetTickCount, iLine, iCol]), AScope,
    iLine, iCol, AImageIndex, Nil);
  FMsg := strMsg;
  FErrorType := ErrType;
  FMethod := strMethod;
End;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Override the default method and returns the Document Error Message.

  @return  a String

**)
Function TDocError.GetAsString: String;

begin
  Result := FMsg;
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
Constructor EDocException.Create(strMsg : String; Token : TTokenInfo;
  strMethodName : String);

Begin
  Inherited Create(Format(strMsg, [Token.Token, Token.Line, Token.Column]),
    scGlobal, Token.Line, Token.Column, iiError, Nil);
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
Constructor EDocException.Create(strMsg, strLiteral : String;
  Token : TTokenInfo; strMethodName : String);

Begin
  Inherited Create(Format(strMsg, [strLiteral, Token.Token, Token.Line,
    Token.Column]), scGlobal, Token.Line, Token.Column, iiError, Nil);
  FExceptionMethod := strMethodName;
End;

(**

  This is the constructor method for the EDocException class.

  @precon  None.
  @postcon Creates an instance of the Document Exception with the message strMsg
           and Method name strMethodName.

  @param   strMsg        as a String
  @param   strMethodName as a String

**)
constructor EDocException.Create(strMsg: String; strMethodName : String);
begin
  Inherited Create(strMsg, scGlobal, 0, 0, iiError, Nil);
  FExceptionMethod := strMethodName;
end;

(**

  This is a getter method for the Message property.

  @precon  None.
  @postcon Returns the message stored in the FName field.

  @return  a String

**)
function EDocException.GetMessage: String;
begin
  Result := Name;
end;

{ TDocumentConflict }

(**

  This is the constructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Initialises the Conflict class.

  @param   Args               as an Array Of TVarRec constant
  @param   iIdentLine         as an Integer
  @param   iIdentColumn       as an Integer
  @param   iCommentLine       as an Integer
  @param   iCommentCol        as an Integer
  @param   strDocConflictMsg  as a String
  @param   strDocConflictDesc as a String
  @param   AImageIndex        as a TImageIndex

**)
constructor TDocumentConflict.Create(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      strDocConflictMsg, strDocConflictDesc : String; AImageIndex : TImageIndex);

begin
  Inherited Create(Format('%4.4d', [iDocConflictCounter]), scGlobal, iIdentLine,
    iIdentColumn, AImageIndex, Nil);
  Inc(iDocConflictCounter);
  FMessage := Format(strDocConflictMsg , Args);
  FCommentLine := iCommentLine;
  FCommentColumn := iCommentCol;
  FComment := TComment.Create(strDocConflictDesc, 0, 0);
end;

(**

  This is the destructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Frees the comment.

**)
destructor TDocumentConflict.Destroy;
begin
  FComment.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Return the document conflict message.

  @return  a String

**)
Function TDocumentConflict.GetAsString: String;

begin
  Result := FMessage;
end;

(**

  This method adds a Compiler Definition to the sources internal list.

  @precon  None.
  @postcon Adds a Compiler Definition to the sources internal list.

  @param   strDef as a String

**)
procedure TBaseLanguageModule.AddDef(strDef : String);

begin
  FCompilerDefs.Add(strDef);
end;

(**

  This method adds an error to the Base Language's Element Collection under
  a sub folder of strCategory.

  @precon  Error must be a valid TElementContainer.
  @postcon Adds an error to the Base Language's Element Collection under
           a sub folder of strCategory.

  @param   Error       as a TElementContainer

**)
Procedure TBaseLanguageModule.AddError(Error: TElementContainer);

Var
  I : TElementContainer;

begin
  I := Add(strErrorsAndWarnings, iiErrorFolder, Nil);
  I.Add(Error);
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

  @param   IsModified       as a Boolean
  @param   strFileName      as a String

**)
constructor TBaseLanguageModule.Create(IsModified : Boolean; strFileName : String);
begin
  Inherited Create(strFileName, scGlobal, 0, 0, iiModule, Nil);
  FFileName := strFileName;
  FModified := IsModified;
  FOwnedItems := TObjectList.Create(True);
  FTokens := TObjectList.Create(True);
  FTokenIndex := 0;
  FPreviousTokenIndex := -1;
  FTickList := TStringList.Create;
  FBodyComment := TObjectList.Create(True);
  FModuleComment := Nil;
  FModuleName := '';
  FModuleNameCol := 0;
  FModuleNameLine := 0;
  FModuleType := mtUnit;
  FCompilerDefs := TStringList.Create;
  FCompilerDefs.Sorted := True;
  FCompilerDefs.Duplicates := dupIgnore;
  FCompilerDefs.CaseSensitive := False;
  FCompilerConditionStack := TList.Create;
  objModuleRootElement := Self;
end;

(**

  This method deletes a definition from the source compiler definitions list.

  @precon  None.
  @postcon Deletes a definition from the source compiler definitions list.

  @param   strDef as a String

**)
procedure TBaseLanguageModule.DeleteDef(strDef : String);

Var
  i : Integer;

begin
  If FCompilerDefs.Find(strDef, i) Then
    FCompilerDefs.Delete(i);
end;

(**

  This is the destructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Frees the memory used by all the collections.

**)
destructor TBaseLanguageModule.Destroy;
begin
  FCompilerConditionStack.Free;
  FCompilerDefs.Free;
  FBodyComment.Free;
  FTickList.Free;
  FDocErrors.Free;
  FTokens.Free;
  FOwnedItems.Free;
  inherited;
end;

(**

  This method adds the given token to the underlying token collection.

  @precon  AToken must be a valid instance of a TTokenInfo class..
  @postcon Adds the given token to the underlying token collection.

  @param   AToken as a TTokenInfo

**)
Procedure TBaseLanguageModule.AddToken(AToken : TTokenInfo);

Begin
  FTokens.Add(AToken);
End;

(**

  This method appends the pased token string to the previous token.

  @precon  None.
  @postcon Appends the pased token string to the previous token.

  @param   strToken as a String

**)
Procedure TBaseLanguageModule.AppendToLastToken(strToken : String);

Begin
  TokenInfo[TokenCount - 1].Append(strToken);
End;

(**

  This method checks to see the the given definition exists in the source list.

  @precon  None.
  @postcon Returns true if the definition exists.

  @param   strDef as a String
  @return  a Boolean

**)
function TBaseLanguageModule.IfDef(strDef : String) : Boolean;

Var
  iIndex : Integer;

begin
  Result := FCompilerDefs.Find(strDef, iIndex);
end;

(**

  This method checks to see if a definition DOES NOT exist in the list.

  @precon  None.
  @postcon Returns true if the definition does not exist.

  @param   strDef as a String
  @return  a Boolean

**)
function TBaseLanguageModule.IfNotDef(strDef : String) : Boolean;

begin
  Result := Not IfDef(strDef);
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
Function TBaseLanguageModule.GetComment(
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
  If FTokenIndex + iOffset > -1 Then
    Begin
      T := FTokens[FTokenIndex + iOffset] As TTokenInfo;
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

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Override and default GetAsString method and returns the name of the
           module.

  @return  a String

**)
Function TBaseLanguageModule.GetAsString : String;

begin
  Result := ExtractFileName(Name);
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

  This is a getter method for the ModuleName property.

  @precon  None.
  @postcon Override this method to change its appearance.

  @return  a String

**)
Function TBaseLanguageModule.GetModuleName : String;

Begin
  Result := FModuleName;
End;


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
    Raise EDocException.Create(strUnExpectedEndOfFile, 'GetToken');
  Result := FTokens[FTokenIndex] As TTokenInfo;
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

Var
  boolContinue : Boolean;
  iSkip : Integer;
  C : TComment;

begin
  iSkip := 0;
  If Token.TokenType = ttCompilerDirective Then // Catch first token as directive
    ProcessCompilerDirective(iSkip);
  Repeat
    If Token.TokenType = ttComment Then
      Begin
        C := TComment.CreateComment(Token.Token, Token.Line, Token.Column);
        If C <> Nil Then
          BodyComments.Add(C);
      End;
    If Not (TokenInfo[FTokenIndex].TokenType In [ttComment, ttCompilerDirective])
      And (iSkip = 0) Then
      FPreviousTokenIndex := FTokenIndex;
    NextToken;
    If Token.TokenType = ttCompilerDirective Then
      ProcessCompilerDirective(iSkip);
    boolContinue := (
      (
        Token.TokenType In [ttComment, ttCompilerDirective]
      ) And
      Not EndOfTokens
    ) Or (iSkip > 0)
  Until Not boolContinue;
    //NextToken;
end;

(**

  This method processes a compiler directive looking for conditional statements.

  @precon  None.
  @postcon Processes a compiler directive looking for conditional statements.

  @param   iSkip as an Integer as a reference

**)
procedure TBaseLanguageModule.ProcessCompilerDirective(var iSkip : Integer);

  (**

    This function returns the definition string from the current compiler
    directive.

    @precon  None.
    @postcon Returns the definition as a string.

    @return  a String

  **)
  Function GetDef : String;

  Begin
    Result := Trim(Copy(Token.Token, 9, Length(Token.Token) - 9));
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
    Result := AnsiCompareText(Copy(strText, 1, Length(strStart)), strStart) = 0;
  End;

  (**

    This method adds the number to the stack and increments the iSkip variable
    by the value passed.

    @precon  None.
    @postcon Adds the number to the stack and increments the iSkip variable
             by the value passed.

    @param   iValue as an Integer

  **)
  Procedure AddToStackAndInc(iValue : Integer);

  Begin
    FCompilerConditionStack.Add(Pointer(iValue));
    Inc(iSkip, iValue);
  End;

Var
  iStack, iStackTop : Integer;

begin
  If Like(Token.Token, '{$DEFINE') Then
    AddDef(GetDef)
  Else If Like(Token.Token, '{$UNDEF') Then
    DeleteDef(GetDef)
  Else If Like(Token.Token, '{$IFDEF') Then
    Begin
      If Not IfDef(GetDef) Then
        AddToStackAndInc(1)
      Else
        AddToStackAndInc(0);
    End
  Else If Like(Token.Token, '{$IFOPT') Then
    Begin
      If Not IfDef(GetDef) Then
        AddToStackAndInc(1)
      Else
        AddToStackAndInc(0);
    End
  Else If Like(Token.Token, '{$IFNDEF') Then
    Begin
      If Not IfNotDef(GetDef) Then
        AddToStackAndInc(1)
      Else
        AddToStackAndInc(0);
    End
  Else If Like(Token.Token, '{$ELSE') Then
    Begin
      iStackTop := FCompilerConditionStack.Count;
      If iStackTop > 0 Then
        Begin
          iStack := Integer(FCompilerConditionStack[iStackTop - 1]);
          If iStack = 1 Then
            Begin
              FCompilerConditionStack[iStackTop - 1] := Pointer(0);
              Dec(iSkip)
            End Else
            Begin
              FCompilerConditionStack[iStackTop - 1] := Pointer(1);
              Inc(iSkip);
            End;
        End Else
          Add(
            TDocError.Create(
              Format(strElseIfMissingIfDef, [Token.Line, Token.Column]),
              scGlobal, 'ProcessCompilerDirective', Token.Line, Token.Column,
              etWarning)
          );
    End
  Else If Like(Token.Token, '{$ENDIF') Then
    Begin
      iStackTop := FCompilerConditionStack.Count;
      If iStackTop > 0 Then
        Begin
          iStack := Integer(FCompilerConditionStack[iStackTop - 1]);
          If iStack = 1 Then
            Dec(iSkip);
          FCompilerConditionStack.Delete(iStackTop - 1);
        End Else
          Add(
            TDocError.Create(
              Format(strElseIfMissingIfDef, [Token.Line, Token.Column]),
              scGlobal, 'ProcessCompilerDirective', Token.Line, Token.Column, etWarning)
          );
    End;
  If iSkip < 0 Then
    iSkip := 0;
end;


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
  If FPreviousTokenIndex >= 0 Then
    Result := FTokens[FPreviousTokenIndex] As TTokenInfo
  Else
    For i := FTokenIndex - 1 DownTo 0 Do
      If Not ((FTokens[i] As TTokenInfo).TokenType In [ttComment,
        ttCompilerDirective]) Then
        Begin
          Result := FTokens[i] As TTokenInfo;
          Break;
        End;
end;

(**

  This method rolls back the current token to the previous valid token if there
  is one else searches for a previous token.

  @precon  None.
  @postcon Rolls back the current token to the previous valid token if there
           is one else searches for a previous token.

**)
Procedure TBaseLanguageModule.RollBackToken;

Begin
  If FPreviousTokenIndex >= 0 Then
    FTokenIndex := FPreviousTokenIndex
  Else
    Begin
      Dec(FTokenIndex);
      While (FTokenIndex > 0) And (TokenInfo[FTokenIndex].TokenType In [ttComment,
        ttCompilerDirective]) Do
        Dec(FTokenIndex);
      If FTokenIndex < 0 Then
        Raise EDocException.Create(strUnExpectedStartOfFile, 'RollBackToken');
    End;
End;

(**

  This method checks the module comment for various type of documentation
  errors.

  @precon  Module is the module to check.
  @postcon The modules comment is checked for errors.

  @param   boolCascade as a Boolean

**)
Procedure TBaseLanguageModule.CheckDocumentation(var boolCascade : Boolean);

Var
  i : Integer;
  strDate : String;
  dtDate, dtFileDate : TDateTime;
  Tag : TTag;

Begin
  If (ModuleComment <> Nil) And (ModuleComment.FindTag('stopdocumentation') >= 0) Then
    Begin
      boolCascade := False;
      Exit;
    End;
  If ((ModuleComment = Nil) Or (ModuleComment.TokenCount = 0)) And
    (doShowUndocumentedModule In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([], ModuleNameLine, ModuleNameCol, ModuleComment,
      DocConflictTable[dctModuleMissingDocumentation]);
  If ModuleComment <> Nil Then
    Begin
      If (doShowMissingModuleDate In BrowseAndDocItOptions.Options) Then
        Begin
          i := ModuleComment.FindTag('date');
          If (i = -1) Or (ModuleComment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, ModuleComment,
              DocConflictTable[dctModuleMissingDate])
          Else
            Begin
              Tag := ModuleComment.Tag[i];
              strDate := Tag.AsString(False);
              If Modified Then
                dtFileDate := Now
              Else
                {$IFDEF VER180}
                FileAge(FileName, dtFileDate);
                {$ELSE}
                dtFileDate := FileDateToDateTime(FileAge(Module.FileName));
                {$ENDIF}
              Try
                dtDate := ConvertDate(strDate);
                If Int(dtDate) <> Int(dtFileDate) Then
                  AddDocumentConflict([strDate, FormatDateTime('dd/mmm/yyyy', dtFileDate)],
                    Tag.Line, Tag.Column, ModuleComment, DocConflictTable[dctModuleIncorrectDate]);
              Except
                AddDocumentConflict([strDate, FormatDateTime('dd/mmm/yyyy', dtFileDate)],
                  Tag.Line, Tag.Column, ModuleComment, DocConflictTable[dctModuleCheckDateError]);
              End
            End;
        End;
      If (doShowMissingModuleVersion In BrowseAndDocItOptions.Options) Then
        Begin
          i := ModuleComment.FindTag('version');
          If (i = -1) Or (ModuleComment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, ModuleComment,
              DocConflictTable[dctModuleMissingVersion])
        End;
      If (doShowMissingModuleAuthor In BrowseAndDocItOptions.Options) Then
        Begin
          i := ModuleComment.FindTag('author');
          If (i = -1) Or (ModuleComment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, ModuleComment,
              DocConflictTable[dctModuleMissingAuthor])
        End;
    End;
  Inherited CheckDocumentation(boolCascade);
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean

**)
Procedure TGenericTypeDecl.CheckDocumentation(var boolCascade : Boolean);

Begin
  If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
    Begin
      If doShowUndocumentedTypes In BrowseAndDocItOptions.Options Then
        AddDocumentConflict([Identifier], Line, Column, Comment,
          DocConflictTable[dctTypeClauseUndocumented]);
    End;
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean

**)
Procedure TGenericConstant.CheckDocumentation(var boolCascade : Boolean);

Begin
  If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
    Begin
      If doShowUndocumentedConsts In BrowseAndDocItOptions.Options Then
        AddDocumentConflict([Identifier], Line, Column, Comment,
          DocConflictTable[dctConstantClauseUndocumented]);
    End;
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean

**)
Procedure TGenericVariable.CheckDocumentation(var boolCascade : Boolean);

Begin
  If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
    Begin
      If doShowUndocumentedVars In BrowseAndDocItOptions.Options Then
        AddDocumentConflict([Identifier], Line, Column, Comment,
          DocConflictTable[dctVariableClauseUndocumented]);
    End;
End;

(**

  This method checks the method passed against the method comments tags and
  highlights missing parameter comments, return tags and missing descriptions.

  @precon  Method is the method declaration that requires checking for document
           conflicts.
  @postcon The passed method is systematicaly check for errors.

  @param   boolCascade as a Boolean

**)
procedure TGenericMethodDecl.CheckDocumentation(var boolCascade : Boolean);

Begin
  If Not FForwardDecl And (Identifier <> '') Then
    Begin
      CheckMethodDocumentation;
      If Comment <> Nil Then
        Begin
          CheckMethodParamCount;
          CheckMethodParameters;
          CheckMethodReturns;
        End;
    End;
  Inherited CheckDocumentation(boolCascade);
end;

(**

  This method check the given method for general document problems, i.e.
  missing or no description.

  @precon  Method is valid method declaration to be checked for documentation.
  @postcon Checks the passed method for docuemntation errors.

**)
Procedure TGenericMethodDecl.CheckMethodDocumentation;

Begin
  If Comment = Nil Then
    Begin
      If doShowMissingProcDocs In BrowseAndDocItOptions.Options Then
        AddDocumentConflict([QualifiedName], Line, Column, Comment,
          DocConflictTable[dctMethodUndocumented]);
      Exit;
    End;
  If (Comment.TokenCount = 0) And
    (doShowMissingDocDesc In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([QualifiedName], Line, Column, Comment,
      DocConflictTable[dctMethodHasNoDesc]);
End;

(**

  This method checks the given method for the correct number of parameters and
  tags.

  @precon  Method is a method declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed method for errors in the parameter count
           documentation.

**)
Procedure TGenericMethodDecl.CheckMethodParamCount;

Var
  i, j, k : Integer;

Begin
  j := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'param' Then
      Inc(j);
  k := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'precon' Then
      Begin
        Inc(k);
        If Comment.Tag[i].TokenCount = 0 Then
          AddDocumentConflict([QualifiedName], Comment.Tag[i].Line,
            Comment.Tag[i].Column, Comment,
            DocConflictTable[dctMethodPreconNotDocumented]);
      End;
  If (ParameterCount <> j) And
    (doShowDiffParamCount In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([QualifiedName], Line, Column, Comment,
      DocConflictTable[dctMethodDiffParamCount]);
  If (k < 1) And (doShowMissingPreCons In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([QualifiedName], Line, Column, Comment,
      DocConflictTable[dctMethodMissingPreCon]);
  If (k > 1) And (doShowMissingPreCons In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([QualifiedName], Line, Column, Comment,
      DocConflictTable[dctMethodTooManyPrecons]);
End;

(**

  This method checks the given method for the correct parameter tags and
  pre conditions.

  @precon  Method is a method declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed method for errors in the parameter documentation.

**)
Procedure TGenericMethodDecl.CheckMethodParameters;

Var
  i, j : Integer;
  iFound : Integer;
  strType : String;
  strParam: String;

Begin
  For i := 0 To ParameterCount - 1 Do
    Begin
      // Parameter name
      iFound := -1;
      With Comment Do
        For j := 0 To TagCount - 1 Do
          If (LowerCase(Tag[j].TagName) = 'param') And (Tag[j].TokenCount > 0) And
            (LowerCase(Tag[j][0]) = Lowercase(Parameters[i].Identifier)) Then
            Begin
              iFound := j;
              Break;
            End;
      If (iFound = -1) And
        (doShowUndocumentedParams In BrowseAndDocItOptions.Options) Then
        AddDocumentConflict([Parameters[i].Identifier, QualifiedName],
          Line, Column, Comment, DocConflictTable[dctMethodUndocumentedParam]);
      // Parameter type
      If iFound > -1 Then
        With Comment Do
          Begin
            strType := '';
            If Tag[iFound].TokenCount > 3 Then
              If AnsiCompareText(Tag[iFound].Token[3], 'ARRAY') = 0 Then
                Begin
                  If Tag[iFound].TokenCount > 5 Then
                    strType := Tag[iFound].Token[5];
                End Else
                  strType := Tag[iFound].Token[3];
            strParam := Parameters[i].ParamReturn;
            If Not (LowerCase(strType) = Lowercase(strParam)) Then
              If (doShowIncorrectParamType In BrowseAndDocItOptions.Options) Then
                AddDocumentConflict([Parameters[i].Identifier, QualifiedName],
                  Line, Column, Comment, DocConflictTable[dctMethodIncorrectParamType]);
          End;
    End;
End;

(**

  This method checks the given method for the correct return information and
  tags.

  @precon  Method is a method declaration that needs the be check for document
           conflicts.
  @postcon The passed method return is checked for errors.

**)
Procedure TGenericMethodDecl.CheckMethodReturns;

Var
  i, j, k : Integer;
  iFound : Integer;

Begin
  iFound := -1;
  k := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'postcon' Then
      Begin
        Inc(k);
        If Comment.Tag[i].TokenCount = 0 Then
          AddDocumentConflict([QualifiedName], Comment.Tag[i].Line,
            Comment.Tag[i].Column, Comment, DocConflictTable[dctMethodPostconNotDocumented]);
      End;
  If MethodType = mtFunction Then
    Begin;
      If (ReturnType.AsString <> '') Then
        Begin
          With Comment Do
            For j := 0 To TagCount - 1 Do
              If AnsiCompareText(Tag[j].TagName, 'return') = 0 Then
                Begin
                  iFound := j;
                  Break;
                End;
        End;
      If (iFound = -1) And
        (doShowUndocumentedReturn In BrowseAndDocItOptions.Options) Then
        AddDocumentConflict([QualifiedName], Line, Column,
          Comment, DocConflictTable[dctMethodUndocumentedReturn])
      Else
        Begin
          If ((Comment.Tag[iFound].TokenCount < 2) Or
            (AnsiCompareText(ReturnType.AsString, Comment.Tag[iFound][1]) <> 0)) And
            (doShowIncorrectReturnType In BrowseAndDocItOptions.Options) Then
            AddDocumentConflict([QualifiedName], Line, Column, Comment,
            DocConflictTable[dctMethodIncorrectReturntype]);
        End;
    End;
  iFound := 0;
  If (k = 0) And (doShowMissingPostCons in BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([QualifiedName], Line, Column, Comment,
      DocConflictTable[dctMethodMissingPostCon]);
  If doShowMissingPostCons in BrowseAndDocItOptions.Options Then
    If (k > 1) And (iFound <> -1) Then
      AddDocumentConflict([QualifiedName], Line, Column, Comment,
        DocConflictTable[dctMethodTooManyPostCons]);
End;

(**

  This method check the given property for all the approriate documentation.

  @precon  Cls is the class declaration that the property belongs too and Prop
           is valid property declaration to be checked for documentation.
  @postcon Checks the passed property for errors in the docuemntation tags.

  @param   boolCascade as a Boolean

**)
procedure TGenericProperty.CheckDocumentation(var boolCascade: Boolean);

begin
  CheckPropertyDocumentation;
  If Comment <> Nil Then
    Begin
      CheckPropertyParamCount;
      CheckPropertyParameters;
      CheckPropertyReturns;
    End;
end;

(**

  This method check the given property for general document problems, i.e.
  missing or no description.

  @precon  Cls is the class declaration that the property belongs too and Prop is
           valid property declaration to be checked for documentation.
  @postcon The passed property of the passed class is checked for documentation
           errors.

**)
Procedure TGenericProperty.CheckPropertyDocumentation;

Begin
  If Comment = Nil Then
    Begin
      If doShowMissingPropertyDoc In BrowseAndDocItOptions.Options Then
        AddDocumentConflict([Identifier], Line, Column, Comment,
        DocConflictTable[dctPropertyUndocumented]);
      Exit;
    End;
  If (Comment.TokenCount = 0) And
    (doShowMissingPropDocDesc In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([Identifier], Line, Column, Comment,
      DocConflictTable[dctPropertyHasNoDesc]);
End;

(**

  This method check the given property for the correct parameter count and pre
  and post condition count.

  @precon  Cls is the class declaration that the property belongs too and Prop is
           valid property declaration to be checked for documentation.
  @postcon The passed property is checked for errors in its parameters.

**)
Procedure TGenericProperty.CheckPropertyParamCount;

Var
  i, j, k : Integer;

Begin
  j := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'param' Then
      Inc(j);
  k := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'precon' Then
      Begin
        Inc(k);
        If Comment.Tag[i].TokenCount = 0 Then
          AddDocumentConflict(['Property', Identifier], Comment.Tag[i].Line,
            Comment.Tag[i].Column, Comment,
            DocConflictTable[dctPropertyPreconNotDocumented]);
      End;
  If (ParameterCount <> j) And
    (doShowDiffPropParamCount In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([Identifier], Line, Column, Comment,
    DocConflictTable[dctPropertyDiffParamCount]);
  If (k < 1) And (doShowMissingPropPreCons In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([Identifier], Line, Column, Comment,
    DocConflictTable[dctPropertyMissingPreCon]);
  If (k > 1) And (doShowMissingPropPreCons In BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([Identifier], Line, Column, Comment,
    DocConflictTable[dctPropertyTooManyPreCons]);
End;

(**

  This method check the given property for correctness of the parameter
  documentation.

  @precon  Cls is the class declaration that the property belongs too and Prop
           is valid property declaration to be checked for documentation.
  @postcon The passed property is check for errors in the parameter
           docuemntation.

**)
Procedure TGenericProperty.CheckPropertyParameters;

Var
  i, j : Integer;
  iFound : Integer;
  strType : String;
  strParam: String;

Begin
  For i := 0 To ParameterCount - 1 Do
    Begin
      // Parameter name
      iFound := -1;
      With Comment Do
        For j := 0 To TagCount - 1 Do
          If (LowerCase(Tag[j].TagName) = 'param') And (Tag[j].TokenCount > 0) And
            (LowerCase(Tag[j][0]) = Lowercase(Parameters[i].Identifier)) Then
            Begin
              iFound := j;
              Break;
            End;
      If (iFound = -1) And (doShowUndocPropParam In BrowseAndDocItOptions.Options) Then
        AddDocumentConflict([Parameters[i].Identifier, Identifier], Line, Column,
          Comment, DocConflictTable[dctPropertyUndocumentedParam]);
      // Parameter type
      If iFound > -1 Then
        With Comment Do
          Begin
            strType := '';
            If Tag[iFound].TokenCount > 3 Then
              If AnsiCompareText(Tag[iFound].Token[3], 'ARRAY') = 0 Then
                Begin
                  If Tag[iFound].TokenCount > 5 Then
                    strType := Tag[iFound].Token[5]
                End Else
                  strType := Tag[iFound].Token[3];
            strParam := Parameters[i].ParamReturn;
            If Not ((LowerCase(strType) = Lowercase(strParam))) Then
              If doShowIncorrectPropParamType In BrowseAndDocItOptions.Options Then
                AddDocumentConflict([Parameters[i].Identifier, Identifier], Line,
                  Column, Comment, DocConflictTable[dctPropertyIncorrectParamType]);
        End;
    End;
End;

(**

  This method check the given property for the correct return parameter.

  @precon  Cls is the class declaration that the property belongs too and Prop
           is valid property declaration to be checked for documentation.
  @postcon Checks the passed property for returns documentation errors.

**)
Procedure TGenericProperty.CheckPropertyReturns;

Var
  i, j, k : Integer;
  iFound : Integer;

Begin
  iFound := -1;
  k := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'postcon' Then
      Begin
        Inc(k);
        If Comment.Tag[i].TokenCount = 0 Then
          AddDocumentConflict([Identifier], Comment.Tag[i].Line,
            Comment.Tag[i].Column, Comment,
            DocConflictTable[dctPropertyPostconNotDocumented]);
      End;
  If (TypeId <> Nil) Then
    Begin
      With Comment Do
        For j := 0 To TagCount - 1 Do
          If (LowerCase(Tag[j].TagName) = 'return') Then
            Begin
              iFound := j;
              Break;
            End;
    End;
  If (iFound = -1) And (doShowUndocPropReturn in BrowseAndDocItOptions.Options) Then
    Begin
      If TypeId <> Nil Then
        AddDocumentConflict([Identifier], Line, Column, Comment,
        DocConflictTable[dctPropertyUndocumentedReturn])
    End Else
    Begin
      If ((Comment.Tag[iFound].TokenCount < 2) Or
        (AnsiCompareText(TypeId.AsString, Comment.Tag[iFound][1]) <> 0)) And
        (doShowIncorrectPropReturnType In BrowseAndDocItOptions.Options) Then
        AddDocumentConflict([Identifier], Line, Column, Comment,
        DocConflictTable[dctPropertyIncorrectReturnType]);
    End;
  If doShowMissingPropPostCons in BrowseAndDocItOptions.Options Then iFound := 0;
  If (k = 0) And (doShowMissingPropPostCons in BrowseAndDocItOptions.Options) Then
    AddDocumentConflict([Identifier], Line, Column, Comment,
    DocConflictTable[dctPropertyMissingPostCon]);
  If doShowMissingPropPostCons in BrowseAndDocItOptions.Options Then
    If (k > 1) And (iFound <> -1) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
      DocConflictTable[dctPropertyTooManyPostCons]);
End;


(**

  This is the constructor method for the TBrowseAndDocItOptions class.

  @precon  None.
  @postcon Does nothing at the moment.

**)
Constructor TBrowseAndDocItOptions.Create;

Begin
  Inherited Create;
  FDefines := TStringList.Create;
End;

(**

  This is the destructor method for the TBrowseAndDocItOptions class.

  @precon  none.
  @postcon Does onthing at the moment except call the inherited destroy method.

**)
Destructor TBrowseAndDocItOptions.Destroy;

Begin
  FDefines.Free;
  Inherited Destroy;
End;

(** This initializations section ensures that there is a valid instance of the
    SpecialTags class and a valid instance of the BrowseAndDocItOption class. **)
Initialization
  iDocConflictCounter := 1;
  objModuleRootElement := Nil;
  SpecialTags := TStringList.Create;
  // Create a default set of Special Tags.
  SpecialTags.AddObject('todo=Things To Do', TObject(iShowInTree And iAutoExpand));
  SpecialTags.AddObject('precon=Pre-Conditions', TObject(0));
  SpecialTags.AddObject('postcon=Post-Conditions', TObject(0));
  SpecialTags.AddObject('param=Parameters', TObject(0));
  SpecialTags.AddObject('return=Returns', TObject(0));
  SpecialTags.AddObject('note=Notes', TObject(0));
  SpecialTags.AddObject('see=Also See', TObject(0));
  SpecialTags.AddObject('exception=Exception Raised', TObject(0));
  SpecialTags.AddObject('bug=Known Bugs', TObject(iShowInTree And iAutoExpand));
  SpecialTags.AddObject('debug=Debugging Code', TObject(iShowInTree And iAutoExpand));
  SpecialTags.AddObject('date=Date Code Last Updated', TObject(0));
  SpecialTags.AddObject('author=Code Author', TObject(0));
  SpecialTags.AddObject('version=Code Version', TObject(0));
  BrowseAndDocItOptions := TBrowseAndDocItOptions.Create;
(** This finalization section ensures that the SpecialTags and
    BrowseAndDocItOptions class are destroyed. **)
Finalization
  BrowseAndDocItOptions.Free;
  SpecialTags.Free;
End.
