(**

  This module contains the base class for all language module to derived from
  and all standard constants across which all language modules have in common.

  @Date    27 Jul 2016
  @Version 1.0
  @Author  David Hoyle

**)
Unit BaseLanguageModule;

Interface

Uses
  SysUtils, Classes, Contnrs, Graphics;


{$INCLUDE CompilerDefinitions.inc}

Type
  (** Type to distinguish Stream position from token index. **)
  TStreamPosition = Integer;
  (** Type to distinguish Stream position from token index. **)
  TTokenIndex = Integer;
  (** An enumerate type to define the stream status and token types. **)
  TBADITokenType = (ttUnknown, ttWhiteSpace, ttReservedWord, ttIdentifier,
    ttNumber, ttSymbol, ttLineEnd, ttSingleLiteral, ttDoubleLiteral,
    ttLineComment, ttBlockComment, ttHTMLStartTag, ttHTMLEndTag, ttDirective,
    ttCompilerDirective, ttLinkTag, ttTreeHeader, ttFileEnd, ttLineContinuation,
    ttCustomUserToken, ttExplorerHighlight);
  (** An enumerate for the scoping of identifiers. **)
  TScope = (scNone, scGlobal, scLocal, scPrivate, scProtected, scPublic,
    scPublished, scFriend);
  (** A set to represent combinations of scopes. **)
  TScopes = Set Of TScope;
  (** An enumerate for the parameter modifiers of methods. **)
  TParamModifier = (pamNone, pamVar, pamConst, pamOut);
  (** An enumerate for the types of modules that can be parsed. **)
  TModuleType = (mtProgram, mtPackage, mtLibrary, mtUnit);
  (** An enumerate for the different methods. **)
  TMethodType = (mtConstructor, mtDestructor, mtProcedure, mtFunction, mtOperator);
  (** An enumerate for warning and errors. **)
  TErrorType = (etHint, etWarning, etError);
  (** A type to return an array of strings **)
  TKeyWords = Array of String;
  (** A type for a set of AnsiChar - used for the function IsInSet() **)
  TSetOfAnsiChar = Set Of AnsiChar;

  (** This is a list of options valable for the display of module information
      **)
  TDocOption = (
    doCustomDrawing,
    doShowCommentHints,

    doShowErrors,
    doShowWarnings,
    doShowHints,
    doShowConflicts,

    doShowUndocumentedTypes,
    doShowUndocumentedRecords,
    doShowUndocumentedObjects,
    doShowUndocumentedClasses,
    doShowUndocumentedInterfaces,
    doShowUndocumentedVars,
    doShowUndocumentedConsts,
    doShowUndocumentedFields,
    doShowUndocumentedClassDecls,

    doShowUndocumentedModule,
    doShowMissingModuleDate,
    doShowCheckModuleDate,
    doShowMissingModuleVersion,
    doShowMissingModuleAuthor,

    doShowMethodMissingDocs,
    doShowMethodMissingDocDesc,
    doShowMethodDiffParamCount,
    doShowMethodUndocumentedParams,
    doShowMethodIncorrectParamType,
    doShowMethodUndocumentedReturn,
    doShowMethodIncorrectReturnType,
    doShowMethodMissingPreCons,
    doShowMethodMissingPostCons,

    doShowPropertyMissingDoc,
    doShowPropertyMissingDocDesc,
    doShowPropertyDiffPropParamCount,
    doShowPropertyUndocumentedParams,
    doShowPropertyIncorrectParamType,
    doShowPropertyUndocumentedReturn,
    doShowPropertyIncorrectReturnType,
    doShowPropertyMissingPreCons,
    doShowPropertyMissingPostCons,

    doShowMissingInitComment,
    doShowMissingFinalComment,

    {doShowIDEErrorsOnSuccessfulParse,}
    doShowParserErrorOrigin,
    doShowUnReferencedSymbols,
    doShowPerformanceCountersInModuleExplorer,
    doShowPrefCountersInDocSummary,
    doStrictConstantExpressions,
    doShowMissingVBExceptionWarnings,
    doAddPreAndPostToComment
  );

  (** An enumerate to associate images with different types of Elements. **)
  TBADIImageIndex = (
    iiNone,

    iiModule,

    iiDocConflictFolder,
    iiDocConflictIncorrect,
    iiDocConflictItem,
    iiDocConflictMissing,

    iiErrorFolder,
    iiError,
    iiWarningFolder,
    iiWarning,

    iiHintFolder,
    iiHint,

    iiUsesLabel,
    iiUsesItem,

    iiPublicTypesLabel,
    iiPrivateTypesLabel,
    iiPublishedTypesLabel,
    iiProtectedTypesLabel,
    iiLocalTypesLabel,
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
    iiLocalField,
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

    iiPublicConstantsLabel,
    iiPrivateConstantsLabel,
    iiPublishedConstantsLabel,
    iiProtectedConstantsLabel,
    iiLocalConstantsLabel,
    iiPublicConstant,
    iiPrivateConstant,
    iiPublishedConstant,
    iiProtectedConstant,
    iiLocalConstant,

    iiPublicResourceStringsLabel,
    iiPrivateResourceStringsLabel,
    iiPublishedResourceStringsLabel,
    iiProtectedResourceStringsLabel,
    iiLocalResourceStringsLabel,
    iiPublicResourceString,
    iiPrivateResourceString,
    iiPublishedResourceString,
    iiProtectedResourceString,
    iiLocalResourceString,

    iiPublicVariablesLabel,
    iiPrivateVariablesLabel,
    iiPublishedVariablesLabel,
    iiProtectedVariablesLabel,
    iiLocalVariablesLabel,
    iiPublicVariable,
    iiPrivateVariable,
    iiPublishedVariable,
    iiProtectedVariable,
    iiLocalVariable,

    iiPublicThreadVarsLabel,
    iiPublicThreadVar,
    iiPrivateThreadVar,
    iiPublishedThreadVar,
    iiProtectedThreadVar,
    iiLocalThreadVar,

    iiPublicClassVariablesLabel,
    iiPrivateClassVariablesLabel,
    iiPublishedClassVariablesLabel,
    iiProtectedClassVariablesLabel,
    iiLocalClassVariablesLabel,
    iiPublicClassVariable,
    iiPrivateClassVariable,
    iiPublishedClassVariable,
    iiProtectedClassVariable,
    iiLocalClassVariable,

    iiExportedHeadingsLabel,

    iiExportedFunctionsLabel,
    iiPublicExportedFunction,
    iiPrivateExportedFunction,
    iiPublishedExportedFunction,
    iiProtectedExportedFunction,
    iiLocalExportedFunction,

    iiPublicLabelsLabel,
    iiPrivateLabelsLabel,
    iiPublishedLabelsLabel,
    iiProtectedLabelsLabel,
    iiLocalLabelsLabel,
    iiPublicLabel,
    iiPrivateLabel,
    iiPublishedLabel,
    iiProtectedLabel,
    iiLocalLabel,

    iiImplementedMethods,
    iiMethodsLabel,

    iiInitialization,
    iiFinalization,

    iiToDoFolder,
    iiToDoItem,

    iiUnknownClsObj
  );

  (** This is a set of display options. **)
  TDocOptions = Set of TDocOption;

  (** This is an enumerate to define the options for the parsing of a module. **)
  TModuleOption = (moParse, moCheckForDocumentConflicts, moProfiling);
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
    dctFieldClauseUndocumented,

    dctClassClauseUndocumented,
    dctRecordClauseUndocumented,
    dctObjectClauseUndocumented,
    dctInterfaceClauseUndocumented,
    dctDispInterfaceClauseUndocumented,

    dctFunctionUndocumented,
    dctFunctionHasNoDesc,
    dctFunctionPreconNotDocumented,
    dctFunctionDiffParamCount,
    dctFunctionMissingPreCon,
    dctFunctionTooManyPrecons,
    dctFunctionUndocumentedParam,
    dctFunctionIncorrectParamType,
    dctFunctionPostconNotDocumented,
    dctFunctionUndocumentedReturn,
    dctFunctionIncorrectReturntype,
    dctFunctionReturnNotRequired,
    dctFunctionMissingPostCon,
    dctFunctionTooManyPostCons,

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

  (** This enumerate defermines the status of the token's reference resolution. **)
  TTokenReference = (trUnknown, trUnresolved, trResolved);

  (** A type of a set of Characters. **)
  TSymbols = Set Of AnsiChar;

  (** This is a class the store information about each token **)
  TTokenInfo = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FToken : String;
    FColumn : Integer;
    FBufferPos: Integer;
    FLine: Integer;
    FLength : Integer;
    FTokenType: TBADITokenType;
    FUToken : String;
    FReference : TTokenReference;
  Public
    Constructor Create(strToken : String; iPos, iLine, iCol,
      iLength : Integer; TType : TBADITokenType); Overload;
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
      @return  a TBADITokenType
    **)
    Property TokenType : TBADITokenType read FTokenType;
    (**
      This property gets and sets the reference information for the token.
      @precon  None.
      @postcon Gets and sets the reference information for the token.
      @return  a TTokenReference
    **)
    Property Reference : TTokenReference Read FReference Write FReference;
  End;

  (** this class defines an object that can contain tokens and has line and
      column numbers. It is the anscester for TTag, TComment and
      TElementContainer. **)
  TBaseContainer = Class {$IFDEF D2005} Abstract {$ENDIF}
  {$IFDEF D2005} Strict {$ENDIF} Private
    FName : String;
    FLine : Integer;
    FColumn : Integer;
    FTokens : TObjectList;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetTokenCount : Integer;
    Function GetTokens(iIndex : Integer) : TTokenInfo;
    Function GetName: String; Virtual;
    Procedure SetName(Value : String); Virtual;
  Public
    Constructor Create(strName : String; iLine, iColumn  : Integer);
    Destructor Destroy; Override;
    Procedure AddToken(strToken : String; ATokenType : TBADITokenType = ttUnknown);
      Overload; Virtual;
    Procedure AddToken(AToken : TTokenInfo); Overload; Virtual;
    Procedure AppendToken(AToken : TTokenInfo); Virtual;
    Procedure InsertToken(strToken : String; iIndex : Integer;
      ATokenType : TBADITokenType = ttUnknown);
    Procedure DeleteToken(iIndex : Integer);
    Procedure ClearTokens;
    Function BuildStringRepresentation(boolIdentifier, boolForDocumentation : Boolean;
      strDelimiter : String; iMaxWidth : Integer;
      strNoSpaceBefore : TSymbols = ['(', '[', '{', ')', ']', '}', ';', ',', '.', '!', '?'];
      strNoSpaceAfter : TSymbols = ['(', '[', '{', '.', '^'];
      strSpaceAfter : TSymbols = ['=', ':', '+', '-', '*', '\'];
      boolShowHTML : Boolean = False) : String; Virtual;
    (**
      This property returns the name of the element.
      @precon  None.
      @postcon Returns the name of the element.
      @return  a String
    **)
    Property Name : String Read GetName Write SetName;
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
  End;

  (** A class to hold text about a single tag **)
  TTag = Class(TBaseContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetTagName : String;
  Public
    Constructor Create(strName : String; iLine, iColumn : Integer); Overload;
    Destructor Destroy; Override;
    Function AsString(iMaxWidth : Integer; boolShowHTML : Boolean) : String;
    (**
      Returns the tag name as a string.
      @precon  None.
      @postcon Returns the tag name as a string.
      @return  a String
    **)
    Property TagName : String Read GetTagName;
  End;

  (** A class the handles and stores all the comment information **)
  TComment = Class(TBaseContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FTags : TObjectList;
    FTagMode : Boolean;
    FLastTag : TTag;
    FTagLine : Integer;
    FTagColumn : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function GetTag(iTagIndex: Integer): TTag;
    function GetTagCount: Integer;
    Procedure ParseComment(strComment : String);
    Procedure ResetTagMode;
  Public
    Constructor Create(srcComment : TComment); Overload;
    Constructor Create(strComment : String; iLine, iCol : Integer); Overload;
    Destructor Destroy; Override;
    Class Function CreateComment(strComment : String; iLine,
      iCol : Integer) : TComment; Virtual;
    Procedure AddToken(strToken : String; iType : TBADITokenType = ttUnknown); Override;
    Procedure Assign(srcComment : TComment); Overload;
    Procedure Assign(strComment : String); Overload;
    Function AsString(iMaxWidth : Integer; boolShowHTML : Boolean) : String;
    Function FindTag(strTagName : String) : Integer;
    Procedure TrimTrailingWhiteSpace;
    Procedure AppendComment(BaseCmt, Source : TComment);
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
  End;

  (** A class type for comment parsers. **)
  TCommentClass = Class Of TComment;

  (** A record to describe document conflict information. **)
  TDocConflictTable = Record
    FMessage      : String;
    FDescription  : String;
    FConflictType : TDocConflictIcon;
  End;

  TDocIssue = Class;

  (** This is a class reference for the TElementContainer class such that the
      descendant classes can clone themselves. **)
  TElementContainerClass = Class Of TElementContainer;

  TLabelContainer = Class;

  (** This enumerate defind the type of information to find. **)
  TFindType = (ftName, ftIdentifier);

  (** This class implements the IElementCollection interface so that this
      element container can be rendered with the module browser. **)
  TElementContainer = Class {$IFDEF D2005} Abstract {$ENDIF} (TBaseContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FElements : TObjectList;
    FComment : TComment;
    FScope : TScope;
    FImageIndex : TBADIImageIndex;
    FSorted  : Boolean;
    FReferenced : Boolean;
    FParent : TElementContainer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementCount : Integer;
    Function GetElements(iIndex : Integer) : TElementContainer;
    Function GetImageIndexAdjustedForScope : Integer;
    Function Find(strName : String; FindType : TFindType = ftName) : Integer;
    Procedure SetSorted(boolValue : Boolean);
    Function FindRoot : TElementContainer;
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Virtual;
    Destructor Destroy; Override;
    Function  Add(AElement : TElementContainer) : TElementContainer; Overload; Virtual;
    Function  Add(Token : TTokenInfo; AScope : TScope; AImageIndex : TBADIImageIndex;
      AComment : TComment) : TElementContainer; Overload; Virtual;
    Function  Add(strToken : String; AImageIndex : TBADIImageIndex;
      AScope : TScope; AComment : TComment) : TElementContainer; Overload; Virtual;
    Function AddUnique(AElement : TElementContainer) : TElementContainer; Virtual;
    Procedure AddTokens(AElement : TElementContainer); Virtual;
    Function  FindElement(strName : String; FindType : TFindType = ftName) : TElementContainer;
    Procedure Assign(Source : TElementContainer); Virtual;
    Function  FindToken(strToken : String) : Integer;
    Procedure DeleteElement(iIndex : Integer);
    Procedure CheckDocumentation(var boolCascade : Boolean); Virtual;
    Function  ReferenceSymbol(AToken : TTokenInfo) : Boolean; Virtual;
    Procedure AddIssue(strMsg : String; AScope : TScope; strMethod : String;
      iLine, iCol : Integer; ErrorType : TErrorType);
    Procedure AddDocumentConflict(Const Args: Array of Const;
      iIdentLine, iIdentColumn : Integer; AComment : TComment;
      strCategory : String; DocConflictRec : TDocConflictTable);
    Function  AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Virtual; Abstract;
    Procedure CheckReferences; Virtual;
    Function ReferenceSection(AToken : TTokenInfo; Section: TLabelContainer) : Boolean;
    (**
      This property returns the number of elements in the collection.
      @precon  None.
      @postcon Returns the number of elements in the collection.
      @return  an Integer
    **)
    Property ElementCount : Integer Read GetElementCount;
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
      This property returns the comment that is associated with this element.
      @precon  None.
      @postcon Returns the comment that is associated with this element.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
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
      @return  a TBADIImageIndex
    **)
    Property ImageIndex : TBADIImageIndex Read FImageIndex Write FImageIndex;
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
    (**
      This property determines if the symbol has been referenced in code.
      @precon  None.
      @postcon Gets or sets the referenced property.
      @return  a Boolean
    **)
    Property Referenced  : Boolean Read FReferenced Write FReferenced;
    (**
      This property returns the parent container of this object. A return of
      nil means that there is no parent.
      @precon  None.
      @postcon Returns the parent container of this object. A return of
               nil means that there is no parent.
      @return  a TElementContainer
    **)
    Property Parent  : TElementContainer Read FParent;
  End;

  (** This class defines a document error. **)
  TDocIssue = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMsg: String;
    FMethod : String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    (**
      Returns the error method of the error stored.
      @precon  None.
      @postcon Returns the error method of the error stored.
      @return  a String
    **)
    Property Method : String Read FMethod;
    (**
      Returns the error message.
      @precon  None.
      @postcon Returns the error message.
      @return  a String
    **)
    Property Msg : String Read FMsg;
  Public
    Constructor Create(strMsg : String; AScope : TScope; strMethod : String; iLine,
      iCol : Integer; AImageIndex : TBADIImageIndex); Reintroduce; Overload;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This class represents a single identifier with line, col and comment
      attributes. **)
  TIdent = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer);

  (** This is a sub class for all types **)
  TGenericTypeDecl = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a sub class for all constants **)
  TGenericConstant = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a sub class for all variables **)
  TGenericVariable = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This class represents a parameter of a method declaration. **)
  TGenericParameter = Class (TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FParamModifier : TParamModifier;
    FArrayOf : Boolean;
    FParamType : TGenericTypeDecl;
    FDefaultValue : String;
  Public
    Constructor Create(ParamMod : TParamModifier; Ident : String;
      boolArrayOf : Boolean; AType : TGenericTypeDecl; Value : String;
      AScope : TScope; iLine, iCol : Integer); ReIntroduce; Overload;
    Destructor Destroy; Override;
    Function IsEquals(Parameter : TGenericParameter) : Boolean; Virtual;
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

  (** This class is an ancester for both methods and properties so that they
      can be handled generically (parameters and returntypes). **)
  TGenericFunction = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FParameters   : TObjectList;
    FReturnType   : TGenericTypeDecl;
    FStartLine    : Integer;
    FEndLine      : Integer;
    FHasProfiling : Boolean;
    FIndent       : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetQualifiedName : String; Virtual; Abstract;
    Function GetParameterCount : Integer;
    Function GetParameters(iIndex : Integer) : TGenericParameter;
    Function RequiresReturn : Boolean; Virtual; Abstract;
    Function FunctionType : String; Virtual; Abstract;
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Destructor Destroy; Override;
    Procedure AddParameter(AParameter : TGenericParameter);
    Function LineofCode : Integer;
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
      Returns the Qualified name of the method.
      @precon  None.
      @postcon Returns the Qualified name of the method.
      @return  a String
    **)
    Property QualifiedName : String Read GetQualifiedName;
    (**
      This property gets and sets the start line number for the code within the
      function.
      @precon  None.
      @postcon Gets and sets the start line number for the code within the
               function.
      @return  an Integer
    **)
    Property StartLine : Integer Read FStartLine Write FStartLine;
    (**
      This property gets and sets the end line number for the code within the
      function.
      @precon  None.
      @postcon Gets and sets the end line number for the code within the
               function.
      @return  an Integer
    **)
    Property EndLine : Integer Read FEndLine Write FEndLine;
    (**
      This property gets and sets whether the function has code profiling
      instrumentation installed.
      @precon  None.
      @postcon Gets and sets whether the function has code profiling
               instrumentation installed.
      @return  a Boolean
    **)
    Property HasProfiling : Boolean Read FHasProfiling Write FHasProfiling;
    (**
      This property gets and sets the Indent of the method.
      @precon  None.
      @postcon Gets and sets the Indent of the method.
      @return  an Integer
    **)
    Property Indent : Integer Read FIndent Write FIndent;
  End;

  (** A type to define sub classes of TGenericFunction **)
  TGenericFunctionClass = Class Of TGenericFunction;

  (** This class represents a method declaration. **)
  TGenericMethodDecl = Class {$IFDEF D2005} Abstract {$ENDIF} (TGenericFunction)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMethodType : TMethodType;
    FClassNames : TStringList;
    FMsg: String;
    FExt: String;
    FClassMethod : Boolean;
    FAlias: String;
    FForwardDecl : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    procedure SetMsg(const Value: String);
    procedure SetExt(const Value: String);
    Function GetQualifiedName : String; Override;
    Function RequiresReturn : Boolean; Override;
    Function FunctionType : String; Override;
    procedure CheckMethodDocumentation;
    procedure CheckMethodParamCount;
    Procedure CheckMethodParameters;
    Procedure CheckMethodReturns;
  Public
    Constructor Create(MethodType : TMethodType; strName : String; AScope : TScope;
      iLine, iCol : Integer); ReIntroduce; Virtual;
    Destructor Destroy; Override;
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
      @return  a TStringList
    **)
    Property ClassNames : TStringList Read FClassNames;
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
  TGenericProperty = Class {$IFDEF D2005} Abstract {$ENDIF} (TGenericFunction)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function GetQualifiedName: String; Override;
    Function RequiresReturn : Boolean; Override;
    Function FunctionType : String; Override;
    procedure CheckPropertyDocumentation;
    procedure CheckPropertyParamCount;
    Procedure CheckPropertyParameters;
    Procedure CheckPropertyReturns;
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

  (** This is a class to represent a module documentation conflict. **)
  TDocumentConflict = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMessage       : String;
    FCommentLine   : Integer;
    FCommentColumn : Integer;
  Public
    Constructor Create(Const Args: Array of Const; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      strDocConflictMsg, strDocConflictDesc : String;
      AImageIndex : TBADIImageIndex); ReIntroduce;
    Destructor Destroy; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
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
  TCharSet = Set of AnsiChar;

  (** A type to define the type of token search. **)
  TSeekToken = (stActual, stFirst);

  (** A class to hold a 64 bit tick count against a name. **)
  TTickOption = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FName  : String;
    FCount : Int64;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strName : String; iCount : Int64);
    (**
      This property returns the name of the tick counter.
      @precon  None.
      @postcon Returns the name of the tick counter.
      @return  a String
    **)
    Property Name : String Read FName;
    (**
      This property returns the value of the tick counter.
      @precon  None.
      @postcon Returns the value of the tick counter.
      @return  an Int64
    **)
    Property Count : Int64 Read FCount;
  End;

  (** A type to define an array of integers. **)
  TArrayOfInteger = Array Of Integer;

  (** This is an abtract class from which all language modules should be
      derived. **)
  TBaseLanguageModule = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOwnedItems : TObjectList;
    FTokenIndex : TTokenIndex;
    FDocErrors: TElementContainer;
    FTickList : TObjectList;
    FModuleName : String;
    FBodyComment : TObjectList;
    FModuleNameCol: Integer;
    FModuleNameLine: Integer;
    FFileName: String;
    FModified : Boolean;
    FCompilerDefs : TStringList;
    FPreviousTokenIndex : TTokenIndex;
    FCompilerConditionStack : TList;
    FCompilerConditionUndoStack : TList;
    FLastComment: TTokenInfo;
    FCommentClass : TCommentClass;
    FShouldUndoCompilerStack: Boolean;
    FLastBodyCommentLine: Integer;
    FModuleOptions : TModuleOptions;
    FTokenStack : TArrayOfInteger;
    FTokenStackTop : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetToken : TTokenInfo;
    function GetOpTickCountName(iIndex: Integer): String;
    function GetOpTickCountByIndex(iIndex: Integer): Int64;
    function GetOpTickCounts: Integer;
    function GetOpTickCount(strStart, strFinish : String): Int64;
    Function GetBodyComment(iIndex : Integer) : TComment;
    Function GetBodyCommentCount : Integer;
    Function PrevToken : TTokenInfo;
    Procedure NextToken;
    Procedure PreviousToken;
    Function EndOfTokens : Boolean;
    Procedure NextNonCommentToken; Virtual;
    Procedure RollBackToken; deprecated;
    Procedure PushTokenPosition;
    Procedure PopTokenPosition;
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Virtual; Abstract;
    Procedure SetTokenIndex(iIndex : TTokenIndex);
    procedure AppendToLastToken(strToken : String);
    procedure ProcessCompilerDirective(var iSkip : Integer); Virtual; Abstract;
    Function GetModuleName : String; Virtual;
    function GetBytes: Int64;
    function GetLines: Integer;
    Procedure ErrorAndSeekToken(strMsg, strMethod, strParam : String;
      SeekTokens: Array Of String; SeekToken : TSeekToken);
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
      This property provide access to a list of compiler defines as a string
      list.
      @precon  None.
      @postcon Provide a string list of compiler defines {$DEFINE xxxxx}
      @return  a TStringList
    **)
    Property CompilerDefines : TStringList Read FCompilerDefs;
    (**
      This property returns the current index of the current Token.
      @precon  None.
      @postcon Returns the current index of the current Token.
      @return  a TTokenIndex
    **)
    Property TokenIndex : TTokenIndex Read FTokenIndex;
    (**
      This property returns the comment class type for the parser.
      @precon  None.
      @postcon Returns the comment class type for the parser.
      @return  a TCommentClass
    **)
    Property CommentClass : TCommentClass Read FCommentClass Write FCommentClass;
  Public
    Constructor CreateParser(Source : String; strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Virtual;
    Destructor Destroy; Override;
    Procedure AddTickCount(strLabel : String);
    Procedure AddDef(strDef : String);
    Procedure DeleteDef(strDef : String);
    Function  IfDef(strDef : String) : Boolean;
    Function  IfNotDef(strDef : String) : Boolean;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function  ReservedWords : TKeyWords; Virtual; Abstract;
    Function  Directives : TKeyWords; Virtual; Abstract;
    Function  AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure AddToExpression(Container : TElementContainer);
    function  IsToken(strToken: String; Container: TElementContainer): Boolean;
    Procedure AddBodyComment(C : TComment);
    Function  DefaultProfilingTemplate : String; Virtual;
    { Properties }
    (**
      This property returns the tick count time between the 2 named tick counts
      previously stored using the AddTickCount() method.
      @precon  None.
      @postcon Returns the time between two counter if both the names are found.
      @param   strStart  as       a String
      @param   strFinish as       a String
      @return  an Int64
    **)
    Property OpTickCount[strStart, strFinish : String] : Int64 Read GetOpTickCount;
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
      @return  an Int64
    **)
    Property OpTickCountByIndex[iIndex : Integer] : Int64 Read GetOpTickCountByIndex;
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
    (**
      This property defines a compiler condition stack for use in the
      ProcessCompilerDefintions method.
      @precon  None.
      @postcon Provides access to the compiler condition stack.
      @return  a TList
    **)
    Property CompilerConditionStack : TList Read FCompilerConditionStack;
    (**
      This property defines a compiler condition undo stack for use in the
      ProcessCompilerDefintions method.
      @precon  None.
      @postcon Provides access to the compiler condition undo stack.
      @return  a TList
    **)
    Property CompilerConditionUndoStack : TList Read FCompilerConditionUndoStack;
    (**
      This property returns the number of bytes in the file.
      @precon  None.
      @postcon Returns the number of bytes in the file.
      @return  an Int64
    **)
    Property Bytes : Int64 Read GetBytes;
    (**
      This property returns the number of lines in the file.
      @precon  None.
      @postcon Returns the number of lines in the file.
      @return  an Integer
    **)
    Property Lines : Integer Read GetLines;
    (**
      This property exposes the Module Options for the module.
      @precon  None.
      @postcon Returns the Module Options for the module.
      @return  a TModuleOptions
    **)
    Property ModOptions : TModuleOptions Read FModuleOptions;
  End;

  (** This enumerate define the position of the editor when an item is selected
      in the module explorer. **)
  TBrowsePosition = (bpCommentTop, bpCommentCentre, bpIdentifierTop,
    bpIdentifierCentre, bpIdentifierCentreShowAllComment);

  (** A record to define the font information for each token type. **)
  TTokenFontInfo = Record
    FForeColour : TColor;
    FStyles     : TFontStyles;
    FBackColour : TColor;
  End;

  (** This is a class to define a set of options for the application. **)
  TBrowseAndDocItOptions = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOptions : TDocOptions;
    FDefines : TStringList;
    FSpecialTags : TStringList;
    FExpandedNodes : TStringList;
    FINIFileName : String;
    FUpdateInterval: Cardinal;
    FScopesToRender : TScopes;
    FBrowsePosition : TBrowsePosition;
    FFontName : String;
    FFontSize : Integer;
    FTokenFontInfo : Array[Low(TBADITokenType)..High(TBADITokenType)] Of TTokenFontInfo;
    FExcludeDocFiles : TStringList;
    FMethodDescriptions : TStringList;
    FScopesToDocument : TScopes;
    FModuleExplorerBGColour : TColor;
    FTokenLimit : Integer;
    FMaxDocOutputWidth: Integer;
    FManagedNodesLife : Integer;
    FTreeColour : TColor;
    FProfilingCode : TStringList;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetTokenFontInfo(ATokenType  : TBADITokenType) : TTokenFontInfo;
    Procedure SetTokenFontInfo(ATokenType  : TBADITokenType; ATokenFontInfo : TTokenFontInfo);
    Procedure LoadSettings;
    function  GetProfilingCode(Module : TBaseLanguageModule): String;
    procedure SetProfilingCode(Module : TBaseLanguageModule; const strValue: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure SaveSettings;
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
    (**
      This property provide access to the special tags string list.
      @precon  None.
      @postcon Provide access to the special tags string list.
      @return  a TStringList
    **)
    Property SpecialTags : TStringList Read FSpecialTags;
    (**
      This property provide access to the expanded nodes string list.
      @precon  None.
      @postcon Provide access to the expanded nodes string list.
      @return  a TStringList
    **)
    Property ExpandedNodes : TStringList Read FExpandedNodes;
    (**
      This property determines the amount of time in milliseonds between the
      last editor update and the next refresh. Interval only, the application
      needs to implement the logic.
      @precon  None.
      @postcon Gets and sets the update interval.
      @return  a Cardinal
    **)
    Property UpdateInterval : Cardinal Read FUpdateInterval Write FUpdateInterval;
    (**
      This property determines the scopes to render in the module explorer.
      @precon  None.
      @postcon Gets and sets the scopes to render.
      @return  a TScopes
    **)
    Property ScopesToRender : TScopes Read FScopesToRender Write FScopesToRender;
    (**
      This property determines the behaviour of the positioning of the cursor
      in the editor window.
      @precon  None.
      @postcon Gets and sets the browse position.
      @return  a TBrowsePosition
    **)
    Property BrowsePosition : TBrowsePosition Read FBrowsePosition Write FBrowsePosition;
    (**
      This property determines the Font Name of the Module explorer.
      @precon  None.
      @postcon Gets or sets the module explorer font name.
      @return  a String
    **)
    Property FontName : String Read FFontName Write FFontName;
    (**
      This property determines the font size of the module explorer font.
      @precon  None.
      @postcon Gets or sets the module explorer font size.
      @return  an Integer
    **)
    Property FontSize : Integer Read FFontSize Write FFontSize;
    (**
      This property determines the colour and style attribute of a token in the
      module explorer
      @precon  None.
      @postcon Gets and sets the colour and style of the token.
      @param   ATokenType as       a TBADITokenType
      @return  a TTokenFontInfo
    **)
    Property TokenFontInfo[ATokenType : TBADITokenType] : TTokenFontInfo Read
      GetTokenFontInfo Write SetTokenFontInfo;
    (**
      This properrty holds a list of files / partial or full which should not be
      documented.
      @precon  None.
      @postcon Gets and sets the list.
      @return  a TStringList
    **)
    Property ExcludeDocFiles : TStringList Read FExcludeDocFiles;
    (**
      This property stores a list of method descriptions related to pattern
      matches.
      @precon  None.
      @postcon Gets and sets the method list.
      @return  a TStringList
    **)
    Property MethodDescriptions : TStringList Read FMethodDescriptions;
    (**
      This property determines the scopes to document.
      @precon  None.
      @postcon Gets and sets the scopes to document.
      @return  a TScopes
    **)
    Property ScopesToDocument : TScopes Read FScopesToDocument Write FScopesToDocument;
    (**
      This gets and sets the background colour for the Module explorer.
      @precon  None.
      @postcon Gets and sets the background colour for the Module explorer.
      @return  a TColor
    **)
    Property BGColour : TColor Read FModuleExplorerBGColour Write FModuleExplorerBGColour;
    (**
      This property gets ans sets the token limit to the module explorer.
      @precon  None.
      @postcon Gets ans sets the token limit to the module explorer.
      @return  an Integer
    **)
    Property TokenLimit : Integer Read FTokenLimit Write FTokenLimit;
    (**
      This property gets and sets the maximum width of the code output in
      documentation.
      @precon  None.
      @postcon Gets and sets the maximum width of the code output in
               documentation.
      @return  an Integer
    **)
    Property MaxDocOutputWidth : Integer Read FMaxDocOutputWidth Write FMaxDocOutputWidth;
    (**
      This property gets and sets the period of time in days which managed nodes
      are alive for.
      @precon  None.
      @postcon Gets and sets the period of time in days which managed nodes
               are alive for.
      @return  an Integer
    **)
    Property ManagedNodesLife : Integer Read FManagedNodesLife Write FManagedNodesLife;
    (**
      This property gets and sets the colour of the explorer tree lines.
      @precon  None.
      @postcon Gets and sets the colour of the explorer tree lines.
      @return  a TColor
    **)
    Property TreeColour : TColor Read FTreeColour Write FTreeColour;
    (**
      This property returns the name of the inifile.
      @precon  None.
      @postcon Returns the name of the inifile.
      @return  a String
    **)
    Property INIFileName : String Read FINIFileName;
    (**
      This property gets and sets the profiling code for the given filenames.
      @precon  None.
      @postcon Gets and sets the profiling code for the given filenames.
      @param   Module as a TBaseLanguageModule
      @return  a String
    **)
    Property ProfilingCode[Module : TBaseLanguageModule] : String Read GetProfilingCode
      Write SetProfilingCode;
  End;

  (** A class to represent a label within the Module Explorer / Documentation **)
  TLabelContainer = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** A class type to define classes in the record structure. **)
  TBaseLanguageModuleClass = Class Of TBaseLanguageModule;

  (** This emunerate descibed the different types of doc comment .**)
  TCommentStyle = (csBlock, csLine, csInSitu);
  (** An enumerate to define the type of comment output that can be generated by
      WriteComment. **)
  TCommentType = (ctNone, ctPascalBlock, ctPascalBrace, ctCPPBlock, ctCPPLine,
    ctVBLine, ctXML);

  (** A class to hold the information required by the system for each registered module
      parser. **)
  TModuleInfo = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FExt       : String;
    FCls       : TBaseLanguageModuleClass;
    FCanDoc    : Boolean;
    FBlockCmt  : TCommentType;
    FLineCmt   : TCommentType;
    FInSituCmt : TCommentType;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strExt : String; Cls : TBaseLanguageModuleClass;
      boolCanDoc : Boolean; iBlockCmt, iLineCmt, iInSituCmt : TCommentType);
    (**
      This property returns the extension associated with the registration.
      @precon  None.
      @postcon Returns the extension associated with the registration.
      @return  a String
    **)
    Property Ext : String Read FExt;
    (**
      This property returns the class reference for this registration.
      @precon  None.
      @postcon Returns the class reference for this registration.
      @return  a TBaseLanguageModuleClass
    **)
    Property Cls : TBaseLanguageModuleClass Read FCls;
    (**
      This property returns whether the registration supports documentation.
      @precon  None.
      @postcon Returns whether the registration supports documentation.
      @return  a Boolean
    **)
    Property CanDoc : Boolean Read FCanDoc;
    (**
      This property returns the comment type for blocks.
      @precon  None.
      @postcon Returns the comment type for blocks.
      @return  a TCommentType
    **)
    Property BlockCmt : TCommentType Read FBlockCmt;
    (**
      This property returns the comment type for lines.
      @precon  None.
      @postcon Returns the comment type for lines.
      @return  a TCommentType
    **)
    Property LineCmt : TCommentType Read FLineCmt;
    (**
      This property returns the comment type for In Situ.
      @precon  None.
      @postcon Returns the comment type for In Situ.
      @return  a TCommentType
    **)
    Property InSituCmt : TCommentType Read FInSituCmt;
  End;

  (** A class to handle all the registered modules in the system. **)
  TModuleDispatcher = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FModules : TObjectList;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function Find(strExt : String) : Integer;
    Function GetCount : Integer;
    Function GetModules(iIndex : Integer) : TModuleInfo;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(strExt : String; Cls : TBaseLanguageModuleClass; boolCanDoc : Boolean;
      iBlockCmt, iLineCmt, iInSituCmt : TCommentType);
    Function Dispatcher(Source : String; strFileName : String;
      boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;
    Function CanParseDocument(strFileName : String) : Boolean;
    Function CanDocumentDocument(strFileName : String) : Boolean;
    Function GetCommentType(strFileName : String;
      CommentStyle : TCommentStyle) : TCommentType;
    (**
      This property returns a TModuleInfo reference for the indexed module.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon Returns a TModuleInfo reference for the indexed module.
      @param   iIndex as an Integer
      @return  a TModuleInfo
    **)
    Property Modules[iIndex : Integer] : TModuleInfo Read GetModules;
    (**
      This property returns the number of registered modules in the dispatcher.
      @precon  None.
      @postcon Returns the number of registered modules in the dispatcher.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** A silent parser abort exception. **)
  EParserAbort = Class(Exception);

ResourceString
  (** Options text for Draw Syntax Highlighted Module Explorer **)
  strDrawSynHighModuleExplorer = 'Draw Syntax Highlighted Module Explorer';
  (** Options text for Show comments in the hints **)
  strShowCommentsInTheHints = 'Show comments in the hints';
  (** Options text for Show Errors **)
  strShowErrors = 'Show Module Errors';
  (** Options text for Show Warnings **)
  strShowWarnings = 'Show Module Warnings';
  (** Options text for Show Hints **)
  strShowHints = 'Show Module Hints';
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
  (** Options text for Show Undocumented Fields **)
  strShowUndocumentedFields = 'Show Undocumented Fields';
  (** Options text for Show Undocumented Class Decls **)
  strShowUndocumentedClassDecls = 'Show Undocumented Class Types, Vars and Consts';
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
  (** Options text for Showing IDE error messages when no parser messages. **)
  {strShowIDEErrorsOnSuccessfulParse = 'Show IDE Error messages if parser is successfully.';}
  (** Options text for showing the origin method of the parser errors. **)
  strShowParserErrorOrigin = 'Show the origin method of the Parser error.';
  (** Options text for showing unreferenced locals and privates. **)
  strShowUnreferencedSymbols = 'Show all unreferenced symbols.';
  (** Options text for showing preformance counters in the module explorer. **)
  strShowPerfCountersInModuleExplorer = 'Show performance counters in the statusbar of the module explorer.';
  (** Options text for showing preformance counters in the documentation summary. **)
  strShowPerfCountersInDocSummary = 'Show performance counters in the documenation summary.';
  (** Options text for strict evaluation of constant expressions. **)
  strStrictConstantExpressions = 'Strict evaluation of constant expressions.';
  (** Options text for showing missing VB/VBA exception warnings. **)
  strShowMissingVBExceptionWarnings = 'Show missing VB/VBA exception warnings.';
  (** Options text for adding pre and post conditions to  comments. **)
  strAddpreAndPostToComments = 'Add Pre and Post Conditions to Comments.';

  (** Label for Documentation Conflicts **)
  strDocumentationConflicts = 'Documentation Conflicts';
  (** Errors label **)
  strErrors = 'Errors';
  (** Warnings label **)
  strWarnings = 'Warnings';
  (** Hints label **)
  strHints = 'Hints';
  (** Label for Uses Clause **)
  strUses = 'Uses';
  (** Label for Types Clause **)
  strTypesLabel = 'Types';
  (** Label for Constants Clause **)
  strConstantsLabel = 'Constants';
  (** Label for Resource Strings Clause **)
  strResourceStringsLabel = 'Resource Strings';
  (** Label for Variables Clause **)
  strVarsLabel = 'Variables';
  (** Label for Class Variables Clause **)
  strClassVarsLabel = 'Class Variables';
  (** Label for Thread Variables Clause **)
  strThreadVarsLabel = 'Thread Variables';
  (** Label for Exported Headings **)
  strExportedHeadingsLabel = 'Exported Headings';
  (** Label for Exports Clause **)
  strExportsLabel = 'Exports';
  (** Label for Implemented Methods **)
  strImplementedMethodsLabel = 'Implemented Methods';
  (** Label for Requires Clause **)
  strRequiresLabel = 'Requires';
  (** Label for Contains Clause **)
  strContainsLabel = 'Contains';
  (** Label for Initialization Clause **)
  strInitializationLabel = 'Initialization';
  (** Label for Finalization Clause **)
  strFinalizationLabel = 'Finalization';
  (** Label for Labels **)
  strLabelsLabel = 'Labels';
  (** Label for fields **)
  strFieldsLabel = 'Fields';
  (** Label for Properties **)
  strPropertiesLabel = 'Properties';
  (** Label for Methods. **)
  strMethodsLabel = 'Methods';

  (** Exception message an unexpected start of file. **)
  strUnExpectedStartOfFile = 'Unexpected start-of-file.';
  (** Exception message for an unexpected end of file. **)
  strUnExpectedEndOfFile = 'Unexpected end-of-file.';
  (** Error message when an identifier is expected but something else is found. **)
  strIdentExpected = 'Identifier expected but ''%s'' found at line %d column %d.';
  (** Error message when an string is expected but something else is found. **)
  strStringExpected = 'String literal expected but ''%s'' found at line %d column %d.';
  (** Error message when an number is expected but something else is found. **)
  strNumberExpected = 'Number expected but ''%s'' found at line %d column %d.';
  (** Error message when an reserved word is expected but something else is
      found. **)
  strReservedWordExpected = 'Expected ''%s'' but ''%s'' found at line %d column %d.';
  (** Error message when an literal character is expected but something else
      is found. **)
  strLiteralExpected = '''%s'' expected but ''%s'' found at line %d column %d.';
  (** Warning for a function not having a return parameter. **)
  strFunctionWarning = 'Function ''%s'' does not have a return type specified.';
  (** An error message for a non defined help file option. **)
  strHelpFileNotDefined = 'There is no help file specified. Please specified a ' +
    'help file in the options dialogue.';
  (** An error message for a missing help file **)
  strHelpFileNotFound = 'The help file ''%s'' was not found.';
  (** An error message for an undeclared class method. **)
  strUndeclaredClassMethod = 'Method ''%s'' has not been declared.';
  (** An error message for an unsatisfied forward reference. **)
  strUnSatisfiedForwardReference = 'Method ''%s'' has an unsatisfied ' +
    'forward reference.';
  (** An error message for a type not found. **)
  strTypeNotFound = 'Type declaration missing but found ''%s'' at line %d column %d.';
  (** An error message when a TypeID is expected. **)
  strTypeIDExpected = 'A TypeID was expected but found ''%s'' at line %d column %d.';
  (** An execption message when a Expr conflict occurs in an expression **)
  strExprConflict = 'The token ''%s'' conflicts with the TYPE of the preceeding ' +
    'expression at line %d column %d.';
  (** An error message if a function is used in a constant expression **)
  strConstExprDesignator = 'The token ''%s'' at line %d column %d is not allowed ' +
    'in a Constant Expression.';
  (** An error message if the first none comment token is not Program,
      Package, Unit or Library. **)
  strModuleKeyWordNotfound = '''%s'' found but module starting keyword PROGRAM, ' +
    'PACKAGE, UNIT or LIBRARY not found.';
  (** An error message for an undefined token in the stream. **)
  strUnDefinedToken = 'The token ''%s'' at line %d column %d is not defined.';
  (** An error message for an $ELSE without a string $IFDEF / $FIFNDEF **)
  strElseIfMissingIfDef = '$ELSE is missing a starting $IFDEF or $IFNDEF at ' +
    'line %d column %d.';
  (** An error message for an $ENDIF without a string $IFDEF / $FIFNDEF **)
  strEndIfMissingIfDef = '$ENDIF is missing a starting $IFDEF or $IFNDEF at ' +
    'line %d column %d.';
  (** An error message for an unknown compiler directive. **)
  strUnknownComDir = 'Unknown compiler directive ''%s'' at line %d column %d.';
  (** An error message for an Ordinal Type not found. **)
  strOrdinalTypeExpected = 'Ordinal type expected but ''%s'' found at line %d ' +
    'column %d.';
  (** An error message for a Type Declaration not found. **)
  strTypeDeclExpected = 'Type Declaration expected but ''%s'' found at line %s ' +
    'column %d.';
  (** An error message for a Label not found. **)
  strLabelExpected = 'Label expected but ''%s'' found at line %s column %d.';
  (** An error message for a Constant Expression found. **)
  strConstExprExpected = 'Constant Expression expected but ''%s'' found at ' +
    'line %d column %d.';
  (** Document conflict message for a unreferenced locals. **)
  strUnreferencedLocal = 'The symbol ''%s'' has not been referenced in the code.';
  (** This is an error message for not enough tokens. **)
  strNotEnoughStrings = 'Not enough strings passed to ErrorAndSeekToken().';

  {----------------------- Documentation Conflict Messages --------------------}

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
  strModuleMissingDate = 'This module is missing a documentation date (''%s'').';
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

  (** This is the tree branch under which field documentation error appear **)
  strFieldDocumentation = 'Field Documentation';
  (** Document conflict message for an undocumented field clause item. **)
  strFieldClauseUndocumented = 'Field ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented Field
      clause item. **)
  strFieldClauseUndocumentedDesc = 'Each Field declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'field is used for.';

  (** This is the tree branch under which record documentation error appear **)
  strRecordDocumentation = 'Record Documentation';
  (** Document conflict message for an undocumented record clause item. **)
  strRecordClauseUndocumented = 'Record type ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented record clause
      item. **)
  strRecordClauseUndocumentedDesc = 'Each Record declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'record represents.';

  (** This is the tree branch under which object documentation error appear **)
  strObjectDocumentation = 'Object Documentation';
  (** Document conflict message for an undocumented object clause item. **)
  strObjectClauseUndocumented = 'Object type ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented object clause
      item. **)
  strObjectClauseUndocumentedDesc = 'Each Object declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'object represents.';

  (** This is the tree branch under which class documentation error appear **)
  strClassDocumentation = 'Class Documentation';
  (** Document conflict message for an undocumented class variable clause item. **)
  strClassClauseUndocumented = 'Class type ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented class variable
      clause item. **)
  strClassClauseUndocumentedDesc = 'Each Class declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'class represents.';

  (** This is the tree branch under which interface documentation error appear **)
  strInterfaceDocumentation = 'Interface Documentation';
  (** Document conflict message for an undocumented interface variable clause item. **)
  strInterfaceClauseUndocumented = 'Interface type ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented interface
      variable clause item. **)
  strInterfaceClauseUndocumentedDesc = 'Each Interface declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'interface represents.';

  (** This is the tree branch under which dispinterface documentation error appear **)
  strDispInterfaceDocumentation = 'DispInterface Documentation';
  (** Document conflict message for an undocumented dispinterface variable clause item. **)
  strDispInterfaceClauseUndocumented = 'DispInterface type ''%s'' is undocumented.';
  (** Document conflict message description for an undocumented dispinterface
      variable clause item. **)
  strDispInterfaceClauseUndocumentedDesc = 'Each DispInterface declaration ' +
    'should have a short description which attempts to decribed what the ' +
    'dispinterface represents.';

  (** Label for Method Documentation Conflicts **)
  strFunctionDocumentation = '%s Documentation';
  (** Document conflict message for missing method documentation. **)
  strFunctionUndocumented = '%s ''%s'' has not been documented.';
  (** Document conflict message description for missing method documentation. **)
  strFunctionUndocumentedDesc = 'Each method or property declaration should ' +
    'have a description which should provide information to future developers ' +
    'regarding the purpose of the method or property. # #In addition to ' +
    'the description each method or property should have a pre-condition statement ' +
    '(@@precon) and a post-condition statement (@@postcon). # #Along with these ' +
    'there should be a list of the parameters and any return types.';
  (** Document conflict message for missing method description. **)
  strFunctionHasNoDesc = '%s ''%s'' has no description.';
  (** Document conflict message descritpion for missing method description. **)
  strFunctionHasNoDescDesc = 'Each method or property declaration should have ' +
    'a description which should provide information to furture developers ' +
    'regarding the purpose of the method or property.';

  (** Document conflict message for different number of parameters and tags. **)
  strFunctionDiffParamCount = '%s ''%s'' has a different parameter count (%d not %d).';
  (** Document conflict message description for different number of parameters
      and tags. **)
  strFunctionDiffParamCountDesc = 'There are a different number of @@param tags ' +
    'in the comment compared to the prameters passed to the method or property.';
  (** Document conflict message for an undocumented parameter. **)
  strFunctionUndocumentedParam = 'Parameter ''%s'' in %s ''%s'' is not ' +
    'documented.';
  (** Document conflict message description for an undocumented parameter. **)
  strFunctionUndocumentedParamDesc = 'The specified parameter in the documented ' +
    'method or property does not have a corresponding @@param tag in the ' +
    'comment header.';
  (** Document conflict message for an incorrect parameter type. **)
  strFunctionIncorrectParamType = 'The parameter type for ''%s'' in %s ''%s'' is ' +
    'incorrect (''%s'').';
  (** Document conflict message description for an incorrect parameter type. **)
  strFunctionIncorrectParamTypeDesc = 'The type of the specified parameter ' +
    'differents from the type provided in the @@param tag of the method or ' +
    'property comment.';

  (** Document conflict message for an undocumented return type. **)
  strFunctionUndocumentedReturn = '%s ''%s''`s return type is not documented.';
  (** Document conflict message descritpion for an undocumented return type. **)
  strFunctionUndocumentedReturnDesc = 'A return type requires an @@return tag ' +
    'in the method or property comment.';
  (** Document conflict message for an incorrect return type. **)
  strFunctionIncorrectReturnType = '%s ''%s''`s return type is incorrect (''%s'').';
  (** Document conflict message description for an incorrect return type. **)
  strFunctionIncorrectReturnTypeDesc = 'The type of the return is not the same ' +
    'as the type defined in the method or property.';
  (** Document conflict message for a return not required. **)
  strFunctionReturnNotRequired = '%s ''%s''`s return type is not required.';
  (** Document conflict message description for a return not required. **)
  strFunctionReturnNotRequiredDesc = 'The type of the return is not ' +
    'required for this type of method or property.';

  (** A documentation message for missing precondition text. **)
  strFunctionPreConNotDocumented = 'A pre-condition in %s ''%s'' is not ' +
    'documented.';
  (** A documentation message description for missing precondition text. **)
  strFunctionPreConNotDocumentedDesc = 'The @@precon tag in the specified method ' +
    'or property is either not present or does not contain a statement. A ' +
    'pre-condition statement says something about the status of the input ' +
    'parameters for the method or property which must be valid for the method ' +
    'or property to function correctly.';
  (** Document conflict message for a missing pre-condition tag. **)
  strFunctionMissingPreCon = '%s ''%s'' has missing pre-condition tags.';
  (** Document conflict message description for a missing pre-condition tag. **)
  strFunctionMissingPreConDesc = 'The method or property comment expected an ' +
    '@@precon tag which says something about the status of the input ' +
    'parameters for the method or property which must be valid for the method ' +
    'or property to function correctly.';
  (** Document conflict message for too many pre-condition tag. **)
  strFunctionTooManyPreCons = '%s ''%s'' has too many pre-condition tags.';
  (** Document conflict message description for too many pre-condition tag. **)
  strFunctionTooManyPreConsDesc = 'The method or property comment has too many ' +
    'pre-condition tags (@@precon).';

  (** A documentation message for missing postcondition text. **)
  strFunctionPostConNotDocumented = 'A post-condition in %s ''%s'' is not ' +
    'documented.';
  (** A documentation message description for missing postcondition text. **)
  strFunctionPostConNotDocumentedDesc = 'The @@prepost tag in the specified ' +
    'method or property is either not present or does not contain a ' +
    'statement. A post-condition statement says something about the status of ' +
    'the output from the method or property which will be valid for the method ' +
    'or property after being called.';
  (** Document conflict message for a missing post-condition tag. **)
  strFunctionMissingPostCon = '%s ''%s'' has a missing post-condition tag.';
  (** Document conflict message description for a missing post-condition tag. **)
  strFunctionMissingPostConDesc = 'The method or property comment expected an ' +
    '@@postcon tag which says something about the status of the out of the ' +
    'method or property which will be valid after the method or property is ' +
    'called.';
  (** Document conflict message for too many post-condition tag. **)
  strFunctionTooManyPostCons = '%s ''%s'' has too many post-condition tags.';
  (** Document conflict message description for too many post-condition tag. **)
  strFunctionTooManyPostConsDesc = 'The method or property comment has too many ' +
    'post-condition tags (@@postcon).';

  (** Label for Finalization Documentation Conflicts **)
  strModuleInitSection = 'Module Initialization Section';
  (** Document conflict message for a missing Finalialization Comment. **)
  strMissingInitComment = 'The module is missing an Initialization comment.';
  (** Document conflict message description a missing Finalialization Comment. **)
  strMissingInitCommentDesc = 'It is usually advised able to document the ' +
    'code contain in the Initialization section of the module so that ' +
    'developers known which portion of the module are automatically ' +
    'created.';

  (** Label for Initialization Documentation Conflicts **)
  strModuleFinalSection = 'Module Finalization Section';
  (** Document conflict message for a missing Initialization Comment. **)
  strMissingFinalComment = 'The module is missing an Finalization comment.';
  (** Document conflict message description a missing Initialization Comment. **)
  strMissingFinalCommentDesc = 'It is usually advised able to document the ' +
    'code contain in the Finalization section of the module so that ' +
    'developers known which portion of the module are automatically ' +
    'destroyed.';

Const
  (** A set of characters for whitespace **)
  strWhiteSpace : Set Of AnsiChar = [#32, #9];
  (** A set of characters for line feed and carriage return **)
  strLineEnd : Set of AnsiChar = [#10, #13];

  (** This is a string array representing the TDocOption enumerates. **)
  DocOptionInfo : Array[Low(TDocOption)..High(TDocOption)] Of TDocOptionRec = (
    (FDescription : strDrawSynHighModuleExplorer;          FEnabled : False),
    (FDescription : strShowCommentsInTheHints;             FEnabled : False),

    (FDescription : strShowErrors;                         FEnabled : True),
    (FDescription : strShowWarnings;                       FEnabled : False),
    (FDescription : strShowHints;                          FEnabled : False),
    (FDescription : strShowDocumentationConflicts;         FEnabled : False),


    (FDescription : strShowUndocumentedTypes;              FEnabled : False),
    (FDescription : strShowUndocumentedRecords;            FEnabled : False),
    (FDescription : strShowUndocumentedObjects;            FEnabled : False),
    (FDescription : strShowUndocumentedClasses;            FEnabled : False),
    (FDescription : strShowUndocumentedInterfaces;         FEnabled : False),
    (FDescription : strShowUndocumentedVariables;          FEnabled : False),
    (FDescription : strShowUndocumentedConstants;          FEnabled : False),
    (FDescription : strShowUndocumentedFields;             FEnabled : False),
    (FDescription : strShowUndocumentedClassDecls;         FEnabled : False),

    (FDescription : strShowUndocumentedModule;             FEnabled : True),
    (FDescription : strShowMissingModuleDate;              FEnabled : False),
    (FDescription : strShowCheckModuleDate;                FEnabled : False),
    (FDescription : strShowMissingModuleVersion;           FEnabled : False),
    (FDescription : strShowMissingModuleAuthor;            FEnabled : False),

    (FDescription : strShowMissingMethodDocumentation;     FEnabled : True),
    (FDescription : strShowMissingMethodDocDesc;           FEnabled : True),
    (FDescription : strShowDiffMethodParameterCount;       FEnabled : True),
    (FDescription : strShowUndocumentedMethodParameters;   FEnabled : True),
    (FDescription : strShowIncorrectMethodParameterType;   FEnabled : True),
    (FDescription : strShowUndocumentedMethodReturn;       FEnabled : True),
    (FDescription : strShowIncorrectMethodReturnType;      FEnabled : True),
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
    (FDescription : strShowMissingFinalComment;            FEnabled : False),

    {(FDescription : strShowIDEErrorsOnSuccessfulParse;     FEnabled : False),}
    (FDescription : strShowParserErrorOrigin;              FEnabled : False),
    (FDescription : strShowUnreferencedSymbols;            FEnabled : False),
    (FDescription : strShowPerfCountersInModuleExplorer;   FEnabled : False),
    (FDescription : strShowPerfCountersInDocSummary;       FEnabled : False),
    (FDescription : strStrictConstantExpressions;          FEnabled : True),
    (FDescription : strShowMissingVBExceptionWarnings;     FEnabled : False),
    (FDescription : strAddPreAndPostToComments;            FEnabled : False)
  );

  (** This is a default set of font information for the application. **)
  strTokenTypeInfo : Array[Low(TBADITokenType)..High(TBADITokenType)] Of TTokenFontInfo = (
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [fsBold]; FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [fsBold]; FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clMaroon;   FStyles : [fsBold]; FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clBlack;    FStyles : [];       FBackColour: clWindow),
    (FForeColour : clInfoText; FStyles : [];       FBackColour: clInfoBk)
  );

  (** This is a constant for special tag items to show in the tree **)
  iShowInTree = $0001;
  (** This is a constant for special tag items to auto expand in the tree **)
  iAutoExpand = $0002;
  (** This is a constant for special tag items to show in the documentation **)
  iShowInDoc = $0004;

  (** This is a list of Image Resource name to be loaded fom the executable. **)
  BADIImageList : Array[Succ(Low(TBADIImageIndex))..High(TBADIImageIndex)] Of TImageIndexInfo = (
    (FResourceName : 'Module';                        FMaskColour: clLime),

    (FResourceName : 'DocConflictFolder';             FMaskColour: clLime),
    (FResourceName : 'DocConflictIncorrect';          FMaskColour: clLime),
    (FResourceName : 'DocConflictItem';               FMaskColour: clLime),
    (FResourceName : 'DocConflictMissing';            FMaskColour: clLime),

    (FResourceName : 'ErrorFolder';                   FMaskColour: clLime),
    (FResourceName : 'Error';                         FMaskColour: clLime),
    (FResourceName : 'WarningFolder';                 FMaskColour: clLime),
    (FResourceName : 'Warning';                       FMaskColour: clLime),
    (FResourceName : 'HintFolder';                    FMaskColour: clFuchsia),
    (FResourceName : 'Hint';                          FMaskColour: clFuchsia),

    (FResourceName : 'UsesLabel';                     FMaskColour: clLime),
    (FResourceName : 'UsesItem';                      FMaskColour: clLime),

    (FResourceName : 'PublicTypesLabel';              FMaskColour: clLime),
    (FResourceName : 'PrivateTypesLabel';             FMaskColour: clLime),
    (FResourceName : 'PublishedTypesLabel';           FMaskColour: clLime),
    (FResourceName : 'ProtectedTypesLabel';           FMaskColour: clLime),
    (FResourceName : 'LocalTypesLabel';               FMaskColour: clLime),
    (FResourceName : 'PublicType';                    FMaskColour: clLime),
    (FResourceName : 'PrivateType';                   FMaskColour: clLime),
    (FResourceName : 'PublishedType';                 FMaskColour: clLime),
    (FResourceName : 'ProtectedType';                 FMaskColour: clLime),
    (FResourceName : 'LocalType';                     FMaskColour: clLime),

    (FResourceName : 'RecordsLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicRecord';                  FMaskColour: clLime),
    (FResourceName : 'PrivateRecord';                 FMaskColour: clLime),
    (FResourceName : 'PublishedRecord';               FMaskColour: clLime),
    (FResourceName : 'ProtectedRecord';               FMaskColour: clLime),
    (FResourceName : 'LocalRecord';                   FMaskColour: clLime),

    (FResourceName : 'FieldsLabel';                   FMaskColour: clLime),
    (FResourceName : 'LocalField';                    FMaskColour: clLime),
    (FResourceName : 'PublicField';                   FMaskColour: clLime),
    (FResourceName : 'PrivateField';                  FMaskColour: clLime),
    (FResourceName : 'PublishedField';                FMaskColour: clLime),
    (FResourceName : 'ProtectedField';                FMaskColour: clLime),

    (FResourceName : 'ObjectsLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicObject';                  FMaskColour: clLime),
    (FResourceName : 'PrivateObject';                 FMaskColour: clLime),
    (FResourceName : 'PublishedObject';               FMaskColour: clLime),
    (FResourceName : 'ProtectedObject';               FMaskColour: clLime),
    (FResourceName : 'LocalObject';                   FMaskColour: clLime),

    (FResourceName : 'PublicConstructor';             FMaskColour: clLime),
    (FResourceName : 'PrivateConstructor';            FMaskColour: clLime),
    (FResourceName : 'PublishedConstructor';          FMaskColour: clLime),
    (FResourceName : 'ProtectedConstructor';          FMaskColour: clLime),
    (FResourceName : 'LocalConstructor';              FMaskColour: clLime),

    (FResourceName : 'PublicDestructor';              FMaskColour: clFuchsia),
    (FResourceName : 'PrivateDestructor';             FMaskColour: clFuchsia),
    (FResourceName : 'PublishedDestructor';           FMaskColour: clFuchsia),
    (FResourceName : 'ProtectedDestructor';           FMaskColour: clFuchsia),
    (FResourceName : 'LocalDestructor';               FMaskColour: clFuchsia),

    (FResourceName : 'PublicProcedure';               FMaskColour: clLime),
    (FResourceName : 'PrivateProcedure';              FMaskColour: clLime),
    (FResourceName : 'PublishedProcedure';            FMaskColour: clLime),
    (FResourceName : 'ProtectedProcedure';            FMaskColour: clLime),
    (FResourceName : 'LocalProcedure';                FMaskColour: clLime),

    (FResourceName : 'PublicFunction';                FMaskColour: clLime),
    (FResourceName : 'PrivateFunction';               FMaskColour: clLime),
    (FResourceName : 'PublishedFunction';             FMaskColour: clLime),
    (FResourceName : 'ProtectedFunction';             FMaskColour: clLime),
    (FResourceName : 'LocalFunction';                 FMaskColour: clLime),

    (FResourceName : 'ClassesLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicClass';                   FMaskColour: clLime),
    (FResourceName : 'PrivateClass';                  FMaskColour: clLime),
    (FResourceName : 'PublishedClass';                FMaskColour: clLime),
    (FResourceName : 'ProtectedClass';                FMaskColour: clLime),
    (FResourceName : 'LocalClass';                    FMaskColour: clLime),

    (FResourceName : 'PropertyLabel';                 FMaskColour: clFuchsia),
    (FResourceName : 'PublicProperty';                FMaskColour: clFuchsia),
    (FResourceName : 'PrivateProperty';               FMaskColour: clFuchsia),
    (FResourceName : 'PublishedProperty';             FMaskColour: clFuchsia),
    (FResourceName : 'ProtectedProperty';             FMaskColour: clFuchsia),
    (FResourceName : 'LocalProperty';                 FMaskColour: clFuchsia),

    (FResourceName : 'InterfacesLabel';               FMaskColour: clLime),
    (FResourceName : 'PublicInterface';               FMaskColour: clLime),
    (FResourceName : 'PrivateInterface';              FMaskColour: clLime),
    (FResourceName : 'PublishedInterface';            FMaskColour: clLime),
    (FResourceName : 'ProtectedInterface';            FMaskColour: clLime),
    (FResourceName : 'LocalInterface';                FMaskColour: clLime),

    (FResourceName : 'DispInterfaceSLabel';           FMaskColour: clLime),
    (FResourceName : 'PublicDispInterface';           FMaskColour: clLime),
    (FResourceName : 'PrivateDispInterface';          FMaskColour: clLime),
    (FResourceName : 'PublishedDispInterface';        FMaskColour: clLime),
    (FResourceName : 'ProtectedDispInterface';        FMaskColour: clLime),
    (FResourceName : 'LocalDispInterface';            FMaskColour: clLime),

    (FResourceName : 'PublicConstantsLabel';          FMaskColour: clLime),
    (FResourceName : 'PrivateConstantsLabel';         FMaskColour: clLime),
    (FResourceName : 'PublishedConstantsLabel';       FMaskColour: clLime),
    (FResourceName : 'ProtectedConstantsLabel';       FMaskColour: clLime),
    (FResourceName : 'LocalConstantsLabel';           FMaskColour: clLime),
    (FResourceName : 'PublicConst';                   FMaskColour: clLime),
    (FResourceName : 'PrivateConst';                  FMaskColour: clLime),
    (FResourceName : 'PublishedConst';                FMaskColour: clLime),
    (FResourceName : 'ProtectedConst';                FMaskColour: clLime),
    (FResourceName : 'LocalConst';                    FMaskColour: clLime),

    (FResourceName : 'PublicResourceStringsLabel';    FMaskColour: clLime),
    (FResourceName : 'PrivateResourceStringsLabel';   FMaskColour: clLime),
    (FResourceName : 'PublishedResourceStringsLabel'; FMaskColour: clLime),
    (FResourceName : 'ProtectedResourceStringsLabel'; FMaskColour: clLime),
    (FResourceName : 'LocalResourceStringsLabel';     FMaskColour: clLime),
    (FResourceName : 'PublicResourceString';          FMaskColour: clLime),
    (FResourceName : 'PrivateResourceString';         FMaskColour: clLime),
    (FResourceName : 'PublishedResourceString';       FMaskColour: clLime),
    (FResourceName : 'ProtectedResourceString';       FMaskColour: clLime),
    (FResourceName : 'LocalResourceString';           FMaskColour: clLime),

    (FResourceName : 'PublicVariablesLabel';          FMaskColour: clLime),
    (FResourceName : 'PrivateVariablesLabel';         FMaskColour: clLime),
    (FResourceName : 'PublishedVariablesLabel';       FMaskColour: clLime),
    (FResourceName : 'ProtectedVariablesLabel';       FMaskColour: clLime),
    (FResourceName : 'LocalVariablesLabel';           FMaskColour: clLime),
    (FResourceName : 'PublicVariable';                FMaskColour: clLime),
    (FResourceName : 'PrivateVariable';               FMaskColour: clLime),
    (FResourceName : 'PublishedVariable';             FMaskColour: clLime),
    (FResourceName : 'ProtectedVariable';             FMaskColour: clLime),
    (FResourceName : 'LocalVariable';                 FMaskColour: clLime),

    (FResourceName : 'PublicThreadVarsLabel';         FMaskColour: clLime),
    (FResourceName : 'PublicThreadVar';               FMaskColour: clLime),
    (FResourceName : 'PrivateThreadVar';              FMaskColour: clLime),
    (FResourceName : 'PublishedThreadVar';            FMaskColour: clLime),
    (FResourceName : 'ProtectedThreadVar';            FMaskColour: clLime),
    (FResourceName : 'LocalThreadVar';                FMaskColour: clLime),

    (FResourceName : 'PublicClassVariablesLabel';     FMaskColour: clLime),
    (FResourceName : 'PrivateClassVariablesLabel';    FMaskColour: clLime),
    (FResourceName : 'PublishedClassVariablesLabel';  FMaskColour: clLime),
    (FResourceName : 'ProtectedClassVariablesLabel';  FMaskColour: clLime),
    (FResourceName : 'LocalClassVariablesLabel';      FMaskColour: clLime),
    (FResourceName : 'PublicClassVariable';           FMaskColour: clLime),
    (FResourceName : 'PrivateClassVariable';          FMaskColour: clLime),
    (FResourceName : 'PublishedClassVariable';        FMaskColour: clLime),
    (FResourceName : 'ProtectedClassVariable';        FMaskColour: clLime),
    (FResourceName : 'LocalClassVariable';            FMaskColour: clLime),

    (FResourceName : 'ExportedHeadingsLabel';         FMaskColour: clLime),

    (FResourceName : 'ExportedFunctionsLabel';        FMaskColour: clLime),
    (FResourceName : 'PublicExportedFunction';        FMaskColour: clLime),
    (FResourceName : 'PrivateExportedFunction';       FMaskColour: clLime),
    (FResourceName : 'PublishedExportedFunction';     FMaskColour: clLime),
    (FResourceName : 'ProtectedExportedFunction';     FMaskColour: clLime),
    (FResourceName : 'LocalExportedFunction';         FMaskColour: clLime),

    (FResourceName : 'PublicLabelsLabel';             FMaskColour: clLime),
    (FResourceName : 'PrivateLabelsLabel';            FMaskColour: clLime),
    (FResourceName : 'PublishedLabelsLabel';          FMaskColour: clLime),
    (FResourceName : 'ProtectedLabelsLabel';          FMaskColour: clLime),
    (FResourceName : 'LocalLabelsLabel';              FMaskColour: clLime),
    (FResourceName : 'LocalLabel';                    FMaskColour: clLime),
    (FResourceName : 'PrivateLabel';                  FMaskColour: clLime),
    (FResourceName : 'ProtectedLabel';                FMaskColour: clLime),
    (FResourceName : 'PublicLabel';                   FMaskColour: clLime),
    (FResourceName : 'PublishedLabel';                FMaskColour: clLime),

    (FResourceName : 'MethodsLabel';                  FMaskColour: clLime),
    (FResourceName : 'ImplementedMethodsLabel';       FMaskColour: clLime),

    (FResourceName : 'InitializationLabel';           FMaskColour: clFuchsia),
    (FResourceName : 'FinalizationLabel';             FMaskColour: clLime),

    (FResourceName : 'TodoFolder';                    FMaskColour: clLime),
    (FResourceName : 'TodoItem';                      FMaskColour: clLime),

    (FResourceName : 'UnknownClsObj';                 FMaskColour: clLime)
  );

  (** A table of information for document conflicts. **)
  DocConflictTable : Array[Low(TDocConflictType)..High(TDocConflictType)] Of TDocConflictTable = (
    (FMessage: strModuleMissingDocumentation;
      FDescription: strModuleMissingDocumentationDesc;
      FConflictType: dciMissing),
    (FMessage: strModuleMissingDate;
      FDescription: strModuleMissingDateDesc;
      FConflictType: dciMissing),
    (FMessage: strModuleIncorrectDate;
      FDescription: strModuleIncorrectDateDesc;
      FConflictType: dciIncorrect),
    (FMessage: strModuleCheckDateError;
      FDescription: strModuleCheckDateErrorDesc;
      FConflictType: dciIncorrect),
    (FMessage: strModuleMissingVersion;
      FDescription: strModuleMissingVersionDesc;
      FConflictType: dciMissing),
    (FMessage: strModuleMissingAuthor;
      FDescription: strModuleMissingAuthorDesc;
      FConflictType: dciMissing),

    (FMessage: strTypeClauseUndocumented;
      FDescription: strTypeClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strConstantClauseUndocumented;
      FDescription: strConstantClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strResourceStringClauseUndocumented;
      FDescription: strResourceStringClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strVariableClauseUndocumented;
      FDescription: strVariableClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strThreadVarClauseUndocumented;
      FDescription: strThreadVarClauseUndocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strFieldClauseUndocumented;
      FDescription: strFieldClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FMessage: strClassClauseUndocumented;
      FDescription: strClassClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FMessage: strRecordClauseUndocumented;
      FDescription: strRecordClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FMessage: strObjectClauseUndocumented;
      FDescription: strObjectClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FMessage: strInterfaceClauseUndocumented;
      FDescription: strInterfaceClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FMessage: strDispInterfaceClauseUndocumented;
      FDescription: strDispInterfaceClauseUndocumentedDesc;
      FConflictType: dciMissing),

    (FMessage: strFunctionUndocumented;
      FDescription: strFunctionUndocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionHasNoDesc;
      FDescription: strFunctionHasNoDescDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionPreconNotDocumented;
      FDescription: strFunctionPreconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionDiffParamCount;
      FDescription: strFunctionDiffParamCountDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionMissingPreCon;
      FDescription: strFunctionMissingPreConDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionTooManyPrecons;
      FDescription: strFunctionTooManyPreconsDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionUndocumentedParam;
      FDescription: strFunctionUndocumentedParamDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionIncorrectParamType;
      FDescription: strFunctionIncorrectParamTypeDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionPostconNotDocumented;
      FDescription: strFunctionPostconNotDocumentedDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionUndocumentedReturn;
      FDescription: strFunctionUndocumentedReturnDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionIncorrectReturntype;
      FDescription: strFunctionIncorrectReturntypeDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionReturnNotRequired;
      FDescription: strFunctionReturnNotRequiredDesc;
      FConflictType: dciItem),
    (FMessage: strFunctionMissingPostCon;
      FDescription: strFunctionMissingPostConDesc;
      FConflictType: dciMissing),
    (FMessage: strFunctionTooManyPostCons;
      FDescription: strFunctionTooManyPostConsDesc;
      FConflictType: dciMissing),

    (FMessage: strMissingInitComment;
      FDescription: strMissingInitCommentDesc;
      FConflictType: dciMissing),
    (FMessage: strMissingFinalComment;
      FDescription: strMissingFinalCommentDesc;
      FConflictType: dciMissing)
  );

  (** A list of strings representing the token types. **)
  strTokenType : Array[Low(TBADITokenType)..High(TBADITokenType)] Of String = (
    'Unknown', 'White Space', 'Reserved Word', 'Identifier', 'Number',
    'Symbol', 'Line End', 'Single Literal', 'Double Literal', 'Line Comment',
    'Block Comment', 'HTML Start Tag', 'HTML End Tag',  'Directive',
    'Compiler Directive', 'Link Tag', 'Tree Header', 'File End', 'Line Continuation',
    'Custom User Token', 'Explorer Highlight');

  (** A constant string to represent the position of the main procedure code in
      a profiling code block. **)
  strMethodCode = '$METHODCODE$';
  (** A constant string to represent the position to insert the method name into
      the profiling code block. **)
  strMethodName = '$METHODNAME$';

Var
  (** This is a global variable for the Browse and Doc It options that need to
      be available throughout the application. **)
  BrowseAndDocItOptions : TBrowseAndDocItOptions;
  (** This is a global variable that is initialised by this module and available
      to all over modules so that they can register their information. **)
  ModuleDispatcher : TModuleDispatcher;

  Function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Function IsInSet(C : Char; strCharSet : TSetOfAnsiChar) : Boolean; {$IFDEF D2005} InLine; {$ENDIF}
  Function PrologCode(strTemplate, strMethod : String; iPadding : Integer) : TStringList;
  Function EpilogCode(strTemplate, strMethod : String; iPadding : Integer) : TStringList;

Implementation

Uses
  Windows, DGHLibrary, INIFiles;

Const
  (** This constant represent the maximum of issue / doc conflicts to add. **)
  iIssueLimit : Integer = 25; //: @todo Add this as a configurable option.


ResourceString
  (** An error message for tying to add one type of element but finding another
      with the same name. **)
  strTryingToAddType = 'Trying to add type ''%s'' but found type ''%s'' with the' +
  ' same name (%s).';

Var
  (** This variable provides an incremental number of making doc conflict
      messages unique. **)
  iDocConflictCounter: Integer;

(**


  This function returns true if the given word is in the supplied word list.
  It uses a binary search, so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and
           strWordList is a static array of words in lowercase and
           alphabetical order.
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

(**

  This method builds a language independant representation of the parameter.

  @precon  None.
  @postcon Returns a string language independant representation of the parameter.

  @param   Param as a TGenericParameter
  @return  a String

**)
function BuildLangIndepRep(Param: TGenericParameter): String;
begin
  Result := '';
  If Param.ParamType = Nil Then
    Exit;
  If Param.ArrayOf Then
    Result := 'Array Of ';
  Result := Result + Param.ParamType.AsString(False, False);
  Case Param.ParamModifier Of
    pamVar: Result := Result + ' as a reference';
    pamConst: Result := Result + ' as a constant';
    pamOut: Result := Result + ' as an out parameter';
  End;
end;

(**

  This function outputs the comment or tag as a string missing out HTML tags if
  not required and any trialing whitespace.

  @precon  C must eb a valid instance of a TBaseContainer.
  @postcon Outputs the comment or tag as a string missing out HTML tags if not
           required and any trialing whitespace.

  @param   C            as a TBaseContainer
  @param   iMaxWidth    as an Integer
  @param   boolShowHTML as a Boolean
  @return  a String

**)
Function OutputCommentAndTag(C : TBaseContainer; iMaxWidth : Integer;
  boolShowHTML: Boolean) : String;

Var
  iToken: Integer;
  iLength : Integer;
  strToken: String;

begin
  Result := '';
  iLength := 0;
  For iToken := 0 To C.TokenCount - 1 Do
    If ((C.Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) And boolShowHTML) Or
      Not (C.Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) Then
      Begin
        If iLength + C.Tokens[iToken].Length > iMaxWidth Then
          Begin
            Result := Result + #13#10;
            iLength := 0;
          End;
        If Not ((iLength = 0) And (C.Tokens[iToken].TokenType In [ttWhiteSpace])) Then
          Begin
            If C.Tokens[iToken].Token = '#' Then
              Begin
                iLength := 0;
                Result := Result + #13#10;
              End Else
              Begin
                strToken := C.Tokens[iToken].Token;
                If (Length(strToken) >= 2) And (strToken[1] = '@') And
                  (strToken[2] = '@') Then
                  strToken := Copy(strToken, 2, Length(strToken) - 1);
                Result := Result + strToken;
              End;
          End;
        Inc(iLength, C.Tokens[iToken].Length);
      End;
end;

(**

  This function centralises the checking of characters in set for both AnsiChars
  and Unicode Chars so that the parser tokeniser are not riddled with
  conditional compilation statements.

  @precon  None.
  @postcon Checks to see if the char is in the set and returns true if so.

  @param   C          as a Char
  @param   strCharSet as a TSetOfAnsiChar
  @return  a Boolean

**)
Function IsInSet(C : Char; strCharSet : TSetOfAnsiChar) : Boolean; {$IFDEF D2005} InLine; {$ENDIF}

Begin
  {$IFNDEF D2009}
  Result := C In strCharSet;
  {$ELSE}
  Result := CharInSet(C, strCharSet);
  {$ENDIF}
End;

(**

  This procedure returns a string list containing the prolog element of code passed in the
  template parameter.

  @precon  strTemplate must contain the macro $METHODCODE$.
  @postcon Returns a string list containing the prolog element of code passed in the
           template parameter.

  @param   strTemplate as a String
  @param   strMethod   as a String
  @param   iPadding    as an Integer
  @return  a TStringList

**)
Function PrologCode(strTemplate, strMethod : String; iPadding : Integer) : TStringList;

Var
  strPadding : String;
  iLine : Integer;
  boolFound : Boolean;

Begin
  Result := TStringList.Create;
  Result.Text := StringReplace(strTemplate, strMethodName, strMethod, [rfReplaceAll]);
  //If Not Like('*' + strMethodCode + '*', strTemplate) Then
  //  Raise Exception.Create(strMethodCode + ' Not Found in Template.');
  boolFound := False;
  While Not boolFound And (Result.Count > 0) Do
    Begin
      If CompareText(Trim(Result[Result.Count - 1]), strMethodCode) = 0 Then
        boolFound := True;
      Result.Delete(Result.Count - 1);
    End;
  strPadding := StringOfChar(#32, iPadding);
  For iLine := 0 To Result.Count - 1 Do
    Result[iLine] := strPadding + Result[iLine];
End;

(**

  This procedure returns a string list containing the epilog element of code passed in the
  template parameter.

  @precon  strTemplate must contain the macro $METHODCODE$.
  @postcon Returns a string list containing the epilog element of code passed in the
           template parameter.

  @param   strTemplate as a String
  @param   strMethod   as a String
  @param   iPadding    as an Integer
  @return  a TStringList

**)
Function EpilogCode(strTemplate, strMethod : String; iPadding : Integer) : TStringList;

Var
  strPadding : String;
  iLine : Integer;
  boolFound : Boolean;

Begin
  Result := TStringList.Create;
  strTemplate := StringReplace(strTemplate, strMethodName, strMethod, [rfReplaceAll]);
  Result.Text := strTemplate;
  If Not Like('*' + strMethodCode + '*', strTemplate) Then
    Raise Exception.Create(strMethodCode + ' Not Found in Template.');
  boolFound := False;
  While Not boolFound And (Result.Count > 0) Do
    Begin
      If CompareText(Trim(Result[0]), strMethodCode) = 0 Then
        boolFound := True;
      Result.Delete(0);
    End;
  strPadding := StringOfChar(#32, iPadding);
  For iLine := 0 To Result.Count - 1 Do
    Result[iLine] := strPadding + Result[iLine];
End;

{ TBaseContainer }

(**

  This method adds the given TTokenInfo object to the token collection.

  @precon  AToken must be a valid token instance.
  @postcon Adds the given TTokenInfo object to the token collection. Note that
           the calling code must not free this memeory - it will be freed by
           this container.

  @param   AToken as a TTokenInfo

**)
procedure TBaseContainer.AddToken(AToken: TTokenInfo);
begin
  FTokens.Add(AToken);
end;

(**

  This method adds a TTokenInfo class representation of the given string to the
  token collection.

  @precon  None.
  @postcon Adds a TTokenInfo class representation of the given string to the
           token collection.

  @param   strToken   as a String
  @param   ATokenType as a TBADITokenType

**)
procedure TBaseContainer.AddToken(strToken : String;
  ATokenType : TBADITokenType = ttUnknown);

begin
  AddToken(TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), ATokenType));
end;

(**

  This method append a copy of the given token to the tokens collection.

  @precon  AToken mustbe a valid instance of a TTokenInfo.
  @postcon Append a copy of the given token to the tokens collection. Note, the
           calling code is responsible for freeing the AToken instance only.

  @param   AToken as a TTokenInfo

**)
procedure TBaseContainer.AppendToken(AToken: TTokenInfo);
begin
  AddToken(TTokenInfo.Create(AToken.Token, AToken.BufferPos, AToken.Line,
    AToken.Column, AToken.Length, AToken.TokenType));
end;

(**

  This method builds a string from the identifer and tokens and tries to
  present it with the style of code you would probably except.

  @precon  None.
  @postcon Builds a string from the identifer and tokens and tries to present
           it with the style of code you would probably except.

  @param   boolIdentifier       as a Boolean
  @param   boolForDocumentation as a Boolean
  @param   strDelimiter         as a String
  @param   iMaxWidth            as an Integer
  @param   strNoSpaceBefore     as a TSymbols
  @param   strNoSpaceAfter      as a TSymbols
  @param   strSpaceAfter        as a TSymbols
  @param   boolShowHTML         as a Boolean
  @return  a String

**)
Function TBaseContainer.BuildStringRepresentation(boolIdentifier,
  boolForDocumentation : Boolean; strDelimiter : String; iMaxWidth : Integer;
  strNoSpaceBefore : TSymbols = ['(', '[', '{', ')', ']', '}', ';', ',', '.', '!', '?'];
  strNoSpaceAfter : TSymbols = ['(', '[', '{', '.', '^'];
  strSpaceAfter : TSymbols = ['=', ':', '+', '-', '*', '\'];
  boolShowHTML : Boolean = False) : String;

Var
  iToken : Integer;
  T, L, D : TTokenInfo;
  boolSpace: Boolean;
  iLength : Integer;

Begin
  Result := '';
  If boolIdentifier Then
    Begin
      Result := Identifier;
      If (Length(Result) > 0) And (Length(strDelimiter) > 0) Then
        If TokenCount > 0 Then
          Begin
            If Not (IsInSet(strDelimiter[1], strNoSpaceBefore)) Then
              Result := Result + #32;
            Result := Result + strDelimiter;
          End;
    End;
  iLength := Length(Result);
  D := TTokenInfo.Create(strDelimiter, 0, 0, 0, Length(strDelimiter), ttSymbol);
  Try
    L := D;
    For iToken := 0 To TokenCount - 1 Do
      If Not (Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) Or
        ((Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) And boolShowHTML) Then
        Begin
          boolSpace := (iToken > -1) Or (strDelimiter <> '');
          T := Tokens[iToken];
          boolSpace := boolSpace And Not (IsInSet(T.Token[1], strNoSpaceBefore));
          If (L <> Nil) And (L.Length > 0) Then
            boolSpace := boolSpace  And Not (IsInSet(L.Token[1], strNoSpaceAfter));
          If Result <> '' Then
            If boolSpace Or ((L.Length > 0) And (IsInSet(L.Token[1], strSpaceAfter))) Then
              If Not (boolForDocumentation And (iLength + T.Length > iMaxWidth)) Then
                Begin
                  If (L.TokenType <> ttHTMLStartTag) And (T.TokenType <> ttHTMLEndTag) Then
                    Result := Result + #32;
                  Inc(iLength);
                End Else
                Begin
                  Result := Result + #13#10#32#32;
                  iLength := 2;
                End;
          Result := Result + T.Token;
          Inc(iLength, T.Length);
          L := T;
        End;
  Finally
    D.Free;
  End;
End;
(**

  This method clears the tokens in the collection.

  @precon  None.
  @postcon Clears the tokens in the collection.

**)
procedure TBaseContainer.ClearTokens;
begin
  FTokens.Clear;
end;

(**

  This is a constructor for the TBaseContainer class.

  @precon  None.
  @postcon Create the token collection and initialises the Line and Column
           data.

  @param   strName as a String
  @param   iLine   as an Integer
  @param   iColumn as an Integer

**)
constructor TBaseContainer.Create(strName : String; iLine, iColumn: Integer);
begin
  FTokens := TObjectList.Create(True);
  FName := strName;
  FLine := iLine;
  FColumn := iColumn;
end;

(**

  This method deletes the indexed token from the token collection.

  @precon  iIndex must be a valid index between 0 and TokenCount - 1.
  @postcon Deletes the indexed token from the token collection.

  @param   iIndex as an Integer

**)
procedure TBaseContainer.DeleteToken(iIndex: Integer);
begin
  FTokens.Delete(iIndex);
end;

(**

  This is a destructor for the TBaseContainer class.

  @precon  None.
  @postcon Frees the token collection and the memory belonging to the tokens.

**)
destructor TBaseContainer.Destroy;
begin
  FTokens.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the element. This can be override for the
           purposes of find / sorting the elements. Identifier still returns
           the FName variable.

  @return  a String

**)
function TBaseContainer.GetName: String;
begin
  Result := FName;
end;

(**

  This is a getter method for the TokenCount property.

  @precon  None.
  @postcon Returns the number of tokens in the collection.

  @return  an Integer

**)
function TBaseContainer.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

(**

  This is a getter method for the Tokens property.

  @precon  iIndex must be a valid index between 0 and TokenCount - 1.
  @postcon Returns the instance of the indexed token.

  @param   iIndex as an Integer
  @return  a TTokenInfo

**)
function TBaseContainer.GetTokens(iIndex: Integer): TTokenInfo;
begin
  Result := FTokens[iIndex] As TTokenInfo;
end;

(**

  This method inserts a token at the given index in the token collection.

  @precon  None.
  @postcon Inserts a token at the given index in the token collection.

  @param   strToken   as a String
  @param   iIndex     as an Integer
  @param   ATokenType as a TBADITokenType

**)
procedure TBaseContainer.InsertToken(strToken: String; iIndex: Integer;
  ATokenType : TBADITokenType = ttUnknown);

begin
  If iIndex >= FTokens.Count Then
    iIndex := FTokens.Count - 1;
  If iIndex < 0 Then
    iIndex := 0;
  FTokens.Insert(iIndex, TTokenInfo.Create(strToken, 0, 0, 0,
    Length(strToken), ATokenType));
end;

(**

  This is a setter method for the Name property.

  @precon  None.
  @postcon Sets the name of the container.

  @param   Value as a String

**)
procedure TBaseContainer.SetName(Value: String);
begin
  FName := Value;
end;

(**

  This is the TTag class`s constructor method. It creates the token list.

  @precon  strName is the name of the new tag to be created, iLine is the line
           number of the tag and iColumn is the column position of the tag.
  @postcon Initialises the comment tag class.

  @param   strName as a String
  @param   iLine   as an Integer
  @param   iColumn as an Integer

**)
constructor TTag.Create(strName: String; iLine, iColumn : Integer);
begin
  Inherited Create(strName, iLine, iColumn);
end;

(**

  This is the TTag class Destructor method. It disploses of the token list.

  @precon  None.
  @postcon Frees the tags tokens.

**)
destructor TTag.Destroy;
begin
  Inherited Destroy;
end;

(**

  This is a getter method for the TagName property.

  @precon  None.
  @postcon Gets the tah name for the tag from the identifer property.

  @return  a String

**)
function TTag.GetTagName: String;
begin
  Result := Identifier;
end;

(**

  This method returns all the tags tokens as a string with spaces in between.

  @precon  ShowHTML determines of the routine output the HTML tags in the
           resulting string.
  @postcon Returns a string representation of the tag.

  @param   iMaxWidth    as an Integer
  @param   boolShowHTML as a Boolean
  @return  a String

**)
function TTag.AsString(iMaxWidth : Integer; boolShowHTML : Boolean): String;

begin
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML);
end;

(** --------------------------------------------------------------------------

  TComment methods

 -------------------------------------------------------------------------- **)

(**

  This method added a token and its type to the token list.

  @precon  strToken is a string to be added as a token and iType is the tokens
           type.
  @postcon Added a token and its type to the token list.

  @param   strToken as a String
  @param   iType    as a TBADITokenType

**)
procedure TComment.AddToken(strToken: String; iType: TBADITokenType);

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
      If Not ((iType = ttWhiteSpace) And (TokenCount = 0)) Then
        AddToken(TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), iType));
    End Else
    Begin
      If Not ((iType = ttWhiteSpace) And (FLastTag.TokenCount = 0)) Then
        FLastTag.AddToken(TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), iType));
    End;
end;

(**

  This method appends a source comments tokens and tabs onto this comment.

  @precon  BaseCmt and Source must be valid instances of TComment.
  @postcon Appends a source comments tokens and tabs onto this comment.

  @param   BaseCmt as a TComment
  @param   Source  as a TComment

**)
procedure TComment.AppendComment(BaseCmt, Source: TComment);

Var
  Tag : TBaseContainer;
  i: Integer;
  j: Integer;

begin
  Tag := Source;
  If TagCount > 0 Then
    Tag := BaseCmt.Tag[TagCount - 1];
  Tag.AddToken(#32, ttWhiteSpace);
  For i := 0 To Source.TokenCount - 1 Do
    Tag.AddToken(Source.Tokens[i].Token , Source.Tokens[i].TokenType);
  For i := 0 To Source.TagCount - 1 Do
    Begin
      FTagLine := Source.Tag[i].Line;
      FTagColumn := Source.Tag[i].Column + Length(Source.Tag[i].Name) + 1;
      AddToken('@' + Source.Tag[i].Name);
      For j := 0 To Source.Tag[i].TokenCount - 1 Do
        AddToken(Source.Tag[i].Tokens[j].Token,
          Source.Tag[i].Tokens[j].TokenType);
    End;

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
      Line := srcComment.Line;
      Column := srcComment.Column;
      // Add tokens from one to the next.
      For i := 0 To srcComment.TokenCount - 1 Do
        AddToken(srcComment.Tokens[i].Token, srcComment.Tokens[i].TokenType);
      For i := 0 To srcComment.TagCount - 1 Do
        Begin
          AddToken('@' + srcComment.Tag[i].TagName, ttIdentifier);
          For j := 0 To srcComment.Tag[i].TokenCount - 1 Do
            AddToken(srcComment.Tag[i].Tokens[j].Token,
              srcComment.Tag[i].Tokens[j].TokenType);
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

  @param   iMaxWidth as an Integer
  @param   boolShowHTML  as a Boolean
  @return  a String

**)
function TComment.AsString(iMaxWidth: Integer; boolShowHTML : Boolean): String;

begin
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML);
end;

(**

  This is the constructor method for the TComment class.

  @precon  None.
  @postcon Allows a comment to be constructed from another comment (clone).

  @param   srcComment as a TComment

**)
Constructor TComment.Create(srcComment : TComment);

Begin
  If srcComment <> Nil Then
    Inherited Create('', srcComment.Line, srcComment.Column)
  Else
    Inherited Create('', 0, 0);
  FLastTag := Nil;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  Assign(srcComment);
End;

(**


  This is the TComment constructor. It create a token list and a tag list. Then
  it passes the comment to the comment parser.

  @precon  strComment is a string of text to be parsed as a comment, iLine is
           the line number of the comment and iCol is the column number of
           the comment.
  @postcon It create a token list and a tag list. Then it passes the comment to
           the comment parser.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
constructor TComment.Create(strComment : String; iLine, iCol : Integer);
begin
  Inherited Create('', iLine, iCol);
  FLastTag := Nil;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  ParseComment(strComment);
end;

(**

  This is a constructor for the TComment class.

  @precon  None.
  @postcon Implements a basic comment with no start and end character removed.
           This method should be overridden by descendants to handle their
           different comment styles.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
class function TComment.CreateComment(strComment: String; iLine,
  iCol: Integer): TComment;
begin
  Result := Create(strComment, iLine, iCol);
end;

(**

  This is the TComment class`s destructor. It disposes of the token list and
  the tag list.

  @precon  None.
  @postcon Frees the classes internal lists.

**)
destructor TComment.Destroy;
begin
  FTags.Free;
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

  This method removes trailing white space tokens from the parsed comments and
  tags.

  @precon  None.
  @postcon Removes trailing white space tokens from the parsed comments and
           tags.

**)
procedure TComment.TrimTrailingWhiteSpace;

Var
  iToken : Integer;
  iTag: Integer;

begin
  If TokenCount > 0 Then
    Begin
      iToken := TokenCount - 1;
      While Tokens[iToken].TokenType In [ttWhiteSpace] Do
        Begin
          DeleteToken(iToken);
          Dec(iToken);
        End;
    End;
  For iTag := 0 To TagCount - 1 Do
    Begin
      If Tag[iTag].TokenCount = 0 Then
        Continue;
      iToken := Tag[iTag].TokenCount - 1;
      While (iToken >= 0) And
        (Tag[iTag].Tokens[iToken].TokenType In [ttWhiteSpace]) Do
        Begin
          Tag[iTag].DeleteToken(iToken);
          Dec(iToken);
        End;
    End;
end;

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
    If CompareText(Tag[i].TagName, strTagName) = 0 Then
      Begin
        Result := i;
        Break;
      End;
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
  TBlockType = (btNone, btHTML, btLink, btSingle, btDouble);

Const
  iTokenCapacity = 25;

Var
  i : Integer;
  CurToken : TBADITokenType;
  LastToken : TBADITokenType;
  strToken : String;
  BlockType : TBlockType;
  iTokenLen : Integer;
  LastTokenAdded: TBADITokenType;

begin
  CurToken := ttUnknown;
  LastToken := ttUnknown;
  strToken := '';
  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);
  BlockType := btNone;
  FTagLine := Line;
  FTagColumn := Column + 1;
  LastTokenAdded := ttUnknown;
  For i := 1 To Length(strComment) Do
    Begin
      LastToken := CurToken;
      If IsInSet(strComment[i], strWhiteSpace) Then
        CurToken := ttWhiteSpace
      Else If IsInSet(strComment[i], ['''']) Then
        CurToken := ttSingleLiteral
      Else If IsInSet(strComment[i], ['"']) Then
        CurToken := ttDoubleLiteral
      Else If IsInSet(strComment[i], ['@', '_', 'a'..'z', 'A'..'Z']) Then
        Begin
          If (LastToken = ttNumber) And (IsInSet(strComment[i], ['A'..'F', 'a'..'f'])) Then
            CurToken := ttNumber
          Else
            CurToken := ttIdentifier;
        End
      Else If IsInSet(strComment[i], ['0'..'9']) Then
        Begin
          CurToken := ttNumber;
          If LastToken = ttIdentifier Then
            CurToken := ttIdentifier;
        End
      Else If IsInSet(strComment[i], strLineEnd) Then
        CurToken := ttLineEnd
      Else If IsInSet(strComment[i], [#33..#128] - ['a'..'z', 'A'..'Z', '@', '#']) Then
        CurToken := ttSymbol
      Else
        CurToken := ttUnknown;

      If ((CurToken <> LastToken) And (BlockType = btNone)) Or
        (strComment[i] = '<') Then
        Begin
          SetLength(strToken, iTokenLen);
          If iTokenLen > 0 Then
            Begin
              If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
                Begin
                  AddToken(strToken, LastToken);
                  LastTokenAdded := LastToken;
                End
              Else If Not (LastTokenAdded In [ttWhiteSpace, ttLineEnd]) Then
              Begin
                AddToken(#32, ttWhiteSpace);
                LastTokenAdded := ttWhiteSpace;
              End;
              LastToken := CurToken;
            End;
          iTokenLen := 1;
          SetLength(strToken, iTokenCapacity);
          strToken[iTokenLen] := strComment[i];
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strComment[i];
        End;

      If (BlockType = btNone) And (strToken[1] = '{') Then
        BlockType := btLink
      Else If (BlockType = btNone) And (strToken[1] = '<') Then
        BlockType := btHTML;

      If (BlockType = btLink) And (strToken[iTokenLen] = '}') Then
        Begin
          BlockType := btNone;
          CurToken := ttLinkTag;
        End;
      If (BlockType = btHTML) And (strToken[iTokenLen] = '>') Then
        Begin
          BlockType := btNone;
          If strToken[2] = '/' Then
            CurToken := ttHTMLEndTag
          Else
            CurToken := ttHTMLStartTag;
        End;

        // Check for single string literals
        If CurToken = ttSingleLiteral Then
          If BlockType = btSingle Then
            BlockType := btNone
          Else If BlockType = btNone Then
            BlockType := btSingle;
        // Check for Double string literals
        If CurToken = ttDoubleLiteral Then
          If BlockType = btDouble Then
            BlockType := btNone
          Else If BlockType = btNone Then
            BlockType := btDouble;

      If strComment[i] = #10 Then
        Begin
          FTagColumn := Column + 1;
          Inc(FTagLine);
        End Else
          Inc(FTagColumn);
    End;
  If (iTokenLen > 0) Then
    Begin
      SetLength(strToken, iTokenLen);
      If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
        AddToken(strToken, LastToken);
    End;
  TrimTrailingWhitespace;
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
  @param   TType    as a TBADITokenType

**)
Constructor TTokenInfo.Create(strToken : String; iPos, iLine, iCol,
  iLength: Integer; TType : TBADITokenType);

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
  FReference := trUnknown;
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
  E: TElementContainer;

begin
  Result := AElement;
  Assert(AElement.Name <> '', 'Can not add a null element to the collection!');
  AElement.FParent := Self;
  i := Find(AElement.Name);
  If i < 0 Then
    FElements.Insert(Abs(i) - 1, AElement)
  Else
    Try
      Result := FElements[i -1] As TElementContainer;
      If Result.Comment = Nil Then
        Result.Comment := AElement.Comment
      Else
        Result.Comment.Assign(AElement.Comment);
      If Not AElement.ClassNameIs(Result.ClassName) Then
        Begin
          E := FindRoot.Add(strErrors, iiErrorFolder, scNone, Nil);
          E.Add(TDocIssue.Create(Format(strTryingToAddType, [AElement.ClassName,
            Result.ClassName, AElement.Name]), scNone, 'TElementContainer.Add',
            AElement.Line, AElement.Column, iiError));
          Raise EParserAbort.Create('Parsing Aborted!');
        End;
    Finally
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
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment
  @return  a TElementContainer

**)
Function TElementContainer.Add(Token: TTokenInfo; AScope : TScope;
  AImageIndex : TBADIImageIndex; AComment: TComment) : TElementContainer;

Var
  i : Integer;

begin
  Assert(Token.Token <> '', 'Can not add a null token to the collection!');
  i := Find(Token.Token);
  If i < 0 Then
    Begin
      Result := TLabelContainer.Create(Token.Token, AScope, Token.Line,
        Token.Column, AImageIndex, AComment);
      Result.FParent := Self;
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
  @param   AImageIndex as a TBADIImageIndex
  @param   AScope      as a TScope
  @param   AComment    as a TComment
  @return  a TElementContainer

**)
function TElementContainer.Add(strToken: String; AImageIndex: TBADIImageIndex;
  AScope : TScope; AComment: TComment): TElementContainer;

Var
  i : Integer;

begin
  Assert(strToken <> '', 'Can not add a null string to the collection!');
  i := Find(strToken);
  If i < 0 Then
    Begin
      Result := TLabelContainer.Create(strToken, AScope, 0, 0, AImageIndex, AComment);
      Result.FParent := Self;
      FElements.Insert(Abs(i) - 1, Result);
    End Else
    Begin
      Result := FElements[i - 1] As TElementContainer;
      If Result.Comment = Nil Then
        Result.Comment := AComment
      Else
        Result.Comment.Assign(AComment);
    End;
end;

(**

  This method adds a specific documentation conflict to the Docuemntation
  conflict collection.

  @precon  None.
  @postcon Adds a specific documentation conflict to the Docuemntation
           conflict collection.

  @param   Args            as an Array Of Const as a constant
  @param   iIdentLine      as an Integer
  @param   iIdentColumn    as an Integer
  @param   AComment        as a TComment
  @param   strCategory     as a String
  @param   DocConflictRec  as a TDocConflictTable

**)
procedure TElementContainer.AddDocumentConflict(Const Args: Array of Const;
  iIdentLine, iIdentColumn : Integer; AComment : TComment;
  strCategory : String; DocConflictRec : TDocConflictTable);

Var
  E, I, R, D : TElementContainer;
  iL, iC : Integer;
  iIcon : TBADIImageIndex;

begin
  iL := 0;
  iC := 0;
  If AComment <> Nil Then
    Begin
      iL := AComment.Line;
      iC := AComment.Column;
    End;
  Case DocConflictRec.FConflictType Of
    dciMissing : iIcon := iiDocConflictMissing;
    dciIncorrect : iIcon := iiDocConflictIncorrect;
  Else
    iIcon := iiDocConflictItem;
  End;
  R := FindRoot;
  D := R.FindElement(strDocumentationConflicts);
  If D = Nil Then
    D := R.Add(TLabelContainer.Create(strDocumentationConflicts, scGlobal, 0,
      0, iiDocConflictFolder, Nil)) As TLabelContainer;
  E := D;
  I := E.FindElement(strCategory);
  If I = Nil Then
    Begin
      I := TLabelContainer.Create(strCategory, scGlobal, 0, 0,
        iiDocConflictFolder, Nil);
      I := E.Add(I);
    End;
  I.Add(TDocumentConflict.Create(Args, iIdentLine, iIdentColumn, iL, iC,
    DocConflictRec.FMessage, DocConflictRec.FDescription, iIcon));
end;

(**


  This method adds an error to the Base Language`s Element Collection under a
  sub folder of strCategory.


  @precon  Error must be a valid TElementContainer.

  @postcon Adds an error to the Base Language`s Element Collection under a sub

           folder of strCategory.


  @param   strMsg    as a String
  @param   AScope    as a TScope
  @param   strMethod as a String
  @param   iLine     as an Integer
  @param   iCol      as an Integer
  @param   ErrorType as a TErrorType

**)
Procedure TElementContainer.AddIssue(strMsg : String; AScope : TScope;
  strMethod : String; iLine, iCol : Integer; ErrorType : TErrorType);

Type
  TIssueRec = Record
    FFolder       : String;
    FFolderImage  : TBADIImageIndex;
    FItemImage    : TBADIImageIndex;
    FTooMany      : String;
  End;

ResourceString
  strTooManyHints = 'Too many hints (%d)...';
  strTooManyWarnings = 'Too many warnings (%d)...';
  strTooManyErrors = 'Too many errors (%d)...';

Const
  recIssues : Array[Low(TErrorType)..High(TErrorType)] Of TIssueRec = (
    (FFolder : strHints;
      FFolderImage : iiHintFolder;
      FItemImage : iiHint;
      FTooMany : strTooManyHints),
    (FFolder : strWarnings;
      FFolderImage : iiWarningFolder;
      FItemImage : iiWarning;
      FTooMany : strTooManyWarnings),
    (FFolder : strErrors;
      FFolderImage : iiErrorFolder;
      FItemImage : iiError;
      FTooMany : strTooManyErrors)
  );

Var
  I : TElementContainer;
  iCount : Integer;

begin
  Case ErrorType Of
    etHint:
      If Not (doShowHints In BrowseAndDocItOptions.Options) Then
        Exit;
    etWarning:
      If Not (doShowWarnings In BrowseAndDocItOptions.Options) Then
        Exit;
    etError:
      If Not (doShowErrors In BrowseAndDocItOptions.Options) Then
        Exit;
  End;
  If Comment <> Nil Then
    Begin
      Case ErrorType Of
        etHint:
          If Comment.FindTag('nohint') > -1 Then
           Exit;
        etWarning:
          If Comment.FindTag('nowarning') > -1 Then
            Exit;
      End;
    End;
  I := FindRoot.Add(recIssues[ErrorType].FFolder, recIssues[ErrorType].FFolderImage,
    scNone, Nil);
  iCount := I.ElementCount;
  If iCount < iIssueLimit Then
    I.Add(TDocIssue.Create(strMsg, AScope, strMethod, iLine, iCol,
      recIssues[ErrorType].FItemImage))
  Else If iCount = iIssueLimit Then
    I.Add(TDocIssue.Create(Format(recIssues[ErrorType].FTooMany, [iCount]), scNone,
      'AddIssue', 0, 0, recIssues[ErrorType].FItemImage));
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

  This method attempts to adds an element to the collection but raises an error IF there
  is already an object of the same name in the collection (duplicate detection).

  @precon  None.
  @postcon Attempts to adds an element to the collection but raises an error IF there is
           already an object of the same name in the collection (duplicate detection).

  @param   AElement as a TElementContainer
  @return  a TElementContainer

**)
function TElementContainer.AddUnique(AElement: TElementContainer): TElementContainer;

ResourceString
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';

Var
  iLine, iCol : Integer;
  strI : String;

begin
  iLine := AElement.Line;
  iCol := AElement.Column;
  strI := AElement.Identifier;
  Result := Add(AElement);
  If Result <> AElement Then
    AddIssue(Format(strDuplicateIdentifierFound, [strI, iLine, iCol]), scNone,
      'AddUnique', iLine, iCol, etError);
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
  Inc(FLength, System.Length(strToken));
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := FUtoken + UpperCase(strToken);
End;

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
  Name := Source.Name;
  FScope := Source.FScope;
  Line := Source.Line;
  Column := Source.Column;
  FComment :=  Source.Comment;
  FImageIndex := Source.FImageIndex;
  ClearTokens;
  For iToken :=  0 To Source.TokenCount - 1 Do
    AppendToken(Source.Tokens[iToken]);
end;

(**

  This method recrusively checks the documentation of the module. Descendants
  need to override this to implement document checking.

  @precon  None.
  @postcon Recrusively checks the documentation of the module.

  @param   boolCascade as a Boolean as a reference

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
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
constructor TElementContainer.Create(strName: String; AScope : TScope;
  iLine, iColumn : Integer; AImageIndex : TBADIImageIndex; AComment: TComment);

begin
  Inherited Create(strName, iLine, iColumn);
  FElements := TObjectList.Create(True);
  FComment := AComment;
  FScope := AScope;
  FImageIndex := AImageIndex;
  FSorted := True;
  FReferenced := False;
  FParent := Nil;
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
  FElements.Free;
  Inherited Destroy;
end;

(**

  This method returns the position of the named container in the current
  containers collection if found else returns the position (as a negative)
  where the item should be inserted in the collection.

  @precon  None.
  @postcon Returns the position of the named container in the current
           containers collection if found else returns the position (as a
           negative) where the item should be inserted in the collection.

  @param   strName  as a String
  @param   FindType as a TFindType
  @return  an Integer

**)
function TElementContainer.Find(strName: String;
  FindType : TFindType = ftName): Integer;

Var
  iFirst : Integer;
  iMid : Integer;
  iLast : Integer;
  iResult : Integer;

begin
  Result := -1;
  If FSorted Then
    Begin // Binary search...
      iFirst := 1;
      iLast := FElements.Count;
      While iFirst <= iLast Do
        Begin
          iMid := (iFirst + iLast) Div 2;
          If FindType = ftName Then
            iResult := CompareText(Elements[iMid].Name, strName)
          Else
            iResult := CompareText(Elements[iMid].Identifier, strName);
          If iResult = 0 Then
            Begin
              Result := iMid;
              Break;
            End
          Else If iResult > 0 Then
            iLast := iMid - 1
          Else
            iFirst := iMid + 1;
        End;
    End Else
    Begin // Sequential search...
      For iFirst := 1 To ElementCount Do
        If CompareText(Elements[iFirst].Name, strName) = 0 Then
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

  @param   strName  as a String
  @param   FindType as a TFindType
  @return  a TElementContainer

**)
function TElementContainer.FindElement(strName: String;
  FindType : TFindType = ftName): TElementContainer;

Var
  i : Integer;

begin
  Result := Nil;
  i := Find(strName, FindType);
  If i > 0 Then
    Result := Elements[i];
end;

(**

  This method finds the root element of the tree containing the current element.

  @precon  None.
  @postcon Finds the root element of the tree containing the current element.

  @return  a TElementContainer

**)
Function TElementContainer.FindRoot : TElementContainer;

begin
  Result := Self;
  While Result.Parent <> Nil Do
    Result := Result.Parent;
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
    If CompareText(strToken, Tokens[i].Token) = 0 Then
      Begin
        Result := i;
        Break;
      End;
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

  This method searches for references to the passed symbol in the passed
  section.

  @precon  None.
  @postcon Returns true if the symbol was found.

  @param   AToken  as a TTokenInfo
  @param   Section as a TLabelContainer
  @return  a Boolean

**)
Function TElementContainer.ReferenceSection(AToken : TTokenInfo;
  Section : TLabelContainer) : Boolean;

Var
  E: TElementContainer;

Begin
  Result := False;
  If Section <> Nil Then
    Begin
      E := Section.FindElement(AToken.Token);
      If E <> Nil Then
        Begin
          E.Referenced := True;
          AToken.Reference := trResolved;
          Result := True;
          Exit;
        End;
    End;
End;

(**

  This method does nothing other than call its parents ReferenceSymbol method.
  Descendant should override this method to resolve symbols in the code.

  @precon  None.
  @postcon Passes the processing of the symbol to its parent IF the parent
           exists.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TElementContainer.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
  If FParent <> Nil Then
    Result := FParent.ReferenceSymbol(AToken);
End;

(**


  This method recursively checks the referenced property and outputs a hint if
  any element is not refrernced which has a scope of Local or Private.

  @precon  None.
  @postcon Recursively checks the referenced property and outputs a hint if
           any element is not refrernced which has a scope of Local or Private.


**)
Procedure TElementContainer.CheckReferences;

Var
  i : Integer;

begin
  If doShowUnReferencedSymbols In BrowseAndDocItOptions.Options Then
    Begin
      If Scope In [scLocal, scPrivate] Then
        If Not Referenced Then
          AddIssue(Format(strUnreferencedLocal, [Identifier]),
            scNone, 'CheckReferences', Line, Column, etHint);
      For i := 1 To ElementCount Do
        Elements[i].CheckReferences;
    End;
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

  This method compares the current parameter with the given parameter and returns
  true if they have the same signature else returns false.

  @precon  None.
  @postcon Returns true if they signatures are the same else returns false.

  @param   Parameter as a TGenericParameter
  @return  a Boolean

**)
Function TGenericParameter.IsEquals(Parameter : TGenericParameter) : Boolean;

Begin
  Result := False;
  If Result And (ParamType <> Nil) And (Parameter.ParamType <> Nil) Then
    Result := Result And
      (ParamModifier = Parameter.ParamModifier) And
      (ParamType.AsString(False, False) = Parameter.ParamType.AsString(False, False));
End;

(** --------------------------------------------------------------------------

  TProperty Methods

 -------------------------------------------------------------------------- **)

(**

  This method checks the property passed against the property comments tags and
  highlights missing parameter comments, return tags and missing descriptions.

  @precon  Method is the property declaration that requires checking for document
           conflicts.
  @postcon The passed property is systematicaly check for errors.

  @param   boolCascade as a Boolean as a reference

**)
procedure TGenericProperty.CheckDocumentation(var boolCascade: Boolean);
begin
  If Identifier <> '' Then
    Begin
      CheckPropertyDocumentation;
      If doShowMethodMissingDocs In BrowseAndDocItOptions.Options Then
        If Comment <> Nil Then
          Begin
            CheckPropertyParamCount;
            CheckPropertyParameters;
            CheckPropertyReturns;
          End;
      Inherited CheckDocumentation(boolCascade);
    End;
end;

(**

  This method check the given property for general document problems, i.e.
  missing or no description.

  @precon  Method is valid property declaration to be checked for documentation.
  @postcon Checks the passed property for docuemntation errors.

**)
procedure TGenericProperty.CheckPropertyDocumentation;
begin
  If doShowPropertyMissingDoc In BrowseAndDocItOptions.Options Then
    Begin
      If Comment = Nil Then
        Begin
          AddDocumentConflict([FunctionType, QualifiedName], Line, Column,
            Comment, Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumented]);
          Exit;
        End;
      If Comment.TokenCount = 0 Then
        AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
          Format(strFunctionDocumentation, [FunctionType]),
          DocConflictTable[dctFunctionHasNoDesc]);
    End;
end;

(**

  This method checks the given property for the correct number of parameters and
  tags.

  @precon  Method is a property declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed property for errors in the parameter count
           documentation.

**)
procedure TGenericProperty.CheckPropertyParamCount;

Var
  i, j, k : Integer;
  boolMissing : Boolean;

Begin
  j := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'param' Then
      Inc(j);
  k := 0;
  boolMissing := True;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'precon' Then
      Begin
        Inc(k);
        boolMissing := boolMissing And (Comment.Tag[i].TokenCount = 0);
      End;
  If doShowPropertyMissingPreCons In BrowseAndDocItOptions.Options Then
    If boolMissing Then
      AddDocumentConflict([FunctionType, QualifiedName], Comment.Line,
        Comment.Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionPreconNotDocumented]);
  If doShowPropertyDiffPropParamCount In BrowseAndDocItOptions.Options Then
    If (ParameterCount <> j) Then
      AddDocumentConflict([FunctionType, QualifiedName, ParameterCount, j], Line,
        Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionDiffParamCount]);
  If doShowPropertyMissingPreCons In BrowseAndDocItOptions.Options Then
    If k < 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPreCon]);
  If doShowPropertyMissingPreCons In BrowseAndDocItOptions.Options Then
    If k > 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPrecons]);
end;

(**

  This method checks the given Property for the correct parameter tags and
  pre conditions.

  @precon  Method is a property declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed method for errors in the parameter documentation.

**)
procedure TGenericProperty.CheckPropertyParameters;

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
            (LowerCase(Tag[j].Tokens[0].Token) = Lowercase(Parameters[i].Identifier)) Then
            Begin
              iFound := j;
              Break;
            End;
      If doShowPropertyUndocumentedParams In BrowseAndDocItOptions.Options Then
        If iFound = -1 Then
          AddDocumentConflict([Parameters[i].Identifier, FunctionType, QualifiedName],
            Line, Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumentedParam]);
      // Parameter type
      If iFound > -1 Then
        With Comment Do
          Begin
            strType := '';
            For j := 6 To Tag[iFound].TokenCount - 1 Do
              Begin
                If (Tag[iFound].Tokens[j].TokenType In [ttSymbol]) And
                  (Tag[iFound].Tokens[j].Token <> '.') Then
                  Break;
                strType := strType + Tag[iFound].Tokens[j].Token;
              End;
            strType := Trim(strType);
            strParam := BuildLangIndepRep(Parameters[i]);
            If doShowPropertyIncorrectParamType In BrowseAndDocItOptions.Options Then
              If CompareText(strType, strParam) <> 0 Then
                AddDocumentConflict([Parameters[i].Identifier, FunctionType,
                  QualifiedName, strParam], Tag[iFound].Line, Tag[iFound].Column,
                  Comment, Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectParamType]);
          End;
    End;
end;

(**

  This method checks the given property for the correct return information and
  tags.

  @precon  Method is a property declaration that needs the be check for
           document conflicts.
  @postcon The passed method return is checked for errors.

**)
procedure TGenericProperty.CheckPropertyReturns;

Var
  i, iNumOfPostCons : Integer;
  iReturnTagIndex : Integer;
  strType : String;
  strReturn : String;

Begin
  iReturnTagIndex := Comment.FindTag('return');
  iNumOfPostCons := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'postcon' Then
      Begin
        Inc(iNumOfPostCons);
        If doShowPropertyMissingPostCons in BrowseAndDocItOptions.Options Then
          If Comment.Tag[i].TokenCount = 0 Then
            AddDocumentConflict([FunctionType, QualifiedName], Comment.Tag[i].Line,
              Comment.Tag[i].Column, Comment, Format(strFunctionDocumentation,
              [FunctionType]), DocConflictTable[dctFunctionPostconNotDocumented]);
      End;
  If RequiresReturn Then
    Begin;
      If iReturnTagIndex = -1 Then
        Begin
          If doShowPropertyUndocumentedReturn In BrowseAndDocItOptions.Options Then
            AddDocumentConflict([FunctionType, QualifiedName], Line, Column,
              Comment, Format(strFunctionDocumentation, [FunctionType]),
              DocConflictTable[dctFunctionUndocumentedReturn])
        End Else
        Begin
          If doShowPropertyIncorrectReturnType In BrowseAndDocItOptions.Options Then
            Begin
              strType := '';
              strReturn := '';
              For i := 2 To Comment.Tag[iReturnTagIndex].TokenCount - 1 Do
                Begin
                  If (Comment.Tag[iReturnTagIndex].Tokens[i].TokenType In [ttSymbol]) And
                    (Comment.Tag[iReturnTagIndex].Tokens[i].Token <> '.') Then
                    Break;
                  strType := strType + Comment.Tag[iReturnTagIndex].Tokens[i].Token;
                End;
              strType := Trim(strType);
              If ReturnType <> Nil Then
                strReturn := ReturnType.AsString(False, False);
              If CompareText(strReturn, strType) <> 0 Then
                AddDocumentConflict([FunctionType, QualifiedName, strReturn],
                  Comment.Tag[iReturnTagIndex].Line, Comment.Tag[iReturnTagIndex].Column, Comment,
                  Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectReturntype]);
            End;
        End;
    End Else
      If Comment.FindTag('return') >= 0 Then
        AddDocumentConflict([FunctionType, QualifiedName],
          Line, Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
          DocConflictTable[dctFunctionReturnNotRequired]);
  If doShowPropertyMissingPostCons in BrowseAndDocItOptions.Options Then
    If iNumOfPostCons = 0 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPostCon]);
  If doShowPropertyMissingPostCons in BrowseAndDocItOptions.Options Then
    If (iNumOfPostCons > 1) And (iReturnTagIndex > -1) Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPostCons]);
end;

(**

  This method returns the function type of 'Property' for the documentation
  of problems with methods.

  @precon  None.
  @postcon Returns the function type of 'Property' for the documentation
           of problems with methods.

  @return  a String

**)
function TGenericProperty.FunctionType: String;
begin
  Result := 'Property';
end;


(**

  This is a getter method for the QualifiedName property.

  @precon  None.
  @postcon Returns the qualified name of the property.

  @return  a String

**)
function TGenericProperty.GetQualifiedName: String;
begin
  Result := Identifier;
end;

(**

  This method returns true as all properties require return types.

  @precon  None.
  @postcon Returns true.

  @return  a Boolean

**)
Function TGenericProperty.RequiresReturn : Boolean;

Begin
  Result := ReturnType <> Nil;
End;

(** --------------------------------------------------------------------------

  TGenericFunction Methods

 -------------------------------------------------------------------------- **)

(**

  This method adds the given parameter to the internal list.

  @precon  AParameter must be a valid instance.
  @postcon Adds the given parameter to the internal list.

  @param   AParameter as a TGenericParameter

**)
procedure TGenericFunction.AddParameter(AParameter: TGenericParameter);
begin
  FParameters.Add(AParameter);
end;

(**

  This method checks the method passed against the method comments tags and
  highlights missing parameter comments, return tags and missing descriptions.

  @precon  Method is the method declaration that requires checking for document
           conflicts.
  @postcon The passed method is systematicaly check for errors.

  @param   boolCascade as a Boolean as a reference

**)
procedure TGenericMethodDecl.CheckDocumentation(var boolCascade : Boolean);

Begin
  If (Not FForwardDecl) And (Identifier <> '') Then
    Begin
      CheckMethodDocumentation;
      If doShowMethodMissingDocs In BrowseAndDocItOptions.Options Then
        If Comment <> Nil Then
          Begin
            CheckMethodParamCount;
            CheckMethodParameters;
            CheckMethodReturns;
          End;
      Inherited CheckDocumentation(boolCascade);
    End;
end;

(**

  This method check the given method for general document problems, i.e.
  missing or no description.

  @precon  Method is valid method declaration to be checked for documentation.
  @postcon Checks the passed method for docuemntation errors.

**)
Procedure TGenericMethodDecl.CheckMethodDocumentation;

Begin
  If doShowMethodMissingDocs In BrowseAndDocItOptions.Options Then
    Begin
      If Comment = Nil Then
        Begin
          AddDocumentConflict([FunctionType, QualifiedName], Line, Column,
            Comment, Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumented]);
          Exit;
        End;
      If Comment.TokenCount = 0 Then
        AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
          Format(strFunctionDocumentation, [FunctionType]),
          DocConflictTable[dctFunctionHasNoDesc]);
    End;
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
  boolMissing : Boolean;

Begin
  j := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'param' Then
      Inc(j);
  k := 0;
  boolMissing := True;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'precon' Then
      Begin
        Inc(k);
        boolMissing := boolMissing And (Comment.Tag[i].TokenCount = 0);
      End;
  If doShowMethodMissingPreCons In BrowseAndDocItOptions.Options Then
    If boolMissing Then
      AddDocumentConflict([FunctionType, QualifiedName], Comment.Line,
        Comment.Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionPreconNotDocumented]);
  If doShowMethodDiffParamCount In BrowseAndDocItOptions.Options Then
    If (ParameterCount <> j) Then
      AddDocumentConflict([FunctionType, QualifiedName, ParameterCount, j], Line,
        Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionDiffParamCount]);
  If doShowMethodMissingPreCons In BrowseAndDocItOptions.Options Then
    If k < 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPreCon]);
  If doShowMethodMissingPreCons In BrowseAndDocItOptions.Options Then
    If k > 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPrecons]);
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
            (LowerCase(Tag[j].Tokens[0].Token) = Lowercase(Parameters[i].Identifier)) Then
            Begin
              iFound := j;
              Break;
            End;
      If doShowMethodUndocumentedParams In BrowseAndDocItOptions.Options Then
        If iFound = -1 Then
          AddDocumentConflict([Parameters[i].Identifier, FunctionType, QualifiedName],
            Line, Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumentedParam]);
      // Parameter type
      If iFound > -1 Then
        With Comment Do
          Begin
            strType := '';
            For j := 6 To Tag[iFound].TokenCount - 1 Do
              Begin
                If (Tag[iFound].Tokens[j].TokenType In [ttSymbol]) And
                  (Tag[iFound].Tokens[j].Token <> '.') Then
                  Break;
                strType := strType + Tag[iFound].Tokens[j].Token;
              End;
            strType := Trim(strType);
            strParam := BuildLangIndepRep(Parameters[i]);
            If doShowMethodIncorrectParamType In BrowseAndDocItOptions.Options Then
              If CompareText(strType, strParam) <> 0 Then
                AddDocumentConflict([Parameters[i].Identifier, FunctionType,
                  QualifiedName, strParam], Tag[iFound].Line, Tag[iFound].Column,
                  Comment, Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectParamType]);
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
  i, iNumOfPostCons : Integer;
  iReturnTagIndex : Integer;
  strType : String;
  strReturn : String;

Begin
  iReturnTagIndex := Comment.FindTag('return');
  iNumOfPostCons := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'postcon' Then
      Begin
        Inc(iNumOfPostCons);
        If doShowMethodMissingPostCons in BrowseAndDocItOptions.Options Then
          If Comment.Tag[i].TokenCount = 0 Then
            AddDocumentConflict([FunctionType, QualifiedName], Comment.Tag[i].Line,
              Comment.Tag[i].Column, Comment, Format(strFunctionDocumentation,
              [FunctionType]), DocConflictTable[dctFunctionPostconNotDocumented]);
      End;
  If RequiresReturn Then
    Begin;
      If iReturnTagIndex = -1 Then
        Begin
          If doShowMethodUndocumentedReturn In BrowseAndDocItOptions.Options Then
            AddDocumentConflict([FunctionType, QualifiedName], Line, Column,
              Comment, Format(strFunctionDocumentation, [FunctionType]),
              DocConflictTable[dctFunctionUndocumentedReturn])
        End Else
        Begin
          If doShowMethodIncorrectReturnType In BrowseAndDocItOptions.Options Then
            Begin
              strType := '';
              strReturn := '';
              For i := 2 To Comment.Tag[iReturnTagIndex].TokenCount - 1 Do
                Begin
                  If (Comment.Tag[iReturnTagIndex].Tokens[i].TokenType In [ttSymbol]) And
                    (Comment.Tag[iReturnTagIndex].Tokens[i].Token <> '.') Then
                    Break;
                  strType := strType + Comment.Tag[iReturnTagIndex].Tokens[i].Token;
                End;
              strType := Trim(strType);
              If ReturnType <> Nil Then
                strReturn := ReturnType.AsString(False, False);
              If CompareText(strReturn, strType) <> 0 Then
                AddDocumentConflict([FunctionType, QualifiedName, strReturn],
                  Comment.Tag[iReturnTagIndex].Line, Comment.Tag[iReturnTagIndex].Column, Comment,
                  Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectReturntype]);
            End;
        End;
    End Else
      If Comment.FindTag('return') >= 0 Then
        AddDocumentConflict([FunctionType, QualifiedName],
          Line, Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
          DocConflictTable[dctFunctionReturnNotRequired]);
  If doShowMethodMissingPostCons in BrowseAndDocItOptions.Options Then
    If iNumOfPostCons = 0 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPostCon]);
  If doShowMethodMissingPostCons in BrowseAndDocItOptions.Options Then
    If (iNumOfPostCons > 1) And (iReturnTagIndex <> -1) Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPostCons]);
End;

(**

  This is a constructor for the TGenericFunction class.

  @precon  None.
  @postcon Creates an object list for parameters and initialises the return type
           to nil.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
constructor TGenericFunction.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FParameters := TObjectList.Create(True);
  FReturnType := Nil;
  FStartLine  := -1;
  FEndLine    := -1;
end;

(**

  This is a destructor for the TGenericFunction class.

  @precon  None.
  @postcon Frees the memory for the parameters and return type.

**)
destructor TGenericFunction.Destroy;
begin
  FReturnType.Free;
  FParameters.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of parameters associated with the method.

  @return  an Integer

**)
function TGenericFunction.GetParameterCount: Integer;
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
function TGenericFunction.GetParameters(iIndex: Integer): TGenericParameter;
begin
  Result := FParameters[iIndex] As TGenericParameter;
end;

(**

  This method returns the number of lines of code in the function.

  @precon  None.
  @postcon Returns the number of lines of code in the function.

  @return  an Integer

**)
function TGenericFunction.LineofCode: Integer;
begin
  Result := FEndLine - FStartLine + 1;
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
  @param   strName    as a String
  @param   AScope     as a TScope
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
Constructor TGenericMethodDecl.Create(MethodType : TMethodType; strName : String;
  AScope : TScope; iLine, iCol : Integer);

Var
  AImageIndex : TBADIImageIndex;

Begin
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
  FClassNames := TStringList.Create;
  Comment := Nil;
  FExt := '';
  FMsg := '';
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
  FClassNames.Free;
  Inherited Destroy;
End;

(**

  This method returns the function type of 'Method' for the documentation
  of problems with methods.

  @precon  None.
  @postcon Returns the function type of 'Method' for the documentation
           of problems with methods.

  @return  a String

**)
function TGenericMethodDecl.FunctionType: String;
begin
  Result := 'Method';
end;

(**

  This method returns a fully qualified name for the method.

  @precon  None.
  @postcon Returns a fully qualified name for the method.

  @return  a String

**)
Function TGenericMethodDecl.GetQualifiedName : String;

var
  i: Integer;

Begin
  Result := '';
  For i := 0 To FClassNames.Count - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + '.';
      Result := Result + FClassNames[i];
    End;
  If Result <> '' Then
    Result := Result + '.';
  Result := Result + Identifier;
End;

(**

  This method returns true for functions only.

  @precon  None.
  @postcon Returns true for function only.

  @return  a Boolean

**)
Function TGenericMethodDecl.RequiresReturn : Boolean;

Begin
  Result := MethodType = mtFunction;
End;
(**

  This is a setter method for the Msg property.

  @precon  Value is the new value to assign to the Msg property.
  @postcon Sets the Message property for the method.

  @param   Value as a String as a constant

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

  @param   Value as a String as a constant

**)
procedure TGenericMethodDecl.SetExt(const Value: String);
begin
  If FExt <> Value Then
    FExt := Value;
end;

(**


  This is the constructor method for the TDocError class.


  @precon  strMsg is the error message to create a doc error for, iLine is the

           line number of the error, iCol is the column number for the

           message, strExceptionMethod is the name of the method the

           error occurred in and ErrType determines if the mesage is a

           warning or an error.

  @postcon Initialises the class.


  @param   strMsg      as a String
  @param   AScope      as a TScope
  @param   strMethod   as a String
  @param   iLine       as an Integer
  @param   iCol        as an Integer
  @param   AImageIndex as a TBADIImageIndex

**)
constructor TDocIssue.Create(strMsg : String; AScope : TScope; strMethod: String;
  iLine, iCol: Integer; AImageIndex : TBADIImageIndex);

Begin
  Inherited Create(Format('%4.4d', [iDocConflictCounter]), AScope, iLine, iCol,
    AImageIndex, Nil);
  Inc(iDocConflictCounter);
  FMsg := strMsg;
  FMethod := strMethod;
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Override the default method and returns the Document Error Message .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDocIssue.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := FMsg;
  If doShowParserErrorOrigin In BrowseAndDocItOptions.Options Then
    Result := Result + Format(' [%s]', [FMethod]);
end;

{ TDocumentConflict }

(**

  This is the constructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Initialises the Conflict class.

  @param   Args               as an Array Of Const as a constant
  @param   iIdentLine         as an Integer
  @param   iIdentColumn       as an Integer
  @param   iCommentLine       as an Integer
  @param   iCommentCol        as an Integer
  @param   strDocConflictMsg  as a String
  @param   strDocConflictDesc as a String
  @param   AImageIndex        as a TBADIImageIndex

**)
constructor TDocumentConflict.Create(Const Args: Array of Const; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      strDocConflictMsg, strDocConflictDesc : String; AImageIndex : TBADIImageIndex);

begin
  Inherited Create(Format('%4.4d', [iDocConflictCounter]), scGlobal, iIdentLine,
    iIdentColumn, AImageIndex, Nil);
  Inc(iDocConflictCounter);
  FMessage := Format(strDocConflictMsg , Args);
  FCommentLine := iCommentLine;
  FCommentColumn := iCommentCol;
  Comment := TComment.Create(strDocConflictDesc, 0, 0);
end;

(**

  This is the destructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Frees the comment.

**)
destructor TDocumentConflict.Destroy;
begin
  Comment.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Return the document conflict message .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDocumentConflict.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;

begin
  Result := FMessage;
end;

(**

  This method adds the comment to the comment collection if it has content
  and is more than 1 line different from the last added comemnt, else appends
  the contents of the comment to the last added comment and frees the passed
  comment.

  @precon  None.
  @postcon Adds the comment to the comment collection if it has content
           and is more than 1 line different from the last added comemnt, else
           appends the contents of the comment to the last added comment and
           frees the passed comment.

  @param   C as a TComment

**)
procedure TBaseLanguageModule.AddBodyComment(C: TComment);

var
  Cmt: TComment;

begin
  If C <> Nil Then
    If (C.TokenCount > 0) Or (C.TagCount > 0) Then
      Begin
        If FBodyComment.Count > 0 Then
          Begin
            Cmt := BodyComment[BodyCommentCount - 1];
            If FLastBodyCommentLine + 1 = C.Line Then
              Begin
                Cmt.AppendComment(Cmt, C);
                Cmt.TrimTrailingWhiteSpace;
                FLastBodyCommentLine := C.Line;
                C.Free;
              End Else
              Begin
                FBodyComment.Add(C);
                FLastBodyCommentLine := C.Line;
              End;
          End Else
          Begin
            FBodyComment.Add(C);
            FLastBodyCommentLine := C.Line;
          End;
      End Else
        C.Free;
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

  This method adds a timer count to the modules OpTickCount collection. This
  can be used to provide timing / profiling information on operations.

  @precon  None.
  @postcon Adds a timer count to the modules OpTickCount collection. This
           can be used to provide timing / profiling information on operations.

  @param   strLabel as a String

**)
procedure TBaseLanguageModule.AddTickCount(strLabel: String);

begin
  FTickList.Add(TTickOption.Create(strLabel, GetTickCount));
end;

(**

  This is the constructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Initialise this base class and Tokensizes the passed stream of
           characters.

  @param   Source        as a String
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
constructor TBaseLanguageModule.CreateParser(Source : String;
  strFileName : String; IsModified : Boolean; ModuleOptions : TModuleOptions);

begin
  Inherited Create(strFileName, scGlobal, 0, 0, iiModule, Nil);
  FModuleOptions := ModuleOptions;
  FFileName := strFileName;
  FModified := IsModified;
  FOwnedItems := TObjectList.Create(True);
  FTokenIndex := 0;
  FPreviousTokenIndex := -1;
  FTickList := TObjectList.Create(True);
  FBodyComment := TObjectList.Create(True);
  FModuleName := strFileName;
  FModuleNameCol := 0;
  FModuleNameLine := 0;
  FCompilerDefs := TStringList.Create;
  FCompilerDefs.Sorted := True;
  FCompilerDefs.Duplicates := dupIgnore;
  {$IFDEF D0006}
  FCompilerDefs.CaseSensitive := False;
  {$ENDIF}
  FCompilerConditionStack := TList.Create;
  FCompilerConditionUndoStack := TList.Create;
  FCommentClass := CommentClass;
  FTokenStackTop := -1;
  SetLength(FTokenStack, 10);
end;

(**

  This method returns the default profilin template for this module.

  @precon  None.
  @postcon Returns the default profilin template for this module.

  @return  a String

**)
function TBaseLanguageModule.DefaultProfilingTemplate: String;

begin
  Result := '$METHODCODE$';
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
  FCompilerConditionUndoStack.Free;
  FCompilerConditionStack.Free;
  FCompilerDefs.Free;
  FBodyComment.Free;
  FTickList.Free;
  FDocErrors.Free;
  FOwnedItems.Free;
  inherited;
end;

(**

  This method appends the pased token string to the previous token.

  @precon  None.
  @postcon Appends the pased token string to the previous token.

  @param   strToken as a String

**)
Procedure TBaseLanguageModule.AppendToLastToken(strToken : String);

Begin
  Tokens[TokenCount - 1].Append(strToken);
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

  This is a getter method for the Lines property.

  @precon  None.
  @postcon Returns the number Lines in the file.

  @return  an Integer

**)
function TBaseLanguageModule.GetLines: Integer;
begin
  Result := 0;
  If TokenCount > 0 Then
    Result := (Tokens[TokenCount - 1] As TTokenInfo).Line;
end;

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

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TBaseLanguageModule.AsString(boolShowIdentifier,
     boolForDocumentation : Boolean) : String;

begin
  Result := ExtractFileName(Name);
end;

(**

  This method adds the current toen to the passed generic container if it is not
  nil and moves to the next non comment token.

  @precon  None.
  @postcon Adds the current toen to the passed generic container if it is not
           nil and moves to the next non comment token.

  @param   Container as a TElementContainer

**)
Procedure TBaseLanguageModule.AddToExpression(Container : TElementContainer);

Begin
  If Container <> Nil Then
    Container.AppendToken(Token);
  NextNonCommentToken;
End;

(**

  This method check the current token against the passed string and if true
  returns true and addeds the token to the generic container.

  @precon  None.
  @postcon Check the current token against the passed string and if true
           returns true and addeds the token to the generic container.

  @param   strToken  as a String
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TBaseLanguageModule.IsToken(strToken : String; Container : TElementContainer): Boolean;

Begin
  Result := strToken = Token.Token;
  If Result Then
    AddToExpression(Container);
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

  This is a getter method for the Bytes property.

  @precon  None.
  @postcon Returns the number of bytes in the file.

  @return  an Int64

**)
function TBaseLanguageModule.GetBytes: Int64;

begin
  Result := 0;
  If TokenCount > 0 Then
    Result := (Tokens[TokenCount - 1] As TTokenInfo).BufferPos +
      (Tokens[TokenCount - 1] As TTokenInfo).Length - 1;
end;

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
  @return  an Int64

**)
function TBaseLanguageModule.GetOpTickCount(strStart, strFinish : String): Int64;

Var
  i : Integer;
  iStart, iFinish : Integer;

begin
  Result := -1;
  iStart := 0;
  iFinish := 0;
  For i := 0 To FTickList.Count - 1 Do
    Begin
      If CompareText((FTickList[i] As TTickOption).Name, strStart) = 0 Then
        iStart := i;
      If CompareText((FTickList[i] As TTickOption).Name, strFinish) = 0 Then
        iFinish := i;
    End;
  If (iStart > -1) And (iFinish > -1) Then
    Result :=
      Integer((FTickList[iFinish] As TTickOption).Count) -
      Integer((FTickList[iStart] As TTickOption).Count);
end;

(**

  This is a getter method for the OpTickCountByIndex property.

  @precon  iIndex must be a valid index.
  @postcon Returns the tick count associated with the passed index.

  @param   iIndex as an Integer
  @return  an Int64

**)
function TBaseLanguageModule.GetOpTickCountByIndex(iIndex: Integer): Int64;

begin
  Result := (FTickList[iIndex] As TTickOption).Count;
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
  Result := (FTickList[iIndex] As TTickOption).Name;
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

  This is a getter method for the Token property.

  @precon  None.
  @postcon Returns a token info object for the current token.

  @return  a TTokenInfo

**)
Function TBaseLanguageModule.GetToken : TTokenInfo;

Begin
  If FTokenIndex >= TokenCount Then
    Begin
      AddIssue(strUnExpectedEndOfFile, scNone, 'GetToken', 0, 0, etError);
      Raise EParserAbort.Create('Parsing Aborted!');
    End;
  Result := Tokens[FTokenIndex] As TTokenInfo;
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
  Result := (Token.TokenType = ttFileEnd) Or (FTokenIndex >= TokenCount);
End;

(**

  This method seeks the first non-comment token in the source code which match
  one of the passed tokens.

  @precon  The Tokens passed MUST be sorted in lowercase and in ascending order.
  @postcon Seeks the first non-comment token in the source code which match
           one of the passed tokens.

  @param   strMsg      as a String
  @param   strMethod   as a String
  @param   strParam    as a String
  @param   SeekTokens  as an Array Of string
  @param   SeekToken   as a TSeekToken

**)
Procedure TBaseLanguageModule.ErrorAndSeekToken(strMsg, strMethod, strParam : String;
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
    0: AddIssue(Format(strMsg, [Token.Line, Token.Column]),
           scGlobal, strMethod, Token.Line, Token.Column, etError);
    1: AddIssue(Format(strMsg, [strParam, Token.Line, Token.Column]),
           scGlobal, strMethod, Token.Line, Token.Column, etError);
    2: AddIssue(Format(strMsg, [strParam, Token.Token, Token.Line,
         Token.Column]), scGlobal, strMethod, Token.Line, Token.Column, etError);
  Else
    AddIssue(strNotEnoughStrings, scGlobal, strMethod, Token.Line, Token.Column, etError);
  End;
  NextNonCommentToken;
  While Not IsKeyWord(Token.Token, SeekTokens) Do
    NextNonCommentToken;
  If SeekToken = stFirst Then
    NextNonCommentToken;
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
  FShouldUndoCompilerStack := False;
  If Token.TokenType = ttCompilerDirective Then // Catch first token as directive
    Begin
      ProcessCompilerDirective(iSkip);
      FShouldUndoCompilerStack := True;
    End;
  Repeat
    If (Token.TokenType In [ttLineComment, ttBlockComment]) And
      (FLastComment <> Token) And (FCommentClass <> Nil) Then
      Begin
        C := FCommentClass.CreateComment(Token.Token,
          Token.Line, Token.Column);
        If C <> Nil Then
          Begin
            AddBodyComment(C);
            FLastComment := Token;
          End;
      End;
    If Not (Tokens[FTokenIndex].TokenType In [ttLineComment, ttBlockComment,
      ttCompilerDirective]) And (iSkip = 0) Then
      FPreviousTokenIndex := FTokenIndex;
    NextToken;
    If Token.TokenType = ttCompilerDirective Then
      Begin
        ProcessCompilerDirective(iSkip);
        FShouldUndoCompilerStack := True;
      End;
    boolContinue := (
      (
        Token.TokenType In [ttLineComment, ttBlockComment, ttCompilerDirective]
      ) And
      Not EndOfTokens
    ) Or (iSkip > 0)
  Until Not boolContinue;
end;

(**

  This method removes the token position from the stack and sets the token position to
  that value.

  @precon  None.
  @postcon The token is moved back to the position of the token  on the top of the stack
           and the stack is decremented.

**)
Procedure TBaseLanguageModule.PopTokenPosition;

Begin
  If FTokenStackTop > -1 Then
    Begin
      FTokenIndex := FTokenStack[FTokenStackTop];
      Dec(FTokenStackTop);
    End Else
      Raise EParserError.Create('Cannot pop the token position stack.');
End;

(**

  This method moves the toke to the previous token in the token list or raises
  an EDocException.

  @precon  None.
  @postcon Moves the token to the previous token in the token list or raises an
           EDocException.

**)
procedure TBaseLanguageModule.PreviousToken;
begin
  Dec(FTokenIndex);
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
    Result := Tokens[FPreviousTokenIndex] As TTokenInfo
  Else
    For i := FTokenIndex - 1 DownTo 0 Do
      If Not ((Tokens[i] As TTokenInfo).TokenType In [ttLineComment,
        ttBlockComment, ttCompilerDirective]) Then
        Begin
          Result := Tokens[i] As TTokenInfo;
          Break;
        End;
end;

(**

  This method pushes the current token position on to the top of a stack.

  @precon  None.
  @postcon the current token position is pushed ont to the top of a stack.

**)
Procedure TBaseLanguageModule.PushTokenPosition;

Var
  T : TArrayOfInteger;
  i: Integer;

Begin
  Inc(FTokenStackTop);
  If FTokenStackTop > High(FTokenStack) Then
    Begin
      SetLength(T, Length(FTokenStack) + 10);
      For i := Low(FTokenStack) To High(FTokenStack) Do
        T[i] := FTokenStack[i];
      FTokenStack := T;
    End;
  FTokenStack[FTokenStackTop] := FTokenIndex;
End;

(**

  This method rolls back the current token to the previous valid token if there
  is one else searches for a previous token.

  @precon  None.
  @postcon Rolls back the current token to the previous valid token if there
           is one else searches for a previous token.

**)
Procedure TBaseLanguageModule.RollBackToken;
var
  iStackTop: Integer;

Begin
  If FShouldUndoCompilerStack Then
    Begin
      iStackTop := CompilerConditionUndoStack.Count - 1;
      If iStackTop >= 0 Then
        Begin
          CompilerConditionStack.Add(CompilerConditionUndoStack[iStackTop]);
          CompilerConditionUndoStack.Delete(iStackTop);
        End;
    End;
  If FPreviousTokenIndex >= 0 Then
    FTokenIndex := FPreviousTokenIndex
  Else
    Begin
      Dec(FTokenIndex);
      While (FTokenIndex > 0) And (Tokens[FTokenIndex].TokenType In [ttLineComment,
        ttBlockComment, ttCompilerDirective]) Do
        Dec(FTokenIndex);
      If FTokenIndex < 0 Then
        Begin
          AddIssue(strUnExpectedStartOfFile, scNone, 'RollBackToken', 0, 0, etError);
          Raise EParserAbort.Create('Parsing Aborted!');
        End;
    End;
End;

(**

  This method checks the module comment for various type of documentation
  errors.

  @precon  Module is the module to check.
  @postcon The modules comment is checked for errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TBaseLanguageModule.CheckDocumentation(var boolCascade : Boolean);

Var
  i : Integer;
  strDate : String;
  dtDate, dtFileDate : TDateTime;
  Tag : TTag;

Begin
  If Not (doShowConflicts In BrowseAndDocItOptions.Options) Then
    Exit;
  For i := 0 To BrowseAndDocItOptions.ExcludeDocFiles.Count -1 Do
    If Like(BrowseAndDocItOptions.ExcludeDocFiles[i], FFileName) Then
      Exit;
  If (Comment <> Nil) And (Comment.FindTag('stopdocumentation') >= 0) Then
    Begin
      boolCascade := False;
      Exit;
    End;
  If doShowUndocumentedModule In BrowseAndDocItOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([], ModuleNameLine, ModuleNameCol, Comment,
        strModuleDocumentation, DocConflictTable[dctModuleMissingDocumentation]);
  If Comment <> Nil Then
    Begin
      If (doShowMissingModuleDate In BrowseAndDocItOptions.Options) Then
        Begin
          i := Comment.FindTag('date');
          If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([FormatDateTime('dd mmm yyyy', Now)],
              ModuleNameLine, ModuleNameCol, Comment,
              strModuleDocumentation, DocConflictTable[dctModuleMissingDate])
          Else
            Begin
              Tag := Comment.Tag[i];
              strDate := Tag.AsString(80, False);
              If Modified Then
                dtFileDate := Now
              Else
                {$IFDEF D2006}
                FileAge(FileName, dtFileDate);
                {$ELSE}
                dtFileDate := FileDateToDateTime(FileAge(FileName));
                {$ENDIF}
              Try
                dtDate := ConvertDate(strDate);
                If Int(dtDate) <> Int(dtFileDate) Then
                  AddDocumentConflict([strDate, FormatDateTime('dd mmm yyyy', dtFileDate)],
                    Tag.Line, Tag.Column, Comment, strModuleDocumentation,
                    DocConflictTable[dctModuleIncorrectDate]);
              Except
                AddDocumentConflict([strDate, FormatDateTime('dd mmm yyyy', dtFileDate)],
                  Tag.Line, Tag.Column, Comment, strModuleDocumentation,
                  DocConflictTable[dctModuleCheckDateError]);
              End
            End;
        End;
      If (doShowMissingModuleVersion In BrowseAndDocItOptions.Options) Then
        Begin
          i := Comment.FindTag('version');
          If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, Comment,
              strModuleDocumentation, DocConflictTable[dctModuleMissingVersion])
        End;
      If (doShowMissingModuleAuthor In BrowseAndDocItOptions.Options) Then
        Begin
          i := Comment.FindTag('author');
          If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, Comment,
              strModuleDocumentation, DocConflictTable[dctModuleMissingAuthor])
        End;
    End;
  Inherited CheckDocumentation(boolCascade);
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericTypeDecl.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowUndocumentedTypes In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      Begin
        AddDocumentConflict([Identifier], Line, Column, Comment,
          strTypeDocumentation, DocConflictTable[dctTypeClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericConstant.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowUndocumentedConsts In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      Begin
        AddDocumentConflict([Identifier], Line, Column, Comment,
          strConstantDocumentation, DocConflictTable[dctConstantClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericVariable.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowUndocumentedVars In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      Begin
        AddDocumentConflict([Identifier], Line, Column, Comment,
          strVariableDocumentation, DocConflictTable[dctVariableClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
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
  FSpecialTags := TStringList.Create;
  // Create a default set of Special Tags.
  FSpecialTags.AddObject('todo=Things To Do', TObject(iShowInTree Or iAutoExpand Or iShowInDoc));
  FSpecialTags.AddObject('precon=Pre-Conditions', TObject(0));
  FSpecialTags.AddObject('postcon=Post-Conditions', TObject(0));
  FSpecialTags.AddObject('param=Parameters', TObject(0));
  FSpecialTags.AddObject('return=Returns', TObject(0));
  FSpecialTags.AddObject('note=Notes', TObject(0));
  FSpecialTags.AddObject('see=Also See', TObject(0));
  FSpecialTags.AddObject('exception=Exception Raised', TObject(0));
  FSpecialTags.AddObject('bug=Known Bugs', TObject(iShowInTree Or iAutoExpand Or iShowInDoc));
  FSpecialTags.AddObject('debug=Debugging Code', TObject(iShowInTree Or iAutoExpand Or iShowInDoc));
  FSpecialTags.AddObject('date=Date Code Last Updated', TObject(0));
  FSpecialTags.AddObject('author=Code Author', TObject(0));
  FSpecialTags.AddObject('version=Code Version', TObject(0));
  FExpandedNodes := TStringList.Create;
  FExpandedNodes.Sorted := True;
  FExpandedNodes.Duplicates := dupIgnore;
  FINIFileName := BuildRootKey(Nil, Nil);
  FScopesToRender := [scPrivate, scProtected, scPublic, scPublished];
  FExcludeDocFiles := TStringList.Create;
  FMethodDescriptions := TStringList.Create;
  FScopesToDocument := [scPublished, scPublic, scProtected, scPrivate];
  FProfilingCode := TStringList.Create;
  LoadSettings;
End;

(**

  This is the destructor method for the TBrowseAndDocItOptions class.

  @precon  none.
  @postcon Does onthing at the moment except call the inherited destroy method.

**)
Destructor TBrowseAndDocItOptions.Destroy;

Begin
  SaveSettings;
  FProfilingCode.Free;
  FMethodDescriptions.Free;
  FExcludeDocFiles.Free;
  FExpandedNodes.Free;
  FSpecialTags.Free;
  FDefines.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the ProfilingCode property.

  @precon  None.
  @postcon Returns the profiling code template for the given filename.

  @param   Module as a TBaseLanguageModule
  @return  a String

**)
function TBrowseAndDocItOptions.GetProfilingCode(Module : TBaseLanguageModule): String;

Var
  strExt : String;

begin
  strExt := ExtractFileExt(Module.FileName);
  Result := StringReplace(FProfilingCode.Values[strExt], '|', #13#10, [rfReplaceAll]);
  If Result = '' Then
    Result := Module.DefaultProfilingTemplate;
end;

(**


  This is a getter method for the TokenFontInfo property.

  @precon  None.
  @postcon Retursn the record information for the token type.


  @param   ATokenType as a TBADITokenType
  @return  a TTokenFontInfo

**)
function TBrowseAndDocItOptions.GetTokenFontInfo(ATokenType: TBADITokenType): TTokenFontInfo;
begin
  Result := FTokenFontInfo[ATokenType];
end;

(**

  This method loads the applications settings from an ini file.

  @precon  None.
  @postcon Loads the applications settings from an ini file.

**)
procedure TBrowseAndDocItOptions.LoadSettings;

Var
  sl :  TStringList;
  i : TDocOption;
  j : Integer;
  iValue : Integer;
  T: TBADITokenType;
  strLine : String;

begin
  With TMemIniFile.Create(FINIFileName) Do
    Try
      For i := Low(TDocOption) to High(TDocOption) Do
        If ReadBool('Options', DocOptionInfo[i].FDescription,
          DocOptionInfo[i].FEnabled) Then
          FOptions := FOptions + [i]
        Else
          FOptions := FOptions - [i];
      sl := TStringList.Create;
      Try
        ReadSection('SpecialTagNames', sl);
        If sl.Count > 0 Then
          FSpecialTags.Clear;
        For j := 0 To sl.Count - 1 Do
          FSpecialTags.AddObject(Format('%s=%s', [sl[j],
            ReadString('SpecialTagNames', sl[j], '')]),
            TObject(ReadInteger('SpecialTags', sl[j], 0)));
        ReadSection('ManagedExpandedNodes', sl);
        For j := 0 To sl.Count - 1 Do
          Begin
            iValue := ReadInteger('ManagedExpandedNodes', sl[j], 0);
            FExpandedNodes.AddObject(StringReplace(sl[j], '|', '=', [rfReplaceAll]),
              TObject(iValue));
          End;
      Finally
        sl.Free;
      End;
      FUpdateInterval := ReadInteger('ModuleExplorer', 'UpdateInterval', 1000);
      FScopesToRender := TScopes(Byte(ReadInteger('ModuleExplorer', 'ScopesToRender',
        Byte(FScopesToRender))));
      FBrowsePosition := TBrowsePosition(ReadInteger('Setup', 'BrowsePosition',
        Integer(bpIdentifierCentreShowAllComment)));
      FFontName := ReadString('ModuleExplorer', 'Name', 'MS Sans Serif');
      FFontSize := ReadInteger('ModuleExplorer', 'Size', 8);
      For T := Low(TBADITokenType) To High(TBADITokenType) Do
        Begin
          FTokenFontInfo[T].FForeColour := StringToColor(ReadString('TokenFontinfo',
            Format('%s.Colour', [strTokenType[T]]), ColorToString(strTokenTypeInfo[T].FForeColour)));
          FTokenFontInfo[T].FStyles := TFontStyles(Byte(ReadInteger('TokenFontinfo',
            Format('%s.Styles', [strTokenType[T]]), Byte(strTokenTypeInfo[T].FStyles))));
          FTokenFontInfo[T].FBackColour := StringToColor(ReadString('TokenFontinfo',
            Format('%s.BackColour', [strTokenType[T]]), ColorToString(strTokenTypeInfo[T].FBackColour)));
        End;
      FExcludeDocFiles.Text := StringReplace(ReadString('Setup', 'ExcludeDocFiles',
        ''), '|', #13#10, [rfReplaceAll]);
      sl := TStringList.Create;
      Try
        ReadSection('MethodDescriptions', sl);
        For j := 0 To sl.Count - 1 Do
          FMethodDescriptions.Add(Format('%s=%s', [sl[j],
            ReadString('MethodDescriptions', sl[j], '')]));
      Finally
        sl.Free;
      End;
      FScopesToDocument := TScopes(Byte(ReadInteger('Documentation', 'Scopes',
        Byte(FScopesToDocument))));
      FModuleExplorerBGColour := StringToColor(ReadString('ModuleExplorer',
        'BGColour', ColorToString(clWindow)));
      FTokenLimit := ReadInteger('ModuleExplorer', 'TokenLimit', 50);
      FMaxDocOutputWidth := ReadInteger('Documentation', 'MaxDocOutputWidth', 80);
      FManagedNodesLife := ReadInteger('ModuleExplorer', 'ManagedNodesLife', 90);
      FTreeColour := StringToColor(ReadString('ModuleExplorer', 'TreeColour', 'clGray'));
      sl := TStringList.Create;
      Try
        ReadSection('ProfilingCode', sl);
        For j := 0 To sl.Count - 1 Do
          Begin
            strLine := ReadString('ProfilingCode', sl[j], '');
            If strLine <> '' Then
              FProfilingCode.Values[sl[j]] := strLine;
          End;
      Finally
        sl.Free;
      End;
    Finally
      Free;
    End;
end;

(**

  This method saves the applications settings to an ini file.

  @precon  None.
  @postcon Saves the applications settings to an ini file.

**)
procedure TBrowseAndDocItOptions.SaveSettings;

Var
  i : TDocOption;
  j : Integer;
  T: TBADITokenType;

begin
  With TMemIniFile.Create(FINIFileName) Do
    Try
      For i := Low(TDocOption) to High(TDocOption) Do
        WriteBool('Options', DocOptionInfo[i].FDescription, i In FOptions);
      EraseSection('SpecialTags');
      EraseSection('SpecialTagNames');
      For j := 0 To FSpecialTags.Count - 1 Do
        Begin
          WriteInteger('SpecialTags', FSpecialTags.Names[j],
            Integer(FSpecialTags.Objects[j]));
          WriteString('SpecialTagNames', FSpecialTags.Names[j],
            FSpecialTags.Values[FSpecialTags.Names[j]]);
        End;
      EraseSection('ManagedExpandedNodes');
      For j := 0 To BrowseAndDocItOptions.ExpandedNodes.Count - 1 Do
        WriteInteger('ManagedExpandedNodes',
          StringReplace(FExpandedNodes[j], '=', '|', [rfReplaceAll]),
          Integer(FExpandedNodes.Objects[j]));
      WriteInteger('ModuleExplorer', 'UpdateInterval', FUpdateInterval);
      WriteInteger('ModuleExplorer', 'ScopesToRender', Byte(FScopesToRender));
      WriteInteger('Setup', 'BrowsePosition', Integer(FBrowsePosition));
      WriteString('ModuleExplorer', 'Name', FFontName);
      WriteInteger('ModuleExplorer', 'Size', FFontSize);
      For T := Low(TBADITokenType) To High(TBADITokenType) Do
        Begin
          WriteString('TokenFontinfo', Format('%s.Colour', [strTokenType[T]]),
            ColorToString(FTokenFontInfo[T].FForeColour));
          WriteInteger('TokenFontinfo', Format('%s.Styles', [strTokenType[T]]),
            Byte(FTokenFontInfo[T].FStyles));
          WriteString('TokenFontinfo', Format('%s.BackColour', [strTokenType[T]]),
            ColorToString(FTokenFontInfo[T].FBackColour));
        End;
      WriteString('Setup', 'ExcludeDocFiles', StringReplace(FExcludeDocFiles.Text,
        #13#10, '|', [rfReplaceAll]));
      EraseSection('MethodDescriptions');
      For j := 0 To FMethodDescriptions.Count - 1 Do
        {$IFDEF D0006}
        WriteString('MethodDescriptions', FMethodDescriptions.Names[j],
          FMethodDescriptions.ValueFromIndex[j]);
        {$ELSE}
        WriteString('MethodDescriptions', FMethodDescriptions.Names[j],
          FMethodDescriptions.Values[FMethodDescriptions.Names[j]]);
        {$ENDIF}
      WriteInteger('Documentation', 'Scopes', Byte(FScopesToDocument));
      WriteString('ModuleExplorer', 'BGColour',
        ColorToString(FModuleExplorerBGColour));
      WriteInteger('ModuleExplorer', 'TokenLimit', FTokenLimit);
      WriteInteger('Documentation', 'MaxDocOutputWidth', FMaxDocOutputWidth);
      WriteInteger('ModuleExplorer', 'ManagedNodesLife', FManagedNodesLife);
      WriteString('ModuleExplorer', 'TreeColour', ColorToString(FTreeColour));
      EraseSection('ProfilingCode');
      For j := 0 To FProfilingCode.Count - 1 Do
        If FProfilingCode.Names[j] <> '' Then
          {$IFDEF D0006}
          WriteString('ProfilingCode', FProfilingCode.Names[j],
            FProfilingCode.ValueFromIndex[j]);
          {$ELSE}
          WriteString('ProfilingCode', FProfilingCode.Names[j],
            FProfilingCode.Values[FProfilingCode.Names[j]]);
          {$ENDIF}
      UpdateFile;
    Finally
      Free;
    End;
end;

(**

  This is a setter method for the ProfilingCode property.

  @precon  None.
  @postcon saves the profiling code for the given filename.

  @param   Module   as a TBaseLanguageModule
  @param   strValue as a String as a constant

**)
procedure TBrowseAndDocItOptions.SetProfilingCode(Module : TBaseLanguageModule;
  const strValue: String);

Var
  strExt : String;

begin
  strExt := ExtractFileExt(Module.FileName);
  FProfilingCode.Values[strExt] := StringReplace(strValue, #13#10, '|', [rfReplaceAll]);
end;

(**


  This is a setter method for the TokenFontInfo property.

  @precon  None.
  @postcon Sets the indexed Token Font Information record.


  @param   ATokenType     as a TBADITokenType
  @param   ATokenFontInfo as a TTokenFontInfo

**)
procedure TBrowseAndDocItOptions.SetTokenFontInfo(ATokenType: TBADITokenType;
  ATokenFontInfo: TTokenFontInfo);
begin
  FTokenFontInfo[ATokenType] := ATokenFontInfo;
end;

{ TLabelContainer }

(**


  This is an overridden constructor to ensure that label are not displayed in
  the unreferenced scope hints.

  @precon  None.
  @postcon Overridden constructor to ensure that label are not displayed in
           the unreferenced scope hints.


  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TLabelContainer.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  Referenced := True;
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the name of the label as a string .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TLabelContainer.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;
begin
  Result := Name;
end;

{ TTickOption }

(**

  A constructor for the TTickOption class.

  @precon  None.
  @postcon Initialises the tick counter.

  @param   strName as a String
  @param   iCount  as an Int64

**)
Constructor TTickOption.Create(strName: String; iCount: Int64);

Begin
  FName := strName;
  FCount := iCount;
End;

{ TModuleInfo }

(**

  A constructor for the TModuleInfo class.

  @precon  None.
  @postcon Initialises the class with information.

  @param   strExt     as a String
  @param   Cls        as a TBaseLanguageModuleClass
  @param   boolCanDoc as a Boolean
  @param   iBlockCmt  as a TCommentType
  @param   iLineCmt   as a TCommentType
  @param   iInSituCmt as a TCommentType

**)
Constructor TModuleInfo.Create(strExt: String; Cls: TBaseLanguageModuleClass;
  boolCanDoc: Boolean; iBlockCmt, iLineCmt, iInSituCmt: TCommentType);

Begin
  FExt := strExt;
  FCls := Cls;
  FCanDoc := boolCanDoc;
  FBlockCmt := iBlockCmt;
  FLineCmt := iLineCmt;
  FInSituCmt := iInSituCmt;
End;

(**

  This is a Sort procedure for a Object List.

  @precon  None.
  @postcon Orders the list by extension.

  @param   Item1 as a Pointer
  @param   Item2 as a Pointer
  @return  an Integer

**)
Function SortModuleInfo(Item1, Item2 : Pointer) : Integer;

Begin
  Result := CompareText(TModuleInfo(Item1).Ext, TModuleInfo(Item2).Ext);
End;

(**

  This method adds a set of registration information into the dispatcher.

  @precon  None.
  @postcon Adds a set of registration information into the dispatcher.

  @param   strExt     as a String
  @param   Cls        as a TBaseLanguageModuleClass
  @param   boolCanDoc as a Boolean
  @param   iBlockCmt  as a TCommentType
  @param   iLineCmt   as a TCommentType
  @param   iInSituCmt as a TCommentType

**)
Procedure TModuleDispatcher.Add(strExt: String; Cls: TBaseLanguageModuleClass;
  boolCanDoc: Boolean; iBlockCmt, iLineCmt, iInSituCmt: TCommentType);

Begin
  FModules.Add(TModuleInfo.Create(strExt, Cls, boolCanDoc, iBlockCmt, iLineCmt,
    iInSituCmt));
  FModules.Sort(SortModuleInfo);
End;

(**

  This method determines if the document can be documented in HTML, RTF, etc,
  i.e. your wouldn`t document a code type that you only wish to browse, say
  XML or HTML.

  @precon  None.
  @postcon Determines if the document can be documented in HTML, RTF, etc.

  @param   strFileName as a String
  @return  a Boolean

**)
Function TModuleDispatcher.CanDocumentDocument(strFileName : String) : Boolean;

Var
  iIndex: Integer;

Begin
  Result := False;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex  > -1 Then
    Result := (FModules[iIndex] As TModuleInfo).CanDoc;
End;

(**


  This method determines if the file can be documented by the system.

  @precon  None.
  @postcon Determines if the file can be documented by the system.


  @param   strFileName as a String
  @return  a Boolean

**)
Function TModuleDispatcher.CanParseDocument(strFileName : String) : Boolean;

Begin
  Result := Find(ExtractFileExt(strFileName)) > -1;
End;

(**

  A constructor for the TModuleDispatcher class.

  @precon  None.
  @postcon Creates a object list to contain the module registration information.

**)
Constructor TModuleDispatcher.Create;

Begin
  FModules := TObjectList.Create(True);
End;

(**

  This function returns the index of the parser information corresponding to the
  passed file extension. If there is no match 0 is returned.

  @precon  None.
  @postcon Returns the index of the parser information corresponding to the
           passed file extension. If there is no match 0 is returned.

  @param   strExt as a String
  @return  an Integer

**)
Function TModuleDispatcher.Find(strExt : String) : Integer;

Var
  iFirst, iMid, iLast : Integer;
  i: Integer;

Begin
  Result := -1;
  iFirst := 0;
  iLast := FModules.Count - 1;
  While iFirst <= iLast Do
    Begin
      iMid := (iFirst + iLast) Div 2;
      i := CompareText((FModules[iMid] As TModuleInfo).Ext, strExt);
      If i = 0 Then
        Begin
          Result := iMid;
          Exit;
        End
      Else If i < 0 Then
        iFirst := iMid + 1
      Else
        iLast := iMid - 1;
    End;
End;

(**

  A destructor for the TModuleDispatcher class.

  @precon  None.
  @postcon Frees the memory used by the module registrations.

**)
Destructor TModuleDispatcher.Destroy;

Begin
  FModules.Free;
  Inherited Destroy;
End;

(**

  This function returns an instance of a TBaseLanguageModule assigned a specific
  language parser depending on the extension of the file passed.

  @precon  Source must be a valid TStream of charcters to parse.
  @postcon Returns an instance of a TBaseLanguageModule assigned a specific
           language parser depending on the extension of the file passed.

  @param   Source        as a String
  @param   strFileName   as a String
  @param   boolModified  as a Boolean
  @param   ModuleOptions as a TModuleOptions
  @return  a TBaseLanguageModule

**)
Function TModuleDispatcher.Dispatcher(Source : String; strFileName : String;
  boolModified : Boolean; ModuleOptions : TModuleOptions) : TBaseLanguageModule;

Var
  iIndex: Integer;

Begin
  Result := Nil;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Result := (FModules[iIndex] As TModuleInfo).Cls.CreateParser(Source, strFileName,
      boolModified, ModuleOptions);
End;

(**

  This method returns the type of comment required for the file name given and
  for the comment style given.

  @precon  None.
  @postcon Returns the type of comment required for the file name given and
           for the comment style given.

  @param   strFileName  as a String
  @param   CommentStyle as a TCommentStyle
  @return  a TCommentType

**)
Function TModuleDispatcher.GetCommentType(strFileName : String; CommentStyle : TCommentStyle) : TCommentType;

Var
  iIndex : Integer;

Begin
  Result := ctNone;
  iIndex := Find(ExtractFileExt(strFileName));
  If iIndex > -1 Then
    Case CommentStyle Of
      csBlock:  Result := (FModules[iIndex] As TModuleInfo).BlockCmt;
      csLine:   Result := (FModules[iIndex] As TModuleInfo).LineCmt;
      csInSitu: Result := (FModules[iIndex] As TModuleInfo).InSituCmt;
    End;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of registrations in the dispatcher.

  @return  an Integer

**)
Function TModuleDispatcher.GetCount: Integer;

Begin
  Result := FModules.Count;
End;

(**

  This is a getter method for the Modules property.

  @precon  iIndex must be between 0 and Count - 1.
  @postcon Returns a reference to the indexed module registration.

  @param   iIndex as an Integer
  @return  a TModuleInfo

**)
Function TModuleDispatcher.GetModules(iIndex: Integer): TModuleInfo;

Begin
  Result := FModules[iIndex] As TModuleInfo;
End;

(** This initializations section ensures that there is a valid instance of the
    BrowseAndDocItOption class. **)
Initialization
  iDocConflictCounter := 1;
  BrowseAndDocItOptions := TBrowseAndDocItOptions.Create;
  ModuleDispatcher := TModuleDispatcher.Create;
(** This finalization section ensures that the BrowseAndDocItOptions class are
    destroyed. **)
Finalization
  ModuleDispatcher.Free;
  BrowseAndDocItOptions.Free;
End.
