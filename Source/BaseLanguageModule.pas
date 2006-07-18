(**

  This module contains the base class for all language module to derived from
  and all standard constants across which all language modules have in common.

  @Date    17 Jul 2006
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
  TParamModifier = (pamNone, pamVar, pamConst, pamOut);
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
           and SaveSettings methods. Therefore perhaps the calling class which
           uses the frameModuleExplorer should call the Loadsettings() and
           SaveSettings() methods and not depend on the frame class to call
           these in the constructor and destructor respectively.
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
    doShowMissingInitComment,
    doShowMissingFinalComment
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
      @precon  None.
      @postcon Returns the identifiers name as a string.
      @return  a String
    **)
    Property Ident : String Read FIdent;
    (**
      Returns the line number of the idenifier.
      @precon  None.
      @postcon Returns the line number of the idenifier.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the identifier.
      @precon  None.
      @postcon Returns the column number of the identifier.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns the comment associated with the identifier.
      @precon  None.
      @postcon Returns the comment associated with the identifier.
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
      Virtual;
    Procedure Assign(src : TIdentList);
    Procedure Sort;
    Function Find(strIdent : String) : Integer;
    Function AsString : String; Virtual;
    (**
      Returns the specifically indexed identifier in the list.
      @precon  iIndex must be a valid index between 0 and Count - 1.
      @postcon Returns the specifically indexed identifier in the list.
      @param   iIndex as       an Integer
      @return  a TIdent
    **)
    Property Idents[iIndex : Integer] : TIdent Read GetIdentInfo; Default;
    (**
      Returns the number of identifiers in the list.
      @precon  None.
      @postcon Returns the number of identifiers in the list.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      Returns the comment associated with the identifier list.
      @precon  None.
      @postcon Returns the comment associated with the identifier list.
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
      @precon  None.
      @postcon Returns the identifier for the generic container.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier Write FIdentifier;
    (**
      Returns the specifically indexed token in the container.
      @precon  iIndex must be a valid index between 0 and Count - 1.
      @postcon Returns the specifically indexed token in the container.
      @param   iIndex as       an Integer
      @return  a String
    **)
    Property Token[iIndex : Integer] : String Read GetToken; Default;
    (**
      Returns the number of tokens in the generic container.
      @precon  None.
      @postcon Returns the number of tokens in the generic container.
      @return  an Integer
    **)
    Property Count : Integer Read GetTokenCount;
    (**
      Returns the comment associated with the generic container.
      @precon  None.
      @postcon Returns the comment associated with the generic container.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the scope of the generic container.
      @precon  None.
      @postcon Returns the scope of the generic container.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
    (**
      Returns the line number for the generic container.
      @precon  None.
      @postcon Returns the line number for the generic container.
      @return  an Integer
    **)
    Property Line : Integer Read FLine Write FLine;
    (**
      Returns the column number of the generic container.
      @precon  None.
      @postcon Returns the column number of the generic container.
      @return  an Integer
    **)
    Property Col : Integer Read FCol Write FCol;
  End;

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
      @precon  None.
      @postcon Returns the comment associcate with this collection.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the specifically indexed item from the generic collection.
      @precon  iIndex must be a valid index between 0 and Count - 1.
      @postcon Returns the specifically indexed item from the generic collection.
      @param   iIndex as       an Integer
      @return  a TGenericContainer
    **)
    Property Items[iIndex : Integer] : TGenericContainer Read GetItem; Default;
    (**
      Returns the number of items in the generic collection.
      @precon  None.
      @postcon Returns the number of items in the generic collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** This is a sub class for all types **)
  TTypeDecl = Class(TGenericContainer);

  (** This is a sub class for general type types **)
  TTypes = Class(TTypeDecl);
  (** This is a sub class for restricted type types **)
  TRestrictedType = Class(TTypeDecl);

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
      boolArrayOf : Boolean; AType : TTypeDecl; Value : String;
      Scope : TScope; iLine, iCol : Integer); Overload;
    Procedure Assign(Parameter : TParameter);
    Destructor Destroy; Override;
    (**
      Returns the parameter modifier : const, var or out.
      @precon  None.
      @postcon Returns the parameter modifier : const, var or out.
      @return  a TParamModifier
    **)
    Property ParamModifier : TParamModifier Read FParamModifier;
    (**
      Returns the parameters idenitfier.
      @precon  None.
      @postcon Returns the parameters idenitfier.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier;
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
      @return  a TTypes
    **)
    Property ParamType : TTypes Read FParamType;
    (**
      Returns the default value of the parameter is there is one.
      @precon  None.
      @postcon Returns the default value of the parameter is there is one.
      @return  a String
    **)
    Property DefaultValue : String Read FDefaultValue;
    (**
      Returns the comment associcated with the parameter (field).
      @precon  None.
      @postcon Returns the comment associcated with the parameter (field).
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the parameters scope with in the record / object / class etc.
      @precon  None.
      @postcon Returns the parameters scope with in the record / object / class etc.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope;
    (**
      Returns the parameters line number.
      @precon  None.
      @postcon Returns the parameters line number.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the parameters column number.
      @precon  None.
      @postcon Returns the parameters column number.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
  End;

  (** A class to define a collection of parameters. **)
  TParameterCollection = Class
  Private
    FParameters : TObjectList;
    Function GetParameter(iIndex : Integer) : TParameter;
    Function GetCount : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Parameter : TParameter);
    Procedure Sort;
    (**
      Returns the specifically indexed parameter of the method.
      @precon  iIndex must be a valid index between 0 and Count - 1.
      @postcon Returns the specifically indexed parameter of the method.
      @param   iIndex as       an Integer
      @return  a TParameter
    **)
    Property Parameter[iIndex : Integer] : TParameter Read GetParameter; Default;
    (**
      Returns the number of parameter the method has.
      @precon  None.
      @postcon Returns the number of parameter the method has.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
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
    FParameters : TParameterCollection;
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
    FLabels : TIdentList;
    Procedure SetClsName(Value : String);
    Procedure SetIdentifier(Value : String);
    Procedure SetReturnType(Value : String);
    procedure SetMsg(const Value: String);
    procedure SetExt(const Value: String);
    Function GetQualifiedName : String;
  Public
    Constructor Create(MethodType : TMethodType; Scope : TScope;
      iLine, iCol : Integer);
    Destructor Destroy; Override;
    Procedure AddDirectives(strDirective : String);
    Function GetAsString(ShowClassName, ShowMethodType : Boolean): String;
    Function HasDirective(strDirective : String) : Boolean;
    Procedure Assign(Method : TMethodDecl);
    (**
      Returns the comment associated with the method.
      @precon  None.
      @postcon Returns the comment associated with the method.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
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
      Returns the methods idenitifier.
      @precon  None.
      @postcon Returns the methods idenitifier.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier Write SetIdentifier;
    (**
      This property defines a collection of parameters associated with the
      method.
      @precon  None.
      @postcon References a collection of parameters associated with the method.
      @return  a TParameterCollection
    **)
    Property Parameters : TParameterCollection Read FParameters;
    (**
      Returns the return type of the method if it is a function.
      @precon  None.
      @postcon Returns the return type of the method if it is a function.
      @return  a String
    **)
    Property ReturnType : String Read FReturnType Write SetReturnType;
    (**
      Returns the string list of directives associated with the method.
      @precon  None.
      @postcon Returns the string list of directives associated with the method.
      @return  a TStringList
    **)
    Property Directives : TStringList Read FDirectives;
    (**
      Returns the internal scope of the method within the class / interface /
      @precon  None.
      @postcon Returns the internal scope of the method within the class / interface /
      module etc.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
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
      Returns the line number of the method.
      @precon  None.
      @postcon Returns the line number of the method.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the method.
      @precon  None.
      @postcon Returns the column number of the method.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns a reference to the local methods collection of this method.
      @precon  None.
      @postcon Returns a reference to the local methods collection of this method.
      @return  a TMethodCollection
    **)
    Property LocalMethods : TMethodCollection Read FLocalMethods;
    (**
      Returns a reference to the types collection of the method.
      @precon  None.
      @postcon Returns a reference to the types collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property Types : TGenericContainerCollection Read FTypes;
    (**
      Returns a reference to the variables collection of the method.
      @precon  None.
      @postcon Returns a reference to the variables collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property Vars : TGenericContainerCollection Read FVars;
    (**
      Returns a reference to the constants collection of the method.
      @precon  None.
      @postcon Returns a reference to the constants collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property Consts : TGenericContainerCollection Read FConsts;
    (**
      Returns a reference to the resource string collection of the method.
      @precon  None.
      @postcon Returns a reference to the resource string collection of the method.
      @return  a TGenericContainerCollection
    **)
    Property ResStrings : TGenericContainerCollection Read FResStrings;
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
      This property defines a list of label identifiers associated with the
      method.
      @precon  None.
      @postcon References an IdentList of labels identifiers associated with the
               method.
      @return  a TIdentList
    **)
    Property Labels : TIdentList Read FLabels;
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
    Function Find(strClsName, strMethodName : String) : Integer;
    Procedure Sort;
    (**
      Returns the specifically indexed method from the methods collection.
      @precon  iIndex must be a valid index between 0 and Count - 1.
      @postcon Returns the specifically indexed method from the methods collection.
      @param   iIndex as       an Integer
      @return  a TMethodDecl
    **)
    Property Method[iIndex : Integer] : TMethodDecl Read GetMethod; Default;
    (**
      Returns the number of methods in the methods collection.
      @precon  None.
      @postcon Returns the number of methods in the methods collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** This is a class that represents properties of a class or interface. **)
  TProperty = Class
  Private
    FParameters : TParameterCollection;
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
    procedure SetDefaultSpec(const Value: String);
    procedure SetImplementsSpec(const Value: String);
    procedure SetIndexSpec(const Value: String);
    procedure SetReadSpec(const Value: String);
    procedure SetStoredSpec(const Value: String);
    procedure SetTypeId(const Value: String);
    procedure SetWriteSpec(const Value: String);
    procedure SetDefaultProperty(const Value: Boolean);
  Public
    Constructor Create(strIdent : String; Scope : TScope; iLine, iCol : Integer);
    Destructor Destroy; Override;
    Procedure Assign(Prop : TProperty);
    (**
      Returns the identifier of the property.
      @precon  None.
      @postcon Returns the identifier of the property.
      @return  a String
    **)
    Property Identifier : String Read FIdentifier;
    (**
      Returns the specifically indexed parameter of the property.
      @precon  None.
      @postcon Returns the specifically indexed parameter of the property.
      @return  a TParameterCollection
    **)
    Property Parameters : TParameterCollection Read FParameters;
    (**
      Returns the type identifier of the property.
      @precon  None.
      @postcon Returns the type identifier of the property.
      @return  a String
    **)
    Property TypeId : String Read FTypeId Write SetTypeId;
    (**
      Returns the
      @precon  None.
      @postcon Returns the
      @return  a String
    **)
    Property IndexSpec : String Read FIndexSpec Write SetIndexSpec;
    (**
      Returns the properties Read specification.
      @precon  None.
      @postcon Returns the properties Read specification.
      @return  a String
    **)
    Property ReadSpec : String Read FReadSpec Write SetReadSpec;
    (**
      Returns the properties write specification.
      @precon  None.
      @postcon Returns the properties write specification.
      @return  a String
    **)
    Property WriteSpec : String Read FWriteSpec Write SetWriteSpec;
    (**
      Returns the properties Stored specification.
      @precon  None.
      @postcon Returns the properties Stored specification.
      @return  a String
    **)
    Property StoredSpec : String Read FStoredSpec Write SetStoredSpec;
    (**
      Returns the property default value.
      @precon  None.
      @postcon Returns the property default value.
      @return  a String
    **)
    Property DefaultSpec : String Read FDefaultSpec Write SetDefaultSpec;
    (**
      Returns whether this property is the classes / interfaces default
      @precon  None.
      @postcon Returns whether this property is the classes / interfaces default
      property.
      @return  a Boolean
    **)
    Property DefaultProperty : Boolean Read FDefaultProperty Write SetDefaultProperty;
    (**
      Returns the implements specification for the property.
      @precon  None.
      @postcon Returns the implements specification for the property.
      @return  a String
    **)
    Property ImplementsSpec : String Read FImplementsSpec Write SetImplementsSpec;
    (**
      Returns a string represnetation of the property with all the appropriate
      @precon  None.
      @postcon Returns a string represnetation of the property with all the appropriate
      specifiers.
      @return  a String
    **)
    Property AsString : String Read GetAsString;
    (**
      Returns the comment associated with the property.
      @precon  None.
      @postcon Returns the comment associated with the property.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      Returns the scope with in the class / interface of the property.
      @precon  None.
      @postcon Returns the scope with in the class / interface of the property.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope;
    (**
      Returns the line number of the property.
      @precon  None.
      @postcon Returns the line number of the property.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the property.
      @precon  None.
      @postcon Returns the column number of the property.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
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

  (** A class to define a collection of properties. **)
  TPropertyCollection = Class
  Private
    FProperties : TObjectList;
    Function GetProperty(iIndex : Integer) : TProperty;
    Function GetCount : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Prop : TProperty);
    Procedure Sort;
    (**
      Returns the indexed property for the class.
      @precon  iIndex must be a valid index between 0 and Count - 1.
      @postcon Returns the indexed property for the class.
      @param   iIndex as       an Integer
      @return  a TProperty
    **)
    Property Properties[iIndex : Integer] : TProperty Read GetProperty; Default;
    (**
      Returns the number of properties in the classes properties collection.
      @precon  None.
      @postcon Returns the number of properties in the classes properties collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** This is a class that represents a record definition. **)
  TRecordDecl = Class(TRestrictedType)
  Private
    FPacked : Boolean;
    FParameters : TParameterCollection;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure AddParameter(Parameter : TParameter);
    Procedure Sort; Override;
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    (**
      Returns the specifically indexed parameter in the record.
      @precon  None.
      @postcon Returns the specifically indexed parameter in the record.
      @return  a TParameterCollection
    **)
    Property Parameters : TParameterCollection Read FParameters;
    (**
      Returns whether the record is packed or not.
      @precon  None.
      @postcon Returns whether the record is packed or not.
      @return  a Boolean
    **)
    Property IsPacked : Boolean Read FPacked Write FPacked;
  End;

  (** This is a class the extends the record definition to handle an object
  definition **)
  TObjectDecl = Class(TRecordDecl)
  Private
    FMethods : TMethodCollection;
    FHeritage : TIdentList;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    Procedure Assign(AObject : TObjectDecl); Virtual;
    Procedure Sort; Override;
    (**
      Returns a reference to the object class heritage.
      @precon  None.
      @postcon Returns a reference to the object class heritage.
      @return  a TIdentList
    **)
    Property Heritage : TIdentList Read FHeritage;
    (**
      This property defines a collection of methods associated with the object.
      @precon  None.
      @postcon References a collection of methods associated with the object.
      @return  a TMethodCollection
    **)
    Property Methods : TMethodCollection Read FMethods;
  End;

  (** This is a class the extends the object definition to handle an class
  definition **)
  TClassDecl = Class(TObjectDecl)
  Private
    FProperties : TPropertyCollection;
    FAbstractClass: Boolean;
    FSealedClass : Boolean;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function AsString(ShowFirstToken : Boolean) : String; Override;
    Procedure Assign(AObject : TObjectDecl); Override;
    Procedure Sort; Override;
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
      This property returns a collection of properties associated with the
      class.
      @precon  None.
      @postcon Returns a collection of properties associated with the class.
      @return  a TPropertyCollection
    **)
    Property Properties : TPropertyCollection Read FProperties;
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
      @precon  None.
      @postcon Returns the GUID for the interface.
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
  Private
    FDimensions : Integer;
  Public
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

  (** This is a sub class for all constants. **)
  TConstant = Class(TGenericContainer);
  (** This is a sub class for all resource strings. **)
  TResourceString = Class(TConstant);
  (** This is a sub class for all variables. **)
  TVar = Class(TGenericContainer);
  (** This is a sub class for all thread variables. **)
  TThreadVar = Class(TVar);

  (** This class defines a parsing error. **)
  TDocError = Class
  Private
    FLine: Integer;
    FCol: Integer;
    FMsg: String;
    FErrorType : TErrorType;
    FMethod : String;
  Public
    Constructor Create(strMsg, strMethod : String; iLine, iCol : Integer;
      ErrType : TErrorType); Overload;
    (**
      Returns the exception message.
      @precon  None.
      @postcon Returns the exception message.
      @return  a String
    **)
    Property Msg : String Read FMsg;
    (**
      Returns the line number where the exception occurred.
      @precon  None.
      @postcon Returns the line number where the exception occurred.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number where the exception occurred.
      @precon  None.
      @postcon Returns the column number where the exception occurred.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
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

  (** This class provides a collection for the parsing errors. **)
  TDocErrorCollection = Class
  Private
    FErrors : TObjectList;
    function GetCount: Integer;
    function GetError(iIndex: Integer): TDocError;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(strMsg, strMethod : String; iLine, iCol : Integer;
      ErrType : TErrorType);
    (**
      Returns the number of errors in the collection.
      @precon  None.
      @postcon Returns the number of errors in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      Returns the specifically indexed error from the collection.
      @precon  iIndex must be a valid index between 1 and Count - 1.
      @postcon Returns the specifically indexed error from the collection.
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
      @precon  None.
      @postcon Returns the line number of the documentation exception.
      @return  an Integer
    **)
    Property Line : Integer Read FLine;
    (**
      Returns the column number of the documentation exception.
      @precon  None.
      @postcon Returns the column number of the documentation exception.
      @return  an Integer
    **)
    Property Col : Integer Read FCol;
    (**
      Returns a string representing the classes ExceptionMethod.
      @precon  None.
      @postcon Returns a string representing the classes ExceptionMethod.
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
    dctPropertyTooManyPostcons,
    dctMissingInitComment,
    dctMissingFinalComment
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
    (**
      This property defines the category under which the conflict should appear.
      @precon  None.
      @postcon Returns the category under which the conflict should appear.
      @return  a String
    **)
    Property Category : String Read GetCategory;
    (**
      This property defines the actual documentation conflict message.
      @precon  None.
      @postcon Return the actual documentation conflict message.
      @return  a String
    **)
    Property Message : String Read FMessage;
    (**
      This property defines the description associated with the
      documentation conflict.
      @precon  None.
      @postcon Returns the description associated with the documentation
               conflict.
      @return  a String
    **)
    Property Description : String Read GetDescription;
    (**
      This property defines the identifier line associated with the documenation
      conflict.
      @precon  None.
      @postcon Returns the identifier line associated with the documenation
               conflict.
      @return  an Integer
    **)
    Property IdentLine : Integer Read FIdentLine;
    (**
      This property defines the identifier column associated with the
      documenation conflict.
      @precon  None.
      @postcon Returns the identifier column associated with the documenation
               conflict.
      @return  an Integer
    **)
    Property IdentColumn : Integer Read FIdentColumn;
    (**
      This property defines the line where the comment associated with the
      conflict starts.
      @precon  None.
      @postcon Returns the line where the comment associated with the conflict
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
    (**
      This property defines the type of documentation conflict.
      @precon  None.
      @postcon Returns the type of documentation conflict.
      @return  a TDocConflictType
    **)
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

  (** A type to define the position before a token of the comment to be
      associated with the identifier. **)
  TCommentPosition = (cpBeforeCurrentToken, cpBeforePreviousToken);

  (** This is a type for a set of characters and the return type of several
      properties. **)
  TCharSet = Set of Char;

//  (** This is a type to define an array of string. **)
//  TArrayOfString = Array Of String;

  (** This is an abtract class from which all language modules should be
      derived. **)
  TBaseLanguageModule = Class {$IFDEF VER180} Abstract {$ENDIF}
  Private
    FOwnedItems : TObjectList;
    FTokens : TObjectList;
    FTokenIndex : TTokenIndex;
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
    FInitSection : TIdent;
    FFinalSection : TIdent;
    FResStrCollection: TGenericContainerCollection;
    FExportsCollection : TGenericContainerCollection;
    FBodyComment : TObjectList;
    FModuleNameCol: Integer;
    FModuleNameLine: Integer;
    FSymbolTable: TGenericContainerCollection;
    FFileName: String;
    FModified : Boolean;
    FDocumentConflicts: TObjectList;
    FCompilerDefs : TStringList;
    FPreviousTokenIndex : TTokenIndex;
    FCompilerConditionStack : TList;
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
  Protected
    Function PrevToken : TTokenInfo;
    Procedure NextToken;
    Function EndOfTokens : Boolean;
    Procedure NextNonCommentToken;
    Procedure RollBackToken;
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
    Procedure SetTokenIndex(iIndex : TTokenIndex);
    Procedure SortDocumentConflicts;
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
    Constructor Create(IsModified : Boolean; strFileName : String);
    Destructor Destroy; Override;
    Procedure AddTickCount(strLabel : String);
    Procedure AddDocumentConflict(Const Args: Array of TVarRec; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol  : Integer;
      DocConflictType : TDocConflictType);
    Function ConvertDate(Const strDate : String) : TDateTime;
    Procedure AddDef(strDef : String);
    Procedure DeleteDef(strDef : String);
    Function IfDef(strDef : String) : Boolean;
    Function IfNotDef(strDef : String) : Boolean;
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
      Returns the number of token within the module after tokenizing.
      @precon  None.
      @postcon Returns the number of token within the module after tokenizing.
      @return  an TDocErrorCollection
    **)
    Property Errors : TDocErrorCollection Read FDocErrors;
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
      Returns a reference to the implemented methods collection.
      @precon  None.
      @postcon Returns a reference to the implemented methods collection.
      @return  a TMethodCollection
    **)
    Property ImplementedMethods : TMethodCollection Read FImplementedMethods;
    (**
      Returns a reference to the constants clause collection.
      @precon  None.
      @postcon Returns a reference to the constants clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Constants : TGenericContainerCollection Read FConstantsCollection;
    (**
      Returns a reference to the variables clause collection.
      @precon  None.
      @postcon Returns a reference to the variables clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Vars : TGenericContainerCollection Read FVarsCollection;
    (**
      Returns a reference to the modules types clause collection.
      @precon  None.
      @postcon Returns a reference to the modules types clause collection.
      @return  a TGenericContainerCollection
    **)
    Property Types : TGenericContainerCollection Read FTypeCollection;
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
      Returns a reference to the modules symbol table. All symbol in the module
      are stored here and disposed of from here. Reference from other collections
      like Types are purely reference only and those collection will not manage
      the symbols life time.
      @precon  None.
      @postcon Returns a reference to the modules symbol table. All symbol in
               the module are stored here and disposed of from here. Reference
               from other collections like Types are purely reference only and
               those collection will not manage the symbols life time.
      @return  a TGenericContainerCollection
    **)
    Property SymbolTable : TGenericContainerCollection Read FSymbolTable;
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
      This property returns a reference to the classes document conflict list.
      @precon  iIndex must be a valid index between 0 and
               DocumentConflictCount - 1
      @postcon This property returns a reference to the classes document
               conflict list.
      @param   iIndex as an Integer
      @return  a TDocumentConflict
    **)
    Property DocumentConflict[iIndex : Integer] : TDocumentConflict
      Read GetDocumentConflict;
    (**
      This property returns the number of documentation conflicts in the module.
      @precon  None.
      @postcon Returns the number of documentation conflicts in the module.
      @return  an Integer
    **)
    Property DocumentConflictCount : Integer Read GetDocumentConflictCount;
    (**
      Returns a reference to the requires clause collection.
      @precon  None.
      @postcon Returns a reference to the requires clause collection.
      @return  a TIdentList
    **)
    Property Requires : TIdentList Read FRequiresClause Write FRequiresClause;
    (**
      Returns a reference to the contains clause collection.
      @precon  None.
      @postcon Returns a reference to the contains clause collection.
      @return  a TIdentList
    **)
    Property Contains : TIdentList Read FContainsClause Write FContainsClause;
    (**
      Returns a reference to the uses clause collection.
      @precon  None.
      @postcon Returns a reference to the uses clause collection.
      @return  a TIdentList
    **)
    Property UsesCls : TIdentList Read FUsesClause Write FUsesClause;
    (**
      Returns a reference to the exported headings collection.
      @precon  None.
      @postcon Returns a reference to the exported headings collection.
      @return  a TMethodCollection
    **)
    Property ExportedHeadings : TMethodCollection Read FExportedHeadings;
    (**
      Returns a reference to the resource string clause collection.
      @precon  None.
      @postcon Returns a reference to the resource string clause collection.
      @return  a TGenericContainerCollection
    **)
    Property ResourceStrings : TGenericContainerCollection Read FResStrCollection;
    (**
      Returns a reference to the ThreadVar clause collection.
      @precon  None.
      @postcon Returns a reference to the ThreadVar clause collection.
      @return  a TGenericContainerCollection
    **)
    Property ThreadVars : TGenericContainerCollection Read FThreadVarsCollection;
    (**
      Returns a reference to the modules Initialization comment.
      @precon  None.
      @postcon Returns a reference to the modules Initialization comment.
      @return  a TIdent
    **)
    Property InitializationSection : TIdent Read FInitSection Write FInitSection;
    (**
      Returns a reference to the modules Finalization comment.
      @precon  None.
      @postcon Returns a reference to the modules Finalization comment.
      @return  a TIdent
    **)
    Property FinalizationSection : TIdent Read FFinalSection Write FFinalSection;
    (**
      Returns a refernce to the modules exports collection.
      @precon  None.
      @postcon Returns a refernce to the modules exports collection.
      @return  a TGenericContainerCollection
    **)
    Property ExportsClause : TGenericContainerCollection Read FExportsCollection
      Write FExportsCollection;
  End;

ResourceString
  (** The registry key for the wizards settings. **)
  strRegRootKey = 'Software\Season''s Fall\Browse and Doc It\';

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
  (** Options text for Show local procedures and functions **)
  strShowLocalProcsAndFuncs = 'Show local procedures and functions';
  (** Options text for Show Documentation Conflicts **)
  strShowDocumentationConflicts = 'Show Documentation Conflicts';
  (** Options text for Manage Expanded Nodes **)
  strManageExpandedNodes = 'Manage Expanded Nodes';
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
    '(@@precon) and a post-condition statement (@postcon). # #Along with these ' +
    'there should be a list of the parameters and any return types.';
  (** Document conflict message for missing method description. **)
  strMethodHasNoDesc = 'Method ''%s'' has no description.';
  (** Document conflict message descritpion for missing method description. **)
  strMethodHasNoDescDesc = 'Each method declaration in the implementation ' +
    'section should have a description which should provide information to ' +
    'furture developers regarding the purpose of the method.';

  (** Label for Method Parameter Documentation Conflicts **)
  strMethodParamDocumentation = 'Method Parameter Documentation';
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

  (** Label for Method Return Documentation Conflicts **)
  strMethodReturnDocumentation = 'Method Return Documentation';
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

  (** Label for Method Pre-Condition Documentation Conflicts **)
  strMethodPreConDocumentation = 'Method Pre-Condition Documentation';
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
    'tags (@@precon).';

  (** Label for Method Post-Condition Documentation Conflicts **)
  strMethodPostConDocumentation = 'Method Post-Condition Documentation';
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
    'tags (@@postcon).';

  (** Label for Property Documentation Conflicts **)
  strPropertyDocumentation = 'Property Documentation';
  (** Document conflict message for missing method documentation. **)
  strPropertyUndocumented = 'Property ''%s'' has not been documented.';
  (** Document conflict message description for missing method documentation. **)
  strPropertyUndocumentedDesc = 'Each property declaration in the class or' +
    'interface should have a description which should provide information to ' +
    'future developers regarding the purpose of the property. # #In addition to ' +
    'the descrition each property should have a pre-condition statement ' +
    '(@@precon) and a post-condition statement (@postcon). # #Along with these ' +
    'there should be a list of the parameters and any return types.';
  (** Document conflict message for missing property description. **)
  strPropertyHasNoDesc = 'Property ''%s'' has no description.';
  (** Document conflict message description for missing property description. **)
  strPropertyHasNoDescDesc = 'Each property declaration in the class or ' +
    'interface should have a description which should provide information to ' +
    'furture developers regarding the purpose of the method.';

  (** Label for Property Parameter Documentation Conflicts **)
  strPropertyParamDocumentation = 'Property Parameter Documentation';
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

  (** Label for Property Return Documentation Conflicts **)
  strPropertyReturnDocumentation = 'Property Return Documentation';
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

  (** Label for Property Pre-Condition Documentation Conflicts **)
  strPropertyPreConDocumentation = 'Property Pre-Condition Documentation';
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
    'tags (@@precon).';

  (** Label for Property Post-Condition Documentation Conflicts **)
  strPropertyPostConDocumentation = 'Property Post-Condition Documentation';
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
    'tags (@@postcon).';

  (** Label for Finalialization Documentation Conflicts **)
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
  strUndeclaredClassMethod = 'Class method "%s.%s" has not been declared.';
  (** An exception message for an unsatisfied forward reference. **)
  strUnSatisfiedForwardReference = 'Class method "%s.%s" has an unsatisfied ' +
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
  (** A list of strings representing the types of methods. **)
  strMethodTypes : Array[mtConstructor..mtFunction] Of String = (
    'Constructor', 'Destructor', 'Procedure', 'Function');
  (** A list of strings representing the parameter modifiers for methods. **)
  strParamModifier : Array[pamNone..pamOut] Of String = ('', 'var ', 'const ',
    'out ');

  (** This is a string array representing the TDocOption enumerates. **)
  DocOptionInfo : Array[Low(TDocOption)..High(TDocOption)] Of TDocOptionRec = (
    (Description : strDrawSynHighModuleExplorer; Enabled : False),
    (Description : strShowCommentsInTheHints; Enabled : False),
    (Description : strShowLocalDeclarationsInMethods; Enabled : False),
    (Description : strShowPrivateDeclarations; Enabled : True),
    (Description : strShowProtectedDeclarations; Enabled : True),
    (Description : strShowPublicDeclarations; Enabled : True),
    (Description : strShowPublishedDeclarations; Enabled : True),
    (Description : strShowLocalProcsAndFuncs; Enabled : True),
    (Description : strShowDocumentationConflicts; Enabled : False),
    (Description : strManageExpandedNodes; Enabled : True),
    (Description : strShowMissingMethodDocumentation; Enabled : True),
    (Description : strShowMissingMethodDocDesc; Enabled : True),
    (Description : strShowDiffMethodParameterCount; Enabled : True),
    (Description : strShowUndocumentedMethodParameters; Enabled : True),
    (Description : strShowIncorrectMethodParameterType; Enabled : True),
    (Description : strShowUndocumentedMethodReturn; Enabled : True),
    (Description : strShowIncorrectMethodReturnType; Enabled : True),
    (Description : strShowUndocumentedTypes; Enabled : False),
    (Description : strShowUndocumentedRecords; Enabled : False),
    (Description : strShowUndocumentedObjects; Enabled : False),
    (Description : strShowUndocumentedClasses; Enabled : False),
    (Description : strShowUndocumentedInterfaces; Enabled : False),
    (Description : strShowUndocumentedVariables; Enabled : False),
    (Description : strShowUndocumentedConstants; Enabled : False),
    (Description : strShowUndocumentedModule; Enabled : True),
    (Description : strShowMissingModuleDate; Enabled : False),
    (Description : strShowCheckModuleDate; Enabled : False),
    (Description : strShowMissingModuleVersion; Enabled : False),
    (Description : strShowMissingModuleAuthor; Enabled : False),
    (Description : strShowMissingMethodPreConditions; Enabled : False),
    (Description : strShowMissingMethodPostConditions; Enabled : False),
    (Description : strShowMissingPropertyDocumentation; Enabled : False),
    (Description : strShowMissingPropertyDocuDesc; Enabled : False),
    (Description : strShowDiffPropertyParameterCount; Enabled : False),
    (Description : strShowUndocumentedPropertyParameter; Enabled : False),
    (Description : strShowIncorrectPropertyParameterType; Enabled : False),
    (Description : strShowUndocumentedPropertyReturnType; Enabled : False),
    (Description : strShowIncorrectPropertyReturnType; Enabled : False),
    (Description : strShowMissingPropertyPreConditions; Enabled : False),
    (Description : strShowMissingPropertyPostConditions; Enabled : False),
    (Description : strShowMissingInitComment; Enabled : False),
    (Description : strShowMissingFinalComment; Enabled : False)
  );

  (** This is a string array representing the TDocOption enumerates. **)
  DocConflictInfo : Array[Low(TDocConflictType)..High(TDocConflictType)]
    Of TDocConflictTypeRec = (
    (Category: strModuleDocumentation; MessageMask:
      strModuleMissingDocumentation; Description:
      strModuleMissingDocumentationDesc),
    (Category: strModuleDocumentation; MessageMask: strModuleMissingDate;
      Description: strModuleMissingDateDesc),
    (Category: strModuleDocumentation; MessageMask: strModuleIncorrectDate;
      Description: strModuleIncorrectDateDesc),
    (Category: strModuleDocumentation; MessageMask: strModuleCheckDateError;
      Description: strModuleCheckDateErrorDesc),
    (Category: strModuleDocumentation; MessageMask: strModuleMissingVersion;
      Description: strModuleMissingVersionDesc),
    (Category: strModuleDocumentation; MessageMask: strModuleMissingAuthor;
      Description: strModuleMissingAuthorDesc),
    (Category: strTypedocumentation; MessageMask: strTypeClauseUndocumented;
      Description: strTypeClauseUndocumentedDesc),
    (Category: strConstantdocumentation; MessageMask:
      strConstantClauseUndocumented; Description:
      strConstantClauseUndocumentedDesc),
    (Category: strResourceStringdocumentation; MessageMask:
      strResourceStringClauseUndocumented; Description:
      strResourceStringClauseUndocumentedDesc),
    (Category: strVariabledocumentation; MessageMask:
      strVariableClauseUndocumented; Description:
      strVariableClauseUndocumentedDesc),
    (Category: strThreadVardocumentation; MessageMask:
      strThreadVarClauseUndocumented; Description:
      strThreadVarClauseUndocumentedDesc),
    (Category: strRecorddocumentation; MessageMask:
      strRecordClauseUndocumented; Description: strRecordClauseUndocumentedDesc),
    (Category: strObjectdocumentation; MessageMask: strObjectClauseUndocumented;
      Description: strObjectClauseUndocumentedDesc),
    (Category: strClassdocumentation; MessageMask: strClassClauseUndocumented;
      Description: strClassClauseUndocumentedDesc),
    (Category: strInterfacedocumentation; MessageMask:
      strInterfaceClauseUndocumented; Description:
      strInterfaceClauseUndocumentedDesc),
    (Category: strDispinterfacedocumentation; MessageMask:
      strDispinterfaceClauseUndocumented; Description:
      strDispinterfaceClauseUndocumentedDesc),
    (Category: strMethodDocumentation; MessageMask: strMethodUndocumented;
      Description: strMethodUndocumentedDesc),
    (Category: strMethodDocumentation; MessageMask: strMethodHasNoDesc;
      Description: strMethodHasNoDescDesc),
    (Category: strMethodParamDocumentation; MessageMask:
      strMethodDiffParamCount; Description: strMethodDiffParamCountDesc),
    (Category: strMethodParamDocumentation; MessageMask:
      strMethodUndocumentedParam; Description: strMethodUndocumentedParamDesc),
    (Category: strMethodParamDocumentation; MessageMask:
      strMethodIncorrectParamType; Description: strMethodIncorrectParamTypeDesc),
    (Category: strMethodReturnDocumentation; MessageMask:
      strMethodUndocumentedReturn; Description: strMethodUndocumentedReturnDesc),
    (Category: strMethodReturnDocumentation; MessageMask:
      strMethodIncorrectReturnType; Description:
      strMethodIncorrectReturnTypeDesc),
    (Category: strMethodPreconDocumentation; MessageMask:
      strMethodPreconNotDocumented; Description:
      strMethodPreconNotDocumentedDesc),
    (Category: strMethodPreconDocumentation; MessageMask:
      strMethodMissingPrecon; Description: strMethodMissingPreconDesc),
    (Category: strMethodPreconDocumentation; MessageMask:
      strMethodTooManyPrecons; Description: strMethodTooManyPreconsDesc),
    (Category: strMethodPostconDocumentation; MessageMask:
      strMethodPostconNotDocumented; Description:
      strMethodPostconNotDocumentedDesc),
    (Category: strMethodPostconDocumentation; MessageMask:
      strMethodMissingPostcon; Description: strMethodMissingPostconDesc),
    (Category: strMethodPostconDocumentation; MessageMask:
      strMethodTooManyPostcons; Description: strMethodTooManyPostconsDesc),
    (Category: strPropertyDocumentation; MessageMask: strPropertyUndocumented;
      Description: strPropertyUndocumentedDesc),
    (Category: strPropertyDocumentation; MessageMask: strPropertyHasNoDesc;
      Description: strPropertyHasNoDescDesc),
    (Category: strPropertyParamDocumentation; MessageMask:
      strPropertyDiffParamCount; Description: strPropertyDiffParamCountDesc),
    (Category: strPropertyParamDocumentation; MessageMask:
      strPropertyUndocumentedParam; Description:
      strPropertyUndocumentedParamDesc),
    (Category: strPropertyParamDocumentation; MessageMask:
      strPropertyIncorrectParamType; Description:
      strPropertyIncorrectParamTypeDesc),
    (Category: strPropertyReturnDocumentation; MessageMask:
      strPropertyUndocumentedReturn; Description:
      strPropertyUndocumentedReturnDesc),
    (Category: strPropertyReturnDocumentation; MessageMask:
      strPropertyIncorrectReturnType; Description:
      strPropertyIncorrectReturnTypeDesc),
    (Category: strPropertyPreconDocumentation; MessageMask:
      strPropertyPreconNotDocumented; Description:
      strPropertyPreconNotDocumentedDesc),
    (Category: strPropertyPreconDocumentation; MessageMask:
      strPropertyMissingPrecon; Description: strPropertyMissingPreconDesc),
    (Category: strPropertyPreconDocumentation; MessageMask:
      strPropertyTooManyPrecons; Description: strPropertyTooManyPreconsDesc),
    (Category: strPropertyPostconDocumentation; MessageMask:
      strPropertyPostconNotDocumented; Description:
      strPropertyPostconNotDocumentedDesc),
    (Category: strPropertyPostconDocumentation; MessageMask:
      strPropertyMissingPostcon; Description: strPropertyMissingPostconDesc),
    (Category: strPropertyPostconDocumentation; MessageMask:
      strPropertyTooManyPostcons; Description: strPropertyTooManyPostconsDesc),
    (Category: strModuleInitSection; MessageMask:
      strMissingInitComment; Description: strMissingInitCommentDesc),
    (Category: strModuleFinalSection; MessageMask:
      strMissingFinalComment; Description: strMissingFinalCommentDesc)
  );

Var
  (** This is a global string list containing the special tags list. **)
  SpecialTags : TStringList;

  Function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Function IsTokenWhiteSpace(strToken : String) : Boolean;

Implementation

Uses
  Windows, StrUtils;

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
    If iIndex <= sl.Count - 1 Then
      Begin
        Val(sl[iIndex], iValue, i);
        If i <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Delete Then
          sl.Delete(iIndex);
      End;
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
  FIdents := TObjectList.Create(True);
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

  This method returns the index of the identifer which matches the given
  identifier else returns -1.

  @precon  None.
  @postcon Returns the index of the identifer which matches the given
           identifier else returns -1.

  @param   strIdent as a String
  @return  an Integer 

**)
Function TIdentList.Find(strIdent : String) : Integer;

Var
  i : Integer;

Begin
  Result := -1;
  For i := 0 To Count - 1 Do
    If AnsiCompareText(strIdent, Idents[i].Ident) = 0 Then
      Begin
        Result := i;
        Break;
      End;
End;

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
          If Heritage.Count + Parameters.Count + Properties.Count + Methods.Count = 0 Then
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
      If AnsiCompareText(strClassName, (FItems[i] As TObjectDecl).Identifier) = 0 Then
        Begin
          Result := i;
          Break;
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
  @param   AType       as a TTypeDecl
  @param   Value       as a String
  @param   Scope       as a TScope
  @param   iLine       as an Integer
  @param   iCol        as an Integer

**)
Constructor TParameter.Create(ParamMod : TParamModifier; Ident : String;
  boolArrayOf : Boolean; AType : TTypeDecl;
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

(**

  This method adds a parameter class to the parameters collection.

  @precon  Paremeter is a parameter to add to the method.
  @postcon Adds a parameter class to the parameters collection.

  @param   Parameter as a TParameter

**)
Procedure TParameterCollection.Add(Parameter : TParameter);

Begin
  FParameters.Add(Parameter);
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of parameters in the method.

  @return  an Integer

**)
Function TParameterCollection.GetCount : Integer;

Begin
  Result := FParameters.Count;
End;

(**

  This is a getter method for the Parameter array property.

  @precon  iIndex is th eindex of the parameter required.
  @postcon Returns a paramemter object for the specified item.

  @param   iIndex as an Integer
  @return  a TParameter

**)
Function TParameterCollection.GetParameter(iIndex : Integer) : TParameter;

Begin
  Result := FParameters[iIndex] As TParameter;
End;

(**

  This is the constructor method for the TParameterCollection class.

  @precon  None
  @postcon Creates the parameter collection.

**)
Constructor TParameterCollection.Create;

Begin
  FParameters := TObjectList.Create(True);
End;

(**

  This is the destructor method for the TParameterCollection class.

  @precon  None.
  @postcon Destroys the parameter collection.

**)
Destructor TParameterCollection.Destroy;

Begin
  FParameters.Free;
  Inherited;
End;

(**

  This method sorts the parameter collection.

  @precon  None.
  @postcon Sorts the parameter collection.

**)
Procedure TParameterCollection.Sort;

Begin
  FParameters.Sort(SortRecordDecl);
End;

(** --------------------------------------------------------------------------

  TProperty Methods

 -------------------------------------------------------------------------- **)

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
  For i := 0 To Prop.Parameters.Count - 1 Do
    Begin
      P := TParameter.Create(pamNone, '', False, Nil, '', scPrivate, 0, 0);
      P.Assign(Prop.Parameters[i]);
      Parameters.Add(P);
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
  FParameters := TParameterCollection.Create;
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
  FParameters.Free;
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
  If Parameters.Count > 0 Then
    Begin
      Result := Result + '[';
      For i := 0 To Parameters.Count - 1 Do
        Begin
          Case Parameters[i].ParamModifier Of
            pamConst : Result := Result + 'Const ';
            pamVar: Result := Result + 'Var ';
            pamOut: Result := Result + 'Out ';
          End;
          If i <> 0 Then
            Result := Result + '; ';
          Result := Result + Parameters[i].Identifier + ' : ' +
            Parameters[i].ParamType.AsString(True);
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

(**

  This method adds a property to the classes property collection.

  @precon  Prop is a property to be added to the class.
  @postcon Adds a property to the classes property collection.

  @param   Prop as a TProperty

**)
procedure TPropertyCollection.Add(Prop: TProperty);
begin
  FProperties.Add(Prop);
end;

(**

  This is a getter method for the Property array property.

  @precon  iIndex is the index of the property required.
  @postcon Returns the property object requested.

  @param   iIndex as an Integer
  @return  a TProperty

**)
function TPropertyCollection.GetProperty(iIndex: Integer): TProperty;
begin
  Result := FProperties[iIndex] As TProperty;
end;

(**

  This is a getter method for the PropertyCount property.

  @precon  None.
  @postcon Returns the number of properties in the class.

  @return  an Integer

**)
function TPropertyCollection.GetCount: Integer;
begin
  Result := FProperties.Count;
end;

(**

  This method sorts the property collection.

  @precon  None.
  @postcon Sorts the property collection.

**)
procedure TPropertyCollection.Sort;

begin
  FProperties.Sort(SortClassDecl);
End;

(**

  This is the constructor method for the TPropertyCollection class.

  @precon  None.
  @postcon Creates the properties collection.

**)
Constructor TPropertyCollection.Create;

Begin
  FProperties := TObjectList.Create(True);
End;

(**

  This is the destructor method for the TPropertyCollection class.

  @precon  None.
  @postcon Destroys the collection.

**)
Destructor TPropertyCollection.Destroy;

Begin
  FProperties.Free;
  Inherited;
End;

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
  FParameters := TParameterCollection.Create;
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
  FLabels := TIdentList.Create;
End;

(**

  This is the destructor method for the TMethodDecl class. It frees the
  parameters collection, the parameter and the directives.

  @precon  None.
  @postcon It frees the parameters collection, the parameter and the directives.

**)
Destructor TMethodDecl.Destroy;

Begin
  FLabels.Free;
  FResStrings.Free;
  FConsts.Free;
  FVars.Free;
  FTypes.Free;
  FLocalMethods.Free;
  FDirectives.Free;
  FParameters.Free;
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
  For i := 0 To Method.Parameters.Count - 1 Do
    Begin
      P := TParameter.Create(pamNone, '', False, Nil, '', scPrivate, 0, 0);
      P.Assign(Method.Parameters[i]);
      Parameters.Add(P);
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
  If Parameters.Count > 0 Then Result := Result + '(';
  For i := 0 To Parameters.Count - 1 Do
    Begin
      Result := Result + strParamModifier[Parameters[i].ParamModifier];
      Result := Result + Parameters[i].Identifier;
      If (i <> Parameters.Count - 1) And (Parameters[i].ParamType.AsString(True) =
        Parameters[i + 1].ParamType.AsString(True)) Then
        Result := Result + ', '
      Else
        If Parameters[i].ParamType.Count > 0 Then
          Begin
            Result := Result + ' :';
            If Parameters[i].ArrayOf Then Result := Result + ' Array Of';
            Result := Result + #32 + Parameters[i].ParamType.AsString(True);
            If i <> Parameters.Count - 1 Then Result := Result + '; ';
          End;
    End;
  If Parameters.Count > 0 Then Result := Result + ')';
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
        Break;
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

  @param   strClsName    as a String
  @param   strMethodName as a String
  @return  an Integer

**)
function TMethodCollection.Find(strClsName, strMethodName: String): Integer;

Var
  i : Integer;

begin
  Result := -1;
  For i := 0 To Count - 1 Do
    If (AnsiCompareText(strClsName, Method[i].ClsName) = 0) And
      (AnsiCompareText(strMethodName, Method[i].Identifier) = 0) Then
      Begin
        Result := i;
        Break;
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
  FParameters.Add(Parameter);
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

Var
  i : Integer;

Begin
  Result := Identifier;
  If Result <> '' Then
    Result := Result + #32;
  Result := Result + 'Record';
  For i := 0 To Parameters.Count - 1 Do
    Result := Result + #32 + Parameters[i].FIdentifier + ' : ' +
      Parameters[i].FParamType.AsString(True) + ';';
  Result := Result  + ' End';
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
  FParameters := TParameterCollection.Create;
end;

(**

  This is the destructor method for the TRecordDecl class.

  @precon  None.
  @postcon Destroy the instance of the class.

**)
destructor TRecordDecl.Destroy;
begin
  FParameters.Free;
  inherited;
end;

(**

  This method sorts the record classes paramters.

  @precon  None.
  @postcon Sorts the record classes paramters.

**)
procedure TRecordDecl.Sort;
begin
  inherited;
  FParameters.Sort;
end;

(** ---------------------------------------------------------------------------

  TObjectDecl Methods

 -------------------------------------------------------------------------- **)

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
  For i := 0 To AObject.Parameters.Count - 1 Do
    Begin
      P := TParameter.Create(pamNone, '', False, Nil, '', scPrivate, 0, 0);
      P.Assign(AObject.Parameters[i]);
      AddParameter(P);
    End;
  For i := 0 To AObject.Methods.Count - 1 Do
    Begin
      M := TMethodDecl.Create(mtProcedure, scPrivate, 0, 0);
      M.Assign(AObject.Methods[i]);
      Methods.Add(M);
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
  FMethods := TMethodCollection.Create;
end;

(**

  This is the destructor method for the TObjectDecl class.

  @precon  None.
  @postcon Destroy the instance of the class.

**)
destructor TObjectDecl.Destroy;
begin
  FMethods.Free;
  FHeritage.Free;
  inherited;
end;

(**

  This method sorts the object class's methods.

  @precon  None.
  @postcon Sorts the object classes methods.

**)
procedure TObjectDecl.Sort;
begin
  inherited;
  FMethods.Sort;
end;

(** ---------------------------------------------------------------------------

  TClassDecl Methods

 -------------------------------------------------------------------------- **)

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
  For i := 0 To A.Properties.Count - 1 Do
    Begin
      P := TProperty.Create('', scPrivate, 0, 0);
      P.Assign(A.Properties[i]);
      Properties.Add(P);
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
  If SealedClass Then
    Result := Result + ' Sealed';
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
  FProperties := TPropertyCollection.Create;
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
  FProperties.Free;
  inherited Destroy;
end;

(**

  This method corts the classes property list.

  @precon  None.
  @postcon Sorts the proerty list.

**)
procedure TClassDecl.Sort;
begin
  inherited;
  FProperties.Sort;
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
  @param   strMethod          as a String
  @param   iLine              as an Integer
  @param   iCol               as an Integer
  @param   ErrType            as a TErrorType

**)
constructor TDocError.Create(strMsg, strMethod: String; iLine, iCol: Integer;
  ErrType : TErrorType);

begin
  FMsg := strMsg;
  FLine := iLine;
  FCol := iCol;
  FErrorType := ErrType;
  FMethod := strMethod;
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
  @param   strMethod     as a String
  @param   iLine         as an Integer
  @param   iCol          as an Integer
  @param   ErrType       as a TErrorType

**)
procedure TDocErrorCollection.Add(strMsg, strMethod: String; iLine, iCol: Integer;
  ErrType : TErrorType);
begin
  FErrors.Add(TDocError.Create(strMsg, strMethod, iLine, iCol, ErrType));
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
  iIdentLine, iIdentColumn, iCommentLine, iCommentCol : Integer;
  DocConflictType: TDocConflictType);

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

  @param   IsModified       as a Boolean
  @param   strFileName      as a String

**)
constructor TBaseLanguageModule.Create(IsModified : Boolean; strFileName : String);
begin
  Inherited Create;
  FFileName := strFileName;
  FModified := IsModified;
  FOwnedItems := TObjectList.Create(True);
  FTokens := TObjectList.Create(True);
  FTokenIndex := 0;
  FPreviousTokenIndex := -1;
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
  FFinalSection := Nil;
  FInitSection := Nil;
  FModuleComment := Nil;
  FModuleName := '';
  FModuleNameCol := 0;
  FModuleNameLine := 0;
  FModuleType := mtUnit;
  FRequiresClause := Nil;
  FUsesClause := Nil;
  FCompilerDefs := TStringList.Create;
  FCompilerDefs.Sorted := True;
  FCompilerDefs.Duplicates := dupIgnore;
  FCompilerDefs.CaseSensitive := False;
  FCompilerConditionStack := TList.Create;
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
  If InitializationSection <> Nil Then
    InitializationSection.Free;
  If FinalizationSection <> Nil Then
    FinalizationSection.Free;
  FCompilerConditionStack.Free;
  FCompilerDefs.Free;
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
    Raise EDocException.Create(strUnExpectedEndOfFile);
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

  @note Needs a stack to hold the $IFDEF / $IFNDEF moves, i.e. iSkip + 1 or
        iSkip + 0 so that $ELSE can know how to handle its moves iSkip - 1
        or iSkip + 1 respectively and also $ENDIF. The stack will hold the
        $IFDEF / $IFNDEF move, i.e. +1 so that the first to find it between
        $ELSE and $ENDIF can reverse the move, i.e. if $ELSE does -1 then
        $ENDIF does nothing because the move is used up.

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
          Errors.Add(Format(strElseIfMissingIfDef, [Token.Line, Token.Column]),
            'ProcessCompilerDirective', Token.Line, Token.Column, etWarning);
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
          Errors.Add(Format(strEndIfMissingIfDef, [Token.Line, Token.Column]),
            'ProcessCompilerDirective', Token.Line, Token.Column, etWarning);
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
        Raise EDocException.Create(strUnExpectedStartOfFile);
    End;
End;

End.
