(**

  This module contains all the simple types used through the Browse and Doc It application.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Types;

Interface

Uses
  SysUtils,
  Graphics;

Type
  (** Type to distinguish Stream position from token index. **)
  TStreamPosition = Integer;
  (** Type to distinguish Stream position from token index. **)
  TTokenIndex = Integer;
  (** An enumerate type to define the stream status and token types. **)
  TBADITokenType = (
    ttUnknown,
    ttWhiteSpace,
    ttReservedWord,
    ttIdentifier,
    ttNumber,
    ttSymbol,
    ttLineEnd,
    ttSingleLiteral,
    ttDoubleLiteral,
    ttLineComment,
    ttBlockComment,
    ttHTMLStartTag,
    ttHTMLEndTag,
    ttDirective,
    ttCompilerDirective,
    ttLinkTag,
    ttTreeHeader,
    ttFileEnd,
    ttLineContinuation,
    ttCustomUserToken,
    ttExplorerHighlight
  );
  (** An enumerate for the scoping of identifiers. **)
  TScope = (
    scNone,
    scGlobal,
    scLocal,
    scPrivate,
    scProtected,
    scPublic,
    scPublished,
    scFriend
  );
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
    iiPublicType,

    iiRecordsLabel,
    iiPublicRecord,

    iiFieldsLabel,
    iiPublicField,

    iiObjectsLabel,
    iiPublicObject,

    iiPublicConstructor,
    iiPublicDestructor,
    iiPublicProcedure,
    iiPublicFunction,

    iiClassesLabel,
    iiPublicClass,

    iiPropertiesLabel,
    iiPublicProperty,

    iiInterfacesLabel,
    iiPublicInterface,

    iiDispInterfacesLabel,
    iiPublicDispInterface,

    iiPublicConstantsLabel,
    iiPublicConstant,

    iiPublicResourceStringsLabel,
    iiPublicResourceString,

    iiPublicVariablesLabel,
    iiPublicVariable,

    iiPublicThreadVarsLabel,
    iiPublicThreadVar,

    iiPublicClassVariablesLabel,
    iiPublicClassVariable,

    iiExportedHeadingsLabel,

    iiExportedFunctionsLabel,
    iiPublicExportedFunction,

    iiPublicLabelsLabel,
    iiPublicLabel,

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
    dctMissingFinalComment,

    dctTooManyConflicts
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

  (** A record to describe document conflict information. **)
  TDocConflictTable = Record
    FMessage      : String;
    FDescription  : String;
    FConflictType : TDocConflictIcon;
  End;

  (** This enumerate defind the type of information to find. **)
  TFindType = (ftName, ftIdentifier);

  (** This is a type for a set of characters and the return type of several
      properties. **)
  TCharSet = Set of AnsiChar;

  (** A type to define the type of token search. **)
  TSeekToken = (stActual, stFirst);

  (** A type to define an array of integers. **)
  TArrayOfInteger = Array Of Integer;

  (** This enumerate defines the type of compiler condition placed on the stack. **)
  TCompilerDefType = (cdtIFDEF, cdtIFNDEF, cdtELSE, cdtENDIF);
  (** An enumerate to define the condition of the compiler definition. **)
  TCompilerCondition = (ccIncludeCode, ccExcludeCode);

  (** This enumerate define the position of the editor when an item is selected
      in the module explorer. **)
  TBrowsePosition = (
    bpCommentTop,
    bpCommentCentre,
    bpIdentifierTop,
    bpIdentifierCentre,
    bpIdentifierCentreShowAllComment
  );

  (** A record to define the font information for each token type. **)
  TTokenFontInfo = Record
    FForeColour : TColor;
    FStyles     : TFontStyles;
    FBackColour : TColor;
  End;

  (** An enumerate to define the different types of issues to limit output for. **)
  TLimitType = (ltErrors, ltWarnings, ltHints, ltConflicts);

  (** This emunerate descibed the different types of doc comment .**)
  TCommentStyle = (csBlock, csLine, csInSitu);
  (** An enumerate to define the type of comment output that can be generated by
      WriteComment. **)
  TCommentType = (ctNone, ctPascalBlock, ctPascalBrace, ctCPPBlock, ctCPPLine,
    ctVBLine, ctXML);

  (** A silent parser abort exception. **)
  EBADIParserAbort = Class(Exception);
  (** An exception or an erro when parsing a file. **)
  EBADIParserError = Class(Exception);

Implementation

End.
