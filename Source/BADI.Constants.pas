(**

  This module contains constants to be used throughout the Browse and Doc It application.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Apr 2017

**)
Unit BADI.Constants;

Interface

Uses
  BADI.ResourceStrings,
  BADI.Types,
  Graphics;

Const
  (** This is a text string of revision from nil and a to z. **)
  strRevision = ' abcdefghijklmnopqrstuvwxyz';
  (** Universal name for all IDEs for use in the splash screen and about boxes. **)
  strSplashScreenName = 'Browse and Doc It %d.%d%s for %s';
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'Freeware by David Hoyle (Build %d.%d.%d.%d)';

  (** A default list of symbols which should not have spaces before them. **)
  strNoSpaceBeforeSymbols = ['(', '[', '{', ')', ']', '}', ';', ',', '.', '!', '?', '<', '>'];
  (** A default list of symbols which should not have spaces after them. **)
  strNoSpaceAfterSymbols = ['(', '[', '{', '.', '^', '<'];
  (** A default list of symbols which should have spaces after them. **)
  strSpaceAfterSymbols = ['=', ':', '+', '-', '*', '\', ','];

  (** A set of characters for whitespace **)
  strWhiteSpace : Set Of AnsiChar = [#32, #9];
  (** A set of characters for line feed and carriage return **)
  strLineEnd : Set of AnsiChar = [#10, #13];

  (** A constant array to define the module explorer option groups. **)
  DocOptionGroups : Array[Low(TDOcOptionGroup)..High(TDOcOptionGroup)] Of String = (
    'General',
    'Errors, Warnings, Hints and Conflicts',
    'Types',
    'Module',
    'Methods',
    'Properties',
    'Initialization / Finalization',
    'Miscellaneous'
  );

  (** This is a string array representing the TDocOption enumerates. **)
  DocOptionInfo : Array[Low(TDocOption)..High(TDocOption)] Of TDocOptionRec = (
    (FDescription : strDrawSynHighModuleExplorer;             FEnabled : False; FGroup: dogGeneral),
    (FDescription : strShowCommentsInTheHints;                FEnabled : False; FGroup: dogGeneral),

    (FDescription : strShowErrors;                            FEnabled : True;  FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strShowWarnings;                          FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strShowHints;                             FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strShowDocumentationConflicts;            FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strSyntaxHighlightErrors;                 FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strSyntaxHighlightWarnings;               FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strSyntaxHighlightHints;                  FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),
    (FDescription : strSyntaxHighlightDocumentationConflicts; FEnabled : False; FGroup: dogErrorsWarningsHintsAndConflicts),


    (FDescription : strShowUndocumentedTypes;                 FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedRecords;               FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedObjects;               FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedClasses;               FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedInterfaces;            FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedVariables;             FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedConstants;             FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedFields;                FEnabled : False; FGroup: dogTypes),
    (FDescription : strShowUndocumentedClassDecls;            FEnabled : False; FGroup: dogTypes),

    (FDescription : strShowUndocumentedModule;                FEnabled : True;  FGroup: dogModule),
    (FDescription : strShowMissingModuleDate;                 FEnabled : False; FGroup: dogModule),
    (FDescription : strShowCheckModuleDate;                   FEnabled : False; FGroup: dogModule),
    (FDescription : strShowMissingModuleVersion;              FEnabled : False; FGroup: dogModule),
    (FDescription : strShowMissingModuleAuthor;               FEnabled : False; FGroup: dogModule),

    (FDescription : strShowMissingMethodDocumentation;        FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowMissingMethodDocDesc;              FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowDiffMethodParameterCount;          FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowUndocumentedMethodParameters;      FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowIncorrectMethodParameterType;      FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowUndocumentedMethodReturn;          FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowIncorrectMethodReturnType;         FEnabled : True;  FGroup: dogMethod),
    (FDescription : strShowMissingMethodPreConditions;        FEnabled : False; FGroup: dogMethod),
    (FDescription : strShowMissingMethodPostConditions;       FEnabled : False; FGroup: dogMethod),

    (FDescription : strShowMissingPropertyDocumentation;      FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowMissingPropertyDocuDesc;           FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowDiffPropertyParameterCount;        FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowUndocumentedPropertyParameter;     FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowIncorrectPropertyParameterType;    FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowUndocumentedPropertyReturnType;    FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowIncorrectPropertyReturnType;       FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowMissingPropertyPreConditions;      FEnabled : False; FGroup: dogProperty),
    (FDescription : strShowMissingPropertyPostConditions;     FEnabled : False; FGroup: dogProperty),

    (FDescription : strShowMissingInitComment;                FEnabled : False; FGroup: dogInitializationFinalization),
    (FDescription : strShowMissingFinalComment;               FEnabled : False; FGroup: dogInitializationFinalization),

    {(FDescription : strShowIDEErrorsOnSuccessfulParse;        FEnabled : False),}
    (FDescription : strShowParserErrorOrigin;                 FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strShowUnreferencedSymbols;               FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strShowPerfCountersInModuleExplorer;      FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strShowPerfCountersInDocSummary;          FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strStrictConstantExpressions;             FEnabled : True;  FGroup: dogMiscellaneous),
    (FDescription : strShowMissingVBExceptionWarnings;        FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strAddPreAndPostToComments;               FEnabled : False; FGroup: dogMiscellaneous)
  );

  (** This is a default set of font information for the application. **)
  strTokenTypeInfo : Array[Low(TBADITokenType)..High(TBADITokenType)] Of TTokenFontInfo = (
    (FForeColour : clRed;        FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [fsBold];               FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [fsBold];               FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clMaroon;     FStyles : [fsBold];               FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clInfoText;   FStyles : [];                     FBackColour: clInfoBk),
    (FForeColour : clWindowText; FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clNavy;       FStyles : [];                     FBackColour: clWindow),
    (FForeColour : clPurple;     FStyles : [fsBold, fsUnderline];  FBackColour: clWindow),
    (FForeColour : clMaroon;     FStyles : [];                     FBackColour: clWindow)
  );

  (** This is a list of image resource masks to be placed of the below images to create vesions
      for different scopes. **)
  BADIScopeList : Array[Low(TScope)..High(TScope)] Of TImageIndexInfo = (
    (FResourceName : 'NoneMask';                      FMaskColour: clLime),
    (FResourceName : 'GlobalMask';                    FMaskColour: clLime),
    (FResourceName : 'LocalMask';                     FMaskColour: clLime),
    (FResourceName : 'PrivateMask';                   FMaskColour: clLime),
    (FResourceName : 'ProtectedMask';                 FMaskColour: clLime),
    (FResourceName : 'PublicMask';                    FMaskColour: clLime),
    (FResourceName : 'PublishedMask';                 FMaskColour: clLime),
    (FResourceName : 'FriendMask';                    FMaskColour: clLime)
  );

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
    (FResourceName : 'PublicType';                    FMaskColour: clLime),

    (FResourceName : 'RecordsLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicRecord';                  FMaskColour: clLime),

    (FResourceName : 'FieldsLabel';                   FMaskColour: clLime),
    (FResourceName : 'PublicField';                   FMaskColour: clLime),

    (FResourceName : 'ObjectsLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicObject';                  FMaskColour: clLime),

    (FResourceName : 'PublicConstructor';             FMaskColour: clLime),
    (FResourceName : 'PublicDestructor';              FMaskColour: clFuchsia),
    (FResourceName : 'PublicProcedure';               FMaskColour: clLime),
    (FResourceName : 'PublicFunction';                FMaskColour: clLime),

    (FResourceName : 'ClassesLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicClass';                   FMaskColour: clLime),

    (FResourceName : 'PropertyLabel';                 FMaskColour: clFuchsia),
    (FResourceName : 'PublicProperty';                FMaskColour: clFuchsia),

    (FResourceName : 'InterfacesLabel';               FMaskColour: clLime),
    (FResourceName : 'PublicInterface';               FMaskColour: clLime),

    (FResourceName : 'DispInterfaceSLabel';           FMaskColour: clLime),
    (FResourceName : 'PublicDispInterface';           FMaskColour: clLime),

    (FResourceName : 'PublicConstantsLabel';          FMaskColour: clLime),
    (FResourceName : 'PublicConst';                   FMaskColour: clLime),

    (FResourceName : 'PublicResourceStringsLabel';    FMaskColour: clLime),
    (FResourceName : 'PublicResourceString';          FMaskColour: clLime),

    (FResourceName : 'PublicVariablesLabel';          FMaskColour: clLime),
    (FResourceName : 'PublicVariable';                FMaskColour: clLime),

    (FResourceName : 'PublicThreadVarsLabel';         FMaskColour: clLime),
    (FResourceName : 'PublicThreadVar';               FMaskColour: clLime),

    (FResourceName : 'PublicClassVariablesLabel';     FMaskColour: clLime),
    (FResourceName : 'PublicClassVariable';           FMaskColour: clLime),

    (FResourceName : 'ExportedHeadingsLabel';         FMaskColour: clLime),

    (FResourceName : 'ExportedFunctionsLabel';        FMaskColour: clLime),
    (FResourceName : 'PublicExportedFunction';        FMaskColour: clLime),

    (FResourceName : 'PublicLabelsLabel';             FMaskColour: clLime),
    (FResourceName : 'PublicLabel';                   FMaskColour: clLime),

    (FResourceName : 'ImplementedMethodsLabel';       FMaskColour: clLime),
    (FResourceName : 'MethodsLabel';                  FMaskColour: clLime),

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
      FConflictType: dciMissing),
    (FMessage: strTooManyConflicts;
      FDescription: strTooManyConflictsDesc;
      FConflictType: dciMissing)
  );

  (** A list of strings representing the token types. **)
  strTokenType : Array[Low(TBADITokenType)..High(TBADITokenType)] Of String = (
    'Unknown',
    'White Space',
    'Reserved Word',
    'Identifier',
    'Number',
    'Symbol',
    'Line End',
    'Single Literal',
    'Double Literal',
    'Line Comment',
    'Block Comment',
    'HTML Start Tag',
    'HTML End Tag',
    'Directive',
    'Compiler Directive',
    'Link Tag',
    'Tree Header',
    'File End',
    'Line Continuation',
    'Custom User Token',
    'Explorer Highlight',
    'Plain Text',
    'Comment Text',
    'Tag Header Text',
    'Tag Text'
  );

  (** A constant string to represent the position of the main procedure code in
      a profiling code block. **)
  strMethodCode = '$METHODCODE$';
  (** A constant string to represent the position to insert the method name into
      the profiling code block. **)
  strMethodName = '$METHODNAME$';

  (** A constant array to describe the menu defaults. **)
  BADIMenus : Array[Low(TBADIMenu)..High(TBADIMenu)] Of TBADIMenuRecord = (
    (FName: 'BADIModuleExplorer';  FCaption: 'Module &Explorer';      FShortcut: 'CTRL+SHIFT+ALT+ENTER'; FMaskColor: clLime),
    (FName: 'BADIDocumentation';   FCaption: '&Documentation...';     FShortcut: 'CTRL+SHIFT+ALT+D';     FMaskColor: clLime),
    (FName: 'BADIDunit';           FCaption: 'D&Unit...';             FShortcut: 'CTRL+SHIFT+ALT+U';     FMaskColor: clLime),
    (FName: 'BADIProfiling';       FCaption: 'Pro&filing...';         FShortcut: 'CTRL+SHIFT+ALT+F';     FMaskColor: clLime),
    (FName: 'BADISep1';            FCaption: '';                      FShortcut: '';                     FMaskColor: clLime),
    (FName: 'BADIFocusEditor';     FCaption: 'Focus Edi&tor';         FShortcut: 'CTRL+SHIFT+ALT+E';     FMaskColor: clLime),
    (FName: 'BADIMethodComment';   FCaption: '&Method Comment';       FShortcut: 'CTRL+SHIFT+ALT+M';     FMaskColor: clLime),
    (FName: 'BADIPropertyComment'; FCaption: '&Property Comment';     FShortcut: 'CTRL+SHIFT+ALT+P';     FMaskColor: clLime),
    (FName: 'BADIBlockComment';    FCaption: 'Block &Comment';        FShortcut: 'CTRL+SHIFT+ALT+B';     FMaskColor: clLime),
    (FName: 'BADILineComment';     FCaption: '&Line Comment';         FShortcut: 'CTRL+SHIFT+ALT+L';     FMaskColor: clLime),
    (FName: 'BADIInSituComment';   FCaption: '&In-Situ Comment';      FShortcut: 'CTRL+SHIFT+ALT+I';     FMaskColor: clLime),
    (FName: 'BADIToDoComment';     FCaption: '&ToDo Comment';         FShortcut: 'CTRL+SHIFT+ALT+T';     FMaskColor: clLime),
    (FName: 'BADISep2';            FCaption: '';                      FShortcut: '';                     FMaskColor: clLime),
    (FName: 'BADIOptions';         FCaption: '&Options...';           FShortcut: 'CTRL+SHIFT+ALT+O';     FMaskColor: clLime)
  );

  (** A constant to represent the initial (failed) position of a wizard reference. **)
  iWizardFailState = -1;

  (** A constant array to represent the Special Tag Properties. **)
  strTagProperty : Array[Low(TBADITagProperty)..High(TBADITagProperty)] Of String = (
    'Show the Tag in the Module Explorer',
    'Auto Expand the Tag in Module Explorer',
    'Show the Tag in Documentation',
    'Fixed Font Tag (preserves LF/CR and Indents)',
    'Syntax Highlight the Tag'
  );

Implementation

End.
