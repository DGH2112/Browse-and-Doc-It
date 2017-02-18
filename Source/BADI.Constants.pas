(**

  This module contains constants to be used throughout the Browse and Doc It application.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.Constants;

Interface

Uses
  BADI.ResourceStrings,
  BADI.Types,
  Graphics;

Const
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
      FConflictType: dciMissing),
    (FMessage: strTooManyConflicts;
      FDescription: strTooManyConflictsDesc;
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

Implementation

End.
