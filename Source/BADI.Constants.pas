(**

  This module contains constants to be used throughout the Browse and Doc It application.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  {$IFDEF DEBUG}
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'David Hoyle (c) 2019 License GNU GPL 3 (DEBUG Build %d.%d.%d.%d)';
  {$ELSE}
  (** This is another message string to appear in the BDS 2005/6 splash screen **)
  strSplashScreenBuild = 'David Hoyle (c) 2019 License GNU GPL 3 (Build %d.%d.%d.%d)';
  {$ENDIF}

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
    strOptionGroupGeneral,
    strOptionGroupErrors,
    strOptionGroupWarnings,
    strOptionGroupHints,
    strOptionGroupConflicts,
    strOptionGroupChecks,
    strOptionGroupMetrics,
    strOptionGroupTypes,
    strOptionGroupModule,
    strOptionGroupMethods,
    strOptionGroupProperties,
    strOptionGroupInitializationFinalization,
    strOptionGroupMiscellaneous
  );

  (** This is a string array representing the TDocOption enumerates. **)
  DocOptionInfo : Array[Low(TDocOption)..High(TDocOption)] Of TDocOptionRec = (
    (FDescription : strDrawSynHighModuleExplorer;             FEnabled : False; FGroup: dogGeneral),
    (FDescription : strShowCommentsInTheHints;                FEnabled : False; FGroup: dogGeneral),
    (FDescription : strShowChildCountinTitles;                FEnabled : False; FGroup: dogGeneral),

    (FDescription : strShowErrors;                            FEnabled : True;  FGroup: dogErrors),
    (FDescription : strExpandErrors;                          FEnabled:   True; FGroup: dogErrors),
    (FDescription : strSyntaxHighlightErrors;                 FEnabled : False; FGroup: dogErrors),

    (FDescription : strShowWarnings;                          FEnabled : False; FGroup: dogWarnings),
    (FDescription : strExpandWarnings;                        FEnabled:   True; FGroup: dogWarnings),
    (FDescription : strSyntaxHighlightWarnings;               FEnabled : False; FGroup: dogWarnings),

    (FDescription : strShowHints;                             FEnabled : False; FGroup: dogHints),
    (FDescription : strExpandHints;                           FEnabled:   True; FGroup: dogHints),
    (FDescription : strSyntaxHighlightHints;                  FEnabled : False; FGroup: dogHints),

    (FDescription : strShowDocumentationConflicts;            FEnabled : False; FGroup: dogConflicts),
    (FDescription : strExpandDocConflicts;                    FEnabled:   True; FGroup: dogConflicts),
    (FDescription : strSyntaxHighlightDocumentationConflicts; FEnabled : False; FGroup: dogConflicts),

    (FDescription : strShowModuleChecks;                      FEnabled : False; FGroup: dogChecks),
    (FDescription : strExpandChecks;                          FEnabled:   True; FGroup: dogChecks),
    (FDescription : strSyntaxHighlightChecks;                 FEnabled : False; FGroup: dogChecks),
    (FDescription : strAutoHideChecksWithNoIssues;            FEnabled : False; FGroup: dogChecks),

    (FDescription : strShowModuleMetrics;                     FEnabled : False; FGroup: dogMetrics),
    (FDescription : strExpandMetrics;                         FEnabled:   True; FGroup: dogMetrics),
    (FDescription : strSyntaxHighlightMetrics;                FEnabled : False; FGroup: dogMetrics),
    (FDescription : strAutoHideMetricsWithNoIssues;           FEnabled : False; FGroup: dogMetrics),



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
    {(FDescription : strShowParserErrorOrigin;                 FEnabled : False; FGroup: dogMiscellaneous),}
    (FDescription : strShowUnreferencedSymbols;               FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strShowPerfCountersInModuleExplorer;      FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strShowPerfCountersInDocSummary;          FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strStrictConstantExpressions;             FEnabled : True;  FGroup: dogMiscellaneous),
    (FDescription : strShowMissingVBExceptionWarnings;        FEnabled : False; FGroup: dogMiscellaneous),
    (FDescription : strAddPreAndPostToComments;               FEnabled : False; FGroup: dogMiscellaneous)
  );

  (** This is a default set of font information for the application. **)
  strTokenTypeInfo : TBADITokenFontInfoTokenSet = (
    (FForeColour : clRed;        FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [fsBold];               FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [fsBold];               FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clMaroon;     FStyles : [fsBold];               FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clBlack;      FStyles : [];                     FBackColour: clNone),
    (FForeColour : clInfoText;   FStyles : [];                     FBackColour: clInfoBk),
    (FForeColour : clWindowText; FStyles : [];                     FBackColour: clNone),
    (FForeColour : clNavy;       FStyles : [];                     FBackColour: clNone),
    (FForeColour : clPurple;     FStyles : [fsBold, fsUnderline];  FBackColour: clNone),
    (FForeColour : clMaroon;     FStyles : [];                     FBackColour: clNone),
    (FForeColour : clAqua;       FStyles : [];                     FBackColour: clNone)
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

    (FResourceName : 'ErrorFolder';                   FMaskColour: clLime),
    (FResourceName : 'Error';                         FMaskColour: clLime),
    (FResourceName : 'WarningFolder';                 FMaskColour: clLime),
    (FResourceName : 'Warning';                       FMaskColour: clLime),
    (FResourceName : 'HintFolder';                    FMaskColour: clFuchsia),
    (FResourceName : 'Hint';                          FMaskColour: clFuchsia),

    (FResourceName : 'DocConflictFolder';             FMaskColour: clLime),
    (FResourceName : 'DocConflictIncorrect';          FMaskColour: clLime),
    (FResourceName : 'DocConflictItem';               FMaskColour: clLime),
    (FResourceName : 'DocConflictMissing';            FMaskColour: clLime),

    (FResourceName : 'MetricFolder';                  FMaskColour: clLime),
    (FResourceName : 'MetricIncorrect';               FMaskColour: clLime),
    (FResourceName : 'MetricItem';                    FMaskColour: clLime),
    (FResourceName : 'MetricMissing';                 FMaskColour: clLime),

    (FResourceName : 'CheckFolder';                   FMaskColour: clFuchsia),
    (FResourceName : 'CheckIncorrect';                FMaskColour: clFuchsia),
    (FResourceName : 'CheckItem';                     FMaskColour: clFuchsia),
    (FResourceName : 'CheckMissing';                  FMaskColour: clFuchsia),

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
    strTokenUnknown,
    strTokenWhiteSpace,
    strTokenReservedWord,
    strTokenIdentifier,
    strTokenNumber,
    strTokenSymbol,
    strTokenLineEnd,
    strTokenSingleLiteral,
    strTokenDoubleLiteral,
    strTokenLineComment,
    strTokenBlockComment,
    strTokenHTMLStartTag,
    strTokenHTMLEndTag,
    strTokenDirective,
    strTokenCompilerDirective,
    strTokenLinkTag,
    strTokenTreeHeader,
    strTokenFileEnd,
    strTokenLineContinuation,
    strTokenCustomUserToken,
    strTokenExplorerHighlight,
    strTokenPlainText,
    strTokenCommentText,
    strTokenTagHeaderText,
    strTokenTagText,
    strSearchHighlight
  );

  (** A constant string to represent the position of the main procedure code in
      a profiling code block. **)
  strMethodCode = '$METHODCODE$';
  (** A constant string to represent the position to insert the method name into
      the profiling code block. **)
  strMethodName = '$METHODNAME$';

  (** A constant array to describe the menu defaults. **)
  BADIMenus : Array[Low(TBADIMenu)..High(TBADIMenu)] Of TBADIMenuRecord = (
    (FName: 'BADIModuleExplorer';   FCaption: strMenuModuleExplorer;   FShortcut: 'CTRL+SHIFT+ALT+ENTER'; FMaskColor: clLime),
    (FName: 'BADIDocumentation';    FCaption: strMenuDocumentation;    FShortcut: 'CTRL+SHIFT+ALT+D';     FMaskColor: clLime),
    (FName: 'BADIDunit';            FCaption: strMenuDUnit;            FShortcut: 'CTRL+SHIFT+ALT+U';     FMaskColor: clLime),
    (FName: 'BADIProfiling';        FCaption: strMenuProfiling;        FShortcut: 'CTRL+SHIFT+ALT+F';     FMaskColor: clLime),
    (FName: 'BADISep1';             FCaption: strMenuSep;              FShortcut: '';                     FMaskColor: clLime),
    (FName: 'BADIFocusEditor';      FCaption: strMenuFocusEditor;      FShortcut: 'CTRL+SHIFT+ALT+E';     FMaskColor: clLime),
    (FName: 'BADIMethodComment';    FCaption: strMenuMethodComment;    FShortcut: 'CTRL+SHIFT+ALT+M';     FMaskColor: clLime),
    (FName: 'BADIPropertyComment';  FCaption: strMenuPropertyComment;  FShortcut: 'CTRL+SHIFT+ALT+P';     FMaskColor: clLime),
    (FName: 'BADIBlockComment';     FCaption: strMenuBlockComment;     FShortcut: 'CTRL+SHIFT+ALT+B';     FMaskColor: clLime),
    (FName: 'BADILineComment';      FCaption: strMenuLineComment;      FShortcut: 'CTRL+SHIFT+ALT+L';     FMaskColor: clLime),
    (FName: 'BADIInSituComment';    FCaption: strMenuInSituComment;    FShortcut: 'CTRL+SHIFT+ALT+I';     FMaskColor: clLime),
    (FName: 'BADIToDoComment';      FCaption: strMenuToDoComment;      FShortcut: 'CTRL+SHIFT+ALT+T';     FMaskColor: clLime),
    (FName: 'BADISep2';             FCaption: strMenuSep;              FShortcut: '';                     FMaskColor: clLime),
    (FName: 'BADIRefactorConstant'; FCaption: strMenuRefactorConstant; FShortcut: 'CTRL+SHIFT+ALT+C';     FMaskColor: clLime),
    (FName: 'BADIMetrics';          FCaption: strMenuMetrics;          FShortcut: 'CTRL+SHIFT+ALT+S';     FMaskColor: clLime),
    (FName: 'BADIChecks';           FCaption: strMenuChecks;           FShortcut: 'CTRL+SHIFT+ALT+H';     FMaskColor: clFuchsia),
    (FName: 'BADISep3';             FCaption: strMenuSep;              FShortcut: '';                     FMaskColor: clLime),
    (FName: 'BADIOptions';          FCaption: strMenuOptions;          FShortcut: 'CTRL+SHIFT+ALT+O';     FMaskColor: clLime)
  );

  (** A constant to represent the initial (failed) position of a wizard reference. **)
  iWizardFailState = -1;

  (** A constant array to represent the Special Tag Properties. **)
  strTagProperty : Array[Low(TBADITagProperty)..High(TBADITagProperty)] Of String = (
    strTagPropShowInExpl,
    strTagPropExpand,
    strTagPropShowInDocs,
    strTagPropFixedFont,
    strTagPropSyntax
  );

  (** A constant name for the Long Method metric so it can be disabled. **)
  strLongMethodNoMetric = 'LongMethod';
  (** A constant name for the Long Parameter List metric so it can be disabled. **)
  strLongParameterListNoMetric = 'LongParameterList';
  (** A constant name for the Long Variable List metric so it can be disabled. **)
  strLongVariableListNoMetric = 'LongVariableList';
  (** A constant name for the Hard Coded Integer metric so it can be disabled. **)
  strHardCodedIntegerNoMetric = 'HardCodedInteger';
  (** A constant name for the Hard Coded Integer Ignore Zeros metric so it can be disabled. **)
  strHCIntIgnoreZeroNoMetric = 'HCIntIgnoreZero';
  (** A constant name for the Hard Coded Integer Ignore Ones metric so it can be disabled. **)
  strHCIntIgnoreOneNoMetric = 'HCIntIgnoreOne';
  (** A constant name for the Hard Coded Numbers metric so it can be disabled. **)
  strHardCodedNumberNoMetric = 'HardCodedNumber';
  (** A constant name for the Hard Coded Numbers Ignore Zero metric so it can be disabled. **)
  strHCNumIgmoreZeroNoMetric = 'HCNumIgmoreZero';
  (** A constant name for the Hard Coded Strings metric so it can be disabled. **)
  strHardCodedStringNoMetric = 'HardCodedString';
  (** A constant name for the Hard Coded Strings Igmore Empty metric so it can be disabled. **)
  strHCStrIgnoreEmptyNoMetric = 'HCStrIgnoreEmpty';
  (** A constant name for the Hard Coded String Ignore Single metric so it can be disabled. **)
  strHCStrIgnoreSingleNoMetric = 'HCStrIgnoreSingle';
  (** A constant name for the Unsorted Method metric so it can be disabled. **)
  strUnsortedModuleNoMetric = 'UnsortedModule';
  (** A constant name for the With Statement metric so it can be disabled. **)
  strWithStatementNoMetric = 'WithStatement';
  (** A constant name for the Goto Statement metric so it can be disabled. **)
  strGotoStatementNoMetric = 'GotoStatement';
  (** A constant name for the NestedIFDepth metric so it can be disabled. **)
  strNestedIFDepthNoMetric = 'NestedIFDepth';
  (** A constant name for the Cyclometric Complexity metric so it can be disabled. **)
  strCyclometricComplexityNoMetric = 'CyclometricComplexity';
  (** A constant name for the Cyclometric Complexity Igmore Expressions metric so it can be disabled. **)
  strCCIncludeExpressionNoMetric = 'CCIncludeExpression';
  (** A constant name for the Toxicity metric so it can be disabled. **)
  strToxicityNoMetric = 'Toxicity';
  (** A constant name for the Empty Except metric so it can be disabled. **)
  strEmptyEXCEPTNoMetric = 'EmptyEXCEPT';
  (** A constant name for the Empty Finally metric so it can be disabled. **)
  strEmptyFINALLYNoMetric = 'EmptyFINALLY';
  (** A constant name for the Exception eating metric so it can be disabled. **)
  strExceptionEatingNoMetric = 'ExceptionEating';
  (** A constant name for the Empty Then metric so it can be disabled. **)
  strEmptyTHENNoMetric = 'EmptyTHEN';
  (** A constant name for the Empty Else metric so it can be disabled. **)
  strEmptyELSENoMetric = 'EmptyELSE';
  (** A constant name for the Empty Case metric so it can be disabled. **)
  strEmptyCASENoMetric = 'EmptyCASE';
  (** A constant name for the Empty For metric so it can be disabled. **)
  strEmptyFORNoMetric = 'EmptyFOR';
  (** A constant name for the Empty While metric so it can be disabled. **)
  strEmptyWHILENoMetric = 'EmptyWHILE';
  (** A constant name for the Empty Repeat metric so it can be disabled. **)
  strEmptyREPEATNoMetric = 'EmptyREPEAT';
  (** A constant name for the Empty Begin End metric so it can be disabled. **)
  strEmptyBEGINENDNoMetric = 'EmptyBEGINEND';
  (** A constant name for the Empty Initialization metric so it can be disabled. **)
  strEmptyInitializationNoMetric = 'EmptyInitialization';
  (** A constant name for the Empty Finalization metric so it can be disabled. **)
  strEmptyFinalizationNoMetric = 'EmptyFinalization';
  (** A constant name for the Empty Method metric so it can be disabled. **)
  strEmptyMethodNoMetric = 'EmptyMethod';
  (** A constant name for the Missing CONST in Parameters metric so it can be disabled. **)
  strMissingCONSTInParamNoMetric = 'MissingCONSTInParam';
  (** A constant name for the Missign Const in Parameters Ignore Event handlers metric so it can be
      disabled. **)
  strMCParmListIgnoreEventsNoMetric = 'MCParmListIgnoreEvents';

  (** A constant array of default options for the Module Metrics. **)
  ModuleMetrics : Array[Low(TBADIModuleMetric)..High(TBADIModuleMetric)] Of TBADIMetricRecord = (
    (
      FName: strLongMethodNoMetric;
      FCategory: strLongMethodImplementationsCat;
      FMessage: strMethodTooLongMsg;
      FDescription: strMethodTooLongDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit: 50.0;
      FLimitType: ltInteger),
    (
      FName: strLongParameterListNoMetric;
      FCategory: strLongMethodParameterListsCat;
      FMessage: strMethodHasTooManyParamsMsg;
      FDescription: strMethodHasTooManyParamsDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  7.0;
      FLimitType: ltInteger),
    (
      FName: strLongVariableListNoMetric;
      FCategory: strLongMethodVariableListsCat;
      FMessage: strMethodHasLongVarListMsg;
      FDescription: strMethodHasLongVarListDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  7.0;
      FLimitType: ltInteger),
    (
      FName: strNestedIFDepthNoMetric;
      FCategory: strNestedIFDepthCat;
      FMessage: strMethodHasHighIFDepthMsg;
      FDescription: strMethodHasHighIFDepthDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  5.0;
      FLimitType: ltInteger),
    (
      FName: strCyclometricComplexityNoMetric;
      FCategory: strMethodCyclometricComplexityCat;
      FMessage: strMethodHasHighCyclometricComplexityMsg;
      FDescription: strMethodHasHighCyclometricComplexityDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit: 10.0;
      FLimitType: ltInteger),
    (
      FName: strToxicityNoMetric;
      FCategory: strMethodToxicityCat;
      FMessage: strMethodHasHighToxocityValueMsg;
      FDescription: strMethodHasHighToxocityValueDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  1.0;
      FLimitType: ltFloat)
  );

  (** A constant array of information for the mnetric sub-options. **)
  ModuleMetricSubOps : Array[Low(TBADIModuleMetricSubOp)..High(TBADIModuleMetricSubOp)] Of
    TBADIMetricSubOpRecord = (
    (FName: 'MethodCCIncIF';         FDescription: strMethodCCIncIF;         FParentMetric: mmCyclometricComplexity),
    (FName: 'MethodCCIncCASE';       FDescription: strMethodCCIncCASE;       FParentMetric: mmCyclometricComplexity),
    (FName: 'MethodCCIncWHILE';      FDescription: strMethodCCIncWHILE;      FParentMetric: mmCyclometricComplexity),
    (FName: 'MethodCCIncREPEAT';     FDescription: strMethodCCIncREPEAT;     FParentMetric: mmCyclometricComplexity),
    (FName: 'MethodCCIncSubExprCat'; FDescription: strMethodCCSubExprCat;    FParentMetric: mmCyclometricComplexity),
    (FName: 'ToxicityIncMethodLen';  FDescription: strToxicityIncMethodLen;  FParentMetric: mmToxicity),
    (FName: 'ToxicityIncParamLen';   FDescription: strToxicityIncParamLen;   FParentMetric: mmToxicity),
    (FName: 'ToxicityIncVarLen';     FDescription: strToxicityIncVarLen;     FParentMetric: mmToxicity),
    (FName: 'ToxicityIncIFDepth';    FDescription: strToxicityIncIFDepth;    FParentMetric: mmToxicity),
    (FName: 'ToxicityIncCycloComp';  FDescription: strToxicityIncCycloComp;  FParentMetric: mmToxicity)
  );
  
  (** A constant array of default options for the Module Checks. **)
  ModuleChecks : Array[Low(TBADIModuleCheck)..High(TBADIModuleCheck)] Of TBADICheckRecord = (
    (
      FName: strHardCodedIntegerNoMetric;
      FCategory: strHardCodedIntegersCat;
      FMessage: strIntegerUsedInMsg;
      FDescription: strIntegerUsedInDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strHardCodedNumberNoMetric;
      FCategory: strHardCodedNumbersCat;
      FMessage: strNumberUsedInMsg;
      FDescription: strNumberUsedInDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strHardCodedStringNoMetric;
      FCategory: strHardCodedStringsCat;
      FMessage: strStringLiteralUsedInMsg;
      FDescription: strStringLiteralUsedInDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strUnsortedModuleNoMetric;
      FCategory: strUnsortedMethodsCat;
      FMessage: strMethodNotSortedMsg;
      FDescription: strMethodNotSortedDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strWithStatementNoMetric;
      FCategory: strUseOfWITHStmtCat;
      FMessage: strWITHUsedInMethodMsg;
      FDescription: strWITHUsedInMethodDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strGotoStatementNoMetric;
      FCategory: strUseOfGOTOStmtCat;
      FMessage: strGOTOUsedInMethodMsg;
      FDescription: strGOTOUsedInMethodDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyEXCEPTNoMetric;
      FCategory: strEmptyEXCEPTBlocksCat;
      FMessage: strEXCEPTClauseMethodEmptyMsg;
      FDescription: strEXCEPTClauseMethodEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyFINALLYNoMetric;
      FCategory: strEmptyFINALLYBlocksCat;
      FMessage: strFINALLYClauseMethodEmptyMsg;
      FDescription: strFINALLYClauseMethodEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strExceptionEatingNoMetric;
      FCategory: strEXCEPTIONEatingCat;
      FMessage: strONStmtCaptureAllExcepsMsg;
      FDescription: strONStmtCaptureAllExcepsDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyTHENNoMetric;
      FCategory: strEmptyTHENBlocksCat;
      FMessage: strTHENClauseInEmptyMsg;
      FDescription: strTHENClauseInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyELSENoMetric;
      FCategory: strEmptyELSEBlocksCat;
      FMessage: strELSEClauseInEmptyMsg;
      FDescription: strELSEClauseInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyCASENoMetric;
      FCategory: strEmptyCASEBlocksCat;
      FMessage: strCASEClauseInEmptyMsg;
      FDescription: strCASEClauseInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyFORNoMetric;
      FCategory: strEmptyFORBlocksCat;
      FMessage: strFORBlockInEmptyMsg;
      FDescription: strFORBlockInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyWHILENoMetric;
      FCategory: strEmptyWHILEBlocksCat;
      FMessage: strWHILEBlockInEmptyMsg;
      FDescription: strWHILEBlockInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyREPEATNoMetric;
      FCategory: strEmptyREPEATBlocksCat;
      FMessage: strREPEATBlockInEmptyMsg;
      FDescription: strREPEATBlockInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyBEGINENDNoMetric;
      FCategory: strEmptyBEGINENDBlocksCat;
      FMessage: strBEGINENDBlockInEmptyMsg;
      FDescription: strBEGINENDBlockInEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyInitializationNoMetric;
      FCategory: strEmptyInitializationBlockCat;
      FMessage: strINITIALIZATIONClauseInModuleEmptyMsg;
      FDescription: strINITIALIZATIONClauseInModuleEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyFinalizationNoMetric;
      FCategory: strEmptyFinalizationBlockCat;
      FMessage: strFINALIZATIONClauseInModuleEmptyMsg;
      FDescription: strFINALIZATIONClauseInModuleEmptyDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strEmptyMethodNoMetric;
      FCategory: strEmptyMethodsCat;
      FMessage: strMethodDoesNotHaveImplementationMsg;
      FDescription: strMethodDoesNotHaveImplementationDesc;
      FConflictType: dciMissing;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone),
    (
      FName: strMissingCONSTInParamNoMetric;
      FCategory: strMissingCONSTinParametersCat;
      FMessage: strParameterInMethodMissingCONSTMsg;
      FDescription: strParameterInMethodMissingCONSTDesc;
      FConflictType: dciItem;
      FEnabled: True;
      FLimit:  0.0;
      FLimitType: ltNone)
  );

  (** A constant array of information for the check sub-options. **)
  ModuleCheckSubOps : Array[Low(TBADIModuleCheckSubOp)..High(TBADIModuleCheckSubOp)] Of
    TBADICheckSubOpRecord = (
    (FName: 'HCIntIgnoreZero';        FDescription: strIgnoreHardCodedIntegerZerosCat;            FParentCheck: mcHardCodedIntegers),
    (FName: 'HCIntIgnoreOne';         FDescription: strIgnoreHardCodedIntegerOnesCat;             FParentCheck: mcHardCodedIntegers),
    (FName: 'HCIntIgnoreDIV2';        FDescription: strIgnoreHardCodedIntegerDIV2Cat;             FParentCheck: mcHardCodedIntegers),
    (FName: 'HCNumIgmoreZero';        FDescription: strIgnoreHardCodedNumberZerosCat;             FParentCheck: mcHardCodedNumbers),
    (FName: 'HCStrIgnoreEmpty';       FDescription: strIgnoreHardCodedEmptyStringsCat;            FParentCheck: mcHardCodedStrings),
    (FName: 'HCStrIgnoreSingle';      FDescription: strIgnoreHardCodedSingleCharStrCat;           FParentCheck: mcHardCodedStrings),
    (FName: 'MCParmListIgnoreEvents'; FDescription: strMissingCONSTInParamIgnoreEventHandlersCat; FParentCheck: mcMissingCONSTInParemterList)
  );

Implementation

End.
