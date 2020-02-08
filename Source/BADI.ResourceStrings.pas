(**

  This module contains all the resource string used in the Browse and Doc It application.

  @Author  David Hoyle
  @Version 1.121
  @Date    08 Feb 2020

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
Unit BADI.ResourceStrings;

Interface

ResourceString
  //
  // Options group descriptions
  //

  (** An option group description for general options. **)
  strOptionGroupGeneral = 'General';
  (** An option group description for Error options. **)
  strOptionGroupErrors = 'Errors';
  (** An option group description for Warnings options. **)
  strOptionGroupWarnings = 'Warnings';
  (** An option group description for Hint options. **)
  strOptionGroupHints = 'Hints';
  (** An option group description for Conflict options. **)
  strOptionGroupConflicts = 'Conflicts';
  (** An option group description for Check options. **)
  strOptionGroupChecks = 'Checks';
  (** An option group description for Metric options. **)
  strOptionGroupMetrics = 'Metrics';
  (** An option group description for Type options. **)
  strOptionGroupTypes = 'Types';
  (** An option group description for Module options. **)
  strOptionGroupModule = 'Module';
  (** An option group description for Method options. **)
  strOptionGroupMethods = 'Methods';
  (** An option group description for Property options. **)
  strOptionGroupProperties = 'Properties';
  (** An option group description for Inital/Finalization options. **)
  strOptionGroupInitializationFinalization = 'Initialization / Finalization';
  (** An option group description for miscellaneous options. **)
  strOptionGroupMiscellaneous = 'Miscellaneous';

  //
  // Documentation Option descriptions
  //
  
  (** Options text for Draw Syntax Highlighted Module Explorer **)
  strDrawSynHighModuleExplorer = 'Draw Syntax Highlighted Module Explorer';
  (** Options text for Show comments in the hints **)
  strShowCommentsInTheHints = 'Show comments in the hints';
  (** Options text for Showing Errors **)
  strShowErrors = 'Show Module Errors';
  (** Options text for Showing Warnings **)
  strShowWarnings = 'Show Module Warnings';
  (** Options text for Showing Hints **)
  strShowHints = 'Show Module Hints';
  (** Options text for Showing Documentation Conflicts **)
  strShowDocumentationConflicts = 'Show Documentation Conflicts';
  (** Options text for Showing Module Metrics. **)
  strShowModuleMetrics = 'Show Module Metrics';
  (** Options text for Showing Module Checks. **)
  strShowModuleChecks = 'Show Module Checks';
  (** Options text for showing child counts. **)
  strShowChildCountInTitles = 'Show Child Count in Titles';
  (** Options text for following the editor cursor. **)
  strFollowEditorCursor = 'Follow the Editor Cursor';
  (** Options text for automatically expanding errors. **)
  strExpandErrors = 'Automatically Expand Errors';
  (** Options text for automatically expanding warnings. **)
  strExpandWarnings = 'Automatically Expand Warnings';
  (** Options text for automatically expanding hints. **)
  strExpandHints = 'Automatically Expand Hints';
  (** Options text for automatically expanding documentation conflicts. **)
  strExpandDocConflicts = 'Automatically Expand Documentation Conflicts';
  (** Options text for automatically expanding modules checks. **)
  strExpandChecks = 'Automatically Expand Checks';
  (** Options text for automatically expanding modules metrics. **)
  strExpandMetrics = 'Automatically Expand Metrics';
  (** Options text for Syntax Highlight Errors **)
  strSyntaxHighlightErrors = 'Syntax Highlight Module Errors';
  (** Options text for Showing Error Icons in the Code Editor **)
  strShowErrorIconsInEditor = 'Show Error Icons in the Code Editor';
  (** Options text for Showing Error Messages in the Code Editor **)
  strShowErrorMsgsInEditor = 'Show Error Messages in the Code Editor';
  (** Options text for Syntax Highlight Warnings **)
  strSyntaxHighlightWarnings = 'Syntax Highlight Module Warnings';
  (** Options text for Showing Warning Icons in the Code Editor **)
  strShowWarningIconsInEditor = 'Show Warning Icons in the Code Editor';
  (** Options text for Showing Warning Messages in the Code Editor **)
  strShowWarningMsgsInEditor = 'Show Warning Messages in the Code Editor';
  (** Options text for Syntax Highlight Hints **)
  strSyntaxHighlightHints = 'Syntax Highlight Module Hints';
  (** Options text for Showing Hint Icons in the Code Editor **)
  strShowHintIconsInEditor = 'Show Hint Icons in the Code Editor';
  (** Options text for Showing Hint Messages in the Code Editor **)
  strShowHintMsgsInEditor = 'Show Hint Messages in the Code Editor';
  (** Options text for Syntax Highlight Documentation Conflicts **)
  strSyntaxHighlightDocumentationConflicts = 'Syntax Highlight Documentation Conflicts';
  (** Options text for Showing Conflict Icons in the Code Editor **)
  strShowConflictIconsInEditor = 'Show Documentation Conflicts Icons in the Code Editor';
  (** Options text for Showing Conflict Messages in the Code Editor **)
  strShowConflictMsgsInEditor = 'Show Documentation Conflicts Messages in the Code Editor';
  (** Options text for Syntax Highlight Checks **)
  strSyntaxHighlightChecks = 'Syntax Highlight Checks';
  (** Options text for Showing Check Icons in the Code Editor **)
  strShowCheckIconsInEditor = 'Show Check Icons in the Code Editor';
  (** Options text for Showing Check Messages in the Code Editor **)
  strShowCheckMsgsInEditor = 'Show Check Messages in the Code Editor';
  (** Options text for Syntax Highlight Metrics **)
  strSyntaxHighlightMetrics = 'Syntax Highlight Metrics';
  (** Options text for Showing Metric Icons in the Code Editor **)
  strShowMetricIconsInEditor = 'Show Metric Icons in the Code Editor';
  (** Options text for Showing Metric Messages in the Code Editor **)
  strShowMetricMsgsInEditor = 'Show Metric Messages in the Code Editor';
  (** Options text for auto hiding checks in the editoe views if they have no issues. **)
  strAutoHideChecksWithNoIssues = 'Auto Hide Checks with no issues in the Editor Views';
  (** Options text for auto hiding metrics in the editoe views if they have no issues. **)
  strAutoHideMetricsWithNoIssues = 'Auto Hide Metrics with no issues in the Editor Views';
  
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
  {: @todo strShowIDEErrorsOnSuccessfulParse = 'Show IDE Error messages if parser is successfully.';}
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

  //
  // Labels for the various objects in the module explorer
  //
  
  (** Errors label **)
  strErrors = 'Errors';
  (** Warnings label **)
  strWarnings = 'Warnings';
  (** Hints label **)
  strHints = 'Hints';
  (** Label for Documentation Conflicts **)
  strDocumentationConflicts = 'Documentation Conflicts';
  (** Label for Checks **)
  strChecks = 'Checks';
  (** Label for Metrics **)
  strMetrics = 'Metrics';
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

  //
  // Parser error messages.
  //

  (** An Exception message for a parsing being aborted. **)
  strParsingAborted = 'Parsing Aborted!';
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
  (** An error message for an unsatisfied declaration. **)
  strUnSatisfiedDeclaration = 'Method ''%s'' has an unsatisfied declaration.';
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

  //
  // Documentation Conflict Messages
  //

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
  (** Label for too many document conflicts. **)
  strTooManyConflicts = 'Too many documentation conflicts (%d)';
  (** Document conflicts message for too many documentation conflicts. **)
  strTooManyConflictsDesc = 'Browse and Doc It limits the number of conflicts that ' +
    'can be see at any time. This can be changed in the options.';

  (** An error message for trying to add one type of element but finding another
      with the same name. **)
  strTryingToAddType = 'Trying to add type ''%s'' but found type ''%s'' with the' +
  ' same name (%s).';
  (** An error message for trying to pop a compiler condition where there isnt one. **)
  strCannotPopCompilerCondition = 'Cannot pop the token position stack.';
  (** An error message for trying to peek a compiler condition where there isnt one. **)
  strCannotPeekTheCompilerCondition = 'Cannot peek the compiler condition stack.';

  (** An error message for a method with a missing parameter. **)
  strTheMethodImplementMissingParam = 'The method implementation on line %d at column %d is missing ' +
    'a parameter!';
  (** An error message for an assignment without a right parameter. **)
  strAssignmentMissing = 'Assignment missing at line %d column %d!';

  //
  // Token names
  //
  
  (** A text string to represents an Unknown token. **)
  strTokenUnknown = 'Unknown';
  (** A text string to represents a Whitespace token. **)
  strTokenWhiteSpace = 'White Space';
  (** A text string to represents a Reserved Word token. **)
  strTokenReservedWord = 'Reserved Word';
  (** A text string to represents an Identifer token. **)
  strTokenIdentifier = 'Identifier';
  (** A text string to represents a Number token. **)
  strTokenNumber = 'Number';
  (** A text string to represents a Symbol token. **)
  strTokenSymbol = 'Symbol';
  (** A text string to represents a Line End token. **)
  strTokenLineEnd = 'Line End';
  (** A text string to represents a Single Literal token. **)
  strTokenSingleLiteral = 'Single Literal';
  (** A text string to represents a Double Literal token. **)
  strTokenDoubleLiteral = 'Double Literal';
  (** A text string to represents a Line Comment token. **)
  strTokenLineComment = 'Line Comment';
  (** A text string to represents a Block Comment token. **)
  strTokenBlockComment = 'Block Comment';
  (** A text string to represents a HTML Start Tag token. **)
  strTokenHTMLStartTag = 'HTML Start Tag';
  (** A text string to represents a HTML End Tag token. **)
  strTokenHTMLEndTag = 'HTML End Tag';
  (** A text string to represents a Directive token. **)
  strTokenDirective = 'Directive';
  (** A text string to represents a Compiler Directive token. **)
  strTokenCompilerDirective = 'Compiler Directive';
  (** A text string to represents a Link Tag token. **)
  strTokenLinkTag = 'Link Tag';
  (** A text string to represents a Tree Header token. **)
  strTokenTreeHeader = 'Tree Header';
  (** A text string to represents a File End token. **)
  strTokenFileEnd = 'File End';
  (** A text string to represents a Line Continuation token. **)
  strTokenLineContinuation = 'Line Continuation';
  (** A text string to represents a Custom User Token token. **)
  strTokenCustomUserToken = 'Custom User Token';
  (** A text string to represents a Explorer Highlight token. **)
  strTokenExplorerHighlight = 'Explorer Highlight';
  (** A text string to represents a Plain Text token. **)
  strTokenPlainText = 'Plain Text';
  (** A text string to represents a Comment Text token. **)
  strTokenCommentText = 'Comment Text';
  (** A text string to represents a Tag Header Text token. **)
  strTokenTagHeaderText = 'Tag Header Text';
  (** A text string to represents a Tag Text token. **)
  strTokenTagText = 'Tag Text';
  (** A text string to represent the search highlight colour. **)
  strSearchHighlight = 'Search Highlight';
  (** A text string to represent the Line highlight colour. **)
  strLineHighlight = 'Line Highlight';
  (** A text string to represent the Doc Issue Editor colour. **)
  strDocIssueEditorText = 'Doc Issue Editor Text';

  //
  // Token Sets
  //
  (** A resource string for an unthemed IDE. **)
  strUnthemed = 'Unthemed';
  (** A resource string for an light themed IDE. **)
  strLightTheme = 'Light Theme';
  (** A resource string for an dark themed IDE. **)
  strDarkTheme = 'Dark Theme';
  (** A resource string for an custom themed IDE. **)
  strCustomTheme = 'Custom Theme';

  //
  // Menu captions
  //

  (** A label for the Module Explorer Menu. **)
  strMenuModuleExplorer = 'Module &Explorer';
  (** A label for the Documentation Menu. **)
  strMenuDocumentation = '&Documentation...';
  (** A label for the DUnit Menu. **)
  strMenuDUnit = 'D&Unit...';
  (** A label for the Profiling Menu. **)
  strMenuProfiling = 'Pro&filing...';
  (** A label for the Separator Menu. **)
  strMenuSep = '';
  (** A label for the Focus Editor Menu. **)
  strMenuFocusEditor = 'Focus Edi&tor';
  (** A label for the Method Comment Menu. **)
  strMenuMethodComment = '&Method Comment';
  (** A label for the Property Comment Menu. **)
  strMenuPropertyComment = '&Property Comment';
  (** A label for the Block Comment Menu. **)
  strMenuBlockComment = 'Block &Comment';
  (** A label for the Line COmment Menu. **)
  strMenuLineComment = '&Line Comment';
  (** A label for the InSitu Comment Menu. **)
  strMenuInSituComment = '&In-Situ Comment';
  (** A label for the ToDoComment Menu. **)
  strMenuToDoComment = '&ToDo Comment';
  (** A label for the Refactor Constant Menu. **)
  strMenuRefactorConstant = '&Refactor Constant...';
  (** A label for the module metrics. **)
  strMenuMetrics = 'Module &Metrics...';
  (** A label for the module checks. **)
  strMenuChecks = 'Module &Checks...';
  (** A label for the Options Menu. **)
  strMenuOptions = '&Options...';

  //
  // Special Tag Properties
  //

  (** A description for a tag to be shown in the module explorer. **)
  strTagPropShowInExpl = 'Show the Tag in the Module Explorer';
  (** A description for a tag to be expanded in the module explorer. **)
  strTagPropExpand = 'Auto Expand the Tag in Module Explorer';
  (** A description for a tag to be shown in the documentation. **)
  strTagPropShowInDocs = 'Show the Tag in Documentation';
  (** A description for a tag to use a fixed font. **)
  strTagPropFixedFont = 'Fixed Font Tag (preserves LF/CR and Indents)';
  (** A description for a tag to use syntax highlighting. **)
  strTagPropSyntax = 'Syntax Highlight the Tag';

  //
  // Metrics
  //
  
  (** A resource string for the Long Method Category. **)
  strLongMethodImplementationsCat = 'Long Method Implementations';
  (** A resource string for the Long Parameter list Category. **)
  strLongMethodParameterListsCat = 'Long Method Parameter Lists';
  (** A resource string for the Long Variable Category. **)
  strLongMethodVariableListsCat = 'Long Method Variable Lists';
  (** A resource string for the Nested IF Depth Category. **)
  strNestedIFDepthCat = 'Nested IF Depths';
  (** A resource string for the Method Cyclometric Complexity Category. **)
  strMethodCyclometricComplexityCat = 'Method Cylometric Complexity';
  (** A resource string for the Method Toxicity Category. **)
  strMethodToxicityCat = 'Method Toxicity';

  (** A resource string for the Including IF Statements in the cyclometric complexity calc. **)
  strMethodCCIncIF= 'Include IF Statements in the Cyclometric Complexity Calculation';
  (** A resource string for the Including CASE Statements in the cyclometric complexity calc. **)
  strMethodCCIncCASE = 'Include CASE Statements in the Cyclometric Complexity Calculation';
  (** A resource string for the Including WHILE Statements in the cyclometric complexity calc. **)
  strMethodCCIncWHILE = 'Include WHILE Statements in the Cyclometric Complexity Calculation';
  (** A resource string for the Including REPEAT Statements in the cyclometric complexity calc. **)
  strMethodCCIncREPEAT = 'Include REPEAT Statements in the Cyclometric Complexity Calculation';
  (** A resource string for the Ignore Boolean Expressions Category. **)
  strMethodCCSubExprCat = 'Include Boolean Sub-Expressions';
  (** A resource string for the Including Method Length in toxicity calc. **)
  strToxicityIncMethodLen = 'Include Method Length in the Toxicity Calculation';
  (** A resource string for the Including Parameter list Length in toxicity calc. **)
  strToxicityIncParamLen = 'Include Parameter List Length in the Toxicity Calculation';
  (** A resource string for the Including variable list Length in toxicity calc. **)
  strToxicityIncVarLen = 'Include Variabe List Length in the Toxicity Calculation';
  (** A resource string for the Including nested IF depth in toxicity calc. **)
  strToxicityIncIFDepth = 'Include Nested IF Depth in the Toxicity Calculation';
  (** A resource string for the Including cyclometric complexity in toxicity calc. **)
  strToxicityIncCycloComp = 'Include Cyclometric Complexity in the Toxicity Calculation';

  (** A message for a method with a long implementation. **)
  strMethodTooLongMsg = 'The method "%s" is too long (%1.0f/%1.0f)!';
  (** A description for a method with a long implementation. **)
  strMethodTooLongDesc = 'The implementation of the method is too long. This can make the code more ' +
    'difficult to maintain. Consider refactoring the code into smaller elements.';
  (** A message for a method with too many parameters. **)
  strMethodHasTooManyParamsMsg = 'The method "%s" has too many parameters (%1.0f/%1.0f)!';
  (** A description for a method with too many parameters. **)
  strMethodHasTooManyParamsDesc = 'The method has too many parameters and this can slow down the speed of the call to this method due to the size of the stack frame that is required. Consider refactoring the code to reduce the number of parameters pass.';
  (** A message for a method with too many variables. **)
  strMethodHasLongVarListMsg = 'The method "%s" has a long variable list (%1.0f/%1.0f)!';
  (** A description for a method with too many variables. **)
  strMethodHasLongVarListDesc = 'The method has too many variables and this can slow down the speed of the call to this method due to the size of the stack frame that is required. Consider refactoring the code to reduce the number of parameters pass.';
  (** A message for a method with a high IF Depth. **)
  strMethodHasHighIFDepthMsg = 'The method "%s" has a high IF depth (%1.0f/%1.0f).';
  (** A description for a method with a high IF Depth. **)
  strMethodHasHighIFDepthDesc = 'A high depth of IF statements in a method can make the method hard ' +
    'to maintain. Consider refactoring the code into smaller parts.';
  (** A message for a method with a high cyclometric complexity. **)
  strMethodHasHighCyclometricComplexityMsg = 'The method "%s" has a high cyclometric complexity ' +
    '(%1.0f/%1.0f).';
  (** A descrpition for a method with a high cyclometric complexity. **)
  strMethodHasHighCyclometricComplexityDesc = 'The cyclometric complexity of a method is a measure of ' +
    'its number of branch points and therefore the number of unit test that could be neede to test ' +
    'the code. If this value is too high it might make your ability to test your code more difficult.';
  (** A message for a method with a high toxicity. **)
  strMethodHasHighToxocityValueMsg = 'The method "%s" has a high toxocity value (%1.3f/%1.3f).';
  (** A description with a high toxicity. **)
  strMethodHasHighToxocityValueDesc = 'The toxicity of a method is a measure of the overall ' +
    'combination of metrics and is an indication of whether the method should be refactored into ' +
    'smaller parts.';

  //
  // Checks
  //
  
  (** A resource string for the Hard Coded Integers Category. **)
  strHardCodedIntegersCat = 'Hard Coded Integers';
  (** A resource string for the Hard Code Numbers Category. **)
  strHardCodedNumbersCat = 'Hard Coded Numbers';
  (** A resource string for the Hard Coded Strings Category. **)
  strHardCodedStringsCat = 'Hard Coded Strings';
  (** A resource string for the Unsorted Method Category. **)
  strUnsortedMethodsCat = 'Unsorted Methods';
  (** A resource string for the With Statement Category. **)
  strUseOfWITHStmtCat = 'Use of WITH statements';
  (** A resource string for the GOT Statements Category. **)
  strUseOfGOTOStmtCat = 'Use of GOTO statements';
  (** A resource string for the Empty EXCEPT Blocks Category. **)
  strEmptyExceptBlocksCat = 'Empty EXCEPT Blocks';
  (** A resource string for the Empty Finally BLocks Category. **)
  strEmptyFinallyBlocksCat = 'Empty FINALLY Blocks';
  (** A resource string for the Exception Eating Category. **)
  strExceptionEatingCat = 'Exception Eating';
  (** A resource string for the Empty THEN Blocks Category. **)
  strEmptyThenBlocksCat = 'Empty THEN Blocks';
  (** A resource string for the Empty ELSE Blocks Category. **)
  strEmptyElseBlocksCat = 'Empty ELSE Blocks';
  (** A resource string for the Empty Case Blocks Category. **)
  strEmptyCaseBlocksCat = 'Empty CASE Blocks';
  (** A resource string for the Empty For BLocks Category. **)
  strEmptyForBlocksCat = 'Empty FOR Blocks';
  (** A resource string for the Empty While BLocks Category. **)
  strEmptyWhileBlocksCat = 'Empty WHILE Blocks';
  (** A resource string for the Empty Repat BLocks Category. **)
  strEmptyRepeatBlocksCat = 'Empty REPEAT Blocks';
  (** A resource string for the Empty BEGIN END Blocks Category. **)
  strEmptyBeginEndBlocksCat = 'Empty BEGIN END Blocks';
  (** A resource string for the Empty Initialization BLocks Category. **)
  strEmptyInitializationBlockCat = 'Empty Initialization';
  (** A resource string for the Empty Finalization BLocks Category. **)
  strEmptyFinalizationBlockCat = 'Empty FInalization';
  (** A resource string for the Empty Methods Category. **)
  strEmptyMethodsCat = 'Empty Methods';
  (** A resource string for the Missing CONST in Parameters Category. **)
  strMissingCONSTInParametersCat = 'Missing CONST in Method Parameters';

  (** A resource string for the Hard Coded Zeros Category. **)
  strIgnoreHardCodedIntegerZerosCat = 'Ignore Hard Coded 0s';
  (** A resource string for the Hard Coded 1s Category. **)
  strIgnoreHardCodedIntegerOnesCat = 'Ignore Hard Coded 1s';
  (** A resource string for the Hard Coded DIV 2 Category. **)
  strIgnoreHardCodedIntegerDIV2Cat = 'Ignore Hard Coded DIV 2s';
  (** A resource string for the Hard Coded 0.0s Category. **)
  strIgnoreHardCodedNumberZerosCat = 'Ignore Hard Coded 0.0s';
  (** A resource string for the Hard Coded Empty Strings Category. **)
  strIgnoreHardCodedEmptyStringsCat = 'Ignore Hard Coded Empty Strings';
  (** A resource string for the Hard Coded Single Char Strings Category. **)
  strIgnoreHardCodedSingleCharStrCat = 'Ignore Hard Coded Single Character Strings';
  (** A resource string for the Ignore Event Handler Category. **)
  strMissingCONSTInParamIgnoreEventHandlersCat = 'Ignore Event Handlers';

  (** A message for the use of a hard coded integer in an implementation. **)
  strIntegerUsedInMsg = 'The integer %s is used in the %s "%s"!';
  (** A description for the use of a hard coded integer in an implementation. **)
  strIntegerUsedInDesc = 'Integers embedded in code can make maintaining code hard if the string should be used somewhere else. Consider creating a constant.';
  (** A message for the use of a hard coded number in an implementation. **)
  strNumberUsedInMsg = 'The number %s is used in the %s "%s"!';
  (** A description for the use of a hard coded number in an implementation. **)
  strNumberUsedInDesc = 'Numbers embedded in code can make maintaining code hard if the string should be used somewhere else. Consider creating a constant.';
  (** A message for the use of a hard coded string in an implementation. **)
  strStringLiteralUsedInMsg = 'The string literal %s is used in the %s "%s"!';
  (** A description for the use of a hard coded string in an implementation. **)
  strStringLiteralUsedInDesc = 'String literal embedded in code can make maintaining code hard if the string should be used somewhere else. Consider creating a constant or resource string.';
  (** A message for the use of an unsorted method implementation. **)
  strMethodNotSortedMsg = 'The method "%s" is not in the correct sort position in the source code!';
  (** A description for the use of an unsorted method implementation. **)
  strMethodNotSortedDesc = 'Sorting method in the source code can help navigation when looking through the code.';
  (** A message for the use of a WITH statement in an implementation. **)
  strWITHUsedInMethodMsg = 'The "WITH" is used in the method "%s"!';
  (** A description for the use of a WITH statement in an implementation. **)
  strWITHUsedInMethodDesc = 'WITH statements can lead to scope clashes. Consider refactoring the code to remove their use.';
  (** A message for the use of a GOTO statement in an implementation. **)
  strGOTOUsedInMethodMsg = 'The "GOTO" is used in the method "%s"!';
  (** A description for the use of a GOTO statement in an implementation. **)
  strGOTOUsedInMethodDesc = 'GOTO statments are considered bad programming practice especially with structured programming. Consider refactoring your code to replace their use with BREAK and CONTINUE.';
  (** A message for an empty EXCEPT block in an implementation. **)
  strEXCEPTClauseMethodEmptyMsg = 'The EXCEPT clause of the method "%s" is empty!';
  (** A description for an empty EXCEPT block in an implementation. **)
  strEXCEPTClauseMethodEmptyDesc = 'An empty EXCEPT clause will hide exception that are raised in ' + 
    'the preceeding code. Add code to handle the exception or remove the TRY EXCEPT END block.';
  (** A message for an empty FINALLY block in an implementation. **)
  strFINALLYClauseMethodEmptyMsg = 'The FINALLY clause of the method "%s" is empty!';
  (** A description for an empty FINALLY block in an implementation. **)
  strFINALLYClauseMethodEmptyDesc = 'An empty FINALLY clause provide no benefit or you have ' + 
    'forgotted to free a resource. Add code to handle the resource or remove the TRY FINALLY END ' + 
    'block.';
  (** A message for an exception eating implementation. **)
  strONStmtCaptureAllExcepsMsg = 'The ON statement in %s "%s" will capture all exceptions.';
  (** A description for an exception eating implementation. **)
  strONStmtCaptureAllExcepsDesc = 'Handling all exceptions raised by a block of code could mask an ' + 
    'underlying error should you not re-raise any errors you do not handle.';
  (** A message for an empty THEN block in an implementation. **)
  strTHENClauseInEmptyMsg = 'The THEN clause in %s "%s" is empty!';
  (** A description for an empty THEN block in an implementation. **)
  strTHENClauseInEmptyDesc = 'The conditional block does not execute any code. Check for coding ' + 
    'errors!';
  (** A message for an empty ELSE block in an implementation. **)
  strELSEClauseInEmptyMsg = 'The ELSE clause in %s "%s" is empty!';
  (** A description for an empty ELSE block in an implementation. **)
  strELSEClauseInEmptyDesc = 'The conditional block does not execute any code. Check for coding ' + 
    'errors!';
  (** A message for an empty CASE block in an implementation. **)
  strCASEClauseInEmptyMsg = 'The CASE clause in %s "%s" is empty!';
  (** A description for an empty CASE block in an implementation. **)
  strCASEClauseInEmptyDesc = 'The conditional block does not execute any code. Check for coding ' + 
    'errors!';
  (** A message for an empty FOR block in an implementation. **)
  strFORBlockInEmptyMsg = 'The FOR block in %s "%s" is empty!';
  (** A description for an empty FOR block in an implementation. **)
  strFORBlockInEmptyDesc = 'The conditional block does not execute any code. Check for coding errors!';
  (** A message for an empty WHILE block in an implementation. **)
  strWHILEBlockInEmptyMsg = 'The WHILE block in %s "%s" is empty!';
  (** A description for an empty WHILE block in an implementation. **)
  strWHILEBlockInEmptyDesc = 'The conditional block does not execute any code. Check for coding ' + 
    'errors!';
  (** A message for an empty REPEAT block in an implementation. **)
  strREPEATBlockInEmptyMsg = 'The REPEAT block in %s "%s" is empty!';
  (** A description for an empty REPEAT block in an implementation. **)
  strREPEATBlockInEmptyDesc = 'The conditional block does not execute any code. Check for coding ' + 
    'errors!';
  (** A message for an empty BEGIN END block in an implementation. **)
  strBEGINENDBlockInEmptyMsg = 'The BEGIN END block in %s "%s" is empty!';
  (** A description for an empty BEGIN END block in an implementation. **)
  strBEGINENDBlockInEmptyDesc = 'The conditional block does not execute any code. Check for coding ' + 
    'errors!';
  (** A message for an empty INITIALIZATION block in an implementation. **)
  strINITIALIZATIONClauseInModuleEmptyMsg = 'The INITIALIZATION clause in the module is empty!';
  (** A description for an empty INITIALIZATION block in an implementation. **)
  strINITIALIZATIONClauseInModuleEmptyDesc = 'If the initializatino cause is empty either you do not ' + 
    'need it or perhaps you have forgotten to implement some initalisation code.';
  (** A message for an empty FINALIZATION block in an implementation. **)
  strFINALIZATIONClauseInModuleEmptyMsg = 'The FINALIZATION clause in the module is empty!';
  (** A description for an empty FINALIZATION block in an implementation. **)
  strFINALIZATIONClauseInModuleEmptyDesc = 'If the finalizatino cause is empty either you do not ' + 
    'need it or perhaps you have forgotten to implement some finalisation code.';
  (** A message for a  method with an empty implementation. **)
  strMethodDoesNotHaveImplementationMsg = 'The method "%s" does not have an implementation!';
  (** A description for a  method with an empty implementation. **)
  strMethodDoesNotHaveImplementationDesc = 'The method does not have an implementation. You may not ' + 
    'need the method and therefore it could be removed.';
  (** A message for a  method with an parameters whic are not VAR or CONST. **)
  strParameterInMethodMissingCONSTMsg = 'The parameter "%s" in method "%s" is missing CONST!';
  (** A description for a  method with an parameters whic are not VAR or CONST. **)
  strParameterInMethodMissingCONSTDesc = 'The use of CONST parameters is preferred to prevent ' + 
    'parameters from being modified. it also allow the compiler to optimism the passed data.';

  
Implementation

End.
