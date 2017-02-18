(**

  This module contains all the resource string used in the Browse and Doc It application.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.ResourceStrings;

Interface

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
  (** Label for too many document conflicts. **)
  strTooManyConflicts = 'Too many documentation conflicts';
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

Implementation

End.
