(**

  This module contains a set of resource strings for use with the VB parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Mar 2017

**)
Unit BADI.VB.ResourceStrings;

Interface

ResourceString
  (** A label for versions. **)
  strVersionLabel = 'Version';
  (** A label for attributes **)
  strAttributesLabel = 'Attributes';
  (** A label for options. **)
  strOptionsLabel = 'Options';
  (** A label for implements. **)
  strImplementsLabel = 'Implements';
  (** A label for declared functions and procedures. **)
  strDeclaresLabel = 'Declarations';
  (** A label for implemented properties. **)
  strImplementedPropertiesLabel = 'Implemented Properties';

  (** Exception message when an value is expected but something else is found. **)
  strValueExpected = 'Value expected but ''%s'' found at line %d column %d.';
  (** An exception messahe for when a line end token is expected. **)
  strLineEndExpected = 'Expected a line end token but ''%s'' found at line %d column %d.';
  (** A message prompt for returns on properties. **)
  strProperyRequiresReturn = 'Propery ''%s'' requires a return parameter.';
  (** A message prompt for parameters in properties. **)
  strProperyRequireParam = 'Propery ''%s'' requires at least 1 parameter.';
  (** A warning message for no push method. **)
  strExceptionPush = 'The method ''%s'' has no Exception.Push method.';
  (** A warning message for no push name. **)
  strExceptionPushName = 'The method ''%s'' has no Exception.Push name.';
  (** A warning message for no pop method. **)
  strExceptionPop = 'The method ''%s'' has no Exception.Pop method.';
  (** A warning message for no error handling. **)
  strErrorHandling = 'The method ''%s'' has no error handling.';
  (** A warning message for an exit statement and error handling. **)
  strExitStatement = 'The method ''%s'' has an Exit statement which may be i' +
    'n conflict with the error handling.';
  (** A warning message for missing push names. **)
  strExceptionPushNameIncorrect = 'The name passed to the Exception.Push me' +
  'thod (%s) is incorrect (''%s.%s'').';
  (** A warning message for missing push parameters. **)
  strExceptionPushParameter = 'The parameter ''%s'' in ''%s.%s'' does not ha' +
  've a corresponding parameter in the Exception.Push statement.';
  (** A warning message for push parameter out of order. **)
  strExceptionPushParamPos = 'The parameter ''%s'' in ''%s.%s'' is not in the ' +
    'the correct position (%d not %d) in the Exception.Push statement.';
  (** A warning message for push parameter count different. **)
  strExceptionPushParamCount = 'The function ''%s.%s'' has the wrong number of ' +
    'Exception.Push parameters (%d not %d).';
  (** A hint message for keyword GOTO found in the code. **)
  strKeywordGOTOFound = 'Keyword GOTO found in function ''%s'' at line %d co' +
  'lumn %d.';
  (** A label mesaage for VB events. **)
  strEventsLabel = 'Events';

Implementation

End.
