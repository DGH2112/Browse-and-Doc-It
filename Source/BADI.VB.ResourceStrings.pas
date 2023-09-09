(**

  This module contains a set of resource strings for use with the VB parser.

  @Author  David Hoyle
  @Version 1.020
  @Date    09 Sep 2023

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
  (** An exception message for when a line end token is expected. **)
  strLineEndExpected = 'Expected a line end token but ''%s'' found at line %d column %d.';
  (** A message prompt for returns on properties. **)
  strProperyRequiresReturn = 'Property ''%s'' requires a return parameter.';
  (** A message prompt for parameters in properties. **)
  strProperyRequireParam = 'Property ''%s'' requires at least 1 parameter.';
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
  strExceptionPushNameIncorrect = 'The name passed to the Exception.Push method (%s) is ' +
    'incorrect (''%s.%s'').';
  (** A warning message for missing push parameters. **)
  strExceptionPushParameter = 'The parameter ''%s'' in ''%s.%s'' does not have a corresponding ' +
    'parameter in the Exception.Push statement.';
  (** A warning message for push parameter out of order. **)
  strExceptionPushParamPos = 'The parameter ''%s'' in ''%s.%s'' is not in the ' +
    'the correct position (%d not %d) in the Exception.Push statement.';
  (** A warning message for push parameter count different. **)
  strExceptionPushParamCount = 'The function ''%s.%s'' has the wrong number of ' +
    'Exception.Push parameters (%d not %d).';
  (** A hint message for keyword GOTO found in the code. **)
  strKeywordGOTOFound = 'Keyword GOTO found in function ''%s'' at line %d column %d.';
  (** A label message for VB events. **)
  strEventsLabel = 'Events';
  (** This is an error message for duplicate identifiers. **)
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';

Implementation

End.
