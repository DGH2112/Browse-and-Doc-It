(**

  This module contains resource string for use by the Object Pascal parser and sub-classes.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.ResourceStrings;

Interface

Resourcestring
  (** This is an error message for rendering a temporay container - SHOULDN'T do this **)
  strTriedToRenderTmpCntr = 'Tried to Render a Temporary Container!';
  (** This is an error message for duplicate identifiers. **)
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';
  (** This is an error message for an invalid operator name. **)
  strInvalidOperator = 'Invalid operator ''%s'' at line %d column %d.';
  (** This is an error message for method which is not permissible in a record, object,
      class or interface **)
  strMethodNotPermitted = 'The method "%s" is not permitted in this context at line %d column %d.';
  (** This is a resorce string for anonymous methods container within methods. **)
  strAnonymousMethods = 'Anonymous Methods';

Implementation

End.
