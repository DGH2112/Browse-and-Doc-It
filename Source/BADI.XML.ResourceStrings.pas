(**

  This module contains resource strings specific to the XML parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    11 Mar 2017

**)
Unit BADI.XML.ResourceStrings;

Interface

Resourcestring
  (** This is a resource string for the document node of the module explorer. **)
  strExpectedWord = 'Expected ''%s'' but found ''%s'' at line %d column %d.';
  (** This is a resource string for an expected version number not found. **)
  strExpectedVersionNum = 'Expected version number ''1.x'' but found ''%s'' ' +
    'at line %d column %d.';
  (** This is a resource string for an invalid version number. **)
  strIsNotAValidVersionNum = '''%s'' is not a valid version number at line %' + 'd column %d.';
  (** This is a resource string for expected whitespace. **)
  strExpectedWhitespace = 'Expected whitespace but found ''%s'' at line %d c' + 'olumn %d.';
  (** This is a resource string for an invalid PI target. **)
  strPITargetCanNotBeNamed = 'PI Target can not be named ''xml'' at line %d ' + 'column %d.';
  (** This is a resource string for an invalid content specification. **)
  strInvalidContentSpec = 'Invalid content specification at line %d column %' + 'd.';
  (** This is a resource string for an expected end tag. **)
  strExpectedEndTag = 'Expected end tag but found ''%s'' at line %d column %' + 'd.';
  (** This is a resource string for an expected end tag name. **)
  atrExpectedEndTagNamed = 'Expected end tag named ''%s'' but found ''%s'' a' +
    't line %d column %d.';
  (** This is a resource string for an invalid EncName **)
  strEncNameContainsInvalidChars = 'EncName ''%s''contains invalid character' +
    's at line %d column %d.';
  (** This is a resource string for an expected <Element> **)
  strExpectedElement = 'Expected <Element> but ''%s'' found at line %d colum' + 'n %d.';
  (** This is a resource string for an attribute appearing more than once. **)
  strAttributeCanNotAppear = 'Attribute ''%s'' can not appear more than once' +
    ' at line %d column %d.';
  (** This is a resource string for xhtml names must be lowercase. **)
  strHTMLElementLowercase = 'HTML element ''%s'' should be in lowercase at l' + 'ine %d column %d.';
  (** This is a resource string for an expected file end token. **)
  strExpectedFileEnd = 'Expected <FileEnd> but found ''%s'' at line %d colum' + 'n %d.';

Implementation

End.
