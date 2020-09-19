(**

  This module contains resource string for use by the Object Pascal parser and sub-classes.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

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
Unit BADI.Pascal.ResourceStrings;

Interface

Resourcestring
  (** This is an error message for rendering a temporary container - SHOULDN`T do this **)
  strTriedToRenderTmpCntr = 'Tried to Render a Temporary Container!';
  (** This is an error message for duplicate identifiers. **)
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';
  (** This is an error message for an invalid operator name. **)
  strInvalidOperator = 'Invalid operator ''%s'' at line %d column %d.';
  (** This is an error message for method which is not permissible in a record, object,
      class or interface **)
  strMethodNotPermitted = 'The method "%s" is not permitted in this context at line %d column %d.';
  (** This is a resource string for anonymous methods container within methods. **)
  strAnonymousMethods = 'Anonymous Methods';

Implementation

End.
