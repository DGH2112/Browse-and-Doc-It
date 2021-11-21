(**

  This module contains types specific to the Object Pascal Parser.

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
Unit BADI.Pascal.Types;

Interface

Uses
  BADI.Types,
  BADI.Comment,
  BADI.ElementContainer;

Type
  (** This is an enumerate to describe the type of constant expression. **)
  TPascalExprType = (petUnknown, petConstExpr, petString, petInteger, petFloat);
  (** This is a set of TExprType enumerates. **)
  TPascalExprTypes = Set of TPascalExprType;

  (** This is a record to represent the token information for type declaration. **)
  TTypeToken = Record
    FIdentifier  : String;
    FLine        : Integer;
    FColumn      : Integer;
    FScope       : TScope;
    FComment     : TComment;
    FContainer   : TElementContainer;
  End;

  (** An enumerate for the types of reference changes that can be done. **)
  TRefType = (rtFields, rtVariables, rtConstants, rtResourceStrings, rtTypes, rtClassVars);
  (** A set of reference checks that need to be undertaken. **)
  TRefTypes = Set of TRefType;

  (** This is a set of permissible method which are allowed on a record, object, class,
      or interface. **)
  TPermissibleMethods = Set Of TMethodType;

Implementation

End.
