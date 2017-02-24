(**

  This module contains types specific to the Object Pascal Parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

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
