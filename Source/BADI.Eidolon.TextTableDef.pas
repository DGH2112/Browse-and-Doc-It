(**

  This module contains a class to represent an Eidolon Text Table Definition.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.TextTableDef;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer;

Type
  (** A class to represent a TextTable definition. **)
  TTextTableDef = Class(TElementContainer)
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Eidolon.Functions;

(**

  This method returns string representation of the Text Table Definition.

  @precon  None.
  @postcon Returns string representation of the Text Table Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TTextTableDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Identifier + '=';
  Result := Result + BuildLiteralString(Self);
End;

End.
