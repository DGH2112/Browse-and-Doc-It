(**

  This module contains functions for use with the Eidolon module patser.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.Functions;

Interface

Uses
  BADI.ElementContainer;

  Function BuildLiteralString(Element : TElementContainer) : String;

Implementation

(**

  This function returns a literal representation of the passed element.

  @precon  Element must be a valid instance of a TElementContainer.
  @postcon Returns a literal representation of the passed element.

  @param   Element as a TElementContainer
  @return  a String

**)
Function BuildLiteralString(Element : TElementContainer) : String;

Var
  i : Integer;

Begin
  Result := '';
  For i := 0 To Element.TokenCount - 1 Do
    Result := Result + Element.Tokens[i].Token;
End;

End.
