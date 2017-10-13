(**

  This module contains a class to represent an Eidolon Association.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.Association;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer;

Type
  (** This is a class to represent an Association in a Requirements Table. **)
  TAssociation = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TAssociation.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
var
  iToken: Integer;
begin
  Result := Identifier;
  If TokenCount > 0 Then
    Begin
      Result := Result + '=';
      For iToken := 0 To TokenCount - 1 Do
        Result := Result + Tokens[iToken].Token;
    End;
end;

End.
