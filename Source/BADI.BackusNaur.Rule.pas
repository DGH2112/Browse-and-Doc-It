(**

  This module contains a class to represent a Backus-Naur rule.

  @Author  David Hoyle
  @Version 1.0
  @Date    05 Mar 2017

**)
Unit BADI.BackusNaur.Rule;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer;

Type
  (** This class represents the BNF rule found in the code. **)
  TBackusNaurRule = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function  AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the BNG Rule.

  @precon  None.
  @postcon Returns a string representation of the BNG Rule.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TBackusNaurRule.AsString(boolShowIdenifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Name + ' ::= ' +
    BuildStringRepresentation(False, boolForDocumentation, '::=',
      BrowseAndDocItOptions.MaxDocOutputWidth, ['.', '+', '*'], ['.']);
end;

End.
