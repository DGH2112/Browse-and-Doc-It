(**

  This module contains a class to represent a Backus-Naur rule.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

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
    Function  AsString(Const boolShowIdenifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options;

(**

  This method returns a string representation of the BNG Rule.

  @precon  None.
  @postcon Returns a string representation of the BNG Rule.

  @param   boolShowIdenifier    as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TBackusNaurRule.AsString(Const boolShowIdenifier, boolForDocumentation: Boolean): String;

Const
  strRulePrefix = ' ::= ';

Begin
  Result := Name + strRulePrefix +
    BuildStringRepresentation(False, boolForDocumentation, Trim(strRulePrefix),
    BADIOptions.MaxDocOutputWidth, ['.', '+', '*'], ['.']);
End;

End.
