(**

   This module contains a class to represent a VB constant declaration.

   @Author  David Hoyle
   @Version 1.0
   @Date    01 Apr 2017

 **)
Unit BADI.VB.ConstantDecl;

Interface

Uses
  BADI.Generic.Constant;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (** A class to represent constants in visual basic. **)
  TVBConstant = Class(TGenericConstant)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the visual basic constant.

  @precon  None .
  @postcon Returns a string representation of the visual basic constant .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TVBConstant.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, 'As',
    BADIOptions.MaxDocOutputWidth);
End;

End.
