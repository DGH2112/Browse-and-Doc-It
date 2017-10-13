(**

   This module contains a class to represent a VB constant declaration.

   @Author  David Hoyle
   @Version 1.0
   @Date    12 Oct 2017

 **)
Unit BADI.VB.ConstantDecl;

Interface

Uses
  BADI.Generic.Constant;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (** A class to represent constants in visual basic. **)
  TVBConstant = Class(TGenericConstant)
  Strict Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the visual basic constant.

  @precon  None .
  @postcon Returns a string representation of the visual basic constant .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBConstant.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Begin
  If (TokenCount > 0) And (Tokens[0].Token = '=') Then
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '',
      BADIOptions.MaxDocOutputWidth)
  Else
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '',
      BADIOptions.MaxDocOutputWidth);
End;

End.
