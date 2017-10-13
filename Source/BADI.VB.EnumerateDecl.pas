(**

  This module contains a class the presents a VB enumerate declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Oct 2017

**)
Unit BADI.VB.EnumerateDecl;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.TypeDecl;

Type
  (** A class to represent Enumerate Declarations **)
  TVBEnumerateDecl = Class(TGenericTypeDecl)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of the visual basic enumerate declaration.

  @precon  None .
  @postcon Returns a string representation of the visual basic enumerate declaration .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBEnumerateDecl.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Const
  strEnum = 'Enum';

Begin
  Result := strEnum;
  If boolShowIdentifier Then
    Result := Result + #32 + Identifier;
End;

End.
