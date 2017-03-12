(**

  This module contains a class to represent an Eidolon Rectangle Time Location Symbol.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.Rectangle;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.CustomFillSymbol;

Type
  (** A class to represent a RECTANGLE time location symbol **)
  TRectangle = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TRectangle.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
Begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
End;

End.
