(**

  This module contains a class to represent an Eidolon Connection Definition.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.ConnectionDef;

Interface

{$INCLUDE CompilerDefinitions.inc}


Uses
  BADI.Eidolon.DBConnection;

Type
  (** A class to represent a connection definition. **)
  TConnectionDef = Class(TDBConnection)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

uses
  BADI.Eidolon.Functions;

(**

  This method returns string representation of the Connection Definition.

  @precon  None.
  @postcon Returns string representation of the Connection Definition.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TConnectionDef.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier + '=';
  Result:= Result + BuildLiteralString(Self);
end;

End.
