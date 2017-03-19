(**

  This module contains a class to represent an event declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Mar 2017

**)
Unit BADI.VB.EventDecl;

Interface

Uses
  BADI.VB.MethodDecl;

Type
  (** A class to represent an event item. **)
  TEventDecl = Class(TVBMethod)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    //Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

End.
