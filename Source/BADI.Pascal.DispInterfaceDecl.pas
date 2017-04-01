(**

  This module contains a class which implements an Object Pascal specific DispInterface Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Pascal.DispInterfaceDecl;

Interface

Uses
  BADI.Pascal.InterfaceDecl;

Type
  (** This is a class the extends the class definition to handle an interface
  definition **)
  TDispInterfaceDecl = Class(TInterfaceDecl)
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the DispInterface declaration with the heritage.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDispInterfaceDecl.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
Var
  iToken: Integer;
Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'='#32;
  Result := Result + 'DispInterface';
  If Heritage.ElementCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 1 To Heritage.ElementCount Do
        Begin
          Result := Result + Heritage.Elements[iToken].AsString(boolShowIdentifier,
            boolForDocumentation);
          If iToken < Heritage.ElementCount Then
            Result := Result + #32',';
        End;
      Result := Result + ')';
    End;
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TDispInterfaceDecl.CheckDocumentation(Var boolCascade: Boolean);
Var
  i: Integer;

Begin
  If doShowUndocumentedInterfaces In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment, strDispInterfaceDocumentation,
        DocConflictTable[dctDispInterfaceClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

End.
