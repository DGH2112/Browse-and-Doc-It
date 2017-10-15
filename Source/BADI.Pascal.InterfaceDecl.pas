(**

  This module contains a class which implements an Object Pascal specific Interface Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Pascal.InterfaceDecl;

Interface

Uses
  BADI.Pascal.ClassDecl;

Type
  (** This is a class the extends the class definition to handle an interface
  definition **)
  TInterfaceDecl = Class(TClassDecl)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FGUID: String;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      Returns the GUID for the interface.
      @precon  None.
      @postcon Returns the GUID for the interface.
      @return  a String
    **)
    Property GUID: String Read FGUID Write FGUID;
  End;

Implementation

uses
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the Interface declaration with the heritage.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TInterfaceDecl.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;
Var
  iToken: Integer;
Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
// If GenericParams <> Nil Then
// Result := Result + GenericParams.AsString;
  If Result <> '' Then
    Result := Result + #32'='#32;
  Result := Result + 'Interface';
  If Heritage.ElementCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 1 To Heritage.ElementCount Do
        Begin
          Result := Result + Heritage.Elements[iToken].AsString(boolShowIdentifier,
            boolForDocumentation);
          If iToken < Heritage.ElementCount Then
            Result := Result + ','#32;
        End;
      Result := Result + ')';
    End;
  If boolForDocumentation Then
    If FGUID <> '' Then
      Result := Result + #13#10 + FGUID;
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TInterfaceDecl.CheckDocumentation(Var boolCascade: Boolean);
Var
  i: Integer;

Begin
  If doShowUndocumentedInterfaces In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment, strInterfaceDocumentation,
        DocConflictTable[dctInterfaceClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

End.
