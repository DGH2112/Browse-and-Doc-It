(**

  This module contains a class which implements an Object Pascal specific Object Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Pascal.ObjectDecl;

Interface

Uses
  BADI.Pascal.RecordDecl,
  BADI.TokenInfo;

Type
  (** This is a class the extends the record definition to handle an object
  definition **)
  TObjectDecl = Class(TRecordDecl)
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    Function ReferenceSymbol(AToken: TTokenInfo): Boolean; Override;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.ElementContainer,
  SysUtils;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Output the name of the Object = '= Object (" HeritageList ")'

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TObjectDecl.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

Var
  iToken: Integer;

begin
  Result := Identifier + #32'='#32'Object';
  If Heritage.TokenCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 0 To Heritage.TokenCount - 1 Do
        Begin
          Result := Result + Heritage.Tokens[iToken].Token;
          If iToken < Heritage.TokenCount - 1  Then
            Result := Result + #32',';
        End;
      Result := Result + ')';
    End;
end;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TObjectDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedObjects In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strObjectDocumentation, DocConflictTable[dctObjectClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method references symbols with the scope of the object / class.

  @precon  None.
  @postcon References symbols with the scope of the object / class.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TObjectDecl.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Var
  i: Integer;
  boolFound: Boolean;
  MethodsLabel : TLabelContainer;

begin
  Result := Inherited ReferenceSymbol(AToken);
  If Result Then
    Exit;
  boolFound := False;
  MethodsLabel := FindElement(strMethodsLabel) As TLabelContainer;
  If MethodsLabel <> Nil Then
    Begin
      For i := 1 To Methodslabel.ElementCount Do
        If CompareText(AToken.Token, Methodslabel[i].Identifier) = 0 Then
          Begin
            Methodslabel[i].Referenced := True;
            AToken.Reference := trResolved;
            boolFound := True;
          End;
    End;
  Result := boolFound;
End;

End.
