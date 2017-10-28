(**

  This module contains a class which implements an Object Pascal specific Class Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Oct 2017

**)
Unit BADI.Pascal.ClassDecl;

Interface

Uses
  BADI.Pascal.ObjectDecl,
  BADI.TokenInfo;

Type
  (** This is a class the extends the object definition to handle an class
  definition **)
  TClassDecl = Class(TObjectDecl)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FAbstractClass: Boolean;
    FSealedClass: Boolean;
    FHelper: Boolean;
    FHelperClass: String;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    Function ReferenceSymbol(Const AToken: TTokenInfo): Boolean; Override;
    (**
      This property defined whether the class is abstract or not.
      @precon  None.
      @postcon None.
      @return  a Boolean
    **)
    Property AbstractClass: Boolean Read FAbstractClass Write FAbstractClass;
    (**
      This property defines whether the class is sealed or not.
      @precon  None.
      @postcon None.
      @return  a Boolean
    **)
    Property SealedClass: Boolean Read FSealedClass Write FSealedClass;
    (**
      This property gets or sets whether the class is a helper class.
      @precon  None.
      @postcon Gets or sets whether the class is a helper class.
      @return  a Boolean
    **)
    Property HelperClass: Boolean Read FHelper Write FHelper;
    (**
      This property gets or sets the class name of the class to be helped.
      @precon  None.
      @postcon Gets or sets the class name of the class to be helped.
      @return  a String
    **)
    Property HelperClassName: String Read FHelperClass Write FHelperClass;
  End;

Implementation

uses
  SysUtils,
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.ElementContainer;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Output the name of the Class = '= Class (" HeritageList ")'

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TClassDecl.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Var
  iToken: Integer;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'='#32;
  If IsTyped Then
    Result := Result + 'Type ';
  Result := Result + 'Class';
  If FAbstractClass Then
    Result := Result + #32'Abstract';
  If FSealedClass Then
    Result := Result + #32'Sealed';
  If FHelper Then
    Result := Result + #32'Helper';
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
  If FHelper Then
    Result := Result + Format(' For %s', [FHelperClass]);
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TClassDecl.CheckDocumentation(Var boolCascade: Boolean);
Var
  i: Integer;

Begin
  If doShowUndocumentedClasses In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Self, strClassDocumentation,
        DocConflictTable[dctClassClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method searches for reference to trhe passed symbol in the classes various section.

  @precon  None.
  @postcon Returns true if the symbol is found.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TClassDecl.ReferenceSymbol(Const AToken: TTokenInfo): Boolean;

Begin
  Result := Inherited ReferenceSymbol(AToken);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strVarsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strConstantsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strTypesLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strClassVarsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strPropertiesLabel) As TLabelContainer);
End;

End.
