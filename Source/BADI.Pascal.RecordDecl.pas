(**

  This module contains a class which implements an Object Pascal specific Record Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Oct 2017

**)
Unit BADI.Pascal.RecordDecl;

Interface

Uses
  BADI.Pascal.TypeDecl,
  BADI.ElementContainer,
  BADI.Pascal.IdentList,
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo;

Type
  (** This is a class that represents a record definition. **)
  TRecordDecl = Class(TRestrictedType)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPacked       : Boolean;
    FFieldsLabel  : TLabelContainer;
    FHeritage     : TIdentList;
    FHelper       : Boolean;
    FHelperClass  : String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const iImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Destructor Destroy; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function  AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function  ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    (**
      Returns whether the record is packed or not.
      @precon  None.
      @postcon Returns whether the record is packed or not.
      @return  a Boolean
    **)
    Property IsPacked : Boolean Read FPacked Write FPacked;
    (**
      This property caches the fields label IF it exists.
      @precon  None.
      @postcon Returns the reference to the fields label.
      @return  a TLabelContainer
    **)
    Property FieldsLabel : TLabelContainer Read FFieldsLabel Write FFieldsLabel;
    (**
      Returns a reference to the object class heritage.
      @precon  None.
      @postcon Returns a reference to the object class heritage.
      @return  a TIdentList
     **)
    Property Heritage : TIdentList Read FHeritage;
    (**
      This property gets or sets whether the class is a helper class.
      @precon  None.
      @postcon Gets or sets whether the class is a helper class.
      @return  a Boolean
    **)
    Property HelperClass : Boolean Read FHelper Write FHelper;
    (**
      This property gets or sets the class name of the class to be helped.
      @precon  None.
      @postcon Gets or sets the class name of the class to be helped.
      @return  a String
    **)
    Property HelperClassName : String Read FHelperClass Write FHelperClass;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the name of the record + '= Record '.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TRecordDecl.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

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
  Result := Result + 'Record';
  If FHelper Then
    Result := Result + #32'Helper';
  If Heritage.ElementCount > 0 Then
    Begin
      Result := Result + '(';
      For iToken := 1 To Heritage.ElementCount Do
        Begin
          Result := Result + Heritage.Elements[iToken].AsString(boolShowIdentifier,
            boolForDocumentation);
          If iToken < Heritage.ElementCount  Then
            Result := Result + ','#32;
        End;
      Result := Result + ')';
    End;
  If FHelper Then
    Result := Result + Format(' For %s', [FHelperClass]);
End;

(**

  This is the constructor method for the TRecordDecl class.

  @precon  None.
  @postcon Initialises the class.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   iImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TRecordDecl.Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const iImageIndex : TBADIImageIndex; Const AComment : TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, iImageIndex, AComment);
  FPacked := False;
  FHeritage := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
  FHeritage.Sorted := False;
end;

(**

  A destructor for the TRecordDecl class.

  @precon  None.
  @postcon Frees the generic parameters.

**)
Destructor TRecordDecl.Destroy;

Begin
  FHeritage.Free;
  Inherited Destroy;
End;

(**

  This method checks a record, object class or interface for documentation.

  @precon  C is a valid gerneic container to be check for class like
           documentation.
  @postcon Checks the passed class for documentation errors.

  @param   boolCascade as a Boolean as a reference

 **)
Procedure TRecordDecl.CheckDocumentation(var boolCascade : Boolean);
var
  i: Integer;

Begin
  If doShowUndocumentedRecords In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Self,
        strRecordDocumentation, DocConflictTable[dctRecordClauseUndocumented]);
  For i := 1 To ElementCount Do
    Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method searches for reference to the passed symbol in the records fields.

  @precon  None.
  @postcon Returns true if the symbol is found.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TRecordDecl.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

Begin
  Result := ReferenceSection(AToken, FFieldsLabel);
  If Not Result And (FindElement(strTypesLabel) <> Nil) Then
    Result := ReferenceSection(AToken, FindElement(strTypesLabel) As TLabelContainer);
  If Not Result And (FindElement(strVarsLabel) <> Nil) Then
    Result := ReferenceSection(AToken, FindElement(strVarsLabel) As TLabelContainer);
  If Not Result And (FindElement(strConstantsLabel) <> Nil) Then
    Result := ReferenceSection(AToken, FindElement(strConstantsLabel) As TLabelContainer);
  If Not Result And (FindElement(strClassVarsLabel) <> Nil) Then
    Result := ReferenceSection(AToken, FindElement(strClassVarsLabel) As TLabelContainer);
End;

End.
