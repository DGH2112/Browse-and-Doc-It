(**

  This module contains a class which implements an Object Pascal specific Type Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Pascal.TypeDecl;

Interface

Uses
  BADI.Generic.TypeDecl;

Type
  (** This is a sub class for general type types **)
  TTypes = Class(TGenericTypeDecl)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FIsTyped: Boolean;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      This property determines whether the type is typed or not.
      @precon  None.
      @postcon Gets or sets whether the type is typed (preceeded by TYPE);
      @return  a Boolean
    **)
    Property IsTyped: Boolean Read FIsTyped Write FIsTyped;
  End;

  (** This is a sub class for restricted type types **)
  TRestrictedType = Class(TTypes);

  (** This is a sub class for typeid types **)
  TTypeID = Class(TTypes);
  (** This is a sub class for Simple Type types **)
  TSimpleType = Class(TTypes);
  (** This is a sub class for Structured Type types **)
  TStrucType = Class(TTypes);
  (** This is a sub class for String types **)
  TStringType = Class(TTypes);
  (** This is a sub class for Procedure types **)
  TProcedureType = Class(TTypes);

(** This is a sub class for Anonymous Method types **)
  TAnonymousReferenceType = Class(TTypes)
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

(** This is a sub class for Variant types **)
  TVariantType = Class(TTypes);
  (** This is a sub class for Class Ref types **)
  TClassRefType = Class(TTypes);

  (** This is a sub class for Ordinal types **)
  TOrdinalType = Class(TSimpleType);
  (** This is a sub class for Real types **)
  TRealType = Class(TSimpleType);

  (** This is a sub class for the Real48 type. **)
  TReal48 = Class(TRealType);
  (** This is a sub class for the Real type. **)
  TReal = Class(TRealType);
  (** This is a sub class for the Single type. **)
  TSingle = Class(TRealType);
  (** This is a sub class for the Double type. **)
  TDouble = Class(TRealType);
  (** This is a sub class for the Extended type. **)
  TExtended = Class(TRealType);
  (** This is a sub class for the Currency type. **)
  TCurrency = Class(TRealType);
  (** This is a sub class for the Complex type. **)
  TComp = Class(TRealType);

  (** This is a sub class for the SubRange type. **)
  TSubRangeType = Class(TOrdinalType);
  (** This is a sub class for the Enumerate type. **)
  TEnumerateType = Class(TOrdinalType);
  (** This is a sub class for the OrdIdent type. **)
  TOrdIdent = Class(TOrdinalType);

  (** This is a sub class for the ShortInt type. **)
  TShortInt = Class(TOrdIdent);
  (** This is a sub class for the SmallInt type. **)
  TSmallInt = Class(TOrdIdent);
  (** This is a sub class for the Integer type. **)
  TInteger = Class(TOrdIdent);
  (** This is a sub class for the Byte type. **)
  TByte = Class(TOrdIdent);
  (** This is a sub class for the LongInt type. **)
  TLongInt = Class(TOrdIdent);
  (** This is a sub class for the Int64 type. **)
  TInt64 = Class(TOrdIdent);
  (** This is a sub class for the Word type. **)
  TWord = Class(TOrdIdent);
  (** This is a sub class for the Boolean type. **)
  TBoolean = Class(TOrdIdent);
  (** This is a sub class for the Char type. **)
  TChar = Class(TOrdIdent);
  (** This is a sub class for the WideChar type. **)
  TWideChar = Class(TOrdIdent);
  (** This is a sub class for the LongWord type. **)
  TLongWord = Class(TOrdIdent);
  (** This is a sub class for the PChar type. **)
  TPChar = Class(TOrdIdent);

  (** This is a sub class for the Variant type. **)
  TVariant = Class(TVariantType);
  (** This is a sub class for the OLEVariant type. **)
  TOLEVariant = Class(TVariantType);

  (** This is a sub class for the String type. **)
  TString = Class(TStringType);
  (** This is a sub class for the AnsiString type. **)
  TAnsiString = Class(TStringType);
  (** This is a sub class for the WideString type. **)
  TWideString = Class(TStringType);
  (** This is a sub class for the ShortString type. **)
  TShortString = Class(TStringType);

  (** This is a sub class for Array types **)
  TArrayType = Class(TStrucType)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FDimensions: Integer;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    Procedure AddDimension;
    (**
      This property defines the number of dmiensions that the array contains.
      @precon  None.
      @postcon Returns the number of dimension that the array contains.
      @return  an Integer
    **)
    Property Dimensions: Integer Read FDimensions;
  End;

  (** This is a sub class for Set types **)
  TSetType = Class(TStrucType);
  (** This is a sub class for File types **)
  TFileType = Class(TStrucType);
  (** This is a sub class for Pointer types **)
  TPointerType = Class(TTypes);

Implementation

Uses
  BADI.Options,
  BADI.Constants;

(**

  This method returns a string representation of the information in the class.

  @precon  None.
  @postcon A string representation of the class is returned.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TAnonymousReferenceType.AsString(boolShowIdentifier, boolForDocumentation
  : Boolean): String;

Begin
  Result := Identifier + ' = Reference To ' + Inherited AsString(False, boolForDocumentation);
End;

(**

  This method increments the internal count of the number of dimensions of the
  array.

  @precon  None.
  @postcon Increments the internal count of the number of dimensions of the
           array.

**)
Procedure TArrayType.AddDimension;

Begin
  Inc(FDimensions);
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns a string representation of the type .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TArrayType.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdentifier And (Identifier <> ''),
    boolForDocumentation, '=', BADIOptions.MaxDocOutputWidth);
End;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns a type formatted with an equals sign between the name and
           the definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TTypes.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Const
  (** A string represetnation of whether a type is typed. **)
  strIsTyped : Array[False..True] Of String = ('', ' Type');

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    '=' + strIsTyped[FIsTyped], BADIOptions.MaxDocOutputWidth,
    strNoSpaceBeforeSymbols - ['(']);
End;

End.
