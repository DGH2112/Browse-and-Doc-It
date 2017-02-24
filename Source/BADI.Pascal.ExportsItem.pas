(**

  This module contains a class which implements an Object Pascal specific Exports Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.ExportsItem;

Interface

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

Type
  (** This class represents an exported method. **)
  TExportsItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FResolved: Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
      AImageIndex: TBADIImageIndex; AComment: TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      This property gets and sets whether the symbol is resolved.
      @precon  None.
      @postcon Gets and sets whether the symbol is resolved.
      @return  a Boolean
    **)
    Property Resolved: Boolean Read FResolved Write FResolved;
  End;

Implementation

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the Exported item declaration .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TExportsItem.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Var
  iToken: Integer;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  For iToken := 0 To TokenCount - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + Tokens[iToken].Token;
    End;
End;

(**

  This is a constructor for the TExportsItem class.

  @precon  None.
  @postcon Initialises the class.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TExportsItem.Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FResolved := False;
End;

End.
