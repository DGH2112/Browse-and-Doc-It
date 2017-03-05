(**

  This module contains a class to represent a DFM file item.

  @Author  David Hoyle
  @Version 1.0
  @Date    05 Mar 2017

**)
Unit BADI.DFM.Item;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

Type
  (** This class represent a DFM Item in the file. **)
  TDFMItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FItemName: String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName: String; Override;
  Public
    Constructor Create(Const strName: String; AScope: TScope; iLine,
      iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the item.

  @precon  None.
  @postcon Returns a string representation of the item.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDFMItem.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

Begin
  Result := 'Item';
End;

(**

  A constructor for the TDFMItem class.

  @precon  None.
  @postcon Creates a unique name for the item.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TDFMItem.Create(Const strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FItemName := Format('%s:%4.4d:%4.4d', [strName, iLine, iColumn]);
End;

(**

  This is an overridden GetName to provide a unqiue name for the item.

  @precon  None.
  @postcon Returns a unqiue name for the item.

  @return  a String

**)
Function TDFMItem.GetName: String;

Begin
  Result := FItemName;
End;

End.
