(**

  This module contains a class to represent an Eidolon Diamond Time Location Symbol.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.Diamond;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.CustomFillSymbol,
  BADI.Types,
  BADI.Comment;

Type
  (** A class to represent a DIAMOND time location symbol **)
  TDiamond = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDiamondSize : Integer;
  Public
    Constructor Create(const strName: String; AScope : TScope; iLine, iColumn : Integer;
      AImageIndex : TBADIImageIndex; AComment: TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a Integer
    **)
    Property DiamondSize : Integer Read FDiamondSize Write FDiamondSize;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDiamond.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + Format('%d', [FDiamondSize]);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

(**

  A constructor for the TDiamond class.

  @precon  None.
  @postcon Creates an intsance of the TDiamond class initialising the properties.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
constructor TDiamond.Create(const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FDiamondSize := 5;
end;

End.
