(**

  This module contains constants for use with the Eidolon module parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.Symbol;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Eidolon.Types,
  BADI.Types,
  BADI.Comment;

Type
  (** A base class for all Time Location Symbols **)
  TSymbol = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FBorderColour    : TColour;
    FBorderLineStyle : TLineStyle;
    FBorderWeight    : TLineWeight;
    FSymbolType      : TSymbolType;
    FLayerIndex      : Integer;
  Public
    Constructor Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the symbol type.
      @precon  None.
      @postcon Gets and sets the symbol type.
      @return  a TSymbolType
    **)
    Property SymbolType : TSymbolType Read FSymbolType Write FSymbolType;
    (**
      This property gets and sets the border colour of the symbol.
      @precon  None.
      @postcon Gets and sets the border colour of the symbol.
      @return  a TColour
    **)
    Property BorderColour : TColour Read FBorderColour Write FBorderColour;
    (**
      This property gets and sets the border line style of the symbol.
      @precon  None.
      @postcon Gets and sets the border line style of the symbol.
      @return  a TLineStyle
    **)
    Property BorderLineStyle : TLineStyle Read FBorderLineStyle Write FBorderLineStyle;
    (**
      This property gets and sets the border line weight of the symbol.
      @precon  None.
      @postcon Gets and sets the border line weight of the symbol.
      @return  a TLineWeight
    **)
    Property BorderWeight : TLineWeight Read FBorderWeight Write FBorderWeight;
    (**
      This property gets and sets the layer index of the symbol.
      @precon  None.
      @postcon Gets and sets the layer index of the symbol.
      @return  an Integer
    **)
    Property LayerIndex : Integer Read FLayerIndex Write FLayerIndex;
  End;

Implementation

Uses
  BADI.Eidolon.Constants;

(**

  This method returns string representation of the basic Time Location Symbol.

  @precon  None.
  @postcon Returns string representation of the basic Time Location Symbol.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
function TSymbol.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=' +
    strSymbolTypes[FSymbolType] + ', ' +
    strColours[FBorderColour] + ', ' +
    strLineStyles[FBorderLineStyle] + ', ' +
    strLineWeights[FBorderWeight];
end;

(**

  A constructor for the TSymbol class.

  @precon  None.
  @postcon Creates a TSymbol as a default Rectangle.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TSymbol.Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
  Const AImageIndex : TBADIImageIndex; Const AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FSymbolType := tstRectangle;
  FBorderColour := xlcBLACK;
  FBorderLineStyle := lsSOLID;
  FBorderWeight := lw0_25;
  FLayerIndex := 0;
end;

End.
