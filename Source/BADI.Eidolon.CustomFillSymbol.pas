(**

  This module contains a class to represent an Eidolon Custom Fill Symbol which is a base class for
  all symbols that require a fill.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.CustomFillSymbol;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.Types,
  BADI.Eidolon.Line,
  BADI.Types,
  BADI.Comment;

Type
  (** A custom class to contain the main properties of symbols with areas. **)
  TCustomFillSymbol = Class(TLine)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FInteriorColour        : TColour;
    FInteriorPattern       : TInteriorPattern;
    FInteriorPatternColour : TColour;
    FTransparency : Integer;
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the Interior Colour of the rectangle.
      @precon  None.
      @postcon Gets and sets the Interior Colour of the rectangle.
      @return  a TColour
    **)
    Property InteriorColour : TColour Read FInteriorColour Write FInteriorColour;
    (**
      This property gets and sets the Interior Pattern of the rectangle.
      @precon  None.
      @postcon Gets and sets the Interior Pattern of the rectangle.
      @return  a TInteriorPattern
    **)
    Property InteriorPattern : TInteriorPattern Read FInteriorPattern Write FInteriorPattern;
    (**
      This property gets and sets the Interior Pattern Colour of the rectangle.
      @precon  None.
      @postcon Gets and sets the Interior Pattern Colour of the rectangle.
      @return  a TColour
    **)
    Property InteriorPatternColour : TColour Read FInteriorPatternColour Write FInteriorPatternColour;
    (**
      This property gets and sets the transparency of the rectangle.
      @precon  None.
      @postcon Gets and sets the transparency of the rectangle.
      @return  a Integer
    **)
    Property Transparency : Integer Read FTransparency Write FTransparency;
  End;

Implementation

Uses
  BADI.Eidolon.Constants;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TCustomFillSymbol.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' +
    strColours[FInteriorColour] + ', ' +
    strInteriorPatterns[FInteriorPattern] + ', ' +
    strColours[FInteriorPatternColour];
end;

(**

  A constructor for the Trectangle class.

  @precon  None.
  @postcon Initialises the transparency to -1.0, i.e. not set.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TCustomFillSymbol.Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FInteriorColour := xlcWHITE;
  FInteriorPattern := ipNONE;
  FInteriorPatternColour := xlcNONE;
  FTransparency := -1;
end;

End.
