(**

  This module contains a class to represent an Eidolon Time Location Symbol Shape.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.TLSSchematic.TLSShape;

Interface

Uses
  BADI.ElementContainer,
  BADI.Eidolon.Types,
  BADI.Types,
  BADI.Comment;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** An abstract class from which Road and Object are derived. **)
  TTLSShape = Class {$IFDEF D2005} Abstract {$ENDIF}(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FStartChainage : Double;
    FEndChainage   : Double;
    FLocation      : TLocation;
    FColour        : TColour;
    FRouteCode     : String;
    FWidth         : Double;
    FLineColour    : TColour;
    FLineStyle     : TLineStyle;
    FLineWeight    : TLineWeight;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    (**
      This property gets and sets the Road start chainage.
      @precon  None.
      @postcon Gets and sets the Road start chainage.
      @return  a Double
    **)
    Property StartChainage : Double Read FStartChainage Write FStartChainage;
    (**
      This property gets and sets the Road End Chainage.
      @precon  None.
      @postcon Gets and sets the Road End Chainage.
      @return  a Double
    **)
    Property EndChainage : Double Read FEndChainage Write FEndChainage;
    (**
      This property gets and sets the Road location.
      @precon  None.
      @postcon Gets and sets the Road location.
      @return  a TLocation
    **)
    Property Location : TLocation Read FLocation Write FLocation;
    (**
      This property gets and sets the Road colour.
      @precon  None.
      @postcon Gets and sets the Road colour.
      @return  a TColour
    **)
    Property Colour : TColour Read FColour Write FColour;
    (**
      This property gets and sets the Route Code of the shape.
      @precon  None.
      @postcon Gets and sets the Route Code of the shape.
      @return  a String
    **)
    Property RouteCode : String Read FRouteCode Write FRouteCode;
    (**
      This property gets and sets the width of the shape.
      @precon  None.
      @postcon Gets and sets the width of the shape.
      @return  a Double
    **)
    Property Width : Double Read FWidth Write FWidth;
    (**
      This property gets and sets the line colour for the shape.
      @precon  None.
      @postcon Gets and sets the line colour for the shape.
      @return  a TColour
    **)
    Property LineColour : TColour Read FLineColour Write FLineColour;
    (**
      This property gets and sets the line style for the shape.
      @precon  None.
      @postcon Gets and sets the line style for the shape.
      @return  a TLineStyle
    **)
    Property LineStyle : TLineStyle Read FLineStyle Write FLineStyle;
    (**
      This property gets and sets the line weight for the shape.
      @precon  None.
      @postcon Gets and sets the line weight for the shape.
      @return  a TLineWeight
    **)
    Property LineWeight : TLineWeight Read FLineWeight Write FLineWeight;
  End;

Implementation

(**

  A constructor for the TTLSShape class.

  @precon  None.
  @postcon Sets the route code to a wilcard to match ALL route codes.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TTLSShape.Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment);

begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FRouteCode := '*'; // Match all routes.
end;

End.
