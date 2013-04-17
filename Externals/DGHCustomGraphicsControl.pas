(**

  This module contains a base custom graphics control that all the DGH graphics
  controls are derived from.

  @Author  David Hoyle
  @Version 1.0
  @Date    17 Apr 2013

**)
Unit DGHCustomGraphicsControl;

Interface

Uses
  Graphics,
  Controls,
  ExtCtrls,
  Windows,
  Classes;

Type
  (** A range of values for the update interval **)
  TInterval = 1 .. 3600;

  (** A range of values for percentages **)
  TPercentage = -1 .. 100;

  (** This is a custom graphics control which implements bevels, bevel width
      and border width. **)
  TDGHCustomGraphicControl = Class(TGraphicControl)
  Private
    FBevelInner   : TPanelBevel;
    FBevelOuter   : TPanelBevel;
    FBevelWidth   : TBevelWidth;
    FBorderWidth  : TBorderWidth;
    FCanInvalidate: Boolean;
    Procedure SetBevelInner(Value: TPanelBevel);
    Procedure SetBevelOuter(Value: TPanelBevel);
    Procedure SetBevelWidth(Value: TBevelWidth);
    Procedure SetBorderWidth(Value: TBorderWidth);
  Protected
    Function GetClientRect: TRect; Override;
    Procedure ChangeCanvasFont(Sender: TObject);
    Procedure Paint; Override;
    (**
      This property determines the inner bevel of the control.
      @precon  None.
      @postcon Determines the inner bevel of the control.
      @return  a TPanelBevel
    **)
    Property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner Default bvNone;
    (**
      This property determines the outer bevel of the control.
      @precon  None.
      @postcon Determines the outer bevel of the control.
      @return  a TPanelBevel
    **)
    Property BevelOuter: TPanelBevel Read FBevelOuter Write SetBevelOuter
      Default bvRaised;
    (**
      This property determines the width of the inner and outer bevels.
      @precon  None.
      @postcon Determines the width of the inner and outer bevels.
      @return  a TBevelWidth
    **)
    Property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth Default 1;
    (**
      This property determines the width between the inner and outer bevel.
      @precon  None.
      @postcon Determines the width between the inner and outer bevel.
      @return  a TBorderWidth
    **)
    Property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth Default 0;
    (**
      This property determines the background colour of the control.
      @precon  None.
      @postcon Determines the background colour of the control.
    **)
    Property Color Default clBtnFace;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Procedure BeginUpdate;
    Procedure EndUpdate;
    (**
      This property returns whether the control can be invalidated.
      @precon  None.
      @postcon Returns whether the control can be invalidated.
      @return  a Boolean
    **)
    Property CanInvalidate: Boolean Read FCanInvalidate;
  Published
    (** Published underlying property @precon None @postcon None **)
    Property Anchors;
    (** Published underlying property @precon None @postcon None **)
    Property Constraints;
    (** Published underlying property @precon None @postcon None **)
    Property OnContextPopup;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseEnter;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseLeave;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseMove;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseUp;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseWheel;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseWheelDown;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseWheelUp;
    (** Published underlying property @precon None @postcon None **)
    Property OnMouseDown;
  End;

Implementation

Uses
  Themes;

{ TDGHCustomGraphicControl }

(**

  This method ends the updating and all invalidating when properties are
  changed.

  @precon  None.
  @postcon Ends the updating and all invalidating when properties are changed.

**)
Procedure TDGHCustomGraphicControl.EndUpdate;

Begin
  FCanInvalidate := True;
  Invalidate;
End;

(**

  This is an overridden getter method for the ClientRect property.

  @precon  None.
  @postcon Returns the client area of the control taking into account the bevels.

  @return  a TRect

**)
Function TDGHCustomGraphicControl.GetClientRect: TRect;

Var
  iBevelSize: Integer;

Begin
  Result := Inherited GetClientRect;
  InflateRect(Result, -FBorderWidth, -FBorderWidth);
  iBevelSize := 0;
  If FBevelOuter <> bvNone Then
    Inc(iBevelSize, FBevelWidth);
  If FBevelInner <> bvNone Then
    Inc(iBevelSize, FBevelWidth);
  InflateRect(Result, -iBevelSize, -iBevelSize);
End;

(**

  This method disables invalidating the control when properties are changed.

  @precon  None.
  @postcon Disables invalidating the control when properties are changed.

**)
Procedure TDGHCustomGraphicControl.BeginUpdate;

Begin
  FCanInvalidate := False;
End;

(**

  This is an on change font event handler to ensures that the canvas gets updated
  with the controls font settings.

  @precon  None.
  @postcon Ensures that the canvas gets updated with the controls font settings.

  @param   Sender as a TObject

**)
Procedure TDGHCustomGraphicControl.ChangeCanvasFont(Sender: TObject);

Begin
  Canvas.Font.Assign(Font);
  If FCanInvalidate Then
    Invalidate;
End;

(**

  This is the constructor method for the TDGHCustomGraphicControl class.

  @precon  None.
  @postcon Initialises the control to its default properties.

  @param   AOwner as a TComponent

**)
Constructor TDGHCustomGraphicControl.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
  Width          := 185;
  Height         := 41;
  FBevelOuter    := bvRaised;
  FBevelWidth    := 1;
  FBorderWidth   := 0;
  Color          := StyleServices.GetSystemColor(clAppWorkSpace);
  Font.OnChange  := ChangeCanvasFont;
  FCanInvalidate := True;
End;

(**

  This is an overridden paint method to draw the graphic control with bevels.

  @precon  None.
  @postcon Draws the control with the appropriate bevels.

**)
Procedure TDGHCustomGraphicControl.Paint;

Var
  Rect                 : TRect;
  TopColor, BottomColor: TColor;

  (**

    This method adjusts the bevel colour appropiate to the style of bevel
    required.

    @precon  None.
    @postcon Adjusts the bevel colour appropiate to the style of bevel required.

    @param   Bevel as a TPanelBevel

  **)
  Procedure AdjustColors(Bevel: TPanelBevel);

  Begin
    TopColor := StyleServices.GetSystemColor(clBtnHighlight);
    If Bevel = bvLowered Then
      TopColor  := StyleServices.GetSystemColor(clBtnShadow);
    BottomColor := StyleServices.GetSystemColor(clBtnShadow);
    If Bevel = bvLowered Then
      BottomColor := StyleServices.GetSystemColor(clBtnHighlight);
  End;

Begin
  Rect := Inherited GetClientRect;
  If FBevelOuter <> bvNone Then
    Begin
      AdjustColors(FBevelOuter);
      Frame3D(Canvas, Rect, TopColor, BottomColor, FBevelWidth);
    End;
  Frame3D(Canvas, Rect, Color, Color, FBorderWidth);
  If FBevelInner <> bvNone Then
    Begin
      AdjustColors(FBevelInner);
      Frame3D(Canvas, Rect, TopColor, BottomColor, FBevelWidth);
    End;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect);
End;

(**

  This is a setter method for the BevelInner property.

  @precon  None.
  @postcon Sets the Inner Bevel property and forced a repaint.

  @param   Value as a TPanelBevel

**)
Procedure TDGHCustomGraphicControl.SetBevelInner(Value: TPanelBevel);

Begin
  FBevelInner := Value;
  If FCanInvalidate Then
    Invalidate;
End;

(**

  This is a setter method for the BevelOuter property.

  @precon  None.
  @postcon Sets the Outer Bevel property and forced a repaint.

  @param   Value as a TPanelBevel

**)
Procedure TDGHCustomGraphicControl.SetBevelOuter(Value: TPanelBevel);

Begin
  FBevelOuter := Value;
  If FCanInvalidate Then
    Invalidate;
End;

(**

  This is a setter method for the Bevel Width property.

  @precon  None.
  @postcon Sets the Bevel Width property and forced a repaint.

  @param   Value as a TBevelWidth

**)
Procedure TDGHCustomGraphicControl.SetBevelWidth(Value: TBevelWidth);

Begin
  FBevelWidth := Value;
  If FCanInvalidate Then
    Invalidate;
End;

(**

  This is a setter method for the Border Width property.

  @precon  None.
  @postcon Sets the Border Width property and forced a repaint.

  @param   Value as a TBorderWidth

**)
Procedure TDGHCustomGraphicControl.SetBorderWidth(Value: TBorderWidth);

Begin
  FBorderWidth := Value;
  If FCanInvalidate Then
    Invalidate;
End;

End.
