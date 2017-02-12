(**
  
  This module contains a simple graphics control which displays a ellipsis path
  is the content is too long for the client rectangle.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Feb 2017

**)
unit DGHEllipsisLabel;

interface

Uses
  Controls, Classes;

Type
  (** A class to represent the ellipsis graphics control. **)
  TDGHEllipsisLabel = Class(TGraphicControl)
  Strict Private
    FCaption : String;
  Strict Protected
    Procedure SetCaption(Const strValue : String);
  Public
    Procedure Paint; Override;
    (**
      This property gets or sets the text of the graphics control.
      @precon  None.
      @postcon Gets or sets the text of the graphics control.
      @return  a String
    **)
    Property Caption : String Read FCaption Write SetCaption;
  Published
    (** Publish this existing property. @precon  None. @postcon None. **)
    Property Align;
  End;

  procedure Register;

implementation

Uses
  Windows, Graphics;

(**

  The standard Delphi register procedure..

  @precon  None
  @postcon Registers the component with the Delphi Palette.

**)
procedure Register;
begin
  RegisterComponents('DGH Controls', [TDGHEllipsisLabel]);
end;

{ TEllipsisLabel }

(**

  This method override the default painting of the graphics control to draw the
  text with an ellipsis path if the text is too long to fit in the available
  client rectangle.

  @precon  None.
  @postcon Draws the caption text with an ellipsis path.

**)
procedure TDGHEllipsisLabel.Paint;

var
  R: TRect;

begin
  R := GetClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  Canvas.Font.Assign(Font);
  DrawText(Canvas.Handle, PChar(FCaption), Length(FCaption), R,
    DT_CENTER Or DT_SINGLELINE Or DT_PATH_ELLIPSIS Or DT_VCENTER);
end;

(**

  This is a setter method for the Caption property.

  @precon  None.
  @postcon Sets the caption for the graphics control and re-paints the control.

  @param   strValue as a String as a constant

**)
procedure TDGHEllipsisLabel.SetCaption(Const strValue: String);
begin
  If FCaption <> strValue Then
    Begin
      FCaption := strValue;
      Invalidate;
    End;
end;

end.
