(**

  This module contains a class to represent an Eidolon Time Location Symbol Object.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Mar 2017

**)
Unit BADI.Eidolon.TLSSchematic.TLSObject;

Interface

Uses
  BADI.Eidolon.TLSSchematic.TLSShape,
  BADI.Eidolon.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent the objects on the schematic diagram. **)
  TTLSObject = Class(TTLSShape)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FText            : String;
    FTextOrientation : TTextOrientation;
    FTextPosition    : TTextPosition;
    FShape           : TSymbolType;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the text of the object.
      @precon  None.
      @postcon Gets and sets the text of the object.
      @return  a String
    **)
    Property Text : String Read FText Write FText;
    (**
      This property gets and sets the text orientation of the object.
      @precon  None.
      @postcon Gets and sets the text orientation of the object.
      @return  a TTextOrientation
    **)
    Property TextOrientation : TTextOrientation Read FTextOrientation
      Write FTextOrientation;
    (**
      This property gets and sets the text position of the object text.
      @precon  None.
      @postcon Gets and sets the text position of the object text.
      @return  a TTextPosition
    **)
    Property TextPosition : TTextPosition Read FTextPosition Write FTextPosition;
    (**
      This property gets and sets the shape of the object.
      @precon  None.
      @postcon Gets and sets the shape of the object.
      @return  a TSymbolType
    **)
    Property Shape : TSymbolType Read FShape Write FShape;
  End;

Implementation

Uses
  SysUtils,
  BADI.Eidolon.TLSSchematic.Constants;

(**

  This method returns a string representation of a schematic object.

  @precon  None.
  @postcon Returns a string representation of a schematic object.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTLSObject.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Format('%s %1.1f, %1.1f, %s, %s, ''%s'', %1.1f%%', [
    ObjectType[Shape] ,StartChainage,
    EndChainage, Locations[Location], strColours[Colour], Text, Width]);
  If RouteCode <> '' Then
    Result := Result + Format(', ''%s''', [RouteCode]);
end;

End.
