(**

  This module contains a class to represent an Eidolon Time Location Symbol Static Text.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Mar 2017

**)
Unit BADI.Eidolon.TLSSchematic.TLSStatic;

Interface

Uses
  BADI.Eidolon.TLSSchematic.TLSObject;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent a n Ellipse shape. **)
  TTLSStatic = Class(TTLSObject)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FStartOffset : Double;
    FEndOffset   : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the start offset of the bounding rectangle
      to the ellipse.
      @precon  None.
      @postcon Gets and sets the start offset of the bounding rectangle
               to the ellipse.
      @return  a Double
    **)
    Property StartOffset : Double Read FStartOffset Write FStartOffset;
    (**
      This property gets and sets the end offset of the bounding rectangle
      to the ellipse.
      @precon  None.
      @postcon Gets and sets the start offset of the bounding rectangle
               to the ellipse.
      @return  a Double
    **)
    Property EndOffset : Double Read FEndOffset Write FEndOffset;
  End;

Implementation

Uses
  SysUtils,
  BADI.Eidolon.TLSSchematic.Constants,
  BADI.Eidolon.Types;

(**

  This method returns a string representation of a schematic object.

  @precon  None.
  @postcon Returns a string representation of a schematic object.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTLSStatic.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

begin
  Result := Format('STATIC%s %1.1f, %1.1f, %1.0f%%, %1.0f%%, %s, %s, ''%s''', [
    ObjectType[Shape],
    StartChainage,
    EndChainage,
    StartOffset,
    EndOffset,
    Locations[Location],
    strColours[Colour],
    Text
  ]);
end;

End.
