(**

  This module contains a class which represents an Eidolon Time Location Symbol Road.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.TLSSchematic.TLSRoad;

Interface

Uses
  BADI.Eidolon.TLSSchematic.TLSShape;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent the roads in the diagram. **)
  TTLSRoad = Class(TTLSShape)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FStartOffset : Integer;
    FEndOffset : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the Road Start Offset.
      @precon  None.
      @postcon Gets and sets the Road Start Offset.
      @return  a Integer
    **)
    Property StartOffset : Integer Read FStartOffset Write FStartOffset;
    (**
      This property gets and sets the Road start offset.
      @precon  None.
      @postcon Gets and sets the Road start offset.
      @return  a Integer
    **)
    Property EndOffset : Integer Read FEndOffset Write FEndOffset;
  End;

Implementation

Uses
  SysUtils,
  BADI.Eidolon.Types, BADI.Eidolon.Constants;

(**

  This method returns a string representation of a road.

  @precon  None.
  @postcon Returns a string representation of a road.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TTLSRoad.AsString(boolShowIdentifier, boolForDocumentation: Boolean) : String;

Begin
  Result := Format('Road %1.1f, %1.1f, %d, %d, %s, %s, %1.1f%%', [StartChainage,
    EndChainage, StartOffset, EndOffset, Locations[Location],
    strColours[Colour], Width]);
  If RouteCode <> '' Then
    Result := Result + Format(', ''%s''', [RouteCode]);
End;

End.
