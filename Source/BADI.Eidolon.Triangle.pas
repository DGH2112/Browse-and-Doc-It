(**

  This module contains a class to represent an Eidolon Triangle Time Location Symbol.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.Triangle;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.CustomFillSymbol,
  BADI.Eidolon.Types,
  BADI.Types,
  BADI.Comment;

Type
  (** A class to represent a DIAMOND time location symbol **)
  TTriangle = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FTriangleType : TTriangleType;
  Public
    Constructor Create(const strName: String; AScope : TScope; iLine, iColumn : Integer;
      AImageIndex : TBADIImageIndex; AComment: TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a TTriangleType
    **)
    Property TriangleType : TTriangleType Read FTriangleType Write FTriangleType;
  End;

Implementation

Uses
  BADI.Eidolon.Constants,
  SysUtils;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTriangle.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + strTriangleTypes[FTriangleType];
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

(**

  A constructor for the TTriangle class.

  @precon  None.
  @postcon Creates an intsance of the TTriangle class initialising the properties.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
constructor TTriangle.Create(const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FTriangleType := ttStartAndEarly;
end;

End.
