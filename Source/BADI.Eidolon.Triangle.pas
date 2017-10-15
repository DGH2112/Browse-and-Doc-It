(**

  This module contains a class to represent an Eidolon Triangle Time Location Symbol.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

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
    Constructor Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
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

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TTriangle.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
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
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TTriangle.Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FTriangleType := ttStartAndEarly;
end;

End.
