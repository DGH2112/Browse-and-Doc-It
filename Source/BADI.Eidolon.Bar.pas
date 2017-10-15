(**

  This module contains a class to represent an Eidolon Bar Time Location Symbol.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.Bar;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.CustomFillSymbol,
  BADI.Types,
  BADI.Comment;

Type
  (** A class to represent a BAR time location symbol **)
  TBar = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FBarWidth : Integer;
  Public
    Constructor Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a Integer
    **)
    Property BarWidth : Integer Read FBarWidth Write FBarWidth;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TBar.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + Format('%d', [FBarWidth]);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

(**

  A constructor for the TBar class.

  @precon  None.
  @postcon Creates an intsance of the TBar class initialising the properties.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TBar.Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FBarWidth := 5;
end;

End.
