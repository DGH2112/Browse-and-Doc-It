(**

  This module contains a class to represent an Eidolon Time Location Symbol Line.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.Line;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.Symbol,
  BADI.Eidolon.Types,
  BADI.Types,
  BADI.Comment;

Type
  (** A class to represent a LINE time location symbol **)
  TLine = Class(TSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FLineStartType : TLineEndType;
    FLineStartSize : TLineEndSize;
    FLineEndType : TLineEndType;
    FLineEndSize : TLineEndSize;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      A property to get and set the Line Start Type.
      @precon  None.
      @postcon Get and set the Line Start Type.
      @return  a TLineEndType
    **)
    Property LineStartType : TLineEndType Read FLineStartType Write FLineStartType;
    (**
      A property to get and set the Line Start Size.
      @precon  None.
      @postcon Get and set the Line Start Size.
      @return  a TLineEndSize
    **)
    Property LineStartSize : TLineEndSize Read FLineStartSize Write FLineStartSize;
    (**
      A property to get and set the Line End Type.
      @precon  None.
      @postcon Get and set the Line End Type.
      @return  a TLineEndType
    **)
    Property LineEndType   : TLineEndType Read FLineEndType   Write FLineEndType;
    (**
      A property to get and set the Line End Size.
      @precon  None.
      @postcon Get and set the Line End Size.
      @return  a TLineEndSize
    **)
    Property LineEndSize   : TLineEndSize Read FLineEndSize   Write FLineEndSize;
  End;

Implementation

Uses
  BADI.Eidolon.Constants;

(**

  This method returns a string representation of the line.

  @precon  None.
  @postcon Returns a string representation of the line.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TLine.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  If (LineStartType <> atNone) Or (LineEndType <> atNone) Then
    Begin
      Result := Result + ', ' + strLineEndTypes[LineStartType];
      Result := Result + ', ' + strLineEndSizes[LineStartSize];
      Result := Result + ', ' + strLineEndTypes[LineEndType];
      Result := Result + ', ' + strLineEndSizes[LineEndSize];
    End;
end;

(**

  A constructor for the TLine class.

  @precon  None.
  @postcon Creates an intsance of the TLine class initialising the properties.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TLine.Create(Const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FLineStartType := atNone;
  FLineStartSize := asMediumMedium;
  FLineEndType := atNone;
  FLineEndSize := asMediumMedium;
end;

End.
