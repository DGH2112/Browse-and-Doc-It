(**

  This module contains a class to represent an Eidolon SuperBar Time Location Symbol.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.SuperBar;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.CustomFillSymbol,
  BADI.Types,
  BADI.Comment;

Type
  (** A class to represent a SUPERBAR time location symbol **)
  TSuperBar = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDateWidth: Double;
    FLocationWidth: Double;
  Public
    Constructor Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
      AImageIndex: TBADIImageIndex; AComment: TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      This property gets and sets the bars date width of the bar.
      @precon  None.
      @postcon Gets and sets the bars date width of the bar.
      @return  a Double
    **)
    Property DateWidth: Double Read FDateWidth Write FDateWidth;
    (**
      This property gets and sets the bars location width of the bar.
      @precon  None.
      @postcon Gets and sets the bars location width of the bar.
      @return  a Double
    **)
    Property LocationWidth: Double Read FLocationWidth Write FLocationWidth;
  End;

Implementation

Uses
  SysUtils;

(**

  Returns a representation of the TSuperBar element.

  @precon  None.
  @postcon Returns a representation of the TSuperBar element.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TSuperBar.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + Format('%1.2n', [FDateWidth]);
  Result := Result + ', ' + Format('%1.2n', [FLocationWidth]);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
End;

(**

  This is a constructor for the TSuperBar class.

  @precon  None.
  @postcon Creates a default TSuperBar class.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TSuperBar.Create(const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FDateWidth := 7;
  FLocationWidth := 100;
End;

End.
