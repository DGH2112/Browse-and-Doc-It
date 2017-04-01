(**

  This module contains a class which implements an Object Pascal specific constant Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Pascal.ConstantDecl;

Interface

Uses
  BADI.Generic.Constant,
  BADI.Types,
  BADI.Comment;

Type
  (** This is a sub class for all constants. **)
  TConstant = Class(TGenericConstant)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FTyped: Boolean;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
      AImageIndex: TBADIImageIndex; AComment: TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      This property determines it the constant is typed or simple.
      @precon  None.
      @postcon Sets or gets whether the constant is typed or simple.
      @return  a Boolean
    **)
    Property Typed: Boolean Read FTyped Write FTyped;
  End;

Implementation

uses
  BADI.Options;

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Formats the constant information depending on whether its a simple
           constant or a typed constant.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TConstant.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  If FTyped Then
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, ':',
      BADIOptions.MaxDocOutputWidth)
  Else
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '=',
      BADIOptions.MaxDocOutputWidth);
End;

(**

  This is the constructor method for the TConstant class.

  @precon  None.
  @postcon Creates an instance of a TConstant declaration.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TConstant.Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);
Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FTyped := False;
End;

End.
