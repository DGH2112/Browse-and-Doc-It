(**

  This module contains a class to represent a n object in a DFM file.

  @Author  David Hoyle
  @Version 1.0
  @date    12 Oct 2017

**)
Unit BADI.DFM.ObjectDecl;

Interface

Uses
  BADI.ElementContainer,
  BADI.DFM.Types,
  BADI.Types,
  BADI.Comment;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represent a DFM object in the file. **)
  TDFMObject = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FObjectType: TObjectType;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property sets and gets the whether the Object, Inherited or Inline.
      @precon  None.
      @postcon Sets and gets the whether the Object, Inherited or Inline.
      @return  a TObjectType
    **)
    Property ObjectType : TObjectType Read FObjectType Write FObjectType;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string represetation of the DFM object.

  @precon  None.
  @postcon Returns a string represetation of the DFM object.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDFMObject.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Case ObjectType Of
    otObject: Result := 'Object';
    otInherited: Result := 'Inherited';
    otinline: Result := 'Inline';
  End;
  Result := Result + #32 + BuildStringRepresentation(True, boolForDocumentation,
    ':', BADIOptions.MaxDocOutputWidth)
End;

(**

  A constructor for the TDFMObject class.

  @precon  None.
  @postcon Initialises FInherited to false.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TDFMObject.Create(Const strName: String; Const AScope: TScope; Const iLine,
  iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FObjectType := otObject;
End;

End.
