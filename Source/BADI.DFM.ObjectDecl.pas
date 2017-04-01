(**

  This module contains a class to represent a n object in a DFM file.

  @Author  David Hoyle
  @Version 1.0
  @date    01 Apr 2017

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
    Constructor Create(const strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
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

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDFMObject.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

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
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TDFMObject.Create(Const strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FObjectType := otObject;
End;

End.
