(**

  This modlue containsa class whic represents a base element class for all the XML elements in the
  parser.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.XML.BaseElement;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

Type
  (** This is a base class for all the XML elements. **)
  TXMLBaseElement = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FElementName: String;
  Public
    Constructor Create(const strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Function GetName : String; Override;
    Function AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the XML Base Element.

  @precon  None.
  @postcon Returns a string representation of the XML Base Element.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TXMLBaseElement.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := Identifier;
End;

(**

  This is a constructor for the TXMLBaseElement class.

  @precon  None.
  @postcon Initialises the class to be not sorted and have a unique name.

  @param   strName     as a String as a Constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TXMLBaseElement.Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  Sorted := False;
  FElementName := Format('%s:%4.4d:%4.4d', [strName, iLine, iColumn]);
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the element created in the constructor.

  @return  a String

**)
Function TXMLBaseElement.GetName: String;

Begin
  Result := FElementName;
End;

End.
