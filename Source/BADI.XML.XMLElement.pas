(**

  This module contains a class to represent an XML Element.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.XML.XMLElement;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  Classes,
  BADI.XML.BaseElement,
  BADI.Types,
  BADI.Comment;

Type
  (** This class represents the individual elements (tags) of the document. **)
  TXMLElement = Class(TXMLBaseElement)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FAttributes : TStringList;
    FContext    : TStringList;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  ContextText : String;
  Public
    Constructor Create(const strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Destructor Destroy; Override;
    Procedure AddContextText(Const strText : String);
    Function AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
    (**
      This property returns the Attributes string list.
      @precon  None.
      @postcon Provides access to the elements attribute names.
      @return  a TStringList
    **)
    Property Attribute : TStringList Read FAttributes;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options;

(**

  This method add tokens to the text (between tags).

  @precon  None.
  @postcon A token is added to the context information.

  @param   strText as a String as a Constant

**)
Procedure TXMLElement.AddContextText(Const strText: String);

Begin
  If FContext.Count < BADIOptions.TokenLimit Then
    FContext.Add(strText);
End;

(**

  This method returns a string representation of the XML Element.

  @precon  None.
  @postcon Returns a string representation of the XML Element.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TXMLElement.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;
Begin
  Result := '<' + BuildStringRepresentation(boolShowIdenifier, boolForDocumentation, '',
    BADIOptions.MaxDocOutputWidth, [#32, '='], [#32, '='], []) + '>' + ContextText + '</'
    + Identifier + '>';
End;

(**

  This method returns a string representation of the token in the context text (between tags).

  @precon  None.
  @postcon A string of the context tokens is return.

  @return  a String

**)
Function TXMLElement.ContextText: String;

Var
  i: Integer;

Begin
  For i := 0 To FContext.Count - 1 Do
    Result := Result + FContext[i];
  If FContext.Count > BADIOptions.TokenLimit Then
    Result := Result + '...';
  Result := Trim(Result);
End;

(**

  This is a constructor for the TXMLElement class.

  @precon  None.
  @postcon Creates an xml element with a unique name derived from the given
           name, line number and column number.

  @param   strName     as a String as a Constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TXMLElement.Create(Const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);
Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FAttributes := TStringList.Create;
  FAttributes.Sorted := True;
  FContext := TStringList.Create;
End;

(**

  This is a destructor for the TXMLElement class.

  @precon  None.
  @postcon Frees the memory used for hold attribute names.

**)
Destructor TXMLElement.Destroy;
Begin
  FContext.Free;
  FAttributes.Free;
  Inherited Destroy;
End;

End.
