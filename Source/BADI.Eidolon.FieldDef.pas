(**

  This module contains a class to represent an Eidolon Field Definition.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.FieldDef;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Eidolon.Types,
  BADI.Types,
  BADI.Comment;

Type
  (** A class to represent a Database definition. **)
  TFieldDef = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOutputName : String;
    FFieldType  : TFieldType;
    FFieldWidth : Integer;
    FPrimaryKey : Boolean;
    FSheetIndex : Integer;
  Public
    Constructor Create(const strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the output name of the field.
      @precon  None.
      @postcon Gets and sets the output name of the field.
      @return  a String
    **)
    Property OutputName : String Read FOutputName Write FOutputName;
    (**
      This property gets and sets the Field Type.
      @precon  None.
      @postcon Gets and sets the Field Type.
      @return  a TFieldType
    **)
    Property FieldType : TFieldType Read FFieldType Write FFieldType;
    (**
      This property gets and sets the Field Width.
      @precon  None.
      @postcon Gets and sets the Field Width.
      @return  a Integer
    **)
    Property FieldWidth : Integer Read FFieldWidth Write FFieldWidth;
    (**
      This property gets and sets the Primary Key.
      @precon  None.
      @postcon Gets and sets the Primary Key.
      @return  a Boolean
    **)
    Property PrimaryKey : Boolean Read FPrimaryKey Write FPrimaryKey;
    (**
      This property gets and sets the Sheet Index.
      @precon  None.
      @postcon Gets and sets the Sheet Index.
      @return  a Integer
    **)
    Property SheetIndex  :Integer Read FSheetIndex Write FSheetIndex;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns string representation of the Field Definition.

  @precon  None.
  @postcon Returns string representation of the Field Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TFieldDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

Const
  strFieldType : Array[Low(TFieldType)..High(TFieldType)] Of String = (
    'B', 'Y', 'I', 'L', 'U', 'S', 'F', 'D', '?', 'C', 'O', 'M', '?',
    '?', '?', '?', '?', '?', '?', '?', '?');
  {
  Unknown = $00000000
  Boolean = $00000001
  Byte = $00000002
  Integer = $00000003
  Long = $00000004
  Currency = $00000005
  Single = $00000006
  Double = $00000007
  Date = $00000008
  Binary = $00000009
  Text = $0000000A
  LongBinary = $0000000B
  Memo = $0000000C
  GUID = $0000000F
  BigInt = $00000010
  VarBinary = $00000011
  Char = $00000012
  Numeric = $00000013
  Decimal = $00000014
  Float = $00000015
  Time = $00000016
  TimeStamp = $00000017
  }

  strPrimaryKey : Array[False..True] Of String = ('', '*');

begin
  Result := strPrimaryKey[FPrimaryKey] + Identifier + ':' +
    strFieldType[FFieldType];
  If FFieldType = ftText Then
    Result := Result + Format('(%d)', [FFieldWidth]);
  If FOutputName <> '' Then
    Result := Result + '=' + FOutputName;
end;

(**

  A constructor for the TFieldDef class.

  @precon  None.
  @postcon Defaults the field type to text with a width of 255 characters.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
constructor TFieldDef.Create(const strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TBADIImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FFieldType := ftText;
  FFieldWidth := 255;
  FPrimaryKey := False;
end;

End.
