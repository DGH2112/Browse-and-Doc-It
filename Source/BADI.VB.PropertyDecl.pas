(**

  This module contains a class to represent a VB property declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    22 Oct 2017

**)
Unit BADI.VB.PropertyDecl;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.PropertyDecl,
  BADI.VB.Interfaces,
  BADI.Types,
  BADI.Comment,
  BADI.VB.Types;

Type
  (** A class to represent properties in visual basic. **)
  TVBProperty = Class(TGenericProperty, IExceptionHandling)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPropertyType: TVBPropertyType;
    FExceptionHandling : IExceptionHandling;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Constructor Create(APropertyType : TVBPropertyType; strName : String;
      AScope : TScope; iLine, iCol : Integer; iImageIndex : TBADIImageIndex;
      AComment : TComment); Reintroduce; Virtual;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    (**
      This property gets and sets the type of visula basic property.
      @precon  None.
      @postcon Gets and sets the type of visula basic property.
      @return  a TVBPropertyType
    **)
    Property PropertyType : TVBPropertyType Read FPropertyType Write FPropertyType;
    (**
      This property implements the IExceptionHandling interface.
      @precon  None.
      @postcon Implements the IExceptionHandling interface.
      @return  an IExceptionHandling
    **)
    Property ExceptionHandling : IExceptionHandling Read FExceptionHandling
      Implements IExceptionHandling;
  End;

Implementation

Uses
  BADI.VB.Constants,
  SysUtils,
  BADI.VB.ResourceStrings,
  BADI.VB.ExceptionHandling;

(**

  This method returns a string representation of a visual basic property.

  @precon  None .
  @postcon Returns a string representation of a visual basic property .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBProperty.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Var
  i: Integer;

Begin
  Result := 'Property ';
  Result := Result + strPropertyType[PropertyType] + ' ';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  Result := Result + '(';
  For i := 0 To ParameterCount - 1 Do
    Begin
      If i > 0 Then
        Result := Result + ',';
      If boolForDocumentation Then
        Result := Result + #13#10
      Else
        If i > 0 Then
        Result := Result + #32;
      If boolForDocumentation Then
        Result := Result + #32#32;
      Result := Result + Parameters[i].AsString(boolShowIdentifier,
        boolForDocumentation);
    End;
  If boolForDocumentation Then
    Result := Result + #13#10;
  Result := Result + ')';
  If (PropertyType = ptGet) And (ReturnType.ElementCount > 0) Then
    Result := Result + #32'As'#32 + ReturnType.AsString(False,
      boolForDocumentation);
End;

(**

  This is an overridden method to additional check for property parameters.

  @precon  None.
  @postcon Overridden method to additional check for property parameters.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TVBProperty.CheckDocumentation(Var boolCascade: Boolean);
Begin
  Inherited CheckDocumentation(boolCascade);
  If PropertyType In [ptGet] Then
    Begin
      If ReturnType = Nil Then
        AddIssue(Format(strProperyRequiresReturn, [Identifier]), scNone, Line, Column, etWarning, Self);
    End
  Else
    Begin
      If ParameterCount = 0 Then
        AddIssue(Format(strProperyRequireParam, [Identifier]), scNone, Line, Column, etWarning, Self);
    End;
End;

(**

  This is a constructor for the TVBProperty class.

  @precon  None .
  @postcon Maps the property type to an internal method type + creates a string
           list for pushed parameters .

  @param   APropertyType as a TVBPropertyType
  @param   strName       as a String
  @param   AScope        as a TScope
  @param   iLine         as an Integer
  @param   iCol          as an Integer
  @param   iImageIndex   as a TBADIImageIndex
  @param   AComment      as a TComment

**)
Constructor TVBProperty.Create(APropertyType: TVBPropertyType; strName: String;
  AScope: TScope; iLine, iCol: Integer; iImageIndex: TBADIImageIndex;
  AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iCol, iImageIndex, AComment);
  FPropertyType := APropertyType;
  FExceptionHandling := TExceptionHandling.Create(strName);
End;

(**

  This is a getter method for the TVBProperty property.

  @precon  None.
  @postcon Returns an altered identifier to distinguish between Get, Let and Set
           properties with the same name.

  @return  a String

**)
Function TVBProperty.GetName: String;
Begin
  Result := strPropertyType[PropertyType] + '.' + Identifier;
End;

End.
