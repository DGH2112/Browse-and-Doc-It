(**

  This module contains a class which implements an Object Pascal specific Property Declaration.

  @Author  David Hoyle
  @Version 1.004
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit BADI.Pascal.PropertyDecl;

Interface

Uses
  BADI.Generic.PropertyDecl,
  BADI.Pascal.IdentList,
  BADI.Types,
  BADI.Comment;

Type
  (** This is a class that defines properties with Object Pascal code. **)
  TPascalProperty = Class(TGenericProperty)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FIndexSpec : String;
    FWriteSpec: String;
    FImplementsSpec: TIdentList;
    FStoredSpec: String;
    FDefaultSpec: String;
    FReadSpec: String;
    FDefaultProperty: Boolean;
    FDispIDSpec: String;
    FReadOnlySpec: Boolean;
    FWriteOnlySpec: Boolean;
    FIsClassProperty : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Constructor Create(Const strIdent: String; Const AScope: TScope; Const iLine, iCol : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Destructor Destroy; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      Returns the
      @precon  None.
      @postcon Returns the
      @return  a String
    **)
    Property IndexSpec : String Read FIndexSpec Write FIndexSpec;
    (**
      Returns the properties Read specification.
      @precon  None.
      @postcon Returns the properties Read specification.
      @return  a String
    **)
    Property ReadSpec : String Read FReadSpec Write FReadSpec;
    (**
      Returns the properties write specification.
      @precon  None.
      @postcon Returns the properties write specification.
      @return  a String
    **)
    Property WriteSpec : String Read FWriteSpec Write FWriteSpec;
    (**
      Returns the properties Stored specification.
      @precon  None.
      @postcon Returns the properties Stored specification.
      @return  a String
    **)
    Property StoredSpec : String Read FStoredSpec Write FStoredSpec;
    (**
      Returns the property default value.
      @precon  None.
      @postcon Returns the property default value.
      @return  a String
    **)
    Property DefaultSpec : String Read FDefaultSpec Write FDefaultSpec;
    (**
      Returns whether this property is the classes / interfaces default
      @precon  None.
      @postcon Returns whether this property is the classes / interfaces default
      property.
      @return  a Boolean
    **)
    Property DefaultProperty : Boolean Read FDefaultProperty Write FDefaultProperty;
    (**
      Returns the implements specification for the property.
      @precon  None.
      @postcon Returns the implements specification for the property.
      @return  a TIdentList
    **)
    Property ImplementsSpec : TIdentList Read FImplementsSpec Write FImplementsSpec;
    (**
      Returns the properties DispID reference.
      @precon  None.
      @postcon Returns the properties DispID reference.
      @return  a String
    **)
    Property DispIdSpec : String Read FDispIDSpec Write FDispIDSpec;
    (**
      Returns whether the property has a ReadOnly specification.
      @precon  None.
      @postcon Returns whether the property has a ReadOnly specification.
      @return  a Boolean
    **)
    Property ReadOnlySpec : Boolean Read FReadOnlySpec Write FReadOnlySpec;
    (**
      Returns whether the property has a WriteOnly specification.
      @precon  None.
      @postcon Returns whether the property has a WriteOnly specification.
      @return  a Boolean
    **)
    Property WriteOnlySpec : Boolean Read FWriteOnlySpec Write FWriteOnlySpec;
    (**
      Returns whether the property has a class prefix.
      @precon  None.
      @postcon gets or sets the property.
      @return  a Boolean
    **)
    Property IsClassProperty : Boolean Read FIsClassProperty Write FIsClassProperty;
  End;

Implementation

uses
  SysUtils,
  BADI.Pascal.Constants,
  BADI.Pascal.Functions;

(**

  This is the constructor method for the TPascalProperty class.

  @precon  None.
  @postcon Initialises the property specifiers.

  @param   strIdent    as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iCol        as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TPascalProperty.Create(Const strIdent: String; Const AScope: TScope; Const iLine, iCol : Integer;
      Const AImageIndex : TBADIImageIndex; Const AComment : TComment);

Begin
  Inherited Create(strIdent, AScope, iLine, iCol, AImageIndex, AComment);
  FDefaultProperty := False;
  FDefaultSpec := '';
  FDispIDSpec := '';
  FImplementsSpec := TIdentList.Create('', scNone, 0, 0, iiNone, Nil);
  FIndexSpec := '';
  FReadOnlySpec := False;
  FWriteOnlySpec := False;
  FReadSpec := '';
  FStoredSpec := '';
  FWriteSpec := '';
  FIsClassProperty := False;
End;

(**

  This is a destructor for the TPascalProperty class.

  @precon  None.
  @postcon Frees any memory used by the Imlpemented Specifications.

**)
Destructor TPascalProperty.Destroy;

Begin
  FImplementsSpec.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns a unique name for properties which are overloaded.

  @return  a String

**)
Function TPascalProperty.GetName: String;

Var
  i: Integer;

Begin
  Result := Identifier;
  If Result = '' Then
    Result := Format('PROP%4.4d', [Random(9999)]);
  For i := 0 To ParameterCount - 1 Do
    Begin
      Result := Result + '.' + strParamModifier[Parameters[i].ParamModifier];
      If Parameters[i].ParamType <> Nil Then
        Begin
          Result := Result + strArrayOf[Parameters[i].ArrayOf];
          Result := Result + Parameters[i].ParamType.AsString(False, False);
        End;
      If ReturnType.ElementCount > 0 Then
        Result := Result + ReturnType.AsString(False, False);
    End;
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Outputs the pascal property declaration .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TPascalProperty.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

  (**

    This is a shorthand routine for output the string specs to the result.

    @precon  None .
    @postcon Output the string specs to the result .

    @param   strName  as a String
    @param   strValue as a String

  **)
  Procedure OutputSpec(strName, strValue : String);

  Begin
    If strValue <> '' Then
      Begin
        If boolForDocumentation Then
          Result := Result + #32#32;
        If strName <> '' Then
          Begin
            If Not boolForDocumentation Then
              Result := Result + #32;
            Result := Result + strName;
          End;
        Result := Result + #32 + strValue;
        If boolForDocumentation Then
          Result := Result + #13#10;
      End;
  End;

Var
  i : Integer;

begin
  Result := 'Property ';
  If IsClassProperty Then Result := 'Class ' + Result;
  If boolShowIdentifier Then Result := Result + Identifier;
  If ParameterCount > 0 Then
    Begin
      Result := Result + '[';
      If boolForDocumentation Then Result := Result + #13#10;
      Result := Result + BuildParameterRepresentation(Self, boolShowIdentifier,
        boolForDocumentation);
      If boolForDocumentation Then Result := Result + #13#10;
      Result := Result + ']';
    End;
  If ReturnType.ElementCount > 0 Then
    Begin
      Result := Result + #32':'#32;
      Result := Result + ReturnType.AsString(False, boolForDocumentation);
    End;
  If boolForDocumentation Then Result := Result + #13#10;
  OutputSpec('Index', FIndexSpec);
  OutputSpec('Read', FReadSpec);
  OutputSpec('Write', FWriteSpec);
  OutputSpec('Stored', FStoredSpec);
  OutputSpec('Default', FDefaultSpec);
  If FImplementsSpec.ElementCount > 0 Then
    Begin
      If boolForDocumentation Then
        Result := Result + #32#32
      Else
        Result := Result + #32;
      Result := Result + 'Implements ';
      For i := 1 To FImplementsSpec.ElementCount Do
        Begin
          If i > 1 Then Result := Result + ', ';
          Result := Result + FImplementsSpec.Elements[i].Identifier;
        End;
      If boolForDocumentation Then Result := Result + #13#10;
    End;
  If FReadOnlySpec Then OutputSpec('', 'ReadOnly');
  If FWriteOnlySpec Then OutputSpec('', 'WriteOnly');
  OutputSpec('DispID', FDispIDSpec);
  If FDefaultProperty Then OutputSpec('', 'Default');
end;

End.
