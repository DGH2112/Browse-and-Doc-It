(**

  This module contains a class the presentat a generic parameter.

  @Author  David Hoyle
  @Version 1.003
  @Date    24 May 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.Generic.Parameter;

Interface

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Generic.TypeDecl;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represents a parameter of a method declaration. **)
  TGenericParameter = Class(TElementContainer)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FParamModifier: TParamModifier;
    FArrayOf: Boolean;
    FParamType: TGenericTypeDecl;
    FDefaultValue: String;
  Public
    Constructor Create(ParamMod: TParamModifier; const Ident: String; boolArrayOf: Boolean;
      AType: TGenericTypeDecl; const Value: String; AScope: TScope; iLine, iCol: Integer);
    Destructor Destroy; Override;
    Function IsEquals(Parameter: TGenericParameter): Boolean; Virtual;
    (**
      Returns the parameter modifier : const, var or out.
      @precon  None.
      @postcon Returns the parameter modifier : const, var or out.
      @return  a TParamModifier
    **)
    Property ParamModifier: TParamModifier Read FParamModifier;
    (**
      Returns whether the parameter is an array parameter.
      @precon  None.
      @postcon Returns whether the parameter is an array parameter.
      @return  a Boolean
    **)
    Property ArrayOf: Boolean Read FArrayOf;
    (**
      Returns the parameter type identifier for the parameter.
      @precon  None.
      @postcon Returns the parameter type identifier for the parameter.
      @return  a TgenericTypeDecl
    **)
    Property ParamType: TGenericTypeDecl Read FParamType;
    (**
      Returns the default value of the parameter is there is one.
      @precon  None.
      @postcon Returns the default value of the parameter is there is one.
      @return  a String
    **)
    Property DefaultValue: String Read FDefaultValue;
    (**
      Returns the parameters scope with in the record / object / class etc.
      @precon  None.
      @postcon Returns the parameters scope with in the record / object / class etc.
      @return  a TScope
    **)
  End;

Implementation

(**

  This is the constructor for the TParameter class. The constructor initialises
  all the attributes of the classes on construction. It creates a string list
  to store the parameter type.

  @precon  ParamMod is an enumerate identifying a const, var or out parameter,
           Ident is the parameters identifier, boolArrayOf indicate whether the
           parameter is an array, AType is the type of the parameter, Value is
           the constant value for the parameter is applicable, Scope is the
           scope of the parameter, iLine is the line number of the parameter
           and iCol is the column number of the icon.
  @postcon The constructor initialises all the attributes of the classes on
           construction. It creates a string list to store the parameter type.

  @param   ParamMod    as a TParamModifier
  @param   Ident       as a String as a constant
  @param   boolArrayOf as a Boolean
  @param   AType       as a TGenericTypeDecl
  @param   Value       as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iCol        as an Integer

**)
Constructor TGenericParameter.Create(ParamMod : TParamModifier; const Ident : String;
  boolArrayOf : Boolean; AType : TGenericTypeDecl;
  const Value : String; AScope : TScope; iLine, iCol : Integer);

Begin
  Inherited Create(Ident, AScope, iLine, iCol, iiNone, Nil);
  If AType <> Nil Then
    Begin
      FParamType := TElementContainerClass(AType.ClassType).Create('', AScope,
        0, 0, iiNone, Nil) As TGenericTypeDecl;
      FParamType.Assign(AType);
    End;
  FParamModifier := ParamMod;
  FArrayOf := boolArrayOf;
  FDefaultValue := Value;
  Assert(Ident <> '', 'Ident in TGenericParameter IS NULL!');
End;

(**

  This is the TParameters destructor method. If frees the parameter type string
  list.

  @precon  None.
  @postcon If frees the parameter type string list.

**)
destructor TGenericParameter.Destroy;
begin
  FParamType.Free;
  inherited;
end;

(**

  This method compares the current parameter with the given parameter and returns
  true if they have the same signature else returns false.

  @precon  None.
  @postcon Returns true if they signatures are the same else returns false.

  @param   Parameter as a TGenericParameter
  @return  a Boolean

**)
Function TGenericParameter.IsEquals(Parameter : TGenericParameter) : Boolean;

Begin
  Result := False;
  If (ParamType <> Nil) And (Parameter.ParamType <> Nil) Then
    Result := (ParamModifier = Parameter.ParamModifier) And
      (ParamType.AsString(False, False) = Parameter.ParamType.AsString(False, False));
End;

End.
