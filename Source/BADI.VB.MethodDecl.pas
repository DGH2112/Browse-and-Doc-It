(**

  This module contains a class to represent a VB method declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

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
Unit BADI.VB.MethodDecl;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.MethodDecl,
  BADI.VB.Interfaces,
  Classes,
  BADI.TYpes;

Type
  (** A class to represent method (SUB & FUNCTION) in visual basic. **)
  TVBMethod = Class(TGenericMethodDecl, IExceptionHandling)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPushParams: TStringList;
    FExceptionHandling : IExceptionHandling;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const MethodType : TMethodType; Const strName : String; Const AScope : TScope;
      Const iLine, iCol : Integer); Override;
    Destructor Destroy; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
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
  BADI.VB.ExceptionHandling;

(**

  This method outputs a string presentation of the method.

  @precon  None .
  @postcon Outputs a string presentation of the method .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBMethod.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Const
  strLib = ' Lib %s';
  strAlias = ' Alias %s';
  strAs = #32'As'#32;

Var
  i: Integer;

Begin
  Result := strMethodType[MethodType] + #32;
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Ext <> '' Then
    Result := Result + Format(strLib, [Ext]);
  If Alias <> '' Then
    Result := Result + Format(strAlias, [Alias]);
  If (Ext <> '') Or (Alias <> '') Then
    Result := Result + #32;
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
  If (MethodType = mtFunction) And (ReturnType.ElementCount > 0) Then
    Result := Result + strAs + ReturnType.AsString(False, boolForDocumentation);
End;

(**

  This is a constructor for the TVBMethod class.

  @precon  None.
  @postcon Adds a string list for managing Pushed parameters.

  @param   MethodType as a TMethodType as a constant
  @param   strName    as a String as a constant
  @param   AScope     as a TScope as a constant
  @param   iLine      as an Integer as a constant
  @param   iCol       as an Integer as a constant

**)
Constructor TVBMethod.Create(Const MethodType : TMethodType; Const strName : String;
  Const AScope : TScope; Const iLine, iCol : Integer);

Begin
  Inherited Create(MethodType, strName, AScope, iLine, iCol);
  FPushParams := TStringList.Create;
  FExceptionHandling := TExceptionHandling.Create(strName);
End;

(**

  This is a destructor for the TVBMethod class.

  @precon  None.
  @postcon Frees the pushed parameters list.

**)
Destructor TVBMethod.Destroy;
Begin
  FPushParams.Free;
  Inherited;
End;

End.
