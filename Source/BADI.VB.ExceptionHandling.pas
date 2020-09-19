(**

  This module contains a class for handling exception handling in VB code.

  @Author  David Hoyle
  @Version 1.001
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
Unit BADI.VB.ExceptionHandling;

Interface

Uses
  BADI.VB.Interfaces,
  Classes;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (** A class to handle exception handling information for method and
      properties. **)
  TExceptionHandling = Class(TInterfacedObject, IExceptionHandling)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FHasErrorHnd : Boolean;
    FHasExit     : Boolean;
    FHasPop      : Boolean;
    FHasPush     : Boolean;
    FPushName    : String;
    FPushParams  : TStringList;
    FExitLine    : Integer;
    FExitCol     : Integer;
    FMethodName  : String;
  Public
    Constructor Create(strMethodName : String);
    Destructor Destroy; Override;
    function GetHasErrorHnd: Boolean;
    function GetHasExit: Boolean;
    function GetHasPop: Boolean;
    function GetHasPush: Boolean;
    function GetPushName: string;
    function GetPushParams: TStringList;
    Function GetExitLine : Integer;
    Function GetExitCol : Integer;
    procedure SetHasErrorHnd(boolValue: Boolean);
    procedure SetHasExit(boolValue: Boolean);
    procedure SetHasPop(boolValue: Boolean);
    procedure SetHasPush(boolValue: Boolean);
    procedure SetPushName(strValue: string);
    Procedure SetExitLine(iLine : Integer);
    Procedure SetExitCol(iCol : Integer);
    Function GetMethodName : String;
  End;

Implementation

(**

  This is a constructor for the TExceptionHandling class.

  @precon  None.
  @postcon Initialises the push parameters string list.

  @param   strMethodName as a String

**)
constructor TExceptionHandling.Create(strMethodName : String);
begin
  Inherited Create;
  FMethodName := strMethodName;
  FPushParams := TStringList.Create;
end;

(**

  This is a destructor for the TExceptionHandling class.

  @precon  None.
  @postcon Frees the push parameters string list.

**)
destructor TExceptionHandling.Destroy;
begin
  FPushParams.Free;
  Inherited Destroy;
end;

(**

  This is a getter method for the ExitCol property.

  @precon  None.
  @postcon Returns the Exit Column number.

  @return  an Integer

**)
function TExceptionHandling.GetExitCol: Integer;
begin
  Result := FExitCol;
end;

(**

  This is a getter method for the ExitLine property.

  @precon  None.
  @postcon Returns the Exit Line number.

  @return  an Integer

**)
function TExceptionHandling.GetExitLine: Integer;
begin
  Result := FExitLine;
end;

(**

  This is a getter method for the HasErrorHnd property.

  @precon  None.
  @postcon Returns whether the method / property has an error handler.

  @return  a Boolean

**)
function TExceptionHandling.GetHasErrorHnd: Boolean;
begin
  Result := FHasErrorHnd;
end;

(**

  This is a getter method for the HasExit property.

  @precon  None.
  @postcon Returns whether the method / property has an exit statement.

  @return  a Boolean

**)
function TExceptionHandling.GetHasExit: Boolean;
begin
  Result := FHasExit;
end;

(**

  This is a getter method for the HasPop property.

  @precon  None.
  @postcon Returns whether the method / property has a pop statement.

  @return  a Boolean

**)
function TExceptionHandling.GetHasPop: Boolean;
begin
  Result := FHasPop;
end;

(**

  This is a getter method for the HasPush property.

  @precon  None.
  @postcon Returns whether the method / property has a push statement.

  @return  a Boolean

**)
function TExceptionHandling.GetHasPush: Boolean;
begin
  Result := FHasPush;
end;

(**

  This is a getter method for the MethodName property.

  @precon  None.
  @postcon Returns the name of the method.

  @return  a String

**)
function TExceptionHandling.GetMethodName: String;
begin
  Result := FMethodName;
end;

(**

  This is a getter method for the HasPush property.

  @precon  None.
  @postcon Returns whether the method / property has a push statement.

  @return  a String

**)
function TExceptionHandling.GetPushName: string;
begin
  Result := FPushName;
end;

(**

  This is a getter method for the PushParams property.

  @precon  None.
  @postcon Returns a string list of push parameters.

  @return  a TStringList

**)
function TExceptionHandling.GetPushParams: TStringList;
begin
  Result := FPushParams;
end;

(**

  This is a setter method for the ExitCol property.

  @precon  None.
  @postcon Sets the Exit Column Number.

  @param   iCol as an Integer

**)
procedure TExceptionHandling.SetExitCol(iCol: Integer);
begin
  FExitCol := iCol;
end;

(**

  This is a setter method for the ExitLine property.

  @precon  None.
  @postcon Sets the Exit Line Number.

  @param   iLine as an Integer

**)
procedure TExceptionHandling.SetExitLine(iLine: Integer);
begin
  FExitLine := iLine;
end;

(**

  This is a setter method for the HasErrorHnd property.

  @precon  None.
  @postcon Sets whether the method / property has an error handler.

  @param   boolValue as a Boolean

**)
procedure TExceptionHandling.SetHasErrorHnd(boolValue: Boolean);
begin
  FHasErrorHnd := boolValue;
end;

(**

  This is a setter method for the HasExit property.

  @precon  None.
  @postcon Sets whether the method / property has an exit statement.

  @param   boolValue as a Boolean

**)
procedure TExceptionHandling.SetHasExit(boolValue: Boolean);
begin
  FHasExit := boolValue;
end;

(**

  This is a setter method for the HasPop property.

  @precon  None.
  @postcon Sets whether the method / property has a pop statement.

  @param   boolValue as a Boolean

**)
procedure TExceptionHandling.SetHasPop(boolValue: Boolean);
begin
  FHasPop := boolValue;
End;

(**

  This is a setter method for the HasPush property.

  @precon  None.
  @postcon Sets whether the method / property has a push statement.

  @param   boolValue as a Boolean

**)
procedure TExceptionHandling.SetHasPush(boolValue: Boolean);
begin
  FHasPush := boolValue;
end;

(**

  This is a setter method for the PushName property.

  @precon  None.
  @postcon Sets the method / property push name.

  @param   strValue as a String

**)
procedure TExceptionHandling.SetPushName(strValue: string);
begin
  FPushName := strValue;
end;

End.
