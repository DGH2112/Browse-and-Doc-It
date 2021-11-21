(**

  This module contains interfaces for use with the VB parser.

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
Unit BADI.VB.Interfaces;

Interface

Uses
  Classes;

Type
  (** An interface to define exception handling capabilities which are
      implemented by methods and properties. **)
  IExceptionHandling = Interface
    Function GetHasPush : Boolean;
    Procedure SetHasPush(boolValue : Boolean);
    Function GetHasPop : Boolean;
    Procedure SetHasPop(boolValue : Boolean);
    Function GetPushName : String;
    Procedure SetPushName(strValue : String);
    Function GetPushParams : TStringList;
    Function GetHasErrorHnd : Boolean;
    Procedure SetHasErrorHnd(boolValue : Boolean);
    Function GetHasExit : Boolean;
    Procedure SetHasExit(boolValue : Boolean);
    Function GetExitLine : Integer;
    Procedure SetExitLine(iLine : Integer);
    Function GetExitCol : Integer;
    Procedure SetExitCol(iCol : Integer);
    Function GetMethodName : String;
    (**
      This property determine if the method has a Exception.Push handler.
      @precon  None.
      @postcon Determine if the method has a Exception.Push handler.
      @return  a Boolean
    **)
    Property HasPush : Boolean Read GetHasPush Write SetHasPush;
    (**
      This property determine if the method has a Exception.Pop handler.
      @precon  None.
      @postcon Determine if the method has a Exception.Pop handler.
      @return  a Boolean
    **)
    Property HasPop  : Boolean Read GetHasPop  Write SetHasPop;
    (**
      This property returns the name of the pushed method.
      @precon  None.
      @postcon Returns the name of the pushed method.
      @return  a String
    **)
    Property PushName : String Read GetPushName Write SetPushName;
    (**
      This property returns a string list of the methods parameters.
      @precon  None.
      @postcon Returns a string list of the methods parameters.
      @return  a TStringList
    **)
    Property PushParams : TStringList Read GetPushParams;
    (**
      This property determines if the method has an error handler.
      @precon  None.
      @postcon Determines if the method has an error handler.
      @return  a Boolean
    **)
    Property HasErrorHnd : Boolean Read GetHasErrorHnd Write SetHasErrorHnd;
    (**
      This property determines if the method has an exit statement.
      @precon  None.
      @postcon Determines if the method has an exit statement.
      @return  a Boolean
    **)
    Property HasExit : Boolean Read GetHasExit Write SetHasExit;
    (**
      This property determines the method line of the exit statement.
      @precon  None.
      @postcon Determines the method line of the exit statement.
      @return  a Integer
    **)
    Property ExitLine : Integer Read GetExitLine Write SetExitLine;
    (**
      This property determines the method column of the exit statement.
      @precon  None.
      @postcon Determines the method column of the exit statement.
      @return  a Integer
    **)
    Property ExitCol : Integer Read GetExitCol Write SetExitCol;
    (**
      This property returns the name of the method with the Exception Handling.
      @precon  None.
      @postcon R
      @return  a String
    **)
    Property MethodName : String Read GetMethodName;
  End;

Implementation

End.
