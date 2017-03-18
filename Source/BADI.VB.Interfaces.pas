(**

  This module contains interfaces for use with the VB parser.

  @Author  David Hoyle
  @Version 1.0
  @date    18 Mar 2017

**)
Unit BADI.VB.Interfaces;

Interface

Uses
  Classes;

Type
  (** An interface to define exception handling capabilties which are
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
      This property returns the name of the method with the ExceptionHandling.
      @precon  None.
      @postcon R
      @return  a String
    **)
    Property MethodName : String Read GetMethodName;
  End;

Implementation

End.
