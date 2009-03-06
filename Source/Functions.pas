(**
  
  This method contains functions that are used global through out the application.

  @Version 1.0
  @Author  David Hoyle.
  @Date    06 Mar 2009

**)
Unit Functions;

Interface

Uses
  SysUtils, Dialogs;

Type
  (** An enumerate to determine whether a file is saved or locked (read only) **)
  TStatus = (msSaved, msLocked);
  (** A set to determine the saved / locked state of a file. **)
  TStatuses = Set Of TStatus;

  Procedure DisplayException(strMsg : String); Overload;
  Procedure DisplayException(strMsg : String; Const Params : Array Of Const); Overload;

Implementation

(**

  This procedure displays a exception message then aborts.

  @precon  None.
  @postcon Displays a exception message then aborts.

  @param   strMsg as a String

**)
Procedure DisplayException(strMsg : String);

Begin
  ShowMessage('Exception:'#13#10#13#10 + StrMsg);
End;

(**

  This procedure displays a formatted exception message then aborts.

  @precon  None.
  @postcon Displays a formatted exception message then aborts.

  @param   strMsg as a String
  @param   Params as an Array Of Const

**)
Procedure DisplayException(strMsg : String; Const Params : Array Of Const);

Begin
  ShowMessage(Format('Exception:'#13#10#13#10 + StrMsg, Params));
End;

End.
