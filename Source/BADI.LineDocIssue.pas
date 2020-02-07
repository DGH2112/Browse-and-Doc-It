(**
  
  This module contains a class which implements the IBADILineDocIssue interface for storing information
  about issues on a line.

  @Author  David Hoyle
  @Version 1.227
  @Date    07 Feb 2020
  
**)
Unit BADI.LineDocIssue;

Interface

Uses
  BADI.Interfaces,
  BADI.Types;

Type
  (** A class which implements the IBADILineDocIssues interface. **)
  TBADILineDocIssue = Class(TInterfacedObject, IBADILineDocIssues)
  Strict Private
    FLimits : TLimitTypes;
    FMsgs : Array[TLimitType] Of String;
  Strict Protected
    Procedure AddIssue(Const eLimitType: TLimitType; Const strMsg: String);
    Function GetLimitTypes: TLimitTypes;
    Function GetMessage(Const eLimitType: TLimitType): String;
  Public
    Constructor Create(Const eLimitType : TLimitType; Const strMsg : String);
  End;

Implementation

(**

  This method adds a document issue type and message to the class.

  @precon  None.
  @postcon The document issue type and message are stored for later retrieval.

  @param   eLimitType as a TLimitType as a constant
  @param   strMsg     as a String as a constant

**)
Procedure TBADILineDocIssue.AddIssue(Const eLimitType: TLimitType; Const strMsg: String);

Begin
  Include(FLimits, eLimitType);
  FMsgs[eLimitType] := strMsg;
End;

(**

  A constructor for the TBADILineDocIssues class.

  @precon  None.
  @postcon Initialises the class with the first issue type and message.

  @param   eLimitType as a TLimitType as a constant
  @param   strMsg     as a String as a constant

**)
Constructor TBADILineDocIssue.Create(Const eLimitType: TLimitType; Const strMsg: String);

Begin
  AddIssue(eLimitType, strMsg);
End;

(**

  This is a getter method for the LimitTypes property.

  @precon  None.
  @postcon Returns a set of the doc iissue limit types for the line.

  @return  a TLimitTypes

**)
Function TBADILineDocIssue.GetLimitTypes: TLimitTypes;

Begin
  Result := FLimits;
End;

(**

  This is a getter method for the Message property.

  @precon  None.
  @postcon Returns the message for the given doc issue limit type.

  @param   eLimitType as a TLimitType as a constant
  @return  a String

**)
Function TBADILineDocIssue.GetMessage(Const eLimitType: TLimitType): String;

Begin
  Result := FMsgs[eLimitType];
End;

End.
