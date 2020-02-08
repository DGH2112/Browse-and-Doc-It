(**
  
  This module contains a class that implements the IBADIDocIssueTotals interface for storing the
  document issues totals for a module.

  @Author  David Hoyle
  @Version 1.111
  @Date    08 Feb 2020
  
**)
Unit BADI.DocIssueTotals;

Interface

Uses
  BADI.Types,
  BADI.Interfaces;

Type
  (** A class to implement the IBADIDocIssueTotals. **)
  TBADIDocIssueTotals = Class(TInterfacedObject, IBADIDocIssueTotals)
  Strict Private
    FTotals : Array[TLimitType] Of Integer;
  Strict Protected
    // IBADIDocIssueTotals
    Function  GetTotals(Const eLimitType: TLimitType): Integer;
    Procedure IncDocIssue(Const eDocIssueType: TLimitType);
    Procedure Clear;
  Public
  End;

Implementation

(**

  This method resets the totals for all doc issues to zero.

  @precon  None.
  @postcon The totals are reset to zero.

**)
Procedure TBADIDocIssueTotals.Clear;

Var
  eDocIssueTotal: TLimitType;

Begin
  For eDocIssueTotal := Low(TLimitType) To High(TLimitType) Do
    FTotals[eDocIssueTotal] := 0;
End;

(**

  This is a getter method for the Totals property.

  @precon  None;
  @postcon Returns the total for the given doc issue type.

  @param   eLimitType as a TLimitType as a constant
  @return  an Integer

**)
Function TBADIDocIssueTotals.GetTotals(Const eLimitType: TLimitType): Integer;

Begin
  Result := FTotals[eLimitType];
End;

(**

  This method increments the counter for the given doc issue type.

  @precon  None.
  @postcon The given doc issue type is incremented.

  @param   eDocIssueType as a TLimitType as a constant

**)
Procedure TBADIDocIssueTotals.IncDocIssue(Const eDocIssueType: TLimitType);

Begin
  Inc(FTotals[eDocIssueType]);
End;

End.
