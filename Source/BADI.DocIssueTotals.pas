(**
  
  This module contains a class that implements the IBADIDocIssueTotals interface for storing the
  document issues totals for a module.

  @Author  David Hoyle
  @Version 1.199
  @Date    09 Feb 2020
  
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
