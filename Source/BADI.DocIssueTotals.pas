(**
  
  This module contains a class that implements the IBADIDocIssueTotals interface for storing the
  document issues totals for a module.

  @Author  David Hoyle
  @Version 1.567
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
Unit BADI.DocIssueTotals;

Interface

Uses
  System.Generics.Collections,
  BADI.Types,
  BADI.Interfaces;

Type
  (** A class to implement the IBADIDocIssueTotals. **)
  TBADIDocIssueTotals = Class(TInterfacedObject, IBADIDocIssueTotals)
  Strict Private
    FTotals : TDictionary<String, TBADITotalInfo>;
  Strict Protected
    // IBADIDocIssueTotals
    Function  GetTotals : TDictionary<String, TBADITotalInfo>;
    Function  ContainsAny(Const setDocIssues : TLimitTypes) : Boolean;
    Procedure IncDocIssue(Const strDocIssueType : String; Const TotalInfo : TBADITotalInfo);
    Procedure Clear;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  BADI.Constants;
  
(**

  This method resets the totals for all doc issues to zero.

  @precon  None.
  @postcon The totals are reset to zero.

**)

Procedure TBADIDocIssueTotals.Clear;

Begin
  FTotals.Clear;
End;

(**

  This method test whether any of the given values are keys in the totals dictionary.

  @precon  None.
  @postcon Returns true of any values are keys in the dictionary.

  @param   setDocIssues as a TLimitTypes as a constant
  @return  a Boolean

**)
Function TBADIDocIssueTotals.ContainsAny(Const setDocIssues: TLimitTypes): Boolean;

Var
  eDocIssue : TLimitType;
  
Begin
  Result := False;
  For eDocIssue := Low(TLimitType) To High(TLimitType) Do
    If (eDocIssue In setDocIssues) And FTotals.ContainsKey(astrLimitType[eDocIssue]) Then
      Begin
        Result := True;
        Break;
      End;
End;

(**

  A constructor for the TBADIDocIssueTotals class.

  @precon  None.
  @postcon Creates a dictionary to store the values.

**)
Constructor TBADIDocIssueTotals.Create;

Begin
  FTotals := TDictionary<String, TBADITotalInfo>.Create;
End;

(**

  A destructor for the TBADIDocIssueTotals class.

  @precon  None.
  @postcon Frees the dictionary.

**)
Destructor TBADIDocIssueTotals.Destroy;

Begin
  FTotals.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Totals property.

  @precon  None;
  @postcon Returns the total for the given doc issue type.

  @return  a TDictionary<String, TBADITotalInfo>

**)
Function TBADIDocIssueTotals.GetTotals : TDictionary<String, TBADITotalInfo>;

Begin
  Result := FTotals;
End;

(**

  This method increments the counter for the given doc issue type.

  @precon  None.
  @postcon The given doc issue type is incremented.

  @param   strDocIssueType as a String as a constant
  @param   TotalInfo       as a TBADITotalInfo as a constant

**)
Procedure TBADIDocIssueTotals.IncDocIssue(Const strDocIssueType : String;
  Const TotalInfo : TBADITotalInfo);

Var
  recValue: TBADITotalInfo;

Begin
  If FTotals.TryGetValue(strDocIssueType, recValue) Then
    Begin
      Inc(recValue.FCounter);
      FTotals.AddOrSetValue(strDocIssueType, recValue);
    End Else
    Begin
      recValue := TotalInfo;
      recValue.FCounter := 1;
      FTotals.Add(strDocIssueType, recValue);
    End;
End;

End.
