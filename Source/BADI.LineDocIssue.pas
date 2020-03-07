(**
  
  This module contains a class which implements the IBADILineDocIssue interface for storing information
  about issues on a line.

  @Author  David Hoyle
  @Version 1.315
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
