(**
  
  This module contains a class which implements the IBADILineDocIssue interface for storing information
  about issues on a line.

  @Author  David Hoyle
  @Version 1.540
  @Date    13 Apr 2020
  
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
  System.Generics.Collections,
  BADI.Interfaces,
  BADI.Types;

Type
  (** A class which implements the IBADILineDocIssues interface. **)
  TBADILineDocIssue = Class(TInterfacedObject, IBADILineDocIssues)
  Strict Private
    FDocIssues : TDictionary<String, TBADIDocIssueInfo>;
  Strict Protected
    Procedure AddIssue(Const strDocIssueType : String; Const DocIssueInfo : TBADIDocIssueInfo);
    Function GetLimitTypes: TArray<String>;
    Function GetMessage(Const strDocIssueType : String): TBADIDocIssueInfo;
  Public
    Constructor Create(Const strDocIssueType : String; Const DocIssueInfo : TBADIDocIssueInfo);
    Destructor Destroy; Override;
  End;

Implementation

(**

  This method adds a document issue type and message to the class.

  @precon  None.
  @postcon The document issue type and message are stored for later retrieval.

  @param   strDocIssueType as a String as a constant
  @param   DocIssueInfo    as a TBADIDocIssueInfo as a constant

**)
Procedure TBADILineDocIssue.AddIssue(Const strDocIssueType : String;
  Const DocIssueInfo : TBADIDocIssueInfo);

Begin
  If Not FDocIssues.ContainsKey(strDocIssueType) Then
    FDocIssues.Add(strDocIssueType, DocIssueInfo);
End;

(**

  A constructor for the TBADILineDocIssues class.

  @precon  None.
  @postcon Initialises the class dictionary with the first issue type and message.

  @param   strDocIssueType as a String as a constant
  @param   DocIssueInfo    as a TBADIDocIssueInfo as a constant

**)
Constructor TBADILineDocIssue.Create(Const strDocIssueType : String;
  Const DocIssueInfo : TBADIDocIssueInfo);

Begin
  FDocIssues := TDictionary<String, TBADIDocIssueInfo>.Create;
  FDocIssues.Add(strDocIssueType, DocIssueInfo);
End;

(**

  A destructor for the TBADILineDocIssue class.

  @precon  None.
  @postcon Frees the dictionary.

**)
Destructor TBADILineDocIssue.Destroy;

Begin
  FDocIssues.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the LimitTypes property.

  @precon  None.
  @postcon Returns a set of the doc iissue limit types for the line.

  @return  a TArray<String>

**)
Function TBADILineDocIssue.GetLimitTypes: TArray<String>;

Var
  iKey: Integer;
  Issue: TPair<String, TBADIDocIssueInfo>;

Begin
  SetLength(Result, FDocIssues.Count);
  iKey := 0;
  For Issue In FDocIssues Do
    Begin
      Result[iKey] := Issue.Key;
      Inc(iKey);
    End;
End;

(**

  This is a getter method for the Message property.

  @precon  None.
  @postcon Returns the message for the given doc issue limit type.

  @param   strDocIssueType as a String as a constant
  @return  a TBADIDocIssueInfo

**)
Function TBADILineDocIssue.GetMessage(Const strDocIssueType : String): TBADIDocIssueInfo;

Begin
  FDocIssues.TryGetValue(strDocIssueType, Result);
End;

End.
