(**
  
  This module contains a class which implements the IBADILineDocIssue interface for storing information
  about issues on a line.

  @Author  David Hoyle
  @Version 1.704
  @Date    01 Aug 2020
  
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
    FSpellingMistakes : TList<TBADISpellingMistake>;
  Strict Protected
    Function  GetLimitTypes: TArray<String>;
    Function  GetSpellingMistakeCount : Integer;
    Function  GetSpellingMistake(Const iIndex : Integer) : TBADISpellingMistake;
    Function  GetMessage(Const strDocIssueType : String): TBADIDocIssueInfo;
    Procedure AddIssue(Const strDocIssueType : String; Const DocIssueInfo : TBADIDocIssueInfo);
    Procedure AddSpellingMistake(Const strWord : String; Const iColumn : Integer);
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

  This method adds a spelling mistake to the collection.

  @precon  None.
  @postcon The spelling mistake (word and column) are added to the collection.

  @param   strWord as a String as a constant
  @param   iColumn as an Integer as a constant

**)
Procedure TBADILineDocIssue.AddSpellingMistake(Const strWord: String; Const iColumn: Integer);

Var
  R : TBADISpellingMistake;
  
Begin
  R.FWord := strWord;
  R.FColumn := iColumn;
  FSpellingMistakes.Add(R);
End;

(**

  A constructor for the TBADILineDocIssue class.

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
  FSpellingMistakes := TList<TBADISpellingMistake>.Create;
End;

(**

  A destructor for the TBADILineDocIssue class.

  @precon  None.
  @postcon Frees the dictionary.

**)
Destructor TBADILineDocIssue.Destroy;

Begin
  FSpellingMistakes.Free;
  FDocIssues.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Limit Types property.

  @precon  None.
  @postcon Returns a set of the doc issue limit types for the line.

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

(**

  This is a getter method for the Spelling Mistake property.

  @precon  iIndex must be between 0 and GetSpellingMistakeCount - 1.
  @postcon Returns the indexed spelling mistake.

  @param   iIndex as an Integer as a constant
  @return  a TBADISpellingMistake

**)
Function TBADILineDocIssue.GetSpellingMistake(Const iIndex: Integer): TBADISpellingMistake;

Begin
  Result := FSpellingMistakes[iIndex];
End;

(**

  This is a getter method for the Spelling Mistake Count property.

  @precon  None.
  @postcon Returns the number of spelling mistakes for the line.

  @return  an Integer

**)
Function TBADILineDocIssue.GetSpellingMistakeCount: Integer;

Begin
  Result := FSpellingMistakes.Count;
End;

End.
