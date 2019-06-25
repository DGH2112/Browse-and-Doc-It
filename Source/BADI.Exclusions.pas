(**
  
  This module contains a class for handling the exclusion for documentation, metrics and checks.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

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
Unit BADI.Exclusions;

Interface

Uses
  RegularExpressions,
  Generics.Collections,
  BADI.Types,
  BADI.Interfaces;

Type
  (** A class which implements the IBADIExclusions interface for managing exclusions for documentation,
      metrics and checks. **)
  TBADIExclusions = Class(TInterfacedObject, IBADIExclusions)
  Strict Private
    FExclusions      : TList<TBADIExclusionRec>;
    FExclusionRegExs : TList<TRegEx>;
  Strict Protected
    // IBADIExclusions
    Function  GetCount: Integer;
    Function  GetExclusion(Const iIndex: Integer): TBADIExclusionRec;
    Procedure SetExclusion(Const iIndex: Integer; Const recValue: TBADIExclusionRec);
    Function  ShouldExclude(Const strFileName: String;
      Const eExclusionType: TBADIExclusionType): Boolean;
    // General Methods
    Procedure UpdateRegEx(Const iIndex : Integer; Const strPattern: String);
    Procedure Add(Const recExclusion: TBADIExclusionRec);
    Procedure Delete(Const iIndex: Integer);
    Procedure Clear;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

(**

  This method adds an exclusion to the collection.

  @precon  None.
  @postcon The item is added to the collection.

  @param   recExclusion as a TBADIExclusionRec as a constant

**)
Procedure TBADIExclusions.Add(Const recExclusion: TBADIExclusionRec);

Begin
  FExclusions.Add(recExclusion);
  FExclusionRegExs.Add(TRegEx.Create(recExclusion.FExclusionPattern, [roIgnoreCase, roCompiled,
    roSingleLine]));
End;

(**

  This method clears the exclusions and regular expression lists.

  @precon  None.
  @postcon Both the rexclusions and reg ex list are empty.

**)
Procedure TBADIExclusions.Clear;

Begin
  FExclusions.Clear;
  FExclusionRegExs.Clear;
End;

(**

  A constructor for the TBADIExclusions class.

  @precon  None.
  @postcon Allocates memory for the collection and regular expressions.

**)
Constructor TBADIExclusions.Create;

Begin
  FExclusions := TList<TBADIExclusionRec>.Create;
  FExclusionRegExs := TList<TRegEx>.Create;
End;

(**

  This method deletes the indexed exclusion (and regex) from the collection.

  @precon  iIndex must be between 0 and GetCount -  1.
  @postcon The item is deleted from the collection.

  @param   iIndex as an Integer as a constant

**)
Procedure TBADIExclusions.Delete(Const iIndex: Integer);

Begin
  FExclusions.Delete(iIndex);
  FExclusionRegExs.Delete(iIndex);
End;

(**

  A destructor for the TBADIExclusions class.

  @precon  None.
  @postcon Frees the allocated memory for the collection.

**)
Destructor TBADIExclusions.Destroy;

Begin
  FExclusions.Free;
  FExclusionRegExs.Free;
  Inherited;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of items in the collection.

  @return  an Integer

**)
Function TBADIExclusions.GetCount: Integer;

Begin
  Result := FExclusions.Count;
End;

(**

  This is a getter method for the Exclusion property.

  @precon  iIndex must be between 0 and GetCount - 1.
  @postcon Returns the indexed exclusion record.

  @param   iIndex as an Integer as a constant
  @return  a TBADIExclusionRec

**)
Function TBADIExclusions.GetExclusion(Const iIndex: Integer): TBADIExclusionRec;

Begin
  Result := FExclusions[iIndex];
End;

(**

  This is a setter method for the Exclusion property.

  @precon  iIndex must be a valud between 0 and GetCount - 1.
  @postcon The exclusion is updated to the geiven exclusions record values.

  @param   iIndex   as an Integer as a constant
  @param   recValue as a TBADIExclusionRec as a constant

**)
Procedure TBADIExclusions.SetExclusion(Const iIndex: Integer; Const recValue: TBADIExclusionRec);

Begin
  FExclusions[iIndex] := recValue;
  UpdateRegEx(iIndex, recValue.FExclusionPattern);
End;

(**

  This method test whether the given filename and exclusion type should be excluded from being reported.

  @precon  None.
  @postcon Returns true if the filename and exclusion type should not be reported.

  @param   strFileName    as a String as a constant
  @param   eExclusionType as a TBADIExclusionType as a constant
  @return  a Boolean

**)
Function TBADIExclusions.ShouldExclude(Const strFileName: String;
  Const eExclusionType: TBADIExclusionType): Boolean;

Var
  iRegEx : Integer;
  
Begin
  Result := False;
  For iRegEx := 0 To FExclusions.Count - 1 Do
    If eExclusionType In FExclusions[iRegEx].FExclusions Then
      If FExclusionRegExs[iRegEx].IsMatch(strFileName) Then
        Begin
          Result := True;
          Break;
        End;
End;

(**

  This method updates the regular expression for the given exclusion index.

  @precon  iIndex must be between 0 and GetCount - 1.
  @postcon The regular expression is updated.

  @param   iIndex     as an Integer as a constant
  @param   strPattern as a String as a constant

**)
Procedure TBADIExclusions.UpdateRegEx(Const iIndex : Integer; Const strPattern: String);

Begin
  FExclusionRegExs[iIndex] := TRegEx.Create(strPattern, [roIgnoreCase, roCompiled, roSingleLine]);
End;

End.
