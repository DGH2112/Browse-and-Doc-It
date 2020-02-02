(**
  
  This module contains a class which implements the IBADIModuleStatsList to manage a list of modules and
  their size and size deltas.

  @Author  David Hoyle
  @Version 1.009
  @Date    02 Feb 2020
  
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
Unit BADI.ModuleStatsList;

Interface

Uses
  System.Generics.Collections,
  BADI.Interfaces;


Type
  (** A class whicih implements the IBADIModuleStatsList interface. **)
  TBADIModuleStatsList = Class(TInterfacedObject, IBADIModuleStatsList)
  Strict Private
    FModuleList : TDictionary<String,IBADIModuleStats>;
  Strict Protected
    Function  GetModuleStats(Const strFileName: String): IBADIModuleStats;
    Procedure Rename(Const strOldFileName: String; Const strNewFileName: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  BADI.ModuleStats;

(**

  A constructor for the TBADIMOduleStatsList class.

  @precon  None.
  @postcon Creates a generic dictionary for holding a list of module filenames and their statistics.

**)
Constructor TBADIModuleStatsList.Create;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FModuleList := TDictionary<String,IBADIModuleStats>.Create;
End;

(**

  A destructor for the TBADIModuleStatsList class.

  @precon  None.
  @postcon Frees the dictionary and the module statistics.

**)
Destructor TBADIModuleStatsList.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FModuleList.Free;
  Inherited;
End;

(**

  This is a getter method for the ModuleStats property.

  @precon  None.
  @postcon Returns the statistics for the given filename. If the file name does not exist a new
           module statistics object is created and then returned.

  @param   strFileName as a String as a constant
  @return  an IBADIModuleStats

**)
Function TBADIModuleStatsList.GetModuleStats(Const strFileName: String): IBADIModuleStats;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetModuleStats', tmoTiming);{$ENDIF}
  If Not FModuleList.ContainsKey(strFileName) Then
    FModuleList.Add(strFileName, TBADIModuleStats.Create);
  Result := FModuleList[strFileName];
End;

(**

  This method renames the existing module statistics with a new filename key.

  @precon  None.
  @postcon The filename key is changed to the new filename.

  @param   strOldFileName as a String as a constant
  @param   strNewFileName as a String as a constant

**)
Procedure TBADIModuleStatsList.Rename(Const strOldFileName, strNewFileName: String);

Var
  MS: IBADIModuleStats;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Rename', tmoTiming);{$ENDIF}
  If CompareText(strOldFileName, strNewFileName) <> 0 Then
    If FModuleList.ContainsKey(strOldFileName) Then
      Begin
        MS := FModuleList[strOldFileName];
        FModuleList.Add(strNewFileName, MS);
        FModuleList.Remove(strOldFileName);
      End;
End;

End.


