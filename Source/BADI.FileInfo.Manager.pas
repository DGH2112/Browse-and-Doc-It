(**
  
  This module contains a class to manage module update information so that the refreshing of the views
  only needs to consider changed modules.

  @Author  David Hoyle
  @Version 1.111
  @Date    28 Aug 2020
  
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
Unit BADI.FileInfo.Manager;

Interface

Uses
  System.Generics.Collections;

Type
  (** This class managed a list of file name and their last modified date so that the view only
      updated modules that have changed. **)
  TBADIFileInfoManager = Class
  Strict Private
    Type
      (** A record to describe the information stored in the collection. **)
      TBADIModuleUpdateRecord = Record
        FFileName    : String;
        FLastUpdated : TDateTime;
      End;
    Strict Private
      FFileInfo : TList<TBADIModuleUpdateRecord>;
  Strict Protected
    Function  Find(Const strFileName : String) : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Const strFileName : String; Const dtFileDate : TDateTime);
    Function  ShouldUpdate(Const strFileName : String; Const dtDateTime : TDateTime) : Boolean;
    Procedure Clear;
  End;

Implementation

Uses
  System.SysUtils;

(**

  This method either adds a new filename / date record to the collection if it does not exists else
  updated the date of the existing file record.

  @precon  None.
  @postcon Either a new record is added else and existing one is updated.

  @param   strFileName as a String as a constant
  @param   dtFileDate  as a TDateTime as a constant

**)
Procedure TBADIFileInfoManager.Add(Const strFileName: String; Const dtFileDate: TDateTime);

Var
  iIndex : Integer;
  recFileInfo : TBADIModuleUpdateRecord;
  
Begin
  iIndex := Find(strFileName);
  If iIndex < 0 Then
    Begin
      recFileInfo.FFileName := strFileName;
      recFileInfo.FLastUpdated := dtFileDate;
      FFileInfo.Add(recFileInfo);
    End Else
    Begin
      recFileInfo := FFileInfo[iIndex];
      recFileInfo.FLastUpdated := dtFileDate;
      FFileInfo[iIndex] := recFileInfo;
    End;
End;

(**

  This method clears the file collection.

  @precon  None.
  @postcon The file collection is empty.

**)
Procedure TBADIFileInfoManager.Clear;

Begin
  FFileInfo.Clear;
End;

(**

  A constructor for the TBADIFileInfoManager class.

  @precon  None.
  @postcon Creates an empty collection.

**)
Constructor TBADIFileInfoManager.Create;

Begin
  FFileInfo := TList<TBADIModuleUpdateRecord>.Create;
End;

(**

  A destructor for the TBADIFileInfoManager class.

  @precon  None.
  @postcon Frees the memory used by the collection.

**)
Destructor TBADIFileInfoManager.Destroy;

Begin
  FFileInfo.Free;
  Inherited Destroy;
End;

(**

  This method used a sequential search to find an existing record with the given filename and returns
  its index if found else return -1.

  @precon  None.
  @postcon Returns the index of an existing record with the files name else returns -1.

  @param   strFileName as a String as a constant
  @return  an Integer

**)
Function TBADIFileInfoManager.Find(Const strFileName: String): Integer;

Var
  iFile : Integer;
  
Begin
  Result :=  -1;
  For iFile := 0 To FFileInfo.Count - 1 Do
    If CompareText(strFileName, FFileInfo[iFile].FFileName) = 0 Then
      Begin
        Result := iFile;
        Break;
      End;
End;

(**

  This method determines whether a module with a given filename needs to be updated.

  @precon  None.
  @postcon Returns true if the given filename is newer than the one stored against the filename in the
           collection.

  @param   strFileName as a String as a constant
  @param   dtDateTime  as a TDateTime as a constant
  @return  a Boolean

**)
Function TBADIFileInfoManager.ShouldUpdate(Const strFileName: String;
  Const dtDateTime: TDateTime): Boolean;

Var
  iIndex : Integer;
  
Begin
  Result :=  True;
  iIndex := Find(strFileName);
  If iIndex >= 0 Then
    Result := dtDateTime > FFileInfo[iIndex].FLastUpdated;
End;

End.
