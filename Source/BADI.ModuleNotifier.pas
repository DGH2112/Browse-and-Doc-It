(**

  This module contains a module notifier to track when a file is saved so that we can update the date
  and version number before the file is saved.

  @Author  David Hoyle
  @Version 1.01
  @Date    29 Jan 2020

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
Unit BADI.ModuleNotifier;

Interface

Uses
  ToolsAPI,
  BADI.Interfaces;

Type
  (** A class which implements the IOTAModuleNotifier interfaces. **)
  TBADIModuleNotifier = Class(TNotifierObject, IInterface, IOTANotifier, IOTAModuleNotifier80,
    IOTAModuleNotifier90, IOTAModuleNotifier)
  Strict Private
    FFileName          : String;
    FModuleRenameEvent : TBADIModuleRenameEvent;
  Strict Protected
    // IOTAModuleNotifier
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
    Function  CheckOverwrite: Boolean;
    Procedure ModuleRenamed(Const NewName: String);
    // IOTAModuleNotifier80
    Function AllowSave: Boolean;
    Function GetOverwriteFileNameCount: Integer;
    Function GetOverwriteFileName(Index: Integer): String;
    Procedure SetSaveFileName(Const FileName: String);
    // IOTAModuleNotifier90
    Procedure BeforeRename(Const OldFileName, NewFileName: String);
    Procedure AfterRename(Const OldFileName, NewFileName: String);
  Public
    Constructor Create(Const strFileName : String; Const ModuleRenameEvent: TBADIModuleRenameEvent);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  BADI.ToolsAPIUtils;

(**

  This method is called after a file has been renamed.

  @precon  None.
  @postcon Updated the internal filename.

  @param   OldFileName as a String as a constant
  @param   NewFileName as a String as a constant

**)
Procedure TBADIModuleNotifier.AfterRename(Const OldFileName, NewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterRename', tmoTiming);{$ENDIF}
  FFileName := NewFileName;
  If Assigned(FModuleRenameEvent) Then
    FModuleRenameEvent(OldFileName, NewFileName);
End;

(**

  This method is called after a file has been saved.

  @precon  None.
  @postcon This method resets the module size counter.

**)
Procedure TBADIModuleNotifier.AfterSave;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterSave', tmoTiming);{$ENDIF}
  //: @todo Reset module file size counter here!
End;

(**

  This method is called to check whether a file can be saved.

  @precon  None.
  @postcon Returns true to ensure all files are saved.

  @return  a Boolean

**)
Function TBADIModuleNotifier.AllowSave: Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AllowSave', tmoTiming);{$ENDIF}
  Result := True;
End;

(**

  This method is called before a file is renamed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod 
  @nohint  OldFileName NewFileName

  @param   OldFileName as a String as a constant
  @param   NewFileName as a String as a constant

**)
Procedure TBADIModuleNotifier.BeforeRename(Const OldFileName, NewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeRename', tmoTiming);{$ENDIF}
End;

(**

  This method is called before a file is saved.

  @precon  None.
  @postcon Here are check whether the module date and version should be updated.

**)
Procedure TBADIModuleNotifier.BeforeSave;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeSave', tmoTiming);{$ENDIF}
  //: @todo Implement updating the date and version here!
End;

(**

  This method is called to see if an check should be done for overwriting readonly files.

  @precon  None.
  @postcon Returns true to ensure this is done.

  @return  a Boolean

**)
Function TBADIModuleNotifier.CheckOverwrite: Boolean;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CheckOverwrite', tmoTiming);{$ENDIF}
  Result := True;
End;

(**

  A constructor for the TBADIMOduleNotifier class.

  @precon  None.
  @postcon Stores the module filenamer and the module rename event.

  @param   strFileName       as a String as a constant
  @param   ModuleRenameEvent as a TBADIModuleRenameEvent as a constant

**)
Constructor TBADIModuleNotifier.Create(Const strFileName : String;
  Const ModuleRenameEvent: TBADIModuleRenameEvent);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FFileName := strFileName;
  FModuleRenameEvent := ModuleRenameEvent;
End;

(**

  A destructor for the TBADIModuleNotifier class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TBADIModuleNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method is called when the notifier is destroyed.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod

**)
Procedure TBADIModuleNotifier.Destroyed;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroyed', tmoTiming);{$ENDIF}
End;

(**

  This is a getter method for the OverwriteFileName property.

  @precon  None.
  @postcon Not Used.

  @nocheck MissingCONSTInParam
  @nohint  Index

  @param   Index as an Integer
  @return  a String

**)
Function TBADIModuleNotifier.GetOverwriteFileName(Index: Integer): String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetOverwriteFileName', tmoTiming);{$ENDIF}
  Result := '';
End;

(**

  This is a getter method for the OverwriteFilenameCount property.

  @precon  None.
  @postcon Not Used.

  @return  an Integer

**)
Function TBADIModuleNotifier.GetOverwriteFileNameCount: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'GetOverwriteFileNameCount', tmoTiming);{$ENDIF}
  Result := 0;
End;

(**

  This method is called when a module is modified.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod

**)
Procedure TBADIModuleNotifier.Modified;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Modified', tmoTiming);{$ENDIF}
End;

(**

  This method is called when a module is renamed.

  @precon  None.
  @postcon Calls the Module Rename Event.

  @param   NewName as a String as a constant

**)
Procedure TBADIModuleNotifier.ModuleRenamed(Const NewName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRenamed', tmoTiming);{$ENDIF}
  If Assigned(FModuleRenameEvent) Then
    FModuleRenameEvent(FFileName, NewName);
  FFileName := NewName;
End;

(**

  This is a setter method for the SaveFileName property.

  @precon  None.
  @postcon Not Used.

  @nocheck EmptyMethod
  @nohint  FileName

  @param   FileName as a String as a constant

**)
Procedure TBADIModuleNotifier.SetSaveFileName(Const FileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SetSaveFileName', tmoTiming);{$ENDIF}
End;

End.
