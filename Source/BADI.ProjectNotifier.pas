(**

  This module contains a project notifier to track when a file is saved so that we can update the date
  and version number before the file is saved.

  @Author  David Hoyle
  @Version 1.184
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
Unit BADI.ProjectNotifier;

Interface

Uses
  ToolsAPI,
  BADI.Interfaces,
  BADI.ModuleNotifier;

Type
  (** This is an IOTAProjectNotifier which is derived from the TBADIModuleNotifier. **)
  TBADIProjectNotifier = Class(TBADIModuleNotifier, IOTAProjectNotifier)
  Strict Private
  Strict Protected
    // IOTAProjectNotifier
    Procedure ModuleAdded(Const AFileName: String);
    Procedure ModuleRemoved(Const AFileName: String);
    Procedure ModuleRenamed(Const AOldFileName, ANewFileName: String); Overload;
  Public
    Constructor Create(Const ModuleStatsList : IBADIModuleStatsList; Const strFileName : String;
      Const ModuleRenameEvent: TBADIModuleRenameEvent);
    Destructor Destroy; Override;
  End;

Implementation

{$IFDEF DEBUG}
Uses
  CodeSiteLogging;
  {$ENDIF DEBUG}

(**

  A constructor for the TBADIProjectNotifier class.

  @precon  ModuleStatsList and ModuleRenameEvents must be valid instances.
  @postcon Calls the inherited method.

  @param   ModuleStatsList   as an IBADIModuleStatsList as a constant
  @param   strFileName       as a String as a constant
  @param   ModuleRenameEvent as a TBADIModuleRenameEvent as a constant

**)
Constructor TBADIProjectNotifier.Create(Const ModuleStatsList: IBADIModuleStatsList;
  Const strFileName: String; Const ModuleRenameEvent: TBADIModuleRenameEvent);
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create(ModuleStatsList, strFileName, ModuleRenameEvent);
End;

(**

  A destructor for the TBADIProjectNotfiier class.

  @precon  None.
  @postcon Used for CodeSite tracing only.

**)
Destructor TBADIProjectNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;

(**

  This method is called when a module is added to the project.

  @precon  None.
  @postcon Does nothing.

  @nohint  AFilename
  @nocheck EmptyMethod

  @param   AFileName as a String as a constant

**)
Procedure TBADIProjectNotifier.ModuleAdded(Const AFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleAdded', tmoTiming);{$ENDIF}
End;

(**

  This method is called when a module is removed from the project.

  @precon  None.
  @postcon Does nothing.

  @nohint  AFilename
  @nocheck EmptyMethod

  @param   AFileName as a String as a constant

**)
Procedure TBADIProjectNotifier.ModuleRemoved(Const AFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRemoved', tmoTiming);{$ENDIF}
End;

(**

  This method is called when the project is renamed.

  @precon  None.
  @postcon Ensures the rename event is fired to update the dictionary lists.

  @param   AOldFileName as a String as a constant
  @param   ANewFileName as a String as a constant

**)
Procedure TBADIProjectNotifier.ModuleRenamed(Const AOldFileName, ANewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRenamed', tmoTiming);{$ENDIF}
  If Assigned(ModuleRenameEvent) Then
    ModuleRenameEvent(AOldFileName, ANewFileName);
  FileName := ANewFileName;
End;

End.




