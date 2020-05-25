(**
  
  This module contains a class which implement the IOTAIDENOtifier interface so that BADI can hook
  module save events to see if there have been changes in the files.

  @Author  David Hoyle
  @Version 1.498
  @Date    09 May 2020
  
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
Unit BADI.IDENotifier;

Interface

Uses
  ToolsAPI,
  BADI.Interfaces;

Type
  (** A class which implements the IOTAIDENotifier interface. **)
  TBADIIDENotifier = Class(TNotifierObject, IInterface, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50,
    IOTAIDENotifier80)
  Strict Private
    Class Var
      (** An integer to hold the IDE Notifier Index. **)
      FIDENotifierIndex : Integer;
  Strict Private
    FModuleNotifiers   : IBADIModuleNotifierList;
    FProjectNotifiers  : IBADIModuleNotifierList;
    FModuleStatsList   : IBADIModuleStatsList;
    FEditViewNotifiers : IBADIModuleNotifierList;
  Strict Protected
    // IOTAIDENotifier
    Procedure AfterCompile(Succeeded: Boolean); Overload;
    Procedure BeforeCompile(Const Project: IOTAProject; Var Cancel: Boolean); OverLoad;
    Procedure FileNotification(NotifyCode: TOTAFileNotification; Const FileName: String;
      Var Cancel: Boolean);
    // IOTAIDENotifier50
    Procedure BeforeCompile(Const Project: IOTAProject; IsCodeInsight: Boolean;
      Var Cancel: Boolean); Overload;
    Procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); Overload;
    // IOTAIDENotifier80
    Procedure AfterCompile(Const Project: IOTAProject; Succeeded:
      Boolean; IsCodeInsight: Boolean); Overload;
    // General Methods
    Procedure InstallNotifier(Const M: IOTAModule; Const FileName: String);
    Procedure UninstallNotifier(Const M: IOTAModule; Const FileName: String);
    Procedure ModuleRenameEvent(Const strOldFileName, strNewFileName : String);
    Procedure InstallEditViewNotifier(Const M : IOTAModule);
    Procedure UninstallEditViewNotifier(Const M : IOTAModule);
  Public
    Constructor Create(Const ModuleStatsList : IBADIModuleStatsList);
    Destructor Destroy; Override;
    Class Procedure InstallIDENotifier(Const ModuleStatsList : IBADIModuleStatsList);
    Class Procedure UninstallIDENotifier;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  BADI.ModuleNotifierList,
  BADI.ModuleNotifier,
  BADI.ProjectNotifier,
  BADI.SourceEditorNotifier;

(**

  This method is called after a project is compiled.

  @precon  None.
  @postcon Not used.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  Succeeded

  @param   Succeeded as a Boolean

**)
Procedure TBADIIDENotifier.AfterCompile(Succeeded: Boolean);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterCompile', tmoTiming);{$ENDIF}
End;

(**

  This method is called after a project is compiled.

  @precon  None.
  @postcon Not used.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  Succeeded IsCodeInsight

  @param   Succeeded     as a Boolean
  @param   IsCodeInsight as a Boolean

**)
Procedure TBADIIDENotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterCompile', tmoTiming);{$ENDIF}
End;

(**

  This method is called after a project is compiled.

  @precon  None.
  @postcon Not used.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  Project Succeeded IsCodeInsight

  @param   Project       as an IOTAProject as a constant
  @param   Succeeded     as a Boolean
  @param   IsCodeInsight as a Boolean

**)
Procedure TBADIIDENotifier.AfterCompile(Const Project: IOTAProject; Succeeded, IsCodeInsight: Boolean);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'AfterCompile', tmoTiming);{$ENDIF}
End;

(**

  This method is called before a project is compiled.

  @precon  None.
  @postcon Not used.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  Project IsCodeInsight Cancel

  @param   Project       as an IOTAProject as a constant
  @param   IsCodeInsight as a Boolean
  @param   Cancel        as a Boolean as a reference

**)
Procedure TBADIIDENotifier.BeforeCompile(Const Project: IOTAProject; IsCodeInsight: Boolean;
  Var Cancel: Boolean);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeCompile', tmoTiming);{$ENDIF}
End;

(**

  This method is called before a project is compiled.

  @precon  None.
  @postcon Not used.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  Project Cancel

  @param   Project       as an IOTAProject as a constant
  @param   Cancel        as a Boolean as a reference

**)
Procedure TBADIIDENotifier.BeforeCompile(Const Project: IOTAProject; Var Cancel: Boolean);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeforeCompile', tmoTiming);{$ENDIF}
End;

(**

  A constructor for the TBADIIDENOtifier class.

  @precon  None.
  @postcon Creates a module notifier list to hold the module notifiers created.

  @param   ModuleStatsList as an IBADIModuleStatsList as a constant

**)
Constructor TBADIIDENotifier.Create(Const ModuleStatsList : IBADIModuleStatsList);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FModuleNotifiers := TBADIModuleNotifierList.Create;
  FProjectNotifiers := TBADIModuleNotifierList.Create;
  FModuleStatsList := ModuleStatsList;
  FEditViewNotifiers := TBADIModuleNotifierList.Create;
End;

(**

  A destructor for the TBADIIDENotifier class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TBADIIDENotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited;
End;

(**

  This method is called when a module is opened and closed (and a number of other things).

  @precon  None.
  @postcon When a module is opened a module notifier is installed to watch for module save events. The
           module notifier is removed when the module is closed.

  @nocheck MissingCONSTInParam

  @param   NotifyCode as a TOTAFileNotification
  @param   FileName   as a String as a constant
  @param   Cancel     as a Boolean as a reference

**)
Procedure TBADIIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; Const FileName: String;
  Var Cancel: Boolean);

Var
  MS : IOTAModuleServices;
  M: IOTAModule;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'FileNotification', tmoTiming);{$ENDIF}
  If Not Cancel And Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      Case NotifyCode Of
        ofnFileOpened:
          Begin
            M := MS.OpenModule(FileName);
            InstallNotifier(M, FileName);
            CodeSite.SendEnum(csmNote, ExtractFileName(M.FileName), TypeInfo(TOTAFileNotification),
              Ord(NotifyCode));
          End;
        ofnFileClosing:
          Begin
            M := MS.OpenModule(FileName);
            UninstallNotifier(M, Filename);
            CodeSite.SendEnum(csmNote, ExtractFileName(M.FileName), TypeInfo(TOTAFileNotification),
              Ord(NotifyCode));
          End;
      End;
    End;
End;

(**

  This method installs a source editor notifier for the given module.

  @precon  None.
  @postcon The source editor notifier is installed.

  @param   M as an IOTAModule as a constant

**)
Procedure TBADIIDENotifier.InstallEditViewNotifier(Const M: IOTAModule);

Var
  i: Integer;
  E : IOTAEditor;
  SE : IOTASourceEditor;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'InstallEditViewNotifier', tmoTiming);{$ENDIF}
  For i := 0 To M.GetModuleFileCount - 1 Do
    Begin
      E := M.GetModuleFileEditor(i);
      If Supports(E, IOTASourceEditor, SE) Then
        //: @bug Without a reference to this interface we cannot rename its file!
        FEditViewNotifiers.Add(M.FileName, SE.AddNotifier(TBADISourceEditorNotifier.Create(SE)));
    End;
End;

(**

  This class method installs the IDE notifier.

  @precon  None.
  @postcon The IDE Notifier is installed.

  @param   ModuleStatsList as an IBADIModuleStatsList as a constant

**)
Class Procedure TBADIIDENotifier.InstallIDENotifier(Const ModuleStatsList : IBADIModuleStatsList);

Var
  S : IOTAServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIIDENotifier.InstallIDENotifier', tmoTiming);{$ENDIF}
  If Supports(BorlandIDEServices, IOTAServices, S) Then
    FIDENotifierIndex := S.AddNotifier(TBADIIDENotifier.Create(ModuleStatsList));
End;

(**

  This method creates a module notifier and installs it into the IDE and then adds the index to the
  module notifier list.

  @precon  M must be a valid instance.
  @postcon The notifier is created, added to the IDE and its index stored.

  @param   M        as an IOTAModule as a constant
  @param   FileName as a String as a constant

**)
Procedure TBADIIDENotifier.InstallNotifier(Const M: IOTAModule; Const FileName: String);

Var
  P : IOTAProject;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'InstallModuleNotifier', tmoTiming);{$ENDIF}
  If Supports(M, IOTAProject, P) Then
    FProjectNotifiers.Add(
      FileName,
      P.AddNotifier(
        TBADIProjectNotifier.Create(
          FModuleStatsList,
          FileName,
          ModuleRenameEvent
        )
      )
    )
  Else
    FModuleNotifiers.Add(
      FileName,
      M.AddNotifier(
        TBADIModuleNotifier.Create(
          FModuleStatsList,
          FileName,
          ModuleRenameEvent
        )
      )
    );
  InstallEditViewNotifier(M);
End;

(**

  This method renames the modules in the module notifier index to ensure they can be removed after a
  name change.

  @precon  None.
  @postcon If the module is found its filename index is updated to the new filename.

  @param   strOldFileName as a String as a constant
  @param   strNewFileName as a String as a constant

**)
Procedure TBADIIDENotifier.ModuleRenameEvent(Const strOldFileName, strNewFileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ModuleRenameEvent', tmoTiming);{$ENDIF}
  FModuleNotifiers.Rename(strOldFileName, strNewFileName);
  FProjectNotifiers.Rename(strOldFileName, strNewFileName);
  FModuleStatsList.Rename(strOldFileName, strNewFileName);
  FEditViewNotifiers.Rename(strOldFileName, strNewFileName);
End;

(**

  This method uninstalls a source editor notifier for the given module.

  @precon  None.
  @postcon The source editor notifier is uninstalled.

  @param   M as an IOTAModule as a constant

**)
Procedure TBADIIDENotifier.UninstallEditViewNotifier(Const M: IOTAModule);

Var
  i: Integer;
  E: IOTAEditor;
  SE : IOTASourceEditor;
  iIndex : Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UninstallEditViewNotifier', tmoTiming);{$ENDIF}
  For i := 0 To M.GetModuleFileCount - 1 Do
    Begin
      E := M.GetModuleFileEditor(i);
      If Supports(E, IOTASourceEditor, SE) Then
        Begin
          iIndex := FEditViewNotifiers.Remove(M.FileName);
          If iIndex > -1 Then
            SE.RemoveNotifier(iIndex);
        End;
    End;
End;

(**

  This class method uninstalls the IDE notifier.

  @precon  None.
  @postcon The IDE Notifier is installed.

**)
Class Procedure TBADIIDENotifier.UninstallIDENotifier;

Var
  S : IOTAServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIIDENotifier.UninstallIDENotifier', tmoTiming);{$ENDIF}
  If Supports(BorlandIDEServices, IOTAServices, S) Then
    S.RemoveNotifier(FIDENotifierIndex);
End;

(**

  This method removes the nodule notifier from the IDE corresponding to the given filename.

  @precon  M must be a valid instance.
  @postcon The notifier is removed from the IDE.

  @param   M        as an IOTAModule as a constant
  @param   FileName as a String as a constant

**)
Procedure TBADIIDENotifier.UninstallNotifier(Const M: IOTAModule; Const FileName: String);

Var
  P : IOTAProject;
  iIndex: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UninstallModuleNotifier', tmoTiming);{$ENDIF}
  UninstallEditViewNotifier(M);
  If Supports(M, IOTAProject, P) Then
    Begin
      iIndex := FProjectNotifiers.Remove(FileName);
      If iIndex > -1 Then
        P.RemoveNotifier(iIndex);
    End Else
    Begin
      iIndex := FModuleNotifiers.Remove(FileName);
      If iIndex > -1 Then
        M.RemoveNotifier(iIndex);
    End;
End;

End.



