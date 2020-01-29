(**
  
  This module contains a class which implement the IOTAIDENOtifier interface so that BADI can hook
  module save events to see if there have been changes in the files.

  @Author  David Hoyle
  @Version 1.0
  @Date    26 Jan 2020
  
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
    FModuleNotifiers : IBADIModuleNotifierList;
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
    Procedure InstallModuleNotifier(Const M: IOTAModule; Const FileName: String);
    Procedure UninstallModuleNotifier(Const M: IOTAModule; Const FileName: String);
    Procedure ModuleRenameEvent(Const strOldFileName, strNewFileName : String);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Class Procedure InstallIDENotifier;
    Class Procedure UninstallIDENotifier;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils, BADI.ModuleNotifierList, BADI.ModuleNotifier;

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

**)
Constructor TBADIIDENotifier.Create;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FModuleNotifiers := TBADIModuleNotifierList.Create;
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
    Case NotifyCode Of
      ofnFileOpened:
        Begin
          M := MS.OpenModule(FileName);
          InstallModuleNotifier(M, FileName);
        End;
      ofnFileClosing:
        Begin
          M := MS.OpenModule(FileName);
          UninstallModuleNotifier(M, Filename);
        End;
    End;
End;

(**

  This class method installs the IDE notifier.

  @precon  None.
  @postcon The IDE Notifier is installed.

**)
Class Procedure TBADIIDENotifier.InstallIDENotifier;

Var
  S : IOTAServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIIDENotifier.InstallIDENotifier', tmoTiming);{$ENDIF}
  If Supports(BorlandIDEServices, IOTAServices, S) Then
    FIDENotifierIndex := S.AddNotifier(TBADIIDENotifier.Create);
End;

(**

  This method creates a module notifier and installs it into the IDE and then adds the index to the
  module notifier list.

  @precon  M must be a valid instance.
  @postcon The notifier is created, added to the IDE and its index stored.

  @param   M        as an IOTAModule as a constant
  @param   FileName as a String as a constant

**)
Procedure TBADIIDENotifier.InstallModuleNotifier(Const M: IOTAModule; Const FileName: String);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'InstallModuleNotifier', tmoTiming);{$ENDIF}
  FModuleNotifiers.Add(FileName, M.AddNotifier(TBADIModuleNotifier.Create(FileName, ModuleRenameEvent)));
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
  FModuleNotifiers.Rename(strOldFileName, strNewFileName);
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
Procedure TBADIIDENotifier.UninstallModuleNotifier(Const M: IOTAModule; Const FileName: String);

Var
  iIndex: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UninstallModuleNotifier', tmoTiming);{$ENDIF}
  iIndex := FModuleNotifiers.Remove(FileName);
  If iIndex > -1 Then
    M.RemoveNotifier(iIndex);
End;

End.
