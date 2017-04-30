(**

  This module contains a class which installs the BADI option frames into the IDEs options
  dialogue.

  @Author  David Hoyle
  @Version 1.0
  @Date    30 Apr 2017

**)
Unit BADI.IDEOptionsInstaller;

Interface

Uses
  Classes,
  BADI.IDEOptionsHandler;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A simple class to encapsulate the installation of option frames into the IDE. **)
  TBADIIDEOptionsInstaller = Class
  Strict Private
    {$IFDEF DXE00}
    FBADIParentFrame      : TBADIIDEOptionsHandler;
    FBADIGeneralOptions   : TBADIIDEOptionsHandler;
    FBADISpecialtags      : TBADIIDEOptionsHandler;
    FBADIModuleExplorer   : TBADIIDEOptionsHandler;
    FBADICodeBrowsing     : TBADIIDEOptionsHandler;
    FBADIExcludedDocs     : TBADIIDEOptionsHandler;
    FBADIMethodDesc       : TBADIIDEOptionsHandler;
    FBADIMenuShortcuts    : TBADIIDEOptionsHandler;
    FBADIModuleExtensions : TBADIIDEOptionsHandler;
    {$ENDIF}
  Strict Protected
    Function IsShortcutUsed(Const iShortcut : TShortcut; Var strActionName : String) : Boolean;
  Public
    Constructor Create(UpdateMenuShortcuts : TNotifyEvent);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  ToolsAPI,
  SysUtils,
  BADI.ParentFrame,
  BADI.CustomOptionsFrame,
  BADI.GeneralOptionsFrame,
  BADI.CodeBrowsingFrame,
  BADI.ExcludedDocFilesFrame,
  BADI.MethodDescriptionsFrame,
  BADI.ModuleExlporerOpsFrame,
  BADI.SpecialTagsFrame,
  BADI.MenuShortcutsFrame,
  BADI.ModuleExtensionsFrame;

{ TBADIIDEOptionsInstaller }

(**

  A constructor for the TBADIIDEOptionsInstaller class.

  @precon  None.
  @postcon Adds the options frames to the IDEs options dialogue.

  @param   UpdateMenuShortcuts as a TNotifyEvent

**)
Constructor TBADIIDEOptionsInstaller.Create(UpdateMenuShortcuts : TNotifyEvent);

Begin
  {$IFDEF DXE00}
  FBADIParentFrame := TBADIIDEOptionsHandler.Create(TfmBADIParentFrame, '');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIParentFrame);
  FBADIGeneralOptions := TBADIIDEOptionsHandler.Create(TfmBADIGeneralOptions, 'General Options');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIGeneralOptions);
  FBADISpecialtags := TBADIIDEOptionsHandler.Create(TfmBADISpecialTagsFrame, 'Special Tags');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADISpecialtags);
  FBADIModuleExplorer := TBADIIDEOptionsHandler.Create(TfmBADIModuleExplorerFrame, 'Module Explorer');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIModuleExplorer);
  FBADICodeBrowsing := TBADIIDEOptionsHandler.Create(TfmBADICodeBrowsingFrame, 'Code Browsing');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADICodeBrowsing);
  FBADIExcludedDocs := TBADIIDEOptionsHandler.Create(TfmBADIExcludedDocFilesFrame, 'Excluded Documentation Files');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIExcludedDocs);
  FBADIMethodDesc := TBADIIDEOptionsHandler.Create(TfmBADIMethodDescriptionsFrame, 'Method Descriptions');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIMethodDesc);
  FBADIMenuShortcuts := TBADIIDEShortcutOptionsHandler.Create(TfmBADIMenuShortcuts, 'Menu Shortcuts',
    UpdateMenuShortcuts, IsShortcutUsed);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIMenuShortcuts);
  FBADIModuleExtensions := TBADIIDEOptionsHandler.Create(TfmBADIModuleExtensionsFrame, 'Module Extensions');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIModuleExtensions);
  {$ENDIF}
End;

(**

  A destructor for the TBADIIDEOptionsInstaller class.

  @precon  None.
  @postcon Removes the option frames from the IDE.

**)
Destructor TBADIIDEOptionsInstaller.Destroy;

Begin
  {$IFDEF DXE00}
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIParentFrame);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIGeneralOptions);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADISpecialtags);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIModuleExplorer);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADICodeBrowsing);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIExcludedDocs);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIMethodDesc);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIMenuShortcuts);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIModuleExtensions);
  {$ENDIF}
  Inherited Destroy;
End;

(**

  This method checks to see if the given shortcut is in use by another part of the IDE.

  @precon  None.
  @postcon returns true of the shortcut is already in use and returns the name of the action in the
           parameter strActionName else returns false.

  @param   iShortcut     as a TShortcut as a constant
  @param   strActionName as a String as a reference
  @return  a Boolean

**)
Function TBADIIDEOptionsInstaller.IsShortcutUsed(Const iShortcut: TShortcut;
  Var strActionName : String): Boolean;

Var
  NS : INTAServices;
  iAction: Integer;

Begin
  Result := False;
  {$IFDEF DXE30} // Shortcut property unfortunately not available in XE2 or below.
  If Supports(BorlandIDEServices, INTAServices, NS) Then
    For iAction := 0 To NS.ActionList.ActionCount - 1 Do
      If NS.ActionList.Actions[iAction].ShortCut = iShortcut Then
        Begin
          strActionName := NS.ActionList.Actions[iAction].Name;
          Result := True;
        End;
  {$ENDIF}
End;

End.


