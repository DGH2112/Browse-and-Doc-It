(**

  This module contains a class which installs the BADI option frames into the IDEs options
  dialogue.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

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
  Public
    Constructor Create(UpdateMenuShortcuts : TNotifyEvent);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  ToolsAPI,
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
  FBADIParentFrame := TBADIIDEOptionsHandler.Create(TfmBADIParentFrame, '', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIParentFrame);
  FBADIGeneralOptions := TBADIIDEOptionsHandler.Create(TfmBADIGeneralOptions, 'General Options', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIGeneralOptions);
  FBADISpecialtags := TBADIIDEOptionsHandler.Create(TfmBADISpecialTagsFrame, 'Special Tags', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADISpecialtags);
  FBADIModuleExplorer := TBADIIDEOptionsHandler.Create(TfmBADIModuleExplorerFrame, 'Module Explorer', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIModuleExplorer);
  FBADICodeBrowsing := TBADIIDEOptionsHandler.Create(TfmBADICodeBrowsingFrame, 'Code Browsing', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADICodeBrowsing);
  FBADIExcludedDocs := TBADIIDEOptionsHandler.Create(TfmBADIExcludedDocFilesFrame, 'Excluded Documentation Files', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIExcludedDocs);
  FBADIMethodDesc := TBADIIDEOptionsHandler.Create(TfmBADIMethodDescriptionsFrame, 'Method Descriptions', Nil);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIMethodDesc);
  FBADIMenuShortcuts := TBADIIDEOptionsHandler.Create(TfmBADIMenuShortcuts, 'Menu Shortcuts', UpdateMenuShortcuts);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIMenuShortcuts);
  FBADIModuleExtensions := TBADIIDEOptionsHandler.Create(TfmBADIModuleExtensionsFrame, 'Module Extensions', Nil);
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

End.
