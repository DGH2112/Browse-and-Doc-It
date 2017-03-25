(**

  This module contains a class which installs the BADI option frames into the IDEs options
  dialogue.

  @Author  David Hoyle
  @Version 1.0
  @Date    25 Mar 2017

**)
Unit BADI.IDEOptionsInstaller;

Interface

Uses
  BADI.IDEOptionsHandler;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A simple class to encapsulate the installation of option frames into the IDE. **)
  TBADIIDEOptionsInstaller = Class
    {$IFDEF 2005} Strict {$ENDIF} Private
    {$IFDEF DXE00}
    FBADIGeneralOptions : TBADIIDEOptionsHandler;
    FBADISpecialtags    : TBADIIDEOptionsHandler;
    FBADIModuleExplorer : TBADIIDEOptionsHandler;
    FBADICodeBrowsing   : TBADIIDEOptionsHandler;
    FBADIExcludedDocs   : TBADIIDEOptionsHandler;
    FBADIMethodDesc     : TBADIIDEOptionsHandler;
    FBADIMenuShortcuts  : TBADIIDEOptionsHandler;
    {$ENDIF}
    {$IFDEF 2005} Strict {$ENDIF} Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  ToolsAPI,
  BADI.CustomOptionsFrame,
  BADI.GeneralOptionsFrame,
  BADI.CodeBrowsingFrame,
  BADI.ExcludedDocFilesFrame,
  BADI.MethodDescriptionsFrame,
  BADI.ModuleExlporerOpsFrame,
  BADI.SpecialTagsFrame,
  BADI.MenuShortcutsFrame;

{ TBADIIDEOptionsInstaller }

(**

  A constructor for the TBADIIDEOptionsInstaller class.

  @precon  None.
  @postcon Adds the options frames to the IDEs options dialogue.

**)
Constructor TBADIIDEOptionsInstaller.Create;

Begin
  {$IFDEF DXE00}
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
  FBADIMenuShortcuts := TBADIIDEOptionsHandler.Create(TfmBADIMenuShortcuts, 'Menu Shortcuts');
  (BorlandIDEServices As INTAEnvironmentOptionsServices).RegisterAddInOptions(FBADIMenuShortcuts);
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
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIGeneralOptions);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADISpecialtags);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIModuleExplorer);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADICodeBrowsing);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIExcludedDocs);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIMethodDesc);
  (BorlandIDEServices As INTAEnvironmentOptionsServices).UnregisterAddInOptions(FBADIMenuShortcuts);
  {$ENDIF}
  Inherited Destroy;
End;

End.
