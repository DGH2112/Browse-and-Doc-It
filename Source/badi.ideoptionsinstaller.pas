(**

  This module contains a class which installs the BADI option frames into the IDEs options
  dialogue.

  @Author  David Hoyle
  @Version 1.045
  @Date    12 Jul 2020

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
    FBADIModuleMetrics    : TBADIIDEOptionsHandler;
    FBADIModuleChecks     : TBADIIDEOptionsHandler;
    FBADIModuleSpelling   : TBADIIDEOptionsHandler;
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
    Constructor Create(Const UpdateMenuShortcuts : TNotifyEvent);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
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
  BADI.ModuleExplorerOpsFrame,
  BADI.SpecialTagsFrame,
  BADI.MenuShortcutsFrame,
  BADI.ModuleExtensionsFrame,
  BADI.ModuleExplorerFrame,
  BADI.Module.Metrics.Options.Frame,
  BADI.Module.Checks.Options.Frame,
  BADI.Spelling.OpsFrame;

(**

  A constructor for the TBADIIDEOptionsInstaller class.

  @precon  None.
  @postcon Adds the options frames to the IDEs options dialogue.

  @param   UpdateMenuShortcuts as a TNotifyEvent as a constant

**)
Constructor TBADIIDEOptionsInstaller.Create(Const UpdateMenuShortcuts : TNotifyEvent);

Resourcestring
  strGeneralOptions        = 'Options';
  strSpecialTags           = 'Special Tags';
  strModuleMetrics         = 'Metrics';
  strModuleChecks          = 'Checks';
  strModuleSpelling        = 'Spelling';
  strModuleExplorer        = 'Explorer';
  strCodeBrowsing          = 'Code Browsing';
  strExcludedDocumentation = 'Excluded Documentation';
  strMethodDescriptions    = 'Method Descriptions';
  strMenuShortcuts         = 'Shortcuts';
  strModuleExtensions      = 'Extensions';

Var
  NEOS : INTAEnvironmentOptionsServices;

Begin
  {$IFDEF DXE00}
  If Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, NEOS) Then
    Begin
      FBADIParentFrame := TBADIIDEOptionsHandler.Create(TfmBADIParentFrame, '');
      NEOS.RegisterAddInOptions(FBADIParentFrame);
      FBADIGeneralOptions := TBADIIDEOptionsHandler.Create(TfmBADIGeneralOptions, strGeneralOptions);
      NEOS.RegisterAddInOptions(FBADIGeneralOptions);
      FBADISpecialtags := TBADIIDEOptionsHandler.Create(TfmBADISpecialTagsFrame, strSpecialTags);
      NEOS.RegisterAddInOptions(FBADISpecialtags);
      FBADIModuleMetrics := TBADIIDEOptionsHandler.Create(TframeBADIModuleMetricsOptions, strModuleMetrics);
      NEOS.RegisterAddInOptions(FBADIModuleMetrics);
      FBADIModuleChecks := TBADIIDEOptionsHandler.Create(TframeBADIModuleChecksOptions, strModuleChecks);
      NEOS.RegisterAddInOptions(FBADIModuleChecks);
      FBADIModuleSpelling := TBADIIDEOptionsHandler.Create(TframeBADISpellingOpions, strModuleSpelling);
      NEOS.RegisterAddInOptions(FBADIModuleSpelling);
      FBADIModuleExplorer := TBADIIDEOptionsHandler.Create(TfmBADIModuleExplorerFrame, strModuleExplorer);
      NEOS.RegisterAddInOptions(FBADIModuleExplorer);
      FBADICodeBrowsing := TBADIIDEOptionsHandler.Create(TfmBADICodeBrowsingFrame, strCodeBrowsing);
      NEOS.RegisterAddInOptions(FBADICodeBrowsing);
      FBADIExcludedDocs := TBADIIDEOptionsHandler.Create(TfmBADIExcludedDocFilesFrame, strExcludedDocumentation);
      NEOS.RegisterAddInOptions(FBADIExcludedDocs);
      FBADIMethodDesc := TBADIIDEOptionsHandler.Create(TfmBADIMethodDescriptionsFrame, strMethodDescriptions);
      NEOS.RegisterAddInOptions(FBADIMethodDesc);
      FBADIMenuShortcuts := TBADIIDEShortcutOptionsHandler.Create(TfmBADIMenuShortcuts, strMenuShortcuts,
        UpdateMenuShortcuts, IsShortcutUsed);
      NEOS.RegisterAddInOptions(FBADIMenuShortcuts);
      FBADIModuleExtensions := TBADIIDEOptionsHandler.Create(TfmBADIModuleExtensionsFrame, strModuleExtensions);
      NEOS.RegisterAddInOptions(FBADIModuleExtensions);
    End;
  {$ENDIF}
End;

(**

  A destructor for the TBADIIDEOptionsInstaller class.

  @precon  None.
  @postcon Removes the option frames from the IDE.

**)
Destructor TBADIIDEOptionsInstaller.Destroy;

Var
  NEOS : INTAEnvironmentOptionsServices;

Begin
  {$IFDEF DXE00}
  If Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, NEOS) Then
    Begin
      NEOS.UnregisterAddInOptions(FBADIParentFrame);
      NEOS.UnregisterAddInOptions(FBADIGeneralOptions);
      NEOS.UnregisterAddInOptions(FBADISpecialtags);
      NEOS.UnregisterAddInOptions(FBADIModuleMetrics);
      NEOS.UnregisterAddInOptions(FBADIModuleChecks);
      NEOS.UnregisterAddInOptions(FBADIModuleSpelling);
      NEOS.UnregisterAddInOptions(FBADIModuleExplorer);
      NEOS.UnregisterAddInOptions(FBADICodeBrowsing);
      NEOS.UnregisterAddInOptions(FBADIExcludedDocs);
      NEOS.UnregisterAddInOptions(FBADIMethodDesc);
      NEOS.UnregisterAddInOptions(FBADIMenuShortcuts);
      NEOS.UnregisterAddInOptions(FBADIModuleExtensions);
    End;
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
