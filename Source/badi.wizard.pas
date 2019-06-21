(**

  This module contains the packages main wizard interface.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

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
Unit BADI.Wizard;

Interface

Uses
  Classes,
  ToolsAPI,
  BADI.Base.Module,
  Types,
  BADI.CommonIDEFunctions,
  {$IFNDEF D2005}
  ExtCtrls,
  Contnrs,
  {$ENDIF}
  BADI.EditorNotifier,
  BADI.IDEOptionsInstaller,
  BADI.IDEMenuInstaller;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is the class which defined the Wizard interface. **)
  TBrowseAndDocItWizard = Class(TNotifierObject, IUnknown, IOTANotifier, IOTAWizard)
  Strict Private
    FBADIIDEMenuInstaller    : TBADIIDEMenuInstaller;
    FBADIIDEOptionsInstaller : TBADIIDEOptionsInstaller;
    FEditorNotifier          : TEditorNotifier;
    FEditorIndex             : Integer;
    FBNFHighlighterIndex     : Integer;
    FEidolonHighlighterIndex : Integer;
  Strict Protected
    // IOTAWizard
    Function GetIDString: String;
    Function GetName: String;
    Function GetState: TWizardState;
    Procedure Execute;
    // IOTAMenuWizard
    {$HINTS OFF}
    Function GetMenuText: String;
    {$HINTS ON}
    // General Methods
    Procedure SelectionChange(Const iIdentLine, iIdentCol, iCommentLine : Integer);
    Procedure Focus(Sender: TObject);
    Procedure OptionsChange(Sender: TObject);
    Procedure UpdateMenuShortcuts(Sender : TObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  BADI.DockableModuleExplorer,
  BADI.Constants, 
  BADI.Module.Metrics, 
  BADI.Module.Metrics.SubView, 
  BADI.Module.Checks, 
  BADI.Module.Checks.SubView,
  BADI.SplashScreen, 
  BADI.AboutBox, 
  BADI.BNFHighlighter,
  BADI.EidolonHighlighter;

(**

  This is the constructor method for the TPascalDocWizard class. This constructor create
  the explorer form and menus.

  @precon  None.
  @postcon Initialises the wizard`s internal data structures and creates a menu
           interface in the IDE.

**)
Constructor TBrowseAndDocItWizard.Create;

Begin
  Inherited Create;
  TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
  TfrmDockableModuleExplorer.HookEventHandlers(SelectionChange, Focus, OptionsChange);
  AddSplashScreen;
  AddAboutBoxEntry;
  FEditorNotifier := TEditorNotifier.Create;
  FEditorIndex := (BorlandIDEServices As IOTAEditorServices).AddNotifier(FEditorNotifier);
  FBADIIDEMenuInstaller := TBADIIDEMenuInstaller.Create(FEditorNotifier);
  FBADIIDEOptionsInstaller := TBADIIDEOptionsInstaller.Create(UpdateMenuShortcuts);
  FBNFHighlighterIndex := (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(
    TBNFHighlighter.Create);
  FEidolonHighlighterIndex := (BorlandIDEServices As IOTAHighlightServices).AddHighlighter(
    TEidolonHighlighter.Create);
  RegisterMetricsEditorView;
  RegisterChecksEditorView;
  RegisterEditorMetricsSubView;
  RegisterEditorChecksSubView;
End;

(**

  This is the destructor method for the TBrowseAndDocItWizard class.

  @precon  None.
  @postcon Saves the wizards settings and frees memory for interval structures.

**)
Destructor TBrowseAndDocItWizard.Destroy;

Begin
  UnregisterEditorChecksSubView;
  UnregisterEditorMetricsSubView;
  UnregisterChecksEditorView;
  UnregisterMetricsEditorView;
  If FEidolonHighlighterIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(FEidolonHighlighterIndex);
  If FBNFHighlighterIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAHighlightServices).RemoveHighlighter(FBNFHighlighterIndex);
  FBADIIDEOptionsInstaller.Free;
  FBADIIDEMenuInstaller.Free;
  If FEditorIndex > iWizardFailState Then
    (BorlandIDEServices As IOTAEditorServices).RemoveNotifier(FEditorIndex);
  RemoveAboutBoxEntry;
  TfrmDockableModuleExplorer.RemoveDockableModuleExplorer;
  Inherited Destroy;
End;

(**

  This is an exceute method for the wizard. Since this wizard is not implemented
  as a menu wizard this method has no code but is required for the interface.

  @nocheck EmptyMethod
  
  @precon  None.
  @postcon None.

**)
Procedure TBrowseAndDocItWizard.Execute;

Begin
  { Do nothing, this is not called }
End;

(**

  This method fires the focus method associated with the menus.

  @precon  None.
  @postcon The editor is focused.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.Focus(Sender: TObject);

Begin
  If Assigned(FBADIIDEMenuInstaller) Then
    FBADIIDEMenuInstaller.Focus(Sender);
End;

(**

  This is a getter method for the IDString property.

  @precon  None.
  @postcon Returns the ID string for the wizard.

  @return  a string

**)
Function TBrowseAndDocItWizard.GetIDString: String;

Const
  strIDString = 'David Hoyle.Browse An Doc It';
Begin
  Result := strIDString;
End;

(**

  This is a getter method for the MenuText property.

  @precon  None.
  @postcon Reutns the Menu text for the wizard.

  @return  a string

**)
Function TBrowseAndDocItWizard.GetMenuText: String;

Const
  strBADIMenuName = 'Browse and Doc It...';
  
Begin
  Result := strBADIMenuName;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the wizard.

  @return  a string

**)
Function TBrowseAndDocItWizard.GetName: String;

Const
  strBADIName = 'David Hoyle.Browse An Doc It';
  
Begin
  Result := strBADIName;
End;

(**

  This is a getter method for the State property.

  @precon  None.
  @postcon Enables the wizard.

  @return  a TWizardState

**)
Function TBrowseAndDocItWizard.GetState: TWizardState;

Begin
  Result := [wsEnabled];
End;

//Procedure TBrowseAndDocItWizard.ModuleExplorerClick(Sender: TObject);
//
//Begin
//  If Assigned(FBADIIDEMenuInstaller) Then
//    FBADIIDEMenuInstaller.ModuleExplorerClick(Sender);
//End;

(**

  This method signals that the options change been changed in the module explorer.

  @precon  None.
  @postcon The module explorer is re-rendered.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.OptionsChange(Sender: TObject);

Begin
  If Assigned(FBADIIDEMenuInstaller) Then
    FBADIIDEMenuInstaller.OptionsChange(Sender);
End;

(**

  This method signifies that the selection has changed.

  @precon  None.
  @postcon Notifies the system that the selection has changed and moves the editor cursor.

  @param   iIdentLine   as an Integer as a constant
  @param   iIdentCol    as an Integer as a constant
  @param   iCommentLine as an Integer as a constant

**)
Procedure TBrowseAndDocItWizard.SelectionChange(Const iIdentLine, iIdentCol, iCommentLine : Integer);

Begin
  If Assigned(FBADIIDEMenuInstaller) Then
    FBADIIDEMenuInstaller.SelectionChange(iIdentLine, iIdentCol, iCommentLine);
End;

(**

  This method updates the menu actions with any new shortcuts that have been saved to the options.

  @precon  None.
  @postcon The menu action shortcuts are updated.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItWizard.UpdateMenuShortcuts(Sender: TObject);

Begin
  FBADIIDEMenuInstaller.UpdateMenuShortcuts;
End;

End.


