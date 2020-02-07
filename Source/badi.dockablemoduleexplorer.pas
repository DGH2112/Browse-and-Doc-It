(**

  This module contains a dockable form which will become the Module Explorer.

  @Author  David Hoyle
  @Version 1.082
  @Date    07 Feb 2020

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
Unit BADI.DockableModuleExplorer;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DockForm,
  BADI.ModuleExplorerFrame,
  BADI.Base.Module,
  BADI.IDEThemingNotifier,
  BADI.Types,
  BADI.Interfaces;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a classifier for the dockable form so that it can be registered
      with the IDE **)
  TfrmDockableModuleExplorerClass = Class Of TfrmDockableModuleExplorer;

  (** This class represents a dockable form that displays the heirarchical
      representation of the modules contents. **)
  TfrmDockableModuleExplorer = Class(TDockableForm)
  Strict Private
    FModuleExplorerFrame: TframeModuleExplorer;
    {$IFDEF DXE102}
    FIDEThemingServciesNotifierIndex : Integer;
    {$ENDIF}
  Strict Protected
    Class Procedure ShowDockableForm(Const Form: TfrmDockableModuleExplorer);
    Class Procedure CreateDockableForm(Var FormVar: TfrmDockableModuleExplorer;
      Const FormClass: TfrmDockableModuleExplorerClass);
    Class Procedure FreeDockableForm(Var FormVar: TfrmDockableModuleExplorer);
    Class Procedure RegisterDockableForm(Const FormClass: TfrmDockableModuleExplorerClass;
      Var FormVar; Const FormName: String);
    Class Procedure UnRegisterDockableForm(Var FormVar);
    Procedure ThemeForm(Sender : TObject);
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure Focus;
    Class Procedure ShowDockableModuleExplorer;
    Class Procedure RemoveDockableModuleExplorer;
    Class Procedure CreateDockableModuleExplorer;
    Class Procedure RenderDocumentTree(Const BaseLanguageModule: TBaseLanguageModule);
    Class Procedure FollowEditorCursor(Const iLine : Integer);
    Class Procedure HookEventHandlers(Const SelectionChangeProc: TSelectionChange;
      Const Focus, ScopeChange: TNotifyEvent);
    Class Function  LineDocIssue(Const iLine : Integer) : IBADILineDocIssues;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  DeskUtil,
  ToolsAPI,
  BADI.ToolsAPIUtils;

Var
  (** This is a private varaible to hold the singleton instance of the
      dockable form. **)
  FormInstance: TfrmDockableModuleExplorer;

(**

  This is the constructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Sets the dockable form up for being saved within the BDS 2006 IDE and
           then creates a Module Explorer Frame and places inside the form.

  @nocheck MissingConstInParam

  @param   AOwner as a TComponent

**)
Constructor TfrmDockableModuleExplorer.Create(AOwner: TComponent);

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
  
Begin
  Inherited Create(AOwner);
  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  FModuleExplorerFrame := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent := Self;
  FModuleExplorerFrame.Align := alClient;
  ThemeForm(Nil);
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    FIDEThemingServciesNotifierIndex := ITS.AddNotifier(TBADIIDEThemeNotifier.Create(ThemeForm))
  {$ENDIF}
End;

(**

  This procedure creates an instance of the dockable form.

  @precon  FormVar is the instance reference and FormCass is the type of class
           to be created..
  @postcon The form instance is created.

  @param   FormVar   as a TfrmDockableModuleExplorer as a reference
  @param   FormClass as a TfrmDockableModuleExplorerClass as a Constant

**)
Class Procedure TfrmDockableModuleExplorer.CreateDockableForm(Var FormVar: TfrmDockableModuleExplorer;
  Const FormClass: TfrmDockableModuleExplorerClass);
  
Begin
  TCustomForm(FormVar) := FormClass.Create(Nil);
  RegisterDockableForm(FormClass, FormVar, TCustomForm(FormVar).Name);
End;

(**

  This is a class method to create the dockable form instance.

  @precon  None.
  @postcon The form instance is created if one is not already present.

**)
Class Procedure TfrmDockableModuleExplorer.CreateDockableModuleExplorer;

Begin
  If Not Assigned(FormInstance) Then
    CreateDockableForm(FormInstance, TfrmDockableModuleExplorer);
End;

(**

  This is the destructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Destroys the Module Explorer Frame and ensures the desktop is saved.

**)
Destructor TfrmDockableModuleExplorer.Destroy;

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
  
Begin
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    ITS.RemoveNotifier(FIDEThemingServciesNotifierIndex);
  {$ENDIF}
  FModuleExplorerFrame.Free;
  SaveStateNecessary := True;
  Inherited;
End;

(**


  This method focuses the modukle explorers tree view the be focused IF
  available.

  @precon  None.
  @postcon Focuses the modukle explorers tree view the be focused IF available.


**)
Procedure TfrmDockableModuleExplorer.Focus;

Begin
  If FModuleExplorerFrame <> Nil Then
    If FModuleExplorerFrame.Visible Then
      If FModuleExplorerFrame.Explorer.Visible Then
        FModuleExplorerFrame.Explorer.SetFocus;
End;

(**

  This class method allowsd the caller to update the selection in the explorer module to align with
  the given line number that is being followed in the editor.

  @precon  None.
  @postcon The Module Explorer has its selection updated to reflect the line number.

  @param   iLine as an Integer as a constant

**)
Class Procedure TfrmDockableModuleExplorer.FollowEditorCursor(Const iLine: Integer);

Begin
  If Assigned(FormInstance) Then
    FormInstance.FModuleExplorerFrame.FollowEditorCursor(iLine);
End;

(**

  This procedure frees the instance of the dockable form.

  @precon  None.
  @postcon Free the instance of the dockable form.

  @param   FormVar as a TfrmDockableModuleExplorer as a reference

**)
Class Procedure TfrmDockableModuleExplorer.FreeDockableForm(Var FormVar: TfrmDockableModuleExplorer);

Begin
  If Assigned(FormVar) Then
    Begin
      UnRegisterDockableForm(FormVar);
      FreeAndNil(FormVar);
    End;
End;

(**

  This is a class method which accepots even handler from the calling class to hand the dockable module 
  explorer`s SelectionChange event handler.

  @precon  None.
  @postcon Sets the SelectionChange event handler for the dockable form.

  @param   SelectionChangeProc as a TSelectionChange as a constant
  @param   Focus               as a TNotifyEvent as a constant
  @param   ScopeChange         as a TNotifyEvent as a constant

**)
Class Procedure TfrmDockableModuleExplorer.HookEventHandlers(Const SelectionChangeProc: TSelectionChange;
  Const Focus, ScopeChange: TNotifyEvent);

Begin
  If Assigned(FormInstance) Then
    Begin
      FormInstance.FModuleExplorerFrame.OnSelectionChange := SelectionChangeProc;
      FormInstance.FModuleExplorerFrame.OnFocus := Focus;
      FormInstance.FModuleExplorerFrame.OnRefresh := ScopeChange;
    End;
End;

(**

  This method returns the limit types that have been associated with the given line number.

  @precon  None.
  @postcon Returns the limit types that have been associated with the given line number.

  @param   iLine as an Integer as a constant
  @return  an IBADILineDocIssues

**)
Class Function TfrmDockableModuleExplorer.LineDocIssue(Const iLine: Integer): IBADILineDocIssues;

Begin
  Result := Nil;
  If Assigned(FormInstance) And Assigned(FormInstance.FModuleExplorerFrame) Then
    Result := FormInstance.FModuleExplorerFrame.LineDocIssues[iLine];
End;

(**

  This procedure registers the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is registered with the IDE.

  @param   FormClass as a TfrmDockableModuleExplorerClass as a Constant
  @param   FormVar
  @param   FormName  as a String as a constant

**)
Class Procedure TfrmDockableModuleExplorer.RegisterDockableForm(
  Const FormClass: TfrmDockableModuleExplorerClass; Var FormVar; Const FormName: String);

Begin
  If @RegisterFieldAddress <> Nil Then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
End;

(**

  This is a class method to remove the dockable form.

  @precon  None.
  @postcon Removes the instance of the dockable form.

**)
Class Procedure TfrmDockableModuleExplorer.RemoveDockableModuleExplorer;

Begin
  FreeDockableForm(FormInstance);
End;

(**

  This method is a class method to all a calling class to render the given
  module in the Dockable Module Explorer.

  @precon  None.
  @postcon If the form module explorer exists then the passed module is
           rendered.

  @param   BaseLanguageModule    as a TBaseLanguageModule as a Constant

**)
Class Procedure TfrmDockableModuleExplorer.RenderDocumentTree(
  Const BaseLanguageModule: TBaseLanguageModule);
  
Begin
  If Assigned(FormInstance) Then
    FormInstance.FModuleExplorerFrame.RenderModule(BaseLanguageModule);
End;

(**

  This procedure makes the dockable module explorer visible.

  @precon  None.
  @postcon Makes the dockable module explorer visible.

  @param   Form as a TfrmDockableModuleExplorer as a Constant

**)
Class Procedure TfrmDockableModuleExplorer.ShowDockableForm(Const Form: TfrmDockableModuleExplorer);

Begin
  If Not Assigned(Form) Then
    Exit;
  If Not Form.Floating Then
    Begin
      Form.ForceShow;
      FocusWindow(Form);
      Form.Focus;
    End
  Else
    Begin
      Form.Show;
      Form.Focus;
    End;
End;

(**

  This method is a class method for displaying the dockable form. If the form
  does not already exist it is created first.

  @precon  None.
  @postcon Displays the dockable form.

**)
Class Procedure TfrmDockableModuleExplorer.ShowDockableModuleExplorer;

Begin
  CreateDockableModuleExplorer;
  ShowDockableForm(FormInstance);
End;

(**

  This method themes the DockableModule Explorer form.

  @precon  None.
  @postcon The form is themed.

  @param   Sender as a TObject

**)
Procedure TfrmDockableModuleExplorer.ThemeForm(Sender : TObject);

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices250;
{$ENDIF}

Begin
  {$IFDEF DXE102}
  //TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmDockableModuleExplorer);
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
    If ITS.IDEThemingEnabled Then
      Begin
        ITS.RegisterFormClass(TfrmDockableModuleExplorer);
        ITS.ApplyTheme(Self);
      End;
  {$ENDIF}
End;

(**

  This method unregisters the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is unregistered with the IDE.

  @param   FormVar

**)
Class Procedure TfrmDockableModuleExplorer.UnRegisterDockableForm(Var FormVar);

Begin
  If @UnRegisterFieldAddress <> Nil Then
    UnRegisterFieldAddress(@FormVar);
End;

End.
