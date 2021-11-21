(**

  This module contains a dockable form which will become the Module Explorer.

  @Author  David Hoyle
  @Version 1.367
  @Date    21 Nov 2021

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

  (** This class represents a dockable form that displays the hierarchical
      representation of the modules contents. **)
  TfrmDockableModuleExplorer = Class(TDockableForm)
  Strict Private
    FModuleExplorerFrame: TframeModuleExplorer;
    {$IFDEF RS102}
    FIDEThemingServciesNotifierIndex : Integer;
    {$ENDIF RS102}
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
    Class Procedure HookEventHandlers(
                      Const SelectionChangeProc: TSelectionChange;
                      Const Focus, ScopeChange: TNotifyEvent;
                      Const IDEErrors : TBADIIDEErrors
                    );
    Class Function  LineDocIssue(Const iLine : Integer) : IBADILineDocIssues;
    Class Function  DocIssueTotals : IBADIDocIssueTotals;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF CODESITE}
  DeskUtil,
  ToolsAPI,
  BADI.ToolsAPIUtils;

Var
  (** This is a private variable to hold the singleton instance of the
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

{$IFDEF RS102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF RS102}
  
Begin
  Inherited Create(AOwner);
  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  FModuleExplorerFrame := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent := Self;
  FModuleExplorerFrame.Align := alClient;
  ThemeForm(Nil);
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    FIDEThemingServciesNotifierIndex := ITS.AddNotifier(TBADIIDEThemeNotifier.Create(ThemeForm))
  {$ENDIF RS102}
End;

(**

  This procedure creates an instance of the dockable form.

  @precon  FormVar is the instance reference and FormClass is the type of class
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

{$IFDEF RS102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF RS102}
  
Begin
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    ITS.RemoveNotifier(FIDEThemingServciesNotifierIndex);
  {$ENDIF RS102}
  FModuleExplorerFrame.Free;
  SaveStateNecessary := True;
  Inherited;
End;

(**

  This method returns the doc issues totals.

  @precon  None.
  @postcon An interfaced object is returned which contains the documentation issue totals.

  @return  an IBADIDocIssueTotals

**)
Class Function TfrmDockableModuleExplorer.DocIssueTotals: IBADIDocIssueTotals;

Begin
  Result := Nil;
  If Assigned(FormInstance) And Assigned(FormInstance.FModuleExplorerFrame) Then
    Result := FormInstance.FModuleExplorerFrame.DocIssueTotals;
End;

(**


  This method focuses the module explorers tree view the be focused IF
  available.

  @precon  None.
  @postcon Focuses the module explorers tree view the be focused IF available.


**)
Procedure TfrmDockableModuleExplorer.Focus;

Begin
  If FModuleExplorerFrame <> Nil Then
    If FModuleExplorerFrame.Visible Then
      If FModuleExplorerFrame.Explorer.Visible Then
        FModuleExplorerFrame.Explorer.SetFocus;
End;

(**

  This class method allows the caller to update the selection in the explorer module to align with
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

  This is a class method which accepts even handler from the calling class to hand the dockable module 
  explorer`s Selection Change event handler.

  @precon  None.
  @postcon Sets the Selection Change event handler for the dockable form.

  @param   SelectionChangeProc as a TSelectionChange as a constant
  @param   Focus               as a TNotifyEvent as a constant
  @param   ScopeChange         as a TNotifyEvent as a constant
  @param   IDEErrors           as a TBADIIDEErrors as a constant

**)
Class Procedure TfrmDockableModuleExplorer.HookEventHandlers(
                  Const SelectionChangeProc: TSelectionChange;
                  Const Focus, ScopeChange: TNotifyEvent;
                  Const IDEErrors : TBADIIDEErrors
                );

Begin
  If Assigned(FormInstance) Then
    Begin
      FormInstance.FModuleExplorerFrame.OnSelectionChange := SelectionChangeProc;
      FormInstance.FModuleExplorerFrame.OnFocus := Focus;
      FormInstance.FModuleExplorerFrame.OnRefresh := ScopeChange;
      FormInstance.FModuleExplorerFrame.OnIDEErrors := IDEErrors;
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

  This method themes the Dockable Module Explorer form.

  @precon  None.
  @postcon The form is themed.

  @param   Sender as a TObject

**)
Procedure TfrmDockableModuleExplorer.ThemeForm(Sender : TObject);

Begin
  {$IFDEF RS102}
  TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmDockableModuleExplorer, Self);
  {$ENDIF RS102}
End;

(**

  This method un-registers the dockable form with the IDE.

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
