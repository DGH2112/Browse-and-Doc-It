(**

  This module contains a dockable form which will become the Module Explorer.

  @Author  David Hoyle
  @Date    03 Jan 2018
  @Version 1.0

**)
unit BADI.DockableModuleExplorer;

interface

uses
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
  BADI.Base.Module;

type
  (** This class represents a dockable form that displays the heirarchical
      representation of the modules contents. **)
  TfrmDockableModuleExplorer = Class(TDockableForm)
  Strict Private
    FModuleExplorerFrame : TframeModuleExplorer;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Procedure Focus;
    Class Procedure ShowDockableModuleExplorer;
    Class Procedure RemoveDockableModuleExplorer;
    Class Procedure CreateDockableModuleExplorer;
    Class Procedure RenderDocumentTree(BaseLanguageModule : TBaseLanguageModule);
    Class Procedure HookEventHandlers(SelectionChangeProc : TSelectionChange;
      Focus, ScopeChange : TNotifyEvent);
  End;

  (** This is a classifier for the dockable form so that it can be registered
      with the IDE **)
  TfrmDockableModuleExplorerClass = Class of TfrmDockableModuleExplorer;

implementation

{$R *.dfm}

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  DeskUtil;

Var
  (** This is a private varaible to hold the singleton instance of the
      dockable form. **)
  FormInstance : TfrmDockableModuleExplorer;

(**

  This procedure makes the dockable module explorer visible.

  @precon  None.
  @postcon Makes the dockable module explorer visible.

  @param   Form as a TfrmDockableModuleExplorer

**)
Procedure ShowDockableForm(Form : TfrmDockableModuleExplorer);

Begin
  If Not Assigned(Form) Then
    Exit;
  If Not Form.Floating Then
    Begin
      Form.ForceShow;
      FocusWindow(Form);
      Form.Focus;
    End Else
    Begin
      Form.Show;
      Form.Focus;
    End;
End;

(**

  This procedure registers the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is registered with the IDE.

  @param   FormClass as a TfrmDockableModuleExplorerClass
  @param   FormVar
  @param   FormName  as a String as a constant

**)
Procedure RegisterDockableForm(FormClass : TfrmDockableModuleExplorerClass;
  var FormVar; Const FormName : String);

Begin
  If @RegisterFieldAddress <> Nil Then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
End;

(**

  This method unregisters the dockable form with the IDE.

  @precon  None.
  @postcon The dockable form is unregistered with the IDE.

  @param   FormVar
  @param   FormName as a String as a constant

**)
Procedure UnRegisterDockableForm(var FormVar; Const FormName : String);
Begin
  If @UnRegisterFieldAddress <> Nil Then
    UnregisterFieldAddress(@FormVar);
End;

(**

  This procedure creates an instance of the dockable form.

  @precon  FormVar is the instance reference and FormCass is the type of class
           to be created..
  @postcon The form instance is created.

  @param   FormVar   as a TfrmDockableModuleExplorer as a reference
  @param   FormClass as a TfrmDockableModuleExplorerClass

**)
Procedure CreateDockableForm(var FormVar : TfrmDockableModuleExplorer;
  FormClass : TfrmDockableModuleExplorerClass);
Begin
  TCustomForm(FormVar) := FormClass.Create(Nil);
  RegisterDockableform(FormClass, FormVar, TCustomForm(FormVar).Name);
End;

(**

  This procedure frees the instance of the dockable form.

  @precon  None.
  @postcon Free the instance of the dockable form.

  @param   FormVar as a TfrmDockableModuleExplorer as a reference

**)
Procedure FreeDockableForm(var FormVar : TfrmDockableModuleExplorer);
Begin
  If Assigned(FormVar) Then
    Begin
      UnRegisterDockableForm(FormVar, FormVar.Name);
      FreeAndNil(FormVar);
    End;
End;

{ TfrmDockableModuleExplorer }

(**

  This is the constructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Sets the dockable form up for being saved within the BDS 2006 IDE and
           then creates a Module Explorer Frame and places inside the form.

  @param   AOwner as a TComponent

**)
constructor TfrmDockableModuleExplorer.Create(AOwner: TComponent);
begin
  inherited;
  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  FModuleExplorerFrame := TframeModuleExplorer.Create(Self);
  FModuleExplorerFrame.Parent := Self;
  FModuleExplorerFrame.Align := alClient;
end;

(**

  This is the destructor method for the TfrmDockableModuleExplorer class.

  @precon  None.
  @postcon Destroys the Module Explorer Frame and ensures the desktop is saved.

**)
destructor TfrmDockableModuleExplorer.Destroy;
begin
  FModuleExplorerFrame.Free;
  SaveStateNecessary := True;
  inherited;
end;

(**


  This method focuses the modukle explorers tree view the be focused IF
  available.

  @precon  None.
  @postcon Focuses the modukle explorers tree view the be focused IF available.


**)
procedure TfrmDockableModuleExplorer.Focus;
begin
  If FModuleExplorerFrame <> Nil Then
    If FModuleExplorerFrame.Visible Then
      If FModuleExplorerFrame.Explorer.Visible Then
        FModuleExplorerFrame.Explorer.SetFocus;
end;

(**

  This is a class method to create the dockable form instance.

  @precon  None.
  @postcon The form instance is created if one is not already present.

**)
class procedure TfrmDockableModuleExplorer.CreateDockableModuleExplorer;
begin
  If Not Assigned(FormInstance) Then
    CreateDockableForm(FormInstance, TfrmDockableModuleExplorer);
end;

(**

  This is a class method to remove the dockable form.

  @precon  None.
  @postcon Removes the instance of the dockable form.

**)
class procedure TfrmDockableModuleExplorer.RemoveDockableModuleExplorer;
begin
  FreeDockableForm(FormInstance);
end;

(**

  This method is a class method for displaying the dockable form. If the form
  does not already exist it is created first.

  @precon  None.
  @postcon Displays the dockable form.

**)
class procedure TfrmDockableModuleExplorer.ShowDockableModuleExplorer;
begin
  CreateDockableModuleExplorer;
  ShowDockableForm(FormInstance);
end;

(**

  This method is a class method to all a calling class to render the given
  module in the Dockable Module Explorer.

  @precon  None.
  @postcon If the form module explorer exists then the passed module is
           rendered.

  @param   BaseLanguageModule    as a TBaseLanguageModule

**)
class procedure TfrmDockableModuleExplorer.RenderDocumentTree(
  BaseLanguageModule: TBaseLanguageModule);
begin
  If Assigned(FormInstance) Then
    If FormInstance.Visible Then
      FormInstance.FModuleExplorerFrame.RenderModule(BaseLanguageModule);
end;

(**

  This is a class method which accepots even handler from the calling class to
  hand the dockable module explorer`s SelectionChange event handler.

  @precon  None.
  @postcon Sets the SelectionChange event handler for the dockable form.

  @param   SelectionChangeProc as a TSelectionChange
  @param   Focus               as a TNotifyEvent
  @param   ScopeChange         as a TNotifyEvent

**)
class procedure TfrmDockableModuleExplorer.HookEventHandlers(
  SelectionChangeProc: TSelectionChange; Focus, ScopeChange : TNotifyEvent);
begin
  If Assigned(FormInstance) Then
    Begin
      FormInstance.FModuleExplorerFrame.OnSelectionChange := SelectionChangeProc;
      FormInstance.FModuleExplorerFrame.OnFocus := Focus;
      FormInstance.FModuleExplorerFrame.OnRefresh := ScopeChange;
    End;
end;

End.


