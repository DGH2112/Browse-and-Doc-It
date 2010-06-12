(**

  This module contains a dockable form which will become the Module Explorer.

  @Author  David Hoyle
  @Date    12 Jun 2010
  @Version 1.0

**)
unit VBEIDEModuleExplorer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ModuleExplorerFrame, BaseLanguageModule;

type
  (** This class represents a dockable form that displays the heirarchical
      representation of the modules contents. **)
  TfrmDockableModuleExplorer = class(TForm)
  private
    FModuleExplorerFrame : TframeModuleExplorer;
  public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Procedure Focus;
    Class Procedure ShowDockableModuleExplorer;
    Class Procedure RemoveDockableModuleExplorer;
    Class Procedure CreateDockableModuleExplorer;
    Class Procedure RenderDocumentTree(BaseLanguageModule : TBaseLanguageModule);
    Class Procedure HookEventHandlers(SelectionChangeProc : TSelectionChange;
      Focus, ScopeChange : TNotifyEvent; FormClose : TCloseEvent);
    Class Function  IsVisible : Boolean;
    Class Procedure SetActivate(Handler : TNotifyEvent);
    Class Procedure SetVisible(boolVisible : Boolean);
    Class Function  GetVisible : Boolean;
    Class Function  GetModuleExplorerPosition: TRect;
    Class Procedure SetModuleExplorerPosition(const Value: TRect);
    Class Function  GetWndHnd : THandle;
  end;

  (** This is a classifier for the dockable form so that it can be registered
      with the IDE **)
  TfrmDockableModuleExplorerClass = Class of TfrmDockableModuleExplorer;

implementation

uses Types;

{$R *.dfm}

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
  Form.Show;
  Form.SetFocus;
  Form.Focus;
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
    FreeAndNil(FormVar);
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
        If FModuleExplorerFrame.Explorer.CanFocus Then
          FModuleExplorerFrame.Explorer.SetFocus;
end;

(**

  This is a getter method for the ModuleExplorerPosition property.

  @precon  None.
  @postcon returns the position of the module explorer form.

  @return  a TRect

**)
Class Function TfrmDockableModuleExplorer.GetModuleExplorerPosition: TRect;

begin
  If FormInstance <> Nil Then
    With FormInstance Do
      Result := Rect(Left, Top, Left + Width, Top + Height);
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

  This method sets the OnActivate event handler for the form instance.

  @precon  None .
  @postcon Sets the OnActivate event handler for the form instance .

  @param   Handler as a TNotifyEvent

**)
class procedure TfrmDockableModuleExplorer.SetActivate(Handler: TNotifyEvent);
begin
  If FormInstance  <> Nil Then
    FormInstance.OnActivate := Handler;
end;

(**

  This is a getter method for the Visible property.

  @precon  None.
  @postcon Sets the forms visibility.

  @return  a Boolean

**)
class function TfrmDockableModuleExplorer.GetVisible: Boolean;
begin
  Result := False;
  If FormInstance <> Nil Then
    Result := FormInstance.Visible;
end;

(**

  This method returns the module explorer main form window handle.

  @precon  None.
  @postcon Returns the module explorer main form window handle.

  @return  a THandle

**)
class function TfrmDockableModuleExplorer.GetWndHnd: THandle;
begin
  Result := 0;
  If FormInstance <> Nil Then
    Result := FormInstance.Handle;
end;

(**

  This method sets the visible property of the form instance.

  @precon  None.
  @postcon Sets the visible property of the form instance.

  @param   boolVisible as a Boolean

**)
class procedure TfrmDockableModuleExplorer.SetVisible(boolVisible : Boolean);
begin
  If FormInstance  <> Nil Then
    Begin
      FormInstance.Visible := boolVisible;
      FormInstance.Focus;
    End;
end;

(**

  This is a setter method for the ModuleExplorerPosition property.

  @precon  None.
  @postcon Sets the position of the module explorer form.

  @param   Value as a TRect as a constant

**)
Class Procedure TfrmDockableModuleExplorer.SetModuleExplorerPosition(const Value: TRect);
begin
  If FormInstance <> Nil Then
    With FormInstance Do
      Begin
        Left := Value.Left;
        Top := Value.Top;
        Width := Value.Right - Value.Left;
        Height := Value.Bottom - Value.Top;
      End;
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

  This method returns whether the form is visible or not.

  @precon  None.
  @postcon Returns true if the form is visible else returns false.

  @return  a Boolean

**)
class function TfrmDockableModuleExplorer.IsVisible: Boolean;
begin
  Result := False;
  If FormInstance <> Nil Then
    Result := FormInstance.Visible;
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
  @param   FormClose           as a TCloseEvent

**)
class procedure TfrmDockableModuleExplorer.HookEventHandlers(
  SelectionChangeProc: TSelectionChange; Focus, ScopeChange : TNotifyEvent;
  FormClose : TCloseEvent);
begin
  If Assigned(FormInstance) Then
    Begin
      FormInstance.FModuleExplorerFrame.OnSelectionChange := SelectionChangeProc;
      FormInstance.FModuleExplorerFrame.OnFocus := Focus;
      FormInstance.FModuleExplorerFrame.OnRefresh := ScopeChange;
      FormInstance.OnClose := FormClose;
    End;
end;

End.
