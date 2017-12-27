(**
  
  This module contains an IDE Style Services notifier for forms and frames to create to get a signal
  when a theme has changed.

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Dec 2017
  
**)
Unit BADI.StyleServices.Notifier;

Interface

Uses
  ToolsAPI;

{$INCLUDE CompilerDefinitions.inc}

{$IFDEF DXE102}
Type
  (** This is a call back procedure (parameterless) to force theme updates in the treeview. **)
  TUpdateProc = Procedure Of Object;

  (** This is a style services notifier to get a signal when the IDE changes theming. **)
  TBADIStyleServicesNotifier = Class(TInterfacedObject, IOTANotifier, INTAIDEThemingServicesNotifier)
  Strict Private
    FUpdateProc : TUpdateProc;
  Strict Protected
    // IOTANotifier
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
    // INTAIDEThemingServiceNotifier
    Procedure ChangedTheme;
    Procedure ChangingTheme;
  Public
    Constructor Create(Const UpdateProc : TUpdateProc);
  End;
{$ENDIF}

Implementation

{$IFDEF DXE102}
(**

  This is an on after save method for the style services notifier.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TBADIStyleServicesNotifier.AfterSave;

Begin
End;

(**

  This is an on before save method for the style services notifier.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TBADIStyleServicesNotifier.BeforeSave;

Begin
End;

(**

  This method is called when an IDE them has changed. It calls the call back method if assigned.

  @precon  None.
  @postcon The call back method is call if assigned.

**)
Procedure TBADIStyleServicesNotifier.ChangedTheme;

Begin
  If Assigned(FUpdateProc) Then
    FUpdateProc;
End;

(**

  This method is called when a them is able to change.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TBADIStyleServicesNotifier.ChangingTheme;

Begin
End;

(**

  A constructor for the TBADIStyleServicesNotifier class.

  @precon  None.
  @postcon Stores a reference to the call back method.

  @param   UpdateProc as a TUpdateProc as a constant

**)
Constructor TBADIStyleServicesNotifier.Create(Const UpdateProc: TUpdateProc);

Begin
  FUpdateProc := UpdateProc;
End;

(**

  This is an on destroyed method for the style services notifier.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TBADIStyleServicesNotifier.Destroyed;

Begin
End;

(**

  This is an on modified method for the style services notifier.

  @precon  None.
  @postcon None.

  @nocheck EmptyMethod

**)
Procedure TBADIStyleServicesNotifier.Modified;

Begin
End;
{$ENDIF}

End.
