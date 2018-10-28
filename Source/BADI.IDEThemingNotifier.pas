(**
  
  This module contains a class which implements an INTAIDETHemingServicesNotifier so that the plug-in
  can react to Them changes in RAD Studio 10.2.2 abd above.

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Oct 2018
  
**)
Unit BADI.IDEThemingNotifier;

Interface

Uses
  ToolsAPI,
  System.Classes;

{$INCLUDE CompilerDefinitions.inc}

{$IFDEF DXE102}
Type
  (** A class which implements the INRAIDEThemingServicesNotifier interface. **)
  TBADIIDEThemeNotifier = Class(TInterfacedObject, IUnknown, IOTANotifier,
    INTAIDEThemingServicesNotifier)
  Strict Private
    FThemeFormCallBack : TNotifyEvent;
  Strict Protected
    // IOTANotifer
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
    // INTAIDEThemingServicesNotifier
    Procedure ChangedTheme;
    Procedure ChangingTheme;
  Public
    Constructor Create(Const ThemeFormProc : TNotifyEvent);
    Destructor Destroy; Override;
  End;
{$ENDIF}

Implementation

{$IFDEF DEBUG}
Uses
  CodeSiteLogging;
{$ENDIF}

{$IFDEF DXE102}

(**

  This is an implementation of the IOTANotifier AfterSave method.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TBADIIDEThemeNotifier.AfterSave;

Begin
End;

(**

  This is an implementation of the IOTANotifier BeforeSave method.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TBADIIDEThemeNotifier.BeforeSave;

Begin
End;

(**

  This method is called by the INTAIDEThemingServicesNotifier when the IDE has finished changing the
  theme.

  @precon  None.
  @postcon Invokes the callback so that the form using this notifier can update its theming.

**)
Procedure TBADIIDEThemeNotifier.ChangedTheme;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ChangedTheme', tmoTiming);{$ENDIF}
  If Assigned(FThemeFormCallBack) Then
    FThemeFormCallBack(Nil);
End;

(**

  This method is called by the INTAIDEThemingServicesNotifier when the IDE start changing the theme.

  @precon  None.
  @postcon Does nothing.

**)
Procedure TBADIIDEThemeNotifier.ChangingTheme;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ChangingTheme', tmoTiming);{$ENDIF}
End;

(**

  A constructor for the TBADIDIEThemeNotifier class.

  @precon  None.
  @postcon Stores a reference to the given callback method to later use.

  @param   ThemeFormProc as a TNotifyEvent as a constant

**)
Constructor TBADIIDEThemeNotifier.Create(Const ThemeFormProc: TNotifyEvent);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FThemeFormCallBack := ThemeFormProc;
End;

(**

  A destructor for the TBADIIDEThemeNotifier class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TBADIIDEThemeNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  Inherited Destroy;
End;

(**

  This is an implementation of the IOTANotifier Destroyed method.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TBADIIDEThemeNotifier.Destroyed;

Begin
End;

(**

  This is an implementation of the IOTANotifier Modified method.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Procedure TBADIIDEThemeNotifier.Modified;

Begin
End;
{$ENDIF}

End.


