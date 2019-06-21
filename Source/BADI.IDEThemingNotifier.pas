(**
  
  This module contains a class which implements an INTAIDETHemingServicesNotifier so that the plug-in
  can react to Them changes in RAD Studio 10.2.2 abd above.

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


