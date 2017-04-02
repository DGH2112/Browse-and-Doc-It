(**

  This module contains an interface that must be implemented by each frame in order for
  them to load and save their settings.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Apr 2017

**)
Unit BADI.CustomOptionsFrame;

Interface

Uses
  BADI.Types,
  Forms;

Type
  (** This is an interface each Options page must implement in order to load and save BADI
      settings **)
  IBADIOptionsFrame = Interface
  ['{4F8C53A5-3F4E-4B24-83F6-722F26AA8B8B}']
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

  (** This is an interface to be implemented by the Shortcut Frame to allow the registration of
      a call back event for checking whether an action is in use. **)
  IBADIInstallShortcutUsedCallBack = Interface
  ['{ECBC6389-DA38-4AE1-A4E9-83E6826E3776}']
    Procedure InstallShortcutUsedCallBack(ShortCutUsed : TBADIShortcutUsedEvent);
  End;

  (** This is a class reference for TFrames. **)
  TFrameClass = Class Of TFrame;

Implementation

End.
