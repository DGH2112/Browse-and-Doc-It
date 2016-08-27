(**

  This module contains an interface that must be implemented by each frame in order for
  them to load and save their settings.

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Aug 2016

**)
Unit BADICustomOptionsFrame;

Interface

Uses
  Forms;

Type
  (** This is an interface each Options page must implement in order to load and save BADI
      settings **)
  IBADIOptionsFrame = Interface
  ['{4F8C53A5-3F4E-4B24-83F6-722F26AA8B8B}']
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

  (** This is a class reference for TFrames. **)
  TFrameClass = Class Of TFrame;

Implementation

End.
