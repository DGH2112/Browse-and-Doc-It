(**

  This module contains an interface that must be implemented by each frame in order for
  them to load and save their settings.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

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
    Procedure InstallShortcutUsedCallBack(Const ShortCutUsed : TBADIShortcutUsedEvent);
  End;

  (** This is a class reference for frames. **)
  TFrameClass = Class Of TFrame;

Implementation

End.
