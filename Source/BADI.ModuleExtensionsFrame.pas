(**

  This module contains a frame for display the module explorer file extensions for editing.

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

  @todo    Replace the TValueListEditor with VST.

**)
Unit BADI.ModuleExtensionsFrame;

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
  BADI.CustomOptionsFrame,
  Grids,
  ValEdit;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class which represents a frame to edit the module explorer file extensions. **)
  TfmBADIModuleExtensionsFrame = Class(TFrame, IBADIOptionsFrame)
    vleModuleExtensions: TValueListEditor;
  Strict Private
  Strict Protected
  Public
    Constructor Create(AOwner : TComponent); Override;
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

Uses
  {$IFNDEF STANDALONEAPP}
  ToolsAPI,
  {$ENDIF}
  BADI.Module.Dispatcher,
  BADI.Options;

{$R *.dfm}

(**

  A constructor for the TfmBADIModuleExtensionsFrame class.

  @precon  None.
  @postcon Themes the ValueListEditor.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TfmBADIModuleExtensionsFrame.Create(AOwner: TComponent);

{$IFNDEF STANDALONEAPP}
{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
{$ENDIF}

Begin
  Inherited Create(AOwner);
  {$IFNDEF STANDALONEAPP}
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      Begin
        vleModuleExtensions.Ctl3D := False;
        vleModuleExtensions.DrawingStyle := gdsGradient;
        vleModuleExtensions.FixedColor := ITS.StyleServices.GetSystemColor(clBtnFace);
        vleModuleExtensions.Color := ITS.StyleServices.GetSystemColor(clWindow);
        vleModuleExtensions.GradientStartColor := ITS.StyleServices.GetSystemColor(clBtnFace);
        vleModuleExtensions.GradientEndColor := ITS.StyleServices.GetSystemColor(clBtnFace);
        vleModuleExtensions.Font.Color := ITS.StyleServices.GetSystemColor(clWindowText);      End;
  {$ENDIF}
  {$ENDIF}
End;

(**

  This method loads the frame with the file extensions in the module dispatcher.

  @precon  None.
  @postcon The frame is loaded with the file exteniosn for each module.

**)
Procedure TfmBADIModuleExtensionsFrame.LoadSettings;

Var
  MD : TBADIDispatcher;
  iModule : Integer;
  strModuleName : String;

Begin
  MD := TBADIDispatcher.BADIDispatcher;
  For iModule := 0 To MD.Count - 1 Do
    Begin
      strModuleName := MD.Modules[iModule].Cls.ClassName;
      vleModuleExtensions.Values[strModuleName] := MD.Modules[iModule].Extensions;
    End;
End;

(**

  This method saves the changes inthe file exnteiosn associate with the modules back to the
  module dispatcher.

  @precon  None.
  @postcon The changes are saved.

**)
Procedure TfmBADIModuleExtensionsFrame.SaveSettings;

Var
  MD : TBADIDispatcher;
  iModule : Integer;
  strModuleName : String;

Begin
  MD := TBADIDispatcher.BADIDispatcher;
  For iModule := 0 To MD.Count - 1 Do
    Begin
      strModuleName := MD.Modules[iModule].Cls.ClassName;
      MD.Modules[iModule].Extensions := vleModuleExtensions.Values[strModuleName];
    End;
End;

End.
