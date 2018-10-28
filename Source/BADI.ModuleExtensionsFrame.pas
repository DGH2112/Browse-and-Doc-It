(**

  This module contains a frame for display the module explorer file extensions for editing.

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Oct 2018

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
  ToolsAPI,
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

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}

Begin
  Inherited Create(AOwner);
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
