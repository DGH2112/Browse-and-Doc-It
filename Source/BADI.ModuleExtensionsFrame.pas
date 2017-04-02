(**

  This module contains a frame for display the module explorer file extensions for editing.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Apr 2017

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

Type
  (** A class which represents a frame to edit the module explorer file extensions. **)
  TfmBADIModuleExtensionsFrame = Class(TFrame, IBADIOptionsFrame)
    vleModuleExtensions: TValueListEditor;
  Strict Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

Uses
  BADI.Module.Dispatcher,
  BADI.Options;

{$R *.dfm}

{ TfmBADIModuleExtensionsFrame }

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
