(**
  
  This module contains a frame which represents a subview to display the modules checks.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Dec 2017
  
**)
Unit BADI.Module.Checks.SubView.Frame;

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
  BADI.Module.Checks.EditorView.Frame,
  Actions,
  ActnList,
  Menus,
  PlatformDefaultStyleActnCtrls,
  ActnPopup,
  ExtCtrls,
  ImageList,
  ImgList;

Type
  (** A frame to display the module checks in a subview. **)
  TframeBADIModuleChecksSubView = Class(TframeBADIModuleChecksEditorView)
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Implementation

{$R *.dfm}

End.
