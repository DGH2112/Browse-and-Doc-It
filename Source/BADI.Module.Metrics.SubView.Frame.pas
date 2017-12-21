(**
  
  A module which contains a descendant class from the metrics frame so that the frame name can be changed
  so there are no component casses in the IDE between the CustomEditView and the CustomEditorSubView when
  using the same frame.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Dec 2017
  
**)
Unit BADI.Module.Metrics.SubView.Frame;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  BADI.Module.Metrics.EditorView.Frame,
  Vcl.ExtCtrls,
  System.ImageList,
  Vcl.ImgList,
  VirtualTrees,
  Actions,
  ActnList,
  Menus,
  PlatformDefaultStyleActnCtrls,
  ActnPopup;

Type
  (** A descendant class for the subview frame. **)
  TframeBADIModuleMetricsSubView = Class(TframeBADIModuleMetricsEditorView)
  Strict Private
  Strict Protected
  Public
  End;

Implementation

{$R *.dfm}

End.
