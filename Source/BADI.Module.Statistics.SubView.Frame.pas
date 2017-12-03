(**
  
  A module which contains a descendant class from the metrics frame so that the frame name can be changed
  so there are no component casses in the IDE between the CustomEditView and the CustomEditorSubView when
  using the same frame.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Dec 2017
  
**)
Unit BADI.Module.Statistics.SubView.Frame;

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
  BADI.Module.Statistics.Frame,
  Vcl.ExtCtrls,
  System.ImageList,
  Vcl.ImgList,
  VirtualTrees;

Type
  (** A descendant class for the subview frame. **)
  TframeBADIModuleStatisticsSubView = Class(TframeBADIModuleStatistics)
  Strict Private
  Strict Protected
  Public
  End;

Implementation

{$R *.dfm}

End.
