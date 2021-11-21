(**
  
  A module which contains a descendant class from the metrics frame so that the frame name can be changed
  so there are no component classes in the IDE between the Custom Edit View and the Custom Editor Sub
  View when using the same frame.

  @Author  David Hoyle
  @Version 1.004
  @Date    28 Aug 2020

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
Unit BADI.Module.Metrics.SubView.Frame;

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
  BADI.Module.Metrics.EditorView.Frame,
  ExtCtrls,
  ImgList,
  VirtualTrees,
  Actions,
  ActnList,
  Menus,
  PlatformDefaultStyleActnCtrls,
  ActnPopup;

Type
  (** A descendant class for the sub-view frame. **)
  TframeBADIModuleMetricsSubView = Class(TframeBADIModuleMetricsEditorView)
  Strict Private
  Strict Protected
  Public
  End;

Implementation

{$R *.dfm}

End.
