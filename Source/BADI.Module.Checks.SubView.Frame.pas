(**
  
  A module which contains a descendant class from the checks frame so that the frame name can be changed
  so there are no component classes in the IDE between the Custom Edit View and the Custom Editor Sub
  View when using the same frame.

  @Author  David Hoyle
  @Version 1.024
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
  ImgList, System.ImageList;

Type
  (** A frame to display the module checks in a sub-view. **)
  TframeBADIModuleChecksSubView = Class(TframeBADIModuleChecksEditorView)
  Strict Private
  Strict Protected
  Public
  End;

Implementation

{$R *.dfm}

End.
