(**
  
  This module contains a custom virtual string tree for use within the BADI plug-in to provide a similar
  look and feel + try and work around the hard coded Style Services issue.

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
Unit BADI.CustomVirtualStringTree;

Interface

Uses
  VirtualTrees,
  Classes,
  Themes;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A custom virtual string tree for use within the IDE. **)
  TBADICustomVirtualStringTree = Class(TVirtualStringTree)
  Strict Private                           
  Strict Protected
  Public
    Constructor Create(AOwner : TComponent); Override;
  End;

Implementation

Uses
  {$IFDEF TOOLSAPI}
  ToolsAPI,
  {$ENDIF}
  SysUtils,
  Graphics;

(**

  A constructor for the TBADICustomVirtualStringTree class.

  @precon  None.
  @postcon Initialises the string tree.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TBADICustomVirtualStringTree.Create(AOwner: TComponent);

Const
  strDefaultFontName = 'Tahoma';
  iDefaultFontSize = 10;
  iDefaultBlendFactor = 64;

Begin
  Inherited Create(AOwner);
  Font.Name := strDefaultFontName;
  Font.Size := iDefaultFontSize;
  Font.Style := [];
  Header.Options := [hoAutoResize, hoOwnerDraw, hoVisible];
  Header.Font.Assign(Font);
  //: @debug REMOVED FROM 7.0.0 HintAnimation := hatFade;
  HintMode := hmTooltip;
  SelectionBlendFactor := iDefaultBlendFactor;
  TreeOptions.MiscOptions := [
    toFullRepaintOnResize,
    toGridExtensions,
    toInitOnSave,
    toToggleOnDblClick,
    toWheelPanning];
  TreeOptions.PaintOptions := [
    toShowButtons,
    toShowDropmark,
    toShowHorzGridLines,
    toShowTreeLines,
    toShowVertGridLines,
    toShowRoot,
    toThemeAware,
    toUseBlendedImages,
    toGhostedIfUnfocused,
    toUseBlendedSelection];
  TreeOptions.SelectionOptions := [
    toExtendedFocus,
    toFullRowSelect,
    toRightClickSelect];
End;

End.
