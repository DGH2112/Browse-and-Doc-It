(**
  
  This module contains a custom virtual string tree for use within the BADI plug-in to provide a similar
  look and feel + try and work around the hard coded StyleServices issue.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Oct 2018
  
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
