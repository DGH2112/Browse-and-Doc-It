(**
  
  This module contains a custom virtual string tree for use within the BADI plug-in to provide a similar
  look and feel + try and work around the hard coded StyleServices issue.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jan 2018
  
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
    FStyleServices : TCustomStyleServices;
  Strict Protected
    Procedure AdvancedHeaderDrawEvent(Sender: TVTHeader; Var PaintInfo: THeaderPaintInfo;
      Const Elements: THeaderPaintElements);
    Procedure HeaderDrawQueryElementsEvent(Sender: TVTHeader; Var PaintInfo: THeaderPaintInfo;
      Var Elements: THeaderPaintElements);
  Public
    Constructor Create(AOwner : TComponent); Override;
    Procedure UpdateTreeColours;
    (**
      This property provides access to the IDEs style services.
      @precon  None.
      @postcon The IDEs style services reference is returned if valid else nil is returned.
      @return  a TCustomStyleServices
    **)
    Property StyleServices : TCustomStyleServices Read FStyleServices;
  End;

Implementation

Uses
  SysUtils,
  Graphics,
  ToolsAPI;

(**

  This is an on advanced header draw event handler.

  @precon  None.
  @postcon Draws the background of the header buttons as a workaround the theming issues.

  @param   Sender    as a TVTHeader
  @param   PaintInfo as a THeaderPaintInfo as a reference
  @param   Elements  as a THeaderPaintElements as a constant

**)
Procedure TBADICustomVirtualStringTree.AdvancedHeaderDrawEvent(Sender: TVTHeader;
  Var PaintInfo: THeaderPaintInfo; Const Elements: THeaderPaintElements);

{$IFDEF DXE102}
Var
  Details: TThemedElementDetails;
{$ENDIF}

Begin
  If (hpeBackground In  Elements) {And (Not PaintInfo.IsDownIndex)} Then
    Begin
      PaintInfo.TargetCanvas.Brush.Color := clBtnFace;
      {$IFDEF DXE102}
      If Assigned(FStyleServices) And (FStyleServices.Enabled) Then
        Begin
          PaintInfo.TargetCanvas.Brush.Color := FStyleServices.GetSystemColor(clBtnFace);
          PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
          Details := StyleServices.GetElementDetails(thHeaderItemRightNormal);
          FStyleServices.DrawElement(
            PaintInfo.TargetCanvas.Handle,
            Details,
            PaintInfo.PaintRectangle,
            PaintInfo.PaintRectangle
          )
        End;
      {$ENDIF}
    End;
End;

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
  HintAnimation := hatFade;
  HintMode := hmTooltip;
  SelectionBlendFactor := iDefaultBlendFactor;
  TreeOptions.MiscOptions := [toFullRepaintOnResize, toGridExtensions, toInitOnSave,
    toToggleOnDblClick, toWheelPanning];
  TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowHorzGridLines,
    toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused,
    toUseBlendedSelection];
  TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect, toRightClickSelect];
  UpdateTreeColours;
  OnAdvancedHeaderDraw := AdvancedHeaderDrawEvent;
  OnHeaderDrawQueryElements := HeaderDrawQueryElementsEvent;
End;

(**

  This is an on header draw query element event handler.

  @precon  None.
  @postcon Only returns the background as this is the only part of the header we want to draw ourselves.

  @param   Sender    as a TVTHeader
  @param   PaintInfo as a THeaderPaintInfo as a reference
  @param   Elements  as a THeaderPaintElements as a reference

**)
Procedure TBADICustomVirtualStringTree.HeaderDrawQueryElementsEvent(Sender: TVTHeader;
  Var PaintInfo: THeaderPaintInfo; Var Elements: THeaderPaintElements);

Begin
  Elements := [hpeBackground];
End;

(**

  This method updates the string tree colours using the IDEs style services (as a workaround for the
  hard coded style services in the component).

  @precon  None.
  @postcon If style services are available then the colours are updated.

**)
Procedure TBADICustomVirtualStringTree.UpdateTreeColours;

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}

Begin
  FStyleServices := Nil;
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      FStyleServices := ITS.StyleServices;
  StyleElements := [];
  If Assigned(FStyleServices) And FStyleServices.Enabled Then
    Begin
      Color := FStyleServices.GetSystemColor(Color);
      Font.Color := FStyleServices.GetSystemColor(Font.Color);
      Header.Font.Color := FStyleServices.GetSystemColor(Header.Font.Color);
      //Colors.BackGroundColor := FStyleServices.GetSystemColor(Colors.BackGroundColor);
      //Colors.HeaderFontColor := FStyleServices.GetSystemColor(Colors.HeaderFontColor);
      //Colors.NodeFontColor := FStyleServices.GetSystemColor(Colors.NodeFontColor);
      Colors.BorderColor := FStyleServices.GetSystemColor(Colors.BorderColor);
      Colors.DisabledColor := FStyleServices.GetSystemColor(Colors.DisabledColor);
      Colors.DropMarkColor := FStyleServices.GetSystemColor(Colors.DropMarkColor);
      Colors.DropTargetColor := FStyleServices.GetSystemColor(Colors.DropTargetColor);
      Colors.DropTargetBorderColor := FStyleServices.GetSystemColor(Colors.DropTargetBorderColor);
      Colors.FocusedSelectionColor := FStyleServices.GetSystemColor(Colors.FocusedSelectionColor);
      Colors.FocusedSelectionBorderColor := FStyleServices.GetSystemColor(Colors.FocusedSelectionBorderColor);
      Colors.GridLineColor := FStyleServices.GetSystemColor(Colors.GridLineColor);
      Colors.HeaderHotColor := FStyleServices.GetSystemColor(Colors.HeaderHotColor);
      Colors.HotColor := FStyleServices.GetSystemColor(Colors.HotColor);
      Colors.SelectionRectangleBlendColor := FStyleServices.GetSystemColor(Colors.SelectionRectangleBlendColor);
      Colors.SelectionRectangleBorderColor := FStyleServices.GetSystemColor(Colors.SelectionRectangleBorderColor);
      Colors.SelectionTextColor := FStyleServices.GetSystemColor(Colors.SelectionTextColor);
      Colors.TreeLineColor := FStyleServices.GetSystemColor(Colors.TreeLineColor);
      Colors.UnfocusedColor := FStyleServices.GetSystemColor(Colors.UnfocusedColor);
      Colors.UnfocusedSelectionColor := FStyleServices.GetSystemColor(Colors.UnfocusedSelectionColor);
      Colors.UnfocusedSelectionBorderColor := FStyleServices.GetSystemColor(Colors.UnfocusedSelectionBorderColor);
      Invalidate;
    End;
  {$ENDIF}
End;

End.
