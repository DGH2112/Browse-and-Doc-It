(**
  
  This module contains a custom hint window to display the module metric totals.

  @Author  David Hoyle
  @Version 4.008
  @Date    21 Nov 2021
  
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

  @todo  Add metrics for the current method to the hint window.

**)
Unit BADI.DocIssuesHintWindow;

Interface

Uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  WinApi.Windows,
  WinAPI.Messages,
  BADI.Types,
  BADI.Interfaces;

Type
  (** An enumerate to define the position of the hint window. **)
  TBADIHintPosition = (hpTop, hpBottom);

  (** A class to define a new hint window for the totals to be displayed within on the Code Editor. **)
  TBADIDocIssueHintWindow = Class(THintWindow)
  Strict Private
    Const
      (** A constant to define the padding around the contents of the hint window. **)
      iPadding = 5;
      (** A constant to represent the size of the icons. **)
      iIconSize = 16;
    Class Var
      (** A class variable to hold the singleton instance of the code editor totals hint window. **)
      FHintWindow : TBADIDocIssueHintWindow;
  Strict Private
    FTotals         : TArray<TBADITotalInfo>;
    FScopeImageList : TImageList;
    FCalcHeight     : Integer;
    FCalcNameWidth  : Integer;
    FCalcNumWidth   : Integer;
    FBackColour     : TColor;
    FForeColour     : TColor;
    FItemHeight     : Integer;
  Strict Protected
    Procedure Paint; Override;
    Function CalcHintRect(
      Const Rect : TRect;
      Const ePosition: TBADIHintPosition;
      Const Totals: IBADIDocIssueTotals
    ): TRect; Reintroduce; Overload; Virtual;
    procedure ActivateHint(Const Rect: TRect); Reintroduce; Overload; Virtual;
    Procedure UpdateBaseFontInfo(Const R: TRect);
    Procedure UpdateFontInfo(Const TotalInfo: TBADITotalInfo);
    Procedure CreateParams(var Params: TCreateParams); Override;
    Procedure MakeHintTransparent;
    Procedure WMNCHitTest(Var Message: TWMNCHitTest); Message WM_NCHITTEST;
    Procedure MouseDownEvent(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Public
    Class Constructor Create;
    Class Destructor Destroy;
    Class Procedure Display(Const Totals: IBADIDocIssueTotals);
    Class Procedure Disappear;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.GraphUtil,
  ToolsAPI,
  BADI.Functions,
  BADI.Options,
  BADI.DockableModuleExplorer,
  BADI.ToolsAPIUtils;

Const
  (** Text for testing the height of a line of text. **)
  strTextHeightTest = 'Wg';

(**

  This method activates the hint (actual displays the hint).

  @precon  None.
  @postcon The hint is displayed.

  @param   Rect      as a TRect as a constant

**)
procedure TBADIDocIssueHintWindow.ActivateHint(Const Rect: TRect);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ActivateHint', tmoTiming);{$ENDIF}
  MakeHintTransparent;
  ActivateHint(Rect, '');
  FHintWindow.OnMouseDown := MouseDownEvent;
End;

(**

  This method calculates the rectangle in which the Code Editor Totals should be displayed.

  @precon  None.
  @postcon Calculates the size and position of the hint window.

  @param   Rect      as a TRect as a constant
  @param   ePosition as a TBADIHintPosition as a constant
  @param   Totals    as an IBADIDocIssueTotals as a constant
  @return  a TRect

**)
Function TBADIDocIssueHintWindow.CalcHintRect(Const Rect : TRect; Const ePosition: TBADIHintPosition;
  Const Totals: IBADIDocIssueTotals): TRect;

Var
  iTextWidth: Integer;
  Issue : TPair<String, TBADITotalInfo>;
  iIndex : Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CalcHintRect', tmoTiming);{$ENDIF}
  If Not Assigned(FScopeImageList) Then
    FScopeImageList := TBADIOptions.BADIOptions.ScopeImageList;
  Result := Rect;
  UpdateBaseFontInfo(Result);
  iIndex := 0;
  FCalcHeight := 0;
  FCalcNameWidth := 0;
  FCalcNumWidth := 0;
  FItemHeight := Max(Canvas.TextHeight(strTextHeightTest), iIconSize) + iPadding;
  If Assigned(Totals) Then
    Begin
  SetLength(FTotals, Totals.Totals.Count);
      For Issue In Totals.Totals Do
        Begin
          FTotals[iIndex].FLabel := Issue.Key;
          FTotals[iIndex].FImageIndex := Issue.Value.FImageIndex;
          FTotals[iIndex].FForeColour := Issue.Value.FForeColour;
          FTotals[iIndex].FBackColour := Issue.Value.FBackColour;
          FTotals[iIndex].FFontStyles := Issue.Value.FFontStyles;
          FTotals[iIndex].FCounter := Issue.Value.FCounter;
          FTotals[iIndex].FFirstLine := Issue.Value.FFirstLine;
          FTotals[iIndex].FFirstCol := Issue.Value.FFirstCol;
          UpdateFontInfo(FTotals[iIndex]);
          Inc(FCalcHeight, FItemHeight);
          iTextWidth := Canvas.TextWidth(FTotals[iIndex].FLabel);
          If iTextWidth > FCalcNameWidth Then
            FCalcNameWidth := iTextWidth;
          iTextWidth := Canvas.TextWidth(Format('%d', [FTotals[iIndex].FCounter]));
          If iTextWidth > FCalcNumWidth Then
            FCalcNumWidth := iTextWidth;
          Inc(iIndex);
        End;
    End;
  Case ePosition Of
    hpTop:    Result.Bottom := Result.Top + FCalcHeight;
    hpBottom: Result.Top := Result.Bottom - FCalcHeight;
  End;
  Result.Left := Result.Right -
    (iPadding + FCalcNameWidth + iPadding + iIconSize + iPadding + FCalcNumWidth + iPadding);
  InflateRect(Result, 1, 1);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelEdges := [];
  BevelKind := bkNone;
  BorderWidth := 0;
End;

(**

  A class constructor for the TBADIDocIssueHintWindow class.

  @precon  None.
  @postcon Creates a singleton instance of the hint window.

**)
Class Constructor TBADIDocIssueHintWindow.Create;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIDocIssueHintWindow.Create', tmoTiming);{$ENDIF}
  FHintWindow := TBADIDocIssueHintWindow.Create(Application.MainForm);
End;

(**

  This is an overridden CreateParams methods for the control.

  @precon  None.
  @postcon Removes the windows border from the hint window.

  @param   Params as a TCreateParams as a reference

**)
Procedure TBADIDocIssueHintWindow.CreateParams(Var Params: TCreateParams);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CreateParams', tmoTiming);{$ENDIF}
  Inherited CreateParams(Params);
  Params.Style := WS_POPUP
End;

(**

  A class destructor for the TBADIDocIssueHintWindow class.

  @precon  None.
  @postcon Frees the hint window.

**)
Class Destructor TBADIDocIssueHintWindow.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIDocIssueHintWindow.Destroy', tmoTiming);{$ENDIF}
  FHintWindow.Free;
End;

(**

  This method hides the hint window.

  @precon  None.
  @postcon The hint window is hidden.

**)
Class Procedure TBADIDocIssueHintWindow.Disappear;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIDocIssueHintWindow.Disappear', tmoTiming);{$ENDIF}
  FHintWindow.ReleaseHandle;
End;

(**

  This method displays the Hint window.

  @precon  None.
  @postcon The hint window is displayed.

  @param   Totals as an IBADIDocIssueTotals as a constant

**)
Class Procedure TBADIDocIssueHintWindow.Display(Const Totals: IBADIDocIssueTotals);

Const
  strTEditControlClsName = 'TEditControl';
  
Var
  ePosition : TBADIHintPosition;
  ES: IOTAEditorServices;
  TV: IOTAEditView;
  R : TRect;
  C: TWinControl;
  i: Integer;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIDocIssueHintWindow.Display', tmoTiming);{$ENDIF}
  R := Rect(0, 0, 0, 0);
  ePosition := hpBottom;
  If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
    Begin
      TV := ES.TopView;
      If Assigned(TV) Then
        Begin
          C := TV.GetEditWindow.Form;
          For i := 0 To C.ComponentCount - 1 Do
            If CompareText(C.Components[i].ClassName, strTEditControlClsName) = 0  Then
              Begin
                C := (C.Components[i] As TWinControl);
                R := C.ClientRect;
                R.TopLeft := C.ClientToScreen(R.TopLeft);
                R.BottomRight := C.ClientToScreen(R.BottomRight);
                Dec(R.Bottom, iPadding);
                Dec(R.Right, iPadding);
                Break;
              End;
          If TV.CursorPos.Line >= TV.TopRow + TV.ViewSize.cy Div 2 Then
            ePosition := hpTop;
        End;
    End;
  R := FHintWindow.CalcHintRect(R, ePosition, Totals);
  If (doShowDocIssueTotalsInEditor In TBADIOptions.BADIOptions.Options) And
     (R.Height > (iPadding + 1)) Then
    FHintWindow.ActivateHint(R)
  Else 
    FHintWindow.ReleaseHandle;
End;

(**

  This method make the hint window transparent.

  @precon  None.
  @postcon The hint window is made transparent.

**)
Procedure TBADIDocIssueHintWindow.MakeHintTransparent;

Const
  iMaxTransparency = 255;
  iTranparency = 192;
  
Var
  Style: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'MakeHintTransparent', tmoTiming);{$ENDIF}
  If iTranparency < iMaxTransparency Then
    Begin
      Style := GetWindowLong(Handle, GWL_EXSTYLE);
      Style := Style Or WS_EX_LAYERED;
      SetWindowLong(Handle, GWL_EXSTYLE, Style);
      SetLayeredWindowAttributes(Handle, 0, iTranparency, LWA_ALPHA);
    End Else
    Begin
      Style := Style Xor WS_EX_LAYERED;
      SetWindowLong(Handle, GWL_EXSTYLE, Style);
    End;
End;

(**

  This is a mouse down event for the hint window.

  @precon  None.
  @postcon Centres the editor on the first occurrence of the clicked on document issue.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TBADIDocIssueHintWindow.MouseDownEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

Var
  iIndex: Integer;
  ES : IOTAEditorServices;
  EP : TOTAEditPos;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'MouseDownEvent', tmoTiming);{$ENDIF}
  iIndex := Y Div FItemHeight;
  If (iIndex >= Low(FTotals)) And (iIndex <= High(FTotals)) Then
    If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
      If Assigned(ES.TopView) Then
        Begin
          EP.Line := FTotals[iIndex].FFirstLine;
          EP.Col := FTotals[iIndex].FFirstCol;
          ES.TopView.CursorPos := EP;
          ES.TopView.Center(EP.Line, EP.Col);
          TBADIToolsAPIFunctions.FocusActiveEditor;
        End;
End;

(**

  This method paints the totals on the Hint Window.

  @precon  None.
  @postcon The totals are painted on the hint window.

**)
Procedure TBADIDocIssueHintWindow.Paint;

Const
  dblBlendFactor = 0.25;

Var
  R, S : TRect;
  iDocIssueType: Integer;
  strText: String;
  iHeight: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Paint', tmoTiming);{$ENDIF}
  DoubleBuffered := True;
  UpdateBaseFontInfo(R);
  Canvas.Pen.Color := BlendColour(Canvas.Brush.Color, clBlack, dblBlendFactor);
  Canvas.Rectangle(0, 0, Width, Height);
  R := Rect(0, 0, Width, Height);
  R.Inflate(-1, -1);
  // Render Totals
  Inc(R.Top, iPadding);
  Dec(R.Right, iPadding);
  For iDocIssueType := Low(FTotals) To High(FTotals) Do
    Begin
      UpdateFontInfo(FTotals[iDocIssueType]);
      R.Left := iPadding;
      iHeight := Max(Canvas.TextHeight(strTextHeightTest), iIconSize);
      R.Bottom := R.Top + iHeight;
      // Draw Label Text
      strText := FTotals[iDocIssueType].FLabel;
      Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
      Inc(R.Left, FCalcNameWidth);
      Inc(R.Left, iPadding);
      // Draw Icon
      S := R;
      Inc(S.Top, iHeight - FScopeImageList.Height);
      FScopeImageList.Draw(
        Canvas,
        S.Left,
        S.Top,
        BADIImageIndex(FTotals[iDocIssueType].FImageIndex, scNone)
      );
      Inc(R.Left, FScopeImageList.Width);
      Inc(R.Left, iPadding);
      // Draw Counter
      strText := Format('%d', [FTotals[iDocIssueType].FCounter]);
      Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
      Inc(R.Top, Canvas.TextHeight(strTextHeightTest) + iPadding);
    End;
End;

(**

  This method updates the canvas with the correct font information for rendering the hint.

  @precon  None.
  @postcon The canvas is updated with the correct font information.

  @param   R as a TRect as a constant

**)
Procedure TBADIDocIssueHintWindow.UpdateBaseFontInfo(Const R: TRect);

Var
  boolUseIDEColours: Boolean;
  TFI: TTokenFontInfo;
  ES: IOTAEditorServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateBaseFontInfo', tmoTiming);{$ENDIF}
  // Background
  boolUseIDEColours := TBADIOptions.BADIOptions.UseIDEEditorColours;
  TFI := TBADIOptions.BADIOptions.TokenFontInfo[boolUseIDEColours][ttPlainText];
  FBackColour := TFI.FBackColour;
  Canvas.Brush.Color := FBackColour;
  Canvas.FillRect(R);
  // Font
  FForeColour := TFI.FForeColour;
  Font.Color := FForeColour;
  If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
    Begin
      Font.Name := TBADIOptions.BADIOptions.TreeFontName;
      Font.Size := TBADIOptions.BADIOptions.TreeFontSize;
    End;
  Canvas.Font.Assign(Font);
End;

(**

  This method updates the font information for the individual items to be rendered.

  @precon  None.
  @postcon The canvas brush and font information is updated.

  @param   TotalInfo as a TBADITotalInfo as a constant

**)
Procedure TBADIDocIssueHintWindow.UpdateFontInfo(Const TotalInfo: TBADITotalInfo);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'UpdateFontInfo', tmoTiming);{$ENDIF}
  Canvas.Brush.Color := FBackColour;
  If TotalInfo.FBackColour <> clNone Then
    Canvas.Brush.Color := TotalInfo.FBackColour;
  Canvas.Font.Color := FForeColour;
  If TotalInfo.FForeColour <> clNone Then
    Canvas.Font.Color := TotalInfo.FForeColour;
  Canvas.Font.Style := TotalInfo.FFontStyles;
End;

(**

  A windows message hit test message handler to override the THintWindow version and allow the hint
  window to react to mouse events rather than being transparent.

  @precon  None.
  @postcon Reverse the transparency of the hint window to accept mouse events.

  @param   Message as a TWMNCHitTest as a reference

**)
Procedure TBADIDocIssueHintWindow.WMNCHitTest(Var Message: TWMNCHitTest);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'WMNCHitTest', tmoTiming);{$ENDIF}
  Message.Result := HTCLIENT;
End;

End.
