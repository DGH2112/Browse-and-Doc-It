(**
  
  This module contains a custom hint window to display the module metric totals.

  @Author  David Hoyle
  @Version 1.879
  @Date    12 Apr 2020
  
  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.DocIssuesHintWindow;

Interface

Uses
  System.Classes,
  Vcl.Controls,
  WinApi.Windows,
  BADI.Types,
  BADI.Interfaces;

Type
  (** A class to define a new hint window for the totals to be displayed within on the Code Editor. **)
  TBADIDocIssueHintWindow = Class(THintWindow)
  Strict Private
    Type
      (** A record to define the information to render. **)
      TTotalRec = Record
        FLabel      : String;
        FImageIndex : TBADIImageIndex;
        FCount      : Integer;
      End;
    Const
      (** A constant to define the padding around the contents of the hint window. **)
      iPadding = 5;
      (** A constant to represent the size of the icons. **)
      iIconSize = 16;
    Class Var
      (** A class variable to hold the singleton instance of the code editor totals hint window. **)
      FHintWindow : TBADIDocIssueHintWindow;
  Strict Private
    FTotals : TArray<TTotalRec>;
    FScopeImageList : TImageList;
    FCalcHeight : Integer;
    FCalcNameWidth : Integer;
    FCalcNumWidth : Integer;
  Strict Protected
    Procedure Paint; Override;
    Function CalcHintRect(Const Rect : TRect; Const Totals: IBADIDocIssueTotals): TRect;
      Reintroduce; Overload;
    Procedure ActivateHint(Const Rect: TRect); Reintroduce;
      Overload;
      procedure UpdateFontInfo(const R: TRect);
  Public
    Class Constructor Create;
    Class Destructor Destroy;
    Class Procedure Display(Const R : TRect; Const Totals: IBADIDocIssueTotals);
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
  Vcl.Graphics,
  Vcl.Forms,
  ToolsAPI,
  BADI.Functions,
  BADI.Options,
  BADI.DockableModuleExplorer;

Const
  (** Text for testing the height of a line of text. **)
  strTextHeightTest = 'Wg';

(**

  This method activates the hint (actual displays the hint).

  @precon  None.
  @postcon The hint is displayed.

  @param   Rect as a TRect as a constant

**)
Procedure TBADIDocIssueHintWindow.ActivateHint(Const Rect: TRect);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ActivateHint', tmoTiming);{$ENDIF}
  ActivateHint(Rect, '');
End;

(**

  This method calculates the rectangle in which the Code Editor Totals should be displayed.

  @precon  None.
  @postcon Calulcates the size and position of the hint window.

  @param   Rect   as a TRect as a constant
  @param   Totals as an IBADIDocIssueTotals as a constant
  @return  a TRect

**)
Function TBADIDocIssueHintWindow.CalcHintRect(Const Rect : TRect;
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
  iIndex := 0;
  FCalcHeight := 0;
  FCalcNameWidth := 0;
  FCalcNumWidth := 0;
  UpdateFontInfo(Result);
  SetLength(FTotals, Totals.Totals.Count);
  For Issue In Totals.Totals Do
    Begin
      FTotals[iIndex].FLabel := Issue.Key;
      FTotals[iIndex].FImageIndex := Issue.Value.FImageIndex;
      FTotals[iIndex].FCount := Issue.Value.FCounter;
      Inc(FCalcHeight, Max(Canvas.TextHeight(strTextHeightTest), iIconSize) + iPadding);
      iTextWidth := Canvas.TextWidth(FTotals[iIndex].FLabel);
      If iTextWidth > FCalcNameWidth Then
        FCalcNameWidth := iTextWidth;
      iTextWidth := Canvas.TextWidth(Format('%d', [FTotals[iIndex].FCount]));
      If iTextWidth > FCalcNumWidth Then
        FCalcNumWidth := iTextWidth;
      Inc(iIndex);
    End;
  Result.Top := Result.Bottom - (FCalcHeight);
  Result.Left := Result.Right -
    (iPadding + FCalcNameWidth + iPadding + iIconSize + iPadding + FCalcNumWidth + iPadding);
  InflateRect(Result, 1, 1);
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
  FHintWindow.ReleaseHandle;
End;

(**

  This method displays the Hint window.

  @precon  None.
  @postcon The hint window is displayed.

  @param   R      as a TRect as a constant
  @param   Totals as an IBADIDocIssueTotals as a constant

**)
Class Procedure TBADIDocIssueHintWindow.Display(Const R : TRect; Const Totals: IBADIDocIssueTotals);

Var
  Rect : TRect;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod('TBADIDocIssueHintWindow.Display', tmoTiming);{$ENDIF}
  Rect := FHintWindow.CalcHintRect(R, Totals);
  If (doShowDocIssueTotalsInEditor In TBADIOptions.BADIOptions.Options) And
     (Rect.Height > (iPadding + 1)) Then
    FHintWindow.ActivateHint(Rect)
  Else 
    FHintWindow.ReleaseHandle;
End;

(**

  This method paints the totals on the Hint Window.

  @precon  None.
  @postcon The totals are painted on the hint window.

**)
Procedure TBADIDocIssueHintWindow.Paint;

Var
  R, S : TRect;
  iDocIssueType: Integer;
  strText: String;
  iHeight: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Paint', tmoTiming);{$ENDIF}
  DoubleBuffered := True;
  R := Rect(0, 0, Width - 1, Height - 1);
  UpdateFontInfo(R);
  // Render Totals
  Inc(R.Top, iPadding);
  Dec(R.Right, iPadding);
  For iDocIssueType := Low(FTotals) To High(FTotals) Do
    Begin
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
      strText := Format('%d', [FTotals[iDocIssueType].FCount]);
      Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
      Inc(R.Top, Canvas.TextHeight(strTextHeightTest) + iPadding);
    End;
End;

(**

  This method updates the canvas with the correct font information for rendering the hint.

  @precon  None.
  @postcon Thre canvas is updated with thr correct font information.

  @param   R as a TRect as a constant

**)
Procedure TBADIDocIssueHintWindow.UpdateFontInfo(Const R: TRect);

Var
  boolUseIDEColours: Boolean;
  TFI: TTokenFontInfo;
  ES: IOTAEditorServices;

Begin
  // Background
  boolUseIDEColours := TBADIOptions.BADIOptions.UseIDEEditorColours;
  TFI := TBADIOptions.BADIOptions.TokenFontInfo[boolUseIDEColours][ttPlainText];
  Canvas.Brush.Color := TFI.FBackColour;
  Canvas.FillRect(R);
  // Font
  Font.Color := TFI.FForeColour;
  Font.Style := TFI.FStyles;
  If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
    Begin
      Font.Name := ES.EditOptions.FontName;
      Font.Size := ES.EditOptions.FontSize;
    End;
  Canvas.Font.Assign(Font);
End;

End.
