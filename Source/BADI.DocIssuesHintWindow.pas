(**
  
  This module contains a custom hint window to display the module metric totals.

  @Author  David Hoyle
  @Version 1.286
  @Date    08 Feb 2020
  
**)
Unit BADI.DocIssuesHintWindow;

Interface

Uses
  Vcl.Controls,
  WinApi.Windows,
  BADI.Types,
  BADI.Interfaces;

Type
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
    FTotals : Array[TLimitType] Of Integer;
  Strict Protected
    Procedure Paint; Override;
    Function CalcHintRect(Const Rect : TRect; Const Totals: IBADIDocIssueTotals): TRect;
      Reintroduce; Overload;
    Procedure ActivateHint(Const Rect: TRect); Reintroduce;
      Overload;
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
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Forms,
  ToolsAPI,
  BADI.Functions,
  BADI.Options;

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
  iHeight: Integer;
  iWidth: Integer;
  eDocIssueType: TLimitType;
  iTextWidth: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'CalcHintRect', tmoTiming);{$ENDIF}
  Result := Rect;
  iHeight := 0;
  iWidth := 0;
  For eDocIssueType := Low(TLimitType) To High(TLimitType) Do
    Begin
      FTotals[eDocIssueType] := Totals.Totals[eDocIssueType];
      If FTotals[eDocIssueType] > 0 Then
        Begin
          Inc(iHeight, Max(Canvas.TextHeight(strTextHeightTest), iIconSize) + iPadding);
          iTextWidth := Canvas.TextWidth(Format('%d', [FTotals[eDocIssueType]]));
          If iTextWidth > iWidth Then
            iWidth := iTextWidth;
        End;
    End;
  Result.Top := Result.Bottom - (iHeight);
  Result.Left := Result.Right - (iPadding + iWidth + iPadding + iIconSize + iPadding);
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
  R : TRect;
  eDocIssueType: TLimitType;
  strText: String;
  boolUseIDEColours: Boolean;
  TFI: TTokenFontInfo;
  ES : IOTAEditorServices;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Paint', tmoTiming);{$ENDIF}
  DoubleBuffered := True;
  R := Rect(0, 0, Width - 1, Height - 1);
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
  // Render Totals
  Inc(R.Top, iPadding);
  Dec(R.Right, iPadding);
  For eDocIssueType := Low(TLimitType) To High(TLimitType) Do
    Begin
      If FTotals[eDocIssueType] > 0 Then
        Begin
          R.Left := iPadding;
          R.Bottom := R.Top + Max(Canvas.TextHeight(strTextHeightTest), iIconSize);
          DrawIcon(Canvas, R, eDocIssueType);
          Inc(R.Left, iPadding);
          strText := Format('%d', [FTotals[eDocIssueType]]);
          Canvas.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
          Inc(R.Top, Canvas.TextHeight(strTextHeightTest) + iPadding);
        End;
    End;
End;

End.
