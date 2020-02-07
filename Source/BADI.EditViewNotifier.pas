(**
  
  This module contains a class whichi implements the INTAEditViewNotifier for drawing on the editor.

  @Author  David Hoyle
  @Version 2.661
  @Date    07 Feb 2020
  
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
Unit BADI.EditViewNotifier;

Interface

Uses
  ToolsAPI,
  System.Types,
  VCL.Graphics,
  WinApi.Windows,
  BADI.Types;

Type
  (** A class which implements the INTAEditorViewNotifier for drawing on the editor. **)
  TBADIEditViewNotifier = Class(TNotifierObject, INTAEditViewNotifier)
  Strict Private
    FPlainTextFontInfo   : TTokenFontInfo;
    FTokenFontInfo       : TTokenFontInfo;
    FLineHighlightColour : TColor;
  Strict Protected
    Procedure BeginPaint(Const View: IOTAEditView; Var FullRepaint: Boolean);
    Procedure EditorIdle(Const View: IOTAEditView);
    Procedure EndPaint(Const View: IOTAEditView);
    Procedure PaintLine(Const View: IOTAEditView; LineNumber: Integer; Const LineText: PAnsiChar;
      Const TextWidth: Word; Const LineAttributes: TOTAAttributeArray; Const Canvas: TCanvas;
      Const TextRect: TRect; Const LineRect: TRect; Const CellSize: TSize);
  Public
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.DockableModuleExplorer,
  BADI.Interfaces,
  BADI.Options;

(**

  This method is called before painting on the editor starts.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod
  @nohint  View

  @param   View        as an IOTAEditView as a constant
  @param   FullRepaint as a Boolean as a reference

**)
Procedure TBADIEditViewNotifier.BeginPaint(Const View: IOTAEditView; Var FullRepaint: Boolean);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeginPaint', tmoTiming);{$ENDIF}
  FullRepaint := True;
  FPlainTextFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[True][ttPlainText];
  FTokenFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[False][ttDocIssueEditorText];
  FLineHighlightColour := TBADIOptions.BADIOptions.TokenFontInfo[True][ttLineHighlight].FBackColour;
  If FTokenFontInfo.FBackColour = clNone Then
    FTokenFontInfo.FBackColour := FPlainTextFontInfo.FBackColour;
  If FTokenFontInfo.FForeColour = clNone Then
    FTokenFontInfo.FForeColour := FPlainTextFontInfo.FForeColour;
End;

(**

  This method is called whent he editor is idle.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod
  @nohint  View

  @param   View as an IOTAEditView as a constant

**)
Procedure TBADIEditViewNotifier.EditorIdle(Const View: IOTAEditView);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EditorIdle', tmoTiming);{$ENDIF}
End;

(**

  This method is called then aining in the editor has finished.

  @precon  None.
  @postcon No used.

  @nocheck EmptyMethod
  @nohint  View

  @param   View as an IOTAEditView as a constant

**)
Procedure TBADIEditViewNotifier.EndPaint(Const View: IOTAEditView);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EndPaint', tmoTiming);{$ENDIF}
End;

(**

  This method is called after each line in the editor is painted.

  @precon  None.
  @postcon This method renders text and images ontot he editor associated with Docuemnt issues and
           conflicts.

  @nocheck MissingCONSTInParam
  @nometric LongParameterList
  @nohint  View LineText TextWidth LineAttributes CellSize

  @param   View           as an IOTAEditView as a constant
  @param   LineNumber     as an Integer
  @param   LineText       as a PAnsiChar as a constant
  @param   TextWidth      as a Word as a constant
  @param   LineAttributes as a TOTAAttributeArray as a constant
  @param   Canvas         as a TCanvas as a constant
  @param   TextRect       as a TRect as a constant
  @param   LineRect       as a TRect as a constant
  @param   CellSize       as a TSize as a constant

**)
Procedure TBADIEditViewNotifier.PaintLine(Const View: IOTAEditView; LineNumber: Integer;
  Const LineText: PAnsiChar; Const TextWidth: Word; Const LineAttributes: TOTAAttributeArray;
  Const Canvas: TCanvas; Const TextRect, LineRect: TRect; Const CellSize: TSize);

  (**

    This method renders a document issue icon onto the editor window at the end of the line with the
    issue.

    @precon  None.
    @postcon The icon is rendered on the editor window and the left of R is moved to the right of the
             drawn icon.

    @param   R          as a TRect as a reference
    @param   eLimitType as a TLimitType as a constant

  **)
  Procedure DrawIcon(Var R : TRect; Const eLimitType : TLimitType);

  Const
    astrIconResNames : Array[TLimitType] Of String = (
      'Error',
      'Warning',
      'Hint',
      'DocConflict',
      'Check',
      'Metric'
    );

  Var
    B : Vcl.Graphics.TBitMap;
    
  Begin
    B := Vcl.Graphics.TBitMap.Create;
    Try
      B.LoadFromResourceName(hInstance, astrIconResNames[eLimitType]);
      B.Transparent := True;
      Canvas.Draw(R.Left, R.Top + (R.Height - B.Height) Div 2, B);
      Inc(R.Left, B.Width);
    Finally
      B.Free;
    End;
  End;

  (**

    This method renders the text message associated with a line document issue.

    @precon  None.
    @postcon The message is printed on the editor to the right of the issue icon. The left edge of the
             R rectangle is moved to the end position of the printed message.

    @param   R       as a TRect as a reference
    @param   strText as a String as a constant

  **)
  Procedure DrawMsgText(Var R : TRect; Const strText : String);

  Var
    strTextToRender : String;
    setFontStyles : TFontStyles;

  Begin
    strTextToRender := strText;
    If View.CursorPos.Line = LineNumber Then
      Canvas.Brush.Color := FLineHighlightColour
    Else
      Canvas.Brush.Color := FTokenFontInfo.FBackColour;
    setFontStyles := Canvas.Font.Style;
    Canvas.Font.Style := FTokenFontInfo.FStyles;
    Canvas.Font.Color := FTokenFontInfo.FForeColour;
    Canvas.TextRect(R, strTextToRender, [tfLeft, tfVerticalCenter]);
    Inc(R.Left, Canvas.TextWidth(strTextToRender));
    Canvas.Font.Style := setFontStyles;
  End;

Const
  iPadding = 5;

Var
  R : TRect;
  LineDocIssue : IBADILineDocIssues;
  eLimitType: TLimitType;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'PaintLine', tmoTiming);{$ENDIF}
  LineDocIssue := TfrmDockableModuleExplorer.LineDocIssue(LineNumber);
  R := LineRect;
  R.Left := TextRect.Right;
  InflateRect(R, -iPadding, 0);
  If Assigned(LineDocIssue) Then
    For eLimitType := Low(TLimitType) To High(TLimitType) Do
      If eLimitType In LineDocIssue.Issues Then
        Begin
          Case eLimitType Of
            ltErrors: DrawIcon(R, ltErrors);
            ltWarnings: DrawIcon(R, ltWarnings);
            ltHints: DrawIcon(R, ltHints);
            ltConflicts: DrawIcon(R, ltConflicts);
            ltChecks: DrawIcon(R, ltChecks);
            ltMetrics: DrawIcon(R, ltMetrics);
          End;
          Inc(R.Left, iPadding);
          DrawMsgText(R, LineDocIssue.Message[eLimitType]);
          Inc(R.Left, iPadding);
        End;
End;

End.
