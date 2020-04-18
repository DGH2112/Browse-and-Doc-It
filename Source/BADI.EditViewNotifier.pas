(**
  
  This module contains a class whichi implements the INTAEditViewNotifier for drawing on the editor.

  @Author  David Hoyle
  @Version 4.404
  @Date    18 Apr 2020
  
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
  System.Classes,
  System.Types,
  VCL.Graphics,
  WinApi.Windows,
  BADI.Interfaces,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

{$IFDEF DXE100}
Type
  (** A class which implements the INTAEditorViewNotifier for drawing on the editor. **)
  TBADIEditViewNotifier = Class(TNotifierObject, INTAEditViewNotifier)
  Strict Private
    Const
      (** A constant to define the padding between the editor content, doc issue icons and text. **)
      iPadding = 5;
    Class Var
      (** A class varaiable to determine whether the paint cycle is a full cycle or not. **)
      FFullRepaint : Boolean;
  Strict Private
    FPlainTextFontInfo   : TTokenFontInfo;
    FCommentFontInfo     : TTokenFontInfo;
    FTokenFontInfo       : TTokenFontInfo;
    //FLineHighlightColour : TColor;
    FIconsToRender       : TStringList;
    FMsgsToRender        : TStringList;
  Strict Protected
    // INTAEditViewNotifier
    Procedure BeginPaint(Const View: IOTAEditView; Var FullRepaint: Boolean);
    Procedure EditorIdle(Const View: IOTAEditView);
    Procedure EndPaint(Const View: IOTAEditView);
    Procedure PaintLine(Const View: IOTAEditView; LineNumber: Integer; Const LineText: PAnsiChar;
      Const TextWidth: Word; Const LineAttributes: TOTAAttributeArray; Const Canvas: TCanvas;
      Const TextRect: TRect; Const LineRect: TRect; Const CellSize: TSize);
    // General Methods
    Procedure DrawMsgText(Const Canvas : TCanvas; Var R : TRect; Const strText : String;
      Const LineDocIssue : TBADIDocIssueInfo);
    Procedure DrawCommentTag(Const Canvas : TCanvas; Var R : TRect; Const strText : String;
      Const LineDocIssue : TBADIDocIssueInfo);
    Procedure MarkUpdateSpecialTags(Const Canvas : TCanvas;  Const strDocIssue : String;
      Const DocIssueInfo : TBADIDocIssueInfo; Const LineText: PAnsiChar; Const TextRect : TRect;
      Const CellSize: TSize);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Class Procedure ForceFullRepaint;
  End;
{$ENDIF DXE100}

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.SysUtils,
  System.StrUtils,
  Vcl.Controls,
  Vcl.Forms,
  BADI.DockableModuleExplorer,
  BADI.Options,
  BADI.DocIssuesHintWindow,
  BADI.Functions,
  BADI.Constants;

{$IFDEF DXE100}
(**

  This method is called before painting on the editor starts.

  @precon  None.
  @postcon Not used.

  @nohint  View

  @param   View        as an IOTAEditView as a constant
  @param   FullRepaint as a Boolean as a reference

**)
Procedure TBADIEditViewNotifier.BeginPaint(Const View: IOTAEditView; Var FullRepaint: Boolean);

  (**

    This procedure updates the FIconsToRender set with a limit type if the given doc option is in the
    given doc option set.

    @precon  None.
    @postcon The FIconsToRender set is updated accordingly.

    @param   DocOps        as a TDocOptions as a constant
    @param   eDocOption    as a TDocOption as a constant
    @param   eDocIssueType as a TLimitType as a constant

  **)
  Procedure IconsToRender(Const DocOps : TDocOptions; Const eDocOption : TDocOption;
    Const eDocIssueType : TLimitType);

  Begin
    If eDocOption In DocOps Then
      FIconsToRender.Add(astrLimitType[eDocIssueType]);
  End;

  (**

    This procedure updates the FMsgoRender set with a limit type if the given doc option is in the
    given doc option set.

    @precon  None.
    @postcon The FMsgsToRender set is updated accordingly.

    @param   DocOps        as a TDocOptions as a constant
    @param   eDocOption    as a TDocOption as a constant
    @param   eDocIssueType as a TLimitType as a constant

  **)
  Procedure MsgsToRender(Const DocOps : TDocOptions; Const eDocOption : TDocOption;
    Const eDocIssueType : TLimitType);

  Begin
    If eDocOption In DocOps Then
      FMsgsToRender.Add(astrLimitType[eDocIssueType]);
  End;

Var
  DocOps: TDocOptions;
  iTag: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'BeginPaint', tmoTiming);{$ENDIF}
  FullRepaint := FFullRepaint;
  FPlainTextFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[True][ttPlainText];
  FCommentFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[True][ttCommentText];
  FTokenFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[False][ttDocIssueEditorText];
  If FTokenFontInfo.FBackColour = clNone Then
    FTokenFontInfo.FBackColour := FPlainTextFontInfo.FBackColour;
  If FTokenFontInfo.FForeColour = clNone Then
    FTokenFontInfo.FForeColour := FPlainTextFontInfo.FForeColour;
  DocOps := TBADIOptions.BADIOptions.Options;
  FIconsToRender.Clear;
  IconsToRender(DocOps, doShowErrorIconsInEditor, ltErrors);
  IconsToRender(DocOps, doShowWarningIconsInEditor, ltWarnings);
  IconsToRender(DocOps, doShowHintIconsInEditor, ltHints);
  IconsToRender(DocOps, doShowConflictIconsInEditor, ltConflicts);
  IconsToRender(DocOps, doShowCheckIconsInEditor, ltChecks);
  IconsToRender(DocOps, doShowMetricIconsInEditor, ltMetrics);
  For iTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    If tpShowInEditor In TBADIOptions.BADIOptions.SpecialTags[iTag].FTagProperties Then
      FIconsToRender.Add(TBADIOptions.BADIOptions.SpecialTags[iTag].FName);
  FMsgsToRender.Clear;
  MsgsToRender(DocOps, doShowErrorMsgsInEditor, ltErrors);
  MsgsToRender(DocOps, doShowWarningMsgsInEditor, ltWarnings);
  MsgsToRender(DocOps, doShowHintMsgsInEditor, ltHints);
  MsgsToRender(DocOps, doShowConflictMsgsInEditor, ltConflicts);
  MsgsToRender(DocOps, doShowCheckMsgsInEditor, ltChecks);
  MsgsToRender(DocOps, doShowMetricMsgsInEditor, ltMetrics);
End;

(**

  A constructor for the TBADIEditViewNotifier class.

  @precon  None.
  @postcon Creates 2 strings list for the icons and msgs to render.

**)
Constructor TBADIEditViewNotifier.Create;

Begin
  Inherited Create;
  FIconsToRender := TStringList.Create;
  FIconsToRender.Sorted := True;
  FIconsToRender.Duplicates := dupIgnore;
  FMsgsToRender := TStringList.Create;
  FMsgsToRender.Sorted := True;
  FMsgsToRender.Duplicates := dupIgnore;
End;

(**

  A destructor for the TBADIEditViewNotifier class.

  @precon  None.
  @postcon Frees the string lists.

**)
Destructor TBADIEditViewNotifier.Destroy;

Begin
  FIconsToRender.Free;
  FMsgsToRender.Free;
  Inherited Destroy;
End;

(**

  This method renders the comment tag associated with a line document issue.

  @precon  None.
  @postcon The comment tag is overwritten on the editor.

  @param   Canvas       as a TCanvas as a constant
  @param   R            as a TRect as a reference
  @param   strText      as a String as a constant
  @param   LineDocIssue as a TBADIDocIssueInfo as a constant

**)
Procedure TBADIEditViewNotifier.DrawCommentTag(Const Canvas : TCanvas; Var R : TRect;
  Const strText : String; Const LineDocIssue : TBADIDocIssueInfo);

Var
  strTextToRender : String;
  setFontStyles : TFontStyles;

Begin
  strTextToRender := strText;
  setFontStyles := Canvas.Font.Style;
  Try
    Canvas.Brush.Color := FCommentFontInfo.FBackColour;
    If LineDocIssue.FBackColour <> clNone Then
      Canvas.Brush.Color := LineDocIssue.FBackColour
    Else
      SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.Font.Style := FCommentFontInfo.FStyles;
    Canvas.Font.Color := FCommentFontInfo.FForeColour;
    If LineDocIssue.FForeColour <> clNone Then
      Canvas.Font.Color := LineDocIssue.FForeColour;
    DrawText(
      Canvas.Handle,
      PChar(strTextToRender),
      Length(strTextToRender),
      R,
      DT_LEFT Or DT_VCENTER
    );
  Finally
    Canvas.Font.Style := setFontStyles;
  End;
End;

(**

  This method renders the text message associated with a line document issue.

  @precon  None.
  @postcon The message is printed on the editor to the right of the issue icon. The left edge of the R 
           rectangle is moved to the end position of the printed message.

  @param   Canvas       as a TCanvas as a constant
  @param   R            as a TRect as a reference
  @param   strText      as a String as a constant
  @param   LineDocIssue as a TBADIDocIssueInfo as a constant

**)
Procedure TBADIEditViewNotifier.DrawMsgText(Const Canvas : TCanvas; Var R : TRect;
  Const strText : String; Const LineDocIssue : TBADIDocIssueInfo);

Var
  strTextToRender : String;
  setFontStyles : TFontStyles;

Begin
  strTextToRender := strText;
  setFontStyles := Canvas.Font.Style;
  Canvas.Font.Style := FTokenFontInfo.FStyles;
  Canvas.Font.Color := FTokenFontInfo.FForeColour;
  If LineDocIssue.FForeColour <> clNone Then
    Canvas.Font.Color := LineDocIssue.FForeColour;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  DrawText(
    Canvas.Handle,
    PChar(strTextToRender),
    Length(strTextToRender),
    R,
    DT_LEFT Or DT_VCENTER
  );
  Inc(R.Left, Canvas.TextWidth(strTextToRender));
  Canvas.Font.Style := setFontStyles;
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

Const
  strTEditControlClsName = 'TEditControl';

Var
  R : TRect;
  C: TWinControl;
  i: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'EndPaint', tmoTiming);{$ENDIF}
  R := Rect(0, 0, 0, 0);
  C := View.GetEditWindow.Form;
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
  If FFullRepaint And Application.MainForm.Visible Then
    TBADIDocIssueHintWindow.Display(R, TfrmDockableModuleExplorer.DocIssueTotals);
  FFullRepaint := False;
End;

(**

  This method sets the next paint cycle to be a full paint cycle.

  @precon  None.
  @postcon The next paint cycle will be a full cycle.

**)
Class Procedure TBADIEditViewNotifier.ForceFullRepaint;

Begin
  FFullRepaint := True;
End;

(**

  This method overwrite sthe tag name in the comment with a coloured version.

  @precon  Canvas and LineText must be valid instances.
  @postcon The special tags are overwritten with coloured text.

  @param   Canvas       as a TCanvas as a constant
  @param   strDocIssue  as a String as a constant
  @param   DocIssueInfo as a TBADIDocIssueInfo as a constant
  @param   LineText     as a PAnsiChar as a constant
  @param   TextRect     as a TRect as a constant
  @param   CellSize     as a TSize as a constant

**)
Procedure TBADIEditViewNotifier.MarkUpdateSpecialTags(Const Canvas : TCanvas; Const strDocIssue : String;
  Const DocIssueInfo : TBADIDocIssueInfo; Const LineText: PAnsiChar; Const TextRect: TRect;
  Const CellSize: TSize);
  
Var
  R: TRect;
  iPos: Integer;
  strText: String;
  
Begin
  If strDocIssue[1] = '@' Then
    Begin
      strText := UTF8ToString(LineText);
      iPos := Pos(strDocIssue, strText);
      R := TextRect;
      Inc(R.Left, Pred(iPos) * CellSize.cx);
      DrawCommentTag(Canvas, R, strDocIssue, DocIssueInfo);
    End;
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

Var
  R : TRect;
  LineDocIssue : IBADILineDocIssues;
  strDocIssue: String;
  recDocIssue : TBADIDocIssueInfo;
  iIndex: Integer;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'PaintLine', tmoTiming);{$ENDIF}
  LineDocIssue := TfrmDockableModuleExplorer.LineDocIssue(LineNumber);
  R := LineRect;
  If Assigned(LineDocIssue) Then
    For strDocIssue In LineDocIssue.Issues Do
      Begin
        recDocIssue := LineDocIssue[strDocIssue];
        If FIconsToRender.Find(recDocIssue.FName, iIndex) Then
          Begin
            TBADIOptions.BADIOptions.ScopeImageList.Draw(
              Canvas,
              R.Left,
              R.Top,
              BADIImageIndex(recDocIssue.FImageIndex, scNone)
            );
            Inc(R.Left, TBADIOptions.BADIOptions.ScopeImageList.Width + iPadding);
            // After first icon change to Text Rect
            If (R.Left > LineRect.Left) And (R.Left < TextRect.Right) Then
              R.Left := TextRect.Right + iPadding;
          End;
        If FMsgsToRender.Find(recDocIssue.FName, iIndex) Then
          Begin
            DrawMsgText(Canvas, R, recDocIssue.FMessage, recDocIssue);
            Inc(R.Left, iPadding);
          End;
        MarkUpdateSpecialTags(Canvas, strDocIssue, recDocIssue, LineText, TextRect, CellSize);
     End;
End;
{$ENDIF DXE100}

End.
