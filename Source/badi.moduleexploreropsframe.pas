(**

  This module contain a class which represents a frame for the modue explorer options.

  @Author  David Hoyle
  @Version 1.0
  @Date    22 Sep 2019

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
Unit BADI.ModuleExplorerOpsFrame;

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
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  BADI.Base.Module,
  BADI.CustomOptionsFrame,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class which represents the frame interface. **)
  TfmBADIModuleExplorerFrame = Class(TFrame, IBADIOptionsFrame)
    cbxTreeFontName: TComboBox;
    cbxBackColour: TColorBox;
    clbxTreeColour: TColorBox;
    udTokenLimit: TUpDown;
    edtTokenLimit: TEdit;
    cbxBGColour: TColorBox;
    gbxFontStyles: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    chkStrikeout: TCheckBox;
    cbxFontColour: TColorBox;
    udTreeFontSize: TUpDown;
    edtTreeFontSize: TEdit;
    lblBackColour: TLabel;
    lblForeColour: TLabel;
    lblTreeColour: TLabel;
    lblTokenLimit: TLabel;
    lblBackgroundColour: TLabel;
    lblTokenTypes: TLabel;
    lblTreeFontSize: TLabel;
    lblTreeFontName: TLabel;
    cbxLimits: TComboBox;
    edtLimits: TEdit;
    udLimits: TUpDown;
    edtFixedFontSize: TEdit;
    udFixedFontSize: TUpDown;
    cbxFixedFontName: TComboBox;
    lblFixedFont: TLabel;
    lblFixedFontSize: TLabel;
    lbxTokenTypes: TComboBox;
    lblIssueLimitTypes: TLabel;
    lblIssueLimit: TLabel;
    pnlModuleExplorerOps: TPanel;
    chkUseIDEEditorColours: TCheckBox;
    gbxTokenFontInfo: TGroupBox;
    GridPanel: TGridPanel;
    Procedure lbxTokenTypesClick(Sender: TObject);
    procedure cbxBackColourChange(Sender: TObject);
    procedure cbxFontColourChange(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkStrikeoutClick(Sender: TObject);
    procedure chkUnderlineClick(Sender: TObject);
    procedure cbxLimitsChange(Sender: TObject);
  Private
    { Private declarations }
    FTokenFontInfo: Array [Low(TBADITokenType) .. High(TBADITokenType)] Of TTokenFontInfo;
    FIssueLimits : Array[Low(TLimitType)..High(TLimitType)] Of Integer;
  Protected
    procedure udLimitsChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: {$IFDEF DXE50}Integer{$ELSE}SmallInt{$ENDIF}; Direction: TUpDownDirection);
  Public
    { Public declarations }
    //: @nometric MissingCONSTInParam
    Constructor Create(AOwner: TComponent); Override;
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Constants,
  BADI.Options,
  BADI.ResourceStrings;

{$R *.dfm}


{ TfmBADIModuleExplorerFrame }

(**

  This is an on change event handler for the Back Colour control.

  @precon  None.
  @postcon Updates the background colour with the new selected colour.

  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.cbxBackColourChange(Sender: TObject);

Begin
  FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FBackColour :=
    cbxBackColour.Selected;
End;

(**


  This is an on change event handler for the Font Colour control.

  @precon  None.
  @postcon Updates the internal list of Token Font Information.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.cbxFontColourChange(Sender: TObject);

Begin
  FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FForeColour :=
    cbxFontColour.Selected;
End;

(**

  This is an on change event handler for the Limits combo control.

  @precon  None.
  @postcon Disables the up down control and updates the up down controls position then
           reenables the up down control.

  @param   Sender as a TObject

**)
procedure TfmBADIModuleExplorerFrame.cbxLimitsChange(Sender: TObject);

begin
  udLimits.OnChangingEx := Nil;
  Try
    udLimits.Position := FIssueLimits[TLimitType(cbxLimits.ItemIndex)];
  Finally
    udLimits.OnChangingEx := udLimitsChangingEx;
  End;
end;

(**


  This is an on click event handler for the bold check box.

  @precon  None.
  @postcon Includes or Excludes the Bold option in the token font info style.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.chkBoldClick(Sender: TObject);

Begin
  If chkBold.Checked Then
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsBold)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsBold);
End;

(**


  This is an on click event handler for the italic check box.

  @precon  None.
  @postcon Includes or Excludes the Italic option in the token font info style.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.chkItalicClick(Sender: TObject);

Begin
  If chkItalic.Checked Then
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsItalic)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsItalic);
End;

(**


  This is an on click event handler for the Strikeout check box.

  @precon  None.
  @postcon Includes or Excludes the Strikeout option in the token font info style.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.chkStrikeoutClick(Sender: TObject);

Begin
  If chkStrikeout.Checked Then
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsStrikeout)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsStrikeout);
End;

(**


  This is an on click event handler for the Underline check box.

  @precon  None.
  @postcon Includes or Excludes the Underline option in the token font info style.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.chkUnderlineClick(Sender: TObject);

Begin
  If chkUnderline.Checked Then
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsUnderline)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsUnderline);
End;

(**

  A constructor for the TfmBADIModueExplorerFrame class.

  @precon  None.
  @postcon Initialises the frame.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TfmBADIModuleExplorerFrame.Create(AOwner: TComponent);

Var
  i: Integer;
  j: TBADITokenType;

Begin
  Inherited Create(AOwner);
  For i := 0 To Screen.Fonts.Count - 1 Do
    Begin
      cbxTreeFontName.Items.Add(Screen.Fonts[i]);
      cbxFixedFontName.Items.Add(Screen.Fonts[i]);
    End;
  For j := Low(TBADITokenType) To High(TBADITokenType) Do
    lbxTokenTypes.Items.Add(strTokenType[j]);
  lbxTokenTypes.ItemIndex := 0;
  cbxLimits.Items.Add(strErrors);
  cbxLimits.Items.Add(strWarnings);
  cbxLimits.Items.Add(strHints);
  cbxLimits.Items.Add(strDocumentationConflicts);
  cbxLimits.Items.Add(strChecks);
  cbxLimits.Items.Add(strMetrics);
  udLimits.OnChangingEx := udLimitsChangingEx;
End;

(**


  This is an on click event handler for the token type list box control.

  @precon  None.
  @postcon Sets the Font Colour and style controls.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.lbxTokenTypesClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  iIndex := lbxTokenTypes.ItemIndex;
  If iIndex > - 1 Then
    Begin
      cbxFontColour.Selected := FTokenFontInfo[TBADITokenType(iIndex)].FForeColour;
      cbxBackColour.Selected := FTokenFontInfo[TBADITokenType(iIndex)].FBackColour;
      chkBold.Checked := fsBold In FTokenFontInfo[TBADITokenType(iIndex)].FStyles;
      chkItalic.Checked := fsItalic In FTokenFontInfo[TBADITokenType(iIndex)].FStyles;
      chkUnderline.Checked := fsUnderline In FTokenFontInfo[TBADITokenType(iIndex)].FStyles;
      chkStrikeout.Checked := fsStrikeout In FTokenFontInfo[TBADITokenType(iIndex)].FStyles;
    End;
End;

(**

  This method loads the options into the frames controls.

  @precon  None.
  @postcon The frame is initialised with information from the options.

**)
Procedure TfmBADIModuleExplorerFrame.LoadSettings;

Var
  j: Integer;
  k: TBADITokenType;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  For j := 0 To cbxTreeFontName.Items.Count - 1 Do
    If cbxTreeFontName.Items[j] = TBADIOptions.BADIOptions.TreeFontName Then
      Begin
        cbxTreeFontName.ItemIndex := j;
        Break;
      End;
  For j := 0 To cbxFixedFontName.Items.Count - 1 Do
    If cbxFixedFontName.Items[j] = TBADIOptions.BADIOptions.FixedFontName Then
      Begin
        cbxFixedFontName.ItemIndex := j;
        Break;
      End;
  For k := Low(TBADITokenType) To High(TBADITokenType) Do
    FTokenFontInfo[k] := TBADIOptions.BADIOptions.TokenFontInfo[False][k];
  chkUseIDEEditorColours.Checked := TBADIOptions.BADIOptions.UseIDEEditorColours;
  udTreeFontSize.Position := TBADIOptions.BADIOptions.TreeFontSize;
  udFixedFontSize.Position := TBADIOptions.BADIOptions.FixedFontSize;
  cbxBGColour.Selected := TBADIOptions.BADIOptions.BGColour[False];
  udTokenLimit.Position := TBADIOptions.BADIOptions.TokenLimit;
  clbxTreeColour.Selected := TBADIOptions.BADIOptions.TreeColour;
  lbxTokenTypesClick(Nil);
  FIssueLimits[ltErrors] := TBADIOptions.BADIOptions.IssueLimits[ltErrors];
  FIssueLimits[ltWarnings] := TBADIOptions.BADIOptions.IssueLimits[ltWarnings];
  FIssueLimits[ltHints] := TBADIOptions.BADIOptions.IssueLimits[ltHints];
  FIssueLimits[ltConflicts] := TBADIOptions.BADIOptions.IssueLimits[ltConflicts];
  FIssueLimits[ltChecks] := TBADIOptions.BADIOptions.IssueLimits[ltChecks];
  FIssueLimits[ltMetrics] := TBADIOptions.BADIOptions.IssueLimits[ltMetrics];
  cbxLimits.ItemIndex := 0;
  cbxLimitsChange(Nil);
End;

(**

  This method saves the options from the frames controls.

  @precon  None.
  @postcon The options are saved.

**)
Procedure TfmBADIModuleExplorerFrame.SaveSettings;

Var
  k: TBADITokenType;
  TokenFontInfo: TBADITokenFontInfoTokenSet;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  TBADIOptions.BADIOptions.TreeFontName := cbxTreeFontName.Text;
  TBADIOptions.BADIOptions.FixedFontName := cbxFixedFontName.Text;
  TokenFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[False];
  For k := Low(TBADITokenType) To High(TBADITokenType) Do
    TokenFontInfo[k] := FTokenFontInfo[k];
  TBADIOptions.BADIOptions.UseIDEEditorColours := chkUseIDEEditorColours.Checked;
  TBADIOptions.BADIOptions.TokenFontInfo[False] := TokenFontInfo;
  TBADIOptions.BADIOptions.TreeFontSize := udTreeFontSize.Position;
  TBADIOptions.BADIOptions.FixedFontSize := udFixedFontSize.Position;
  TBADIOptions.BADIOptions.BGColour[False] := cbxBGColour.Selected;
  TBADIOptions.BADIOptions.TokenLimit := udTokenLimit.Position;
  TBADIOptions.BADIOptions.TreeColour := clbxTreeColour.Selected;
  TBADIOptions.BADIOptions.IssueLimits[ltErrors] := FIssueLimits[ltErrors];
  TBADIOptions.BADIOptions.IssueLimits[ltWarnings] := FIssueLimits[ltWarnings];
  TBADIOptions.BADIOptions.IssueLimits[ltHints] := FIssueLimits[ltHints];
  TBADIOptions.BADIOptions.IssueLimits[ltConflicts] := FIssueLimits[ltConflicts];
  TBADIOptions.BADIOptions.IssueLimits[ltChecks] := FIssueLimits[ltChecks];
  TBADIOptions.BADIOptions.IssueLimits[ltMetrics] := FIssueLimits[ltMetrics];
End;

(**

  This is an on change ex event handler for the limits up down control.

  @precon  None.
  @postcon Updates the Issue limits private variable with changes.

  @param   Sender      as a TObject
  @param   AllowChange as a Boolean as a reference
  @param   NewValue    as an Integer
  @param   Direction   as a TUpDownDirection

**)
Procedure TfmBADIModuleExplorerFrame.udLimitsChangingEx(Sender: TObject;
  Var AllowChange: Boolean; NewValue: {$IFDEF DXE50}Integer{$ELSE}SmallInt{$ENDIF};
  Direction: TUpDownDirection);

Begin
  //AllowChange := (NewValue > 0) And (NewValue <= 100);
  FIssueLimits[TLimitType(cbxLimits.ItemIndex)] := NewValue;
End;

End.

