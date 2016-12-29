(**

  This module contain a class which represents a frame for the modue explorer options.

  @Version 1.0
  @Author  David Hoyle
  @Date    29 Dec 2016

**)
Unit BADIModuleExlporerOpsFrame;

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
  BaseLanguageModule,
  BADICustomOptionsFrame;

Type
  (** This is a class which represents the frame interface. **)
  TfmBADIModuleExplorerFrame = Class(TFrame, IBADIOptionsFrame)
    cbxFontName: TComboBox;
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
    lbxTokenTypes: TListBox;
    udFontSize: TUpDown;
    edtFontSize: TEdit;
    lblBackColour: TLabel;
    lblForeColour: TLabel;
    lblTreeColour: TLabel;
    lblTokenLimit: TLabel;
    lblBackgroundColour: TLabel;
    lblTokenTypes: TLabel;
    lblFontSize: TLabel;
    lblFontName: TLabel;
    cbxLimits: TComboBox;
    edtLimits: TEdit;
    udLimits: TUpDown;
    Procedure lbxTokenTypesClick(Sender: TObject);
    procedure cbxBackColourChange(Sender: TObject);
    procedure cbxFontColourChange(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkStrikeoutClick(Sender: TObject);
    procedure chkUnderlineClick(Sender: TObject);
    procedure cbxLimitsChange(Sender: TObject);
    procedure udLimitsChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
  Private
    { Private declarations }
    FTokenFontInfo: Array [Low(TBADITokenType) .. High(TBADITokenType)] Of TTokenFontInfo;
    FIssueLimits : Array[Low(TLimitType)..High(TLimitType)] Of Integer;
  Public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

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

  @param   AOwner as a TComponent

**)
Constructor TfmBADIModuleExplorerFrame.Create(AOwner: TComponent);

Var
  i: Integer;
  j: TBADITokenType;

Begin
  Inherited Create(AOwner);
  For i := 0 To Screen.Fonts.Count - 1 Do
    cbxFontName.Items.Add(Screen.Fonts[i]);
  For j := Low(TBADITokenType) To High(TBADITokenType) Do
    lbxTokenTypes.Items.Add(strTokenType[j]);
  lbxTokenTypes.ItemIndex := 0;
  cbxLimits.Items.Add('Errors');
  cbxLimits.Items.Add('Warnings');
  cbxLimits.Items.Add('Hints');
  cbxLimits.Items.Add('Conflicts');
End;

(**


  This is an on click event handler for the token type list box control.

  @precon  None.
  @postcon Sets the Font Colour and style controls.


  @param   Sender as a TObject

**)
Procedure TfmBADIModuleExplorerFrame.lbxTokenTypesClick(Sender: TObject);

Begin
  With lbxTokenTypes Do
    If ItemIndex > - 1 Then
      Begin
        cbxFontColour.Selected := FTokenFontInfo[TBADITokenType(ItemIndex)].FForeColour;
        cbxBackColour.Selected := FTokenFontInfo[TBADITokenType(ItemIndex)].FBackColour;
        chkBold.Checked := fsBold In FTokenFontInfo[TBADITokenType(ItemIndex)].FStyles;
        chkItalic.Checked := fsItalic In FTokenFontInfo[TBADITokenType(ItemIndex)
          ].FStyles;
        chkUnderline.Checked := fsUnderline In FTokenFontInfo
          [TBADITokenType(ItemIndex)].FStyles;
        chkStrikeout.Checked := fsStrikeout In FTokenFontInfo
          [TBADITokenType(ItemIndex)].FStyles;
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
  For j := 0 To cbxFontName.Items.Count - 1 Do
    If cbxFontName.Items[j] = BrowseAndDocItOptions.FontName Then
      Begin
        cbxFontName.ItemIndex := j;
        Break;
      End;
  For k := Low(TBADITokenType) To High(TBADITokenType) Do
    FTokenFontInfo[k] := BrowseAndDocItOptions.TokenFontInfo[k];
  udFontSize.Position := BrowseAndDocItOptions.FontSize;
  cbxBGColour.Selected := BrowseAndDocItOptions.BGColour;
  udTokenLimit.Position := BrowseAndDocItOptions.TokenLimit;
  clbxTreeColour.Selected := BrowseAndDocItOptions.TreeColour;
  lbxTokenTypesClick(Nil);
  FIssueLimits[ltErrors] := BrowseAndDocItOptions.IssueLimits[ltErrors];
  FIssueLimits[ltWarnings] := BrowseAndDocItOptions.IssueLimits[ltWarnings];
  FIssueLimits[ltHints] := BrowseAndDocItOptions.IssueLimits[ltHints];
  FIssueLimits[ltConflicts] := BrowseAndDocItOptions.IssueLimits[ltConflicts];
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

Begin
  BrowseAndDocItOptions.FontName := cbxFontName.Text;
  For k := Low(TBADITokenType) To High(TBADITokenType) Do
    BrowseAndDocItOptions.TokenFontInfo[k] := FTokenFontInfo[k];
  BrowseAndDocItOptions.FontSize := udFontSize.Position;
  BrowseAndDocItOptions.BGColour := cbxBGColour.Selected;
  BrowseAndDocItOptions.TokenLimit := udTokenLimit.Position;
  BrowseAndDocItOptions.TreeColour := clbxTreeColour.Selected;
  BrowseAndDocItOptions.IssueLimits[ltErrors] := FIssueLimits[ltErrors];
  BrowseAndDocItOptions.IssueLimits[ltWarnings] := FIssueLimits[ltWarnings];
  BrowseAndDocItOptions.IssueLimits[ltHints] := FIssueLimits[ltHints];
  BrowseAndDocItOptions.IssueLimits[ltConflicts] := FIssueLimits[ltConflicts];
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
  Var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);

Begin
  AllowChange := (NewValue > 0) And (NewValue <= 100);
  FIssueLimits[TLimitType(cbxLimits.ItemIndex)] := NewValue;
End;

End.
