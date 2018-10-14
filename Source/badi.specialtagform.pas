(**

  This form provides a mean for editing Special tags names and description
  through a single method.

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Oct 2018

**)
Unit BADI.SpecialTagForm;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  CheckLst,
  BADI.Types,
  Vcl.ExtCtrls;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** Form for editing special tags **)
  TfrmSpecialTag = Class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblDescription: TLabel;
    edtDescription: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lbxTagProperties: TCheckListBox;
    lblTagProperties: TLabel;
    gbxFontStyles: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderlined: TCheckBox;
    chkStrikeout: TCheckBox;
    gpFontStyles: TGridPanel;
    lblFontColour: TLabel;
    cbxFontColour: TColorBox;
    cbxBackColour: TColorBox;
    lblBackColour: TLabel;
    procedure btnOKClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(Var SpecialTag : TBADISpecialTag): Boolean;
  End;

Implementation

Uses
  ToolsAPI,
  BADI.Constants;

{$R *.DFM}

{ TfrmSpecialTag }

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Ensures that the name and description are valid before confirming the dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmSpecialTag.btnOKClick(Sender: TObject);

ResourceString
  strYouNeedToSpecificValidNameForTag = 'You need to specific a valid name for the tag!';
  strYouNeedToSpecificValidDescriptionForTag = 'You need to specific a valid description for the tag!';

Begin
  If edtName.Text = '' Then
    Begin
      MessageDlg(strYouNeedToSpecificValidNameForTag, mtError, [mbOK], 0);
      ModalResult := mrNone;
    End Else
  If edtDescription.Text = '' Then
    Begin
      MessageDlg(strYouNeedToSpecificValidDescriptionForTag, mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
End;

(**

  This method accepts a name and description string and displays them for editing in the form. If the 
  user presses OK the form returns true.

  @precon  strName is the tags name, strDescription is the tags description, boolShow is a boolean value
           it indicate whether to show the tag in the browser, boolExpand is a boolean value it 
           indicate whether to expand the tag in the browser and Returns true if the OK button was 
           pressed.
  @postcon Displays the special tag form.

  @param   SpecialTag as a TBADISpecialTag as a reference
  @return  a Boolean

**)
Class Function TfrmSpecialTag.Execute(Var SpecialTag : TBADISpecialTag): Boolean;

Var
  frm : TfrmSpecialTag;
  eTagProp : TBADITagProperty;
  iIndex: Integer;
  {$IFDEF DXE102}
  ITS : IOTAIDEThemingServices250;
  {$ENDIF}

Begin
  { $IFDEF DXE102
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.RegisterFormClass(TfrmSpecialTag); // <= Blows up IDE
  {$ENDIF}
  frm := TfrmSpecialTag.Create(Application.MainForm);
  Try
    {$IFDEF DXE102}
    If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
      If ITS.IDEThemingEnabled Then
        ITS.ApplyTheme(frm);
    {$ENDIF}
    Result := False;
    frm.edtName.Text := SpecialTag.FName;
    frm.edtDescription.Text := SpecialTag.FDescription;
    For eTagProp := Low(TBADITagProperty) To High(TBADITagProperty) Do
      Begin
        iIndex := frm.lbxTagProperties.Items.Add(strTagProperty[eTagProp]);
        frm.lbxTagProperties.Checked[iIndex] := eTagProp In SpecialTag.FTagProperties;
      End;
    frm.chkBold.Checked := fsBold In SpecialTag.FFontStyles;
    frm.chkItalic.Checked := fsItalic In SpecialTag.FFontStyles;
    frm.chkUnderlined.Checked := fsUnderline In SpecialTag.FFontStyles;
    frm.chkStrikeout.Checked := fsStrikeOut In SpecialTag.FFontStyles;
    frm.cbxFontColour.Selected := SpecialTag.FFontColour;
    frm.cbxBackColour.Selected := SpecialTag.FBackColour;
    If frm.ShowModal = mrOK Then
      Begin
        SpecialTag.FName := frm.edtName.Text;
        SpecialTag.FDescription := frm.edtDescription.Text;
        SpecialTag.FTagProperties := [];
        For eTagProp := Low(TBADITagProperty) To High(TBADITagProperty) Do
          If frm.lbxTagProperties.Checked[Integer(eTagProp)] Then
            Include(SpecialTag.FTagProperties, eTagProp);
        SpecialTag.FFontStyles := [];
        If frm.chkBold.Checked Then
          Include(SpecialTag.FFontStyles, fsBold);
        If frm.chkItalic.Checked Then
          Include(SpecialTag.FFontStyles, fsItalic);
        If frm.chkUnderlined.Checked Then
          Include(SpecialTag.FFontStyles, fsUnderline);
        If frm.chkStrikeout.Checked Then
          Include(SpecialTag.FFontStyles, fsStrikeOut);
        SpecialTag.FFontColour := frm.cbxFontColour.Selected;
        SpecialTag.FBackColour := frm.cbxBackColour.Selected;
        Result := True
      End;
  Finally
    frm.Free;
  End;
End;

End.
