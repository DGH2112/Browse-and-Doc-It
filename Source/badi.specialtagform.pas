(**

  This form provides a mean for editing Special tags names and description
  through a single method.

  @Author  David Hoyle
  @Version 1.0
  @Date    27 Oct 2018

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
    pnlForm: TPanel;
    procedure btnOKClick(Sender: TObject);
  Strict Private
  Strict Protected
    Procedure InitialiseForm(Const SpecialTag : TBADISpecialTag);
    Procedure FinaliseForm(Var SpecialTag : TBADISpecialTag);
  Public
    Class Function Execute(Var SpecialTag : TBADISpecialTag): Boolean;
  End;

Implementation

Uses
  ToolsAPI,
  BADI.Constants, BADI.ToolsAPIUtils;

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
Begin
  Result := False;
  frm := TfrmSpecialTag.Create(Application.MainForm);
  Try
    { $IFDEF DXE102 
    TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmSpecialTag);
    TBADIToolsAPIFunctions.ApplyTheming(Self);
    {$ENDIF}
    frm.InitialiseForm(SpecialTag);
    If frm.ShowModal = mrOK Then
      Begin
        frm.FinaliseForm(SpecialTag);
        Result := True
      End;
  Finally
    frm.Free;
  End;
End;

(**

  This method updates the special tag with the changes made in the form.

  @precon  None.
  @postcon The special tag is updated.

  @param   SpecialTag as a TBADISpecialTag as a reference

**)
Procedure TfrmSpecialTag.FinaliseForm(Var SpecialTag : TBADISpecialTag);

Var
  eTagProp : TBADITagProperty;
  
Begin
  SpecialTag.FName := edtName.Text;
  SpecialTag.FDescription := edtDescription.Text;
  SpecialTag.FTagProperties := [];
  For eTagProp := Low(TBADITagProperty) To High(TBADITagProperty) Do
    If lbxTagProperties.Checked[Integer(eTagProp)] Then
      Include(SpecialTag.FTagProperties, eTagProp);
  SpecialTag.FFontStyles := [];
  If chkBold.Checked Then
    Include(SpecialTag.FFontStyles, fsBold);
  If chkItalic.Checked Then
    Include(SpecialTag.FFontStyles, fsItalic);
  If chkUnderlined.Checked Then
    Include(SpecialTag.FFontStyles, fsUnderline);
  If chkStrikeout.Checked Then
    Include(SpecialTag.FFontStyles, fsStrikeOut);
  SpecialTag.FFontColour := cbxFontColour.Selected;
  SpecialTag.FBackColour := cbxBackColour.Selected;
End;

(**

  This method intialises the fopm with the information in the special tag.

  @precon  None.
  @postcon The information for the special tag is set in the form controls.

  @param   SpecialTag as a TBADISpecialTag as a constant

**)
Procedure TfrmSpecialTag.InitialiseForm(Const SpecialTag : TBADISpecialTag);

Var
  eTagProp : TBADITagProperty;
  iIndex: Integer;

Begin
  edtName.Text := SpecialTag.FName;
  edtDescription.Text := SpecialTag.FDescription;
  For eTagProp := Low(TBADITagProperty) To High(TBADITagProperty) Do
    Begin
      iIndex := lbxTagProperties.Items.Add(strTagProperty[eTagProp]);
      lbxTagProperties.Checked[iIndex] := eTagProp In SpecialTag.FTagProperties;
    End;
  chkBold.Checked := fsBold In SpecialTag.FFontStyles;
  chkItalic.Checked := fsItalic In SpecialTag.FFontStyles;
  chkUnderlined.Checked := fsUnderline In SpecialTag.FFontStyles;
  chkStrikeout.Checked := fsStrikeOut In SpecialTag.FFontStyles;
  CbxFontColour.Selected := SpecialTag.FFontColour;
  cbxBackColour.Selected := SpecialTag.FBackColour;
End;

End.
