(**

  This form provides a mean for editing Special tags names and description
  through a single method.

  @Author  David Hoyle
  @Version 1.496
  @Date    19 Sep 2020

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
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.ComCtrls;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** Form for editing special tags **)
  TfrmSpecialTag = Class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblDescription: TLabel;
    edtDescription: TEdit;
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
    btnCancel: TButton;
    btnOK: TButton;
    ilButtons: TImageList;
    lblImageIndex: TLabel;
    cbxImageIndex: TComboBoxEx;
    procedure btnOKClick(Sender: TObject);
  Strict Private
    FBADIImages : TImageList;
  Strict Protected
    Procedure InitialiseForm(Const BADIImages : TImageList; Const SpecialTag : TBADISpecialTag);
    Procedure FinaliseForm(Var SpecialTag : TBADISpecialTag);
  Public
    Class Function Execute(Const BADIImages : TImageList; Var SpecialTag : TBADISpecialTag): Boolean;
  End;

Implementation

Uses
  {$IFNDEF STANDALONEAPP}
  ToolsAPI,
  {$ENDIF}
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.TypInfo,
  BADI.ToolsAPIUtils,
  BADI.Constants,
  BADI.Functions;

{$R *.DFM}

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

  @precon  BADIImages must be a valid instance.
  @postcon Displays the special tag form.

  @param   BADIImages as a TImageList as a constant
  @param   SpecialTag as a TBADISpecialTag as a reference
  @return  a Boolean

**)
Class Function TfrmSpecialTag.Execute(Const BADIImages : TImageList;
  Var SpecialTag : TBADISpecialTag): Boolean;

Var
  frm : TfrmSpecialTag;
Begin
  Result := False;
  frm := TfrmSpecialTag.Create(Application.MainForm);
  Try
    TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmSpecialTag, frm);
    frm.InitialiseForm(BADIImages, SpecialTag);
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
  SpecialTag.FIconImage := TBADIImageIndex(cbxImageIndex.ItemIndex);
End;

(**

  This method initialises the form with the information in the special tag.

  @precon  None.
  @postcon The information for the special tag is set in the form controls.

  @param   BADIImages as a TImageList as a constant
  @param   SpecialTag as a TBADISpecialTag as a constant

**)
Procedure TfrmSpecialTag.InitialiseForm(Const BADIImages : TImageList;
  Const SpecialTag : TBADISpecialTag);

Const
  iFirstChar = 1;
  iCharLen = 2;

Var
  eTagProp : TBADITagProperty;
  iIndex: Integer;
  eImageIndex: TBADIImageIndex;
  strName: String;
  Item: TComboExItem;

Begin
  FBADIImages := BADIImages;
  cbxImageIndex.Images := BADIImages;
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
  For eImageIndex := Low(TBADIImageIndex) To High(TBADIImageIndex) Do
    Begin
      Item := cbxImageIndex.ItemsEx.Add;
      strName := GetEnumName(TypeInfo(TBADIImageIndex), Ord(eImageIndex));
      Delete(strName, iFirstChar, iCharLen);
      Item.Caption := strName;
      Item.ImageIndex := BADIImageIndex(eImageIndex, scNone);
    End;
  cbxImageIndex.ItemIndex := Integer(SpecialTag.FIconImage);
End;

End.
