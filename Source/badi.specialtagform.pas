(**

  This form provides a mean for editing Special tags names and description
  through a single method.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Apr 2017

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
  BADI.Types;

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
  Private
    { Private declarations }
  Public
    { Public declarations }
    Class Function Execute(Var strName, strDescription: String;
      Var setTagProperties : TBADITagProperties): Boolean;
  End;

Implementation

Uses
  BADI.Constants;

{$R *.DFM}

{ TfrmSpecialTag }

(**

  This method accepts a name and description string and displays them for editing in the form. If
  the user presses OK the form returns true.

  @precon  strName is the tags name, strDescription is the tags description, boolShow is a boolean
           value it indicate whether to show the tag in the browser, boolExpand is a boolean
           value it indicate whether to expand the tag in the browser and Returns true if the OK
           button was pressed.
  @postcon Displays the special tag form.

  @param   strName          as a String as a reference
  @param   strDescription   as a String as a reference
  @param   setTagProperties as a TBADITagProperties as a reference
  @return  a Boolean

**)
Class Function TfrmSpecialTag.Execute(Var strName, strDescription: String;
  Var setTagProperties : TBADITagProperties): Boolean;

Var
  frm : TfrmSpecialTag;
  eTagProp : TBADITagProperty;
  iIndex: Integer;

Begin
  frm := TfrmSpecialTag.Create(Nil);
  Try
    Result := False;
    frm.edtName.Text := strName;
    frm.edtDescription.Text := strDescription;
    For eTagProp := Low(TBADITagProperty) To High(TBADITagProperty) Do
      Begin
        iIndex := frm.lbxTagProperties.Items.Add(strTagProperty[eTagProp]);
        frm.lbxTagProperties.Checked[iIndex] := eTagProp In setTagProperties;
      End;
    If frm.ShowModal = mrOK Then
      Begin
        strName := frm.edtName.Text;
        strDescription := frm.edtDescription.Text;
        setTagProperties := [];
        For eTagProp := Low(TBADITagProperty) To High(TBADITagProperty) Do
          If frm.lbxTagProperties.Checked[Integer(eTagProp)] Then
            Include(setTagProperties, eTagProp);
        Result := True
      End;
  Finally
    frm.Free;
  End;
End;

End.
