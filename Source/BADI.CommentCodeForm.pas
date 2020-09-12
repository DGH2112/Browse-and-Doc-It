(**
  
  This module contains a class which represents a form for selecting the type of comment to insert along
  with an options documentation tag.

  @Author  David Hoyle
  @Version 1.487
  @Date    12 Sep 2020
  
**)
Unit BADI.CommentCodeForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  BADI.Types;

Type
  (** A class to represent the dialogue for inserting comments with tags. **)
  TfrmCommentCode = Class(TForm)
    lblCommentStyle: TLabel;
    cbxCommentType: TComboBox;
    cbxTagName: TComboBox;
    lblTagName: TLabel;
    pnlFudgePanel: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
  Strict Private
  Strict Protected
    Procedure InitialiseDlg(Const strTagName : String; Const eCommentType : TCommentType);
    Procedure FinaliseDlg(Var strTagName : String; Var eCommentType : TCommentType);
    procedure InitialiseCommentTypes(const eCommentType: TCommentType);
    procedure InitialiseTagNames(const strTagName: String);
  Public
    Class Function Execute(Var strTagName : String; Var eCommentType : TCommentType) : Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  ToolsAPI,
  BADI.Interfaces,
  BADI.Module.Dispatcher,
  BADI.Constants,
  BADI.ToolsAPIUtils,
  BADI.Options;

(**

  This method is the main way to invoke the dialogue.

  @precon  None.
  @postcon Displays the dialogue in which the user can select the comment type and options tag.

  @param   strTagName   as a String as a reference
  @param   eCommentType as a TCommentType as a reference
  @return  a Boolean

**)
Class Function TfrmCommentCode.Execute(Var strTagName : String; Var eCommentType : TCommentType) : Boolean;

Var
  F: TfrmCommentCode;

Begin
  Result := False;
  F := TfrmCommentCode.Create(Application.MainForm);
  Try
    F.InitialiseDlg(strTagName, eCommentType);
    If F.ShowModal = mrOK Then
      Begin
        F.FinaliseDlg(strTagName, eCommentType);
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This method finalises the dialogue be returning the settings selected by the user.

  @precon  None.
  @postcon The var parameters are updated with the selections in the dialogue.

  @param   strTagName   as a String as a reference
  @param   eCommentType as a TCommentType as a reference

**)
Procedure TfrmCommentCode.FinaliseDlg(Var strTagName : String; Var eCommentType : TCommentType);

Var
  eCmtType: TCommentType;
  strCmtType: String;

Begin
  strTagName := cbxTagName.Text;
  For eCmtType := Low(TCommentType) To High(TCommentType) Do
    Begin
      strCmtType := Format('%s ... %s', [
        astrCmtTerminals[eCmtType].FStart,
        astrCmtTerminals[eCmtType].FBlockEnd
        ]);
      If strCmtType = cbxCommentType.Text Then
        eCommentType := eCmtType;
    End;
End;

(**

  This is an On Form Create Event Handler for the TfrmCommentCode class.

  @precon  None.
  @postcon Registers the form for theming in the IDE.

  @param   Sender as a TObject

**)
Procedure TfrmCommentCode.FormCreate(Sender: TObject);

Begin
  TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmCommentCode, Self);
End;

(**

  This method initialises the comment types control.

  @precon  None.
  @postcon The comment type list is initialised and set to the given comment type.

  @param   eCommentType as a TCommentType as a constant

**)
Procedure TfrmCommentCode.InitialiseCommentTypes(Const eCommentType: TCommentType);

Var
  MS: IOTAMOduleServices;
  strFileName: String;
  eCmtType: TCommentType;
  setCommentTypes: TCommentTypes;
  iStyleIndex: Integer;

Begin
  iStyleIndex := - 1;
  If Supports(BorlandIDEServices, IOTAMOduleServices, MS) Then
    Begin
      strFileName := ExtractFileName(MS.CurrentModule.FileName);
      setCommentTypes := TBADIDispatcher.BADIDispatcher.GetCommentTypes(strFileName);
      For eCmtType := Low(TCommentType) To High(TCommentType) Do
        If eCmtType In setCommentTypes Then
          Begin
            cbxCommentType.Items.Add(Format('%s ... %s', [
              astrCmtTerminals[eCmtType].FStart,
              astrCmtTerminals[eCmtType].FBlockEnd
              ]));
            If eCmtType = eCommentType Then
              iStyleIndex := cbxCommentType.Items.Count - 1;
          End;
      cbxCommentType.ItemIndex := iStyleIndex;
      If (cbxCommentType.ItemIndex = - 1) And (cbxCommentType.Items.Count > 0) Then
        cbxCommentType.ItemIndex := 0;
    End;
End;

(**

  This method initialises the dialogue contents.

  @precon  None.
  @postcon The dialogue is initialised.

  @param   strTagName   as a String as a constant
  @param   eCommentType as a TCommentType as a constant

**)
Procedure TfrmCommentCode.InitialiseDlg(Const strTagName : String; Const eCommentType : TCommentType);

Begin
  InitialiseTagNames(strTagName);
  InitialiseCommentTypes(eCommentType);
End;

(**

  This method initialises the Tag names component.

  @precon  None.
  @postcon The list of tag names is initialised and the combo box set to the given tag name.

  @param   strTagName as a String as a constant

**)
Procedure TfrmCommentCode.InitialiseTagNames(Const strTagName: String);

Var
  iTag: Integer;

Begin
  cbxTagName.Items.Add('');
  For iTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    cbxTagName.Items.Add(TBADIOptions.BADIOptions.SpecialTags[iTag].FName);
  cbxTagName.ItemIndex := cbxTagName.Items.IndexOf(strTagName);
  If (cbxTagName.ItemIndex = - 1) And (cbxTagName.Items.Count > 0) Then
    cbxTagName.ItemIndex := 0;
End;

End.
