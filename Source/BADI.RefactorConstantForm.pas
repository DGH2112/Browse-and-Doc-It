(**
  
  This module contains a class which represents a form for editing a constant / resource string
  refactoring.

  @Author  David Hoyle
  @Version 1.0
  @date    19 Nov 2017
  
**)
Unit BADI.RefactorConstantForm;

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
  Buttons,
  BADI.Refactoring.Functions;

Type
  (** A class to represent the form for editing the refactoring. **)
  TfrmBADIRefactorConstant = Class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    edtLiteral: TEdit;
    lblLiteral: TLabel;
    lblScope: TLabel;
    cbxScope: TComboBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbxType: TComboBox;
    lblType: TLabel;
    chkNewLine: TCheckBox;
    Procedure btnOKClick(Sender: TObject);
  Strict Private
    Procedure InitialiseDialogue(Const setScopes: TBADIRefactoringScopes; 
      Const setTypes: TBADIRefactoringTypes; Var boolNewLine : Boolean);
  Strict Protected
  Public
    Class Function Execute(Var RefactoringInfo : TBADIRefactoringInfo;
      Var boolNewLine : Boolean): Boolean;
  End;

Implementation

{$R *.dfm}

Const
  (** A constant array of refactoring scope names. **)
  strScope: Array [Low(TBADIRefactoringScope) .. High(TBADIRefactoringScope)] Of String = (
    'Local', 'Implementation', 'Interface');
  (** A constant array of refactoring type names. **)
  strType: Array [Low(TBADIRefactoringType) .. High(TBADIRefactoringType)] Of String = (
    'Constant', 'ResourceString');

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Checks that the name is valid and if not displays a message.

  @param   Sender as a TObject

**)
Procedure TfrmBADIRefactorConstant.btnOKClick(Sender: TObject);

ResourceString
  strMsg = 'You must specific a valid name for the refactoring!';

Begin
  If Length(edtName.Text) = 0 Then
    Begin
      MessageDlg(strMsg, mtError, [mbOK], 0);
      edtName.SetFocus;
      ModalResult := mrNone;
    End;
End;

(**

  This method invokes the dialogue display.

  @precon  None.
  @postcon The dialogue is displayed for editing the refactoring.

  @param   RefactoringInfo as a TBADIRefactoringInfo as a reference
  @param   boolNewLine     as a Boolean as a reference
  @return  a Boolean

**)
Class Function TfrmBADIRefactorConstant.Execute(Var RefactoringInfo : TBADIRefactoringInfo;
  Var boolNewLine : Boolean): Boolean;

Var
  F: TfrmBADIRefactorConstant;

Begin
  Result := False;
  F := TfrmBADIRefactorConstant.Create(Application.MainForm);
  Try
    F.edtName.Text := RefactoringInfo.Name;
    F.edtLiteral.Text := RefactoringInfo.Token.Token;
    F.InitialiseDialogue(RefactoringInfo.Scopes, RefactoringInfo.Types, boolNewLine);
    If F.ShowModal = mrOK Then
      Begin
        RefactoringInfo.Name := F.edtName.Text;
        RefactoringInfo.Scopes := [];
        RefactoringInfo.Scopes := RefactoringInfo.Scopes +
          [TBADIRefactoringScope(Byte(F.cbxScope.ItemIndex))];
        RefactoringInfo.Types := [];
        RefactoringInfo.Types := RefactoringInfo.Types +
          [TBADIRefactoringType(Byte(F.cbxType.ItemIndex))];
        boolNewLine := F.chkNewLine.Checked;
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This method intialises the dialogue with available scopes and types.

  @precon  None.
  @postcon The form is intialised.

  @param   setScopes   as a TBADIRefactoringScopes as a constant
  @param   setTypes    as a TBADIRefactoringTypes as a constant
  @param   boolNewLine as a Boolean as a reference

**)
Procedure TfrmBADIRefactorConstant.InitialiseDialogue(Const setScopes: TBADIRefactoringScopes;
  Const setTypes: TBADIRefactoringTypes; Var boolNewLine : Boolean);

Var
  eScope: TBADIRefactoringScope;
  eType: TBADIRefactoringType;

Begin
  For eScope := Low(TBADIRefactoringScope) To High(TBADIRefactoringScope) Do
    If eScope In setScopes Then
      cbxScope.Items.Add(strScope[eScope]);
  cbxScope.ItemIndex := 0;
  cbxScope.Enabled := cbxScope.Items.Count > 1;
  For eType := Low(TBADIRefactoringType) To High(TBADIRefactoringType) Do
    If eType In setTypes Then
      cbxType.Items.Add(strType[eType]);
  cbxType.ItemIndex := 0;
  cbxType.Enabled := cbxType.Items.Count > 1;
  chkNewLine.Checked := boolNewLine;
End;

End.
