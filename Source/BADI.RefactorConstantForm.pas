(**
  
  This module contains a class which represents a form for editing a constant / resource string
  refactoring.

  @Author  David Hoyle
  @Version 1.037
  @Date    21 Nov 2021

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
  ExtCtrls,
  BADI.Refactoring.Functions;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent the form for editing the refactoring. **)
  TfrmBADIRefactorConstant = Class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    edtLiteral: TEdit;
    lblLiteral: TLabel;
    lblScope: TLabel;
    cbxScope: TComboBox;
    cbxType: TComboBox;
    lblType: TLabel;
    chkNewLine: TCheckBox;
    pnlFudgePanel: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    ilButtons: TImageList;
    Procedure btnOKClick(Sender: TObject);
  Strict Private
    FRefactoringInfo : TBADIRefactoringInfo;
  Strict Protected
    Procedure InitialiseDialogue(Const setScopes: TBADIRefactoringScopes; 
      Const setTypes: TBADIRefactoringTypes; Var boolNewLine : Boolean);
  Public
    Class Function Execute(Var RefactoringInfo : TBADIRefactoringInfo;
      Var boolNewLine : Boolean): Boolean;
  End;

Implementation

Uses
  {$IFDEF RS102}
  ToolsAPI,
  {$ENDIF RS102}
  BADI.ResourceStrings, 
  BADI.ElementContainer, BADI.ToolsAPIUtils;

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
  strAlreadyExistsInMethodMsg = 'The identifier "%s" already exists in method "%s"!';
  strAlreadyExistsInModuleMsg = 'The identifier "%s" already exists in module "%s"!';

Const
  strSectionsToCheck : Array[1..5] Of String = (
    strUses, strTypesLabel, strResourceStringsLabel, strConstantsLabel, strVarsLabel);
var
  iSection: Integer;
  E: TElementContainer;
    
Begin
  If Length(edtName.Text) = 0 Then
    Begin
      MessageDlg(strMsg, mtError, [mbOK], 0);
      edtName.SetFocus;
      ModalResult := mrNone;
    End;
  If Assigned(FRefactoringInfo.Method) Then
    For iSection := Low(strSectionsToCheck) To High(strSectionsToCheck) Do
      Begin
        E := FRefactoringInfo.Method.FindElement(strSectionsToCheck[iSection]);
        If Assigned(E) Then
          Begin
            E := E.FindElement(edtName.text);
            If Assigned(E) Then
              Begin
                MessageDlg(Format(strAlreadyExistsInMethodMsg, [edtName.Text,
                  FRefactoringInfo.Method.QualifiedName]), mtError, [mbOK], 0);
                edtName.SetFocus;
                ModalResult := mrNone;
                Exit;
              End;
          End;
      End;
  For iSection := Low(strSectionsToCheck) To High(strSectionsToCheck) Do
    Begin
      E := FRefactoringInfo.Module.FindElement(strSectionsToCheck[iSection]);
      If Assigned(E) Then
        Begin
          E := E.FindElement(edtName.text);
          If Assigned(E) Then
            Begin
              MessageDlg(Format(strAlreadyExistsInModuleMsg, [edtName.Text,
                FRefactoringInfo.Module.Name]), mtError, [mbOK], 0);
              edtName.SetFocus;
              ModalResult := mrNone;
              Exit;
            End;
        End;
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
  eScope: TBADIRefactoringScope;
  eType : TBADIRefactoringType;

Begin
  Result := False;
  F := TfrmBADIRefactorConstant.Create(Application.MainForm);
  Try
    TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmBADIRefactorConstant, F);
    F.edtName.Text := RefactoringInfo.Name;
    F.edtLiteral.Text := RefactoringInfo.Token.Token;
    F.InitialiseDialogue(RefactoringInfo.Scopes, RefactoringInfo.Types, boolNewLine);
    F.FRefactoringInfo := RefactoringInfo;
    If F.ShowModal = mrOK Then
      Begin
        RefactoringInfo.Name := F.edtName.Text;
        RefactoringInfo.Scopes := [];
        For eScope := Low(TBADIRefactoringScope) To High(TBADIRefactoringScope) Do
          If Comparetext(F.cbxScope.Text, strScope[eScope]) = 0 Then
            Begin
              RefactoringInfo.Scopes := RefactoringInfo.Scopes + [eScope];
              Break;
            End;
        RefactoringInfo.Types := [];
        For eType := Low(TBADIRefactoringType) To High(TBADIRefactoringType) Do
          If Comparetext(F.cbxType.Text, strType[eType]) = 0 Then
            Begin
              RefactoringInfo.Types := RefactoringInfo.Types + [eType];
              Break;
            End;
        boolNewLine := F.chkNewLine.Checked;
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

(**

  This method initialises the dialogue with available scopes and types.

  @precon  None.
  @postcon The form is initialised.

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
