(**
  
  This module contains a form for editing the dictionaries.

  @Author  David Hoyle
  @Version 1.278
  @Date    26 Jul 2020
  
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
Unit BADI.Spelling.DictionaryEditorForm;

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
  Vcl.ExtCtrls,
  Vcl.ImgList;

Type
  (** A class to represent the form for editing the dictionaries. **)
  TfrmDictionaryEditor = Class(TForm)
    gpnlDictionaries: TGridPanel;
    lblLocal: TLabel;
    lblProject: TLabel;
    lblIgnore: TLabel;
    lbxLocal: TListBox;
    lbxProject: TListBox;
    lbxIgnore: TListBox;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    ilButtons: TImageList;
    procedure lbxLocalDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxLocalDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure lbxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  Strict Private
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
  Public
    Class Procedure Execute;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  BADI.Interfaces,
  BADI.Options,
  BADI.ToolsAPIUtils;

(**

  This method invokes the dialogue for editing the dictionaries.

  @precon  None.
  @postcon The dialogue is displayed.

**)
Class Procedure TfrmDictionaryEditor.Execute;

Var
  F : TfrmDictionaryEditor;

Begin
  F := TfrmDictionaryEditor.Create(Application.MainForm);
  Try
    TBADIToolsAPIFunctions.RegisterFormClassForTheming(TfrmDictionaryEditor, F);
    F.LoadSettings;
    If F.ShowModal = mrOK Then
      F.SaveSettings;
  Finally
    F.Free;
  End;
End;

(**

  This is a Key Down event handler for the list box controls.

  @precon  None.
  @postcon If CTRL+DEL is pressed the selected item in the sender list box is deleted.

  @param   Sender as a TObject
  @param   Key    as a Word as a reference
  @param   Shift  as a TShiftState

**)
Procedure TfrmDictionaryEditor.lbxKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);

Var
  LB: TListBox;

Begin
  If (Key= VK_DELETE) And (Shift = [ssCtrl]) Then
    If Sender Is TListBox Then
      Begin
        LB := Sender As TListBox;
        If LB.ItemIndex > - 1 Then
          LB.Items.Delete(LB.ItemIndex);
      End;
End;

(**

  This is a drag drop event handler for the list boxes.

  @precon  None.
  @postcon Deletes the selected word in the source dictionary and adds it to the sender dictionary.

  @param   Sender as a TObject
  @param   Source as a TObject
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfrmDictionaryEditor.lbxLocalDragDrop(Sender, Source: TObject; X, Y: Integer);

Var
  Src: TListBox;

Begin
  Src := (Source As TListBox);
  (Sender As TListBox).Items.Add(Src.Items[Src.ItemIndex]);
  Src.Items.Delete(Src.ItemIndex);
End;

(**

  This is a drag over event handler for the list boxes.

  @precon  None.
  @postcon If both source and sender are list boxes and they are not the same the drag operation is
           set to true.

  @param   Sender as a TObject
  @param   Source as a TObject
  @param   X      as an Integer
  @param   Y      as an Integer
  @param   State  as a TDragState
  @param   Accept as a Boolean as a reference

**)
Procedure TfrmDictionaryEditor.lbxLocalDragOver(Sender, Source: TObject; X, Y:
  Integer; State: TDragState; Var Accept: Boolean);

Begin
  Accept := (Sender Is TListBox) And (Source Is TListBox) And (Sender <> Source);
End;

(**

  This method loads the dictionaries into the list box controls.

  @precon  None.
  @postcon The dictionaries are loaded into the list box controls.

**)
Procedure TfrmDictionaryEditor.LoadSettings;

Begin
  lbxLocal.Items.Assign(TBADIOptions.BADIOptions.LocalDictionary);
  lbxProject.Items.Assign(TBADIOptions.BADIOptions.ProjectDictionary);
  lbxIgnore.Items.Assign(TBADIOptions.BADIOptions.IgnoreDictionary);
End;

(**

  This method saves the dictionaries from the list box controls.

  @precon  None.
  @postcon The dictionaries are saved from the list box controls.

**)
Procedure TfrmDictionaryEditor.SaveSettings;

Begin
  TBADIOptions.BADIOptions.LocalDictionary.Assign(lbxLocal.Items);
  TBADIOptions.BADIOptions.ProjectDictionary.Assign(lbxProject.Items);
  TBADIOptions.BADIOptions.IgnoreDictionary.Assign(lbxIgnore.Items);
End;

End.
