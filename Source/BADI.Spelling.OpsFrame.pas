(**
  
  This module contains a frame for configuring the dictionaries.

  @Author  David Hoyle
  @Version 1.213
  @Date    25 Jul 2020
  
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
Unit BADI.Spelling.OpsFrame;

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
  BADI.CustomOptionsFrame,
  System.ImageList,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ExtDlgs;

Type
  (** An IDE Options frame for configuring the spelling dictionaries. **)
  TframeBADISpellingOpions = Class(TFrame, IBADIOptionsFrame)
    lblLanguageDictionary: TLabel;
    edtLanguageDictionary: TButtonedEdit;
    ilBUttons: TImageList;
    edtLocalDictionary: TButtonedEdit;
    lblLocalDictionary: TLabel;
    edtIgnoreDictionary: TButtonedEdit;
    lblIgnoreDictionary: TLabel;
    dlgOpenTextFile: TOpenTextFileDialog;
    pnlButtons: TPanel;
    btnEditDictionaries: TButton;
    procedure btnEditDictionariesClick(Sender: TObject);
    Procedure edtDictionaryRightButtonClick(Sender: TObject);
  Strict Private
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
  Public
  End;

Implementation

{$R *.dfm}


Uses
  BADI.Options,
  BADI.Spelling.DictionaryEditorForm;

(**

  This is an on click event handler for the Edit Dictionaries button.

  @precon  None.
  @postcon Displays the dictionary editor form.

  @param   Sender as a TObject

**)
Procedure TframeBADISpellingOpions.btnEditDictionariesClick(Sender: TObject);

Begin
  TfrmDictionaryEditor.Execute;
End;

(**

  This is an on click event handler for the buttoned edit controls.

  @precon  None.
  @postcon Allows the user to update the edit controls with full filenames to dictionaries.

  @param   Sender as a TObject

**)
Procedure TframeBADISpellingOpions.edtDictionaryRightButtonClick(Sender: TObject);

Begin
  If dlgOpenTextFile.Execute Then
    (Sender As TButtonedEdit).Text := dlgOpenTextFile.FileName;
End;

(**

  This method loads the dictionary filenames from the options.

  @precon  None.
  @postcon The dictionary filenames are loaded.

**)
Procedure TframeBADISpellingOpions.LoadSettings;

Begin
  edtLanguageDictionary.Text := TBADIOptions.BADIOptions.LanguageDictionaryFile;
  edtLocalDictionary.Text := TBADIOptions.BADIOptions.LocalDictionaryFile;
  edtIgnoreDictionary.Text := TBADIOptions.BADIOptions.IgnoreDictionaryFile;
End;

(**

  This method save the dictionary filenames to the options.

  @precon  None.
  @postcon The dictionary filenames are saved.

**)
Procedure TframeBADISpellingOpions.SaveSettings;

Begin
  TBADIOptions.BADIOptions.LanguageDictionaryFile := edtLanguageDictionary.Text;
  TBADIOptions.BADIOptions.LocalDictionaryFile := edtLocalDictionary.Text;
  TBADIOptions.BADIOptions.IgnoreDictionaryFile := edtIgnoreDictionary.Text;
End;

End.
