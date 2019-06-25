(**

  This module provide an options form for configuring which file extensions
  are mapped to which directories.

  @Version 1.0
  @Author  David Hoyle
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit FolderConfig;

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
  ComCtrls;

Type
  (** A class to represent the form interface. **)
  TfrmFolders = Class(TForm)
    lvFolders: TListView;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure lvFoldersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure lvFoldersCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Var DefaultDraw: Boolean);
  Private
    { Private declarations }
    Procedure LoadSettings(const strINIFileName: String);
    Procedure SaveSettings(const strINIFileName: String);
    Function GetExtList(iIndex: Integer): String;
  Public
    { Public declarations }
    Class Procedure Execute(const strINIFileName: String; Folders: TStringList);
  End;

Implementation

Uses
  IniFiles,
  FileCtrl,
  DGHLibrary,
  BADI.Base.Module, BADI.Module.Dispatcher;

{$R *.dfm}

{ TfrmFolders }

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Adds a directory to the list.

  @param   Sender as a TObject

**)
Procedure TfrmFolders.btnAddClick(Sender: TObject);

Var
  Item  : TListItem;
  strDir: String;
  j     : Integer;

Begin
  If SelectDirectory(strDir, [], 0) Then
    Begin
      Item         := lvFolders.Items.Add;
      Item.Caption := strDir;
      Item.Checked := True;
      For j := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do //FI:W528
        Item.SubItems.Add('0')
    End;
End;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected directory from the list.

  @param   Sender as a TObject

**)
Procedure TfrmFolders.btnDeleteClick(Sender: TObject);

Var
  iIndex: Integer;

Begin
  If lvFolders.ItemIndex > -1 Then
    Begin
      iIndex := lvFolders.ItemIndex;
      lvFolders.Items.Delete(iIndex);
      If iIndex > lvFolders.Items.Count - 1 Then
        Dec(iIndex);
      If lvFolders.Items.Count > 0 Then
        Begin
          If iIndex > lvFolders.Items.Count - 1 Then
            iIndex := lvFolders.Items.Count - 1;
          lvFolders.ItemIndex              := iIndex;
          lvFolders.Items[iIndex].Selected := True;
        End;
    End;
End;

(**

  This is the main interface method for invoking the dialogue.

  @precon  Folders must be a valid instance.
  @postcon Invokes the dialogue.

  @param   strINIFileName as a String as a constant
  @param   Folders        as a TStringList

**)
Class Procedure TfrmFolders.Execute(const strINIFileName: String; Folders: TStringList);

Var
  Item  : TListItem;
  i, j  : Integer;
  Column: TListColumn;

Begin
  With TfrmFolders.Create(Nil) Do
    Try
      // Columns
      For j := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
        Begin
          Column           := lvFolders.Columns.Add;
          Column.Caption   := TBADIDispatcher.BADIDispatcher.Modules[j].Extensions;
          Column.Alignment := taCenter;
          Column.Width     := 10 + lvFolders.Canvas.TextWidth(Column.Caption) + 10;
          Column.MaxWidth  := Column.Width;
        End;
      LoadSettings(strINIFileName);
      // Items
      For i := 0 To Folders.Count - 1 Do
        Begin
          Item         := lvFolders.Items.Add;
          Item.Caption := Folders.Names[i];
          Item.Checked := Integer(Folders.Objects[i]) > 0;
          For j        := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
            If Like(';*' + TBADIDispatcher.BADIDispatcher.Modules[j].Extensions + '*;', ';' +
              Folders.ValueFromIndex[i] + ';') Then
              Item.SubItems.Add('1')
            Else
              Item.SubItems.Add('0');
        End;
      If ShowModal = mrOK Then
        Begin
          Folders.Clear;
          For i := 0 To lvFolders.Items.Count - 1 Do
            Folders.AddObject(Format('%s=%s', [lvFolders.Items[i].Caption,
              GetExtList(i)]), TObject(lvFolders.Items[i].Checked));
          SaveSettings(strINIFileName);
        End;
    Finally
      Free;
    End;
End;

(**

  This method returns a semi-colon delimited list of selected file extensions
  for the given indexed folder.

  @precon  None.
  @postcon Returns a semi-colon delimited list of selected file extensions
           for the given indexed folder

  @param   iIndex as an Integer
  @return  a String

**)
Function TfrmFolders.GetExtList(iIndex: Integer): String;

Var
  i: Integer;

Begin
  Result := '';
  For i  := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
    If lvFolders.Items[iIndex].SubItems[i] = '1' Then
      Begin
        If Result <> '' Then
          Result := Result + ';';
        Result   := Result + TBADIDispatcher.BADIDispatcher.Modules[i].Extensions;
      End;
End;

(**

  This method loads the forms settings from the INI File.

  @precon  None.
  @postcon Loads the forms settings from the INI File.

  @param   strINIFileName as a String as a constant

**)
Procedure TfrmFolders.LoadSettings(const strINIFileName: String);
Begin
  With TIniFile.Create(strINIFileName) Do
    Try
      Top    := ReadInteger('FoldersDlg', 'Top', Screen.Height Div 2 - Height Div 2);
      Left   := ReadInteger('FoldersDlg', 'Left', Screen.Width Div 2 - Width Div 2);
      Height := ReadInteger('FoldersDlg', 'Height', Height);
      Width  := ReadInteger('FoldersDlg', 'Width', Width);
      lvFolders.Column[0].Width := ReadInteger('FoldersDlg', 'FolderWidth', 250);
    Finally
      Free;
    End;
End;

(**

  This method is an on CustumDrawSubItem event handler for the folder list view.

  @precon  None.
  @postcon Colours the subitems red for 0 or green for 1.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   SubItem     as an Integer
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TfrmFolders.lvFoldersCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  Var DefaultDraw: Boolean);

Begin
  If Item.SubItems[SubItem - 1] = '1' Then
    Sender.Canvas.Brush.Color := clGreen
  Else
    Sender.Canvas.Brush.Color := clRed;
End;

(**

  This is an on mouse down event handler for the folder list view.

  @precon  None.
  @postcon Finds the subitem under which the mouse is clicked and toggles the
           value of the item between 0 and 1.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfrmFolders.lvFoldersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

Var
  i, j     : Integer;
  Item     : TListItem;
  R        : TRect;
  iPosition: Integer;

Begin
  iPosition := lvFolders.Columns[0].Width;
  For i     := 0 To lvFolders.Items.Count - 1 Do
    Begin
      Item := lvFolders.Items[i];
      R    := Item.DisplayRect(drBounds);
      If (Y >= Item.Top) And (Y < Item.Top + R.Bottom - R.Top) Then
        For j := 1 To lvFolders.Columns.Count - 1 Do
          Begin
            If (X >= iPosition) AND (X < iPosition + lvFolders.Column[j].Width) Then
              Begin
                If Item.SubItems[j - 1] = '0' Then
                  Item.SubItems[j - 1] := '1'
                Else
                  Item.SubItems[j - 1] := '0';
                Exit;
              End;
            Inc(iPosition, lvFolders.Column[j].Width);
          End;
    End;
End;

(**

  This method saves the forms settings to the INI File.

  @precon  None.
  @postcon Saves the forms settings to the INI File.

  @param   strINIFileName as a String as a constant

**)
Procedure TfrmFolders.SaveSettings(const strINIFileName: String);
Begin
  With TIniFile.Create(strINIFileName) Do
    Try
      WriteInteger('FoldersDlg', 'Top', Top);
      WriteInteger('FoldersDlg', 'Left', Left);
      WriteInteger('FoldersDlg', 'Height', Height);
      WriteInteger('FoldersDlg', 'Width', Width);
      WriteInteger('FoldersDlg', 'FolderWidth', lvFolders.Column[0].Width);
    Finally
      Free;
    End;
End;

End.
