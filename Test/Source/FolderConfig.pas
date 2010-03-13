(**

  This module provide an options form for configuring which file extensions
  are mapped to which directories.

  @Version 1.0
  @Author  David Hoyle
  @Date    13 Mar 2010

**)
unit FolderConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls;

type
  (** A class to represent the form interface. **)
  TfrmFolders = class(TForm)
    lvFolders: TListView;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvFoldersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvFoldersCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    { Private declarations }
    Procedure LoadSettings(strINIFileName : String);
    Procedure SaveSettings(strINIFileName : String);
    Function GetExtList(iIndex : Integer) : String;
  public
    { Public declarations }
    Class Procedure Execute(strINIFileName : String; Folders : TStringList);
  end;

implementation

Uses
  IniFiles, FileCtrl, ModuleDispatcher, DGHLibrary;

{$R *.dfm}

{ TfrmFolders }

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Adds a directory to the list.

  @param   Sender as a TObject

**)
procedure TfrmFolders.btnAddClick(Sender: TObject);

var
  Item: TListItem;
  strDir : String;
  j : Integer;

begin
  If SelectDirectory(strDir, [], 0) Then
    Begin
      Item := lvFolders.Items.Add;
      Item.Caption := strDir;
      Item.Checked := True;
      For j := Low(Modules) To High(Modules) Do
        Item.SubItems.Add('0')
    End;
end;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected directory from the list.

  @param   Sender as a TObject

**)
procedure TfrmFolders.btnDeleteClick(Sender: TObject);

var
  iIndex: Integer;

begin
  If lvFolders.ItemIndex > -1 Then
    Begin
      iIndex := lvFolders.ItemIndex;
      lvFolders.Items.Delete(iIndex);
      If iIndex > lvFolders.Items.Count - 1 Then
        Dec(iIndex);
      lvFolders.ItemIndex := iIndex;
      lvFolders.Items[iIndex].Selected := True;
    End;
end;

(**

  This is the main interface method for invoking the dialogue.

  @precon  Folders must be a valid instance.
  @postcon Invokes the dialogue.

  @param   strINIFileName as a String
  @param   Folders        as a TStringList

**)
class procedure TfrmFolders.Execute(strINIFileName : String; Folders: TStringList);

var
  Item: TListItem;
  i, j : Integer;
  Column: TListColumn;

begin
  With TfrmFolders.Create(Nil) Do
    Try
      // Columns
      For j := Low(Modules) To High(Modules) Do
        Begin
          Column := lvFolders.Columns.Add;
          Column.Caption := Modules[j].FExt;
          Column.Alignment := taCenter;
          Column.MaxWidth := 50;
          Column.MaxWidth := 50;
        End;
      LoadSettings(strINIFileName);
      // Items
      For i := 0 To Folders.Count - 1 Do
        Begin
          Item := lvFolders.Items.Add;
          Item.Caption := Folders.Names[i];
          Item.Checked := Integer(Folders.Objects[i]) > 0;
          For j := Low(Modules) To High(Modules) Do
            If Like(';*' + Modules[j].FExt + '*;', ';' +
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
end;

(**

  This method returns a semi-colon delimited list of selected file extensions
  for the given indexed folder.

  @precon  None.
  @postcon Returns a semi-colon delimited list of selected file extensions
           for the given indexed folder

  @param   iIndex as an Integer
  @return  a String

**)
Function TfrmFolders.GetExtList(iIndex : Integer) : String;

Var
  i : Integer;

Begin
  Result := '';
  For i := Low(Modules) To High(Modules) Do
    If lvFolders.Items[iIndex].SubItems[i] = '1' Then
      Begin
        If Result <> '' Then
          Result := Result + ';';
        Result := Result + Modules[i].FExt;
      End;
End;

(**

  This method loads the forms settings from the INI File.

  @precon  None.
  @postcon Loads the forms settings from the INI File.

  @param   strINIFileName as a String

**)
procedure TfrmFolders.LoadSettings(strINIFileName : String);
begin
  With TIniFile.Create(strINIFileName) Do
    Try
      Top := ReadInteger('FoldersDlg', 'Top', Screen.Height Div 2 - Height Div 2);
      Left := ReadInteger('FoldersDlg', 'Left', Screen.Width Div 2 - Width Div 2);
      Height := ReadInteger('FoldersDlg', 'Height', Height);
      Width := ReadInteger('FoldersDlg', 'Width', Width);
      lvFolders.Column[0].Width := ReadInteger('FoldersDlg', 'FolderWidth', 250);
    Finally
      Free;
    End;
end;

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
procedure TfrmFolders.lvFoldersCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);

begin
  If Item.SubItems[SubItem - 1] = '1' Then
    Sender.Canvas.Brush.Color := clGreen
  Else
    Sender.Canvas.Brush.Color := clRed;
end;

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
procedure TfrmFolders.lvFoldersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

Var
  i, j : Integer;
  Item : TListItem;
  R : TRect;
  iPosition : Integer;

begin
  iPosition := lvFolders.Columns[0].Width;
  For i := 0 To lvFolders.Items.Count - 1 Do
    Begin
      Item := lvFolders.Items[i];
      R := Item.DisplayRect(drBounds);
      If (Y >= Item.Top) And (Y < Item.Top + R.Bottom - R.Top) Then
        For j := 1 To lvFolders.Columns.Count - 1 Do
          Begin
            If (X >= iPosition) AND (X < iPosition + lvFolders.Column[j].Width) Then
              Begin
                If Item.SubItems[j - 1] = '0' Then
                  Item.SubItems[j - 1] := '1'
                Else
                  Item.SubItems[j - 1] := '0';
                lvFolders.Invalidate;
                Exit;
              End;
            Inc(iPosition, lvFolders.Column[j].Width);
          End;
    End;
end;

(**

  This method saves the forms settings to the INI File.

  @precon  None.
  @postcon Saves the forms settings to the INI File.

  @param   strINIFileName as a String

**)
procedure TfrmFolders.SaveSettings(strINIFileName : String);
begin
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
end;

end.
