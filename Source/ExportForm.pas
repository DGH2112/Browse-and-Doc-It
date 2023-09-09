(**
  
  This module defines a dialogue for exporting modules from the current VB
  project to disk.

  @Version 1.004
  @Author  David Hoyle
  @Date    02 Sep 2023

**)
unit ExportForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, CheckLst, Buttons, ComCtrls, ImgList, BADI.Functions, System.ImageList;

type
  (** This class represents a form for exporting modules to disk. **)
  TfrmExport = class(TForm)
    lbDirectory: TDirectoryListBox;
    cmbDrive: TDriveComboBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnNewDir: TBitBtn;
    lblProject: TLabel;
    lvModules: TListView;
    ilImages: TImageList;
    btnSelect: TBitBtn;
    procedure btnNewDirClick(Sender: TObject);
    procedure lvModulesResize(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure lvModulesDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure lvModulesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(strTitle, strProject : String;
      Modules : TStringList; var strPath : String; var Rect : TRect) : Boolean;
  end;

implementation

{$R *.DFM}

(**

  This is a button on click event handler for the NewDir button.

  @precon  None.
  @postcon Prompts the user for a new directory name under the currently
           selected directory.

  @param   Sender as a TObject

**)
procedure TfrmExport.btnNewDirClick(Sender: TObject);

Var
  strDir : String;

begin
  If InputQuery('New Directory', 'Please enter the new directory name:',
    strDir) Then
    Begin
      If Not SysUtils.ForceDirectories(lbDirectory.Directory + '\' + strDir) Then
        MessageDlg('There was a problem creatig the directory.', mtError,
          [mbOK], 0)
      Else
        lbDirectory.Directory := lbDirectory.Directory + '\' + strDir;
      lbDirectory.Update;
    End;
end;

(**

  This is a class method for invoking the export dialogue and getting
  information back to the caller.

  @precon  None.
  @postcon Displays a dialogue where the users can select the modules to export
           and the directory where to export them. The Modules string list and
           the strPath string are updated with the information selected. 

  @param   strTitle   as a String
  @param   strProject as a String
  @param   Modules    as a TStringList
  @param   strPath    as a String as a reference
  @param   Rect       as a TRect as a reference
  @return  a Boolean   

**)
class Function TfrmExport.Execute(strTitle, strProject : String;
  Modules : TStringList; var strPath : String; var Rect : TRect) : Boolean;

Var
  i : Integer;
  Item : TListItem;
  State : TStatuses;

begin
  With TfrmExport.Create(Nil) Do
    Try
      Result := False;
      Caption := strTitle;
      lblProject.Caption := 'Project: ' + strProject;
      If SysUtils.DirectoryExists(strPath) Then lbDirectory.Directory := strPath;
      For i := 0 To Modules.Count - 1 Do
        Begin
          State := TStatuses(Byte(Modules.Objects[i]));
          If Not (msLocked In State) Then
            Begin
              Item := lvModules.Items.Add;
              Item.Caption := Modules.Names[i];
              Case StrToInt(Modules.Values[Modules.Names[i]]) Of
                1 : Item.ImageIndex := 0;
                2 : Item.ImageIndex := 1;
                3 : Item.ImageIndex := 2;
                11 : Item.ImageIndex := 3;
                100 : Item.ImageIndex := 4;
              Else
                Item.ImageIndex := 0;
              End;
              Item.Checked := Not (msSaved In State);
            End
        End;
      For i := 0 To Modules.Count - 1 Do
        Begin
          State := TStatuses(Byte(Modules.Objects[i]));
          If msLocked In State Then
            Begin
              Item := lvModules.Items.Add;
              Item.Caption := Modules.Names[i];
              Case StrToInt(Modules.Values[Modules.Names[i]]) Of
                1 : Item.ImageIndex := 0;
                2 : Item.ImageIndex := 1;
                3 : Item.ImageIndex := 2;
                11 : Item.ImageIndex := 3;
                100 : Item.ImageIndex := 4;
              Else
                Item.ImageIndex := 0;
              End;
              Item.Checked := Not (msSaved In State);
              Item.ImageIndex := Item.ImageIndex + 5;
            End;
        End;
      Top := Rect.Top;
      Left := Rect.Left;
      Width := Rect.Right - Rect.Left;
      Height := Rect.Bottom - Rect.Top;
      If ShowModal = mrOK Then
        Begin
          Result := True;
          Modules.Clear;
          For i := 0 To lvModules.Items.Count - 1 Do
            If lvModules.Items[i].Checked Then
              Modules.Add(lvModules.Items[i].Caption);
          strPath := lbDirectory.Directory;
          Rect.Top := Top;
          Rect.Left := Left;
          Rect.Right := Left + Width;
          Rect.Bottom := Top + Height;
        End;
    Finally
      Free;
    End;
end;

(**

  This is a component resize event handler for the list view.

  @precon  None.
  @postcon Resizes the columns width when the form is resized.

  @param   Sender as a TObject

**)
procedure TfrmExport.lvModulesResize(Sender: TObject);
begin
  lvModules.Column[0].Width := ClientWidth - 28;
end;

(**

  This method selects all the modules in the list.

  @precon  None.
  @postcon Selects all the modules in the list.

  @param   Sender as a TObject

**)
procedure TfrmExport.btnSelectClick(Sender: TObject);

Var
  i : Integer;

begin
  For i := 0 To lvModules.Items.Count - 1 Do
    lvModules.Items[i].Checked := (btnSelect.Caption = '&Select All') Or
      ((lvModules.Items[i].ImageIndex In [0..4]) And (btnSelect.Caption = '&Select'));
  If btnSelect.Caption = '&Select' Then
    btnSelect.Caption := '&Select All'
  Else If btnSelect.Caption = '&Select All' Then
    btnSelect.Caption := '&Deselect All'
  Else
    btnSelect.Caption := '&Select';
end;

(**

  This is an on draw item event handler for the list view.

  @precon  None.
  @postcon Draws the items grayed out if read-only.

  @param   Sender as a TCustomListView
  @param   Item   as a TListItem
  @param   Rect   as a TRect
  @param   State  as a TOwnerDrawState

**)
procedure TfrmExport.lvModulesDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);

Var
  R : TRect;

begin
  // Draw background
  Sender.Canvas.FillRect(Rect);
  R := Classes.Rect(Rect.Left + 33, Rect.Top, Rect.Right, Rect.Bottom);
  If odSelected In State Then
    Begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.FillRect(R);
    End;
  // Draw focused rectangle
  If odFocused In State Then
    Sender.Canvas.DrawFocusRect(R);
  // Draw check boxes
  If Item.Checked Then
    ilImages.Draw(Sender.Canvas, Rect.Left + 1, Rect.Top, 11, True)
  Else
    ilImages.Draw(Sender.Canvas, Rect.Left + 1, Rect.Top, 10, True);
  // Output Item Image
  ilImages.Draw(Sender.Canvas, Rect.Left + 17, Rect.Top, Item.ImageIndex, True);
  // Output Text
  If Item.ImageIndex In [0..4] Then
    Sender.Canvas.Font.Color := clWindowText
  Else
    Sender.Canvas.Font.Color := clGrayText;
  Sender.Canvas.TextOut(Rect.Left + 35, Rect.Top + 2, Item.Caption);
end;

(**

  This is an on mouse down event handler.

  @precon  None.
  @postcon Checks and uncheckes the items in the list.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
procedure TfrmExport.lvModulesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

Var
  Item : TListItem;

begin
  Item := lvModules.GetItemAt(X, Y);
  If Item = Nil Then Exit;
  If X In [1..17] Then Item.Checked := Not Item.Checked;
end;

end.
