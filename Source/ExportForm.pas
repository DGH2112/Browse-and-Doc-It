(**
  
  This module defines a dialogue for exporting modules from the current VB
  project to disk.

  @Version 1.867
  @Author  David Hoyle
  @Date    17 Sep 2023

**)
unit ExportForm;

interface

uses
  System.SysUtils,
  System.Classes,
  System.ImageList,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.FileCtrl,
  Vcl.CheckLst,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.ImgList,
  BADI.Functions;

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
  Strict Private
    Type
      (** An enumerate to describe the module image icons. **)
      TModuleImageIndex = (miiModule, miiClass, miiProject, miiProjectGroup, miiDocument);
  Strict Protected
    Procedure InitialiseDlg(Const strTitle, strProject : String;
      Const Modules : TStringList; Const strPath : String; Const Rect : TRect);
  Public
    { Public declarations }
    Class Function Execute(Const strTitle, strProject : String;
      Const Modules : TStringList; var strPath : String; var Rect : TRect) : Boolean;
  end;

implementation

uses
  System.UITypes,
  VBIDE_TLB;

{$R *.DFM}

(**

  This is a button on click event handler for the New Directory button.

  @precon  None.
  @postcon Prompts the user for a new directory name under the currently
           selected directory.

  @param   Sender as a TObject

**)
procedure TfrmExport.btnNewDirClick(Sender: TObject);

ResourceString
  strCaption = 'New Directory';
  strPrompt = 'Please enter the new directory name:';
  strErrorMsg = 'There was a problem creating the directory.';

Var
  strDir : String;

begin
  If InputQuery(strCaption, strPrompt, strDir) Then
    Begin
      If Not System.SysUtils.ForceDirectories(lbDirectory.Directory + '\' + strDir) Then
        MessageDlg(strErrorMsg, mtError, [mbOK], 0)
      Else
        lbDirectory.Directory := lbDirectory.Directory + '\' + strDir;
      lbDirectory.Update;
    End;
end;

(**

  This method selects all the modules in the list.

  @precon  None.
  @postcon Selects all the modules in the list.

  @param   Sender as a TObject

**)
procedure TfrmExport.btnSelectClick(Sender: TObject);

ResourceString
  strSelectAll = '&Select All';
  strSelect = '&Select';
  strDeselectAll = '&Deselect All';

Var
  i : Integer;

begin
  For i := 0 To lvModules.Items.Count - 1 Do
    lvModules.Items[i].Checked :=
      (btnSelect.Caption = strSelectAll) Or
      (
        (TModuleImageIndex(lvModules.Items[i].ImageIndex) In [miiModule..miiDocument]) And
        (btnSelect.Caption = strSelect)
      );
  If btnSelect.Caption = strSelect Then
    btnSelect.Caption := strSelectAll
  Else If btnSelect.Caption = strSelectAll Then
    btnSelect.Caption := strDeselectAll
  Else
    btnSelect.Caption := strSelect;
end;

(**

  This is a class method for invoking the export dialogue and getting information back to the caller.

  @precon  None.
  @postcon Displays a dialogue where the users can select the modules to export and the directory where 
           to export them. The Modules string list and the strPath string are updated with the 
           information selected.

  @param   strTitle   as a String as a constant
  @param   strProject as a String as a constant
  @param   Modules    as a TStringList as a constant
  @param   strPath    as a String as a reference
  @param   Rect       as a TRect as a reference
  @return  a Boolean

**)
class Function TfrmExport.Execute(Const strTitle, strProject : String;
  Const Modules : TStringList; Var strPath : String; Var Rect : TRect) : Boolean;

Var
  F : TfrmExport;
  i : Integer;

begin
  F := TfrmExport.Create(Nil);
  Try
    Result := False;
    F.InitialiseDlg(strTitle, strProject, Modules, strPath, Rect);
    If F.ShowModal = mrOK Then
      Begin
        Result := True;
        Modules.Clear;
        For i := 0 To F.lvModules.Items.Count - 1 Do
          If F.lvModules.Items[i].Checked Then
            Modules.Add(F.lvModules.Items[i].Caption);
        strPath := F.lbDirectory.Directory;
        Rect.Top := F.Top;
        Rect.Left := F.Left;
        Rect.Right := F.Left + F.Width;
        Rect.Bottom := F.Top + F.Height;
      End;
  Finally
    F.Free;
  End;
end;

(**

  This method initialises the dialogue with a list of modules and whether they need to be saved.

  @precon  Modules must be a valid instance.
  @postcon The dialogue displays a list of modules to be saved.

  @param   strTitle   as a String as a constant
  @param   strProject as a String as a constant
  @param   Modules    as a TStringList as a constant
  @param   strPath    as a String as a constant
  @param   Rect       as a TRect as a constant

**)
Procedure TfrmExport.InitialiseDlg(Const strTitle, strProject : String; Const Modules : TStringList;
  Const strPath : String; Const Rect : TRect);

ResourceString
  strProjectLabel = 'Project: ';

Const
  iLockedImageStartingIndex = 5;
  vbextDocument = 100;

Var
  i: Integer;
  Item: TListItem;
  State: TStatuses;
  
Begin
  Caption := strTitle;
  lblProject.Caption := strProjectLabel + strProject;
  If System.SysUtils.DirectoryExists(strPath) Then
    lbDirectory.Directory := strPath;
  For i := 0 To Modules.Count - 1 Do
    Begin
      State := TStatuses(Byte(Modules.Objects[i]));
      If Not (msSaved In State) Then
        Begin
          Item := lvModules.Items.Add;
          Item.Caption := Modules.Names[i];
          Case StrToInt(Modules.Values[Modules.Names[i]]) Of
            vbextFileTypeModule:       Item.ImageIndex := Ord(TModuleImageIndex.miiModule);
            vbextFileTypeClass:        Item.ImageIndex := Ord(TModuleImageIndex.miiClass);
            vbextFileTypeProject:      Item.ImageIndex := Ord(TModuleImageIndex.miiProject);
            vbextFileTypeGroupProject: Item.ImageIndex := Ord(TModuleImageIndex.miiProjectGroup);
            vbextDocument:             Item.ImageIndex := Ord(TModuleImageIndex.miiDocument);
          Else
            Item.ImageIndex := 0;
          End;
          Item.Checked := True;
        End
    End;
  For i := 0 To Modules.Count - 1 Do
    Begin
      State := TStatuses(Byte(Modules.Objects[i]));
      If msSaved In State Then
        Begin
          Item := lvModules.Items.Add;
          Item.Caption := Modules.Names[i];
          Case StrToInt(Modules.Values[Modules.Names[i]]) Of
            vbextFileTypeModule:       Item.ImageIndex := Ord(TModuleImageIndex.miiModule);
            vbextFileTypeClass:        Item.ImageIndex := Ord(TModuleImageIndex.miiClass);
            vbextFileTypeProject:      Item.ImageIndex := Ord(TModuleImageIndex.miiProject);
            vbextFileTypeGroupProject: Item.ImageIndex := Ord(TModuleImageIndex.miiProjectGroup);
            vbextDocument:             Item.ImageIndex := Ord(TModuleImageIndex.miiDocument);
          Else
            Item.ImageIndex := 0;
          End;
          Item.Checked := False;
          If msLocked In State Then
            Item.ImageIndex := Item.ImageIndex + iLockedImageStartingIndex;
        End;
    End;
  Top := Rect.Top;
  Left := Rect.Left;
  Width := Rect.Width;
  Height := Rect.Height;
  Self.MakeFullyVisible();
End;

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

Const
  iUncheckedImageIndex = 10;
  iCheckedImageIndex = 11;
  iTextYOffset = 2;

Var
  R : TRect;

begin
  // Draw background
  Sender.Canvas.FillRect(Rect);
  R := System.Classes.Rect(
    Rect.Left + ilImages.Width + 1 + ilImages.Width + 1,
    Rect.Top,
    Rect.Right,
    Rect.Bottom
  );
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
    ilImages.Draw(Sender.Canvas, Rect.Left + 1, Rect.Top, iCheckedImageIndex, True)
  Else
    ilImages.Draw(Sender.Canvas, Rect.Left + 1, Rect.Top, iUncheckedImageIndex, True);
  // Output Item Image
  ilImages.Draw(Sender.Canvas, Rect.Left + ilImages.Width + 1, Rect.Top, Item.ImageIndex, True);
  // Output Text
  If TModuleImageIndex(Item.ImageIndex) In [miiModule..miiDocument] Then
    Sender.Canvas.Font.Color := clWindowText
  Else
    Sender.Canvas.Font.Color := clGrayText;
  Sender.Canvas.TextOut(
    Rect.Left + ilImages.Width + 1 + ilImages.Width + 1,
    Rect.Top + iTextYOffset,
    Item.Caption
  );
end;

(**

  This is an on mouse down event handler.

  @precon  None.
  @postcon Checks and unchecks the items in the list.

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
  If X In [0..ilImages.Width] Then
    Item.Checked := Not Item.Checked;
end;

(**

  This is a component resize event handler for the list view.

  @precon  None.
  @postcon Resizes the columns width when the form is resized.

  @param   Sender as a TObject

**)
procedure TfrmExport.lvModulesResize(Sender: TObject);

Const
  iScrollBarWidth = 28;

begin
  lvModules.Column[0].Width := ClientWidth - iScrollBarWidth;
end;

end.
