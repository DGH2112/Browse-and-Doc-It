(**

  This module contains a class which represents a form for viewing, editing and
  inserts code fragments.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Oct 2009

**)
unit CodeFragmentsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, SynEditHighlighter, SynHighlighterVB, SynEdit,
  Buttons, ComCtrls, ImgList, StdActns, ActnList, Menus, IniFiles;

type
  (** A class to represent the form interface. **)
  TfrmInsertCodeFragments = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    edtCode: TSynEdit;
    SynVBSyn1: TSynVBSyn;
    btnCancel: TBitBtn;
    btnInsert: TBitBtn;
    btnOK: TBitBtn;
    btnDelete: TBitBtn;
    tvCodeFragments: TTreeView;
    ilImages: TImageList;
    btnCreate: TBitBtn;
    pmEdit: TPopupMenu;
    alEdit: TActionList;
    actEditCopy: TEditCopy;
    actEditCut: TEditCut;
    actEditDelete: TEditDelete;
    actEditPaste: TEditPaste;
    actEditSelectAll: TEditSelectAll;
    actEditUndo: TEditUndo;
    Undo1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure tvCodeFragmentsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure tvCodeFragmentsEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure tvCodeFragmentsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvCodeFragmentsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure tvCodeFragmentsDblClick(Sender: TObject);
    procedure pmEditPopup(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FFile : String;
    ExpandedNodes : TStringList;
    Function CheckForChanges : Boolean;
    procedure PopulateTree;
    function NodePath(N: TTreeNode): String;
    procedure AddFilesToNode(R: TTreeNode; strPath: String);
    procedure GetExpandedNodes(Node: TTreeNode);
    function GetNodePath(Node: TTreeNode): String;
    procedure SearchFolders(R: TTreeNode; strPath: String);
    procedure SetExpandedNodes(Node: TTreeNode);
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Function Execute(strPath : String; var R : TRect;
      var iWidth : Integer) : String;
  end;

  Function CleanFileName(strText : String) : String;

var
  (** This is the root path of the code fragment library. **)
  FPath : String;

implementation

{$R *.DFM}

Uses
  FileCtrl, ShellAPI, IDETools, BaseLanguageModule;

ResourceString
  (** A resource string to signify the code has changed. **)
  strFileUpdated = 'The code fragment "%s" has been changed. Do you want to ' +
    'update the code fragment?';


{ TfrmInsertCodeFragments }

(**

  This function cleans a given filename of invalid characters and returns the
  cleaned string.

  @precon  None.
  @postcon Cleans a given filename of invalid characters and returns the
           cleaned string.

  @param   strText as a String
  @return  a String

**)
Function CleanFileName(strText : String) : String;

Const
  strValidChars : Set of Char = [#32..#255] - ['<', '>', ':', '"', '/', '|', '\'];

Var
  i : Integer;

Begin
  For i := 1 To Length(strText) Do
    If strText[i] In strValidChars Then
      Result := Result + strText[i];
End;

(**

  This metyhod checks to see if the code editor has modified code and returns
  true if the code has been modified.

  @precon  None.
  @postcon Checks to see if the code editor has modified code and returns trueif the code has been modified.

  @return  a Boolean

**)
Function TfrmInsertCodeFragments.CheckForChanges : Boolean;

begin
  Result := True;
  If edtCode.Modified Then
    Case MessageDlg(Format(strFileUpdated, [ChangeFileExt(ExtractFileName(FFile), '')]),
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) Of
      mrYes: edtCode.Lines.SaveToFile(FFile);
      mrCancel : Result := False;
    End;
end;

(**

  This method provide the interface to the form for viewing, editing and getting
  code fragment text.

  @precon  None.
  @postcon Displays the form and returns the code fragment selected IF the
           Insert button is pressed.

  @param   strPath as a String
  @param   R       as a TRect as a reference
  @param   iWidth  as an Integer as a reference
  @return  a String

**)
class function TfrmInsertCodeFragments.Execute(strPath: String; var R : TRect;
  var iWidth : Integer): String;

Var
  frm : TfrmInsertCodeFragments;

  (**

    Updates the rectangle coordinates.

    @precon  None.
    @postcon Updates the R rectangle with the forms position.

  **)
  Procedure UpdateRect;

  Begin
    R.Top := frm.Top;
    R.Left := frm.Left;
    R.Bottom := frm.Top + frm.Height;
    R.Right := frm.Left + frm.Width;
    iWidth := frm.tvCodeFragments.Width;
  End;

begin
  FPath := strPath;
  frm := TfrmInsertCodeFragments.Create(Nil);
  Try
    frm.Top := R.Top;
    frm.Left := R.Left;
    frm.Height := R.Bottom - R.Top;
    frm.Width := R.Right - R.Left;
    frm.tvCodeFragments.Width := iWidth;
    frm.PopulateTree;
    Case frm.ShowModal Of
      mrOK :
        Begin
          frm.CheckForChanges;
          UpdateRect;
        End;
      mrIgnore :
        Begin
          frm.CheckForChanges;
          Result := frm.edtCode.Lines.Text;
          UpdateRect;
        End;
    End;
  Finally
    frm.Free;
  End;
end;

(**

  This method adds files to the nodes of the tree view based on their path.

  @precon  R must be a valid tree node.
  @postcon Adds files to the nodes of the tree view based on their path to
           produce a heirarchial structure.

  @param   R       as a TTreeNode
  @param   strPath as a String

**)
Procedure TfrmInsertCodeFragments.AddFilesToNode(R : TTreeNode; strPath : String);

Var
  N : TTreeNode;
  rec : TSearchRec;
  iRes : Integer;

Begin
  iRes := FindFirst(strPath + '*.txt', faAnyFile, rec);
  Try
    While iRes = 0 do
      Begin
        N := tvCodeFragments.Items.AddChild(R, ChangeFileExt(rec.Name, ''));
        N.ImageINdex := 1;
        N.SelectedIndex := 1;
        iRes := FindNext(rec);
      End;
  Finally
    FindClose(rec);
  End;
End;

(**

  This method searches the given folder for files and adds them to the passed
  tree node.

  @precon  R must be a valid tree node.
  @postcon Searches the given folder for files and adds them to the passed
           tree node.

  @param   R       as a TTreeNode
  @param   strPath as a String

**)
Procedure TfrmInsertCodeFragments.SearchFolders(R : TTreeNode; strPath : String);

Var
  rec : TSearchRec;
  iRes : Integer;
  N : TTreeNode;

Begin
  iRes := FindFirst(strPath + '*.*', faAnyFile, rec);
  Try
    While iRes = 0 do
      Begin
        If rec.Attr And faDirectory <> 0 Then
          If Not ((rec.Name = '.') Or (rec.Name = '..')) Then
            Begin
              N := tvCodeFragments.Items.AddChild(R, rec.Name);
              N.ImageINdex := 0;
              N.SelectedIndex := 0;
              SearchFolders(N, strPath + rec.Name + '\');
              AddFilesToNode(N, strPath + rec.Name + '\');
            End;
        iRes := FindNext(rec);
      End;
  Finally
    FindClose(rec);
  End;
End;

(**

  This method returns the path of the specified tree node.

  @precon  Node is the tree node to be pathed.
  @postcon Returns a string representation of the tree nodes path excluding
           the root item.

  @param   Node as a TTreeNode
  @return  a String

**)
Function TfrmInsertCodeFragments.GetNodePath(Node : TTreeNode) : String;

Var
  P : TTreeNode;
  str : String;

Begin
  str := '';
  P := Node;
  While P <> Nil Do
    Begin
      If P.Parent <> Nil Then
        str := P.Text + '.' + str;
      P := P.Parent;
    End;
  Result := str;
End;

(**

  This method gets the tree nodes that are currently expanded and stores them
  in a string list.

  @precon  Node is the tree node to be tested for expansion.
  @postcon Gets the tree nodes that are currently expanded and stores them
           in a string list.

  @param   Node as a TTreeNode

**)
Procedure TfrmInsertCodeFragments.GetExpandedNodes(Node : TTreeNode);

Var
  str : String;
  iIndex : Integer;

Begin
  str := GetNodePath(Node);
  If Node.Expanded Then
    Begin
      If Not ExpandedNodes.Find(str, iIndex) Then
        iIndex := ExpandedNodes.Add(str);
      ExpandedNodes.Objects[iIndex] := TObject(1);
    End Else
      If ExpandedNodes.Find(str, iIndex) Then
        ExpandedNodes.Delete(iIndex);
End;

(**

  This method expands the tree view nodes if they are foudn in the list..

  @precon  Node is the tree node to be expanded.
  @postcon Expands the tree view nodes if they are foudn in the list..

  @param   Node as a TTreeNode

**)
Procedure TfrmInsertCodeFragments.SetExpandedNodes(Node : TTreeNode);

Var
  j : Integer;

Begin
  If ExpandedNodes.Find(GetNodePath(Node), j) Then
    Node.Expanded := True;
End;

(**

  This method populates the tree view with code fragments while maintaining the
  nodes that have been expanded between sessions.

  @precon  None.
  @postcon Populates the tree view with code fragments while maintaining the
           nodes that have been expanded between sessions.

**)
Procedure TfrmInsertCodeFragments.PopulateTree;

Var
  R : TTreeNode;
  i : Integer;

Begin
  tvCodeFragments.Items.BeginUpdate;
  Try
    For i := 0 To tvCodeFragments.Items.Count - 1 Do
      GetExpandedNodes(tvCodeFragments.Items[i]);
    tvCodeFragments.Items.Clear;
    R := tvCodeFragments.Items.Add(Nil, 'Code Fragments');
    R.ImageIndex := 0;
    R.SelectedIndex := 0;
    SearchFolders(R, FPath);
    AddFilesToNode(R, FPath);
    R.Expand(False);
    For i := 0 To tvCodeFragments.Items.Count - 1 Do
      SetExpandedNodes(tvCodeFragments.Items[i]);
  Finally
    tvCodeFragments.Items.EndUpdate;
  End;
End;


(**

  This is an on changing event handler for the code fragment tree view.

  @precon  None.
  @postcon Loads the editor with the selected tree nodes code fragement after
           checking that the editor has not changes and saving the changed code.

  @param   Sender      as a TObject
  @param   Node        as a TTreeNode
  @param   AllowChange as a Boolean as a reference

**)
procedure TfrmInsertCodeFragments.tvCodeFragmentsChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);

begin
  If Node = Nil Then Exit;
  If Not CheckForChanges Then
    Begin
      AllowChange := False;
      Exit;
    End;
  If Node.ImageIndex = 0 Then
    Begin
      FFile := '';
      edtCode.Lines.Text := '';
      edtCode.Modified := False;
      Exit;
    End;
  FFile := NodePath(Node);
  If Not FileExists(FFile) Then
    Exit;
  edtCode.Lines.LoadFromFile(FFile);
  edtCode.Modified := False;
end;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected code fragment from the disk.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.btnDeleteClick(Sender: TObject);

Var
  N : TTreeNode;

begin
  N := tvCodeFragments.Selected;
  If N = Nil Then Exit;
  If N.HasChildren Then
    Begin
      MessageDlg('The directory must not have files with in it before deletion.',
        mtInformation, [mbOK], 0);
      Exit;
    End;
  Case N.ImageIndex Of
    0 : If MessageDlg(Format('Are you sure you want to delete the directory "%s" and ' +
          'all its files?', [NodePath(N)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
          RemoveDir(NodePath(N));
    1 : If MessageDlg(Format('Are you sure you want to delete the code fragment "%s"?',
          [NodePath(N)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
          DeleteFile(NodePath(N));
  End;
  PopulateTree;
end;

(**

  This is an on click event handler for the Create button.

  @precon  None.
  @postcon Creates a new code fragement on hard disk and updates the tree view.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.btnCreateClick(Sender: TObject);

Var
  N : TTreeNode;
  strFolder : String;

begin
  N := tvCodeFragments.Selected;
  If N = Nil Then Exit;
  If N.ImageIndex = 0 Then
    Begin
      If InputQuery('Create a Folder', 'Please enter the folder name', strFolder) Then
        Begin
          strFolder := CleanFileName(strFolder);
          If Not ForceDirectories(NodePath(N) + strFolder) Then
            MessageDlg(Format('Could not create folder "%s".', [NodePath(N) + strFolder]),
              mtError, [mbOK], 0);
          PopulateTree;
        End;
    End Else
      MessageDlg('To create a folder you must select a folder.', mtWarning, [mbOK], 0);
end;

(**

  This method returns a string representation of the path to the given tree
  Node.

  @precon  N must be a valid tree node.
  @postcon Returns a string representation of the path to the given tree
           Node.

  @param   N as a TTreeNode
  @return  a String

**)
Function TfrmInsertCodeFragments.NodePath(N : TTreeNode) : String;

Var
  P : TTreeNode;

Begin
  P := N.Parent;
  Try
    If P = Nil Then
      Exit;
    If N.ImageIndex = 0 Then
      Result := N.Text + '\'
    Else
      Result := N.Text + '.Txt';
    While P <> Nil Do
      Begin
        If P.Parent = Nil Then Exit;
        Result := P.Text + '\' + Result;
        P := P.Parent;
      End;
  Finally
    Result := FPath + Result;
  End;
End;

(**

  This is an on edited event handler for the tree view.

  @precon  None.
  @postcon If the tree node is edited this function renames the on diskfile to
           the new name provided.

  @param   Sender as a TObject
  @param   Node   as a TTreeNode
  @param   S      as a String as a reference

**)
procedure TfrmInsertCodeFragments.tvCodeFragmentsEdited(Sender: TObject;
  Node: TTreeNode; var S: String);

Var
  strFileName : String;

begin
  strFileName := NodePath(Node);
  If Node.ImageIndex = 1 Then
    RenameFile(strFileName, ExtractFilePath(strFileName) + CleanFileName(S) + '.Txt')
  Else
    Begin
      strFileName := Copy(strFileName, 1, Length(strFileName) - 1);
      RenameFile(strFileName, ExtractFilePath(strFileName) + CleanFileName(S))
    End;
  PopulateTree;
end;

(**

  This is an on drag over event handler for the tree view.

  @precon  None.
  @postcon If the item doesn`t have any sub node that drag operation is
           accepted.

  @param   Sender as a TObject
  @param   Source as a TObject
  @param   X      as an Integer
  @param   Y      as an Integer
  @param   State  as a TDragState
  @param   Accept as a Boolean as a reference

**)
procedure TfrmInsertCodeFragments.tvCodeFragmentsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

Var
  N : TTreeNode;

begin
  Accept := False;
  N := tvCodeFragments.GetNodeAt(X, Y);
  If N = Nil Then
    Exit;
  If N.ImageIndex = 0 Then
    Accept := True;
end;

(**

  This is an on drag drop event handler for the tree view.

  @precon  None.
  @postcon Moves the file(s) dragged and dropped to the new disk location.

  @param   Sender as a TObject
  @param   Source as a TObject
  @param   X      as an Integer
  @param   Y      as an Integer

**)
procedure TfrmInsertCodeFragments.tvCodeFragmentsDragDrop(Sender,
  Source: TObject; X, Y: Integer);

Type
  TBUffer = Array[0..1024] Of Char;

Var
  N, S : TTreeNode;
  rec : TSHFileOpStruct;
  Src, Dest : TBuffer;
  strSrc, strDest : String;

begin
  N := tvCodeFragments.GetNodeAt(X, Y);
  If N = Nil Then Exit;
  strDest := NodePath(N);
  S := tvCodeFragments.Selected;
  If S = Nil Then Exit;
  strSrc := NodePath(S);
  If strSrc[Length(strSrc)] <> '\' Then
    strDest := strDest + ExtractFileName(strSrc)
  Else
    strSrc := Copy(strSrc, 1, Length(strSrc) - 1);
  rec.Wnd := Self.Handle;
  rec.wFunc := FO_MOVE;
  FillChar(Src, SizeOf(Src), 0);
  StrCopy(Src, PChar(strSrc));
  rec.pFrom := Src;
  FillChar(Dest, SizeOf(Dest), 0);
  StrCopy(Dest, PChar(strDest));
  rec.pTo := Dest;
  rec.fFlags := FOF_ALLOWUNDO Or FOF_RENAMEONCOLLISION;
  rec.hNameMappings := Nil;
  rec.lpszProgressTitle := 'Moving Code Fragments';
  SHFileOperation(rec);
  PopulateTree;
end;

(**

  This is an on double click event handler for the tree view.

  @precon  None.
  @postcon Does not invoke the confirmation of the dialofue.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.tvCodeFragmentsDblClick(Sender: TObject);
begin
  ModalResult := mrIgnore;
end;

(**

  This is a constructor for the TfrmInsertCodeFragment class.

  @precon  None.
  @postcon Loads the expanded nodes information from the INI file.

  @param   AOwner as a TComponent

**)
constructor TfrmInsertCodeFragments.Create(AOwner: TComponent);

Var
  sl : TStringList;
  i : Integer;

begin
  Inherited Create(AOwner);
  ExpandedNodes := TStringList.Create;
  ExpandedNodes.Sorted := True;
  sl := TStringList.Create;
  Try
    With TIniFile.Create(BrowseAndDocItOptions.INIFileName) Do
      Try
        ReadSection('ExpandedNodes', sl);
        For i := 0 To sl.Count - 1 Do
          ExpandedNodes.Add(ReadString('ExpandedNodes', sl[i], ''));
      Finally
        Free;
      End;
  Finally
    sl.Free;
  End;
end;

(**

  This is a destructor for the TfrmInsertCodeFragment class.

  @precon  None.
  @postcon Saves the expanded nodes information to the INI file.

**)
destructor TfrmInsertCodeFragments.Destroy;

Var
  i : Integer;

begin
  For i := 0 To tvCodeFragments.Items.Count - 1 Do
    GetExpandedNodes(tvCodeFragments.Items[i]);
  With TIniFile.Create(BrowseAndDocItOptions.INIFileName) Do
    Try
      EraseSection('ExpandedNodes');
      For i := 0 To ExpandedNodes.Count - 1 Do
        WriteString('ExpandedNodes', Format('Item%d', [i]), ExpandedNodes[i]);
    Finally
      Free;
    End;
  ExpandedNodes.Free;
  inherited;
end;

(**

  This is an on popup event handler for updating the menu items on the pop menu.

  @precon  None.
  @postcon Updates the item items based on the condition of the editor.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.pmEditPopup(Sender: TObject);
begin
  actEditUndo.Enabled := edtCode.CanUndo;
  actEditCut.Enabled := edtCode.SelAvail;
  actEditCopy.Enabled := edtCode.SelAvail;
  actEditPaste.Enabled := edtCode.CanPaste;
  actEditDelete.Enabled := edtCode.SelAvail;
end;

(**

  This is an on execute event handler for the Edit Undo action.

  @precon  None.
  @postcon Undoes the last edit in the editor.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.actEditUndoExecute(Sender: TObject);
begin
  edtCode.Undo;
end;

(**

  This is an on execute event handler for the Edit Cut action.

  @precon  None.
  @postcon Cuts the selected text to the clipboard.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.actEditCutExecute(Sender: TObject);
begin
  edtCode.CutToClipboard;
end;

(**

  This is an on execute event handler for the Edit Copy action.

  @precon  None.
  @postcon Copies the selected code to the clipboard.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.actEditCopyExecute(Sender: TObject);
begin
  edtCode.CopyToClipboard;
end;

(**

  This is an on execute event handler for the Edit Paste action.

  @precon  None.
  @postcon Pastes the text on the clipboard at the current cursor position.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.actEditPasteExecute(Sender: TObject);
begin
  edtCode.PasteFromClipboard;
end;

(**

  This is an on execute event handler for the Edit Delete action.

  @precon  None.
  @postcon Clears the selected text in thr editor.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.actEditDeleteExecute(Sender: TObject);
begin
  edtCode.ClearSelection;
end;

(**

  This is an on execute event handler for the Edit Select All action.

  @precon  None.
  @postcon Selects all the text in the editor.

  @param   Sender as a TObject

**)
procedure TfrmInsertCodeFragments.actEditSelectAllExecute(Sender: TObject);
begin
  edtCode.SelectAll;
end;

(**

  This is a on key down event handler for the form.

  @precon  None.
  @postcon Allows the editing of the treeview text after pressing F2.

  @param   Sender as a TObject
  @param   Key    as a Word as a reference
  @param   Shift  as a TShiftState

**)
procedure TfrmInsertCodeFragments.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  If (Key = VK_F2) Then
    If tvCodeFragments.Selected <> Nil Then
      tvCodeFragments.Selected.EditText;
end;

end.
