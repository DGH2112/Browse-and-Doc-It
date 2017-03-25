(**

  This module contains a frame for editing the menu actino shortcuts.

  @Author  David Hoyle
  @version 1.0
  @date    25 Mar 2017

**)
Unit BADI.MenuShortcutsFrame;

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
  BADI.CustomOptionsFrame,
  ComCtrls,
  StdCtrls,
  Buttons;

Type
  (** A class to represent a frame in which menu action shortcuts can be edited. **)
  TfmBADIMenuShortcuts = Class(TFrame, IBADIOptionsFrame)
    lvMenuShortcuts: TListView;
    hkMenuShortcut: THotKey;
    btnAssign: TBitBtn;
    procedure lvMenuShortcutsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnAssignClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  Menus,
  BADI.Types,
  BADI.Constants,
  BADI.Options;

{ TfmMenuShortcuts }

(**

  This is an on click event handler for the Assign button.

  @precon  None.
  @postcon Assgins the hot key shortcut to the selected list view item.

  @param   Sender as a TObject

**)
Procedure TfmBADIMenuShortcuts.btnAssignClick(Sender: TObject);

Begin
  lvMenuShortcuts.Items[lvMenuShortcuts.ItemIndex].SubItems[1] :=
    ShortCutToText(hkMenuShortcut.HotKey)
End;

(**

  This method loads the menu action shortcuts into the frames list view.

  @precon  None.
  @postcon The frames listview is loads with the menu shortcuts.

**)
Procedure TfmBADIMenuShortcuts.LoadSettings;

Var
  iBADIMenu : TBADIMenu;
  Item: TListItem;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    Begin
      Item := lvMenuShortcuts.Items.Add;
      Item.Caption := BADIMenus[iBADIMenu].FName;
      Item.SubItems.Add(BADIMenus[iBADIMenu].FCaption);
      Item.SubItems.Add(BrowseAndDocItOptions.MenuShortcut[iBADIMenu]);
    End;
End;

(**

  This is an on select item event handler for the list view.

  @precon  None.
  @postcon Enables or disables the hotkey control and assgined button and initialises the hotkey
           control with the selected shortcut.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TfmBADIMenuShortcuts.lvMenuShortcutsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);

Begin
  hkMenuShortcut.Enabled := Assigned(Item);
  btnAssign.Enabled := Assigned(Item);
  If Assigned(Item) Then
    hkMenuShortcut.HotKey := TextToShortcut(Item.SubItems[1]);
End;

(**

  This method saves the shortcuts back to the application settings.

  @precon  None.
  @postcon Any changes in the shortcuts are saved.

**)
Procedure TfmBADIMenuShortcuts.SaveSettings;

Var
  iBADIMenu : TBADIMenu;
  Item: TListItem;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    Begin
      Item := lvMenuShortcuts.Items[Integer(iBADIMenu)];
      BrowseAndDocItOptions.MenuShortcut[iBADIMenu] := Item.SubItems[1];
    End;
End;

End.
