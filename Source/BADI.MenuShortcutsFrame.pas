(**

  This module contains a frame for editing the menu actino shortcuts.

  @Author  David Hoyle
  @version 1.0
  @date    14 Oct 2018

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
  BADI.Types,
  BADI.CustomOptionsFrame,
  ComCtrls,
  StdCtrls,
  Buttons;

Type
  (** A class to represent a frame in which menu action shortcuts can be edited. **)
  TfmBADIMenuShortcuts = Class(TFrame, IBADIOptionsFrame, IBADIInstallShortcutUsedCallBack)
    lvMenuShortcuts: TListView;
    hkMenuShortcut: THotKey;
    lblInformation: TLabel;
    btnAssign: TButton;
    procedure lvMenuShortcutsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnAssignClick(Sender: TObject);
    procedure hkMenuShortcutChange(Sender: TObject);
  Strict Private
    { Private declarations }
    FShortcutUsedEvent : TBADIShortcutUsedEvent;
  Strict Protected
    { Protected declarations }
    Procedure InstallShortcutUsedCallBack(Const ShortCutUsed: TBADIShortcutUsedEvent);
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  Menus,
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

  This is an on change event handler for the Menu Shortcut control.

  @precon  None.
  @postcon Checks the shortcut for being in use and display a message in the information label.

  @param   Sender as a TObject

**)
Procedure TfmBADIMenuShortcuts.hkMenuShortcutChange(Sender: TObject);

ResourceString
  strThisShortcutInUseBy = 'This shortcut is in use by: %s';
  strShortcutNotInUse = 'Shortcut not in use.';

Var
  strActionName : String;

Begin
  If hkMenuShortcut.HotKey > 0 Then
    Begin
      If Assigned(FShortcutUsedEvent) And
         FShortcutUsedEvent(hkMenuShortcut.HotKey, strActionName) Then
        Begin
          lblInformation.Caption := Format(strThisShortcutInUseBy, [strActionName]);
          lblInformation.Font.Color := clRed;
          Exit;
        End;
      lblInformation.Caption := strShortcutNotInUse;
      lblInformation.Font.Color := clGreen;
    End Else
      lblInformation.Caption := '';
End;

(**

  This method assigns an event handler call back in the frame to check that the shortcut is not already 
  in use.

  @precon  ShortcutUsed must be a valid method or Nil.
  @postcon The callbeck event handler is set.

  @param   ShortCutUsed as a TBADIShortcutUsedEvent as a constant

**)
Procedure TfmBADIMenuShortcuts.InstallShortcutUsedCallBack(Const ShortCutUsed: TBADIShortcutUsedEvent);

Begin
  FShortcutUsedEvent := ShortCutUsed;
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
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    If BADIMenus[iBADIMenu].FCaption <> '' Then
      Begin
        Item := lvMenuShortcuts.Items.Add;
        Item.Caption := BADIMenus[iBADIMenu].FName;
        Item.Data := Pointer(iBADIMenu);
        Item.SubItems.Add(BADIMenus[iBADIMenu].FCaption);
        Item.SubItems.Add(TBADIOptions.BADIOptions.MenuShortcut[iBADIMenu]);
      End;
  lvMenuShortcutsSelectItem(Nil, Nil, False);
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
  iBADIMenu : Integer;
  Item: TListItem;
  eBADIMenu: TBADIMenu;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  For iBADIMenu := 0 To lvMenuShortcuts.Items.Count - 1 Do
    Begin
      Item := lvMenuShortcuts.Items[iBADIMenu];
      eBADIMenu := TBADIMenu(Item.Data);
      TBADIOptions.BADIOptions.MenuShortcut[eBADIMenu] := Item.SubItems[1];
    End;
End;

End.

