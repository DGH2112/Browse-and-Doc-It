object fmBADIMenuShortcuts: TfmBADIMenuShortcuts
  Left = 0
  Top = 0
  Width = 484
  Height = 318
  TabOrder = 0
  DesignSize = (
    484
    318)
  object lvMenuShortcuts: TListView
    Left = 3
    Top = 3
    Width = 478
    Height = 284
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Menu Text'
        Width = 150
      end
      item
        AutoSize = True
        Caption = 'Shortcut'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = lvMenuShortcutsSelectItem
  end
  object hkMenuShortcut: THotKey
    Left = 3
    Top = 293
    Width = 397
    Height = 19
    Anchors = [akLeft, akRight, akBottom]
    HotKey = 0
    Modifiers = []
    TabOrder = 1
  end
  object btnAssign: TBitBtn
    Left = 406
    Top = 290
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Assign'
    TabOrder = 2
    OnClick = btnAssignClick
  end
end
