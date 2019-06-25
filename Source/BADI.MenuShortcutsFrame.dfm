object fmBADIMenuShortcuts: TfmBADIMenuShortcuts
  Left = 0
  Top = 0
  Width = 491
  Height = 303
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    491
    303)
  object lblInformation: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 249
    Width = 485
    Height = 23
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitTop = 264
    ExplicitWidth = 478
  end
  object lvMenuShortcuts: TListView
    Left = 3
    Top = 3
    Width = 485
    Height = 240
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
    Top = 275
    Width = 404
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    HotKey = 0
    Modifiers = []
    TabOrder = 1
    OnChange = hkMenuShortcutChange
  end
  object btnAssign: TButton
    Left = 413
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Assign'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = btnAssignClick
  end
end
