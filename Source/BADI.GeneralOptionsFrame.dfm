object fmBADIGeneralOptions: TfmBADIGeneralOptions
  Left = 0
  Top = 0
  Width = 525
  Height = 291
  Constraints.MinWidth = 525
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object IntervalPanel: TPanel
    Left = 0
    Top = 249
    Width = 525
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 508
    DesignSize = (
      525
      42)
    object lblRefreshInterval: TLabel
      Left = 7
      Top = 10
      Width = 91
      Height = 16
      Caption = 'Refresh &Interval'
    end
    object lblManagedNodesLife: TLabel
      Left = 184
      Top = 10
      Width = 238
      Height = 23
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Life-time of Managed Nodes (in days)'
      ExplicitWidth = 221
    end
    object edtUpdateInterval: TEdit
      Left = 103
      Top = 7
      Width = 70
      Height = 24
      TabOrder = 0
      Text = '100'
    end
    object udUpdateInterval: TUpDown
      Left = 173
      Top = 7
      Width = 16
      Height = 24
      Associate = edtUpdateInterval
      Min = 100
      Max = 30000
      Increment = 100
      Position = 100
      TabOrder = 1
    end
    object edtManagedNodesLife: TEdit
      Left = 428
      Top = 7
      Width = 70
      Height = 24
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = '100'
      ExplicitLeft = 411
    end
    object udManagedNodesLife: TUpDown
      Left = 498
      Top = 7
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtManagedNodesLife
      Max = 365
      Position = 100
      TabOrder = 3
      ExplicitLeft = 481
    end
  end
  object lvOptions: TListView
    Left = 0
    Top = 0
    Width = 525
    Height = 249
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Module Explorer View Options'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
