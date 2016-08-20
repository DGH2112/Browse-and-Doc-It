object fmBADIGeneralOptions: TfmBADIGeneralOptions
  Left = 0
  Top = 0
  Width = 749
  Height = 476
  TabOrder = 0
  object clbOptions: TCheckListBox
    Left = 0
    Top = 0
    Width = 749
    Height = 434
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    ExplicitLeft = -83
    ExplicitWidth = 832
    ExplicitHeight = 431
  end
  object IntervalPanel: TPanel
    Left = 0
    Top = 434
    Width = 749
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -83
    ExplicitTop = 431
    ExplicitWidth = 832
    DesignSize = (
      749
      42)
    object lblRefreshInterval: TLabel
      Left = 0
      Top = 11
      Width = 180
      Height = 13
      Caption = 'Refresh &Interval after Editor changes'
    end
    object lblManagedNodesLife: TLabel
      Left = 500
      Top = 11
      Width = 179
      Height = 13
      Caption = 'Life-time of Managed Nodes (in days)'
    end
    object edtUpdateInterval: TEdit
      Left = 239
      Top = 7
      Width = 70
      Height = 21
      TabOrder = 0
      Text = '100'
    end
    object udUpdateInterval: TUpDown
      Left = 309
      Top = 7
      Width = 16
      Height = 21
      Associate = edtUpdateInterval
      Min = 100
      Max = 30000
      Increment = 100
      Position = 100
      TabOrder = 1
    end
    object edtManagedNodesLife: TEdit
      Left = 652
      Top = 7
      Width = 70
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = '100'
      ExplicitLeft = 735
    end
    object udManagedNodesLife: TUpDown
      Left = 722
      Top = 7
      Width = 16
      Height = 21
      Anchors = [akTop, akRight]
      Associate = edtManagedNodesLife
      Max = 365
      Position = 100
      TabOrder = 3
      ExplicitLeft = 805
    end
  end
end
