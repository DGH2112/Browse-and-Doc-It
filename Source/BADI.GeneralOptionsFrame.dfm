object fmBADIGeneralOptions: TfmBADIGeneralOptions
  Left = 0
  Top = 0
  Width = 508
  Height = 291
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object clbOptions: TCheckListBox
    Left = 0
    Top = 0
    Width = 508
    Height = 249
    Align = alClient
    TabOrder = 0
  end
  object IntervalPanel: TPanel
    Left = 0
    Top = 249
    Width = 508
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      508
      42)
    object lblRefreshInterval: TLabel
      Left = 7
      Top = 10
      Width = 91
      Height = 23
      Caption = 'Refresh &Interval'
    end
    object lblManagedNodesLife: TLabel
      Left = 184
      Top = 10
      Width = 221
      Height = 23
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Life-time of Managed Nodes (in days)'
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
      Left = 411
      Top = 7
      Width = 70
      Height = 24
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = '100'
    end
    object udManagedNodesLife: TUpDown
      Left = 481
      Top = 7
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtManagedNodesLife
      Max = 365
      Position = 100
      TabOrder = 3
    end
  end
end
