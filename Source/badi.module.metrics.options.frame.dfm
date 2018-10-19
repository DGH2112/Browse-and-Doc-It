object frameBADIModuleMetricsOptions: TframeBADIModuleMetricsOptions
  Left = 0
  Top = 0
  Width = 415
  Height = 391
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 259
    Width = 415
    Height = 132
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      415
      132)
    object lblToxicityPower: TLabel
      Left = 8
      Top = 11
      Width = 83
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Toxicity &Power'
      FocusControl = edtToxicityPower
    end
    object lblToxicitySummation: TLabel
      Left = 8
      Top = 41
      Width = 112
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Toxicity &Summation'
    end
    object lblMetricLowerLimit: TLabel
      Left = 8
      Top = 71
      Width = 105
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Metric &Lower Limit'
      FocusControl = edtMetricLowerLimit
    end
    object lblMetricUpperLimit: TLabel
      Left = 8
      Top = 101
      Width = 104
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Metric &Upper Limit'
      FocusControl = edtMetricUpperLimit
    end
    object edtToxicityPower: TEdit
      Left = 348
      Top = 8
      Width = 41
      Height = 24
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      ReadOnly = True
      TabOrder = 0
      Text = '1'
    end
    object udToxicityPower: TUpDown
      Left = 389
      Top = 8
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtToxicityPower
      Min = 1
      Max = 10
      Position = 1
      TabOrder = 1
    end
    object cbxToxicitySummation: TComboBox
      Left = 158
      Top = 38
      Width = 249
      Height = 24
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 2
    end
    object udMetricLowerLimit: TUpDown
      Left = 389
      Top = 68
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtMetricLowerLimit
      Min = 1
      Max = 199
      Position = 1
      TabOrder = 4
    end
    object edtMetricLowerLimit: TEdit
      Left = 348
      Top = 68
      Width = 41
      Height = 24
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      ReadOnly = True
      TabOrder = 3
      Text = '1'
    end
    object udMetricUpperLimit: TUpDown
      Left = 389
      Top = 98
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtMetricUpperLimit
      Min = 1
      Max = 199
      Position = 1
      TabOrder = 6
    end
    object edtMetricUpperLimit: TEdit
      Left = 348
      Top = 98
      Width = 41
      Height = 24
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      ReadOnly = True
      TabOrder = 5
      Text = '1'
    end
  end
end
