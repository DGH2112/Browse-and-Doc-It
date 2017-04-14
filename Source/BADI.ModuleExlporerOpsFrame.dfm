object fmBADIModuleExplorerFrame: TfmBADIModuleExplorerFrame
  Left = 0
  Top = 0
  Width = 467
  Height = 428
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    467
    428)
  object lblBackColour: TLabel
    Left = 3
    Top = 184
    Width = 67
    Height = 16
    Caption = '&Back Colour'
    FocusControl = cbxBackColour
  end
  object lblForeColour: TLabel
    Left = 3
    Top = 156
    Width = 67
    Height = 16
    Caption = '&Fore Colour'
    FocusControl = cbxFontColour
  end
  object lblTreeColour: TLabel
    Left = 3
    Top = 310
    Width = 88
    Height = 16
    Caption = 'Treeline &Colour'
  end
  object lblTokenLimit: TLabel
    Left = 3
    Top = 338
    Width = 66
    Height = 16
    Caption = '&Token Limit'
  end
  object lblBackgroundColour: TLabel
    Left = 3
    Top = 282
    Width = 107
    Height = 16
    Caption = 'Background &Colour'
  end
  object lblTokenTypes: TLabel
    Left = 3
    Top = 126
    Width = 73
    Height = 16
    Caption = '&Token Types'
    FocusControl = lbxTokenTypes
  end
  object lblTreeFontSize: TLabel
    Left = 3
    Top = 36
    Width = 84
    Height = 16
    Caption = 'Tree Font &Size'
    FocusControl = edtTreeFontSize
  end
  object lblTreeFontName: TLabel
    Left = 3
    Top = 6
    Width = 98
    Height = 16
    Caption = 'Tree Font &Name:'
    FocusControl = cbxTreeFontName
  end
  object lblFixedFont: TLabel
    Left = 3
    Top = 66
    Width = 101
    Height = 16
    Caption = '&Fixed Font Name:'
    FocusControl = cbxFixedFontName
  end
  object lblFixedFontSize: TLabel
    Left = 3
    Top = 96
    Width = 87
    Height = 16
    Caption = 'Fixed Font Si&ze'
    FocusControl = edtFixedFontSize
  end
  object lblIssueLimitTypes: TLabel
    Left = 3
    Top = 368
    Width = 99
    Height = 16
    Caption = '&Issue Limit Types'
    FocusControl = cbxLimits
  end
  object lblIssueLimit: TLabel
    Left = 3
    Top = 398
    Width = 67
    Height = 16
    Caption = '&Issue Limits'
    FocusControl = edtLimits
  end
  object cbxTreeFontName: TComboBox
    Left = 120
    Top = 3
    Width = 344
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitWidth = 357
  end
  object cbxBackColour: TColorBox
    Left = 120
    Top = 181
    Width = 344
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = cbxBackColourChange
    ExplicitWidth = 438
  end
  object clbxTreeColour: TColorBox
    Left = 120
    Top = 307
    Width = 344
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 11
    ExplicitWidth = 438
  end
  object udTokenLimit: TUpDown
    Left = 448
    Top = 335
    Width = 16
    Height = 24
    Anchors = [akTop, akRight]
    Associate = edtTokenLimit
    Min = 10
    Max = 32600
    Position = 10
    TabOrder = 13
    ExplicitLeft = 542
  end
  object edtTokenLimit: TEdit
    Left = 120
    Top = 335
    Width = 322
    Height = 24
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 12
    Text = '10'
    ExplicitWidth = 416
  end
  object cbxBGColour: TColorBox
    Left = 120
    Top = 279
    Width = 344
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
    ExplicitWidth = 438
  end
  object gbxFontStyles: TGroupBox
    Left = 3
    Top = 209
    Width = 461
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Font Styles'
    TabOrder = 9
    ExplicitWidth = 555
    object chkBold: TCheckBox
      Left = 15
      Top = 26
      Width = 58
      Height = 21
      Caption = '&Bold'
      TabOrder = 0
      OnClick = chkBoldClick
    end
    object chkItalic: TCheckBox
      Left = 79
      Top = 28
      Width = 57
      Height = 21
      Caption = '&Italic'
      TabOrder = 1
      OnClick = chkItalicClick
    end
    object chkUnderline: TCheckBox
      Left = 151
      Top = 27
      Width = 82
      Height = 21
      Caption = '&Underline'
      TabOrder = 2
      OnClick = chkUnderlineClick
    end
    object chkStrikeout: TCheckBox
      Left = 252
      Top = 30
      Width = 82
      Height = 21
      Caption = '&Strikeout'
      TabOrder = 3
      OnClick = chkStrikeoutClick
    end
  end
  object cbxFontColour: TColorBox
    Left = 120
    Top = 153
    Width = 344
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = cbxFontColourChange
    ExplicitWidth = 438
  end
  object udTreeFontSize: TUpDown
    Left = 448
    Top = 33
    Width = 16
    Height = 24
    Anchors = [akTop, akRight]
    Associate = edtTreeFontSize
    Min = 8
    Max = 72
    Position = 8
    TabOrder = 2
    ExplicitLeft = 461
  end
  object edtTreeFontSize: TEdit
    Left = 120
    Top = 33
    Width = 322
    Height = 24
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
    Text = '8'
    ExplicitWidth = 416
  end
  object cbxLimits: TComboBox
    Left = 120
    Top = 365
    Width = 344
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 14
    OnChange = cbxLimitsChange
  end
  object edtLimits: TEdit
    Left = 120
    Top = 395
    Width = 322
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 15
    Text = '0'
  end
  object udLimits: TUpDown
    Left = 448
    Top = 395
    Width = 16
    Height = 24
    Anchors = [akTop, akRight]
    Associate = edtLimits
    TabOrder = 16
  end
  object edtFixedFontSize: TEdit
    Left = 120
    Top = 93
    Width = 322
    Height = 24
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 4
    Text = '8'
    ExplicitWidth = 416
  end
  object udFixedFontSize: TUpDown
    Left = 448
    Top = 93
    Width = 16
    Height = 24
    Anchors = [akTop, akRight]
    Associate = edtFixedFontSize
    Min = 8
    Max = 72
    Position = 8
    TabOrder = 5
    ExplicitLeft = 542
  end
  object cbxFixedFontName: TComboBox
    Left = 120
    Top = 63
    Width = 344
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ExplicitWidth = 438
  end
  object lbxTokenTypes: TComboBox
    Left = 120
    Top = 123
    Width = 344
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    Text = 'lbxTokenTypes'
    OnClick = lbxTokenTypesClick
    ExplicitWidth = 438
  end
end
