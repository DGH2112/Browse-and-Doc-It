object fmBADIModuleExplorerFrame: TfmBADIModuleExplorerFrame
  Left = 0
  Top = 0
  Width = 441
  Height = 411
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    441
    411)
  object lblBackColour: TLabel
    Left = 261
    Top = 124
    Width = 67
    Height = 16
    Anchors = [akRight, akBottom]
    Caption = '&Back Colour'
    FocusControl = cbxBackColour
  end
  object lblForeColour: TLabel
    Left = 261
    Top = 77
    Width = 67
    Height = 16
    Anchors = [akRight, akBottom]
    Caption = '&Fore Colour'
    FocusControl = cbxFontColour
  end
  object lblTreeColour: TLabel
    Left = 261
    Top = 367
    Width = 88
    Height = 16
    Anchors = [akRight, akBottom]
    Caption = 'Treeline &Colour'
  end
  object lblTokenLimit: TLabel
    Left = 134
    Top = 33
    Width = 66
    Height = 16
    Caption = '&Token Limit'
  end
  object lblBackgroundColour: TLabel
    Left = 262
    Top = 320
    Width = 107
    Height = 16
    Anchors = [akRight, akBottom]
    Caption = 'Background &Colour'
  end
  object lblTokenTypes: TLabel
    Left = 3
    Top = 60
    Width = 73
    Height = 16
    Caption = '&Token Types'
    FocusControl = lbxTokenTypes
  end
  object lblFontSize: TLabel
    Left = 3
    Top = 33
    Width = 53
    Height = 16
    Caption = 'Font &Size'
    FocusControl = edtFontSize
  end
  object lblFontName: TLabel
    Left = 3
    Top = 6
    Width = 67
    Height = 16
    Caption = 'Font &Name:'
    FocusControl = cbxFontName
  end
  object cbxFontName: TComboBox
    Left = 72
    Top = 3
    Width = 366
    Height = 24
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cbxBackColour: TColorBox
    Left = 261
    Top = 143
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnChange = cbxBackColourChange
  end
  object clbxTreeColour: TColorBox
    Left = 261
    Top = 386
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object udTokenLimit: TUpDown
    Left = 228
    Top = 30
    Width = 16
    Height = 24
    Associate = edtTokenLimit
    Min = 10
    Max = 32600
    Position = 10
    TabOrder = 3
  end
  object edtTokenLimit: TEdit
    Left = 193
    Top = 30
    Width = 35
    Height = 24
    Alignment = taRightJustify
    TabOrder = 4
    Text = '10'
  end
  object cbxBGColour: TColorBox
    Left = 261
    Top = 339
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 5
  end
  object gbxFontStyles: TGroupBox
    Left = 261
    Top = 171
    Width = 177
    Height = 143
    Anchors = [akRight, akBottom]
    Caption = 'Font Styles'
    TabOrder = 6
    object chkBold: TCheckBox
      Left = 15
      Top = 26
      Width = 119
      Height = 21
      Caption = '&Bold'
      TabOrder = 0
      OnClick = chkBoldClick
    end
    object chkItalic: TCheckBox
      Left = 16
      Top = 54
      Width = 119
      Height = 21
      Caption = '&Italic'
      TabOrder = 1
      OnClick = chkItalicClick
    end
    object chkUnderline: TCheckBox
      Left = 16
      Top = 82
      Width = 119
      Height = 21
      Caption = '&Underline'
      TabOrder = 2
      OnClick = chkUnderlineClick
    end
    object chkStrikeout: TCheckBox
      Left = 15
      Top = 111
      Width = 119
      Height = 21
      Caption = '&Strikeout'
      TabOrder = 3
      OnClick = chkStrikeoutClick
    end
  end
  object cbxFontColour: TColorBox
    Left = 261
    Top = 96
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 7
    OnChange = cbxFontColourChange
  end
  object lbxTokenTypes: TListBox
    Left = 3
    Top = 79
    Width = 252
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 8
    OnClick = lbxTokenTypesClick
  end
  object udFontSize: TUpDown
    Left = 112
    Top = 30
    Width = 16
    Height = 24
    Associate = edtFontSize
    Min = 8
    Max = 72
    Position = 8
    TabOrder = 9
  end
  object edtFontSize: TEdit
    Left = 72
    Top = 30
    Width = 40
    Height = 24
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 10
    Text = '8'
  end
  object cbxLimits: TComboBox
    Left = 251
    Top = 30
    Width = 123
    Height = 24
    Style = csDropDownList
    Anchors = [akTop, akRight]
    TabOrder = 11
    OnChange = cbxLimitsChange
  end
  object edtLimits: TEdit
    Left = 380
    Top = 30
    Width = 41
    Height = 24
    Anchors = [akTop, akRight]
    ReadOnly = True
    TabOrder = 12
    Text = '0'
  end
  object udLimits: TUpDown
    Left = 421
    Top = 30
    Width = 16
    Height = 24
    Anchors = [akTop, akRight]
    Associate = edtLimits
    TabOrder = 13
  end
end
