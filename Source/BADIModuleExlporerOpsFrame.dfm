object fmBADIModuleExplorerFrame: TfmBADIModuleExplorerFrame
  Left = 0
  Top = 0
  Width = 359
  Height = 411
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    359
    411)
  object lblBackColour: TLabel
    Left = 179
    Top = 124
    Width = 56
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '&Back Colour'
    FocusControl = cbxBackColour
    ExplicitTop = 158
  end
  object lblForeColour: TLabel
    Left = 179
    Top = 77
    Width = 56
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '&Fore Colour'
    FocusControl = cbxFontColour
    ExplicitTop = 111
  end
  object lblTreeColour: TLabel
    Left = 179
    Top = 367
    Width = 72
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Treeline &Colour'
    ExplicitTop = 401
  end
  object lblTokenLimit: TLabel
    Left = 240
    Top = 33
    Width = 53
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '&Token Limit'
    ExplicitLeft = 377
  end
  object lblBackgroundColour: TLabel
    Left = 180
    Top = 320
    Width = 90
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Background &Colour'
    ExplicitTop = 354
  end
  object lblTokenTypes: TLabel
    Left = 3
    Top = 60
    Width = 61
    Height = 13
    Caption = '&Token Types'
    FocusControl = lbxTokenTypes
  end
  object lblFontSize: TLabel
    Left = 3
    Top = 33
    Width = 44
    Height = 13
    Caption = 'Font &Size'
    FocusControl = edtFontSize
  end
  object lblFontName: TLabel
    Left = 3
    Top = 6
    Width = 56
    Height = 13
    Caption = 'Font &Name:'
    FocusControl = cbxFontName
  end
  object cbxFontName: TComboBox
    Left = 72
    Top = 3
    Width = 284
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cbxBackColour: TColorBox
    Left = 179
    Top = 143
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 1
    ExplicitTop = 177
  end
  object clbxTreeColour: TColorBox
    Left = 179
    Top = 386
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 2
    ExplicitTop = 420
  end
  object udTokenLimit: TUpDown
    Left = 334
    Top = 30
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtTokenLimit
    Min = 10
    Max = 32600
    Position = 10
    TabOrder = 3
  end
  object edtTokenLimit: TEdit
    Left = 299
    Top = 30
    Width = 35
    Height = 21
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '10'
  end
  object cbxBGColour: TColorBox
    Left = 179
    Top = 339
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 5
    OnChange = cbxBackColourChange
    ExplicitTop = 373
  end
  object gbxFontStyles: TGroupBox
    Left = 179
    Top = 171
    Width = 177
    Height = 143
    Anchors = [akRight, akBottom]
    Caption = 'Font Styles'
    TabOrder = 6
    ExplicitTop = 205
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
    Left = 179
    Top = 96
    Width = 177
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 7
    OnChange = cbxFontColourChange
    ExplicitTop = 130
  end
  object lbxTokenTypes: TListBox
    Left = 3
    Top = 79
    Width = 170
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 8
    OnClick = lbxTokenTypesClick
    ExplicitHeight = 363
  end
  object udFontSize: TUpDown
    Left = 112
    Top = 30
    Width = 16
    Height = 21
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
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 10
    Text = '8'
  end
end
