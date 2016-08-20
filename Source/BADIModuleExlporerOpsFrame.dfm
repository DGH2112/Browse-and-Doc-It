object fmBADIModuleExplorerFrame: TfmBADIModuleExplorerFrame
  Left = 0
  Top = 0
  Width = 683
  Height = 423
  TabOrder = 0
  DesignSize = (
    683
    423)
  object lblBackColour: TLabel
    Left = 473
    Top = 111
    Width = 56
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '&Back Colour'
    FocusControl = cbxBackColour
    ExplicitLeft = 868
    ExplicitTop = 373
  end
  object lblForeColour: TLabel
    Left = 473
    Top = 52
    Width = 56
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = '&Fore Colour'
    FocusControl = cbxFontColour
    ExplicitLeft = 868
    ExplicitTop = 314
  end
  object lblTreeColour: TLabel
    Left = 473
    Top = 374
    Width = 72
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Treeline &Colour'
    ExplicitLeft = 868
    ExplicitTop = 636
  end
  object lblTokenLimit: TLabel
    Left = 564
    Top = 33
    Width = 53
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '&Token Limit'
  end
  object lblBackgroundColour: TLabel
    Left = 473
    Top = 316
    Width = 90
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Background &Colour'
    ExplicitLeft = 868
    ExplicitTop = 578
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
    Width = 608
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object cbxBackColour: TColorBox
    Left = 473
    Top = 130
    Width = 207
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 1
    ExplicitLeft = 868
    ExplicitTop = 392
  end
  object clbxTreeColour: TColorBox
    Left = 473
    Top = 398
    Width = 207
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 2
    ExplicitLeft = 868
    ExplicitTop = 660
  end
  object udTokenLimit: TUpDown
    Left = 664
    Top = 30
    Width = 16
    Height = 24
    Anchors = [akTop, akRight]
    Associate = edtTokenLimit
    Min = 10
    Max = 32600
    Position = 10
    TabOrder = 3
    ExplicitLeft = 1185
  end
  object edtTokenLimit: TEdit
    Left = 623
    Top = 30
    Width = 35
    Height = 21
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '10'
  end
  object cbxBGColour: TColorBox
    Left = 473
    Top = 340
    Width = 207
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 5
    OnChange = cbxBackColourChange
    ExplicitLeft = 868
    ExplicitTop = 602
  end
  object gbxFontStyles: TGroupBox
    Left = 473
    Top = 165
    Width = 207
    Height = 143
    Anchors = [akRight, akBottom]
    Caption = 'Font Styles'
    TabOrder = 6
    ExplicitLeft = 868
    ExplicitTop = 427
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
    Left = 473
    Top = 71
    Width = 207
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
    Anchors = [akRight, akBottom]
    TabOrder = 7
    OnChange = cbxFontColourChange
    ExplicitLeft = 868
    ExplicitTop = 333
  end
  object lbxTokenTypes: TListBox
    Left = 3
    Top = 79
    Width = 464
    Height = 341
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 8
    OnClick = lbxTokenTypesClick
    ExplicitWidth = 510
    ExplicitHeight = 365
  end
  object udFontSize: TUpDown
    Left = 118
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
    Height = 21
    Alignment = taRightJustify
    ReadOnly = True
    TabOrder = 10
    Text = '8'
  end
end
