object fmBADIModuleExplorerFrame: TfmBADIModuleExplorerFrame
  Left = 0
  Top = 0
  Width = 649
  Height = 699
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlModuleExplorerOps: TPanel
    Left = 0
    Top = 0
    Width = 649
    Height = 699
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 112
    ExplicitTop = 24
    DesignSize = (
      649
      699)
    object lblBackgroundColour: TLabel
      Left = 3
      Top = 540
      Width = 107
      Height = 16
      Caption = 'Background &Colour'
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
    object lblIssueLimit: TLabel
      Left = 3
      Top = 656
      Width = 67
      Height = 16
      Caption = '&Issue Limits'
      FocusControl = edtLimits
    end
    object lblIssueLimitTypes: TLabel
      Left = 3
      Top = 626
      Width = 99
      Height = 16
      Caption = '&Issue Limit Types'
      FocusControl = cbxLimits
    end
    object lblTokenLimit: TLabel
      Left = 3
      Top = 596
      Width = 66
      Height = 16
      Caption = '&Token Limit'
    end
    object lblTreeColour: TLabel
      Left = 3
      Top = 568
      Width = 88
      Height = 16
      Caption = 'Treeline &Colour'
    end
    object lblTreeFontName: TLabel
      Left = 3
      Top = 6
      Width = 98
      Height = 16
      Caption = 'Tree Font &Name:'
      FocusControl = cbxTreeFontName
    end
    object lblTreeFontSize: TLabel
      Left = 3
      Top = 36
      Width = 84
      Height = 16
      Caption = 'Tree Font &Size'
      FocusControl = edtTreeFontSize
    end
    object cbxBGColour: TColorBox
      Left = 120
      Top = 537
      Width = 526
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object cbxFixedFontName: TComboBox
      Left = 120
      Top = 63
      Width = 526
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 759
    end
    object cbxLimits: TComboBox
      Left = 120
      Top = 623
      Width = 526
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = cbxLimitsChange
    end
    object cbxTreeFontName: TComboBox
      Left = 120
      Top = 3
      Width = 526
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      ExplicitWidth = 759
    end
    object clbxTreeColour: TColorBox
      Left = 120
      Top = 565
      Width = 526
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
    object edtFixedFontSize: TEdit
      Left = 120
      Top = 93
      Width = 504
      Height = 24
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 5
      Text = '8'
      ExplicitWidth = 737
    end
    object edtLimits: TEdit
      Left = 120
      Top = 653
      Width = 504
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 6
      Text = '0'
    end
    object edtTokenLimit: TEdit
      Left = 120
      Top = 593
      Width = 504
      Height = 24
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
      Text = '10'
    end
    object edtTreeFontSize: TEdit
      Left = 120
      Top = 33
      Width = 504
      Height = 24
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 8
      Text = '8'
      ExplicitWidth = 737
    end
    object udFixedFontSize: TUpDown
      Left = 630
      Top = 93
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtFixedFontSize
      Min = 8
      Max = 72
      Position = 8
      TabOrder = 9
    end
    object udLimits: TUpDown
      Left = 630
      Top = 653
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtLimits
      TabOrder = 10
    end
    object udTokenLimit: TUpDown
      Left = 630
      Top = 593
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtTokenLimit
      Min = 10
      Max = 32600
      Position = 10
      TabOrder = 11
    end
    object udTreeFontSize: TUpDown
      Left = 630
      Top = 33
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtTreeFontSize
      Min = 8
      Max = 72
      Position = 8
      TabOrder = 12
    end
    object chkUseIDEEditorColours: TCheckBox
      Left = 3
      Top = 126
      Width = 643
      Height = 17
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Use IDE Editor Colours'
      TabOrder = 13
    end
    object gbxTokenFontInfo: TGroupBox
      Left = 3
      Top = 149
      Width = 643
      Height = 188
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Token Font Information'
      TabOrder = 14
      DesignSize = (
        643
        188)
      object lblTokenTypes: TLabel
        Left = 11
        Top = 28
        Width = 73
        Height = 16
        Caption = '&Token Types'
        FocusControl = lbxTokenTypes
      end
      object lblForeColour: TLabel
        Left = 11
        Top = 58
        Width = 67
        Height = 16
        Caption = '&Fore Colour'
        FocusControl = cbxFontColour
      end
      object lblBackColour: TLabel
        Left = 11
        Top = 86
        Width = 67
        Height = 16
        Caption = '&Back Colour'
        FocusControl = cbxBackColour
      end
      object lbxTokenTypes: TComboBox
        Left = 117
        Top = 25
        Width = 516
        Height = 24
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnClick = lbxTokenTypesClick
      end
      object cbxFontColour: TColorBox
        Left = 117
        Top = 55
        Width = 516
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = cbxFontColourChange
      end
      object cbxBackColour: TColorBox
        Left = 117
        Top = 83
        Width = 516
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = cbxBackColourChange
      end
      object gbxFontStyles: TGroupBox
        Left = 11
        Top = 111
        Width = 622
        Height = 66
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Font Styles'
        TabOrder = 3
        ExplicitHeight = 98
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
          Top = 26
          Width = 57
          Height = 21
          Caption = '&Italic'
          TabOrder = 1
          OnClick = chkItalicClick
        end
        object chkUnderline: TCheckBox
          Left = 151
          Top = 26
          Width = 82
          Height = 21
          Caption = '&Underline'
          TabOrder = 2
          OnClick = chkUnderlineClick
        end
        object chkStrikeout: TCheckBox
          Left = 252
          Top = 26
          Width = 82
          Height = 21
          Caption = '&Strikeout'
          TabOrder = 3
          OnClick = chkStrikeoutClick
        end
      end
    end
  end
end
