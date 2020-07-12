object fmBADIModuleExplorerFrame: TfmBADIModuleExplorerFrame
  Left = 0
  Top = 0
  Width = 649
  Height = 501
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
    Height = 501
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      649
      501)
    object lblBackgroundColour: TLabel
      Left = 3
      Top = 351
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
      Top = 467
      Width = 67
      Height = 16
      Caption = '&Issue Limits'
      FocusControl = edtLimits
    end
    object lblIssueLimitTypes: TLabel
      Left = 3
      Top = 437
      Width = 99
      Height = 16
      Caption = '&Issue Limit Types'
      FocusControl = cbxLimits
    end
    object lblTokenLimit: TLabel
      Left = 3
      Top = 407
      Width = 66
      Height = 16
      Caption = '&Token Limit'
    end
    object lblTreeColour: TLabel
      Left = 3
      Top = 379
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
      Top = 348
      Width = 526
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 8
    end
    object cbxFixedFontName: TComboBox
      Left = 120
      Top = 63
      Width = 526
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object cbxLimits: TComboBox
      Left = 120
      Top = 434
      Width = 526
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 12
      OnChange = cbxLimitsChange
    end
    object cbxTreeFontName: TComboBox
      Left = 120
      Top = 3
      Width = 526
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object clbxTreeColour: TColorBox
      Left = 120
      Top = 376
      Width = 526
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 9
    end
    object edtFixedFontSize: TEdit
      Left = 120
      Top = 93
      Width = 508
      Height = 24
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 4
      Text = '8'
    end
    object edtLimits: TEdit
      Left = 120
      Top = 464
      Width = 508
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 13
      Text = '0'
    end
    object edtTokenLimit: TEdit
      Left = 120
      Top = 404
      Width = 508
      Height = 24
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 10
      Text = '10'
    end
    object edtTreeFontSize: TEdit
      Left = 120
      Top = 33
      Width = 508
      Height = 24
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 1
      Text = '8'
    end
    object udFixedFontSize: TUpDown
      Left = 628
      Top = 93
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtFixedFontSize
      Min = 8
      Max = 72
      Position = 8
      TabOrder = 5
    end
    object udLimits: TUpDown
      Left = 628
      Top = 464
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtLimits
      TabOrder = 14
    end
    object udTokenLimit: TUpDown
      Left = 628
      Top = 404
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
      Left = 628
      Top = 33
      Width = 16
      Height = 24
      Anchors = [akTop, akRight]
      Associate = edtTreeFontSize
      Min = 8
      Max = 72
      Position = 8
      TabOrder = 2
    end
    object chkUseIDEEditorColours: TCheckBox
      Left = 3
      Top = 126
      Width = 643
      Height = 17
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Use IDE Editor Colours'
      TabOrder = 6
    end
    object gbxTokenFontInfo: TGroupBox
      Left = 3
      Top = 149
      Width = 643
      Height = 188
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Token Font Information'
      TabOrder = 7
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
        DesignSize = (
          622
          66)
        object GridPanel: TGridPanel
          Left = 3
          Top = 16
          Width = 616
          Height = 41
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          ColumnCollection = <
            item
              Value = 24.685464234117820000
            end
            item
              Value = 24.685464729784820000
            end
            item
              Value = 24.685464659132070000
            end
            item
              Value = 25.974025974025970000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = chkBold
              Row = 0
            end
            item
              Column = 1
              Control = chkItalic
              Row = 0
            end
            item
              Column = 2
              Control = chkStrikeout
              Row = 0
            end
            item
              Column = 3
              Control = chkUnderline
              Row = 0
            end>
          RowCollection = <
            item
              Value = 100.000000000000000000
            end>
          TabOrder = 0
          object chkBold: TCheckBox
            AlignWithMargins = True
            Left = 8
            Top = 3
            Width = 136
            Height = 35
            Margins.Left = 8
            Margins.Right = 8
            Align = alClient
            Caption = '&Bold'
            TabOrder = 0
            OnClick = chkBoldClick
            ExplicitWidth = 137
          end
          object chkItalic: TCheckBox
            AlignWithMargins = True
            Left = 160
            Top = 3
            Width = 136
            Height = 35
            Margins.Left = 8
            Margins.Right = 8
            Align = alClient
            Caption = '&Italic'
            TabOrder = 1
            OnClick = chkItalicClick
            ExplicitLeft = 161
            ExplicitWidth = 137
          end
          object chkStrikeout: TCheckBox
            AlignWithMargins = True
            Left = 312
            Top = 3
            Width = 136
            Height = 35
            Margins.Left = 8
            Margins.Right = 8
            Align = alClient
            Caption = '&Strikeout'
            TabOrder = 2
            OnClick = chkStrikeoutClick
            ExplicitLeft = 314
            ExplicitWidth = 137
          end
          object chkUnderline: TCheckBox
            AlignWithMargins = True
            Left = 464
            Top = 3
            Width = 144
            Height = 35
            Margins.Left = 8
            Margins.Right = 8
            Align = alClient
            Caption = '&Underline'
            TabOrder = 3
            OnClick = chkUnderlineClick
            ExplicitLeft = 467
            ExplicitWidth = 141
          end
        end
      end
    end
  end
end
