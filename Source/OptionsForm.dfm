object frmOptions: TfrmOptions
  Left = 445
  Top = 269
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 562
  ClientWidth = 860
  Color = clBtnFace
  Constraints.MinHeight = 431
  Constraints.MinWidth = 677
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    860
    562)
  PixelsPerInch = 96
  TextHeight = 16
  object OptionTab: TPageControl
    Left = 10
    Top = 10
    Width = 840
    Height = 504
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = tabGeneralOptions
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabGeneralOptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'General Options'
      object clbOptions: TCheckListBox
        Left = 0
        Top = 0
        Width = 832
        Height = 431
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        TabOrder = 0
      end
      object IntervalPanel: TPanel
        Left = 0
        Top = 431
        Width = 832
        Height = 42
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          832
          42)
        object lblRefreshInterval: TLabel
          Left = 0
          Top = 11
          Width = 215
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Refresh &Interval after Editor changes'
        end
        object lblManagedNodesLife: TLabel
          Left = 500
          Top = 11
          Width = 223
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Life-time of Managed Nodes (in days)'
        end
        object edtUpdateInterval: TEdit
          Left = 239
          Top = 7
          Width = 70
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          TabOrder = 0
          Text = '100'
        end
        object udUpdateInterval: TUpDown
          Left = 309
          Top = 7
          Width = 20
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Associate = edtUpdateInterval
          Min = 100
          Max = 30000
          Increment = 100
          Position = 100
          TabOrder = 1
        end
        object edtManagedNodesLife: TEdit
          Left = 735
          Top = 7
          Width = 70
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akTop, akRight]
          TabOrder = 2
          Text = '100'
        end
        object udManagedNodesLife: TUpDown
          Left = 805
          Top = 7
          Width = 19
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Anchors = [akTop, akRight]
          Associate = edtManagedNodesLife
          Max = 365
          Position = 100
          TabOrder = 3
        end
      end
    end
    object tabSpecialTags: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Special Tags'
      ImageIndex = 1
      DesignSize = (
        832
        473)
      object lbSpecialTags: TListBox
        Left = 4
        Top = 32
        Width = 823
        Height = 396
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        OnDblClick = btnEditClick
        OnDrawItem = lbSpecialTagsDrawItem
        OnMouseDown = lbSpecialTagsMouseDown
      end
      object HeaderControl1: THeaderControl
        Left = 4
        Top = 4
        Width = 823
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alNone
        Anchors = [akLeft, akTop, akRight]
        FullDrag = False
        Enabled = False
        Sections = <
          item
            ImageIndex = -1
            Text = 'Show in Tree'
            Width = 80
          end
          item
            ImageIndex = -1
            Text = 'Auto Expand'
            Width = 80
          end
          item
            ImageIndex = -1
            Text = 'Show In Doc'
            Width = 80
          end
          item
            ImageIndex = -1
            Text = 'Tag Name'
            Width = 100
          end
          item
            ImageIndex = -1
            Text = 'Tag Description'
            Width = 300
          end>
        Style = hsFlat
      end
      object btnAdd: TBitBtn
        Left = 336
        Top = 436
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddClick
      end
      object btnDelete: TBitBtn
        Left = 535
        Top = 436
        Width = 93
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
      end
      object btnEdit: TBitBtn
        Left = 436
        Top = 436
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Edit'
        TabOrder = 4
        OnClick = btnEditClick
      end
      object btnMoveDown: TBitBtn
        Left = 735
        Top = 436
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = '&Move Down'
        TabOrder = 5
        OnClick = btnMoveDownClick
      end
      object btnMoveUp: TBitBtn
        Left = 635
        Top = 436
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akRight, akBottom]
        Caption = 'Move &Up'
        TabOrder = 6
        OnClick = btnMoveUpClick
      end
    end
    object tabModuleExplorer: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Module Explorer'
      ImageIndex = 3
      DesignSize = (
        832
        473)
      object lblFontName: TLabel
        Left = 4
        Top = 7
        Width = 69
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Font &Name:'
        FocusControl = cbxFontName
      end
      object lblFontSize: TLabel
        Left = 4
        Top = 41
        Width = 55
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Font &Size'
        FocusControl = edtFontSize
      end
      object lblTokenTypes: TLabel
        Left = 4
        Top = 81
        Width = 81
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&Token Types'
        FocusControl = lbxTokenTypes
      end
      object lblBackgroundColour: TLabel
        Left = 620
        Top = 348
        Width = 115
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = 'Background &Colour'
      end
      object lblTokenLimit: TLabel
        Left = 545
        Top = 41
        Width = 69
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&Token Limit'
      end
      object lblTreeColour: TLabel
        Left = 620
        Top = 406
        Width = 92
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = 'Treeline &Colour'
      end
      object lblForeColour: TLabel
        Left = 620
        Top = 81
        Width = 70
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&Fore Colour'
        FocusControl = cbxFontColour
      end
      object lblBackColour: TLabel
        Left = 620
        Top = 138
        Width = 73
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&Back Colour'
        FocusControl = cbxBackColour
      end
      object cbxFontName: TComboBox
        Left = 97
        Top = 4
        Width = 731
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object edtFontSize: TEdit
        Left = 97
        Top = 37
        Width = 227
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ReadOnly = True
        TabOrder = 1
        Text = '8'
      end
      object udFontSize: TUpDown
        Left = 324
        Top = 37
        Width = 19
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Associate = edtFontSize
        Min = 8
        Max = 72
        Position = 8
        TabOrder = 2
      end
      object lbxTokenTypes: TListBox
        Left = 4
        Top = 105
        Width = 609
        Height = 363
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 5
        OnClick = lbxTokenTypesClick
      end
      object cbxFontColour: TColorBox
        Left = 620
        Top = 103
        Width = 207
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnChange = cbxFontColourChange
      end
      object gbxFontStyles: TGroupBox
        Left = 620
        Top = 197
        Width = 207
        Height = 143
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = 'Font Styles'
        TabOrder = 8
        object chkBold: TCheckBox
          Left = 15
          Top = 26
          Width = 119
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Bold'
          TabOrder = 0
          OnClick = chkBoldClick
        end
        object chkItalic: TCheckBox
          Left = 16
          Top = 54
          Width = 119
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Italic'
          TabOrder = 1
          OnClick = chkItalicClick
        end
        object chkUnderline: TCheckBox
          Left = 16
          Top = 82
          Width = 119
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Underline'
          TabOrder = 2
          OnClick = chkUnderlineClick
        end
        object chkStrikeout: TCheckBox
          Left = 15
          Top = 111
          Width = 119
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = '&Strikeout'
          TabOrder = 3
          OnClick = chkStrikeoutClick
        end
      end
      object cbxBGColour: TColorBox
        Left = 620
        Top = 372
        Width = 207
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akTop, akRight]
        TabOrder = 9
      end
      object edtTokenLimit: TEdit
        Left = 620
        Top = 37
        Width = 185
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 3
        Text = '10'
      end
      object udTokenLimit: TUpDown
        Left = 805
        Top = 37
        Width = 20
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Associate = edtTokenLimit
        Min = 10
        Max = 32600
        Position = 10
        TabOrder = 4
      end
      object clbxTreeColour: TColorBox
        Left = 620
        Top = 430
        Width = 207
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akTop, akRight]
        TabOrder = 10
      end
      object cbxBackColour: TColorBox
        Left = 620
        Top = 162
        Width = 207
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        Anchors = [akTop, akRight]
        TabOrder = 7
        OnChange = cbxBackColourChange
      end
    end
    object tabCodeBrowsing: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Code Browsing'
      ImageIndex = 4
      DesignSize = (
        832
        473)
      object rgpBrowsePosition: TRadioGroup
        Left = 4
        Top = 4
        Width = 823
        Height = 462
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Browse Position'
        Items.Strings = (
          'Comment top aligned with top of the editor'
          'Comment top aligned with the centre of the editor'
          'Identifier aligned with top of the editor'
          'Identifier aligned with the centre of the editor'
          'Identifier centred in the editor but show all of the comment')
        TabOrder = 0
      end
    end
    object tabExcludeDocFiles: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Exclude Doc Files'
      ImageIndex = 5
      DesignSize = (
        832
        473)
      object mmoExcludeDocFiles: TMemo
        Left = 4
        Top = 4
        Width = 823
        Height = 462
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'mmoExcludeDocFiles')
        TabOrder = 0
      end
    end
    object tabMethodDescriptions: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Method Descriptions'
      ImageIndex = 6
      DesignSize = (
        832
        473)
      object btnAddDesc: TBitBtn
        Left = 735
        Top = 4
        Width = 92
        Height = 30
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = '&Add'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333FF33333333FF333993333333300033377F3333333777333993333333
          300033F77FFF3333377739999993333333333777777F3333333F399999933333
          33003777777333333377333993333333330033377F3333333377333993333333
          3333333773333333333F333333333333330033333333F33333773333333C3333
          330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
          333333333337733333FF3333333C333330003333333733333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        TabOrder = 0
        OnClick = btnAddDescClick
      end
      object btnEditDesc: TBitBtn
        Left = 735
        Top = 42
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = '&Edit'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
          000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
          00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
          F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
          0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
          FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
          FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
          0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
          00333377737FFFFF773333303300000003333337337777777333}
        NumGlyphs = 2
        TabOrder = 1
        OnClick = btnEditDescClick
      end
      object btnDeleteDesc: TBitBtn
        Left = 735
        Top = 80
        Width = 92
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akTop, akRight]
        Caption = '&Delete'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000130B0000130B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333FF33333333333330003333333333333777333333333333
          300033FFFFFF3333377739999993333333333777777F3333333F399999933333
          3300377777733333337733333333333333003333333333333377333333333333
          3333333333333333333F333333333333330033333F33333333773333C3333333
          330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
          993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
          333333377F33333333FF3333C333333330003333733333333777333333333333
          3000333333333333377733333333333333333333333333333333}
        NumGlyphs = 2
        TabOrder = 2
        OnClick = btnDeleteDescClick
      end
      object hctlMethodDescriptions: THeaderControl
        Left = 4
        Top = 4
        Width = 723
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alNone
        Anchors = [akLeft, akTop, akRight]
        Sections = <
          item
            ImageIndex = -1
            Text = 'Pattern'
            Width = 150
          end
          item
            ImageIndex = -1
            Text = 'Description'
            Width = 400
          end>
        Style = hsFlat
      end
      object lbxMethodDescriptions: TListBox
        Left = 4
        Top = 32
        Width = 723
        Height = 434
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 4
        OnDblClick = lbxMethodDescriptionsDblClick
        OnDrawItem = lbxMethodDescriptionsDrawItem
      end
    end
  end
  object bbtnCancel: TBitBtn
    Left = 758
    Top = 522
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    TabOrder = 2
  end
  object bbtnOK: TBitBtn
    Left = 658
    Top = 522
    Width = 93
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Kind = bkOK
    TabOrder = 3
  end
  object btnCheckForUpdates: TBitBtn
    Left = 8
    Top = 521
    Width = 221
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'Configure Check for Updates...'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDDDDDDDDDDDDDDDDDDCDCDCDDCDCDDDDDDCDCDCDDCDCDDDCDDCCCDCDDCDCC
      CDDDDCDC1CDDCDCDCDCDDCD9CCCCCCCCCDDDDDDD1DDDDDDDDDDDDDD91DDDDDA2
      DDDDDDD91DDDDDAA2DDDDDDD91DDDAAAA2DDDDDDD91DDA2DAA2DDDDDD91DAADD
      DAA2D91119DDADDDDDAADD999DDDDDDDDDDADDDDDDDDDDDDDDDD}
    TabOrder = 1
    OnClick = btnCheckForUpdatesClick
  end
  object CheckedImages: TImageList
    Left = 254
    Top = 188
    Bitmap = {
      494C010102000400080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008000000080000000800000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000800000008000000080000000800000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF0000008000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF00000080
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000800000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000800000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000800000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF00C0C0C0000000FF000000FF000000FF000000FF000000FF000000
      FF000000800000000000000000000000000000000000000000000000000000FF
      000000FF0000C0C0C00000FF000000FF000000FF000000FF000000FF000000FF
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF00C0C0C000C0C0C0000000FF000000FF000000FF000000FF000000
      FF000000FF0000000000000000000000000000000000000000000000000000FF
      000000FF0000C0C0C000C0C0C00000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF00C0C0C000C0C0C0000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FF000000FF0000C0C0C000C0C0C00000FF000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FF000000FF000000FF000000FF000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FC3FFC3F00000000F81FF81F00000000F00FF00F00000000
      E007E00700000000E007E00700000000E007E00700000000E007E00700000000
      E007E00700000000F00FF00F00000000F81FF81F00000000FC3FFC3F00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
