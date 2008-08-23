object frmOptions: TfrmOptions
  Left = 445
  Top = 269
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 424
  ClientWidth = 664
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    664
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object OptionTab: TPageControl
    Left = 8
    Top = 8
    Width = 648
    Height = 377
    ActivePage = Page1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Page1: TTabSheet
      Caption = 'General Options'
      object clbOptions: TCheckListBox
        Left = 0
        Top = 0
        Width = 640
        Height = 315
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
      object IntervalPanel: TPanel
        Left = 0
        Top = 315
        Width = 640
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblRefreshInterval: TLabel
          Left = 8
          Top = 12
          Width = 173
          Height = 13
          Caption = 'Refresh &Interval after Editor changes'
        end
        object edtUpdateInterval: TEdit
          Left = 200
          Top = 11
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '100'
        end
        object udUpdateInterval: TUpDown
          Left = 257
          Top = 11
          Width = 16
          Height = 21
          Associate = edtUpdateInterval
          Min = 100
          Max = 30000
          Increment = 100
          Position = 100
          TabOrder = 1
        end
      end
    end
    object Page2: TTabSheet
      Caption = 'Special Tags'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        640
        349)
      object lbSpecialTags: TListBox
        Left = 3
        Top = 19
        Width = 634
        Height = 296
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 16
        TabOrder = 0
        OnDblClick = btnEditClick
        OnDrawItem = lbSpecialTagsDrawItem
        OnMouseDown = lbSpecialTagsMouseDown
      end
      object HeaderControl1: THeaderControl
        Left = 3
        Top = 3
        Width = 634
        Height = 17
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
        Left = 238
        Top = 321
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddClick
      end
      object btnDelete: TBitBtn
        Left = 400
        Top = 321
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnDeleteClick
      end
      object btnEdit: TBitBtn
        Left = 319
        Top = 321
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Edit'
        TabOrder = 4
        OnClick = btnEditClick
      end
      object btnMoveDown: TBitBtn
        Left = 562
        Top = 321
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Move Down'
        TabOrder = 5
        OnClick = btnMoveDownClick
      end
      object btnMoveUp: TBitBtn
        Left = 481
        Top = 321
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Move &Up'
        TabOrder = 6
        OnClick = btnMoveUpClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Module Explorer'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        640
        349)
      object lblFontName: TLabel
        Left = 3
        Top = 6
        Width = 55
        Height = 13
        Caption = 'Font &Name:'
        FocusControl = cbxFontName
      end
      object lblFontSize: TLabel
        Left = 3
        Top = 33
        Width = 44
        Height = 13
        Caption = 'Font &Size'
        FocusControl = edtFontSize
      end
      object lblTokenTypes: TLabel
        Left = 3
        Top = 66
        Width = 63
        Height = 13
        Caption = '&Token Types'
        FocusControl = lbxTokenTypes
      end
      object cbxFontName: TComboBox
        Left = 79
        Top = 3
        Width = 559
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 0
      end
      object edtFontSize: TEdit
        Left = 79
        Top = 30
        Width = 540
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 1
        Text = '8'
      end
      object udFontSize: TUpDown
        Left = 619
        Top = 30
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = edtFontSize
        Min = 8
        Max = 72
        Position = 8
        TabOrder = 2
      end
      object lbxTokenTypes: TListBox
        Left = 3
        Top = 84
        Width = 460
        Height = 262
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 3
        OnClick = lbxTokenTypesClick
      end
      object cbxFontColour: TColorBox
        Left = 469
        Top = 84
        Width = 168
        Height = 22
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 4
        OnChange = cbxFontColourChange
      end
      object gbxFontStyles: TGroupBox
        Left = 469
        Top = 112
        Width = 168
        Height = 234
        Anchors = [akTop, akRight, akBottom]
        Caption = 'Font Styles'
        TabOrder = 5
        object chkBold: TCheckBox
          Left = 12
          Top = 21
          Width = 97
          Height = 17
          Caption = '&Bold'
          TabOrder = 0
          OnClick = chkBoldClick
        end
        object chkItalic: TCheckBox
          Left = 13
          Top = 44
          Width = 97
          Height = 17
          Caption = '&Italic'
          TabOrder = 1
          OnClick = chkItalicClick
        end
        object chkUnderline: TCheckBox
          Left = 13
          Top = 67
          Width = 97
          Height = 17
          Caption = '&Underline'
          TabOrder = 2
          OnClick = chkUnderlineClick
        end
        object chkStrikeout: TCheckBox
          Left = 12
          Top = 90
          Width = 97
          Height = 17
          Caption = '&Strikeout'
          TabOrder = 3
          OnClick = chkStrikeoutClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Code Browsing'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        640
        349)
      object rgpBrowsePosition: TRadioGroup
        Left = 3
        Top = 3
        Width = 634
        Height = 343
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
      Caption = 'Exclude Doc Files'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        640
        349)
      object mmoExcludeDocFiles: TMemo
        Left = 3
        Top = 3
        Width = 634
        Height = 343
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'mmoExcludeDocFiles')
        TabOrder = 0
      end
    end
    object tabMethodDescriptions: TTabSheet
      Caption = 'Method Descriptions'
      ImageIndex = 6
      DesignSize = (
        640
        349)
      object lvMethodDescriptions: TListView
        Left = 3
        Top = 3
        Width = 553
        Height = 343
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Pattern'
            Width = 150
          end
          item
            Caption = 'Description'
            Width = 375
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvMethodDescriptionsDblClick
      end
      object btnAddDesc: TBitBtn
        Left = 562
        Top = 3
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Add'
        TabOrder = 1
        OnClick = btnAddDescClick
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
      end
      object btnEditDesc: TBitBtn
        Left = 562
        Top = 34
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Edit'
        TabOrder = 2
        OnClick = btnEditDescClick
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
      end
      object btnDeleteDesc: TBitBtn
        Left = 562
        Top = 65
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnDeleteDescClick
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
      end
    end
    object HelpFilePage: TTabSheet
      Caption = ' Help File Directory'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object HelpFileDir: TDirectoryListBox
        Left = 0
        Top = 0
        Width = 640
        Height = 349
        Align = alClient
        ItemHeight = 16
        TabOrder = 0
      end
    end
  end
  object bbtnCancel: TBitBtn
    Left = 581
    Top = 391
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
  object bbtnOK: TBitBtn
    Left = 500
    Top = 391
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
  end
  object CheckedImages: TImageList
    Left = 50
    Top = 48
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
