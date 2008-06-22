object frmEditorOptions: TfrmEditorOptions
  Left = 493
  Top = 291
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 362
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    323
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 307
    Height = 315
    ActivePage = VisualTab
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object VisualTab: TTabSheet
      Caption = 'Visual'
      object lblActiveLineColour: TLabel
        Left = 3
        Top = 34
        Width = 86
        Height = 13
        Caption = '&Active Line Colour'
        FocusControl = cbxActiveLineColour
      end
      object lblEditorFontName: TLabel
        Left = 3
        Top = 62
        Width = 82
        Height = 13
        Caption = 'Editor Font &Name'
        FocusControl = cbxFontName
      end
      object lblEditorFontSize: TLabel
        Left = 3
        Top = 89
        Width = 74
        Height = 13
        Caption = 'Editor Font &Size'
        FocusControl = edtFontSize
      end
      object lblRightEdgePosition: TLabel
        Left = 3
        Top = 116
        Width = 93
        Height = 13
        Caption = '&Right Edge Position'
        FocusControl = edtRightEdge
      end
      object lblRightEdgeColour: TLabel
        Left = 3
        Top = 143
        Width = 86
        Height = 13
        Caption = 'Right &Edge Colour'
        FocusControl = cbxRightEdgeColour
      end
      object lblForeColour: TLabel
        Left = 3
        Top = 171
        Width = 123
        Height = 13
        Caption = 'Selected Text &Fore Colour'
        FocusControl = cbxSelectedForeground
      end
      object lblBackColour: TLabel
        Left = 3
        Top = 199
        Width = 127
        Height = 13
        Caption = 'Selected &Text Back Colour'
        FocusControl = cbxSelectedBackground
      end
      object lblEditorBackgroundColour: TLabel
        Left = 3
        Top = 6
        Width = 121
        Height = 13
        Caption = 'Editor &Background Colour'
        FocusControl = cbxEditorBackgroundColour
      end
      object lblTabWidth: TLabel
        Left = 3
        Top = 227
        Width = 97
        Height = 13
        Caption = 'Tab &width in Spaces'
        FocusControl = edtTabWidth
      end
      object cbxActiveLineColour: TColorBox
        Left = 151
        Top = 31
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 1
      end
      object cbxFontName: TComboBox
        Left = 151
        Top = 59
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
      end
      object edtFontSize: TEdit
        Left = 152
        Top = 86
        Width = 129
        Height = 21
        TabOrder = 3
        Text = '6'
      end
      object udEditorFontSize: TUpDown
        Left = 281
        Top = 86
        Width = 16
        Height = 21
        Associate = edtFontSize
        Min = 6
        Max = 72
        Position = 6
        TabOrder = 4
      end
      object chxLineNumbers: TCheckBox
        Left = 3
        Top = 256
        Width = 293
        Height = 17
        Caption = 'S&how Line Numbers in the Editor Gutter'
        TabOrder = 12
      end
      object edtRightEdge: TEdit
        Left = 152
        Top = 113
        Width = 129
        Height = 21
        TabOrder = 5
        Text = '0'
      end
      object udRightEdgePosition: TUpDown
        Left = 281
        Top = 113
        Width = 16
        Height = 21
        Associate = edtRightEdge
        Max = 1024
        TabOrder = 6
      end
      object cbxRightEdgeColour: TColorBox
        Left = 152
        Top = 140
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 7
      end
      object cbxSelectedForeground: TColorBox
        Left = 152
        Top = 168
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 8
      end
      object cbxSelectedBackground: TColorBox
        Left = 152
        Top = 196
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 9
      end
      object cbxEditorBackgroundColour: TColorBox
        Left = 151
        Top = 3
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object edtTabWidth: TEdit
        Left = 152
        Top = 224
        Width = 129
        Height = 21
        TabOrder = 10
        Text = '1'
      end
      object udTabWidth: TUpDown
        Left = 281
        Top = 224
        Width = 16
        Height = 21
        Associate = edtTabWidth
        Min = 1
        Max = 8
        Position = 1
        TabOrder = 11
      end
    end
    object BehaviourTab: TTabSheet
      Caption = 'Behaviour'
      ImageIndex = 1
      DesignSize = (
        299
        287)
      object clbOptions: TCheckListBox
        Left = 3
        Top = 3
        Width = 293
        Height = 281
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object SyntaxTab: TTabSheet
      Caption = 'Syntax Highlighting'
      ImageIndex = 2
      DesignSize = (
        299
        287)
      object lblAttrForeColour: TLabel
        Left = 143
        Top = 40
        Width = 129
        Height = 13
        Caption = 'Attribute &Foreground Colour'
        FocusControl = cbxAttrForeColour
      end
      object lblAttrBackColour: TLabel
        Left = 147
        Top = 87
        Width = 133
        Height = 13
        Caption = 'Attribute Background &Colour'
        FocusControl = cbxAttrBackColour
      end
      object lblAttributes: TLabel
        Left = 3
        Top = 37
        Width = 44
        Height = 13
        Caption = '&Attributes'
        FocusControl = lbAttributes
      end
      object lblHighlighterType: TLabel
        Left = 3
        Top = 3
        Width = 294
        Height = 21
        AutoSize = False
        Caption = 'No Highligher Available'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbAttributes: TListBox
        Left = 3
        Top = 56
        Width = 134
        Height = 228
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbAttributesClick
      end
      object cbxAttrForeColour: TColorBox
        Left = 143
        Top = 59
        Width = 153
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 1
        OnChange = AttributeChange
      end
      object cbxAttrBackColour: TColorBox
        Left = 143
        Top = 104
        Width = 153
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 2
        OnChange = AttributeChange
      end
      object grpFontStyles: TGroupBox
        Left = 143
        Top = 132
        Width = 153
        Height = 145
        Caption = 'Attribute Font Styles'
        TabOrder = 3
        object cbxBold: TCheckBox
          Left = 8
          Top = 24
          Width = 97
          Height = 17
          Caption = '&Bold'
          TabOrder = 0
          OnClick = AttributeChange
        end
        object cbxItalic: TCheckBox
          Left = 8
          Top = 48
          Width = 97
          Height = 17
          Caption = '&Italic'
          TabOrder = 1
          OnClick = AttributeChange
        end
        object cbxUnderlined: TCheckBox
          Left = 8
          Top = 72
          Width = 97
          Height = 17
          Caption = '&Underlined'
          TabOrder = 2
          OnClick = AttributeChange
        end
        object cbxStrikeout: TCheckBox
          Left = 8
          Top = 96
          Width = 97
          Height = 17
          Caption = '&Strikeout'
          TabOrder = 3
          OnClick = AttributeChange
        end
      end
    end
  end
  object btnOK: TBitBtn
    Left = 240
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 159
    Top = 330
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
end
