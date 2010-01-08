object frmEditorOptions: TfrmEditorOptions
  Left = 493
  Top = 291
  ActiveControl = cbxEditorBackgroundColour
  BorderIcons = []
  Caption = 'Editor Options'
  ClientHeight = 356
  ClientWidth = 442
  Color = clBtnFace
  Constraints.MinHeight = 390
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    442
    356)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 426
    Height = 309
    ActivePage = VisualTab
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object VisualTab: TTabSheet
      Caption = 'Visual'
      DesignSize = (
        418
        281)
      object lblActiveLineColour: TLabel
        Left = 3
        Top = 34
        Width = 86
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Active Line Colour'
        FocusControl = cbxActiveLineColour
      end
      object lblEditorFontName: TLabel
        Left = 3
        Top = 62
        Width = 82
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor Font &Name'
        FocusControl = cbxFontName
      end
      object lblEditorFontSize: TLabel
        Left = 3
        Top = 89
        Width = 74
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor Font &Size'
        FocusControl = edtFontSize
      end
      object lblRightEdgePosition: TLabel
        Left = 3
        Top = 116
        Width = 93
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Right Edge Position'
        FocusControl = edtRightEdge
      end
      object lblRightEdgeColour: TLabel
        Left = 3
        Top = 143
        Width = 86
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Right &Edge Colour'
        FocusControl = cbxRightEdgeColour
      end
      object lblForeColour: TLabel
        Left = 3
        Top = 171
        Width = 123
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Selected Text &Fore Colour'
        FocusControl = cbxSelectedForeground
      end
      object lblBackColour: TLabel
        Left = 3
        Top = 199
        Width = 127
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Selected &Text Back Colour'
        FocusControl = cbxSelectedBackground
      end
      object lblEditorBackgroundColour: TLabel
        Left = 3
        Top = 6
        Width = 121
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor &Background Colour'
        FocusControl = cbxEditorBackgroundColour
      end
      object lblTabWidth: TLabel
        Left = 3
        Top = 227
        Width = 97
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Tab &width in Spaces'
        FocusControl = edtTabWidth
      end
      object cbxActiveLineColour: TColorBox
        Left = 233
        Top = 31
        Width = 182
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 1
      end
      object cbxFontName: TComboBox
        Left = 233
        Top = 59
        Width = 182
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
      end
      object edtFontSize: TEdit
        Left = 233
        Top = 86
        Width = 167
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 3
        Text = '6'
      end
      object udEditorFontSize: TUpDown
        Left = 400
        Top = 86
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = edtFontSize
        Min = 6
        Max = 72
        Position = 6
        TabOrder = 4
      end
      object chxLineNumbers: TCheckBox
        Left = 3
        Top = 256
        Width = 412
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'S&how Line Numbers in the Editor Gutter'
        TabOrder = 12
      end
      object edtRightEdge: TEdit
        Left = 233
        Top = 113
        Width = 167
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 5
        Text = '0'
      end
      object udRightEdgePosition: TUpDown
        Left = 400
        Top = 113
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = edtRightEdge
        Max = 1024
        TabOrder = 6
      end
      object cbxRightEdgeColour: TColorBox
        Left = 233
        Top = 140
        Width = 183
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 7
      end
      object cbxSelectedForeground: TColorBox
        Left = 233
        Top = 168
        Width = 183
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 8
      end
      object cbxSelectedBackground: TColorBox
        Left = 233
        Top = 196
        Width = 183
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 9
      end
      object cbxEditorBackgroundColour: TColorBox
        Left = 233
        Top = 3
        Width = 182
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 0
      end
      object edtTabWidth: TEdit
        Left = 233
        Top = 224
        Width = 167
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 10
        Text = '1'
      end
      object udTabWidth: TUpDown
        Left = 400
        Top = 224
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 394
      ExplicitHeight = 363
      DesignSize = (
        418
        281)
      object clbOptions: TCheckListBox
        Left = 3
        Top = 3
        Width = 412
        Height = 275
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object SyntaxTab: TTabSheet
      Caption = 'Syntax Highlighting'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 394
      ExplicitHeight = 363
      DesignSize = (
        418
        281)
      object lblAttrForeColour: TLabel
        Left = 262
        Top = 40
        Width = 129
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Attribute &Foreground Colour'
        FocusControl = cbxAttrForeColour
        ExplicitLeft = 143
      end
      object lblAttrBackColour: TLabel
        Left = 266
        Top = 87
        Width = 133
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Attribute Background &Colour'
        FocusControl = cbxAttrBackColour
        ExplicitLeft = 147
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
        Width = 413
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'No Highligher Available'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 294
      end
      object lbAttributes: TListBox
        Left = 3
        Top = 56
        Width = 253
        Height = 222
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbAttributesClick
      end
      object cbxAttrForeColour: TColorBox
        Left = 262
        Top = 59
        Width = 153
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 1
        OnChange = AttributeChange
        ExplicitLeft = 238
      end
      object cbxAttrBackColour: TColorBox
        Left = 262
        Top = 104
        Width = 153
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 2
        OnChange = AttributeChange
        ExplicitLeft = 238
      end
      object grpFontStyles: TGroupBox
        Left = 262
        Top = 132
        Width = 153
        Height = 146
        Anchors = [akTop, akRight, akBottom]
        Caption = 'Attribute Font Styles'
        TabOrder = 3
        ExplicitLeft = 238
        ExplicitHeight = 228
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
    Left = 359
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 278
    Top = 324
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 1
  end
end
