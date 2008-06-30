object frmEditorOptions: TfrmEditorOptions
  Left = 493
  Top = 291
  ActiveControl = cbxEditorBackgroundColour
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 438
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    418
    438)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 402
    Height = 391
    ActivePage = VisualTab
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 307
    ExplicitHeight = 315
    object VisualTab: TTabSheet
      Caption = 'Visual'
      ExplicitWidth = 299
      ExplicitHeight = 287
      DesignSize = (
        394
        363)
      object lblActiveLineColour: TLabel
        Left = 3
        Top = 34
        Width = 181
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Active Line Colour'
        FocusControl = cbxActiveLineColour
        ExplicitWidth = 86
      end
      object lblEditorFontName: TLabel
        Left = 3
        Top = 62
        Width = 177
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor Font &Name'
        FocusControl = cbxFontName
        ExplicitWidth = 82
      end
      object lblEditorFontSize: TLabel
        Left = 3
        Top = 89
        Width = 169
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor Font &Size'
        FocusControl = edtFontSize
        ExplicitWidth = 74
      end
      object lblRightEdgePosition: TLabel
        Left = 3
        Top = 116
        Width = 188
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Right Edge Position'
        FocusControl = edtRightEdge
        ExplicitWidth = 93
      end
      object lblRightEdgeColour: TLabel
        Left = 3
        Top = 143
        Width = 181
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Right &Edge Colour'
        FocusControl = cbxRightEdgeColour
        ExplicitWidth = 86
      end
      object lblForeColour: TLabel
        Left = 3
        Top = 171
        Width = 218
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Selected Text &Fore Colour'
        FocusControl = cbxSelectedForeground
        ExplicitWidth = 123
      end
      object lblBackColour: TLabel
        Left = 3
        Top = 199
        Width = 222
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Selected &Text Back Colour'
        FocusControl = cbxSelectedBackground
        ExplicitWidth = 127
      end
      object lblEditorBackgroundColour: TLabel
        Left = 3
        Top = 6
        Width = 216
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Editor &Background Colour'
        FocusControl = cbxEditorBackgroundColour
        ExplicitWidth = 121
      end
      object lblTabWidth: TLabel
        Left = 3
        Top = 227
        Width = 192
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Tab &width in Spaces'
        FocusControl = edtTabWidth
        ExplicitWidth = 97
      end
      object cbxActiveLineColour: TColorBox
        Left = 246
        Top = 31
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 1
        ExplicitLeft = 151
      end
      object cbxFontName: TComboBox
        Left = 246
        Top = 59
        Width = 145
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
        ExplicitLeft = 151
      end
      object edtFontSize: TEdit
        Left = 247
        Top = 86
        Width = 129
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 3
        Text = '6'
        ExplicitLeft = 152
      end
      object udEditorFontSize: TUpDown
        Left = 376
        Top = 86
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = edtFontSize
        Min = 6
        Max = 72
        Position = 6
        TabOrder = 4
        ExplicitLeft = 281
      end
      object chxLineNumbers: TCheckBox
        Left = 3
        Top = 256
        Width = 388
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'S&how Line Numbers in the Editor Gutter'
        TabOrder = 12
        ExplicitWidth = 293
      end
      object edtRightEdge: TEdit
        Left = 247
        Top = 113
        Width = 129
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 5
        Text = '0'
        ExplicitLeft = 152
      end
      object udRightEdgePosition: TUpDown
        Left = 376
        Top = 113
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = edtRightEdge
        Max = 1024
        TabOrder = 6
        ExplicitLeft = 281
      end
      object cbxRightEdgeColour: TColorBox
        Left = 247
        Top = 140
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 7
        ExplicitLeft = 152
      end
      object cbxSelectedForeground: TColorBox
        Left = 247
        Top = 168
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 8
        ExplicitLeft = 152
      end
      object cbxSelectedBackground: TColorBox
        Left = 247
        Top = 196
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 9
        ExplicitLeft = 152
      end
      object cbxEditorBackgroundColour: TColorBox
        Left = 246
        Top = 3
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 0
        ExplicitLeft = 151
      end
      object edtTabWidth: TEdit
        Left = 247
        Top = 224
        Width = 129
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 10
        Text = '1'
        ExplicitLeft = 152
      end
      object udTabWidth: TUpDown
        Left = 376
        Top = 224
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = edtTabWidth
        Min = 1
        Max = 8
        Position = 1
        TabOrder = 11
        ExplicitLeft = 281
      end
    end
    object BehaviourTab: TTabSheet
      Caption = 'Behaviour'
      ImageIndex = 1
      ExplicitWidth = 299
      ExplicitHeight = 287
      DesignSize = (
        394
        363)
      object clbOptions: TCheckListBox
        Left = 3
        Top = 3
        Width = 388
        Height = 357
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 293
        ExplicitHeight = 281
      end
    end
    object SyntaxTab: TTabSheet
      Caption = 'Syntax Highlighting'
      ImageIndex = 2
      ExplicitWidth = 299
      ExplicitHeight = 287
      DesignSize = (
        394
        363)
      object lblAttrForeColour: TLabel
        Left = 238
        Top = 40
        Width = 129
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Attribute &Foreground Colour'
        FocusControl = cbxAttrForeColour
        ExplicitLeft = 143
      end
      object lblAttrBackColour: TLabel
        Left = 242
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
        Width = 389
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
        Width = 229
        Height = 304
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbAttributesClick
        ExplicitWidth = 134
        ExplicitHeight = 228
      end
      object cbxAttrForeColour: TColorBox
        Left = 238
        Top = 59
        Width = 153
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 1
        OnChange = AttributeChange
        ExplicitLeft = 143
      end
      object cbxAttrBackColour: TColorBox
        Left = 238
        Top = 104
        Width = 153
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akTop, akRight]
        ItemHeight = 16
        TabOrder = 2
        OnChange = AttributeChange
        ExplicitLeft = 143
      end
      object grpFontStyles: TGroupBox
        Left = 238
        Top = 132
        Width = 153
        Height = 228
        Anchors = [akTop, akRight, akBottom]
        Caption = 'Attribute Font Styles'
        TabOrder = 3
        ExplicitLeft = 143
        ExplicitHeight = 152
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
    Left = 335
    Top = 405
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
    ExplicitLeft = 240
    ExplicitTop = 329
  end
  object btnCancel: TBitBtn
    Left = 254
    Top = 406
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkCancel
    ExplicitLeft = 159
    ExplicitTop = 330
  end
end
