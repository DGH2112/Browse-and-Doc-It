object frmSpecialTag: TfrmSpecialTag
  Left = 518
  Top = 459
  BorderStyle = bsDialog
  Caption = 'Special Tag'
  ClientHeight = 331
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object pnlForm: TPanel
    Left = 0
    Top = 0
    Width = 479
    Height = 331
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      479
      331)
    object lblBackColour: TLabel
      Left = 76
      Top = 273
      Width = 67
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = '&Back Colour'
      FocusControl = cbxBackColour
    end
    object lblDescription: TLabel
      Left = 8
      Top = 41
      Width = 63
      Height = 16
      Caption = '&Description'
      FocusControl = edtDescription
    end
    object lblFontColour: TLabel
      Left = 76
      Top = 245
      Width = 66
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = '&Font Colour'
      FocusControl = cbxFontColour
    end
    object lblName: TLabel
      Left = 8
      Top = 11
      Width = 33
      Height = 16
      Caption = '&Name'
      FocusControl = edtName
    end
    object lblTagProperties: TLabel
      Left = 8
      Top = 72
      Width = 58
      Height = 16
      Caption = '&Properties'
      FocusControl = lbxTagProperties
    end
    object btnCancel: TBitBtn
      Left = 396
      Top = 298
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 7
    end
    object btnOK: TBitBtn
      Left = 315
      Top = 298
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 6
      OnClick = btnOKClick
    end
    object cbxBackColour: TColorBox
      Left = 160
      Top = 270
      Width = 311
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 5
    end
    object cbxFontColour: TColorBox
      Left = 160
      Top = 242
      Width = 311
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames, cbCustomColors]
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 4
    end
    object edtDescription: TEdit
      Left = 76
      Top = 38
      Width = 395
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edtName: TEdit
      Left = 76
      Top = 8
      Width = 395
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object gbxFontStyles: TGroupBox
      Left = 76
      Top = 184
      Width = 395
      Height = 52
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Font Styles'
      TabOrder = 3
      object gpFontStyles: TGridPanel
        AlignWithMargins = True
        Left = 12
        Top = 21
        Width = 371
        Height = 26
        Margins.Left = 10
        Margins.Right = 10
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 25.000000000209280000
          end
          item
            Value = 25.000000000062780000
          end
          item
            Value = 24.999999999816510000
          end
          item
            Value = 24.999999999911420000
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
            Control = chkUnderlined
            Row = 0
          end
          item
            Column = 3
            Control = chkStrikeout
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        DesignSize = (
          371
          26)
        object chkBold: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 4
          Width = 86
          Height = 17
          Anchors = []
          Caption = '&Bold'
          TabOrder = 0
        end
        object chkItalic: TCheckBox
          AlignWithMargins = True
          Left = 95
          Top = 4
          Width = 86
          Height = 17
          Anchors = []
          Caption = '&Italic'
          TabOrder = 1
        end
        object chkUnderlined: TCheckBox
          AlignWithMargins = True
          Left = 187
          Top = 4
          Width = 86
          Height = 17
          Anchors = []
          Caption = '&Underlined'
          TabOrder = 2
        end
        object chkStrikeout: TCheckBox
          AlignWithMargins = True
          Left = 280
          Top = 4
          Width = 87
          Height = 17
          Anchors = []
          Caption = '&Strikeout'
          TabOrder = 3
        end
      end
    end
    object lbxTagProperties: TCheckListBox
      Left = 76
      Top = 68
      Width = 395
      Height = 110
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
    end
  end
end
