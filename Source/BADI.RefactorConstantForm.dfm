object frmBADIRefactorConstant: TfrmBADIRefactorConstant
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'BADI Refactor Constant'
  ClientHeight = 185
  ClientWidth = 437
  Color = clBtnFace
  Constraints.MinHeight = 220
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object pnlFudgePanel: TPanel
    Left = 0
    Top = 0
    Width = 437
    Height = 185
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      437
      185)
    object lblLiteral: TLabel
      Left = 8
      Top = 38
      Width = 39
      Height = 13
      Caption = '&Literal:'
      FocusControl = edtLiteral
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblName: TLabel
      Left = 8
      Top = 11
      Width = 35
      Height = 13
      Caption = '&Name:'
      FocusControl = edtName
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblScope: TLabel
      Left = 8
      Top = 92
      Width = 37
      Height = 13
      Caption = '&Scope:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblType: TLabel
      Left = 8
      Top = 65
      Width = 31
      Height = 13
      Caption = '&Type:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnCancel: TBitBtn
      Left = 354
      Top = 152
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 6
    end
    object btnOK: TBitBtn
      Left = 273
      Top = 152
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 5
      OnClick = btnOKClick
    end
    object cbxScope: TComboBox
      Left = 72
      Top = 89
      Width = 357
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object cbxType: TComboBox
      Left = 72
      Top = 62
      Width = 357
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object chkNewLine: TCheckBox
      Left = 72
      Top = 119
      Width = 357
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'New &Line between declaration sections'
      TabOrder = 4
    end
    object edtLiteral: TEdit
      Left = 72
      Top = 35
      Width = 357
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      ReadOnly = True
      TabOrder = 1
    end
    object edtName: TEdit
      Left = 72
      Top = 8
      Width = 357
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
end
