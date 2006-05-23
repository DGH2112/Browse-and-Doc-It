object frmSpecialTag: TfrmSpecialTag
  Left = 518
  Top = 459
  BorderStyle = bsDialog
  Caption = 'Special Tag'
  ClientHeight = 95
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 4
    Top = 8
    Width = 28
    Height = 13
    Caption = '&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 4
    Top = 32
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object edtName: TEdit
    Left = 80
    Top = 4
    Width = 273
    Height = 21
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 80
    Top = 28
    Width = 273
    Height = 21
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 200
    Top = 68
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 280
    Top = 68
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object cbxShowInTree: TCheckBox
    Left = 4
    Top = 56
    Width = 97
    Height = 17
    Caption = '&Show In Tree'
    TabOrder = 4
  end
  object cbxAutoExpand: TCheckBox
    Left = 4
    Top = 76
    Width = 97
    Height = 17
    Caption = '&Auto Expand'
    TabOrder = 5
  end
end
