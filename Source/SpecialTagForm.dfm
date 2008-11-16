object frmSpecialTag: TfrmSpecialTag
  Left = 518
  Top = 459
  BorderStyle = bsDialog
  Caption = 'Special Tag'
  ClientHeight = 132
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    438
    132)
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 11
    Width = 28
    Height = 13
    Caption = '&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 8
    Top = 38
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object edtName: TEdit
    Left = 76
    Top = 8
    Width = 354
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 76
    Top = 35
    Width = 354
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 274
    Top = 99
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 355
    Top = 99
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    Kind = bkCancel
  end
  object cbxShowInTree: TCheckBox
    Left = 8
    Top = 62
    Width = 260
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Show In Tree'
    TabOrder = 2
  end
  object cbxAutoExpand: TCheckBox
    Left = 8
    Top = 85
    Width = 260
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Auto Expand'
    TabOrder = 3
  end
  object chkShowInDoc: TCheckBox
    Left = 8
    Top = 108
    Width = 260
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Show In Documentation'
    TabOrder = 4
  end
end
