object frmSpecialTag: TfrmSpecialTag
  Left = 518
  Top = 459
  BorderStyle = bsDialog
  Caption = 'Special Tag'
  ClientHeight = 234
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    438
    234)
  PixelsPerInch = 96
  TextHeight = 16
  object lblName: TLabel
    Left = 8
    Top = 11
    Width = 33
    Height = 16
    Caption = '&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 8
    Top = 41
    Width = 63
    Height = 16
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object lblTagProperties: TLabel
    Left = 8
    Top = 72
    Width = 58
    Height = 16
    Caption = '&Properties'
    FocusControl = lbxTagProperties
  end
  object edtName: TEdit
    Left = 76
    Top = 8
    Width = 354
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 76
    Top = 38
    Width = 354
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 274
    Top = 201
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 355
    Top = 201
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
  object lbxTagProperties: TCheckListBox
    Left = 76
    Top = 68
    Width = 354
    Height = 127
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
end
