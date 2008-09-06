object frmDocumentationOptions: TfrmDocumentationOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Documentation Options'
  ClientHeight = 191
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    350
    191)
  PixelsPerInch = 96
  TextHeight = 13
  object rgpDocumentationOptions: TRadioGroup
    Left = 8
    Top = 8
    Width = 253
    Height = 175
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Documentation Options'
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 267
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 1
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 267
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 2
    Kind = bkCancel
  end
end
