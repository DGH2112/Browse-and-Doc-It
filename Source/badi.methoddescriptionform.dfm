object frmMethodDescriptions: TfrmMethodDescriptions
  Left = 529
  Top = 589
  Caption = 'Method Descriptions'
  ClientHeight = 100
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MaxHeight = 135
  Constraints.MinHeight = 135
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    624
    100)
  PixelsPerInch = 96
  TextHeight = 16
  object lblPattern: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 16
    Caption = '&Pattern'
    FocusControl = edtPattern
  end
  object lblDescription: TLabel
    Left = 8
    Top = 54
    Width = 63
    Height = 16
    Caption = '&Description'
  end
  object edtPattern: TEdit
    Left = 8
    Top = 27
    Width = 519
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 8
    Top = 73
    Width = 519
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 533
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 533
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end