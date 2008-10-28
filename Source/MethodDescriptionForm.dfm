object frmMethodDescriptions: TfrmMethodDescriptions
  Left = 529
  Top = 589
  Width = 640
  Height = 135
  Caption = 'Method Descriptions'
  Color = clBtnFace
  Constraints.MaxHeight = 135
  Constraints.MinHeight = 135
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    632
    101)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPattern: TLabel
    Left = 8
    Top = 8
    Width = 36
    Height = 13
    Caption = '&Pattern'
    FocusControl = edtPattern
  end
  object lblDescription: TLabel
    Left = 8
    Top = 54
    Width = 53
    Height = 13
    Caption = '&Description'
  end
  object edtPattern: TEdit
    Left = 8
    Top = 27
    Width = 535
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 8
    Top = 73
    Width = 535
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 549
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 549
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 3
    Kind = bkCancel
  end
end
