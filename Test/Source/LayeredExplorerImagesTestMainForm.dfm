object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 900
  ClientWidth = 599
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lvImages: TListView
    Left = 0
    Top = 0
    Width = 599
    Height = 900
    Align = alClient
    Columns = <
      item
        Caption = 'Image'
        Width = 200
      end>
    SmallImages = ilImages
    TabOrder = 0
    ViewStyle = vsReport
  end
  object ilImages: TImageList
    ColorDepth = cd24Bit
    Left = 296
    Top = 160
  end
end
