object frmOptions: TfrmOptions
  Left = 445
  Top = 269
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 562
  ClientWidth = 860
  Color = clBtnFace
  Constraints.MinHeight = 431
  Constraints.MinWidth = 677
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    860
    562)
  PixelsPerInch = 96
  TextHeight = 16
  object OptionTab: TPageControl
    Left = 10
    Top = 10
    Width = 840
    Height = 513
    ActivePage = tabGeneralOptions
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabGeneralOptions: TTabSheet
      Caption = 'General Options'
    end
    object tabSpecialTags: TTabSheet
      Caption = 'Special Tags'
      ImageIndex = 1
    end
    object tabModuleExplorer: TTabSheet
      Caption = 'Module Explorer'
      ImageIndex = 3
    end
    object tabCodeBrowsing: TTabSheet
      Caption = 'Code Browsing'
      ImageIndex = 4
    end
    object tabExcludeDocFiles: TTabSheet
      Caption = 'Exclude Doc Files'
      ImageIndex = 5
    end
    object tabMethodDescriptions: TTabSheet
      Caption = 'Method Descriptions'
      ImageIndex = 6
    end
  end
  object bbtnCancel: TBitBtn
    Left = 777
    Top = 529
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object bbtnOK: TBitBtn
    Left = 696
    Top = 529
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCheckForUpdates: TBitBtn
    Left = 8
    Top = 529
    Width = 221
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Configure Check for Updates...'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
      DDDDDDDDDDDDDDDDDDDDDCDCDCDDCDCDDDDDDCDCDCDDCDCDDDCDDCCCDCDDCDCC
      CDDDDCDC1CDDCDCDCDCDDCD9CCCCCCCCCDDDDDDD1DDDDDDDDDDDDDD91DDDDDA2
      DDDDDDD91DDDDDAA2DDDDDDD91DDDAAAA2DDDDDDD91DDA2DAA2DDDDDD91DAADD
      DAA2D91119DDADDDDDAADD999DDDDDDDDDDADDDDDDDDDDDDDDDD}
    TabOrder = 1
    OnClick = btnCheckForUpdatesClick
  end
end
