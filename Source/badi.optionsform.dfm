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
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  DesignSize = (
    860
    562)
  TextHeight = 16
  object OptionTab: TPageControl
    Left = 10
    Top = 10
    Width = 840
    Height = 513
    ActivePage = tabModuleExtensions
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
    object tabMenuShortcuts: TTabSheet
      Caption = 'Menu Shortcuts'
      ImageIndex = 6
    end
    object tabModuleExtensions: TTabSheet
      Caption = 'Module Extensions'
      ImageIndex = 7
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
    TabOrder = 2
  end
  object bbtnOK: TBitBtn
    Left = 696
    Top = 529
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
  end
end
