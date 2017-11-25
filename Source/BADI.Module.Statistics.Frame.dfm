object frameBADIModuleStatistics: TframeBADIModuleStatistics
  Left = 0
  Top = 0
  Width = 737
  Height = 498
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object vstStatistics: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 731
    Height = 492
    Align = alClient
    EmptyListMessage = 'Nothing to see here....'
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    HintAnimation = hatFade
    HintMode = hmTooltip
    Images = ilScopeImages
    TabOrder = 0
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toGridExtensions, toInitOnSave]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = vstStatisticsBeforeCellPaint
    OnGetText = vstStatisticsGetText
    OnGetImageIndex = vstStatisticsGetImageIndex
    Columns = <
      item
        Position = 0
        Width = 217
        WideText = 'Method'
      end
      item
        Alignment = taCenter
        Position = 1
        Width = 85
        WideText = 'Lines'
      end
      item
        Alignment = taCenter
        Position = 2
        Width = 85
        WideText = 'Parameters'
      end
      item
        Alignment = taCenter
        Position = 3
        Width = 85
        WideText = 'Variables'
      end
      item
        Alignment = taCenter
        Position = 4
        Width = 85
        WideText = 'IF Depth'
      end
      item
        Alignment = taCenter
        Position = 5
        Width = 85
        WideText = 'Complexity'
      end
      item
        Alignment = taCenter
        Position = 6
        Width = 85
        WideText = 'Toxicity'
      end>
  end
  object ilScopeImages: TImageList
    Left = 40
    Top = 64
  end
  object tmFocusTimer: TTimer
    Interval = 100
    OnTimer = tmFocusTimerTimer
    Left = 128
    Top = 64
  end
end
