object frameBADIModuleStatistics: TframeBADIModuleStatistics
  Left = 0
  Top = 0
  Width = 894
  Height = 498
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object vstStatistics: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 894
    Height = 498
    Align = alClient
    EmptyListMessage = 'Nothing to see here....'
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    HintAnimation = hatFade
    HintMode = hmTooltip
    Images = ilScopeImages
    PopupMenu = pabContextMenu
    ScrollBarOptions.AlwaysVisible = True
    SelectionBlendFactor = 64
    TabOrder = 0
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
    OnBeforeCellPaint = vstStatisticsBeforeCellPaint
    OnCompareNodes = vstStatisticsCompareNodes
    OnFreeNode = vstStatisticsFreeNode
    OnGetText = vstStatisticsGetText
    OnPaintText = vstStatisticsPaintText
    OnGetImageIndex = vstStatisticsGetImageIndex
    Columns = <
      item
        MinWidth = 300
        Position = 0
        Width = 444
        WideText = 'Method'
      end
      item
        Alignment = taCenter
        Position = 1
        Width = 75
        WideText = 'Lines'
      end
      item
        Alignment = taCenter
        Position = 2
        Width = 75
        WideText = 'Params'
      end
      item
        Alignment = taCenter
        Position = 3
        Width = 75
        WideText = 'Variables'
      end
      item
        Alignment = taCenter
        Position = 4
        Width = 75
        WideText = 'IF Depth'
      end
      item
        Alignment = taCenter
        Position = 5
        Width = 75
        WideText = 'Complexity'
      end
      item
        Alignment = taCenter
        Position = 6
        Width = 75
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
  object pabContextMenu: TPopupActionBar
    Left = 128
    Top = 120
    object ExpandAll1: TMenuItem
      Action = actExpandAll
    end
    object CollapseAll1: TMenuItem
      Action = actCollapseAll
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Expand1: TMenuItem
      Action = actExpand
    end
    object Collapse1: TMenuItem
      Action = actCollapse
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ExpandIssues1: TMenuItem
      Action = actExpandIssues
    end
  end
  object alActions: TActionList
    Left = 40
    Top = 120
    object actExpandAll: TAction
      Caption = 'Expand All'
      OnExecute = actExpandAllExecute
    end
    object actCollapseAll: TAction
      Caption = 'Collapse All'
      OnExecute = actCollapseAllExecute
    end
    object actExpand: TAction
      Caption = 'Expand'
      OnExecute = actExpandExecute
      OnUpdate = actExpandUpdate
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      OnExecute = actCollapseExecute
      OnUpdate = actCollapseUpdate
    end
    object actExpandIssues: TAction
      Caption = 'Expand Issues'
      OnExecute = actExpandIssuesExecute
    end
  end
end
