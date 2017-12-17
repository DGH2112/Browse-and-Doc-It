object frameBADIModuleMetrics: TframeBADIModuleMetrics
  Left = 0
  Top = 0
  Width = 419
  Height = 344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object vstMetrics: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 413
    Height = 338
    Align = alClient
    EditDelay = 250
    Header.AutoSizeIndex = 0
    Header.Height = 20
    Header.Options = [hoAutoResize, hoColumnResize, hoShowImages, hoVisible]
    Header.ParentFont = True
    SelectionBlendFactor = 64
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toWheelPanning, toEditOnClick, toEditOnDblClick]
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
    OnBeforeCellPaint = vstMetricsBeforeCellPaint
    OnChecked = vstMetricsChecked
    OnEditing = vstMetricsEditing
    OnGetText = vstMetricsGetText
    OnPaintText = vstMetricsPaintText
    OnHeaderClick = vstMetricsHeaderClick
    OnNewText = vstMetricsNewText
    Columns = <
      item
        CheckBox = True
        Position = 0
        Width = 184
        WideText = 'Module Metrics and Checks'
      end
      item
        Position = 1
        Width = 150
        WideText = 'Name'
      end
      item
        Alignment = taRightJustify
        Position = 2
        Width = 75
        WideText = 'Limit'
      end>
  end
end
