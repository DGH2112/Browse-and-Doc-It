object frameBADIModuleChecksEditorView: TframeBADIModuleChecksEditorView
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
    object N3: TMenuItem
      Caption = '-'
    end
    object ShowAllColumns1: TMenuItem
      Action = actShowAllColumns
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
    object actShowAllColumns: TAction
      Caption = '&Show All Columns'
      OnExecute = actShowAllColumnsExecute
    end
  end
end
