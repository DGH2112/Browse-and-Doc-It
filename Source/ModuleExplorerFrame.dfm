object frameModuleExplorer: TframeModuleExplorer
  Left = 0
  Top = 0
  Width = 243
  Height = 323
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object stbStatusBar: TStatusBar
    Left = 0
    Top = 304
    Width = 243
    Height = 19
    Panels = <>
  end
  object tvExplorer: TTreeView
    Left = 0
    Top = 0
    Width = 243
    Height = 304
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HideSelection = False
    Images = ilScopeImages
    Indent = 19
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 1
    ToolTips = False
    OnClick = tvExplorerClick
    OnCustomDrawItem = tvExplorerCustomDrawItem
    OnKeyDown = tvExplorerKeyDown
    OnMouseMove = tvExplorerMouseMove
  end
  object ilScopeImages: TImageList
    Left = 32
    Top = 25
  end
end
