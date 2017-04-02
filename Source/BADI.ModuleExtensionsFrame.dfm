object fmBADIModuleExtensionsFrame: TfmBADIModuleExtensionsFrame
  Left = 0
  Top = 0
  Width = 583
  Height = 325
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    583
    325)
  object vleModuleExtensions: TValueListEditor
    Left = 3
    Top = 3
    Width = 577
    Height = 319
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultColWidth = 200
    DefaultRowHeight = 20
    FixedCols = 1
    KeyOptions = [keyEdit]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goThumbTracking]
    ScrollBars = ssVertical
    TabOrder = 0
    TitleCaptions.Strings = (
      'Module Name'
      'File Extensions (semi-colon separated)')
    ColWidths = (
      200
      371)
  end
end
