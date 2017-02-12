object fmBADICodeBrowsingFrame: TfmBADICodeBrowsingFrame
  Left = 0
  Top = 0
  Width = 505
  Height = 314
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    505
    314)
  object rgpBrowsePosition: TRadioGroup
    Left = 3
    Top = 3
    Width = 499
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Browse Position'
    Items.Strings = (
      'Comment top aligned with top of the editor'
      'Comment top aligned with the centre of the editor'
      'Identifier aligned with top of the editor'
      'Identifier aligned with the centre of the editor'
      'Identifier centred in the editor but show all of the comment')
    TabOrder = 0
  end
end
