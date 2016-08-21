object fmBADICodeBrowsingFrame: TfmBADICodeBrowsingFrame
  Left = 0
  Top = 0
  Width = 734
  Height = 469
  TabOrder = 0
  DesignSize = (
    734
    469)
  object rgpBrowsePosition: TRadioGroup
    Left = 3
    Top = 3
    Width = 728
    Height = 463
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
