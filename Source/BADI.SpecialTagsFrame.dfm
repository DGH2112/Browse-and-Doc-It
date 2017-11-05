object fmBADISpecialTagsFrame: TfmBADISpecialTagsFrame
  Left = 0
  Top = 0
  Width = 645
  Height = 423
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    645
    423)
  object btnDelete: TBitBtn
    Left = 405
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Delete'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333FF33333333333330003333333333333777333333333333
      300033FFFFFF3333377739999993333333333777777F3333333F399999933333
      3300377777733333337733333333333333003333333333333377333333333333
      3333333333333333333F333333333333330033333F33333333773333C3333333
      330033337F3333333377333CC3333333333333F77FFFFFFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC399333777777777737733333CC3333333
      333333377F33333333FF3333C333333330003333733333333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnDeleteClick
  end
  object btnEdit: TBitBtn
    Left = 324
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Edit'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
      000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
      00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
      F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
      0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
      FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
      FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
      0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
      00333377737FFFFF773333303300000003333337337777777333}
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnEditClick
  end
  object btnMoveDown: TBitBtn
    Left = 567
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Down'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333393333
      333333333337F3333333333333397333333333333337FF333333333333C94333
      3333333333737F333333333333C9473333333333337373F3333333333CC94433
      3333333337F7F7F3333333333CC94473333333333737F73F33333333CCC94443
      333333337F37F37F33333333CCC94447333333337337F373F333333CCCC94444
      33333337F337F337F333333CCCC94444733333373337F3373F3333CCCCC94444
      4333337F3337FF337F3333CCCCC94444473333733F7773FF73F33CCCCC393444
      443337F37737F773F7F33CCC33393374447337F73337F33737FFCCC333393333
      444377733337F333777FC3333339333337437333333733333373}
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnMoveDownClick
  end
  object btnMoveUp: TBitBtn
    Left = 486
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Up'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
      337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
      4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
      44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
      473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
      7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
      33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
      33333333333737F333333333333C943333333333333737333333333333339733
      3333333333337F33333333333333933333333333333373333333}
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnMoveUpClick
  end
  object btnAdd: TBitBtn
    Left = 243
    Top = 395
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Add'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333FF33333333FF333993333333300033377F3333333777333993333333
      300033F77FFF3333377739999993333333333777777F3333333F399999933333
      33003777777333333377333993333333330033377F3333333377333993333333
      3333333773333333333F333333333333330033333333F33333773333333C3333
      330033333337FF3333773333333CC333333333FFFFF77FFF3FF33CCCCCCCCCC3
      993337777777777F77F33CCCCCCCCCC3993337777777777377333333333CC333
      333333333337733333FF3333333C333330003333333733333777333333333333
      3000333333333333377733333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 0
    OnClick = btnAddClick
  end
  object vstSpecialTags: TVirtualStringTree
    Left = 3
    Top = 3
    Width = 639
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 1
    Header.Height = 20
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    TabOrder = 5
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
    OnBeforeCellPaint = vstSpecialTagsBeforeCellPaint
    OnClick = vstSpecialTagsClick
    OnDblClick = vstSpecialTagsDblClick
    OnGetText = vstSpecialTagsGetText
    OnPaintText = vstSpecialTagsPaintText
    OnMouseDown = vstSpecialTagsMouseDown
    Columns = <
      item
        Position = 0
        Width = 100
        WideText = 'Tag Name'
      end
      item
        MinWidth = 200
        Position = 1
        Width = 235
        WideText = 'Tag Description'
      end
      item
        Alignment = taCenter
        Position = 2
        Width = 60
        WideText = 'Tree'
      end
      item
        Alignment = taCenter
        Position = 3
        Width = 60
        WideText = 'Expand'
      end
      item
        Alignment = taCenter
        Position = 4
        Width = 60
        WideText = 'Docs'
      end
      item
        Alignment = taCenter
        Position = 5
        Width = 60
        WideText = 'Fixed'
      end
      item
        Alignment = taCenter
        Position = 6
        Width = 60
        WideText = 'Syntax'
      end>
  end
end
