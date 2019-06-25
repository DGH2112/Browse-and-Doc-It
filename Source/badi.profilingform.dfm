object frmProfiling: TfrmProfiling
  Left = 0
  Top = 0
  ActiveControl = vstMethods
  BorderIcons = []
  Caption = 'Profiling'
  ClientHeight = 677
  ClientWidth = 509
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    509
    677)
  PixelsPerInch = 96
  TextHeight = 16
  object btnOK: TBitBtn
    Left = 345
    Top = 647
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 426
    Top = 647
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
  end
  object pnlPanel: TPanel
    Left = 8
    Top = 8
    Width = 493
    Height = 633
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlPanel'
    TabOrder = 0
    object Splitter: TSplitter
      Left = 0
      Top = 445
      Width = 493
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object vstMethods: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 493
      Height = 445
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Height = 20
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
      Images = ilScopeImages
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      OnGetImageIndex = vstMethodsGetImageIndex
      Columns = <
        item
          Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coFixed, coSmartResize, coAllowFocus]
          Position = 0
          Width = 489
          WideText = 'Methods'
        end>
    end
    object mmoCode: TMemo
      Left = 0
      Top = 448
      Width = 493
      Height = 185
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      HideSelection = False
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
  object ilScopeImages: TImageList
    Left = 69
    Top = 379
  end
end
