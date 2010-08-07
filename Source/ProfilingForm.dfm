object frmProfiling: TfrmProfiling
  Left = 0
  Top = 0
  ActiveControl = vstTestCases
  BorderIcons = []
  Caption = 'DUnit'
  ClientHeight = 677
  ClientWidth = 509
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
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
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 345
    Top = 647
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnOKClick
    ExplicitLeft = 228
    ExplicitTop = 436
  end
  object btnCancel: TBitBtn
    Left = 426
    Top = 647
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 2
    ExplicitLeft = 309
    ExplicitTop = 436
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
    ExplicitWidth = 527
    ExplicitHeight = 718
    object Splitter: TSplitter
      Left = 0
      Top = 445
      Width = 493
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 441
      ExplicitWidth = 55
    end
    object vstTestCases: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 493
      Height = 445
      Align = alClient
      CheckImageKind = ckXP
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
      Header.Style = hsXPStyle
      Images = ilScopeImages
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      OnGetImageIndex = vstTestCasesGetImageIndex
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
      ExplicitTop = 496
      ExplicitWidth = 446
    end
  end
  object ilScopeImages: TImageList
    Left = 69
    Top = 379
  end
end
