object frmDUnit: TfrmDUnit
  Left = 0
  Top = 0
  ActiveControl = rdoExistingProject
  BorderIcons = [biSystemMenu]
  Caption = 'DUnit'
  ClientHeight = 565
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 600
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
    384
    565)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaseClass: TLabel
    Left = 8
    Top = 406
    Width = 51
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Base Class'
    FocusControl = cbxBaseClass
  end
  object lblTestSuiteName: TLabel
    Left = 8
    Top = 433
    Width = 78
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Test Suite &Name'
    FocusControl = edtTestSuiteName
  end
  object gbxProject: TGroupBox
    Left = 8
    Top = 8
    Width = 368
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'DUnit Project'
    TabOrder = 0
    DesignSize = (
      368
      81)
    object rdoExistingProject: TRadioButton
      Left = 14
      Top = 51
      Width = 105
      Height = 17
      Caption = '&Existing Project:'
      TabOrder = 3
      OnClick = rdoNewExistingProject
    end
    object cbxExistingProject: TComboBox
      Left = 125
      Top = 49
      Width = 229
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = rdoNewExistingUnit
    end
    object rdoNewProject: TRadioButton
      Left = 14
      Top = 24
      Width = 105
      Height = 17
      Caption = '&New Project'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rdoNewExistingProject
    end
    object edtNewProjectName: TEdit
      Left = 125
      Top = 22
      Width = 229
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object btnOK: TBitBtn
    Left = 220
    Top = 532
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 301
    Top = 532
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 6
  end
  object gbxUnit: TGroupBox
    Left = 8
    Top = 95
    Width = 368
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'DUnit Unit'
    TabOrder = 1
    DesignSize = (
      368
      81)
    object rdoExistingUnit: TRadioButton
      Left = 14
      Top = 51
      Width = 105
      Height = 17
      Caption = 'E&xisting Unit:'
      TabOrder = 3
      OnClick = rdoNewExistingUnit
    end
    object cbxExistingUnit: TComboBox
      Left = 125
      Top = 49
      Width = 229
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = cbxExistingUnitChange
    end
    object rdoNewUnit: TRadioButton
      Left = 14
      Top = 24
      Width = 105
      Height = 17
      Caption = 'New &Unit'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rdoNewExistingUnit
    end
    object edtNewUnitName: TEdit
      Left = 125
      Top = 22
      Width = 229
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object vstTestCases: TVirtualStringTree
    Left = 8
    Top = 182
    Width = 368
    Height = 215
    Anchors = [akLeft, akTop, akRight, akBottom]
    CheckImageKind = ckXP
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 17
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = ilScopeImages
    TabOrder = 2
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    OnGetImageIndex = vstTestCasesGetImageIndex
    Columns = <>
  end
  object cbxBaseClass: TComboBox
    Left = 92
    Top = 403
    Width = 284
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
  object edtTestSuiteName: TEdit
    Left = 92
    Top = 430
    Width = 284
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object chkRemoveIAndTFromObject: TCheckBox
    Left = 8
    Top = 457
    Width = 368
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&Remove Object First Letter'
    TabOrder = 7
  end
  object gpNameOptions: TGridPanel
    Left = 8
    Top = 480
    Width = 368
    Height = 46
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblClassName
        Row = 0
      end
      item
        Column = 1
        Control = lblMethodName
        Row = 0
      end
      item
        Column = 0
        Control = edtClassName
        Row = 1
      end
      item
        Column = 1
        Control = edtMethodName
        Row = 1
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 21.000000000000000000
      end>
    TabOrder = 8
    object lblClassName: TLabel
      Left = 0
      Top = 0
      Width = 82
      Height = 13
      Align = alClient
      Caption = '&Class Name Mask'
      FocusControl = edtClassName
    end
    object lblMethodName: TLabel
      AlignWithMargins = True
      Left = 187
      Top = 0
      Width = 93
      Height = 13
      Margins.Top = 0
      Margins.Right = 0
      Align = alClient
      Caption = 'Method Name &Mask'
      FocusControl = edtMethodName
    end
    object edtClassName: TEdit
      Left = 0
      Top = 25
      Width = 184
      Height = 21
      Align = alClient
      TabOrder = 0
      Text = 'edtClassName'
    end
    object edtMethodName: TEdit
      AlignWithMargins = True
      Left = 187
      Top = 25
      Width = 181
      Height = 21
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 1
      Text = 'edtMethodName'
    end
  end
  object ilScopeImages: TImageList
    Left = 45
    Top = 219
  end
end
