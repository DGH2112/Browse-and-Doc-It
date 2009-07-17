object frmDUnit: TfrmDUnit
  Left = 0
  Top = 0
  ActiveControl = rdoExistingProject
  BorderIcons = []
  Caption = 'DUnit'
  ClientHeight = 466
  ClientWidth = 392
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
    392
    466)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaseClass: TLabel
    Left = 8
    Top = 385
    Width = 51
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Base Class'
    FocusControl = cbxBaseClass
  end
  object lblTestSuiteName: TLabel
    Left = 8
    Top = 412
    Width = 78
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Test Suite &Name'
    FocusControl = edtTestSuiteName
  end
  object gbxProject: TGroupBox
    Left = 8
    Top = 8
    Width = 376
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'DUnit Project'
    TabOrder = 0
    DesignSize = (
      376
      81)
    object rdoExistingProject: TRadioButton
      Left = 14
      Top = 51
      Width = 105
      Height = 17
      Caption = '&Existing Project:'
      TabOrder = 0
      OnClick = rdoNewExistingProject
    end
    object cbxExistingProject: TComboBox
      Left = 125
      Top = 49
      Width = 237
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = rdoNewExistingUnit
    end
    object rdoNewProject: TRadioButton
      Left = 14
      Top = 24
      Width = 105
      Height = 17
      Caption = '&New Project'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rdoNewExistingProject
    end
    object edtNewProjectName: TEdit
      Left = 125
      Top = 22
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object btnOK: TBitBtn
    Left = 228
    Top = 436
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 309
    Top = 436
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    TabOrder = 6
  end
  object gbxUnit: TGroupBox
    Left = 8
    Top = 95
    Width = 376
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'DUnit Unit'
    TabOrder = 1
    DesignSize = (
      376
      81)
    object rdoExistingUnit: TRadioButton
      Left = 14
      Top = 51
      Width = 105
      Height = 17
      Caption = 'E&xisting Unit:'
      TabOrder = 2
      OnClick = rdoNewExistingUnit
    end
    object cbxExistingUnit: TComboBox
      Left = 125
      Top = 49
      Width = 237
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 3
    end
    object rdoNewUnit: TRadioButton
      Left = 14
      Top = 24
      Width = 105
      Height = 17
      Caption = 'N&ew Unit'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rdoNewExistingUnit
    end
    object edtNewUnitName: TEdit
      Left = 125
      Top = 22
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object vstTestCases: TVirtualStringTree
    Left = 8
    Top = 182
    Width = 376
    Height = 194
    Anchors = [akLeft, akTop, akRight, akBottom]
    CheckImageKind = ckXP
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
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
    Top = 382
    Width = 292
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object edtTestSuiteName: TEdit
    Left = 92
    Top = 409
    Width = 292
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object ilScopeImages: TImageList
    Left = 45
    Top = 219
  end
end
