object frmDocumentationOptions: TfrmDocumentationOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Documentation Options'
  ClientHeight = 283
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    350
    283)
  PixelsPerInch = 96
  TextHeight = 13
  object rgpDocumentationOptions: TRadioGroup
    Left = 8
    Top = 8
    Width = 253
    Height = 118
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Documentation Options'
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 267
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 267
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 3
    Kind = bkCancel
  end
  object gbxScopeOptions: TGroupBox
    Left = 8
    Top = 132
    Width = 253
    Height = 143
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Scope Options'
    TabOrder = 1
    object chkLocal: TCheckBox
      Left = 14
      Top = 22
      Width = 97
      Height = 17
      Caption = '&Local'
      TabOrder = 0
    end
    object chkPrivate: TCheckBox
      Left = 14
      Top = 45
      Width = 97
      Height = 17
      Caption = 'Private'
      TabOrder = 1
    end
    object chkProtected: TCheckBox
      Left = 14
      Top = 68
      Width = 97
      Height = 17
      Caption = 'P&rotected'
      TabOrder = 2
    end
    object chkPublic: TCheckBox
      Left = 14
      Top = 91
      Width = 97
      Height = 17
      Caption = 'P&ublic'
      TabOrder = 3
    end
    object chkPublished: TCheckBox
      Left = 14
      Top = 114
      Width = 97
      Height = 17
      Caption = 'Pu&blished'
      TabOrder = 4
    end
  end
end
