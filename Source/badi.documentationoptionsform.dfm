object frmDocumentationOptions: TfrmDocumentationOptions
  Left = 648
  Top = 534
  BorderStyle = bsDialog
  Caption = 'Documentation Options'
  ClientHeight = 409
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    350
    409)
  PixelsPerInch = 96
  TextHeight = 16
  object lblCSSComment: TLabel
    Left = 8
    Top = 320
    Width = 334
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'If you wish to change the layout and style of the HTML output, e' +
      'dit the 2 CSS files stored in the Styles\ directory of the Brows' +
      'eAndDocIt module installation directory. These are always copied' +
      ' to the destination directory for the target HTML documentation.'
    WordWrap = True
  end
  object rgpDocumentationOptions: TRadioGroup
    Left = 8
    Top = 8
    Width = 253
    Height = 154
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
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 267
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object gbxScopeOptions: TGroupBox
    Left = 8
    Top = 168
    Width = 253
    Height = 146
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
