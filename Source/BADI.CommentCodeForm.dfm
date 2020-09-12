object frmCommentCode: TfrmCommentCode
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Comment Code'
  ClientHeight = 105
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object pnlFudgePanel: TPanel
    Left = 0
    Top = 0
    Width = 272
    Height = 105
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 80
    ExplicitWidth = 313
    ExplicitHeight = 153
    DesignSize = (
      272
      105)
    object lblCommentStyle: TLabel
      Left = 8
      Top = 41
      Width = 73
      Height = 16
      AutoSize = False
      Caption = '&Style'
      FocusControl = cbxTagName
    end
    object lblTagName: TLabel
      Left = 8
      Top = 11
      Width = 73
      Height = 16
      AutoSize = False
      Caption = '&Tag Name'
      FocusControl = cbxTagName
    end
    object cbxCommentType: TComboBox
      Left = 87
      Top = 38
      Width = 176
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 250
    end
    object cbxTagName: TComboBox
      Left = 87
      Top = 8
      Width = 176
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      Sorted = True
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 107
      Top = 72
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      ExplicitTop = 172
    end
    object btnCancel: TBitBtn
      Left = 188
      Top = 72
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
      ExplicitTop = 172
    end
  end
end
