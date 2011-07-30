object frmCheckForUpdatesOptions: TfrmCheckForUpdatesOptions
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Check for Updates Options'
  ClientHeight = 158
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    329
    158)
  PixelsPerInch = 96
  TextHeight = 13
  object lblInterval: TLabel
    Left = 8
    Top = 97
    Width = 104
    Height = 13
    Caption = 'Check Interval (days)'
  end
  object btnOK: TBitBtn
    Left = 165
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkOK
    ParentDoubleBuffered = False
    TabOrder = 4
    ExplicitLeft = 274
    ExplicitTop = 263
  end
  object btnCancel: TBitBtn
    Left = 246
    Top = 125
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkCancel
    ParentDoubleBuffered = False
    TabOrder = 5
    ExplicitLeft = 355
    ExplicitTop = 263
  end
  object gbxLastUpdate: TGroupBox
    Left = 8
    Top = 8
    Width = 313
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Last Check Date'
    TabOrder = 0
    ExplicitWidth = 422
    DesignSize = (
      313
      57)
    object pnlLastUpdate: TPanel
      Left = 3
      Top = 16
      Width = 307
      Height = 38
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      Caption = 'Never checked.'
      TabOrder = 0
      ExplicitWidth = 416
    end
  end
  object chkEnabled: TCheckBox
    Left = 8
    Top = 71
    Width = 313
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Enable Check for Updates periodically on Application startup.'
    TabOrder = 1
    ExplicitWidth = 321
  end
  object edtInterval: TEdit
    Left = 214
    Top = 94
    Width = 85
    Height = 21
    Anchors = [akTop, akRight]
    ReadOnly = True
    TabOrder = 2
    Text = '0'
    ExplicitLeft = 268
  end
  object udInterval: TUpDown
    Left = 305
    Top = 94
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtInterval
    Min = 1
    Max = 365
    TabOrder = 3
    ExplicitLeft = 359
  end
end
