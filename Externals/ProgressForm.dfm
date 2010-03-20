object frmProgress: TfrmProgress
  Left = 414
  Top = 426
  BorderStyle = bsToolWindow
  Caption = 'Progress'
  ClientHeight = 97
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPanel1: TPanel
    Left = 0
    Top = 38
    Width = 400
    Height = 26
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object prbProgressBar1: TProgressBar
      Left = 5
      Top = 5
      Width = 390
      Height = 16
      Align = alClient
      TabOrder = 0
    end
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 38
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
  end
  object pnlButton: TPanel
    Left = 0
    Top = 64
    Width = 400
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnCancel: TBitBtn
      Left = 166
      Top = 1
      Width = 75
      Height = 25
      Kind = bkCancel
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
end
