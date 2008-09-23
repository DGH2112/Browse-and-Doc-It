object frmProgress: TfrmProgress
  Left = 414
  Top = 426
  BorderStyle = bsToolWindow
  Caption = 'Progress'
  ClientHeight = 66
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
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPanel1: TPanel
    Left = 0
    Top = 40
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
  object pnlMsg: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 40
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
