object FrmMain: TFrmMain
  Left = 305
  Top = 140
  Width = 1142
  Height = 656
  Caption = 'FrmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainGrid: TcxGrid
    Left = 8
    Top = 104
    Width = 1105
    Height = 473
    TabOrder = 0
  end
  object Button1: TButton
    Left = 40
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 360
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
  end
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security In' +
      'fo=False;Initial Catalog=lt00;Data Source=.'
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 96
    Top = 24
  end
  object Qry: TADOQuery
    Connection = ADOConnection1
    Parameters = <>
    SQL.Strings = (
      'SELECT ClassID, Data FROM SCMGridviews')
    Left = 256
    Top = 24
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = MainGrid
    PopupMenus = <>
    Left = 504
    Top = 56
  end
end
