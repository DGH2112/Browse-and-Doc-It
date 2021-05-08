object frmTokenForm: TfrmTokenForm
  Left = 374
  Top = 222
  Caption = 'Token Form'
  ClientHeight = 732
  ClientWidth = 890
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object lvListView1: TListView
    Left = 0
    Top = 0
    Width = 890
    Height = 732
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Columns = <
      item
        Width = 62
      end
      item
        Caption = 'Type'
        Width = 123
      end
      item
        Caption = 'Position'
        Width = 86
      end
      item
        Caption = 'Line'
        Width = 62
      end
      item
        Caption = 'Column'
        Width = 80
      end
      item
        Caption = 'Length'
        Width = 74
      end
      item
        Caption = 'Token'
        Width = 492
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Consolas'
    Font.Style = []
    HideSelection = False
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = lvListView1CustomDrawItem
  end
end
