object frmTokenForm: TfrmTokenForm
  Left = 374
  Top = 222
  Caption = 'Token Form'
  ClientHeight = 595
  ClientWidth = 723
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvListView1: TListView
    Left = 0
    Top = 0
    Width = 723
    Height = 595
    Align = alClient
    Columns = <
      item
      end
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Position'
        Width = 70
      end
      item
        Caption = 'Line'
      end
      item
        Caption = 'Column'
        Width = 65
      end
      item
        Caption = 'Length'
        Width = 60
      end
      item
        Caption = 'Token'
        Width = 400
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = lvListView1CustomDrawItem
  end
end
