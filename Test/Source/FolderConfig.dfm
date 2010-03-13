object frmFolders: TfrmFolders
  Left = 0
  Top = 0
  Caption = 'Folder Configuration'
  ClientHeight = 386
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    651
    386)
  PixelsPerInch = 96
  TextHeight = 13
  object lvFolders: TListView
    Left = 8
    Top = 8
    Width = 635
    Height = 339
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Folder'
        Width = 250
      end>
    HideSelection = False
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawSubItem = lvFoldersCustomDrawSubItem
    OnMouseDown = lvFoldersMouseDown
  end
  object btnOK: TBitBtn
    Left = 487
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    TabOrder = 1
  end
  object btnCancel: TBitBtn
    Left = 568
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    TabOrder = 2
  end
  object btnAdd: TBitBtn
    Left = 8
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnDelete: TBitBtn
    Left = 89
    Top = 353
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
end
