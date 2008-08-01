object frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm
  Left = 161
  Top = 170
  ActiveControl = lvFileList
  Caption = 'frmBrowseAndDocItTestForm'
  ClientHeight = 763
  ClientWidth = 907
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 689
    Top = 43
    Height = 720
    ResizeStyle = rsUpdate
    ExplicitHeight = 690
  end
  object Panel1: TPanel
    Left = 273
    Top = 43
    Width = 416
    Height = 720
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 907
    Height = 43
    Align = alTop
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Tokens'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CheckBox1: TCheckBox
      Left = 96
      Top = 14
      Width = 153
      Height = 17
      Caption = 'Special Characters'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object btnQuit: TButton
      Left = 216
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Quit'
      TabOrder = 2
      OnClick = btnQuitClick
    end
    object btnOptions: TButton
      Left = 297
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Options'
      TabOrder = 3
      OnClick = btnOptionsClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 43
    Width = 273
    Height = 720
    Align = alLeft
    Caption = 'Panel3'
    TabOrder = 1
    DesignSize = (
      273
      720)
    object lvFileList: TListView
      Left = 8
      Top = 35
      Width = 259
      Height = 678
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'FileName'
          Width = 185
        end
        item
          Caption = 'Errors'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvFileListSelectItem
    end
    object edtDirectory: TEdit
      Left = 8
      Top = 8
      Width = 257
      Height = 21
      TabOrder = 1
      Text = 'edtDirectory'
      OnChange = edtDirectoryChange
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 345
    Top = 139
    object Undo1: TMenuItem
      Action = EditUndo1
    end
    object Cut1: TMenuItem
      Action = EditCut1
    end
    object Copy1: TMenuItem
      Action = EditCopy1
    end
    object Paste1: TMenuItem
      Action = EditPaste1
    end
    object Delete1: TMenuItem
      Action = EditDelete1
    end
    object SelectAll1: TMenuItem
      Action = EditSelectAll1
    end
  end
  object ActionList1: TActionList
    Left = 345
    Top = 195
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
  end
end
