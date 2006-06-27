object frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm
  Left = 161
  Top = 170
  Width = 915
  Height = 766
  Caption = 'frmBrowseAndDocItTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 689
    Top = 43
    Height = 690
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 273
    Top = 43
    Width = 416
    Height = 690
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
  end
  object SynEdit1: TSynEdit
    Left = 692
    Top = 43
    Width = 215
    Height = 690
    Align = alClient
    ActiveLineColor = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = PopupMenu1
    TabOrder = 1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn1
    InsertCaret = ctHalfBlock
    Lines.Strings = (
      '')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoRightMouseMovesCursor, eoScrollPastEof, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    OnChange = SynEdit1Change
    OnStatusChange = SynEdit1StatusChange
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
  end
  object Panel3: TPanel
    Left = 0
    Top = 43
    Width = 273
    Height = 690
    Align = alLeft
    Caption = 'Panel3'
    TabOrder = 3
    DesignSize = (
      273
      690)
    object lvFileList: TListView
      Left = 8
      Top = 40
      Width = 259
      Height = 641
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
      OnClick = FileListClick
    end
    object edtDirectory: TEdit
      Left = 8
      Top = 8
      Width = 233
      Height = 21
      TabOrder = 1
      Text = 'edtDirectory'
    end
    object btnDirectory: TButton
      Left = 240
      Top = 8
      Width = 27
      Height = 25
      Caption = '...'
      TabOrder = 2
      OnClick = btnDirectoryClick
    end
  end
  object SynPasSyn1: TSynPasSyn
    AsmAttri.Foreground = clMaroon
    CommentAttri.Foreground = clPurple
    DirectiveAttri.Foreground = clPurple
    DirectiveAttri.Style = []
    IdentifierAttri.Foreground = clNavy
    NumberAttri.Foreground = clGreen
    NumberAttri.Style = [fsBold]
    FloatAttri.Foreground = clGreen
    FloatAttri.Style = [fsBold]
    HexAttri.Foreground = clGreen
    HexAttri.Style = [fsBold]
    StringAttri.Foreground = clTeal
    StringAttri.Style = [fsBold]
    CharAttri.Foreground = clTeal
    CharAttri.Style = [fsBold]
    SymbolAttri.Foreground = clGreen
    SymbolAttri.Style = [fsBold]
    Left = 504
    Top = 144
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
