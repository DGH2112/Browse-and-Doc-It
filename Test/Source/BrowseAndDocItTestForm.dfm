object frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm
  Left = 161
  Top = 170
  Caption = 'frmBrowseAndDocItTestForm'
  ClientHeight = 733
  ClientWidth = 907
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
    ExplicitHeight = 746
  end
  object Panel1: TPanel
    Left = 273
    Top = 43
    Width = 416
    Height = 690
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitHeight = 745
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
    ExplicitWidth = 419
    ExplicitHeight = 745
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 907
    Height = 43
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 1111
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
    ExplicitHeight = 745
    DesignSize = (
      273
      690)
    object DirectoryListBox1: TDirectoryListBox
      Left = 8
      Top = 31
      Width = 259
      Height = 259
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      TabOrder = 0
      OnChange = DirectoryListBox1Change
    end
    object DriveComboBox1: TDriveComboBox
      Left = 8
      Top = 6
      Width = 259
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      DirList = DirectoryListBox1
      TabOrder = 1
    end
    object lvFileList: TListView
      Left = 8
      Top = 296
      Width = 259
      Height = 385
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Direc'
          Width = 200
        end
        item
          Caption = 'Errors'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 2
      ViewStyle = vsReport
      OnClick = FileListClick
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
end
