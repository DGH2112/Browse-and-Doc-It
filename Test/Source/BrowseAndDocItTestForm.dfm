object frmBrowseAndDocItTestForm: TfrmBrowseAndDocItTestForm
  Left = 229
  Top = 180
  Caption = 'frmBrowseAndDocItTestForm'
  ClientHeight = 789
  ClientWidth = 1111
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 417
    Height = 789
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
  end
  object SynEdit1: TSynEdit
    Left = 417
    Top = 0
    Width = 694
    Height = 789
    Align = alClient
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
    Lines.Strings = (
      '')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoKeepCaretX, eoScrollPastEof, eoScrollPastEol, eoShowScrollHint, eoShowSpecialChars, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces]
    OnChange = SynEdit1Change
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
