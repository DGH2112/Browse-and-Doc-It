object frmCheckForUpdates: TfrmCheckForUpdates
  Left = 0
  Top = 0
  ActiveControl = lbInformation
  BorderStyle = bsToolWindow
  Caption = 'Checking for Software Updates...'
  ClientHeight = 171
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    544
    171)
  PixelsPerInch = 96
  TextHeight = 13
  object lblWebSite: TLabel
    Left = 8
    Top = 143
    Width = 50
    Height = 13
    Cursor = crHandPoint
    Hint = 'Click to open this website...'
    Caption = 'lblWebSite'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = lblWebSiteClick
    OnMouseEnter = lblWebSiteMouseEnter
    OnMouseLeave = lblWebSiteMouseLeave
  end
  object lbInformation: TListBox
    Left = 8
    Top = 8
    Width = 528
    Height = 124
    Style = lbOwnerDrawVariable
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 0
    OnDrawItem = lbInformationDrawItem
    OnMeasureItem = lbInformationMeasureItem
  end
  object btnOK: TBitBtn
    Left = 461
    Top = 138
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    Enabled = False
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnOKClick
  end
  object tmFinish: TTimer
    Enabled = False
    OnTimer = tmFinishTimer
    Left = 40
    Top = 40
  end
end
