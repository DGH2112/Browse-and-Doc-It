object fmBADIParentFrame: TfmBADIParentFrame
  Left = 0
  Top = 0
  Width = 436
  Height = 555
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object lblBADI: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 430
    Height = 29
    Align = alTop
    Caption = 'Browse and Doc It'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    ExplicitWidth = 221
  end
  object lblAuthor: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 60
    Width = 430
    Height = 16
    Align = alTop
    Caption = 'Author: David Hoyle (c)  2019 GNU GPL 3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 233
  end
  object lblBuild: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 38
    Width = 430
    Height = 16
    Align = alTop
    Caption = 'lblBuild'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 40
  end
  object lblPleaseSelect: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 478
    Width = 430
    Height = 16
    Align = alBottom
    Caption = 'Please select a sub-options category...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 220
  end
  object lblEurekaLog: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 500
    Width = 430
    Height = 52
    Align = alBottom
    AutoSize = False
    Caption = 'EurekaLog'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlBottom
    ExplicitTop = 552
    ExplicitWidth = 378
  end
  object lblBuildDate: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 82
    Width = 430
    Height = 16
    Align = alTop
    Caption = 'lblBuildDate'
    ExplicitWidth = 66
  end
  object lblInformation: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 104
    Width = 430
    Height = 368
    Align = alClient
    Lines.Strings = (
      
        'Browse and Doc It is a RAD Studio plug-in for browsing, checking' +
        ' and '
      'documenting your code.'
      ''
      'Copyright (C) 2019  David Hoyle '
      '(https://github.com/DGH2112/Browse-and-Doc-It/)'
      ''
      
        'This program is free software: you can redistribute it and/or mo' +
        'dify it '
      
        'under the terms of the GNU General Public License as published b' +
        'y '
      
        'the Free Software Foundation, either version 3 of the License, o' +
        'r (at '
      'your option) any later version.'
      ''
      
        'This program is distributed in the hope that it will be useful, ' +
        'but '
      'WITHOUT ANY WARRANTY; without even the implied warranty of '
      'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See '
      'the GNU General Public License for more details.'
      ''
      
        'You should have received a copy of the GNU General Public Licens' +
        'e '
      'along with this program.  If not, see '
      '<https://www.gnu.org/licenses/>.')
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
