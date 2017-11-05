object frameModuleExplorer: TframeModuleExplorer
  Left = 0
  Top = 0
  Width = 354
  Height = 413
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentDoubleBuffered = False
  ParentFont = False
  ParentShowHint = False
  ShowHint = False
  TabOrder = 0
  TabStop = True
  OnEnter = FrameEnter
  object stbStatusBar: TStatusBar
    Left = 0
    Top = 394
    Width = 354
    Height = 19
    Panels = <
      item
        Width = 65
      end
      item
        Width = 65
      end
      item
        Width = 65
      end
      item
        Width = 50
      end>
    ParentShowHint = False
    ShowHint = True
  end
  object tbrExplorerScope: TToolBar
    Left = 0
    Top = 0
    Width = 354
    Height = 22
    AutoSize = True
    ButtonWidth = 19
    Images = ilToolbar
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = actLocal
    end
    object ToolButton2: TToolButton
      Left = 19
      Top = 0
      Action = actPrivate
    end
    object ToolButton3: TToolButton
      Left = 38
      Top = 0
      Action = actProtected
    end
    object ToolButton4: TToolButton
      Left = 57
      Top = 0
      Action = actPublic
    end
    object ToolButton5: TToolButton
      Left = 76
      Top = 0
      Action = actPublished
    end
    object tbtnSeparator: TToolButton
      Left = 95
      Top = 0
      Width = 8
      Caption = 'tbtnSeparator'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object tbtnSyntaxHighlight: TToolButton
      Left = 103
      Top = 0
      Action = actSyntax
    end
    object tbtnShowHints: TToolButton
      Left = 122
      Top = 0
      Action = actShowHints
    end
    object tbtnSep2: TToolButton
      Left = 141
      Top = 0
      Width = 8
      Caption = 'tbtnSep2'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbtnErrors: TToolButton
      Left = 149
      Top = 0
      Action = actErrors
    end
    object tbtnWarnings: TToolButton
      Left = 168
      Top = 0
      Action = actWarnings
    end
    object tbtnHints: TToolButton
      Left = 187
      Top = 0
      Action = actHints
    end
    object tbtnConflicts: TToolButton
      Left = 206
      Top = 0
      Action = actConflicts
    end
    object btnChecksAndMetrics: TToolButton
      Left = 225
      Top = 0
      Action = actChecksAndMetrics
    end
    object tbtnSep3: TToolButton
      Left = 244
      Top = 0
      Width = 8
      Caption = 'tbtnSep3'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbtnMethods: TToolButton
      Left = 252
      Top = 0
      Action = actMethods
    end
    object tbtnProperties: TToolButton
      Left = 271
      Top = 0
      Action = actProperties
    end
    object tbtnConstants: TToolButton
      Left = 290
      Top = 0
      Action = actConstants
    end
    object tbtnVariables: TToolButton
      Left = 309
      Top = 0
      Action = actVariables
    end
    object tbtnTypes: TToolButton
      Left = 328
      Top = 0
      Action = actTypes
    end
  end
  object edtExplorerFilter: TEdit
    Left = 0
    Top = 22
    Width = 354
    Height = 24
    Align = alTop
    TabOrder = 1
    OnChange = edtExplorerFilterChange
    OnKeyPress = edtExplorerFilterKeyPress
    OnMouseActivate = edtExplorerFilterMouseActivate
  end
  object ilScopeImages: TImageList
    Left = 29
    Top = 75
  end
  object ilToolbar: TImageList
    Height = 12
    Width = 12
    Left = 31
    Top = 124
    Bitmap = {
      494C01011100130058010C000C00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000003C0000000100200000000000002D
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000737A7D00C0C0
      C000737A7D00737A7D00C0C0C000737A7D00737A7D00737A7D00737A7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000737A
      7D00C0C0C000737A7D00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000737A7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000737A7D00737A7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000737A7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000737A
      7D00C0C0C000737A7D00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000737A7D00737A
      7D00737A7D00737A7D00C0C0C000737A7D00737A7D00737A7D00737A7D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000737A7D00C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000808080008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF008080800000000000000000000000000000000000000000000000
      000000000000000000000000000000FF000000FF000000FF000000FF00008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000FF00FF00FF00FF0080808000000000000000000000000000000000000000
      0000000000000000000000000000000000008080400080804000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF008080800000000000000000000000000000000000000000000000
      0000000000000000000000FF000000FF000000FF000000FF000000FF000000FF
      0000808080008080800000000000000000000000000000000000000000000000
      0000FF00FF00FF00FF0080808000808080000000000000000000000000000000
      0000000000000000000000000000000000008080400080804000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF008080800080808000808080008080800000000000000000000000
      00000000000000FF000000FF000000FF0000808080000000000000FF000000FF
      000000FF0000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00808080000000000000000000000000000000
      0000000000000000000000000000000000008080400080804000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF008080800080808000000000000000
      00000000000000FF000000FF0000808080000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00808080008080800000000000000000000000
      0000000000000000000000000000000000008080400080804000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0080808000000000000000
      00000000000000FF000000FF0000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF008080800000000000FF00FF00FF00FF008080800000000000000000000000
      0000000000000000000000000000000000008080400080804000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF00808080000000FF000000FF000000FF0080808000000000000000
      00000000000000FF000000FF0000808080000000000000000000000000008080
      8000808080008080800000000000000000000000000000000000FF00FF00FF00
      FF000000000000000000FF00FF00FF00FF008080800080808000000000000000
      0000000000000000000000000000000000008080400080804000808080000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF00808080000000FF000000FF000000FF0080808000000000000000
      0000000000000000000000FF000000FF0000808080008080800000FF000000FF
      000000FF000000000000000000000000000000000000FF00FF00FF00FF008080
      8000000000000000000000000000FF00FF00FF00FF0080808000000000000000
      0000000000000000000000000000000000008080400080804000808080008080
      80008080800080808000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      000000000000000000000000000000FF000000FF000000FF000000FF000000FF
      00000000000000000000000000000000000000000000FF00FF00FF00FF000000
      0000000000000000000000000000FF00FF00FF00FF0000000000000000000000
      0000000000008080400080804000808040008080400080804000808040008080
      40008080400080808000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      00000000000000000000000000000000000000FF000000FF000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080400080804000808040008080400080804000808040008080
      4000808040000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008A768A008A768A000000000000000000000000000000
      0000000000008A768A008A768A00000000000000000000000000000000000000
      0000000000000000800000008000000080000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000730D0000730D0000730D000000000000000000000000000000
      000000000000FF010500FF0105008A768A0000000000FF0000008A758A00817F
      8100FF010500FF0105008A768A00000000000000000000000000000000000000
      00000000FF000000FF00FFFFFF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000E6FF0000E6FF00FF19190000E6
      FF0000E6FF000000000000000000000000000000000000000000000000000000
      000000FD190000FD19001902E80000FD190000FD190000000000000000000000
      000000000000FF010500FF0000008A768A0084080000FF000000840900008A76
      8A00FF000000FF0000008A768A00000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF0000008000000000000000
      000000000000000000000000000000E6FF0000E6FF0000E6FF0000E6FF0000E6
      FF0000E6FF0000008000000000000000000000000000000000000000000000FD
      190000FD190000FD190000FD190000FD190000FD190000730D00000000000000
      000000000000FF000000FF0000008A768A00FD020000FF000000FD0200008A76
      8A00FF000000FF0000008A768A00000000000000000000000000000000000000
      FF000000FF000000FF00FFFFFF000000FF000000FF0000008000000000000000
      000000000000000000000000000000E6FF0000E6FF0000E6FF00FF19190000E6
      FF0000E6FF0000008000000000000000000000000000000000000000000000FD
      190000FD190000FD19001902E80000FD190000FD190000730D00000000000000
      000000000000FF000000FF0000008A758A00FF000000FC030000FF0000008A75
      8A00FF000000FF0105008A768A00000000000000000000000000000000000000
      FF000000FF000000FF00FFFFFF000000FF000000FF0000008000000000000000
      000000000000000000000000000000E6FF0000E6FF0000E6FF00FF19190000E6
      FF0000E6FF0000008000000000000000000000000000000000000000000000FD
      190000FD190000FD19001902E80000FD190000FD190000730D00000000000000
      000000000000FF000000FF0000008A758A00FF0000008A758A00FF0000008A75
      8A00FF000000FF0105008A768A00000000000000000000000000000000000000
      00000000FF000000FF00FFFFFF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000E6FF0000E6FF00FF19190000E6
      FF0000E6FF000000000000000000000000000000000000000000000000000000
      000000FD190000FD19001902E80000FD190000FD190000000000000000000000
      000000000000FF010500FF00000086070000FD0300008A768A00FD0200008507
      0000FF000000FF0105008A768A00000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000E6FF0000E6FF0000E6
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000FD190000FD190000FD19000000000000000000000000000000
      000000000000FF010500FF000000FE010000830900000000000083090000FE01
      0000FF000000FF0105008A768A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF010500FF000000FF0000008A758A000000000000000000FF00
      0000FF000000FF0105008A768A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF010500FF000000FF000000000000000000000000000000FF00
      0000FF000000FF01050000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001973190019731900197319001973190019738C001973
      8C0019738C0019738C0019738C00000000000000000000000000738C7300738C
      7300738C7300738C7300738C7300738C7300738C7300738C7300738C7300738C
      7300000000000000000000000000000000000000000000030000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008080000000800000000000000000000000000000000000000000000000
      00000000000019E6190019E6190019E6190019E619001973190019E6FF0019E6
      FF0019E6FF0019E6FF0019738C00000000000000000000190000001900000019
      0000001900000019000000190000001900000019000000190000001900000019
      0000000000000000000000000000F9FFF900FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008080000000800000000000000000000000000000000000000000000000
      00000000000019E6190019E6190019E6190019E619001973190019E6FF0019E6
      FF0019E6FF0019E6FF0019738C0000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000C0C0C0000000000000000000000000000000000000000000000000000000
      0000008080000000800000000000000000000000000000000000000000000000
      00000000000019E6190019E6190019E6190019E619001973190019E6FF0019E6
      FF0019E6FF0019E6FF0019738C0000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000019E6190019E6190019E6190019E619001973190019E6FF0019E6
      FF0019E6FF0019E6FF0019738C0000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000FFFF000080800000000000000000000000000000000000000000000000
      00000000000002008C0002008C0002008C0002008C0002008C008C0019008C00
      19008C0019008C0019008C00190000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000FFFF000080800000000000000000000000000000000000000000000000
      0000000000001900FF001900FF001900FF001900FF0002008C00FF001900FF00
      1900FF001900FF0019008C00190000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000037E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000008080000080800000000000000000000000000000808000008080000000
      0000000000001900FF001900FF001900FF001900FF0002008C00FF001900FF00
      1900FF001900FF0019008C00190000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000000000000080000000800000008000000080000000
      8000000080000000800000000000000000000000000000FFFF00008080000080
      8000008080000080800000808000008080000080800000808000000000000000
      0000000000001900FF001900FF001900FF001900FF0002008C00FF001900FF00
      1900FF001900FF0019008C00190000000000000000000019000000FFE60000FF
      E60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FFE60000FF
      E600000000000000000000000000737A7D000000800000008000000080000000
      800000008000737A7D000000000000000000000000000000000000FFFF000080
      8000008080000080800000808000008080000080800000000000000000000000
      0000000000001900FF001900FF001900FF001900FF0002008C00FF001900FF00
      1900FF001900FF0019008C000200000000000000000000190000001900000019
      0000001900000019000000190000001900000019000000190000001900000019
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000080800000808000008080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000080800000808000008080000000000000000000000000000000
      0000000000000000000000000000000000000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      00000000800000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080800000000000000000000080
      8000008080000000000000000000000000000000000000000000000000000000
      00000000800000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080800000000000000000000080
      8000008080000000000000000000000000000000000000000000000000000000
      00000000800000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000080008000800080000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000080800000000000000000000080
      8000008080000000000000000000000000000000000000000000000000000000
      00000000800000FFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF008000
      8000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      800000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF008000
      8000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000000000000000000000000000000000000000000000800000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF000000000000FFFF00000000000000000000FF
      FF000000000000000000000000000000000000000000000000000000800000FF
      FF0000FFFF00000080000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000080
      8000008080000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      800000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000000000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000080000000800000008000000080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000300000003C0000000100010000000000E00100000000000000000000
      000000000000000000000000FFFFFF00FFF0000000000000C010000000000000
      C010000000000000C010000000000000C010000000000000C010000000000000
      C010000000000000C010000000000000C010000000000000C010000000000000
      C010000000000000FFF0000000000000FFFFFFFFFFFF0000E3FF0FF9FF9F0000
      C3FE07F1FF1F0000C3FC03F0FF1F0000C07847E0FF1F0000C038FFE07F1F0000
      C038FFC47F1F0000C038E3CC3F1F0000C03C078E3F030000C07E0F9E78030000
      C0FF1FFFF8070000FFFFFFFFFFFF0000FFFFFFFFFFFF0000FFFFFFFFFCF90000
      F8FF8FF8F8810000F07F07F078010000E03E03E038010000E03E03E038010000
      E03E03E038010000F07F07F078010000F8FF8FF8F8410000FFFFFFFFF8610000
      FFFFFFFFF8E30000FFFFFFFFFFFF0000FFFFFFFFFFFF0000F1FC01C00F8F0000
      F1F801800E070000F1F801800E030000F1F801800C010000F1F801800C010000
      F1F801800C010000F19801800C010000801801800E030000803801800E030000
      C07801800FFF0000FFFFFFFFFFFF0000807FFFFFFFFF0000807F03F07FFF0000
      807F03F07FFF000087FF03F0FFFF000087FF03F07F9F000087FF03F0FF0F0000
      87FE03E07F0F000087FC03C03F9F000087FC07C03FFF000087FE07E07FFF0000
      87FF0FF0FFFF0000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object alToolbar: TActionList
    Images = ilToolbar
    Left = 33
    Top = 173
    object actLocal: TAction
      Category = 'Scope'
      Caption = 'actLocal'
      Hint = 'Show Local Elements'
      ImageIndex = 0
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actPrivate: TAction
      Category = 'Scope'
      Caption = 'actPrivate'
      Hint = 'Show Private Elements'
      ImageIndex = 1
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actProtected: TAction
      Category = 'Scope'
      Caption = 'actProtected'
      Hint = 'Show Protected Elements'
      ImageIndex = 2
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actPublic: TAction
      Category = 'Scope'
      Caption = 'actPublic'
      Hint = 'Show Public Elements'
      ImageIndex = 3
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actPublished: TAction
      Category = 'Scope'
      Caption = 'actPublished'
      Hint = 'Show Published Elements'
      ImageIndex = 4
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actSyntax: TAction
      Category = 'Options'
      Caption = '&Syntax'
      Hint = 'Show Syntax Highlighted Treeview'
      ImageIndex = 5
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actShowHints: TAction
      Category = 'Options'
      Caption = 'actShowHints'
      Hint = 'Show Comments in Hints'
      ImageIndex = 6
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actErrors: TAction
      Category = 'Options'
      Caption = 'actErrors'
      Hint = 'Show Errors'
      ImageIndex = 8
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actWarnings: TAction
      Category = 'Options'
      Caption = 'actWarnings'
      Hint = 'Show Warnings'
      ImageIndex = 9
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actHints: TAction
      Category = 'Options'
      Caption = 'actHints'
      Hint = 'Show Hints'
      ImageIndex = 10
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actConflicts: TAction
      Category = 'Options'
      Caption = 'actConflicts'
      Hint = 'Show Documentation Conflicts'
      ImageIndex = 7
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actChecksAndMetrics: TAction
      Category = 'Options'
      Caption = 'Show Checks snd Metrics'
      ImageIndex = 16
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actMethods: TAction
      Category = 'Options'
      Caption = 'actMethods'
      Hint = 'Show Method Documentation Conflicts'
      ImageIndex = 11
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actProperties: TAction
      Category = 'Options'
      Caption = 'actProperties'
      Hint = 'Show Property Documentation Conflicts'
      ImageIndex = 12
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actConstants: TAction
      Category = 'Options'
      Caption = 'actConstants'
      Hint = 'Show Constant Documentation Conflicts'
      ImageIndex = 13
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actVariables: TAction
      Category = 'Options'
      Caption = 'actVariables'
      Hint = 'Show Variable Documentation Conflicts'
      ImageIndex = 14
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
    object actTypes: TAction
      Category = 'Options'
      Caption = 'actTypes'
      Hint = 'Show Types Documentation Conflicts'
      ImageIndex = 15
      OnExecute = actLocalExecute
      OnUpdate = actLocalUpdate
    end
  end
end
