object TBADIActiveXToolWndForm: TTBADIActiveXToolWndForm
  Left = 0
  Top = 0
  Width = 394
  Height = 330
  AxBorderStyle = afbNone
  Caption = 'TBADIActiveXToolWndForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = ActiveFormCreate
  TextHeight = 15
  object tmUpdateBounds: TTimer
    Enabled = False
    OnTimer = tmUpdateBoundsTimer
    Left = 56
    Top = 32
  end
end
