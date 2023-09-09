object frmMyTestForm: TfrmMyTestForm
  Left = 0
  Top = 0
  Caption = 'frmMyTestForm'
  ClientHeight = 308
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object lblHello: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 423
    Height = 302
    Align = alClient
    Alignment = taCenter
    Caption = 'Hello Dave'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 152
    ExplicitHeight = 39
  end
end
