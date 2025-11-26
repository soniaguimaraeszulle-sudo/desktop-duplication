object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Servidor de Controle Remoto'
  ClientHeight = 500
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 57
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 26
      Height = 13
      Caption = 'Porta:'
    end
    object btnStart: TButton
      Left = 168
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Iniciar'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 249
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Parar'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopClick
    end
    object edtPort: TEdit
      Left = 48
      Top = 16
      Width = 97
      Height = 21
      TabOrder = 2
      Text = '9999'
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 57
    Width = 900
    Height = 424
    Align = alClient
    Columns = <>
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 481
    Width = 900
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 456
    Top = 216
  end
end
