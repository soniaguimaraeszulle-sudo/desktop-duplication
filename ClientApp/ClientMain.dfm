object FormClientMain: TFormClientMain
  Left = 0
  Top = 0
  Caption = 'Cliente de Controle Remoto'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 89
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 49
      Height = 13
      Caption = 'Servidor:'
    end
    object Label2: TLabel
      Left = 232
      Top = 19
      Width = 26
      Height = 13
      Caption = 'Porta:'
    end
    object edtServerIP: TEdit
      Left = 71
      Top = 16
      Width = 146
      Height = 21
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object edtServerPort: TEdit
      Left = 264
      Top = 16
      Width = 89
      Height = 21
      TabOrder = 1
      Text = '9999'
    end
    object btnConnect: TButton
      Left = 16
      Top = 51
      Width = 75
      Height = 25
      Caption = 'Conectar'
      TabOrder = 2
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 97
      Top = 51
      Width = 75
      Height = 25
      Caption = 'Desconectar'
      Enabled = False
      TabOrder = 3
      OnClick = btnDisconnectClick
    end
    object chkAutoStart: TCheckBox
      Left = 192
      Top = 55
      Width = 97
      Height = 17
      Caption = 'Iniciar autom'#225'tico'
      TabOrder = 4
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 89
    Width = 600
    Height = 292
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 381
    Width = 600
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
