object FormRemoteView: TFormRemoteView
  Left = 0
  Top = 0
  Caption = 'Visualiza'#231#227'o Remota'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 57
    Align = alTop
    TabOrder = 0
    object btnStartView: TButton
      Left = 16
      Top = 16
      Width = 100
      Height = 25
      Caption = 'Iniciar Visualiza'#231#227'o'
      TabOrder = 0
      OnClick = btnStartViewClick
    end
    object btnStopView: TButton
      Left = 122
      Top = 16
      Width = 100
      Height = 25
      Caption = 'Parar Visualiza'#231#227'o'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopViewClick
    end
    object btnLockScreen: TButton
      Left = 240
      Top = 16
      Width = 100
      Height = 25
      Caption = 'Travar Tela'
      TabOrder = 2
      OnClick = btnLockScreenClick
    end
    object btnUnlockScreen: TButton
      Left = 346
      Top = 16
      Width = 100
      Height = 25
      Caption = 'Destravar Tela'
      TabOrder = 3
      OnClick = btnUnlockScreenClick
    end
    object Label1: TLabel
      Left = 468
      Top = 21
      Width = 41
      Height = 13
      Caption = 'Monitor:'
    end
    object ToggleSwitch1: TToggleSwitch
      Left = 515
      Top = 18
      Width = 100
      Height = 20
      StateCaptions.CaptionOn = 'Monitor 2'
      StateCaptions.CaptionOff = 'Monitor 1'
      TabOrder = 4
      OnClick = ToggleSwitch1Click
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 57
    Width = 800
    Height = 524
    Align = alClient
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 796
      Height = 520
      Align = alClient
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Timer1: TTimer
    Enabled = False
    Left = 456
    Top = 224
  end
end
