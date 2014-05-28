object frmSupportDataMain: TfrmSupportDataMain
  Left = 0
  Top = 0
  Caption = 'Collect data for Zinsser Support'
  ClientHeight = 352
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Label3: TLabel
    Left = 40
    Top = 47
    Width = 109
    Height = 15
    Caption = 'When did it happen?'
  end
  object Button1: TButton
    Left = 352
    Top = 100
    Width = 138
    Height = 25
    Caption = 'Collect Data'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 496
    Top = 100
    Width = 160
    Height = 25
    Caption = 'Collect Data with mail'
    TabOrder = 4
    OnClick = Button2Click
  end
  object DateTimePicker1: TDateTimePicker
    Left = 168
    Top = 47
    Width = 186
    Height = 23
    Date = 41211.636185578700000000
    Time = 41211.636185578700000000
    TabOrder = 1
  end
  object RadioButton1: TRadioButton
    Left = 25
    Top = 24
    Width = 256
    Height = 17
    Caption = 'Collect data of a today or some day before:'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 25
    Top = 84
    Width = 113
    Height = 17
    Caption = 'Collect All'
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 0
    Top = 154
    Width = 685
    Height = 198
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 352
    Top = 131
    Width = 201
    Height = 17
    TabOrder = 6
  end
end
