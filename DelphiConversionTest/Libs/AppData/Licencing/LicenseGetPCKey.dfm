object frmLicenseGetPCKey: TfrmLicenseGetPCKey
  Left = 0
  Top = 0
  Caption = 'License'
  ClientHeight = 171
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 32
    Width = 449
    Height = 15
    Caption = 
      'Please copy this Key and send it to Zinsser Analytic: license@zi' +
      'nsser-analytic.com'
  end
  object Edit1: TEdit
    Left = 24
    Top = 64
    Width = 428
    Height = 23
    ReadOnly = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 377
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button1Click
  end
end
