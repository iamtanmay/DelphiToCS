object ToolErrForm: TToolErrForm
  Tag = 27010
  Left = 355
  Top = 227
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Tool Error Handling'
  ClientHeight = 242
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Shape2: TShape
    Left = 0
    Top = 0
    Width = 555
    Height = 185
    Align = alTop
    Pen.Color = clWhite
  end
  object Label1: TLabel
    Tag = 27020
    Left = 48
    Top = 64
    Width = 299
    Height = 15
    Caption = 'Please, choose one of the following error handling types:'
    Color = clWhite
    ParentColor = False
  end
  object Bevel2: TBevel
    Left = 0
    Top = 185
    Width = 555
    Height = 16
    Align = alTop
    Shape = bsTopLine
  end
  object Image1: TImage
    Left = 208
    Top = 134
    Width = 16
    Height = 17
    AutoSize = True
    Picture.Data = {
      07544269746D617066030000424D660300000000000036000000280000001000
      000011000000010018000000000030030000C30E0000C30E0000000000000000
      0000FFFFFFFFFFFFFFFFFFF7F7F7FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE8E8E879868C2E70864D6F
      7D7E878CB1B2B2D8D8D8F2F2F2FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFF4B7A8C0093B600CBF900CBF900C2EF01A7D40D89AD26728B4E707D
      80888DB2B2B3D9D9D9F3F3F3FFFFFFFFFFFFFFFFFF448DA8018FAF01C1EB01CD
      F901CCF900CCF900CBF90D687D0E85A000C1EF01A6D30D88AB28728AA4ABAEFF
      FFFFFFFFFFB7CBD6038BAD03A8CB03D0F903CFF902CFF902CEFA294045484F50
      01CCF900CCF900CBF900CBF92F99B9FFFFFFFFFFFFFFFFFF3089A80599BB06CC
      F105D4FA05D3FA05D2FA08B1D10F9DB903D0F903CFF902CEF902CEF971ACC3FF
      FFFFFFFFFFFFFFFFA5C0CE0799BC08B6D909D8FA08D7FA08D7FA03738604839A
      06D4FA05D3FA05D3FA0CADD4E7EDF0FFFFFFFFFFFFFFFFFFFCFCFC268CAD0BA8
      CB0CD8F70CDDFB0BDCFB023941023E4709D9FB09D9FA08D7FA72ADC4FFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFF95B7C80DAACE0FC7E60FE2FC0FE2FC032D32032E33
      0DDFFB0CDEFB12B4D4E8EEF1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F8F91F93
      B811B8DD13E5FB12E7FC14313317323411E5FC10E4FB75AFC4FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF86B1C413BBE115D6F116EDFD3040413D4647
      15EBFD17BBD4E9EEF1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFF2
      F41B9FC616C8ED19F1FD3D474854575718EFFC76B1C4FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF76ABC118C9F21BE4FA248F922F797B
      1EC2D5EAEFF2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFE6EBEF1AACD41BD5FA1EF9FE1EF8FE79B3C5FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6CA8C11CD1FB1EEDFE22C2D1
      EBF0F2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFF6F7F89EBCCB9BBBCAD3DFE5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  end
  object lblTop: TLabel
    Left = 48
    Top = 24
    Width = 187
    Height = 21
    Caption = 'A tool is still in the gripper!'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label3: TLabel
    Left = 240
    Top = 136
    Width = 136
    Height = 15
    Caption = '-> The tool will fall down!'
    Color = clWhite
    ParentColor = False
  end
  object rbBringBack: TRadioButton
    Left = 72
    Top = 96
    Width = 273
    Height = 17
    Caption = 'Bring back tool'
    Checked = True
    Color = clWhite
    ParentColor = False
    TabOrder = 0
    TabStop = True
  end
  object rbOpenGripper: TRadioButton
    Left = 72
    Top = 136
    Width = 129
    Height = 17
    Caption = 'Open handler'
    Color = clWhite
    ParentColor = False
    TabOrder = 1
  end
  object btnOK: TButton
    Tag = 530
    Left = 372
    Top = 206
    Width = 74
    Height = 23
    Caption = 'Continue'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Tag = 540
    Left = 460
    Top = 206
    Width = 74
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
