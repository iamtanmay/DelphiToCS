object frmMethodSettingsEditor: TfrmMethodSettingsEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'frmMethodSettingsEditor'
  ClientHeight = 421
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 762
    Height = 377
    ActivePage = tbStartOptions
    Align = alClient
    TabOrder = 0
    object tbCommon: TTabSheet
      Caption = 'Common'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblMethodComment: TLabel
        Left = 32
        Top = 32
        Width = 102
        Height = 15
        Caption = 'Method Comment:'
      end
      object lblLineBreakInfo: TLabel
        Left = 32
        Top = 232
        Width = 21
        Height = 15
        Caption = 'info'
      end
      object Memo1: TMemo
        Left = 32
        Top = 53
        Width = 672
        Height = 161
        ScrollBars = ssVertical
        TabOrder = 0
        OnKeyDown = Memo1KeyDown
      end
      object cbEditInRunner: TCheckBox
        Left = 32
        Top = 272
        Width = 217
        Height = 17
        Caption = 'Can be edited in ZARunner'
        TabOrder = 1
      end
    end
    object tbStartOptions: TTabSheet
      Caption = 'Start Options'
      ImageIndex = 1
      object cbStartable: TCheckBox
        Left = 24
        Top = 16
        Width = 489
        Height = 17
        Caption = 'Method is startable'
        TabOrder = 0
        OnClick = cbStartableClick
      end
      object pnStartable: TPanel
        Left = 17
        Top = 48
        Width = 713
        Height = 287
        BevelOuter = bvNone
        TabOrder = 1
        object lblRestartEvent: TLabel
          Left = 32
          Top = 224
          Width = 71
          Height = 15
          Caption = 'Restart Event:'
        end
        object cbUseLayout: TCheckBox
          Left = 32
          Top = 11
          Width = 220
          Height = 17
          Caption = 'Use Layout'
          TabOrder = 0
          OnClick = cbUseLayoutClick
        end
        object rbUseDisplayComponent: TRadioButton
          Left = 32
          Top = 153
          Width = 241
          Height = 17
          Caption = 'Use default display component'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = rbUseDisplayComponentClick
        end
        object rbNoDisplayComponent: TRadioButton
          Left = 32
          Top = 176
          Width = 361
          Height = 17
          Caption = 'Use defined display component'
          TabOrder = 2
          OnClick = rbNoDisplayComponentClick
        end
        object cbLayoutName: TComboBox
          Left = 263
          Top = 8
          Width = 145
          Height = 23
          TabOrder = 3
        end
        object cbChooseLayout: TCheckBox
          Left = 32
          Top = 37
          Width = 220
          Height = 17
          Caption = 'Choose Layout'
          TabOrder = 4
          OnClick = cbChooseLayoutClick
        end
        object cbUseStandardInit: TCheckBox
          Left = 32
          Top = 77
          Width = 425
          Height = 17
          Caption = 'Use Standard Init'
          TabOrder = 5
        end
        object cbDeleteRunData: TCheckBox
          Left = 32
          Top = 109
          Width = 481
          Height = 17
          Caption = 'Delete Run Data'
          TabOrder = 6
        end
        object cbDisplayComponentName: TComboBox
          Left = 295
          Top = 147
          Width = 145
          Height = 23
          TabOrder = 7
        end
        object cbRestartEventName: TComboBox
          Left = 263
          Top = 216
          Width = 145
          Height = 23
          TabOrder = 8
        end
      end
    end
    object tbBuildingBlockOptions: TTabSheet
      Caption = 'Building Block Options'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblImageFileName: TLabel
        Left = 56
        Top = 99
        Width = 92
        Height = 15
        Caption = 'Image File Name:'
      end
      object cbIsBuildingBlock: TCheckBox
        Left = 56
        Top = 48
        Width = 217
        Height = 17
        Caption = 'Is Building Block'
        TabOrder = 0
      end
      object edImageFileName: TEdit
        Left = 208
        Top = 96
        Width = 457
        Height = 23
        TabOrder = 1
      end
      object btnImageFile: TButton
        Left = 671
        Top = 95
        Width = 26
        Height = 25
        Caption = '...'
        TabOrder = 2
        OnClick = btnImageFileClick
      end
    end
    object tbAttributes: TTabSheet
      Caption = 'Attributes'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rbReadOnly: TRadioButton
        Left = 89
        Top = 56
        Width = 113
        Height = 17
        Caption = 'Read Only'
        TabOrder = 0
      end
      object rbHidden: TRadioButton
        Left = 89
        Top = 79
        Width = 113
        Height = 17
        Caption = 'Hidden'
        TabOrder = 1
      end
      object rbDefault: TRadioButton
        Left = 89
        Top = 33
        Width = 113
        Height = 17
        Caption = 'Default'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 762
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      762
      44)
    object btnOK: TButton
      Tag = 530
      Left = 582
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Tag = 540
      Left = 670
      Top = 11
      Width = 74
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
