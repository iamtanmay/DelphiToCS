object FrmScheduleChartOptions: TFrmScheduleChartOptions
  Left = 420
  Top = 185
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Chart Options'
  ClientHeight = 501
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel2: TBevel
    Left = 0
    Top = 459
    Width = 387
    Height = 3
    Align = alBottom
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 462
    Width = 387
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TButton
      Left = 273
      Top = 6
      Width = 103
      Height = 27
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TButton
      Tag = 510
      Left = 164
      Top = 7
      Width = 103
      Height = 27
      Caption = '&OK'
      ModalResult = 1
      TabOrder = 1
    end
  end
  object pnlTabs: TPanel
    Left = 0
    Top = 0
    Width = 387
    Height = 459
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object pgctlMain: TPageControl
      Left = 6
      Top = 6
      Width = 375
      Height = 447
      ActivePage = shtImportDefs
      Align = alClient
      TabOrder = 0
      object shtImportDefs: TTabSheet
        Caption = 'Time Mode'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object rgrpTimeMode: TRadioGroup
          Left = 13
          Top = 9
          Width = 341
          Height = 49
          Caption = 'Time Mode'
          Columns = 2
          Items.Strings = (
            'Relative'
            'Absolute')
          TabOrder = 0
          OnClick = rgrpTimeModeClick
        end
        object grpAbsTimeMode: TGroupBox
          Left = 13
          Top = 63
          Width = 341
          Height = 330
          Caption = 'Absolute Time Mode'
          TabOrder = 1
          object rgrpTimeDefined: TRadioGroup
            Left = 11
            Top = 18
            Width = 318
            Height = 49
            Caption = 'Time Defined'
            Columns = 2
            Items.Strings = (
              'Now'
              'At')
            TabOrder = 0
            OnClick = rgrpTimeDefinedClick
          end
          object grpTimeDefinedAt: TGroupBox
            Left = 12
            Top = 75
            Width = 317
            Height = 238
            Caption = 'Time Defined At'
            TabOrder = 1
            object Label1: TLabel
              Left = 13
              Top = 19
              Width = 27
              Height = 15
              Caption = 'Date:'
            end
            object Label2: TLabel
              Left = 222
              Top = 18
              Width = 30
              Height = 15
              Caption = 'Time:'
            end
            object calDate: TMonthCalendar
              Left = 11
              Top = 35
              Width = 201
              Height = 153
              Date = 38366.468604375000000000
              TabOrder = 0
              WeekNumbers = True
            end
            object dtpTime: TDateTimePicker
              Left = 221
              Top = 35
              Width = 88
              Height = 23
              Date = 38366.375000000000000000
              Time = 38366.375000000000000000
              DateMode = dmUpDown
              Kind = dtkTime
              TabOrder = 1
            end
          end
        end
      end
    end
  end
end
