object frmWorkspaceEditor: TfrmWorkspaceEditor
  Left = 141
  Top = 205
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Workspace Editor'
  ClientHeight = 422
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PrintScale = poPrintToFit
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 304
    Top = 0
    Width = 458
    Height = 384
    ActivePage = TabSheet1
    Align = alRight
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Basic'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox7: TGroupBox
        Tag = 51550
        Left = 15
        Top = 167
        Width = 168
        Height = 65
        Caption = 'Appearance'
        TabOrder = 0
        object Shape1: TShape
          Left = 16
          Top = 18
          Width = 33
          Height = 33
          Brush.Color = clScrollBar
        end
        object btnColor: TButton
          Tag = 51560
          Left = 64
          Top = 24
          Width = 89
          Height = 25
          Caption = 'Change Color'
          TabOrder = 0
          OnClick = btnColorClick
        end
      end
      object RackTyp: TGroupBox
        Tag = 51090
        Left = 15
        Top = 15
        Width = 168
        Height = 50
        Caption = 'Type'
        TabOrder = 1
        object Label11: TLabel
          Tag = 50140
          Left = 8
          Top = 21
          Width = 32
          Height = 15
          Caption = 'Name'
        end
        object edTypeName: TEdit
          Left = 48
          Top = 16
          Width = 113
          Height = 23
          TabOrder = 0
          Text = 'edTypeName'
        end
      end
      object gbAbmessungen: TGroupBox
        Tag = 51100
        Left = 15
        Top = 69
        Width = 168
        Height = 93
        Caption = 'Dimensions [mm] '
        TabOrder = 2
        object Label1: TLabel
          Tag = 50170
          Left = 8
          Top = 24
          Width = 61
          Height = 15
          Caption = 'Length   (X)'
        end
        object Label2: TLabel
          Tag = 50180
          Left = 8
          Top = 48
          Width = 62
          Height = 15
          Caption = 'Width     (Y)'
        end
        object Label3: TLabel
          Tag = 50190
          Left = 8
          Top = 72
          Width = 63
          Height = 15
          Caption = 'Height    (Z)'
        end
        object edSizeX: TEdit
          Left = 108
          Top = 16
          Width = 50
          Height = 23
          TabOrder = 0
          Text = 'edSizeX'
        end
        object edSizeY: TEdit
          Left = 108
          Top = 40
          Width = 50
          Height = 23
          TabOrder = 1
          Text = 'edSizeY'
        end
        object edSizeZ: TEdit
          Left = 108
          Top = 64
          Width = 50
          Height = 23
          TabOrder = 2
          Text = 'edSizeZ'
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'View Relation'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 455
      object rdoViewType: TRadioGroup
        Tag = 51900
        Left = 8
        Top = 8
        Width = 104
        Height = 61
        Caption = 'rdoViewType'
        Items.Strings = (
          'Def'
          'Cust')
        TabOrder = 0
        OnClick = rdoViewTypeClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'World Relation'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 455
    end
    object TabSheet3: TTabSheet
      Caption = 'Devices'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 455
      object grpArmSelection: TGroupBox
        Left = 4
        Top = 72
        Width = 429
        Height = 276
        Caption = 'Device Selection'
        TabOrder = 0
        object Label4: TLabel
          Left = 12
          Top = 23
          Width = 94
          Height = 15
          Caption = 'Available Devices:'
        end
        object btnAddArm: TSpeedButton
          Left = 200
          Top = 112
          Width = 26
          Height = 26
          Glyph.Data = {
            BA040000424DBA0400000000000036040000280000000B0000000B0000000100
            08000000000084000000C40E0000C40E00000001000000010000C0C0C000A45D
            0500A55E0600A65F0700A55E0700A65F0800A5600300A45E0300A55F0400A660
            0500A25E0500A35F0600A25D0600A15F0600A05D0600A4600700A35E0700A260
            0700A15E0700A15E0900A05F0300A3600400A461050000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000000B
            00000000000000000000000C12000000000000000000000F0110000000000112
            0B12120B0315150000000E0803100B0D08120B030000151602050A06130F1509
            12000B140F0B0D0D010402100000121615120709020B0B000000000000000013
            0F0B00000000000000000012090000000000000000000011000000000000}
          OnClick = btnAddArmClick
        end
        object btnRemoveArm: TSpeedButton
          Left = 200
          Top = 152
          Width = 26
          Height = 26
          Glyph.Data = {
            BA040000424DBA0400000000000036040000280000000B0000000B0000000100
            08000000000084000000C40E0000C40E000000010000000100003E00D0003E00
            CE003D00CD003D00CC003E01CD003F02CE004000D1004000D0004100D0004000
            CE004100CE003F00CD003F00CC004000CC004101CF004201CE004001CD004001
            CC004101CC00C0C0C00000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000013131313130F
            1313131313FF1313131301091313131313FF131313070C091313131313FF1313
            11071000090707100EFF13000B0109110907100211FF0A0B0510030102100104
            0BFF1309090309040610090608FF13130A070B030801030110FF1313130A0409
            1313131313FF131313130D0A1313131313FF1313131313121313131313FF}
          OnClick = btnRemoveArmClick
        end
        object Label5: TLabel
          Left = 236
          Top = 24
          Width = 72
          Height = 15
          Caption = 'Used Devices:'
        end
        object tvAvailableDevices: TTreeView
          Left = 8
          Top = 40
          Width = 184
          Height = 225
          Indent = 19
          ReadOnly = True
          TabOrder = 0
        end
        object tvUsedDevices: TTreeView
          Left = 232
          Top = 40
          Width = 184
          Height = 225
          Indent = 19
          ReadOnly = True
          TabOrder = 1
        end
      end
      object rdoArmSelectionMode: TRadioGroup
        Left = 4
        Top = 8
        Width = 429
        Height = 60
        Caption = 'Device Selection Mode'
        ItemIndex = 0
        Items.Strings = (
          'All'
          'Custom')
        TabOrder = 1
        OnClick = rdoArmSelectionModeClick
      end
    end
  end
  object Panel2x: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 384
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clBtnShadow
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 384
    Width = 762
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnApply: TButton
      Tag = 590
      Left = 652
      Top = 6
      Width = 81
      Height = 25
      Caption = 'Apply'
      TabOrder = 0
      OnClick = btnApplyClick
    end
    object btnOK: TButton
      Tag = 510
      Left = 472
      Top = 6
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Tag = 520
      Left = 562
      Top = 6
      Width = 81
      Height = 25
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = btnCancelClick
    end
  end
  object ColorDialog1: TColorDialog
    Left = 36
    Top = 51
  end
end
