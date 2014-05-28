object frmEvalCarr: TfrmEvalCarr
  Tag = 53060
  Left = 762
  Top = 201
  HorzScrollBar.Tracking = True
  VertScrollBar.Tracking = True
  BorderStyle = bsToolWindow
  Caption = 'Evaluate Carrier'
  ClientHeight = 298
  ClientWidth = 316
  Color = clBtnFace
  DockSite = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
    FFFFFFFFFAAAAAAFFFFFFFFFFFFFFFFFFFFFFFAAAFFFFFFAAAFFFFFFFFFFFFFF
    FFFFAAFFFFFFFFFFFFAAFFFFFFFFFFFFFFFAFFFFFFAAAAFFFFFFAFFFFFFFFFFF
    FAAFFFFAAAFFFFAAAFFFFAAFFFFFFFFFAFFFFAAFFFFFFFFFFAFFFFFAFFFFFFFF
    AFFAAFFFFFFFFFFFFFFAAFFAFFFFFFFAFFAFFFFF00000000FFFFFAFFAFFFFFAF
    FAFFFFAA0FFFFFF0FAFFFFFFFAFFFAFFFFFFFFFF0FFFFFF0FFAFFFAFFFAFFAFF
    AFFFFAFF0FFFFFF0FFFFFFFAFFAFFAFFAFFFFFFF0FFFFFF0FFFAFFFAFFAFAFFF
    AFFFAFFF0FFFFFF0FFFFFFFAFFFAAFFAFFFFFFFF0FFFFFF0AFFFAFFFAFFAAFFA
    FFFAFFAF0FFFFFF0FFFFAFFFAFFAAFFAFFFAFFAF0FFFFFF0FAFFAFFFAFFAAFFA
    FFFAFFAF0FFFFFF0FAFFAFFFAFFAAFFAFFFAFFFA0FFFFFF0FFFFAFFFAFFAAFFA
    FFFFAFFF0FFFFFF0FFFFAFFFAFFAAFFFAFFFAFFF0FFFFFF0FFFFFFFAFFFAFAFF
    AFFFFAFF00FFFF00FFFAFFFAFFAFFAFFAFFFFFFFF000000FFFAFFFFAFFAFFAFF
    FFFFFFAAFFFFFFFFFAFFFFAFFFAFFFAFFAFFFFFA00000000AFFFFFFFFAFFFFFA
    FFAFFFFF00000000FFFFFAFFAFFFFFFFAFFAAFFFFFFFFFFFFFFAAFFAFFFFFFFF
    AFFFFAFFFFFFFFFFFFAFFFFAFFFFFFFFFAAFFFFAAAFFFFAAAFFFFAAFFFFFFFFF
    FFFAFFFFFFAAAAFFFFFFAFFFFFFFFFFFFFFFAAFFFFFFFFFFFFAAFFFFFFFFFFFF
    FFFFFFAAAFFFFFFAAAFFFFFFFFFFFFFFFFFFFFFFFAAAAAAFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 316
    Height = 76
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clSilver
    TabOrder = 0
    Visible = False
    object mm: TLabel
      Tag = 53070
      Left = 6
      Top = 32
      Width = 71
      Height = 15
      Caption = 'Saved Values:'
    end
    object Label1: TLabel
      Left = 24
      Top = 55
      Width = 10
      Height = 15
      Caption = 'X:'
    end
    object Label2: TLabel
      Left = 168
      Top = 55
      Width = 10
      Height = 15
      Caption = 'Y:'
    end
    object Label7: TLabel
      Tag = 53540
      Left = 80
      Top = 8
      Width = 80
      Height = 15
      Caption = 'Current Values:'
      Visible = False
    end
    object Label8: TLabel
      Left = 176
      Top = 8
      Width = 10
      Height = 15
      Caption = 'X:'
      Visible = False
    end
    object Label9: TLabel
      Left = 241
      Top = 8
      Width = 10
      Height = 15
      Caption = 'Y:'
      Visible = False
    end
    object XEdit: TEdit
      Left = 40
      Top = 53
      Width = 51
      Height = 17
      TabStop = False
      AutoSize = False
      ReadOnly = True
      TabOrder = 0
    end
    object yEdit: TEdit
      Left = 184
      Top = 53
      Width = 51
      Height = 17
      TabStop = False
      AutoSize = False
      ReadOnly = True
      TabOrder = 1
    end
    object yUpdateBtn: TButton
      Tag = 53100
      Left = 237
      Top = 52
      Width = 73
      Height = 18
      Caption = 'Update'
      TabOrder = 2
      TabStop = False
      OnClick = yUpdateBtnClick
    end
    object XUpdateBtn: TButton
      Tag = 53090
      Left = 88
      Top = 52
      Width = 74
      Height = 18
      Caption = 'Update'
      TabOrder = 3
      TabStop = False
      OnClick = XUpdateBtnClick
    end
    object Edit4: TEdit
      Left = 192
      Top = 5
      Width = 36
      Height = 17
      TabStop = False
      AutoSize = False
      ReadOnly = True
      TabOrder = 4
      Visible = False
    end
    object Edit5: TEdit
      Left = 257
      Top = 5
      Width = 37
      Height = 17
      TabStop = False
      AutoSize = False
      ReadOnly = True
      TabOrder = 5
      Visible = False
    end
  end
  object RefPanel: TPanel
    Left = 0
    Top = 76
    Width = 316
    Height = 182
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clSilver
    TabOrder = 1
    ExplicitTop = 57
    ExplicitHeight = 178
    object Label15: TLabel
      Tag = 53170
      Left = 6
      Top = 5
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Reference Point'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI Semibold'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel1: TBevel
      Left = 80
      Top = 25
      Width = 121
      Height = 88
      Style = bsRaised
    end
    object Label4: TLabel
      Left = 166
      Top = 124
      Width = 10
      Height = 15
      Caption = 'X:'
    end
    object Label5: TLabel
      Left = 238
      Top = 124
      Width = 10
      Height = 15
      Caption = 'Y:'
    end
    object Label6: TLabel
      Tag = 53040
      Left = 157
      Top = 6
      Width = 150
      Height = 15
      Alignment = taRightJustify
      Caption = 'Reference points out of limit'
      Color = clRed
      ParentColor = False
    end
    object lblTube: TLabel
      Tag = 53520
      Left = 8
      Top = 70
      Width = 297
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Click on the Rack position you want to use as reference point'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ref1: TRadioButton
      Tag = 53550
      Left = 8
      Top = 31
      Width = 89
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Left Rear Edge'
      Checked = True
      Color = clSilver
      ParentColor = False
      TabOrder = 0
      TabStop = True
      OnClick = ReferenceClick
    end
    object ref2: TRadioButton
      Tag = 53560
      Left = 176
      Top = 31
      Width = 129
      Height = 17
      Caption = 'Right rear edge'
      TabOrder = 1
      OnClick = ReferenceClick
    end
    object ref4: TRadioButton
      Tag = 53580
      Left = 176
      Top = 90
      Width = 129
      Height = 17
      Caption = 'Right front edge'
      TabOrder = 2
      OnClick = ReferenceClick
    end
    object ref3: TRadioButton
      Tag = 53570
      Left = 8
      Top = 90
      Width = 89
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Left front edge'
      Color = clSilver
      ParentColor = False
      TabOrder = 3
      OnClick = ReferenceClick
    end
    object refTube: TRadioButton
      Tag = 53590
      Left = 104
      Top = 52
      Width = 89
      Height = 17
      Caption = 'Rack position'
      TabOrder = 4
      Visible = False
    end
    object Edit1: TEdit
      Left = 181
      Top = 122
      Width = 50
      Height = 17
      TabStop = False
      AutoSize = False
      ReadOnly = True
      TabOrder = 5
      Text = '*'
    end
    object Edit2: TEdit
      Left = 253
      Top = 122
      Width = 50
      Height = 17
      TabStop = False
      AutoSize = False
      ReadOnly = True
      TabOrder = 6
      Text = '*'
    end
    object btnMoveToRefPoint: TButton
      Tag = 53180
      Left = 84
      Top = 146
      Width = 225
      Height = 25
      Caption = '&Move To Reference Point'
      Default = True
      TabOrder = 7
      OnClick = btnMoveToRefPointClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 258
    Width = 316
    Height = 40
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    ExplicitTop = 235
    object btnOK: TButton
      Tag = 510
      Left = 184
      Top = 8
      Width = 81
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Tag = 520
      Left = 64
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
