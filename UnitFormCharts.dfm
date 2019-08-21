object FormCharts: TFormCharts
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = #1054#1073#1079#1086#1088' '#1072#1088#1093#1080#1074#1072' '#1075#1088#1072#1092#1080#1082#1086#1074
  ClientHeight = 304
  ClientWidth = 344
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 304
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 443
    ExplicitTop = 3
    ExplicitWidth = 306
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 344
      Height = 31
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   '#1044#1072#1090#1072
      Color = clGradientInactiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      ExplicitWidth = 265
      object ComboBox1: TComboBox
        Left = 77
        Top = 2
        Width = 132
        Height = 26
        Style = csOwnerDrawFixed
        Color = clHighlightText
        ItemHeight = 20
        ItemIndex = 0
        TabOrder = 0
        Text = '11.11.2018'
        OnChange = ComboBox1Change
        Items.Strings = (
          '11.11.2018')
      end
    end
    object StringGrid1: TStringGrid
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 338
      Height = 267
      Align = alClient
      BorderStyle = bsNone
      ColCount = 2
      DefaultRowHeight = 22
      DefaultDrawing = False
      FixedColor = clBackground
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      GradientEndColor = clBlack
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      ParentFont = False
      TabOrder = 1
      OnDrawCell = StringGrid1DrawCell
      OnSelectCell = StringGrid1SelectCell
      ExplicitLeft = 0
      ExplicitWidth = 434
      ColWidths = (
        64
        64)
      RowHeights = (
        22)
    end
  end
end
