object FormReadVars: TFormReadVars
  Left = 0
  Top = 0
  Caption = 'FormReadVars'
  ClientHeight = 507
  ClientWidth = 874
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 874
    Height = 507
    Align = alClient
    BorderStyle = bsNone
    ColCount = 3
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
    OnDblClick = StringGrid1DblClick
    OnDrawCell = StringGrid1DrawCell
    OnKeyPress = StringGrid1KeyPress
    OnMouseDown = StringGrid1MouseDown
    OnSelectCell = StringGrid1SelectCell
    OnSetEditText = StringGrid1SetEditText
    ColWidths = (
      64
      64
      64)
    RowHeights = (
      24)
  end
end
