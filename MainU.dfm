object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 852
    Height = 411
    Align = alClient
    Columns = <
      item
        Caption = 'Event class'
      end
      item
        Alignment = taCenter
        Caption = 'Event time'
      end
      item
        Alignment = taCenter
        Caption = 'TimeStamp'
      end>
    DoubleBuffered = True
    OwnerDraw = True
    RowSelect = True
    ParentDoubleBuffered = False
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    OnDrawItem = ListView1DrawItem
    ExplicitLeft = 8
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 424
    Top = 208
  end
end
