object frmStockTransactions: TfrmStockTransactions
  Left = 0
  Top = 0
  Caption = 'frmStockTransactions'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 73
    Align = alTop
    Caption = 'Header'
    TabOrder = 0
    object ComboBox1: TComboBox
      Left = 88
      Top = 26
      Width = 145
      Height = 23
      TabOrder = 0
      Text = 'ComboBox1'
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 368
    Width = 624
    Height = 73
    Align = alBottom
    Caption = 'Footer'
    TabOrder = 1
    object Edit1: TEdit
      Left = 152
      Top = 32
      Width = 121
      Height = 23
      TabOrder = 0
      Text = 'Edit1'
    end
  end
end
