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
  OnShow = FormShow
  TextHeight = 15
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 73
    Align = alTop
    Caption = 'Header'
    TabOrder = 0
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 368
    Width = 624
    Height = 73
    Align = alBottom
    Caption = 'Footer'
    TabOrder = 1
  end
  object pnlContent: TPanel
    Left = 0
    Top = 73
    Width = 624
    Height = 295
    Align = alClient
    Caption = 'Content'
    TabOrder = 2
    ExplicitLeft = 400
    ExplicitTop = 224
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
end
