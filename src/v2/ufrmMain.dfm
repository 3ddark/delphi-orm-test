object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ORM Test'
  ClientHeight = 184
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  TextHeight = 13
  object btnResetTables: TButton
    Left = 32
    Top = 8
    Width = 147
    Height = 25
    Caption = 'Reset Tables'
    TabOrder = 0
    OnClick = btnResetTablesClick
  end
  object btnAddBusiness: TButton
    Left = 32
    Top = 90
    Width = 147
    Height = 25
    Caption = 'Add Business'
    TabOrder = 4
    OnClick = btnAddBusinessClick
  end
  object btnFillTestData: TButton
    Left = 32
    Top = 49
    Width = 147
    Height = 25
    Caption = 'Fill Test Data'
    TabOrder = 2
    OnClick = btnFillTestDataClick
  end
  object btnUpdateBusiness: TButton
    Left = 32
    Top = 131
    Width = 147
    Height = 25
    Caption = 'Update Business'
    TabOrder = 6
    OnClick = btnUpdateBusinessClick
  end
  object btnGetOneByCodeFilter: TButton
    Left = 224
    Top = 8
    Width = 169
    Height = 25
    Caption = 'GetOne By Code Filter'
    TabOrder = 1
    OnClick = btnGetOneByCodeFilterClick
  end
  object btnGridListInvoices: TButton
    Left = 432
    Top = 8
    Width = 169
    Height = 25
    Caption = 'DBGrid List Invoices'
    TabOrder = 3
    OnClick = btnGridListInvoicesClick
  end
  object btnGridListStocks: TButton
    Left = 432
    Top = 49
    Width = 169
    Height = 25
    Caption = 'DBGrid List Stocks'
    TabOrder = 5
    OnClick = btnGridListStocksClick
  end
  object btnGridExample: TButton
    Left = 432
    Top = 90
    Width = 169
    Height = 25
    Caption = 'DBGrid Example'
    TabOrder = 7
    OnClick = btnGridExampleClick
  end
end
