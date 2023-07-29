object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ORM Test'
  ClientHeight = 184
  ClientWidth = 208
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
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
    TabOrder = 2
    OnClick = btnAddBusinessClick
  end
  object btnFillTestData: TButton
    Left = 32
    Top = 49
    Width = 147
    Height = 25
    Caption = 'Fill Test Data'
    TabOrder = 1
    OnClick = btnFillTestDataClick
  end
  object btnUpdateBusiness: TButton
    Left = 32
    Top = 131
    Width = 147
    Height = 25
    Caption = 'Update Business'
    TabOrder = 3
    OnClick = btnUpdateBusinessClick
  end
end
