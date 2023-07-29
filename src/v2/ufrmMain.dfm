object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object btnResetTables: TButton
    Left = 8
    Top = 8
    Width = 107
    Height = 25
    Caption = 'Reset Tables'
    TabOrder = 0
    OnClick = btnResetTablesClick
  end
  object btnAddBusiness: TButton
    Left = 144
    Top = 8
    Width = 107
    Height = 25
    Caption = 'Add Business'
    TabOrder = 1
    OnClick = btnAddBusinessClick
  end
  object btnFillTestData: TButton
    Left = 8
    Top = 56
    Width = 107
    Height = 25
    Caption = 'Fill Test Data'
    TabOrder = 2
    OnClick = btnFillTestDataClick
  end
  object btnUpdateBusiness: TButton
    Left = 144
    Top = 56
    Width = 107
    Height = 25
    Caption = 'Update Business'
    TabOrder = 3
    OnClick = btnUpdateBusinessClick
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cCP_UTF16
    Catalog = ''
    HostName = ''
    Port = 0
    Database = ''
    User = ''
    Password = ''
    Protocol = ''
    Left = 312
    Top = 152
  end
  object ZQuery1: TZQuery
    Params = <>
    Left = 320
    Top = 160
  end
end
