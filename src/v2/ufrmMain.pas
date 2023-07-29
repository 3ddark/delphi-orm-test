unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.DateUtils,
  System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls,
  ZAbstractRODataset, ZAbstractDataset, ZDataset, ZAbstractConnection, ZConnection,
  Ths.Erp.Database.Table, Ths.Erp.Database.Manager,
  Persons, Stocks, Invoices, StockTransactions, AccountTransactions;

type
  TfrmMain = class(TForm)
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    btnResetTables: TButton;
    btnAddBusiness: TButton;
    btnFillTestData: TButton;
    btnUpdateBusiness: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnResetTablesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnFillTestDataClick(Sender: TObject);
    procedure btnAddBusinessClick(Sender: TObject);
    procedure btnUpdateBusinessClick(Sender: TObject);
  private
    FManager: TEntityManager;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnResetTablesClick(Sender: TObject);
begin
  FManager.DeleteBatch(TInvoice, '', False);
  FManager.DeleteBatch(TStock, '', False);
end;

procedure TfrmMain.btnUpdateBusinessClick(Sender: TObject);
var
  LInvoice: TInvoice;
  LInvoices: TArray<TTable>;
  LInvoiceLine: TInvoiceLine;
begin
  FManager.LogicalSelect(TInvoice, LInvoices, '', True, True, False, TInvoice.BusinessSelect);
  if Length(LInvoices) = 1 then
  begin
    LInvoice := TInvoice(LInvoices[0]);
    try
      LInvoices[0] := nil;
      SetLength(LInvoices, 0);

      LInvoice.HesapKodu.Value := '120-001-015';

      LInvoiceLine := TInvoiceLine.Create();
      LInvoiceLine.StokKodu.Value := 'PC2G';
      LInvoiceLine.Iskonto.Value := 20;
      LInvoiceLine.Miktar.Value := 1;
      LInvoiceLine.Fiyat.Value := 20000;
      LInvoiceLine.Kdv.Value := 20;
      LInvoice.AddLine(LInvoiceLine);

      FManager.LogicalUpdate([LInvoice], True, True, False, TInvoice.BusinessUpdate);
    finally
      LInvoice.DisposeOf;
    end;
  end;
end;

procedure TfrmMain.btnAddBusinessClick(Sender: TObject);
var
  LInv: TInvoice;
  LInvL: TInvoiceLine;
begin
  LInv := TInvoice.Create;
  try
    LInv.FaturaNo.Value := 'FTR23000001234';
    LInv.FaturaTarihi.Value := EncodeDate(2023, 7, 19);
    LInv.HesapKodu.Value := '120-001-007';
    LInv.HesapIsmi.Value := 'ABC Ltd Şti';
    LInv.FaturaTipi.Value := 1;
    LInv.Para.Value := 'TRY';

    LInvL := TInvoiceLine.Create;
    LInvL.StokKodu.Value := 'PC1';
    LInvL.Iskonto.Value := 10;
    LInvL.Miktar.Value := 2;
    LInvL.Fiyat.Value := 100;
    LInvL.Kdv.Value := 20;
    LInv.AddLine(LInvL);

    FManager.LogicalInsert([LInv], True, True, False, TInvoice.BusinessInsert);
  finally
    LInv.DisposeOf;
  end;
end;

procedure TfrmMain.btnFillTestDataClick(Sender: TObject);
var
  ATable: TStock;
begin
  ATable := TStock.Create();
  try
    ATable.StokKodu.Value := 'PC1';
    ATable.StokAdi.Value := 'Bilgisayar Paket 1';
    FManager.Insert(ATable, False);

    ATable.StokKodu.Value := 'PC2G';
    ATable.StokAdi.Value := 'Bilgisayar Paket 2 Gaming';
    FManager.Insert(ATable, False);

    ATable.StokKodu.Value := 'MONLG1';
    ATable.StokAdi.Value := 'Monitör LG 19"';
    FManager.Insert(ATable, False);

    ATable.StokKodu.Value := 'MONLG2';
    ATable.StokAdi.Value := 'Monitör LG 21"';
    FManager.Insert(ATable, False);

    ATable.StokKodu.Value := 'MONLG3C';
    ATable.StokAdi.Value := 'Monitör LG 24" Curved';
    FManager.Insert(ATable, False);
  finally
    ATable.DisposeOf;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
//var
//  n1: Integer;
//  LPerson: TPerson;
//  LPersons: TArray<TTable>;
//  LAdres: TPersonAdres;
begin
  ZConnection1 := TZConnection.Create(nil);
  ZConnection1.Protocol := 'postgresql-9';
  ZConnection1.Database := 'ths_erp';
  ZConnection1.HostName := 'localhost';
  ZConnection1.User := 'postgres';
  ZConnection1.Password := 'qwe';
  ZConnection1.Connect;

  FManager := TEntityManager.Create(ZConnection1);

//  LPerson := TPerson.Create;
//  try
//    FManager.LogicalSelect(LPerson.Id.QryName + '=10', True, True, False, TPerson.BusinessSelect);
//    FManager.GetOne(LPerson, 10, False);
//    LPerson.PersonName.Value := UpperCase(LPerson.PersonName.AsString);
//    FManager.Update(LPerson);
//    LPerson.PersonName.Value :=  LowerCase(LPerson.PersonName.AsString);
//    LPerson.PersonAge.Value := 27;
//    LPerson.Salary.Value := 350;
//    FManager.CustomUpdate(LPerson, [LPerson.PersonName, LPerson.PersonAge]);
//    FManager.GetList(TPerson, LPersons, 'id > 0', False);
//  finally
//    LPerson.DisposeOf;
//    for n1 := 0 to Length(LPersons)-1 do
//      LPersons[n1].DisposeOf;
//    SetLength(LPersons, 0);
//  end;

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FManager.Free;
  ZConnection1.Disconnect;
  ZConnection1.DisposeOf;
end;

end.
