unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Data.DB, System.Generics.Collections,
  Ths.Orm.ManagerStack,
  Ths.Orm.Table,
  Ths.Orm.Manager,
  Persons, Stocks, Invoices;

type
  TfrmMain = class(TForm)
    btnResetTables: TButton;
    btnAddBusiness: TButton;
    btnFillTestData: TButton;
    btnUpdateBusiness: TButton;
    btnGetOneByCodeFilter: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnResetTablesClick(Sender: TObject);
    procedure btnFillTestDataClick(Sender: TObject);
    procedure btnAddBusinessClick(Sender: TObject);
    procedure btnUpdateBusinessClick(Sender: TObject);
    procedure btnGetOneByCodeFilterClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
//var
//  n1: Integer;
//  LPerson: TPerson;
//  LPersons: TArray<TTable>;
//  LAdres: TPersonAdres;
begin
  TManagerStack.prepareManager(
    'localhost',
    'ths_erp',
    'postgres',
    'qwe',
    ExtractFilePath(Application.ExeName) + 'lib' + PathDelim + 'libpq.dll',
    5432
  );

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

    ManagerMain.LogicalInsert(LInv, True, True, False, LInv.BusinessInsert);
  finally
    LInv.DisposeOf;
  end;
end;

procedure TfrmMain.btnFillTestDataClick(Sender: TObject);
var
  ATable: TStock;
begin
  ManagerMain.StartTrans;
  ATable := TStock.Create();
  try
    ATable.StokKodu.Value := 'PC1';
    ATable.StokAdi.Value := 'Bilgisayar Paket 1';
    ManagerMain.Insert(ATable, False);

    ATable.StokKodu.Value := 'PC2G';
    ATable.StokAdi.Value := 'Bilgisayar Paket 2 Gaming';
    ManagerMain.Insert(ATable, False);

    ATable.StokKodu.Value := 'MONLG1';
    ATable.StokAdi.Value := 'Monitör LG 19"';
    ManagerMain.Insert(ATable, False);

    ATable.StokKodu.Value := 'MONLG2';
    ATable.StokAdi.Value := 'Monitör LG 21"';
    ManagerMain.Insert(ATable, False);

    ATable.StokKodu.Value := 'MONLG3C';
    ATable.StokAdi.Value := 'Monitör LG 24" Curved';
    ManagerMain.Insert(ATable, False);
    ManagerMain.CommitTrans;
  finally
    ATable.DisposeOf;
  end;
end;

procedure TfrmMain.btnGetOneByCodeFilterClick(Sender: TObject);
var
  LStock: TStock;
  LFilter: string;
begin
  LStock := TStock.Create;
  try
    LFilter := LStock.StokKodu.QryName + '=' + QuotedStr('PC2G');
  finally
    LStock.DisposeOf;
    LStock := nil;
  end;
  ManagerMain.GetOne(LStock, LFilter, True);
end;

procedure TfrmMain.btnResetTablesClick(Sender: TObject);
begin
  ManagerMain.StartTrans;
  ManagerMain.DeleteBatch<TInvoice>('', False);
  ManagerMain.DeleteBatch<TStock>('', False);
  ManagerMain.CommitTrans;
end;

procedure TfrmMain.btnUpdateBusinessClick(Sender: TObject);
var
  LInvoice: TInvoice;
  LInvoiceLine: TInvoiceLine;
begin
  ManagerMain.LogicalSelectOne(LInvoice, '1=1', True, True, False);
  try
    LInvoice.HesapKodu.Value := '120-001-015';

    LInvoiceLine := TInvoiceLine.Create();
    LInvoiceLine.StokKodu.Value := 'PC2G';
    LInvoiceLine.Iskonto.Value := 20;
    LInvoiceLine.Miktar.Value := 1;
    LInvoiceLine.Fiyat.Value := 20000;
    LInvoiceLine.Kdv.Value := 20;
    LInvoice.AddLine(LInvoiceLine);
    //ManagerMain.LogicalUpdate(LInvoice, False, True, False, TInvoice.BusinessUpdate);
  finally
    LInvoice.DisposeOf;
  end;
end;

end.
