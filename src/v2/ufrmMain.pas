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
begin
  TManagerStack.prepareManager(
    'localhost',
    'testdb',
    'postgres',
    'qwe',
    ExtractFilePath(Application.ExeName) + 'lib' + PathDelim + 'libpq.dll',
    5432
  );
end;

procedure TfrmMain.btnAddBusinessClick(Sender: TObject);
var
  LInv: TInvoice;
  LInvL: TInvoiceLine;
begin
  LInv := TInvoice.Create;
  try
    LInv.InvoiceNo.Value := 'FTR23000001234';
    LInv.InvoiceDate.Value := EncodeDate(2023, 7, 19);
    LInv.AccountCode.Value := '120-001-007';
    LInv.AccountName.Value := 'ABC Ltd Şti';
    LInv.DocumentType.Value := 1;
    LInv.Currency.Value := 'TRY';

    LInvL := TInvoiceLine.Create;
    LInvL.StockCode.Value := 'PC1';
    LInvL.Discount.Value := 10;
    LInvL.Quantity.Value := 2;
    LInvL.Price.Value := 100;
    LInvL.VAT.Value := 20;
    LInv.AddLine(LInvL);

    ManagerMain.LogicalInsertOne(LInv, True, True, False);
  finally
    LInv.DisposeOf;
  end;
end;

procedure TfrmMain.btnFillTestDataClick(Sender: TObject);
var
  AStock: TStock;
  AStocks: TObjectList<TStock>;
begin
  ManagerMain.StartTrans;
  AStocks := TObjectList<TStock>.Create();
  try
    AStock := TStock.Create;
    AStock.StockCode.Value := 'PC1';
    AStock.StockName.Value := 'Computer Packet 1';
    AStocks.Add(AStock);

    AStock := TStock.Create;
    AStock.StockCode.Value := 'PC2G';
    AStock.StockName.Value := 'Computer Packet 2 Gaming';
    AStocks.Add(AStock);

    AStock := TStock.Create;
    AStock.StockCode.Value := 'MONLG1';
    AStock.StockName.Value := 'Monitor LG 19"';
    AStocks.Add(AStock);

    AStock := TStock.Create;
    AStock.StockCode.Value := 'MONLG2';
    AStock.StockName.Value := 'Monitor LG 21"';
    AStocks.Add(AStock);

    AStock := TStock.Create;
    AStock.StockCode.Value := 'MONLG3C';
    AStock.StockName.Value := 'Monitor LG 24" Curved';
    AStocks.Add(AStock);

    ManagerMain.LogicalInsertList<TStock>(AStocks, True, True, True);
  finally
    AStocks.DisposeOf;
  end;
end;

procedure TfrmMain.btnGetOneByCodeFilterClick(Sender: TObject);
var
  LStock, LStockClone: TStock;
  LFilter: string;
begin
  LStock := TStock.Create;
  try
    LFilter := LStock.StockCode.QryName + '=' + QuotedStr('PC2G');
  finally
    LStock.DisposeOf;
    LStock := nil;
  end;
  ManagerMain.GetOne(LStock, LFilter, True);

  LStockClone := ManagerMain.Clone(LStock);

  LStock.DisposeOf;
  LStockClone.DisposeOf;
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
  try
    if ManagerMain.LogicalSelectOne(LInvoice, '1=1', True, True, False) then
    begin
      LInvoice.AccountCode.Value := '120-001-015';

      LInvoiceLine := TInvoiceLine.Create();
      LInvoiceLine.StockCode.Value := 'PC2G';
      LInvoiceLine.Discount.Value := 20;
      LInvoiceLine.Quantity.Value := 1;
      LInvoiceLine.Price.Value := 20000;
      LInvoiceLine.VAT.Value := 20;
      LInvoice.AddLine(LInvoiceLine);
      ManagerMain.LogicalUpdateOne(LInvoice, False, True, False);
    end;
  finally
    LInvoice.DisposeOf;
  end;
end;

end.
