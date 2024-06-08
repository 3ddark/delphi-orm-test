unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Data.DB, System.Generics.Collections, ufrmGrid,
  Ths.Orm.ManagerStack, Ths.Orm.Table, Ths.Orm.Manager;

type
  TfrmMain = class(TForm)
    btnResetTables: TButton;
    btnAddBusiness: TButton;
    btnFillTestData: TButton;
    btnUpdateBusiness: TButton;
    btnGetOneByCodeFilter: TButton;
    btnGridListInvoices: TButton;
    btnGridListStocks: TButton;
    btnGridExample: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnResetTablesClick(Sender: TObject);
    procedure btnFillTestDataClick(Sender: TObject);
    procedure btnAddBusinessClick(Sender: TObject);
    procedure btnUpdateBusinessClick(Sender: TObject);
    procedure btnGetOneByCodeFilterClick(Sender: TObject);
    procedure btnGridListInvoicesClick(Sender: TObject);
    procedure btnGridListStocksClick(Sender: TObject);
    procedure btnGridExampleClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Stocks,
  Persons,
  Invoices, ufrmInvoices,
  ufrmStockTransactions;

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

    ManagerApp.LogicalInsertOne(LInv, True, True, False);
  finally
    LInv.Free;
  end;
end;

procedure TfrmMain.btnFillTestDataClick(Sender: TObject);
var
  AStock: TStock;
  AStocks: TObjectList<TStock>;
begin
  ManagerApp.StartTrans;
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

    ManagerApp.LogicalInsertList<TStock>(AStocks, True, True, True);
  finally
    AStocks.Free;
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
    LStock.Free;
    LStock := nil;
  end;

  try
    LStockClone := nil;
    if ManagerApp.GetOne(LStock, LFilter, True) then
      LStockClone := ManagerApp.Clone(LStock);
  finally
    if LStock <> nil then
      LStock.Free;
    if LStockClone <> nil then
      FreeAndNil(LStockClone);
  end;
end;

procedure TfrmMain.btnGridExampleClick(Sender: TObject);
begin
  TfrmStockTransactions.Create(Self).ShowModal;
end;

procedure TfrmMain.btnGridListInvoicesClick(Sender: TObject);
begin
  TfrmInvoices<TInvoice>.Create(Self, TInvoice.Create, TInvoice.GetSelectSQL).ShowModal;
end;

procedure TfrmMain.btnGridListStocksClick(Sender: TObject);
begin
  TfrmGrid<TStock>.Create(Self, TStock.Create, TStock.GetSelectSQL).ShowModal;
end;

procedure TfrmMain.btnResetTablesClick(Sender: TObject);
begin
  ManagerApp.StartTrans;
  ManagerApp.DeleteBatch<TInvoice>('', False);
  ManagerApp.DeleteBatch<TStock>('', False);
  ManagerApp.CommitTrans;
end;

procedure TfrmMain.btnUpdateBusinessClick(Sender: TObject);
var
  LInvoice: TInvoice;
  LInvoiceLine: TInvoiceLine;
begin
  try
    if ManagerApp.LogicalSelectOne(LInvoice, '1=1', True, True, False) then
    begin
      LInvoice.AccountCode.Value := '120-001-015';

      LInvoiceLine := TInvoiceLine.Create();
      LInvoiceLine.StockCode.Value := 'PC2G';
      LInvoiceLine.Discount.Value := 20;
      LInvoiceLine.Quantity.Value := 1;
      LInvoiceLine.Price.Value := 20000;
      LInvoiceLine.VAT.Value := 20;
      LInvoice.AddLine(LInvoiceLine);
      ManagerApp.LogicalUpdateOne(LInvoice, False, True, False);
    end;
  finally
    LInvoice.Free;
  end;
end;

end.
