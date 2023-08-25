unit StockTransactions;

interface

uses Data.DB, Ths.Orm.Table, Ths.Orm.Manager;

type
  TStockTransactionType = (sttDirectionIn, sttDirectionOut);

  TStockTransaction = class(TThsTable)
  private
    FStockCode: TThsField;
    FTransactionDate: TThsField;
    FDirection: TThsField;
    FQuantity: TThsField;
    FPrice: TThsField;
    FCurrencyPrice: TThsField;
    FCurrency: TThsField;
    FInvoiceId: TThsField;
    FInvoiceLineId: TThsField;
    FWaybillId: TThsField;
    FWaybillLineId: TThsField;
  public
    property StockCode: TThsField read FStockCode write FStockCode;
    property TransactionDate: TThsField read FTransactionDate write FTransactionDate;
    property Direction: TThsField read FDirection write FDirection;
    property Quantity: TThsField read FQuantity write FQuantity;
    property Price: TThsField read FPrice write FPrice;
    property CurrencyPrice: TThsField read FCurrencyPrice write FCurrencyPrice;
    property Currency: TThsField read FCurrency write FCurrency;
    property InvoiceId: TThsField read FInvoiceId write FInvoiceId;
    property InvoiceLineId: TThsField read FInvoiceLineId write FInvoiceLineId;
    property WaybillId: TThsField read FWaybillId write FWaybillId;
    property WaybillLineId: TThsField read FWaybillLineId write FWaybillLineId;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TStockTransaction; reintroduce; overload;
  end;

implementation

constructor TStockTransaction.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'stock_transactions';
  Self.TableSourceCode := '1000';

  inherited;

  FStockCode := TThsField.Create('stock_code', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FTransactionDate := TThsField.Create('transaction_date', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FDirection := TThsField.Create('direction', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);  //0 Giriş 1 Çıkış
  FQuantity := TThsField.Create('quantity', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPrice := TThsField.Create('price', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FCurrencyPrice := TThsField.Create('currency_price', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FCurrency := TThsField.Create('currency', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FInvoiceId := TThsField.Create('invoice_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FInvoiceLineId := TThsField.Create('invoice_line_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FWaybillId := TThsField.Create('waybill_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FWaybillLineId := TThsField.Create('waybill_line_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
end;

destructor TStockTransaction.Destroy;
begin

  inherited;
end;

function TStockTransaction.Clone: TStockTransaction;
begin
  Result := TStockTransaction.Create();
  Result.CloneData(Self);
end;

end.
