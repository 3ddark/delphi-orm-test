unit AccountTransactions;

interface

uses Data.DB, Ths.Orm.Table, Ths.Orm.Manager;

type
  TAccountTransactionType = (attCredit, aatDebit);

  TAccountTransaction = class(TThsTable)
  private
    FAccountCode: TThsField;
    FTransactionDate: TThsField;
    FTransactionType: TThsField;
    FQuantity: TThsField;
    FPrice: TThsField;
    FcurrencyPrice: TThsField;
    FCurrency: TThsField;
    FInvoiceId: TThsField;
  public
    property AccountCode: TThsField read FAccountCode write FAccountCode;
    property TransactionDate: TThsField read FTransactionDate write FTransactionDate;
    property TransactionType: TThsField read FTransactionType write FTransactionType;
    property Quantity: TThsField read FQuantity write FQuantity;
    property Price: TThsField read FPrice write FPrice;
    property currencyPrice: TThsField read FcurrencyPrice write FcurrencyPrice;
    property Currency: TThsField read FCurrency write FCurrency;
    property InvoiceId: TThsField read FInvoiceId write FInvoiceId;

    constructor Create(); override;
    destructor Destroy; override;
  end;

implementation

constructor TAccountTransaction.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'account_transactions';
  Self.TableSourceCode := '1000';

  inherited;

  FAccountCode := TThsField.Create('account_code', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FTransactionDate := TThsField.Create('transaction_date', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FTransactionType := TThsField.Create('transaction_type', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);  //0 Alacak 1 Borc
  FQuantity := TThsField.Create('quantity', ftFloat, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPrice := TThsField.Create('price', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FcurrencyPrice := TThsField.Create('currency_price', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FCurrency := TThsField.Create('currency', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FInvoiceId := TThsField.Create('invoice_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
end;

destructor TAccountTransaction.Destroy;
begin

  inherited;
end;

end.
