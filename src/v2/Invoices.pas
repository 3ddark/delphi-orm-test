unit Invoices;

interface

uses
  System.SysUtils, Data.DB, Ths.Orm.Table, Ths.Orm.Manager, System.Generics.Collections,
  Ths.Orm.ManagerStack, StockTransactions;

type
  TInvoiceLine = class;

  TInvoice = class(TThsTable)
  private
    FInvoiceNo: TThsField;
    FInvoiceDate: TThsField;
    FAccountCode: TThsField;
    FAccountName: TThsField;
    FDocumentType: TThsField;
    FCurrency: TThsField;
    FInvoiceLines: TObjectList<TInvoiceLine>;
  public
    property InvoiceNo: TThsField read FInvoiceNo write FInvoiceNo;
    property InvoiceDate: TThsField read FInvoiceDate write FInvoiceDate;
    property AccountCode: TThsField read FAccountCode write FAccountCode;
    property AccountName: TThsField read FAccountName write FAccountName;
    property DocumentType: TThsField read FDocumentType write FDocumentType;
    property Currency: TThsField read FCurrency write FCurrency;

    property InvoiceLines: TObjectList<TInvoiceLine> read FInvoiceLines write FInvoiceLines;

    constructor Create(); override;
    destructor Destroy; override;

    function AddLine(AInvoiceLine: TInvoiceLine): Boolean;
    function UpdateLine(AInvoiceLine: TInvoiceLine): Boolean; overload;
    function RemoveLine(AInvoiceLine: TInvoiceLine): Boolean; overload;

    function Clone: TInvoice; reintroduce; overload;

    function BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean; override;
    function BusinessInsert(APermissionCheck: Boolean): Boolean; override;
    function BusinessUpdate(APermissionCheck: Boolean): Boolean; override;
    function BusinessDelete(APermissionCheck: Boolean): Boolean; override;
  end;

  TInvoiceLine = class(TThsTable)
  private
    FHeaderId: TThsField;
    FStockCode: TThsField;
    FDiscount: TThsField;
    FQuantity: TThsField;
    FPrice: TThsField;
    FVAT: TThsField;

    FHeader: TInvoice;
    function CalculateAmount: Boolean;
    procedure AddStockTransaction(APermissionCheck: Boolean);
    procedure UpdateStockTransaction(APermissionCheck: Boolean);
  public
    property HeaderId: TThsField read FHeaderId write FHeaderId;
    property StockCode: TThsField read FStockCode write FStockCode;
    property Discount: TThsField read FDiscount write FDiscount;
    property Quantity: TThsField read FQuantity write FQuantity;
    property Price: TThsField read FPrice write FPrice;
    property VAT: TThsField read FVAT write FVAT;

    property Header: TInvoice read FHeader write FHeader;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TInvoiceLine; reintroduce; overload;
  end;

implementation

function TInvoice.BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  LInvLs: TObjectList<TInvoiceLine>;
  AInvoiceLine: TInvoiceLine;
begin
  AInvoiceLine := TInvoiceLine.Create();
  try
    Result := ManagerMain.GetList<TInvoiceLine>(LInvLs, AInvoiceLine.FHeaderId.QryName + '=' + Self.Id.AsString, ALock, APermissionCheck);
    Self.InvoiceLines.Free;
    Self.InvoiceLines := nil;
    Self.InvoiceLines := LInvLs;
  finally
    FreeAndNil(AInvoiceLine);
  end;

  for AInvoiceLine in Self.InvoiceLines do
    AInvoiceLine.Header := Self;
end;

function TInvoice.BusinessInsert(APermissionCheck: Boolean): Boolean;
var
  ALine: TInvoiceLine;
begin
  Result := ManagerMain.Insert(Self, APermissionCheck);
  try
    for ALine in Self.InvoiceLines do
    begin
      ALine.HeaderId.Value := Self.Id.Value;
      ManagerMain.Insert(ALine, False);

      ALine.AddStockTransaction(False);
    end;
  except
    Result := False;
  end;
end;

function TInvoice.BusinessUpdate(APermissionCheck: Boolean): Boolean;
var
  ALine: TInvoiceLine;
begin
  ManagerMain.Update(Self, APermissionCheck);
  for ALine in Self.InvoiceLines do
  begin
    if ALine.Id.Value <= 0 then
    begin
      ALine.HeaderId.Value := Self.Id.Value;
      ManagerMain.Insert(ALine, False);
      ALine.AddStockTransaction(False);
    end
    else
    begin
      ManagerMain.Update(ALine, False);
      ALine.UpdateStockTransaction(False);
    end;
  end;
  Result := True;
end;

function TInvoice.BusinessDelete(APermissionCheck: Boolean): Boolean;
begin
  Result := True;
end;

constructor TInvoice.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'invoices';
  Self.TableSourceCode := '1000';

  inherited;

  FInvoiceNo := TThsField.Create('invoice_no', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FInvoiceDate := TThsField.Create('invoice_date', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FAccountCode := TThsField.Create('account_code', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FAccountName := TThsField.Create('account_name', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FDocumentType := TThsField.Create('document_type', ftSmallint, -1, Self, [fpSelect, fpInsert, fpUpdate]);//0 Return, 1 Sale, 2 Export
  FCurrency := TThsField.Create('currency', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);

  FInvoiceLines := TObjectList<TInvoiceLine>.Create;
end;

destructor TInvoice.Destroy;
begin
  FInvoiceLines.DisposeOf;
  inherited;
end;

function TInvoice.AddLine(AInvoiceLine: TInvoiceLine): Boolean;
begin
  AInvoiceLine.FHeader := Self;
  AInvoiceLine.CalculateAmount;
  FInvoiceLines.Add(AInvoiceLine);
  Result := True;
end;

function TInvoice.UpdateLine(AInvoiceLine: TInvoiceLine): Boolean;
begin
  AInvoiceLine.CalculateAmount;
  Result := True;
end;

function TInvoice.RemoveLine(AInvoiceLine: TInvoiceLine): Boolean;
begin
  FInvoiceLines.Remove(AInvoiceLine);
  Result := True;
end;

function TInvoice.Clone: TInvoice;
begin
  Result := TInvoice.Create();
  Result.CloneData(Self);
end;

{ TInvoiceLine }

constructor TInvoiceLine.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'invoice_lines';
  Self.TableSourceCode := '1000';

  inherited;

  FHeaderId := TThsField.Create('header_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FStockCode := TThsField.Create('stock_code', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FDiscount := TThsField.Create('discount', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FQuantity := TThsField.Create('quantity', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPrice := TThsField.Create('price', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FVAT := TThsField.Create('vat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
end;

destructor TInvoiceLine.Destroy;
begin
  FHeader := nil;
  inherited;
end;

function TInvoiceLine.CalculateAmount: Boolean;
//var
//  FAmount: Extended;
begin
//  FAmount := Self.FFiyat.AsFloat * (100-Self.FIskonto.AsFloat) / 100 * Self.FMiktar.AsFloat * ((Self.FKdv.AsFloat / 100) + 1);
  Result := True;
end;

function TInvoiceLine.Clone: TInvoiceLine;
begin
  Result := TInvoiceLine.Create();
  Result.CloneData(Self);
end;

procedure TInvoiceLine.AddStockTransaction(APermissionCheck: Boolean);
var
  LStockTransaction: TStockTransaction;
begin
  LStockTransaction := TStockTransaction.Create();
  try
    LStockTransaction.StockCode.Value := Self.StockCode.Value;
    LStockTransaction.TransactionDate.Value := Self.Header.InvoiceDate.Value;
    LStockTransaction.Direction.Value := Ord(sttDirectionOut);
    LStockTransaction.Quantity.Value := Self.Quantity.Value;
    LStockTransaction.Price.Value := Self.Price.Value;
    LStockTransaction.CurrencyPrice.Value := 0;
    LStockTransaction.Currency.Value := Self.Header.Currency.Value;
    LStockTransaction.InvoiceId.Value := Self.Header.Id.Value;
    LStockTransaction.InvoiceLineId.Value := Self.Id.Value;

    ManagerMain.Insert(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.DisposeOf;
  end;
end;

procedure TInvoiceLine.UpdateStockTransaction(APermissionCheck: Boolean);
var
  LStockTransaction, LStockTransaction2: TStockTransaction;
begin
  LStockTransaction2 := TStockTransaction.Create();
  try
    ManagerMain.GetOne(LStockTransaction, LStockTransaction2.InvoiceId.QryName + '=' + Self.FHeaderId.AsString + ' and ' +
                                          LStockTransaction2.InvoiceLineId.QryName + '=' + Self.Id.AsString,
                                          True, False);
    LStockTransaction2.DisposeOf;
    LStockTransaction.StockCode.Value := Self.StockCode.Value;
    LStockTransaction.TransactionDate.Value := Self.Header.InvoiceDate.Value;
    LStockTransaction.Direction.Value := Ord(sttDirectionOut);
    LStockTransaction.Quantity.Value := Self.Quantity.Value;
    LStockTransaction.Price.Value := Self.Price.Value;
    LStockTransaction.CurrencyPrice.Value := 0;
    LStockTransaction.Currency.Value := Self.Header.Currency.Value;
    LStockTransaction.InvoiceId.Value := Self.Header.Id.Value;
    LStockTransaction.InvoiceLineId.Value := Self.Id.Value;

    ManagerMain.Update(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.DisposeOf;
  end;
end;

end.
