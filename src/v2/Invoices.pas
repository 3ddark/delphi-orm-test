unit Invoices;

interface

uses
  System.SysUtils, System.Generics.Collections, Data.DB,
  Ths.Orm.Table, Ths.Orm.Manager, Ths.Orm.ManagerStack,
  StockTransactions;

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

    function AddLine(const AInvoiceLine: TInvoiceLine): Boolean;
    function UpdateLine(const AInvoiceLine: TInvoiceLine): Boolean; overload;
    function RemoveLine(const AInvoiceLine: TInvoiceLine): Boolean; overload;

    function BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean; override;
    function BusinessInsert(APermissionCheck: Boolean): Boolean; override;
    function BusinessUpdate(APermissionCheck: Boolean): Boolean; override;
    function BusinessDelete(APermissionCheck: Boolean): Boolean; override;

    class function GetSelectSQL: string; override;
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
  end;

implementation

function TInvoice.BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  LInvLs: TObjectList<TInvoiceLine>;
  AInvoiceLine: TInvoiceLine;
begin
  try
    AInvoiceLine := TInvoiceLine.Create();
    try
      Result := ManagerApp.GetList<TInvoiceLine>(LInvLs, AInvoiceLine.FHeaderId.QryName + '=' + Self.Id.AsString, ALock, APermissionCheck);
      if Result then
      begin
        Self.InvoiceLines.Free;
        Self.InvoiceLines := nil;
        Self.InvoiceLines := LInvLs;
      end;
    finally
      FreeAndNil(AInvoiceLine);
    end;

    if Result then
    begin
      for AInvoiceLine in Self.InvoiceLines do
        AInvoiceLine.Header := Self;
    end;
  except
    Result := False;
  end;
end;

function TInvoice.BusinessInsert(APermissionCheck: Boolean): Boolean;
var
  ALine: TInvoiceLine;
begin
  Result := ManagerApp.Insert(Self, APermissionCheck);
  try
    if Result then
    begin
      for ALine in Self.InvoiceLines do
      begin
        ALine.HeaderId.Value := Self.Id.Value;
        if ManagerApp.Insert(ALine, False) then
          ALine.AddStockTransaction(False);
      end;
    end;
  except
    Result := False;
  end;
end;

function TInvoice.BusinessUpdate(APermissionCheck: Boolean): Boolean;
var
  ALine: TInvoiceLine;
begin
  Result := ManagerApp.Update(Self, APermissionCheck);
  try
    if Result then
    begin
      for ALine in Self.InvoiceLines do
      begin
        if ALine.Id.Value <= 0 then
        begin
          ALine.HeaderId.Value := Self.Id.Value;
          if ManagerApp.Insert(ALine, False) then
            ALine.AddStockTransaction(False);
        end
        else
        begin
          if ManagerApp.Update(ALine, False) then
            ALine.UpdateStockTransaction(False);
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TInvoice.BusinessDelete(APermissionCheck: Boolean): Boolean;
begin
  Result := ManagerApp.Delete(Self, APermissionCheck);
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

  Self.FInvoiceLines := TObjectList<TInvoiceLine>.Create(True);
end;

destructor TInvoice.Destroy;
begin
  Self.FInvoiceLines.Free;
  inherited;
end;

class function TInvoice.GetSelectSQL: string;
var
  LTable: TInvoice;
begin
  LTable := TInvoice.Create;
  try
    Result := ManagerApp.PrepareSelectGridQuery(LTable, [
      TGridColumn.NewItem('Invoice No', LTable.FInvoiceNo),
      TGridColumn.NewItem('Invoice Date', LTable.FInvoiceDate),
      TGridColumn.NewItem('Account', LTable.FAccountCode)
    ]);
  finally
    LTable.Free;
  end;
end;

function TInvoice.AddLine(const AInvoiceLine: TInvoiceLine): Boolean;
begin
  AInvoiceLine.FHeader := Self;
  AInvoiceLine.CalculateAmount;
  Self.FInvoiceLines.Add(AInvoiceLine);
  Result := True;
end;

function TInvoice.UpdateLine(const AInvoiceLine: TInvoiceLine): Boolean;
begin
  AInvoiceLine.CalculateAmount;
  Result := True;
end;

function TInvoice.RemoveLine(const AInvoiceLine: TInvoiceLine): Boolean;
begin
  Self.FInvoiceLines.Remove(AInvoiceLine);
  Result := True;
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
  Self.FHeader := nil;
  inherited;
end;

function TInvoiceLine.CalculateAmount: Boolean;
//var
//  FAmount: Extended;
begin
//  FAmount := Self.FFiyat.AsFloat * (100-Self.FIskonto.AsFloat) / 100 * Self.FMiktar.AsFloat * ((Self.FKdv.AsFloat / 100) + 1);
  Result := True;
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

    ManagerApp.Insert(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.Free;
  end;
end;

procedure TInvoiceLine.UpdateStockTransaction(APermissionCheck: Boolean);
var
  LStockTransaction, LStockTransaction2: TStockTransaction;
begin
  LStockTransaction2 := TStockTransaction.Create();
  try
    ManagerApp.GetOne(LStockTransaction, LStockTransaction2.InvoiceId.QryName + '=' + Self.FHeaderId.AsString + ' and ' +
                                          LStockTransaction2.InvoiceLineId.QryName + '=' + Self.Id.AsString,
                                          True, False);
    LStockTransaction2.Free;
    LStockTransaction.StockCode.Value := Self.StockCode.Value;
    LStockTransaction.TransactionDate.Value := Self.Header.InvoiceDate.Value;
    LStockTransaction.Direction.Value := Ord(sttDirectionOut);
    LStockTransaction.Quantity.Value := Self.Quantity.Value;
    LStockTransaction.Price.Value := Self.Price.Value;
    LStockTransaction.CurrencyPrice.Value := 0;
    LStockTransaction.Currency.Value := Self.Header.Currency.Value;
    LStockTransaction.InvoiceId.Value := Self.Header.Id.Value;
    LStockTransaction.InvoiceLineId.Value := Self.Id.Value;

    ManagerApp.Update(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.Free;
  end;
end;

end.
