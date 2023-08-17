unit Invoices;

interface

uses
  Data.DB, Ths.Orm.Table, Ths.Orm.Manager, System.Generics.Collections,
  StockTransactions;

type
  TInvoiceLine = class;

  TInvoice = class(TThsTable)
  private
    FFaturaNo: TThsField;
    FFaturaTarihi: TThsField;
    FHesapKodu: TThsField;
    FHesapIsmi: TThsField;
    FFaturaTipi: TThsField;
    FPara: TThsField;
    FInvoiceLines: TObjectList<TInvoiceLine>;
  public
    property FaturaNo: TThsField read FFaturaNo write FFaturaNo;
    property FaturaTarihi: TThsField read FFaturaTarihi write FFaturaTarihi;
    property HesapKodu: TThsField read FHesapKodu write FHesapKodu;
    property HesapIsmi: TThsField read FHesapIsmi write FHesapIsmi;
    property FaturaTipi: TThsField read FFaturaTipi write FFaturaTipi;
    property Para: TThsField read FPara write FPara;

    property InvoiceLines: TObjectList<TInvoiceLine> read FInvoiceLines write FInvoiceLines;

    constructor Create(); override;
    destructor Destroy; override;

    function AddLine(AInvoiceLine: TInvoiceLine): Boolean;
    function UpdateLine(AInvoiceLine: TInvoiceLine): Boolean; overload;
    function RemoveLine(AInvoiceLine: TInvoiceLine): Boolean; overload;

    function Clone: TInvoice; reintroduce; overload;

    class procedure BusinessSelect(AClass: TClass; var ATable: TThsTable; AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
    class procedure BusinessInsert(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean);
    class procedure BusinessUpdate(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean);
    class procedure BusinessDelete(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean);
  end;

  TInvoiceLine = class(TThsTable)
  private
    FHeaderId: TThsField;
    FStokKodu: TThsField;
    FIskonto: TThsField;
    FMiktar: TThsField;
    FFiyat: TThsField;
    FKdv: TThsField;

    FHeader: TInvoice;
    function CalculateAmount: Boolean;
    procedure AddStockTransaction(AManager: TEntityManager; APermissionCheck: Boolean);
    procedure UpdateStockTransaction(AManager: TEntityManager; APermissionCheck: Boolean);
  public
    property HeaderId: TThsField read FHeaderId write FHeaderId;
    property StokKodu: TThsField read FStokKodu write FStokKodu;
    property Iskonto: TThsField read FIskonto write FIskonto;
    property Miktar: TThsField read FMiktar write FMiktar;
    property Fiyat: TThsField read FFiyat write FFiyat;
    property Kdv: TThsField read FKdv write FKdv;

    property Header: TInvoice read FHeader write FHeader;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TInvoiceLine; reintroduce; overload;
  end;

implementation

class procedure TInvoice.BusinessSelect(AClass: TClass; var ATable: TThsTable; AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
var
  n1: Integer;
  LInvLs: TObjectList<TInvoiceLine>;
  AInvoice: TInvoice;
  AInvoiceLine: TInvoiceLine;
begin
  AManager.GetOne(ATable, AFilter, ALock, APermissionCheck);

  AInvoice := ATable as TInvoice;
  AInvoiceLine := TInvoiceLine.Create();
  AManager.GetList<TInvoiceLine>(LInvLs, AInvoiceLine.FHeaderId.QryName + '=' + AInvoice.Id.AsString, ALock, APermissionCheck);
  AInvoiceLine.DisposeOf;
  for n1 := 0 to LInvLs.Count-1 do
  begin
    AInvoice.AddLine(LInvLs.Items[n1]);
    LInvLs.Items[n1] := nil;
  end;
end;

class procedure TInvoice.BusinessInsert(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean);
var
  AInvoice: TInvoice;
  ALine: TInvoiceLine;
begin
  AInvoice := ATable as TInvoice;
  AManager.Insert(AInvoice, APermissionCheck);
  for ALine in AInvoice.InvoiceLines do
  begin
    ALine.HeaderId.Value := AInvoice.Id.Value;
    AManager.Insert(ALine, False);

    ALine.AddStockTransaction(AManager, False);
  end;
end;

class procedure TInvoice.BusinessUpdate(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean);
var
  AInvoice: TInvoice;
  ALine: TInvoiceLine;
begin
  AInvoice := ATable as TInvoice;
  AManager.Update(AInvoice, APermissionCheck);
  for ALine in AInvoice.InvoiceLines do
  begin
    if ALine.Id.Value <= 0 then
    begin
      ALine.HeaderId.Value := AInvoice.Id.Value;
      AManager.Insert(ALine, False);
      ALine.AddStockTransaction(AManager, False);
    end
    else
    begin
      AManager.Update(ALine, False);
      ALine.UpdateStockTransaction(AManager, False);
    end;
  end;
end;

class procedure TInvoice.BusinessDelete(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean);
begin

end;

constructor TInvoice.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_invoices';
  Self.TableSourceCode := '1000';

  inherited;

  FFaturaNo := TThsField.Create('fatura_no', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTarihi := TThsField.Create('fatura_tarihi', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapKodu := TThsField.Create('hesap_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapIsmi := TThsField.Create('hesa_ismi', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTipi := TThsField.Create('fatura_tipi', ftSmallint, -1, Self, [fpSelect, fpInsert, fpUpdate]);//0 Return, 1 Sale, 2 Export
  FPara := TThsField.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);

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
  Self.TableName := 'a_invoice_lines';
  Self.TableSourceCode := '1000';

  inherited;

  FHeaderId := TThsField.Create('header_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FStokKodu := TThsField.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FIskonto := TThsField.Create('iskonto', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FMiktar := TThsField.Create('miktar', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TThsField.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FKdv := TThsField.Create('kdv', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
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

procedure TInvoiceLine.AddStockTransaction(AManager: TEntityManager; APermissionCheck: Boolean);
var
  LStockTransaction: TStockTransaction;
begin
  LStockTransaction := TStockTransaction.Create();
  try
    LStockTransaction.StokKodu.Value := Self.StokKodu.Value;
    LStockTransaction.Tarih.Value := Self.Header.FaturaTarihi.Value;
    LStockTransaction.Tip.Value := Ord(sttCikis);
    LStockTransaction.Miktar.Value := Self.Miktar.Value;
    LStockTransaction.Fiyat.Value := Self.Fiyat.Value;
    LStockTransaction.DovizFiyat.Value := Self.StokKodu.Value;
    LStockTransaction.Para.Value := Self.Header.Para.Value;
    LStockTransaction.FaturaId.Value := Self.Header.Id.Value;
    LStockTransaction.FaturaDetayId.Value := Self.Id.Value;

    AManager.Insert(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.DisposeOf;
  end;
end;

procedure TInvoiceLine.UpdateStockTransaction(AManager: TEntityManager; APermissionCheck: Boolean);
var
  LStockTransaction: TStockTransaction;
begin
  LStockTransaction := TStockTransaction.Create();
  try
    AManager.GetOne(LStockTransaction, LStockTransaction.FaturaId.QryName + '=' + Self.FHeaderId.AsString + ' and ' +
                                       LStockTransaction.FaturaDetayId.QryName + '=' + Self.Id.AsString,
                                       True, False);
    LStockTransaction.StokKodu.Value := Self.StokKodu.Value;
    LStockTransaction.Tarih.Value := Self.Header.FaturaTarihi.Value;
    LStockTransaction.Tip.Value := Ord(sttCikis);
    LStockTransaction.Miktar.Value := Self.Miktar.Value;
    LStockTransaction.Fiyat.Value := Self.Fiyat.Value;
    LStockTransaction.DovizFiyat.Value := Self.StokKodu.Value;
    LStockTransaction.Para.Value := Self.Header.Para.Value;
    LStockTransaction.FaturaId.Value := Self.Header.Id.Value;
    LStockTransaction.FaturaDetayId.Value := Self.Id.Value;

    AManager.Update(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.DisposeOf;
  end;
end;

end.
