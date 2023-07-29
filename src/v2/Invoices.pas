unit Invoices;

interface

uses
  Data.DB, System.SysUtils, System.Generics.Collections,
  Ths.Erp.Database.Table, Ths.Erp.Database.Manager, StockTransactions;

type
  TInvoiceLine = class;

  TInvoice = class(TTable)
  private
    FFaturaNo: TFieldDB;
    FFaturaTarihi: TFieldDB;
    FHesapKodu: TFieldDB;
    FHesapIsmi: TFieldDB;
    FFaturaTipi: TFieldDB;
    FPara: TFieldDB;
    FInvoiceLines: TList<TInvoiceLine>;
  public
    property FaturaNo: TFieldDB read FFaturaNo write FFaturaNo;
    property FaturaTarihi: TFieldDB read FFaturaTarihi write FFaturaTarihi;
    property HesapKodu: TFieldDB read FHesapKodu write FHesapKodu;
    property HesapIsmi: TFieldDB read FHesapIsmi write FHesapIsmi;
    property FaturaTipi: TFieldDB read FFaturaTipi write FFaturaTipi;
    property Para: TFieldDB read FPara write FPara;

    property InvoiceLines: TList<TInvoiceLine> read FInvoiceLines write FInvoiceLines;

    constructor Create(); override;
    destructor Destroy; override;

    function AddLine(AInvoiceLine: TInvoiceLine): Boolean;
    function UpdateLine(AInvoiceLine: TInvoiceLine): Boolean; overload;
    function RemoveLine(AInvoiceLine: TInvoiceLine): Boolean; overload;

    function Clone: TInvoice; reintroduce; overload;

    class procedure BusinessSelect(AClass: TClass; var AList: TArray<TTable>; AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
    class procedure BusinessInsert(AManager: TEntityManager; ATables: TArray<TTable>; APermissionCheck: Boolean);
    class procedure BusinessUpdate(AManager: TEntityManager; ATables: TArray<TTable>; APermissionCheck: Boolean);
    class procedure BusinessDelete(AManager: TEntityManager; ATables: TArray<TTable>; APermissionCheck: Boolean);
  end;

  TInvoiceLine = class(TTable)
  private
    FHeaderId: TFieldDB;
    FStokKodu: TFieldDB;
    FIskonto: TFieldDB;
    FMiktar: TFieldDB;
    FFiyat: TFieldDB;
    FKdv: TFieldDB;

    FHeader: TInvoice;
    function CalculateAmount: Boolean;
    procedure AddStockTransaction(AManager: TEntityManager; APermissionCheck: Boolean);
    procedure UpdateStockTransaction(AManager: TEntityManager; APermissionCheck: Boolean);
  public
    property HeaderId: TFieldDB read FHeaderId write FHeaderId;
    property StokKodu: TFieldDB read FStokKodu write FStokKodu;
    property Iskonto: TFieldDB read FIskonto write FIskonto;
    property Miktar: TFieldDB read FMiktar write FMiktar;
    property Fiyat: TFieldDB read FFiyat write FFiyat;
    property Kdv: TFieldDB read FKdv write FKdv;

    property Header: TInvoice read FHeader write FHeader;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TInvoiceLine; reintroduce; overload;
  end;

implementation

class procedure TInvoice.BusinessSelect(AClass: TClass; var AList: TArray<TTable>; AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
var
  n1, n2: Integer;
  LInvLs: TArray<TTable>;
  AInvoice: TInvoice;
  AInvoiceLine: TInvoiceLine;
begin
  AManager.GetList(AClass, AList, AFilter, ALock, APermissionCheck);
  for n1 := 0 to Length(AList)-1 do
  begin
    AInvoice := AList[n1] as TInvoice;
    AInvoiceLine := TInvoiceLine.Create();
    AManager.GetList(TInvoiceLine, LInvLs, AInvoiceLine.FHeaderId.QryName + '=' + AInvoice.Id.AsString, ALock, APermissionCheck);
    AInvoiceLine.DisposeOf;
    for n2 := 0 to Length(LInvLs)-1 do
    begin
      AInvoiceLine := LInvLs[n2] as TInvoiceLine;
      AInvoice.AddLine(AInvoiceLine);
      LInvLs[n2] := nil;
    end;
  end;
end;

class procedure TInvoice.BusinessInsert(AManager: TEntityManager; ATables: TArray<TTable>; APermissionCheck: Boolean);
var
  ATable: TTable;
  AInvoice: TInvoice;
  ALine: TInvoiceLine;
begin
  for ATable in ATables do
  begin
    AInvoice := TInvoice(ATable);
    AManager.Insert(AInvoice, APermissionCheck);
    for ALine in AInvoice.InvoiceLines do
    begin
      ALine.HeaderId.Value := AInvoice.Id.Value;
      AManager.Insert(ALine, False);

      ALine.AddStockTransaction(AManager, False);
    end;
  end;
end;

class procedure TInvoice.BusinessUpdate(AManager: TEntityManager; ATables: TArray<TTable>; APermissionCheck: Boolean);
var
  ATable: TTable;
  AInvoice: TInvoice;
  ALine: TInvoiceLine;
begin
  for ATable in ATables do
  begin
    AInvoice := TInvoice(ATable);
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
end;

class procedure TInvoice.BusinessDelete(AManager: TEntityManager; ATables: TArray<TTable>; APermissionCheck: Boolean);
begin

end;

constructor TInvoice.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_invoices';
  Self.TableSourceCode := '1000';

  inherited;

  FFaturaNo := TFieldDB.Create('fatura_no', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTarihi := TFieldDB.Create('fatura_tarihi', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapKodu := TFieldDB.Create('hesap_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapIsmi := TFieldDB.Create('hesa_ismi', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTipi := TFieldDB.Create('fatura_tipi', ftSmallint, -1, Self, [fpSelect, fpInsert, fpUpdate]);//0 Iade, 1 Satış, 2 Ihracat
  FPara := TFieldDB.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);

  FInvoiceLines := TList<TInvoiceLine>.Create;
end;

destructor TInvoice.Destroy;
var
  ATable: TInvoiceLine;
begin
  for ATable in FInvoiceLines do
    ATable.DisposeOf;
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

  FHeaderId := TFieldDB.Create('header_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FStokKodu := TFieldDB.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FIskonto := TFieldDB.Create('iskonto', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FMiktar := TFieldDB.Create('miktar', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TFieldDB.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FKdv := TFieldDB.Create('kdv', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
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
