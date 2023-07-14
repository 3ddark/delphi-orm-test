unit Invoices;

interface

uses Data.DB, Ths.Erp.Database.Table, Ths.Erp.Database.Manager;

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
    FInvoiceLines: TArray<TInvoiceLine>;
  public
    property FaturaNo: TFieldDB read FFaturaNo write FFaturaNo;
    property FaturaTarihi: TFieldDB read FFaturaTarihi write FFaturaTarihi;
    property HesapKodu: TFieldDB read FHesapKodu write FHesapKodu;
    property HesapIsmi: TFieldDB read FHesapIsmi write FHesapIsmi;
    property FaturaTipi: TFieldDB read FFaturaTipi write FFaturaTipi;
    property Para: TFieldDB read FPara write FPara;

    property InvoiceLines: TArray<TInvoiceLine> read FInvoiceLines write FInvoiceLines;

    constructor Create; override;
    destructor Destroy; override;

    function Clone: TInvoice; reintroduce; overload;
    class procedure BusinessSelect(AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
    class procedure BusinessInsert(AManager: TEntityManager; APermissionCheck: Boolean);
    class procedure BusinessUpdate(AManager: TEntityManager; APermissionCheck: Boolean);
    class procedure BusinessDelete(AManager: TEntityManager; APermissionCheck: Boolean);
  end;

  TInvoiceLine = class(TTable)
  private
    FStokKodu: TFieldDB;
    FIskonto: TFieldDB;
    FMiktar: TFieldDB;
    FFiyat: TFieldDB;
    FKdv: TFieldDB;
    FPara: TFieldDB;

    FHeader: TInvoice;
  public
    property StokKodu: TFieldDB read FStokKodu write FStokKodu;
    property Iskonto: TFieldDB read FIskonto write FIskonto;
    property Miktar: TFieldDB read FMiktar write FMiktar;
    property Fiyat: TFieldDB read FFiyat write FFiyat;
    property Kdv: TFieldDB read FKdv write FKdv;
    property Para: TFieldDB read FPara write FPara;

    property Header: TInvoice read FHeader;

    constructor Create; override;
    destructor Destroy; override;

    function Clone: TInvoiceLine; reintroduce; overload;
  end;

implementation

class procedure TInvoice.BusinessDelete(AManager: TEntityManager; APermissionCheck: Boolean);
begin

end;

class procedure TInvoice.BusinessInsert(AManager: TEntityManager; APermissionCheck: Boolean);
begin

end;

class procedure TInvoice.BusinessSelect(AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
begin

end;

class procedure TInvoice.BusinessUpdate(AManager: TEntityManager; APermissionCheck: Boolean);
begin

end;

constructor TInvoice.Create;
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_invoices';
  Self.TableSourceCode := '1000';

  inherited;

  FFaturaNo := TFieldDB.Create('fatura_no', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTarihi := TFieldDB.Create('fatura_tarihi', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapKodu := TFieldDB.Create('hesap_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapIsmi := TFieldDB.Create('hesa_ismi', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTipi := TFieldDB.Create('fatura_tipi', ftSmallint, -1, Self, [fpSelect, fpInsert, fpUpdate]);//0 Iade, 1 Satýþ, 2 Ihracat
  FPara := TFieldDB.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);

  SetLength(FInvoiceLines, 0);
end;

destructor TInvoice.Destroy;
var
  ATable: TInvoiceLine;
begin
  for ATable in FInvoiceLines do
    ATable.DisposeOf;
  SetLength(FInvoiceLines, 0);
  inherited;
end;

function TInvoice.Clone: TInvoice;
begin
  Result := TInvoice.Create;
  Result.CloneData(Self);
end;

{ TInvoiceLine }

constructor TInvoiceLine.Create;
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_invoice_lines';
  Self.TableSourceCode := '1000';

  inherited;

  FStokKodu := TFieldDB.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FIskonto := TFieldDB.Create('iskonto', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FMiktar := TFieldDB.Create('miktar', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TFieldDB.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FKdv := TFieldDB.Create('kdv', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPara := TFieldDB.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
end;

destructor TInvoiceLine.Destroy;
begin
  FHeader := nil;
  inherited;
end;

function TInvoiceLine.Clone: TInvoiceLine;
begin
  Result := TInvoiceLine.Create;
  Result.CloneData(Self);
end;

end.
