unit StockTransactions;

interface

uses Data.DB, Ths.Erp.Database.Table;

type
  TStockTransactionType = (sttGiris, sttCikis);

  TStockTransaction = class(TTable)
  private
    FStokKodu: TFieldDB;
    FTarih: TFieldDB;
    FTip: TFieldDB;
    FMiktar: TFieldDB;
    FFiyat: TFieldDB;
    FDovizFiyat: TFieldDB;
    FPara: TFieldDB;
    FFaturaId: TFieldDB;
    FFaturaDetayId: TFieldDB;
  public
    property StokKodu: TFieldDB read FStokKodu write FStokKodu;
    property Tarih: TFieldDB read FTarih write FTarih;
    property Tip: TFieldDB read FTip write FTip;
    property Miktar: TFieldDB read FMiktar write FMiktar;
    property Fiyat: TFieldDB read FFiyat write FFiyat;
    property DovizFiyat: TFieldDB read FDovizFiyat write FDovizFiyat;
    property Para: TFieldDB read FPara write FPara;
    property FaturaId: TFieldDB read FFaturaId write FFaturaId;
    property FaturaDetayId: TFieldDB read FFaturaDetayId write FFaturaDetayId;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TStockTransaction; reintroduce; overload;
  end;

implementation

constructor TStockTransaction.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_stock_transactions';
  Self.TableSourceCode := '1000';

  inherited;

  FStokKodu := TFieldDB.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FTarih := TFieldDB.Create('tarih', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FTip := TFieldDB.Create('tip', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);  //0 Giriş 1 Çıkış
  FMiktar := TFieldDB.Create('miktar', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TFieldDB.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FDovizFiyat := TFieldDB.Create('doviz_fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPara := TFieldDB.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaId := TFieldDB.Create('fatura_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaDetayId := TFieldDB.Create('fatura_detay_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
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
