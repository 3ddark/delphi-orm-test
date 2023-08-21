unit StockTransactions;

interface

uses Data.DB, Ths.Orm.Table, Ths.Orm.Manager;

type
  TStockTransactionType = (sttGiris, sttCikis);

  TStockTransaction = class(TThsTable)
  private
    FStokKodu: TThsField;
    FTarih: TThsField;
    FTip: TThsField;
    FMiktar: TThsField;
    FFiyat: TThsField;
    FDovizFiyat: TThsField;
    FPara: TThsField;
    FFaturaId: TThsField;
    FFaturaDetayId: TThsField;
    FIrsaliyeId: TThsField;
    FIrsaliyeDetayId: TThsField;
  public
    property StokKodu: TThsField read FStokKodu write FStokKodu;
    property Tarih: TThsField read FTarih write FTarih;
    property Tip: TThsField read FTip write FTip;
    property Miktar: TThsField read FMiktar write FMiktar;
    property Fiyat: TThsField read FFiyat write FFiyat;
    property DovizFiyat: TThsField read FDovizFiyat write FDovizFiyat;
    property Para: TThsField read FPara write FPara;
    property FaturaId: TThsField read FFaturaId write FFaturaId;
    property FaturaDetayId: TThsField read FFaturaDetayId write FFaturaDetayId;
    property IrsaliyeId: TThsField read FIrsaliyeId write FIrsaliyeId;
    property IrsaliyeDetayId: TThsField read FIrsaliyeDetayId write FIrsaliyeDetayId;

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

  FStokKodu := TThsField.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FTarih := TThsField.Create('tarih', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FTip := TThsField.Create('tip', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);  //0 Giriş 1 Çıkış
  FMiktar := TThsField.Create('miktar', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TThsField.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FDovizFiyat := TThsField.Create('doviz_fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPara := TThsField.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaId := TThsField.Create('fatura_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaDetayId := TThsField.Create('fatura_detay_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FIrsaliyeId := TThsField.Create('irsaliye_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FIrsaliyeDetayId := TThsField.Create('irsaliye_detay_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
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
