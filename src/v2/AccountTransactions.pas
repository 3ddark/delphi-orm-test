unit AccountTransactions;

interface

uses Data.DB, Ths.Erp.Database.Table, Ths.Erp.Database.Manager;

type
  TAccountTransactionType = (attAlacak, aatBorc);

  TAccountTransaction = class(TTable)
  private
    FHesapKodu: TFieldDB;
    FTarih: TFieldDB;
    FTip: TFieldDB;
    FMiktar: TFieldDB;
    FFiyat: TFieldDB;
    FDovizFiyat: TFieldDB;
    FPara: TFieldDB;
  public
    property StokKodu: TFieldDB read FHesapKodu write FHesapKodu;
    property Tarih: TFieldDB read FTarih write FTarih;
    property Tip: TFieldDB read FTip write FTip;
    property Miktar: TFieldDB read FMiktar write FMiktar;
    property Fiyat: TFieldDB read FFiyat write FFiyat;
    property DovizFiyat: TFieldDB read FDovizFiyat write FDovizFiyat;
    property Para: TFieldDB read FPara write FPara;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TAccountTransaction; reintroduce; overload;
  end;

implementation

constructor TAccountTransaction.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_account_transactions';
  Self.TableSourceCode := '1000';

  inherited;

  FHesapKodu := TFieldDB.Create('hesap_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FTarih := TFieldDB.Create('tarih', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FTip := TFieldDB.Create('tip', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);  //0 Alacak 1 Borc
  FMiktar := TFieldDB.Create('miktar', ftFloat, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TFieldDB.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FDovizFiyat := TFieldDB.Create('doviz_fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPara := TFieldDB.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
end;

destructor TAccountTransaction.Destroy;
begin

  inherited;
end;

function TAccountTransaction.Clone: TAccountTransaction;
begin
  Result := TAccountTransaction.Create();
  Result.CloneData(Self);
end;

end.
