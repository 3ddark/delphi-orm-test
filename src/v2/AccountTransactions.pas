unit AccountTransactions;

interface

uses Data.DB, Ths.Orm.Table, Ths.Orm.Manager;

type
  TAccountTransactionType = (attAlacak, aatBorc);

  TAccountTransaction = class(TThsTable)
  private
    FHesapKodu: TThsField;
    FTarih: TThsField;
    FTip: TThsField;
    FMiktar: TThsField;
    FFiyat: TThsField;
    FDovizFiyat: TThsField;
    FPara: TThsField;
  public
    property StokKodu: TThsField read FHesapKodu write FHesapKodu;
    property Tarih: TThsField read FTarih write FTarih;
    property Tip: TThsField read FTip write FTip;
    property Miktar: TThsField read FMiktar write FMiktar;
    property Fiyat: TThsField read FFiyat write FFiyat;
    property DovizFiyat: TThsField read FDovizFiyat write FDovizFiyat;
    property Para: TThsField read FPara write FPara;

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

  FHesapKodu := TThsField.Create('hesap_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FTarih := TThsField.Create('tarih', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FTip := TThsField.Create('tip', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);  //0 Alacak 1 Borc
  FMiktar := TThsField.Create('miktar', ftFloat, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TThsField.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FDovizFiyat := TThsField.Create('doviz_fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FPara := TThsField.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
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
