unit Stocks;

interface

uses Data.DB, Ths.Erp.Database.Table;

type
  TStock = class(TTable)
  private
    FStokKodu: TFieldDB;
    FStokAdi: TFieldDB;
  public
    property StokKodu: TFieldDB read FStokKodu write FStokKodu;
    property StokAdi: TFieldDB read FStokAdi write FStokAdi;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TStock; reintroduce; overload;
  end;

implementation

constructor TStock.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_stocks';
  Self.TableSourceCode := '1000';

  inherited;

  FStokKodu := TFieldDB.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FStokAdi := TFieldDB.Create('stok_adi', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
end;

destructor TStock.Destroy;
begin

  inherited;
end;

function TStock.Clone: TStock;
begin
  Result := TStock.Create();
  Result.CloneData(Self);
end;

end.
