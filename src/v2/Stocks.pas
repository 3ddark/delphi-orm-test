unit Stocks;

interface

uses Data.DB, Ths.Orm.Table;

type
  TStock = class(TThsTable)
  private
    FStokKodu: TThsField;
    FStokAdi: TThsField;
  public
    property StokKodu: TThsField read FStokKodu write FStokKodu;
    property StokAdi: TThsField read FStokAdi write FStokAdi;

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

  FStokKodu := TThsField.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FStokAdi := TThsField.Create('stok_adi', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
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
