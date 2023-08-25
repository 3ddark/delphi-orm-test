unit Stocks;

interface

uses Data.DB, Ths.Orm.Table;

type
  TStock = class(TThsTable)
  private
    FStockCode: TThsField;
    FStockName: TThsField;
  public
    property StockCode: TThsField read FStockCode write FStockCode;
    property StockName: TThsField read FStockName write FStockName;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TStock; reintroduce; overload;
  end;

implementation

constructor TStock.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'stocks';
  Self.TableSourceCode := '1000';

  inherited;

  FStockCode := TThsField.Create('stock_code', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FStockName := TThsField.Create('stock_name', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
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
