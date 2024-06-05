unit Stocks;

interface

uses
  System.SysUtils, System.Generics.Collections, Data.DB,
  Ths.Orm.Table, Ths.Orm.Manager, Ths.Orm.ManagerStack;

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

    class function GetSelectSQL: string; override;
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

class function TStock.GetSelectSQL: string;
var
  LTable: TStock;
begin
  LTable := TStock.Create;
  try
    Result := ManagerApp.PrepareSelectGridQuery(LTable, [
      TGridColumn.NewItem('ID', LTable.Id),
      TGridColumn.NewItem('Stock Code', LTable.FStockCode),
      TGridColumn.NewItem('Stock Name', LTable.FStockName)
    ]);
  finally
    LTable.Free;
  end;
end;

end.
