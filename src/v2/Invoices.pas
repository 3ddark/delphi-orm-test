unit Invoices;

interface

uses
  System.SysUtils, Data.DB, Ths.Orm.Table, Ths.Orm.Manager, System.Generics.Collections,
  Ths.Orm.ManagerStack, StockTransactions;

type
  TInvoiceLine = class;

  TInvoice = class(TThsTable)
  private
    FFaturaNo: TThsField;
    FFaturaTarihi: TThsField;
    FHesapKodu: TThsField;
    FHesapIsmi: TThsField;
    FFaturaTipi: TThsField;
    FPara: TThsField;
    FInvoiceLines: TObjectList<TInvoiceLine>;
  public
    property FaturaNo: TThsField read FFaturaNo write FFaturaNo;
    property FaturaTarihi: TThsField read FFaturaTarihi write FFaturaTarihi;
    property HesapKodu: TThsField read FHesapKodu write FHesapKodu;
    property HesapIsmi: TThsField read FHesapIsmi write FHesapIsmi;
    property FaturaTipi: TThsField read FFaturaTipi write FFaturaTipi;
    property Para: TThsField read FPara write FPara;

    property InvoiceLines: TObjectList<TInvoiceLine> read FInvoiceLines write FInvoiceLines;

    constructor Create(); override;
    destructor Destroy; override;

    function AddLine(AInvoiceLine: TInvoiceLine): Boolean;
    function UpdateLine(AInvoiceLine: TInvoiceLine): Boolean; overload;
    function RemoveLine(AInvoiceLine: TInvoiceLine): Boolean; overload;

    function Clone: TInvoice; reintroduce; overload;

    function BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean; override;
    function BusinessInsert(APermissionCheck: Boolean): Boolean; override;
    function BusinessUpdate(APermissionCheck: Boolean): Boolean; override;
    function BusinessDelete(APermissionCheck: Boolean): Boolean; override;
  end;

  TInvoiceLine = class(TThsTable)
  private
    FHeaderId: TThsField;
    FStokKodu: TThsField;
    FIskonto: TThsField;
    FMiktar: TThsField;
    FFiyat: TThsField;
    FKdv: TThsField;

    FHeader: TInvoice;
    function CalculateAmount: Boolean;
    procedure AddStockTransaction(APermissionCheck: Boolean);
    procedure UpdateStockTransaction(APermissionCheck: Boolean);
  public
    property HeaderId: TThsField read FHeaderId write FHeaderId;
    property StokKodu: TThsField read FStokKodu write FStokKodu;
    property Iskonto: TThsField read FIskonto write FIskonto;
    property Miktar: TThsField read FMiktar write FMiktar;
    property Fiyat: TThsField read FFiyat write FFiyat;
    property Kdv: TThsField read FKdv write FKdv;

    property Header: TInvoice read FHeader write FHeader;

    constructor Create(); override;
    destructor Destroy; override;

    function Clone: TInvoiceLine; reintroduce; overload;
  end;

implementation

function TInvoice.BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  LInvLs: TObjectList<TInvoiceLine>;
  AInvoiceLine: TInvoiceLine;
begin
  AInvoiceLine := TInvoiceLine.Create();
  try
    Result := ManagerMain.GetList<TInvoiceLine>(LInvLs, AInvoiceLine.FHeaderId.QryName + '=' + Self.Id.AsString, ALock, APermissionCheck);
    Self.InvoiceLines.Free;
    Self.InvoiceLines := nil;
    Self.InvoiceLines := LInvLs;
  finally
    FreeAndNil(AInvoiceLine);
  end;

  for AInvoiceLine in Self.InvoiceLines do
    AInvoiceLine.Header := Self;
end;

function TInvoice.BusinessInsert(APermissionCheck: Boolean): Boolean;
var
  ALine: TInvoiceLine;
begin
  Result := ManagerMain.Insert(Self, APermissionCheck);
  try
    for ALine in Self.InvoiceLines do
    begin
      ALine.HeaderId.Value := Self.Id.Value;
      ManagerMain.Insert(ALine, False);

      ALine.AddStockTransaction(False);
    end;
  except
    Result := False;
  end;
end;

function TInvoice.BusinessUpdate(APermissionCheck: Boolean): Boolean;
var
  ALine: TInvoiceLine;
begin
  ManagerMain.Update(Self, APermissionCheck);
  for ALine in Self.InvoiceLines do
  begin
    if ALine.Id.Value <= 0 then
    begin
      ALine.HeaderId.Value := Self.Id.Value;
      ManagerMain.Insert(ALine, False);
      ALine.AddStockTransaction(False);
    end
    else
    begin
      ManagerMain.Update(ALine, False);
      ALine.UpdateStockTransaction(False);
    end;
  end;
  Result := True;
end;

function TInvoice.BusinessDelete(APermissionCheck: Boolean): Boolean;
begin
  Result := True;
end;

constructor TInvoice.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'a_invoices';
  Self.TableSourceCode := '1000';

  inherited;

  FFaturaNo := TThsField.Create('fatura_no', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTarihi := TThsField.Create('fatura_tarihi', ftDateTime, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapKodu := TThsField.Create('hesap_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FHesapIsmi := TThsField.Create('hesa_ismi', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FFaturaTipi := TThsField.Create('fatura_tipi', ftSmallint, -1, Self, [fpSelect, fpInsert, fpUpdate]);//0 Return, 1 Sale, 2 Export
  FPara := TThsField.Create('para', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);

  FInvoiceLines := TObjectList<TInvoiceLine>.Create;
end;

destructor TInvoice.Destroy;
begin
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

  FHeaderId := TThsField.Create('header_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FStokKodu := TThsField.Create('stok_kodu', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FIskonto := TThsField.Create('iskonto', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FMiktar := TThsField.Create('miktar', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FFiyat := TThsField.Create('fiyat', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FKdv := TThsField.Create('kdv', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
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

procedure TInvoiceLine.AddStockTransaction(APermissionCheck: Boolean);
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

    ManagerMain.Insert(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.DisposeOf;
  end;
end;

procedure TInvoiceLine.UpdateStockTransaction(APermissionCheck: Boolean);
var
  LStockTransaction: TStockTransaction;
begin
  LStockTransaction := TStockTransaction.Create();
  try
    ManagerMain.GetOne(LStockTransaction, LStockTransaction.FaturaId.QryName + '=' + Self.FHeaderId.AsString + ' and ' +
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

    ManagerMain.Update(LStockTransaction, APermissionCheck);
  finally
    LStockTransaction.DisposeOf;
  end;
end;

end.
