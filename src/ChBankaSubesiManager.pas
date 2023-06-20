unit ChBankaSubesiManager;

interface

uses
  Generics.Collections, SysUtils, StrUtils, Data.DB,
  ZAbstractConnection, ZAbstractDataset, ZDataset,
  Entity, EntityManager, ChBankalar;

type
  TChBankaSubesiManager = class(TEntityManager<TChBankaSubesi>)
  private
    FModel: TChBankaSubesi;
  protected
    function BusinessGet(const AFilter: string = ''; ALock: Boolean = False): TList<TChBankaSubesi>; override;
    procedure BusinessAdd(AModels: TArray<TChBankaSubesi>); override;
    procedure BusinessUpdate(AModels: TArray<TChBankaSubesi>); override;
    procedure BusinessDelete(const AIDs: TArray<Int64>); override;
  public
    function GetById(AID: Int64; ALock: Boolean = False): TChBankaSubesi; override;
    function GetList(const AIDs: TArray<Int64>; ALock: Boolean = False): TList<TChBankaSubesi>; reintroduce; overload;
    function GetList(const AFilter: string = ''; ALock: Boolean = False): TList<TChBankaSubesi>; reintroduce; overload;
    procedure Add(AModel: TChBankaSubesi); override;
    procedure AddBatch(AModels: TArray<TChBankaSubesi>); override;
    procedure Update(AModel: TChBankaSubesi); override;
    procedure UpdateBatch(AModels: TArray<TChBankaSubesi>); override;
    procedure Delete(const AID: Int64); override;
    procedure DeleteBatch(const AIDs: TArray<Int64>); reintroduce; overload;
    procedure DeleteBatch(const AFilter: string = ''); reintroduce; overload;

    constructor Create(AConnection: TZAbstractConnection); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses ChBankaManager;

constructor TChBankaSubesiManager.Create(AConnection: TZAbstractConnection);
begin
  FModel := TChBankaSubesi.Create;
  inherited Create(AConnection);
end;

function TChBankaSubesiManager.GetById(AId: Int64; ALock: Boolean): TChBankaSubesi;
var
  LCmd: TZQuery;
  LBankaManager: TChBankaManager;
begin
  Result := TChBankaSubesi.Create;

  if AId = 0 then
    Exit;

  LCmd := TZQuery.Create(nil);
  LBankaManager := nil;
  if Self.LazzyLoading then
    LBankaManager := TChBankaManager.Create(Self.Connection);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := Result.GetQuery(AID);
    LCmd.Prepare;
    if LCmd.Prepared then
    begin
      LCmd.Open;
      while not LCmd.Eof do
      begin
        Result.SetValuesFromDbFields(LCmd.Fields);
        if Self.LazzyLoading then
          Result.Banka := LBankaManager.GetById(Result.BankaID);
        LCmd.Next;
      end;
    end;
  finally
    LCmd.Free;
    if Self.LazzyLoading then
      LBankaManager.Free;
  end;
end;

function TChBankaSubesiManager.GetList(const AIDs: TArray<Int64>; ALock: Boolean): TList<TChBankaSubesi>;
var
  LFilter: string;
  n1: Integer;
begin
  LFilter := '';
  for n1 := 0 to Length(AIDs)-1 do
    LFilter := LFilter + AIDs[n1].ToString + ',';

  if LFilter <> '' then
    LFilter := ' and id in (' + LeftStr(LFilter, Length(LFilter)-1) + ')';

  Result := GetList(LFilter);
end;

function TChBankaSubesiManager.GetList(const AFilter: string; ALock: Boolean): TList<TChBankaSubesi>;
var
  LCmd: TZQuery;
  LItem: TChBankaSubesi;
  LTableName: string;
begin
  Result := TList<TChBankaSubesi>.Create;

  if (Trim(AFilter) <> '') and (LeftStr(Trim(AFilter), 3) <> 'and') and (LeftStr(Trim(AFilter), 3) <> 'AND') then
    raise Exception.Create('Filtreler and ile baþlamak zorunda!!!');

  LTableName := Self.FModel.GetTableName;

  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('SELECT ' + LTableName + '.id FROM ' + LTableName + ' WHERE 1=1 ' + AFilter);
    LCmd.Prepare;
    if LCmd.Prepared then
    begin
      LCmd.Open;
      LCmd.First;
      while not LCmd.Eof do
      begin
        LItem := Self.GetById(LCmd.Fields.Fields[0].AsLargeInt, ALock);
        Result.Add(LItem);
        LCmd.Next;
      end;

    end;
  finally
    LCmd.Free;
  end;
end;

procedure TChBankaSubesiManager.Add(AModel: TChBankaSubesi);
var
  LCmd: TZQuery;
  LBankaManager: TChBankaManager;
begin
  if AModel = nil then
    Exit;

  LCmd := TZQuery.Create(nil);
  LBankaManager := nil;
  if Self.LazzyLoading then
    LBankaManager := TChBankaManager.Create(Self.Connection);
  try
    if Self.LazzyLoading then
      if AModel.Banka.ID < 1 then
        LBankaManager.Add(AModel.Banka);

    LCmd.Connection := Connection;
    LCmd.SQL.Text := AModel.AddQuery;
    LCmd.Prepare;
    if LCmd.Prepared then
    begin
      LCmd.Open;
      AModel.Id := LCmd.Fields.Fields[0].AsLargeInt;
    end;
  finally
    LCmd.Free;
    if Self.LazzyLoading then
      LBankaManager.Free;
  end;
end;

procedure TChBankaSubesiManager.AddBatch(AModels: TArray<TChBankaSubesi>);
var
  n1: Integer;
begin
  if AModels = nil then
    Exit;

  for n1 := Low(AModels) to High(AModels) do
    Add(AModels[n1]);
end;

function TChBankaSubesiManager.BusinessGet(const AFilter: string; ALock: Boolean): TList<TChBankaSubesi>;
begin
  Result := Self.GetList(AFilter, ALock);
end;

procedure TChBankaSubesiManager.BusinessAdd(AModels: TArray<TChBankaSubesi>);
begin
  Self.AddBatch(AModels);
end;

procedure TChBankaSubesiManager.BusinessUpdate(AModels: TArray<TChBankaSubesi>);
begin
  Self.UpdateBatch(AModels);
end;

procedure TChBankaSubesiManager.BusinessDelete(const AIDs: TArray<Int64>);
begin
  Self.DeleteBatch(AIDs);
end;

procedure TChBankaSubesiManager.Update(AModel: TChBankaSubesi);
var
  LCmd: TZQuery;
  LBankaManager: TChBankaManager;
begin
  if AModel = nil then
    Exit;

  LCmd := TZQuery.Create(nil);
  LBankaManager := nil;
  if Self.LazzyLoading then
    LBankaManager := TChBankaManager.Create(Self.Connection);
  try
    if Self.LazzyLoading then
      if AModel.Banka.ID < 1 then
        LBankaManager.Add(AModel.Banka);

    LCmd.Connection := Connection;
    LCmd.SQL.Text := AModel.UpdateQuery;
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
    if Self.LazzyLoading then
      LBankaManager.Free;
  end;
end;

procedure TChBankaSubesiManager.UpdateBatch(AModels: TArray<TChBankaSubesi>);
var
  n1: Integer;
begin
  if AModels = nil then
    Exit;

  for n1 := Low(AModels) to High(AModels) do
    Update(AModels[n1]);
end;

procedure TChBankaSubesiManager.Delete(const AID: Int64);
var
  LCmd: TZQuery;
  LTableName: string;
begin
  if AID = 0 then
    Exit;

  LTableName := Self.FModel.GetTableName;

  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := Self.FModel.DeleteQuery(AID);
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
  end;
end;

procedure TChBankaSubesiManager.DeleteBatch(const AFilter: string);
var
  LCmd: TZQuery;
  LTableName: string;
begin
  if Trim(AFilter) = '' then
    Exit;

  LTableName := Self.FModel.GetTableName;

  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := Self.FModel.DeleteBatchQuery(AFilter);
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
  end;
end;

destructor TChBankaSubesiManager.Destroy;
begin
  FModel.Free;
  inherited;
end;

procedure TChBankaSubesiManager.DeleteBatch(const AIDs: TArray<Int64>);
var
  n1: Integer;
begin
  if AIDs = nil then
    Exit;

  for n1 := Low(AIDs) to High(AIDs) do
    Delete(AIDs[n1]);
end;

end.
