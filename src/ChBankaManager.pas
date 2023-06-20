unit ChBankaManager;

interface

uses
System.TypInfo, Rtti,
  Generics.Collections, SysUtils, StrUtils, Data.DB,
  ZAbstractConnection, ZAbstractDataset, ZDataset,
  Entity, EntityManager, ChBankalar;

type
  TChBankaManager = class(TEntityManager<TChBanka>)
  private
    FModel: TChBanka;
  protected
    function BusinessGet(const AFilter: string = ''; ALock: Boolean = False): TList<TChBanka>; override;
    procedure BusinessAdd(AModels: TArray<TChBanka>); override;
    procedure BusinessUpdate(AModels: TArray<TChBanka>); override;
    procedure BusinessDelete(const AIDs: TArray<Int64>); override;
  public
    function GetById(AID: Int64; ALock: Boolean = False): TChBanka; override;
    function GetList(const AIDs: TArray<Int64>; ALock: Boolean = False): TList<TChBanka>; reintroduce; overload;
    function GetList(const AFilter: string = ''; ALock: Boolean = False): TList<TChBanka>; reintroduce; overload;
    procedure Add(AModel: TChBanka); override;
    procedure AddBatch(AModels: TArray<TChBanka>); override;
    procedure Update(AModel: TChBanka); override;
    procedure UpdateBatch(AModels: TArray<TChBanka>); override;
    procedure Delete(const AID: Int64); override;
    procedure DeleteBatch(const AIDs: TArray<Int64>); reintroduce; overload;
    procedure DeleteBatch(const AFilter: string = ''); reintroduce; overload;

    constructor Create(AConnection: TZAbstractConnection); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses ChBankaSubesiManager;

constructor TChBankaManager.Create(AConnection: TZAbstractConnection);
begin
  FModel := TChBanka.Create;
  inherited Create(AConnection);
end;

function TChBankaManager.GetById(AId: Int64; ALock: Boolean): TChBanka;
var
  LCmd: TZQuery;
  LBankaSubesiManager: TChBankaSubesiManager;
begin
  Result := TChBanka.Create;

  if AId = 0 then
    Exit;

  LCmd := TZQuery.Create(nil);
  LBankaSubesiManager := nil;
  if Self.LazzyLoading then
  begin
    LBankaSubesiManager := TChBankaSubesiManager.Create(Self.Connection);
    LBankaSubesiManager.DisableLazzyLoading;
  end;
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim(Result.GetQuery(AID));
    LCmd.Prepare;
    if LCmd.Prepared then
    begin
      LCmd.Open;
      while not LCmd.Eof do
      begin
        Result.SetValuesFromDbFields(LCmd.Fields);
        if Self.LazzyLoading then
          Result.BankaSubeleri := LBankaSubesiManager.GetList(' and banka_id=' + IntToStr(Result.ID));
        LCmd.Next;
      end;
    end;
  finally
    LCmd.Free;
    if Self.LazzyLoading then
      LBankaSubesiManager.Free;
  end;
end;

function TChBankaManager.GetList(const AIDs: TArray<Int64>; ALock: Boolean): TList<TChBanka>;
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

function TChBankaManager.GetList(const AFilter: string; ALock: Boolean): TList<TChBanka>;
var
  LCmd: TZQuery;
  LItem: TChBanka;
  LTableName: string;
begin
  Result := TList<TChBanka>.Create;

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

procedure TChBankaManager.Add(AModel: TChBanka);
var
  LCmd: TZQuery;
  n1: Integer;
  LBankaSubesiManager: TChBankaSubesiManager;
begin
  if AModel = nil then
    Exit;

  LCmd := TZQuery.Create(nil);
  LBankaSubesiManager := nil;
  if Self.LazzyLoading then
  begin
    LBankaSubesiManager := TChBankaSubesiManager.Create(Self.Connection);
    LBankaSubesiManager.DisableLazzyLoading;
  end;
  try
    if Self.LazzyLoading then
      for n1 := 0 to AModel.BankaSubeleri.Count -1 do
        LBankaSubesiManager.Add(AModel.BankaSubeleri[n1]);

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
      LBankaSubesiManager.Free;
  end;
end;

procedure TChBankaManager.AddBatch(AModels: TArray<TChBanka>);
var
  n1: Integer;
begin
  if AModels = nil then
    Exit;

  for n1 := Low(AModels) to High(AModels) do
    Add(AModels[n1]);
end;

function TChBankaManager.BusinessGet(const AFilter: string; ALock: Boolean): TList<TChBanka>;
begin
  Result := Self.GetList(AFilter, ALock);
end;

procedure TChBankaManager.BusinessAdd(AModels: TArray<TChBanka>);
begin
  Self.AddBatch(AModels);
end;

procedure TChBankaManager.BusinessUpdate(AModels: TArray<TChBanka>);
begin
  Self.UpdateBatch(AModels);
end;

procedure TChBankaManager.BusinessDelete(const AIDs: TArray<Int64>);
begin
  Self.DeleteBatch(AIDs);
end;

procedure TChBankaManager.Update(AModel: TChBanka);
var
  LCmd: TZQuery;
  n1: Integer;
  LBankaSubesiManager: TChBankaSubesiManager;
begin
  if AModel = nil then
    Exit;

  LCmd := TZQuery.Create(nil);
  LBankaSubesiManager := nil;
  if Self.LazzyLoading then
  begin
    LBankaSubesiManager := TChBankaSubesiManager.Create(Self.Connection);
    LBankaSubesiManager.DisableLazzyLoading;
  end;
  try
    if Self.LazzyLoading then
      for n1 := 0 to AModel.BankaSubeleri.Count -1 do
        LBankaSubesiManager.Add(AModel.BankaSubeleri[n1]);

    LCmd.Connection := Connection;
    LCmd.SQL.Text := AModel.UpdateQuery;
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
    if Self.LazzyLoading then
      LBankaSubesiManager.Free;
  end;
end;

procedure TChBankaManager.UpdateBatch(AModels: TArray<TChBanka>);
var
  n1: Integer;
begin
  if AModels = nil then
    Exit;

  for n1 := Low(AModels) to High(AModels) do
    Update(AModels[n1]);
end;

procedure TChBankaManager.Delete(const AID: Int64);
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

procedure TChBankaManager.DeleteBatch(const AFilter: string);
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

destructor TChBankaManager.Destroy;
begin
  FModel.Free;
  inherited;
end;

procedure TChBankaManager.DeleteBatch(const AIDs: TArray<Int64>);
var
  n1: Integer;
begin
  if AIDs = nil then
    Exit;

  for n1 := Low(AIDs) to High(AIDs) do
    Delete(AIDs[n1]);
end;

end.
