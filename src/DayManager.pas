unit DayManager;

interface

uses
  Generics.Collections, SysUtils, StrUtils, Data.DB,
  ZAbstractConnection, ZAbstractDataset, ZDataset,
  Entity, EntityManager, SysDay;

type
  TDayManager = class(TEntityManager<TSysDay>)
  private
    FModel: TSysDay;
  protected
    function BusinessGet(const AFilter: string = ''; ALock: Boolean = False): TList<TSysDay>; override;
    procedure BusinessAdd(AModels: TArray<TSysDay>); override;
    procedure BusinessUpdate(AModels: TArray<TSysDay>); override;
    procedure BusinessDelete(const AIDs: TArray<Int64>); override;
  public
    function GetById(AID: Int64; ALock: Boolean = False): TSysDay; override;
    function GetList(const AIDs: TArray<Int64>; ALock: Boolean = False): TList<TSysDay>; reintroduce; overload;
    function GetList(const AFilter: string = ''; ALock: Boolean = False): TList<TSysDay>; reintroduce; overload;
    procedure Add(AModel: TSysDay); override;
    procedure AddBatch(AModels: TArray<TSysDay>); override;
    procedure Update(AModel: TSysDay); override;
    procedure UpdateBatch(AModels: TArray<TSysDay>); override;
    procedure Delete(const AID: Int64); override;
    procedure DeleteBatch(const AIDs: TArray<Int64>); reintroduce; overload;
    procedure DeleteBatch(const AFilter: string = ''); reintroduce; overload;

    constructor Create(AConnection: TZAbstractConnection); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

constructor TDayManager.Create(AConnection: TZAbstractConnection);
begin
  FModel := TSysDay.Create;
  inherited Create(AConnection);
end;

function TDayManager.GetById(AId: Int64; ALock: Boolean): TSysDay;
var
  LCmd: TZQuery;
  LTableName: string;
begin
  Result := TSysDay.Create;

  if AId = 0 then
    Exit;

  LTableName := Result.GetTableName(True);

  if LTableName = '' then
    Exit;

  LCmd := TZQuery.Create(nil);
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
        LCmd.Next;
      end;
    end;
  finally
    LCmd.Free;
  end;
end;

function TDayManager.GetList(const AIDs: TArray<Int64>; ALock: Boolean): TList<TSysDay>;
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

function TDayManager.GetList(const AFilter: string; ALock: Boolean): TList<TSysDay>;
var
  LCmd: TZQuery;
  LItem: TSysDay;
  LTableName: string;
begin
  Result := TList<TSysDay>.Create;

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

procedure TDayManager.Add(AModel: TSysDay);
var
  LCmd: TZQuery;
begin
  if AModel = nil then
    Exit;

  LCmd := TZQuery.Create(nil);
  try
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
  end;
end;

procedure TDayManager.AddBatch(AModels: TArray<TSysDay>);
var
  n1: Integer;
begin
  if AModels = nil then
    Exit;

  for n1 := Low(AModels) to High(AModels) do
    Add(AModels[n1]);
end;

function TDayManager.BusinessGet(const AFilter: string; ALock: Boolean): TList<TSysDay>;
begin
  Result := Self.GetList(AFilter, ALock);
end;

procedure TDayManager.BusinessAdd(AModels: TArray<TSysDay>);
begin
  Self.AddBatch(AModels);
end;

procedure TDayManager.BusinessUpdate(AModels: TArray<TSysDay>);
begin
  Self.UpdateBatch(AModels);
end;

procedure TDayManager.BusinessDelete(const AIDs: TArray<Int64>);
begin
  Self.DeleteBatch(AIDs);
end;

procedure TDayManager.Update(AModel: TSysDay);
var
  LCmd: TZQuery;
begin
  if AModel = nil then
    Exit;

  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := AModel.UpdateQuery;
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
  end;
end;

procedure TDayManager.UpdateBatch(AModels: TArray<TSysDay>);
var
  n1: Integer;
begin
  if AModels = nil then
    Exit;

  for n1 := Low(AModels) to High(AModels) do
    Update(AModels[n1]);
end;

procedure TDayManager.Delete(const AID: Int64);
var
  LCmd: TZQuery;
begin
  if AID = 0 then
    Exit;

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

procedure TDayManager.DeleteBatch(const AFilter: string);
var
  LCmd: TZQuery;
begin
  if Trim(AFilter) = '' then
    Exit;

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

destructor TDayManager.Destroy;
begin
  FModel.Free;
  inherited;
end;

procedure TDayManager.DeleteBatch(const AIDs: TArray<Int64>);
var
  n1: Integer;
begin
  if AIDs = nil then
    Exit;

  for n1 := Low(AIDs) to High(AIDs) do
    Delete(AIDs[n1]);
end;

end.
