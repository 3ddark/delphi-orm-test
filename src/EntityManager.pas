unit EntityManager;

interface

uses
  SysUtils, StrUtils, Generics.Collections, System.TypInfo, Rtti,
  ZAbstractConnection, Data.DB, Entity;

type
  TEntityManager<T> = class
  private
    FConnection: TZAbstractConnection;
    FLazzyLoading: Boolean;
  protected
    function Connection: TZAbstractConnection;

    function BusinessGet(const AFilter: string = ''; ALock: Boolean = False): TList<T>; virtual; abstract;
    procedure BusinessAdd(AModels: TArray<T>); virtual; abstract;
    procedure BusinessUpdate(AModels: TArray<T>); virtual; abstract;
    procedure BusinessDelete(const AIDs: TArray<Int64>); virtual; abstract;
  public
    property LazzyLoading: Boolean read FLazzyLoading;
    function GetById(AID: Int64; ALock: Boolean = False): T; virtual; abstract;
    function GetList(const AIDs: TArray<Int64>; ALock: Boolean = False): TList<T>; overload; virtual; abstract;
    function GetList(const AFilter: string = ''; ALock: Boolean = False): TList<T>; overload; virtual; abstract;
    procedure Add(AModel: T); virtual; abstract;
    procedure AddBatch(AModels: TArray<T>); virtual; abstract;
    procedure Update(AModel: T); virtual; abstract;
    procedure UpdateBatch(AModels: TArray<T>); virtual; abstract;
    procedure Delete(const AID: Int64); virtual; abstract;
    procedure DeleteBatch(const AIDs: TArray<Int64>); overload; virtual; abstract;
    procedure DeleteBatch(const AFilter: string); overload; virtual; abstract;

    procedure TransactionStart;
    procedure TransactionCommit;
    procedure TransactionRollback;

    function LogicalGet(const AFilter: string = ''; ALock: Boolean = False; AWithBegin: Boolean = False): TList<T>; virtual;
    procedure LogicalAdd(AModels: TArray<T>; AWithBegin: Boolean = False; AWithCommit: Boolean = False); virtual;
    procedure LogicalUpdate(AModels: TArray<T>; AWithCommit: Boolean = False); virtual;
    procedure LogicalDelete(const AIDs: TArray<Int64>; AWithCommit: Boolean = False); virtual;

    constructor Create(AConnection: TZAbstractConnection); virtual;

    class procedure DestoryList(var AList: TList<T>);

    function DisableLazzyLoading: Boolean;
    function EnableLazzyLoading: Boolean;
  end;

implementation

function TEntityManager<T>.Connection: TZAbstractConnection;
begin
  Result := FConnection;
end;

constructor TEntityManager<T>.Create(AConnection: TZAbstractConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection Required');

  FConnection := AConnection;
  FLazzyLoading := True;
end;

class procedure TEntityManager<T>.DestoryList(var AList: TList<T>);
var
  n1: Integer;
  AObject: T;
begin
  for n1 := 0 to AList.Count-1 do
  begin
    AObject := AList.Items[n1];
    FreeAndNil(AObject);
  end;

  FreeAndNil(AList);
end;

function TEntityManager<T>.DisableLazzyLoading: Boolean;
begin
  Self.FLazzyLoading := False;
end;

function TEntityManager<T>.EnableLazzyLoading: Boolean;
begin
  Self.FLazzyLoading := True;
end;

function TEntityManager<T>.LogicalGet(const AFilter: string; ALock, AWithBegin: Boolean): TList<T>;
begin
  try
    if AWithBegin then
      Connection.StartTransaction;

    Result := Self.BusinessGet(AFilter, ALock);
  except
    Connection.Rollback;
  end;
end;

procedure TEntityManager<T>.LogicalAdd(AModels: TArray<T>; AWithBegin, AWithCommit: Boolean);
begin
  try
    if AWithBegin then
      Connection.StartTransaction;

    Self.BusinessAdd(AModels);

    if AWithCommit then
      Connection.Commit;
  except
    Connection.Rollback;
  end;
end;

procedure TEntityManager<T>.LogicalUpdate(AModels: TArray<T>; AWithCommit: Boolean);
begin
  try
    Self.BusinessUpdate(AModels);

    if AWithCommit then
      Connection.Commit;
  except
    Connection.Rollback;
  end;
end;

procedure TEntityManager<T>.LogicalDelete(const AIDs: TArray<Int64>; AWithCommit: Boolean);
begin
  try
    Self.BusinessDelete(AIDs);

    if AWithCommit then
      Connection.Commit;
  except
    Connection.Rollback;
  end;
end;

procedure TEntityManager<T>.TransactionCommit;
begin
  Connection.Commit;
end;

procedure TEntityManager<T>.TransactionRollback;
begin
  Connection.Rollback;
end;

procedure TEntityManager<T>.TransactionStart;
begin
  Connection.StartTransaction;
end;

end.
