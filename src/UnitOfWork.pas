unit UnitOfWork;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils, Classes,
  FireDAC.Comp.Client, FireDAC.Phys,FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error,
  SharedFormTypes, Entity, Repository;

type
  TUnitOfWork = class
  private
    class var FInstance: TUnitOfWork;
    class var FLock: TObject;
    FCache: TDictionary<TClass, IInterface>;
  private
    FConnection: TFDConnection;

    function GetConnection: TFDConnection;
  protected
    constructor Create(AConnection: TFDConnection);
  public
    destructor Destroy; override;

    class procedure Initialize(AConnection: TFDConnection);
    class function Instance: TUnitOfWork;

    function GetRepository<T: TEntity, constructor; R: class, constructor>: IRepository<T>;

    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: Boolean;

    function IsAuthorized(APermissionType: TPermissionType; APermissionControl: Boolean; AShowException: Boolean=True): Boolean;

    property Connection: TFDConnection read GetConnection;
  end;

implementation

constructor TUnitOfWork.Create(AConnection: TFDConnection);
begin
  FCache := TDictionary<TClass, IInterface>.Create;
  Self.FConnection := AConnection;
end;

destructor TUnitOfWork.Destroy;
begin

  inherited;
end;

function TUnitOfWork.GetConnection: TFDConnection;
begin
  Result := FInstance.FConnection;
end;

function TUnitOfWork.GetRepository<T, R>: IRepository<T>;
var
  LRepo: IInterface;
  LKey: TClass;
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiMethod: TRttiMethod;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('TRepositoryManager initialize edilmeden kullanılamaz!');

  LKey := R;

  if not FCache.TryGetValue(LKey, LRepo) then
  begin
    LContext := TRttiContext.Create;
    try
      LRttiType := LContext.GetType(R);
      LRttiMethod := LRttiType.GetMethod('Create');
      if Assigned(LRttiMethod) and (Length(LRttiMethod.GetParameters) <> 1) then
        raise Exception.CreateFmt('%s uygun constructor bulamadı!', [R.ClassName]);
      LRepo := LRttiMethod.Invoke(LRttiType.AsInstance.MetaclassType, [FConnection]).AsInterface as IRepository<T>;
    finally
      LContext.Free;
    end;

    FCache.Add(LKey, LRepo);
  end;

  Result := LRepo as IRepository<T>;
end;

procedure TUnitOfWork.BeginTransaction;
begin
  FInstance.FConnection.StartTransaction;
  //Logger.RunLog('TRANSACTION BEGIN');
end;

procedure TUnitOfWork.Commit;
begin
  FInstance.FConnection.Commit;
  //Logger.RunLog('TRANSACTION COMMIT');
end;

procedure TUnitOfWork.Rollback;
begin
  FInstance.FConnection.Rollback;
  //Logger.RunLog('TRANSACTION ROLLBACK');
end;

function TUnitOfWork.InTransaction: Boolean;
begin
  Result := FInstance.Connection.InTransaction;
end;

function TUnitOfWork.IsAuthorized(APermissionType: TPermissionType; APermissionControl: Boolean; AShowException: Boolean=True): Boolean;
begin
  Result := True;
end;

class procedure TUnitOfWork.Initialize(AConnection: TFDConnection);
begin
  if FInstance = nil then
  begin
    TMonitor.Enter(FLock);
    try
      if FInstance = nil then
        FInstance := TUnitOfWork.Create(AConnection);
    finally
      TMonitor.Exit(FLock);
    end;
  end;
end;

class function TUnitOfWork.Instance: TUnitOfWork;
begin
  if FInstance = nil then
    raise Exception.Create('TUnitOfWork not initialized. Call Initialize(AConnection) first.');
  Result := FInstance;
end;

initialization
  TUnitOfWork.FLock := TObject.Create;

finalization
  TUnitOfWork.FInstance.Free;
  TUnitOfWork.FLock.Free;

end.
