unit RepositoryManager;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Repository, Entity;

type
  TRepositoryManager = class
  strict private
    class var FInstance: TRepositoryManager;
    FConnection: TFDConnection;
    FCache: TDictionary<TClass, IInterface>;
  private
    constructor Create;
  public
    destructor Destroy; override;

    class function Instance: TRepositoryManager;
    procedure Initialize(AConnection: TFDConnection);

    function GetRepository<T: TEntity, constructor; R: class, constructor>: IRepository<T>;
  end;

implementation

constructor TRepositoryManager.Create;
begin
  inherited Create;
  FCache := TDictionary<TClass, IInterface>.Create;
end;

destructor TRepositoryManager.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TRepositoryManager.Initialize(AConnection: TFDConnection);
begin
  if Assigned(FConnection) then
    raise Exception.Create('TRepositoryManager zaten initialize edilmiş!');
  FConnection := AConnection;
end;

class function TRepositoryManager.Instance: TRepositoryManager;
begin
  if FInstance = nil then
    FInstance := TRepositoryManager.Create;
  Result := FInstance;
end;

function TRepositoryManager.GetRepository<T, R>: IRepository<T>;
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
    // RTTI ile parametreli constructor çağırıyoruz
    LContext := TRttiContext.Create;
    try
      LRttiType := LContext.GetType(R);
      LRttiMethod := LRttiType.GetMethod('Create');
      if Assigned(LRttiMethod) and (Length(LRttiMethod.GetParameters) = 1) then
      begin
        LRepo := LRttiMethod.Invoke(LRttiType.AsInstance.MetaclassType,
                  [FConnection]).AsInterface as IRepository<T>;
      end
      else
        raise Exception.CreateFmt('%s uygun constructor bulamadı!', [R.ClassName]);
    finally
      LContext.Free;
    end;

    FCache.Add(LKey, LRepo);
  end;

  Result := LRepo as IRepository<T>;
end;

end.
