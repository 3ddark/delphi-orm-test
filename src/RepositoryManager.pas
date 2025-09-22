unit RepositoryManager;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils, Repository, Entity;

type
  TRepositoryManager = class
  strict private
    class var FInstance: TRepositoryManager;
    FCache: TDictionary<TClass, IInterface>;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TRepositoryManager;

    function GetRepository<T: class, constructor; R: TEntityManager<T>, constructor>: IRepository<T>;
  end;

implementation

constructor TRepositoryManager.Create;
begin
  FCache := TDictionary<TClass, IInterface>.Create;
end;

destructor TRepositoryManager.Destroy;
begin
  FCache.Free;
  inherited;
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
begin
  if not FCache.TryGetValue(R, LRepo) then
  begin
    LRepo := R.Create;
    FCache.Add(R, LRepo);
  end;
  Result := LRepo as IRepository<T>;
end;

end.
