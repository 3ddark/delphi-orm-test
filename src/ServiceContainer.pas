unit ServiceContainer;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils, Service,
  Entity;

type
  TServiceContainer = class
  private
    class var FInstance: TServiceContainer;
    class var FLock: TObject;
    FCache: TDictionary<TClass, IInterface>;
  protected
    constructor Create;
  public
    destructor Destroy; override;

    function GetService<T: TEntity, constructor; R: class, constructor>: IService<T>;

    class function Instance: TServiceContainer;
  end;

implementation

constructor TServiceContainer.Create;
begin
  inherited Create;
  FCache := TDictionary<TClass, IInterface>.Create;
end;

destructor TServiceContainer.Destroy;
begin
  // interface ler referans sayimi ile otomatik temizlenecek
  inherited;
end;

function TServiceContainer.GetService<T, R>: IService<T>;
var
  LSvc: IInterface;
  LKey: TClass;
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiMethod: TRttiMethod;
begin
  LKey := R;

  if not FCache.TryGetValue(LKey, LSvc) then
  begin
    LContext := TRttiContext.Create;
    try
      LRttiType := LContext.GetType(R);
      LRttiMethod := LRttiType.GetMethod('Create');
      if Assigned(LRttiMethod) and (Length(LRttiMethod.GetParameters) <> 1) then
        raise Exception.CreateFmt('%s uygun constructor bulamadı!', [R.ClassName]);
      LSvc := LRttiMethod.Invoke(LRttiType.AsInstance.MetaclassType, []).AsInterface as IService<T>;
    finally
      LContext.Free;
    end;

    FCache.Add(LKey, LSvc);
  end;

  Result := LSvc as IService<T>;
end;

class function TServiceContainer.Instance: TServiceContainer;
begin
  if FInstance = nil then
  begin
    TMonitor.Enter(FLock);
    try
      if FInstance = nil then
        FInstance := TServiceContainer.Create;
    finally
      TMonitor.Exit(FLock);
    end;
  end;
  Result := FInstance;
end;

initialization
  TServiceContainer.FLock := TObject.Create;

finalization
  TServiceContainer.FInstance.Free;
  TServiceContainer.FLock.Free;

end.
