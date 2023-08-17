unit Ths.Orm.ManagerStack;

interface

uses
  Ths.Orm.Manager;

type
  TManagerStack = class
  public
    class procedure prepareManager(AHostName, ADatabase, AUserName, AUserPass, ALibraryPath: string; APort: Integer);
    destructor Destroy; override;
  end;

var
  ManagerMain: TEntityManager;

implementation

{ TManagerStack }

destructor TManagerStack.Destroy;
begin
  ManagerMain.Free;
  inherited;
end;

class procedure TManagerStack.prepareManager(AHostName, ADatabase, AUserName, AUserPass, ALibraryPath: string; APort: Integer);
begin
  ManagerMain := TEntityManager.Create(AHostName, ADatabase, AUserName, AUserPass, ALibraryPath, APort);
end;

initialization

finalization
  ManagerMain.Free;

end.
