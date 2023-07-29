unit Ths.Erp.Database.ManagerStack;

interface

uses
  ZAbstractConnection, ZConnection, Ths.Erp.Database.Manager;

type
  TManagerStack = class
  public
    class procedure prepareManager;
    destructor Destroy; override;
  end;

var
  ManagerMain: TEntityManager;
  ConnMain: TZConnection;

implementation

{ TManagerStack }

destructor TManagerStack.Destroy;
begin
  ManagerMain.Free;
  ConnMain.Disconnect;
  ConnMain.DisposeOf;
  inherited;
end;

class procedure TManagerStack.prepareManager;
begin
  ConnMain := TZConnection.Create(nil);
  ConnMain.Protocol := 'postgresql-9';
  ConnMain.Database := 'ths_erp';
  ConnMain.HostName := 'localhost';
  ConnMain.User := 'postgres';
  ConnMain.Password := 'qwe';
  ConnMain.Connect;

  ManagerMain := TEntityManager.Create(ConnMain);
end;

initialization

finalization
  ManagerMain.Free;
  ConnMain.Disconnect;
  ConnMain.DisposeOf;

end.
