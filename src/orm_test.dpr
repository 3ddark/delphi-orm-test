program orm_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Generics.Collections,
  ZConnection,
  Entity in 'Entity.pas',
  EntityManager in 'EntityManager.pas',
  ChBankalar in 'ChBankalar.pas',
  SysGunler in 'SysGunler.pas',
  EntityAttributes in 'EntityAttributes.pas';

var
  LConn: TZConnection;
  LMan: TEntityManager;
begin
  try
    LConn := TZConnection.Create(nil);
    LConn.Protocol := 'postgresql-9';
    LConn.Database := 'ths_erp';
    LConn.HostName := 'localhost';
    LConn.User := 'postgres';
    LConn.Password := 'qwe';
    LConn.Connect;

    LMan := TEntityManager.Create(LConn);

    ReadLn;

    LMan.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
