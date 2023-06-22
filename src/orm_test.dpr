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
  LMan: TEntityManager2;
  LBanka: TChBanka;
  LBankaSube: TChBankaSubesi;
begin
  try
    LConn := TZConnection.Create(nil);
    LConn.Protocol := 'postgresql-9';
    LConn.Database := 'ths_erp';
    LConn.HostName := 'localhost';
    LConn.User := 'postgres';
    LConn.Password := 'qwe';
    LConn.Connect;

    LMan := TEntityManager2.Create(LConn);
    LBanka := LMan.GetById(TChBanka, 2) as TChBanka;

//    LBankaSube := LMan.GetById(TChBankaSubesi, 1) as TChBankaSubesi;

    Writeln('end of process');

    FreeAndNil(LBankaSube);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
