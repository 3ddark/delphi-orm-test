program orm_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Generics.Collections,
  ZConnection,
  Entity in 'Entity.pas',
  EntityManager in 'EntityManager.pas',
  SysDay in 'SysDay.pas',
  DayManager in 'DayManager.pas',
  ChBankalar in 'ChBankalar.pas',
  ChBankaManager in 'ChBankaManager.pas',
  ChBankaSubesiManager in 'ChBankaSubesiManager.pas';

var
  LConn: TZConnection;
(*
  LDayMan: TDayManager;
  LDay: TSysDay;
  LDays: TList<TSysDay>;
*)
  LBankaMan: TChBankaManager;
  LBanka, LBanka2: TChBanka;
  LBankalar: TList<TChBanka>;

  LBankaSubeMan: TChBankaSubesiManager;
//  LBankaSube: TChBankaSubesi;
  LBankaSubeleri: TList<TChBankaSubesi>;
begin
  try
    LConn := TZConnection.Create(nil);
    LConn.Protocol := 'postgresql-9';
    LConn.Database := 'mydb';
    LConn.HostName := 'localhost';
    LConn.User := 'postgres';
    LConn.Password := 'qwe';
    LConn.Connect;
(*
    LDayMan := TDayManager.Create(LConn);
    LDay := LDayMan.GetById(27);

//    LDay.DayName := 'TestXXXXXxxxxx';
//    LDayMan.Update(LDay);

    LDayMan.Delete(27);

    LDays := LDayMan.GetList('');
    LDayMan.DestoryList(LDays);
*)
    LBankaMan := TChBankaManager.Create(LConn);
    LBanka := LBankaMan.GetById(1);
    LBanka2 := LBanka.Clone<TChBanka>;
    LBanka.Clear;
    FreeAndNil(LBanka);
//    LBankalar := LBankaMan.GetList('');
//    LBankaMan.DestoryList(LBankalar);

//    LBankaSubeMan := TChBankaSubesiManager.Create(LConn);
(*    LBankaSube := LBankaSubeMan.GetById(1);
    LBankaSubeMan.DisableLazzyLoading;
    LBankaSubeleri := LBankaSubeMan.GetList('');
    LBankaSubeMan.DestoryList(LBankaSubeleri);
*)
//    LBankaSubeleri := LBankaSubeMan.LogicalGet('');
//    LBankaSubeMan.DestoryList(LBankaSubeleri);
    Writeln('aaaa');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
