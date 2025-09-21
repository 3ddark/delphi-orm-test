program orm_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Phys.PG,
  FireDAC.Stan.Intf,
  FireDAC.Comp.UI,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.PGDef,
  FireDAC.Stan.Def,
  FireDAC.DApt,
  FireDAC.Stan.Async,
  Entity in 'Entity.pas',
  Repository in 'Repository.pas',
  EntityAttributes in 'EntityAttributes.pas',
  Persons in 'Persons.pas',
  FilterCriterion in 'FilterCriterion.pas';

var
  LConn: TFDConnection;
  FPhys: TFDPhysPgDriverLink;

  LMan: TEntityManager<TPerson>;

  lstr: string;

  LPerson: TPerson;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
//    LConn := TFDConnection.Create(nil);
//    LConn.Protocol := 'postgresql-9';
//    LConn.Database := 'mydb-test';
//    LConn.HostName := 'localhost';
//    LConn.User := 'postgres';
//    LConn.Password := 'qwe';
//    LConn.Connect;

    LConn := TFDConnection.Create(nil);
    LConn.LoginPrompt := False;

//    LConn.BeforeConnect       := ConnBeforeConnect;
//    LConn.BeforeDisconnect    := ConnBeforeDisconnect;
//    LConn.AfterConnect        := ConnAfterConnect;
//    LConn.AfterDisconnect     := ConnAfterDisconnect;
//    LConn.BeforeStartTransaction := ConnOnStartTransaction;
//    LConn.AfterCommit         := ConnOnCommit;
//    LConn.AfterRollback       := ConnOnRollback;

    FPhys := nil;
    LConn.DriverName := 'PG';
    with LConn.Params as TFDPhysPGConnectionDefParams do
    begin
      Server := 'localhost';
      Database := 'postgres';
      UserName := 'postgres';
      Password := 'qwe';
      Port := 5432;
      CharacterSet := TFDPGCharacterSet.csUTF8;
    end;

    FPhys := TFDPhysPgDriverLink.Create(nil);
    FPhys.VendorLib := TPath.Combine(TPath.Combine(ExtractFilePath(ParamStr(0)), 'lib'), 'libpq.dll');


    LMan := TEntityManager<TPerson>.Create(LConn);
    LPerson := LMan.FindById(2, False);
    FreeAndNil(LPerson);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
