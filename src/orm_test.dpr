program orm_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.IOUtils,
  Generics.Collections,
  FireDAC.Comp.Client, FireDAC.Phys.PG, FireDAC.Stan.Intf, FireDAC.Comp.UI,
  FireDAC.Comp.DataSet, FireDAC.Phys.PGDef, FireDAC.Stan.Def, FireDAC.DApt,
  FireDAC.Stan.Async,
  Entity in 'Entity.pas',
  EntityManager in 'EntityManager.pas',
  EntityAttributes in 'EntityAttributes.pas',
  Persons in 'Persons.pas';

var
  LConn: TFDConnection;
  FPhys: TFDPhysPgDriverLink;

  LMan: TEntityManager;

  lstr: string;

  LPerson: TPerson;
  LPersons: TObjectList<TPerson>;
  LAddress: TPersonAddress;
begin
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




    LMan := TEntityManager.Create(LConn);
(*
    LPerson := TPerson.Create;
    LPerson.PersonName := 'Person1';
    LPerson.PersonAge := 20;
    LPerson.Salary := 1000;
    LMan.Add(LPerson);//Add One

    LPerson := LMan.GetByOne<TPerson>(LPerson.Id, False);//GetOne
    Writeln(LPerson.PersonName);
    LMan.Delete<TPerson>(LPerson.Id);//Delete with ID

    LPerson := TPerson.Create;
    LPerson.PersonName := 'Person2';
    LPerson.PersonAge := 20;
    LPerson.Salary := 1000;
    LMan.Add(LPerson);//Add One

    LPerson := LMan.GetByOne<TPerson>(LPerson.Id, False);//GetOne
    Writeln(LPerson.PersonName);
    LMan.Delete(LPerson);//Delete with Model

    LPersons := TObjectList<TPerson>.Create(True);
    try
      LPerson := TPerson.Create;
      LPerson.PersonName := 'Person3';
      LPerson.PersonAge := 20;
      LPerson.Salary := 1000;
      LPersons.Add(LPerson);
      LPerson := TPerson.Create;
      LPerson.PersonName := 'Person4';
      LPerson.PersonAge := 20;
      LPerson.Salary := 1000;
      LPersons.Add(LPerson);
      LMan.AddBatch<TPerson>(LPersons.ToArray);//Add Batch
    finally
      LPersons.Free;
    end;


    LPersons := LMan.GetList<TPerson>('', False);//GetList
    for LPerson in LPersons do
      Writeln(LPerson.PersonName);
    LMan.DeleteBatch<TPerson>(LPersons.ToArray);//Delete Batch with Models

    LPersons := TObjectList<TPerson>.Create(True);
    try
      LPerson := TPerson.Create;
      LPerson.PersonName := 'Person5';
      LPerson.PersonAge := 26;
      LPerson.Salary := 2000;
      LPersons.Add(LPerson);
      LPerson := TPerson.Create;
      LPerson.PersonName := 'Person6';
      LPerson.PersonAge := 22;
      LPerson.Salary := 3000;
      LPersons.Add(LPerson);
      LMan.AddBatch<TPerson>(LPersons.ToArray);//Add Batch
    finally
      LPersons.Free;
    end;

    LPersons := LMan.GetList<TPerson>('', False);//GetList
    for LPerson in LPersons do
      Writeln(LPerson.PersonName);
    LMan.DeleteBatch<TPerson>([LPersons[0].Id, LPersons[1].Id]);//Delete Batch with Id

    LPersons := TObjectList<TPerson>.Create(True);
    try
      LPerson := TPerson.Create;
      LPerson.PersonName := 'Person7';
      LPerson.PersonAge := 12;
      LPerson.Salary := 200;
      LPersons.Add(LPerson);
      LPerson := TPerson.Create;
      LPerson.PersonName := 'Person8';
      LPerson.PersonAge := 32;
      LPerson.Salary := 500;
      LPersons.Add(LPerson);
      LMan.AddBatch<TPerson>(LPersons.ToArray);//Add Batch
    finally
      LPersons.Free;
    end;
    LMan.DeleteBatch<TPerson>(' and id > 1');//Delete Batch with Filter


    LPersons := TObjectList<TPerson>.Create(True);
    try
      LPerson := TPerson.Create;
      LPerson.PersonName := 'PXX';
      LPerson.PersonAge := 17;
      LPerson.Salary := 160;
      LPersons.Add(LPerson);
      LPerson := TPerson.Create;
      LPerson.PersonName := 'PYY';
      LPerson.PersonAge := 14;
      LPerson.Salary := 320;
      LPersons.Add(LPerson);
      LMan.AddBatch<TPerson>(LPersons.ToArray);//Add Batch
    finally
      LPersons.Free;
    end;

    LPersons := LMan.GetList<TPerson>('', False);//GetList
    for LPerson in LPersons do
    begin
      Writeln(LPerson.PersonName + ' before');
      LPerson.PersonName := LPerson.PersonName + LPerson.Id.ToString;
      LMan.Update(LPerson);//Update
      Writeln(LPerson.PersonName + ' after');
    end;
    LMan.DeleteBatch<TPerson>('');//Delete Batch with Filter


    LPersons := TObjectList<TPerson>.Create(True);
    try
      LPerson := TPerson.Create;
      LPerson.PersonName := 'AAA';
      LPerson.PersonAge := 17;
      LPerson.Salary := 160;
      LPersons.Add(LPerson);
      LPerson := TPerson.Create;
      LPerson.PersonName := 'BBB';
      LPerson.PersonAge := 14;
      LPerson.Salary := 320;
      LPersons.Add(LPerson);
      LMan.AddBatch<TPerson>(LPersons.ToArray);//Add Batch
    finally
      LPersons.Free;
    end;

    LPersons := LMan.GetList<TPerson>('', False);//GetList
    for LPerson in LPersons do
    begin
      LPerson.PersonName := LPerson.PersonName + LPerson.Id.ToString;
    end;

    LMan.UpdateBatch<TPerson>(LPersons.ToArray);

    LPersons := LMan.GetList<TPerson>('', False);
    for LPerson in LPersons do
      Writeln(LPerson.PersonName);

    LMan.DeleteBatch<TPerson>(LPersons.ToArray);
*)
    LPersons := LMan.GetList<TPerson>('', False);
    LMan.DeleteBatch<TPerson>(LPersons.ToArray);

    LPerson := TPerson.Create;
    LPerson.PersonName := 'Person1';
    LPerson.PersonAge := 20;
    LPerson.Salary := 1000;
    LMan.Add(LPerson);//Add One

    LAddress := TPersonAddress.Create;
    LAddress.City := 'Istanbul';
    LAddress.Country := 'Turkey';
    LAddress.PersonId := LPerson.Id;
    LPerson.Addresses.Add(LAddress);

    LAddress := TPersonAddress.Create;
    LAddress.City := 'Bochum';
    LAddress.Country := 'Germany';
    LAddress.PersonId := LPerson.Id;
    LPerson.Addresses.Add(LAddress);
    LMan.AddBatch<TPersonAddress>(LPerson.Addresses.ToArray);

    LPerson := LMan.GetByOne<TPerson>(LPerson.Id, False);

//    LMan.AddBatch<TPersonAddress>(LPerson.Addresses.ToArray);
//    LPersons := LMan.GetList<TPerson>('', False);

    Readln(lstr);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
