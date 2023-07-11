program orm_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Generics.Collections,
  ZConnection,
  Entity in 'Entity.pas',
  EntityManager in 'EntityManager.pas',
  EntityAttributes in 'EntityAttributes.pas',
  Persons in 'Persons.pas';

var
  LConn: TZConnection;
  LMan: TEntityManager;

  lstr: string;

  LPerson: TPerson;
  LPersons: TObjectList<TPerson>;
  LAddress: TPersonAddress;
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

    Readln(lstr);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
