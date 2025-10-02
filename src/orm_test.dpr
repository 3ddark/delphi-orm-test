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
  FilterCriterion in 'FilterCriterion.pas',
  RepositoryManager in 'RepositoryManager.pas',
  CascadeHelper in 'CascadeHelper.pas',
  LocalizationManager in 'LocalizationManager.pas',
  PersonRepository in 'PersonRepository.pas';

var
  LConn: TFDConnection;
  FPhys: TFDPhysPgDriverLink;

  LRepoPerson: IRepository<TPerson>;
  LPerson: TPerson;
  LPersonAddress: TPersonAddress;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    LConn := TFDConnection.Create(nil);
    LConn.LoginPrompt := False;

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

    TRepositoryManager.Instance.Initialize(LConn);

    LRepoPerson := TRepositoryManager.Instance.GetRepository<TPerson, TPersonRepository>;
    LPerson := LRepoPerson.FindById(3, False, [ioIncludeAll]);

//    LRepoPerson.Delete(LPerson);

//    LPerson := LRepoPerson.FindById(2, False, [ioIncludeChildren]);


    LPerson := TPerson.Create;
    LPerson.PersonName := 'John Doe';
    LPerson.PersonAge := 130;
    LPerson.Salary := 100000000;

    LPersonAddress := TPersonAddress.Create;
    LPersonAddress.Country := 'Turkey';
    LPersonAddress.City := 'Istanbul';
    LPersonAddress.PersonId := LPerson.Id;
    LPerson.Addresses.Add(LPersonAddress);

    LPersonAddress := TPersonAddress.Create;
    LPersonAddress.Country := 'Germany';
    LPersonAddress.City := 'Bochum';
    LPersonAddress.PersonId := LPerson.Id;
    LPerson.Addresses.Add(LPersonAddress);

    LPerson.Validate();

    LRepoPerson.Add(LPerson, TCascadeHelper.Insert);

    LPerson := LRepoPerson.FindById(2, False);
    FreeAndNil(LPerson);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
