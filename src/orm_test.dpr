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
  CascadeHelper in 'CascadeHelper.pas',
  Entity in 'Entity.pas',
  EntityAttributes in 'EntityAttributes.pas',
  FilterCriterion in 'FilterCriterion.pas',
  LocalizationManager in 'LocalizationManager.pas',
  Repository in 'Repository.pas',
  Service in 'Service.pas',
  SharedFormTypes in 'SharedFormTypes.pas',
  UnitOfWork in 'UnitOfWork.pas',
  PersonRepository in 'PersonRepository.pas',
  Persons in 'Persons.pas',
  PersonService in 'PersonService.pas';

var
  LConn: TFDConnection;
  FPhys: TFDPhysPgDriverLink;
  LPersonSvc: TPersonService;

  LPerson: TPerson;
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

    TUnitOfWork.Initialize(LConn);

    LPersonSvc := TPersonService.Create;

    LPerson := LPersonSvc.FindById(3, False);

    Writeln(LPerson.PersonName);

    LPerson.PersonName := 'Veli Deli';
    LPerson.Salary := 2500;

    LPersonSvc.BusinessUpdate(LPerson, True, True, True);

    Writeln(LPerson.PersonName);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
