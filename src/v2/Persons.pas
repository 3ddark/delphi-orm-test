unit Persons;

interface

uses Data.DB, Ths.Erp.Database.Table, Ths.Erp.Database.Manager;

type
  TPersonAdres = class;

  TPerson = class(TTable)
  private
    FPersonName: TFieldDB;
    FPersonAge: TFieldDB;
    FSalary: TFieldDB;
    FAdres: TPersonAdres;
    procedure SetAdres(const Value: TPersonAdres);
    procedure SetPersonAge(const Value: TFieldDB);
    procedure SetPersonName(const Value: TFieldDB);
    procedure SetSalary(const Value: TFieldDB);
  public
    property PersonName: TFieldDB read FPersonName write SetPersonName;
    property PersonAge: TFieldDB read FPersonAge write SetPersonAge;
    property Salary: TFieldDB read FSalary write SetSalary;
    property Adres: TPersonAdres read FAdres write SetAdres;

    constructor Create; override;
    destructor Destroy; override;

    function Clone: TPerson; reintroduce; overload;
    class procedure BusinessSelect(AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
  end;

  TPersonAdres = class(TTable)
  private
    FCountry: TFieldDB;
    FCity: TFieldDB;
    FPersonId: TFieldDB;
    procedure SetCountry(const Value: TFieldDB);
    procedure SetCity(const Value: TFieldDB);
    procedure SetPersonId(const Value: TFieldDB);
  public
    Person: TPerson;
    property Country: TFieldDB read FCountry write SetCountry;
    property City: TFieldDB read FCity write SetCity;
    property PersonId: TFieldDB read FPersonId write SetPersonId;

    constructor Create(APerson: TPerson = nil); reintroduce; overload;
    destructor Destroy; override;

    function Clone: TPersonAdres; reintroduce; overload;
  end;


implementation

class procedure TPerson.BusinessSelect(AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean);
var
  n1: Integer;
  LPerson: TPerson;
  LPersons: TArray<TTable>;
begin
  try
    AManager.GetList(TPerson, LPersons, AFilter, ALock, APermissionCheck);
    for n1 := 0 to Length(LPersons)-1 do
    begin
      LPerson := LPersons[n1] as TPerson;
      AManager.GetOne(LPerson.FAdres, LPerson.FAdres.PersonId.QryName + '=' + LPerson.Id.AsString, ALock, APermissionCheck);
    end;
  finally
    for n1 := 0 to Length(LPersons)-1 do
      LPersons[n1].DisposeOf;
    SetLength(LPersons, 0);
  end;
end;

constructor TPerson.Create;
begin
  Self.SchemaName := 'public';
  Self.TableName := 'aa_persons';
  Self.TableSourceCode := '1000';

  inherited;

  FPersonName := TFieldDB.Create('person_name', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FPersonAge := TFieldDB.Create('person_age', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FSalary := TFieldDB.Create('salary', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FAdres := TPersonAdres.Create(Self);
end;

destructor TPerson.Destroy;
begin
  FAdres.DisposeOf;
  inherited;
end;

function TPerson.Clone: TPerson;
begin
  Result := TPerson.Create;
  Result.CloneData(Self);
end;

procedure TPerson.SetAdres(const Value: TPersonAdres);
begin
  FAdres := Value;
end;

procedure TPerson.SetPersonAge(const Value: TFieldDB);
begin
  FPersonAge := Value;
end;

procedure TPerson.SetPersonName(const Value: TFieldDB);
begin
  FPersonName := Value;
end;

procedure TPerson.SetSalary(const Value: TFieldDB);
begin
  FSalary := Value;
end;

constructor TPersonAdres.Create(APerson: TPerson);
begin
  Self.SchemaName := 'public';
  Self.TableName := 'aa_person_addresses';
  Self.TableSourceCode := '1000';

  inherited Create;

  FCountry := TFieldDB.Create('country', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FCity := TFieldDB.Create('city', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FPersonId := TFieldDB.Create('person_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);

  Person := APerson;
end;

destructor TPersonAdres.Destroy;
begin
  Person := nil;
  inherited;
end;

function TPersonAdres.Clone: TPersonAdres;
begin
  Result := TPersonAdres.Create;
  Result.CloneData(Self);
end;

procedure TPersonAdres.SetCity(const Value: TFieldDB);
begin
  FCity := Value;
end;

procedure TPersonAdres.SetCountry(const Value: TFieldDB);
begin
  FCountry := Value;
end;

procedure TPersonAdres.SetPersonId(const Value: TFieldDB);
begin
  FPersonId := Value;
end;

end.
