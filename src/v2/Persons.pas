unit Persons;

interface

uses
  Data.DB, Ths.Orm.Table, Ths.Orm.Manager, System.Generics.Collections,
  Ths.Orm.ManagerStack;

type
  TPersonAdres = class;

  TPerson = class(TThsTable)
  private
    FPersonName: TThsField;
    FPersonAge: TThsField;
    FSalary: TThsField;
    FAdres: TPersonAdres;
    procedure SetAdres(const Value: TPersonAdres);
    procedure SetPersonAge(const Value: TThsField);
    procedure SetPersonName(const Value: TThsField);
    procedure SetSalary(const Value: TThsField);
  public
    property PersonName: TThsField read FPersonName write SetPersonName;
    property PersonAge: TThsField read FPersonAge write SetPersonAge;
    property Salary: TThsField read FSalary write SetSalary;
    property Adres: TPersonAdres read FAdres write SetAdres;

    constructor Create(); override;
    destructor Destroy; override;

    function BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean; override;
    function BusinessInsert(APermissionCheck: Boolean): Boolean; override;
    function BusinessUpdate(APermissionCheck: Boolean): Boolean; override;
    function BusinessDelete(APermissionCheck: Boolean): Boolean; override;
  end;

  TPersonAdres = class(TThsTable)
  private
    FCountry: TThsField;
    FCity: TThsField;
    FPersonId: TThsField;
    procedure SetCountry(const Value: TThsField);
    procedure SetCity(const Value: TThsField);
    procedure SetPersonId(const Value: TThsField);
  public
    Person: TPerson;
    property Country: TThsField read FCountry write SetCountry;
    property City: TThsField read FCity write SetCity;
    property PersonId: TThsField read FPersonId write SetPersonId;

    constructor Create(APerson: TPerson = nil); reintroduce; overload;
    destructor Destroy; override;
  end;


implementation

function TPerson.BusinessSelect(AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
begin
  Result := True;
  try
    //ManagerMain.GetOne(Self.Adres, Self.Adres.PersonId.QryName + '=' + Self.Id.AsString, ALock, False);
  finally
  end;
end;

function TPerson.BusinessInsert(APermissionCheck: Boolean): Boolean;
begin
  Result := True;
end;

function TPerson.BusinessUpdate(APermissionCheck: Boolean): Boolean;
begin
  Result := True;
end;

function TPerson.BusinessDelete(APermissionCheck: Boolean): Boolean;
begin
 Result := True;
end;

constructor TPerson.Create();
begin
  Self.SchemaName := 'public';
  Self.TableName := 'persons';
  Self.TableSourceCode := '1000';

  inherited;

  FPersonName := TThsField.Create('person_name', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FPersonAge := TThsField.Create('person_age', ftSmallint, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FSalary := TThsField.Create('salary', ftBCD, 0, Self, [fpSelect, fpInsert, fpUpdate]);
  FAdres := TPersonAdres.Create(Self);
end;

destructor TPerson.Destroy;
begin
  FAdres.DisposeOf;
  inherited;
end;

procedure TPerson.SetAdres(const Value: TPersonAdres);
begin
  FAdres := Value;
end;

procedure TPerson.SetPersonAge(const Value: TThsField);
begin
  FPersonAge := Value;
end;

procedure TPerson.SetPersonName(const Value: TThsField);
begin
  FPersonName := Value;
end;

procedure TPerson.SetSalary(const Value: TThsField);
begin
  FSalary := Value;
end;

constructor TPersonAdres.Create(APerson: TPerson);
begin
  Self.SchemaName := 'public';
  Self.TableName := 'person_addresses';
  Self.TableSourceCode := '1000';

  inherited Create();

  FCountry := TThsField.Create('country', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FCity := TThsField.Create('city', ftString, '', Self, [fpSelect, fpInsert, fpUpdate]);
  FPersonId := TThsField.Create('person_id', ftLargeint, 0, Self, [fpSelect, fpInsert, fpUpdate]);

  Person := APerson;
end;

destructor TPersonAdres.Destroy;
begin
  Person := nil;
  inherited;
end;

procedure TPersonAdres.SetCity(const Value: TThsField);
begin
  FCity := Value;
end;

procedure TPersonAdres.SetCountry(const Value: TThsField);
begin
  FCountry := Value;
end;

procedure TPersonAdres.SetPersonId(const Value: TThsField);
begin
  FPersonId := Value;
end;

end.
