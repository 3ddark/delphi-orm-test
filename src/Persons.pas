unit Persons;

interface

uses
  Generics.Collections, System.SysUtils, System.Types, EntityAttributes, Entity;

type
  TPerson = class;

  [Table('aa_person_addresses', 'public')]
  TPersonAddress = class(TEntity, IEntity)
  private
    FPersonId: Int64;
    FCountry: string;
    FCity: string;
    FPerson: TPerson;
  public
    [Column('person_id', [cpNotNull], [cucFind, cucAdd, cucUpdate])]
    [Required('person.personid.required', True)]
    property PersonId: Int64 read FPersonId write FPersonId;

    [Column('country', [cpNotNull], [cucFind, cucAdd, cucUpdate])]
    [Required('person.country.required', True)]
    [MaxLength(32, 'validation.maxlength', True)]
    property Country: string read FCountry write FCountry;

    [Column('city', [cpNotNull], [cucFind, cucAdd, cucUpdate])]
    [Required('person.city.required', True)]
    [MaxLength(32, 'validation.maxlength', True)]
    property City: string read FCity write FCity;

    [BelongsTo('PersonId', 'Id', 'TPerson')]
    [NotMapped]
    property Person: TPerson read FPerson write FPerson;

    constructor Create; override;
    destructor Destroy; override;
  end;


  [TableAttribute('aa_persons', 'public')]
  [IndexAttribute('idx_person_name', 'person_name', True)]
  [SoftDelete('deleted_at', 'deleted_by')]
  TPerson = class(TEntity, IEntity)
  private
    FPersonName: string;
    FPersonAge: Integer;
    FSalary: Double;
    FAddresses: TObjectList<TPersonAddress>;
   protected
   public
    function Validate: TValidationResult; override;
    [Column('person_name', [cpNotNull], [cucFind, cucAdd, cucUpdate])]
    [Required('person.name.required', True)]
    [MaxLength(16, 'validation.maxlength', True)]
    property PersonName: string read FPersonName write FPersonName;

    [Column('person_age', [cpNotNull], [cucFind, cucAdd, cucUpdate])]
    [Required('person.age.required', True)]
    [Range(0, 100, 'validation.range', True)]
    property PersonAge: Integer read FPersonAge write FPersonAge;

    [Column('salary', [cpNotNull], [cucFind, cucUpdate])]
    [Required('person.salary.required', True)]
    [Range(0.0, 99999999.0, 'validation.range', True)]
    property Salary: Double read FSalary write FSalary;

    [HasMany('PersonId', 'Id', 'TPersonAddress')]
    [NotMapped]
    property Addresses: TObjectList<TPersonAddress> read FAddresses write FAddresses;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

constructor TPerson.Create;
begin
  inherited;
  FAddresses := TObjectList<TPersonAddress>.Create(True);
  FPersonName := '';
  FPersonAge := 0;
  FSalary := 0.0;
end;

destructor TPerson.Destroy;
begin
  if Assigned(FAddresses) then
    FreeAndNil(FAddresses);
  inherited;
end;

function TPerson.Validate: TValidationResult;
begin
  Result := inherited;
end;

constructor TPersonAddress.Create;
begin
  inherited;
  FPersonId := 0;
  FCountry := '';
  FCity := '';
  FPerson := nil;
end;

destructor TPersonAddress.Destroy;
begin

  inherited;
end;

end.
