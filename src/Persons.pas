unit Persons;

interface

uses
  Generics.Collections, SysUtils, System.Types,
  EntityAttributes, Entity;
(*
-- DROP TABLE IF EXISTS public.aa_person_addresses;
-- DROP TABLE IF EXISTS public.aa_persons;
CREATE TABLE IF NOT EXISTS public.aa_persons
(
  id bigserial,
  person_name character varying(16) COLLATE pg_catalog."default",
  person_age smallint not null,
  salary numeric(18,2) NOT NULL DEFAULT 0,
  PRIMARY KEY (id),
	UNIQUE(person_name)
)
TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.aa_persons
    OWNER to postgres;


CREATE TABLE IF NOT EXISTS public.aa_person_addresses
(
  id bigserial,
  country character varying(32) COLLATE pg_catalog."default",
  city character varying(32) COLLATE pg_catalog."default",
  person_id bigint,
  PRIMARY KEY (id),
  FOREIGN KEY(person_id)
    REFERENCES public.aa_persons (id) MATCH SIMPLE
    ON UPDATE CASCADE
    ON DELETE CASCADE
)
TABLESPACE pg_default;

ALTER TABLE IF EXISTS public.aa_person_addresses
    OWNER to postgres;
*)

type
  TPerson = class;

  [Table('aa_person_addresses', 'public')]
  TPersonAddress = class(TEntity)
  private
    FPersonId: Int64;
    FCountry: string;
    FCity: string;

    FPerson: TPerson;
  public
    [Column('person_id')]
    property PersonId: Int64 read FPersonId write FPersonId;
    [Column('country')]
    property Country: string read FCountry write FCountry;
    [Column('city')]
    property City: string read FCity write FCity;

    [HasOne('Id', 'PersonId')]
    property Person: TPerson read FPerson write FPerson;

    constructor Create; override;
  end;

  [Table('aa_persons', 'public')]
  TPerson = class(TEntity)
  private
    FPersonName: string;
    FPersonAge: SmallInt;
    FSalary: Double;

    FAddresses: TObjectList<TPersonAddress>;
  public
    [Column('person_name')]
    property PersonName: string read FPersonName write FPersonName;

    [Column('person_age')]
    property PersonAge: SmallInt read FPersonAge write FPersonAge;

    [Column('salary')]
    property Salary: Double read FSalary write FSalary;

    [HasMany('PersonId', 'Id')]
    property Addresses: TObjectList<TPersonAddress> read FAddresses write FAddresses;

    constructor Create; override;
  end;

implementation

constructor TPerson.Create;
begin
  inherited;
  Self.FAddresses := TObjectList<TPersonAddress>.Create(True);
end;

constructor TPersonAddress.Create;
begin
  inherited;
  Self.Person := TPerson.Create;
end;

end.
