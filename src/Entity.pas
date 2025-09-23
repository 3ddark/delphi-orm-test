unit Entity;

interface

uses
  SysUtils, StrUtils, Classes, Generics.Collections, System.TypInfo, Rtti,
  FireDAC.Comp.Client, Data.DB, EntityAttributes;

type
  IEntity = interface
    ['{03DA6AFF-C934-443B-976E-3D400662465C}']
  end;

  TEntity = class(TInterfacedObject, IEntity)
  private
    FId: Int64;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
  public
    [Column('id', [cpPrimaryKey, cpAutoIncrement], [cucFind])]
    property Id: Int64 read FId write FId;

    [Column('created_at', [cpNotNull], [cucAdd, cucFind])]
    [CreatedAt]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;

    [Column('updated_at', [], [cucUpdate, cucFind])]
    [UpdatedAt]
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;

    constructor Create; virtual;
  end;

implementation

constructor TEntity.Create;
begin
  inherited;
end;

end.
