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
  public
    [Column('id', [cpPrimaryKey, cpUnique, cpNotNull, cpAutoIncrement], [cucFind])]
    property Id: Int64 read FId write FId;

    constructor Create; virtual;
  end;

implementation

constructor TEntity.Create;
begin
  inherited;
end;

end.
