unit SysGunler;

interface

uses
  Generics.Collections, SysUtils, Entity, EntityAttributes;

type
  [Table('sys_gunler', 'public')]
  TSysGun = class(TEntity)
  private
    FDayName: string;
  public
    [Column('gun_adi', [cpUnique, cpNotNull], 16, 0, 0)]
    property DayName: string read FDayName write FDayName;
  end;

implementation

end.
