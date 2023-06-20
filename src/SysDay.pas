unit SysDay;

interface

uses
  Entity, Generics.Collections, SysUtils;

type
  [EntityAttribute('sys_days', 'public')]
  TSysDay = class(TEntity)
  private
    FDayName: string;
    FTemp: string;
  public
    [ColumnAttribute('day_name', [cpUnique, cpNotNull], 16, 0, 0)]
    property DayName: string read FDayName write FDayName;

    [NotMapped]
    property Temp: string read FTemp write FTemp;
  end;

implementation

end.
