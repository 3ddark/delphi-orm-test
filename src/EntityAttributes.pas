unit EntityAttributes;

interface

uses
  System.Classes;

type
  TColumnProperty = (cpNotNull, cpUnique, cpPrimaryKey, cpAutoIncrement);
  TColumnProperties = set of TColumnProperty;

  TColumnUseCriteria = (cucFind, cucAdd, cucUpdate, cucDelete);
  TColumnUseCriterias = set of TColumnUseCriteria;

  Table = class(TCustomAttribute)
  private
    FName: string;
    FSchema: string;
  public
    constructor Create(const AName: string; const ASchema: string = ''); overload;
    property Name: string read FName;
    property Schema: string read FSchema;
    function FullName: string;
  end;

  Column = class(TCustomAttribute)
  private
    FName: string;
    FProperties: TColumnProperties;
    FSqlUseWhichCols: TColumnUseCriterias;
  public
    constructor Create(const AName: string = ''; AProperties: TColumnProperties = []; ASqlUseWhichCols: TColumnUseCriterias = []); overload;
    property Name: string read FName;
    property Properties: TColumnProperties read FProperties;
    property SqlUseWhichCols: TColumnUseCriterias read FSqlUseWhichCols;
    function IsPrimaryKey: Boolean;
    function IsNotNull: Boolean;
    function IsUnique: Boolean;
    function IsAutoIncrement: Boolean;
  end;

  NotMapped = class(TCustomAttribute)
  public
    constructor Create;
  end;

  HasMany = class(TCustomAttribute)
  private
    FFilterPropertyName: string;
    FValuePropertyName: string;
  public
    constructor Create(const AFilterPropertyName, AValuePropertyName: string);
    property FilterPropertyName: string read FFilterPropertyName;
    property ValuePropertyName: string read FValuePropertyName;
  end;

  HasOne = class(TCustomAttribute)
  private
    FFilterPropertyName: string;
    FValuePropertyName: string;
  public
    constructor Create(const AFilterPropertyName, AValuePropertyName: string);
    property FilterPropertyName: string read FFilterPropertyName;
    property ValuePropertyName: string read FValuePropertyName;
  end;

  BelongsTo = class(TCustomAttribute)
  private
    FLocalKey: string;
    FRemoteKey: string;
  public
    constructor Create(const ALocalKey, ARemoteKey: string);
    property LocalKey: string read FLocalKey;
    property RemoteKey: string read FRemoteKey;
  end;

implementation

constructor Table.Create(const AName: string; const ASchema: string = '');
begin
  inherited Create;
  FName := AName;
  FSchema := ASchema;
end;

function Table.FullName: string;
begin
  if FSchema <> '' then
    Result := FSchema + '.' + FName
  else
    Result := FName;
end;

constructor Column.Create(const AName: string = ''; AProperties: TColumnProperties = []; ASqlUseWhichCols: TColumnUseCriterias = []);
begin
  inherited Create;
  FName := AName;
  FProperties := AProperties;
  FSqlUseWhichCols := ASqlUseWhichCols;
end;

function Column.IsPrimaryKey: Boolean;
begin
  Result := cpPrimaryKey in FProperties;
end;

function Column.IsNotNull: Boolean;
begin
  Result := cpNotNull in FProperties;
end;

function Column.IsUnique: Boolean;
begin
  Result := cpUnique in FProperties;
end;

function Column.IsAutoIncrement: Boolean;
begin
  Result := cpAutoIncrement in FProperties;
end;

constructor NotMapped.Create;
begin
  inherited Create;
end;

constructor HasMany.Create(const AFilterPropertyName, AValuePropertyName: string);
begin
  inherited Create;
  FFilterPropertyName := AFilterPropertyName;
  FValuePropertyName := AValuePropertyName;
end;

constructor HasOne.Create(const AFilterPropertyName, AValuePropertyName: string);
begin
  inherited Create;
  FFilterPropertyName := AFilterPropertyName;
  FValuePropertyName := AValuePropertyName;
end;

constructor BelongsTo.Create(const ALocalKey, ARemoteKey: string);
begin
  inherited Create;
  FLocalKey := ALocalKey;
  FRemoteKey := ARemoteKey;
end;

end.
