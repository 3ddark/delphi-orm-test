unit EntityAttributes;

interface

uses
  System.Classes, System.SysUtils, System.Variants, System.Generics.Collections,
  LocalizationManager;

type
  TColumnProperty = (cpNotNull, cpUnique, cpPrimaryKey, cpAutoIncrement);
  TColumnProperties = set of TColumnProperty;

  TColumnUseCriteria = (cucFind, cucAdd, cucUpdate, cucDelete);
  TColumnUseCriterias = set of TColumnUseCriteria;

  TDataType = (dtString, dtInteger, dtBigInt, dtFloat, dtDouble, dtDecimal, dtDateTime,
               dtDate, dtTime, dtBoolean, dtText, dtBlob, dtGUID, dtJSON, dtEnum,
               //postgres special types
               dtUUID, dtArray, dtJSONB, dtHStore, dtPoint, dtPolygon, dtInet, dtMacAddr,
               dtTSVector, dtInterval, dtNumeric, dtSerial, dtBigSerial, dtBytea);

  TCascadeAction = (caNone, caRestrict, caCascade, caSetNull, caSetDefault);

  TValidationError = class
  private
    FFieldName: string;
    FMessage: string;
  public
    constructor Create(const AFieldName, AMessage: string);
    property FieldName: string read FFieldName;
    property Message: string read FMessage;
  end;

  TValidationResult = class
  private
    FIsValid: Boolean;
    FErrors: TList<TValidationError>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddError(const AFieldName, AMessage: string);
    function GetErrors: TArray<TValidationError>;
    property IsValid: Boolean read FIsValid;
    property Errors: TArray<TValidationError> read GetErrors;
  end;

  Table = class(TCustomAttribute)
  private
    FName: string;
    FSchema: string;
    FEngine: string;
    FCharset: string;
  public
    constructor Create(const AName: string; const ASchema: string = '';
                      const AEngine: string = ''; const ACharset: string = 'utf8mb4');
    property Name: string read FName;
    property Schema: string read FSchema;
    property Engine: string read FEngine;
    property Charset: string read FCharset;
    function FullName: string;
  end;

  Column = class(TCustomAttribute)
  private
    FName: string;
    FProperties: TColumnProperties;
    FSqlUseWhichCols: TColumnUseCriterias;
    FDataType: TDataType;
    FLength: Integer;
    FPrecision: Integer;
    FScale: Integer;
    FComment: string;
  public
    constructor Create(const AName: string; AProperties: TColumnProperties = [];
                      ASqlUseWhichCols: TColumnUseCriterias = [];
                      ADataType: TDataType = dtString; ALength: Integer = 0;
                      APrecision: Integer = 0; AScale: Integer = 0;
                      const AComment: string = '');
    property Name: string read FName;
    property Properties: TColumnProperties read FProperties;
    property SqlUseWhichCols: TColumnUseCriterias read FSqlUseWhichCols;
    property DataType: TDataType read FDataType;
    property Length: Integer read FLength;
    property Precision: Integer read FPrecision;
    property Scale: Integer read FScale;
    property Comment: string read FComment;
    function IsPrimaryKey: Boolean;
    function IsNotNull: Boolean;
    function IsUnique: Boolean;
    function IsAutoIncrement: Boolean;
  end;

  Inherits = class(TCustomAttribute)
  private
    FParentTable: string;
  public
    constructor Create(const AParentTable: string);
    property ParentTable: string read FParentTable;
  end;

  Index = class(TCustomAttribute)
  private
    FName: string;
    FColumns: TArray<string>;
    FUnique: Boolean;
    FType: string;
  public
    constructor Create(const AName: string; const AColumns: array of string; AUnique: Boolean = False; const AType: string = 'BTREE'); overload;
    constructor Create(const AName: string; const AColumn: string; AUnique: Boolean = False; const AType: string = 'BTREE'); overload;
    property Name: string read FName;
    property Columns: TArray<string> read FColumns;
    property Unique: Boolean read FUnique;
    property IndexType: string read FType;
  end;

  UniqueIndex = class(Index)
  public
    constructor Create(const AName: string; const AColumns: TArray<string>; const AType: string = 'BTREE');
  end;

  CompositeKey = class(TCustomAttribute)
  private
    FColumns: TArray<string>;
  public
    constructor Create(const AColumns: TArray<string>);
    property Columns: TArray<string> read FColumns;
  end;

  Required = class(TCustomAttribute)
  private
    FMessage: string;
    FMessageKey: string;
    FUseTranslation: Boolean;
  public
    constructor Create(const AMessage: string = ''); overload;
    constructor Create(const AMessageKey: string; const AUseTranslation: Boolean); overload;
    function Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
    property Message: string read FMessage;
    property MessageKey: string read FMessageKey;
    property UseTranslation: Boolean read FUseTranslation;
  end;

  MinLength = class(TCustomAttribute)
  private
    FMinLength: Integer;
    FMessage: string;
    FMessageKey: string;
    FUseTranslation: Boolean;
  public
    constructor Create(AMinLength: Integer; const AMessage: string = ''); overload;
    constructor Create(AMinLength: Integer; const AMessageKey: string; const AUseTranslation: Boolean); overload;
    function Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
    property MinLength: Integer read FMinLength;
    property UseTranslation: Boolean read FUseTranslation;
  end;

  MaxLength = class(TCustomAttribute)
  private
    FMaxLength: Integer;
    FMessage: string;
    FMessageKey: string;
    FUseTranslation: Boolean;
  public
    constructor Create(AMaxLength: Integer; const AMessage: string = ''); overload;
    constructor Create(AMaxLength: Integer; const AMessageKey: string; const AUseTranslation: Boolean); overload;
    function Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
    property MaxLength: Integer read FMaxLength;
    property UseTranslation: Boolean read FUseTranslation;
  end;

  Range = class(TCustomAttribute)
  private
    FMinValue: Variant;
    FMaxValue: Variant;
    FMessage: string;
    FMessageKey: string;
    FUseTranslation: Boolean;
  public
    constructor Create(const AMinValue, AMaxValue: Variant; const AMessage: string = ''); overload;
    constructor Create(const AMinValue, AMaxValue: Variant; const AMessageKey: string; const AUseTranslation: Boolean); overload;
    function Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
    property UseTranslation: Boolean read FUseTranslation;
  end;

  Email = class(TCustomAttribute)
  private
    FMessage: string;
    FMessageKey: string;
    FUseTranslation: Boolean;
  public
    constructor Create(const AMessage: string = 'Invalid email format'); overload;
    constructor Create(const AMessageKey: string; const AUseTranslation: Boolean); overload;
    function Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
    property UseTranslation: Boolean read FUseTranslation;
  end;

  RegEx = class(TCustomAttribute)
  private
    FPattern: string;
    FMessage: string;
    FMessageKey: string;
    FUseTranslation: Boolean;
  public
    constructor Create(const APattern: string; const AMessage: string = 'Invalid format'); overload;
    constructor Create(const APattern: string; const AMessageKey: string; const AUseTranslation: Boolean); overload;
    function Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
    property UseTranslation: Boolean read FUseTranslation;
  end;

  CreatedAt = class(TCustomAttribute)
  private
    FColumnName: string;
    FAutoUpdate: Boolean;
  public
    constructor Create(const AColumnName: string = 'created_at'; AAutoUpdate: Boolean = True);
    property ColumnName: string read FColumnName;
    property AutoUpdate: Boolean read FAutoUpdate;
  end;

  UpdatedAt = class(TCustomAttribute)
  private
    FColumnName: string;
    FAutoUpdate: Boolean;
  public
    constructor Create(const AColumnName: string = 'updated_at'; AAutoUpdate: Boolean = True);
    property ColumnName: string read FColumnName;
    property AutoUpdate: Boolean read FAutoUpdate;
  end;

  CreatedBy = class(TCustomAttribute)
  private
    FColumnName: string;
    FUserIdProvider: string;
  public
    constructor Create(const AColumnName: string = 'created_by'; const AUserIdProvider: string = '');
    property ColumnName: string read FColumnName;
    property UserIdProvider: string read FUserIdProvider;
  end;

  UpdatedBy = class(TCustomAttribute)
  private
    FColumnName: string;
    FUserIdProvider: string;
  public
    constructor Create(const AColumnName: string = 'updated_by'; const AUserIdProvider: string = '');
    property ColumnName: string read FColumnName;
    property UserIdProvider: string read FUserIdProvider;
  end;

  SoftDelete = class(TCustomAttribute)
  private
    FDeletedAtColumn: string;
    FDeletedByColumn: string;
  public
    constructor Create(const ADeletedAtColumn: string = 'deleted_at'; const ADeletedByColumn: string = '');
    property DeletedAtColumn: string read FDeletedAtColumn;
    property DeletedByColumn: string read FDeletedByColumn;
  end;

  HasManyAttribute = class(TCustomAttribute)
  private
    FForeignKeyProperty: string;
    FLocalKeyProperty: string;
    FRelatedClass: string;
    FOnDelete: TCascadeAction;
    FOnUpdate: TCascadeAction;
    FOrderBy: string;
    FWhere: string;
  public
    constructor Create(const AForeignKeyProperty: string = '';
                      const ALocalKeyProperty: string = 'Id';
                      const ARelatedClass: string = '';
                      AOnDelete: TCascadeAction = caNone;
                      AOnUpdate: TCascadeAction = caNone;
                      const AOrderBy: string = '';
                      const AWhere: string = '');

    property ForeignKeyProperty: string read FForeignKeyProperty;
    property LocalKeyProperty: string read FLocalKeyProperty;
    property RelatedClass: string read FRelatedClass;
    property OnDelete: TCascadeAction read FOnDelete;
    property OnUpdate: TCascadeAction read FOnUpdate;
    property OrderBy: string read FOrderBy;
    property Where: string read FWhere;
  end;

  HasOneAttribute = class(TCustomAttribute)
  private
    FForeignKeyProperty: string;
    FLocalKeyProperty: string;
    FRelatedClass: string;
    FOnDelete: TCascadeAction;
    FOnUpdate: TCascadeAction;
  public
    constructor Create(const AForeignKeyProperty: string = '';
                      const ALocalKeyProperty: string = 'Id';
                      const ARelatedClass: string = '';
                      AOnDelete: TCascadeAction = caNone;
                      AOnUpdate: TCascadeAction = caNone);

    property ForeignKeyProperty: string read FForeignKeyProperty;
    property LocalKeyProperty: string read FLocalKeyProperty;
    property RelatedClass: string read FRelatedClass;
    property OnDelete: TCascadeAction read FOnDelete;
    property OnUpdate: TCascadeAction read FOnUpdate;
  end;

  BelongsToAttribute = class(TCustomAttribute)
  private
    FLocalKeyProperty: string;
    FRemoteKeyProperty: string;
    FRelatedClass: string;
    FOnDelete: TCascadeAction;
    FOnUpdate: TCascadeAction;
  public
    constructor Create(const ALocalKeyProperty: string = '';
                      const ARemoteKeyProperty: string = 'Id';
                      const ARelatedClass: string = '';
                      AOnDelete: TCascadeAction = caNone;
                      AOnUpdate: TCascadeAction = caNone);

    property LocalKeyProperty: string read FLocalKeyProperty;
    property RemoteKeyProperty: string read FRemoteKeyProperty;
    property RelatedClass: string read FRelatedClass;
    property OnDelete: TCascadeAction read FOnDelete;
    property OnUpdate: TCascadeAction read FOnUpdate;
  end;

  ManyToManyAttribute = class(TCustomAttribute)
  private
    FPivotTable: string;
    FLocalKey: string;
    FRemoteKey: string;
    FClassName: string;
    FTableName: string;
    FPivotLocalKey: string;
    FPivotRemoteKey: string;
    FOrderBy: string;
    FWhere: string;
  public
    constructor Create(const APivotTable: string;
                      const ALocalKey: string = '';
                      const ARemoteKey: string = '';
                      const AClassName: string = '';
                      const ATableName: string = '';
                      const APivotLocalKey: string = '';
                      const APivotRemoteKey: string = '';
                      const AOrderBy: string = '';
                      const AWhere: string = '');

    property PivotTable: string read FPivotTable;
    property LocalKey: string read FLocalKey;
    property RemoteKey: string read FRemoteKey;
    property ClassName: string read FClassName;
    property TableName: string read FTableName;
    property PivotLocalKey: string read FPivotLocalKey;
    property PivotRemoteKey: string read FPivotRemoteKey;
    property OrderBy: string read FOrderBy;
    property Where: string read FWhere;
  end;

  NotMapped = class(TCustomAttribute)
  public
    constructor Create;
  end;

  Transient = class(TCustomAttribute)
  public
    constructor Create;
  end;

  Version = class(TCustomAttribute)
  private
    FColumnName: string;
  public
    constructor Create(const AColumnName: string = 'version');
    property ColumnName: string read FColumnName;
  end;

  JsonColumn = class(TCustomAttribute)
  private
    FColumnName: string;
  public
    constructor Create(const AColumnName: string = '');
    property ColumnName: string read FColumnName;
  end;

  Enum = class(TCustomAttribute)
  private
    FValues: TArray<string>;
    FDefaultValue: string;
  public
    constructor Create(const AValues: TArray<string>; const ADefaultValue: string = '');
    property Values: TArray<string> read FValues;
    property DefaultValue: string read FDefaultValue;
  end;

implementation

uses
  System.RegularExpressions;

constructor TValidationError.Create(const AFieldName, AMessage: string);
begin
  inherited Create;
  FFieldName := AFieldName;
  FMessage := AMessage;
end;

constructor TValidationResult.Create;
begin
  inherited Create;
  FIsValid := True;
  FErrors := TList<TValidationError>.Create;
end;

destructor TValidationResult.Destroy;
begin
  if Assigned(FErrors) then
  begin
    while FErrors.Count > 0 do
    begin
      FErrors[0].Free;
      FErrors.Delete(0);
    end;
    FErrors.Free;
  end;
  inherited;
end;

procedure TValidationResult.AddError(const AFieldName, AMessage: string);
begin
  FErrors.Add(TValidationError.Create(AFieldName, AMessage));
  FIsValid := False;
end;

function TValidationResult.GetErrors: TArray<TValidationError>;
begin
  Result := FErrors.ToArray;
end;

constructor Table.Create(const AName: string; const ASchema: string = ''; const AEngine: string = ''; const ACharset: string = 'utf8mb4');
begin
  inherited Create;
  FName := AName;
  FSchema := ASchema;
  FEngine := AEngine;
  FCharset := ACharset;
end;

function Table.FullName: string;
begin
  if FSchema <> '' then
    Result := FSchema + '.' + FName
  else
    Result := FName;
end;

constructor Column.Create(const AName: string; AProperties: TColumnProperties = [];
                         ASqlUseWhichCols: TColumnUseCriterias = [];
                         ADataType: TDataType = dtString; ALength: Integer = 0;
                         APrecision: Integer = 0; AScale: Integer = 0;
                         const AComment: string = '');
begin
  inherited Create;
  FName := AName;
  FProperties := AProperties;
  FSqlUseWhichCols := ASqlUseWhichCols;
  FDataType := ADataType;
  FLength := ALength;
  FPrecision := APrecision;
  FScale := AScale;
  FComment := AComment;
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

constructor Index.Create(const AName: string; const AColumns: array of string; AUnique: Boolean = False; const AType: string = 'BTREE');
var
  I: Integer;
begin
  inherited Create;

  FName := AName;
  FUnique := AUnique;
  FType := AType;

  SetLength(FColumns, Length(AColumns));
  for I := Low(AColumns) to High(AColumns) do
    FColumns[I] := AColumns[I];
end;

constructor Index.Create(const AName: string; const AColumn: string; AUnique: Boolean = False; const AType: string = 'BTREE');
begin
  inherited Create;
  FName := AName;
  FColumns := TArray<string>.Create(AColumn);
  FUnique := AUnique;
  FType := AType;
end;

constructor UniqueIndex.Create(const AName: string; const AColumns: TArray<string>; const AType: string = 'BTREE');
begin
  inherited Create(AName, AColumns, True, AType);
end;

constructor CompositeKey.Create(const AColumns: TArray<string>);
begin
  inherited Create;
  FColumns := AColumns;
end;

constructor Required.Create(const AMessage: string = '');
begin
  inherited Create;
  FMessage := AMessage;
  FMessageKey := '';
  FUseTranslation := False;
  if FMessage = '' then
    FMessage := 'Field is required';
end;

constructor Required.Create(const AMessageKey: string; const AUseTranslation: Boolean);
begin
  inherited Create;
  FMessageKey := AMessageKey;
  FUseTranslation := AUseTranslation;
  if AUseTranslation then
    FMessage := ''
  else
    FMessage := AMessageKey;
end;

function Required.Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
var
  ErrorMessage: string;
begin
  Result := TValidationResult.Create;
  if VarIsNull(AValue) or VarIsEmpty(AValue) or (VarToStr(AValue) = '') then
  begin
    if FUseTranslation and (FMessageKey <> '') then
      ErrorMessage := TLocalizationManager.Translate(FMessageKey, FMessage)
    else
      ErrorMessage := FMessage;

    Result.AddError(AFieldName, ErrorMessage);
  end;
end;

constructor MinLength.Create(AMinLength: Integer; const AMessage: string = '');
begin
  inherited Create;
  FMinLength := AMinLength;
  FMessage := AMessage;
  FMessageKey := '';
  FUseTranslation := False;
  if FMessage = '' then
    FMessage := Format('Field must be at least %d characters long', [AMinLength]);
end;

constructor MinLength.Create(AMinLength: Integer; const AMessageKey: string; const AUseTranslation: Boolean);
begin
  inherited Create;
  FMinLength := AMinLength;
  FMessageKey := AMessageKey;
  FUseTranslation := AUseTranslation;
  if AUseTranslation then
    FMessage := ''
  else
    FMessage := AMessageKey;
end;

function MinLength.Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
var
  StrValue: string;
  ErrorMessage: string;
begin
  Result := TValidationResult.Create;
  if not VarIsNull(AValue) and not VarIsEmpty(AValue) then
  begin
    StrValue := VarToStr(AValue);
    if Length(StrValue) < FMinLength then
    begin
      if FUseTranslation and (FMessageKey <> '') then
        ErrorMessage := TLocalizationManager.Translate(FMessageKey, [FMinLength], FMessage)
      else
        ErrorMessage := FMessage;

      Result.AddError(AFieldName, ErrorMessage);
    end;
  end;
end;

constructor MaxLength.Create(AMaxLength: Integer; const AMessage: string = '');
begin
  inherited Create;
  FMaxLength := AMaxLength;
  FMessage := AMessage;
  FMessageKey := '';
  FUseTranslation := False;
  if AMessage = '' then
    FMessage := Format('Field must not exceed %d characters', [AMaxLength]);
end;

constructor MaxLength.Create(AMaxLength: Integer; const AMessageKey: string; const AUseTranslation: Boolean);
begin
  inherited Create;
  FMaxLength := AMaxLength;
  FMessageKey := AMessageKey;
  FUseTranslation := AUseTranslation;
  if AUseTranslation then
    FMessage := ''
  else
    FMessage := AMessageKey;
end;

function MaxLength.Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
var
  StrValue: string;
  ErrorMessage: string;
begin
  Result := TValidationResult.Create;
  if not VarIsNull(AValue) and not VarIsEmpty(AValue) then
  begin
    StrValue := VarToStr(AValue);
    if Length(StrValue) > FMaxLength then
    begin
      if FUseTranslation and (FMessageKey <> '') then
        ErrorMessage := TLocalizationManager.Translate(FMessageKey, [FMaxLength], FMessage)
      else
        ErrorMessage := FMessage;

      Result.AddError(AFieldName, ErrorMessage);
    end;
  end;
end;

constructor Range.Create(const AMinValue, AMaxValue: Variant; const AMessage: string = '');
begin
  inherited Create;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FMessage := AMessage;
  FMessageKey := '';
  FUseTranslation := False;
  if AMessage = '' then
    FMessage := Format('Field must be between %s and %s', [VarToStr(AMinValue), VarToStr(AMaxValue)]);
end;

constructor Range.Create(const AMinValue, AMaxValue: Variant; const AMessageKey: string; const AUseTranslation: Boolean);
begin
  inherited Create;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FMessageKey := AMessageKey;
  FUseTranslation := AUseTranslation;
  if AUseTranslation then
    FMessage := ''
  else
    FMessage := AMessageKey;
end;

function Range.Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
var
  ErrorMessage: string;
begin
  Result := TValidationResult.Create;
  if not VarIsNull(AValue) and not VarIsEmpty(AValue) then
  begin
    if (AValue < FMinValue) or (AValue > FMaxValue) then
    begin
      if FUseTranslation and (FMessageKey <> '') then
        ErrorMessage := TLocalizationManager.Translate(FMessageKey, [VarToStr(FMinValue), VarToStr(FMaxValue)], FMessage)
      else
        ErrorMessage := FMessage;

      Result.AddError(AFieldName, ErrorMessage);
    end;
  end;
end;

constructor Email.Create(const AMessage: string = 'Invalid email format');
begin
  inherited Create;
  FMessage := AMessage;
  FMessageKey := '';
  FUseTranslation := False;
end;

constructor Email.Create(const AMessageKey: string; const AUseTranslation: Boolean);
begin
  inherited Create;
  FMessageKey := AMessageKey;
  FUseTranslation := AUseTranslation;
  if AUseTranslation then
    FMessage := ''
  else
    FMessage := AMessageKey;
end;

function Email.Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
var
  EmailRegex: string;
  StrValue: string;
  ErrorMessage: string;
begin
  Result := TValidationResult.Create;
  if not VarIsNull(AValue) and not VarIsEmpty(AValue) then
  begin
    StrValue := VarToStr(AValue);
    EmailRegex := '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}';
    if not TRegEx.IsMatch(StrValue, EmailRegex) then
    begin
      if FUseTranslation and (FMessageKey <> '') then
        ErrorMessage := TLocalizationManager.Translate(FMessageKey, FMessage)
      else
        ErrorMessage := FMessage;

      Result.AddError(AFieldName, ErrorMessage);
    end;
  end;
end;

constructor RegEx.Create(const APattern: string; const AMessage: string = 'Invalid format');
begin
  inherited Create;
  FPattern := APattern;
  FMessage := AMessage;
  FMessageKey := '';
  FUseTranslation := False;
end;

constructor RegEx.Create(const APattern: string; const AMessageKey: string; const AUseTranslation: Boolean);
begin
  inherited Create;
  FPattern := APattern;
  FMessageKey := AMessageKey;
  FUseTranslation := AUseTranslation;
  if AUseTranslation then
    FMessage := ''
  else
    FMessage := AMessageKey;
end;

function RegEx.Validate(const AValue: Variant; const AFieldName: string): TValidationResult;
var
  StrValue: string;
  ErrorMessage: string;
begin
  Result := TValidationResult.Create;
  if not VarIsNull(AValue) and not VarIsEmpty(AValue) then
  begin
    StrValue := VarToStr(AValue);
    if not TRegEx.IsMatch(StrValue, FPattern) then
    begin
      if FUseTranslation and (FMessageKey <> '') then
        ErrorMessage := TLocalizationManager.Translate(FMessageKey, FMessage)
      else
        ErrorMessage := FMessage;

      Result.AddError(AFieldName, ErrorMessage);
    end;
  end;
end;

constructor CreatedAt.Create(const AColumnName: string = 'created_at'; AAutoUpdate: Boolean = True);
begin
  inherited Create;
  FColumnName := AColumnName;
  FAutoUpdate := AAutoUpdate;
end;

constructor UpdatedAt.Create(const AColumnName: string = 'updated_at'; AAutoUpdate: Boolean = True);
begin
  inherited Create;
  FColumnName := AColumnName;
  FAutoUpdate := AAutoUpdate;
end;

constructor CreatedBy.Create(const AColumnName: string = 'created_by'; const AUserIdProvider: string = '');
begin
  inherited Create;
  FColumnName := AColumnName;
  FUserIdProvider := AUserIdProvider;
end;

constructor UpdatedBy.Create(const AColumnName: string = 'updated_by'; const AUserIdProvider: string = '');
begin
  inherited Create;
  FColumnName := AColumnName;
  FUserIdProvider := AUserIdProvider;
end;

constructor SoftDelete.Create(const ADeletedAtColumn: string = 'deleted_at';
                             const ADeletedByColumn: string = '');
begin
  inherited Create;
  FDeletedAtColumn := ADeletedAtColumn;
  FDeletedByColumn := ADeletedByColumn;
end;

constructor HasManyAttribute.Create(const AForeignKeyProperty: string = '';
                                   const ALocalKeyProperty: string = 'Id';
                                   const ARelatedClass: string = '';
                                   AOnDelete: TCascadeAction = caNone;
                                   AOnUpdate: TCascadeAction = caNone;
                                   const AOrderBy: string = '';
                                   const AWhere: string = '');
begin
  inherited Create;
  FForeignKeyProperty := AForeignKeyProperty;
  FLocalKeyProperty := ALocalKeyProperty;
  FRelatedClass := ARelatedClass;
  FOnDelete := AOnDelete;
  FOnUpdate := AOnUpdate;
  FOrderBy := AOrderBy;
  FWhere := AWhere;
end;

constructor HasOneAttribute.Create(const AForeignKeyProperty: string = '';
                                  const ALocalKeyProperty: string = 'Id';
                                  const ARelatedClass: string = '';
                                  AOnDelete: TCascadeAction = caNone;
                                  AOnUpdate: TCascadeAction = caNone);
begin
  inherited Create;
  FForeignKeyProperty := AForeignKeyProperty;
  FLocalKeyProperty := ALocalKeyProperty;
  FRelatedClass := ARelatedClass;
  FOnDelete := AOnDelete;
  FOnUpdate := AOnUpdate;
end;

constructor BelongsToAttribute.Create(const ALocalKeyProperty: string = '';
                                     const ARemoteKeyProperty: string = 'Id';
                                     const ARelatedClass: string = '';
                                     AOnDelete: TCascadeAction = caNone;
                                     AOnUpdate: TCascadeAction = caNone);
begin
  inherited Create;
  FLocalKeyProperty := ALocalKeyProperty;
  FRemoteKeyProperty := ARemoteKeyProperty;
  FRelatedClass := ARelatedClass;
  FOnDelete := AOnDelete;
  FOnUpdate := AOnUpdate;
end;

constructor ManyToManyAttribute.Create(const APivotTable: string;
                                      const ALocalKey: string = '';
                                      const ARemoteKey: string = '';
                                      const AClassName: string = '';
                                      const ATableName: string = '';
                                      const APivotLocalKey: string = '';
                                      const APivotRemoteKey: string = '';
                                      const AOrderBy: string = '';
                                      const AWhere: string = '');
begin
  inherited Create;
  FPivotTable := APivotTable;
  FLocalKey := ALocalKey;
  FRemoteKey := ARemoteKey;
  FClassName := AClassName;
  FTableName := ATableName;
  FPivotLocalKey := APivotLocalKey;
  FPivotRemoteKey := APivotRemoteKey;
  FOrderBy := AOrderBy;
  FWhere := AWhere;
end;

constructor NotMapped.Create;
begin
  inherited Create;
end;

constructor Transient.Create;
begin
  inherited Create;
end;

constructor Version.Create(const AColumnName: string = 'version');
begin
  inherited Create;
  FColumnName := AColumnName;
end;

constructor JsonColumn.Create(const AColumnName: string = '');
begin
  inherited Create;
  FColumnName := AColumnName;
end;

constructor Enum.Create(const AValues: TArray<string>; const ADefaultValue: string = '');
begin
  inherited Create;
  FValues := AValues;
  FDefaultValue := ADefaultValue;
end;

constructor Inherits.Create(const AParentTable: string);
begin
  inherited Create;
  FParentTable := AParentTable;
end;

end.
