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
    FCreatedBy: Int64;
    FUpdatedAt: TDateTime;
    FUpdatedBy: Int64;
    FDeletedAt: TDateTime;
    FDeletedBy: Int64;
  protected
    function Validate: TValidationResult; virtual;
    function ValidateProperty(const APropertyName: string; const AValue: Variant): TValidationResult;
  public
    [Column('id', [cpPrimaryKey, cpAutoIncrement], [cucFind])]
    property Id: Int64 read FId write FId;

    [Column('created_at', [cpNotNull], [cucAdd, cucFind])]
    [CreatedAt]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;

    [Column('created_by', [], [cucAdd, cucFind])]
    [CreatedBy]
    property CreatedBy: Int64 read FCreatedBy write FCreatedBy;

    [Column('updated_at', [], [cucUpdate, cucFind])]
    [UpdatedAt]
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;

    [Column('updated_by', [], [cucUpdate, cucFind])]
    [UpdatedBy]
    property UpdatedBy: Int64 read FUpdatedBy write FUpdatedBy;

    [Column('deleted_at', [], [cucFind])]
    property DeletedAt: TDateTime read FDeletedAt write FDeletedAt;

    [Column('deleted_by', [], [cucFind])]
    property DeletedBy: Int64 read FDeletedBy write FDeletedBy;

    constructor Create; virtual;
    function IsDeleted: Boolean;
  end;

implementation

constructor TEntity.Create;
begin
  inherited;
  FId := 0;
  FCreatedAt := 0;
  FUpdatedAt := 0;
  FCreatedBy := 0;
  FUpdatedBy := 0;
  FDeletedAt := 0;
  FDeletedBy := 0;
end;

function TEntity.IsDeleted: Boolean;
begin
  Result := FDeletedAt > 0;
end;

function TEntity.Validate: TValidationResult;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  PropertyValue: TValue;
  PropertyResult: TValidationResult;
  HasColumnAttribute: Boolean;
  Attribute: TCustomAttribute;
  Error: TValidationError;
begin
  Result := TValidationResult.Create;

  try
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(Self.ClassType);
      if not Assigned(RttiType) then
        Exit;

      // Check all properties
      for RttiProperty in RttiType.GetProperties do
      begin
        // Only validate properties with Column attribute
        HasColumnAttribute := False;
        for Attribute in RttiProperty.GetAttributes do
        begin
          if Attribute is Column then
          begin
            HasColumnAttribute := True;
            Break;
          end;
        end;

        if not HasColumnAttribute then
          Continue;

        try
          // Get property value
          PropertyValue := RttiProperty.GetValue(Self);

          // Validate property
          PropertyResult := ValidateProperty(RttiProperty.Name, PropertyValue.AsVariant);
          try
            // Add errors to main result
            if not PropertyResult.IsValid then
            begin
              for Error in PropertyResult.Errors do
                Result.AddError(Error.FieldName, Error.Message);
            end;
          finally
            PropertyResult.Free;
          end;
        except
          on E: Exception do
          begin
            Result.AddError(RttiProperty.Name, 'Property validation error: ' + E.Message);
          end;
        end;
      end;
    finally
      RttiContext.Free;
    end;
  except
    on E: Exception do
    begin
      Result.AddError('General', 'Validation error: ' + E.Message);
    end;
  end;
end;

function TEntity.ValidateProperty(const APropertyName: string; const AValue: Variant): TValidationResult;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  Attribute: TCustomAttribute;
  ValidationResult: TValidationResult;
  Error: TValidationError;
begin
  Result := TValidationResult.Create;

  try
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(Self.ClassType);
      if not Assigned(RttiType) then
        Exit;

      RttiProperty := RttiType.GetProperty(APropertyName);
      if not Assigned(RttiProperty) then
        Exit;

      // Check all validation attributes
      for Attribute in RttiProperty.GetAttributes do
      begin
        ValidationResult := nil;

        // Required validation
        if Attribute is Required then
          ValidationResult := Required(Attribute).Validate(AValue, APropertyName)
        // MinLength validation
        else if Attribute is MinLength then
          ValidationResult := MinLength(Attribute).Validate(AValue, APropertyName)
        // MaxLength validation
        else if Attribute is MaxLength then
          ValidationResult := MaxLength(Attribute).Validate(AValue, APropertyName)
        // Range validation
        else if Attribute is Range then
          ValidationResult := Range(Attribute).Validate(AValue, APropertyName)
        // Email validation
        else if Attribute is Email then
          ValidationResult := Email(Attribute).Validate(AValue, APropertyName)
        // RegEx validation
        else if Attribute is RegEx then
          ValidationResult := RegEx(Attribute).Validate(AValue, APropertyName);

        // Process validation result
        if Assigned(ValidationResult) then
        try
          if not ValidationResult.IsValid then
          begin
            for Error in ValidationResult.Errors do
              Result.AddError(Error.FieldName, Error.Message);
          end;
        finally
          ValidationResult.Free;
        end;
      end;
    finally
      RttiContext.Free;
    end;
  except
    on E: Exception do
    begin
      Result.AddError(APropertyName, 'Validation error: ' + E.Message);
    end;
  end;
end;

end.
