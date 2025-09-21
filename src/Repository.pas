unit Repository;

interface

uses
  SysUtils, StrUtils, System.Classes, Generics.Collections, System.TypInfo,
  Rtti, Data.DB, FireDAC.Comp.Client,
  Entity, EntityAttributes, FilterCriterion;

type
  IRepository<T> = interface
    ['{808825C5-94CA-4B8F-BCEA-D351F4F6813E}']
  end;

  TEntityManager<T: TEntity> = class(TInterfacedObject, IRepository<T>)
  private
    FConnection: TFDConnection;
    function MethodCall(AClass: T; AMethodName: string; AParameters: array of TValue): T; overload;

    function GenerateSelectSql(AClass: TClass; const AWhereClause: string = ''): string;
    function GetSelectColumns(AClass: TClass): string;
    procedure FillEntityFromDataSet(AEntity: TObject; ADataSet: TFDQuery);
    procedure FillNestedEntities(AEntity: TObject);
    function GetPrimaryKeyColumn(AClass: TClass): string;
    function GetColumnName(AProp: TRttiProperty): string;
    function ShouldUseInSelect(AProp: TRttiProperty): Boolean;
    function CreateEntityInstance<TEntity>: TEntity;
    function CreateEntityInstanceByClass(AClass: TClass): TObject;
    function ExtractGenericTypeFromList(AListType: TRttiType): TClass;
  protected
    function Connection: TFDConnection;
    function GetTableName(AClass: TClass): string;
    function GetFullTableName(AClass: TClass): string;
  public
    function FindById(AId: TValue; ALock: Boolean = False): T;
    function FindOne(AFilter: TFilterCriteria; ALock: Boolean = False): T;
    function Find(AFilter: TFilterCriteria; ALock: Boolean = False): TObjectList<T>;

    procedure Add(AModel: T); overload;
    procedure AddBatch(AModels: TArray<T>); overload;

    procedure Update(AModel: T);
    procedure UpdateBatch(AModels: TArray<T>); overload;

    procedure Delete(AID: Int64); overload;
    procedure Delete(AModel: T); overload;
    procedure DeleteBatch(AModels: TArray<T>); reintroduce; overload;
    procedure DeleteBatch(AIDs: TArray<Int64>); reintroduce; overload;
    procedure DeleteBatch(AFilter: TFilterCriteria); reintroduce; overload;

    function Clone(ASource: T): T;

    constructor Create(AConnection: TFDConnection);
  end;

implementation

constructor TEntityManager<T>.Create(AConnection: TFDConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection Required');

  FConnection := AConnection;
end;

function TEntityManager<T>.Connection: TFDConnection;
begin
  Result := FConnection;
end;

function TEntityManager<T>.GetTableName(AClass: TClass): string;
var
  ACtx: TRttiContext;
  AType: TRttiType;
  AAttr : TCustomAttribute;
begin
  Result := AClass.ClassName;

  ACtx := TRttiContext.Create;
  try
    AType := ACtx.GetType(AClass);
    for AAttr in AType.GetAttributes do
      if AAttr is Table then
      begin
        Result := (AAttr as Table).Name;
        Break;
      end;
  finally
    ACtx.Free;
  end;
end;

function TEntityManager<T>.GetFullTableName(AClass: TClass): string;
var
  ACtx: TRttiContext;
  AType: TRttiType;
  AAttr : TCustomAttribute;
  TableAttr: Table;
begin
  Result := AClass.ClassName;

  ACtx := TRttiContext.Create;
  try
    AType := ACtx.GetType(AClass);
    for AAttr in AType.GetAttributes do
      if AAttr is Table then
      begin
        TableAttr := AAttr as Table;
        Result := TableAttr.FullName;
        Break;
      end;
  finally
    ACtx.Free;
  end;
end;

function TEntityManager<T>.GetPrimaryKeyColumn(AClass: TClass): string;
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
begin
  Result := 'id';

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(AClass);
    for prop in rType.GetProperties do
    begin
      for attr in prop.GetAttributes do
      begin
        if attr is Column then
        begin
          colAttr := attr as Column;
          if colAttr.IsPrimaryKey then
          begin
            if colAttr.Name <> '' then
              Result := colAttr.Name
            else
              Result := LowerCase(prop.Name);
            Exit;
          end;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

function TEntityManager<T>.GetColumnName(AProp: TRttiProperty): string;
var
  attr: TCustomAttribute;
  colAttr: Column;
begin
  Result := LowerCase(AProp.Name);

  for attr in AProp.GetAttributes do
  begin
    if attr is Column then
    begin
      colAttr := attr as Column;
      if colAttr.Name <> '' then
      begin
        Result := colAttr.Name;
        Break;
      end;
    end;
  end;
end;

function TEntityManager<T>.ShouldUseInSelect(AProp: TRttiProperty): Boolean;
var
  attr: TCustomAttribute;
  colAttr: Column;
  hasColumnAttr: Boolean;
begin
  Result := False;
  hasColumnAttr := False;

  for attr in AProp.GetAttributes do
  begin
    if attr is NotMapped then
      Exit(False);

    if attr is HasOne then
      Exit(False);

    if attr is HasMany then
      Exit(False);

    if attr is Column then
    begin
      hasColumnAttr := True;
      colAttr := attr as Column;

      Result := (cucFind in colAttr.SqlUseWhichCols) or (colAttr.SqlUseWhichCols = []);
      Break;
    end;
  end;

  Result := hasColumnAttr and Result;
end;

function TEntityManager<T>.GetSelectColumns(AClass: TClass): string;
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  columns: TStringList;
begin
  columns := TStringList.Create;
  try
    ctx := TRttiContext.Create;
    try
      rType := ctx.GetType(AClass);
      for prop in rType.GetProperties do
      begin
        if ShouldUseInSelect(prop) then
        begin
          columns.Add(GetColumnName(prop));
        end;
      end;
    finally
      ctx.Free;
    end;

    if columns.Count = 0 then
      Result := '*'
    else
      Result := columns.CommaText.Replace('"', '');
  finally
    columns.Free;
  end;
end;

function TEntityManager<T>.GenerateSelectSql(AClass: TClass; const AWhereClause: string): string;
var
  tableName, columns: string;
begin
  tableName := GetFullTableName(AClass);
  columns := GetSelectColumns(AClass);

  Result := Format('SELECT %s FROM %s', [columns, tableName]);

  if AWhereClause <> '' then
    Result := Result + ' WHERE ' + AWhereClause;
end;

function TEntityManager<T>.CreateEntityInstance<TEntity>: TEntity;
var
  ctx: TRttiContext;
  rType: TRttiType;
  method: TRttiMethod;
  rMetod: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(TypeInfo(TEntity));
    rMetod := nil;

    for method in rType.GetMethods do
    begin
      if method.IsConstructor and (Length(method.GetParameters) = 0) then
      begin
        rMetod := method;
        Break;
      end;
    end;

    if Assigned(rMetod) then
      Result := rMetod.Invoke(rType.AsInstance.MetaclassType, []).AsType<TEntity>
    else
      raise Exception.CreateFmt('No default constructor found for %s', [rType.ClassName]);
  finally
    ctx.Free;
  end;
end;

procedure TEntityManager<T>.FillEntityFromDataSet(AEntity: TObject; ADataSet: TFDQuery);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  field: TField;
  columnName: string;
  propValue: TValue;
begin
  if not Assigned(AEntity) or not Assigned(ADataSet) then
    Exit;

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(AEntity.ClassType);

    for prop in rType.GetProperties do
    begin
      if not prop.IsWritable then
        Continue;

      if not ShouldUseInSelect(prop) then
        Continue;

      columnName := GetColumnName(prop);
      field := ADataSet.FindField(columnName);

      if Assigned(field) and not field.IsNull then
      begin
        case prop.PropertyType.TypeKind of
          tkInteger:
            begin
              if prop.PropertyType.Name = 'SmallInt' then
                propValue := TValue.From<SmallInt>(field.AsInteger)
              else
                propValue := TValue.From<Integer>(field.AsInteger);
            end;
          tkInt64:
            propValue := TValue.From<Int64>(field.AsLargeInt);
          tkFloat:
            begin
              if prop.PropertyType.Name = 'Double' then
                propValue := TValue.From<Double>(field.AsFloat)
              else
                propValue := TValue.From<Single>(field.AsFloat);
            end;
          tkString, tkLString, tkWString, tkUString:
            propValue := TValue.From<string>(field.AsString);
          tkEnumeration:
            begin
              if prop.PropertyType.Name = 'Boolean' then
                propValue := TValue.From<Boolean>(field.AsBoolean);
            end;
        end;

        try
          prop.SetValue(AEntity, propValue);
        except
          on E: Exception do
            Continue;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

procedure TEntityManager<T>.FillNestedEntities(AEntity: TObject);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  hasOneAttr: HasOne;
  hasManyAttr: HasMany;
  belongsToAttr: BelongsTo;
  filterProp: TRttiProperty;
  filterValue: TValue;
  nestedQuery: TFDQuery;
  sql: string;
  nestedEntity: TObject;
  nestedList: TObject;
  addMethod: TRttiMethod;
  nestedEntityClass: TClass;
  clearMethod: TRttiMethod;
  valueColumnName: string;
  valueProp: TRttiProperty;
begin
  if not Assigned(AEntity) then
    Exit;

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(AEntity.ClassType);

    for prop in rType.GetProperties do
    begin
      for attr in prop.GetAttributes do
      begin
        if attr is BelongsTo then
        begin
          belongsToAttr := attr as BelongsTo;

          filterProp := rType.GetProperty(belongsToAttr.LocalKey);
          if not Assigned(filterProp) then
            Continue;

          filterValue := filterProp.GetValue(AEntity);
          if filterValue.IsEmpty or (filterValue.AsInt64 = 0) then
            Continue;

          nestedEntityClass := nil;
          if prop.PropertyType.IsInstance then
            nestedEntityClass := prop.PropertyType.AsInstance.MetaclassType;

          if not Assigned(nestedEntityClass) then
            Continue;

          valueColumnName := belongsToAttr.RemoteKey;
          valueProp := ctx.GetType(nestedEntityClass).GetProperty(belongsToAttr.RemoteKey);
          if Assigned(valueProp) then
            valueColumnName := GetColumnName(valueProp);

          nestedQuery := TFDQuery.Create(nil);
          try
            nestedQuery.Connection := FConnection;

            sql := GenerateSelectSql(nestedEntityClass, Format('%s = :%s', [valueColumnName, 'param_value']));
            nestedQuery.SQL.Text := sql;
            nestedQuery.ParamByName('param_value').Value := filterValue.AsVariant;
            nestedQuery.Open;

            if not nestedQuery.IsEmpty then
            begin
              nestedEntity := CreateEntityInstanceByClass(nestedEntityClass);
              FillEntityFromDataSet(nestedEntity, nestedQuery);

              prop.SetValue(AEntity, nestedEntity);

              FillNestedEntities(nestedEntity);
            end;
          finally
            nestedQuery.Free;
          end;
        end
        else if attr is HasOne then
        begin
          hasOneAttr := attr as HasOne;

          filterProp := rType.GetProperty(hasOneAttr.FilterPropertyName);
          if not Assigned(filterProp) then
            Continue;

          filterValue := filterProp.GetValue(AEntity);
          if filterValue.IsEmpty then
            Continue;

          nestedEntityClass := nil;
          if prop.PropertyType.IsInstance then
            nestedEntityClass := prop.PropertyType.AsInstance.MetaclassType;

          if not Assigned(nestedEntityClass) then
            Continue;

          valueColumnName := hasOneAttr.ValuePropertyName;
          valueProp := ctx.GetType(nestedEntityClass).GetProperty(hasOneAttr.ValuePropertyName);
          if Assigned(valueProp) then
            valueColumnName := GetColumnName(valueProp);

          nestedQuery := TFDQuery.Create(nil);
          try
            nestedQuery.Connection := FConnection;

            sql := GenerateSelectSql(nestedEntityClass, Format('%s = :%s', [valueColumnName, 'param_value']));
            nestedQuery.SQL.Text := sql;
            nestedQuery.ParamByName('param_value').Value := filterValue.AsVariant;
            nestedQuery.Open;

            if not nestedQuery.IsEmpty then
            begin
              nestedEntity := CreateEntityInstanceByClass(nestedEntityClass);
              FillEntityFromDataSet(nestedEntity, nestedQuery);
              prop.SetValue(AEntity, nestedEntity);
              FillNestedEntities(nestedEntity);
            end;
          finally
            nestedQuery.Free;
          end;
        end
        else if attr is HasMany then
        begin
          hasManyAttr := attr as HasMany;

          filterProp := rType.GetProperty(hasManyAttr.FilterPropertyName);
          if not Assigned(filterProp) then
            Continue;

          filterValue := filterProp.GetValue(AEntity);
          if filterValue.IsEmpty then
            Continue;

          nestedList := prop.GetValue(AEntity).AsObject;
          if not Assigned(nestedList) then
            Continue;

          nestedEntityClass := ExtractGenericTypeFromList(prop.PropertyType);
          if not Assigned(nestedEntityClass) then
            Continue;

          valueColumnName := hasManyAttr.ValuePropertyName;
          valueProp := ctx.GetType(nestedEntityClass).GetProperty(hasManyAttr.ValuePropertyName);
          if Assigned(valueProp) then
            valueColumnName := GetColumnName(valueProp);

          nestedQuery := TFDQuery.Create(nil);
          try
            nestedQuery.Connection := FConnection;

            sql := GenerateSelectSql(nestedEntityClass, Format('%s = :%s', [valueColumnName, 'param_value']));
            nestedQuery.SQL.Text := sql;
            nestedQuery.ParamByName('param_value').Value := filterValue.AsVariant;
            nestedQuery.Open;

            clearMethod := ctx.GetType(nestedList.ClassType).GetMethod('Clear');
            if Assigned(clearMethod) then
              clearMethod.Invoke(nestedList, []);

            addMethod := ctx.GetType(nestedList.ClassType).GetMethod('Add');
            if Assigned(addMethod) then
            begin
              while not nestedQuery.Eof do
              begin
                nestedEntity := CreateEntityInstanceByClass(nestedEntityClass);
                FillEntityFromDataSet(nestedEntity, nestedQuery);
                addMethod.Invoke(nestedList, [nestedEntity]);

                // Note: Avoid recursive loading for child entities in HasMany to prevent infinite loops
                // FillNestedEntities(nestedEntity); // Uncomment if needed but be careful of circular references

                nestedQuery.Next;
              end;
            end;
          finally
            nestedQuery.Free;
          end;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

function TEntityManager<T>.CreateEntityInstanceByClass(AClass: TClass): TObject;
var
  ctx: TRttiContext;
  rType: TRttiType;
  method: TRttiMethod;
  rMetod: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(AClass);
    rMetod := nil;

    for method in rType.GetMethods do
    begin
      if method.IsConstructor and (Length(method.GetParameters) = 0) then
      begin
        rMetod := method;
        Break;
      end;
    end;

    if Assigned(rMetod) then
      Result := rMetod.Invoke(rType.AsInstance.MetaclassType, []).AsObject
    else
      raise Exception.CreateFmt('No default constructor found for %s', [AClass.ClassName]);
  finally
    ctx.Free;
  end;
end;

function TEntityManager<T>.ExtractGenericTypeFromList(AListType: TRttiType): TClass;
var
  ctx: TRttiContext;
  typeName: string;
  startPos, endPos: Integer;
  genericTypeName: string;
  genericType: TRttiType;
begin
  Result := nil;

  if not Assigned(AListType) then
    Exit;

  typeName := AListType.Name;

  if not typeName.StartsWith('TObjectList<') then
    Exit;

  startPos := Pos('<', typeName);
  endPos := Pos('>', typeName);

  if (startPos > 0) and (endPos > startPos) then
  begin
    genericTypeName := Copy(typeName, startPos + 1, endPos - startPos - 1);

    ctx := TRttiContext.Create;
    try
      genericType := ctx.FindType(genericTypeName);

      if not Assigned(genericType) then
      begin
        genericType := ctx.FindType('System.Generics.Collections.' + genericTypeName);
        if not Assigned(genericType) then
          genericType := ctx.FindType('Generics.Collections.' + genericTypeName);
      end;

      if Assigned(genericType) and genericType.IsInstance then
        Result := genericType.AsInstance.MetaclassType;

    finally
      ctx.Free;
    end;
  end;
end;

function TEntityManager<T>.FindById(AId: TValue; ALock: Boolean = False): T;
var
  query: TFDQuery;
  sql: string;
  pkColumn: string;
  lockClause: string;
begin
  Result := nil;

  if AId.IsEmpty then
    Exit;

  query := TFDQuery.Create(nil);
  try
    query.Connection := FConnection;

    pkColumn := GetPrimaryKeyColumn(T);
    lockClause := IfThen(ALock, ' FOR UPDATE', '');

    sql := GenerateSelectSql(T, Format('%s = :%s', [pkColumn, pkColumn])) + lockClause;

    query.SQL.Text := sql;
    query.ParamByName(pkColumn).Value := AId.AsVariant;
    query.Open;

    if not query.IsEmpty then
    begin
      Result := CreateEntityInstanceByClass(T) as T;
      FillEntityFromDataSet(Result, query);
      FillNestedEntities(Result);
    end;
  finally
    query.Free;
  end;
end;

function TEntityManager<T>.MethodCall(AClass: T; AMethodName: string; AParameters: array of TValue): T;
var
  ctx: TRttiContext;
  m: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    m := ctx.GetType(T).GetMethod(AMethodName);
    if Assigned(m) then
      Result := m.Invoke(T, AParameters).AsType<T>
    else
      raise Exception.Create(Format('Cannot find method "%s" in the object', [AMethodName]));
  finally
    ctx.Free;
  end;
end;

function TEntityManager<T>.FindOne(AFilter: TFilterCriteria; ALock: Boolean): T;
begin
  Result := nil;
  // TODO: Implement based on filter criteria
end;

function TEntityManager<T>.Find(AFilter: TFilterCriteria; ALock: Boolean): TObjectList<T>;
begin
  Result := nil;
  // TODO: Implement based on filter criteria
end;

procedure TEntityManager<T>.Add(AModel: T);
begin
  // TODO: Implement INSERT operation
end;

procedure TEntityManager<T>.AddBatch(AModels: TArray<T>);
begin
  // TODO: Implement batch INSERT operation
end;

procedure TEntityManager<T>.Update(AModel: T);
begin
  // TODO: Implement UPDATE operation
end;

procedure TEntityManager<T>.UpdateBatch(AModels: TArray<T>);
begin
  // TODO: Implement batch UPDATE operation
end;

procedure TEntityManager<T>.Delete(AID: Int64);
begin
  // TODO: Implement DELETE by ID operation
end;

procedure TEntityManager<T>.Delete(AModel: T);
begin
  // TODO: Implement DELETE by model operation
end;

procedure TEntityManager<T>.DeleteBatch(AModels: TArray<T>);
begin
  // TODO: Implement batch DELETE by models operation
end;

procedure TEntityManager<T>.DeleteBatch(AIDs: TArray<Int64>);
begin
  // TODO: Implement batch DELETE by IDs operation
end;

procedure TEntityManager<T>.DeleteBatch(AFilter: TFilterCriteria);
begin
  // TODO: Implement batch DELETE by filter operation
end;

function TEntityManager<T>.Clone(ASource: T): T;
begin
  Result := nil;
  // TODO: Implement clone operation
end;

end.
