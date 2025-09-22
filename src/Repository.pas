unit Repository;

interface

uses
  SysUtils, StrUtils, System.Classes, Generics.Collections, System.TypInfo,
  Rtti, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, System.Variants,
  Entity, EntityAttributes, FilterCriterion;

type
  IRepository<T> = interface
    ['{808825C5-94CA-4B8F-BCEA-D351F4F6813E}']
  end;

  TRepository<T: TEntity> = class(TInterfacedObject, IRepository<T>)
  private
    FConnection: TFDConnection;

    function GenerateSelectSql(AClass: TClass; const AWhereClause: string = ''): string;
    function GetSelectColumns(AClass: TClass): string;
    procedure FillEntityFromDataSet(AEntity: TObject; ADataSet: TFDQuery);
    procedure FillNestedEntities(AEntity: TObject);
    function GetPrimaryKeyColumn(AClass: TClass): string;
    function GetColumnName(AProp: TRttiProperty): string;
    function ShouldUseInSelect(AProp: TRttiProperty): Boolean;
    function CreateEntityInstanceByClass(AClass: TClass): TObject;
    function ExtractGenericTypeFromList(AListType: TRttiType): TClass;
    procedure ProcessHasManyInserts(AModel: T; AParentId: Int64);
    procedure InsertNestedEntity(AEntity: TObject; AEntityClass: TClass);
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

constructor TRepository<T>.Create(AConnection: TFDConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection Required');

  FConnection := AConnection;
end;

function TRepository<T>.Connection: TFDConnection;
begin
  Result := FConnection;
end;

function TRepository<T>.GetTableName(AClass: TClass): string;
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

procedure TRepository<T>.InsertNestedEntity(AEntity: TObject; AEntityClass: TClass);
var
  query: TFDQuery;
  insertSql: string;
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  columns, values: TStringList;
  columnName: string;
  propValue: TValue;
  insertedId: Int64;
  pkColumn: string;
  pkProp: TRttiProperty;
begin
  if not Assigned(AEntity) then
    Exit;

  query := TFDQuery.Create(nil);
  columns := TStringList.Create;
  values := TStringList.Create;
  try
    query.Connection := FConnection;

    ctx := TRttiContext.Create;
    try
      rType := ctx.GetType(AEntityClass);

      // INSERT için kolonları hazırla
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        colAttr := nil;
        for attr in prop.GetAttributes do
        begin
          if attr is NotMapped then
            Break;
          if attr is HasOne then
            Break;
          if attr is HasMany then
            Break;
          if attr is BelongsTo then
            Break;
          if attr is Column then
          begin
            colAttr := attr as Column;
            Break;
          end;
        end;

        if not Assigned(colAttr) or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucAdd in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);
        propValue := prop.GetValue(AEntity);

        if propValue.IsEmpty and colAttr.IsNotNull then
          Continue;

        columns.Add(columnName);
        values.Add(':' + columnName);
      end;

      if columns.Count = 0 then
        Exit;

      // INSERT SQL'i oluştur
      insertSql := Format('INSERT INTO %s (%s) VALUES (%s)', [
        GetFullTableName(AEntityClass),
        columns.CommaText.Replace('"', ''),
        values.CommaText.Replace('"', '')
      ]);

      // PostgreSQL için RETURNING ekle
      pkColumn := GetPrimaryKeyColumn(AEntityClass);
      insertSql := insertSql + ' RETURNING ' + pkColumn;

      query.SQL.Text := insertSql;

      // Parametreleri set et
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        colAttr := nil;
        for attr in prop.GetAttributes do
        begin
          if attr is Column then
          begin
            colAttr := attr as Column;
            Break;
          end;
        end;

        if not Assigned(colAttr) or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucAdd in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);
        if query.ParamByName(columnName) <> nil then
        begin
          propValue := prop.GetValue(AEntity);
          if not propValue.IsEmpty then
            query.ParamByName(columnName).Value := propValue.AsVariant
          else
            query.ParamByName(columnName).Value := Null;
        end;
      end;

      // INSERT işlemini gerçekleştir
      query.Open;

      if not query.IsEmpty then
      begin
        insertedId := query.Fields[0].AsLargeInt;

        // Nested nesnenin ID'sini güncelle
        pkProp := nil;
        for prop in rType.GetProperties do
        begin
          if not prop.IsWritable then
            Continue;

          for attr in prop.GetAttributes do
          begin
            if attr is Column then
            begin
              colAttr := attr as Column;
              if colAttr.IsPrimaryKey then
              begin
                pkProp := prop;
                Break;
              end;
            end;
          end;
          if Assigned(pkProp) then
            Break;
        end;

        if Assigned(pkProp) then
          pkProp.SetValue(AEntity, TValue.From<Int64>(insertedId));
      end;

    finally
      ctx.Free;
    end;
  finally
    query.Free;
    columns.Free;
    values.Free;
  end;
end;

procedure TRepository<T>.ProcessHasManyInserts(AModel: T; AParentId: Int64);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  hasManyAttr: HasMany;
  nestedList: TObject;
  nestedEntityClass: TClass;
  listType: TRttiType;
  countProp: TRttiProperty;
  getItemMethod: TRttiMethod;
  count, i: Integer;
  nestedEntity: TObject;
  filterProp: TRttiProperty;
  propValue: TValue;
  method: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(T);

    for prop in rType.GetProperties do
    begin
      for attr in prop.GetAttributes do
      begin
        if attr is HasMany then
        begin
          hasManyAttr := attr as HasMany;

          // Nested list'i al - TObject(AModel) cast kullan
          propValue := prop.GetValue(TObject(AModel));
          nestedList := propValue.AsObject;
          if not Assigned(nestedList) then
            Continue;

          // Generic type'ı çıkar
          nestedEntityClass := ExtractGenericTypeFromList(prop.PropertyType);
          if not Assigned(nestedEntityClass) then
            Continue;

          // List'teki item sayısını al - Count property'si kullan
          listType := ctx.GetType(nestedList.ClassType);
          countProp := listType.GetProperty('Count');
          if not Assigned(countProp) then
            Continue;

          count := countProp.GetValue(nestedList).AsInteger;
          if count = 0 then
            Continue;

          // GetItem metodunu bul
          getItemMethod := nil;
          for method in listType.GetMethods do
          begin
            if (method.Name = 'GetItem') and (Length(method.GetParameters) = 1) then
            begin
              getItemMethod := method;
              Break;
            end;
          end;

          if not Assigned(getItemMethod) then
            Continue;

          // Her nested entity için INSERT işlemi yap
          for i := 0 to count - 1 do
          begin
            nestedEntity := getItemMethod.Invoke(nestedList, [i]).AsObject;
            if not Assigned(nestedEntity) then
              Continue;

            // Parent ID'yi set et
            filterProp := ctx.GetType(nestedEntityClass).GetProperty(hasManyAttr.ValuePropertyName);
            if Assigned(filterProp) and filterProp.IsWritable then
              filterProp.SetValue(nestedEntity, TValue.From<Int64>(AParentId));

            // Nested entity için repository oluştur ve insert et
            InsertNestedEntity(nestedEntity, nestedEntityClass);
          end;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

function TRepository<T>.GetFullTableName(AClass: TClass): string;
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

function TRepository<T>.GetPrimaryKeyColumn(AClass: TClass): string;
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

function TRepository<T>.GetColumnName(AProp: TRttiProperty): string;
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

function TRepository<T>.ShouldUseInSelect(AProp: TRttiProperty): Boolean;
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

function TRepository<T>.GetSelectColumns(AClass: TClass): string;
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

function TRepository<T>.GenerateSelectSql(AClass: TClass; const AWhereClause: string): string;
var
  tableName, columns: string;
begin
  tableName := GetFullTableName(AClass);
  columns := GetSelectColumns(AClass);

  Result := Format('SELECT %s FROM %s', [columns, tableName]);

  if AWhereClause <> '' then
    Result := Result + ' WHERE ' + AWhereClause;
end;

procedure TRepository<T>.FillEntityFromDataSet(AEntity: TObject; ADataSet: TFDQuery);
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

procedure TRepository<T>.FillNestedEntities(AEntity: TObject);
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

function TRepository<T>.CreateEntityInstanceByClass(AClass: TClass): TObject;
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

function TRepository<T>.ExtractGenericTypeFromList(AListType: TRttiType): TClass;
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

function TRepository<T>.FindById(AId: TValue; ALock: Boolean = False): T;
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

function TRepository<T>.FindOne(AFilter: TFilterCriteria; ALock: Boolean): T;
begin
  Result := nil;
  // TODO: Implement based on filter criteria
end;

function TRepository<T>.Find(AFilter: TFilterCriteria; ALock: Boolean): TObjectList<T>;
begin
  Result := nil;
  // TODO: Implement based on filter criteria
end;

procedure TRepository<T>.Add(AModel: T);
var
  query: TFDQuery;
  insertSql: string;
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  columns, values: TStringList;
  columnName: string;
  propValue: TValue;
  insertedId: Int64;
  pkColumn: string;
begin
  if not Assigned(AModel) then
    Exit;

  query := TFDQuery.Create(nil);
  columns := TStringList.Create;
  values := TStringList.Create;
  try
    query.Connection := FConnection;

    ctx := TRttiContext.Create;
    try
      rType := ctx.GetType(T);

      // Ana tablo için INSERT SQL'i oluştur
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        // Column attribute'unu kontrol et
        colAttr := nil;
        for attr in prop.GetAttributes do
        begin
          if attr is NotMapped then
            Break;
          if attr is HasOne then
            Break;
          if attr is HasMany then
            Break;
          if attr is BelongsTo then
            Break;
          if attr is Column then
          begin
            colAttr := attr as Column;
            Break;
          end;
        end;

        // Column attribute yoksa veya mapped değilse atla
        if not Assigned(colAttr) then
          Continue;

        // Auto increment alanları INSERT'e dahil etme
        if colAttr.IsAutoIncrement then
          Continue;

        // cucAdd kullanım kriterini kontrol et
        if (colAttr.SqlUseWhichCols <> []) and not (cucAdd in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);
        propValue := prop.GetValue(TObject(AModel));

        // Boş değerleri kontrol et (nullable olmayan alanlar için)
        if propValue.IsEmpty and colAttr.IsNotNull then
          Continue;

        columns.Add(columnName);
        values.Add(':' + columnName);
      end;

      if columns.Count = 0 then
        raise Exception.Create('No columns to insert');

      // INSERT SQL'i oluştur
      insertSql := Format('INSERT INTO %s (%s) VALUES (%s)', [
        GetFullTableName(T),
        columns.CommaText.Replace('"', ''),
        values.CommaText.Replace('"', '')
      ]);

      // PostgreSQL için RETURNING ekle
      pkColumn := GetPrimaryKeyColumn(T);
      insertSql := insertSql + ' RETURNING ' + pkColumn;

      query.SQL.Text := insertSql;

      // Parametreleri set et
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        colAttr := nil;
        for attr in prop.GetAttributes do
        begin
          if attr is Column then
          begin
            colAttr := attr as Column;
            Break;
          end;
        end;

        if not Assigned(colAttr) or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucAdd in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);
        if query.ParamByName(columnName) <> nil then
        begin
          propValue := prop.GetValue(TObject(AModel));
          if not propValue.IsEmpty then
            query.ParamByName(columnName).Value := propValue.AsVariant
          else
            query.ParamByName(columnName).Value := Null;
        end;
      end;

      // INSERT işlemini gerçekleştir
      query.Open;

      if not query.IsEmpty then
      begin
        insertedId := query.Fields[0].AsLargeInt;

        // Ana nesnenin ID'sini güncelle
        for prop in rType.GetProperties do
        begin
          if not prop.IsWritable then
            Continue;

          for attr in prop.GetAttributes do
          begin
            if attr is Column then
            begin
              colAttr := attr as Column;
              if colAttr.IsPrimaryKey then
              begin
                prop.SetValue(TObject(AModel), TValue.From<Int64>(insertedId));
                Break;
              end;
            end;
          end;
        end;

        query.Close;

        // Nested objeleri işle (HasMany relationships)
        ProcessHasManyInserts(AModel, insertedId);
      end;

    finally
      ctx.Free;
    end;
  finally
    query.Free;
    columns.Free;
    values.Free;
  end;
end;

procedure TRepository<T>.AddBatch(AModels: TArray<T>);
begin
  // TODO: Implement batch INSERT operation
end;

procedure TRepository<T>.Update(AModel: T);
begin
  // TODO: Implement UPDATE operation
end;

procedure TRepository<T>.UpdateBatch(AModels: TArray<T>);
begin
  // TODO: Implement batch UPDATE operation
end;

procedure TRepository<T>.Delete(AID: Int64);
var
  Q: TFDQuery;
  SQL, PKCol: string;
begin
  PKCol := GetPrimaryKeyColumn(TClass(T));
  SQL := Format('DELETE FROM %s WHERE %s = :%s', [GetTableName(TClass(T)), PKCol, PKCol]);

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := SQL;
    Q.ParamByName(PKCol).AsLargeInt := AID;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TRepository<T>.Delete(AModel: T);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  PKCol: string;
  PKVal: TValue;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(TClass(T));
    PKCol := GetPrimaryKeyColumn(TClass(T));

    for prop in rType.GetProperties do
    begin
      if (prop.Visibility = mvPublished)
      or (prop.Visibility = mvPublic)
      then
      begin
        if SameText(GetColumnName(prop), PKCol) then
        begin
          PKVal := prop.GetValue(TObject(AModel));
          Delete(PKVal.AsInt64);
          Exit;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

procedure TRepository<T>.DeleteBatch(AModels: TArray<T>);
var
  Model: T;
begin
  for Model in AModels do
    Delete(Model);
end;

procedure TRepository<T>.DeleteBatch(AIDs: TArray<Int64>);
var
  ID: Int64;
begin
  for ID in AIDs do
    Delete(ID);
end;

procedure TRepository<T>.DeleteBatch(AFilter: TFilterCriteria);
var
  Q: TFDQuery;
  SQL: string;
  FC: TFilterCriterion;
  i: Integer;
begin
  SQL := 'DELETE FROM ' + GetTableName(TClass(T)) + ' WHERE ';
  for i := 0 to AFilter.Count - 1 do
  begin
    FC := AFilter[i];
    if i > 0 then
      SQL := SQL + ' AND ';
    SQL := SQL + FC.PropertyNamePath + ' ' + FC.Operator + ' :' + FC.PropertyNamePath;
  end;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := SQL;
    for i := 0 to AFilter.Count - 1 do
    begin
      Q.ParamByName(AFilter[i].PropertyNamePath).Value := AFilter[i].Value.AsVariant;
    end;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

function TRepository<T>.Clone(ASource: T): T;
begin
  Result := nil;
  // TODO: Implement clone operation
end;

end.
