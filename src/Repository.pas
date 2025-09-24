unit Repository;

interface

uses
  SysUtils, StrUtils, System.Classes, Generics.Collections, System.TypInfo,
  Rtti, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, System.Variants,
  Entity, EntityAttributes, FilterCriterion;

type
  TIncludeOption = (
    ioIncludeNone,          // Load only main entity
    ioIncludeChildren,      // Load child entities (HasMany relations)
    ioIncludeParent,        // Load parent entities (BelongsTo relations)
    ioIncludeGrandChildren, // Load children of children (2 levels deep)
    ioIncludeAll,           // Load all nested entities
    ioIncludeSpecific       // Load specific relations (use with relation names)
  );
  TIncludeOptions = set of TIncludeOption;

  TRelationNames = TArray<string>;


  TCascadeOperation = (coInsert, coUpdate, coDelete);
  TCascadeOperations = set of TCascadeOperation;

  IRepository<T: TEntity> = interface
    ['{808825C5-94CA-4B8F-BCEA-D351F4F6813E}']
    function FindById(AId: TValue; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): T;
    function FindOne(AFilter: TFilterCriteria; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): T;
    function Find(AFilter: TFilterCriteria; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): TObjectList<T>;

    procedure Add(AModel: T; ACascade: TCascadeOperations = []); overload;
    procedure AddBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []); overload;

    procedure Update(AModel: T; ACascade: TCascadeOperations = []);
    procedure UpdateBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []); overload;

    procedure Delete(AID: Int64; ACascade: TCascadeOperations = []); overload;
    procedure Delete(AModel: T; ACascade: TCascadeOperations = []); overload;
    procedure DeleteBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []); overload;
    procedure DeleteBatch(AIDs: TArray<Int64>; ACascade: TCascadeOperations = []); overload;
    procedure DeleteBatch(AFilter: TFilterCriteria; ACascade: TCascadeOperations = []); overload;

    function Clone(ASource: T): T;
  end;

  TRepository<T: TEntity> = class(TInterfacedObject, IRepository<T>)
  private
    FConnection: TFDConnection;

    function StringListToArray(AStringList: TStringList): TArray<string>;
    function GenerateSelectSql(AClass: TClass; const AWhereClause: string = ''): string;
    function GetSelectColumns(AClass: TClass): string;
    function GetColumnNameForProperty(const APropertyName: string): string;
    function GetPrimaryKeyColumn(AClass: TClass): string;
    function GetColumnName(AProp: TRttiProperty): string;
    function ShouldUseInSelect(AProp: TRttiProperty): Boolean;
    function CreateEntityInstanceByClass(AClass: TClass): TObject;
    function ExtractGenericTypeFromList(AListType: TRttiType): TClass;
    procedure FillEntityFromDataSet(AEntity: TObject; ADataSet: TFDQuery);
    procedure FillNestedEntitiesWithInclude(AEntity: TObject; AInclude: TIncludeOptions; ARelations: TRelationNames = nil);
    procedure LoadChildEntitiesWithInclude(AEntity: TObject; AInclude: TIncludeOptions; ARelations: TRelationNames);
    procedure LoadParentEntitiesWithInclude(AEntity: TObject; AInclude: TIncludeOptions; ARelations: TRelationNames);
    procedure LoadChildProperty(AEntity: TObject; AProp: TRttiProperty; AHasManyAttr: HasManyAttribute);
    procedure LoadParentProperty(AEntity: TObject; AProp: TRttiProperty; ABelongsToAttr: BelongsToAttribute);
    procedure LoadGrandChildEntitiesRecursive(AEntity: TObject);
    procedure LoadSpecificRelationsOnly(AEntity: TObject; ARelations: TRelationNames);
    function ShouldLoadThisRelation(const ARelationName: string; ARelations: TRelationNames; AInclude: TIncludeOptions): Boolean;
    function IsChildRelationProperty(AProp: TRttiProperty): Boolean;
    function CreateListInstance(AListType: TRttiType): TObject;
    function CreateSafeParameterName(const APropertyName: string; AIndex: Integer): string;
    procedure AddToList(AList: TObject; AItem: TObject);
    function GetListCount(AList: TObject): Integer;
    function GetListItem(AList: TObject; AIndex: Integer): TObject;
    procedure ProcessHasManyInserts(AModel: T; AParentId: Int64);
    procedure InsertNestedEntity(AEntity: TObject; AEntityClass: TClass);
    procedure ProcessHasManyUpdates(AModel: T; AParentId: Int64);
    procedure UpdateNestedEntity(AEntity: TObject; AEntityClass: TClass);
    procedure ProcessCascadeDeletes(AModel: T; ACascade: TCascadeOperations);
    procedure CloneEntityProperties(ASource, ATarget: TObject; AEntityClass: TClass; ADeepClone: Boolean);
  protected
    function Connection: TFDConnection;
    function GetTableName(AClass: TClass): string;
    function GetFullTableName(AClass: TClass): string;
  public
    function FindById(AId: TValue; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): T;
    function FindOne(AFilter: TFilterCriteria; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): T;
    function Find(AFilter: TFilterCriteria; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): TObjectList<T>;

    procedure Add(AModel: T; ACascade: TCascadeOperations = []); overload;
    procedure AddBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []); overload;

    procedure Update(AModel: T; ACascade: TCascadeOperations = []);
    procedure UpdateBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []); overload;

    procedure Delete(AID: Int64; ACascade: TCascadeOperations = []); overload;
    procedure Delete(AModel: T; ACascade: TCascadeOperations = []); overload;
    procedure DeleteBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []); reintroduce; overload;
    procedure DeleteBatch(AIDs: TArray<Int64>; ACascade: TCascadeOperations = []); reintroduce; overload;
    procedure DeleteBatch(AFilter: TFilterCriteria; ACascade: TCascadeOperations = []); reintroduce; overload;

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
          if attr is HasOneAttribute then
            Break;
          if attr is HasManyAttribute then
            Break;
          if attr is BelongsToAttribute then
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
  hasManyAttr: HasManyAttribute;
  nestedList: TObject;
  nestedEntityClass: TClass;
  listType: TRttiType;
  count, i: Integer;
  nestedEntity: TObject;
  foreignKeyProp: TRttiProperty; // Ismi 'filterProp' yerine 'foreignKeyProp' olarak değiştirildi
  propValue: TValue;
  method: TRttiMethod;
  getItemMethod: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(T);

    for prop in rType.GetProperties do
    begin
      hasManyAttr := nil;
      for attr in prop.GetAttributes do
      begin
        if attr is HasManyAttribute then
        begin
          hasManyAttr := attr as HasManyAttribute;
          Break;
        end;
      end;

      if not Assigned(hasManyAttr) then
        Continue;

      propValue := prop.GetValue(TObject(AModel));
      nestedList := propValue.AsObject;
      if not Assigned(nestedList) then
        Continue;

      nestedEntityClass := ExtractGenericTypeFromList(prop.PropertyType);
      if not Assigned(nestedEntityClass) then
        Continue;

      count := GetListCount(nestedList);
      if count = 0 then
        Continue;

      // Her nested entity için INSERT işlemi yap
      for i := 0 to count - 1 do
      begin
        nestedEntity := GetListItem(nestedList, i);
        if not Assigned(nestedEntity) then
          Continue;

        // DÜZELTME: Child entity'de parent'ın ID'sini tutan foreign key property'sini bul ve set et.
        foreignKeyProp := ctx.GetType(nestedEntityClass).GetProperty(hasManyAttr.ForeignKeyProperty);
        if Assigned(foreignKeyProp) and foreignKeyProp.IsWritable then
        begin
          foreignKeyProp.SetValue(nestedEntity, TValue.From<Int64>(AParentId));
        end
        else
        begin
            // Hata veya uyarı loglanabilir: ForeignKeyProperty bulunamadı.
            Continue;
        end;

        // Nested entity için insert et (cascade olmadan)
        InsertNestedEntity(nestedEntity, nestedEntityClass);
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

  if not Assigned(AClass) then
    Exit;

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(AClass);
    if not Assigned(rType) then
      Exit;

    for prop in rType.GetProperties do
    begin
      if not prop.IsReadable then
        Continue;

      try
        for attr in prop.GetAttributes do
        begin
          if attr is Column then
          begin
            colAttr := Column(attr);
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
      except
        // Skip problematic attributes but continue processing
        Continue;
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

    if attr is HasOneAttribute then
      Exit(False);

    if attr is HasManyAttribute then
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

function TRepository<T>.StringListToArray(AStringList: TStringList): TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, AStringList.Count);
  for i := 0 to AStringList.Count - 1 do
    Result[i] := AStringList[i];
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
        // FIX: Type-safe field value assignment, AsVariant kullanma
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
              if prop.PropertyType.Handle = TypeInfo(TDateTime) then
                propValue := TValue.From<TDateTime>(field.AsDateTime)
              else if prop.PropertyType.Handle = TypeInfo(Double) then
                propValue := TValue.From<Double>(field.AsFloat)
              else if prop.PropertyType.Handle = TypeInfo(Single) then
                propValue := TValue.From<Single>(field.AsSingle)
              else
                propValue := TValue.From<Extended>(field.AsFloat);
            end;
          tkString, tkLString, tkWString, tkUString:
            propValue := TValue.From<string>(field.AsString);
          tkEnumeration:
            begin
              if prop.PropertyType.Handle = TypeInfo(Boolean) then
                propValue := TValue.From<Boolean>(field.AsBoolean);
            end;
        end;

        try
          prop.SetValue(AEntity, propValue);
        except
          on E: Exception do
            // Log error but continue with other properties
            Continue;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

procedure TRepository<T>.FillNestedEntitiesWithInclude(AEntity: TObject; AInclude: TIncludeOptions; ARelations: TRelationNames = nil);
begin
  // Exit if no includes requested
  if (AInclude = [ioIncludeNone]) or (AEntity = nil) then
    Exit;

  // Load all relations
  if ioIncludeAll in AInclude then
  begin
    LoadChildEntitiesWithInclude(AEntity, [ioIncludeAll], nil);
    LoadParentEntitiesWithInclude(AEntity, [ioIncludeAll], nil);
    if ioIncludeGrandChildren in AInclude then
      LoadGrandChildEntitiesRecursive(AEntity);
    Exit;
  end;

  // Load specific relation types
  if ioIncludeChildren in AInclude then
    LoadChildEntitiesWithInclude(AEntity, AInclude, ARelations);

  if ioIncludeParent in AInclude then
    LoadParentEntitiesWithInclude(AEntity, AInclude, ARelations);

  if ioIncludeGrandChildren in AInclude then
    LoadGrandChildEntitiesRecursive(AEntity);

  if ioIncludeSpecific in AInclude then
    LoadSpecificRelationsOnly(AEntity, ARelations);
end;

procedure TRepository<T>.LoadChildEntitiesWithInclude(AEntity: TObject; AInclude: TIncludeOptions; ARelations: TRelationNames);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LRelationName: string;
  LHasManyAttr: HasManyAttribute;
  LAttr: TCustomAttribute;
  LFound: Boolean;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AEntity.ClassInfo);

    for LProp in LType.GetProperties do
    begin
      LFound := False;
      LHasManyAttr := nil;

      // Check if this property has HasMany attribute
      for LAttr in LProp.GetAttributes do
      begin
        if LAttr is HasManyAttribute then
        begin
          LHasManyAttr := HasManyAttribute(LAttr);
          LFound := True;
          Break;
        end;
      end;

      if LFound then
      begin
        LRelationName := LProp.Name;

        // Check if we should load this relation
        if ShouldLoadThisRelation(LRelationName, ARelations, AInclude) then
        begin
          LoadChildProperty(AEntity, LProp, LHasManyAttr);
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TRepository<T>.LoadParentEntitiesWithInclude(AEntity: TObject; AInclude: TIncludeOptions; ARelations: TRelationNames);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LRelationName: string;
  LBelongsToAttr: BelongsToAttribute;
  LAttr: TCustomAttribute;
  LFound: Boolean;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AEntity.ClassInfo);

    for LProp in LType.GetProperties do
    begin
      LFound := False;
      LBelongsToAttr := nil;

      // Check if this property has BelongsTo attribute
      for LAttr in LProp.GetAttributes do
      begin
        if LAttr is BelongsToAttribute then
        begin
          LBelongsToAttr := BelongsToAttribute(LAttr);
          LFound := True;
          Break;
        end;
      end;

      if LFound then
      begin
        LRelationName := LProp.Name;

        // Check if we should load this relation
        if ShouldLoadThisRelation(LRelationName, ARelations, AInclude) then
        begin
          LoadParentProperty(AEntity, LProp, LBelongsToAttr);
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TRepository<T>.LoadChildProperty(AEntity: TObject; AProp: TRttiProperty; AHasManyAttr: HasManyAttribute);
var
  LQuery: TFDQuery;
  LChildClass: TClass;
  LChildEntity: TObject;
  LListValue: TValue;
  LList: TObject;
  LForeignKey: string;
  LParentId: TValue;
  LParentIdProp: TRttiProperty;
  LContext: TRttiContext;
  LParentType: TRttiType;
  LSql: string;
  LWhereClause: string;
begin
  LQuery := TFDQuery.Create(nil);
  LContext := TRttiContext.Create;
  try
    LQuery.Connection := Connection;

    // Get child class from generic list type
    LChildClass := ExtractGenericTypeFromList(AProp.PropertyType);
    if LChildClass = nil then
      Exit;

    // Get parent ID
    LParentType := LContext.GetType(AEntity.ClassInfo);
    LParentIdProp := LParentType.GetProperty('Id'); // Assuming 'Id' is primary key
    if LParentIdProp = nil then
      Exit;

    LParentId := LParentIdProp.GetValue(AEntity);
    if LParentId.IsEmpty then
      Exit;

    // Get foreign key column name from attribute or convention
    if AHasManyAttr.ForeignKeyProperty <> '' then
      LForeignKey := AHasManyAttr.ForeignKeyProperty
    else
      LForeignKey := Format('%s_id', [GetTableName(AEntity.ClassInfo).ToLower]); // Convention: parent_table_id

    // Build WHERE clause
    LWhereClause := Format('%s = %s', [LForeignKey, QuotedStr(LParentId.ToString)]);

    // Generate and execute SQL for child entities
    LSql := GenerateSelectSql(LChildClass, LWhereClause);
    LQuery.SQL.Text := LSql;
    LQuery.Open;

    // Create list instance
    LList := CreateListInstance(AProp.PropertyType);
    if LList = nil then
      Exit;

    // Fill child entities
    LQuery.First;
    while not LQuery.Eof do
    begin
      LChildEntity := CreateEntityInstanceByClass(LChildClass);
      FillEntityFromDataSet(LChildEntity, LQuery);

      // Add to list
      AddToList(LList, LChildEntity);

      LQuery.Next;
    end;

    // Set the list property
    LListValue := TValue.From<TObject>(LList);
    AProp.SetValue(AEntity, LListValue);

  finally
    LContext.Free;
    LQuery.Free;
  end;
end;

procedure TRepository<T>.LoadParentProperty(AEntity: TObject; AProp: TRttiProperty; ABelongsToAttr: BelongsToAttribute);
var
  LQuery: TFDQuery;
  LParentClass: TClass;
  LParentEntity: TObject;
  LForeignKey: string;
  LForeignIdValue: TValue;
  LForeignIdProp: TRttiProperty;
  LContext: TRttiContext;
  LEntityType: TRttiType;
  LSql: string;
  LWhereClause: string;
  LParentValue: TValue;
begin
  LQuery := TFDQuery.Create(nil);
  LContext := TRttiContext.Create;
  try
    LQuery.Connection := Connection;

    // Get parent class
    LParentClass := AProp.PropertyType.AsInstance.MetaclassType;
    if LParentClass = nil then
      Exit;

    // Get foreign key column name from attribute or convention
    if ABelongsToAttr.RemoteKeyProperty <> '' then
      LForeignKey := ABelongsToAttr.RemoteKeyProperty
    else
      LForeignKey := Format('%s_id', [AProp.Name.ToLower]); // Convention: property_name_id

    // Get foreign key value from entity
    LEntityType := LContext.GetType(AEntity.ClassInfo);
    LForeignIdProp := LEntityType.GetProperty(LForeignKey.Replace('_id', 'Id')); // Convert snake_case to PascalCase
    if LForeignIdProp = nil then
      Exit;

    LForeignIdValue := LForeignIdProp.GetValue(AEntity);
    if LForeignIdValue.IsEmpty then
      Exit;

    // Build WHERE clause for parent entity
    LWhereClause := Format('%s = %s', [GetPrimaryKeyColumn(LParentClass), QuotedStr(LForeignIdValue.ToString)]);

    // Generate and execute SQL for parent entity
    LSql := GenerateSelectSql(LParentClass, LWhereClause);
    LQuery.SQL.Text := LSql;
    LQuery.Open;

    if not LQuery.IsEmpty then
    begin
      LParentEntity := CreateEntityInstanceByClass(LParentClass);
      FillEntityFromDataSet(LParentEntity, LQuery);

      // Set the parent property
      LParentValue := TValue.From<TObject>(LParentEntity);
      AProp.SetValue(AEntity, LParentValue);
    end;

  finally
    LContext.Free;
    LQuery.Free;
  end;
end;

procedure TRepository<T>.LoadGrandChildEntitiesRecursive(AEntity: TObject);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LList: TObject;
  LListValue: TValue;
  LChildEntity: TObject;
  LIndex: Integer;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AEntity.ClassInfo);

    for LProp in LType.GetProperties do
    begin
      if IsChildRelationProperty(LProp) then
      begin
        LListValue := LProp.GetValue(AEntity);
        if not LListValue.IsEmpty then
        begin
          LList := LListValue.AsObject;
          if LList <> nil then
          begin
            // Iterate through child entities and load their children
            for LIndex := 0 to GetListCount(LList) - 1 do
            begin
              LChildEntity := GetListItem(LList, LIndex);
              if LChildEntity <> nil then
              begin
                // Recursively load children of children
                FillNestedEntitiesWithInclude(LChildEntity, [ioIncludeChildren], nil);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TRepository<T>.LoadSpecificRelationsOnly(AEntity: TObject; ARelations: TRelationNames);
begin
  if ARelations = nil then
    Exit;

  // Load both child and parent relations that match the specified names
  LoadChildEntitiesWithInclude(AEntity, [ioIncludeSpecific], ARelations);
  LoadParentEntitiesWithInclude(AEntity, [ioIncludeSpecific], ARelations);
end;

function TRepository<T>.ShouldLoadThisRelation(const ARelationName: string; ARelations: TRelationNames; AInclude: TIncludeOptions): Boolean;
var
  LRelation: string;
begin
  // If ioIncludeAll is set, load everything
  if ioIncludeAll in AInclude then
  begin
    Result := True;
    Exit;
  end;

  // If ioIncludeSpecific is set, only load specified relations
  if ioIncludeSpecific in AInclude then
  begin
    Result := False;
    if ARelations <> nil then
    begin
      for LRelation in ARelations do
      begin
        if SameText(LRelation, ARelationName) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
    Exit;
  end;

  // For other include options, load all relations of that type
  Result := True;
end;

function TRepository<T>.IsChildRelationProperty(AProp: TRttiProperty): Boolean;
var
  LAttr: TCustomAttribute;
begin
  Result := False;
  for LAttr in AProp.GetAttributes do
  begin
    if LAttr is HasManyAttribute then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TRepository<T>.CreateListInstance(AListType: TRttiType): TObject;
var
  LContext: TRttiContext;
  LListClass: TClass;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    if AListType.AsInstance <> nil then
    begin
      LListClass := AListType.AsInstance.MetaclassType;
      Result := LListClass.Create;
    end;
  finally
    LContext.Free;
  end;
end;

function TRepository<T>.CreateSafeParameterName(const APropertyName: string; AIndex: Integer): string;
begin
  Result := Format('p_%s_%d', [APropertyName.Replace('.', '_').Replace('-', '_'), AIndex]);
end;

procedure TRepository<T>.AddToList(AList: TObject; AItem: TObject);
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AList.ClassInfo);
    LMethod := LType.GetMethod('Add');
    if LMethod <> nil then
      LMethod.Invoke(AList, [AItem]);
  finally
    LContext.Free;
  end;
end;

function TRepository<T>.GetListCount(AList: TObject): Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
begin
  Result := 0;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AList.ClassInfo);
    LProp := LType.GetProperty('Count');
    if LProp <> nil then
      Result := LProp.GetValue(AList).AsInteger;
  finally
    LContext.Free;
  end;
end;

function TRepository<T>.GetListItem(AList: TObject; AIndex: Integer): TObject;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LIndexedProp: TRttiIndexedProperty;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AList.ClassInfo);

    // First try to find GetItem method
    LMethod := LType.GetMethod('GetItem');
    if LMethod <> nil then
    begin
      Result := LMethod.Invoke(AList, [AIndex]).AsObject;
    end
    else
    begin
      // Alternative: Try indexed property
      for LIndexedProp in LType.GetIndexedProperties do
      begin
        if LIndexedProp.Name = 'Items' then
        begin
          Result := LIndexedProp.GetValue(AList, [AIndex]).AsObject;
          Break;
        end;
      end;
    end;
  finally
    LContext.Free;
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

procedure TRepository<T>.ProcessHasManyUpdates(AModel: T; AParentId: Int64);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop, prop2: TRttiProperty;
  attr: TCustomAttribute;
  hasManyAttr: HasManyAttribute;
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
  nestedEntityId: TValue;
  nestedEntityIdProp: TRttiProperty;
begin
  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(T);

    for prop in rType.GetProperties do
    begin
      hasManyAttr := nil;

      // HasMany attribute'ını bul
      for attr in prop.GetAttributes do
      begin
        if attr is HasManyAttribute then
        begin
          hasManyAttr := attr as HasManyAttribute;
          Break;
        end;
      end;

      if not Assigned(hasManyAttr) then
        Continue;

      // Nested list'i al
      propValue := prop.GetValue(TObject(AModel));
      nestedList := propValue.AsObject;
      if not Assigned(nestedList) then
        Continue;

      // Generic type'ı çıkar
      nestedEntityClass := ExtractGenericTypeFromList(prop.PropertyType);
      if not Assigned(nestedEntityClass) then
        Continue;

      // List'teki item sayısını al
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

      // Her nested entity için UPDATE/INSERT işlemi yap
      for i := 0 to count - 1 do
      begin
        nestedEntity := getItemMethod.Invoke(nestedList, [i]).AsObject;
        if not Assigned(nestedEntity) then
          Continue;

        // Parent ID'yi set et
        filterProp := ctx.GetType(nestedEntityClass).GetProperty(hasManyAttr.LocalKeyProperty);
        if Assigned(filterProp) and filterProp.IsWritable then
          filterProp.SetValue(nestedEntity, TValue.From<Int64>(AParentId));

        // Nested entity'nin ID'sini kontrol et
        nestedEntityIdProp := nil;
        for prop2 in ctx.GetType(nestedEntityClass).GetProperties do
        begin
          for attr in prop2.GetAttributes do
          begin
            if attr is Column then
            begin
              if (attr as Column).IsPrimaryKey then
              begin
                nestedEntityIdProp := prop2;
                Break;
              end;
            end;
          end;
          if Assigned(nestedEntityIdProp) then
            Break;
        end;

        if Assigned(nestedEntityIdProp) then
        begin
          nestedEntityId := nestedEntityIdProp.GetValue(nestedEntity);

          // ID varsa UPDATE, yoksa INSERT yap
          if nestedEntityId.IsEmpty or (nestedEntityId.AsInt64 <= 0) then
          begin
            // INSERT işlemi
            InsertNestedEntity(nestedEntity, nestedEntityClass);
          end
          else
          begin
            // UPDATE işlemi
            UpdateNestedEntity(nestedEntity, nestedEntityClass);
          end;
        end;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

procedure TRepository<T>.UpdateNestedEntity(AEntity: TObject; AEntityClass: TClass);
var
  query: TFDQuery;
  updateSql: string;
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  setParts: TStringList;
  whereClause: string;
  columnName: string;
  propValue: TValue;
  pkColumn: string;
  pkValue: TValue;
begin
  if not Assigned(AEntity) then
    Exit;

  query := TFDQuery.Create(nil);
  setParts := TStringList.Create;
  try
    query.Connection := FConnection;

    ctx := TRttiContext.Create;
    try
      rType := ctx.GetType(AEntityClass);

      // Primary Key değerini bul
      pkColumn := GetPrimaryKeyColumn(AEntityClass);
      pkValue := TValue.Empty;

      for prop in rType.GetProperties do
      begin
        for attr in prop.GetAttributes do
        begin
          if attr is Column then
          begin
            colAttr := attr as Column;
            if colAttr.IsPrimaryKey then
            begin
              pkValue := prop.GetValue(AEntity);
              Break;
            end;
          end;
        end;
        if not pkValue.IsEmpty then
          Break;
      end;

      if pkValue.IsEmpty or (pkValue.AsInt64 <= 0) then
        Exit; // ID yoksa update yapma

      // UPDATE için SET clause'unu oluştur
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        colAttr := nil;
        for attr in prop.GetAttributes do
        begin
          if attr is NotMapped then
            Break;
          if attr is HasOneAttribute then
            Break;
          if attr is HasManyAttribute then
            Break;
          if attr is BelongsToAttribute then
            Break;
          if attr is Column then
          begin
            colAttr := attr as Column;
            Break;
          end;
        end;

        if not Assigned(colAttr) then
          Continue;

        if colAttr.IsPrimaryKey or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucUpdate in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);
        setParts.Add(columnName + ' = :' + columnName);
      end;

      if setParts.Count = 0 then
        Exit;

      // UPDATE SQL'i oluştur
      whereClause := pkColumn + ' = :' + pkColumn;
      updateSql := Format('UPDATE %s SET %s WHERE %s', [
        GetFullTableName(AEntityClass),
        setParts.CommaText.Replace('"', ''),
        whereClause
      ]);

      query.SQL.Text := updateSql;
      query.ParamByName(pkColumn).Value := pkValue.AsVariant;

      // SET parametrelerini set et
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

        if not Assigned(colAttr) or colAttr.IsPrimaryKey or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucUpdate in colAttr.SqlUseWhichCols) then
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

      // UPDATE işlemini gerçekleştir
      query.ExecSQL;

    finally
      ctx.Free;
    end;
  finally
    query.Free;
    setParts.Free;
  end;
end;

procedure TRepository<T>.CloneEntityProperties(ASource, ATarget: TObject; AEntityClass: TClass; ADeepClone: Boolean);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  propValue: TValue;
begin
  if not Assigned(ASource) or not Assigned(ATarget) then
    Exit;

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(AEntityClass);

    for prop in rType.GetProperties do
    begin
      if not prop.IsReadable or not prop.IsWritable then
        Continue;

      // Sadece Column property'lerini kopyala (nested relationship'leri atla - circular reference önlemi)
      colAttr := nil;
      for attr in prop.GetAttributes do
      begin
        if attr is Column then
        begin
          colAttr := attr as Column;
          Break;
        end
        else if attr is NotMapped then
        begin
          Break; // NotMapped property'leri atla
        end
        else if (attr is HasOneAttribute) or (attr is HasManyAttribute) or (attr is BelongsToAttribute) then
        begin
          if not ADeepClone then
            Break; // Deep clone değilse relationship'leri atla
        end;
      end;

      if not Assigned(colAttr) then
        Continue;

      propValue := prop.GetValue(ASource);

      // Primary key auto increment ise ve 0'dan büyükse, klonda 0 yap
      if colAttr.IsPrimaryKey and colAttr.IsAutoIncrement and not propValue.IsEmpty then
      begin
        case prop.PropertyType.TypeKind of
          tkInteger: prop.SetValue(ATarget, TValue.From<Integer>(0));
          tkInt64: prop.SetValue(ATarget, TValue.From<Int64>(0));
        end;
      end
      else
      begin
        // Diğer property'leri direkt kopyala
        if not propValue.IsEmpty then
          prop.SetValue(ATarget, propValue);
      end;
    end;

  finally
    ctx.Free;
  end;

end;

procedure TRepository<T>.ProcessCascadeDeletes(AModel: T; ACascade: TCascadeOperations);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  hasManyAttr: HasManyAttribute;
  nestedList: TObject;
  nestedEntityClass: TClass;
  listType: TRttiType;
  countProp: TRttiProperty;
  getItemMethod: TRttiMethod;
  count, i: Integer;
  nestedEntity: TObject;
  method: TRttiMethod;
  deleteQuery: TFDQuery;
  deleteSql: string;
  filterProp: TRttiProperty;
  filterValue: TValue;
  valueColumnName: string;
  valueProp: TRttiProperty;
begin
  if not (coDelete in ACascade) then
    Exit;

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(T);

    for prop in rType.GetProperties do
    begin
      hasManyAttr := nil;

      // HasMany attribute'ını bul
      for attr in prop.GetAttributes do
      begin
        if attr is HasManyAttribute then
        begin
          hasManyAttr := attr as HasManyAttribute;
          Break;
        end;
      end;

      if not Assigned(hasManyAttr) then
        Continue;

      // Parent ID'yi al
      filterProp := rType.GetProperty(hasManyAttr.LocalKeyProperty);
      if not Assigned(filterProp) then
        Continue;

      filterValue := filterProp.GetValue(TObject(AModel));
      if filterValue.IsEmpty then
        Continue;

      // Generic type'ı çıkar
      nestedEntityClass := ExtractGenericTypeFromList(prop.PropertyType);
      if not Assigned(nestedEntityClass) then
        Continue;

      // Child tablosundaki foreign key column adını al
      valueColumnName := hasManyAttr.ForeignKeyProperty;
      valueProp := ctx.GetType(nestedEntityClass).GetProperty(hasManyAttr.ForeignKeyProperty);
      if Assigned(valueProp) then
        valueColumnName := GetColumnName(valueProp);

      // Child kayıtları sil
      deleteQuery := TFDQuery.Create(nil);
      try
        deleteQuery.Connection := FConnection;
        deleteSql := Format('DELETE FROM %s WHERE %s = :parent_id', [
          GetFullTableName(nestedEntityClass),
          valueColumnName
        ]);
        deleteQuery.SQL.Text := deleteSql;
        deleteQuery.ParamByName('parent_id').Value := filterValue.AsVariant;
        deleteQuery.ExecSQL;
      finally
        deleteQuery.Free;
      end;
    end;
  finally
    ctx.Free;
  end;
end;

function TRepository<T>.GetColumnNameForProperty(const APropertyName: string): string;
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
begin
  Result := LowerCase(APropertyName); // Default olarak lowercase

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(T);
    prop := rType.GetProperty(APropertyName);

    if Assigned(prop) then
      Result := GetColumnName(prop);
  finally
    ctx.Free;
  end;
end;

function TRepository<T>.FindById(AId: TValue; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): T;
var
  LQuery: TFDQuery;
  LWhereClause: string;
  LLockClause: string;
  LSql: string;
  LPrimaryKeyColumn: string;
begin
  Result := nil;
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := Connection;

    // WHERE koşulunu parametreli olarak oluştur
    LPrimaryKeyColumn := GetPrimaryKeyColumn(T);
    LWhereClause := Format('%s = :pk_id', [LPrimaryKeyColumn]);

    // Gerekirse FOR UPDATE kilidini ekle (PostgreSQL syntax)
    LLockClause := '';
    if ALock then
      LLockClause := ' FOR UPDATE';

    // SQL'i oluştur ve parametreyi ata
    LSql := GenerateSelectSql(T, LWhereClause) + LLockClause;
    LQuery.SQL.Text := LSql;
    LQuery.ParamByName('pk_id').Value := AId.AsVariant;
    LQuery.Open;

    if not LQuery.IsEmpty then
    begin
      // FIX: Proper instantiation using RTTI
      Result := T(CreateEntityInstanceByClass(T));
      FillEntityFromDataSet(Result, LQuery);

      // İlişkili verileri yükle
      if (AInclude <> [ioIncludeNone]) or (ARelations <> nil) then
        FillNestedEntitiesWithInclude(Result, AInclude, ARelations);
    end;
  finally
    LQuery.Free;
  end;
end;

function TRepository<T>.FindOne(AFilter: TFilterCriteria; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): T;
var
  LQuery: TFDQuery;
  LWhereClause: TStringBuilder;
  LSql: string;
  LLockClause: string;
  i: Integer;
  LFilter: TFilterCriterion;
  LColumnName: string;
  LParamName: string;
begin
  Result := nil;

  if not Assigned(AFilter) or (AFilter.Count = 0) then
    raise Exception.Create('FindOne requires at least one filter criterion.');

  LQuery := TFDQuery.Create(nil);
  LWhereClause := TStringBuilder.Create;
  try
    LQuery.Connection := Connection;

    // WHERE koşulunu dinamik ve parametreli olarak oluştur
    for i := 0 to AFilter.Count - 1 do
    begin
      LFilter := AFilter[i];
      if i > 0 then
        LWhereClause.Append(' AND ');

      LColumnName := GetColumnNameForProperty(LFilter.PropertyNamePath);
      // FIX: Improved parameter naming to avoid conflicts
      LParamName := Format('p_%s_%d', [LFilter.PropertyNamePath.Replace('.', '_'), i]);

      LWhereClause.Append(LColumnName).Append(' ').Append(LFilter.Operator).Append(' :').Append(LParamName);

      // FIX: Use Params.Add instead of CreateParam for better compatibility
      LQuery.Params.Add(LParamName, LFilter.Value.AsVariant);
    end;

    // Gerekirse FOR UPDATE kilidini ekle
    LLockClause := '';
    if ALock then
      LLockClause := ' FOR UPDATE';

    // FIX: Use database-specific LIMIT syntax - PostgreSQL uses LIMIT, SQL Server uses TOP
    LSql := GenerateSelectSql(T, LWhereClause.ToString) + LLockClause + ' LIMIT 1';
    LQuery.SQL.Text := LSql;

    LQuery.Open;

    if not LQuery.IsEmpty then
    begin
      // FIX: Proper instantiation using RTTI
      Result := T(CreateEntityInstanceByClass(T));
      FillEntityFromDataSet(Result, LQuery);

      // İlişkili verileri yükle
      if (AInclude <> [ioIncludeNone]) or (ARelations <> nil) then
        FillNestedEntitiesWithInclude(Result, AInclude, ARelations);
    end;
  finally
    LQuery.Free;
    LWhereClause.Free;
  end;
end;

function TRepository<T>.Find(AFilter: TFilterCriteria; ALock: Boolean = False; AInclude: TIncludeOptions = [ioIncludeNone]; ARelations: TRelationNames = nil): TObjectList<T>;
var
  LQuery: TFDQuery;
  LWhereClause: TStringBuilder;
  LSql: string;
  LLockClause: string;
  i: Integer;
  LFilter: TFilterCriterion;
  LColumnName: string;
  LParamName: string;
  LEntity: T;
begin
  Result := TObjectList<T>.Create(True);
  LQuery := TFDQuery.Create(nil);
  LWhereClause := TStringBuilder.Create;
  try
    LQuery.Connection := Connection;

    // WHERE koşulunu dinamik ve parametreli olarak oluştur
    if Assigned(AFilter) and (AFilter.Count > 0) then
    begin
      for i := 0 to AFilter.Count - 1 do
      begin
        LFilter := AFilter[i];
        if i > 0 then
          LWhereClause.Append(' AND ');

        LColumnName := GetColumnNameForProperty(LFilter.PropertyNamePath);
        // FIX: Improved parameter naming
        LParamName := Format('p_%s_%d', [LFilter.PropertyNamePath.Replace('.', '_'), i]);

        LWhereClause.Append(LColumnName).Append(' ').Append(LFilter.Operator).Append(' :').Append(LParamName);

        // FIX: Use Params.Add instead of CreateParam
        LQuery.Params.Add(LParamName, LFilter.Value.AsVariant);
      end;
    end;

    // Gerekirse FOR UPDATE kilidini ekle
    LLockClause := '';
    if ALock then
      LLockClause := ' FOR UPDATE';

    // FIX: No unnecessary ORDER BY or LIMIT for Find method
    LSql := GenerateSelectSql(T, LWhereClause.ToString) + LLockClause;
    LQuery.SQL.Text := LSql;
    LQuery.Open;

    // Sonuçları TObjectList'e doldur
    while not LQuery.Eof do
    begin
      // FIX: Proper instantiation using RTTI
      LEntity := T(CreateEntityInstanceByClass(T));
      FillEntityFromDataSet(LEntity, LQuery);

      // Her bir entity için ilişkili verileri yükle
      if (AInclude <> [ioIncludeNone]) or (ARelations <> nil) then
        FillNestedEntitiesWithInclude(LEntity, AInclude, ARelations);

      Result.Add(LEntity);
      LQuery.Next;
    end;
  finally
    LQuery.Free;
    LWhereClause.Free;
  end;
end;

procedure TRepository<T>.Add(AModel: T; ACascade: TCascadeOperations = []);
var
  query: TFDQuery;
  insertSql: string;
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  createdAtAttr: CreatedAt;
  updatedAtAttr: UpdatedAt;
  createdByAttr: CreatedBy;
  updatedByAttr: UpdatedBy;
  columns, values: TStringList;
  columnName: string;
  propValue: TValue;
  insertedId: Int64;
  pkColumn: string;
  pkProp: TRttiProperty;
  isAttributeProcessed: Boolean;
begin
  if not Assigned(AModel) then
    raise Exception.Create('Model cannot be nil');

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  query := TFDQuery.Create(nil);
  columns := TStringList.Create;
  values := TStringList.Create;
  try
    query.Connection := FConnection;

    ctx := TRttiContext.Create;
    try
      rType := ctx.GetType(T);

      // FIX: Otomatik alanları (CreatedAt, CreatedBy vs.) Add işlemi öncesi doldur
      for prop in rType.GetProperties do
      begin
        if not prop.IsWritable then
          Continue;

        isAttributeProcessed := False;
        for attr in prop.GetAttributes do
        begin
          if attr is CreatedAt then
          begin
            createdAtAttr := attr as CreatedAt;
            if createdAtAttr.AutoUpdate then
            begin
              propValue := prop.GetValue(TObject(AModel));
              // Sadece boşsa veya default DateTime değeriyse ata
              if propValue.IsEmpty or (propValue.AsType<TDateTime> <= 0) then
                prop.SetValue(TObject(AModel), TValue.From<TDateTime>(Now));
            end;
            isAttributeProcessed := True;
            Break;
          end
          else if attr is UpdatedAt then
          begin
            // FIX: UpdatedAt'ı Add işleminde de set et
            updatedAtAttr := attr as UpdatedAt;
            if updatedAtAttr.AutoUpdate then
            begin
              prop.SetValue(TObject(AModel), TValue.From<TDateTime>(Now));
            end;
            isAttributeProcessed := True;
            Break;
          end
          else if attr is CreatedBy then
          begin
            createdByAttr := attr as CreatedBy;
            propValue := prop.GetValue(TObject(AModel));
            // Sadece boşsa ata - UserIdProvider implementasyonu gerekli
            if propValue.IsEmpty then
            begin
              // TODO: Implement user ID provider mechanism
              // Example: prop.SetValue(TObject(AModel), TValue.From<Int64>(GetCurrentUserId));
            end;
            isAttributeProcessed := True;
            Break;
          end
          else if attr is UpdatedBy then
          begin
            // FIX: UpdatedBy'ı Add işleminde de set et
            updatedByAttr := attr as UpdatedBy;
            propValue := prop.GetValue(TObject(AModel));
            if propValue.IsEmpty then
            begin
              // TODO: Implement user ID provider mechanism
              // Example: prop.SetValue(TObject(AModel), TValue.From<Int64>(GetCurrentUserId));
            end;
            isAttributeProcessed := True;
            Break;
          end;
        end;
      end;

      // Ana tablo için INSERT SQL'i oluştur
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        colAttr := nil;
        isAttributeProcessed := False;

        for attr in prop.GetAttributes do
        begin
          if (attr is NotMapped) or (attr is HasOneAttribute) or
             (attr is HasManyAttribute) or (attr is BelongsToAttribute) then
          begin
            isAttributeProcessed := True; // İlişkisel property'leri atla
            Break;
          end;
          if attr is Column then
          begin
            colAttr := attr as Column;
          end;
        end;

        // FIX: Eğer relationship attribute'u varsa atla
        if isAttributeProcessed then
          Continue;

        if not Assigned(colAttr) or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucAdd in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);
        propValue := prop.GetValue(TObject(AModel));

        // FIX: NotNull kontrolünü daha iyi yap
        if propValue.IsEmpty then
        begin
          if colAttr.IsNotNull then
            raise Exception.CreateFmt('Required field %s cannot be null', [prop.Name])
          else
            Continue; // NULL değerler için parametre ekleme
        end;

        columns.Add(columnName);
        values.Add(':' + columnName);
      end;

      if columns.Count = 0 then
        raise Exception.Create('No columns to insert for entity ' + T.ClassName);

      // FIX: CommaText property'si tırnak işareti ekleyebilir, manuel join kullan
      insertSql := Format('INSERT INTO %s (%s) VALUES (%s)', [
        GetFullTableName(T),
        string.Join(',', columns.ToStringArray),
        string.Join(',', values.ToStringArray)
      ]);

      // FIX: Database-specific RETURNING clause
      pkColumn := GetPrimaryKeyColumn(T);
      if pkColumn <> '' then
      begin
        // PostgreSQL syntax - diğer veritabanları için uyarlanabilir
        insertSql := insertSql + ' RETURNING ' + pkColumn;
      end;

      query.SQL.Text := insertSql;

      // FIX: Parametreleri daha güvenli şekilde set et
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
        if query.Params.FindParam(columnName) <> nil then
        begin
          propValue := prop.GetValue(TObject(AModel));
          if not propValue.IsEmpty then
          begin
            // FIX: Type-specific parameter assignment
            try
              query.ParamByName(columnName).Value := propValue.AsVariant;
            except
              on E: Exception do
                raise Exception.CreateFmt('Error setting parameter %s: %s', [columnName, E.Message]);
            end;
          end
          else
            query.ParamByName(columnName).Value := Null;
        end;
      end;

      // FIX: Execute query with proper error handling
      try
        query.Open;
      except
        on E: Exception do
          raise Exception.CreateFmt('Failed to insert entity %s: %s', [T.ClassName, E.Message]);
      end;

      // FIX: Primary key handling
      if (pkColumn <> '') and (not query.IsEmpty) then
      begin
        insertedId := query.Fields[0].AsLargeInt;

        // Ana nesnenin ID'sini güncelle
        pkProp := nil;
        for prop in rType.GetProperties do
        begin
          if not prop.IsWritable then
            Continue;

          colAttr := nil;
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
        begin
          try
            pkProp.SetValue(TObject(AModel), TValue.From<Int64>(insertedId));
          except
            on E: Exception do
              raise Exception.CreateFmt('Failed to set primary key value: %s', [E.Message]);
          end;
        end;

        // Nested objeleri işle (HasMany relationships)
        if coInsert in ACascade then
        begin
          try
            ProcessHasManyInserts(AModel, insertedId);
          except
            on E: Exception do
              raise Exception.CreateFmt('Failed to process cascade inserts: %s', [E.Message]);
          end;
        end;
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

procedure TRepository<T>.AddBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []);
var
  i: Integer;
  transaction: TFDTransaction;
  wasInTransaction: Boolean;
begin
  if Length(AModels) = 0 then
    Exit;

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  // FIX: Null model kontrolü
  for i := 0 to High(AModels) do
  begin
    if not Assigned(AModels[i]) then
      raise Exception.CreateFmt('Model at index %d cannot be nil', [i]);
  end;

  // FIX: Transaction durumunu kontrol et
  wasInTransaction := FConnection.InTransaction;

  if wasInTransaction then
  begin
    // Zaten bir transaction içindeyse, tekrar başlatma
    try
      for i := 0 to High(AModels) do
        Add(AModels[i], ACascade);
    except
      on E: Exception do
        raise Exception.CreateFmt('Batch insert failed at index %d: %s', [i, E.Message]);
    end;
  end
  else
  begin
    // Yeni bir transaction başlat
    transaction := TFDTransaction.Create(nil);
    try
      transaction.Connection := FConnection;

      try
        transaction.StartTransaction;

        try
          for i := 0 to High(AModels) do
            Add(AModels[i], ACascade);

          transaction.Commit;
        except
          on E: Exception do
          begin
            if transaction.Active then
              transaction.Rollback;
            raise Exception.CreateFmt('Batch insert failed at index %d: %s', [i, E.Message]);
          end;
        end;
      except
        on E: Exception do
          raise Exception.CreateFmt('Transaction error during batch insert: %s', [E.Message]);
      end;
    finally
      transaction.Free;
    end;
  end;
end;

procedure TRepository<T>.Update(AModel: T; ACascade: TCascadeOperations = []);
var
  query: TFDQuery;
  updateSql: string;
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  updatedAtAttr: UpdatedAt;
  updatedByAttr: UpdatedBy;
  versionAttr: Version;
  versionProp: TRttiProperty;
  setParts: TStringList;
  whereClause: string;
  columnName: string;
  propValue: TValue;
  pkColumn: string;
  pkValue: TValue;
  pkProp: TRttiProperty;
  versionValue: TValue;
  isAttributeProcessed: Boolean;
begin
  if not Assigned(AModel) then
    raise Exception.Create('Model cannot be nil');

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  query := TFDQuery.Create(nil);
  setParts := TStringList.Create;
  versionProp := nil;
  pkProp := nil;
  try
    query.Connection := FConnection;
    ctx := TRttiContext.Create;
    try
      rType := ctx.GetType(T);

      // FIX: Primary Key property'sini önce bul
      pkColumn := GetPrimaryKeyColumn(T);
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
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

      if not Assigned(pkProp) then
        raise Exception.Create('Primary key property not found for entity ' + T.ClassName);

      pkValue := pkProp.GetValue(TObject(AModel));
      if pkValue.IsEmpty or (pkValue.AsInt64 <= 0) then
        raise Exception.Create('Primary key value is required for update operation');

      // Otomatik alanları (UpdatedAt, UpdatedBy vs.) Update işlemi öncesi doldur
      for prop in rType.GetProperties do
      begin
        if not prop.IsWritable then
          Continue;

        isAttributeProcessed := False;
        for attr in prop.GetAttributes do
        begin
          if attr is UpdatedAt then
          begin
            updatedAtAttr := attr as UpdatedAt;
            if updatedAtAttr.AutoUpdate then
              prop.SetValue(TObject(AModel), TValue.From<TDateTime>(Now));
            isAttributeProcessed := True;
            Break;
          end
          else if attr is UpdatedBy then
          begin
            updatedByAttr := attr as UpdatedBy;
            propValue := prop.GetValue(TObject(AModel));
            // TODO: Implement user provider mechanism
            // if propValue.IsEmpty then
            //   prop.SetValue(TObject(AModel), TValue.From<Int64>(GetCurrentUserId));
            isAttributeProcessed := True;
            Break;
          end
          else if attr is Version then
          begin
            versionProp := prop; // Versiyon property'sini sakla
            isAttributeProcessed := True;
            Break;
          end;
        end;
      end;

      // UPDATE için SET clause'unu oluştur
      for prop in rType.GetProperties do
      begin
        if not prop.IsReadable then
          Continue;

        colAttr := nil;
        isAttributeProcessed := False;

        for attr in prop.GetAttributes do
        begin
          if (attr is NotMapped) or (attr is HasManyAttribute) or
             (attr is BelongsToAttribute) or (attr is HasOneAttribute) then
          begin
            isAttributeProcessed := True;
            Break;
          end;
          if attr is Column then
            colAttr := attr as Column;
        end;

        // FIX: Relationship attribute'ları atla
        if isAttributeProcessed then
          Continue;

        if not Assigned(colAttr) or colAttr.IsPrimaryKey or colAttr.IsAutoIncrement then
          Continue;

        if (colAttr.SqlUseWhichCols <> []) and not (cucUpdate in colAttr.SqlUseWhichCols) then
          Continue;

        columnName := GetColumnName(prop);

        // FIX: Versiyon kolonuysa, SET ifadesi özel olacak ve current value'yu sakla
        if prop = versionProp then
        begin
          versionValue := prop.GetValue(TObject(AModel));
          setParts.Add(Format('%s = %s + 1', [columnName, columnName]));
        end
        else
        begin
          setParts.Add(columnName + ' = :' + columnName);
        end;
      end;

      if setParts.Count = 0 then
        Exit; // Güncellenecek alan yoksa çık

      // FIX: WHERE koşulunu daha güvenli oluştur
      whereClause := pkColumn + ' = :pk_value';

      // FIX: Version-based optimistic locking için current version değerini kontrol et
      if Assigned(versionProp) then
      begin
        if versionValue.IsEmpty then
          raise Exception.Create('Version value is required for optimistic locking');
        columnName := GetColumnName(versionProp);
        whereClause := whereClause + ' AND ' + columnName + ' = :version_value';
      end;

      // FIX: String.Join kullan, CommaText problemini çöz
      updateSql := Format('UPDATE %s SET %s WHERE %s', [
        GetFullTableName(T),
        string.Join(',', StringListToArray(setParts)),
        whereClause
      ]);

      query.SQL.Text := updateSql;

      // FIX: WHERE parametrelerini güvenli şekilde ata
      try
        query.ParamByName('pk_value').Value := pkValue.AsVariant;

        if Assigned(versionProp) then
          query.ParamByName('version_value').Value := versionValue.AsVariant;

        // SET parametrelerini ata (version hariç)
        for prop in rType.GetProperties do
        begin
          if not prop.IsReadable then
            Continue;

          colAttr := nil;
          isAttributeProcessed := False;

          for attr in prop.GetAttributes do
          begin
            if (attr is NotMapped) or (attr is HasManyAttribute) or
               (attr is BelongsToAttribute) or (attr is HasOneAttribute) then
            begin
              isAttributeProcessed := True;
              Break;
            end;
            if attr is Column then
              colAttr := attr as Column;
          end;

          if isAttributeProcessed then
            Continue;

          if not Assigned(colAttr) or colAttr.IsPrimaryKey or
             colAttr.IsAutoIncrement or (prop = versionProp) then
            Continue;

          if (colAttr.SqlUseWhichCols <> []) and not (cucUpdate in colAttr.SqlUseWhichCols) then
            Continue;

          columnName := GetColumnName(prop);
          if query.Params.FindParam(columnName) <> nil then
          begin
            propValue := prop.GetValue(TObject(AModel));
            if not propValue.IsEmpty then
              query.ParamByName(columnName).Value := propValue.AsVariant
            else
              query.ParamByName(columnName).Value := Null;
          end;
        end;

      except
        on E: Exception do
          raise Exception.CreateFmt('Error setting update parameters: %s', [E.Message]);
      end;

      // FIX: UPDATE işlemini gerçekleştir ve hata kontrolü yap
      try
        query.ExecSQL;

        // FIX: Optimistic locking kontrolü
        if query.RowsAffected < 1 then
        begin
          if Assigned(versionProp) then
            raise Exception.Create('Concurrency error: The record was modified by another user or does not exist')
          else
            raise Exception.Create('Update failed: Record not found or no changes detected');
        end;

        // FIX: Version değerini model'de güncelle
        if Assigned(versionProp) and versionProp.IsWritable then
        begin
          try
            versionProp.SetValue(TObject(AModel), TValue.From<Integer>(versionValue.AsInteger + 1));
          except
            // Version update hatası kritik değil, log'lanabilir
          end;
        end;

      except
        on E: Exception do
          raise Exception.CreateFmt('Failed to update entity %s: %s', [T.ClassName, E.Message]);
      end;

      // Nested objeleri işle
      if coUpdate in ACascade then
      begin
        try
          ProcessHasManyUpdates(AModel, pkValue.AsInt64);
        except
          on E: Exception do
            raise Exception.CreateFmt('Failed to process cascade updates: %s', [E.Message]);
        end;
      end;

    finally
      ctx.Free;
    end;
  finally
    query.Free;
    setParts.Free;
  end;
end;

procedure TRepository<T>.UpdateBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []);
var
  i: Integer;
  transaction: TFDTransaction;
  wasInTransaction: Boolean;
begin
  if Length(AModels) = 0 then
    Exit;

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  // FIX: Null model kontrolü
  for i := 0 to High(AModels) do
  begin
    if not Assigned(AModels[i]) then
      raise Exception.CreateFmt('Model at index %d cannot be nil', [i]);
  end;

  wasInTransaction := FConnection.InTransaction;

  if wasInTransaction then
  begin
    // Zaten bir transaction içindeyse
    try
      for i := 0 to High(AModels) do
        Update(AModels[i], ACascade);
    except
      on E: Exception do
        raise Exception.CreateFmt('Batch update failed at index %d: %s', [i, E.Message]);
    end;
  end
  else
  begin
    // Yeni transaction başlat
    transaction := TFDTransaction.Create(nil);
    try
      transaction.Connection := FConnection;

      try
        transaction.StartTransaction;

        try
          for i := 0 to High(AModels) do
            Update(AModels[i], ACascade);

          transaction.Commit;
        except
          on E: Exception do
          begin
            if transaction.Active then
              transaction.Rollback;
            raise Exception.CreateFmt('Batch update failed at index %d: %s', [i, E.Message]);
          end;
        end;
      except
        on E: Exception do
          raise Exception.CreateFmt('Transaction error during batch update: %s', [E.Message]);
      end;
    finally
      transaction.Free;
    end;
  end;
end;

procedure TRepository<T>.Delete(AID: Int64; ACascade: TCascadeOperations = []);
var
  model: T;
  query: TFDQuery;
  sql, pkColumn: string;
  ctx: TRttiContext;
  rType: TRttiType;
  attr: TCustomAttribute;
  softDeleteAttr: SoftDelete;
begin
  if AID <= 0 then
    raise Exception.Create('Invalid ID: ID must be greater than 0');

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  // FIX: SoftDelete attribute'unu kontrol et
  ctx := TRttiContext.Create;
  softDeleteAttr := nil;
  try
    rType := ctx.GetType(T);
    for attr in rType.GetAttributes do
    begin
      if attr is SoftDelete then
      begin
        softDeleteAttr := attr as SoftDelete;
        Break;
      end;
    end;
  finally
    ctx.Free;
  end;

  // FIX: Cascade delete işlemi gerekiyorsa önce modeli yükle
  model := nil;
  if coDelete in ACascade then
  begin
    try
      model := FindById(AID);
      if Assigned(model) then
      begin
        try
          ProcessCascadeDeletes(model, ACascade);
        except
          on E: Exception do
          begin
            if Assigned(model) then
              model.Free;
            raise Exception.CreateFmt('Failed to process cascade deletes: %s', [E.Message]);
          end;
        end;
      end
      else
        raise Exception.CreateFmt('Record with ID %d not found for cascade delete', [AID]);
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to load model for cascade delete: %s', [E.Message]);
    end;
  end;

  query := TFDQuery.Create(nil);
  try
    query.Connection := FConnection;
    pkColumn := GetPrimaryKeyColumn(T);

    if Assigned(softDeleteAttr) then
    begin
      // SOFT DELETE LOGIC
      sql := Format('UPDATE %s SET %s = :deleted_at', [
        GetFullTableName(T),
        softDeleteAttr.DeletedAtColumn
      ]);

      // FIX: DeletedBy column kontrolü ve user ID set etme
      if softDeleteAttr.DeletedByColumn <> '' then
        sql := sql + Format(', %s = :deleted_by', [softDeleteAttr.DeletedByColumn]);

      sql := sql + Format(' WHERE %s = :pk_id', [pkColumn]);

      // FIX: Soft delete için already deleted kontrolü ekle
      sql := sql + Format(' AND %s IS NULL', [softDeleteAttr.DeletedAtColumn]);

      query.SQL.Text := sql;

      try
        query.ParamByName('deleted_at').AsDateTime := Now;
        if softDeleteAttr.DeletedByColumn <> '' then
        begin
          // TODO: Implement user provider mechanism
          query.ParamByName('deleted_by').AsLargeInt := 0; // GetCurrentUserId()
        end;
        query.ParamByName('pk_id').AsLargeInt := AID;
      except
        on E: Exception do
          raise Exception.CreateFmt('Error setting soft delete parameters: %s', [E.Message]);
      end;
    end
    else
    begin
      // HARD DELETE LOGIC
      sql := Format('DELETE FROM %s WHERE %s = :pk_id', [
        GetFullTableName(T),
        pkColumn
      ]);
      query.SQL.Text := sql;

      try
        query.ParamByName('pk_id').AsLargeInt := AID;
      except
        on E: Exception do
          raise Exception.CreateFmt('Error setting delete parameters: %s', [E.Message]);
      end;
    end;

    // FIX: Execute with error handling and row count check
    try
      query.ExecSQL;

      if query.RowsAffected < 1 then
      begin
        if Assigned(softDeleteAttr) then
          raise Exception.CreateFmt('Soft delete failed: Record with ID %d not found or already deleted', [AID])
        else
          raise Exception.CreateFmt('Delete failed: Record with ID %d not found', [AID]);
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to delete record with ID %d: %s', [AID, E.Message]);
    end;

  finally
    query.Free;
    if Assigned(model) then
      model.Free;
  end;
end;

procedure TRepository<T>.Delete(AModel: T; ACascade: TCascadeOperations = []);
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  idValue: TValue;
  pkFound: Boolean;
begin
  if not Assigned(AModel) then
    raise Exception.Create('Model cannot be nil');

  ctx := TRttiContext.Create;
  pkFound := False;
  try
    rType := ctx.GetType(T);
    for prop in rType.GetProperties do
    begin
      if not prop.IsReadable then
        Continue;

      for attr in prop.GetAttributes do
      begin
        if attr is Column then
        begin
          colAttr := attr as Column;
          if colAttr.IsPrimaryKey then
          begin
            idValue := prop.GetValue(TObject(AModel));
            if not idValue.IsEmpty and (idValue.AsType<Int64> > 0) then
            begin
              // FIX: ID ile silme metodunu çağır
              Delete(idValue.AsType<Int64>, ACascade);
              pkFound := True;
            end
            else
            begin
              raise Exception.Create('Primary key value is invalid or not set');
            end;
            Break;
          end;
        end;
      end;
      if pkFound then
        Break;
    end;

    if not pkFound then
      raise Exception.Create('Primary key property not found in entity ' + T.ClassName);

  finally
    ctx.Free;
  end;
end;

procedure TRepository<T>.DeleteBatch(AModels: TArray<T>; ACascade: TCascadeOperations = []);
var
  i: Integer;
  transaction: TFDTransaction;
  wasInTransaction: Boolean;
begin
  if Length(AModels) = 0 then
    Exit;

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  // FIX: Null model kontrolü
  for i := 0 to High(AModels) do
  begin
    if not Assigned(AModels[i]) then
      raise Exception.CreateFmt('Model at index %d cannot be nil', [i]);
  end;

  wasInTransaction := FConnection.InTransaction;

  if wasInTransaction then
  begin
    // Zaten transaction içindeyse
    try
      for i := 0 to High(AModels) do
        Delete(AModels[i], ACascade);
    except
      on E: Exception do
        raise Exception.CreateFmt('Batch delete failed at index %d: %s', [i, E.Message]);
    end;
  end
  else
  begin
    // Yeni transaction başlat
    transaction := TFDTransaction.Create(nil);
    try
      transaction.Connection := FConnection;

      try
        transaction.StartTransaction;

        try
          for i := 0 to High(AModels) do
            Delete(AModels[i], ACascade);

          transaction.Commit;
        except
          on E: Exception do
          begin
            if transaction.Active then
              transaction.Rollback;
            raise Exception.CreateFmt('Batch delete failed at index %d: %s', [i, E.Message]);
          end;
        end;
      except
        on E: Exception do
          raise Exception.CreateFmt('Transaction error during batch delete: %s', [E.Message]);
      end;
    finally
      transaction.Free;
    end;
  end;
end;

procedure TRepository<T>.DeleteBatch(AIDs: TArray<Int64>; ACascade: TCascadeOperations = []);
var
  i: Integer;
  transaction: TFDTransaction;
  query: TFDQuery;
  sql, pkColumn: string;
  ctx: TRttiContext;
  rType: TRttiType;
  attr: TCustomAttribute;
  softDeleteAttr: SoftDelete;
  idList: string;
  wasInTransaction: Boolean;
begin
  if Length(AIDs) = 0 then
    Exit;

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  // FIX: Invalid ID kontrolü
  for i := 0 to High(AIDs) do
  begin
    if AIDs[i] <= 0 then
      raise Exception.CreateFmt('Invalid ID at index %d: ID must be greater than 0', [i]);
  end;

  // Cascade delete gerekiyorsa tek tek işlem yap
  if coDelete in ACascade then
  begin
    wasInTransaction := FConnection.InTransaction;

    if wasInTransaction then
    begin
      try
        for i := 0 to High(AIDs) do
          Delete(AIDs[i], ACascade);
      except
        on E: Exception do
          raise Exception.CreateFmt('Cascade delete failed at ID %d (index %d): %s', [AIDs[i], i, E.Message]);
      end;
    end
    else
    begin
      transaction := TFDTransaction.Create(nil);
      try
        transaction.Connection := FConnection;

        try
          transaction.StartTransaction;

          try
            for i := 0 to High(AIDs) do
              Delete(AIDs[i], ACascade);

            transaction.Commit;
          except
            on E: Exception do
            begin
              if transaction.Active then
                transaction.Rollback;
              raise Exception.CreateFmt('Cascade delete failed at ID %d (index %d): %s', [AIDs[i], i, E.Message]);
            end;
          end;
        except
          on E: Exception do
            raise Exception.CreateFmt('Transaction error during cascade delete: %s', [E.Message]);
        end;
      finally
        transaction.Free;
      end;
    end;
    Exit;
  end;

  // BULK DELETE (Non-cascade)
  ctx := TRttiContext.Create;
  softDeleteAttr := nil;
  try
    rType := ctx.GetType(T);
    for attr in rType.GetAttributes do
    begin
      if attr is SoftDelete then
      begin
        softDeleteAttr := attr as SoftDelete;
        Break;
      end;
    end;
  finally
    ctx.Free;
  end;

  // FIX: ID listesini manuel oluştur (FireDAC array macro yerine)
  idList := '';
  for i := 0 to High(AIDs) do
  begin
    if i > 0 then
      idList := idList + ',';
    idList := idList + IntToStr(AIDs[i]);
  end;

  query := TFDQuery.Create(nil);
  try
    query.Connection := FConnection;
    pkColumn := GetPrimaryKeyColumn(T);

    if Assigned(softDeleteAttr) then
    begin
      // BULK SOFT DELETE
      sql := Format('UPDATE %s SET %s = :deleted_at', [
        GetFullTableName(T),
        softDeleteAttr.DeletedAtColumn
      ]);

      if softDeleteAttr.DeletedByColumn <> '' then
        sql := sql + Format(', %s = :deleted_by', [softDeleteAttr.DeletedByColumn]);

      sql := sql + Format(' WHERE %s IN (%s)', [pkColumn, idList]);

      // FIX: Already deleted kontrolü ekle
      sql := sql + Format(' AND %s IS NULL', [softDeleteAttr.DeletedAtColumn]);

      query.SQL.Text := sql;

      try
        query.ParamByName('deleted_at').AsDateTime := Now;
        if softDeleteAttr.DeletedByColumn <> '' then
          query.ParamByName('deleted_by').AsLargeInt := 0; // TODO: GetCurrentUserId()
      except
        on E: Exception do
          raise Exception.CreateFmt('Error setting bulk soft delete parameters: %s', [E.Message]);
      end;
    end
    else
    begin
      // BULK HARD DELETE
      sql := Format('DELETE FROM %s WHERE %s IN (%s)', [
        GetFullTableName(T),
        pkColumn,
        idList
      ]);
      query.SQL.Text := sql;
    end;

    try
      query.ExecSQL;

      // FIX: Row count kontrolü
      if query.RowsAffected < 1 then
      begin
        if Assigned(softDeleteAttr) then
          raise Exception.Create('Bulk soft delete failed: No records found or all records already deleted')
        else
          raise Exception.Create('Bulk delete failed: No records found');
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to execute bulk delete: %s', [E.Message]);
    end;

  finally
    query.Free;
  end;
end;

procedure TRepository<T>.DeleteBatch(AFilter: TFilterCriteria; ACascade: TCascadeOperations = []);
var
  query: TFDQuery;
  sql, whereClause: string;
  fc: TFilterCriterion;
  i: Integer;
  models: TObjectList<T>;
  transaction: TFDTransaction;
  ctx: TRttiContext;
  rType: TRttiType;
  attr: TCustomAttribute;
  softDeleteAttr: SoftDelete;
  wasInTransaction: Boolean;
  paramName: string;
begin
  if not Assigned(AFilter) or (AFilter.Count = 0) then
    raise Exception.Create('Filter criteria cannot be empty');

  if not Assigned(FConnection) then
    raise Exception.Create('Database connection is not available');

  // Cascade delete gerekiyorsa önce kayıtları yükle
  if coDelete in ACascade then
  begin
    models := nil;
    try
      models := Find(AFilter);
      if Assigned(models) and (models.Count > 0) then
      begin
        wasInTransaction := FConnection.InTransaction;

        if wasInTransaction then
        begin
          try
            for i := 0 to models.Count - 1 do
              Delete(models[i], ACascade);
          except
            on E: Exception do
              raise Exception.CreateFmt('Cascade delete failed at index %d: %s', [i, E.Message]);
          end;
        end
        else
        begin
          transaction := TFDTransaction.Create(nil);
          try
            transaction.Connection := FConnection;

            try
              transaction.StartTransaction;

              try
                for i := 0 to models.Count - 1 do
                  Delete(models[i], ACascade);

                transaction.Commit;
              except
                on E: Exception do
                begin
                  if transaction.Active then
                    transaction.Rollback;
                  raise Exception.CreateFmt('Cascade delete failed at index %d: %s', [i, E.Message]);
                end;
              end;
            except
              on E: Exception do
                raise Exception.CreateFmt('Transaction error during cascade delete: %s', [E.Message]);
            end;
          finally
            transaction.Free;
          end;
        end;
      end;
    finally
      if Assigned(models) then
        models.Free;
    end;
    Exit;
  end;

  // FIX: SoftDelete kontrolü
  ctx := TRttiContext.Create;
  softDeleteAttr := nil;
  try
    rType := ctx.GetType(T);
    for attr in rType.GetAttributes do
    begin
      if attr is SoftDelete then
      begin
        softDeleteAttr := attr as SoftDelete;
        Break;
      end;
    end;
  finally
    ctx.Free;
  end;

  // FIX: WHERE clause'unu güvenli şekilde oluştur
  whereClause := '';
  for i := 0 to AFilter.Count - 1 do
  begin
    fc := AFilter[i];
    if i > 0 then
      whereClause := whereClause + ' AND ';

    paramName := Format('filter_param_%d', [i]);
    whereClause := whereClause + GetColumnNameForProperty(fc.PropertyNamePath) +
                  ' ' + fc.Operator + ' :' + paramName;
  end;

  query := TFDQuery.Create(nil);
  try
    query.Connection := FConnection;

    if Assigned(softDeleteAttr) then
    begin
      // FILTER-BASED SOFT DELETE
      sql := Format('UPDATE %s SET %s = :deleted_at', [
        GetFullTableName(T),
        softDeleteAttr.DeletedAtColumn
      ]);

      if softDeleteAttr.DeletedByColumn <> '' then
        sql := sql + Format(', %s = :deleted_by', [softDeleteAttr.DeletedByColumn]);

      sql := sql + ' WHERE ' + whereClause;

      // FIX: Already deleted kontrolü
      sql := sql + Format(' AND %s IS NULL', [softDeleteAttr.DeletedAtColumn]);

      query.SQL.Text := sql;

      try
        query.ParamByName('deleted_at').AsDateTime := Now;
        if softDeleteAttr.DeletedByColumn <> '' then
          query.ParamByName('deleted_by').AsLargeInt := 0; // TODO: GetCurrentUserId()
      except
        on E: Exception do
          raise Exception.CreateFmt('Error setting soft delete parameters: %s', [E.Message]);
      end;
    end
    else
    begin
      // FILTER-BASED HARD DELETE
      sql := 'DELETE FROM ' + GetFullTableName(T) + ' WHERE ' + whereClause;
      query.SQL.Text := sql;
    end;

    // FIX: Filter parametrelerini güvenli şekilde set et
    try
      for i := 0 to AFilter.Count - 1 do
      begin
        paramName := Format('filter_param_%d', [i]);
        query.ParamByName(paramName).Value := AFilter[i].Value.AsVariant;
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Error setting filter parameters: %s', [E.Message]);
    end;

    try
      query.ExecSQL;

      // FIX: Row count kontrolü (opsiyonel - warning verebilir)
      if query.RowsAffected < 1 then
      begin
        // Bu durumda exception atmak yerine warning log'lanabilir
        // çünkü filter ile eşleşen kayıt olmayabilir
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Failed to delete records with filter: %s', [E.Message]);
    end;

  finally
    query.Free;
  end;
end;

function TRepository<T>.Clone(ASource: T): T;
var
  ctx: TRttiContext;
  rType: TRttiType;
  prop: TRttiProperty;
  attr: TCustomAttribute;
  colAttr: Column;
  hasOneAttr: HasOneAttribute;
  hasManyAttr: HasManyAttribute;
  belongsToAttr: BelongsToAttribute;
  propValue: TValue;
  sourceList: TObject;
  targetList: TObject;
  listType: TRttiType;
  countProp: TRttiProperty;
  getItemMethod: TRttiMethod;
  addMethod: TRttiMethod;
  clearMethod: TRttiMethod;
  count, i: Integer;
  sourceItem: TObject;
  clonedItem: TObject;
  sourceNestedEntity: TObject;
  clonedNestedEntity: TObject;
  nestedEntityClass: TClass;
  method: TRttiMethod;
begin
  Result := nil;

  if not Assigned(ASource) then
    Exit;

  ctx := TRttiContext.Create;
  try
    rType := ctx.GetType(T);

    // Yeni instance oluştur
    Result := CreateEntityInstanceByClass(T) as T;

    // Tüm property'leri kopyala
    for prop in rType.GetProperties do
    begin
      if not prop.IsReadable or not prop.IsWritable then
        Continue;

      // Attribute'ları kontrol et
      colAttr := nil;
      hasOneAttr := nil;
      hasManyAttr := nil;
      belongsToAttr := nil;

      for attr in prop.GetAttributes do
      begin
        if attr is NotMapped then
        begin
          // NotMapped property'leri atla
          Break;
        end
        else if attr is Column then
        begin
          colAttr := attr as Column;
        end
        else if attr is HasOneAttribute then
        begin
          hasOneAttr := attr as HasOneAttribute;
        end
        else if attr is HasManyAttribute then
        begin
          hasManyAttr := attr as HasManyAttribute;
        end
        else if attr is BelongsToAttribute then
        begin
          belongsToAttr := attr as BelongsToAttribute;
        end;
      end;

      // NotMapped property ise atla
      if (colAttr = nil) and (hasOneAttr = nil) and (hasManyAttr = nil) and (belongsToAttr = nil) then
        Continue;

      propValue := prop.GetValue(TObject(ASource));

      // Column property'si ise direkt kopyala
      if Assigned(colAttr) then
      begin
        // Primary key auto increment ise ve 0'dan büyükse, klonda 0 yap (yeni kayıt olarak eklenmesi için)
        if colAttr.IsPrimaryKey and colAttr.IsAutoIncrement and not propValue.IsEmpty then
        begin
          case prop.PropertyType.TypeKind of
            tkInteger: prop.SetValue(TObject(Result), TValue.From<Integer>(0));
            tkInt64: prop.SetValue(TObject(Result), TValue.From<Int64>(0));
          end;
        end
        else
        begin
          // Diğer column property'leri direkt kopyala
          if not propValue.IsEmpty then
            prop.SetValue(TObject(Result), propValue);
        end;
      end
      // BelongsTo relationship ise nested entity'yi klonla
      else if Assigned(belongsToAttr) then
      begin
        sourceNestedEntity := propValue.AsObject;
        if Assigned(sourceNestedEntity) then
        begin
          nestedEntityClass := prop.PropertyType.AsInstance.MetaclassType;
          clonedNestedEntity := CreateEntityInstanceByClass(nestedEntityClass);

          // Recursive clone (ama sadece 1 seviye - circular reference'ı önlemek için)
          CloneEntityProperties(sourceNestedEntity, clonedNestedEntity, nestedEntityClass, False);
          prop.SetValue(TObject(Result), clonedNestedEntity);
        end;
      end
      // HasOne relationship ise nested entity'yi klonla
      else if Assigned(hasOneAttr) then
      begin
        sourceNestedEntity := propValue.AsObject;
        if Assigned(sourceNestedEntity) then
        begin
          nestedEntityClass := prop.PropertyType.AsInstance.MetaclassType;
          clonedNestedEntity := CreateEntityInstanceByClass(nestedEntityClass);

          // Recursive clone (ama sadece 1 seviye - circular reference'ı önlemek için)
          CloneEntityProperties(sourceNestedEntity, clonedNestedEntity, nestedEntityClass, False);
          prop.SetValue(TObject(Result), clonedNestedEntity);
        end;
      end
      // HasMany relationship ise list'i klonla
      else if Assigned(hasManyAttr) then
      begin
        sourceList := propValue.AsObject;
        targetList := prop.GetValue(TObject(Result)).AsObject;

        if Assigned(sourceList) and Assigned(targetList) then
        begin
          nestedEntityClass := ExtractGenericTypeFromList(prop.PropertyType);
          if not Assigned(nestedEntityClass) then
            Continue;

          listType := ctx.GetType(sourceList.ClassType);

          // Count property'sini al
          countProp := listType.GetProperty('Count');
          if not Assigned(countProp) then
            Continue;

          count := countProp.GetValue(sourceList).AsInteger;
          if count = 0 then
            Continue;

          // GetItem ve Add metodlarını bul
          getItemMethod := nil;
          addMethod := nil;
          clearMethod := nil;

          for method in listType.GetMethods do
          begin
            if (method.Name = 'GetItem') and (Length(method.GetParameters) = 1) then
              getItemMethod := method
            else if (method.Name = 'Add') and (Length(method.GetParameters) = 1) then
              addMethod := method
            else if (method.Name = 'Clear') and (Length(method.GetParameters) = 0) then
              clearMethod := method;
          end;

          if not Assigned(getItemMethod) or not Assigned(addMethod) then
            Continue;

          // Target list'i temizle
          if Assigned(clearMethod) then
            clearMethod.Invoke(targetList, []);

          // Her item'ı klonla ve target list'e ekle
          for i := 0 to count - 1 do
          begin
            sourceItem := getItemMethod.Invoke(sourceList, [i]).AsObject;
            if Assigned(sourceItem) then
            begin
              clonedItem := CreateEntityInstanceByClass(nestedEntityClass);

              // Nested entity'yi klonla (ama daha derin klonlama yapma - circular reference önlemi)
              CloneEntityProperties(sourceItem, clonedItem, nestedEntityClass, False);

              addMethod.Invoke(targetList, [clonedItem]);
            end;
          end;
        end;
      end;
    end;

  finally
    ctx.Free;
  end;
end;

end.
