unit MigrationGenerator;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  System.TypInfo, System.StrUtils,
  Entity, EntityAttributes;

type
  /// <summary>
  /// Tablo şema bilgisi
  /// </summary>
  TTableSchema = record
    TableName: string;
    SchemaName: string;
    FullTableName: string;
    Columns: TList<TColumnSchema>;
    Indexes: TList<TIndexSchema>;
    ForeignKeys: TList<TForeignKeySchema>;
    CheckConstraints: TList<TCheckConstraintSchema>;
    HasSoftDelete: Boolean;
    SoftDeleteColumn: string;
    SoftDeleteByColumn: string;
  end;

  /// <summary>
  /// Kolon şema bilgisi
  /// </summary>
  TColumnSchema = record
    ColumnName: string;
    PropertyName: string;
    DataType: string;
    PostgresType: string;
    IsNullable: Boolean;
    IsPrimaryKey: Boolean;
    IsAutoIncrement: Boolean;
    DefaultValue: string;
    Length: Integer;
    Precision: Integer;
    Scale: Integer;
    Collation: string;
  end;

  /// <summary>
  /// Index şema bilgisi
  /// </summary>
  TIndexSchema = record
    IndexName: string;
    Columns: TArray<string>;
    IsUnique: Boolean;
  end;

  /// <summary>
  /// Foreign key şema bilgisi
  /// </summary>
  TForeignKeySchema = record
    ConstraintName: string;
    ReferencedTable: string;
    ReferencedColumn: string;
    LocalColumn: string;
    OnDelete: string;
    OnUpdate: string;
  end;

  /// <summary>
  /// Check constraint şema bilgisi
  /// </summary>
  TCheckConstraintSchema = record
    ConstraintName: string;
    Expression: string;
  end;

  /// <summary>
  /// Migration generator - Entity'den SQL üretir
  /// </summary>
  TMigrationGenerator = class
  private
    /// <summary>
    /// Entity sınıfından tablo şemasını çıkarır
    /// </summary>
    function ExtractTableSchema(AEntityClass: TClass): TTableSchema;

    /// <summary>
    /// Delphi tipini PostgreSQL tipine çevirir
    /// </summary>
    function MapDelphiTypeToPostgres(AProp: TRttiProperty; AColumn: Column): string;

    /// <summary>
    /// Kolon tanımı SQL'i üretir
    /// </summary>
    function GenerateColumnDefinition(const AColumn: TColumnSchema): string;

    /// <summary>
    /// Index oluşturma SQL'i üretir
    /// </summary>
    function GenerateIndexSql(const ATableName: string; const AIndex: TIndexSchema): string;

    /// <summary>
    /// Foreign key constraint SQL'i üretir
    /// </summary>
    function GenerateForeignKeySql(const ATableName: string; const AFk: TForeignKeySchema): string;

    /// <summary>
    /// Check constraint SQL'i üretir
    /// </summary>
    function GenerateCheckConstraintSql(const ATableName: string; const ACheck: TCheckConstraintSchema): string;

    /// <summary>
    /// Kolon için comment SQL'i üretir
    /// </summary>
    function GenerateColumnCommentSql(const ATableName, AColumnName, AComment: string): string;

  public
    /// <summary>
    /// CREATE TABLE SQL'i üretir
    /// </summary>
    function GenerateCreateTableSql(AEntityClass: TClass): string;

    /// <summary>
    /// DROP TABLE SQL'i üretir
    /// </summary>
    function GenerateDropTableSql(AEntityClass: TClass): string;

    /// <summary>
    /// ADD COLUMN SQL'i üretir
    /// </summary>
    function GenerateAddColumnSql(AEntityClass: TClass; const APropertyName: string): string;

    /// <summary>
    /// DROP COLUMN SQL'i üretir
    /// </summary>
    function GenerateDropColumnSql(AEntityClass: TClass; const AColumnName: string): string;

    /// <summary>
    /// ALTER COLUMN SQL'i üretir
    /// </summary>
    function GenerateAlterColumnSql(AEntityClass: TClass; const APropertyName: string): string;

    /// <summary>
    /// CREATE INDEX SQL'i üretir
    /// </summary>
    function GenerateCreateIndexSql(AEntityClass: TClass; const AIndexName: string): string;

    /// <summary>
    /// DROP INDEX SQL'i üretir
    /// </summary>
    function GenerateDropIndexSql(const AIndexName: string): string;

    /// <summary>
    /// ADD FOREIGN KEY SQL'i üretir
    /// </summary>
    function GenerateAddForeignKeySql(AEntityClass: TClass; const AFkName: string): string;

    /// <summary>
    /// DROP FOREIGN KEY SQL'i üretir
    /// </summary>
    function GenerateDropForeignKeySql(AEntityClass: TClass; const AFkName: string): string;

    /// <summary>
    /// Birden fazla entity için toplu CREATE TABLE SQL'i üretir
    /// </summary>
    function GenerateCreateTablesSql(AEntityClasses: TArray<TClass>): string;

    /// <summary>
    /// Tablo şemasını alır (test/debug için)
    /// </summary>
    function GetTableSchema(AEntityClass: TClass): TTableSchema;
  end;

implementation

{ TMigrationGenerator }

function TMigrationGenerator.ExtractTableSchema(AEntityClass: TClass): TTableSchema;
var
  Ctx: TRttiContext;
  RType: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  TableAttr: Table;
  ColAttr: Column;
  IndexAttr: IndexAttribute;
  FkAttr: ForeignKeyAttribute;
  CheckAttr: CheckConstraintAttribute;
  SoftDeleteAttr: SoftDelete;
  DefValAttr: DefaultValueAttribute;
  ColumnSchema: TColumnSchema;
  IndexSchema: TIndexSchema;
  FkSchema: TForeignKeySchema;
  CheckSchema: TCheckConstraintSchema;
begin
  Result.Columns := TList<TColumnSchema>.Create;
  Result.Indexes := TList<TIndexSchema>.Create;
  Result.ForeignKeys := TList<TForeignKeySchema>.Create;
  Result.CheckConstraints := TList<TCheckConstraintSchema>.Create;
  Result.HasSoftDelete := False;

  Ctx := TRttiContext.Create;
  try
    RType := Ctx.GetType(AEntityClass);

    // Tablo adını al
    Result.TableName := AEntityClass.ClassName;
    Result.SchemaName := 'public';

    for Attr in RType.GetAttributes do
    begin
      if Attr is Table then
      begin
        TableAttr := Table(Attr);
        Result.TableName := TableAttr.Name;
        Result.FullTableName := TableAttr.FullName;
        if Pos('.', Result.FullTableName) > 0 then
        begin
          Result.SchemaName := Copy(Result.FullTableName, 1, Pos('.', Result.FullTableName) - 1);
          Result.TableName := Copy(Result.FullTableName, Pos('.', Result.FullTableName) + 1, Length(Result.FullTableName));
        end;
      end
      else if Attr is SoftDelete then
      begin
        SoftDeleteAttr := SoftDelete(Attr);
        Result.HasSoftDelete := True;
        Result.SoftDeleteColumn := SoftDeleteAttr.DeletedAtColumn;
        Result.SoftDeleteByColumn := SoftDeleteAttr.DeletedByColumn;
      end
      else if Attr is CheckConstraintAttribute then
      begin
        CheckAttr := CheckConstraintAttribute(Attr);
        CheckSchema.ConstraintName := CheckAttr.Name;
        CheckSchema.Expression := CheckAttr.Expression;
        Result.CheckConstraints.Add(CheckSchema);
      end;
    end;

    // Kolonları çıkar
    for Prop in RType.GetProperties do
    begin
      if not Prop.IsReadable then
        Continue;

      ColAttr := nil;
      DefValAttr := nil;

      // Property attribute'larını kontrol et
      for Attr in Prop.GetAttributes do
      begin
        if Attr is NotMapped then
          Break
        else if Attr is HasManyAttribute then
          Break
        else if Attr is HasOneAttribute then
          Break
        else if Attr is BelongsToAttribute then
          Break
        else if Attr is Column then
          ColAttr := Column(Attr)
        else if Attr is DefaultValueAttribute then
          DefValAttr := DefaultValueAttribute(Attr)
        else if Attr is IndexAttribute then
        begin
          IndexAttr := IndexAttribute(Attr);
          IndexSchema.IndexName := IndexAttr.Name;
          IndexSchema.Columns := IndexAttr.Columns;
          IndexSchema.IsUnique := IndexAttr.IsUnique;
          Result.Indexes.Add(IndexSchema);
        end
        else if Attr is ForeignKeyAttribute then
        begin
          FkAttr := ForeignKeyAttribute(Attr);
          FkSchema.ConstraintName := FkAttr.Name;
          FkSchema.ReferencedTable := FkAttr.ReferencedTable;
          FkSchema.ReferencedColumn := FkAttr.ReferencedColumn;
          FkSchema.LocalColumn := IfThen(ColAttr <> nil, ColAttr.Name, LowerCase(Prop.Name));
          FkSchema.OnDelete := FkAttr.OnDelete;
          FkSchema.OnUpdate := FkAttr.OnUpdate;
          Result.ForeignKeys.Add(FkSchema);
        end;
      end;

      if ColAttr = nil then
        Continue;

      // Kolon bilgilerini doldur
      ColumnSchema.ColumnName := IfThen(ColAttr.Name <> '', ColAttr.Name, LowerCase(Prop.Name));
      ColumnSchema.PropertyName := Prop.Name;
      ColumnSchema.IsNullable := not ColAttr.IsNotNull;
      ColumnSchema.IsPrimaryKey := ColAttr.IsPrimaryKey;
      ColumnSchema.IsAutoIncrement := ColAttr.IsAutoIncrement;
      ColumnSchema.DefaultValue := IfThen(DefValAttr <> nil, DefValAttr.Value, '');
      ColumnSchema.Length := ColAttr.Length;
      ColumnSchema.Precision := ColAttr.Precision;
      ColumnSchema.Scale := ColAttr.Scale;
      ColumnSchema.Collation := ColAttr.Collation;

      // Tip dönüşümü
      ColumnSchema.DataType := Prop.PropertyType.Name;
      ColumnSchema.PostgresType := MapDelphiTypeToPostgres(Prop, ColAttr);

      Result.Columns.Add(ColumnSchema);
    end;

  finally
    Ctx.Free;
  end;
end;

function TMigrationGenerator.MapDelphiTypeToPostgres(AProp: TRttiProperty; AColumn: Column): string;
var
  TypeKind: TTypeKind;
  TypeName: string;
begin
  TypeKind := AProp.PropertyType.TypeKind;
  TypeName := AProp.PropertyType.Name;

  case TypeKind of
    tkInteger:
      begin
        if AColumn.IsPrimaryKey and AColumn.IsAutoIncrement then
          Result := 'SERIAL'
        else if TypeName = 'SmallInt' then
          Result := 'SMALLINT'
        else
          Result := 'INTEGER';
      end;

    tkInt64:
      begin
        if AColumn.IsPrimaryKey and AColumn.IsAutoIncrement then
          Result := 'BIGSERIAL'
        else
          Result := 'BIGINT';
      end;

    tkFloat:
      begin
        if AProp.PropertyType.Handle = TypeInfo(TDateTime) then
          Result := 'TIMESTAMP'
        else if AProp.PropertyType.Handle = TypeInfo(TDate) then
          Result := 'DATE'
        else if AProp.PropertyType.Handle = TypeInfo(TTime) then
          Result := 'TIME'
        else if AProp.PropertyType.Handle = TypeInfo(Double) then
        begin
          if (AColumn.Precision > 0) and (AColumn.Scale > 0) then
            Result := Format('NUMERIC(%d,%d)', [AColumn.Precision, AColumn.Scale])
          else
            Result := 'DOUBLE PRECISION';
        end
        else if AProp.PropertyType.Handle = TypeInfo(Single) then
          Result := 'REAL'
        else if AProp.PropertyType.Handle = TypeInfo(Currency) then
          Result := 'MONEY'
        else
          Result := 'DOUBLE PRECISION';
      end;

    tkString, tkLString, tkWString, tkUString:
      begin
        if AColumn.Length > 0 then
          Result := Format('VARCHAR(%d)', [AColumn.Length])
        else
          Result := 'TEXT';
      end;

    tkEnumeration:
      begin
        if AProp.PropertyType.Handle = TypeInfo(Boolean) then
          Result := 'BOOLEAN'
        else
          Result := 'SMALLINT'; // Enum'ları integer olarak sakla
      end;

    tkChar, tkWChar:
      Result := 'CHAR(1)';

  else
    Result := 'TEXT'; // Default
  end;
end;

function TMigrationGenerator.GenerateColumnDefinition(const AColumn: TColumnSchema): string;
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ' ';
    Parts.StrictDelimiter := True;

    // Kolon adı ve tipi
    Parts.Add(AColumn.ColumnName);
    Parts.Add(AColumn.PostgresType);

    // Collation
    if AColumn.Collation <> '' then
      Parts.Add('COLLATE ' + AColumn.Collation);

    // NULL/NOT NULL
    if not AColumn.IsNullable then
      Parts.Add('NOT NULL')
    else if not AColumn.IsPrimaryKey then
      Parts.Add('NULL');

    // Default value
    if AColumn.DefaultValue <> '' then
      Parts.Add('DEFAULT ' + AColumn.DefaultValue);

    // Primary key
    if AColumn.IsPrimaryKey and not AColumn.IsAutoIncrement then
      Parts.Add('PRIMARY KEY');

    Result := Parts.DelimitedText;
  finally
    Parts.Free;
  end;
end;

function TMigrationGenerator.GenerateCreateTableSql(AEntityClass: TClass): string;
var
  Schema: TTableSchema;
  SQL: TStringBuilder;
  Column: TColumnSchema;
  Index: TIndexSchema;
  Fk: TForeignKeySchema;
  Check: TCheckConstraintSchema;
  i: Integer;
  PkColumns: TStringList;
begin
  Schema := ExtractTableSchema(AEntityClass);
  SQL := TStringBuilder.Create;
  PkColumns := TStringList.Create;
  try
    // CREATE TABLE
    SQL.AppendFormat('CREATE TABLE IF NOT EXISTS %s (', [Schema.FullTableName]);
    SQL.AppendLine;

    // Kolonları ekle
    for i := 0 to Schema.Columns.Count - 1 do
    begin
      Column := Schema.Columns[i];
      SQL.Append('  ');
      SQL.Append(GenerateColumnDefinition(Column));

      if Column.IsPrimaryKey then
        PkColumns.Add(Column.ColumnName);

      if i < Schema.Columns.Count - 1 then
        SQL.Append(',');
      SQL.AppendLine;
    end;

    // Composite Primary Key varsa
    if PkColumns.Count > 1 then
    begin
      SQL.Append(',');
      SQL.AppendLine;
      SQL.AppendFormat('  PRIMARY KEY (%s)', [PkColumns.CommaText.Replace('"', '')]);
      SQL.AppendLine;
    end;

    SQL.Append(');');
    SQL.AppendLine;
    SQL.AppendLine;

    // Index'leri ekle
    for Index in Schema.Indexes do
    begin
      SQL.Append(GenerateIndexSql(Schema.FullTableName, Index));
      SQL.AppendLine;
    end;

    // Foreign key'leri ekle
    for Fk in Schema.ForeignKeys do
    begin
      SQL.Append(GenerateForeignKeySql(Schema.FullTableName, Fk));
      SQL.AppendLine;
    end;

    // Check constraint'leri ekle
    for Check in Schema.CheckConstraints do
    begin
      SQL.Append(GenerateCheckConstraintSql(Schema.FullTableName, Check));
      SQL.AppendLine;
    end;

    Result := SQL.ToString;
  finally
    SQL.Free;
    PkColumns.Free;
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateDropTableSql(AEntityClass: TClass): string;
var
  Schema: TTableSchema;
begin
  Schema := ExtractTableSchema(AEntityClass);
  try
    Result := Format('DROP TABLE IF EXISTS %s CASCADE;', [Schema.FullTableName]);
  finally
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateIndexSql(const ATableName: string; const AIndex: TIndexSchema): string;
var
  UniqueStr: string;
  Columns: string;
  i: Integer;
begin
  UniqueStr := IfThen(AIndex.IsUnique, 'UNIQUE ', '');

  Columns := '';
  for i := 0 to High(AIndex.Columns) do
  begin
    if i > 0 then
      Columns := Columns + ', ';
    Columns := Columns + AIndex.Columns[i];
  end;

  Result := Format('CREATE %sINDEX IF NOT EXISTS %s ON %s (%s);',
                  [UniqueStr, AIndex.IndexName, ATableName, Columns]);
end;

function TMigrationGenerator.GenerateForeignKeySql(const ATableName: string; const AFk: TForeignKeySchema): string;
begin
  Result := Format('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s) ON DELETE %s ON UPDATE %s;',
                  [ATableName, AFk.ConstraintName, AFk.LocalColumn,
                   AFk.ReferencedTable, AFk.ReferencedColumn,
                   AFk.OnDelete, AFk.OnUpdate]);
end;

function TMigrationGenerator.GenerateCheckConstraintSql(const ATableName: string; const ACheck: TCheckConstraintSchema): string;
begin
  Result := Format('ALTER TABLE %s ADD CONSTRAINT %s CHECK (%s);',
                  [ATableName, ACheck.ConstraintName, ACheck.Expression]);
end;

function TMigrationGenerator.GenerateColumnCommentSql(const ATableName, AColumnName, AComment: string): string;
begin
  Result := Format('COMMENT ON COLUMN %s.%s IS ''%s'';',
                  [ATableName, AColumnName, AComment.Replace('''', '''''')]);
end;

function TMigrationGenerator.GenerateAddColumnSql(AEntityClass: TClass; const APropertyName: string): string;
var
  Schema: TTableSchema;
  Column: TColumnSchema;
  Found: Boolean;
begin
  Schema := ExtractTableSchema(AEntityClass);
  try
    Found := False;
    for Column in Schema.Columns do
    begin
      if SameText(Column.PropertyName, APropertyName) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      raise Exception.CreateFmt('Property %s not found in entity %s', [APropertyName, AEntityClass.ClassName]);

    Result := Format('ALTER TABLE %s ADD COLUMN %s;',
                    [Schema.FullTableName, GenerateColumnDefinition(Column)]);
  finally
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateDropColumnSql(AEntityClass: TClass; const AColumnName: string): string;
var
  Schema: TTableSchema;
begin
  Schema := ExtractTableSchema(AEntityClass);
  try
    Result := Format('ALTER TABLE %s DROP COLUMN IF EXISTS %s CASCADE;',
                    [Schema.FullTableName, AColumnName]);
  finally
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateAlterColumnSql(AEntityClass: TClass; const APropertyName: string): string;
var
  Schema: TTableSchema;
  Column: TColumnSchema;
  Found: Boolean;
  SQL: TStringBuilder;
begin
  Schema := ExtractTableSchema(AEntityClass);
  SQL := TStringBuilder.Create;
  try
    Found := False;
    for Column in Schema.Columns do
    begin
      if SameText(Column.PropertyName, APropertyName) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      raise Exception.CreateFmt('Property %s not found in entity %s', [APropertyName, AEntityClass.ClassName]);

    // Tip değişikliği
    SQL.AppendFormat('ALTER TABLE %s ALTER COLUMN %s TYPE %s;',
                    [Schema.FullTableName, Column.ColumnName, Column.PostgresType]);
    SQL.AppendLine;

    // NULL/NOT NULL değişikliği
    if Column.IsNullable then
      SQL.AppendFormat('ALTER TABLE %s ALTER COLUMN %s DROP NOT NULL;',
                      [Schema.FullTableName, Column.ColumnName])
    else
      SQL.AppendFormat('ALTER TABLE %s ALTER COLUMN %s SET NOT NULL;',
                      [Schema.FullTableName, Column.ColumnName]);

    Result := SQL.ToString;
  finally
    SQL.Free;
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateCreateIndexSql(AEntityClass: TClass; const AIndexName: string): string;
var
  Schema: TTableSchema;
  Index: TIndexSchema;
  Found: Boolean;
begin
  Schema := ExtractTableSchema(AEntityClass);
  try
    Found := False;
    for Index in Schema.Indexes do
    begin
      if SameText(Index.IndexName, AIndexName) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      raise Exception.CreateFmt('Index %s not found in entity %s', [AIndexName, AEntityClass.ClassName]);

    Result := GenerateIndexSql(Schema.FullTableName, Index);
  finally
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateDropIndexSql(const AIndexName: string): string;
begin
  Result := Format('DROP INDEX IF EXISTS %s CASCADE;', [AIndexName]);
end;

function TMigrationGenerator.GenerateAddForeignKeySql(AEntityClass: TClass; const AFkName: string): string;
var
  Schema: TTableSchema;
  Fk: TForeignKeySchema;
  Found: Boolean;
begin
  Schema := ExtractTableSchema(AEntityClass);
  try
    Found := False;
    for Fk in Schema.ForeignKeys do
    begin
      if SameText(Fk.ConstraintName, AFkName) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      raise Exception.CreateFmt('Foreign key %s not found in entity %s', [AFkName, AEntityClass.ClassName]);

    Result := GenerateForeignKeySql(Schema.FullTableName, Fk);
  finally
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateDropForeignKeySql(AEntityClass: TClass; const AFkName: string): string;
var
  Schema: TTableSchema;
begin
  Schema := ExtractTableSchema(AEntityClass);
  try
    Result := Format('ALTER TABLE %s DROP CONSTRAINT IF EXISTS %s CASCADE;',
                    [Schema.FullTableName, AFkName]);
  finally
    Schema.Columns.Free;
    Schema.Indexes.Free;
    Schema.ForeignKeys.Free;
    Schema.CheckConstraints.Free;
  end;
end;

function TMigrationGenerator.GenerateCreateTablesSql(AEntityClasses: TArray<TClass>): string;
var
  SQL: TStringBuilder;
  EntityClass: TClass;
begin
  SQL := TStringBuilder.Create;
  try
    SQL.AppendLine('-- Generated migration for multiple tables');
    SQL.AppendLine('-- Generated at: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    SQL.AppendLine;

    for EntityClass in AEntityClasses do
    begin
      SQL.AppendLine('-- Table: ' + EntityClass.ClassName);
      SQL.Append(GenerateCreateTableSql(EntityClass));
      SQL.AppendLine;
    end;

    Result := SQL.ToString;
  finally
    SQL.Free;
  end;
end;

function TMigrationGenerator.GetTableSchema(AEntityClass: TClass): TTableSchema;
begin
  Result := ExtractTableSchema(AEntityClass);
end;

end.
