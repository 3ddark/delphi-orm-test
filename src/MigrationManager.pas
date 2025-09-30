unit MigrationManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Rtti,
  System.DateUtils, System.JSON,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Phys.PG,
  Entity, EntityAttributes, Repository, FilterCriterion, MigrationHistory;

type
  /// <summary>
  /// Migration tanımı - abstract base class
  /// </summary>
  TMigration = class abstract
  private
    FMigrationId: string;
    FMigrationName: string;
    FBatch: Integer;
  protected
    /// <summary>
    /// Migration uygulama SQL'lerini döndürür
    /// </summary>
    function GetUpSql: string; virtual; abstract;

    /// <summary>
    /// Migration geri alma SQL'lerini döndürür
    /// </summary>
    function GetDownSql: string; virtual; abstract;
  public
    constructor Create(const AMigrationId, AMigrationName: string); virtual;

    property MigrationId: string read FMigrationId;
    property MigrationName: string read FMigrationName;
    property Batch: Integer read FBatch write FBatch;

    /// <summary>
    /// Up SQL'i döndürür
    /// </summary>
    function Up: string;

    /// <summary>
    /// Down SQL'i döndürür
    /// </summary>
    function Down: string;
  end;

  TMigrationClass = class of TMigration;

  /// <summary>
  /// Migration sonuç bilgisi
  /// </summary>
  TMigrationResult = record
    Success: Boolean;
    MigrationId: string;
    MigrationName: string;
    ExecutionTime: Integer;
    ErrorMessage: string;
  end;

  /// <summary>
  /// Migration Manager - Ana yönetim sınıfı
  /// </summary>
  TMigrationManager = class
  private
    FConnection: TFDConnection;
    FHistoryRepository: IRepository<TMigrationHistoryEntity>;
    FSnapshotRepository: IRepository<TMigrationSnapshotEntity>;
    FRegisteredMigrations: TObjectList<TMigration>;

    /// <summary>
    /// Migration history tablosunun var olup olmadığını kontrol eder
    /// </summary>
    function HistoryTableExists: Boolean;

    /// <summary>
    /// Migration history tablosunu oluşturur
    /// </summary>
    procedure CreateHistoryTable;

    /// <summary>
    /// Snapshot tablosunu oluşturur
    /// </summary>
    procedure CreateSnapshotTable;

    /// <summary>
    /// Uygulanmış son batch numarasını döndürür
    /// </summary>
    function GetLastBatchNumber: Integer;

    /// <summary>
    /// Migration'ı history tablosuna kaydeder
    /// </summary>
    procedure RecordMigration(AMigration: TMigration; AStatus: TMigrationStatus;
                             const AUpSql, ADownSql: string; AExecutionTime: Integer;
                             const AErrorMessage: string = '');

    /// <summary>
    /// Migration'ı uygular
    /// </summary>
    function ExecuteMigration(AMigration: TMigration; ABatch: Integer): TMigrationResult;

    /// <summary>
    /// Migration'ı geri alır
    /// </summary>
    function RollbackMigration(AHistoryEntry: TMigrationHistoryEntity): TMigrationResult;

    /// <summary>
    /// PostgreSQL versiyonunu döndürür
    /// </summary>
    function GetDatabaseVersion: string;

    /// <summary>
    /// Tüm tabloların şemasını JSON olarak çıkarır
    /// </summary>
    function ExtractDatabaseSchema: string;

    /// <summary>
    /// Snapshot kaydı oluşturur
    /// </summary>
    procedure CreateSnapshot(const AMigrationId: string);

  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;

    /// <summary>
    /// Migration kaydeder
    /// </summary>
    procedure RegisterMigration(AMigration: TMigration);

    /// <summary>
    /// Tüm pending migration'ları uygular
    /// </summary>
    function Migrate: TArray<TMigrationResult>;

    /// <summary>
    /// Belirli bir migration'a kadar uygular
    /// </summary>
    function MigrateTo(const AMigrationId: string): TArray<TMigrationResult>;

    /// <summary>
    /// Son batch'i geri alır
    /// </summary>
    function RollbackLastBatch: TArray<TMigrationResult>;

    /// <summary>
    /// Belirli bir migration'a kadar geri alır
    /// </summary>
    function RollbackTo(const AMigrationId: string): TArray<TMigrationResult>;

    /// <summary>
    /// Tüm migration'ları geri alır
    /// </summary>
    function RollbackAll: TArray<TMigrationResult>;

    /// <summary>
    /// Uygulanmış migration'ları listeler
    /// </summary>
    function GetAppliedMigrations: TObjectList<TMigrationHistoryEntity>;

    /// <summary>
    /// Pending migration'ları listeler
    /// </summary>
    function GetPendingMigrations: TList<TMigration>;

    /// <summary>
    /// Migration durumunu kontrol eder
    /// </summary>
    function GetMigrationStatus: string;

    /// <summary>
    /// Database'i temizler ve yeniden oluşturur
    /// </summary>
    procedure ResetDatabase;
  end;

implementation

uses
  System.StrUtils;

{ TMigration }

constructor TMigration.Create(const AMigrationId, AMigrationName: string);
begin
  inherited Create;
  FMigrationId := AMigrationId;
  FMigrationName := AMigrationName;
  FBatch := 0;
end;

function TMigration.Up: string;
begin
  Result := GetUpSql;
end;

function TMigration.Down: string;
begin
  Result := GetDownSql;
end;

{ TMigrationManager }

constructor TMigrationManager.Create(AConnection: TFDConnection);
begin
  inherited Create;

  if not Assigned(AConnection) then
    raise Exception.Create('Database connection is required');

  FConnection := AConnection;
  FRegisteredMigrations := TObjectList<TMigration>.Create(True);

  // History tablosu yoksa oluştur
  if not HistoryTableExists then
  begin
    CreateHistoryTable;
    CreateSnapshotTable;
  end;

  // Repository'leri başlat
  FHistoryRepository := TRepository<TMigrationHistoryEntity>.Create(FConnection);
  FSnapshotRepository := TRepository<TMigrationSnapshotEntity>.Create(FConnection);
end;

destructor TMigrationManager.Destroy;
begin
  FRegisteredMigrations.Free;
  inherited;
end;

function TMigrationManager.HistoryTableExists: Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT EXISTS (' +
      '  SELECT FROM information_schema.tables ' +
      '  WHERE table_schema = ''public'' ' +
      '  AND table_name = ''__migration_history''' +
      ')';
    Query.Open;
    Result := Query.Fields[0].AsBoolean;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.CreateHistoryTable;
var
  Query: TFDQuery;
  SQL: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Migration History tablosu
    SQL :=
      'CREATE TABLE IF NOT EXISTS public.__migration_history (' +
      '  id BIGSERIAL PRIMARY KEY,' +
      '  migration_id VARCHAR(255) NOT NULL UNIQUE,' +
      '  migration_name VARCHAR(500) NOT NULL,' +
      '  applied_at TIMESTAMP,' +
      '  rolled_back_at TIMESTAMP,' +
      '  status SMALLINT NOT NULL DEFAULT 0,' +
      '  up_sql TEXT,' +
      '  down_sql TEXT,' +
      '  database_version VARCHAR(100),' +
      '  execution_time INTEGER DEFAULT 0,' +
      '  error_message TEXT,' +
      '  batch INTEGER NOT NULL DEFAULT 0' +
      ');' +
      'CREATE INDEX IF NOT EXISTS idx_migration_id ON public.__migration_history(migration_id);' +
      'CREATE INDEX IF NOT EXISTS idx_status ON public.__migration_history(status);' +
      'CREATE INDEX IF NOT EXISTS idx_batch ON public.__migration_history(batch);';

    Query.SQL.Text := SQL;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.CreateSnapshotTable;
var
  Query: TFDQuery;
  SQL: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    SQL :=
      'CREATE TABLE IF NOT EXISTS public.__migration_snapshots (' +
      '  id BIGSERIAL PRIMARY KEY,' +
      '  migration_id VARCHAR(255) NOT NULL,' +
      '  table_name VARCHAR(255) NOT NULL,' +
      '  schema_json TEXT NOT NULL,' +
      '  created_at TIMESTAMP NOT NULL DEFAULT NOW(),' +
      '  CONSTRAINT fk_snapshot_migration ' +
      '    FOREIGN KEY (migration_id) ' +
      '    REFERENCES public.__migration_history(migration_id) ' +
      '    ON DELETE CASCADE ON UPDATE CASCADE' +
      ');';

    Query.SQL.Text := SQL;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TMigrationManager.GetLastBatchNumber: Integer;
var
  Query: TFDQuery;
begin
  Result := 0;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT COALESCE(MAX(batch), 0) as last_batch ' +
      'FROM public.__migration_history ' +
      'WHERE status = :status';
    Query.ParamByName('status').AsInteger := Ord(msApplied);
    Query.Open;

    if not Query.IsEmpty then
      Result := Query.FieldByName('last_batch').AsInteger;
  finally
    Query.Free;
  end;
end;

function TMigrationManager.GetDatabaseVersion: string;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT version()';
    Query.Open;
    Result := Query.Fields[0].AsString;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.RecordMigration(AMigration: TMigration;
  AStatus: TMigrationStatus; const AUpSql, ADownSql: string;
  AExecutionTime: Integer; const AErrorMessage: string);
var
  History: TMigrationHistoryEntity;
begin
  History := TMigrationHistoryEntity.Create;
  try
    History.MigrationId := AMigration.MigrationId;
    History.MigrationName := AMigration.MigrationName;
    History.Status := AStatus;
    History.UpSql := AUpSql;
    History.DownSql := ADownSql;
    History.DatabaseVersion := GetDatabaseVersion;
    History.ExecutionTime := AExecutionTime;
    History.ErrorMessage := AErrorMessage;
    History.Batch := AMigration.Batch;

    if AStatus = msApplied then
      History.AppliedAt := Now
    else if AStatus = msRolledBack then
      History.RolledBackAt := Now;

    FHistoryRepository.Add(History);
  finally
    History.Free;
  end;
end;

function TMigrationManager.ExecuteMigration(AMigration: TMigration;
  ABatch: Integer): TMigrationResult;
var
  StartTime: TDateTime;
  UpSql, DownSql: string;
  Query: TFDQuery;
begin
  Result.MigrationId := AMigration.MigrationId;
  Result.MigrationName := AMigration.MigrationName;
  Result.Success := False;
  Result.ErrorMessage := '';

  StartTime := Now;
  AMigration.Batch := ABatch;

  try
    UpSql := AMigration.Up;
    DownSql := AMigration.Down;

    // SQL'i çalıştır
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text := UpSql;
      Query.ExecSQL;
    finally
      Query.Free;
    end;

    Result.ExecutionTime := MilliSecondsBetween(Now, StartTime);
    Result.Success := True;

    // Başarılı migration'ı kaydet
    RecordMigration(AMigration, msApplied, UpSql, DownSql, Result.ExecutionTime);

    // Snapshot oluştur
    CreateSnapshot(AMigration.MigrationId);

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.Message;
      Result.ExecutionTime := MilliSecondsBetween(Now, StartTime);

      // Başarısız migration'ı kaydet
      RecordMigration(AMigration, msFailed, UpSql, DownSql,
                     Result.ExecutionTime, E.Message);
    end;
  end;
end;

function TMigrationManager.RollbackMigration(
  AHistoryEntry: TMigrationHistoryEntity): TMigrationResult;
var
  StartTime: TDateTime;
  Query: TFDQuery;
begin
  Result.MigrationId := AHistoryEntry.MigrationId;
  Result.MigrationName := AHistoryEntry.MigrationName;
  Result.Success := False;
  Result.ErrorMessage := '';

  StartTime := Now;

  try
    if AHistoryEntry.DownSql = '' then
      raise Exception.Create('Down SQL is empty, cannot rollback');

    // Rollback SQL'i çalıştır
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text := AHistoryEntry.DownSql;
      Query.ExecSQL;
    finally
      Query.Free;
    end;

    Result.ExecutionTime := MilliSecondsBetween(Now, StartTime);
    Result.Success := True;

    // Rollback durumunu güncelle
    AHistoryEntry.Status := msRolledBack;
    AHistoryEntry.RolledBackAt := Now;
    FHistoryRepository.Update(AHistoryEntry);

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.Message;
      Result.ExecutionTime := MilliSecondsBetween(Now, StartTime);
    end;
  end;
end;

procedure TMigrationManager.RegisterMigration(AMigration: TMigration);
begin
  FRegisteredMigrations.Add(AMigration);
end;

function TMigrationManager.Migrate: TArray<TMigrationResult>;
var
  PendingMigrations: TList<TMigration>;
  Migration: TMigration;
  Results: TList<TMigrationResult>;
  CurrentBatch: Integer;
  MigrationResult: TMigrationResult;
begin
  PendingMigrations := GetPendingMigrations;
  Results := TList<TMigrationResult>.Create;
  try
    if PendingMigrations.Count = 0 then
    begin
      SetLength(Result, 0);
      Exit;
    end;

    CurrentBatch := GetLastBatchNumber + 1;

    FConnection.StartTransaction;
    try
      for Migration in PendingMigrations do
      begin
        MigrationResult := ExecuteMigration(Migration, CurrentBatch);
        Results.Add(MigrationResult);

        if not MigrationResult.Success then
        begin
          FConnection.Rollback;
          Break;
        end;
      end;

      FConnection.Commit;
    except
      on E: Exception do
      begin
        if FConnection.InTransaction then
          FConnection.Rollback;
        raise;
      end;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
    PendingMigrations.Free;
  end;
end;

function TMigrationManager.MigrateTo(const AMigrationId: string): TArray<TMigrationResult>;
var
  PendingMigrations: TList<TMigration>;
  Migration: TMigration;
  Results: TList<TMigrationResult>;
  CurrentBatch: Integer;
  Found: Boolean;
begin
  PendingMigrations := GetPendingMigrations;
  Results := TList<TMigrationResult>.Create;
  try
    CurrentBatch := GetLastBatchNumber + 1;
    Found := False;

    FConnection.StartTransaction;
    try
      for Migration in PendingMigrations do
      begin
        Results.Add(ExecuteMigration(Migration, CurrentBatch));

        if Migration.MigrationId = AMigrationId then
        begin
          Found := True;
          Break;
        end;
      end;

      if not Found then
        raise Exception.CreateFmt('Migration %s not found', [AMigrationId]);

      FConnection.Commit;
    except
      on E: Exception do
      begin
        if FConnection.InTransaction then
          FConnection.Rollback;
        raise;
      end;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
    PendingMigrations.Free;
  end;
end;

function TMigrationManager.RollbackLastBatch: TArray<TMigrationResult>;
var
  LastBatch: Integer;
  AppliedMigrations: TObjectList<TMigrationHistoryEntity>;
  Migration: TMigrationHistoryEntity;
  Results: TList<TMigrationResult>;
  i: Integer;
begin
  LastBatch := GetLastBatchNumber;

  if LastBatch = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  // Son batch'teki migration'ları al (ters sırada)
  AppliedMigrations := FHistoryRepository.Find(
    TFilterCriteria.Create
      .Add('Batch', '=', LastBatch)
      .Add('Status', '=', Ord(msApplied))
  );

  Results := TList<TMigrationResult>.Create;
  try
    FConnection.StartTransaction;
    try
      // Ters sırada rollback yap
      for i := AppliedMigrations.Count - 1 downto 0 do
      begin
        Migration := AppliedMigrations[i];
        Results.Add(RollbackMigration(Migration));
      end;

      FConnection.Commit;
    except
      on E: Exception do
      begin
        if FConnection.InTransaction then
          FConnection.Rollback;
        raise;
      end;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
    AppliedMigrations.Free;
  end;
end;

function TMigrationManager.RollbackTo(const AMigrationId: string): TArray<TMigrationResult>;
var
  AppliedMigrations: TObjectList<TMigrationHistoryEntity>;
  Migration: TMigrationHistoryEntity;
  Results: TList<TMigrationResult>;
  i: Integer;
  Found: Boolean;
begin
  AppliedMigrations := FHistoryRepository.Find(
    TFilterCriteria.Create.Add('Status', '=', Ord(msApplied))
  );

  Results := TList<TMigrationResult>.Create;
  try
    Found := False;

    FConnection.StartTransaction;
    try
      // Ters sırada rollback yap
      for i := AppliedMigrations.Count - 1 downto 0 do
      begin
        Migration := AppliedMigrations[i];

        if Migration.MigrationId = AMigrationId then
        begin
          Found := True;
          Break;
        end;

        Results.Add(RollbackMigration(Migration));
      end;

      if not Found then
        raise Exception.CreateFmt('Migration %s not found', [AMigrationId]);

      FConnection.Commit;
    except
      on E: Exception do
      begin
        if FConnection.InTransaction then
          FConnection.Rollback;
        raise;
      end;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
    AppliedMigrations.Free;
  end;
end;

function TMigrationManager.RollbackAll: TArray<TMigrationResult>;
var
  AppliedMigrations: TObjectList<TMigrationHistoryEntity>;
  Migration: TMigrationHistoryEntity;
  Results: TList<TMigrationResult>;
  i: Integer;
begin
  AppliedMigrations := FHistoryRepository.Find(
    TFilterCriteria.Create.Add('Status', '=', Ord(msApplied))
  );

  Results := TList<TMigrationResult>.Create;
  try
    FConnection.StartTransaction;
    try
      for i := AppliedMigrations.Count - 1 downto 0 do
      begin
        Migration := AppliedMigrations[i];
        Results.Add(RollbackMigration(Migration));
      end;

      FConnection.Commit;
    except
      on E: Exception do
      begin
        if FConnection.InTransaction then
          FConnection.Rollback;
        raise;
      end;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
    AppliedMigrations.Free;
  end;
end;

function TMigrationManager.GetAppliedMigrations: TObjectList<TMigrationHistoryEntity>;
begin
  Result := FHistoryRepository.Find(
    TFilterCriteria.Create.Add('Status', '=', Ord(msApplied))
  );
end;

function TMigrationManager.GetPendingMigrations: TList<TMigration>;
var
  AppliedMigrations: TObjectList<TMigrationHistoryEntity>;
  Migration: TMigration;
  AppliedIds: TStringList;
  History: TMigrationHistoryEntity;
begin
  Result := TList<TMigration>.Create;
  AppliedIds := TStringList.Create;
  try
    // Uygulanmış migration ID'lerini al
    AppliedMigrations := GetAppliedMigrations;
    try
      for History in AppliedMigrations do
        AppliedIds.Add(History.MigrationId);
    finally
      AppliedMigrations.Free;
    end;

    // Uygulanmamış migration'ları bul
    for Migration in FRegisteredMigrations do
    begin
      if AppliedIds.IndexOf(Migration.MigrationId) = -1 then
        Result.Add(Migration);
    end;
  finally
    AppliedIds.Free;
  end;
end;

function TMigrationManager.GetMigrationStatus: string;
var
  Applied, Pending: Integer;
  AppliedList: TObjectList<TMigrationHistoryEntity>;
  PendingList: TList<TMigration>;
begin
  AppliedList := GetAppliedMigrations;
  PendingList := GetPendingMigrations;
  try
    Applied := AppliedList.Count;
    Pending := PendingList.Count;

    Result := Format('Applied: %d, Pending: %d, Total: %d',
                    [Applied, Pending, Applied + Pending]);
  finally
    AppliedList.Free;
    PendingList.Free;
  end;
end;

function TMigrationManager.ExtractDatabaseSchema: string;
var
  Query: TFDQuery;
  JSON: TJSONObject;
  TablesArray: TJSONArray;
  TableObj: TJSONObject;
begin
  JSON := TJSONObject.Create;
  TablesArray := TJSONArray.Create;
  try
    JSON.AddPair('extracted_at', TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)));
    JSON.AddPair('database_version', TJSONString.Create(GetDatabaseVersion));

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text :=
        'SELECT table_name ' +
        'FROM information_schema.tables ' +
        'WHERE table_schema = ''public'' ' +
        'AND table_type = ''BASE TABLE'' ' +
        'AND table_name NOT LIKE ''__migration%''';
      Query.Open;

      while not Query.Eof do
      begin
        TableObj := TJSONObject.Create;
        TableObj.AddPair('table_name', TJSONString.Create(Query.FieldByName('table_name').AsString));
        TablesArray.AddElement(TableObj);
        Query.Next;
      end;
    finally
      Query.Free;
    end;

    JSON.AddPair('tables', TablesArray);
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

procedure TMigrationManager.CreateSnapshot(const AMigrationId: string);
var
  Snapshot: TMigrationSnapshotEntity;
  SchemaJson: string;
begin
  SchemaJson := ExtractDatabaseSchema;

  Snapshot := TMigrationSnapshotEntity.Create;
  try
    Snapshot.MigrationId := AMigrationId;
    Snapshot.TableName := 'database_full_schema';
    Snapshot.SchemaJson := SchemaJson;
    Snapshot.CreatedAt := Now;

    FSnapshotRepository.Add(Snapshot);
  finally
    Snapshot.Free;
  end;
end;

procedure TMigrationManager.ResetDatabase;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    FConnection.StartTransaction;
    try
      // Tüm tabloları sil (cascade)
      Query.SQL.Text :=
        'DO $$ DECLARE ' +
        '  r RECORD; ' +
        'BEGIN ' +
        '  FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = ''public'') LOOP ' +
        '    EXECUTE ''DROP TABLE IF EXISTS '' || quote_ident(r.tablename) || '' CASCADE''; ' +
        '  END LOOP; ' +
        'END $$;';
      Query.ExecSQL;

      // History tablolarını yeniden oluştur
      CreateHistoryTable;
      CreateSnapshotTable;

      FConnection.Commit;
    except
      on E: Exception do
      begin
        if FConnection.InTransaction then
          FConnection.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

end.
