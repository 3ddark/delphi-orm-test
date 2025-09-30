unit MigrationHistory;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Entity, EntityAttributes;

type
  /// <summary>
  /// Migration durumları
  /// </summary>
  TMigrationStatus = (
    msPending,      // Beklemede
    msApplied,      // Uygulandı
    msRolledBack,   // Geri alındı
    msFailed        // Başarısız
  );

  /// <summary>
  /// Migration history kaydı - EF Core'daki __EFMigrationsHistory tablosuna benzer
  /// </summary>
  [Table('__migration_history', 'public')]
  TMigrationHistoryEntity = class(TEntity)
  private
    FId: Int64;
    FMigrationId: string;
    FMigrationName: string;
    FAppliedAt: TDateTime;
    FRolledBackAt: TDateTime;
    FStatus: TMigrationStatus;
    FUpSql: string;
    FDownSql: string;
    FDatabaseVersion: string;
    FExecutionTime: Integer; // milliseconds
    FErrorMessage: string;
    FBatch: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    [Column('id', [cpPrimaryKey], [cucFind, cucAdd, cucUpdate], dtBigInt)]
    property Id: Int64 read FId write FId;

    [Column('migration_id', False, False, True)]
    [Index('idx_migration_id', ['migration_id'], True)]
    property MigrationId: string read FMigrationId write FMigrationId;

    [Column('migration_name', False, False, True)]
    property MigrationName: string read FMigrationName write FMigrationName;

    [Column('applied_at')]
    property AppliedAt: TDateTime read FAppliedAt write FAppliedAt;

    [Column('rolled_back_at')]
    property RolledBackAt: TDateTime read FRolledBackAt write FRolledBackAt;

    [Column('status', False, False, True)]
    [Index('idx_status', ['status'])]
    property Status: TMigrationStatus read FStatus write FStatus;

    [Column('up_sql')]
    property UpSql: string read FUpSql write FUpSql;

    [Column('down_sql')]
    property DownSql: string read FDownSql write FDownSql;

    [Column('database_version')]
    property DatabaseVersion: string read FDatabaseVersion write FDatabaseVersion;

    [Column('execution_time')]
    property ExecutionTime: Integer read FExecutionTime write FExecutionTime;

    [Column('error_message')]
    property ErrorMessage: string read FErrorMessage write FErrorMessage;

    [Column('batch', False, False, True)]
    [Index('idx_batch', ['batch'])]
    property Batch: Integer read FBatch write FBatch;
  end;

  /// <summary>
  /// Migration snapshot - tablonun son durumunu tutar
  /// </summary>
  [Table('__migration_snapshots', 'public')]
  TMigrationSnapshotEntity = class(TEntity)
  private
    FId: Int64;
    FMigrationId: string;
    FTableName: string;
    FSchemaJson: string; // JSON formatında tablo şeması
    FCreatedAt: TDateTime;
  public
    constructor Create;
  published
    [Column('id', True, True)]
    property Id: Int64 read FId write FId;

    [Column('migration_id', False, False, True)]
    [ForeignKey('fk_snapshot_migration', '__migration_history', 'migration_id', 'CASCADE', 'CASCADE')]
    property MigrationId: string read FMigrationId write FMigrationId;

    [Column('table_name', False, False, True)]
    property TableName: string read FTableName write FTableName;

    [Column('schema_json', False, False, True)]
    property SchemaJson: string read FSchemaJson write FSchemaJson;

    [Column('created_at', False, False, True)]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

implementation

{ TMigrationHistoryEntity }

constructor TMigrationHistoryEntity.Create;
begin
  inherited;
  FAppliedAt := 0;
  FRolledBackAt := 0;
  FStatus := msPending;
  FExecutionTime := 0;
  FBatch := 0;
end;

destructor TMigrationHistoryEntity.Destroy;
begin
  inherited;
end;

{ TMigrationSnapshotEntity }

constructor TMigrationSnapshotEntity.Create;
begin
  inherited;
  FCreatedAt := Now;
end;

end.
