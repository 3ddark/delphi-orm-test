unit Ths.Orm.Manager;

interface

uses
  System.SysUtils, Classes, StrUtils, System.Variants, Data.DB,
  System.Rtti, System.Generics.Collections,
  ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZAbstractConnection, ZConnection,
  Ths.Orm.Table;

type
  TEntityManager = class;
  TPermissionTypes = (prtRead, prtAdd, prtUpdate, prtDelete, prtSpecial);

  TBusinessOperationEvent = procedure(AManager: TEntityManager; ATable: TThsTable; APermissionCheck: Boolean) of Object;

  TEntityManager = class
  private
    FId: Int64;
    FConnection: TZConnection;

    function NewQuery: TZQuery;

    function CallCreateMethod<T>: T;

    function PrepareSelectQuery(ATable: TThsTable): string;
    function PrepareSelectCustomQuery(ATable: TThsTable; AFields: TArray<TThsField>): string;
    function PrepareInsertQuery(ATable: TThsTable): string;
    function PrepareUpdateQuery(ATable: TThsTable): string;
    function PrepareUpdateCustomQuery(ATable: TThsTable; AFields: TArray<TThsField>): string;
    function PrepareDeleteQuery(ATable: TThsTable): string;

    function GetOneBase<T: Class>(var ATable: T; AFilter: string; ALock: Boolean): Boolean;
    function GetOneCustomBase<T: Class>(var ATable: T; AFields: TArray<TThsField>; AFilter: string; ALock: Boolean): Boolean;
  public
    property Connection: TZConnection read FConnection;

    function GetNewRecordId: Int64;

    constructor Create(AHostName, ADatabase, AUserName, AUserPass, ALibraryPath: string; APort: Integer); virtual;
    destructor Destroy; override;

    function GetList<T: Class>(var AList: TObjectList<T>; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean;
    function GetListCustom<T: Class>(var AList: TObjectList<T>; AFields: TArray<TThsField>; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean;

    function GetOne<T: Class>(var ATable: T; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;
    function GetOneCustom<T: Class>(var ATable: T; AFields: TArray<TThsField>; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;
    function GetOne<T: Class>(var ATable: T; AID: Int64; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;
    function GetOneCustom<T: Class>(var ATable: T; AFields: TArray<TThsField>; AID: Int64; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;

    function Insert<T: Class>(ATable: T; APermissionCheck: Boolean=True): Boolean;
    function DoInsert<T: Class>(ATable: T; APermissionCheck: Boolean=True): Boolean;
    function BeforeInsertDB<T: Class>(ATable: T): Boolean;
    function AfterInsertDB<T: Class>(ATable: T): Boolean;

    function Update<T: Class>(ATable: T; APermissionCheck: Boolean=True): Boolean;
    function DoUpdate<T: Class>(ATable: T; APermissionCheck: Boolean): Boolean;
    function BeforeUpdateDB<T: Class>(ATable: T): Boolean;
    function AfterUpdateDB<T: Class>(ATable: T): Boolean;

    function CustomUpdate<T: Class>(ATable: T; AFields: TArray<TThsField>; APermissionCheck: Boolean=True): Boolean;
    function DoCustomUpdate<T: Class>(ATable: T; AFields: TArray<TThsField>; APermissionCheck: Boolean): Boolean;
    function BeforeCustomUpdateDB<T: Class>(ATable: T): Boolean;
    function AfterCustomUpdateDB<T: Class>(ATable: T): Boolean;

    function DeleteBatch<T: Class>(AFilter: string; APermissionCheck: Boolean=True): Boolean; overload;
    function DeleteBatch<T: Class>(ATables: TObjectList<T>; APermissionCheck: Boolean=True): Boolean; overload;
    function Delete<T: Class>(ATable: T; APermissionCheck: Boolean=True): Boolean;
    function DoDelete<T: Class>(ATable: T; APermissionCheck: Boolean): Boolean;
    function BeforeDeleteDB<T: Class>(ATable: T): Boolean;
    function AfterDeleteDB<T: Class>(ATable: T): Boolean;

    function LogicalSelectOne<T: Class>(var ATable: T; AFilter: string; ALock, AWithBegin, APermissionCheck: Boolean): Boolean;
    function LogicalSelectList<T: Class>(var ATables: TObjectList<T>; AFilter: string; ALock, AWithBegin, APermissionCheck: Boolean): Boolean;
    function LogicalInsertOne<T: Class>(ATable: T; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
    function LogicalInsertList<T: Class>(ATables: TObjectList<T>; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
    function LogicalUpdateOne<T: Class>(ATable: T; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
    function LogicalUpdateList<T: Class>(ATables: TObjectList<T>; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
    function LogicalDeleteOne<T: Class>(ATable: T; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
    function LogicalDeleteList<T: Class>(ATables: TObjectList<T>; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;

    function Clone<T: Class>(ASrc: T): T;

    procedure SetPostgresServerVariable(AVarName, AValue: string);

    procedure Listen(ATableName: string); virtual;
    procedure Unlisten(ATableName: string); virtual;
    procedure Notify(ATableName: string); virtual;

    function IsAuthorized(ATableSourceCode: string; APermissionType: TPermissionTypes; APermissionCheck: Boolean; AShowException: Boolean=True): Boolean;

    procedure StartTrans(AConnection: TZAbstractConnection = nil);
    procedure CommitTrans(AConnection: TZAbstractConnection = nil);
    procedure RollbackTrans(AConnection: TZAbstractConnection = nil);

    function GetToday(): TDateTime;
  end;

implementation

uses Logger;

function TEntityManager.CallCreateMethod<T>: T;
var
  AModel: TObject;

  rC: TRttiContext;
  rT: TRttiType;
  rM: TRttiMethod;
  n1: Integer;
  rPrms: TArray<TRttiParameter>;
  rParams: TArray<TValue>;
begin
  rT := rC.GetType(TypeInfo(T));
  rM := rT.GetMethod('Create');
  rPrms := rM.GetParameters;
  SetLength(rParams, Length(rPrms));
  for n1 := 0 to Length(rPrms) - 1 do
    case rPrms[n1].ParamType.TypeKind of // do whatever you need to initialize parameters
      tkClass:
        rParams[n1] := TValue.From<TObject>(nil);
      tkString, tkLString:
        rParams[n1] := TValue.From<string>('');
      tkUString:
        rParams[n1] := TValue.From<UnicodeString>('');
      tkWideString:
        rParams[n1] := TValue.From<UnicodeString>('');
      tkInteger, tkInt64:
        rParams[n1] := TValue.From<Integer>(0);
      tkFloat:
        rParams[n1] := TValue.From<Double>(0);
    end;

  if rM.IsConstructor then
  begin
    Result := rM.Invoke(rT.AsInstance.MetaclassType, rParams).AsType<T>;
  end;
end;

constructor TEntityManager.Create(AHostName, ADatabase, AUserName, AUserPass, ALibraryPath: string; APort: Integer);
begin
  FConnection := TZConnection.Create(nil);

  FConnection.LibraryLocation := ALibraryPath;
  FConnection.Protocol := 'postgresql-9';
  FConnection.ClientCodepage := 'UTF8';
  FConnection.HostName := AHostName;
  FConnection.Database := ADatabase;
  FConnection.User := AUserName;
  FConnection.Password := AUserPass;
  FConnection.Port := APort;
  FConnection.LoginPrompt := False;
  FConnection.LibLocation := ALibraryPath;

  with Self.NewQuery do
  try
    SQL.Text := 'SELECT pg_backend_pid()';
    Open;
    GLogger.DBConnectionPID := Fields.Fields[0].AsString;
    Close;
  finally
    Free;
  end
end;

destructor TEntityManager.Destroy;
begin
  FreeAndNil(FConnection);
  inherited;
end;

function TEntityManager.GetNewRecordId: Int64;
begin
  FId := FId - 1;
  Result := FId;
end;

function TEntityManager.GetList<T>(var AList: TObjectList<T>; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  ATable: T;
  AFieldDB: TThsField;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  try
    AList := TObjectList<T>.Create;

    ATable := CallCreateMethod<T>;
    if not Self.IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtRead, APermissionCheck) then
    begin
      TThsTable(ATable).Free;
      TThsTable(ATable) := nil;
      Exit;
    end;

    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := Self.PrepareSelectQuery(ATable as TThsTable) + ' WHERE ' + IfThen(AFilter = '', '1=1', AFilter);
      TThsTable(ATable).Free;
      TThsTable(ATable) := nil;
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        if LQry.RecordCount > 0 then
          Result := True;
        LQry.First;
        while not LQry.Eof do
        begin
          ATable := CallCreateMethod<T>;
          for AFieldDB in (ATable as TThsTable).Fields do
          begin
            if fpSelect in AFieldDB.FieldIslemTipleri then
            begin
              for AField in LQry.Fields do
              begin
                if AFieldDB.FieldName = AField.FieldName then
                begin
                  AFieldDB.Value := AField.Value;
                  Break;
                end;
              end;
            end;
          end;

          AList.Add(ATable);

          LQry.Next;
        end;
      end;
    finally
      LQry.DisposeOf;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetListCustom<T>(var AList: TObjectList<T>; AFields: TArray<TThsField>; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  ATable: T;
  AFieldDB: TThsField;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  try
    ATable := CallCreateMethod<T>;
    if not Self.IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtRead, APermissionCheck) then
    begin
      TThsTable(ATable).Free;
      TThsTable(ATable) := nil;
      Exit;
    end;

    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := Self.PrepareSelectCustomQuery(ATable as TThsTable, AFields) + ' WHERE ' + AFilter;
      TThsTable(ATable).Free;
      TThsTable(ATable) := nil;
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        if LQry.RecordCount > 0 then
          Result := True;
        LQry.First;
        while not LQry.Eof do
        begin
          ATable := CallCreateMethod<T>;
          for AFieldDB in (ATable as TThsTable).Fields do
          begin
            if fpSelect in AFieldDB.FieldIslemTipleri then
            begin
              for AField in LQry.Fields do
              begin
                if AFieldDB.FieldName = AField.FieldName then
                begin
                  AFieldDB.Value := AField.Value;
                  Break;
                end;
              end;
            end;
          end;

          AList.Add(ATable);

          LQry.Next;
        end;
      end;
    finally
      LQry.DisposeOf;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOne<T>(var ATable: T; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
begin
  try
    Result := False;
    try
      ATable := CallCreateMethod<T>;
      if not Self.IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtRead, APermissionCheck) then
        Exit;
    finally
      TThsTable(ATable).Free;
      TThsTable(ATable) := nil;
    end;

    Result := GetOneBase(ATable, AFilter, ALock);
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOneCustom<T>(var ATable: T; AFields: TArray<TThsField>; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  LTable: T;
begin
  try
    Result := False;
    try
      LTable := CallCreateMethod<T>;
      if not Self.IsAuthorized((LTable as TThsTable).TableSourceCode, TPermissionTypes.prtRead, APermissionCheck) then
        Exit;
      Result := GetOneCustomBase(ATable, AFields, AFilter, ALock);
    finally
      TThsTable(LTable).Free;
      TThsTable(LTable) := nil;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOne<T>(var ATable: T; AID: Int64; ALock: Boolean; APermissionCheck: Boolean): Boolean;
var
  LTable: T;
begin
  try
    Result := False;
    try
      LTable := CallCreateMethod<T>;
      if not Self.IsAuthorized((LTable as TThsTable).TableSourceCode, TPermissionTypes.prtRead, APermissionCheck) then
        Exit;

      Result := GetOneBase(ATable, 'id=' + AID.ToString, ALock)
    finally
      TThsTable(LTable).Free;
      TThsTable(LTable) := nil;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOneCustom<T>(var ATable: T; AFields: TArray<TThsField>; AID: Int64; ALock, APermissionCheck: Boolean): Boolean;
var
  LTable: T;
begin
  try
    Result := False;
    try
      LTable := CallCreateMethod<T>;
      if not Self.IsAuthorized((LTable as TThsTable).TableSourceCode, TPermissionTypes.prtRead, APermissionCheck) then
        Exit;

      Result := GetOneCustomBase(ATable, AFields, 'id=' + AID.ToString, ALock);
    finally
      TThsTable(LTable).Free;
      TThsTable(LTable) := nil;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOneBase<T>(var ATable: T; AFilter: string; ALock: Boolean): Boolean;
var
  AFieldDB: TThsField;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  LQry := Self.NewQuery;
  ATable := CallCreateMethod<T>;
  try
    if ((ATable as TThsTable).TableName = '') then
      Exit;

    LQry.SQL.Text := Self.PrepareSelectQuery((ATable as TThsTable)) + ' WHERE ' + IfThen(AFilter = '', '1=1', AFilter);
    LQry.SQL.Text := LQry.SQL.Text + IfThen(ALock, ' FOR UPDATE OF ' + (ATable as TThsTable).TableName + ' NOWAIT;', ';');

    LQry.Prepare;
    if LQry.Prepared then
    begin
      LQry.Open;
      GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
      if LQry.RecordCount > 1 then
        raise Exception.Create('Found one more than records!!!');

      if LQry.RecordCount = 1 then
        Result := True;
      for AFieldDB in (ATable as TThsTable).Fields do
      begin
        if fpSelect in AFieldDB.FieldIslemTipleri then
        begin
          for AField in LQry.Fields do
          begin
            if AFieldDB.FieldName = AField.FieldName then
            begin
              AFieldDB.Value := AField.Value;
              Break;
            end;
          end;
        end;
      end;
    end;
  finally
    LQry.DisposeOf;
  end;
end;

function TEntityManager.GetOneCustomBase<T>(var ATable: T; AFields: TArray<TThsField>; AFilter: string; ALock: Boolean): Boolean;
var
  AFieldDB: TThsField;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  LQry := Self.NewQuery;
  ATable := CallCreateMethod<T>;
  try
    if ((ATable as TThsTable).TableName = '') then
      Exit;

    LQry.SQL.Text := Self.PrepareSelectCustomQuery((ATable as TThsTable), AFields) + ' WHERE ' + IfThen(AFilter = '', '1=1', AFilter);
    LQry.SQL.Text := LQry.SQL.Text + IfThen(ALock, ' FOR UPDATE OF ' + (ATable as TThsTable).TableName + ' NOWAIT;', ';');

    LQry.Prepare;
    if LQry.Prepared then
    begin
      LQry.Open;
      GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
      if LQry.RecordCount > 1 then
        raise Exception.Create('Found one more than records!!!');

      if LQry.RecordCount = 1 then
        Result := True;
      for AFieldDB in (ATable as TThsTable).Fields do
      begin
        if fpSelect in AFieldDB.FieldIslemTipleri then
        begin
          for AField in LQry.Fields do
          begin
            if AFieldDB.FieldName = AField.FieldName then
            begin
              AFieldDB.Value := AField.Value;
              Break;
            end;
          end;
        end;
      end;
    end;
  finally
    LQry.DisposeOf;
  end;
end;

function TEntityManager.BeforeInsertDB<T>(ATable: T): Boolean;
begin
  Result := (ATable as TThsTable).Validate;
end;

function TEntityManager.Insert<T>(ATable: T; APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if not IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtAdd, APermissionCheck) then
      Exit;

    Result := BeforeInsertDB(ATable);
    if Result then
    begin
      Result := DoInsert(ATable, APermissionCheck);
      if Result then
        AfterInsertDB(ATable);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.DoInsert<T>(ATable: T; APermissionCheck: Boolean): Boolean;
var
  LQry: TZQuery;
begin
  Result := False;
  LQry := Self.NewQuery;
  try
    LQry.SQL.Text := PrepareInsertQuery(ATable as TThsTable);
    LQry.Prepare;
    if LQry.Prepared then
    begin
      LQry.Open;
      GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
      (ATable as TThsTable).Id.Value := LQry.Fields.Fields[0].Value;
      LQry.Close;
      Result := True;
    end;
  finally
    LQry.DisposeOf;
  end;
end;

function TEntityManager.AfterInsertDB<T>(ATable: T): Boolean;
begin
  GLogger.RunLog('INSERTING ' + ATable.ClassName + ' id: ' + IntToStr((ATable as TThsTable).Id.AsInt64));
  Result := True;
end;

function TEntityManager.BeforeUpdateDB<T>(ATable: T): Boolean;
begin
  Result := (ATable as TThsTable).Validate;
end;

function TEntityManager.Update<T>(ATable: T; APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if not IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtUpdate, APermissionCheck) then
      Exit;

    Result := BeforeUpdateDB(ATable);
    if Result then
    begin
      Result := DoUpdate(ATable, APermissionCheck);
      if Result then
        AfterUpdateDB(ATable);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.DoUpdate<T>(ATable: T; APermissionCheck: Boolean): Boolean;
var
  LQry: TZQuery;
begin
  LQry := Self.NewQuery;
  Result := False;
  try
    LQry.SQL.Text := PrepareUpdateQuery(ATable as TThsTable);
    LQry.Prepare;
    if LQry.Prepared then
    begin
      LQry.ExecSQL;
      GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
      LQry.Close;
      Result := True;
    end;
  finally
    LQry.DisposeOf;
  end;
end;

function TEntityManager.AfterUpdateDB<T>(ATable: T): Boolean;
begin
  GLogger.RunLog('UPDATING ' + (ATable as TThsTable).ClassName + ' id: ' + IntToStr((ATable as TThsTable).Id.AsInt64));
  Result := True;
end;

function TEntityManager.BeforeCustomUpdateDB<T>(ATable: T): Boolean;
begin
  Result := (ATable as TThsTable).Validate;
end;

function TEntityManager.CustomUpdate<T>(ATable: T; AFields: TArray<TThsField>; APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if not IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtUpdate, APermissionCheck) then
      Exit;

    Result := BeforeCustomUpdateDB(ATable);
    if Result then
    begin
      Result := DoCustomUpdate(ATable, AFields, APermissionCheck);
      if Result then
        AfterCUstomUpdateDB(ATable);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.DoCustomUpdate<T>(ATable: T; AFields: TArray<TThsField>; APermissionCheck: Boolean): Boolean;
var
  LQry: TZQuery;
begin
  Result := False;
  LQry := Self.NewQuery;
  try
    LQry.SQL.Text := PrepareUpdateCustomQuery(ATable as TThsTable, AFields);
    LQry.Prepare;
    if LQry.Prepared then
    begin
      LQry.ExecSQL;
      GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
      LQry.Close;
      Result := True;
    end;
  finally
    LQry.DisposeOf;
  end;
end;

function TEntityManager.AfterCustomUpdateDB<T>(ATable: T): Boolean;
begin
  GLogger.RunLog('CUSTOM UPDATING ' + (ATable as TThsTable).ClassName + ' id: ' + IntToStr((ATable as TThsTable).Id.AsInt64));
  Result := True;
end;

function TEntityManager.BeforeDeleteDB<T>(ATable: T): Boolean;
begin
  Result := True;
end;

function TEntityManager.Delete<T>(ATable: T; APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if not IsAuthorized((ATable as TThsTable).TableSourceCode, TPermissionTypes.prtDelete, APermissionCheck) then
      Exit;

    Result := BeforeDeleteDB(ATable);
    if Result then
    begin
      Result := DoDelete(ATable, APermissionCheck);
      if Result then
        AfterDeleteDB(ATable);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.DoDelete<T>(ATable: T; APermissionCheck: Boolean): Boolean;
begin
  with Self.NewQuery do
  try
    SQL.Text := PrepareDeleteQuery((ATable as TThsTable));
    ExecSQL;
    GLogger.RunLog(SQL.Text.Replace(sLineBreak, ''));
    Result := True;
  finally
    Free;
  end;
end;

function TEntityManager.AfterDeleteDB<T>(ATable: T): Boolean;
begin
  GLogger.RunLog('DELETING ' + (ATable as TThsTable).ClassName + ' id: ' + IntToStr((ATable as TThsTable).Id.AsInt64));
  Result := True;
end;

function TEntityManager.DeleteBatch<T>(AFilter: string; APermissionCheck: Boolean): Boolean;
var
  ATable: T;
  LQry: TZQuery;
begin
  Result := False;
  try
    ATable := CallCreateMethod<T>;

    if not Self.IsAuthorized(TThsTable(ATable).TableSourceCode, TPermissionTypes.prtDelete, APermissionCheck) then
    begin
      TThsTable(ATable).Free;
      TThsTable(ATable) := nil;
      Exit;
    end;

    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := Self.PrepareDeleteQuery((ATable as TThsTable)) + IfThen(AFilter <> '', ' and ' + AFilter, '') + ';';
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.ExecSQL;
        GLogger.RunLog(ReplaceStr(LQry.SQL.Text, sLineBreak, EmptyStr) + ' - BATCH DELETING');
        TThsTable(ATable).Free;
        TThsTable(ATable) := nil;
        Result := True;
      end;
    finally
      LQry.DisposeOf;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.DeleteBatch<T>(ATables: TObjectList<T>; APermissionCheck: Boolean): Boolean;
var
  ATable: T;
  LPermissionCheck: Boolean;
begin
  Self.StartTrans();
  try
    LPermissionCheck := APermissionCheck;
    Result := LPermissionCheck;
    for ATable in ATables do
    begin
      if Result then
      begin
        Result := Self.Delete(ATable, LPermissionCheck);
        if APermissionCheck then
          LPermissionCheck := False;
      end;
    end;
    Self.CommitTrans();
  except
    on E: Exception do
    begin
      Self.RollbackTrans();
    end;
  end;
end;

function TEntityManager.LogicalSelectOne<T>(var ATable: T; AFilter: string; ALock, AWithBegin, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if not ALock then
      AWithBegin := False;

    if AWithBegin then
      Self.StartTrans;

    Result := Self.GetOne<T>(ATable, AFilter, ALock, APermissionCheck);
    if Result then
      Result := (ATable as TThsTable).BusinessSelect(AFilter, ALock, APermissionCheck);
  except
    on E: Exception do
    begin
      Result := False;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalSelectList<T>(var ATables: TObjectList<T>; AFilter: string; ALock, AWithBegin, APermissionCheck: Boolean): Boolean;
var
  ATable: T;
begin
  Result := False;
  try
    if not ALock then
      AWithBegin := False;

    if AWithBegin then
      Self.StartTrans;

    Result := Self.GetList<T>(ATables, AFilter, ALock, APermissionCheck);

    for ATable in ATables do
      if Result then
        Result := (ATable as TThsTable).BusinessSelect(AFilter, ALock, APermissionCheck);
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalInsertOne<T>(ATable: T; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if AWithBegin then
      StartTrans;

    Result := (ATable as TThsTable).BusinessInsert(APermissionCheck);

    if AWithCommit then
      CommitTrans;
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalInsertList<T>(ATables: TObjectList<T>; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
var
  ATable: T;
begin
  Result := True;
  try
    if AWithBegin then
      StartTrans;

    for ATable in ATables do
      if Result then
        Result := (ATable as TThsTable).BusinessInsert(APermissionCheck)
      else
      begin
        Self.RollbackTrans;
        Exit;
      end;

    if AWithCommit then
      CommitTrans;
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalUpdateOne<T>(ATable: T; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if AWithBegin then
      StartTrans;

    (ATable as TThsTable).BusinessUpdate(APermissionCheck);

    if AWithCommit then
      CommitTrans;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalUpdateList<T>(ATables: TObjectList<T>; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
var
  ATable: T;
begin
  Result := False;
  try
    if AWithBegin then
      StartTrans;

    for ATable in ATables do
      (ATable as TThsTable).BusinessUpdate(APermissionCheck);

    if AWithCommit then
      CommitTrans;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalDeleteOne<T>(ATable: T; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if AWithBegin then
      StartTrans;

    (ATable as TThsTable).BusinessDelete(APermissionCheck);

    if AWithCommit then
      CommitTrans;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalDeleteList<T>(ATables: TObjectList<T>; AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
var
  ATable: T;
begin
  Result := False;
  try
    if AWithBegin then
      StartTrans;

    for ATable in ATables do
      (ATable as TThsTable).BusinessDelete(APermissionCheck);

    if AWithCommit then
      CommitTrans;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

procedure TEntityManager.Listen(ATableName: string);
begin
  try
    if ATableName = '' then
      raise Exception.Create('Table name required. You cant do "LISTEN" process!!!"');

    with Self.NewQuery do
    try
      SQL.Text := 'listen ' + ATableName + ';';
      ExecSQL;
    finally
      Free;
    end;
    GLogger.RunLog('LISTENING ' + ATableName);
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

procedure TEntityManager.Unlisten(ATableName: string);
begin
  try
    if ATableName = '' then
      raise Exception.Create('Table name required. You cant do "UNLISTEN" process!!!"');

    with Self.NewQuery do
    try
      SQL.Text := 'unlisten ' + ATableName + ';';
      ExecSQL;
    finally
      Free;
    end;
    GLogger.RunLog('UNLISTENING ' + ATableName );
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

procedure TEntityManager.Notify(ATableName: string);
begin
  try
    if ATableName = '' then
      raise Exception.Create('Table name required. You cant do "NOTIFY" process!!!"');

    with Self.NewQuery do
    try
      SQL.Text := 'notify ' + ATableName + ';';
      ExecSQL;
    finally
      Free;
    end;
    GLogger.RunLog('NOTIFYING ' + ATableName );
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.NewQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := FConnection;
end;

function TEntityManager.IsAuthorized(ATableSourceCode: string; APermissionType: TPermissionTypes; APermissionCheck, AShowException: Boolean): Boolean;
//var
//  LMessage: string;
//  LPermSourceCode: string;
//  n1: Integer;
begin
  Result := True;
  if not APermissionCheck then
    Result := True;
(*
    Result := False;
    LMessage := '';

    LPermSourceCode := ATableSourceCode;
    for n1 := 0 to GSysUserAccessRight.List.Count-1 do
    begin
      if  (TSysUserAccessRight(GSysUserAccessRight.List[n1]).SourceCode.AsString = LPermSourceCode) then
      begin
        if (APermissionType = ptRead) and (TSysUserAccessRight(GSysUserAccessRight.List[n1]).IsRead.AsBoolean) then
        begin
          Result := True;
          LMessage := 'SELECT';
          Break;
        end
        else if (APermissionType = ptAddRecord) and (TSysUserAccessRight(GSysUserAccessRight.List[n1]).IsAddRecord.AsBoolean) then
        begin
          Result := True;
          LMessage := 'INSERT';
          Break;
        end
        else if (APermissionType = ptUpdate) and (TSysUserAccessRight(GSysUserAccessRight.List[n1]).IsUpdate.AsBoolean) then
        begin
          Result := True;
          LMessage := 'UPDATE';
          Break;
        end
        else if (APermissionType = ptDelete) and (TSysUserAccessRight(GSysUserAccessRight.List[n1]).IsDelete.AsBoolean) then
        begin
          Result := True;
          LMessage := 'DELETE';
          Break;
        end
        else if (APermissionType = ptSpecial) and (TSysUserAccessRight(GSysUserAccessRight.List[n1]).IsSpecial.AsBoolean) then
        begin
          Result := True;
          LMessage := 'SPECIAL';
          Break;
        end
      end;
    end;

    LMessage := LMessage + ' ' + LPermSourceCode;

    if (not Result) and AShowException then
      raise Exception.Create(
        'İşlem ' + LMessage + AddLBs(2) +
        'Bu kaynağa erişim hakkınız yok! : ' + TableName + ' ' + ClassName + AddLBs +
        'Bu tablo için erişim kaynak kodu hatası: ' + TableName + ' ' + LPermSourceCode);
*)
end;

function TEntityManager.PrepareSelectQuery(ATable: TThsTable): string;
var
  AFieldDB: TThsField;
  LFields: string;
begin
  LFields := '';
  for AFieldDB in ATable.Fields do
    if fpSelect in AFieldDB.FieldIslemTipleri then
      LFields := LFields + AFieldDB.QryName + ',';
  Result := 'SELECT ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' FROM ' + IfThen(ATable.SchemaName = '', '', ATable.SchemaName + '.') + ATable.TableName;
end;

function TEntityManager.PrepareSelectCustomQuery(ATable: TThsTable; AFields: TArray<TThsField>): string;
var
  AFieldDB: TThsField;
  LFields: string;
begin
  LFields := '';
  for AFieldDB in AFields do
    if fpSelect in AFieldDB.FieldIslemTipleri then
      LFields := LFields + AFieldDB.QryName + ',';
  Result := 'SELECT ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' FROM ' + IfThen(ATable.SchemaName = '', '', ATable.SchemaName + '.') + ATable.TableName;
end;

function TEntityManager.PrepareInsertQuery(ATable: TThsTable): string;
var
  AFieldDB: TThsField;
  LFields, LValues: string;
begin
  LFields := '';
  LValues := '';
  for AFieldDB in ATable.Fields do
    if fpInsert in AFieldDB.FieldIslemTipleri then
    begin
      LFields := LFields + AFieldDB.FieldName + ',';
      if (AFieldDB.DataType = ftString)
      or (AFieldDB.DataType = ftWideString)
      or (AFieldDB.DataType = ftMemo)
      or (AFieldDB.DataType = ftWideMemo)
      or (AFieldDB.DataType = ftFmtMemo)
      or (AFieldDB.DataType = ftGuid)
      or (AFieldDB.DataType = ftFixedChar)
      or (AFieldDB.DataType = ftFixedWideChar)
      then
        LValues := LValues + QuotedStr(AFieldDB.AsString) + ','
      else
      if (AFieldDB.DataType = ftSmallint)
      or (AFieldDB.DataType = ftSmallint)
      or (AFieldDB.DataType = ftLargeint)
      or (AFieldDB.DataType = ftWord)
      or (AFieldDB.DataType = ftLongWord)
      or (AFieldDB.DataType = ftShortint)
      or (AFieldDB.DataType = ftByte)
      then
        LValues := LValues + AFieldDB.AsString + ','
      else if (AFieldDB.DataType = ftDate) then
        LValues := LValues + QuotedStr(DateToStr(AFieldDB.AsDate)) + ','
      else if (AFieldDB.DataType = ftTime) then
        LValues := LValues + QuotedStr(TimeToStr(AFieldDB.AsTime)) + ','
      else
      if (AFieldDB.DataType = ftDateTime)
      or (AFieldDB.DataType = ftTimeStamp)
      then
        LValues := LValues + QuotedStr(DateTimeToStr(AFieldDB.AsDateTime)) + ','
      else
      if (AFieldDB.DataType = ftFloat)
      or (AFieldDB.DataType = ftBCD)
      or (AFieldDB.DataType = ftFMTBcd)
      then
        LValues := LValues + FloatToStr(AFieldDB.AsFloat) + ','
    end;

  Result := 'INSERT INTO ' + ATable.TableName + '(' + LeftStr(Trim(LFields), Length(LFields)-1) + ')' +
              'VALUES (' + LeftStr(Trim(LValues), Length(LValues)-1) + ') RETURNING id;';
end;

function TEntityManager.PrepareUpdateQuery(ATable: TThsTable): string;
var
  AFieldDB: TThsField;
  LQryText, LFields: string;
begin
  LQryText := 'INSERT INTO ' + ATable.TableName;
  LFields := '';
  for AFieldDB in ATable.Fields do
    if fpUpdate in AFieldDB.FieldIslemTipleri then
    begin
      if (AFieldDB.DataType = ftString)
      or (AFieldDB.DataType = ftWideString)
      or (AFieldDB.DataType = ftMemo)
      or (AFieldDB.DataType = ftWideMemo)
      or (AFieldDB.DataType = ftFmtMemo)
      or (AFieldDB.DataType = ftGuid)
      or (AFieldDB.DataType = ftFixedChar)
      or (AFieldDB.DataType = ftFixedWideChar)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(AFieldDB.AsString) + ','
      else
      if (AFieldDB.DataType = ftSmallint)
      or (AFieldDB.DataType = ftSmallint)
      or (AFieldDB.DataType = ftLargeint)
      or (AFieldDB.DataType = ftWord)
      or (AFieldDB.DataType = ftLongWord)
      or (AFieldDB.DataType = ftShortint)
      or (AFieldDB.DataType = ftByte)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + AFieldDB.AsString + ','
      else if (AFieldDB.DataType = ftDate) then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(DateToStr(AFieldDB.AsDate)) + ','
      else if (AFieldDB.DataType = ftTime) then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(TimeToStr(AFieldDB.AsTime)) + ','
      else
      if (AFieldDB.DataType = ftDateTime)
      or (AFieldDB.DataType = ftTimeStamp)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(DateTimeToStr(AFieldDB.AsDateTime)) + ','
      else
      if (AFieldDB.DataType = ftFloat)
      or (AFieldDB.DataType = ftBCD)
      or (AFieldDB.DataType = ftFMTBcd)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + FloatToStr(AFieldDB.AsFloat) + ','
    end;

  Result := 'UPDATE ' + ATable.TableName + ' SET ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' WHERE id=' + ATable.Id.AsString + ';';
end;

function TEntityManager.PrepareUpdateCustomQuery(ATable: TThsTable; AFields: TArray<TThsField>): string;
var
  AFieldDB: TThsField;
  LQryText, LFields: string;
begin
  LQryText := 'INSERT INTO ' + ATable.TableName;
  LFields := '';
  for AFieldDB in AFields do
    if fpUpdate in AFieldDB.FieldIslemTipleri then
    begin
      if (AFieldDB.DataType = ftString)
      or (AFieldDB.DataType = ftWideString)
      or (AFieldDB.DataType = ftMemo)
      or (AFieldDB.DataType = ftWideMemo)
      or (AFieldDB.DataType = ftFmtMemo)
      or (AFieldDB.DataType = ftGuid)
      or (AFieldDB.DataType = ftFixedChar)
      or (AFieldDB.DataType = ftFixedWideChar)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(AFieldDB.AsString) + ','
      else
      if (AFieldDB.DataType = ftSmallint)
      or (AFieldDB.DataType = ftSmallint)
      or (AFieldDB.DataType = ftLargeint)
      or (AFieldDB.DataType = ftWord)
      or (AFieldDB.DataType = ftLongWord)
      or (AFieldDB.DataType = ftShortint)
      or (AFieldDB.DataType = ftByte)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + AFieldDB.AsString + ','
      else if (AFieldDB.DataType = ftDate) then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(DateToStr(AFieldDB.AsDate)) + ','
      else if (AFieldDB.DataType = ftTime) then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(TimeToStr(AFieldDB.AsTime)) + ','
      else
      if (AFieldDB.DataType = ftDateTime)
      or (AFieldDB.DataType = ftTimeStamp)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + QuotedStr(DateTimeToStr(AFieldDB.AsDateTime)) + ','
      else
      if (AFieldDB.DataType = ftFloat)
      or (AFieldDB.DataType = ftBCD)
      or (AFieldDB.DataType = ftFMTBcd)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + FloatToStr(AFieldDB.AsFloat) + ','
    end;

  Result := 'UPDATE ' + ATable.TableName + ' SET ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' WHERE id=' + ATable.Id.AsString + ';';
end;

function TEntityManager.PrepareDeleteQuery(ATable: TThsTable): string;
begin
  if ATable.Id.AsInt64 > 0 then
    Result := 'DELETE FROM ' + ATable.TableName + ' WHERE ' + ATable.Id.QryName + '=' + ATable.Id.AsString
  else
    Result := 'DELETE FROM ' + ATable.TableName + ' WHERE 1=1 ';
end;

function TEntityManager.Clone<T>(ASrc: T): T;
var
  AFieldSrc, AFieldDes: TThsField;
begin
  Result := CallCreateMethod<T>;
  for AFieldSrc in (ASrc as TThsTable).Fields do
  begin
    for AFieldDes in (Result as TThsTable).Fields do
    begin
      if  (AFieldSrc.FieldName = AFieldDes.FieldName)
      and (AFieldSrc.FieldName = AFieldDes.FieldName)
      then
      begin
        AFieldDes.Value := AFieldSrc.Value;
        Break;
      end;
    end;
  end;
end;

procedure TEntityManager.SetPostgresServerVariable(AVarName, AValue: string);
begin
  Connection.ExecuteDirect('SET ' + AVarName + '=' + QuotedStr(AValue));
end;

procedure TEntityManager.StartTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if not LConnection.InTransaction then
  begin
    LConnection.StartTransaction;
    GLogger.RunLog('START TRANSACTION');
  end;
end;

procedure TEntityManager.CommitTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if LConnection.InTransaction then
  begin
    LConnection.Commit;
    GLogger.RunLog('COMMIT TRANSACTION');
  end;
end;

procedure TEntityManager.RollbackTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if LConnection.InTransaction then
  begin
    LConnection.Rollback;
    GLogger.RunLog('ROLLBACK TRANSACTION');
  end;
end;

function TEntityManager.GetToday: TDateTime;
var
  LQry: TZQuery;
begin
  Result := 0;
  LQry := Self.NewQuery;
  try
    LQry.Close;
    LQry.SQL.Text := 'SELECT CURRENT_DATE;';
    LQry.Open;
    while NOT LQry.EOF do
    begin
      Result := LQry.Fields.Fields[0].AsDateTime;
      LQry.Next;
    end;
  finally
    LQry.DisposeOf;
  end;
end;

end.
