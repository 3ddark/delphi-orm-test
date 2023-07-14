unit Ths.Erp.Database.Manager;

interface

uses
  System.SysUtils, Classes, StrUtils, System.Variants, Data.DB,
  System.Rtti, System.Generics.Collections,
  ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZAbstractConnection, ZConnection,
  Ths.Erp.Database.Table;

type
  TEntityManager = class;
  TPermissionType = (ptRead, ptAddRecord, ptUpdate, ptDelete, ptSpecial);

  TBusinessSelectEvent = procedure(AManager: TEntityManager; AFilter: string; ALock, APermissionCheck: Boolean) of Object;
  TBusinessOperationEvent = procedure(APermissionCheck: Boolean) of Object;

  TEntityManager = class
  private
    FConnection: TZAbstractConnection;

    function NewQuery: TZQuery;

    function CallCreateMethod(AClass: TClass): TTable; overload;

    function PrepareSelectQuery(ATable: TTable): string;
    function PrepareSelectCustomQuery(ATable: TTable; AFields: TArray<TFieldDB>): string;
    function PrepareInsertQuery(ATable: TTable): string;
    function PrepareUpdateQuery(ATable: TTable): string;
    function PrepareUpdateCustomQuery(ATable: TTable; AFields: TArray<TFieldDB>): string;
    function PrepareDeleteQuery(ATable: TTable): string;

    function GetOneBase(ATable: TTable; AFilter: string; ALock: Boolean): Boolean;
    function GetOneCustomBase(const ATable: TTable; AFields: TArray<TFieldDB>; AFilter: string; ALock: Boolean): Boolean;
  public
    property Connection: TZAbstractConnection read FConnection;

    constructor Create(AConnection: TZAbstractConnection); virtual;
    destructor Destroy; override;

    function GetList(AClass: TClass; var AList: TArray<TTable>; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean;
    function GetListCustom(AClass: TClass; var AList: TArray<TTable>; AFields: TArray<TFieldDB>; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean;

    function GetOne(ATable: TTable; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;
    function GetOneCustom(ATable: TTable; AFields: TArray<TFieldDB>; AFilter: string; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;
    function GetOne(ATable: TTable; AID: Int64; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;
    function GetOneCustom(ATable: TTable; AFields: TArray<TFieldDB>; AID: Int64; ALock: Boolean; APermissionCheck: Boolean=True): Boolean; overload;

    function Insert(ATable: TTable; APermissionCheck: Boolean=True): Boolean; virtual;
    function DoInsert(ATable: TTable; APermissionCheck: Boolean=True): Boolean;
    function BeforeInsertDB(ATable: TTable): Boolean; virtual;
    function AfterInsertDB(ATable: TTable): Boolean; virtual;

    function Update(ATable: TTable; APermissionCheck: Boolean=True): Boolean; virtual;
    function DoUpdate(ATable: TTable; APermissionCheck: Boolean): Boolean;
    function BeforeUpdateDB(ATable: TTable): Boolean; virtual;
    function AfterUpdateDB(ATable: TTable): Boolean; virtual;

    function CustomUpdate(ATable: TTable; AFields: TArray<TFieldDB>; APermissionCheck: Boolean=True): Boolean; virtual;
    function DoCustomUpdate(ATable: TTable; AFields: TArray<TFieldDB>; APermissionCheck: Boolean): Boolean;
    function BeforeCustomUpdateDB(ATable: TTable): Boolean; virtual;
    function AfterCustomUpdateDB(ATable: TTable): Boolean; virtual;

    function DeleteBatch(ATables: TArray<TTable>; APermissionCheck: Boolean=True): Boolean; virtual;
    function Delete(ATable: TTable; APermissionCheck: Boolean=True): Boolean; virtual;
    function DoDelete(ATable: TTable; APermissionCheck: Boolean): Boolean; virtual;
    function BeforeDeleteDB(ATable: TTable): Boolean; virtual;
    function AfterDeleteDB(ATable: TTable): Boolean; virtual;

    function LogicalSelect(AFilter: string; ALock, AWithBegin, APermissionCheck: Boolean; AProcBusinessSelect: TBusinessSelectEvent): Boolean; virtual;
    function LogicalInsert(AWithBegin, AWithCommit, APermissionCheck: Boolean):Boolean; virtual;
    function LogicalUpdate(AWithCommit, APermissionCheck: Boolean):Boolean; virtual;
    function LogicalDelete(AWithCommit, APermissionCheck: Boolean):Boolean; virtual;

    procedure Listen(ATableName: string); virtual;
    procedure Unlisten(ATableName: string); virtual;
    procedure Notify(ATableName: string); virtual;

    function IsAuthorized(ATableSourceCode: string; APermissionType: TPermissionType; APermissionCheck: Boolean; AShowException: Boolean=True): Boolean;

    procedure StartTrans(AConnection: TZAbstractConnection = nil);
    procedure CommitTrans(AConnection: TZAbstractConnection = nil);
    procedure RollbackTrans(AConnection: TZAbstractConnection = nil);
  end;

implementation

uses Logger, Persons;

function TEntityManager.CallCreateMethod(AClass: TClass): TTable;
var
  AModel: TObject;

  rC: TRttiContext;
  rT: TRttiType;
  rM: TRttiMethod;
  rPrms: TArray<TRttiParameter>;
begin
  Result := nil;
  rT := rC.GetType(AClass);
  rM := rT.GetMethod('Create');
  rPrms := rM.GetParameters;
  if rM.IsConstructor then
  begin
    AModel := rM.Invoke(rT.AsInstance.MetaclassType, []).AsObject;
    Result := TTable(AModel);
  end;
end;

constructor TEntityManager.Create(AConnection: TZAbstractConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection required');

  FConnection := AConnection;

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
  FConnection := nil;
  inherited;
end;

function TEntityManager.GetList(AClass: TClass; var AList: TArray<TTable>; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  n1: Integer;
  ATable: TTable;
  AFieldDB: TFieldDB;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  try
    ATable := CallCreateMethod(AClass) as TTable;

    if not Self.IsAuthorized(ATable.TableSourceCode, ptRead, APermissionCheck) then
    begin
      ATable.DisposeOf;
      Exit;
    end;

    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := Self.PrepareSelectQuery(ATable) + ' WHERE ' + AFilter;
      ATable.DisposeOf;
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        if LQry.RecordCount > 0 then
          Result := True;
        n1 := 0;
        SetLength(AList, n1);
        LQry.First;
        while not LQry.Eof do
        begin
          ATable := CallCreateMethod(AClass) as TTable;
          for AFieldDB in (ATable).Fields do
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

          Inc(n1);
          SetLength(AList, n1);
          AList[n1-1] := ATable;

          LQry.Next;
        end;
      end;
    finally
      LQry.DisposeOf;
    end;
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetListCustom(AClass: TClass; var AList: TArray<TTable>; AFields: TArray<TFieldDB>; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
var
  n1: Integer;
  ATable: TTable;
  AFieldDB: TFieldDB;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  try
    ATable := CallCreateMethod(AClass) as TTable;

    if not Self.IsAuthorized(ATable.TableSourceCode, ptRead, APermissionCheck) then
    begin
      ATable.DisposeOf;
      Exit;
    end;

    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := Self.PrepareSelectCustomQuery(ATable, AFields) + ' WHERE ' + AFilter;
      ATable.DisposeOf;
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        if LQry.RecordCount > 0 then
          Result := True;
        n1 := 0;
        SetLength(AList, n1);
        LQry.First;
        while not LQry.Eof do
        begin
          ATable := CallCreateMethod(AClass) as TTable;
          for AFieldDB in (ATable).Fields do
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

          Inc(n1);
          SetLength(AList, n1);
          AList[n1-1] := ATable;

          LQry.Next;
        end;
      end;
    finally
      LQry.DisposeOf;
    end;
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOne(ATable: TTable; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
begin
  Result := Self.IsAuthorized(ATable.TableSourceCode, ptRead, APermissionCheck);
  if not Result then
    Exit;

  Result := GetOneBase(ATable, AFilter, ALock);
end;

function TEntityManager.GetOneCustom(ATable: TTable; AFields: TArray<TFieldDB>; AFilter: string; ALock, APermissionCheck: Boolean): Boolean;
begin
  Result := Self.IsAuthorized(ATable.TableSourceCode, ptRead, APermissionCheck);
  if not Result then
    Exit;

  Result := GetOneCustomBase(ATable, AFields, AFilter, ALock);
end;

function TEntityManager.GetOne(ATable: TTable; AID: Int64; ALock: Boolean; APermissionCheck: Boolean): Boolean;
begin
  Result := Self.IsAuthorized(ATable.TableSourceCode, ptRead, APermissionCheck);
  if not Result then
    Exit;

  Result := GetOneBase(ATable, 'id=' + AID.ToString, ALock)
end;

function TEntityManager.GetOneCustom(ATable: TTable; AFields: TArray<TFieldDB>; AID: Int64; ALock, APermissionCheck: Boolean): Boolean;
begin
  Result := Self.IsAuthorized(ATable.TableSourceCode, ptRead, APermissionCheck);
  if not Result then
    Exit;

  Result := GetOneCustomBase(ATable, AFields, 'id=' + AID.ToString, ALock);
end;

function TEntityManager.GetOneBase(ATable: TTable; AFilter: string; ALock: Boolean): Boolean;
var
  AFieldDB: TFieldDB;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  try
    try
      if (ATable.TableName = '') then
        Exit;
    except
      raise Exception.Create('GetOne ile doldurulacak olan ATable parametresi tanýmlý olmak zorunda!!!');
    end;

    LQry := Self.NewQuery;
    try
      if AFilter = '' then
        AFilter := '1=1';
      LQry.SQL.Text := Self.PrepareSelectQuery((ATable)) + ' WHERE ' + AFilter;
      LQry.SQL.Text := LQry.SQL.Text + IfThen(ALock, ' FOR UPDATE OF ' + ATable.TableName + ' NOWAIT;', ';');

      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
        if LQry.RecordCount > 1 then
          raise Exception.Create('Verilen parametre ile birden fazla kayýt bulundu!!!');

        if LQry.RecordCount = 1 then
          Result := True;
        for AFieldDB in (ATable).Fields do
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
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.GetOneCustomBase(const ATable: TTable; AFields: TArray<TFieldDB>; AFilter: string; ALock: Boolean): Boolean;
var
  AFieldDB: TFieldDB;
  AField: TField;
  LQry: TZQuery;
begin
  Result := False;
  try
    try
      if (ATable.TableName = '') then
        Exit;
    except
      raise Exception.Create('GetOne ile doldurulacak olan ATable parametresi tanýmlý olmak zorunda!!!');
    end;

    LQry := Self.NewQuery;
    try
      if AFilter = '' then
        AFilter := '1=1';
      LQry.SQL.Text := Self.PrepareSelectCustomQuery(ATable, AFields) + ' WHERE ' + AFilter;
      LQry.SQL.Text := LQry.SQL.Text + IfThen(ALock, ' FOR UPDATE OF ' + ATable.TableName + ' NOWAIT;', ';');

      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
        if LQry.RecordCount > 1 then
          raise Exception.Create('Verilen parametre ile birden fazla kayýt bulundu!!!');

        if LQry.RecordCount = 1 then
          Result := True;
        for AFieldDB in (ATable).Fields do
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
  except
    on E: Exception do
    begin
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.BeforeInsertDB(ATable: TTable): Boolean;
begin
  Result := ATable.Validate;
end;

function TEntityManager.Insert(ATable: TTable; APermissionCheck: Boolean): Boolean;
begin
  Result := IsAuthorized(ATable.TableSourceCode, ptAddRecord, APermissionCheck);
  if not Result then
    Exit;

  if BeforeInsertDB(ATable) then
    if DoInsert(ATable, APermissionCheck) then
      AfterInsertDB(ATable);
end;

function TEntityManager.DoInsert(ATable: TTable; APermissionCheck: Boolean): Boolean;
var
  LQry: TZQuery;
begin
  Result := True;
  try
    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := PrepareInsertQuery(ATable);
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.Open;
        GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
        ATable.Id.Value := LQry.Fields.Fields[0].Value;
        LQry.Close;
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

function TEntityManager.AfterInsertDB(ATable: TTable): Boolean;
begin
  GLogger.RunLog('INSERTING ' + ATable.ClassName + ' id: ' + IntToStr(ATable.Id.AsInteger));
  Result := True;
end;

function TEntityManager.BeforeUpdateDB(ATable: TTable): Boolean;
begin
  Result := ATable.Validate;
end;

function TEntityManager.Update(ATable: TTable; APermissionCheck: Boolean): Boolean;
begin
  Result := IsAuthorized(ATable.TableSourceCode, ptUpdate, APermissionCheck);
  if not Result then
    Exit;

  if BeforeUpdateDB(ATable) then
    if DoUpdate(ATable, APermissionCheck) then
      AfterUpdateDB(ATable);
end;

function TEntityManager.DoUpdate(ATable: TTable; APermissionCheck: Boolean): Boolean;
var
  LQry: TZQuery;
begin
  Result := True;
  try
    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := PrepareUpdateQuery(ATable);
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.ExecSQL;
        GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
        LQry.Close;
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

function TEntityManager.AfterUpdateDB(ATable: TTable): Boolean;
begin
  GLogger.RunLog('UPDATING ' + ATable.ClassName + ' id: ' + IntToStr(ATable.Id.AsInteger));
  Result := True;
end;

function TEntityManager.BeforeCustomUpdateDB(ATable: TTable): Boolean;
begin
  Result := ATable.Validate;
end;

function TEntityManager.CustomUpdate(ATable: TTable; AFields: TArray<TFieldDB>; APermissionCheck: Boolean): Boolean;
begin
  Result := IsAuthorized(ATable.TableSourceCode, ptUpdate, APermissionCheck);
  if not Result then
    Exit;

  if BeforeCustomUpdateDB(ATable) then
    if DoCustomUpdate(ATable, AFields, APermissionCheck) then
      AfterCUstomUpdateDB(ATable);
end;

function TEntityManager.DoCustomUpdate(ATable: TTable; AFields: TArray<TFieldDB>; APermissionCheck: Boolean): Boolean;
var
  LQry: TZQuery;
begin
  Result := True;
  try
    LQry := Self.NewQuery;
    try
      LQry.SQL.Text := PrepareUpdateCustomQuery(ATable, AFields);
      LQry.Prepare;
      if LQry.Prepared then
      begin
        LQry.ExecSQL;
        GLogger.RunLog(LQry.SQL.Text.Replace(sLineBreak, ''));
        LQry.Close;
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

function TEntityManager.AfterCustomUpdateDB(ATable: TTable): Boolean;
begin
  GLogger.RunLog('CUSTOM UPDATING ' + ATable.ClassName + ' id: ' + IntToStr(ATable.Id.AsInteger));
  Result := True;
end;

function TEntityManager.BeforeDeleteDB(ATable: TTable): Boolean;
begin
  Result := True;
end;

function TEntityManager.Delete(ATable: TTable; APermissionCheck: Boolean): Boolean;
begin
  Result := IsAuthorized(ATable.TableSourceCode, ptDelete, APermissionCheck);
  if not Result then
    Exit;

  if BeforeDeleteDB(ATable) then
    if DoDelete(ATable, APermissionCheck) then
      AfterDeleteDB(ATable);
end;

function TEntityManager.DeleteBatch(ATables: TArray<TTable>; APermissionCheck: Boolean): Boolean;
var
  ATable: TTable;
  LPermissionCheck: Boolean;
begin
  LPermissionCheck := APermissionCheck;
  for ATable in ATables do
  begin
    Self.Delete(ATable, LPermissionCheck);
    if APermissionCheck then
      LPermissionCheck := False;
  end;
  Result := True;
end;

function TEntityManager.DoDelete(ATable: TTable; APermissionCheck: Boolean): Boolean;
begin
  with Self.NewQuery do
  try
    SQL.Text := PrepareDeleteQuery(ATable);
    ExecSQL;
    GLogger.RunLog(SQL.Text.Replace(sLineBreak, ''));
    Result := True;
  finally
    Free;
  end;
end;

function TEntityManager.AfterDeleteDB(ATable: TTable): Boolean;
begin
  GLogger.RunLog('DELETING ' + ATable.ClassName + ' id: ' + IntToStr(ATable.Id.AsInteger));
  Result := True;
end;

function TEntityManager.LogicalSelect(AFilter: string; ALock, AWithBegin, APermissionCheck: Boolean; AProcBusinessSelect: TBusinessSelectEvent): Boolean;
begin
  Result := False;
  try
    if not Assigned(AProcBusinessSelect) then
      raise Exception.Create('BusinessSelect event olmak zorunda!!!');

    if not ALock then
      AWithBegin := False;

    if AWithBegin then
      StartTrans;

    AProcBusinessSelect(Self, AFilter, ALock, APermissionCheck);

    Result := True;
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalInsert(AWithBegin, AWithCommit, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    if AWithBegin then StartTrans;

    //

    if AWithCommit then CommitTrans;
    Result := True;
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalUpdate(AWithCommit, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    //

    if AWithCommit then CommitTrans;
    Result := True;
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

function TEntityManager.LogicalDelete(AWithCommit, APermissionCheck: Boolean): Boolean;
begin
  Result := False;
  try
    //

    if AWithCommit then CommitTrans;
    Result := True;
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
      GLogger.ErrorLog(E);
    end;
  end;
end;

procedure TEntityManager.Listen(ATableName: string);
begin
  try
    if ATableName = '' then
      raise Exception.Create('Tablo adý olmak zorunda "LISTEN iþlemini yapamazsýn!!!"');

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
      raise Exception.Create('Tablo adý olmak zorunda "UNLISTEN iþlemini yapamazsýn!!!"');

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
      raise Exception.Create('Tablo adý olmak zorunda "NOTIFY iþlemini yapamazsýn!!!"');

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

function TEntityManager.IsAuthorized(ATableSourceCode: string; APermissionType: TPermissionType; APermissionCheck, AShowException: Boolean): Boolean;
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
        'Ýþlem ' + LMessage + AddLBs(2) +
        'Bu kaynaða eriþim hakkýnýz yok! : ' + TableName + ' ' + ClassName + AddLBs +
        'Bu tablo için eriþim kaynak kodu hatasý: ' + TableName + ' ' + LPermSourceCode);
*)
end;

function TEntityManager.PrepareSelectQuery(ATable: TTable): string;
var
  AFieldDB: TFieldDB;
  LFields: string;
begin
  LFields := '';
  for AFieldDB in ATable.Fields do
    if fpSelect in AFieldDB.FieldIslemTipleri then
      LFields := LFields + AFieldDB.FieldName + ',';
  Result := 'SELECT ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' FROM ' + ATable.TableName;
end;

function TEntityManager.PrepareSelectCustomQuery(ATable: TTable; AFields: TArray<TFieldDB>): string;
var
  AFieldDB: TFieldDB;
  LFields: string;
begin
  LFields := '';
  for AFieldDB in AFields do
    if fpSelect in AFieldDB.FieldIslemTipleri then
      LFields := LFields + AFieldDB.FieldName + ',';
  Result := 'SELECT ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' FROM ' + ATable.TableName;
end;

function TEntityManager.PrepareInsertQuery(ATable: TTable): string;
var
  AFieldDB: TFieldDB;
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
        LValues := LValues + DateToStr(AFieldDB.AsDate) + ','
      else if (AFieldDB.DataType = ftTime) then
        LValues := LValues + TimeToStr(AFieldDB.AsTime) + ','
      else
      if (AFieldDB.DataType = ftDateTime)
      or (AFieldDB.DataType = ftTimeStamp)
      then
        LValues := LValues + DateTimeToStr(AFieldDB.AsDateTime) + ','
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

function TEntityManager.PrepareUpdateQuery(ATable: TTable): string;
var
  AFieldDB: TFieldDB;
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
        LFields := LFields + AFieldDB.FieldName + '=' + DateToStr(AFieldDB.AsDate) + ','
      else if (AFieldDB.DataType = ftTime) then
        LFields := LFields + AFieldDB.FieldName + '=' + TimeToStr(AFieldDB.AsTime) + ','
      else
      if (AFieldDB.DataType = ftDateTime)
      or (AFieldDB.DataType = ftTimeStamp)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + DateTimeToStr(AFieldDB.AsDateTime) + ','
      else
      if (AFieldDB.DataType = ftFloat)
      or (AFieldDB.DataType = ftBCD)
      or (AFieldDB.DataType = ftFMTBcd)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + FloatToStr(AFieldDB.AsFloat) + ','
    end;

  Result := 'UPDATE ' + ATable.TableName + ' SET ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' WHERE id=' + ATable.Id.AsString + ';';
end;

function TEntityManager.PrepareUpdateCustomQuery(ATable: TTable; AFields: TArray<TFieldDB>): string;
var
  AFieldDB: TFieldDB;
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
        LFields := LFields + AFieldDB.FieldName + '=' + DateToStr(AFieldDB.AsDate) + ','
      else if (AFieldDB.DataType = ftTime) then
        LFields := LFields + AFieldDB.FieldName + '=' + TimeToStr(AFieldDB.AsTime) + ','
      else
      if (AFieldDB.DataType = ftDateTime)
      or (AFieldDB.DataType = ftTimeStamp)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + DateTimeToStr(AFieldDB.AsDateTime) + ','
      else
      if (AFieldDB.DataType = ftFloat)
      or (AFieldDB.DataType = ftBCD)
      or (AFieldDB.DataType = ftFMTBcd)
      then
        LFields := LFields + AFieldDB.FieldName + '=' + FloatToStr(AFieldDB.AsFloat) + ','
    end;

  Result := 'UPDATE ' + ATable.TableName + ' SET ' + LeftStr(Trim(LFields), Length(LFields)-1) + ' WHERE id=' + ATable.Id.AsString + ';';
end;

function TEntityManager.PrepareDeleteQuery(ATable: TTable): string;
begin
  Result := 'DELETE FROM ' + ATable.TableName + ' WHERE id=' + ATable.Id.AsString + ';';
end;

procedure TEntityManager.StartTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if not LConnection.InTransaction then
    LConnection.StartTransaction;
end;

procedure TEntityManager.CommitTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if not LConnection.InTransaction then
    LConnection.StartTransaction;
end;

procedure TEntityManager.RollbackTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if not LConnection.InTransaction then
    LConnection.StartTransaction;
end;

end.
