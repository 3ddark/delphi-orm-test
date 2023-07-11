unit EntityManager;

interface

uses
  SysUtils, StrUtils, Generics.Collections, System.TypInfo, Rtti,
  ZAbstractConnection, ZAbstractDataset, ZDataset, Entity,
  Data.DB, EntityAttributes;

type
  TEntityManager = class
  private
    FConnection: TZAbstractConnection;
    
    function MethodCall(AObject: TObject; AMethodName: string; AParameters: array of TValue): TValue; overload;
    function MethodCall(AClass: TClass; AMethodName: string; AParameters: array of TValue): TValue; overload;

    function GetSQLWithFilter(AValue: TValue; ARttiType: TRttiType): string;
    function GetRelations(ARttiType: TRttiType): TArray<TRttiProperty>;

    procedure SetModelValueFromQuery(AModel: TObject; ARttiType: TRttiType; ASQL: string); overload;
    procedure SetListModelValueFromQuery(AList: TObject; ARttiType: TRttiType; ASQL: string); overload;
  protected
    function Connection: TZAbstractConnection;
    function GetTableName(AClass: TClass): string;


    function GetByOne(ATypeInfo: PTypeInfo; AValue: TValue; ALock: Boolean = False; Model: TObject = nil): TObject; overload;
    procedure GetList<T>(AObject: TObject; AFilter: string; ALock: Boolean); overload;
    procedure GetList(AObject: TObject; AClass: TClass; AFilter: string; ALock: Boolean; ALazyLoading: Boolean = False); overload;

    procedure AddBatch(AModels: TObject); overload;
    procedure UpdateBatch(AModels: TObject); overload;
  public
    function GetByOne<T: class>(AValue: TValue; ALock: Boolean = False): T; overload;
    function GetList<T: class>(AFilter: string = ''; ALock: Boolean = False): TObjectList<T>; overload;

    procedure Add<T: class>(AModel: T); overload;
    procedure AddBatch<T: class>(AModels: TArray<T>); overload;

    procedure Update(AModel: TObject);
    procedure UpdateBatch<T: class>(AModels: TArray<T>); overload;

    procedure Delete<T>(AID: Int64); overload;
    procedure Delete<T>(AModel: T); overload;
    procedure DeleteBatch<T: class>(AModels: TArray<T>); reintroduce; overload;
    procedure DeleteBatch<T>(AIDs: TArray<Int64>); reintroduce; overload;
    procedure DeleteBatch<T>(AFilter: string = ''); reintroduce; overload;

    function Clone<T>(ASource: T): T;

    procedure StartTrans(AConnection: TZAbstractConnection = nil);
    procedure CommitTrans(AConnection: TZAbstractConnection = nil);
    procedure RollbackTrans(AConnection: TZAbstractConnection = nil);

    function LogicalGet<T: class>(AFilter: string = ''; ALock: Boolean = False; AWithBegin: Boolean = False): TObjectList<T>;
    procedure LogicalAdd<T: class>(AModels: TArray<T>; AWithBegin: Boolean = False; AWithCommit: Boolean = False);
    procedure LogicalUpdate<T: class>(AModels: TArray<T>; AWithCommit: Boolean = False);
    procedure LogicalDelete<T: class>(AIDs: TArray<Int64>; AWithCommit: Boolean = False);

    constructor Create(AConnection: TZAbstractConnection);
  end;

implementation

uses
  System.Classes;

constructor TEntityManager.Create(AConnection: TZAbstractConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection Required');

  FConnection := AConnection;
end;

function TEntityManager.Connection: TZAbstractConnection;
begin
  Result := FConnection;
end;

function TEntityManager.GetTableName(AClass: TClass): string;
var
  ACtx: TRttiContext;
  AType: TRttiType;
  AAttr : TCustomAttribute;
begin
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

function TEntityManager.GetRelations(ARttiType: TRttiType): TArray<TRttiProperty>;
var
  rP: TRttiProperty;
  rA: TCustomAttribute;
begin
  SetLength(Result, 0);
  for rP in ARttiType.GetProperties do
  begin
    if rP.IsReadable and rP.IsWritable and (rP.Visibility in [mvPublished, mvPublic]) then
    begin
      for rA in rP.GetAttributes do
      begin
        if (rP.PropertyType.TypeKind = tkClass) and (rA is HasOne) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[Length(Result)-1] := rP;
        end
        else if (rP.PropertyType.TypeKind = tkClass) and (rA is HasMany) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[Length(Result)-1] := rP;
        end;
      end;
    end;
  end;
end;

function TEntityManager.GetSQLWithFilter(AValue: TValue; ARttiType: TRttiType): string;
var
  rP: TRttiProperty;
  rA: TCustomAttribute;
  LTableName, LColName, LColNames, LColID: string;
begin
  LTableName := Self.GetTableName(ARttiType.AsInstance.MetaclassType);
  if LTableName = '' then Exit;

  LColNames := '';
  for rP in ARttiType.GetProperties do
  begin
    if rP.IsReadable and rP.IsWritable and (rP.Visibility in [mvPublished, mvPublic]) then
    begin
      for rA in rP.GetAttributes do
      begin
        if rA is Column then
        begin
          LColName := Column(rA).Name;
          if LColName = '' then
            raise Exception.Create('Column Attribute must be declared.' + sLineBreak + 'If it is not used use "NotMapped" attribute!!!');
          if LColName = 'id' then
            LColID := LColName
          else
            LColNames := LColNames + LColName + ',';
        end;
      end;
    end;
  end;

  if LColNames = '' then
    raise Exception.Create('field_name not found for SQL sorugusu için field_name bilgilerine ulaþýlamadý!!!');

  LColNames := LColID + ',' + LColNames;

  Result := Trim('SELECT ' + LeftStr(LColNames, Length(LColNames)-1) + ' FROM ' + LTableName + ' WHERE 1=1 ' + AValue.AsString);
end;

function TEntityManager.MethodCall(AClass: TClass; AMethodName: string; AParameters: array of TValue): TValue;
var
  ctx: TRttiContext;
  m: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    m := ctx.GetType(AClass).GetMethod(AMethodName);
    if Assigned(m) then
      Result := m.Invoke(AClass, AParameters)
    else
      raise Exception.Create(Format('Cannot find method "%s" in the object', [AMethodName]));
  finally
    ctx.Free;
  end;
end;

function TEntityManager.MethodCall(AObject: TObject; AMethodName: string; AParameters: array of TValue): TValue;
var
  ctx: TRttiContext;
  m: TRttiMethod;
begin
  ctx := TRttiContext.Create;
  try
    if Assigned(AObject) then
      m := ctx.GetType(AObject.ClassType).GetMethod(AMethodName)
    else
      raise Exception.Create('Object is not defined');

    if Assigned(m) then
      Result := m.Invoke(AObject, AParameters)
    else
      raise Exception.Create(Format('Cannot find method "%s" in the object', [AMethodName]));
  finally
    ctx.Free;
  end;
end;

procedure TEntityManager.SetModelValueFromQuery(AModel: TObject; ARttiType: TRttiType; ASQL: string);
var
  LCmd: TZQuery;
  AField: TField;
  rP: TRttiProperty;
  rA: TCustomAttribute;
begin
  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := ASQL;
    LCmd.Prepare;

    if LCmd.Prepared then
    begin
      LCmd.Open;
      while not LCmd.Eof do
      begin
        for rP in ARttiType.GetProperties do
        begin
          if rP.IsReadable and rP.IsWritable and (rP.Visibility in [mvPublic, mvPublished]) then
          begin
            for rA in rP.GetAttributes do
            begin
              if (rA is Column) then
              begin
                for AField in LCmd.Fields do
                begin
                  if Column(rA).Name = AField.FieldName then
                  begin
                    case AField.DataType of
                      ftUnknown: ;
                      ftString        : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftSmallint      : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftInteger       : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftWord          : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftBoolean: ;
                      ftFloat: ;
                      ftCurrency: ;
                      ftBCD: ;
                      ftDate: ;
                      ftTime: ;
                      ftDateTime: ;
                      ftBytes: ;
                      ftVarBytes: ;
                      ftAutoInc: ;
                      ftBlob: ;
                      ftMemo          : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftGraphic: ;
                      ftFmtMemo       : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftParadoxOle: ;
                      ftDBaseOle: ;
                      ftTypedBinary: ;
                      ftCursor: ;
                      ftFixedChar     : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftWideString    : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftLargeint      : rP.SetValue(AModel, TValue.From(AField.AsLargeInt));
                      ftADT: ;
                      ftArray: ;
                      ftReference: ;
                      ftDataSet: ;
                      ftOraBlob: ;
                      ftOraClob: ;
                      ftVariant       : rP.SetValue(AModel, TValue.From(AField.Value));
                      ftInterface: ;
                      ftIDispatch: ;
                      ftGuid: ;
                      ftTimeStamp: ;
                      ftFMTBcd: ;
                      ftFixedWideChar: ;
                      ftWideMemo      : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftOraTimeStamp: ;
                      ftOraInterval: ;
                      ftLongWord      : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftShortint      : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftByte          : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftExtended: ;
                      ftConnection: ;
                      ftParams: ;
                      ftStream: ;
                      ftTimeStampOffset: ;
                      ftObject: ;
                      ftSingle: ;
                    end;
                    Break;
                  end;
                end;
              end;
            end;

          end;
        end;

        LCmd.Next;
      end;
    end;
  finally
    LCmd.Free;
  end;
end;

procedure TEntityManager.SetListModelValueFromQuery(AList: TObject; ARttiType: TRttiType; ASQL: string);
var
  LCmd: TZQuery;
  AField: TField;
  rP: TRttiProperty;
  rA: TCustomAttribute;
  AModel: TObject;
begin
  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := ASQL;
    LCmd.Prepare;

    if LCmd.Prepared then
    begin
      LCmd.Open;
      while not LCmd.Eof do
      begin
        AModel := MethodCall(ARttiType.AsInstance.MetaclassType, 'Create', []).AsObject;
        for rP in ARttiType.GetProperties do
        begin
          if rP.IsReadable and rP.IsWritable and (rP.Visibility in [mvPublic, mvPublished]) then
          begin
            for rA in rP.GetAttributes do
            begin
              if (rA is Column) then
              begin
                for AField in LCmd.Fields do
                begin
                  if Column(rA).Name = AField.FieldName then
                  begin
                    case AField.DataType of
                      ftUnknown: ;
                      ftString        : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftSmallint      : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftInteger       : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftWord          : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftBoolean: ;
                      ftFloat: ;
                      ftCurrency: ;
                      ftBCD: ;
                      ftDate: ;
                      ftTime: ;
                      ftDateTime: ;
                      ftBytes: ;
                      ftVarBytes: ;
                      ftAutoInc: ;
                      ftBlob: ;
                      ftMemo          : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftGraphic: ;
                      ftFmtMemo       : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftParadoxOle: ;
                      ftDBaseOle: ;
                      ftTypedBinary: ;
                      ftCursor: ;
                      ftFixedChar     : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftWideString    : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftLargeint      : rP.SetValue(AModel, TValue.From(AField.AsLargeInt));
                      ftADT: ;
                      ftArray: ;
                      ftReference: ;
                      ftDataSet: ;
                      ftOraBlob: ;
                      ftOraClob: ;
                      ftVariant       : rP.SetValue(AModel, TValue.From(AField.Value));
                      ftInterface: ;
                      ftIDispatch: ;
                      ftGuid: ;
                      ftTimeStamp: ;
                      ftFMTBcd: ;
                      ftFixedWideChar: ;
                      ftWideMemo      : rP.SetValue(AModel, TValue.From(AField.AsString));
                      ftOraTimeStamp: ;
                      ftOraInterval: ;
                      ftLongWord      : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftShortint      : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftByte          : rP.SetValue(AModel, TValue.From(AField.AsInteger));
                      ftExtended: ;
                      ftConnection: ;
                      ftParams: ;
                      ftStream: ;
                      ftTimeStampOffset: ;
                      ftObject: ;
                      ftSingle: ;
                    end;
                    Break;
                  end;
                end;
              end;
            end;

          end;
        end;

        MethodCall(AList, 'Add', [AModel]);

        LCmd.Next;
      end;
    end;
  finally
    LCmd.Free;
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
    LConnection.Commit;
end;

procedure TEntityManager.RollbackTrans(AConnection: TZAbstractConnection);
var
  LConnection: TZAbstractConnection;
begin
  LConnection := Connection;
  if Assigned(AConnection) then
    LConnection := AConnection;
  if LConnection.InTransaction then
    LConnection.Rollback;
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

function TEntityManager.GetByOne<T>(AValue: TValue; ALock: Boolean): T;
begin
  Result := T(GetByOne(TypeInfo(T), AValue, ALock));
end;

function TEntityManager.GetByOne(ATypeInfo: PTypeInfo; AValue: TValue; ALock: Boolean; Model: TObject): TObject;
var
  rC: TRttiContext;
  rPck: TRttiPackage;
  rT, rT2: TRttiType;
  rP, rPFilter, rPValue: TRttiProperty;
  rPs: TArray<TRttiProperty>;
  LValue: TValue;
  rA, rA2: TCustomAttribute;
  AModel, AModel2: TObject;
  LFilter, LFilterValue, LTypeName, LQry: string;
  StartBracetPos, EndBracetPos: Integer;
begin
  rC := TRttiContext.Create;
  try
    rT := rC.GetType(ATypeInfo);

    if Assigned(Model) then
      Result := Model
    else
      Result := MethodCall(rT.AsInstance.MetaclassType, 'Create', []).AsObject;

    if AValue.IsType<Int64>(False) then
      LQry := GetSQLWithFilter(TValue.From<string>(' and id=' + AValue.AsInt64.ToString), rT)
    else if AValue.IsType<string>(False) then
      LQry := GetSQLWithFilter(TValue.From<string>(AValue.AsString), rT);

    SetModelValueFromQuery(Result, rT, LQry);

    rT := rC.GetType(Result.ClassType);
    rPs := GetRelations(rT);

    for rP in rPs do
    begin
      if  (RightStr(rP.PropertyType.QualifiedName.Split(['<'])[0], 5) = 'TList')
      and (rP.PropertyType.TypeKind = tkClass)
      then
      begin
        for rA in rP.GetAttributes do
        begin
          if (rA is HasMany) then
          begin
            rPValue := rT.GetProperty((rA as HasMany).ValuePropertyName);
            LFilterValue := rPValue.GetValue(Result).AsVariant;

            rPck := rC.GetPackages[0];
            StartBracetPos := Pos('<', rP.PropertyType.QualifiedName)+1;
            EndBracetPos := Pos('>', rP.PropertyType.QualifiedName);
            LTypeName := Copy(rP.PropertyType.QualifiedName, StartBracetPos, EndBracetPos-StartBracetPos);
            rT2 := rPck.FindType(LTypeName);

            for rPFilter in rT2.GetProperties do
            begin
              if (rPFilter.Name = (rA as HasMany).FilterPropertyName) then
              begin
                for rA2 in rPFilter.GetAttributes do
                begin
                  if (rA2 is Column) then
                  begin
                    LFilter := ' AND ' + (rA2 as Column).Name + '=' + QuotedStr(LFilterValue);
                    Break;
                  end;
                end;
              end;
            end;

            LValue := rP.GetValue(Result);
            AModel := LValue.AsObject;
            Self.GetList(AModel, rT2.AsInstance.MetaclassType, LFilter, False);

            Break;
          end
          else
            Break;
        end;
      end
      else if (rP.PropertyType.TypeKind = tkClass) then
      begin
        for rA in rP.GetAttributes do
        begin
          if (rA is HasOne) then
          begin
            rPValue := rT.GetProperty((rA as HasOne).ValuePropertyName);
            LFilterValue := rPValue.GetValue(Result).AsVariant;

            LValue := rP.GetValue(Result);
            AModel := LValue.AsObject;
            AModel2 := Self.GetByOne(AModel.ClassInfo, LFilterValue.ToInt64, ALock, AModel);
            Break;
          end
        end;
      end;
    end;
  finally
    rC.Free;
  end;
end;

function TEntityManager.GetList<T>(AFilter: string; ALock: Boolean): TObjectList<T>;
begin
  Result := TObjectList<T>.Create(True);
  GetList<T>(Result, AFilter, ALock);
end;

procedure TEntityManager.GetList<T>(AObject: TObject; AFilter: string; ALock: Boolean);
var
  rC: TRttiContext;
  rT: TRttiType;
begin
  rC := TRttiContext.Create;
  try
    rT := rC.GetType(TypeInfo(T));
    GetList(AObject, rT.AsInstance.MetaclassType, AFilter, ALock, True);
  finally
    rC.Free;
  end;
end;

procedure TEntityManager.GetList(AObject: TObject; AClass: TClass; AFilter: string; ALock: Boolean; ALazyLoading: Boolean);
var
  rC: TRttiContext;
  rPck: TRttiPackage;
  rT, rT2: TRttiType;
  rP, rPValue, rPFilter: TRttiProperty;
  rPs: TArray<TRttiProperty>;
  rA, rA2: TCustomAttribute;
  LValue: TValue;
  AModel, AModel2, AModelListItem: TObject;
  LTableName, LSQL, LTypeName, LFilter, LFilterValue: string;
  StartBracetPos, EndBracetPos: Integer;
begin
  LTableName := Self.GetTableName(AClass);
  if LTableName = '' then Exit;

  rC := TRttiContext.Create;
  try
    rT := rC.GetType(AClass);
    MethodCall(AObject, 'Clear', []);
    LSQL := GetSQLWithFilter(AFilter, rT);
    rPs := GetRelations(rT);

    SetListModelValueFromQuery(AObject, rT, LSQL);

    if not ALazyLoading then Exit;

    for AModelListItem in TObjectList<TObject>(AObject) do
    begin
      rT := rC.GetType(AModelListItem.ClassType);
      rPs := GetRelations(rT);

      for rP in rPs do
      begin
        if  (RightStr(rP.PropertyType.QualifiedName.Split(['<'])[0], 11) = 'TObjectList')
        and (rP.PropertyType.TypeKind = tkClass)
        then
        begin
          for rA in rP.GetAttributes do
          begin
            if (rA is HasMany) then
            begin
              rPValue := rT.GetProperty((rA as HasMany).ValuePropertyName);
              LFilterValue := rPValue.GetValue(AModelListItem).AsVariant;

              rPck := rC.GetPackages[0];
              StartBracetPos := Pos('<', rP.PropertyType.QualifiedName)+1;
              EndBracetPos := Pos('>', rP.PropertyType.QualifiedName);
              LTypeName := Copy(rP.PropertyType.QualifiedName, StartBracetPos, EndBracetPos-StartBracetPos);
              rT2 := rPck.FindType(LTypeName);

              for rPFilter in rT2.GetProperties do
              begin
                if (rPFilter.Name = (rA as HasMany).FilterPropertyName) then
                begin
                  for rA2 in rPFilter.GetAttributes do
                  begin
                    if (rA2 is Column) then
                    begin
                      LFilter := ' AND ' + (rA2 as Column).Name + '=' + QuotedStr(LFilterValue);
                      Break;
                    end;
                  end;
                end;
              end;

              LValue := rP.GetValue(AModelListItem);
              AModel := LValue.AsObject;
              Self.GetList(AModel, rT2.AsInstance.MetaclassType, LFilter, False);

              Break;
            end
            else
              Break;
          end;
        end
        else if (rP.PropertyType.TypeKind = tkClass) then
        begin
          for rA in rP.GetAttributes do
          begin
            if (rA is HasOne) then
            begin
              rPValue := rT.GetProperty((rA as HasOne).ValuePropertyName);
              LFilterValue := rPValue.GetValue(AObject).AsVariant;

              LValue := rP.GetValue(AObject);
              AModel := LValue.AsObject;
              AModel2 := Self.GetByOne(AModel.ClassInfo, LFilterValue.ToInt64, ALock, AModel);
              Break;
            end
          end;
        end;
      end;
    end;
  finally
    rC.Free;
  end;
end;

procedure TEntityManager.Add<T>(AModel: T);
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AProp : TRttiProperty;
  LAttr : TCustomAttribute;

  LTable : string;
  LNames: string;
  LValues: string;
  LPointer: Pointer;
begin
  ACtx := TRttiContext.Create;
  LCmd := TZQuery.Create(nil);
  AProp := nil;
  try
    AType := ACtx.GetType(TypeInfo(T));
    LTable := Self.GetTableName(AType.AsInstance.MetaclassType);

    Move(AModel, LPointer, SizeOf(Pointer));

    LNames := '';
    LValues := '';
    for AProp in AType.GetProperties do
    begin
      if AProp.Visibility in [mvPublished, mvPublic] then
      begin
        for LAttr in AProp.GetAttributes do
        begin
          if LAttr is Column then
          begin
            if Column(LAttr).Name = '' then
              raise Exception.Create('Column attribute must be declared!!!');

            if Column(LAttr).Name = 'id' then
              Break;

            LNames := LNames + Column(LAttr).Name + ',';

            case AProp.PropertyType.TypeKind of
              tkUnknown       : ;
              tkInteger       : LValues := LValues + IntToStr(AProp.GetValue(LPointer).AsInteger) + ',';
              tkChar          : LValues := LValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkEnumeration   : raise Exception.Create('Not implemented data type');
              tkFloat         : LValues := LValues + FloatToStr(AProp.GetValue(LPointer).AsExtended) + ',';
              tkString        : LValues := LValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkSet           : raise Exception.Create('Not implemented data type');
              tkClass         : raise Exception.Create('Not implemented data type');
              tkMethod        : raise Exception.Create('Not implemented data type');
              tkWChar         : LValues := LValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkLString       : LValues := LValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkWString       : LValues := LValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkVariant       : raise Exception.Create('Not implemented data type');
              tkArray         : raise Exception.Create('Not implemented data type');
              tkRecord        : raise Exception.Create('Not implemented data type');
              tkInterface     : raise Exception.Create('Not implemented data type');
              tkInt64         : LValues := LValues + IntToStr(AProp.GetValue(LPointer).AsInt64) + ',';
              tkDynArray      : raise Exception.Create('Not implemented data type');
              tkUString       : LValues := LValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkClassRef      : raise Exception.Create('Not implemented data type');
              tkPointer       : raise Exception.Create('Not implemented data type');
              tkProcedure     : raise Exception.Create('Not implemented data type');
              tkMRecord       : raise Exception.Create('Not implemented data type');
            end;
          end;
        end;
      end;
    end;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('INSERT INTO ' + LTable + '(' + LeftStr(LNames, Length(LNames)-1) + ') VALUES (' + LeftStr(LValues, Length(LValues)-1) + ') RETURNING id;');
    LCmd.Prepare;
    if LCmd.Prepared and (AProp <> nil) then
    begin
      LCmd.Open;
      AProp.SetValue(LPointer, TValue.From(LCmd.Fields.Fields[0].AsLargeInt));
    end;
  finally
    ACtx.Free;
    LCmd.Free;
  end;
end;

procedure TEntityManager.AddBatch(AModels: TObject);
var
  AModel: TObject;
begin
  for AModel in TObjectList<TObject>(AModels) do
    Self.Add(AModel);
end;

procedure TEntityManager.AddBatch<T>(AModels: TArray<T>);
var
  AModel: T;
begin
  for AModel in AModels do
    Self.Add(AModel);
end;

procedure TEntityManager.Update(AModel: TObject);
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AProp : TRttiProperty;
  LAttr : TCustomAttribute;

  LColName : string;
  LTableName : string;
  LColNames: string;

  LID: Int64;
begin
  ACtx := TRttiContext.Create;
  LCmd := TZQuery.Create(nil);
  try
    AType := ACtx.GetType(AModel.ClassType);
    LTableName := Self.GetTableName(AModel.ClassType);
    LID := 0;

    LColNames := '';
    for AProp in AType.GetProperties do
    begin
      if AProp.IsReadable and AProp.IsWritable and (AProp.Visibility in [mvPublished, mvPublic]) then
      begin
        for LAttr in AProp.GetAttributes do
          if Assigned(LAttr) and (LAttr is Column) then
          begin
            LColName := Column(LAttr).Name;

            if LColName = '' then
              raise Exception.Create('Column attribute must be declared!!!');

            if LColName = 'id' then
            begin
              LID := AProp.GetValue(AModel).AsInt64;
              Break;
            end;

            case AProp.PropertyType.TypeKind of
              tkUnknown       : ;
              tkInteger       : LColNames := LColNames + LColName + '=' + IntToStr(AProp.GetValue(AModel).AsInteger) + ',';
              tkChar          : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(AModel).AsString) + ',';
              tkEnumeration   : raise Exception.Create('Not implemented data type');
              tkFloat         : LColNames := LColNames + LColName + '=' + FloatToStr(AProp.GetValue(AModel).AsExtended) + ',';
              tkString        : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(AModel).AsString) + ',';
              tkSet           : raise Exception.Create('Not implemented data type');
              tkClass         : raise Exception.Create('Not implemented data type');
              tkMethod        : raise Exception.Create('Not implemented data type');
              tkWChar         : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(AModel).AsString) + ',';
              tkLString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(AModel).AsString) + ',';
              tkWString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(AModel).AsString) + ',';
              tkVariant       : raise Exception.Create('Not implemented data type');
              tkArray         : raise Exception.Create('Not implemented data type');
              tkRecord        : raise Exception.Create('Not implemented data type');
              tkInterface     : raise Exception.Create('Not implemented data type');
              tkInt64         : LColNames := LColNames + LColName + '=' + IntToStr(AProp.GetValue(AModel).AsInt64) + ',';
              tkDynArray      : raise Exception.Create('Not implemented data type');
              tkUString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(AModel).AsString) + ',';
              tkClassRef      : raise Exception.Create('Not implemented data type');
              tkPointer       : raise Exception.Create('Not implemented data type');
              tkProcedure     : raise Exception.Create('Not implemented data type');
              tkMRecord       : raise Exception.Create('Not implemented data type');
            end;
          end;
      end;
    end;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('UPDATE ' + LTableName + ' SET ' + LeftStr(LColNames, Length(LColNames)-1) + ' WHERE id=' + IntToStr(LID));
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    ACtx.Free;
    LCmd.Free;
  end;
end;

procedure TEntityManager.UpdateBatch(AModels: TObject);
var
  AModel: TObject;
begin
  for AModel in TObjectList<TObject>(AModels) do
    Self.Update(AModel);
end;

procedure TEntityManager.UpdateBatch<T>(AModels: TArray<T>);
var
  AModel: T;
begin
  for AModel in AModels do
    Self.Update(AModel);
end;

procedure TEntityManager.Delete<T>(AID: Int64);
var
  rC: TRttiContext;
  rT: TRttiType;
  LTableName : string;
  LCmd: TZQuery;
begin
  if AID = 0 then Exit;

  LCmd := TZQuery.Create(nil);
  rC := TRttiContext.Create;
  rT := rC.GetType(TypeInfo(T));
  try
    LTableName := Self.GetTableName(rT.AsInstance.MetaclassType);
    if LTableName = '' then Exit;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('DELETE FROM ' + LTableName + ' WHERE id=' + IntToStr(AID));
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
    rC.Free;
  end;
end;

procedure TEntityManager.Delete<T>(AModel: T);
var
  rC: TRttiContext;
  rT: TRttiType;
  rP: TRttiProperty;
  LV: TValue;
  LPointer: Pointer;
  LTableName : string;
  LID: Int64;
  LCmd: TZQuery;
begin
  LCmd := TZQuery.Create(nil);
  rC := TRttiContext.Create;
  rT := rC.GetType(TypeInfo(T));
  try
    LTableName := Self.GetTableName(rT.AsInstance.MetaclassType);
    if LTableName = '' then Exit;

    rP := rT.GetProperty('Id');
    if not Assigned(rP) then
      Exit;

    Move(AModel, LPointer, SizeOf(Pointer));
    LID := rP.GetValue(LPointer).AsInt64;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('DELETE FROM ' + LTableName + ' WHERE id=' + IntToStr(LID));
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
    rC.Free;
  end;
end;

procedure TEntityManager.DeleteBatch<T>(AIDs: TArray<Int64>);
var
  LFilter: string;
  LID: Int64;
begin
  LFilter := '';
  for LID in AIDs do
    LFilter := LFilter + LID.ToString + ',';

  LFilter := ReverseString(Trim(LFilter));
  System.Delete(LFilter, 1, 1);
  LFilter := ReverseString(LFilter);
  LFilter := ' and id in (' + LFilter + ')';
  Self.DeleteBatch<T>(LFilter);
end;

procedure TEntityManager.DeleteBatch<T>(AModels: TArray<T>);
var
  AModel: T;
begin
  for AModel in AModels do
    Self.Delete(AModel);
end;

procedure TEntityManager.DeleteBatch<T>(AFilter: string);
var
  rC: TRttiContext;
  rT: TRttiType;
  LTableName : string;
  LCmd: TZQuery;
begin
  rC := TRttiContext.Create;
  rT := rC.GetType(TypeInfo(T));
  LCmd := TZQuery.Create(nil);
  try
    LTableName := Self.GetTableName(rT.AsInstance.MetaclassType);
    if LTableName = '' then Exit;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('DELETE FROM ' + LTableName + ' WHERE 1=1 ' + AFilter);
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    rC.Free;
    LCmd.Free;
  end;
end;

function TEntityManager.Clone<T>(ASource: T): T;
var
  ACtx: TRttiContext;
  AType: TRttiType;
  AProp : TRttiProperty;
  AMethod: TRttiMethod;
  SourceAsPointer, ResultAsPointer: Pointer;
begin
  ACtx := TRttiContext.Create;
  try
    AType := ACtx.GetType(TypeInfo(T));
    for AMethod in AType.GetMethods do
    begin
      if Assigned(AMethod) and AMethod.IsConstructor and AType.IsInstance then
      begin
        Result := AMethod.Invoke(AType.AsInstance.MetaclassType, []).AsType<T>;
        break;
      end;
    end;

    Move(ASource, SourceAsPointer, SizeOf(Pointer));
    Move(Result, ResultAsPointer, SizeOf(Pointer));

    for AProp in AType.GetProperties do
      if  (AProp.Visibility in [mvPublished, mvPublic])
      and AProp.IsReadable
      and AProp.IsWritable
      then
        AProp.SetValue(ResultAsPointer, AProp.GetValue(SourceAsPointer));
  finally
    ACtx.Free;
  end;
end;

function TEntityManager.LogicalGet<T>(AFilter: string; ALock, AWithBegin: Boolean): TObjectList<T>;
begin
  try
    if not ALock then
      AWithBegin := False;

    if AWithBegin then
      Self.StartTrans;

    Result := GetList<T>(AFilter, ALock);
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
    end;
  end;
end;

procedure TEntityManager.LogicalAdd<T>(AModels: TArray<T>; AWithBegin, AWithCommit: Boolean);
begin
  try
    if AWithBegin then
      Self.StartTrans;
    Self.AddBatch<T>(AModels);
    if AWithCommit then
      Self.CommitTrans;
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
    end;
  end;
end;

procedure TEntityManager.LogicalUpdate<T>(AModels: TArray<T>; AWithCommit: Boolean);
begin
  try
    Self.UpdateBatch<T>(AModels);
    if AWithCommit then
      Self.StartTrans;
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
    end;
  end;
end;

procedure TEntityManager.LogicalDelete<T>(AIDs: TArray<Int64>; AWithCommit: Boolean);
begin
  try
    Self.DeleteBatch<T>(AIDs);
    if AWithCommit then
      Self.CommitTrans();
  except
    on E: Exception do
    begin
      Self.RollbackTrans;
    end;
  end;
end;

end.
