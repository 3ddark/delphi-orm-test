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
    FLazzyLoading: Boolean;
  protected
    function Connection: TZAbstractConnection;

    function GetTableName(AClass: TClass): string;
  public
    function GetById(AClass: TClass; AID: Int64; ALock: Boolean = False): TObject; virtual;
    function GetList(AClass: TClass; const AIDs: TArray<Int64>; ALock: Boolean = False): TArray<TObject>; reintroduce; overload; virtual;
    function GetList(AClass: TClass; const AFilter: string = ''; ALock: Boolean = False): TArray<TObject>; reintroduce; overload; virtual;
    procedure Add(AModel: TObject);
    procedure AddBatch<T>(AModels: TArray<T>);
    procedure Update(AModel: TObject);
    procedure UpdateBatch<T>(AModels: TArray<T>);
    procedure Delete(AClass: TClass; const AID: Int64);
    procedure DeleteBatch<T>(const AIDs: TArray<Int64>); reintroduce; overload;
    procedure DeleteBatch<T>(const AFilter: string = ''); reintroduce; overload;

    function Clone<T>(ASource: T): T;

    procedure TransactionStart;
    procedure TransactionCommit;
    procedure TransactionRollback;

    function LogicalGet<T>(const AFilter: string = ''; ALock: Boolean = False; AWithBegin: Boolean = False): TList<T>;
    procedure LogicalAdd<T>(AModels: TArray<T>; AWithBegin: Boolean = False; AWithCommit: Boolean = False);
    procedure LogicalUpdate<T>(AModels: TArray<T>; AWithCommit: Boolean = False);
    procedure LogicalDelete<T>(const AIDs: TArray<Int64>; AWithCommit: Boolean = False);

    constructor Create(AConnection: TZAbstractConnection);
  end;

implementation

uses
  System.Classes;

procedure TEntityManager.Add(AModel: TObject);
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
    AType := ACtx.GetType(AModel.ClassType);
    LTable := Self.GetTableName(AModel.ClassType);

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

function TEntityManager.GetById(AClass: TClass; AID: Int64; ALock: Boolean): TObject;
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AMethod: TRttiMethod;

  AProp: TRttiProperty;
  APropFilter: TRttiProperty;
  APropValue: TRttiProperty;
  AProps: TArray<TRttiProperty>;

  AValue: TValue;
  AValues: TArray<TValue>;
  LAttr: TCustomAttribute;
  LAttr2: TCustomAttribute;

  AModel: TObject;
  AModelArr: TArray<TObject>;

  LColName: string;
  LTableName: string;
  LColNames: string;
  LColID: string;
  LPointer: Pointer;
  LPointerArr: Pointer;

  AField: TField;

  LPackage: TRttiPackage;

  LFilter: string;
  LFilterValue: string;

  StartBracetPos, EndBracetPos: Integer;
begin
  if AId = 0 then Exit;

  LTableName := Self.GetTableName(AClass);

  if LTableName = '' then Exit;

  ACtx := TRttiContext.Create;
  LCmd := TZQuery.Create(nil);
  try
    AType := ACtx.GetType(AClass);

    for AMethod in AType.GetMethods do
    begin
      if Assigned(AMethod) and AMethod.IsConstructor and AType.IsInstance then
      begin
        Result := TEntity(AMethod.Invoke(AType.AsInstance.MetaclassType, []).AsObject);
        break;
      end;
    end;

    LColNames := '';
    SetLength(AProps, 0);
    for AProp in AType.GetProperties do
    begin
      if AProp.IsReadable and AProp.IsWritable and (AProp.Visibility in [mvPublished, mvPublic]) then
      begin
        for LAttr in AProp.GetAttributes do
        begin
          if LAttr is Column then
          begin
            LColName := Column(LAttr).Name;
            if LColName = '' then
              raise Exception.Create('Column Attribute must be declared.' + sLineBreak + 'If it is not used use "NotMapped" attribute!!!');
            if LColName = 'id' then
              LColID := LColName
            else
              LColNames := LColNames + LColName + ',';
          end
          else if (AProp.PropertyType.TypeKind = tkClass) and (LAttr is OneToOne) then
          begin
            SetLength(AProps, Length(AProps)+1);
            AProps[Length(AProps)-1] := AProp;
          end
          else if (AProp.PropertyType.TypeKind = tkDynArray) and (LAttr is OneToMany) then
          begin
            SetLength(AProps, Length(AProps)+1);
            AProps[Length(AProps)-1] := AProp;
          end;
        end;
      end;
    end;

    if LColNames = '' then
      raise Exception.Create('field_name not found for SQL sorugusu için field_name bilgilerine ulaþýlamadý!!!');

    LColNames := LColID + ',' + LColNames;

    Move(Result, LPointer, SizeOf(Pointer));

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('SELECT ' + LeftStr(LColNames, Length(LColNames)-1) + ' FROM ' + LTableName + ' WHERE ' + LeftStr(LColNames, 2) + '=' + IntToStr(AID));
    LCmd.Prepare;
    if LCmd.Prepared then
    begin
      LCmd.Open;
      while not LCmd.Eof do
      begin
        for AProp in AType.GetProperties do
          if AProp.IsReadable and AProp.IsWritable and (AProp.Visibility in [mvPublic, mvPublished]) then
          begin
            for LAttr in AProp.GetAttributes do
              if (LAttr is Column) then
                for AField in LCmd.Fields do
                  if Column(LAttr).Name = AField.FieldName then
                  begin
                    case AField.DataType of
                      ftUnknown: ;
                      ftString        : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftSmallint      : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftInteger       : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftWord          : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
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
                      ftMemo          : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftGraphic: ;
                      ftFmtMemo       : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftParadoxOle: ;
                      ftDBaseOle: ;
                      ftTypedBinary: ;
                      ftCursor: ;
                      ftFixedChar     : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftWideString    : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftLargeint      : AProp.SetValue(LPointer, TValue.From(AField.AsLargeInt));
                      ftADT: ;
                      ftArray: ;
                      ftReference: ;
                      ftDataSet: ;
                      ftOraBlob: ;
                      ftOraClob: ;
                      ftVariant       : AProp.SetValue(LPointer, TValue.From(AField.Value));
                      ftInterface: ;
                      ftIDispatch: ;
                      ftGuid: ;
                      ftTimeStamp: ;
                      ftFMTBcd: ;
                      ftFixedWideChar: ;
                      ftWideMemo      : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftOraTimeStamp: ;
                      ftOraInterval: ;
                      ftLongWord      : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftShortint      : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftByte          : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
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
        LCmd.Next;
      end;
    end;

    LAttr := nil;
    for AProp in AProps do
    begin
      for LAttr in AProp.GetAttributes do
      begin
        if (LAttr is OneToOne) then
        begin
          APropValue := AType.GetProperty((LAttr as OneToOne).ValuePropertyName);
          LFilterValue := APropValue.GetValue(LPointer).AsVariant;
          Break;
        end
        else if (LAttr is OneToMany) then
        begin
          APropValue := AType.GetProperty((LAttr as OneToMany).ValuePropertyName);
          LFilterValue := APropValue.GetValue(LPointer).AsVariant;
          Break;
        end;
      end;

        if (AProp.PropertyType.TypeKind = tkDynArray) then
        begin
          AValue := AProp.GetValue(LPointer);

          LPackage := ACtx.GetPackages[0];
          StartBracetPos := Pos('<', AProp.PropertyType.QualifiedName);
          EndBracetPos := Pos('>', AProp.PropertyType.QualifiedName);
          AType := LPackage.FindType(Copy(AProp.PropertyType.QualifiedName, StartBracetPos, EndBracetPos-StartBracetPos));

          for APropFilter in AType.GetProperties do
            if (APropFilter.Name = (LAttr as OneToMany).FilterPropertyName) then
              for LAttr2 in APropFilter.GetAttributes do
                if (LAttr2 is Column) then
                begin
                  LFilter := ' AND ' + (LAttr2 as Column).Name + '=' + QuotedStr(LFilterValue);
                  Break;
                end;

          AModelArr := Self.GetList(AType.AsInstance.MetaclassType, LFilter, ALock);

          SetLength(AValues, 0);
          for AModel in AModelArr do
          begin
            SetLength(AValues, Length(AValues)+1);
            AValues[Length(AValues)-1] := TValue.From(AModel);
          end;
          Move(AModelArr, LPointerArr, SizeOf(Pointer));

          AProp.SetValue(LPointer, TValue.FromArray(AProp.PropertyType.Handle, AValues));
        end
        else
        begin
          AValue := AProp.GetValue(LPointer);
          AModel := AValue.AsObject;
          AModel := Self.GetById(AModel.ClassType, LFilterValue.ToInt64, ALock);
          AProp.SetValue(LPointer, AModel);
        end;
    end;

  finally
    ACtx.Free;
    LCmd.Free;
  end;
end;

function TEntityManager.GetList(AClass: TClass; const AFilter: string; ALock: Boolean): TArray<TObject>;
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AMethod: TRttiMethod;

  AProp: TRttiProperty;
  APropFilter: TRttiProperty;
  APropValue: TRttiProperty;
  AProps: TArray<TRttiProperty>;

  AValue: TValue;
  LAttr: TCustomAttribute;
  LAttr2: TCustomAttribute;

  AModel: TObject;
  AModelArr: TArray<TObject>;

  LColName: string;
  LTableName: string;
  LColNames: string;
  LColID: string;
  LPointer: Pointer;

  AField: TField;
  LTmpArr: TArray<string>;

  LFilter: string;
  LFilterValue: string;
begin
  LTableName := Self.GetTableName(AClass);

  if LTableName = '' then Exit;

  ACtx := TRttiContext.Create;
  LCmd := TZQuery.Create(nil);
  try
    AType := ACtx.GetType(AClass);





    LColNames := '';
    SetLength(AProps, 0);
    for AProp in AType.GetProperties do
    begin
      if AProp.IsReadable and AProp.IsWritable and (AProp.Visibility in [mvPublished, mvPublic]) then
      begin
        for LAttr in AProp.GetAttributes do
        begin
          if LAttr is Column then
          begin
            LColName := Column(LAttr).Name;
            if LColName = '' then
              raise Exception.Create('Column Attribute must be declared.' + sLineBreak + 'If it is not used use "NotMapped" attribute!!!');
            if LColName = 'id' then
              LColID := LColName
            else
              LColNames := LColNames + LColName + ',';
          end
          else if (AProp.PropertyType.TypeKind = tkClass) and (LAttr is OneToOne) then
          begin
            SetLength(AProps, Length(AProps)+1);
            AProps[Length(AProps)-1] := AProp;
          end
          else if (AProp.PropertyType.TypeKind = tkDynArray) and (LAttr is OneToMany) then
          begin
            SetLength(AProps, Length(AProps)+1);
            AProps[Length(AProps)-1] := AProp;
          end;
        end;
      end;
    end;

    if LColNames = '' then
      raise Exception.Create('field_name not found for SQL sorugusu için field_name bilgilerine ulaþýlamadý!!!');

    LColNames := LColID + ',' + LColNames;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('SELECT ' + LeftStr(LColNames, Length(LColNames)-1) + ' FROM ' + LTableName + ' WHERE 1=1 ' + AFilter);
    LCmd.Prepare;
    if LCmd.Prepared then
    begin
      LCmd.Open;
      LCmd.First;
      while not LCmd.Eof do
      begin

        AModel := nil;
        for AMethod in AType.GetMethods do
        begin
          if Assigned(AMethod) and AMethod.IsConstructor and AType.IsInstance then
          begin
            AModel := TEntity(AMethod.Invoke(AType.AsInstance.MetaclassType, []).AsObject);
            Move(AModel, LPointer, SizeOf(Pointer));
            break;
          end;
        end;

        if AModel = nil then
          Break;

        for AProp in AType.GetProperties do
          if AProp.IsReadable and AProp.IsWritable and (AProp.Visibility in [mvPublic, mvPublished]) then
          begin
            for LAttr in AProp.GetAttributes do
              if (LAttr is Column) then
                for AField in LCmd.Fields do
                  if Column(LAttr).Name = AField.FieldName then
                  begin
                    case AField.DataType of
                      ftUnknown: ;
                      ftString        : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftSmallint      : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftInteger       : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftWord          : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
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
                      ftMemo          : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftGraphic: ;
                      ftFmtMemo       : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftParadoxOle: ;
                      ftDBaseOle: ;
                      ftTypedBinary: ;
                      ftCursor: ;
                      ftFixedChar     : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftWideString    : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftLargeint      : AProp.SetValue(LPointer, TValue.From(AField.AsLargeInt));
                      ftADT: ;
                      ftArray: ;
                      ftReference: ;
                      ftDataSet: ;
                      ftOraBlob: ;
                      ftOraClob: ;
                      ftVariant       : AProp.SetValue(LPointer, TValue.From(AField.Value));
                      ftInterface: ;
                      ftIDispatch: ;
                      ftGuid: ;
                      ftTimeStamp: ;
                      ftFMTBcd: ;
                      ftFixedWideChar: ;
                      ftWideMemo      : AProp.SetValue(LPointer, TValue.From(AField.AsString));
                      ftOraTimeStamp: ;
                      ftOraInterval: ;
                      ftLongWord      : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftShortint      : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
                      ftByte          : AProp.SetValue(LPointer, TValue.From(AField.AsInteger));
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

        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := AModel;
        LCmd.Next;
      end;
    end;
  finally
    ACtx.Free;
    LCmd.Free;
  end;
end;

function TEntityManager.GetList(AClass: TClass; const AIDs: TArray<Int64>; ALock: Boolean): TArray<TObject>;
begin
  Result := [];
  Writeln('GetList with ID Array');
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

procedure TEntityManager.LogicalAdd<T>(AModels: TArray<T>; AWithBegin, AWithCommit: Boolean);
begin
//
end;

procedure TEntityManager.LogicalDelete<T>(const AIDs: TArray<Int64>; AWithCommit: Boolean);
begin
//
end;

function TEntityManager.LogicalGet<T>(const AFilter: string; ALock, AWithBegin: Boolean): TList<T>;
begin
//
end;

procedure TEntityManager.LogicalUpdate<T>(AModels: TArray<T>; AWithCommit: Boolean);
begin
//
end;

procedure TEntityManager.TransactionCommit;
begin
//
end;

procedure TEntityManager.TransactionRollback;
begin
//
end;

procedure TEntityManager.TransactionStart;
begin
//
end;

procedure TEntityManager.Update(AModel: TObject);
var
  ACtx: TRttiContext;
  AType: TRttiType;
  AProp : TRttiProperty;
  LAttr : TCustomAttribute;

  LColName : string;
  LTableName : string;
  LColNames: string;

  LPointer: Pointer;
  LCmd: TZQuery;
  LID: Int64;
begin
  ACtx := TRttiContext.Create;
  LCmd := TZQuery.Create(nil);
  try
    AType := ACtx.GetType(AModel);

    Move(AModel, LPointer, SizeOf(Pointer));

    LTableName := Self.GetTableName(AModel.ClassType);

    LID := 0;

    LColNames := '';
    for AProp in AType.GetProperties do
    begin
      if AProp.Visibility in [mvPublished, mvPublic] then
      begin
        for LAttr in AProp.GetAttributes do
        begin
          if LAttr is Column then
          begin
            LColName := Column(LAttr).Name;

            if LColName = '' then
              raise Exception.Create('ColumnAttribute tanýmlý deðil. Bu bilgi tanýmlanmak zorunda!!!');

            if LColName = 'id' then
            begin
              LID := AProp.GetValue(LPointer).AsInt64;
              Break;
            end;

            case AProp.PropertyType.TypeKind of
              tkUnknown       : ;
              tkInteger       : LColNames := LColNames + LColName + '=' + IntToStr(AProp.GetValue(LPointer).AsInteger) + ',';
              tkChar          : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkEnumeration   : raise Exception.Create('Tanýmlý olmayan data type');
              tkFloat         : LColNames := LColNames + LColName + '=' + FloatToStr(AProp.GetValue(LPointer).AsExtended) + ',';
              tkString        : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkSet           : raise Exception.Create('Tanýmlý olmayan data type');
              tkClass         : raise Exception.Create('Tanýmlý olmayan data type');
              tkMethod        : raise Exception.Create('Tanýmlý olmayan data type');
              tkWChar         : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkLString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkWString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkVariant       : raise Exception.Create('Tanýmlý olmayan data type');
              tkArray         : raise Exception.Create('Tanýmlý olmayan data type');
              tkRecord        : raise Exception.Create('Tanýmlý olmayan data type');
              tkInterface     : raise Exception.Create('Tanýmlý olmayan data type');
              tkInt64         : LColNames := LColNames + LColName + '=' + IntToStr(AProp.GetValue(LPointer).AsInt64) + ',';
              tkDynArray      : raise Exception.Create('Tanýmlý olmayan data type');
              tkUString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkClassRef      : raise Exception.Create('Tanýmlý olmayan data type');
              tkPointer       : raise Exception.Create('Tanýmlý olmayan data type');
              tkProcedure     : raise Exception.Create('Tanýmlý olmayan data type');
              tkMRecord       : raise Exception.Create('Tanýmlý olmayan data type');
            end;
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

procedure TEntityManager.AddBatch<T>(AModels: TArray<T>);
begin
//
end;

procedure TEntityManager.UpdateBatch<T>(AModels: TArray<T>);
begin
//
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

function TEntityManager.Connection: TZAbstractConnection;
begin
  Result := FConnection;
end;

constructor TEntityManager.Create(AConnection: TZAbstractConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection Required');

  FConnection := AConnection;
  FLazzyLoading := True;
end;

procedure TEntityManager.Delete(AClass: TClass; const AID: Int64);
var
  LTableName : string;
  LCmd: TZQuery;
begin
  if AID = 0 then Exit;

  LTableName := Self.GetTableName(AClass);
  if LTableName = '' then Exit;

  LCmd := TZQuery.Create(nil);
  try
    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('DELETE FROM ' + LTableName + ' WHERE id=' + IntToStr(AID));
    LCmd.Prepare;
    if LCmd.Prepared then
      LCmd.ExecSQL;
  finally
    LCmd.Free;
  end;
end;

procedure TEntityManager.DeleteBatch<T>(const AIDs: TArray<Int64>);
begin
//
end;

procedure TEntityManager.DeleteBatch<T>(const AFilter: string);
begin
//
end;

end.
