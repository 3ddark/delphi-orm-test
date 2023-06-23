unit EntityManager;

interface

uses
  SysUtils, StrUtils, Generics.Collections, System.TypInfo, Rtti,
  ZAbstractConnection, ZAbstractDataset, ZDataset, Entity,
  Data.DB, EntityAttributes;

type
  TEntityManager2 = class
  private
    FConnection: TZAbstractConnection;
    FLazzyLoading: Boolean;
  protected
    function Connection: TZAbstractConnection;

    function GetTableName(AClass: TClass): string;
  public
    function GetById(AClass: TClass; AID: Int64; ALock: Boolean = False): TObject;
    function GetList(AClass: TClass; const AIDs: TArray<Int64>; ALock: Boolean = False): TArray<TObject>; reintroduce; overload;
    function GetList(AClass: TClass; const AFilter: string = ''; ALock: Boolean = False): TArray<TObject>; reintroduce; overload;
    procedure Add(AModel: TEntity);
    procedure AddBatch<T>(AModels: TArray<T>);
    procedure Update(AModel: TEntity);
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

procedure TEntityManager2.Add(AModel: TEntity);
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AProp : TRttiProperty;
  LAttr : TCustomAttribute;

  LColName : string;
  LTableName : string;
  LColNames: string;
  LColValues: string;
  LPointer: Pointer;
begin
  ACtx := TRttiContext.Create;
  LCmd := TZQuery.Create(nil);
  AProp := nil;
  try
    AType := ACtx.GetType(AModel);
    LTableName := Self.GetTableName(AModel.ClassType);

    Move(AModel, LPointer, SizeOf(Pointer));

    LColNames := '';
    LColValues := '';
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
              raise Exception.Create('ColumnAttribute tan�ml� de�il. Bu bilgi tan�mlanmak zorunda!!!');

            if LColName = 'id' then
              Break;

            LColNames := LColNames + LColName + ',';

            case AProp.PropertyType.TypeKind of
              tkUnknown       : ;
              tkInteger       : LColValues := LColValues + IntToStr(AProp.GetValue(LPointer).AsInteger) + ',';
              tkChar          : LColValues := LColValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkEnumeration   : raise Exception.Create('Tan�ml� olmayan data type');
              tkFloat         : LColValues := LColValues + FloatToStr(AProp.GetValue(LPointer).AsExtended) + ',';
              tkString        : LColValues := LColValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkSet           : raise Exception.Create('Tan�ml� olmayan data type');
              tkClass         : raise Exception.Create('Tan�ml� olmayan data type');
              tkMethod        : raise Exception.Create('Tan�ml� olmayan data type');
              tkWChar         : LColValues := LColValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkLString       : LColValues := LColValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkWString       : LColValues := LColValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkVariant       : raise Exception.Create('Tan�ml� olmayan data type');
              tkArray         : raise Exception.Create('Tan�ml� olmayan data type');
              tkRecord        : raise Exception.Create('Tan�ml� olmayan data type');
              tkInterface     : raise Exception.Create('Tan�ml� olmayan data type');
              tkInt64         : LColValues := LColValues + IntToStr(AProp.GetValue(LPointer).AsInt64) + ',';
              tkDynArray      : raise Exception.Create('Tan�ml� olmayan data type');
              tkUString       : LColValues := LColValues + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkClassRef      : raise Exception.Create('Tan�ml� olmayan data type');
              tkPointer       : raise Exception.Create('Tan�ml� olmayan data type');
              tkProcedure     : raise Exception.Create('Tan�ml� olmayan data type');
              tkMRecord       : raise Exception.Create('Tan�ml� olmayan data type');
            end;
          end;
        end;
      end;
    end;

    LCmd.Connection := Connection;
    LCmd.SQL.Text := Trim('INSERT INTO ' + LTableName + '(' + LeftStr(LColNames, Length(LColNames)-1) + ') VALUES (' + LeftStr(LColValues, Length(LColValues)-1) + ') RETURNING id;');
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

function TEntityManager2.GetById(AClass: TClass; AID: Int64; ALock: Boolean): TObject;
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AMethod: TRttiMethod;

  AProp: TRttiProperty;
  APropFilter: TRttiProperty;
  APropValue: TRttiProperty;
  AProps: TArray<TRttiProperty>;

  AParam: TRttiParameter;
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
  IsList: Boolean;
  LTmpArr: TArray<string>;

  LPackage: TRttiPackage;

  LFilter: string;
  LFilterValue: string;
  n1: Integer;
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
        for AParam in AMethod.GetParameters do
        begin
          SetLength(AValues, Length(AValues)+1);
          AValues[Length(AValues)-1] := AParam;
        end;
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
      raise Exception.Create('field_name not found for SQL sorugusu i�in field_name bilgilerine ula��lamad�!!!');

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
          LTmpArr := AProp.PropertyType.QualifiedName.Split(['<']);
          AType := LPackage.FindType(LTmpArr[Length(LTmpArr)-1].Replace('>', ''));
          Writeln(AType.Name);
          Writeln(AType.AsInstance.MetaclassType.ClassName);

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

function TEntityManager2.GetList(AClass: TClass; const AFilter: string; ALock: Boolean): TArray<TObject>;
var
  LCmd: TZQuery;

  ACtx: TRttiContext;
  AType: TRttiType;
  AMethod: TRttiMethod;

  AProp: TRttiProperty;
  APropFilter: TRttiProperty;
  APropValue: TRttiProperty;
  AProps: TArray<TRttiProperty>;

  AParam: TRttiParameter;
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

  AField: TField;
  IsList: Boolean;
  LTmpArr: TArray<string>;

  LPackage: TRttiPackage;

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
      raise Exception.Create('field_name not found for SQL sorugusu i�in field_name bilgilerine ula��lamad�!!!');

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
            for AParam in AMethod.GetParameters do
            begin
              SetLength(AValues, Length(AValues)+1);
              AValues[Length(AValues)-1] := AParam;
            end;
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






(*
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
          LTmpArr := AProp.PropertyType.QualifiedName.Split(['<']);
          AType := LPackage.FindType(LTmpArr[Length(LTmpArr)-1].Replace('>', ''));
          Writeln(AType.Name);
          Writeln(AType.AsInstance.MetaclassType.ClassName);

          for APropFilter in AType.GetProperties do
            if (APropFilter.Name = (LAttr as OneToMany).FilterPropertyName) then
              for LAttr2 in APropFilter.GetAttributes do
                if (LAttr2 is Column) then
                begin
                  LFilter := ' AND ' + (LAttr2 as Column).Name + '=' + QuotedStr(LFilterValue);
                  Break;
                end;

          AModelArr := Self.GetList(AValue.AsClass, LFilter, ALock);

          AType := ACtx.GetType(AModelArr);
          AProp.SetValue(LPointer, TValue.FromArray(AType.Handle, AValue));
        end
        else
        begin
          AValue := AProp.GetValue(LPointer);
          AModel := AValue.AsObject;
          AModel := Self.GetById(AModel.ClassType, LFilterValue.ToInt64, ALock);
          AProp.SetValue(LPointer, AModel);
        end;
    end;
*)
  finally
    ACtx.Free;
    LCmd.Free;
  end;
end;

function TEntityManager2.GetList(AClass: TClass; const AIDs: TArray<Int64>; ALock: Boolean): TArray<TObject>;
begin
  Result := [];
  Writeln('GetList with ID Array');
end;

function TEntityManager2.GetTableName(AClass: TClass): string;
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

procedure TEntityManager2.LogicalAdd<T>(AModels: TArray<T>; AWithBegin, AWithCommit: Boolean);
begin
//
end;

procedure TEntityManager2.LogicalDelete<T>(const AIDs: TArray<Int64>; AWithCommit: Boolean);
begin
//
end;

function TEntityManager2.LogicalGet<T>(const AFilter: string; ALock, AWithBegin: Boolean): TList<T>;
begin
//
end;

procedure TEntityManager2.LogicalUpdate<T>(AModels: TArray<T>; AWithCommit: Boolean);
begin
//
end;

procedure TEntityManager2.TransactionCommit;
begin
//
end;

procedure TEntityManager2.TransactionRollback;
begin
//
end;

procedure TEntityManager2.TransactionStart;
begin
//
end;

procedure TEntityManager2.Update(AModel: TEntity);
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
              raise Exception.Create('ColumnAttribute tan�ml� de�il. Bu bilgi tan�mlanmak zorunda!!!');

            if LColName = 'id' then
            begin
              LID := AProp.GetValue(LPointer).AsInt64;
              Break;
            end;

            case AProp.PropertyType.TypeKind of
              tkUnknown       : ;
              tkInteger       : LColNames := LColNames + LColName + '=' + IntToStr(AProp.GetValue(LPointer).AsInteger) + ',';
              tkChar          : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkEnumeration   : raise Exception.Create('Tan�ml� olmayan data type');
              tkFloat         : LColNames := LColNames + LColName + '=' + FloatToStr(AProp.GetValue(LPointer).AsExtended) + ',';
              tkString        : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkSet           : raise Exception.Create('Tan�ml� olmayan data type');
              tkClass         : raise Exception.Create('Tan�ml� olmayan data type');
              tkMethod        : raise Exception.Create('Tan�ml� olmayan data type');
              tkWChar         : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkLString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkWString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkVariant       : raise Exception.Create('Tan�ml� olmayan data type');
              tkArray         : raise Exception.Create('Tan�ml� olmayan data type');
              tkRecord        : raise Exception.Create('Tan�ml� olmayan data type');
              tkInterface     : raise Exception.Create('Tan�ml� olmayan data type');
              tkInt64         : LColNames := LColNames + LColName + '=' + IntToStr(AProp.GetValue(LPointer).AsInt64) + ',';
              tkDynArray      : raise Exception.Create('Tan�ml� olmayan data type');
              tkUString       : LColNames := LColNames + LColName + '=' + QuotedStr(AProp.GetValue(LPointer).AsString) + ',';
              tkClassRef      : raise Exception.Create('Tan�ml� olmayan data type');
              tkPointer       : raise Exception.Create('Tan�ml� olmayan data type');
              tkProcedure     : raise Exception.Create('Tan�ml� olmayan data type');
              tkMRecord       : raise Exception.Create('Tan�ml� olmayan data type');
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

procedure TEntityManager2.AddBatch<T>(AModels: TArray<T>);
begin
//
end;

procedure TEntityManager2.UpdateBatch<T>(AModels: TArray<T>);
begin
//
end;

function TEntityManager2.Clone<T>(ASource: T): T;
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

function TEntityManager2.Connection: TZAbstractConnection;
begin
  Result := FConnection;
end;

constructor TEntityManager2.Create(AConnection: TZAbstractConnection);
begin
  if AConnection = nil then
    raise Exception.Create('Connection Required');

  FConnection := AConnection;
  FLazzyLoading := True;
end;

procedure TEntityManager2.Delete(AClass: TClass; const AID: Int64);
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

procedure TEntityManager2.DeleteBatch<T>(const AIDs: TArray<Int64>);
begin
//
end;

procedure TEntityManager2.DeleteBatch<T>(const AFilter: string);
begin
//
end;

end.