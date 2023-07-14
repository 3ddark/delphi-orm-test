unit Ths.Erp.Database.Table;

interface

uses
  SysUtils,
  Classes,
  Dialogs,
  StrUtils,
  DateUtils,
  System.Variants,
  Data.DB;

{$M+}

type
  TFieldIslemTipi = (fpSelect, fpInsert, fpUpdate);
  TFieldIslemTipleri = set of TFieldIslemTipi;

  TTable = class;

  TFieldDB = class
  private
    FFieldName: string;
    FDataType: TFieldType;
    FValue: Variant;
    FSize: Integer;
    FIsNullable: Boolean;
    FOtherFieldName: string;
    FOwnerTable: TTable;
    FFieldIslemTipleri: TFieldIslemTipleri;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    procedure SetFieldIslemTipleri(const Value: TFieldIslemTipleri);
  public
    property FieldName: string read FFieldName write FFieldName;
    property DataType: TFieldType read FDataType write FDataType;
    property Value: Variant read GetValue write SetValue;
    property Size: Integer read FSize write FSize;
    property IsNullable: Boolean read FIsNullable write FIsNullable;
    property OtherFieldName: string read FOtherFieldName write FOtherFieldName;
    property OwnerTable: TTable read FOwnerTable write FOwnerTable;
    property FieldIslemTipleri: TFieldIslemTipleri read FFieldIslemTipleri write SetFieldIslemTipleri;

    constructor Create(AFieldName: string;
                     AFieldType: TFieldType;
                     AValue: Variant;
                     AOwnerTable: TTable;
                     AFieldIslemTipleri: TFieldIslemTipleri;
                     AOtherFieldName: string = '';
                     AMaxLength: Integer=0;
                     AIsNullable: Boolean=True);

    procedure Clone(var AField: TFieldDB);

    function AsBoolean: Boolean;
    function AsString: string;
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsDate: TDate;
    function AsTime: TTime;
    function AsDateTime: TDateTime;
    function AsFloat: Double;

    function QryName: string;
  end;


  TTable = class
  private
    FSchemaName: string;
    FTableName: string;
    FTableSourceCode: string;
    FFields: TArray<TFieldDB>;

    function GetTableName: string;
    procedure SetTableName(ATableName: string);
  published
    constructor Create; virtual;
    destructor Destroy; override;
  public
    Id: TFieldDB;

    property SchemaName: string read FSchemaName write FSchemaName;
    property TableName: string read GetTableName write SetTableName;
    property TableSourceCode: string read FTableSourceCode write FTableSourceCode;
    property Fields: TArray<TFieldDB> read FFields write FFields;

    procedure Clear; virtual;
    function Clone: TTable; virtual; abstract;
    function Validate: Boolean; virtual;
    procedure CloneData(ASrcTable: TTable);
  end;

implementation

constructor  TFieldDB.Create(
  AFieldName: string;
  AFieldType: TFieldType;
  AValue: Variant;
  AOwnerTable: TTable;
  AFieldIslemTipleri: TFieldIslemTipleri;
  AOtherFieldName: string = '';
  AMaxLength: Integer=0;
  AIsNullable: Boolean=True);
begin
  FFieldName := AFieldName;
  FDataType := AFieldType;
  FValue := AValue;
  FOwnerTable := AOwnerTable;
  FFieldIslemTipleri := AFieldIslemTipleri;
  FOtherFieldName := IfThen(AOtherFieldName = '', FFieldName, AOtherFieldName);
  FSize := AMaxLength;
  FIsNullable := AIsNullable;

  if FOwnerTable <> nil then
  begin
    SetLength(FOwnerTable.FFields, Length(FOwnerTable.FFields)+1);
    FOwnerTable.FFields[Length(FOwnerTable.FFields)-1] := Self;
  end;
end;

function TFieldDB.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TFieldDB.SetFieldIslemTipleri(const Value: TFieldIslemTipleri);
begin
  FFieldIslemTipleri := Value;
end;

procedure TFieldDB.SetValue(const Value: Variant);
begin
  FValue := Value;

  if VarIsStr(FValue) and (FValue = '') then
    case FDataType of
      ftUnknown: ;
      ftString: ;
      ftSmallint: FValue := 0;
      ftInteger: FValue := 0;
      ftWord: FValue := 0;
      ftBoolean: ;
      ftFloat: FValue := 0;
      ftCurrency: FValue := 0;
      ftBCD: ;
      ftDate: FValue := 0;
      ftTime: FValue := 0;
      ftDateTime: FValue := 0;
      ftBytes: ;
      ftVarBytes: ;
      ftAutoInc: ;
      ftBlob: ;
      ftMemo: ;
      ftGraphic: ;
      ftFmtMemo: ;
      ftParadoxOle: ;
      ftDBaseOle: ;
      ftTypedBinary: ;
      ftCursor: ;
      ftFixedChar: ;
      ftWideString: ;
      ftLargeint: ;
      ftADT: ;
      ftArray: ;
      ftReference: ;
      ftDataSet: ;
      ftOraBlob: ;
      ftOraClob: ;
      ftVariant: ;
      ftInterface: ;
      ftIDispatch: ;
      ftGuid: ;
      ftTimeStamp: FValue := 0;
      ftFMTBcd: FValue := 0;
      ftFixedWideChar: ;
      ftWideMemo: ;
      ftOraTimeStamp: ;
      ftOraInterval: ;
      ftLongWord: FValue := 0;
      ftShortint: FValue := 0;
      ftByte: ;
      ftExtended: ;
      ftConnection: ;
      ftParams: ;
      ftStream: ;
      ftTimeStampOffset: ;
      ftObject: ;
      ftSingle: ;
    end;
end;

function TFieldDB.QryName: string;
begin
  Result := IfThen(FOwnerTable.SchemaName <> '', FOwnerTable.SchemaName + '.', '') + FOwnerTable.TableName + '.' + FieldName;
end;

function TFieldDB.AsBoolean: Boolean;
begin
  Result := False;
  if VarType(FValue) and varTypeMask = varBoolean then
    Result := FValue;
end;

function TFieldDB.AsDate: TDate;
begin
  if VarIsFloat(Value) or VarIsNumeric(Value) then
    Result := VarToDateTime(Value)
  else
    Result := StrToDateDef(AsString, 0);
end;

function TFieldDB.AsTime: TTime;
begin
  if VarIsFloat(Value) or VarIsNumeric(Value) then
    Result := TimeOf(VarToDateTime(Value))
  else
    Result := TimeOf(StrToDateTimeDef(AsString, 0));
end;

function TFieldDB.AsDateTime: TDateTime;
begin
  if VarIsFloat(Value) or VarIsNumeric(Value) then
    Result := VarToDateTime(Value)
  else
    Result := StrToDateTimeDef(AsString, 0);
end;

function TFieldDB.AsFloat: Double;
begin
  Result := StrToFloatDef(AsString, 0);
end;

function TFieldDB.AsInt64: Int64;
begin
  Result := StrToInt64Def(AsString, 0);
end;

function TFieldDB.AsInteger: Integer;
begin
  Result := StrToIntDef(AsString, 0);
end;

function TFieldDB.AsString: string;
begin
  Result := System.Variants.VarToStr(Value);
end;

procedure TFieldDB.Clone(var AField: TFieldDB);
begin
  AField.FOwnerTable := FOwnerTable;
  AField.FFieldName := FFieldName;
  AField.FDataType := FDataType;
  AField.FValue := FValue;
  AField.FSize := FSize;
  AField.FIsNullable := FIsNullable;
  AField.FOtherFieldName := FOtherFieldName;
end;

procedure TTable.CloneData(ASrcTable: TTable);
var
  AFieldSrc, AFieldDes: TFieldDB;
begin
  for AFieldSrc in ASrcTable.Fields do
    for AFieldDes in Self.Fields do
      if  (AFieldSrc.FieldName = AFieldDes.FieldName)
      and (AFieldSrc.FieldName = AFieldDes.FieldName)
      then
      begin
        AFieldDes.Value := AFieldSrc.Value;
        Break;
      end;
end;

constructor TTable.Create;
begin
  if Trim(FTableName) = '' then
    raise Exception.Create('Table s�n�flar� Inherited Create i�leminden �nce Tablo ad� tan�mlanmak zorunda!!!');

  SetLength(FFields, 0);

  Id := TFieldDB.Create('id', ftInteger, 0, Self, [fpSelect, fpUpdate]);
  Id.Value := -1;//FDatabase.GetNewRecordId;
end;

function TTable.GetTableName: string;
begin
  Result := FTableName;
end;

procedure TTable.SetTableName(ATableName: string);
begin
  FTableName := ATableName;
end;

destructor TTable.Destroy;
var
  AField: TFieldDB;
begin
  for AField in Fields do
    AField.DisposeOf;
  SetLength(FFields, 0);
  inherited;
end;

procedure TTable.Clear;
var
  AField: TFieldDB;
begin
  for AField in FFields do
  begin
    with AField do
    begin
      if (DataType = ftString)
      or (DataType = ftWideString)
      or (DataType = ftMemo)
      or (DataType = ftWideMemo)
      or (DataType = ftBytes)
      or (DataType = ftFmtMemo)
      or (DataType = ftFixedChar)
      or (DataType = ftFixedWideChar)
      then
        Value := ''
      else
      if (DataType = ftSmallint)
      or (DataType = ftInteger)
      or (DataType = ftWord)
      or (DataType = ftFloat)
      or (DataType = ftCurrency)
      or (DataType = ftBCD)
      or (DataType = ftDate)
      or (DataType = ftTime)
      or (DataType = ftDateTime)
      or (DataType = ftBytes)
      or (DataType = ftVarBytes)
      or (DataType = ftAutoInc)
      or (DataType = ftLargeint)
      or (DataType = ftTimeStamp)
      or (DataType = ftShortint)
      or (DataType = ftByte)
      then
        Value := 0
      else if (DataType = ftBoolean) then
        Value := False
      else if (DataType = ftBlob) then
        Value := Null;
    end;
  end;
end;

function TTable.Validate: Boolean;
begin
  Result := True;
end;

end.
