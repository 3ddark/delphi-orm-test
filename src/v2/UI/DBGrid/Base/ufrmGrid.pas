unit ufrmGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.Menus,
  FireDAC.Comp.Client,
  Ths.Orm.Manager;

type
  TfrmGrid<T> = class(TForm)
    FDataSource: TDataSource;
    status: TStatusBar;
  private
    FQry: TFDQuery;
    FTable: T;
    FMniRemoveGridSort: TMenuItem;
    FGrd: TDBGrid;
    procedure SetQry(const Value: TFDQuery);
    procedure SetTable(const Value: T);
    procedure SetMniRemoveGridSort(const Value: TMenuItem);
    procedure SetGrd(const Value: TDBGrid);
  public
    property Qry: TFDQuery read FQry write SetQry;
    property Table: T read FTable write SetTable;
    property Grd: TDBGrid read FGrd write SetGrd;

    property MniRemoveGridSort: TMenuItem read FMniRemoveGridSort write SetMniRemoveGridSort;

    constructor Create(AOwner: TComponent; ATable: T; ASQL: string; Dummy: Integer  = 0); reintroduce; overload;
    destructor Destroy; override;
  published
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);

    procedure grdDblClick(Sender: TObject);
    procedure grdCellClick(Column: TColumn);
    procedure grdKeyPress(Sender: TObject; var Key: Char);
    procedure grdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdTitleClick(Column: TColumn);
    procedure grdDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);

    procedure SortGridTitle(Sender: TObject);
  end;

implementation

uses Ths.Orm.ManagerStack;

constructor TfrmGrid<T>.Create(AOwner: TComponent; ATable: T; ASQL: string; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  FTable := ATable;
  FQry := ManagerApp.NewQuery;
  FQry.SQL.Text := ASQL;

  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FQry;

  Self.Position := poScreenCenter;
  Self.KeyPreview := True;
  Self.Constraints.MinWidth := 640;
  Self.Constraints.MinWidth := 480;
  //form event
  Self.OnCreate := FormCreate;
  Self.OnShow := FormShow;
  Self.OnClose := FormClose;
  Self.OnKeyPress := FormKeyPress;

  status := TStatusBar.Create(Self);
  status.Parent := Self;

  grd := TDBGrid.Create(Self);
  grd.Align := alClient;
  grd.DataSource := FDataSource;
  grd.Parent := Self;
  grd.Options := grd.Options - [dgEditing, dgConfirmDelete];
  //grd events
  grd.OnDblClick := grdDblClick;
  grd.OnCellClick := grdCellClick;
  grd.OnKeyPress := grdKeyPress;
  grd.OnKeyDown := grdKeyDown;
  grd.OnKeyUp := grdKeyUp;
  grd.OnTitleClick := grdTitleClick;
  grd.OnDrawColumnCell := grdDrawColumnCell;

  MniRemoveGridSort := TMenuItem.Create(nil);
end;

destructor TfrmGrid<T>.Destroy;
begin
  FQry.Close;
  FQry.Free;
  FDataSource.Free;

  MniRemoveGridSort.Free;

  PObject(@FTable).Free;

  inherited;
end;

procedure TfrmGrid<T>.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmGrid<T>.FormCreate(Sender: TObject);
begin
//
end;

procedure TfrmGrid<T>.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_ESCAPE) then
    Self.Close;
end;

procedure TfrmGrid<T>.FormShow(Sender: TObject);
begin
  Self.Caption := 'Base Title';
  FQry.Open;
end;

procedure TfrmGrid<T>.grdCellClick(Column: TColumn);
begin
//
end;

procedure TfrmGrid<T>.grdDblClick(Sender: TObject);
begin
//
end;

procedure TfrmGrid<T>.grdDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
//
end;

procedure TfrmGrid<T>.grdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TfrmGrid<T>.grdKeyPress(Sender: TObject; var Key: Char);
begin
//
end;

procedure TfrmGrid<T>.grdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TfrmGrid<T>.grdTitleClick(Column: TColumn);
begin
  SortGridTitle(Column);
end;

procedure TfrmGrid<T>.SetGrd(const Value: TDBGrid);
begin
  FGrd := Value;
end;

procedure TfrmGrid<T>.SetMniRemoveGridSort(const Value: TMenuItem);
begin
  FMniRemoveGridSort := Value;
end;

procedure TfrmGrid<T>.SetQry(const Value: TFDQuery);
begin
  FQry := Value;
end;

procedure TfrmGrid<T>.SetTable(const Value: T);
begin
  FTable := Value;
end;

procedure TfrmGrid<T>.SortGridTitle(Sender: TObject);
var
  sl: TStringList;
  LOrderList: string;
  LOrderedColumn, LIsCTRLKeyPress: Boolean;
  nIndex: Integer;
  LColumn: TColumn;
  AQuery: TFDQuery;
begin
  if Sender is TColumn then
    LColumn := Sender as TColumn
  else
    Exit;

  LOrderedColumn := False;
  LIsCTRLKeyPress := False;
  sl := TStringList.Create;
  try
    AQuery := TFDQuery(grd.DataSource.DataSet);
    begin
//      if isCtrlDown then
//        LIsCTRLKeyPress := True;

      //sort düzenle
      sl.Delimiter := ';';
      if AQuery.IndexFieldNames <> '' then
        sl.DelimitedText := AQuery.IndexFieldNames;

      if LIsCTRLKeyPress then
      begin
        //CTRL tuşuna basılmışsa
        for nIndex := 0 to sl.Count-1 do
          if (LColumn.FieldName + ':A' = sl.Strings[nIndex]) or (LColumn.FieldName + ':D' = sl.Strings[nIndex]) then
            LOrderedColumn := True;

        if LOrderedColumn then
        begin
          //listede zaten varsa ASC DESC değişimi yap
          for nIndex := 0 to sl.Count-1 do
          begin
            if (LColumn.FieldName + ':A' = sl.Strings[nIndex]) then
              sl.Strings[nIndex] := LColumn.FieldName + ':D'
            else if (LColumn.FieldName + ':D' = sl.Strings[nIndex]) then
              sl.Strings[nIndex] := LColumn.FieldName + ':A';
          end;
        end
        else
        begin
          //listede yoksa direkt ASC olarak ekle
          if sl.Count > 0 then
            sl.Add(LColumn.FieldName + ':A');
        end;
      end
      else
      begin
        //CTRL tuşuna basılmamışsa hepsini sil ve direkt olarak ekle
        if sl.Count = 0 then
          LOrderList := LColumn.FieldName + ':A'
        else
        begin
          for nIndex := 0 to sl.Count-1 do
            if (LColumn.FieldName + ':A' = sl.Strings[nIndex]) or (LColumn.FieldName + ':D' = sl.Strings[nIndex]) then
              LOrderedColumn := True;

          if LOrderedColumn then
          begin
            for nIndex := 0 to sl.Count-1 do
              if (LColumn.FieldName + ':A' = sl.Strings[nIndex]) then
                LOrderList := LColumn.FieldName + ':D'
              else if (LColumn.FieldName + ':D' = sl.Strings[nIndex]) then
                LOrderList := LColumn.FieldName + ':A';
          end
          else
            LOrderList := LColumn.FieldName + ':A';
        end;
        sl.Clear;
        sl.Add(LOrderList);
      end;

      LOrderList := '';

      for nIndex := 0 to sl.Count-1 do
      begin
        LOrderList := LOrderList + sl.Strings[nIndex] + ';';
        if nIndex = sl.Count-1 then
          LOrderList := LeftStr(LOrderList, Length(LOrderList)-1);
      end;

      if LOrderList <> '' then
        MniRemoveGridSort.Visible := True;

      AQuery.IndexFieldNames := LOrderList;
    end;
  finally
    sl.Free;
  end;
end;

end.

