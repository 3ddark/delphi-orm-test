unit ufrmGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids,
  ZAbstractRODataset, ZDataset,
  Ths.Orm.Manager;

type
  TfrmGrid<T> = class(TForm)
    grd: TDBGrid;
    FDataSource: TDataSource;
    status: TStatusBar;
  private
    FQry: TZQuery;
    FTable: T;
    procedure SetQry(const Value: TZQuery);
    procedure SetTable(const Value: T);
  protected
  public
    property Qry: TZQuery read FQry write SetQry;
    property Table: T read FTable write SetTable;

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
end;

destructor TfrmGrid<T>.Destroy;
begin
  FQry.Close;
  FQry.Free;
  FDataSource.Free;

  PObject(@FTable).DisposeOf;

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
var
  LSortType: TSortType;
begin
  if Qry.SortType = stAscending then
    LSortType := stDescending
  else if Qry.SortType = stDescending then
    LSortType := stAscending
  else if Qry.SortType = stIgnored then
    LSortType := stAscending;

  Qry.SortedFields := '"' + Column.Field.FieldName + '"';
  Qry.SortType := LSortType;
end;

procedure TfrmGrid<T>.SetQry(const Value: TZQuery);
begin
  FQry := Value;
end;

procedure TfrmGrid<T>.SetTable(const Value: T);
begin
  FTable := Value;
end;

end.

