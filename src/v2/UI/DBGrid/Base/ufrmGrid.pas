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
    FTable: T;
    FQry: TFDQuery;
    FGrd: TDBGrid;

    FGridPopMenu: TPopupMenu;
    FmniPreview: TMenuItem;
    FmniExportExcel: TMenuItem;
    FmniPrint: TMenuItem;
    FmniRemoveGridSort: TMenuItem;

    procedure SetQry(const Value: TFDQuery);
    procedure SetTable(const Value: T);
    procedure SetGrd(const Value: TDBGrid);

    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetmniExportExcel(const Value: TMenuItem);
    procedure SetmniPreview(const Value: TMenuItem);
    procedure SetmniPrint(const Value: TMenuItem);
    procedure SetmniRemoveGridSort(const Value: TMenuItem);
  public
    property Qry: TFDQuery read FQry write SetQry;
    property Table: T read FTable write SetTable;
    property Grd: TDBGrid read FGrd write SetGrd;

    property GridPopMenu: TPopupMenu read FGridPopMenu write SetPopupMenu;
    property mniExportExcel: TMenuItem read FmniExportExcel write SetmniExportExcel;
    property mniPreview: TMenuItem read FmniPreview write SetmniPreview;
    property mniPrint: TMenuItem read FmniPrint write SetmniPrint;
    property mniRemoveGridSort: TMenuItem read FmniRemoveGridSort write SetmniRemoveGridSort;

    function AddMenu(ATitle, AMenuName: string; AClickEvent: TNotifyEvent; AShortCut: TShortCut = 0; AParentMenu: TMenuItem = nil): TMenuItem;
    procedure AddPopupMenuSpliter(AParentMenu: TMenuItem = nil);
    procedure MenuExportExcelClick(Sender: TObject);
    procedure MenuPreviewClick(Sender: TObject);
    procedure MenuPrintClick(Sender: TObject);
    procedure MenuRemoveSortClick(Sender: TObject);
    procedure MenuMsgClick(Sender: TObject);

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

function TfrmGrid<T>.AddMenu(ATitle, AMenuName: string; AClickEvent: TNotifyEvent; AShortCut: TShortCut; AParentMenu: TMenuItem): TMenuItem;
begin
  if AParentMenu <> nil then
  begin
    Result := TMenuItem.Create(AParentMenu);
    Result.Caption := ATitle;
    Result.Name := AMenuName;
    Result.ShortCut := AShortCut;
    Result.OnClick := AClickEvent;
    AParentMenu.Add(Result);
  end
  else
  begin
    Result := TMenuItem.Create(Self.GridPopMenu);
    Result.Caption := ATitle;
    Result.Name := AMenuName;
    Result.ShortCut := AShortCut;
    Result.OnClick := AClickEvent;
    Self.GridPopMenu.Items.Add(Result);
  end;
end;

procedure TfrmGrid<T>.AddPopupMenuSpliter(AParentMenu: TMenuItem);
var
  LMenu: TMenuItem;
begin
  if Assigned(AParentMenu) then
  begin
    LMenu := TMenuItem.Create(AParentMenu);
    LMenu.Caption := '-';
    AParentMenu.Add(LMenu);
  end
  else
  begin
    LMenu := TMenuItem.Create(GridPopMenu);
    LMenu.Caption := '-';
    GridPopMenu.Items.Add(LMenu);
  end;
end;

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

  GridPopMenu := TPopupMenu.Create(Self);
  mniPreview := AddMenu('Preview', 'mniPreview', MenuPreviewClick);
  AddPopupMenuSpliter();
  mniExportExcel := AddMenu('Export Excel', 'mniExportExcel', MenuExportExcelClick);
  mniPrint := AddMenu('Print', 'mniPrint', MenuPrintClick);
  mniRemoveGridSort := AddMenu('Remove Sort', 'mniRemoveGridSort', MenuRemoveSortClick);
  AddMenu('Message', 'mniMsg', MenuMsgClick);


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
  grd.PopupMenu := Self.GridPopMenu;
end;

destructor TfrmGrid<T>.Destroy;
begin
  FQry.Close;
  FQry.Free;
  FDataSource.Free;

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

procedure TfrmGrid<T>.SetQry(const Value: TFDQuery);
begin
  FQry := Value;
end;

procedure TfrmGrid<T>.SetTable(const Value: T);
begin
  FTable := Value;
end;

procedure TfrmGrid<T>.SetGrd(const Value: TDBGrid);
begin
  FGrd := Value;
end;

procedure TfrmGrid<T>.SetPopupMenu(const Value: TPopupMenu);
begin
  FGridPopMenu := Value;
end;

procedure TfrmGrid<T>.SetmniExportExcel(const Value: TMenuItem);
begin
  FmniExportExcel := Value;
end;

procedure TfrmGrid<T>.SetmniPreview(const Value: TMenuItem);
begin
  FmniPreview := Value;
end;

procedure TfrmGrid<T>.SetmniPrint(const Value: TMenuItem);
begin
  FmniPrint := Value;
end;

procedure TfrmGrid<T>.SetmniRemoveGridSort(const Value: TMenuItem);
begin
  FmniRemoveGridSort := Value;
end;

procedure TfrmGrid<T>.MenuExportExcelClick(Sender: TObject);
begin
  ShowMessage('not implemented yet');
end;

procedure TfrmGrid<T>.MenuPreviewClick(Sender: TObject);
begin
  ShowMessage('not implemented yet');
end;

procedure TfrmGrid<T>.MenuPrintClick(Sender: TObject);
begin
  ShowMessage('not implemented yet');
end;

procedure TfrmGrid<T>.MenuRemoveSortClick(Sender: TObject);
begin
  ShowMessage('not implemented yet');
end;

procedure TfrmGrid<T>.MenuMsgClick(Sender: TObject);
begin
  ShowMessage('Hello from MenuItem ' + Sender.ClassName);
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
        mniRemoveGridSort.Visible := True;

      AQuery.IndexFieldNames := LOrderList;
    end;
  finally
    sl.Free;
  end;
end;

end.

