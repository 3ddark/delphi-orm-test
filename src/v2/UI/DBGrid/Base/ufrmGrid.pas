unit ufrmGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Grids, Vcl.ExtCtrls, Vcl.DBGrids, Data.DB,
  Vcl.Clipbrd, FireDAC.Comp.Client, Ths.Orm.Manager;

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
    FmniFilter: TMenuItem;
    FmniFilterRemove: TMenuItem;
    FmniExportExcel: TMenuItem;
    FmniExportCsv: TMenuItem;
    FmniPrint: TMenuItem;
    FmniRemoveGridSort: TMenuItem;
    FHeader: TPanel;
    FFooter: TPanel;
    FContainer: TPanel;
    procedure PrepareForm();
    procedure PrepareGrid();
    procedure PreparePopupMenu();
    procedure DoFilter;
    procedure DoRemoveFilter;
    procedure SetQry(const Value: TFDQuery);
    procedure SetTable(const Value: T);
    procedure SetGrd(const Value: TDBGrid);

    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetmniPreview(const Value: TMenuItem);
    procedure SetmniFilter(const Value: TMenuItem);
    procedure SetmniFilterRemove(const Value: TMenuItem);
    procedure SetmniExportExcel(const Value: TMenuItem);
    procedure SetmniExportCsv(const Value: TMenuItem);
    procedure SetmniPrint(const Value: TMenuItem);
    procedure SetmniRemoveGridSort(const Value: TMenuItem);

    procedure SetFooter(const Value: TPanel);
    procedure SetHeader(const Value: TPanel);
    procedure SetContainer(const Value: TPanel);
  public
    property Qry: TFDQuery read FQry write SetQry;
    property Table: T read FTable write SetTable;
    property Grd: TDBGrid read FGrd write SetGrd;
    property Container: TPanel read FContainer write SetContainer;
    property Header: TPanel read FHeader write SetHeader;
    property Footer: TPanel read FFooter write SetFooter;

    property GridPopMenu: TPopupMenu read FGridPopMenu write SetPopupMenu;
    property mniPreview: TMenuItem read FmniPreview write SetmniPreview;
    property mniFilter: TMenuItem read FmniFilter write SetmniFilter;
    property mniFilterRemove: TMenuItem read FmniFilterRemove write SetmniFilterRemove;
    property mniExportExcel: TMenuItem read FmniExportExcel write SetmniExportExcel;
    property mniExportCsv: TMenuItem read FmniExportCsv write SetmniExportCsv;
    property mniPrint: TMenuItem read FmniPrint write SetmniPrint;
    property mniRemoveGridSort: TMenuItem read FmniRemoveGridSort write SetmniRemoveGridSort;

    constructor Create(AOwner: TComponent; ATable: T; ASQL: string; Dummy: Integer = 0); reintroduce; overload;
    destructor Destroy; override;
  published
    //***form***
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure FormKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    //***dbgrid***
    procedure grdDblClick(Sender: TObject); virtual;
    procedure grdCellClick(Column: TColumn); virtual;
    procedure grdKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure grdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure grdKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure grdTitleClick(Column: TColumn); virtual;
    procedure grdDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState); virtual;
    procedure SortGridTitle(Sender: TObject);
    //***query***
    procedure AfterDatasetOpen(Dataset: TDataset); virtual;
    procedure OnFilterDataset(Dataset: TDataset; var Accept: Boolean); virtual;
    //***datasource***
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    //***status bar***
    procedure RefreshStatucRecorCount();
    //***menu***
    function AddMenu(ATitle, AMenuName: string; AClickEvent: TNotifyEvent;
      AVibisle: Boolean = True; AShortCut: TShortCut = 0;
      AParentMenu: TMenuItem = nil): TMenuItem;
    procedure AddPopupMenuSpliter(AParentMenu: TMenuItem = nil);
    procedure mniPreviewClick(Sender: TObject); virtual;
    procedure mniFilterClick(Sender: TObject); virtual;
    procedure mniFilterRemoveClick(Sender: TObject); virtual;
    procedure mniExportExcelClick(Sender: TObject); virtual;
    procedure mniExportCsvClick(Sender: TObject); virtual;
    procedure mniPrintClick(Sender: TObject); virtual;
    procedure mniRemoveSortClick(Sender: TObject); virtual;
  end;

implementation

uses Ths.Orm.ManagerStack;

function TfrmGrid<T>.AddMenu(ATitle, AMenuName: string; AClickEvent: TNotifyEvent;
  AVibisle: Boolean; AShortCut: TShortCut; AParentMenu: TMenuItem): TMenuItem;
begin
  if AParentMenu <> nil then
  begin
    Result := TMenuItem.Create(AParentMenu);
    Result.Caption := ATitle;
    Result.Name := AMenuName;
    Result.ShortCut := AShortCut;
    Result.OnClick := AClickEvent;
    Result.Visible := AVibisle;
    AParentMenu.Add(Result);
  end
  else
  begin
    Result := TMenuItem.Create(Self.GridPopMenu);
    Result.Caption := ATitle;
    Result.Name := AMenuName;
    Result.ShortCut := AShortCut;
    Result.OnClick := AClickEvent;
    Result.Visible := AVibisle;
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

procedure TfrmGrid<T>.AfterDatasetOpen(Dataset: TDataset);
begin
  RefreshStatucRecorCount();
end;

constructor TfrmGrid<T>.Create(AOwner: TComponent; ATable: T; ASQL: string; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  FTable := ATable;
  FQry := ManagerApp.NewQuery;
  FQry.AfterOpen := AfterDatasetOpen;
  FQry.OnFilterRecord := OnFilterDataset;
  FQry.SQL.Text := ASQL;

  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FQry;
  FDataSource.OnDataChange := DataSourceDataChange;

  PrepareForm();
end;

procedure TfrmGrid<T>.DataSourceDataChange(Sender: TObject; Field: TField);
begin
//
end;

destructor TfrmGrid<T>.Destroy;
begin
  FQry.Close;
  FQry.Free;
  FDataSource.Free;

  PObject(@FTable).Free;

  inherited;
end;

procedure TfrmGrid<T>.DoFilter;
begin
  Grd.DataSource.DataSet.Filtered := False;
  if Grd.DataSource.DataSet.Filter = '' then
    Grd.DataSource.DataSet.Filter := Grd.SelectedField.FieldName + '=' + QuotedStr(Grd.SelectedField.Value)
  else
    Grd.DataSource.DataSet.Filter := Grd.DataSource.DataSet.Filter + ' and ' + Grd.SelectedField.FieldName + '=' + QuotedStr(Grd.SelectedField.Value);
  Grd.DataSource.DataSet.Filtered := True;
  mniFilterRemove.Enabled := (Grd.DataSource.DataSet.Filter <> '');
  RefreshStatucRecorCount();
end;

procedure TfrmGrid<T>.DoRemoveFilter;
begin
  Grd.DataSource.DataSet.Filtered := False;
  Grd.DataSource.DataSet.Filter := '';
  mniFilterRemove.Enabled := False;
  RefreshStatucRecorCount();
end;

procedure TfrmGrid<T>.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmGrid<T>.FormCreate(Sender: TObject);
begin
//
end;

procedure TfrmGrid<T>.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
end;

procedure TfrmGrid<T>.FormKeyPress(Sender: TObject; var Key: Char);
var
  LKeyboardState: TKeyboardState;
  LShiftState: TShiftState;
begin
  if Key = Chr(VK_ESCAPE) then
  begin
    Self.Close;
  end;
end;

procedure TfrmGrid<T>.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//
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
var
  AGrid: TDBGrid;
begin
  if not (Sender is TDBGrid) then
    Exit;

  AGrid := (Sender as TDBGrid);
  if AGrid.Focused then
  begin
    if Shift = [ssCtrl, ssShift] then
    begin
      if (UpCase(Char(Key)) = 'C') then
      begin
        Clipboard.Clear;
        Clipboard.SetTextBuf(PWideChar(Grd.DataSource.DataSet.Fields.Fields[0].AsString + sLineBreak));
      end;
    end
    else if Shift = [ssCtrl] then
    begin
      if (UpCase(Char(Key)) = 'C') then
      begin
        Clipboard.Clear;
        Clipboard.SetTextBuf(PWideChar(Grd.SelectedField.AsString + sLineBreak));
      end;
    end;
  end;
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

procedure TfrmGrid<T>.SetContainer(const Value: TPanel);
begin
  FContainer := Value;
end;

procedure TfrmGrid<T>.SetFooter(const Value: TPanel);
begin
  FFooter := Value;
end;

procedure TfrmGrid<T>.SetGrd(const Value: TDBGrid);
begin
  FGrd := Value;
end;

procedure TfrmGrid<T>.SetHeader(const Value: TPanel);
begin
  FHeader := Value;
end;

procedure TfrmGrid<T>.SetPopupMenu(const Value: TPopupMenu);
begin
  FGridPopMenu := Value;
end;

procedure TfrmGrid<T>.SetmniExportCsv(const Value: TMenuItem);
begin
  FmniExportCsv := Value;
end;

procedure TfrmGrid<T>.SetmniExportExcel(const Value: TMenuItem);
begin
  FmniExportExcel := Value;
end;

procedure TfrmGrid<T>.SetmniFilter(const Value: TMenuItem);
begin
  FmniFilter := Value;
end;

procedure TfrmGrid<T>.SetmniFilterRemove(const Value: TMenuItem);
begin
  FmniFilterRemove := Value;
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

procedure TfrmGrid<T>.mniExportExcelClick(Sender: TObject);
begin
  ShowMessage('not implemented yet!' + sLineBreak + 'Export Excel');
end;

procedure TfrmGrid<T>.mniFilterClick(Sender: TObject);
begin
  DoFilter;
end;

procedure TfrmGrid<T>.mniFilterRemoveClick(Sender: TObject);
begin
  DoRemoveFilter;
end;

procedure TfrmGrid<T>.mniExportCsvClick(Sender: TObject);
begin
  ShowMessage('not implemented yet!' + sLineBreak + 'Export Csv');
end;

procedure TfrmGrid<T>.mniPreviewClick(Sender: TObject);
begin
  ShowMessage('not implemented yet!' + sLineBreak + 'Preview');
end;

procedure TfrmGrid<T>.mniPrintClick(Sender: TObject);
begin
  ShowMessage('not implemented yet!' + sLineBreak + 'Print');
end;

procedure TfrmGrid<T>.mniRemoveSortClick(Sender: TObject);
begin
  if Qry.IndexFieldNames <> '' then
    Qry.IndexFieldNames := '';
end;

procedure TfrmGrid<T>.OnFilterDataset(Dataset: TDataset; var Accept: Boolean);
begin
  //
end;

procedure TfrmGrid<T>.PrepareForm;
begin
  Self.Position := poScreenCenter;
  Self.KeyPreview := True;
  Self.Constraints.MinWidth := 640;
  Self.Constraints.MaxWidth := Monitor.Width;
  Self.Constraints.MinHeight := 480;
  Self.Constraints.MaxHeight := Monitor.Height;
  Self.Color := clRed;
  //form event
  Self.OnCreate := FormCreate;
  Self.OnShow := FormShow;
  Self.OnClose := FormClose;
  Self.OnKeyPress := FormKeyPress;
  Self.OnKeyDown := FormKeyDown;
  Self.OnKeyUp := FormKeyUp;

  Container := TPanel.Create(Self);
  Container.Parent := Self;
  Container.Align := alClient;
  Container.BevelOuter := bvNone;
  Container.ParentColor := True;
  Container.Visible := True;

  PrepareGrid;

  Header := TPanel.Create(Container);
  Header.Parent := Container;
  Header.Align := alTop;
  Header.BevelOuter := bvNone;
  Header.Height := 70;
  Header.ParentColor := True;
  Header.Visible := True;

  Footer := TPanel.Create(Container);
  Footer.Parent := Container;
  Footer.Align := alBottom;
  Footer.BevelOuter := bvNone;
  Footer.Height := 70;
  Footer.ParentColor := True;
  Footer.Visible := True;

  status := TStatusBar.Create(Self);
  status.Align := alBottom;
  status.Parent := Self;
  with status.Panels.Add do
  begin
    Width := 100;
  end;
end;

procedure TfrmGrid<T>.PrepareGrid;
begin
  PreparePopupMenu();

  Grd := TDBGrid.Create(Container);
  Grd.Parent := Container;
  Grd.Align := alClient;
  Grd.DataSource := FDataSource;
  Grd.Options := Grd.Options - [dgEditing, dgConfirmDelete];
  //grd events
  Grd.OnDblClick := grdDblClick;
  Grd.OnCellClick := grdCellClick;
  Grd.OnKeyPress := grdKeyPress;
  Grd.OnKeyDown := grdKeyDown;
  Grd.OnKeyUp := grdKeyUp;
  Grd.OnTitleClick := grdTitleClick;
  Grd.OnDrawColumnCell := grdDrawColumnCell;
  Grd.PopupMenu := Self.GridPopMenu;
end;

procedure TfrmGrid<T>.PreparePopupMenu;
begin
  GridPopMenu := TPopupMenu.Create(Self);
  mniPreview := AddMenu('Preview', 'mniPreview', mniPreviewClick);
  AddPopupMenuSpliter();
  mniFilter := AddMenu('Filter', 'mniFilter', mniFilterClick, True, TextToShortCut('F3'));
  mniFilterRemove := AddMenu('Remove Filter', 'mniFilterRemove', mniFilterRemoveClick, True, TextToShortCut('F8'));
  mniFilterRemove.Enabled := False;
  AddPopupMenuSpliter();
  mniExportExcel := AddMenu('Export Excel', 'mniExportExcel', mniExportExcelClick, True, TextToShortCut('Ctrl+E'));
  mniExportCsv := AddMenu('Export Csv File', 'mniExportCsv', mniExportCsvClick, True, TextToShortCut('Ctrl+Shift+E'));
  mniPrint := AddMenu('Print', 'mniPrint', mniPrintClick, True, TextToShortCut('Ctrl+P'));
  mniRemoveGridSort := AddMenu('Remove Sort', 'mniRemoveGridSort', mniRemoveSortClick);
  mniRemoveGridSort.Enabled := False;
end;

procedure TfrmGrid<T>.RefreshStatucRecorCount();
begin
  if status.Panels.Count > 0 then
    status.Panels.Items[0].Text := Format('Records: %d', [Grd.DataSource.DataSet.RecordCount]);
end;

procedure TfrmGrid<T>.SortGridTitle(Sender: TObject);
var
  sl: TStringList;
  LOrderList: string;
  LOrderedColumn: Boolean;
  nIndex: Integer;
  LColumn: TColumn;
  AQuery: TFDQuery;
begin
  if Sender is TColumn then
    LColumn := Sender as TColumn
  else
    Exit;

  LOrderedColumn := False;
  sl := TStringList.Create;
  try
    AQuery := TFDQuery(Grd.DataSource.DataSet);
    begin
      //sort düzenle
      sl.Delimiter := ';';
      if AQuery.IndexFieldNames <> '' then
        sl.DelimitedText := AQuery.IndexFieldNames;

      if KeyboardStateToShiftState() = [ssCtrl] then
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
        mniRemoveGridSort.Enabled := True;

      AQuery.IndexFieldNames := LOrderList;
    end;
  finally
    sl.Free;
  end;
end;

end.

