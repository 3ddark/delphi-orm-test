unit ufrmStockTransactions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ufrmGrid, StockTransactions,
  Vcl.ExtCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfrmStockTransactions = class(TfrmGrid<TStockTransaction>)
    pnlHeader: TPanel;
    pnlFooter: TPanel;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    procedure FormShow(Sender: TObject); override;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); reintroduce; overload;
    constructor Create(AOwner: TComponent; ATable: TStockTransaction; ASQL: string; Dummy: Integer = 0); reintroduce; overload;
  end;

var
  frmStockTransactions: TfrmStockTransactions;

implementation

{$R *.dfm}

constructor TfrmStockTransactions.Create(AOwner: TComponent; ATable: TStockTransaction;
  ASQL: string; Dummy: Integer);
begin
  inherited Create(AOwner, ATable, ASQL, Dummy);
end;

constructor TfrmStockTransactions.Create(AOwner: TComponent);
begin
  Create(AOwner, TStockTransaction.Create, TStockTransaction.GetSelectSQL);
end;

procedure TfrmStockTransactions.FormShow(Sender: TObject);
begin
  inherited;
  Self.Caption := 'Stock Transactions on DBGRID';
end;

end.
