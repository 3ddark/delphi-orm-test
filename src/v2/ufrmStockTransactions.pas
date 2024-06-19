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
  end;

var
  frmStockTransactions: TfrmStockTransactions;

implementation

{$R *.dfm}

constructor TfrmStockTransactions.Create(AOwner: TComponent);
begin
  inherited;
  inherited Create(AOwner, TStockTransaction.Create, TStockTransaction.GetSelectSQL);

  pnlHeader.Parent := Header;
  pnlFooter.Parent := Footer;
end;

procedure TfrmStockTransactions.FormShow(Sender: TObject);
begin
  inherited;
  Self.Caption := 'Stock Transactions on DBGRID';
end;

end.
