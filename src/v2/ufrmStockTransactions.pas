unit ufrmStockTransactions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ufrmGrid, StockTransactions,
  Vcl.ExtCtrls;

type
  TfrmStockTransactions = class(TForm)
    pnlHeader: TPanel;
    pnlFooter: TPanel;
    pnlContent: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStockTransactions: TfrmStockTransactions;

implementation

{$R *.dfm}

procedure TfrmStockTransactions.FormShow(Sender: TObject);
var
  LForm: TfrmGrid<TStockTransaction>;
begin
  LForm := TfrmGrid<TStockTransaction>.Create(pnlContent, TStockTransaction.Create, TStockTransaction.GetSelectSQL);
  LForm.Grd.Parent := pnlContent;
  LForm.Show;
end;

end.
