unit ufrmInvoices;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ComCtrls,
  Vcl.DBGrids, ufrmGrid, ZDataset;

type
  TfrmInvoices<T> = class(TfrmGrid<T>)
  private

  published
    procedure FormShow(Sender: TObject); override;
  public

  end;

implementation

procedure TfrmInvoices<T>.FormShow(Sender: TObject);
begin
  inherited;
  Self.Caption := 'Invoices on DBGRID';
end;

end.
