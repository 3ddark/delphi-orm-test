program NewFramework;

uses
  Vcl.Forms,
  ufrmDashboard in 'ufrmDashboard.pas' {frmDashboard},
  Ths.Orm.Table in 'Ths.Orm.Table.pas',
  Ths.Orm.Manager in 'Ths.Orm.Manager.pas',
  Logger in 'Logger.pas',
  Persons in 'Persons.pas',
  StockTransactions in 'StockTransactions.pas',
  Invoices in 'Invoices.pas',
  AccountTransactions in 'AccountTransactions.pas',
  Stocks in 'Stocks.pas',
  Ths.Orm.ManagerStack in 'Ths.Orm.ManagerStack.pas',
  ufrmGrid in 'UI\DBGrid\Base\ufrmGrid.pas',
  ufrmInvoices in 'UI\DBGrid\ufrmInvoices.pas',
  ufrmStockTransactions in 'ufrmStockTransactions.pas' {frmStockTransactions},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDashboard, frmDashboard);
  Application.Run;
end.
