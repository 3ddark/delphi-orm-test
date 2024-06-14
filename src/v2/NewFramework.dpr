program NewFramework;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
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
  TStyleManager.TrySetStyle('Aqua Light Slate');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
