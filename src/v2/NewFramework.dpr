program NewFramework;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  Ths.Erp.Database.Table in 'Ths.Erp.Database.Table.pas',
  Ths.Erp.Database.Manager in 'Ths.Erp.Database.Manager.pas',
  Logger in 'Logger.pas',
  Persons in 'Persons.pas',
  StockTransactions in 'StockTransactions.pas',
  Invoices in 'Invoices.pas',
  AccountTransactions in 'AccountTransactions.pas',
  Stocks in 'Stocks.pas',
  Ths.Erp.Database.ManagerStack in 'Ths.Erp.Database.ManagerStack.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
