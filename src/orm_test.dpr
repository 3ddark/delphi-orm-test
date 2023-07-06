program orm_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Generics.Collections,
  ZConnection,
  Entity in 'Entity.pas',
  EntityManager in 'EntityManager.pas',
  ChBankalar in 'ChBankalar.pas',
  SysGunler in 'SysGunler.pas',
  EntityAttributes in 'EntityAttributes.pas';

var
  LConn: TZConnection;
  LMan: TEntityManager;
  LBank: TChBanka;
  LBanks: TObjectList<TChBanka>;
  LBankBranch: TChBankaSubesi;
  lstr: string;
  LID: Int64;
begin
  try
    LConn := TZConnection.Create(nil);
    LConn.Protocol := 'postgresql-9';
    LConn.Database := 'ths_erp';
    LConn.HostName := 'localhost';
    LConn.User := 'postgres';
    LConn.Password := 'qwe';
    LConn.Connect;

    LMan := TEntityManager.Create(LConn);
    LID := 2;
    LBank := LMan.GetByOne<TChBanka>(LID);
    Writeln('Get Bank By Id ' + LID.ToString);
    Writeln(Format('%s, %s', [LBank.ID.ToString, LBank.BankaAdi]));
    for LBankBranch in LBank.BankaSubeleri do
      Writeln(Format('%s%s %s %s', [#9, LBankBranch.ID.ToString, LBankBranch.SubeKodu.ToString, LBankBranch.SubeAdi]));

    LID := 1;
    Writeln(sLineBreak + 'Get Bank Brach By Id ' + LID.ToString);
    LBankBranch := LMan.GetByOne<TChBankaSubesi>(LID);
    Writeln(Format('%s %s %s', [LBankBranch.ID.ToString, LBankBranch.SubeKodu.ToString, LBankBranch.SubeAdi]));
    Writeln(Format('%s%s %s', [#9, LBankBranch.Banka.ID.ToString, LBankBranch.Banka.BankaAdi]));

    Writeln(sLineBreak + 'Get All Banks');
    LBanks := LMan.GetList<TChBanka>('');
    for LBank in LBanks do
    begin
      Writeln(Format('%s %s', [LBank.ID.ToString, LBank.BankaAdi]));
      for LBankBranch in LBank.BankaSubeleri do
        Writeln(Format('%s%s %s %s', [#9,LBankBranch.ID.ToString, LBankBranch.SubeKodu.ToString, LBankBranch.SubeAdi]));
    end;

    LMan.Free;

    Readln(lstr);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
