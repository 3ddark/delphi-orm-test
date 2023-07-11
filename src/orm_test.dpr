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
  EntityAttributes in 'EntityAttributes.pas',
  SysUlkeler in 'SysUlkeler.pas';

var
  LConn: TZConnection;
  LMan: TEntityManager;
  LBank: TChBanka;
  LBanks: TObjectList<TChBanka>;
  LBankBranch: TChBankaSubesi;
  lstr: string;
  LID: Int64;

  LCountry: TSysUlke;
  LCountries: TObjectList<TSysUlke>;
  LCity: TSysSehir;
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
    LMan.StartTrans();

    LCountries := TObjectList<TSysUlke>.Create(True);

    LCountries := LMan.GetList<TSysUlke>(' and ulke_kodu in (''DX'', ''TX'', ''XX'')');
    //LMan.DeleteBatch<TSysUlke>(' and ulke_kodu in (''DX'', ''TX'', ''XX'')');

    for LCountry in LCountries do
      LMan.Delete<TSysUlke>(LCountry);

    LCountry := TSysUlke.Create;
    LCountry.UlkeKodu := 'YX';
    LCountry.UlkeAdi := 'Almanya';
    LCountries.Add(LCountry);

    LCountry := TSysUlke.Create;
    LCountry.UlkeKodu := 'ZX';
    LCountry.UlkeAdi := 'Türkiyeee';
    LCountries.Add(LCountry);

    LCountry := TSysUlke.Create;
    LCountry.UlkeKodu := 'RX';
    LCountry.UlkeAdi := 'Azeri';
    LCountries.Add(LCountry);

    LMan.AddBatch<TSysUlke>(LCountries.ToArray);

    for LCountry in LCountries do
      Writeln(LCountry.UlkeAdi);

    for LCountry in LCountries do
      LCountry.UlkeAdi := UpperCase(LCountry.UlkeAdi);

    LMan.UpdateBatch<TSysUlke>(LCountries.ToArray);

    LCountries.Clear;

    LCountries := LMan.GetList<TSysUlke>('');

    for LCountry in LCountries do
      Writeln(LCountry.UlkeAdi);

    LMan.CommitTrans();
    LMan.Free;

    Readln(lstr);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
