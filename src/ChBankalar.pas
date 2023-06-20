unit ChBankalar;

interface

uses
  Entity, Generics.Collections, SysUtils, System.Types;

type
  TChBanka = class;

  [EntityAttribute('ch_banka_subeleri', 'public')]
  TChBankaSubesi = class(TEntity)
  private
    FBankaID: Int64;
    FSubeKodu: Integer;
    FSubeAdi: string;
    FSubeSehirID: Int64;

    FBanka: TChBanka;
  public
    [ColumnAttribute('banka_id', [], 0, 0, 0)]
    property BankaID: Int64 read FBankaID write FBankaID;
    [ColumnAttribute('sube_kodu', [], 0, 0, 0)]
    property SubeKodu: Integer read FSubeKodu write FSubeKodu;
    [ColumnAttribute('sube_adi', [cpNotNull], 64, 0, 0)]
    property SubeAdi: string read FSubeAdi write FSubeAdi;
    [NotMapped]
    property SubeSehirID: Int64 read FSubeSehirID write FSubeSehirID;

    property Banka: TChBanka read FBanka write FBanka;

    constructor Create; override;
  end;

  [EntityAttribute('ch_bankalar', 'public')]
  TChBanka = class(TEntity)
  private
    FBankaAdi: string;
    FSwiftKodu: string;

    FBankaSubeleri: TList<TChBankaSubesi>;
  public
    [ColumnAttribute('banka_adi', [cpUnique, cpNotNull], 64, 0, 0)]
    property BankaAdi: string read FBankaAdi write FBankaAdi;

    [ColumnAttribute('swift_kodu', [], 16, 0, 0)]
    property SwiftKodu: string read FSwiftKodu write FSwiftKodu;

    property BankaSubeleri: TList<TChBankaSubesi> read FBankaSubeleri write FBankaSubeleri;

    constructor Create; override;
  end;

implementation

constructor TChBanka.Create;
begin
  inherited;
  Self.FBankaSubeleri := TList<TChBankaSubesi>.Create;
end;

constructor TChBankaSubesi.Create;
begin
  inherited;
  Self.Banka := TChBanka.Create;
end;

end.
