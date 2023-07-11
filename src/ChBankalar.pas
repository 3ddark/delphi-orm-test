unit ChBankalar;

interface

uses
  Entity, Generics.Collections, SysUtils, System.Types, EntityAttributes;

type
  TChBanka = class;

  [Table('ch_banka_subeleri', 'public')]
  TChBankaSubesi = class(TEntity)
  private
    FBankaId: Int64;
    FSubeKodu: Integer;
    FSubeAdi: string;
    FSubeSehirID: Int64;

    FBanka: TChBanka;
  public
    [Column('banka_id', [], 0, 0, 0)]
    property BankaId: Int64 read FBankaId write FBankaId;
    [Column('sube_kodu', [], 0, 0, 0)]
    property SubeKodu: Integer read FSubeKodu write FSubeKodu;
    [Column('sube_adi', [cpNotNull], 64, 0, 0)]
    property SubeAdi: string read FSubeAdi write FSubeAdi;
    [NotMapped]
    property SubeSehirID: Int64 read FSubeSehirID write FSubeSehirID;

    [HasOne('Id', 'BankaId')]
    property Banka: TChBanka read FBanka write FBanka;

    constructor Create; override;
  end;

  [Table('ch_bankalar', 'public')]
  TChBanka = class(TEntity)
  private
    FBankaAdi: string;
    FSwiftKodu: string;

    FBankaSubeleri: TObjectList<TChBankaSubesi>;
  public
    [Column('banka_adi', [cpUnique, cpNotNull], 64, 0, 0)]
    property BankaAdi: string read FBankaAdi write FBankaAdi;

    [Column('swift_kodu', [], 16, 0, 0)]
    property SwiftKodu: string read FSwiftKodu write FSwiftKodu;

    [HasMany('BankaId', 'Id')]
    property BankaSubeleri: TObjectList<TChBankaSubesi> read FBankaSubeleri write FBankaSubeleri;

    constructor Create; override;
  end;

implementation

constructor TChBanka.Create;
begin
  inherited;
  Self.FBankaSubeleri := TObjectList<TChBankaSubesi>.Create(True);
end;

constructor TChBankaSubesi.Create;
begin
  inherited;
  Self.Banka := TChBanka.Create;
end;

end.
