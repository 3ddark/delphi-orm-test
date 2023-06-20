unit ChBankaSubeleri;

interface

uses
  Entity, SysUtils;

type
  [EntityAttribute('ch_banka_subeleri', 'public')]
  TChBankaSubesi = class(TEntity)
  private
    FBankaID: Int64;
    FSubeKodu: Integer;
    FSubeAdi: string;
    FSubeSehirID: Int64;

//    FBanka: TChBanka;
  public
    [ColumnAttribute('banka_id', [], 0, 0, 0)]
    property BankaID: Int64 read FBankaID write FBankaID;
    [ColumnAttribute('sube_kodu', [], 0, 0, 0)]
    property SubeKodu: Integer read FSubeKodu write FSubeKodu;
    [ColumnAttribute('sube_adi', [cpNotNull], 64, 0, 0)]
    property SubeAdi: string read FSubeAdi write FSubeAdi;
    [NotMapped]
    property SubeSehirID: Int64 read FSubeSehirID write FSubeSehirID;

//    property Banka: TChBanka read FBanka write FBanka;

    constructor Create; override;
  end;

implementation

{ TBankaSubesi }

constructor TChBankaSubesi.Create;
begin
  inherited;
//  Self.Banka := TChBanka.Create;
end;

end.
