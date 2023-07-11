unit SysUlkeler;

interface

uses
  Generics.Collections, SysUtils, Entity, EntityAttributes;

type
  TSysUlke = class;

  [Table('sys_sehirler', 'public')]
  TSysSehir = class(TEntity)
  private
    FSehir: string;
    FUlkeId: Int64;

    FUlke: TSysUlke;
  public
    [Column('sehir')]
    property Sehir: string read FSehir write FSehir;

    [Column('ulke_id')]
    property UlkeId: Int64 read FUlkeId write FUlkeId;

    [HasOne('Id', 'UlkeId')]
    property Ulke: TSysUlke read FUlke write FUlke;
  end;

  [Table('sys_ulkeler', 'public')]
  TSysUlke = class(TEntity)
  private
    FUlkeKodu: string;
    FUlkeAdi: string;
    FSehirler: TObjectList<TSysSehir>;
  public
    [Column('ulke_kodu', [cpUnique, cpNotNull], 2, 0, 0)]
    property UlkeKodu: string read FUlkeKodu write FUlkeKodu;

    [Column('ulke_adi', [cpNotNull], 128)]
    property UlkeAdi: string read FUlkeAdi write FUlkeAdi;

    [HasMany('UlkeId', 'Id')]
    property Sehirler: TObjectList<TSysSehir> read FSehirler write FSehirler;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

constructor TSysUlke.Create;
begin
  inherited;
  FSehirler := TObjectList<TSysSehir>.Create(True);
end;

destructor TSysUlke.Destroy;
begin
  FSehirler.Free;
  inherited;
end;

end.
