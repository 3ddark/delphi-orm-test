unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, ZAbstractRODataset, System.Generics.Collections,
  ZAbstractDataset, ZDataset, ZAbstractConnection, ZConnection,
  Ths.Erp.Database.Table, Ths.Erp.Database.Manager, Persons;

type
  TForm6 = class(TForm)
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    procedure FormCreate(Sender: TObject);
  private
    FManager: TEntityManager;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.FormCreate(Sender: TObject);
var
  n1: Integer;
  LPerson: TPerson;
  LPersons: TArray<TTable>;
  LAdres: TPersonAdres;
begin
  ZConnection1 := TZConnection.Create(nil);
  ZConnection1.Protocol := 'postgresql-9';
  ZConnection1.Database := 'ths_erp';
  ZConnection1.HostName := 'localhost';
  ZConnection1.User := 'postgres';
  ZConnection1.Password := 'qwe';
  ZConnection1.Connect;

  FManager := TEntityManager.Create(ZConnection1);

  LPerson := TPerson.Create;
  try
    FManager.LogicalSelect(LPerson.Id.QryName + '=10', True, True, False, TPerson.BusinessSelect);
//    FManager.GetOne(LPerson, 10, False);
//    LPerson.PersonName.Value := UpperCase(LPerson.PersonName.AsString);
//    FManager.Update(LPerson);
//    LPerson.PersonName.Value :=  LowerCase(LPerson.PersonName.AsString);
//    LPerson.PersonAge.Value := 27;
//    LPerson.Salary.Value := 350;
//    FManager.CustomUpdate(LPerson, [LPerson.PersonName, LPerson.PersonAge]);
//    FManager.GetList(TPerson, LPersons, 'id > 0', False);
  finally
    LPerson.DisposeOf;
//    for n1 := 0 to Length(LPersons)-1 do
//      LPersons[n1].DisposeOf;
//    SetLength(LPersons, 0);
  end;

  FManager.Free;
  ZConnection1.Disconnect;
  ZConnection1.DisposeOf;
end;

end.
