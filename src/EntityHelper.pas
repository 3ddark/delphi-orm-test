unit EntityHelper;

interface

uses System.SysUtils, Generics.Collections, Entity;

type
  TEntityHelper<T> = class

  end;

implementation

class procedure TEntityHelper<T>.DestoryList(var AList: TList<T>);
var
  n1: Integer;
  AObject: T;
begin
  for n1 := 0 to AList.Count-1 do
  begin
    AObject := AList.Items[n1];
    FreeAndNil(AObject);
  end;

  FreeAndNil(AList);
end;

end.
