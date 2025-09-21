unit FilterCriterion;

interface

uses
  System.Rtti, System.Generics.Collections;

type
  TFilterCriterion = record
  private
    PropertyNamePath: string;
    Operator: string;
    Value: TValue;
  public
    class function New(const APropertyNamePath, AOperator: string; AValue: TValue): TFilterCriterion; static;
  end;

  TFilterCriteria = TList<TFilterCriterion>;

implementation

class function TFilterCriterion.New(const APropertyNamePath, AOperator: string; AValue: TValue): TFilterCriterion;
begin
  Result.PropertyNamePath := APropertyNamePath;
  Result.Operator := AOperator;
  Result.Value := AValue;
end;

end.
