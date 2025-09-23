unit PersonRepository;

interface

uses Repository, Persons;

type
  TPersonRepository = class(TRepository<TPerson>)
  public
    //additional processes
    //procedure Someone
    //function AnotherOne: Boolean;
  end;

implementation

end.
