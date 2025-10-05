unit PersonService;

interface

uses
  System.SysUtils, System.Generics.Collections, SharedFormTypes,
  Entity, EntityAttributes, Repository, Service, PersonRepository, Persons;

type
  TPersonService = class(TService<TPerson>)
  public
    function CreateQueryForUI(const AFilterKey: string): string; override;
    function Find(AFilter: string; ALock: Boolean): TList<TPerson>; override;
    function FindById(AId: Int64; ALock: Boolean): TPerson; override;
    procedure Add(AEntity: TPerson); override;
    procedure Update(AEntity: TPerson); override;
    procedure Delete(AId: Int64); override;

    function BusinessFindById(AId: Int64; AWithBegin, ALock, APermissionControl: Boolean): TPerson; override;
    function BusinessFind(AFilter: string; AWithBegin, ALock, APermissionControl: Boolean): TList<TPerson>; override;
    procedure BusinessInsert(AEntity: TPerson; AWithBegin, AWithCommit, APermissionControl: Boolean); override;
    procedure BusinessUpdate(AEntity: TPerson; AWithBegin, AWithCommit, APermissionControl: Boolean); override;
    procedure BusinessDelete(AEntity: TPerson; AWithBegin, AWithCommit, APermissionControl: Boolean); override;
  end;

implementation

procedure TPersonService.Add(AEntity: TPerson);
begin
  Self.UoW.GetRepository<TPerson, TPersonRepository>.Add(AEntity);
end;

procedure TPersonService.BusinessDelete(AEntity: TPerson; AWithBegin, AWithCommit, APermissionControl: Boolean);
begin
  if not Self.UoW.IsAuthorized(ptDelete, APermissionControl) then
    Exit;

  if AWithBegin then
    Self.UoW.BeginTransaction;
  try
    Self.UoW.GetRepository<TPerson, TPersonRepository>.Delete(AEntity);
    if AWithCommit then
    begin
      if Self.UoW.InTransaction then
        Self.UoW.Commit;
    end;
  except
    on E: Exception do
    begin
      if Self.UoW.InTransaction then
        Self.UoW.Rollback;
      raise;
    end;
  end;
end;

function TPersonService.BusinessFind(AFilter: string; AWithBegin, ALock, APermissionControl: Boolean): TList<TPerson>;
begin
  Result := nil;
  if not Self.UoW.IsAuthorized(ptRead, APermissionControl) then
    Exit;
  Result := Self.UoW.GetRepository<TPerson, TPersonRepository>.Find(nil, ALock, [ioIncludeAll]);
end;

function TPersonService.BusinessFindById(AId: Int64; AWithBegin, ALock, APermissionControl: Boolean): TPerson;
begin
  Result := nil;
  if not Self.UoW.IsAuthorized(ptRead, APermissionControl) then
    Exit;
  Result := Self.UoW.GetRepository<TPerson, TPersonRepository>.FindById(AId, ALock, [ioIncludeAll]);
end;

procedure TPersonService.BusinessInsert(AEntity: TPerson; AWithBegin, AWithCommit, APermissionControl: Boolean);
begin
  if not Self.UoW.IsAuthorized(ptAddRecord, APermissionControl) then
    Exit;

  if AWithBegin then
    Self.UoW.BeginTransaction;
  try
    Self.UoW.GetRepository<TPerson, TPersonRepository>.Add(AEntity);
    if AWithCommit then
    begin
      if Self.UoW.InTransaction then
        Self.UoW.Commit;
    end;
  except
    on E: Exception do
    begin
      if Self.UoW.InTransaction then
        Self.UoW.Rollback;
      raise;
    end;
  end;
end;

procedure TPersonService.BusinessUpdate(AEntity: TPerson; AWithBegin, AWithCommit, APermissionControl: Boolean);
begin
  if not Self.UoW.IsAuthorized(ptUpdate, APermissionControl) then
    Exit;

  if AWithBegin then
    Self.UoW.BeginTransaction;
  try
    Self.UoW.GetRepository<TPerson, TPersonRepository>.Update(AEntity);
    if AWithCommit then
    begin
      if Self.UoW.InTransaction then
        Self.UoW.Commit;
    end;
  except
    on E: Exception do
    begin
      if Self.UoW.InTransaction then
        Self.UoW.Rollback;
      raise;
    end
  end;
end;

function TPersonService.CreateQueryForUI(const AFilterKey: string): string;
begin
 Result := '';
end;

procedure TPersonService.Delete(AId: Int64);
begin
  Self.UoW.GetRepository<TPerson, TPersonRepository>.Delete(AId);
end;

function TPersonService.Find(AFilter: string; ALock: Boolean): TList<TPerson>;
begin
  Result := Self.UoW.GetRepository<TPerson, TPersonRepository>.Find(nil, ALock);
end;

function TPersonService.FindById(AId: Int64; ALock: Boolean): TPerson;
begin
  Result := Self.UoW.GetRepository<TPerson, TPersonRepository>.FindById(AId, ALock);
end;

procedure TPersonService.Update(AEntity: TPerson);
begin
  Self.UoW.GetRepository<TPerson, TPersonRepository>.Update(AEntity);
end;

end.
