unit CascadeHelper;

interface

uses Repository;

type
  // Cascade operation combinations i�in helper constants
  TCascadeHelper = class
  public
    const
      // Temel kombinasyonlar
      None: TCascadeOperations = [];
      Insert: TCascadeOperations = [coInsert];
      Update: TCascadeOperations = [coUpdate];
      Delete: TCascadeOperations = [coDelete];

      // �kili kombinasyonlar
      InsertUpdate: TCascadeOperations = [coInsert, coUpdate];
      InsertDelete: TCascadeOperations = [coInsert, coDelete];
      UpdateDelete: TCascadeOperations = [coUpdate, coDelete];

      // T�m operasyonlar
      All: TCascadeOperations = [coInsert, coUpdate, coDelete];
  end;

implementation

end.
