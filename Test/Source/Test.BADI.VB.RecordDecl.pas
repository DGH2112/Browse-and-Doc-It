Unit Test.BADI.VB.RecordDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBRecordDecl

  TestTVBRecordDecl = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

uses
  BADI.VB.RecordDecl,
  BADI.Types;

procedure TestTVBRecordDecl.TestAsString;

Var
  R : TVBRecordDecl;

begin
  R := TVBRecordDecl.Create('Identifier', scProtected, 12, 13, iiPublicRecord, Nil);
  Try
    CheckEquals('Type Identifier', R.AsString(True, False));
  Finally
    R.Free;
  End;
end;

initialization
  RegisterTest('VB Module Tests', TestTVBRecordDecl.Suite);
End.
