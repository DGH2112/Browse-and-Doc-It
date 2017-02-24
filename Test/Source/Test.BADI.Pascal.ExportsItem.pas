Unit Test.BADI.Pascal.ExportsItem;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ExportsItem;

Type
  TestTExportsItem = Class(TExtendedTestCase)
  Strict Private
    FExportsItem: TExportsItem;
  Public
    Procedure TestCreate;
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTExportsItem.SetUp;
Begin
  FExportsItem := TExportsItem.Create('MyMethod', scPublic, 12, 23, iiPublicExportedFunction, Nil);
End;

Procedure TestTExportsItem.TearDown;
Begin
  FExportsItem.Free;
  FExportsItem := Nil;
End;

Procedure TestTExportsItem.TestAsString;
Begin
  Checkequals('MyMethod', FExportsItem.AsString(True, False));
End;

Procedure TestTExportsItem.TestCreate;
Begin
  Checkequals('MyMethod', FExportsItem.Identifier);
  Checkequals(scPublic, FExportsItem.Scope);
  Checkequals(12, FExportsItem.Line);
  Checkequals(23, FExportsItem.Column);
  Checkequals(iiPublicExportedFunction, FExportsItem.ImageIndex);
  Checkequals(iiPublicExportedFunction, FExportsItem.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTExportsItem.Suite);
End.
