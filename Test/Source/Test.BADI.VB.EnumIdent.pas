Unit Test.BADI.VB.EnumIdent;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.EnumIdent;

Type
  //
  // Test Class for the TVBEnumIdent Class Methods.
  //
  TestTVBEnumIdent = Class(TExtendedTestCase)
  Strict Private
    FVBEnumIdent : TVBEnumIdent;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

uses
  BADI.Types,
  BADI.Functions,
  TestFramework;

//
// Test methods for the class TVBEnumIdent.
//
Procedure TestTVBEnumIdent.Setup;

Begin
  FVBEnumIdent := TVBEnumIdent.Create('MyEnumIdent', scPublic, 12, 23,
    iiUsesItem, Nil);
End;

Procedure TestTVBEnumIdent.TearDown;

Begin
  FVBEnumIdent.Free;
End;

Procedure TestTVBEnumIdent.TestAsString;

Begin
  CheckEquals('MyEnumIdent', FVBEnumIdent.AsString(true, True));
  FVBEnumIdent.AddToken('1');
  CheckEquals('MyEnumIdent = 1', FVBEnumIdent.AsString(true, True));
End;

procedure TestTVBEnumIdent.TestCreate;
begin
  CheckEquals('MyEnumIdent', FVBEnumIdent.Identifier);
  CheckEquals(scPublic, FVBEnumIdent.Scope);
  CheckEquals(12, FVBEnumIdent.Line);
  CheckEquals(23, FVBEnumIdent.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scPublic), FVBEnumIdent.ImageIndexAdjustedForScope);
  Check(Nil = FVBEnumIdent.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBEnumIdent.Suite);
End.
