Unit Test.BADI.VB.EnumerateDecl;

Interface

Uses
  Test.BADI.Base.Module,
  BADI.VB.EnumerateDecl;

Type
  //
  // Test Class for the TVBEnumerateDecl Class Methods.
  //
  TestTVBEnumerateDecl = Class(TExtendedTestCase)
  Strict Private
    FVBEnumerateDecl : TVBEnumerateDecl;
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
// Test methods for the class TVBEnumerateDecl.
//
Procedure TestTVBEnumerateDecl.Setup;

Begin
  FVBEnumerateDecl := TVBEnumerateDecl.Create('MyEnumerate', scPrivate, 12, 23,
    iiPublicType, Nil);
End;

Procedure TestTVBEnumerateDecl.TearDown;

Begin
  FVBEnumerateDecl.Free;
End;

Procedure TestTVBEnumerateDecl.TestAsString;

Begin
  CheckEquals('Enum MyEnumerate', FVBEnumerateDecl.AsString(true, True));
End;

procedure TestTVBEnumerateDecl.TestCreate;
begin
  CheckEquals('MyEnumerate', FVBEnumerateDecl.Identifier);
  CheckEquals(scPrivate, FVBEnumerateDecl.Scope);
  CheckEquals(12, FVBEnumerateDecl.Line);
  CheckEquals(23, FVBEnumerateDecl.Column);
  CheckEquals(BADIImageIndex(iiPublicType, scPrivate), FVBEnumerateDecl.ImageIndexAdjustedForScope);
  Check(Nil = FVBEnumerateDecl.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBEnumerateDecl.Suite);
End.
