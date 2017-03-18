Unit Test.BADI.VB.Option;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.Option;

Type
  //
  // Test Class for the TVBOption Class Methods.
  //
  TestTVBOption = Class(TExtendedTestCase)
  Strict Private
    FVBOption : TVBOption;
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
// Test methods for the class TVBOption.
//
Procedure TestTVBOption.Setup;

Begin
  FVBOption := TVBOption.Create('Option', scNone, 12, 23, iiUsesItem, Nil);
End;

Procedure TestTVBOption.TearDown;

Begin
  FVBOption.Free;
End;

Procedure TestTVBOption.TestAsString;

Begin
  CheckEquals('Option', FVBOption.AsString(True, True));
End;

procedure TestTVBOption.TestCreate;
begin
  CheckEquals('Option', FVBOption.Identifier);
  CheckEquals(scNone, FVBOption.Scope);
  CheckEquals(12, FVBOption.Line);
  CheckEquals(23, FVBOption.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scNone), FVBOption.ImageIndexAdjustedForScope);
  Check(Nil = FVBOption.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBOption.Suite);
End.
