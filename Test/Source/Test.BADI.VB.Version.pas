Unit Test.BADI.VB.Version;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.Version;

Type
  //
  // Test Class for the TVBVersion Class Methods.
  //
  TestTVBVersion = Class(TExtendedTestCase)
  Strict Private
    FVBVersion : TVBVersion;
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
// Test methods for the class TVBVersion.
//
Procedure TestTVBVersion.Setup;

Begin
  FVBVersion := TVBVersion.Create('Version', scPublic, 12, 23, iiUsesItem, Nil);
End;

Procedure TestTVBVersion.TearDown;

Begin
  FVBVersion.Free;
End;

Procedure TestTVBVersion.TestAsString;

Begin
  CheckEquals('Version', FVBVersion.AsString(True, True));
  FVBVersion.AddToken('1.0');
  FVBVersion.AddToken('Class');
  CheckEquals('Version 1.0 Class', FVBVersion.AsString(True, True));
End;

procedure TestTVBVersion.TestCreate;
begin
  CheckEquals('Version', FVBVersion.Identifier);
  CheckEquals(scPublic, FVBVersion.Scope);
  CheckEquals(12, FVBVersion.Line);
  CheckEquals(23, FVBVersion.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scPublic), FVBVersion.ImageIndexAdjustedForScope);
  Check(Nil = FVBVersion.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBVersion.Suite);
End.
