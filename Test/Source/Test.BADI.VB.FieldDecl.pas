Unit Test.BADI.VB.FieldDecl;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.FieldDecl;

Type
  //
  // Test Class for the TVBField Class Methods.
  //
  TestTVBField = Class(TExtendedTestCase)
  Strict Private
    FVBField : TVBField;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Functions,
  TestFramework;

//
// Test methods for the class TVBField.
//
Procedure TestTVBField.Setup;

Begin
  FVBField := TVBField.Create('MyField', scPublic, 12, 23, iiPublicField, Nil);
End;

Procedure TestTVBField.TearDown;

Begin
  FVBField.Free;
End;

Procedure TestTVBField.TestAsString;

Begin
  CheckEquals('MyField', FVBField.AsString(True, True));
  FVBField.AddToken('Long');
  CheckEquals('MyField As Long', FVBField.AsString(True, True));
End;

procedure TestTVBField.TestCreate;
begin
  CheckEquals('MyField', FVBField.Identifier);
  CheckEquals(scPublic, FVBField.Scope);
  CheckEquals(12, FVBField.Line);
  CheckEquals(23, FVBField.Column);
  CheckEquals(BADIImageIndex(iiPublicField, scPublic), FVBField.ImageIndexAdjustedForScope);
  Check(Nil = FVBField.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBField.Suite);
End.
