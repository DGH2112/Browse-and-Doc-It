Unit Test.BADI.DFM.PropertyDecl;

Interface

Uses
  TestFramework,
  Test.BADI.base.Module,
  BADI.DFM.PropertyDecl;

Type
  //
  // Test Class for the TDFMProperty Class Methods.
  //
  TestTDFMProperty = Class(TExtendedTestCase)
  Strict Private
    FDFMProperty: TDFMProperty;
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
  BADI.Functions;

//
// Test methods for the class TDFMProperty.
//
Procedure TestTDFMProperty.SetUp;

Begin
  FDFMProperty := TDFMProperty.Create('MyIdentifier', scPublic, 12, 23, iiPublicProperty, Nil);
  FDFMProperty.AddToken('AnIdentifier');
End;

Procedure TestTDFMProperty.TearDown;

Begin
  FDFMProperty.Free;
End;

Procedure TestTDFMProperty.TestAsString;

Begin
  CheckEquals('MyIdentifier = AnIdentifier', FDFMProperty.AsString(True, True));
End;

Procedure TestTDFMProperty.TestCreate;
Begin
  CheckEquals('MyIdentifier', FDFMProperty.Identifier);
  CheckEquals(scPublic, FDFMProperty.Scope);
  CheckEquals(12, FDFMProperty.Line);
  CheckEquals(23, FDFMProperty.Column);
  CheckEquals(BADIImageIndex(iiPublicProperty, scPublic), FDFMProperty.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('DFM Module', TestTDFMProperty.Suite);
End.
