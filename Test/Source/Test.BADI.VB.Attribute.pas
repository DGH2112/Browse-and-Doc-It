Unit Test.BADI.VB.Attribute;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.Attribute;

Type
  //
  // Test Class for the TVBAttribute Class Methods.
  //
  TestTVBAttribute = Class(TExtendedTestCase)
  Strict Private
    FVBAttribute : TVBAttribute;
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
// Test methods for the class TVBAttribute.
//
Procedure TestTVBAttribute.Setup;

Begin
  FVBAttribute := TVBAttribute.Create('Attribute', scPublic, 12, 23, iiUsesItem,
    Nil);
End;

Procedure TestTVBAttribute.TearDown;

Begin
  FVBAttribute.Free;
End;

Procedure TestTVBAttribute.TestAsString;

Begin
  CheckEquals('Attribute', FVBAttribute.AsString(True, True));
  FVBAttribute.AddToken('iThing');
  FVBAttribute.AddToken('=');
  FVBAttribute.AddToken('1');
  CheckEquals('Attribute iThing = 1', FVBAttribute.AsString(True, True));
End;

procedure TestTVBAttribute.TestCreate;
begin
  CheckEquals('Attribute', FVBAttribute.Identifier);
  CheckEquals(scPublic, FVBAttribute.Scope);
  CheckEquals(12, FVBAttribute.Line);
  CheckEquals(23, FVBAttribute.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scPublic), FVBAttribute.ImageIndexAdjustedForScope);
  Check(Nil = FVBAttribute.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBAttribute.Suite);
End.
