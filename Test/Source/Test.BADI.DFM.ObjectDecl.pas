Unit Test.BADI.DFM.ObjectDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.DFM.ObjectDecl;

Type
  //
  // Test Class for the TDFMObject Class Methods.
  //
  TestTDFMObject = Class(TExtendedTestCase)
  Strict Private
    FDFMObject : TDFMObject;
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
// Test methods for the class TDFMObject.
//
Procedure TestTDFMObject.Setup;

Begin
  FDFMObject := TDFMObject.Create('Identifier', scPublic, 12, 23, iiPublicObject, Nil);
  FDFMObject.AddToken('TfrmMyForm');
End;

Procedure TestTDFMObject.TearDown;

Begin
  FDFMObject.Free;
End;

Procedure TestTDFMObject.TestAsString;

Begin
  Checkequals('Object Identifier : TfrmMyForm', FDFMObject.AsString(True, True));
End;

procedure TestTDFMObject.TestCreate;
begin
  CheckEquals('Identifier', FDFMObject.Identifier);
  CheckEquals(scPublic, FDFMObject.Scope);
  CheckEquals(12, FDFMObject.Line);
  CheckEquals(23, FDFMObject.Column);
  CheckEquals(BADIImageIndex(iiPublicObject, scPublic), FDFMObject.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('DFM Module', TestTDFMObject.Suite);
End.
