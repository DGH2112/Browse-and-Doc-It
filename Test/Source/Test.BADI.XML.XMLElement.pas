Unit Test.BADI.XML.XMLElement;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.XMLElement;

Type
  //
  // Test Class for the TXMLElement Class Methods.
  //
  TestTXMLElement = Class(TExtendedTestCase)
  Strict Private
    FXMLElement : TXMLElement;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreate;
    Procedure TestGetName;
  End;


Implementation

Uses
  BADI.Types,
  BADI.Functions;

//
// Test Methods for Class TXMLElement.
//
Procedure TestTXMLElement.Setup;
Begin
  FXMLElement := TXMLElement.Create('xml', scNone, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTXMLElement.TearDown;

Begin
  FXMLElement.Free;
End;

Procedure TestTXMLElement.TestAsString;

Begin
  CheckEquals('<xml></xml>', FXMLElement.AsString(True, True));
End;

Procedure TestTXMLElement.TestCreate;

Begin
  CheckEquals('xml', FXMLElement.Identifier);
  CheckEquals(scNone, FXMLElement.Scope);
  CheckEquals(12, FXMLElement.Line);
  CheckEquals(23, FXMLElement.Column);
  CheckEquals(BADIImageIndex(iiPublicObject, scNone), FXMLElement.ImageIndexAdjustedForScope);
End;

Procedure TestTXMLElement.TestGetName;

Begin
  CheckEquals('xml:0012:0023', FXMLElement.GetName);
End;

Initialization
  RegisterTest('XML Module', TestTXMLElement.Suite);
End.
