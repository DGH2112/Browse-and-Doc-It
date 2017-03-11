Unit Test.BADI.XML.XMLDocType;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.DocType;

Type
  //
  // Test Class for the TXMLDocType Class Methods.
  //
  TestTXMLDocType = Class(TExtendedTestCase)
  Strict Private
    FXMLDocType : TXMLDocType;
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
// Test Methods for Class TXMLDocType.
//
Procedure TestTXMLDocType.Setup;
Begin
  FXMLDocType := TXMLDocType.Create('!DOCTYPE', scNone, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTXMLDocType.TearDown;

Begin
  FXMLDocType.Free;
End;

Procedure TestTXMLDocType.TestAsString;

Begin
  CheckEquals('!DOCTYPE', FXMLDocType.AsString(True, True));
End;

procedure TestTXMLDocType.TestCreate;
begin
  CheckEquals('!DOCTYPE', FXMLDocType.Identifier);
  CheckEquals(scNone, FXMLDocType.Scope);
  CheckEquals(12, FXMLDocType.Line);
  CheckEquals(23, FXMLDocType.Column);
  CheckEquals(BADIImageIndex(iiPublicObject, scNone), FXMLDocType.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('XML Module', TestTXMLDocType.Suite);
End.
