Unit Test.BADI.XML.XMLDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.XMLDecl;

Type
  //
  // Test Class for the TXMLDecl Class Methods.
  //
  TestTXMLDecl = Class(TExtendedTestCase)
  Strict Private
    FXMLDecl : TXMLDecl;
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
// Test Methods for Class TXMLDecl.
//
Procedure TestTXMLDecl.Setup;
Begin
  FXMLDecl := TXMLDecl.Create('xml', scNone, 12, 23, iiPublicType, Nil);
End;

Procedure TestTXMLDecl.TearDown;

Begin
  FXMLDecl.Free;
End;

Procedure TestTXMLDecl.TestAsString;

Begin
  CheckEquals('xml', FXMLDecl.AsString(True, True));
End;

procedure TestTXMLDecl.TestCreate;
begin
  CheckEquals('xml', FXMLDecl.Identifier);
  CheckEquals(scNone, FXMLDecl.Scope);
  CheckEquals(12, FXMLDecl.Line);
  CheckEquals(23, FXMLDecl.Column);
  CheckEquals(BADIImageIndex(iiPublicType, scNone), FXMLDecl.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('XML Module', TestTXMLDecl.Suite);
End.
