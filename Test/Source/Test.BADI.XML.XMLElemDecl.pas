Unit Test.BADI.XML.XMLElemDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.XMLElemDecl;

Type
  //
  // Test Class for the TXMLElemDecl Class Methods.
  //
  TestTXMLElemDecl = Class(TExtendedTestCase)
  Strict Private
    FXMLElemDecl: TXMLElemDecl;
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
// Test Methods for Class TXMLElemDecl.
//
Procedure TestTXMLElemDecl.SetUp;
Begin
  FXMLElemDecl := TXMLElemDecl.Create('!ELEMENT', scNone, 12, 23, iiPublicObject, Nil);
  FXMLElemDecl.AddToken('Element');
End;

Procedure TestTXMLElemDecl.TearDown;

Begin
  FXMLElemDecl.Free;
End;

Procedure TestTXMLElemDecl.TestAsString;

Begin
  CheckEquals('!ELEMENT Element', FXMLElemDecl.AsString(True, True));
End;

Procedure TestTXMLElemDecl.TestCreate;
Begin
  CheckEquals('!ELEMENT', FXMLElemDecl.Identifier);
  CheckEquals(scNone, FXMLElemDecl.Scope);
  CheckEquals(12, FXMLElemDecl.Line);
  CheckEquals(23, FXMLElemDecl.Column);
  CheckEquals(
  BADIImageIndex(iiPublicObject, scNone), FXMLElemDecl.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('XML Module', TestTXMLElemDecl.Suite);
End.
