Unit Test.BADI.Pascal.ObjectDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ObjectDecl;

Type
  TestTObjectDecl = Class(TExtendedTestCase)
  Strict Private
    FObjectDecl: TObjectDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
    Procedure TestReferenceSymbol;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo,
  BADI.Functions;

Procedure TestTObjectDecl.SetUp;
Begin
  FObjectDecl := TObjectDecl.Create('MyObject', scProtected, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTObjectDecl.TearDown;
Begin
  FObjectDecl.Free;
  FObjectDecl := Nil;
End;

Procedure TestTObjectDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;

Begin
  CheckEquals(0, FObjectDecl.ElementCount);
  FObjectDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FObjectDecl.ElementCount);
  CheckEquals('1) Object type ''MyObject'' is undocumented.', FObjectDecl.DocConflict(1));
  FObjectDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the object.', 0, 0);
  Try
    FObjectDecl.Comment := C;
    FObjectDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FObjectDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTObjectDecl.TestCreate;
Begin
  CheckEquals('MyObject', FObjectDecl.Identifier);
  CheckEquals(scProtected, FObjectDecl.Scope);
  CheckEquals(12, FObjectDecl.Line);
  CheckEquals(23, FObjectDecl.Column);
  CheckEquals(iiPublicObject, FObjectDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicObject, scProtected), FObjectDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTObjectDecl.TestAsString;
Begin
  CheckEquals('MyObject = Object', FObjectDecl.AsString(True, False));
End;

Procedure TestTObjectDecl.TestReferenceSymbol;
Var
  AToken: TTokenInfo;
Begin
  AToken := TTokenInfo.Create('Hello', 0, 0, 0, 5, ttIdentifier);
  Try
    CheckEquals(False, FObjectDecl.ReferenceSymbol(AToken));
    // : @todo Requires more tests.
  Finally
    AToken.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTObjectDecl.Suite);
End.
