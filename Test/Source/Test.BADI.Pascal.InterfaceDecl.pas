Unit Test.BADI.Pascal.InterfaceDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.InterfaceDecl;

Type
  TestTInterfaceDecl = Class(TExtendedTestCase)
  Strict Private
    FInterfaceDecl: TInterfaceDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo,
  BADI.Functions;

Procedure TestTInterfaceDecl.SetUp;
Begin
  FInterfaceDecl := TInterfaceDecl.Create('MyInterface', scPublished, 12, 23,
    iiPublicInterface, Nil);
End;

Procedure TestTInterfaceDecl.TearDown;
Begin
  FInterfaceDecl.Free;
  FInterfaceDecl := Nil;
End;

Procedure TestTInterfaceDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FInterfaceDecl.ElementCount);
  FInterfaceDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FInterfaceDecl.ElementCount);
  CheckEquals('1) Interface type ''MyInterface'' is undocumented.', FInterfaceDecl.DocConflict(1));
  FInterfaceDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the Interface.', 0, 0);
  Try
    FInterfaceDecl.Comment := C;
    FInterfaceDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FInterfaceDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTInterfaceDecl.TestCreate;
Begin
  CheckEquals('MyInterface', FInterfaceDecl.Identifier);
  CheckEquals(scPublished, FInterfaceDecl.Scope);
  CheckEquals(12, FInterfaceDecl.Line);
  CheckEquals(23, FInterfaceDecl.Column);
  CheckEquals(iiPublicInterface, FInterfaceDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicInterface, scPublished), FInterfaceDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTInterfaceDecl.TestAsString;
Var
  H: TTokenInfo;
Begin
  CheckEquals('MyInterface = Interface', FInterfaceDecl.AsString(True, False));
  H := TTokenInfo.Create('IUnknown', 0, 0, 0, 7, ttIdentifier);
  Try
    FInterfaceDecl.Heritage.Add(H, scNone, iiNone, Nil);
    CheckEquals('MyInterface = Interface(IUnknown)', FInterfaceDecl.AsString(True, False));
  Finally
    H.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTInterfaceDecl.Suite);
End.
