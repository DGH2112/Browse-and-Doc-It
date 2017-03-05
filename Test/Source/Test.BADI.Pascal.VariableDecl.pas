Unit Test.BADI.Pascal.VariableDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.VariableDecl;

Type
  TestTVar = Class(TExtendedTestCase)
  Strict Private
    FVar: TVar;
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

Procedure TestTVar.SetUp;
Begin
  FVar := TVar.Create('MyVar', scPrivate, 12, 23, iiPublicVariable, Nil);
  FVar.AddToken('Integer');
End;

Procedure TestTVar.TearDown;
Begin
  FVar.Free;
  FVar := Nil;
End;

Procedure TestTVar.TestAsString;
Begin
  Checkequals('MyVar : Integer', FVar.AsString(True, False));
End;

Procedure TestTVar.TestCreate;
Begin
  Checkequals('MyVar', FVar.Identifier);
  Checkequals(scPrivate, FVar.Scope);
  Checkequals(12, FVar.Line);
  Checkequals(23, FVar.Column);
  Checkequals(iiPublicVariable, FVar.ImageIndex);
  Checkequals(BADIImageIndex(iiPublicVariable, scPrivate), FVar.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTVar.Suite);
End.
