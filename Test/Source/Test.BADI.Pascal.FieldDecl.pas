Unit Test.BADI.Pascal.FieldDecl;

Interface

Uses
  TestFramework,
  Test.BADI.base.Module,
  BADI.Pascal.FieldDecl;

Type
  TestTField = Class(TExtendedTestCase)
  Strict Private
    FField: TField;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestCheckDocumentation;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment;

Procedure TestTField.SetUp;
Begin
  FField := TField.Create('MyField', scPublic, 12, 23, iiPublicField, Nil);
  FField.AddToken('Integer');
End;

Procedure TestTField.TearDown;
Begin
  FField.Free;
  FField := Nil;
End;

Procedure TestTField.TestAsString;
Begin
  CheckEquals('MyField : Integer', FField.AsString(True, False));
End;

Procedure TestTField.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FField.ElementCount);
  FField.CheckDocumentation(boolCascade);
  CheckEquals(1, FField.ElementCount);
  CheckEquals('1) Field ''MyField'' is undocumented.', FField.DocConflict(1));
  FField.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the field.', 0, 0);
  Try
    FField.Comment := C;
    FField.CheckDocumentation(boolCascade);
    CheckEquals(0, FField.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTField.TestCreate;
Begin
  CheckEquals('MyField', FField.Identifier);
  CheckEquals(scPublic, FField.Scope);
  CheckEquals(12, FField.Line);
  CheckEquals(23, FField.Column);
  CheckEquals(iiPublicField, FField.ImageIndex);
  CheckEquals(iiPublicField, FField.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTField.Suite);
End.
