Unit Test.BADI.LabelContainer;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.ElementContainer;

Type
  TestTLabelContainer = Class(TExtendedTestCase)
  Strict Private
    FLabelContainer: TLabelContainer;
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
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Options;

Procedure TestTLabelContainer.SetUp;

Begin
  FLabelContainer := TLabelContainer.Create(strImplementedMethodsLabel, scNone, 12, 23,
    iiImplementedMethods, Nil);
  TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options - [doShowChildCountInTitles];
End;

Procedure TestTLabelContainer.TearDown;
Begin
  FLabelContainer.Free;
  FLabelContainer := Nil;
End;

Procedure TestTLabelContainer.TestAsString;
Begin
  CheckEquals(strImplementedMethodsLabel, FLabelContainer.AsString(True, False));
End;

Procedure TestTLabelContainer.TestCreate;
Begin
  CheckEquals(strImplementedMethodsLabel, FLabelContainer.Identifier);
  CheckEquals(scNone, FLabelContainer.Scope);
  CheckEquals(12, FLabelContainer.Line);
  CheckEquals(23, FLabelContainer.Column);
  CheckEquals(BADIImageIndex(iiImplementedMethods, scNone), FLabelContainer.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTLabelContainer.Suite);
End.
