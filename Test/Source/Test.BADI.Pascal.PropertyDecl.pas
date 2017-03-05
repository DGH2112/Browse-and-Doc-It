Unit Test.BADI.Pascal.PropertyDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.PropertyDecl;

Type
  TestTPascalProperty = Class(TExtendedTestCase)
  Strict Private
    FPascalProperty: TPascalProperty;
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
  BADI.Pascal.TypeDecl,
  BADI.Pascal.ParameterDecl,
  BADI.Functions;

Procedure TestTPascalProperty.SetUp;

Var
  T: TTypes;

Begin
  FPascalProperty := TPascalProperty.Create('MyProperty', scProtected, 12, 23,
    iiPublicProperty, Nil);
  T := TTypes.Create('String', scNone, 0, 0, iiNone, Nil);
  T.AddToken('String', ttIdentifier);
  FPascalProperty.ReturnType.Add(T);
End;

Procedure TestTPascalProperty.TearDown;
Begin
  FPascalProperty.Free;
  FPascalProperty := Nil;
End;

Procedure TestTPascalProperty.TestAsString;
Var
  AType: TTypes;
  P: TPascalParameter;
Begin
  Checkequals('Property MyProperty : String', FPascalProperty.AsString(True, False));
  Checkequals('Property MyProperty : String'#13#10, FPascalProperty.AsString(True, True));
  AType := TTypes.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('Integer');
    P := TPascalParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FPascalProperty.AddParameter(P);
  Finally
    AType.Free;
  End;
  Checkequals('Property MyProperty[Param1 : Integer] : String',
    FPascalProperty.AsString(True, False));
  Checkequals('Property MyProperty['#13#10#32#32'Param1 : Integer'#13#10'] : String'#13#10,
    FPascalProperty.AsString(True, True));
  AType := TTypes.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TPascalParameter.Create(pamNone, 'Param2', False, AType, '', scNone, 0, 0);
    FPascalProperty.AddParameter(P);
  Finally
    AType.Free;
  End;
  Checkequals('Property MyProperty[Param1 : Integer; Param2 : String] : String',
    FPascalProperty.AsString(True, False));
  Checkequals
    ('Property MyProperty['#13#10#32#32'Param1 : Integer;'#13#10#32#32'Param2 : String'#13#10'] : String'#13#10,
    FPascalProperty.AsString(True, True));
  FPascalProperty.ReadSpec := 'FValue';
  FPascalProperty.WriteSpec := 'FValue';
  Checkequals
    ('Property MyProperty[Param1 : Integer; Param2 : String] : String Read FValue Write FValue',
    FPascalProperty.AsString(True, False));
  Checkequals
    ('Property MyProperty['#13#10#32#32'Param1 : Integer;'#13#10#32#32'Param2 : String'#13#10'] : String'#13#10'  Read FValue'#13#10'  Write FValue'#13#10,
    FPascalProperty.AsString(True, True));
  FPascalProperty.ImplementsSpec.Add('mythingy', iiNone, scNone, Nil);
  FPascalProperty.ImplementsSpec.Add('mythingy2', iiNone, scNone, Nil);
  Checkequals
    ('Property MyProperty[Param1 : Integer; Param2 : String] : String Read FValue Write FValue Implements mythingy, mythingy2',
    FPascalProperty.AsString(True, False));
  Checkequals
    ('Property MyProperty['#13#10#32#32'Param1 : Integer;'#13#10#32#32'Param2 : String'#13#10'] : String'#13#10'  Read FValue'#13#10'  Write FValue'#13#10'  Implements mythingy, mythingy2'#13#10,
    FPascalProperty.AsString(True, True));
End;

Procedure TestTPascalProperty.TestCreate;
Begin
  Checkequals('MyProperty', FPascalProperty.Identifier);
  Checkequals(scProtected, FPascalProperty.Scope);
  Checkequals(12, FPascalProperty.Line);
  Checkequals(23, FPascalProperty.Column);
  Checkequals(iiPublicProperty, FPascalProperty.ImageIndex);
  Checkequals(BADIImageIndex(iiPublicProperty, scProtected), FPascalProperty.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalProperty.Suite);
End.
