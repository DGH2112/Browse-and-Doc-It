Unit Test.BADI.VB.ConstantDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBConstant

  TestTVBConstant = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

uses
  BADI.VB.ConstantDecl,
  BADI.Types;

procedure TestTVBConstant.TestAsString;

var
  C : TVBConstant;

begin
  C := TVBConstant.Create('Identifier', scPublic, 10, 12, iiPublicConstant, Nil);
  Try
    C.AddToken('Integer');
    CheckEquals('Identifier As Integer', C.AsString(True, False));
  Finally
    C.Free;
  End;
  C := TVBConstant.Create('Identifier', scPublic, 10, 12, iiPublicConstant, Nil);
  Try
    C.AddToken('MSForms');
    C.AddToken('.');
    C.AddToken('Integer');
    CheckEquals('Identifier As MSForms.Integer', C.AsString(True, False));
  Finally
    C.Free;
  End;
  C := TVBConstant.Create('Identifier', scPublic, 10, 12, iiPublicConstant, Nil);
  Try
    C.AddToken('MSForms');
    C.AddToken('.');
    C.AddToken('Integer');
    C.AddToken('=');
    C.AddToken('44');
    CheckEquals('Identifier As MSForms.Integer = 44', C.AsString(True, False));
  Finally
    C.Free;
  End;
end;

initialization
  RegisterTest('VB Module Tests', TestTVBConstant.Suite);
End.
