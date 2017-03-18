Unit Test.BADI.VB.TypeDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBTypeDecl

  TestTVBTypeDecl = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

uses
  BADI.VB.TypeDecl,
  BADI.Types;

procedure TestTVBTypeDecl.TestAsString;

Var
  T : TVBTypeDecl;

begin
  T := TVBTypeDecl.Create('temp', scNone, 0, 0, iiNone, Nil);
  Try
    T.AddToken('MSForm');
    CheckEquals('MSForm', T.AsString(False, False));
  Finally
    T.Free;
  End;
  T := TVBTypeDecl.Create('temp', scNone, 0, 0, iiNone, Nil);
  Try
    T.AddToken('MSForm');
    T.AddToken('.');
    T.AddToken('Integer');
    CheckEquals('MSForm.Integer', T.AsString(False, False));
  Finally
    T.Free;
  End;
end;

initialization
  RegisterTest('VB Module Tests', TestTVBTypeDecl.Suite);
End.
