Unit Test.BADI.VB.VariableDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBVar

  TestTVBVar = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

uses
  BADI.VB.VariableDecl,
  BADI.Types;

procedure TestTVBVar.TestAsString;

var
  V : TVBVar;

begin
  V := TVBVar.Create('Identifier', scPrivate, 10, 12, iiPublicVariable, Nil);
  Try
    V.AddToken('String');
    CheckEquals('Identifier As String', V.AsString(True, False));
  Finally
    V.Free;
  End;
  V := TVBVar.Create('Identifier', scPrivate, 10, 12, iiPublicVariable, Nil);
  Try
    V.AddToken('MSForms');
    V.AddToken('.');
    V.AddToken('String');
    CheckEquals('Identifier As MSForms.String', V.AsString(True, False));
  Finally
    V.Free;
  End;
  V := TVBVar.Create('Identifier', scPrivate, 10, 12, iiPublicVariable, Nil);
  Try
    V.AddToken('String');
    V.WithEvents := True;
    CheckEquals('WithEvents Identifier As String', V.AsString(True, False));
  Finally
    V.Free;
  End;
  V := TVBVar.Create('Identifier', scPrivate, 10, 12, iiPublicVariable, Nil);
  Try
    V.AddDimension('', '');
    V.AddToken('String');
    CheckEquals('Identifier() As String', V.AsString(True, False));
  Finally
    V.Free;
  End;
  V := TVBVar.Create('Identifier', scPrivate, 10, 12, iiPublicVariable, Nil);
  Try
    V.AddDimension('1', '10');
    V.AddToken('MSForms');
    V.AddToken('.');
    V.AddToken('String');
    CheckEquals('Identifier(1 to 10) As MSForms.String', V.AsString(True, False));
  Finally
    V.Free;
  End;
  V := TVBVar.Create('Identifier', scPrivate, 10, 12, iiPublicVariable, Nil);
  Try
    V.AddDimension('1', '2');
    V.AddDimension('0', '4');
    V.AddToken('String');
    CheckEquals('Identifier(1 to 2, 0 to 4) As String', V.AsString(True, False));
  Finally
    V.Free;
  End;
end;

initialization
  RegisterTest('VB Module Tests', TestTVBVar.Suite);
End.
