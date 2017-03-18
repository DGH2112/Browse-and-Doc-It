Unit Test.BADI.VB.ParameterDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBParameter

  TestTVBParameter = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

Uses
  BADI.VB.Parameter,
  BADI.VB.TypeDecl,
  BADI.Types;

procedure TestTVBParameter.TestAsString;

Var
  P : TVBParameter;
  ST : TVBTypeDecl;

begin
  ST := TVBTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    ST.AddToken('Double');
    P := TVBParameter.Create(pamNone, 'Identifier', False, ST, '', scNone, 10, 12);
    Try
      CheckEquals('Identifier As Double', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    ST.Free;
  End;
  ST := TVBTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    ST.AddToken('MSForms');
    ST.AddToken('.');
    ST.AddToken('Integer');
    P := TVBParameter.Create(pamVar, 'Identifier', False, ST, '', scNone, 10, 12);
    Try
      CheckEquals('ByRef Identifier As MSForms.Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    ST.Free;
  End;
  ST := TVBTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    ST.AddToken('Integer');
    P := TVBParameter.Create(pamConst, 'Identifier', False, ST, '', scNone, 10, 12);
    Try
      CheckEquals('ByVal Identifier As Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    ST.Free;
  End;
  ST := TVBTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    ST.AddToken('Integer');
    P := TVBParameter.Create(pamConst, 'Identifier', False, ST, '0', scNone, 10, 12);
    Try
      CheckEquals('ByVal Identifier As Integer = 0', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    ST.Free;
  End;
  ST := TVBTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    ST.AddToken('String');
    P := TVBParameter.Create(pamConst, 'Identifier', False, ST, '""', scNone, 10, 12);
    Try
      CheckEquals('ByVal Identifier As String = ""', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    ST.Free;
  End;
  ST := TVBTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    ST.AddToken('String');
    P := TVBParameter.Create(pamConst, 'Identifier', False, ST, '"Hello"', scNone, 10, 12);
    Try
      CheckEquals('ByVal Identifier As String = "Hello"', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    ST.Free;
  End;
end;

Initialization
  RegisterTest('VB Module Tests', TestTVBParameter.Suite);
End.
