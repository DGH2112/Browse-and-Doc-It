(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
  
**)
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
