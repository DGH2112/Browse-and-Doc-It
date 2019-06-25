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
Unit Test.BADI.Pascal.Parameter;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module;

Type
  TestTPascalParameter = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Pascal.TypeDecl,
  BADI.Pascal.ParameterDecl;

Procedure TestTPascalParameter.TestAsString;

Var
  P: TPascalParameter;
  AType: TInteger;

Begin
  AType := TInteger.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('Integer');
    P := TPascalParameter.Create(pamNone, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamVar, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('var MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamConst, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('const MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamOut, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('out MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamNone, 'MyParam', True, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('MyParam : Array Of Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamNone, 'MyParam', False, AType, '0', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('MyParam : Integer = 0', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    AType.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalParameter.Suite);
End.
