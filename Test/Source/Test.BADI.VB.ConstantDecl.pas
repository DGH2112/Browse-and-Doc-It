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
    C.AddToken('As');
    C.AddToken('Integer');
    CheckEquals('Identifier As Integer', C.AsString(True, False));
  Finally
    C.Free;
  End;
  C := TVBConstant.Create('Identifier', scPublic, 10, 12, iiPublicConstant, Nil);
  Try
    C.AddToken('As');
    C.AddToken('MSForms');
    C.AddToken('.');
    C.AddToken('Integer');
    CheckEquals('Identifier As MSForms.Integer', C.AsString(True, False));
  Finally
    C.Free;
  End;
  C := TVBConstant.Create('Identifier', scPublic, 10, 12, iiPublicConstant, Nil);
  Try
    C.AddToken('As');
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
