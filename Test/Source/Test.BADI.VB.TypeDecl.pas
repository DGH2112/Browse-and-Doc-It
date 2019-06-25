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
