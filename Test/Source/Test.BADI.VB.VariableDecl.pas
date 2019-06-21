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
