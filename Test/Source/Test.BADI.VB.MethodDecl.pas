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
Unit Test.BADI.VB.MethodDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBMethod

  TestTVBMethod = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

uses
  BADI.VB.MethodDecl,
  BADI.Types,
  BADI.VB.TypeDecl,
  BADI.VB.Parameter;

procedure TestTVBMethod.TestAsString;

Var
  M : TVBMethod;
  T: TVBTypeDecl;

begin
  M := TVBMethod.Create(mtProcedure, 'MyMethod', scPrivate, 10 ,12);
  Try
    CheckEquals('Sub MyMethod()', M.AsString(True, False));
    CheckEquals('Sub MyMethod('#13#10')', M.AsString(True, True));
  Finally
    M.Free;
  End;
  M := TVBMethod.Create(mtFunction, 'MyMethod', scPrivate, 10 ,12);
  Try
    T := TVBTypeDecl.Create('String', scNone, 10, 12, iiNone, Nil);
    T.AddToken('String');
    M.ReturnType.Add(T);
    CheckEquals('Function MyMethod() As String', M.AsString(True, False));
    CheckEquals('Function MyMethod('#13#10') As String', M.AsString(True, True));
  Finally
    M.Free;
  End;
  M := TVBMethod.Create(mtFunction, 'MyMethod', scPrivate, 10 ,12);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    M.ReturnType.Add(T);
    CheckEquals('Function MyMethod() As MSForms.Integer', M.AsString(True, False));
    CheckEquals('Function MyMethod('#13#10') As MSForms.Integer', M.AsString(true, True));
  Finally
    M.Free;
  End;
  M := TVBMethod.Create(mtFunction, 'MyMethod', scPrivate, 10 ,12);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    M.ReturnType.Add(T);
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('String');
      M.AddParameter(TVBParameter.Create(pamNone, 'Ident1', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Function MyMethod(Ident1 As String) As MSForms.Integer', M.AsString(True, False));
    CheckEquals('Function MyMethod('#13#10'  Ident1 As String'#13#10') As MSForms.Integer', M.AsString(True, True));
  Finally
    M.Free;
  End;
  M := TVBMethod.Create(mtFunction, 'MyMethod', scPrivate, 10 ,12);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    M.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('String');
      M.AddParameter(TVBParameter.Create(pamNone, 'Ident1', False, T, '"Hello"', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Function MyMethod(Ident1 As String = "Hello") As MSForms.Integer', M.AsString(True, False));
    CheckEquals('Function MyMethod('#13#10'  Ident1 As String = "Hello"'#13#10') As MSForms.Integer', M.AsString(True, True));
  Finally
    M.Free;
  End;
  M := TVBMethod.Create(mtFunction, 'MyMethod', scPrivate, 10 ,12);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    M.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('MSForms');
      T.AddToken('.');
      T.AddToken('Integer');
      M.AddParameter(TVBParameter.Create(pamVar, 'Ident1', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('Integer');
      M.AddParameter(TVBParameter.Create(pamConst, 'Ident2', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Function MyMethod(ByRef Ident1 As MSForms.Integer, ByVal Ident2 As Integer) As MSForms.Integer', M.AsString(True, False));
    CheckEquals('Function MyMethod('#13#10'  ByRef Ident1 As MSForms.Integer,'#13#10'  ByVal Ident2 As Integer'#13#10') As MSForms.Integer', M.AsString(True, True));
  Finally
    M.Free;
  End;
  M := TVBMethod.Create(mtFunction, 'MyMethod', scPrivate, 10 ,12);
  Try
    M.Ext := '"Kernal32"';
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    M.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('String');
      M.AddParameter(TVBParameter.Create(pamNone, 'Ident1', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Function MyMethod Lib "Kernal32" (Ident1 As String) As MSForms.Integer', M.AsString(True, False));
    CheckEquals('Function MyMethod Lib "Kernal32" ('#13#10'  Ident1 As String'#13#10') As MSForms.Integer', M.AsString(True, True));
  Finally
    M.Free;
  End;
end;

initialization
  RegisterTest('VB Module Tests', TestTVBMethod.Suite);
End.
