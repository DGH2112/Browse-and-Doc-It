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
Unit Test.BADI.VB.PropertyDecl;

Interface

Uses
  TestFramework;

Type
  // Test methods for class TVBProperty

  TestTVBProperty = class(TTestCase)
  strict private
  public
  published
    procedure TestAsString;
  end;

Implementation

uses
  BADI.VB.PropertyDecl,
  BADI.VB.TypeDecl,
  BADI.VB.Types,
  BADI.Types,
  BADI.VB.Parameter;

procedure TestTVBProperty.TestAsString;

var
  P : TVBProperty;
  T : TVBTypeDecl;

begin
  P := TVBProperty.Create(ptLet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    CheckEquals('Property Let MyProperty()', P.AsString(True, False));
    CheckEquals('Property Let MyProperty('#13#10')', P.AsString(True, True));
  Finally
    P.Free;
  End;
  P := TVBProperty.Create(ptSet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    CheckEquals('Property Set MyProperty()', P.AsString(True, False));
    CheckEquals('Property Set MyProperty('#13#10')', P.AsString(True, True));
  Finally
    P.Free;
  End;
  P := TVBProperty.Create(ptGet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    T := TVBTypeDecl.Create('String', scNone, 10, 12, iiNone, Nil);
    P.ReturnType.Add(T);
    T.AddToken('String');
    CheckEquals('Property Get MyProperty() As String', P.AsString(True, False));
    CheckEquals('Property Get MyProperty('#13#10') As String', P.AsString(True, True));
  Finally
    P.Free;
  End;
  P := TVBProperty.Create(ptGet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    P.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    CheckEquals('Property Get MyProperty() As MSForms.Integer', P.AsString(True, False));
    CheckEquals('Property Get MyProperty('#13#10') As MSForms.Integer', P.AsString(True, True));
  Finally
    P.Free;
  End;
  P := TVBProperty.Create(ptGet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    P.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('String');
      P.AddParameter(TVBParameter.Create(pamNone, 'Ident1', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Property Get MyProperty(Ident1 As String) As MSForms.Integer', P.AsString(True, False));
    CheckEquals('Property Get MyProperty('#13#10'  Ident1 As String'#13#10') As MSForms.Integer', P.AsString(True, True));
  Finally
    P.Free;
  End;
  P := TVBProperty.Create(ptGet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    P.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('String');
      P.AddParameter(TVBParameter.Create(pamVar, 'Ident1', False, T, '""', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Property Get MyProperty(ByRef Ident1 As String = "") As MSForms.Integer', P.AsString(True, False));
    CheckEquals('Property Get MyProperty('#13#10'  ByRef Ident1 As String = ""'#13#10') As MSForms.Integer', P.AsString(True, True));
  Finally
    P.Free;
  End;
  P := TVBProperty.Create(ptGet, 'MyProperty', scPrivate, 10 ,12,
    iiPublicProperty, Nil);
  Try
    T := TVBTypeDecl.Create('Integer', scNone, 10, 12, iiNone, Nil);
    P.ReturnType.Add(T);
    T.AddToken('MSForms');
    T.AddToken('.');
    T.AddToken('Integer');
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('MSForms');
      T.AddToken('.');
      T.AddToken('Integer');
      P.AddParameter(TVBParameter.Create(pamVar, 'Ident1', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    T := TVBTypeDecl.Create('', scNone, 10, 12, iiNone, Nil);
    Try
      T.AddToken('Integer');
      P.AddParameter(TVBParameter.Create(pamConst, 'Ident2', False, T, '', scNone, 10, 12));
    Finally
      T.Free;
    End;
    CheckEquals('Property Get MyProperty(ByRef Ident1 As MSForms.Integer, ByVal Ident2 As Integer) As MSForms.Integer', P.AsString(True, False));
    CheckEquals('Property Get MyProperty('#13#10'  ByRef Ident1 As MSForms.Integer,'#13#10'  ByVal Ident2 As Integer'#13#10') As MSForms.Integer', P.AsString(True, True));
  Finally
    P.Free;
  End;
end;

initialization
  RegisterTest('VB Module Tests', TestTVBProperty.Suite);
End.
