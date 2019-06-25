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
Unit Test.BADI.Pascal.MethodDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.MethodDecl;

Type
  TestTPascalMethod = Class(TExtendedTestCase)
  Strict Private
    FPascalMethod: TPascalMethod;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAddDirectives;
    Procedure TestHasDirective;
    Procedure TestAsString;
    Procedure TestReferenceSymbol;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Pascal.TypeDecl,
  BADI.ElementContainer,
  Test.BADI.Pascal.Parameter,
  BADI.Pascal.ParameterDecl,
  BADI.TokenInfo;

Procedure TestTPascalMethod.SetUp;

Var
  T: TElementContainer;

Begin
  FPascalMethod := TPascalMethod.Create(mtFunction, 'MyFunction', scPrivate, 12, 23);
  T := TTypes.Create('Integer', scNone, 0, 0, iiNone, Nil);
  T.AddToken('Integer', ttIdentifier);
  FPascalMethod.ReturnType.Add(T);
End;

Procedure TestTPascalMethod.TearDown;

Begin
  FPascalMethod.Free;
  FPascalMethod := Nil;
End;

Procedure TestTPascalMethod.TestAddDirectives;

Begin
  FPascalMethod.AddDirectives('Virtual');
  FPascalMethod.AddDirectives('Message');
  CheckEquals(True, FPascalMethod.HasDirective('Virtual'));
  CheckEquals(True, FPascalMethod.HasDirective('Message'));
End;

Procedure TestTPascalMethod.TestHasDirective;

Begin
  FPascalMethod.AddDirectives('Virtual');
  FPascalMethod.AddDirectives('Message');
  CheckEquals(True, FPascalMethod.HasDirective('Virtual'));
  CheckEquals(True, FPascalMethod.HasDirective('Message'));
End;

Procedure TestTPascalMethod.TestAsString;

Var
  P: TPascalParameter;
  AType: TTypes;

Begin
  CheckEquals('Function MyFunction : Integer', FPascalMethod.AsString(True, False));
  CheckEquals('Function MyFunction : Integer', FPascalMethod.AsString(True, True));
  AType := TTypes.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('Integer');
    P := TPascalParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FPascalMethod.AddParameter(P);
  Finally
    AType.Free;
  End;
  CheckEquals('Function MyFunction(Param1 : Integer) : Integer',
    FPascalMethod.AsString(True, False));
  CheckEquals('Function MyFunction('#13#10#32#32'Param1 : Integer'#13#10') : Integer',
    FPascalMethod.AsString(True, True));
  AType := TTypes.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TPascalParameter.Create(pamNone, 'Param2', False, AType, '', scNone, 0, 0);
    FPascalMethod.AddParameter(P);
  Finally
    AType.Free;
  End;
  CheckEquals('Function MyFunction(Param1 : Integer; Param2 : String) : Integer',
    FPascalMethod.AsString(True, False));
  CheckEquals
    ('Function MyFunction('#13#10#32#32'Param1 : Integer;'#13#10#32#32'Param2 : String'#13#10') : Integer',
    FPascalMethod.AsString(True, True));
End;

Procedure TestTPascalMethod.TestCreate;

Const
  strMethodTypes: Array [mtConstructor .. mtFunction] Of String = ('Constructor', 'Destructor',
    'Procedure', 'Function');
Begin
  CheckEquals(strMethodTypes[mtFunction], strMethodTypes[FPascalMethod.MethodType]);
  CheckEquals('MyFunction', FPascalMethod.Identifier);
  CheckEquals(scPrivate, FPascalMethod.Scope);
  CheckEquals(12, FPascalMethod.Line);
  CheckEquals(23, FPascalMethod.Column);
End;

Procedure TestTPascalMethod.TestReferenceSymbol;

Var
  AToken: TTokenInfo;

Begin
  AToken := TTokenInfo.Create('Hello', 0, 0, 0, 5, ttIdentifier);
  Try
    CheckEquals(False, FPascalMethod.ReferenceSymbol(AToken));
    // : @todo Requires more tests.
  Finally
    AToken.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalMethod.Suite);
End.
