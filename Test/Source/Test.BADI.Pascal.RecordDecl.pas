(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.001
  @Date    24 May 2020

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
Unit Test.BADI.Pascal.RecordDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.RecordDecl;

Type
  TestTRecordDecl = Class(TExtendedTestCase)
  Strict Private
    FRecordDecl: TRecordDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
    Procedure TestReferenceSymbol;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo,
  BADI.Functions;

Procedure TestTRecordDecl.SetUp;

Begin
  FRecordDecl := TRecordDecl.Create('MyRecord', scProtected, 12, 23, iiPublicRecord, Nil);
End;

Procedure TestTRecordDecl.TearDown;

Begin
  FRecordDecl.Free;
  FRecordDecl := Nil;
End;

Procedure TestTRecordDecl.TestCheckDocumentation;

Var
  boolCascade: Boolean;
  C: TComment;

Begin
  CheckEquals(0, FRecordDecl.ElementCount);
  FRecordDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FRecordDecl.ElementCount);
  CheckEquals('1) Record type ''MyRecord'' is undocumented.', FRecordDecl.DocConflict(1));
  FRecordDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the record.', 0, 0, 0);
  Try
    FRecordDecl.Comment := C;
    FRecordDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FRecordDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTRecordDecl.TestCreate;

Begin
  CheckEquals(scProtected, FRecordDecl.Scope);
  CheckEquals(12, FRecordDecl.Line);
  CheckEquals(23, FRecordDecl.Column);
  CheckEquals(iiPublicRecord, FRecordDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicRecord, scProtected), FRecordDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTRecordDecl.TestAsString;

Begin
  CheckEquals('MyRecord = Record', FRecordDecl.AsString(True, False));
End;

Procedure TestTRecordDecl.TestReferenceSymbol;

Var
  AToken: TTokenInfo;

Begin
  AToken := TTokenInfo.Create('Hello', 0, 1, 2, 5, ttUnknown);
  Try
    CheckEquals(False, FRecordDecl.ReferenceSymbol(AToken));
    // : @todo Requires more tests.
  Finally
    AToken.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTRecordDecl.Suite);
End.
