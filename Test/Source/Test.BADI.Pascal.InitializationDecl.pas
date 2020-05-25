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
Unit Test.BADI.Pascal.InitializationDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.InitializationDecl;

Type
  TestTInitializationSection = Class(TExtendedTestCase)
  Strict Private
    FInitializationSection: TInitializationSection;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestCheckDocumentation;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.Functions;

Procedure TestTInitializationSection.SetUp;
Begin
  FInitializationSection := TInitializationSection.Create('Initialization', scNone, 12, 23,
    iiInitialization, Nil);
End;

Procedure TestTInitializationSection.TearDown;
Begin
  FInitializationSection.Free;
  FInitializationSection := Nil;
End;

Procedure TestTInitializationSection.TestAsString;
Begin
  Checkequals('Initialization', FInitializationSection.AsString(True, False));
End;

Procedure TestTInitializationSection.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  Checkequals(0, FInitializationSection.ElementCount);
  FInitializationSection.CheckDocumentation(boolCascade);
  Checkequals(1, FInitializationSection.ElementCount);
  Checkequals('1) The module is missing an Initialization comment.',
    FInitializationSection.DocConflict(1));
  FInitializationSection.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the initialisation section.', 0, 0, 0);
  Try
    FInitializationSection.Comment := C;
    FInitializationSection.CheckDocumentation(boolCascade);
    Checkequals(0, FInitializationSection.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTInitializationSection.TestCreate;
Begin
  Checkequals('Initialization', FInitializationSection.Identifier);
  Checkequals(scNone, FInitializationSection.Scope);
  Checkequals(12, FInitializationSection.Line);
  Checkequals(23, FInitializationSection.Column);
  Checkequals(BADIImageIndex(iiInitialization, scNone), FInitializationSection.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTInitializationSection.Suite);
End.
