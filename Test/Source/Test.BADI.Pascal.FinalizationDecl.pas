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
Unit Test.BADI.Pascal.FinalizationDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.FinalizationDecl;

Type
  TestTFinalizationSection = Class(TExtendedTestCase)
  Strict Private
    FFinalizationSection: TFinalizationSection;
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
  BADI.Comment, BADI.Functions;

Procedure TestTFinalizationSection.SetUp;
Begin
  FFinalizationSection := TFinalizationSection.Create('Finalization', scNone, 12, 23,
    iiFinalization, Nil);
End;

Procedure TestTFinalizationSection.TearDown;
Begin
  FFinalizationSection.Free;
  FFinalizationSection := Nil;
End;

Procedure TestTFinalizationSection.TestAsString;
Begin
  Checkequals('Finalization', FFinalizationSection.AsString(True, False));
End;

Procedure TestTFinalizationSection.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  Checkequals(0, FFinalizationSection.ElementCount);
  FFinalizationSection.CheckDocumentation(boolCascade);
  Checkequals(1, FFinalizationSection.ElementCount);
  Checkequals('1) The module is missing an Finalization comment.',
    FFinalizationSection.DocConflict(1));
  FFinalizationSection.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the finalisation section.', 0, 0, 0);
  Try
    FFinalizationSection.Comment := C;
    FFinalizationSection.CheckDocumentation(boolCascade);
    Checkequals(0, FFinalizationSection.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTFinalizationSection.TestCreate;
Begin
  Checkequals('Finalization', FFinalizationSection.Identifier);
  Checkequals(scNone, FFinalizationSection.Scope);
  Checkequals(12, FFinalizationSection.Line);
  Checkequals(23, FFinalizationSection.Column);
  Checkequals(BADIImageIndex(iiFinalization, scNone), FFinalizationSection.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTFinalizationSection.Suite);
End.
