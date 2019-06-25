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
Unit Test.BADI.LabelContainer;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.ElementContainer;

Type
  TestTLabelContainer = Class(TExtendedTestCase)
  Strict Private
    FLabelContainer: TLabelContainer;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Options;

Procedure TestTLabelContainer.SetUp;

Begin
  FLabelContainer := TLabelContainer.Create(strImplementedMethodsLabel, scNone, 12, 23,
    iiImplementedMethods, Nil);
  TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options - [doShowChildCountInTitles];
End;

Procedure TestTLabelContainer.TearDown;
Begin
  FLabelContainer.Free;
  FLabelContainer := Nil;
End;

Procedure TestTLabelContainer.TestAsString;
Begin
  CheckEquals(strImplementedMethodsLabel, FLabelContainer.AsString(True, False));
End;

Procedure TestTLabelContainer.TestCreate;
Begin
  CheckEquals(strImplementedMethodsLabel, FLabelContainer.Identifier);
  CheckEquals(scNone, FLabelContainer.Scope);
  CheckEquals(12, FLabelContainer.Line);
  CheckEquals(23, FLabelContainer.Column);
  CheckEquals(BADIImageIndex(iiImplementedMethods, scNone), FLabelContainer.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTLabelContainer.Suite);
End.
