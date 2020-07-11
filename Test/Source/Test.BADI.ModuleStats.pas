(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.103
  @Date    24 Jun 2020

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
Unit Test.BADI.ModuleStats;

Interface

Uses
  TestFramework,
  BADI.Interfaces;

Type
  TBADIModuleStatsTest = Class(TTestCase)
  Strict Private
    FModuleStats : IBADIModuleStats;
  Strict Protected
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestUpdate;
    Procedure TestReset;
    Procedure UpdateSize;
  End;

Implementation

Uses
  BADI.ModuleStats;

Procedure TBADIModuleStatsTest.Setup;

Begin
  FModuleStats := TBADIModuleStats.Create('D:\Path\Filename.ext');
End;

Procedure TBADIModuleStatsTest.TearDown;

Begin
  FModuleStats := Nil;
End;

Procedure TBADIModuleStatsTest.TestCreate;

Begin
  CheckEquals(0, FModuleStats.SizeChange);
End;

Procedure TBADIModuleStatsTest.TestReset;

Begin
  FModuleStats.Update(1000, 0);
  CheckEquals(0, FModuleStats.SizeChange);
  FModuleStats.Update(1025, 0);
  CheckEquals(25, FModuleStats.SizeChange);
  FModuleStats.Reset();
  CheckEquals(0, FModuleStats.SizeChange);
End;

Procedure TBADIModuleStatsTest.TestUpdate;

Begin
  FModuleStats.Update(1000, 0);
  CheckEquals(0, FModuleStats.SizeChange);
  FModuleStats.Update(1025, 0);
  CheckEquals(25, FModuleStats.SizeChange);
  FModuleStats.Update(990, 0);
  CheckEquals(60, FModuleStats.SizeChange);
End;

Procedure TBADIModuleStatsTest.UpdateSize;

Begin
  FModuleStats.Update(1000, 0);
  CheckEquals(0, FModuleStats.SizeChange);
  FModuleStats.Update(1025, 0);
  CheckEquals(25, FModuleStats.SizeChange);
  FModuleStats.Update(990, 0);
  CheckEquals(60, FModuleStats.SizeChange);
End;

Initialization
  RegisterTest('Module Stats Tests', TBADIModuleStatsTest.Suite);
End.
