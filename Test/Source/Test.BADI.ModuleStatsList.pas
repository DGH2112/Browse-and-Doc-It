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
Unit Test.BADI.ModuleStatsList;

Interface

Uses
  TestFramework,
  BADI.Interfaces;

Type
  TBADIModuleStatsListTests = Class(TTestCase)
  Strict Private
    FModuleStatsList : IBADIModuleStatsList;
  Strict Protected
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAdd;
    Procedure TestRename;
  End;

Implementation

Uses
  BADI.ModuleStatsList;

Procedure TBADIModuleStatsListTests.SetUp;

Begin
  FModuleStatsList := TBADIModuleStatsList.Create;
End;

Procedure TBADIModuleStatsListTests.TearDown;

Begin
  FModuleStatsList := Nil;
End;

Procedure TBADIModuleStatsListTests.TestAdd;

Const
  strFile = 'D:\Path\FileName.Ext';

Begin
  FModuleStatsList.ModuleStats[strFile].Update(1000, 0);
  CheckEquals(0, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(1100, 0);
  CheckEquals(100, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(950, 0);
  CheckEquals(250, FModuleStatsList.ModuleStats[strFile].SizeChange);
End;

Procedure TBADIModuleStatsListTests.TestRename;

Const
  strFile = 'D:\Path\FileName.Ext';
  strNewFile = 'D:\Path\NewFileName.Ext';

Begin
  FModuleStatsList.ModuleStats[strFile].Update(1000, 0);
  CheckEquals(0, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(1100, 0);
  CheckEquals(100, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(950, 0);
  CheckEquals(250, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.Rename(strFile, strNewFile);
  CheckEquals(250, FModuleStatsList.ModuleStats[strNewFile].SizeChange);
End;

Initialization                                                        
  RegisterTest('BADIModuleStatsList', TBADIModuleStatsListTests.Suite);
End.
