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
  FModuleStatsList.ModuleStats[strFile].Update(1000);
  CheckEquals(0, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(1100);
  CheckEquals(100, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(950);
  CheckEquals(250, FModuleStatsList.ModuleStats[strFile].SizeChange);
End;

Procedure TBADIModuleStatsListTests.TestRename;

Const
  strFile = 'D:\Path\FileName.Ext';
  strNewFile = 'D:\Path\NewFileName.Ext';

Begin
  FModuleStatsList.ModuleStats[strFile].Update(1000);
  CheckEquals(0, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(1100);
  CheckEquals(100, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.ModuleStats[strFile].Update(950);
  CheckEquals(250, FModuleStatsList.ModuleStats[strFile].SizeChange);
  FModuleStatsList.Rename(strFile, strNewFile);
  CheckEquals(250, FModuleStatsList.ModuleStats[strNewFile].SizeChange);
End;

Initialization                                                        
  RegisterTest('BADIModuleStatsList', TBADIModuleStatsListTests.Suite);
End.
