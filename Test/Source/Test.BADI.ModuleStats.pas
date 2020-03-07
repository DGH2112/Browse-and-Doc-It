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
  FModuleStats.Update(1000);
  CheckEquals(0, FModuleStats.SizeChange);
  FModuleStats.Update(1025);
  CheckEquals(25, FModuleStats.SizeChange);
  FModuleStats.Reset();
  CheckEquals(0, FModuleStats.SizeChange);
End;

Procedure TBADIModuleStatsTest.TestUpdate;

Begin
  FModuleStats.Update(1000);
  CheckEquals(0, FModuleStats.SizeChange);
  FModuleStats.Update(1025);
  CheckEquals(25, FModuleStats.SizeChange);
  FModuleStats.Update(990);
  CheckEquals(60, FModuleStats.SizeChange);
End;

Procedure TBADIModuleStatsTest.UpdateSize;

Begin
  FModuleStats.Update(1000);
  CheckEquals(0, FModuleStats.SizeChange);
  FModuleStats.Update(1025);
  CheckEquals(25, FModuleStats.SizeChange);
  FModuleStats.Update(990);
  CheckEquals(60, FModuleStats.SizeChange);
End;

Initialization
  RegisterTest('Module Stats Tests', TBADIModuleStatsTest.Suite);
End.
