unit Test.DUnitXUnit1;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TMyTestObject = class(TObject) 
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation

Uses
  Classes;

procedure TMyTestObject.Test1;
var
  T: TStringList;
begin
  T := TStringList.Create;
  Assert.AreEqual(T.CLassName, 'TStringList');
end;

procedure TMyTestObject.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
end.
