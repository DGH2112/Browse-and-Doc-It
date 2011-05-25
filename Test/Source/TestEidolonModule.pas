Unit TestEidolonModule;

Interface

Uses
  TestFramework, EidolonModule, BaseLanguageModule, TestBaseLanguageModule;

Type
  //
  // Test Class for the TEidolonComment Class Methods.
  //
  TestTEidolonComment = Class(TExtendedTestCase)
  Strict Private
    FEidolonComment : TEidolonComment;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreateComment;
  End;

  //
  // Test Class for the TEidolonModule Class Methods.
  //
  TestTEidolonModule = Class(TExtendedTestCase)
  Strict Private
    FEidolonModule : TEidolonModule;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreateParser;
    Procedure TestReservedWords;
    Procedure TestGetComment;
    Procedure TestTokenizeStream;
    Procedure TestGoal;
    Procedure TestTextTable;
    Procedure TestDBTable;
    Procedure TestTimeLocationTable;
    Procedure TestTextTableDef;
    Procedure TestFieldDef;
    Procedure TestDatabaseDef;
    Procedure TestTypeInfo;
    Procedure TestConnectionDef;
    Procedure TestTableNameDef;
    Procedure TestTimeLocationDef;
    Procedure TestLine;
    Procedure TestLine2;
    Procedure TestBorderDef;
    Procedure TestBorderColour;
    Procedure TestBorderLineStyle;
    Procedure TestBorderWeight;
    Procedure TestRectangle;
    Procedure TestBar;
    Procedure TestInteriorDef;
    Procedure TestTransparency;
    Procedure TestInteriorColour;
    Procedure TestInteriorPattern;
    Procedure TestInteriorPatternColour;
    Procedure TestBarWidth;
    Procedure TestDiamond;
    Procedure TestTriangle;
    Procedure TestEllipse;
    Procedure TestOutputTable;
    Procedure TestRequirementsTable;
    Procedure TestsDiamondSize;
    Procedure TestTriangleType;
    Procedure TestEllipseSize;
    Procedure TestAssociationDef;
    Procedure TestBlankLinesBetweenDefs;
  End;

Implementation


//
// Test Methods for Class TEidolonComment.
//
Procedure TestTEidolonComment.Setup;
Begin
  FEidolonComment := TEidolonComment.Create('This is a comment!', 12, 23);
End;

Procedure TestTEidolonComment.TearDown;

Begin
  FEidolonComment.Free;
End;

Procedure TestTEidolonComment.TestCreateComment;
var
  C: TComment;

Begin
  C := TEidolonComment.CreateComment('/** This is a comment! **/', 12, 23);
  Try
    CheckEquals('This is a comment!', C.AsString(9999, False));
    CheckEquals(12, C.Line);
    CheckEquals(23, C.Column);
  Finally
    C.Free;
  End;
End;

//
// Test Methods for Class TEidolonModule.
//
Procedure TestTEidolonModule.Setup;
Begin
  FEidolonModule := TEidolonModule.CreateParser('', 'D:\Path\MyMapFile.Map',
    False, [moParse]);
End;

Procedure TestTEidolonModule.TearDown;

Begin
  FEidolonModule.Free;
End;

procedure TestTEidolonModule.TestAssociationDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Korax 97 Requirements List=Class(RequirementsTable)'#13#10 +
    '  {'#13#10 +
    '    #DataBase=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\Korax97.mdb'#13#10 +
    '    #Connection='#13#10 +
    '    #TableName=Requirements List'#13#10 +
    '    *Activity Code:C(50)'#13#10 +
    '    *Resource Id:C(9)'#13#10 +
    '    Resource Level:F'#13#10 +
    '    @ActId=Act Id'#13#10 +
    '    @Measure=Measure'#13#10 +
    '    @Resource=Resource'#13#10 +
    '    @Duration=Durtn'#13#10 +
    '    *ActIdOut:C(12)=Activity Id'#13#10 +
    '    *ResIdOut:C(12)=Resource Id'#13#10 +
    '    LevelOut:F=Level'#13#10 +
    '    TypeOut:C(12)=Type'#13#10 +
    '    @LevelType=Level'#13#10 +
    '    @TotalType=Total'#13#10 +
    '  }'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Requirements Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Korax 97 Requirements List=Class(RequirementsTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(3, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Associations', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[3].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTEidolonModule.TestAsString;

Begin
  CheckEquals('MyMapFile', FEidolonModule.AsString(True, True));
End;

procedure TestTEidolonModule.TestBar;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= bar , Black , Solid , 0.25 , Blue , None  , None , 5 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Bar, Black, Solid, 0.25, Blue, None, None, 5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= bar , Black , Solid , 0.25 , Blue , None  , None , 5, 25 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Bar, Black, Solid, 0.25, Blue, None, None, 5, 25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestBarWidth;

Var
  M: TBaseLanguageModule;
  strSource : String;

Begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Bar, Black, Solid, 0.5, White, None, None, 5'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Bar, Black, Solid, 0.5, White, None, None, 5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Bar, Black, Solid, 0.5, White, None, None, 100'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Bar, Black, Solid, 0.5, White, None, None, 100', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestBlankLinesBetweenDefs;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Korax 97 Info=Class(TextTable)'#13#10 +
    '  {'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    'Korax 97 Info 2=Class(TextTable)'#13#10 +
    '  {'#13#10 +
    '  }'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
 Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestBorderColour;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, None, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, None, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Brown, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Brown, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, OliveGreen, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, OliveGreen, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, DarkGreen, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, DarkGreen, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, DarkTeal, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, DarkTeal, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, DarkBlue, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, DarkBlue, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Indigo, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Indigo, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Gray-80%, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Gray-80%, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, DarkRed, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, DarkRed, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Orange, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Orange, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, DarkYellow, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, DarkYellow, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Green, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Green, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Teal, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Teal, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Blue, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Blue, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Blue-Gray, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Blue-Gray, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Gray-50%, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Gray-50%, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Red, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Red, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, LightOrange, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, LightOrange, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Lime, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Lime, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, SeaGreen, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, SeaGreen, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Aqua, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Aqua, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, LightBlue, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, LightBlue, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Violet, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Violet, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Gray-40%, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Gray-40%, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Pink, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Pink, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Gold, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Gold, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Yellow, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Yellow, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, BrightGreen, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, BrightGreen, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Turquoise, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Turquoise, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, SkyBlue, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, SkyBlue, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Plum, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Plum, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Gray-25%, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Gray-25%, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Rose, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Rose, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Tan, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Tan, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, LightYellow, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, LightYellow, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, LightGreen, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, LightGreen, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, LightTurquoise, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, LightTurquoise, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, PaleBlue, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, PaleBlue, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Lavender, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Lavender, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, White, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, White, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestBorderDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '  Description:C(255)=Activity Name'#13#10 +
    '  Start Date:D'#13#10 +
    '  Finish Date:D'#13#10 +
    '  Start Chainage:F'#13#10 +
    '  Finish Chainage:F'#13#10 +
    '  Time Location Symbol:C(255)'#13#10 +
    '  &Drainage=Line,Blue,Solid,0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    Checkequals(7, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Time Location Symbol:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[7].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Drainage=Line, Blue, Solid, 0.25', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestBorderLineStyle;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, RoundDot, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, RoundDot, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, SquareDot, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, SquareDot, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Dash, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Dash, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, DashDot, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, DashDot, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, LongDash, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, LongDash, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, LongDashDot, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, LongDashDot, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, DashDotDot, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, DashDotDot, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestBorderWeight;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 0.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 0.5'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 0.5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 1'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 1', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 1.5'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 1.5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 2.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 2.25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 3'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 3', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 4.5'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 4.5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, 6'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, 6', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, Double'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, Double', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, DoubleThinThick'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, DoubleThinThick', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, DoubleThickThin'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, DoubleThickThin', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Line, Black, Solid, TripleThickBetweenThin'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Line, Black, Solid, TripleThickBetweenThin', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestConnectionDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #DATABASE=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '  #CONNECTION='#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Connection=', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #DATABASE=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '  #CONNECTION=Paradox 4.x;'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Connection=Paradox 4.x;', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #DATABASE=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '  #CONNECTION=dBase IV;'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Connection=dBase IV;', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #DATABASE=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '  #CONNECTION=FoxPro 2.6;'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Connection=FoxPro 2.6;', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #DATABASE=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '  #CONNECTION=Text;'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Connection=Text;', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTEidolonModule.TestCreateParser;

var
  M: TEidolonModule;

Begin
  M := TEidolonModule.CreateParser('', 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals('MyMapFile.map', M.ModuleName);
  Finally
    M.Free;
  End;
End;

procedure TestTEidolonModule.TestDatabaseDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\Korax97.mdb'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\Korax97.mdb', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestDBTable;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '  {  '#13#10 +
    '  }  '#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestDiamond;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Diamond , Black , Solid , 0.25 , Blue , None  , None , 5 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Diamond, Black, Solid, 0.25, Blue, None, None, 5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Diamond , Black , Solid , 0.25 , Blue , None  , None , 5, 25 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Diamond, Black, Solid, 0.25, Blue, None, None, 5, 25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestEllipse;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Ellipse , Black , Solid , 0.25 , Blue , None  , None , 5 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Ellipse, Black, Solid, 0.25, Blue, None, None, 5', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Ellipse , Black , Solid , 0.25 , Blue , None  , None , 5, 25 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Ellipse, Black, Solid, 0.25, Blue, None, None, 5, 25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestEllipseSize;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Ellipse , Black , Solid , 0.25 , Blue , None  , None , 1 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Ellipse, Black, Solid, 0.25, Blue, None, None, 1', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Ellipse , Black , Solid , 0.25 , Blue , None  , None , 100 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Ellipse, Black, Solid, 0.25, Blue, None, None, 100', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestFieldDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:C(255)=Act ID'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)=Act ID', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  *Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('*Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestGetComment;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource := '// This is a comment.'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
  Finally
    M.Free;
  End;
  strSource := '/* This is a comment. */'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
  Finally
    M.Free;
  End;
  strSource :=
    '/**'#13#10 +
    '  This is a test comment.'#13#10 +
    '**/'#13#10 +
    'This is a test table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  First Field:C(10)'#13#10 +
    '  Second Field:I'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(1, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1].Comment <> Nil, 'Test for associated comment');
    CheckEquals('This is a test comment.', M.Elements[1].Elements[1].Comment.AsString(9999, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a test table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  First Field:C(10)'#13#10 +
    '//  Second Field:I'#13#10 +
    '  Third Field:C(25)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(1, M.Elements[1].ElementCount);
  Finally
    M.Free;
  End;
  strSource :=
    'This is a test table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  First Field:C(10)'#13#10 +
    ''#13#10 +
    '  '#13#10 +
    '  //Second Field:I'#13#10 +
    '  //  Second Field:I'#13#10 +
    '  Third Field:C(25)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(1, M.Elements[1].ElementCount);
  Finally
    M.Free;
  End;
  strSource :=
    'This is a test table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  // #TABLENAME='#13#10 +
    '  First Field:C(10)'#13#10 +
    '  Second Field:I'#13#10 +
    '  Third Field:C(25)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(1, M.Elements[1].ElementCount);
  Finally
    M.Free;
  End;
  strSource :=
    'This is a test table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #Database='#13#10 +
    '  //  #Connection='#13#10 +
    '  #TABLENAME='#13#10 +
    '  First Field:C(10)'#13#10 +
    '  Second Field:I'#13#10 +
    '  Third Field:C(25)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(1, M.Elements[1].ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestGoal;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource := '';
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestInteriorColour;

Var
  M: TBaseLanguageModule;
  strSource : String;

Begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, None, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, None, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Black, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Black, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Brown, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Brown, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, OliveGreen, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, OliveGreen, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, DarkGreen, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, DarkGreen, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, DarkTeal, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, DarkTeal, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, DarkBlue, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, DarkBlue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Indigo, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Indigo, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Gray-80%, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Gray-80%, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, DarkRed, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, DarkRed, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Orange, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Orange, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, DarkYellow, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, DarkYellow, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Green, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Green, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Teal, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Teal, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Blue, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Blue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Blue-Gray, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Blue-Gray, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Gray-50%, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Gray-50%, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Red, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Red, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, LightOrange, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, LightOrange, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Lime, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Lime, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, SeaGreen, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, SeaGreen, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Aqua, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Aqua, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, LightBlue, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, LightBlue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Violet, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Violet, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Gray-40%, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Gray-40%, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Pink, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Pink, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Gold, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Gold, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Yellow, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Yellow, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, BrightGreen, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, BrightGreen, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Turquoise, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Turquoise, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, SkyBlue, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, SkyBlue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Plum, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Plum, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Gray-25%, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Gray-25%, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Rose, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Rose, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Tan, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Tan, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, LightYellow, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, LightYellow, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, LightGreen, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, LightGreen, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, LightTurquoise, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, LightTurquoise, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, PaleBlue, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, PaleBlue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, Lavender, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, Lavender, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestInteriorDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Rectangle , Black , Solid , 0.25 , Blue , None  , None '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.25, Blue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestInteriorPattern;

Var
  M: TBaseLanguageModule;
  strSource : String;

Begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 10Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 10Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 20Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 20Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 25Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 25Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 30Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 30Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 40Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 40Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 50Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 50Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 5Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 5Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 60Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 60Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 70Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 70Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 75Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 75Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 80Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 80Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, 90Percent, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, 90Percent, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DarkDownwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DarkDownwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Darkhorizontal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DarkHorizontal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DarkUpwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DarkUpwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DarkVertical, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DarkVertical, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DashedDownwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DashedDownwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DashedHorizontal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DashedHorizontal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DashedUpwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DashedUpwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DashedVertical, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DashedVertical, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DiagonalBrick, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DiagonalBrick, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, DottedGrid, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, DottedGrid, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, HorizontalBrick, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, HorizontalBrick, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LargeCheckerBoard, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LargeCheckerBoard, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LargeConfetti, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LargeConfetti, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LargeGrid, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LargeGrid, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LightDownwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LightDownwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LightHorizontal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LightHorizontal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LightUpwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LightUpwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, LightVertical, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, LightVertical, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, NarrowHorizontal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, NarrowHorizontal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, NarrowVertical, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, NarrowVertical, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, OutlinedDiamond, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, OutlinedDiamond, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Plaid, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Plaid, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Shingle, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Shingle, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, SmallCheckerBoard, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, SmallCheckerBoard, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, SmallConfetti, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, SmallConfetti, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, SmallGrid, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, SmallGrid, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, SolidDiamond, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, SolidDiamond, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Sphere, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Sphere, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Trellis, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Trellis, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Wave, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Wave, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Weave, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Weave, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, WideDownwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, WideDownwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, WideUpwardDiagonal, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, WideUpwardDiagonal, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Zigzag, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Zigzag, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestInteriorPatternColour;

Var
  M: TBaseLanguageModule;
  strSource : String;

Begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, None, None'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Black'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Black', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Brown'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Brown', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, OliveGreen'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, OliveGreen', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkGreen'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkGreen', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkTeal'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkTeal', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkBlue'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkBlue', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Indigo'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Indigo', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-80%'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-80%', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkRed'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkRed', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Orange'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Orange', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkYellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, DarkYellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Green'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Green', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Teal'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Teal', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Blue'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Blue', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Blue-Gray'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Blue-Gray', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-50%'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-50%', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Red'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Red', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightOrange'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightOrange', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Lime'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Lime', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, SeaGreen'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, SeaGreen', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Aqua'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Aqua', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightBlue'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightBlue', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Violet'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Violet', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-40%'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-40%', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Pink'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Pink', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gold'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gold', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Yellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Yellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, BrightGreen'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, BrightGreen', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Turquoise'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Turquoise', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, SkyBlue'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, SkyBlue', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Plum'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Plum', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-25%'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Gray-25%', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Rose'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Rose', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Tan'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Tan', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightYellow'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightYellow', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightGreen'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightGreen', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightTurquoise'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, LightTurquoise', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, PaleBlue'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, PaleBlue', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Lavender'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, Lavender', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage=Rectangle, Black, Solid, 0.5, White, Divot, White'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.5, White, Divot, White', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTEidolonModule.TestReservedWords;

Var
  Words : TKeyWords;
  i : Integer;

Begin
  Words := FEidolonModule.ReservedWords;
  CheckEquals('bar', Words[0]);
  CheckEquals('triangle', Words[11]);
  For i := Low(Words) To Pred(High(Words)) Do
    Check(Words[i] < Words[i + 1], Words[i] + '!<' + Words[i + 1]);
End;


procedure TestTEidolonModule.TestLine;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '  Description:C(255)=Activity Name'#13#10 +
    '  Start Date:D'#13#10 +
    '  Finish Date:D'#13#10 +
    '  Start Chainage:F'#13#10 +
    '  Finish Chainage:F'#13#10 +
    '  Time Location Symbol:C(255)'#13#10 +
    '  &Drainage= Line , Blue , Solid , 0.25 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    Checkequals(7, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Time Location Symbol:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[7].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Drainage=Line, Blue, Solid, 0.25', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestLine2;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '  Description:C(255)=Activity Name'#13#10 +
    '  Start Date:D'#13#10 +
    '  Finish Date:D'#13#10 +
    '  Start Chainage:F'#13#10 +
    '  Finish Chainage:F'#13#10 +
    '  Time Location Symbol:C(255)'#13#10 +
    '  &Drainage01= Line , Blue , Solid , 0.25, None      , ShortNarrow , None      , ShortNarrow '#13#10 +
    '  &Drainage02= Line , Blue , Solid , 0.25, Diamond   , ShortMedium , Diamond   , ShortMedium '#13#10 +
    '  &Drainage03= Line , Blue , Solid , 0.25, Open      , ShortWide   , Open      , ShortWide   '#13#10 +
    '  &Drainage04= Line , Blue , Solid , 0.25, Oval      , MediumNarrow, Oval      , MediumNarrow'#13#10 +
    '  &Drainage05= Line , Blue , Solid , 0.25, Stealth   , MediumMedium, Stealth   , MediumMedium'#13#10 +
    '  &Drainage06= Line , Blue , Solid , 0.25, Triangle  , MediumWide  , None      , MediumWide  '#13#10 +
    '  &Drainage07= Line , Blue , Solid , 0.25, None      , LongNarrow  , Triangle  , LongNarrow  '#13#10 +
    '  &Drainage08= Line , Blue , Solid , 0.25, Oval      , LongMedium  , None      , LongMedium  '#13#10 +
    '  &Drainage09= Line , Blue , Solid , 0.25, None      , LongWide    , Open      , LongWide    '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    Checkequals(7, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Time Location Symbol:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[7].AsString(True, True));
    Checkequals(9, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Drainage01=Line, Blue, Solid, 0.25', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Drainage02=Line, Blue, Solid, 0.25, Diamond, ShortMedium, Diamond, ShortMedium', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('Drainage03=Line, Blue, Solid, 0.25, Open, ShortWide, Open, ShortWide', M.Elements[1].Elements[1].Elements[2].Elements[3].AsString(True, True));
    CheckEquals('Drainage04=Line, Blue, Solid, 0.25, Oval, MediumNarrow, Oval, MediumNarrow', M.Elements[1].Elements[1].Elements[2].Elements[4].AsString(True, True));
    CheckEquals('Drainage05=Line, Blue, Solid, 0.25, Stealth, MediumMedium, Stealth, MediumMedium', M.Elements[1].Elements[1].Elements[2].Elements[5].AsString(True, True));
    CheckEquals('Drainage06=Line, Blue, Solid, 0.25, Triangle, MediumWide, None, MediumWide', M.Elements[1].Elements[1].Elements[2].Elements[6].AsString(True, True));
    CheckEquals('Drainage07=Line, Blue, Solid, 0.25, None, LongNarrow, Triangle, LongNarrow', M.Elements[1].Elements[1].Elements[2].Elements[7].AsString(True, True));
    CheckEquals('Drainage08=Line, Blue, Solid, 0.25, Oval, LongMedium, None, LongMedium', M.Elements[1].Elements[1].Elements[2].Elements[8].AsString(True, True));
    CheckEquals('Drainage09=Line, Blue, Solid, 0.25, None, LongWide, Open, LongWide', M.Elements[1].Elements[1].Elements[2].Elements[9].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestOutputTable;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Korax 97 Output Table=Class(OutputTable)'#13#10 +
    '  {'#13#10 +
    '    #DataBase=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\Korax97.mdb'#13#10 +
    '    #Connection='#13#10 +
    '    #TableName=Output Table'#13#10 +
    '    @DataBase=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\Korax97.mdb'#13#10 +
    '    @Connection='#13#10 +
    '    @TableName=Areas'#13#10 +
    '    *Code:C(50)'#13#10 +
    '    Description:C(75)'#13#10 +
    '    Rate:F'#13#10 +
    '    Units:C(50)'#13#10 +
    '    Comments:C(100)'#13#10 +
    '    Area:C(50)'#13#10 +
    '    For Buffer:B'#13#10 +
    '  }'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Output Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Korax 97 Output Table=Class(OutputTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(3, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(7, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Area:C(50)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Units:C(50)', M.Elements[1].Elements[1].Elements[1].Elements[7].AsString(True, True));
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(3, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Secondary', M.Elements[1].Elements[1].Elements[3].AsString(True, True));
    CheckEquals(3, M.Elements[1].Elements[1].Elements[3].ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestRectangle;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Rectangle , Black , Solid , 0.25 , Blue , None  , None '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.25, Blue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Rectangle , Black , Solid , 0.25 , Blue , None  , None , 25 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.25, Blue, None, None, 25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestRequirementsTable;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Korax 97 Requirements List=Class(RequirementsTable)'#13#10 +
    '  {'#13#10 +
    '    #DataBase=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\Korax97.mdb'#13#10 +
    '    #Connection='#13#10 +
    '    #TableName=Requirements List'#13#10 +
    '    *Activity Code:C(50)'#13#10 +
    '    *Resource Id:C(9)'#13#10 +
    '    Resource Level:F'#13#10 +
    '    @ActId=Act Id'#13#10 +
    '    @Measure=Measure'#13#10 +
    '    @Resource=Resource'#13#10 +
    '    @Duration=Durtn'#13#10 +
    '    *ActIdOut:C(12)=Activity Id'#13#10 +
    '    *ResIdOut:C(12)=Resource Id'#13#10 +
    '    LevelOut:F=Level'#13#10 +
    '    TypeOut:C(12)=Type'#13#10 +
    '    @LevelType=Level'#13#10 +
    '    @TotalType=Total'#13#10 +
    '  }'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Requirements Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Korax 97 Requirements List=Class(RequirementsTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(3, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Associations', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[3].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestsDiamondSize;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Diamond , Black , Solid , 0.25 , Blue , None  , None , 1 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Diamond, Black, Solid, 0.25, Blue, None, None, 1', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Diamond , Black , Solid , 0.25 , Blue , None  , None , 100 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Diamond, Black, Solid, 0.25, Blue, None, None, 100', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTableNameDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a database table=Class(DBTable)'#13#10 +
    '{'#13#10 +
    '  #DATABASE=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\'#13#10 +
    '  #CONNECTION=Paradox 4.x;'#13#10 +
    '  #TABLENAME=My Table Name'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Database Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a database table=Class(DBTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Primary', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(3, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Connection=Paradox 4.x;', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
    CheckEquals('Database=' + Copy(ParamStr(0), 1, 1) + ':\HoylD\Excel\Korax 97\Database (Full)\', M.Elements[1].Elements[1].Elements[2].Elements[2].AsString(True, True));
    CheckEquals('TableName=My Table Name', M.Elements[1].Elements[1].Elements[2].Elements[3].AsString(True, True));
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTextTable;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '  {  '#13#10 +
    '  }  '#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTextTableDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTimeLocationDef;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '  Description:C(255)=Activity Name'#13#10 +
    '  Start Date:D'#13#10 +
    '  Finish Date:D'#13#10 +
    '  Start Chainage:F'#13#10 +
    '  Finish Chainage:F'#13#10 +
    '  Time Location Symbol:C(255)'#13#10 +
    '  &Drainage=Line, Blue, Solid, 0.25'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    Checkequals(7, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Time Location Symbol:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[7].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[2].ElementCount);
    CheckEquals('Drainage=Line, Blue, Solid, 0.25', M.Elements[1].Elements[1].Elements[2].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTimeLocationTable;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '  {  '#13#10 +
    '  }  '#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTokenizeStream;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource := '/* This is a comment. */'#13#10+
  'Name=Class(TextTable)'#13#10 +
  '{'#13#10 +
  '  Activity ID:C(255)'#13#10 +
  '//  Description:C(255)'#13#10 +
  '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(33, M.TokenCount);
    CheckEquals('/* This is a comment. */', M.Tokens[0].Token);
    CheckEquals(ttBlockComment, M.Tokens[0].TokenType);
    CheckEquals('<LF>', M.Tokens[1].Token);
    CheckEquals(ttLineEnd, M.Tokens[1].TokenType);
    CheckEquals('<CR>', M.Tokens[2].Token);
    CheckEquals(ttLineEnd, M.Tokens[2].TokenType);
    CheckEquals('Name', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('=', M.Tokens[4].Token);
    CheckEquals(ttSymbol, M.Tokens[4].TokenType);
    CheckEquals('Class', M.Tokens[5].Token);
    CheckEquals(ttReservedWord, M.Tokens[5].TokenType);
    CheckEquals('(', M.Tokens[6].Token);
    CheckEquals(ttSymbol, M.Tokens[6].TokenType);
    CheckEquals('TextTable', M.Tokens[7].Token);
    CheckEquals(ttReservedWord, M.Tokens[7].TokenType);
    CheckEquals(')', M.Tokens[8].Token);
    CheckEquals(ttSymbol, M.Tokens[8].TokenType);
    CheckEquals('<LF>', M.Tokens[9].Token);
    CheckEquals(ttLineEnd, M.Tokens[9].TokenType);
    CheckEquals('<CR>', M.Tokens[10].Token);
    CheckEquals(ttLineEnd, M.Tokens[10].TokenType);
    CheckEquals('{', M.Tokens[11].Token);
    CheckEquals(ttSymbol, M.Tokens[11].TokenType);
    CheckEquals('<LF>', M.Tokens[12].Token);
    CheckEquals(ttLineEnd, M.Tokens[12].TokenType);
    CheckEquals('<CR>', M.Tokens[13].Token);
    CheckEquals(ttLineEnd, M.Tokens[13].TokenType);
    CheckEquals(' ', M.Tokens[14].Token);
    CheckEquals(ttWhiteSpace, M.Tokens[14].TokenType);
    CheckEquals(' ', M.Tokens[15].Token);
    CheckEquals(ttWhiteSpace, M.Tokens[15].TokenType);
    CheckEquals('Activity', M.Tokens[16].Token);
    CheckEquals(ttIdentifier, M.Tokens[16].TokenType);
    CheckEquals(' ', M.Tokens[17].Token);
    CheckEquals(ttWhiteSpace, M.Tokens[17].TokenType);
    CheckEquals('ID', M.Tokens[18].Token);
    CheckEquals(ttIdentifier, M.Tokens[18].TokenType);
    CheckEquals(':', M.Tokens[19].Token);
    CheckEquals(ttSymbol, M.Tokens[19].TokenType);
    CheckEquals('C', M.Tokens[20].Token);
    CheckEquals(ttIdentifier, M.Tokens[20].TokenType);
    CheckEquals('(', M.Tokens[21].Token);
    CheckEquals(ttSymbol, M.Tokens[21].TokenType);
    CheckEquals('255', M.Tokens[22].Token);
    CheckEquals(ttNumber, M.Tokens[22].TokenType);
    CheckEquals(')', M.Tokens[23].Token);
    CheckEquals(ttSymbol, M.Tokens[23].TokenType);
    CheckEquals('<LF>', M.Tokens[24].Token);
    CheckEquals(ttLineEnd, M.Tokens[24].TokenType);
    CheckEquals('<CR>', M.Tokens[25].Token);
    CheckEquals(ttLineEnd, M.Tokens[25].TokenType);
    CheckEquals('//  Description:C(255)', M.Tokens[26].Token);
    CheckEquals(ttLineComment, M.Tokens[26].TokenType);
    CheckEquals('<LF>', M.Tokens[27].Token);
    CheckEquals(ttLineEnd, M.Tokens[27].TokenType);
    CheckEquals('<CR>', M.Tokens[28].Token);
    CheckEquals(ttLineEnd, M.Tokens[28].TokenType);
    CheckEquals('}', M.Tokens[29].Token);
    CheckEquals(ttSymbol, M.Tokens[29].TokenType);
    CheckEquals('<LF>', M.Tokens[30].Token);
    CheckEquals(ttLineEnd, M.Tokens[30].TokenType);
    CheckEquals('<CR>', M.Tokens[31].Token);
    CheckEquals(ttLineEnd, M.Tokens[31].TokenType);
    CheckEquals('<end-of-file>', M.Tokens[32].Token);
    CheckEquals(ttFileEnd, M.Tokens[32].TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTransparency;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Rectangle , Black , Solid , 0.25 , Blue , None  , None '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.25, Blue, None, None', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Rectangle , Black , Solid , 0.25 , Blue , None  , None , 0 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.25, Blue, None, None, 0', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Rectangle , Black , Solid , 0.25 , Blue , None  , None , 100 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Rectangle, Black, Solid, 0.25, Blue, None, None, 100', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTriangle;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Triangle , Black , Solid , 0.25 , Blue , None  , None , StartAndEarly '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Triangle, Black, Solid, 0.25, Blue, None, None, StartAndEarly', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Triangle , Black , Solid , 0.25 , Blue , None  , None , StartAndEarly, 25 '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Triangle, Black, Solid, 0.25, Blue, None, None, StartAndEarly, 25', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTriangleType;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Triangle , Black , Solid , 0.25 , Blue , None  , None , StartAndEarly '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Triangle, Black, Solid, 0.25, Blue, None, None, StartAndEarly', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Triangle , Black , Solid , 0.25 , Blue , None  , None , EndAndEarly '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Triangle, Black, Solid, 0.25, Blue, None, None, EndAndEarly', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Triangle , Black , Solid , 0.25 , Blue , None  , None , EndAndLate '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Triangle, Black, Solid, 0.25, Blue, None, None, EndAndLate', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a time location table=Class(TimeLocationTable)'#13#10 +
    '{'#13#10 +
    '  &Drainage= Triangle , Black , Solid , 0.25 , Blue , None  , None , StartAndLate '#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Time Location Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a time location table=Class(TimeLocationTable)', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Symbols', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    Checkequals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Drainage=Triangle, Black, Solid, 0.25, Blue, None, None, StartAndLate', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTEidolonModule.TestTypeInfo;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:C(255)'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:C(255)', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:B'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:B', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:Y'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:Y', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:I'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:I', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:L'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:L', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:U'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:U', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:F'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:F', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:S'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:S', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:D'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:D', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:O'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:O', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'This is a text table=Class(TextTable)'#13#10 +
    '{'#13#10 +
    '  #TABLENAME=D:\Path\Text Table.txt'#13#10 +
    '  Activity ID:M'#13#10 +
    '}'#13#10;
  M := TEidolonModule.CreateParser(strSource, 'D:\Path\MyMapFile.map', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Text Table Definitions', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('This is a text table=Class(TextTable)', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].Elements[1].ElementCount);
    CheckEquals('Fields', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals('TableName=D:\Path\Text Table.txt', M.Elements[1].Elements[1].Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    CheckEquals('Activity ID:M', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Initialization
  RegisterTest('Eidolon Module', TestTEidolonComment.Suite);
  RegisterTest('Eidolon Module', TestTEidolonModule.Suite);
End.