Unit TestEidolonTLSSchematicModule;

Interface

Uses
  TestFramework, EidolonTLSSchematicModule, TestBaseLanguageModule;

Type
  //
  // Test Class for the TTLSSchematicComment Class Methods.
  //
  TestTTLSSchematicComment = Class(TExtendedTestCase)
  Strict Private
    FTLSSchematicComment : TTLSSchematicComment;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreateComment;
  End;

  //
  // Test Class for the TTLSSchematicModule Class Methods.
  //
  TestTTLSSchematicModule = Class(TExtendedTestCase)
  Strict Private
    FTLSSchematicModule : TTLSSchematicModule;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreateParser;
    Procedure TestDirectives;
    Procedure TestReferenceSymbol;
    Procedure TestReservedWords;
    Procedure TestGoal;
    Procedure TestRoad;
    Procedure TestObject;
    Procedure TextEllipse;
    Procedure TestMargins;
    Procedure TestSpacing;
    Procedure TestStartChainage;
    Procedure TestEndChainage;
    Procedure TestStartOffset;
    Procedure TestEndOffset;
    Procedure TestLocation;
    Procedure TestColourName;
    Procedure TestDebugging;
    Procedure TestCentreLine;
    Procedure TestNamedObjectWidths;
    Procedure TestLines;
    Procedure TestNoText;
    Procedure TestTextOrientation;
    Procedure TestText;
    Procedure TestStaticEllipse;
    Procedure TestStaticObject;
    Procedure TestDiamond;
    Procedure TestStaticDiamond;
  End;

Implementation

Uses
  BaseLanguageModule, EidolonTypes;

//
// Test Methods for Class TTLSSchematicComment.
//
Procedure TestTTLSSchematicComment.Setup;
Begin
  FTLSSchematicComment := TTLSSchematicComment.Create('This is a comment.', 12, 23);
End;

Procedure TestTTLSSchematicComment.TearDown;

Begin
  FTLSSchematicComment.Free;
End;

Procedure TestTTLSSchematicComment.TestCreateComment;

var
  FComment: TComment;

Begin
  FComment := TTLSSchematicComment.CreateComment('/** This is a comment. **/', 12, 23);
  Try
    CheckEquals(12, FComment.Line);
    CheckEquals(23, FComment.Column);
    CheckEquals('This is a comment.', FComment.AsString(9999, True));
  Finally
    FComment.Free;
  End;
End;

//
// Test Methods for Class TTLSSchematicModule.
//
procedure TestTTLSSchematicModule.TestNoText;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'Notext ''Gantry Beam Erection'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strNoTexts, M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Gantry Beam Erection', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(False, M.SuppressedText['Gantry Beam Erectio']);
    CheckEquals(True, M.SuppressedText['Gantry Beam Erection']);
  Finally
    M.Free;
  End;
end;

Procedure TestTTLSSchematicModule.Setup;

Const
  strSource =
    'Roads 2%;'#13#10 +
    'Road -1000.0, 2000.0, 1, 1, Left, Blue;'#13#10 +
    'Road -1000.0, 2000.0, 1, 1, Right, Blue;'#13#10 +
    'Roads 4%;'#13#10 +
    'Road -1000.0, 2000.0, 2, 2, Left, Blue, ''Route Code'';'#13#10 +
    'Road -1000.0, 2000.0, 2, 2, Right, Blue, ''Route Code'';'#13#10 +
    'DEBUG;'#13#10 +
    'Objects 2%;'#13#10 +
    'Object 100, 200, Left, Blue, ''This is an object'';'#13#10 +
    'Object 200, 300, Right, Blue, ''This is an object'';'#13#10 +
    'Object 300, 400, Both, Blue, ''This is an object'';'#13#10 +
    'CENTRELINE 45.6%;'#13#10 +
    'Objects 4%;'#13#10 +
    'Object 400, 500, Over, Blue, ''This is an object'';'#13#10 +
    'Object 400, 500, Over, Blue, ''This is an object'', ''Route Code'';'#13#10 +
    'Object 500, 600, Under, Blue, ''This is an object'';'#13#10 +
    'Margins 2.5%;'#13#10 +
    'Spacing 2.5%;'#13#10;

Begin
  FTLSSchematicModule := TTLSSchematicModule.CreateParser(
    strSource, 'D:\Path\Time Chainage.ext', False, [moParse]);
End;

Procedure TestTTLSSchematicModule.TearDown;

Begin
  FTLSSchematicModule.Free;
End;

Procedure TestTTLSSchematicModule.TestAsString;

Begin
  CheckEquals('Time Chainage Schematic Diagram',
    FTLSSchematicModule.AsString(True, True));
End;

procedure TestTTLSSchematicModule.TestCentreLine;

Var
  Ss, C : TElementContainer;

begin
  Ss := FTLSSchematicModule.FindElement('Settings');
  Check(Nil <> Ss);
  CheckEquals(3, Ss.ElementCount);
  C := Ss.FindElement('CentreLine');
  Check(Nil <> C);
  CheckEquals('CentreLine 45.6%', C.AsString(True, True));
end;

procedure TestTTLSSchematicModule.TestColourName;

var
  M: TBaseLanguageModule;
  strSource : String;
  iColour: TColour;

begin
  For iColour := Low(TColour) To High(TColour) Do
    Begin
      strSource :=
        'Road -1000, 2000, 1, 1, Left, ' + strColours[iColour] + ';'#13#10;
      M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
      Try
        CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
        CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
        CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
        CheckEquals(1, M.ElementCount);
        CheckEquals('Roads', M.Elements[1].AsString(True, True));
        CheckEquals(1, M.Elements[1].ElementCount);
        CheckEquals('Road -1000.0, 2000.0, 1, 1, Left, ' + strColours[iColour] + ', 5.0%, ''*''',
          M.Elements[1].Elements[1].AsString(True, True));
      Finally
        M.Free;
      End;
    End;
end;

Procedure TestTTLSSchematicModule.TestCreateParser;

Begin
  CheckEquals(0, FTLSSchematicModule.HeadingCount(strErrors), FTLSSchematicModule.FirstError);
  CheckEquals(0, FTLSSchematicModule.HeadingCount(strHints), FTLSSchematicModule.FirstHint);
  CheckEquals(0, FTLSSchematicModule.HeadingCount(strWarnings), FTLSSchematicModule.FirstWarning);
End;

procedure TestTTLSSchematicModule.TestDebugging;
begin
  CheckEquals(True, FTLSSchematicModule.Debug);
end;

procedure TestTTLSSchematicModule.TestDiamond;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'DIAMOND 900, 1100, Left, Yellow, ''My Diamond'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals('DIAMOND 900.0, 1100.0, Left, Yellow, ''My Diamond'', 5.0%, ''*''', (M.Elements[1].Elements[1] As TTLSObject).AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTTLSSchematicModule.TestDirectives;

Begin
  Check(Nil = FTLSSchematicModule.Directives);
End;

procedure TestTTLSSchematicModule.TestStaticDiamond;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'STATICDIAMOND 900, 1100, 45%, 55%, Left, Yellow, ''My Diamond'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals('STATICDIAMOND 900.0, 1100.0, 45%, 55%, Left, Yellow, ''My Diamond''', (M.Elements[1].Elements[1] As TTLSObject).AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestStaticEllipse;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'STATICELLIPSE 900, 1100, 45%, 55%, Left, Yellow, ''My Ellipse'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals('STATICELLIPSE 900.0, 1100.0, 45%, 55%, Left, Yellow, ''My Ellipse''', (M.Elements[1].Elements[1] As TTLSObject).AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestStaticObject;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'STATICOBJECT 900, 1100, 45%, 55%, Left, Yellow, ''My Object'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals('STATICOBJECT 900.0, 1100.0, 45%, 55%, Left, Yellow, ''My Object''', (M.Elements[1].Elements[1] As TTLSObject).AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestEndChainage;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Road 1234.5, 4567.8, 1, 1, Left, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Road 1234.5, 4567.8, 1, 1, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestEndOffset;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Road 1234.5, 4567.8, 2, 1, Left, Black;'#13#10 +
    'Road 1234.5, 4567.8, 2, -1, Left, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].ElementCount);
    CheckEquals('Road 1234.5, 4567.8, 2, 1, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Road 1234.5, 4567.8, 2, -1, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestGoal;

var
  R, S, O: TElementContainer;

begin
  CheckEquals(3, FTLSSchematicModule.ElementCount);
  R := FTLSSchematicModule.FindElement('Roads');
  Check(R <> Nil);
  O := FTLSSchematicModule.FindElement('Objects');
  Check(O <> Nil);
  S := FTLSSchematicModule.FindElement('Settings');
  Check(S <> Nil);
end;

procedure TestTTLSSchematicModule.TestLines;

var
  M: TBaseLanguageModule;
  strSource : String;
  R: TTLSRoad;

begin
  strSource :=
    'Road 1234.5, 4567.8, 1, 1, Left, Black;'#13#10 +
    'LINES Blue, Dash, 0.5;'#13#10 +
    'Road 1234.5, 4567.8, 1, 1, Left, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].ElementCount);
    R := M.Elements[1].Elements[1] As TTLSRoad;
    Check(R.LineColour = xlcBlack);
    Check(R.LineStyle = lsSolid);
    Check(R.LineWeight = lw0_25);
    R := M.Elements[1].Elements[2] As TTLSRoad;
    Check(R.LineColour = xlcBlue);
    Check(R.LineStyle = lsDash);
    Check(R.LineWeight = lw0_5);
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestLocation;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Road 1234.5, 4567.8, 1, 1, Left, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Road 1234.5, 4567.8, 1, 1, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'Road 1234.5, 4567.8, 1, 1, Right, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Road 1234.5, 4567.8, 1, 1, Right, Black, 5.0%, ''*''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestMargins;

Var
  Ms, M : TElementContainer;

begin
  Ms := FTLSSchematicModule.FindElement('Settings');
  Check(Nil <> Ms);
  CheckEquals(3, Ms.ElementCount);
  M := Ms.FindElement('Margins');
  Check(Nil <> M);
  CheckEquals('Margins 2.5%', M.AsString(True, True));
end;

procedure TestTTLSSchematicModule.TestNamedObjectWidths;

Var
  strSource : String;
  M : TTLSSchematicModule;
  NWs: TElementContainer;
  NW: TElementContainer;

Begin
  strSource :=
    'Objects 2%, ''Civils'';'#13#10 +
    'Objects 4%, ''Permanent Way'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    NWs := M.FindElement(strNamedWidths);
    Check(NWs <> Nil);
    NW := NWs.FindElement('');
    Check(NW = Nil);
    NW := NWs.FindElement('Civils');
    Check(NW <> Nil);
    CheckEquals(2, (NW As TSchematicSetting).Percentage);
    NW := NWs.FindElement('Permanent Way');
    Check(NW <> Nil);
    CheckEquals(4, (NW As TSchematicSetting).Percentage);
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestObject;

var
  Os, O: TElementContainer;

begin
  Os := FTLSSchematicModule.FindElement('Objects');
  Check(Nil <> Os);
  CheckEquals(6, Os.ElementCount);
  O := Os.Elements[1];
  CheckEquals('OBJECT 100.0, 200.0, Left, Blue, ''This is an object'', 2.0%, ''*''', O.AsString(True, True));
  O := Os.Elements[2];
  CheckEquals('OBJECT 200.0, 300.0, Right, Blue, ''This is an object'', 2.0%, ''*''', O.AsString(True, True));
  O := Os.Elements[3];
  CheckEquals('OBJECT 300.0, 400.0, Both, Blue, ''This is an object'', 2.0%, ''*''', O.AsString(True, True));
  O := Os.Elements[4];
  CheckEquals('OBJECT 400.0, 500.0, Over, Blue, ''This is an object'', 4.0%, ''*''', O.AsString(True, True));
  O := Os.Elements[5];
  CheckEquals('OBJECT 400.0, 500.0, Over, Blue, ''This is an object'', 4.0%, ''Route Code''', O.AsString(True, True));
  O := Os.Elements[6];
  CheckEquals('OBJECT 500.0, 600.0, Under, Blue, ''This is an object'', 4.0%, ''*''', O.AsString(True, True));
end;

Procedure TestTTLSSchematicModule.TestReferenceSymbol;

Var
  T : TTokenInfo;

Begin
  T := TTokenInfo.Create('Hello', 1, 2, 3, 5, ttIdentifier);
  Try
    CheckEquals(False, FTLSSchematicModule.ReferenceSymbol(T));
  Finally
    T.Free;
  End;
End;

Procedure TestTTLSSchematicModule.TestReservedWords;
var
  W: TKeyWords;

Begin
  W := FTLSSchematicModule.ReservedWords;
  CheckEquals(0, Low(W));
  CheckEquals(16, High(W));
  CheckEquals('centreline',      W[0]);
  CheckEquals('debug',           W[1]);
  CheckEquals('diamond',         W[2]);
  CheckEquals('ellipse',         W[3]);
  CheckEquals('lines',           W[4]);
  CheckEquals('margins',         W[5]);
  CheckEquals('notext',          W[6]);
  CheckEquals('object',          W[7]);
  CheckEquals('objects',         W[8]);
  CheckEquals('road',            W[9]);
  CheckEquals('roads',           W[10]);
  CheckEquals('spacing',         W[11]);
  CheckEquals('staticdiamond',   W[12]);
  CheckEquals('staticellipse',   W[13]);
  CheckEquals('staticobject',    W[14]);
  CheckEquals('text',            W[15]);
  CheckEquals('textorientation', W[16]);
End;


procedure TestTTLSSchematicModule.TestRoad;

var
  Rs: TElementContainer;
  R: TElementContainer;

begin
  Rs := FTLSSchematicModule.FindElement('Roads');
  Check(Nil <> Rs);
  CheckEquals(4, Rs.ElementCount);
  R := Rs.Elements[1];
  CheckEquals('Road -1000.0, 2000.0, 1, 1, Left, Blue, 2.0%, ''*''', R.AsString(True, True));
  R := Rs.Elements[2];
  CheckEquals('Road -1000.0, 2000.0, 1, 1, Right, Blue, 2.0%, ''*''', R.AsString(True, True));
  R := Rs.Elements[3];
  CheckEquals('Road -1000.0, 2000.0, 2, 2, Left, Blue, 4.0%, ''Route Code''', R.AsString(True, True));
  R := Rs.Elements[4];
  CheckEquals('Road -1000.0, 2000.0, 2, 2, Right, Blue, 4.0%, ''Route Code''', R.AsString(True, True));
end;

procedure TestTTLSSchematicModule.TestSpacing;

Var
  Ss, S : TElementContainer;

begin
  Ss := FTLSSchematicModule.FindElement('Settings');
  Check(Nil <> Ss);
  CheckEquals(3, Ss.ElementCount);
  S := Ss.FindElement('Spacing');
  Check(Nil <> S);
  CheckEquals('Spacing 2.5%', S.AsString(True, True));
end;

procedure TestTTLSSchematicModule.TestStartChainage;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Road 1234.5, 2000, 1, 1, Left, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('Road 1234.5, 2000.0, 1, 1, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestStartOffset;

var
  M: TBaseLanguageModule;
  strSource : String;

begin
  strSource :=
    'Road 1234.5, 4567.8, 1, 2, Left, Black;'#13#10 +
    'Road 1234.5, 4567.8, -1, 2, Left, Black;'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('Roads', M.Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].ElementCount);
    CheckEquals('Road 1234.5, 4567.8, 1, 2, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals('Road 1234.5, 4567.8, -1, 2, Left, Black, 5.0%, ''*''', M.Elements[1].Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestText;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'Object 123, 234, Over, Blue, ''An Object'';'#13#10 +
    'Text Inside;'#13#10 +
    'Text Inside, ''Gantry Beam Erection'';'#13#10 +
    'Object 123, 234, Over, Blue, ''An Object'';'#13#10 +
    'Text Outside;'#13#10 +
    'Object 123, 234, Over, Blue, ''An Object'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(3, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals(Integer(tpOutside), Integer((M.Elements[1].Elements[1] As TTLSObject).TextPosition));
    Check(M.Elements[1].Elements[2] Is TTLSObject);
    CheckEquals(Integer(tpInside), Integer((M.Elements[1].Elements[2] As TTLSObject).TextPosition));
    Check(M.Elements[1].Elements[3] Is TTLSObject);
    CheckEquals(Integer(tpOutside), Integer((M.Elements[1].Elements[3] As TTLSObject).TextPosition));
    CheckEquals(Integer(tpOutside), Integer(M.TextPosition['Gantry Beam Erectio']));
    CheckEquals(Integer(tpInside), Integer(M.TextPosition['Gantry Beam Erection']));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TestTextOrientation;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'Object 123, 234, Over, Blue, ''An Object'';'#13#10 +
    'TextOrientation Vertical, ''Gantry Beam Erection'';'#13#10 +
    'Object 123, 234, Over, Blue, ''An Object'';'#13#10 +
    'TextOrientation Vertical;'#13#10 +
    'Object 123, 234, Over, Blue, ''An Object'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(3, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals(Integer(toHorizontal), Integer((M.Elements[1].Elements[1] As TTLSObject).TextOrientation));
    Check(M.Elements[1].Elements[2] Is TTLSObject);
    CheckEquals(Integer(toHorizontal), Integer((M.Elements[1].Elements[2] As TTLSObject).TextOrientation));
    Check(M.Elements[1].Elements[3] Is TTLSObject);
    CheckEquals(Integer(toVertical), Integer((M.Elements[1].Elements[3] As TTLSObject).TextOrientation));
    CheckEquals(Integer(toHorizontal), Integer(M.TextOrientation['Gantry Beam Erectio']));
    CheckEquals(Integer(toVertical), Integer(M.TextOrientation['Gantry Beam Erection']));
  Finally
    M.Free;
  End;
end;

procedure TestTTLSSchematicModule.TextEllipse;

var
  M: TTLSSchematicModule;
  strSource : String;

begin
  strSource :=
    'ELLIPSE 900, 1100, Left, Yellow, ''My Ellipse'';'#13#10;
  M := TTLSSchematicModule.CreateParser(strSource, 'D:\Path\MyFile.Schematic', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(strObjects, M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Check(M.Elements[1].Elements[1] Is TTLSObject);
    CheckEquals('ELLIPSE 900.0, 1100.0, Left, Yellow, ''My Ellipse'', 5.0%, ''*''', (M.Elements[1].Elements[1] As TTLSObject).AsString(True, True));
  Finally
    M.Free;
  End;
end;

Initialization
  RegisterTest('TLS Schematic Module', TestTTLSSchematicComment.Suite);
  RegisterTest('TLS Schematic Module', TestTTLSSchematicModule.Suite);
End.