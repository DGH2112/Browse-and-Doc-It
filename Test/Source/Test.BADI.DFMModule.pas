Unit Test.BADI.DFMModule;

Interface

Uses
  TestFramework,
  BADI.DFMModule,
  Test.BADI.BaseLanguageModule;

Type
  //
  // Test Class for the TDFMModule Class Methods.
  //
  TestTDFMModule = Class(TExtendedTestCase)
  Strict Private
    FDFMModule : TDFMModule;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreateParser;
    Procedure TestTokenizeStream;
    Procedure TestReservedWords;
    Procedure TestDFMObject;
    Procedure TestDFMProperty;
    Procedure TestDFMIdentifier;
    Procedure TestStringLiteral;
    Procedure TestNumber;
    Procedure TestDFMSet;
    Procedure TestItemList;
    Procedure TestBinaryData;
    Procedure TestListData;
    Procedure TestQualifiedIdent;
    Procedure TestIdentList;
    Procedure TestItem;
    Procedure TestListDataElements;
    Procedure TestFailure01;
  End;

  //
  // Test Class for the TDFMObject Class Methods.
  //
  TestTDFMObject = Class(TExtendedTestCase)
  Strict Private
    FDFMObject : TDFMObject;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

  //
  // Test Class for the TDFMProperty Class Methods.
  //
  TestTDFMProperty = Class(TExtendedTestCase)
  Strict Private
    FDFMProperty : TDFMProperty;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.BaseLanguageModule;

//
// Test Methods for Class TDFMModule.
//
Procedure TestTDFMModule.Setup;
Begin
  FDFMModule := TDFMModule.CreateParser('', 'D:\Path\DFMFile.dfm', True, [moParse]);
End;

Procedure TestTDFMModule.TearDown;

Begin
  FDFMModule.Free;
End;

Procedure TestTDFMModule.TestAsString;

Begin
  CheckEquals('DFMFile.dfm', FDFMModule.AsString(True, True));
End;

procedure TestTDFMModule.TestBinaryData;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = {'#13#10+
    '    123456ffabced'#13#10 +
    '    87450382384df}'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = {'#13#10'    123456ffabced'#13#10'    87450382384df}',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTDFMModule.TestCreateParser;

Begin
  CheckEquals('D:\Path\DFMFile.dfm', FDFMModule.Identifier);
End;

procedure TestTDFMModule.TestDFMIdentifier;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = AnIdentifier'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = AnIdentifier', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = Oops.AnIdentifier'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = Oops.AnIdentifier', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = MoreOops.Oops.AnIdentifier'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = MoreOops.Oops.AnIdentifier', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestDFMObject;

Var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  object MyIdentifier : TObject'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('Object MyIdentifier : TObject', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  object MyIdentifier : TObject'#13#10 +
    '  end'#13#10 +
    '  object MyIdentifier2 : TObject'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].ElementCount);
    Checkequals('Object MyIdentifier : TObject', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals('Object MyIdentifier2 : TObject', M.Elements[1].Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  object MyIdentifier : TObject[1]'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('Object MyIdentifier : TObject[1]', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestDFMProperty;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = AnIdentifier'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = AnIdentifier', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = AnIdentifier'#13#10 +
    '  MyIdentifier2 = AnIdentifier2'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(2, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = AnIdentifier', M.Elements[1].Elements[1].AsString(True, True));
    Checkequals('MyIdentifier2 = AnIdentifier2', M.Elements[1].Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  TestGrammarForErrors(
    TDFMModule,
    '%s',
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = AnIdentifier'#13#10 +
    '  MyIdentifier2 = AnIdentifier2'#13#10 +
    '  inline Identifier10 : TfrmMyFrame'#13#10 +
    '    MyIdentifier11 = AnIdentifier11'#13#10 +
    '    MyIdentifier12 = AnIdentifier12'#13#10 +
    '  end'#13#10 +
    'end',
    '',
    [ttErrors, ttWarnings],
    [
      'Identifier|Object Identifier : TfrmMyForm|scPublic',
      'Identifier\MyIdentifier|MyIdentifier = AnIdentifier|scPublic',
      'Identifier\MyIdentifier2|MyIdentifier2 = AnIdentifier2|scPublic',
      'Identifier\Identifier10|Inline Identifier10 : TfrmMyFrame|scPublic',
      'Identifier\Identifier10\MyIdentifier11|MyIdentifier11 = AnIdentifier11|scPublic',
      'Identifier\Identifier10\MyIdentifier12|MyIdentifier12 = AnIdentifier12|scPublic'
    ]
  );
end;

procedure TestTDFMModule.TestDFMSet;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = []'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = []', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestFailure01;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object frmLoadFromCRD: TfrmLoadFromCRD'#13#10 +
    '  object pnlPanel1: TPanel'#13#10 +
    '    Left = 4'#13#10 +
    '    Top = 4'#13#10 +
    '    Width = 229'#13#10 +
    '    Height = 201'#13#10 +
    '    Caption = ''pnlPanel1'''#13#10 +
    '    TabOrder = 0'#13#10 +
    '    object lbStrings: TRzTabbedListBox'#13#10 +
    '      Left = 1'#13#10 +
    '      Top = 18'#13#10 +
    '      Width = 227'#13#10 +
    '      Height = 182'#13#10 +
    '      TabStops.Min = -2147483647'#13#10 +
    '      TabStops.Max = 2147483647'#13#10 +
    '      TabStops.Integers = ('#13#10 +
    '        -21'#13#10 +
    '        -31)'#13#10 +
    '      Align = alClient'#13#10 +
    '      ItemHeight = 13'#13#10 +
    '      TabOrder = 1'#13#10 +
    '      OnDblClick = lbStringsDblClick'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestIdentList;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = [Ident1]'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = [Ident1]',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = [Ident1, Ident2]'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = [Ident1, Ident2]',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = [Ident1, Ident2, Ident3]'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = [Ident1, Ident2, Ident3]',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestItem;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = <'#13#10 +
    '    Item'#13#10 +
    '    end'#13#10 +
    '  >'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = <>', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].ElementCount);
    Checkequals('Item', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = <'#13#10 +
    '    Item'#13#10 +
    '      MyOtherProp = ''Hello'''#13#10 +
    '    end'#13#10 +
    '  >'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = <>', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].ElementCount);
    Checkequals('Item', M.Elements[1].Elements[1].Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].Elements[1].Elements[1].ElementCount);
    Checkequals('MyOtherProp = ''Hello''', M.Elements[1].Elements[1].Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestItemList;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = <'#13#10 +
    '  >'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = <>', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = <>'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = <>', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTDFMModule.TestReservedWords;

Var
  Words : TKeyWords;
  i : Integer;

Begin
  Words := FDFMModule.ReservedWords;
  CheckEquals(0, Low(Words));
  CheckEquals(3, High(Words));
  CheckEquals('end', Words[0]);
  CheckEquals('inherited', Words[1]);
  CheckEquals('inline', Words[2]);
  CheckEquals('object', Words[3]);
  For i := Low(Words) To Pred(High(Words)) Do
    Check(Words[i] < Words[i + 1], Words[i] + '!<' + Words[i + 1]);
End;

procedure TestTDFMModule.TestNumber;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = 1000'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = 1000', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = -1000'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = -1000', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = $1000'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = $1000', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = $00FF'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = $00FF', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = 10.00'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = 10.00', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestListDataElements;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ('#13#10 +
    '    123'#13#10 +
    '    456)'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = (123 456)', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ('#13#10 +
    '    ''Hello'''#13#10 +
    '    ''Goodbye'')'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = (''Hello'' ''Goodbye'')', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ('#13#10 +
    '    ''Hell'' +'#13#10 +
    '      ''o'''#13#10 +
    '    ''Goodbye'')'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = (''Hell'' + ''o'' ''Goodbye'')', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestListData;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ('#13#10+
    '    )'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = ()',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestQualifiedIdent;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = 1234'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = 1234',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  Oops.MyIdentifier = 1234'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('Oops.MyIdentifier = 1234',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MoreOops.Oops.MyIdentifier = 1234'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MoreOops.Oops.MyIdentifier = 1234',
      M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTDFMModule.TestStringLiteral;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ''This is a string literal.'''#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = ''This is a string literal.''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ''This is a string literal.'' +'#13#10 +
    '    ''This is a second string.'''#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = ''This is a string literal.'' + ''This is a second string.''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ''This is a string literal.''#9''This is a second string.'''#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(1, M.ElementCount);
    Checkequals('Object Identifier : TfrmMyForm', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    Checkequals('MyIdentifier = ''This is a string literal.''#9''This is a second string.''', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;


procedure TestTDFMModule.TestTokenizeStream;

var
  strSource : String;

Var
  M : TBaseLanguageModule;

begin
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = 12.00'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, []);
  Try
    CheckEquals(9, M.TokenCount);
    CheckEquals('object', M.Tokens[0].Token);
    CheckEquals(ttReservedWord, M.Tokens[0].TokenType);
    CheckEquals('Identifier', M.Tokens[1].Token);
    CheckEquals(ttIdentifier, M.Tokens[1].TokenType);
    CheckEquals(':', M.Tokens[2].Token);
    CheckEquals(ttSymbol, M.Tokens[2].TokenType);
    CheckEquals('TfrmMyForm', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('MyIdentifier', M.Tokens[4].Token);
    CheckEquals(ttIdentifier, M.Tokens[4].TokenType);
    CheckEquals('=', M.Tokens[5].Token);
    CheckEquals(ttSymbol, M.Tokens[5].TokenType);
    CheckEquals('12.00', M.Tokens[6].Token);
    CheckEquals(ttNumber, M.Tokens[6].TokenType);
    CheckEquals('end', M.Tokens[7].Token);
    CheckEquals(ttReservedWord, M.Tokens[7].TokenType);
    CheckEquals('<FileEnd>', M.Tokens[8].Token);
    CheckEquals(ttFileEnd, M.Tokens[8].TokenType);
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ''Hello''#9''Goodbye'''#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, []);
  Try
    CheckEquals(11, M.TokenCount);
    CheckEquals('object', M.Tokens[0].Token);
    CheckEquals(ttReservedWord, M.Tokens[0].TokenType);
    CheckEquals('Identifier', M.Tokens[1].Token);
    CheckEquals(ttIdentifier, M.Tokens[1].TokenType);
    CheckEquals(':', M.Tokens[2].Token);
    CheckEquals(ttSymbol, M.Tokens[2].TokenType);
    CheckEquals('TfrmMyForm', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('MyIdentifier', M.Tokens[4].Token);
    CheckEquals(ttIdentifier, M.Tokens[4].TokenType);
    CheckEquals('=', M.Tokens[5].Token);
    CheckEquals(ttSymbol, M.Tokens[5].TokenType);
    CheckEquals('''Hello''', M.Tokens[6].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[6].TokenType);
    CheckEquals('#9', M.Tokens[7].Token);
    CheckEquals(ttDoubleLiteral, M.Tokens[7].TokenType);
    CheckEquals('''Goodbye''', M.Tokens[8].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[8].TokenType);
    CheckEquals('end', M.Tokens[9].Token);
    CheckEquals(ttReservedWord, M.Tokens[9].TokenType);
    CheckEquals('<FileEnd>', M.Tokens[10].Token);
    CheckEquals(ttFileEnd, M.Tokens[10].TokenType);
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ('#13#10 +
    '    ''#Hello'''#13#10 +
    '    ''#Goodbye'')'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, []);
  Try
    CheckEquals(12, M.TokenCount);
    CheckEquals('object', M.Tokens[0].Token);
    CheckEquals(ttReservedWord, M.Tokens[0].TokenType);
    CheckEquals('Identifier', M.Tokens[1].Token);
    CheckEquals(ttIdentifier, M.Tokens[1].TokenType);
    CheckEquals(':', M.Tokens[2].Token);
    CheckEquals(ttSymbol, M.Tokens[2].TokenType);
    CheckEquals('TfrmMyForm', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('MyIdentifier', M.Tokens[4].Token);
    CheckEquals(ttIdentifier, M.Tokens[4].TokenType);
    CheckEquals('=', M.Tokens[5].Token);
    CheckEquals(ttSymbol, M.Tokens[5].TokenType);
    CheckEquals('(', M.Tokens[6].Token);
    CheckEquals(ttSymbol, M.Tokens[6].TokenType);
    CheckEquals('''#Hello''', M.Tokens[7].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[7].TokenType);
    CheckEquals('''#Goodbye''', M.Tokens[8].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[8].TokenType);
    CheckEquals(')', M.Tokens[9].Token);
    CheckEquals(ttSymbol, M.Tokens[9].TokenType);
    CheckEquals('end', M.Tokens[10].Token);
    CheckEquals(ttReservedWord, M.Tokens[10].TokenType);
    CheckEquals('<FileEnd>', M.Tokens[11].Token);
    CheckEquals(ttFileEnd, M.Tokens[11].TokenType);
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ('#13#10 +
    '    ''Hello%'''#13#10 +
    '    ''Good%bye'')'#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, []);
  Try
    CheckEquals(12, M.TokenCount);
    CheckEquals('object', M.Tokens[0].Token);
    CheckEquals(ttReservedWord, M.Tokens[0].TokenType);
    CheckEquals('Identifier', M.Tokens[1].Token);
    CheckEquals(ttIdentifier, M.Tokens[1].TokenType);
    CheckEquals(':', M.Tokens[2].Token);
    CheckEquals(ttSymbol, M.Tokens[2].TokenType);
    CheckEquals('TfrmMyForm', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('MyIdentifier', M.Tokens[4].Token);
    CheckEquals(ttIdentifier, M.Tokens[4].TokenType);
    CheckEquals('=', M.Tokens[5].Token);
    CheckEquals(ttSymbol, M.Tokens[5].TokenType);
    CheckEquals('(', M.Tokens[6].Token);
    CheckEquals(ttSymbol, M.Tokens[6].TokenType);
    CheckEquals('''Hello%''', M.Tokens[7].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[7].TokenType);
    CheckEquals('''Good%bye''', M.Tokens[8].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[8].TokenType);
    CheckEquals(')', M.Tokens[9].Token);
    CheckEquals(ttSymbol, M.Tokens[9].TokenType);
    CheckEquals('end', M.Tokens[10].Token);
    CheckEquals(ttReservedWord, M.Tokens[10].TokenType);
    CheckEquals('<FileEnd>', M.Tokens[11].Token);
    CheckEquals(ttFileEnd, M.Tokens[11].TokenType);
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ''1234567890-=!"£$%^&*()_+qwertyuiop[]QWERTYUIOP{}asdfghjkl;#ASDFGHJKL:@~zxcvbnm,./ZXCVBNM<>?`¬'''#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, []);
  Try
    CheckEquals(9, M.TokenCount);
    CheckEquals('object', M.Tokens[0].Token);
    CheckEquals(ttReservedWord, M.Tokens[0].TokenType);
    CheckEquals('Identifier', M.Tokens[1].Token);
    CheckEquals(ttIdentifier, M.Tokens[1].TokenType);
    CheckEquals(':', M.Tokens[2].Token);
    CheckEquals(ttSymbol, M.Tokens[2].TokenType);
    CheckEquals('TfrmMyForm', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('MyIdentifier', M.Tokens[4].Token);
    CheckEquals(ttIdentifier, M.Tokens[4].TokenType);
    CheckEquals('=', M.Tokens[5].Token);
    CheckEquals(ttSymbol, M.Tokens[5].TokenType);
    CheckEquals('''1234567890-=!"£$%^&*()_+qwertyuiop[]QWERTYUIOP{}asdfghjkl;#ASDFGHJKL:@~zxcvbnm,./ZXCVBNM<>?`¬''', M.Tokens[6].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[6].TokenType);
    CheckEquals('end', M.Tokens[7].Token);
    CheckEquals(ttReservedWord, M.Tokens[7].TokenType);
    CheckEquals('<FileEnd>', M.Tokens[8].Token);
    CheckEquals(ttFileEnd, M.Tokens[8].TokenType);
  Finally
    M.Free;
  End;
  strSource :=
    'object Identifier : TfrmMyForm'#13#10 +
    '  MyIdentifier = ''!"#$%&''#39''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`'''#13#10 +
    'end'#13#10;
  M := TDFMModule.CreateParser(strSource, 'D:\Path\DFMFile.dfm', True, []);
  Try
    CheckEquals(11, M.TokenCount);
    CheckEquals('object', M.Tokens[0].Token);
    CheckEquals(ttReservedWord, M.Tokens[0].TokenType);
    CheckEquals('Identifier', M.Tokens[1].Token);
    CheckEquals(ttIdentifier, M.Tokens[1].TokenType);
    CheckEquals(':', M.Tokens[2].Token);
    CheckEquals(ttSymbol, M.Tokens[2].TokenType);
    CheckEquals('TfrmMyForm', M.Tokens[3].Token);
    CheckEquals(ttIdentifier, M.Tokens[3].TokenType);
    CheckEquals('MyIdentifier', M.Tokens[4].Token);
    CheckEquals(ttIdentifier, M.Tokens[4].TokenType);
    CheckEquals('=', M.Tokens[5].Token);
    CheckEquals(ttSymbol, M.Tokens[5].TokenType);
    CheckEquals('''!"#$%&''', M.Tokens[6].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[6].TokenType);
    CheckEquals('#39', M.Tokens[7].Token);
    CheckEquals(ttDoubleLiteral, M.Tokens[7].TokenType);
    CheckEquals('''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`''', M.Tokens[8].Token);
    CheckEquals(ttSingleLiteral, M.Tokens[8].TokenType);
    CheckEquals('end', M.Tokens[9].Token);
    CheckEquals(ttReservedWord, M.Tokens[9].TokenType);
    CheckEquals('<FileEnd>', M.Tokens[10].Token);
    CheckEquals(ttFileEnd, M.Tokens[10].TokenType);
  Finally
    M.Free;
  End;
end;

//
// Test methods for the class TDFMObject.
//
Procedure TestTDFMObject.Setup;

Begin
  FDFMObject := TDFMObject.Create('Identifier', scPublic, 12, 23, iiPublicObject, Nil);
  FDFMObject.AddToken('TfrmMyForm');
End;

Procedure TestTDFMObject.TearDown;

Begin
  FDFMObject.Free;
End;

Procedure TestTDFMObject.TestAsString;

Begin
  Checkequals('Object Identifier : TfrmMyForm', FDFMObject.AsString(True, True));
End;

procedure TestTDFMObject.TestCreate;
begin
  CheckEquals('Identifier', FDFMObject.Identifier);
  CheckEquals(scPublic, FDFMObject.Scope);
  CheckEquals(12, FDFMObject.Line);
  CheckEquals(23, FDFMObject.Column);
  CheckEquals(iiPublicObject, FDFMObject.ImageIndex);
  CheckEquals(iiPublicObject, FDFMObject.ImageIndexAdjustedForScope);
end;

//
// Test methods for the class TDFMProperty.
//
Procedure TestTDFMProperty.Setup;

Begin
  FDFMProperty := TDFMProperty.Create('MyIdentifier', scPublic, 12, 23, iiPublicProperty, Nil);
  FDFMProperty.AddToken('AnIdentifier');
End;

Procedure TestTDFMProperty.TearDown;

Begin
  FDFMProperty.Free;
End;

Procedure TestTDFMProperty.TestAsString;

Begin
  CheckEquals('MyIdentifier = AnIdentifier', FDFMProperty.AsString(True, True));
End;

procedure TestTDFMProperty.TestCreate;
begin
  CheckEquals('MyIdentifier', FDFMProperty.Identifier);
  CheckEquals(scPublic, FDFMProperty.Scope);
  CheckEquals(12, FDFMProperty.Line);
  CheckEquals(23, FDFMProperty.Column);
  CheckEquals(iiPublicProperty, FDFMProperty.ImageIndex);
  CheckEquals(iiPublicProperty, FDFMProperty.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('DFM Module', TestTDFMModule.Suite);
  RegisterTest('DFM Module', TestTDFMObject.Suite);
  RegisterTest('DFM Module', TestTDFMProperty.Suite);
End.