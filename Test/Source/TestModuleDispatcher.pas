Unit TestModuleDispatcher;

Interface

Uses
  TestFramework, ModuleDispatcher, TestBaseLanguageModule;

Type
  //
  // Test Class for the Functions Class Methods.
  //
  TestFunctions = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestModulesOrder;
    Procedure TestCanDocumentDocument;
    Procedure TestCanParseDocument;
    Procedure TestDispatcher;
    Procedure TestGetCommentType;
  End;

Implementation

Uses
  CommonIDEFunctions, BaseLanguageModule;

//
// Test Methods for Class Functions.
//
Procedure TestFunctions.TestCanDocumentDocument;

Begin
  Check(Not CanDocumentDocument('MyFile.ini'), 'MyFile.ini');
  Check(CanDocumentDocument('MyFile.bas'), 'MyFile.bas');
  Check(CanDocumentDocument('MyFile.bnf'), 'MyFile.bnf');
  Check(CanDocumentDocument('MyFile.cls'), 'MyFile.cls');
  Check(Not CanDocumentDocument('MyFile.dfm'), 'MyFile.dfm');
  Check(CanDocumentDocument('MyFile.dpk'), 'MyFile.dpk');
  Check(CanDocumentDocument('MyFile.dpr'), 'MyFile.dpr');
  Check(Not CanDocumentDocument('MyFile.dtd'), 'MyFile.dtd');
  Check(CanDocumentDocument('MyFile.frm'), 'MyFile.frm');
  Check(Not CanDocumentDocument('MyFile.htm'), 'MyFile.htm');
  Check(Not CanDocumentDocument('MyFile.html'), 'MyFile.html');
  Check(CanDocumentDocument('MyFile.map'), 'MyFile.map');
  Check(CanDocumentDocument('MyFile.pas'), 'MyFile.pas');
  Check(Not CanDocumentDocument('MyFile.xml'), 'MyFile.xml');
  Check(Not CanDocumentDocument('MyFile.xsd'), 'MyFile.xsd');
End;

Procedure TestFunctions.TestCanParseDocument;

Begin
  Check(Not CanParseDocument('MyFile.ini'), 'MyFile.ini');
  Check(CanParseDocument('MyFile.bas'), 'MyFile.bas');
  Check(CanParseDocument('MyFile.bnf'), 'MyFile.bnf');
  Check(CanParseDocument('MyFile.cls'), 'MyFile.cls');
  Check(CanParseDocument('MyFile.dfm'), 'MyFile.dfm');
  Check(CanParseDocument('MyFile.dpk'), 'MyFile.dpk');
  Check(CanParseDocument('MyFile.dpr'), 'MyFile.dpr');
  Check(CanParseDocument('MyFile.dtd'), 'MyFile.dtd');
  Check(CanParseDocument('MyFile.frm'), 'MyFile.frm');
  Check(CanParseDocument('MyFile.htm'), 'MyFile.htm');
  Check(CanParseDocument('MyFile.html'), 'MyFile.html');
  Check(CanParseDocument('MyFile.map'), 'MyFile.map');
  Check(CanParseDocument('MyFile.pas'), 'MyFile.pas');
  Check(CanParseDocument('MyFile.xml'), 'MyFile.xml');
  Check(CanParseDocument('MyFile.xsd'), 'MyFile.xsd');
End;

Procedure TestFunctions.TestDispatcher;

Var
  M : TBaseLanguageModule;

Begin
  M := ModuleDispatcher.Dispatcher('', 'MyFile.ini', False, []);
  Try
    Check(M = Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.bas', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.bnf', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.cls', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.dfm', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.dpk', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.dpr', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.dtd', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.frm', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.htm', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.html', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.map', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.pas', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.xml', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := ModuleDispatcher.Dispatcher('', 'MyFile.xsd', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
End;

Procedure TestFunctions.TestGetCommentType;

Begin
  CheckEquals(GetCommentType('MyFile.ini',  csBlock),  ctNone, 'MyFile.ini - Block');
  CheckEquals(GetCommentType('MyFile.ini',  csLine),   ctNone, 'MyFile.ini - Line');
  CheckEquals(GetCommentType('MyFile.ini',  csInSitu), ctNone, 'MyFile.ini - InSitu');
  CheckEquals(GetCommentType('MyFile.bas',  csBlock),  ctVBLine, 'MyFile.bas - Block');
  CheckEquals(GetCommentType('MyFile.bas',  csLine),   ctVBLine, 'MyFile.bas - Line');
  CheckEquals(GetCommentType('MyFile.bas',  csInSitu), ctVBLine, 'MyFile.bas - InSitu');
  CheckEquals(GetCommentType('MyFile.bnf',  csBlock),  ctCPPBlock, 'MyFile.bnf - Block');
  CheckEquals(GetCommentType('MyFile.bnf',  csLine),   ctCPPBlock, 'MyFile.bnf - Line');
  CheckEquals(GetCommentType('MyFile.bnf',  csInSitu), ctCPPBlock, 'MyFile.bnf - InSitu');
  CheckEquals(GetCommentType('MyFile.cls',  csBlock),  ctVBLine, 'MyFile.cls - Block');
  CheckEquals(GetCommentType('MyFile.cls',  csLine),   ctVBLine, 'MyFile.cls - Line');
  CheckEquals(GetCommentType('MyFile.cls',  csInSitu), ctVBLine, 'MyFile.cls - InSitu');
  CheckEquals(GetCommentType('MyFile.dfm',  csBlock),  ctPascalBlock, 'MyFile.dfm - Block');
  CheckEquals(GetCommentType('MyFile.dfm',  csLine),   ctPascalBlock, 'MyFile.dfm - Line');
  CheckEquals(GetCommentType('MyFile.dfm',  csInSitu), ctPascalBlock, 'MyFile.dfm - InSitu');
  CheckEquals(GetCommentType('MyFile.dpk',  csBlock),  ctPascalBlock, 'MyFile.dpk - Block');
  CheckEquals(GetCommentType('MyFile.dpk',  csLine),   ctPascalBlock, 'MyFile.dpk - Line');
  CheckEquals(GetCommentType('MyFile.dpk',  csInSitu), ctPascalBlock, 'MyFile.dpk - InSitu');
  CheckEquals(GetCommentType('MyFile.dpr',  csBlock),  ctPascalBlock, 'MyFile.dpr - Block');
  CheckEquals(GetCommentType('MyFile.dpr',  csLine),   ctPascalBlock, 'MyFile.dpr - Line');
  CheckEquals(GetCommentType('MyFile.dpr',  csInSitu), ctPascalBlock, 'MyFile.dpr - InSitu');
  CheckEquals(GetCommentType('MyFile.dtd',  csBlock),  ctXML, 'MyFile.dtd - Block');
  CheckEquals(GetCommentType('MyFile.dtd',  csLine),   ctXML, 'MyFile.dtd - Line');
  CheckEquals(GetCommentType('MyFile.dtd',  csInSitu), ctXML, 'MyFile.dtd - InSitu');
  CheckEquals(GetCommentType('MyFile.frm',  csBlock),  ctVBLine, 'MyFile.frm - Block');
  CheckEquals(GetCommentType('MyFile.frm',  csLine),   ctVBLine, 'MyFile.frm - Line');
  CheckEquals(GetCommentType('MyFile.frm',  csInSitu), ctVBLine, 'MyFile.frm - InSitu');
  CheckEquals(GetCommentType('MyFile.htm',  csBlock),  ctXML, 'MyFile.htm - Block');
  CheckEquals(GetCommentType('MyFile.htm',  csLine),   ctXML, 'MyFile.htm - Line');
  CheckEquals(GetCommentType('MyFile.htm',  csInSitu), ctXML, 'MyFile.htm - InSitu');
  CheckEquals(GetCommentType('MyFile.html', csBlock),  ctXML, 'MyFile.html - Block');
  CheckEquals(GetCommentType('MyFile.html', csLine),   ctXML, 'MyFile.html - Line');
  CheckEquals(GetCommentType('MyFile.html', csInSitu), ctXML, 'MyFile.html - InSitu');
  CheckEquals(GetCommentType('MyFile.map',  csBlock),  ctCPPBlock, 'MyFile.map - Block');
  CheckEquals(GetCommentType('MyFile.map',  csLine),   ctCPPLine, 'MyFile.map - Line');
  CheckEquals(GetCommentType('MyFile.map',  csInSitu), ctCPPBlock, 'MyFile.map - InSitu');
  CheckEquals(GetCommentType('MyFile.pas',  csBlock),  ctPascalBlock, 'MyFile.pas - Block');
  CheckEquals(GetCommentType('MyFile.pas',  csLine),   ctPascalBlock, 'MyFile.pas - Line');
  CheckEquals(GetCommentType('MyFile.pas',  csInSitu), ctPascalBlock, 'MyFile.pas - InSitu');
  CheckEquals(GetCommentType('MyFile.xml',  csBlock),  ctXML, 'MyFile.xml - Block');
  CheckEquals(GetCommentType('MyFile.xml',  csLine),   ctXML, 'MyFile.xml - Line');
  CheckEquals(GetCommentType('MyFile.xml',  csInSitu), ctXML, 'MyFile.xml - InSitu');
  CheckEquals(GetCommentType('MyFile.xsd',  csBlock),  ctXML, 'MyFile.xsd - Block');
  CheckEquals(GetCommentType('MyFile.xsd',  csLine),   ctXML, 'MyFile.xsd - Line');
  CheckEquals(GetCommentType('MyFile.xsd',  csInSitu), ctXML, 'MyFile.xsd - InSitu');
End;


procedure TestFunctions.TestModulesOrder;

Var
  i : Integer;

begin
  For i := Low(Modules) To Pred(High(Modules)) Do
    Check(Modules[i].FExt < Modules[i + 1].FExt, Modules[i].FExt + '!<' + Modules[i + 1].FExt);
end;

Initialization
  RegisterTest('Module Dispatcher Test', TestFunctions.Suite);
End.