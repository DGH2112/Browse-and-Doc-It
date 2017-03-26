Unit Test.BADI.Module.Dispatcher;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module;

Type
  //
  // Test Class for the Functions Class Methods.
  //
  TestFunctions = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestCanDocumentDocument;
    Procedure TestCanParseDocument;
    Procedure TestDispatcher;
    Procedure TestGetCommentType;
  End;

Implementation

Uses
  BADI.CommonIDEFunctions,
  BADI.Base.Module,
  BADI.Module.Dispatcher,
  BADI.Types;

//
// Test Methods for Class Functions.
//
Procedure TestFunctions.TestCanDocumentDocument;

Begin
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.css'), 'MyFile.css');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.bas'), 'MyFile.bas');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.bnf'), 'MyFile.bnf');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.cls'), 'MyFile.cls');
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.dfm'), 'MyFile.dfm');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.dpk'), 'MyFile.dpk');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.dpr'), 'MyFile.dpr');
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.dtd'), 'MyFile.dtd');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.frm'), 'MyFile.frm');
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.htm'), 'MyFile.htm');
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.html'), 'MyFile.html');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.map'), 'MyFile.map');
  Check(ModuleDispatcher.CanDocumentDocument('MyFile.pas'), 'MyFile.pas');
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.xml'), 'MyFile.xml');
  Check(Not ModuleDispatcher.CanDocumentDocument('MyFile.xsd'), 'MyFile.xsd');
End;

Procedure TestFunctions.TestCanParseDocument;

Begin
  Check(Not ModuleDispatcher.CanParseDocument('MyFile.css'), 'MyFile.css');
  Check(ModuleDispatcher.CanParseDocument('MyFile.bas'), 'MyFile.bas');
  Check(ModuleDispatcher.CanParseDocument('MyFile.bnf'), 'MyFile.bnf');
  Check(ModuleDispatcher.CanParseDocument('MyFile.cls'), 'MyFile.cls');
  Check(ModuleDispatcher.CanParseDocument('MyFile.dfm'), 'MyFile.dfm');
  Check(ModuleDispatcher.CanParseDocument('MyFile.dpk'), 'MyFile.dpk');
  Check(ModuleDispatcher.CanParseDocument('MyFile.dpr'), 'MyFile.dpr');
  Check(ModuleDispatcher.CanParseDocument('MyFile.dtd'), 'MyFile.dtd');
  Check(ModuleDispatcher.CanParseDocument('MyFile.frm'), 'MyFile.frm');
  Check(ModuleDispatcher.CanParseDocument('MyFile.htm'), 'MyFile.htm');
  Check(ModuleDispatcher.CanParseDocument('MyFile.html'), 'MyFile.html');
  Check(ModuleDispatcher.CanParseDocument('MyFile.map'), 'MyFile.map');
  Check(ModuleDispatcher.CanParseDocument('MyFile.pas'), 'MyFile.pas');
  Check(ModuleDispatcher.CanParseDocument('MyFile.xml'), 'MyFile.xml');
  Check(ModuleDispatcher.CanParseDocument('MyFile.xsd'), 'MyFile.xsd');
End;

Procedure TestFunctions.TestDispatcher;

Var
  M : TBaseLanguageModule;

Begin
  M := ModuleDispatcher.Dispatcher('', 'MyFile.css', False, []);
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
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.css',  csBlock),  ctNone, 'MyFile.css - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.css',  csLine),   ctNone, 'MyFile.css - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.css',  csInSitu), ctNone, 'MyFile.css - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.bas',  csBlock),  ctVBLine, 'MyFile.bas - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.bas',  csLine),   ctVBLine, 'MyFile.bas - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.bas',  csInSitu), ctVBLine, 'MyFile.bas - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.bnf',  csBlock),  ctCPPBlock, 'MyFile.bnf - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.bnf',  csLine),   ctCPPBlock, 'MyFile.bnf - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.bnf',  csInSitu), ctCPPBlock, 'MyFile.bnf - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.cls',  csBlock),  ctVBLine, 'MyFile.cls - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.cls',  csLine),   ctVBLine, 'MyFile.cls - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.cls',  csInSitu), ctVBLine, 'MyFile.cls - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dfm',  csBlock),  ctPascalBlock, 'MyFile.dfm - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dfm',  csLine),   ctPascalBlock, 'MyFile.dfm - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dfm',  csInSitu), ctPascalBlock, 'MyFile.dfm - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dpk',  csBlock),  ctPascalBlock, 'MyFile.dpk - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dpk',  csLine),   ctPascalBlock, 'MyFile.dpk - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dpk',  csInSitu), ctPascalBlock, 'MyFile.dpk - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dpr',  csBlock),  ctPascalBlock, 'MyFile.dpr - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dpr',  csLine),   ctPascalBlock, 'MyFile.dpr - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dpr',  csInSitu), ctPascalBlock, 'MyFile.dpr - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dtd',  csBlock),  ctXML, 'MyFile.dtd - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dtd',  csLine),   ctXML, 'MyFile.dtd - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.dtd',  csInSitu), ctXML, 'MyFile.dtd - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.frm',  csBlock),  ctVBLine, 'MyFile.frm - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.frm',  csLine),   ctVBLine, 'MyFile.frm - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.frm',  csInSitu), ctVBLine, 'MyFile.frm - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.htm',  csBlock),  ctXML, 'MyFile.htm - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.htm',  csLine),   ctXML, 'MyFile.htm - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.htm',  csInSitu), ctXML, 'MyFile.htm - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.html', csBlock),  ctXML, 'MyFile.html - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.html', csLine),   ctXML, 'MyFile.html - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.html', csInSitu), ctXML, 'MyFile.html - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.map',  csBlock),  ctCPPBlock, 'MyFile.map - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.map',  csLine),   ctCPPLine, 'MyFile.map - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.map',  csInSitu), ctCPPBlock, 'MyFile.map - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.pas',  csBlock),  ctPascalBlock, 'MyFile.pas - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.pas',  csLine),   ctPascalBlock, 'MyFile.pas - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.pas',  csInSitu), ctPascalBlock, 'MyFile.pas - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.xml',  csBlock),  ctXML, 'MyFile.xml - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.xml',  csLine),   ctXML, 'MyFile.xml - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.xml',  csInSitu), ctXML, 'MyFile.xml - InSitu');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.xsd',  csBlock),  ctXML, 'MyFile.xsd - Block');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.xsd',  csLine),   ctXML, 'MyFile.xsd - Line');
  CheckEquals(ModuleDispatcher.GetCommentType('MyFile.xsd',  csInSitu), ctXML, 'MyFile.xsd - InSitu');
End;


Initialization
  RegisterTest('Module Dispatcher Test', TestFunctions.Suite);
End.