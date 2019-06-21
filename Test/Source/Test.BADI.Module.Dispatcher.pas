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
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.css'), 'MyFile.css');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.bas'), 'MyFile.bas');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.bnf'), 'MyFile.bnf');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.cls'), 'MyFile.cls');
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.dfm'), 'MyFile.dfm');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.dpk'), 'MyFile.dpk');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.dpr'), 'MyFile.dpr');
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.dtd'), 'MyFile.dtd');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.frm'), 'MyFile.frm');
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.htm'), 'MyFile.htm');
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.html'), 'MyFile.html');
  Check(TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.pas'), 'MyFile.pas');
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.xml'), 'MyFile.xml');
  Check(Not TBADIDispatcher.BADIDispatcher.CanDocumentDocument('MyFile.xsd'), 'MyFile.xsd');
End;

Procedure TestFunctions.TestCanParseDocument;

Begin
  Check(Not TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.css'), 'MyFile.css');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.bas'), 'MyFile.bas');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.bnf'), 'MyFile.bnf');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.cls'), 'MyFile.cls');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.dfm'), 'MyFile.dfm');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.dpk'), 'MyFile.dpk');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.dpr'), 'MyFile.dpr');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.dtd'), 'MyFile.dtd');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.frm'), 'MyFile.frm');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.htm'), 'MyFile.htm');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.html'), 'MyFile.html');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.pas'), 'MyFile.pas');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.xml'), 'MyFile.xml');
  Check(TBADIDispatcher.BADIDispatcher.CanParseDocument('MyFile.xsd'), 'MyFile.xsd');
End;

Procedure TestFunctions.TestDispatcher;

Var
  M : TBaseLanguageModule;

Begin
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.css', False, []);
  Try
    Check(M = Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.bas', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.bnf', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.cls', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.dfm', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.dpk', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.dpr', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.dtd', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.frm', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.htm', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.html', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.pas', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.xml', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher('', 'MyFile.xsd', False, []);
  Try
    Check(M <> Nil);
  Finally
    M.Free;
  End;
End;

Procedure TestFunctions.TestGetCommentType;

Begin
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.css',  csBlock),  ctNone, 'MyFile.css - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.css',  csLine),   ctNone, 'MyFile.css - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.css',  csInSitu), ctNone, 'MyFile.css - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.bas',  csBlock),  ctVBLine, 'MyFile.bas - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.bas',  csLine),   ctVBLine, 'MyFile.bas - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.bas',  csInSitu), ctVBLine, 'MyFile.bas - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.bnf',  csBlock),  ctCPPBlock, 'MyFile.bnf - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.bnf',  csLine),   ctCPPBlock, 'MyFile.bnf - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.bnf',  csInSitu), ctCPPBlock, 'MyFile.bnf - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.cls',  csBlock),  ctVBLine, 'MyFile.cls - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.cls',  csLine),   ctVBLine, 'MyFile.cls - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.cls',  csInSitu), ctVBLine, 'MyFile.cls - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dfm',  csBlock),  ctPascalBlock, 'MyFile.dfm - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dfm',  csLine),   ctPascalBlock, 'MyFile.dfm - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dfm',  csInSitu), ctPascalBlock, 'MyFile.dfm - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dpk',  csBlock),  ctPascalBlock, 'MyFile.dpk - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dpk',  csLine),   ctPascalBlock, 'MyFile.dpk - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dpk',  csInSitu), ctPascalBlock, 'MyFile.dpk - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dpr',  csBlock),  ctPascalBlock, 'MyFile.dpr - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dpr',  csLine),   ctPascalBlock, 'MyFile.dpr - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dpr',  csInSitu), ctPascalBlock, 'MyFile.dpr - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dtd',  csBlock),  ctXML, 'MyFile.dtd - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dtd',  csLine),   ctXML, 'MyFile.dtd - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.dtd',  csInSitu), ctXML, 'MyFile.dtd - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.frm',  csBlock),  ctVBLine, 'MyFile.frm - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.frm',  csLine),   ctVBLine, 'MyFile.frm - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.frm',  csInSitu), ctVBLine, 'MyFile.frm - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.htm',  csBlock),  ctXML, 'MyFile.htm - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.htm',  csLine),   ctXML, 'MyFile.htm - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.htm',  csInSitu), ctXML, 'MyFile.htm - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.html', csBlock),  ctXML, 'MyFile.html - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.html', csLine),   ctXML, 'MyFile.html - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.html', csInSitu), ctXML, 'MyFile.html - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.pas',  csBlock),  ctPascalBlock, 'MyFile.pas - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.pas',  csLine),   ctPascalBlock, 'MyFile.pas - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.pas',  csInSitu), ctPascalBlock, 'MyFile.pas - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.xml',  csBlock),  ctXML, 'MyFile.xml - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.xml',  csLine),   ctXML, 'MyFile.xml - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.xml',  csInSitu), ctXML, 'MyFile.xml - InSitu');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.xsd',  csBlock),  ctXML, 'MyFile.xsd - Block');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.xsd',  csLine),   ctXML, 'MyFile.xsd - Line');
  CheckEquals(TBADIDispatcher.BADIDispatcher.GetCommentType('MyFile.xsd',  csInSitu), ctXML, 'MyFile.xsd - InSitu');
End;

Initialization
  RegisterTest('Module Dispatcher Test', TestFunctions.Suite);
End.