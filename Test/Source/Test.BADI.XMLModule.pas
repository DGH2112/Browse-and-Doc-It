Unit Test.BADI.XMLModule;

Interface

Uses
  TestFramework,
  Test.BADI.BaseLanguageModule,
  BADI.XMLModule,
  BADI.BaseLanguageModule;

Type
  //
  // Test Class for the TXMLDecl Class Methods.
  //
  TestTXMLDecl = Class(TExtendedTestCase)
  Strict Private
    FXMLDecl : TXMLDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

  //
  // Test Class for the TXMLDocType Class Methods.
  //
  TestTXMLDocType = Class(TExtendedTestCase)
  Strict Private
    FXMLDocType : TXMLDocType;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

  //
  // Test Class for the TXMLElemDecl Class Methods.
  //
  TestTXMLElemDecl = Class(TExtendedTestCase)
  Strict Private
    FXMLElemDecl : TXMLElemDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

  //
  // Test Class for the TXMLElement Class Methods.
  //
  TestTXMLElement = Class(TExtendedTestCase)
  Strict Private
    FXMLElement : TXMLElement;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreate;
    Procedure TestGetName;
  End;

  //
  // Test Class for the TXMLModule Class Methods.
  //
  TestTXMLModule = Class(TExtendedTestCase)
  Strict Private
    FXMLModule : TXMLModule;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCheckDocumentation;
    Procedure TestCreateParser;
    Procedure TestDocument;
    Procedure TestName;
    Procedure TestProlog;
    Procedure TestElement;
    Procedure TestMisc;
    Procedure TestXMLDecl;
    Procedure TestDocTypeDecl;
    Procedure TestSTag;
    Procedure TestXMLName;
    Procedure TestContent;
    Procedure TestETag;
    Procedure TestXMLComment;
    Procedure TestXMLPI;
    Procedure TestWhitespace;
    Procedure TestVersionInfo;
    Procedure TestEncodingDecl;
    Procedure TestSDDecl;
    Procedure TestExternalID;
    Procedure TestIntSubSet;
    Procedure TestAttribute;
    Procedure TestEq;
    Procedure TestVersionNum;
    Procedure TestEncName;
    Procedure TestPITarget;
    Procedure TestEntityValue;
    Procedure TestNDataDecl;
    Procedure TestSystemLiteral;
    Procedure TestPubIDLiteral;
    Procedure TestMarkupDecl;
    Procedure TestDeclSep;
    Procedure TestElementDecl;
    Procedure TestAttListDecl;
    Procedure TestEntityDecl;
    Procedure TestNotationDecl;
    Procedure TestContentSpec;
    Procedure TestMixed;
    Procedure TestChildren;
    Procedure TestSeq;
    Procedure TestChoice;
    Procedure TestCharData;
    Procedure TestReference;
    Procedure TestCDSect;
    Procedure TestAttValue;
    Procedure TestPEReference;
    Procedure TestAttDef;
    Procedure TestGEDecl;
    Procedure TestPEDecl;
    Procedure TestPublicID;
    Procedure TestEntityRef;
    Procedure TestCharRef;
    Procedure TestCDStart;
    Procedure TestCDData;
    Procedure TestCDEnd;
    Procedure TestAttType;
    Procedure TestDefaultDecl;
    Procedure TestEntityDef;
    Procedure TestStringType;
    Procedure TestTokenizedType;
    Procedure TestEnumerateTyped;
    Procedure TestEnumeration;
    Procedure TestNmToken;
    Procedure TestNameChar;
    Procedure TestNotationType;
    Procedure TestExtSubSet;
    Procedure TestTextDecl;
    Procedure TestExtSubSetDecl;
    Procedure TestConditionalSect;
    Procedure TestIncludeSect;
    Procedure TestIngoreSect;
    Procedure TestExample01;
    Procedure TestExample02;
    Procedure TestExample03;
    Procedure TestExample04;
    Procedure TestExample05;
    Procedure TestExample06;
    Procedure TestExample07;
    Procedure TestExample08;
    Procedure TestExample09;
    Procedure TestExample10;
    Procedure TestExample11;
    Procedure TestExample13;
    Procedure TestExample14;
    Procedure TestExample15;
    Procedure TestExample16;
    Procedure TestExample18;
    Procedure TestExample20;
    Procedure TestExample21;
    Procedure TestExample23;
    Procedure TestExample25;
    Procedure TestExample26;
    Procedure TestExample27;
    Procedure TestExample28;
    Procedure TestExample29;
    Procedure TestExample30;
    Procedure TestExample32;
    Procedure TestExample33;
    Procedure TestExample34;
  Public //: @todo Disabled check until I understand the grammar better.
    Procedure TestExample12;
    Procedure TestExample17;
    Procedure TestExample19;
    Procedure TestExample22;
    Procedure TestExample24;
    Procedure TestExample31;
  End;

Implementation

//
// Test Methods for Class TXMLDecl.
//
Procedure TestTXMLDecl.Setup;
Begin
  FXMLDecl := TXMLDecl.Create('xml', scNone, 12, 23, iiPublicType, Nil);
End;

Procedure TestTXMLDecl.TearDown;

Begin
  FXMLDecl.Free;
End;

Procedure TestTXMLDecl.TestAsString;

Begin
  CheckEquals('xml', FXMLDecl.AsString(True, True));
End;

procedure TestTXMLDecl.TestCreate;
begin
  CheckEquals('xml', FXMLDecl.Identifier);
  CheckEquals(scNone, FXMLDecl.Scope);
  CheckEquals(12, FXMLDecl.Line);
  CheckEquals(23, FXMLDecl.Column);
  CheckEquals(iiPublicType, FXMLDecl.ImageIndex);
  CheckEquals(iiPublicType, FXMLDecl.ImageIndexAdjustedForScope);
end;

//
// Test Methods for Class TXMLDocType.
//
Procedure TestTXMLDocType.Setup;
Begin
  FXMLDocType := TXMLDocType.Create('!DOCTYPE', scNone, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTXMLDocType.TearDown;

Begin
  FXMLDocType.Free;
End;

Procedure TestTXMLDocType.TestAsString;

Begin
  CheckEquals('!DOCTYPE', FXMLDocType.AsString(True, True));
End;

procedure TestTXMLDocType.TestCreate;
begin
  CheckEquals('!DOCTYPE', FXMLDocType.Identifier);
  CheckEquals(scNone, FXMLDocType.Scope);
  CheckEquals(12, FXMLDocType.Line);
  CheckEquals(23, FXMLDocType.Column);
  CheckEquals(iiPublicObject, FXMLDocType.ImageIndex);
  CheckEquals(iiPublicObject, FXMLDocType.ImageIndexAdjustedForScope);
end;

//
// Test Methods for Class TXMLElemDecl.
//
Procedure TestTXMLElemDecl.Setup;
Begin
  FXMLElemDecl := TXMLElemDecl.Create('!ELEMENT', scNone, 12, 23, iiPublicObject, Nil);
  FXMLElemDecl.AddToken('Element');
End;

Procedure TestTXMLElemDecl.TearDown;

Begin
  FXMLElemDecl.Free;
End;

Procedure TestTXMLElemDecl.TestAsString;

Begin
  CheckEquals('!ELEMENT Element', FXMLElemDecl.AsString(True, True));
End;

procedure TestTXMLElemDecl.TestCreate;
begin
  CheckEquals('!ELEMENT', FXMLElemDecl.Identifier);
  CheckEquals(scNone, FXMLElemDecl.Scope);
  CheckEquals(12, FXMLElemDecl.Line);
  CheckEquals(23, FXMLElemDecl.Column);
  CheckEquals(iiPublicObject, FXMLElemDecl.ImageIndex);
  CheckEquals(iiPublicObject, FXMLElemDecl.ImageIndexAdjustedForScope);
end;

//
// Test Methods for Class TXMLElement.
//
Procedure TestTXMLElement.Setup;
Begin
  FXMLElement := TXMLElement.Create('xml', scNone, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTXMLElement.TearDown;

Begin
  FXMLElement.Free;
End;

Procedure TestTXMLElement.TestAsString;

Begin
  CheckEquals('<xml></xml>', FXMLElement.AsString(True, True));
End;

Procedure TestTXMLElement.TestCreate;

Begin
  CheckEquals('xml', FXMLElement.Identifier);
  CheckEquals(scNone, FXMLElement.Scope);
  CheckEquals(12, FXMLElement.Line);
  CheckEquals(23, FXMLElement.Column);
  CheckEquals(iiPublicObject, FXMLElement.ImageIndex);
  CheckEquals(iiPublicObject, FXMLElement.ImageIndexAdjustedForScope);
End;

Procedure TestTXMLElement.TestGetName;

Begin
  CheckEquals('xml:0012:0023', FXMLElement.GetName);
End;

//
// Test Methods for Class TXMLModule.
//
Procedure TestTXMLModule.Setup;

Const
  strSource =
    '<Element>'#13#10 +
    '</Element>'#13#10;
Begin
  FXMLModule := TXMLModule.CreateParser(strSource, 'D:\Path\MyXmlFile.xml', True,
    [moParse, moCheckForDocumentConflicts]);
End;

Procedure TestTXMLModule.TearDown;

Begin
  FXMLModule.Free;
End;

Procedure TestTXMLModule.TestAsString;

Begin
  CheckEquals('MyXmlFile', FXMLModule.AsString(True,True));
End;

procedure TestTXMLModule.TestAttDef;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name AnotherName CDATA #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name AnotherName CDATA #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestAttListDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Hello IDREF #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Hello IDREF #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestAttribute;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element Name="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element Name="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element Name=''Hello''></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element Name=''Hello''></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestAttType;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 CDATA #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 CDATA #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 ID #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 ID #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 NOTATION (Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 NOTATION (Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 NOTATION (Name|Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 NOTATION (Name | Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 (Name|Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name | Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestAttValue;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element Name="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element Name="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestCDData;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[ ]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[Some information!]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestCDEnd;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[ ]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[Some information!]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestCDSect;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[ ]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[Some information!]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestCDStart;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[ ]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[Some information!]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestCharData;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>This is some information, that I think? you &lt;need&gt; to test!</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>This is some information, that I think? you &lt;need&gt; to test!</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestCharRef;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>&#123;</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>&#123;</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>&#x00FF;</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>&#x00FF;</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTXMLModule.TestCheckDocumentation;

var
  boolCascade: Boolean;

Begin
  FXMLModule.CheckDocumentation(boolCascade);
  CheckEquals(1, FXMLModule.HeadingCount(strDocumentationConflicts), FXMLModule.DocConflict(1));
  CheckEquals('1) Module ''MyXmlFile.xml'' is missing a ''?xml'' element.', FXMLModule.DocConflict(1));
End;

procedure TestTXMLModule.TestChildren;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT Name (Name1 | Name2)?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name1 | Name2)?', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (Name1,Name2)*>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name1, Name2)*', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (Name1 , Name2, Name3)+>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name1, Name2, Name3)+', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (Name1 , (Name2 | Name4), Name3)+>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name1, (Name2 | Name4), Name3)+', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name ((Name1|Name5)+, (Name2 | Name4)?, Name3)+>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name ((Name1 | Name5)+, (Name2 | Name4)?, Name3)+', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestChoice;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT Name (Name1 | Name2)>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name1 | Name2)', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestConditionalSect;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<![INCLUDE [<!ELEMENT Name EMPTY>]]>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('INCLUDE []', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('!ELEMENT Name EMPTY', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestContent;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  Text'#13#10 +
    '  <Element/>'#13#10 +
    '  Text'#13#10 +
    '  <Element></Element>'#13#10 +
    '  Text'#13#10 +
    '  &Hello;'#13#10 +
    '  text'#13#10 +
    '  <![CDATA[Oops]]>'#13#10 +
    '  text'#13#10 +
    '  <?Name Goodbye?>'#13#10 +
    '  text'#13#10 +
    '  <!-- Comment -->'#13#10 +
    '  text'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>Text        Text        Text    &Hello;    text        text</Element>', M.Elements[1].AsString(True, True));
    CheckEquals(3, M.Elements[1].ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[1].Elements[2].AsString(True, True));
    CheckEquals('?Name Goodbye?', M.Elements[1].Elements[3].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestContentSpec;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT Name EMPTY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name EMPTY', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name ANY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name ANY', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (#PCDATA)>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (#PCDATA)', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (Name)>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name)', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Procedure TestTXMLModule.TestCreateParser;

Begin
  CheckEquals(11, FXMLModule.TokenCount);
  CheckEquals('D:\Path\MyXmlFile.xml', FXMLModule.FileName);
  CheckEquals(0, FXMLModule.HeadingCount(strErrors), FXMLModule.FirstError);
  CheckEquals(0, FXMLModule.HeadingCount(strWarnings), FXMLModule.FirstWarning);
  CheckEquals(0, FXMLModule.HeadingCount(strHints), FXMLModule.FirstHint);
End;

procedure TestTXMLModule.TestDeclSep;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!DOCTYPE html [%Name;]>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html []', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('%Name;', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE html [ ]>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html []', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestDefaultDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 CDATA #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 CDATA #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 CDATA #IMPLIED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 CDATA #IMPLIED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 CDATA #FIXED "%Name;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 CDATA #FIXED "%Name;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 CDATA "%Name;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 CDATA "%Name;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestDocTypeDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!DOCTYPE html>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE html SYSTEM "123">'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html SYSTEM "123"', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE html SYSTEM "123" [<!ELEMENT Name ANY>]>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html SYSTEM "123" []', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('!ELEMENT Name ANY', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestDocument;

Var
  strCode : String;
  M: TBaseLanguageModule;

begin
  strCode := '<Element>'#13#10'</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<?xml version="1.0"?>'#13#10'<Element>'#13#10'</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<?xml version="1.0"?>'#13#10'<Element>'#13#10'</Element>'#13#10'<!-- Comment -->'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element></Element>'#13#10'<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestElement;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <Hello/>'#13#10 +
    '  <Goodbye>'#13#10 +
    '  </Goodbye>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals(2, M.Elements[1].ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
    CheckEquals('<Hello></Hello>', M.Elements[1].Elements[1].AsString(True, True));
    CheckEquals('<Goodbye></Goodbye>', M.Elements[1].Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestElementDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT name EMPTY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT name EMPTY', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT name ANY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT name ANY', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEncName;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0" encoding="ABC-456-789"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="ABC-456-789"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding="ABC_456_789"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="ABC_456_789"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding="ABC.456.789"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="ABC.456.789"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEncodingDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0" encoding="ABC-456-789"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="ABC-456-789"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding=''ABC-456-789''?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding=''ABC-456-789''?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEntityDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY Name "%Hello;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name "%Hello;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ENTITY % Name "%Hello;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY % Name "%Hello;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEntityDef;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY name SYSTEM "SystemLiteral">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY name SYSTEM "SystemLiteral"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ENTITY name SYSTEM "SystemLiteral" NDATA Name>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY name SYSTEM "SystemLiteral" NDATA Name', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ENTITY name "%Name;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY name "%Name;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEntityRef;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>&Name;</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>&Name;</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEntityValue;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY Name "%Hello;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name "%Hello;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ENTITY Name ''%Hello;''>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name ''%Hello;''', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEnumerateTyped;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 NOTATION (Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 NOTATION (Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 (Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEnumeration;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 (Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 ( Name ) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 (Name|Name2) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name | Name2) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 ( Name | Name2 ) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name | Name2) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestEq;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element Goodbye="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element Goodbye="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestETag;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element></Element >'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample01;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode := '<!-- declaration for <head> & <body> -->'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample02;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <![CDATA[<greeting>Hello, world!</greeting>]]>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample03;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
     '<?xml version="1.0"?>'#13#10 +
     '<greeting>Hello, world!</greeting>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample04;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
     '<greeting>Hello, world!</greeting>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample05;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<!DOCTYPE greeting SYSTEM "hello.dtd">'#13#10 +
    '<greeting>Hello, world!</greeting>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample06;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0" encoding="UTF-8"?>'#13#10 +
    '<!DOCTYPE greeting [<!ELEMENT greeting (#PCDATA)>]>'#13#10 +
    '<greeting>Hello, world!</greeting>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample07;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode := '<?xml version="1.0" standalone=''yes''?>'#13#10 +
  '<Element/>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample08;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST peom xml:space (default|preserve) ''preserve''>'#13#10 +
    '<!ATTLIST pre xml:space (preserve) #FIXED ''preserve''>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample09;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<p xml:lang="en">The quick brown fox jumps over the lazy dog.</p>'#13#10 +
    '<p xml:lang="en-GB">What colour is it?</p>'#13#10 +
    '<p xml:lang="en-US">What color is it?</p>'#13#10 +
    '<sp who="Faust" desc=''leise'' xml:lang="de">'#13#10 +
    '  <l>Habe nun, ach! Philosophie,</l>'#13#10 +
    '  <l>Juristerei, und Medizin</l>'#13#10 +
    '  <l>und leider auch Theologie</l>'#13#10 +
    '  <l>durchaus studiert mit heißem Bemüh`n.</l>'#13#10 +
    '</sp>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(4, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample10;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST poem xml:lang CDATA ''fr''>'#13#10 +
    '<!ATTLIST gloss xml:lang CDATA ''en''>'#13#10 +
    '<!ATTLIST note xml:lang CDATA ''en''>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample11;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<IMG align="left"'#13#10 +
    'src="http://www.w3.org/Icons/WWW/w3c_home" />'#13#10 +
    '<br></br>'#13#10 +
    '<br/>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample12;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT br EMPTY>'#13#10 +
    '<!ELEMENT p (#PCDATA|emph)* >'#13#10 +
    '<!ELEMENT %name.para; %content.para; >'#13#10 +
    '<!ELEMENT container ANY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(4, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample13;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT spec (front, body, back?)>'#13#10 +
    '<!ELEMENT div1 (head, (p | list | note)*, div2*)>'#13#10 +
    '<!ELEMENT dictionary-body (%div.mix; | %dict.mix;)*>'#13#10
    ;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample14;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT p (#PCDATA|a|ul|b|i|em)*>'#13#10 +
    '<!ELEMENT p (#PCDATA | %font; | %phrase; | %special; | %form;)* >'#13#10 +
    '<!ELEMENT b (#PCDATA)>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample15;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST termdef id ID #REQUIRED name CDATA #IMPLIED>'#13#10 +
    '<!ATTLIST list type (bullets|ordered|glossary) "ordered">'#13#10 +
    '<!ATTLIST form method CDATA #FIXED "POST">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample16;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY d "&#xD;">'#13#10 +
    '<!ENTITY a "&#xA;">'#13#10 +
    '<!ENTITY da "&#xD;&#xA;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample17;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY % draft ''INCLUDE'' >'#13#10 +
    '<!ENTITY % final ''IGNORE'' >'#13#10 +
    '<![%draft;['#13#10 +
    '<!ELEMENT book (comments*, title, body, supplements?)>'#13#10 +
    ']]>'#13#10 +
    '<![%final;['#13#10 +
    '<!ELEMENT book (title, body, supplements?)>'#13#10 +
    ']]>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample18;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Text>Type <key>less-than</key> (&#x3C;) to save options.'#13#10 +
    'This document was prepared on &docdate; and'#13#10 +
    'is classified &security-level;.</Text>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample19;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!-- declare the parameter entity "ISOLat2"... -->'#13#10 +
    '<!ENTITY % ISOLat2'#13#10 +
    'SYSTEM "http://www.xml.com/iso/isolat2-xml.entities" >'#13#10 +
    '<!-- ... now reference it. -->'#13#10 +
    '%ISOLat2;'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample20;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY Pub-Status "This is a pre-release of the'#13#10 +
    'specification.">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample21;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY open-hatch'#13#10 +
    'SYSTEM "http://www.textuality.com/boilerplate/OpenHatch.xml">'#13#10 +
    '<!ENTITY open-hatch'#13#10 +
    'PUBLIC "-//Textuality//TEXT Standard open-hatch boilerplate//EN"'#13#10 +
    '"http://www.textuality.com/boilerplate/OpenHatch.xml">'#13#10 +
    '<!ENTITY hatch-pic'#13#10 +
    'SYSTEM "../grafix/OpenHatch.gif"'#13#10 +
    'NDATA gif >'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample22;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml encoding=''UTF-8''?>'#13#10 +
    '<?xml encoding=''EUC-JP''?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample23;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY % YN ''"Yes"'' >'#13#10 +
    '<!ENTITY WhatHeSaid "He said %YN;" >'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample24;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY EndAttr "27''" >'#13#10 +
    '<element attribute=''a-&EndAttr;>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample25;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY % pub "&#xc9;ditions Gallimard" >'#13#10 +
    '<!ENTITY rights "All rights reserved" >'#13#10 +
    '<!ENTITY book "La Peste: Albert Camus,'#13#10 +
    '&#xA9; 1947 %pub;. &rights;" >'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample26;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Text>La Peste: Albert Camus,'#13#10 +
    '© 1947 Éditions Gallimard. &rights;</Text>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample27;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY lt "&#38;#60;">'#13#10 +
    '<!ENTITY gt "&#62;">'#13#10 +
    '<!ENTITY amp "&#38;#38;">'#13#10 +
    '<!ENTITY apos "&#39;">'#13#10 +
    '<!ENTITY quot "&#34;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(5, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample28;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY example "<p>An ampersand (&#38;#38;) may be escaped'#13#10 +
    'numerically (&#38;#38;#38;) or with a general entity'#13#10 +
    '(&amp;amp;).</p>" >'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample29;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<p>An ampersand (&#38;) may be escaped'#13#10 +
    'numerically (&#38;#38;) or with a general entity'#13#10 +
    '(&amp;amp;).</p>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample30;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Text>An ampersand (&) may be escaped'#13#10 +
    'numerically (&#38;) or with a general entity'#13#10 +
    '(&amp;).</Text>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample31;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version=''1.0''?>'#13#10 +
    '<!DOCTYPE test [<!ELEMENT test (#PCDATA) >'#13#10 +
    '<!ENTITY % xx ''&#37;zz;''>'#13#10 +
    '<!ENTITY % zz ''&#60;!ENTITY tricky "error-prone" >'' >'#13#10 +
    '%xx;'#13#10 +
    ']>'#13#10 +
    '<test>This sample shows a &tricky; method.</test>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(6, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample32;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!DOCTYPE foo [<!ENTITY x "&lt;">]>'#13#10 +
    '<foo attr="&x;"/>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample33;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY x "&#60;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExample34;

Var
  strCode: String;
  M: TBaseLanguageModule;

Begin
  strCode :=
    '<Element>This don''t have to fail!</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>This don''t have to fail!</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
End;

Procedure TestTXMLModule.TestExternalID;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!DOCTYPE name SYSTEM "Hello">'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE name SYSTEM "Hello"', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE name SYSTEM ''Hello''>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE name SYSTEM ''Hello''', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE name PUBLIC "PublicID" "Hello">'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE name PUBLIC "PublicID" "Hello"', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE name PUBLIC ''PublicID'' ''Hello''>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE name PUBLIC ''PublicID'' ''Hello''', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExtSubSet;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  //: @bug Trim whitespace
  strCode :=
    '<?xml encoding="UFT-8"?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('?xml encoding="UFT-8"?', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestExtSubSetDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml encoding="UFT-8"?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('?xml encoding="UFT-8"?', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name EMPTY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name EMPTY', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestGEDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY Name SYSTEM "PublicLiteral">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name SYSTEM "PublicLiteral"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestIncludeSect;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<![INCLUDE [<!ELEMENT Name EMPTY>]]>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('INCLUDE []', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('!ELEMENT Name EMPTY', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestIngoreSect;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<![IGNORE [Some Characters to ignore.<![Ignore even more.]]> Some more characters to ignore]]>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('IGNORE []', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestIntSubSet;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!DOCTYPE html [<!ELEMENT Name ANY>]>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html []', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('!ELEMENT Name ANY', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!DOCTYPE html [%Name;]>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('!DOCTYPE html []', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('%Name;', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestMarkupDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT Name ANY>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name ANY', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Hello CDATA #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Hello CDATA #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ENTITY Name "%Value;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name "%Value;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!NOTATION Name SYSTEM "Hello">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!NOTATION Name SYSTEM "Hello"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?Name?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('?Name?', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!-- Comment -->'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(0, M.ElementCount);
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestMisc;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<Element></Element>'#13#10 +
    '<!-- Comment -->'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<?Name Hello?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('?Name Hello?', M.Elements[2].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[3].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestMixed;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT Name (#PCDATA)>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (#PCDATA)', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name ( #PCDATA )>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (#PCDATA)', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (#PCDATA | Name1)*>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (#PCDATA | Name1)*', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ELEMENT Name (#PCDATA|Name1|Name2)*>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (#PCDATA | Name1 | Name2)*', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestName;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode := '<Element>'#13#10'</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01>'#13#10'</Element01>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01></Element01>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01.Oops>'#13#10'</Element01.Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01.Oops></Element01.Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01-Oops>'#13#10'</Element01-Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01-Oops></Element01-Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01_Oops>'#13#10'</Element01_Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01_Oops></Element01_Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01:Oops>'#13#10'</Element01:Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01:Oops></Element01:Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestNameChar;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode := '<Element>'#13#10'</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01>'#13#10'</Element01>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01></Element01>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01.Oops>'#13#10'</Element01.Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01.Oops></Element01.Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01-Oops>'#13#10'</Element01-Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01-Oops></Element01-Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01_Oops>'#13#10'</Element01_Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01_Oops></Element01_Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode := '<Element01:Oops>'#13#10'</Element01:Oops>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element01:Oops></Element01:Oops>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestNDataDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY Name SYSTEM "Oops" NDATA AnotherName>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name SYSTEM "Oops" NDATA AnotherName', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestNmToken;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 (Name | Name2 | Name3 | Name4) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 (Name | Name2 | Name3 | Name4) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestNotationDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!NOTATION Name SYSTEM "ExternalID">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!NOTATION Name SYSTEM "ExternalID"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!NOTATION Name PUBLIC "PublicLiteral" "Hello">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!NOTATION Name PUBLIC "PublicLiteral" "Hello"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestNotationType;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode := '<!ATTLIST Name Name2 NOTATION (Name) #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 NOTATION (Name) #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestPEDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY % Name "%Name;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY % Name "%Name;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestPEReference;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY MyName "%Hello;">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY MyName "%Hello;"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestPITarget;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>'#13#10 +
    '  <?Hello?>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('?Hello?', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>'#13#10 +
    '  <?Hello Goodbye Dave?>'#13#10 +
    '</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('?Hello Goodbye Dave?', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestProlog;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0"?>'#13#10'<!-- Comment -->'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0"?>'#13#10'<!DOCTYPE html>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(3, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('!DOCTYPE html', M.Elements[2].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[3].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestPubIDLiteral;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!NOTATION Name PUBLIC "PublicChars" "Hello">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!NOTATION Name PUBLIC "PublicChars" "Hello"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!NOTATION Name PUBLIC ''PublicChars'' ''Hello''>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!NOTATION Name PUBLIC ''PublicChars'' ''Hello''', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestPublicID;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!NOTATION Name PUBLIC "PublicChars" "SystemLiteral">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!NOTATION Name PUBLIC "PublicChars" "SystemLiteral"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestReference;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element>%name;</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>%name;</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>&#123;</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>&#123;</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element>&#x00FF;</Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element>&#x00FF;</Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestSDDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0" standalone="yes"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" standalone="yes"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" standalone="no"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" standalone="no"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" standalone=''yes''?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" standalone=''yes''?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" standalone=''no''?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" standalone=''no''?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestSeq;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ELEMENT Name (Name1,Name2)>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ELEMENT Name (Name1, Name2)', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestSTag;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element ></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element Name="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element Name="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element Name="Hello" ></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element Name="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestStringType;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 CDATA #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 CDATA #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestSystemLiteral;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ENTITY Name SYSTEM "Oops">'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name SYSTEM "Oops"', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ENTITY Name SYSTEM ''Oops''>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ENTITY Name SYSTEM ''Oops''', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestTextDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml encoding="UFT-8"?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('?xml encoding="UFT-8"?', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding="UFT-8"?>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="UFT-8"?', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestTokenizedType;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!ATTLIST Name Name2 ID #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 ID #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 IDREF #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 IDREF #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 IDREFS #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 IDREFS #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 ENTITY #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 ENTITY #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 NMTOKEN #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 NMTOKEN #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!ATTLIST Name Name2 NMTOKENS #REQUIRED>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.dtd', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('!ATTLIST Name Name2 NMTOKENS #REQUIRED', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestVersionInfo;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version=''1.0''?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version=''1.0''?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestVersionNum;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version=''1.0''?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version=''1.0''?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestWhitespace;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    ' <Element > <!-- --> </Element > '#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestXMLComment;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<!-- Comment -->'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!-- Comment --><A></A>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('<A></A>', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!-- Comment --><A><!-- Comment --></A>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('<A></A>', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<!-- Comment --><A><!-- Comment --></A><!-- Comment -->'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('<A></A>', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestXMLDecl;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<?xml version="1.0"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding="ABC-1234-567-89"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="ABC-1234-567-89"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding="A1234_56789" standalone="yes"?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="A1234_56789" standalone="yes"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<?xml version="1.0" encoding="A12-3456789" standalone="no" ?>'#13#10 +
    '<Element></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(2, M.ElementCount);
    CheckEquals('?xml version="1.0" encoding="A12-3456789" standalone="no"?', M.Elements[1].AsString(True, True));
    CheckEquals('<Element></Element>', M.Elements[2].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestXMLName;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element name="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element name="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element name_="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element name_="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element name_info="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element name_info="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
  strCode :=
    '<Element name:info="Hello"></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element name:info="Hello"></Element>', M.Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

procedure TestTXMLModule.TestXMLPI;

Var
 strCode : String;
 M : TBaseLanguageModule;

begin
  strCode :=
    '<Element><?Name Hello?></Element>'#13#10;
  M := TXMLModule.CreateParser(strCode, 'D:\Path\Filename.xml', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(1, M.ElementCount);
    CheckEquals('<Element></Element>', M.Elements[1].AsString(True, True));
    CheckEquals(1, M.Elements[1].ElementCount);
    CheckEquals('?Name Hello?', M.Elements[1].Elements[1].AsString(True, True));
  Finally
    M.Free;
  End;
end;

Initialization
  RegisterTest('XML Module', TestTXMLDecl.Suite);
  RegisterTest('XML Module', TestTXMLDocType.Suite);
  RegisterTest('XML Module', TestTXMLElemDecl.Suite);
  RegisterTest('XML Module', TestTXMLElement.Suite);
  RegisterTest('XML Module', TestTXMLModule.Suite);
End.
