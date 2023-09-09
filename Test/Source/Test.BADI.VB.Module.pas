(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.243
  @Date    09 Sep 2023

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
  
**)
unit Test.BADI.VB.Module;

interface

uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.VB.Module;

type
  TestTVBModule = Class(TExtendedTestCase)
  Strict Private
    FVBModule: TVBModule;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestReservedWords;
    Procedure TestDirectives;
    Procedure TestVersion;
    Procedure TestAttributes;
    Procedure TestFunctions;
    Procedure TestOption;
    Procedure TestImplements;
    Procedure TestConsts;
    Procedure TestVars;
    Procedure TestSubs;
    Procedure TestDeclares;
    Procedure TestProperties;
    Procedure TestRecords;
    Procedure TestEnums;
    Procedure TestGetComment;
    Procedure TestCheckDocumentation;
    Procedure TestCombinations;
    Procedure TestDisablers;
    Procedure TestFailure01;
    Procedure TestFailure02;
    Procedure TestFailure03;
    Procedure TestFailure04;
    Procedure TestFailure05;
    Procedure TestFailure06;
    Procedure TestFailure07;
    Procedure TestFailure08;
    procedure TestFailure09;
    procedure TestFailure10;
    Procedure TestFailure11;
    Procedure TestFailure12;
    Procedure TestFailure13;
    Procedure Testfailure14;
    Procedure TestFailure15;
  End;

implementation

Uses
  BADI.Types,
  BADI.Base.Module,
  BADI.ResourceStrings,
  BADI.VB.ResourceStrings,
  SysUtils,
  BADI.ElementContainer,
  BADI.Comment,
  BADI.VB.MethodDecl,
  BADI.Options;

{ TestTVBModule }

procedure TestTVBModule.TestDirectives;

Var
  Words : TKeyWords;
  i : Integer;

begin
  Words :=  FVBModule.Directives;
  For i := Low(Words) To Pred(High(Words)) Do
    Check(Words[i] < Words[i + 1], Words[i] + '!<' + Words[i + 1]);
end;

procedure TestTVBModule.SetUp;
begin
  FVBModule := TVBModule.CreateParser('', 'VBFile.cls', False, [moParse]);
end;

procedure TestTVBModule.TearDown;
begin
  FVBModule.Free;
end;

procedure TestTVBModule.TestAttributes;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Attribute VB_Name = "frmWireRunWizard"'#13#10 +
    'Attribute VB_GlobalNameSpace = False'#13#10 +
    'Attribute VB_Creatable = False'#13#10 +
    'Attribute VB_PredeclaredId = True'#13#10 +
    'Attribute VB_Exposed = False'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(5, M.HeadingCount(strAttributesLabel));
    CheckEquals(scNone, M.FindElement(strAttributesLabel).Elements[1].Scope);
    CheckEquals(scNone, M.FindElement(strAttributesLabel).Elements[2].Scope);
    CheckEquals(scNone, M.FindElement(strAttributesLabel).Elements[3].Scope);
    CheckEquals(scNone, M.FindElement(strAttributesLabel).Elements[4].Scope);
    CheckEquals(scNone, M.FindElement(strAttributesLabel).Elements[5].Scope);
    // Remember these are now sorted...
    CheckEquals('VB_Creatable = False', M.FindElement(strAttributesLabel).Elements[1].AsString(True, False));
    CheckEquals('VB_Exposed = False', M.FindElement(strAttributesLabel).Elements[2].AsString(True, False));
    CheckEquals('VB_GlobalNameSpace = False', M.FindElement(strAttributesLabel).Elements[3].AsString(True, False));
    CheckEquals('VB_Name = "frmWireRunWizard"', M.FindElement(strAttributesLabel).Elements[4].AsString(True, False));
    CheckEquals('VB_PredeclaredId = True', M.FindElement(strAttributesLabel).Elements[5].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestCheckDocumentation;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    ''''#13#10 +
    ''' This is a module comment.'#13#10 +
    ''''#13#10 +
    ''' @author David Hoyle'#13#10 +
    ''' @version 1.0'#13#10 +
    ''' @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    Check(M.Comment = Nil, 'Module Comment is NOT NIL!');
  Finally
    M.Free;
  End;
  strCode :=
    ''':'#13#10 +
    ''' This is a module comment.'#13#10 +
    ''''#13#10 +
    ''' @author David Hoyle'#13#10 +
    ''' @version 1.0'#13#10 +
    ''' @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'Option explicit'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module Comment is NOT NIL!');
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals('1) This module has no document comment.', M.DocConflict(1));
    CheckEquals('', M.Comment.AsString(9999, True));
  Finally
    M.Free;
  End;
  strCode :=
    ''':'#13#10 +
    ''': This is a module comment.'#13#10 +
    ''''#13#10 +
    ''' @author David Hoyle'#13#10 +
    ''' @version 1.0'#13#10 +
    ''' @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'version 1.0'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module Comment is NOT NIL!');
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals('This is a module comment.', M.Comment.AsString(9999, True));
    CheckEquals('1) This module is missing a documentation date (''' + FormatDateTime('dd mmm yyyy', Now) + ''').', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''':'#13#10 +
    ''': This is a module comment.'#13#10 +
    ''':'#13#10 +
    ''' @author David Hoyle'#13#10 +
    ''' @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'attribute iMyAttr = 1'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module Comment is NOT NIL!');
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals('This is a module comment.', M.Comment.AsString(9999, True));
    CheckEquals('1) This module is missing a documentation version.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''':'#13#10 +
    ''': This is a module comment.'#13#10 +
    ''':'#13#10 +
    ''' @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module Comment is NOT NIL!');
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals('This is a module comment.', M.Comment.AsString(9999, True));
    CheckEquals('1) This module is missing a documentation author.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''':'#13#10 +
    ''': This is a module comment.'#13#10 +
    ''':'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module Comment is NOT NIL!');
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    'private const iCAPACITY AS Long = 1'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Constant ''iCAPACITY'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''' This is a standard comment.'#13#10 +
    'private const iCAPACITY AS Long = 1'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Constant ''iCAPACITY'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''': This is a document comment.'#13#10 +
    'private const iCAPACITY AS Long = 1'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    'private iCAPACITY AS Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Variable ''iCAPACITY'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''' This is a standard comment.'#13#10 +
    'private iCAPACITY AS Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Variable ''iCAPACITY'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''': This is a document comment.'#13#10 +
    'private iCAPACITY AS Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    'private type TMyType'#13#10 +
    'end Type'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Type ''TMyType'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''' This is a standard comment.'#13#10 +
    'private type TMyType'#13#10 +
    'end Type'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Type ''TMyType'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''': This is a document comment.'#13#10 +
    'private type TMyType'#13#10 +
    'end Type'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    'private enum TMyType'#13#10 +
    'end enum'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Type ''TMyType'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''' This is a standard comment.'#13#10 +
    'private enum TMyType'#13#10 +
    'end enum'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Type ''TMyType'' is undocumented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''': This is a document comment.'#13#10 +
    'private enum TMyType'#13#10 +
    'end enum'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    'function MyFunction(iParam As Long) As Boolean'#13#10 +
    'end function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Method ''VBFile.MyFunction'' has not been documented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''' This is a standard comment.'#13#10 +
    ''' @precon  None.'#13#10 +
    ''' @Postcon None.'#13#10 +
    ''' @param  iParam as a Long'#13#10 +
    ''' @return a Boolean'#13#10 +
    'function MyFunction(iParam As Long) As Boolean'#13#10 +
    'end function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Method ''VBFile.MyFunction'' has not been documented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''': This is a standard comment.'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''': @param  iParam as a Long'#13#10 +
    ''': @return a Boolean'#13#10 +
    'function MyFunction(iParam As Long) As Boolean'#13#10 +
    'end function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    'property get MyProperty(iParam As Long) As Boolean'#13#10 +
    'end property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Property ''MyProperty'' has not been documented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''' This is a standard comment.'#13#10 +
    ''' @precon  None.'#13#10 +
    ''' @Postcon None.'#13#10 +
    ''' @param  iParam as a Long'#13#10 +
    ''' @return a Boolean'#13#10 +
    'property get MyProperty(iParam As Long) As Boolean'#13#10 +
    'end property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(1, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals('1) Property ''MyProperty'' has not been documented.', M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment.'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'option compare text'#13#10 +
    ''': This is a standard comment.'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''': @param  iParam as a Long'#13#10 +
    ''': @return a Boolean'#13#10 +
    'property get MyProperty(iParam As Long) As Boolean'#13#10 +
    'end property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestCombinations;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'VERSION 5.00'#13#10 +
    'Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmWireRunWizard'#13#10 +
    '   Caption         =   "Wire Run Wizard"'#13#10 +
    '   ClientHeight    =   4620'#13#10 +
    '   ClientLeft      =   45'#13#10 +
    '   ClientTop       =   435'#13#10 +
    '   ClientWidth     =   6180'#13#10 +
    '   OleObjectBlob   =   "frmWireRunWizard.frx":0000'#13#10 +
    '   StartUpPosition =   1  ''CenterOwner'#13#10 +
    'End'#13#10 +
    'Attribute VB_Name = "frmWireRunWizard"'#13#10 +
    'Attribute VB_GlobalNameSpace = False'#13#10 +
    'Attribute VB_Creatable = False'#13#10 +
    'Attribute VB_PredeclaredId = True'#13#10 +
    'Attribute VB_Exposed = False'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': Hello'#13#10 +
    ''': @Something Else'#13#10 +
    ''':'#13#10 +
    'Option Explicit'#13#10 +
    'Option Compare Text'#13#10 +
    'Option Private Module'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.frm', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);

    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(5, M.HeadingCount(strAttributesLabel));
    CheckEquals(3, M.HeadingCount(strOptionsLabel));

    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
    CheckEquals(scNone, M.FindElement(strAttributesLabel).Elements[3].Scope);
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[2].Scope);

    CheckEquals('VB_GlobalNameSpace = False', M.FindElement(strAttributesLabel).Elements[3].AsString(True, False));
    CheckEquals('VERSION 5.00', M.FindElement(strVersionLabel).Elements[1].AsString(True, False));
    CheckEquals('ClientTop = 435', M.FindElement(strVersionLabel).Elements[1].Elements[1].Elements[4].AsString(True, False));
    CheckEquals('Explicit', M.FindElement(strOptionsLabel).Elements[2].AsString(True, False));
    Check(M.Comment <> Nil, 'Module Comment is NULL!');
  Finally
    M.Free;
  End;
  strCode :=
    ''#13#10 +
    ''''''#13#10 +
    ''''' Hello'#13#10 +
    ''''' @Something Else'#13#10 +
    ''''''#13#10 +
    'Option Explicit'#13#10 +
    'Option Compare Text'#13#10 +
    'Option Private Module'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.frm', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(0, M.HeadingCount(strVersionLabel));
    CheckEquals(0, M.HeadingCount(strAttributesLabel));
    CheckEquals(3, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[2].Scope);
    CheckEquals('Explicit', M.FindElement(strOptionsLabel).Elements[2].AsString(True, False));
    Check(M.Comment <> Nil, 'Module Comment is NULL!');
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestConsts;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Const iLong As Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Const iLong As Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPrivate, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Const iLong As Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Const strText As String = "Hello"'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('strText As String = "Hello"', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Const iLong As Long = 123'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPrivate, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long = 123', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Const iLong As Long = &HFF00'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long = &HFF00', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Const iLong1 As Long = &HFF00'#13#10 +
    'Private Const iLong2 As Long = 123'#13#10 +
    'Public Const iLong3 As Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strConstantsLabel).Elements[2].Scope);
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[3].Scope);
    CheckEquals('iLong1 As Long = &HFF00', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
    CheckEquals('iLong2 As Long = 123', M.FindElement(strConstantsLabel).Elements[2].AsString(True, False));
    CheckEquals('iLong3 As Long', M.FindElement(strConstantsLabel).Elements[3].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestDeclares;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Declare Sub Test Lib "Kernal32" ()'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Sub Test Lib "Kernal32" ()', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Sub Test Lib "Kernal32" Alias "TestA" ()'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Sub Test Lib "Kernal32" Alias "TestA" ()', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Sub Test Lib "Kernal32" Alias "TestA" (i As Long)'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Sub Test Lib "Kernal32" Alias "TestA" (i As Long)', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Sub Test1 Lib "Kernal32" Alias "TestA" ()'#13#10 +
    'Private Declare Sub Test2 Lib "Kernal32" Alias "TestA" (i As Long)'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(2, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strDeclaresLabel).Elements[2].Scope);
    CheckEquals('Sub Test1 Lib "Kernal32" Alias "TestA" ()', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
    CheckEquals('Sub Test2 Lib "Kernal32" Alias "TestA" (i As Long)', M.FindElement(strDeclaresLabel).Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Function Test Lib "Kernal32" () As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Function Test Lib "Kernal32" () As String', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Function Test Lib "Kernal32" Alias "TestA" () As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Function Test Lib "Kernal32" Alias "TestA" () As String', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Function Test Lib "Kernal32" Alias "TestA" (i As Long) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Function Test Lib "Kernal32" Alias "TestA" (i As Long) As String', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Declare Function Test1 Lib "Kernal32" Alias "TestA" () As Long'#13#10 +
    'Private Declare Function Test2 Lib "Kernal32" Alias "TestA" (i As Long) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(2, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strDeclaresLabel).Elements[2].Scope);
    CheckEquals('Function Test1 Lib "Kernal32" Alias "TestA" () As Long', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
    CheckEquals('Function Test2 Lib "Kernal32" Alias "TestA" (i As Long) As String', M.FindElement(strDeclaresLabel).Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestDisablers;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'option compare text'#13#10 +
    ''': @noerror'#13#10 +
    ''': @noexception'#13#10 +
    'sub mysub(iParam as Long)'#13#10 +
    'end sub'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    ''': @noerror'#13#10 +
    ''': @noexception'#13#10 +
    'option compare text'#13#10 +
    'sub mysub(iParam as Long)'#13#10 +
    'end sub'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestEnums;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Enum THello'#13#10 +
    '  FID = 1'#13#10 +
    '  FName = 2'#13#10 +
    'End Enum'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strTypesLabel));
    CheckEquals(scPublic, M.FindElement(strTypesLabel).Elements[1].Scope);
    CheckEquals('Enum THello', M.FindElement(strTypesLabel).Elements[1].AsString(True, False));
    CheckEquals('FID = 1', M.FindElement(strTypesLabel).Elements[1].Elements[1].AsString(True, False));
    CheckEquals('FName = 2', M.FindElement(strTypesLabel).Elements[1].Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Enum THello'#13#10 +
    '  FID'#13#10 +
    '  FName'#13#10 +
    'End Enum'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strTypesLabel));
    CheckEquals(scPrivate, M.FindElement(strTypesLabel).Elements[1].Scope);
    CheckEquals('Enum THello', M.FindElement(strTypesLabel).Elements[1].AsString(True, False));
    CheckEquals('FID', M.FindElement(strTypesLabel).Elements[1].Elements[1].AsString(True, False));
    CheckEquals('FName', M.FindElement(strTypesLabel).Elements[1].Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure01;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    'Type RECT'#13#10 +
    '  ''Specifies the x-coordinate of the upper-left corner of the rectangle.'#13#10 +
    '  Left As Long'#13#10 +
    '  ''Specifies the y-coordinate of the upper-left corner of the rectangle.'#13#10 +
    '  Top As Long'#13#10 +
    '  ''Specifies the x-coordinate of the lower-right corner of the rectangle.'#13#10 +
    '  Right As Long'#13#10 +
    '  ''Specifies the y-coordinate of the lower-right corner of the rectangle.'#13#10 +
    '  Bottom As Long'#13#10 +
    'End Type'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strTypesLabel));
    CheckEquals(scPublic, M.FindElement(strTypesLabel).Elements[1].Scope);
    CheckEquals('Type RECT', M.FindElement(strTypesLabel).Elements[1].AsString(True, False));
    CheckEquals('Left As Long', M.FindElement(strTypesLabel).Elements[1].Elements[2].AsString(True, False));
    CheckEquals('Top As Long', M.FindElement(strTypesLabel).Elements[1].Elements[4].AsString(True, False));
    CheckEquals('Right As Long', M.FindElement(strTypesLabel).Elements[1].Elements[3].AsString(True, False));
    CheckEquals('Bottom As Long', M.FindElement(strTypesLabel).Elements[1].Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure02;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    'VERSION 5.00'#13#10 +
    'Begin VB.Form Form1'#13#10 +
    '   Caption         =   "Form1"'#13#10 +
    '   ClientHeight    =   5670'#13#10 +
    '   ClientLeft      =   60'#13#10 +
    '   ClientTop       =   345'#13#10 +
    '   ClientWidth     =   6885'#13#10 +
    '   LinkTopic       =   "Form1"'#13#10 +
    '   ScaleHeight     =   5670'#13#10 +
    '   ScaleWidth      =   6885'#13#10 +
    '   StartUpPosition =   3  ''Windows Default'#13#10 +
    '   Begin VB.CommandButton Command2'#13#10 +
    '      Caption         =   "Create File"'#13#10 +
    '      Height          =   495'#13#10 +
    '      Left            =   3120'#13#10 +
    '      TabIndex        =   2'#13#10 +
    '      Top             =   240'#13#10 +
    '      Width           =   3375'#13#10 +
    '   End'#13#10 +
    '   Begin VB.TextBox Text1'#13#10 +
    '      Height          =   4695'#13#10 +
    '      Left            =   240'#13#10 +
    '      MultiLine       =   -1  ''True'#13#10 +
    '      ScrollBars      =   3  ''Both'#13#10 +
    '      TabIndex        =   1'#13#10 +
    '      Top             =   840'#13#10 +
    '      Width           =   6375'#13#10 +
    '   End'#13#10 +
    '   Begin VB.CommandButton Command1'#13#10 +
    '      Caption         =   "LoadFile"'#13#10 +
    '      Height          =   495'#13#10 +
    '      Left            =   240'#13#10 +
    '      TabIndex        =   0'#13#10 +
    '      Top             =   240'#13#10 +
    '      Width           =   2535'#13#10 +
    '   End'#13#10 +
    'End'#13#10 +
    'Attribute VB_Name = "Form1"'#13#10 +
    'Attribute VB_GlobalNameSpace = False'#13#10 +
    'Attribute VB_Creatable = False'#13#10 +
    'Attribute VB_PredeclaredId = True'#13#10 +
    'Attribute VB_Exposed = False'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure03;

Var
  M : TBaseLanguageModule;
  strCode : String;
  I: TElementContainer;
  F: TElementContainer;
  C: TComment;
  boolCascade: Boolean;

Begin
  strCode :=
    ''': Comment.'#13#10 +
    ''': @Author DGH'#13#10 +
    ''': @Date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''': @version 1'#13#10 +
    'VERSION 5.00'#13#10 +
    'Option Compare Text'#13#10 +
    'Option Explicit'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    I := M.FindElement(strImplementedMethodsLabel);
    Check(I <> Nil, 'Implemented Methods is NULL!');
    F := I.FindElement('MyMethod', ftIdentifier);
    Check(F <> Nil, 'MyMethod is NULL!');
    C := F.Comment;
    Check(C <> Nil, 'MyMethod.Comment is NULL!');
    If F is TVBMethod Then
      (F As TVBMethod).CheckDocumentation(boolCascade);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
  Finally
    M.Free;
  End;
  strCode :=
    ''': Comment.'#13#10 +
    ''': @Author DGH'#13#10 +
    ''': @Date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''': @version 1'#13#10 +
    'VERSION 5.00'#13#10 +
    'Option Compare Text'#13#10 +
    'Option Explicit'#13#10 +
    ''#13#10 +
    ''''''#13#10 +
    ''''' This is a method comment.'#13#10 +
    ''''''#13#10 +
    ''''' @precon  None.'#13#10 +
    ''''' @postcon None.'#13#10 +
    ''''''#13#10 +
    ''''' @param  iParam as a Long as a Reference'#13#10 +
    ''''' @return a Boolean'#13#10 +
    ''''''#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    I := M.FindElement(strImplementedMethodsLabel);
    Check(I <> Nil, 'Implemented Methods is NULL!');
    F := I.FindElement('MyMethod', ftIdentifier);
    Check(F <> Nil, 'MyMethod is NULL!');
    C := F.Comment;
    Check(C <> Nil, 'MyMethod.Comment is NULL!');
    If F is TVBMethod Then
      (F As TVBMethod).CheckDocumentation(boolCascade);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure04;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment = Nil, 'Module comment is NOT Nil!');
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 5.00'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment = Nil, 'Module comment is NOT Nil!');
  Finally
    M.Free;
  End;
  strCode :=
    'Option Compare Text'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment = Nil, 'Module comment is NOT Nil!');
  Finally
    M.Free;
  End;
  strCode :=
    'Attribute MyAttr = 1'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment = Nil, 'Module comment is NOT Nil!');
  Finally
    M.Free;
  End;
  strCode :=
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment = Nil, 'Module comment is NOT Nil!');
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment'#13#10 +
    ''': @date    ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''': @version 1.0'#13#10 +
    ''': @author  David Hoyle'#13#10 +
    'VERSION 5.00'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment is NIL!');
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment'#13#10 +
    ''': @date    ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''': @version 1.0'#13#10 +
    ''': @author  David Hoyle'#13#10 +
    'Option Compare Text'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment is NIL!');
  Finally
    M.Free;
  End;
  strCode :=
    ''': This is a module comment'#13#10 +
    ''': @date    ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''': @version 1.0'#13#10 +
    ''': @author  David Hoyle'#13#10 +
    'Attribute MyAttr = 1'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a method comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  None.'#13#10 +
    ''': @postcon None.'#13#10 +
    ''':'#13#10 +
    ''': @param  iParam as a Long as a Reference'#13#10 +
    ''': @return a Boolean'#13#10 +
    ''':'#13#10 +
    'Private Function MyMethod(ByRef iParam as Long) As Boolean'#13#10 +
    'End Function'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment is NIL!');
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure05;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    'Property Let Field(ByRef strText As String, iField As Long, strValue As String)'#13#10 +
    '  Const iPipe As Long = 124'#13#10 +
    '  Dim i As Long'#13#10 +
    '  Dim iFields As Long'#13#10 +
    '  Dim Result As String'#13#10 +
    '  Exception.Push "TActAndRelDifferences.Field", strText, iField, strValue'#13#10 +
    '  On Error GoTo ErrHnd'#13#10 +
    '  Result = ""'#13#10 +
    '  iFields = CharCount(strText, iPipe) + 1'#13#10 +
    '  For i = 1 To iFields'#13#10 +
    '    If Result <> "" Then Result = Result & "|"'#13#10 +
    '    If i <> iField Then'#13#10 +
    '      Result = Result & GetField(strText, i, iPipe)'#13#10 +
    '    Else'#13#10 +
    '      Result = Result & strValue'#13#10 +
    '    End If'#13#10 +
    '  Next i'#13#10 +
    '  strText = Result'#13#10 +
    'ErrHnd:'#13#10 +
    '  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err'#13#10 +
    '  Exception.Pop'#13#10 +
    'End Property '#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure06;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    ''':'#13#10 +
    ''': This is a module comment.'#13#10 +
    ''':'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date    ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''': @author  David Hoyle'#13#10 +
    'Option Compare Text'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''': This is a function comment.'#13#10 +
    ''':'#13#10 +
    ''': @precon  '#13#10 +
    ''': @postcon '#13#10 +
    ''':'#13#10 +
    'sub Hello()'#13#10 +
    'end sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment is NIL!');
    CheckEquals(1, M.Comment.Line);
    CheckEquals(1, M.Comment.Column);
    CheckEquals(4, M.Comment.Tag[0].Line);
    CheckEquals(5, M.Comment.Tag[0].Column);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure07;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    'Option Compare Text'#13#10 +
    'function Hello() as string()'#13#10 +
    'end function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure08;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    'Option Compare Text'#13#10 +
    ''#13#10 +
    'Public Sub Hello()'#13#10 +
    '  ForwardRef(1)'#13#10 +
    'End Sub'#13#10 +
    ''#13#10 +
    'Private Sub ForwardRef(i As Long)'#13#10 +
    ''#13#10 +
    'End Sub'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure09;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options +
    [doShowMissingVBExceptionWarnings];
  Try
    strCode :=
      'Option Compare Text'#13#10 +
      ''#13#10 +
      'Public Sub Hello()'#13#10 +
      '  Exception.Push "VBFile.Hello1" '#13#10 +
      '  On Error Goto ErrHnd'#13#10 +
      '  DoSomething'#13#10 +
      'ErrHnd:'#13#10 +
      '  If Err.Number <> 0 Then Exception.DisplayException Err'#13#10 +
      '  Exception.Pop'#13#10 +
      'End Sub'#13#10 +
      ''#13#10;
    M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      CheckEquals(1, M.HeadingCount(strWarnings), M.FirstWarning);
      //: @debug CheckEquals('  [The name passed to the Exception.Push method ("VBFile.Hello1") is incorrect (''VBFile.Hello'').]', M.firstwarning);
      CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
      CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Finally
      M.Free;
    End;
    strCode :=
      'Option Compare Text'#13#10 +
      ''#13#10 +
      'Public Sub Hello()'#13#10 +
      '  Exception.Push "VBFile.Hello"'#13#10 +
      '  On Error Goto ErrHnd'#13#10 +
      '  DoSomething'#13#10 +
      'ErrHnd:'#13#10 +
      '  If Err.Number <> 0 Then Exception.DisplayException Err'#13#10 +
      '  Exception.Pop'#13#10 +
      'End Sub'#13#10 +
      ''#13#10;
    M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
      CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
      CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Finally
      M.Free;
    End;
  Finally
    TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options -
      [doShowMissingVBExceptionWarnings];
  End;
end;

procedure TestTVBModule.TestFailure10;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options +
    [doShowMissingVBExceptionWarnings];
  Try
    strCode :=
      'Option Compare Text'#13#10 +
      ''#13#10 +
      'Public Sub Hello(iParam as Long)'#13#10 +
      '  Exception.Push "VBFile.Hello" '#13#10 +
      '  On Error Goto ErrHnd'#13#10 +
      '  DoSomething'#13#10 +
      'ErrHnd:'#13#10 +
      '  If Err.Number <> 0 Then Exception.DisplayException Err'#13#10 +
      '  Exception.Pop'#13#10 +
      'End Sub'#13#10 +
      ''#13#10;
    M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      CheckEquals(2, M.HeadingCount(strWarnings), M.FirstWarning);
      //: @debug CheckEquals('  [The parameter ''iParam'' in ''VBFile.Hello'' does not have a corresponding parameter in the Exception.Push statement.]', M.firstwarning);
      CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
      CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Finally
      M.Free;
    End;
    strCode :=
      'Option Compare Text'#13#10 +
      ''#13#10 +
      'Public Sub Hello(iParam as Long)'#13#10 +
      '  Exception.Push "VBFile.Hello", iParam'#13#10 +
      '  On Error Goto ErrHnd'#13#10 +
      '  DoSomething'#13#10 +
      'ErrHnd:'#13#10 +
      '  If Err.Number <> 0 Then Exception.DisplayException Err'#13#10 +
      '  Exception.Pop'#13#10 +
      'End Sub'#13#10 +
      ''#13#10;
    M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
      CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
      CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Finally
      M.Free;
    End;
  Finally
    TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options -
      [doShowMissingVBExceptionWarnings];
  End;
end;

procedure TestTVBModule.TestFailure11;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    ''':'#13#10 +
    ''': @todo Hello dave'#13#10 +
    ''':'#13#10 +
    'Option Compare Text'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment IS Nil');
    CheckEquals('todo', M.Comment.Tag[0].TagName);
    CheckEquals(2, M.Comment.Tag[0].Line);
    CheckEquals(5, M.Comment.Tag[0].Column);
  Finally
    M.Free;
  End;
  strCode :=
    '  '':'#13#10 +
    '  '': @todo Hello dave'#13#10 +
    '  '':'#13#10 +
    'Option Compare Text'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment IS Nil');
    CheckEquals('todo', M.Comment.Tag[0].TagName);
    CheckEquals(2, M.Comment.Tag[0].Line);
    CheckEquals(7, M.Comment.Tag[0].Column);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure12;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    ''':'#13#10 +
    ''': @todo Hello'#13#10 +
    ''':       Dave.'#13#10 +
    ''': @see  Goodbye.'#13#10 +
    ''':'#13#10 +
    'Option Compare Text'#13#10 +
    ''#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    Check(M.Comment <> Nil, 'Module comment IS Nil');
    CheckEquals('todo', M.Comment.Tag[0].TagName);
    CheckEquals(2, M.Comment.Tag[0].Line);
    CheckEquals(5, M.Comment.Tag[0].Column);
    CheckEquals('Hello Dave.', M.Comment.Tag[0].AsString(9999, True));
    CheckEquals(1, M.BodyCommentCount);
    CheckEquals('Hello Dave.', M.BodyComment[0].Tag[0].AsString(9999, True));
    CheckEquals('Goodbye.', M.BodyComment[0].Tag[1].AsString(9999, True));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestFailure13;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  strCode :=
    ''':'#13#10 +
    ''':  This module contains a class to represent the preceeding relationships from an activity'#13#10 +
    ''':  and calculate the driving path which is defined as the path where the predecessor activity +'#13#10 +
    ''':  Lag - Successor date is a minimum for a specified relationship type (FS, SS, FF, etc).'#13#10 +
    ''':'#13#10 +
    ''':  @Author  David Hoyle'#13#10 +
    ''':  @Date    ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    ''':  @Version 1.0'#13#10 +
    ''':'#13#10 +
    'Option Explicit'#13#10 +
    'Option Compare Text'#13#10 +
    ''#13#10 +
    ''': A private constant to define the growth capacity of the relationship array'#13#10 +
    'Private Const iCAPACITY As Long = 10'#13#10 +
    ''#13#10 +
    ''': A private array to hold the relationships'#13#10 +
    'Private FRelationships() As TRelationship'#13#10 +
    ''': A private variable to hold the number of relationships in the collection.'#13#10 +
    'Private FCount As Long'#13#10 +
    ''': A private variable to hold the selected relationship.'#13#10 +
    'Private FIndex As Long'#13#10 +
    ''': A private variable to hold the relationship selection form.'#13#10 +
    'Private frm As New frmRelationships'#13#10 +
    ''': A private variable to hold a collection of calendars'#13#10 +
    'Private FCalendars As New TCalendars '': @bug Is not cached across Preds and Succ!'#13#10 +
    ''#13#10 +
    ''':'#13#10 +
    ''':  This method calculates the relationships between the given activity and its predecessors'#13#10 +
    ''':  a returns true if there are any.'#13#10 +
    ''':'#13#10 +
    ''':  @precon  iProjectID must be a valid project'#13#10 +
    ''':  @postcon Returns true if relationships are found.'#13#10 +
    ''':'#13#10 +
    ''':  @param   iProject     as a Long'#13#10 +
    ''':  @param   strSuccActID as a String'#13#10 +
    ''':  @return  a Boolean'#13#10 +
    ''':'#13#10 +
    'Public Function GetPredecessor(iProject As Long, strSuccActID As String) As Boolean'#13#10 +
    '  Exception.Push "TPredRelationships.GetPredecessor", iProject, strSuccActID'#13#10 +
    '  On Error GoTo ErrHnd'#13#10 +
    '  FCount = 0'#13#10 +
    '  FIndex = 1'#13#10 +
    '  BuildRelationshipList iProject, strSuccActID'#13#10 +
    '  DeleteRelationships'#13#10 +
    '  If FCount > 1 Then FIndex = frm.Execute(iProject, strSuccActID, Me)'#13#10 +
    '  GetPredecessor = (FCount > 0)'#13#10 +
    'ErrHnd:'#13#10 +
    '  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err'#13#10 +
    '  Exception.Pop'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.Testfailure14;

Var
  M : TBaseLanguageModule;
  strCode : String;

Begin
  TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options +
    [doShowMissingVBExceptionWarnings];
  Try
    strCode :=
      'Option Explicit'#13#10 +
      'Option Compare Text'#13#10 +
      ''#13#10 +
      'Public Function GetPredecessor() As Boolean'#13#10 +
      '  Exception.Push "VBFile.GetPredecessor", iProject, strSuccActID'#13#10 +
      '  On Error GoTo ErrHnd'#13#10 +
      '  FCount = 0'#13#10 +
      '  FIndex = 1'#13#10 +
      '  BuildRelationshipList iProject, strSuccActID'#13#10 +
      '  DeleteRelationships'#13#10 +
      '  If FCount > 1 Then FIndex = frm.Execute(iProject, strSuccActID, Me)'#13#10 +
      '  GetPredecessor = (FCount > 0)'#13#10 +
      'ErrHnd:'#13#10 +
      '  If Err.Number <> 0 Then Exception.DisplayErrorMessage Err'#13#10 +
      '  Exception.Pop'#13#10 +
      'End Function'#13#10;
    M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      CheckEquals(1, M.HeadingCount(strWarnings), M.FirstWarning);
      CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
      CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
      //: @debug CheckEquals('  [The function ''VBFile.GetPredecessor'' has the wrong number of Exception.Push parameters (0 not 2).]', M.FirstWarning)
    Finally
      M.Free;
    End;
  Finally
    TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options -
      [doShowMissingVBExceptionWarnings];
  End;
end;

Procedure TestTVBModule.TestFailure15;

Var
  M : TBaseLanguageModule;
  strCode : String;
  
Begin
  TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options +
    [doShowMissingVBExceptionWarnings];
  Try
    strCode :=
      'Option Explicit'#13#10 +
      'Option Compare Text'#13#10 +
      ''#13#10 +
      'Public Property Get Selected() As Boolean'#13#10 +
      'End Property'#13#10 +
      #13#10 +
      'Public Property Get Selected() As Boolean'#13#10 +
      'End Property'#13#10 +
      #13#10;
    M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
    Try
      CheckEquals(1, M.HeadingCount(strErrors), M.FirstError);
      CheckEquals(5, M.HeadingCount(strWarnings), M.FirstWarning);
      CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
      CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
      //: @debug CheckEquals('  [The function ''VBFile.GetPredecessor'' has the wrong number of Exception.Push parameters (0 not 2).]', M.FirstWarning)
    Finally
      M.Free;
    End;
  Finally
    TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options -
      [doShowMissingVBExceptionWarnings];
  End;
End;

procedure TestTVBModule.TestFunctions;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Function Test() As Long'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Function Test() As Long', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Function Test(i as long) As Long'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPrivate, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Function Test(i As long) As Long', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Friend Function Test(i as long) As Long'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scFriend, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Function Test(i As long) As Long', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Function Test(Optional i as long = 0) As Long'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Function Test(Optional i As long = 0) As Long', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Function Test(i as msforms.long, str as String) As Long'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Function Test(i As msforms.long, str As String) As Long', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Function Test(ParamArray i as long) As MSForms.Integer'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Function Test(ParamArray i As long) As MSForms.Integer', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Function Test1(i as msforms.long, str as String) As Long'#13#10 +
    'End Function'#13#10 +
    'Private Function Test2(i as msforms.long) As Long'#13#10 +
    'End Function'#13#10 +
    'Function Test3(Optional i as Long = 0) As Long'#13#10 +
    'End Function'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strImplementedMethodsLabel).Elements[2].Scope);
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[3].Scope);
    CheckEquals('Function Test1(i As msforms.long, str As String) As Long', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
    CheckEquals('Function Test2(i As msforms.long) As Long', M.FindElement(strImplementedMethodsLabel).Elements[2].AsString(True, False));
    CheckEquals('Function Test3(Optional i As Long = 0) As Long', M.FindElement(strImplementedMethodsLabel).Elements[3].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestGetComment;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    ''':'#13#10 +
    ''': This is a module comment.'#13#10 +
    ''':'#13#10 +
    ''': @author David Hoyle'#13#10 +
    ''': @version 1.0'#13#10 +
    ''': @date ' + FormatDateTime('dd mmm yyyy', Now) + #13#10 +
    'Option Explicit'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(0, M.HeadingCount(strDocumentationConflicts), M.DocConflict(1));
    Check(M.Comment <> Nil, 'Module Comment is NIL!');
    CheckEquals('This is a module comment.', M.Comment.AsString(80, True));
    CheckEquals('David Hoyle', M.Comment.Tag[0].AsString(80, True));
    CheckEquals('1.0', M.Comment.Tag[1].AsString(80, True));
    CheckEquals(FormatDateTime('dd mmm yyyy', Now), M.Comment.Tag[2].AsString(80, True));
    Checkequals(1, M.Comment.Line);
    Checkequals(1, M.Comment.Column);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestImplements;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Option Explicit'#13#10 +
    'Implements ITestCase'#13#10 +
    'Implements ITestManager'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestOption;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Option Base 1'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Base 1', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Base 0'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Base 0', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Compare Binary'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Compare Binary', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Compare Database'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Compare Database', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Compare Text'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Compare Text', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Explicit'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Explicit', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Private Module'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals('Private Module', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Option Explicit'#13#10 +
    'Option Compare Text'#13#10 +
    'Option Private Module'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strOptionsLabel));
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[1].Scope);
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[2].Scope);
    CheckEquals(scNone, M.FindElement(strOptionsLabel).Elements[3].Scope);
    CheckEquals('Compare Text', M.FindElement(strOptionsLabel).Elements[1].AsString(True, False));
    CheckEquals('Explicit', M.FindElement(strOptionsLabel).Elements[2].AsString(True, False));
    CheckEquals('Private Module', M.FindElement(strOptionsLabel).Elements[3].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestProperties;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Property Get Test() As String'#13#10 +
    'End Property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedPropertiesLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedPropertiesLabel).Elements[1].Scope);
    CheckEquals('Property Get Test() As String', M.FindElement(strImplementedPropertiesLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Property Let Test(i As Long)'#13#10 +
    'End Property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedPropertiesLabel));
    CheckEquals(scPrivate, M.FindElement(strImplementedPropertiesLabel).Elements[1].Scope);
    CheckEquals('Property Let Test(i As Long)', M.FindElement(strImplementedPropertiesLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Friend Property Let Test(i As Long)'#13#10 +
    'End Property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedPropertiesLabel));
    CheckEquals(scFriend, M.FindElement(strImplementedPropertiesLabel).Elements[1].Scope);
    CheckEquals('Property Let Test(i As Long)', M.FindElement(strImplementedPropertiesLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Property Set Test(Obj As TObject)'#13#10 +
    'End Property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedPropertiesLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedPropertiesLabel).Elements[1].Scope);
    CheckEquals('Property Set Test(Obj As TObject)', M.FindElement(strImplementedPropertiesLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Property Let Test1(i As Long)'#13#10 +
    'End Property'#13#10 +
    'Public Property Set Test2(Obj As TObject)'#13#10 +
    'End Property'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(2, M.HeadingCount(strImplementedPropertiesLabel));
    CheckEquals(scPrivate, M.FindElement(strImplementedPropertiesLabel).Elements[1].Scope);
    CheckEquals(scPublic, M.FindElement(strImplementedPropertiesLabel).Elements[2].Scope);
    CheckEquals('Property Let Test1(i As Long)', M.FindElement(strImplementedPropertiesLabel).Elements[1].AsString(True, False));
    CheckEquals('Property Set Test2(Obj As TObject)', M.FindElement(strImplementedPropertiesLabel).Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestRecords;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Type THello'#13#10 +
    '  FID As Long'#13#10 +
    '  FName As String'#13#10 +
    'End Type'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strTypesLabel));
    CheckEquals(scPublic, M.FindElement(strTypesLabel).Elements[1].Scope);
    CheckEquals('Type THello', M.FindElement(strTypesLabel).Elements[1].AsString(True, False));
    CheckEquals('FID As Long', M.FindElement(strTypesLabel).Elements[1].Elements[1].AsString(True, False));
    CheckEquals('FName As String', M.FindElement(strTypesLabel).Elements[1].Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Type THello'#13#10 +
    '  FID As Long'#13#10 +
    '  FName As String'#13#10 +
    'End Type'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strTypesLabel));
    CheckEquals(scPrivate, M.FindElement(strTypesLabel).Elements[1].Scope);
    CheckEquals('Type THello', M.FindElement(strTypesLabel).Elements[1].AsString(True, False));
    CheckEquals('FID As Long', M.FindElement(strTypesLabel).Elements[1].Elements[1].AsString(True, False));
    CheckEquals('FName As String', M.FindElement(strTypesLabel).Elements[1].Elements[2].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestReservedWords;

Var
  Words : TKeyWords;
  i : Integer;

begin
  Words := FVBModule.ReservedWords;
  For i := Low(Words) To Pred(High(Words)) Do
    Check(Words[i] < Words[i + 1], Words[i] + '!<' + Words[i + 1]);
end;

procedure TestTVBModule.TestSubs;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Sub Test()'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Sub Test()', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Sub Test(i as long)'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPrivate, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Sub Test(i As long)', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Friend Sub Test(i as long)'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scFriend, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Sub Test(i As long)', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Sub Test(Optional i as long = 0)'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Sub Test(Optional i As long = 0)', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Sub Test(i as msforms.long, str as String)'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Sub Test(i As msforms.long, str As String)', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Sub Test(ParamArray i as long)'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals('Sub Test(ParamArray i As long)', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Sub Test1(i as msforms.long, str as String)'#13#10 +
    'End Sub'#13#10 +
    'Private Sub Test2(i as msforms.long)'#13#10 +
    'End Sub'#13#10 +
    ''': A comment'#13#10 +
    'Sub Test3(Optional i as Long = 0)'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strImplementedMethodsLabel));
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strImplementedMethodsLabel).Elements[2].Scope);
    CheckEquals(scPublic, M.FindElement(strImplementedMethodsLabel).Elements[3].Scope);
    CheckEquals('Sub Test1(i As msforms.long, str As String)', M.FindElement(strImplementedMethodsLabel).Elements[1].AsString(True, False));
    CheckEquals('Sub Test2(i As msforms.long)', M.FindElement(strImplementedMethodsLabel).Elements[2].AsString(True, False));
    CheckEquals('Sub Test3(Optional i As Long = 0)', M.FindElement(strImplementedMethodsLabel).Elements[3].AsString(True, False));
    CheckEquals(5, M.FindElement(strImplementedMethodsLabel).Elements[3].Comment.Line);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestVars;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Dim strText As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private strText As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPrivate, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private strText() As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPrivate, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText() As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText(1 to 10) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText(1 to 10) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private strText(1 to 10, 1 to 2) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPrivate, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText(1 to 10, 1 to 2) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText(1 to 10, 1 to 2) As MSForms.Integer'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText(1 to 10, 1 to 2) As MSForms.Integer', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public WithEvents strText As Integer'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('WithEvents strText As Integer', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText1 As Integer'#13#10 +
    'Private strText2 As String'#13#10 +
    'Dim strText3 As Double'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strVarsLabel).Elements[2].Scope);
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[3].Scope);
    CheckEquals('strText1 As Integer', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
    CheckEquals('strText2 As String', M.FindElement(strVarsLabel).Elements[2].AsString(True, False));
    CheckEquals('strText3 As Double', M.FindElement(strVarsLabel).Elements[3].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText1 As Integer, strText2 As String'#13#10 +
    'Dim strText3 As Double'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[2].Scope);
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[3].Scope);
    CheckEquals('strText1 As Integer', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
    CheckEquals('strText2 As String', M.FindElement(strVarsLabel).Elements[2].AsString(True, False));
    CheckEquals('strText3 As Double', M.FindElement(strVarsLabel).Elements[3].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestVersion;

Var
  M : TBaseLanguageModule;
  strCode : String;
  VL: TElementContainer;

begin
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'BEGIN'#13#10 +
    '  Multiuse = -1  ''True'#13#10 +
    'END'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
    CheckEquals('VERSION 1.0 CLASS', M.FindElement(strVersionLabel).Elements[1].AsString(True, False));
    CheckEquals('Multiuse = - 1', M.FindElement(strVersionLabel).Elements[1].Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 5.00'#13#10 +
    'Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmWireRunWizard'#13#10 +
    '   Caption         =   "Wire Run Wizard"'#13#10 +
    '   ClientHeight    =   4620'#13#10 +
    '   ClientLeft      =   45'#13#10 +
    '   ClientTop       =   435'#13#10 +
    '   ClientWidth     =   6180'#13#10 +
    '   OleObjectBlob   =   "frmWireRunWizard.frx":0000'#13#10 +
    '   StartUpPosition =   1  ''CenterOwner'#13#10 +
    'End'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Frm', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    VL := M.FindElement(strVersionLabel);
    CheckEquals(scNone, VL.Elements[1].Scope);
    CheckEquals('VERSION 5.00', VL.Elements[1].AsString(True, False));
    CheckEquals('Caption = "Wire Run Wizard"', VL.Elements[1].Elements[1].Elements[1].AsString(True, False));
    CheckEquals('ClientHeight = 4620', VL.Elements[1].Elements[1].Elements[2].AsString(True, False));
    CheckEquals('ClientLeft = 45', VL.Elements[1].Elements[1].Elements[3].AsString(True, False));
    CheckEquals('ClientTop = 435', VL.Elements[1].Elements[1].Elements[4].AsString(True, False));
    CheckEquals('ClientWidth = 6180', VL.Elements[1].Elements[1].Elements[5].AsString(True, False));
    CheckEquals('OleObjectBlob = "frmWireRunWizard.frx" : 0000', VL.Elements[1].Elements[1].Elements[6].AsString(True, False));
    CheckEquals('StartUpPosition = 1', VL.Elements[1].Elements[1].Elements[7].AsString(True, False));
  Finally
    M.Free;
  End;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('VB Module Tests', TestTVBModule.Suite);
End.
