(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

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
unit Test.BADI.VB.ModuleFull;

interface

uses
  TestFramework,
  Classes,
  BADI.Base.Module,
  BADI.VB.ModuleFull,
  Contnrs,
  SysUtils,
  Test.BADI.Base.Module;

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
    Procedure TestGoal;
    Procedure TestVersion;
    Procedure TestVBBegin;
    Procedure TestAttributes;
    Procedure TestOption;
    Procedure TestImplements;
    Procedure TestDeclarations;
    Procedure TestInterfaceSection;
    Procedure TestImplementationSection;
    Procedure TestConsts;
    Procedure TestDims;
    Procedure TestVars;
    Procedure TestVarDecl;
    Procedure TestProcessVar;
    Procedure TestArraySizeDecl;
    Procedure TestDeclares;
    Procedure TestFunctions;
    Procedure TestSubs;
    Procedure TestProperties;
    Procedure TestRecords;
    Procedure TestEnums;
    Procedure TestGetComment;
    Procedure TestCheckDocumentation;
    Procedure TestCombinations;
    Procedure TestDisablers;
  End;

implementation

uses
  BADI.Types,
  BADI.ResourceStrings,
  BADI.Comment,
  BADI.Functions, BADI.VB.ResourceStrings;

{ TestTVBModule }

procedure TestTVBModule.Setup;
begin
  FVBModule := TVBMOdule.CreateParser('', 'VBFile.cls', False, [moParse]);
end;

procedure TestTVBModule.TearDown;
begin
  FVBModule.Free;
end;

procedure TestTVBModule.TestArraySizeDecl;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Public strText(10) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText(0 To 10) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText(10, 10) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText(0 To 10, 0 To 10) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public strText(1 To 10, 0 To 10) As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText(1 To 10, 0 To 10) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    'Const iLong = 12'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong = 12', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private Const iLong As Long = 12'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPrivate, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long = 12', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Const iLong As Long = 12'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long = 12', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Const strText As String = "Hello"'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPrivate, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long = 123', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Const iLong As MyModule.Long = &HFF00'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals('iLong As MyModule.Long = &HFF00', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public Const iLong1 As Long = &HFF00'#13#10 +
    'Private Const iLong2 As Long = 123'#13#10 +
    'Public Const iLong3 As Long = 12'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(3, M.HeadingCount(strConstantsLabel));
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[1].Scope);
    CheckEquals(scPrivate, M.FindElement(strConstantsLabel).Elements[2].Scope);
    CheckEquals(scPublic, M.FindElement(strConstantsLabel).Elements[3].Scope);
    CheckEquals('iLong1 As Long = &HFF00', M.FindElement(strConstantsLabel).Elements[1].AsString(True, False));
    CheckEquals('iLong2 As Long = 123', M.FindElement(strConstantsLabel).Elements[2].AsString(True, False));
    CheckEquals('iLong3 As Long = 12', M.FindElement(strConstantsLabel).Elements[3].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestDeclarations;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Declare Sub Test Lib "Kernal32" ()'#13#10 +
    'Sub Test()'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strDeclaresLabel));
    CheckEquals(scPublic, M.FindElement(strDeclaresLabel).Elements[1].Scope);
    CheckEquals('Sub Test Lib "Kernal32" ()', M.FindElement(strDeclaresLabel).Elements[1].AsString(True, False));
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

procedure TestTVBModule.TestDims;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Dim iLong as Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Public iLong as Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'Private iLong as Long'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPrivate, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('iLong As Long', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestDirectives;

Var
  Words : TKeyWords;
  i : Integer;

begin
  Words := FVBModule.Directives;
  For i := Low(Words) To Pred(High(Words)) Do
    Check(Words[i] < Words[i + 1], Words[i] + '!<' + Words[i + 1]);
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

procedure TestTVBModule.TestGoal;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode := '';
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 1.0 CLASS'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'Attribute MyAttribute = 123.0'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'Option Compare Text'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'Implements MyUnit.Identifier, Identifier'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'Sub Test()'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'Attribute MyAttribute = 123.0'#13#10 +
    'Option Compare Text'#13#10 +
    'Implements MyUnit.Identifier, Identifier'#13#10 +
    'Sub Test()'#13#10 +
    'End Sub'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse, moCheckForDocumentConflicts]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestImplementationSection;

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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
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
  strCode :=
    'Option Explicit'#13#10 +
    'Implements MyUnit.ITestCase'#13#10 +
    'Implements MyUnit.ITestManager'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', True, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
  Finally
    M.Free;
  End;
  strCode :=
    'Option Explicit'#13#10 +
    'Implements MyUnit.ITestCase, MyUnit.ITestManager'#13#10;
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

procedure TestTVBModule.TestInterfaceSection;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Declare Sub Test Lib "Kernal32" ()'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
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

procedure TestTVBModule.TestProcessVar;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'Dim strText'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
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
    'Public strText() As String'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVarsLabel));
    CheckEquals(scPublic, M.FindElement(strVarsLabel).Elements[1].Scope);
    CheckEquals('strText() As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
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
  Words : TKEyWords;
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

procedure TestTVBModule.TestVarDecl;

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
    CheckEquals('strText(1 To 10) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
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
    CheckEquals('strText(1 To 10, 1 To 2) As String', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
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
    CheckEquals('strText(1 To 10, 1 To 2) As MSForms.Integer', M.FindElement(strVarsLabel).Elements[1].AsString(True, False));
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
end;

procedure TestTVBModule.TestVars;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
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

procedure TestTVBModule.TestVBBegin;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'BEGIN'#13#10 +
    'END'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'BEGIN {FEE116AE-62F1-46D0-A6C4-571A3B61A733} Identifier'#13#10 +
    'END'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'BEGIN Qualified.Identifier Identifier'#13#10 +
    'END'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'BEGINPROPERTY Identifier'#13#10 +
    'ENDPROPERTY'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
  Finally
    M.Free;
  End;
end;

procedure TestTVBModule.TestVersion;

Var
  M : TBaseLanguageModule;
  strCode : String;

begin
  strCode :=
    'VERSION 1.0'#13#10 +
    'BEGIN'#13#10 +
    'END'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
    CheckEquals('VERSION 1.0', M.FindElement(strVersionLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
  strCode :=
    'VERSION 1.0 CLASS'#13#10 +
    'BEGIN'#13#10 +
    'END'#13#10;
  M := TVBModule.CreateParser(strCode, 'VBFile.Cls', False, [moParse]);
  Try
    CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
    CheckEquals(0, M.HeadingCount(strWarnings), M.FirstWarning);
    CheckEquals(0, M.HeadingCount(strHints), M.FirstHint);
    CheckEquals(ttFileEnd, M.CurrentToken.TokenType);
    CheckEquals(1, M.HeadingCount(strVersionLabel));
    CheckEquals(scNone, M.FindElement(strVersionLabel).Elements[1].Scope);
    CheckEquals('VERSION 1.0 CLASS', M.FindElement(strVersionLabel).Elements[1].AsString(True, False));
  Finally
    M.Free;
  End;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest('VB Module Full Tests', TestTVBModule.Suite);
End.



