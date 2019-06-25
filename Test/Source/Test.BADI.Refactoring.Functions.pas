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
Unit Test.BADI.Refactoring.Functions;

Interface

Uses
  TypInfo,
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Refactoring.Functions;

Type
  //
  // Test Class for the Functions Class Methods.
  //
  TRefactoringExtendedTestCase = Class(TExtendedTestCase)
  Strict Private
  Public
    Procedure CheckEquals(setExpected, setActual : TBADIRefactoringTypes; strMsg: String = '');
      Overload; Virtual;
    Procedure CheckEquals(setExpected, setActual : TBADIRefactoringScopes; strMsg: String = '');
      Overload; Virtual;
    Procedure CheckEquals(eExpected, eActual : TBADIRefactoringInsertionType; strMsg: String = '');
      Overload; Virtual;
    Procedure CheckEquals(eExpected, eActual : TBADIRefactoringInsertPosition; strMsg: String = '');
      Overload; Virtual;
  End;
  
  TTestRefactoringFunctions = Class(TRefactoringExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestConstStringLiteralInMethodWithNothing;
    Procedure TestConstStringLiteralInMethodWithUses;
    Procedure TestConstStringLiteralInMethodWithType;
    Procedure TestConstStringLiteralInMethodWithVar;
    Procedure TestResStrStringLiteralInMethodWithConst;
    Procedure TestConstStringLiteralInMethodWithResStr;
    Procedure TestConstStringLiteralInMethodWithConst;
    Procedure TestResStrStringLiteralInMethodWithResStr;
  End;

Implementation

Uses
  BADI.Pascal.Module, 
  BADI.Types, 
  BADI.ResourceStrings;

Procedure TTestRefactoringFunctions.TestConstStringLiteralInMethodWithConst;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'Const'#13#10 +                        //  5
    '  iInteger = 1;'#13#10 +              //  6
    '  dblDouble = 1.23;'#13#10 +          //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'Const'#13#10 +                        // 13
    '  iInt64 = 23;'#13#10 +               // 14
    '  sFloat =23.45;'#13#10 +             // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'Const'#13#10 +                        // 19
    '  bBoolean = True;'#13#10 +           // 20
    '  qQwerty = 23;'#13#10 +              // 21
    ''#13#10 +                             // 22
    'Begin'#13#10 +                        // 23
    '  WriteLn(''Hello'');'#13#10 +        // 24
    'End;'#13#10 +                         // 25
    ''#13#10 +                             // 26
    'End.';                                // 27

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(24, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(24, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtConstant];
      RI.Scopes := [rsLocal];
      iLine := 23;
      CheckEquals(True,  Assigned(RI.FindCRSElement(RI.Method, iLine)),           'Local 1');
      CheckEquals(22, iLine,                                                      'Local 2');
      CheckEquals(23, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(22, R.FLine,                                                    'Local 4');
      CheckEquals(ripAfter, R.FPosition,                                          'Local 5');
      CheckEquals(ritAppend, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtConstant];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(True,  Assigned(RI.FindCRSElement(M, iLine)),                   'Impl 1');
      CheckEquals(16, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(16, R.FLine,                                                    'Impl 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Impl 4');
      CheckEquals(ritAppend, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtConstant];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(True,  Assigned(RI.FindCRSElement(M, iLine)),                   'Inter 1');
      Checkequals(8, iLine,                                                      'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(8, R.FLine,                                                     'Inter 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Inter 4');
      CheckEquals(ritAppend, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestConstStringLiteralInMethodWithNothing;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    '  Procedure MyProc;'#13#10 +          //  5
    ''#13#10 +                             //  6
    'Implementation'#13#10 +               //  7
    ''#13#10 +                             //  8
    'Procedure TMyClass.MyProc;'#13#10 +   //  9
    ''#13#10 +                             // 10
    'Begin'#13#10 +                        // 11
    '  WriteLn(''Hello'');'#13#10 +        // 12
    'End;'#13#10 +                         // 13
    ''#13#10 +                             // 14
    'End.';                                // 15

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(12, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(12, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(7, RI.ImplementationToken.Line,                                 'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtConstant];
      RI.Scopes := [rsLocal];
      iLine := 11;
      CheckEquals(False,  Assigned(RI.FindCRSElement(RI.Method, iLine)),          'Local 1');
      CheckEquals(11, iLine,                                                      'Local 2');
      CheckEquals(11, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(11, R.FLine,                                                    'Local 4');
      CheckEquals(ripBefore,  R.FPosition,                                        'Local 5');
      CheckEquals(ritCreate, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtConstant];
      RI.Scopes := [rsImplementation];
      iLine := 8;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Impl 1');
      CheckEquals(8, iLine,                                                       'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(8, R.FLine,                                                     'Impl 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Impl 4');
      CheckEquals(ritCreate, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtConstant];
      RI.Scopes := [rsInterface];
      iLine := 7;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Inter 1');
      Checkequals(7, iLine,                                                       'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(7, R.FLine,                                                     'Inter 3');
      CheckEquals(ripBefore,  R.FPosition,                                        'Inter 4');
      CheckEquals(ritCreate, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestResStrStringLiteralInMethodWithConst;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'Const'#13#10 +                        //  5
    '  iInteger = 1;'#13#10 +              //  6
    '  dblDouble = 1.23;'#13#10 +          //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'Const'#13#10 +                        // 13
    '  iInt64 = 23;'#13#10 +               // 14
    '  sFloat =23.45;'#13#10 +             // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'Const'#13#10 +                        // 19
    '  bBoolean = True;'#13#10 +           // 20
    '  qQwerty = 23;'#13#10 +              // 21
    ''#13#10 +                             // 22
    'Begin'#13#10 +                        // 23
    '  WriteLn(''Hello'');'#13#10 +        // 24
    'End;'#13#10 +                         // 25
    ''#13#10 +                             // 26
    'End.';                                // 27

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(24, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(24, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtResourceString];
      RI.Scopes := [rsLocal];
      iLine := 23;
      CheckEquals(False,  Assigned(RI.FindCRSElement(RI.Method, iLine)),          'Local 1');
      CheckEquals(23, iLine,                                                      'Local 2');
      CheckEquals(23, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(19, R.FLine,                                                    'Local 4');
      CheckEquals(ripBefore, R.FPosition,                                         'Local 5');
      CheckEquals(ritCreate, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtResourceString];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Impl 1');
      CheckEquals(12, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(13, R.FLine,                                                    'Impl 3');
      CheckEquals(ripBefore,  R.FPosition,                                        'Impl 4');
      CheckEquals(ritCreate, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtResourceString];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Inter 1');
      Checkequals(11, iLine,                                                      'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(5, R.FLine,                                                     'Inter 3');
      CheckEquals(ripBefore,  R.FPosition,                                        'Inter 4');
      CheckEquals(ritCreate, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestResStrStringLiteralInMethodWithResStr;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'ResourceString'#13#10 +               //  5
    '  S1 = ''Hello'';'#13#10 +            //  6
    '  S2 = ''Goodbye'';'#13#10 +          //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'ResourceString'#13#10 +               // 13
    '  S3 = ''Welcome'';'#13#10 +          // 14
    '  S4 = ''To'';'#13#10 +               // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'ResourceString'#13#10 +               // 19
    '  S5 = ''Planet'';'#13#10 +           // 20
    '  S6 = ''Earth'';'#13#10 +            // 21
    ''#13#10 +                             // 22
    'Begin'#13#10 +                        // 23
    '  WriteLn(''Hello'');'#13#10 +        // 24
    'End;'#13#10 +                         // 25
    ''#13#10 +                             // 26
    'End.';                                // 27

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(24, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(24, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtResourceString];
      RI.Scopes := [rsLocal];
      iLine := 23;
      CheckEquals(True,  Assigned(RI.FindCRSElement(RI.Method, iLine)),           'Local 1');
      CheckEquals(22, iLine,                                                      'Local 2');
      CheckEquals(23, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(22, R.FLine,                                                    'Local 4');
      CheckEquals(ripAfter, R.FPosition,                                          'Local 5');
      CheckEquals(ritAppend, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtResourceString];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(True,  Assigned(RI.FindCRSElement(M, iLine)),                   'Impl 1');
      CheckEquals(16, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(16, R.FLine,                                                    'Impl 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Impl 4');
      CheckEquals(ritAppend, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtResourceString];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(True,  Assigned(RI.FindCRSElement(M, iLine)),                   'Inter 1');
      Checkequals(8, iLine,                                                       'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(8, R.FLine,                                                     'Inter 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Inter 4');
      CheckEquals(ritAppend, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestConstStringLiteralInMethodWithResStr;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'ResourceString'#13#10 +               //  5
    '  S1 = ''Hello'';'#13#10 +            //  6
    '  S2 = ''Goodbye'';'#13#10 +          //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'ResourceString'#13#10 +               // 13
    '  S3 = ''Welcome'';'#13#10 +          // 14
    '  S4 = ''To'';'#13#10 +               // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'ResourceString'#13#10 +               // 19
    '  S5 = ''Planet'';'#13#10 +           // 20
    '  S6 = ''Earth'';'#13#10 +            // 21
    ''#13#10 +                             // 22
    'Begin'#13#10 +                        // 23
    '  WriteLn(''Hello'');'#13#10 +        // 24
    'End;'#13#10 +                         // 25
    ''#13#10 +                             // 26
    'End.';                                // 27

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(24, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(24, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtConstant];
      RI.Scopes := [rsLocal];
      iLine := 23;
      CheckEquals(False,  Assigned(RI.FindCRSElement(RI.Method, iLine)),          'Local 1');
      CheckEquals(23, iLine,                                                      'Local 2');
      CheckEquals(23, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(22, R.FLine,                                                    'Local 4');
      CheckEquals(ripAfter, R.FPosition,                                          'Local 5');
      CheckEquals(ritCreate, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtConstant];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Impl 1');
      CheckEquals(12, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(16, R.FLine,                                                    'Impl 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Impl 4');
      CheckEquals(ritCreate, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtConstant];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Inter 1');
      Checkequals(11, iLine,                                                      'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(8, R.FLine,                                                     'Inter 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Inter 4');
      CheckEquals(ritCreate, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestConstStringLiteralInMethodWithType;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'Type'#13#10 +                         //  5
    '  TInteger = Integer;'#13#10 +        //  6
    '  TDouble = Doulbe;'#13#10 +          //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'Type'#13#10 +                         // 13
    '  TInt64 = Int64;'#13#10 +            // 14
    '  TFloat = Real;'#13#10 +             // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'Type'#13#10 +                         // 19
    '  TBoolean = Boolean;'#13#10 +        // 20
    '  TQwerty = Integer;'#13#10 +         // 21
    ''#13#10 +                             // 22
    'Begin'#13#10 +                        // 23
    '  WriteLn(''Hello'');'#13#10 +        // 24
    'End;'#13#10 +                         // 25
    ''#13#10 +                             // 26
    'End.';                                // 27

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(24, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(24, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtConstant];
      RI.Scopes := [rsLocal];
      iLine := 23;
      CheckEquals(False,  Assigned(RI.FindCRSElement(RI.Method, iLine)),          'Local 1');
      CheckEquals(23, iLine,                                                      'Local 2');
      CheckEquals(23, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(22, R.FLine,                                                    'Local 4');
      CheckEquals(ripAfter, R.FPosition,                                          'Local 5');
      CheckEquals(ritCreate, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtConstant];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Impl 1');
      CheckEquals(12, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(16, R.FLine,                                                    'Impl 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Impl 4');
      CheckEquals(ritCreate, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtConstant];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Inter 1');
      Checkequals(11, iLine,                                                      'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(8, R.FLine,                                                     'Inter 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Inter 4');
      CheckEquals(ritCreate, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestConstStringLiteralInMethodWithUses;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'Uses'#13#10 +                         //  5
    '  Windows,'#13#10 +                   //  6
    '  SysUtils;'#13#10 +                  //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'Uses'#13#10 +                         // 13
    '  Classes,'#13#10 +                   // 14
    '  Graphics;'#13#10 +                  // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'Begin'#13#10 +                        // 19
    '  WriteLn(''Hello'');'#13#10 +        // 20
    'End;'#13#10 +                         // 21
    ''#13#10 +                             // 22
    'End.';                                // 23

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(20, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(20, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals(True, RI.Scopes = [rsLocal..rsInterface],                       'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals(True, RI.Types = [rtConstant..rtResourceString],                'Types');
      // Local
      RI.Types := [rtConstant];
      RI.Scopes := [rsLocal];
      iLine := 19;
      CheckEquals(False,  Assigned(RI.FindCRSElement(RI.Method, iLine)),          'Local 1');
      CheckEquals(19, iLine,                                                      'Local 2');
      CheckEquals(19, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(19, R.FLine,                                                    'Local 4');
      CheckEquals(ripBefore,  R.FPosition,                                        'Local 5');
      CheckEquals(ritCreate, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtConstant];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Impl 1');
      CheckEquals(12, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(16, R.FLine,                                                    'Impl 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Impl 4');
      CheckEquals(ritCreate, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtConstant];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Inter 1');
      Checkequals(11, iLine,                                                      'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(8, R.FLine,                                                     'Inter 3');
      CheckEquals(ripAfter,  R.FPosition,                                         'Inter 4');
      CheckEquals(ritCreate, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

Procedure TTestRefactoringFunctions.TestConstStringLiteralInMethodWithVar;

Const
  strSource = 
    'Unit MyUnit;'#13#10 +                 //  1
    ''#13#10 +                             //  2
    'Interface'#13#10 +                    //  3
    ''#13#10 +                             //  4
    'Var'#13#10 +                          //  5
    '  iInteger : Integer;'#13#10 +        //  6
    '  dblDouble : Doulbe;'#13#10 +        //  7
    ''#13#10 +                             //  8
    '  Procedure MyProc;'#13#10 +          //  9
    ''#13#10 +                             // 10
    'Implementation'#13#10 +               // 11
    ''#13#10 +                             // 12
    'Var'#13#10 +                          // 13
    '  iInt64 : Int64;'#13#10 +            // 14
    '  sFloat : Real;'#13#10 +             // 15
    ''#13#10 +                             // 16
    'Procedure TMyClass.MyProc;'#13#10 +   // 17
    ''#13#10 +                             // 18
    'Var'#13#10 +                          // 19
    '  bBoolean : Boolean;'#13#10 +        // 20
    '  qQwerty : Integer;'#13#10 +         // 21
    ''#13#10 +                             // 22
    'Begin'#13#10 +                        // 23
    '  WriteLn(''Hello'');'#13#10 +        // 24
    'End;'#13#10 +                         // 25
    ''#13#10 +                             // 26
    'End.';                                // 27

Var
  M : TPascalModule;
  RI: TBADIRefactoringInfo;
  iLine: Integer;
  R: TBADIRefactoringInsertionInfo;
  
Begin
  M := TPascalModule.CreateParser(strSource, 'Test.pas', False, [moParse]);
  Try
    RI := TBADIRefactoringInfo.Create(M);
    Try
      CheckEquals(0, M.HeadingCount(strErrors), M.FirstError);
      RI.UpdateScopeAndType(24, 12);
      CheckEquals(True, Assigned(RI.Token),                                       'Token');
      CheckEquals(24, RI.Token.Line,                                              'Token.Line');
      CheckEquals(11, RI.Token.Column,                                            'Token.Column');
      CheckEquals(True, Assigned(RI.InterfaceToken),                              'Assigned(Interface)');
      CheckEquals(3, RI.InterfaceToken.Line,                                      'Interface.Line');
      CheckEquals(1, RI.InterfaceToken.Column,                                    'Interface.Column');
      CheckEquals(11, RI.ImplementationToken.Line,                                'Interface.Line');
      CheckEquals(1, RI.ImplementationToken.Column,                               'Interface.Column');
      CheckEquals([rsLocal..rsInterface], RI.Scopes,                              'Scope');
      CheckEquals(True, Assigned(RI.Method),                                      'Method');
      CheckEquals([rtConstant..rtResourceString], RI.Types,                       'Types');
      // Local
      RI.Types := [rtConstant];
      RI.Scopes := [rsLocal];
      iLine := 23;
      CheckEquals(False,  Assigned(RI.FindCRSElement(RI.Method, iLine)),          'Local 1');
      CheckEquals(23, iLine,                                                      'Local 2');
      CheckEquals(23, RI.Method.StartLine - 1,                                    'Local 3');
      R := RI.RefactorConstResStr(RI.Method, scLocal);
      CheckEquals(19, R.FLine,                                                    'Local 4');
      CheckEquals(ripBefore, R.FPosition,                                         'Local 5');
      CheckEquals(ritCreate, R.FType,                                             'Local 6');
      // Implementation
      RI.Types := [rtConstant];
      RI.Scopes := [rsImplementation];
      iLine := 12;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Impl 1');
      CheckEquals(12, iLine,                                                      'Impl 2');
      R := RI.RefactorConstResStr(M, scPrivate);
      CheckEquals(13, R.FLine,                                                    'Impl 3');
      CheckEquals(ripBefore,  R.FPosition,                                        'Impl 4');
      CheckEquals(ritCreate, R.FType,                                             'Impl 5');
      // Interface
      RI.Types := [rtConstant];
      RI.Scopes := [rsInterface];
      iLine := 11;
      CheckEquals(False,  Assigned(RI.FindCRSElement(M, iLine)),                  'Inter 1');
      Checkequals(11, iLine,                                                      'Inter 2');
      R := RI.RefactorConstResStr(M, scPublic);
      CheckEquals(5, R.FLine,                                                     'Inter 3');
      CheckEquals(ripBefore,  R.FPosition,                                        'Inter 4');
      CheckEquals(ritCreate, R.FType,                                             'Inter 5');
    Finally
      RI.Free;
    End;
  Finally
    M.Free;
  End;
End;

{ TRefactoringExtendedTestCase }

Procedure TRefactoringExtendedTestCase.CheckEquals(setExpected, setActual: TBADIRefactoringScopes;
  strMsg: String);
  
Var
  eScope : TBADIRefactoringScope;
  strExpected, strActual : String;
  
Begin
  For eScope := Low(TBADIRefactoringScope) To High(TBADIRefactoringScope) Do
    If eScope In setExpected Then
      Begin
        If strExpected <> '' Then
          strExpected := strExpected + ',';
        strExpected := strExpected + GetEnumName(TypeInfo(TBADIRefactoringScope), Ord(eScope));
      End;
  For eScope := Low(TBADIRefactoringScope) To High(TBADIRefactoringScope) Do
    If eScope In setActual Then
      Begin
        If strActual <> '' Then
          strActual := strActual + ',';
        strActual := strActual + GetEnumName(TypeInfo(TBADIRefactoringScope), Ord(eScope));
      End;
  CheckEquals(
    strExpected,
    strActual,
    strMsg
  );
End;

Procedure TRefactoringExtendedTestCase.CheckEquals(setExpected, setActual: TBADIRefactoringTypes;
  strMsg: String);

Var
  eType : TBADIRefactoringType;
  strExpected, strActual : String;
  
Begin
  For eType := Low(TBADIRefactoringType) To High(TBADIRefactoringType) Do
    If eType In setExpected Then
      Begin
        If strExpected <> '' Then
          strExpected := strExpected + ',';
        strExpected := strExpected + GetEnumName(TypeInfo(TBADIRefactoringType), Ord(eType));
      End;
  For eType := Low(TBADIRefactoringType) To High(TBADIRefactoringType) Do
    If eType In setActual Then
      Begin
        If strActual <> '' Then
          strActual := strActual + ',';
        strActual := strActual + GetEnumName(TypeInfo(TBADIRefactoringType), Ord(eType));
      End;
  CheckEquals(
    strExpected,
    strActual,
    strMsg
  );
End;

Procedure TRefactoringExtendedTestCase.CheckEquals(eExpected,
  eActual: TBADIRefactoringInsertPosition; strMsg: String);

Begin
  CheckEquals(
    GetEnumName(TypeInfo(TBADIRefactoringInsertPosition), Ord(eExpected)),
    GetEnumName(TypeInfo(TBADIRefactoringInsertPosition), Ord(eActual)),
    strMsg);
End;

Procedure TRefactoringExtendedTestCase.CheckEquals(eExpected, eActual: TBADIRefactoringInsertionType;
  strMsg: String);

Begin
  CheckEquals(
    GetEnumName(TypeInfo(TBADIRefactoringInsertionType), Ord(eExpected)),
    GetEnumName(TypeInfo(TBADIRefactoringInsertionType), Ord(eActual)),
    strMsg);
End;

Initialization
  RegisterTest('Refactoring Functions Tests', TTestRefactoringFunctions.Suite);
End.
