(**
  
  This module contains a class that handles the calculation of the insertion point and type of insert for
  a refactoring.

  @Author  David Hoyle
  @Version 6.257
  @Date    06 Sep 2020

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
Unit BADI.Refactoring.Functions;

Interface

Uses
  BADI.Base.Module,
  BADI.Generic.FunctionDecl, 
  BADI.TokenInfo, 
  BADI.Types, 
  BADI.ResourceStrings, 
  BADI.ElementContainer;

Type
  (** An enumerate for the type of the refactoring - constant or resource string. **)
  TBADIRefactoringType = (rtConstant, rtResourceString);
  (** A set of the above refactoring types. **)
  TBADIRefactoringTypes = Set Of TBADIRefactoringType;

  (** An enumerate for the scope of the refactoring. **)
  TBADIRefactoringScope = (rsLocal, rsImplementation, rsInterface);
  (** A set of the above refactoring scopes. **)
  TBADIRefactoringScopes = Set Of TBADIRefactoringScope;

  (** An enumerate to define the type of refactoring insertion - append to existing section or create a
      new section. **)
  TBADIRefactoringInsertionType = (ritAppend, ritCreate);

  (** An enumerate to define whether the refactoring should be inserted above or below the given
      line. **)
  TBADIRefactoringInsertPosition = (ripAfter, ripBefore);
  
  (** A record to returns the refactoring information. **)
  TBADIRefactoringInsertionInfo = Record
    FLine     : Integer;
    FType     : TBADIRefactoringInsertionType;
    FPosition : TBADIRefactoringInsertPosition;
  End;
  
  (** A record of static methods to encapsulate the refactoring functions. **)
  TBADIRefactoringInfo = Class
  Private
    FModule         : TBaseLanguageModule;
    FInterface      : TTokenInfo;
    FImplementation : TTokenInfo;
    FScopes         : TBADIRefactoringScopes;
    FTypes          : TBADIRefactoringTypes;
    FMethod         : TGenericFunction;
    FToken          : TTokenInfo;
    FName           : String;
  Private
    Function  GetScope : TScope;
    Function  ExtractText(Const strText: String): String;
    Function  NewDeclarationPosition(Const Container : TElementContainer;
      Const iDefaultStartLine: Integer; Const setScopes : TScopes): TBADIRefactoringInsertionInfo;
    Function  NewMaxLinePosition(Const Container: TElementContainer; Const eScope : TScope): Integer;
    Function  NewMinLinePosition(Const Container: TElementContainer; Const setScope : TScopes): Integer;
    Function  NewDeclarationFromExistingCRS(Const Container : TElementContainer;
      Const setScopes : TScopes; Var ePosition : TBADIRefactoringInsertPosition) : Integer;
    Function  NewDeclarationAfterUses(Const Container : TElementContainer;
      Const setScopes : TScopes): Integer;
    Function  IsInMethod(Const iLine: Integer): Boolean;
    Function  RecurseMethods(Const Container: TElementContainer; Const iLine : Integer): Boolean;
    Function  FindBeginNotInMethod : TTokenInfo;
  Public
    Constructor Create(Const Module : TBaseLanguageModule);
    Function  FindToken(Const iLine, iColumn: Integer): TTokenIndex;
    Procedure UpdateScopeAndType(Const iLine, iColumn : Integer);
    Function  RefactoringType : TBADIRefactoringType;
    Function  FindCRSElement(Const Container: TElementContainer; Var iLine : Integer): TElementContainer;
    Function  RefactorConstResStr(Const Container : TElementContainer;
      Const eScope : TScope) : TBADIRefactoringInsertionInfo;
    (**
      This property exposes the parsed module.
      @precon  None.
      @postcon Returns the parsed module.
      @return  a TBaseLanguageModule
    **)
    Property Module : TBaseLanguageModule Read FModule;
    (**
      This property returns the Token being refactored for positional referencing.
      @precon  None.
      @postcon Returns the Token being refactored for positional referencing.
      @return  a TTokenInfo
    **)
    Property  Token : TTokenInfo Read FToken;
    (**
      This property returns the Interface Token in the unit for positional referencing.
      @precon  None.
      @postcon Returns the Interface Token in the unit for positional referencing.
      @return  a TTokenInfo
    **)
    Property  InterfaceToken : TTokenInfo Read FInterface;
    (**
      This property returns the Implementation Token in the unit for positional referencing.
      @precon  None.
      @postcon Returns the Implementation Token in the unit for positional referencing.
      @return  a TTokenInfo
    **)
    Property  ImplementationToken : TTokenInfo Read FImplementation;
    (**
      A property to returns the refactoring scopes that are allowed.
      @precon  None.
      @postcon Returns the refactoring scopes that are allowed.
      @return  a TBADIRefactoringScopes
    **)
    Property  Scopes : TBADIRefactoringScopes Read FScopes Write FScopes;
    (**
      A property to return the selected scope to insert the refactoring into.
      @precon  None.
      @postcon Return the selected scope to insert the refactoring into.
      @return  a TScope
    **)
    Property  Scope : TScope Read GetScope;
    (**
      This property returns the method that can be used for refactoring.
      @precon  None.
      @postcon Returns the method that can be used for refactoring.
      @return  a TGenericFunction
    **)
    Property  Method : TGenericFunction Read FMethod;
    (**
      A property to returns the refactoring types that are allowed.
      @precon  None.
      @postcon Returns the refactoring types that are allowed.
      @return  a TBADIRefactoringTypes
    **)
    Property  Types : TBADIRefactoringTypes Read FTypes Write FTypes;
    (**
      A property to get and set the refactoring name.
      @precon  None.
      @postcon Returns the refactoring name.
      @return  a String
    **)
    Property Name : String Read FName Write FName;
  End;

Const
  (** A constant array of section names to search for when trying to find the correct refactoring
      location. **)
  strSectionNames : Array[Low(TBADIRefactoringType)..High(TBADIRefactoringType)] Of String = (
    strConstantsLabel, strResourceStringsLabel);

Implementation

Uses
  SysUtils,
  BADI.Pascal.Module,
  BADI.Functions, 
  BADI.Pascal.RecordDecl;

(**

  A constructor for the TBADIRefactoringInfo class.

  @precon  None.
  @postcon Stores a reference to the module.

  @param   Module as a TBaseLanguageModule as a constant

**)
Constructor TBADIRefactoringInfo.Create(Const Module: TBaseLanguageModule);

Begin
  FModule := Module;
End;

(**

  This method extracts the valid identifier characters from the given text for the name of the literal.

  @precon  None.
  @postcon The first 63 valid identifier characters are extracted from the given string.

  @param   strText as a String as a constant
  @return  a String

**)
Function TBADIRefactoringInfo.ExtractText(Const strText: String): String;

  (**

    This method adds the tokenised word to the result if its more than a single character and not
    in a list of words to skip.

    @precon  None.
    @postcon The word is added to the result if applicable.

    @param   strToken as a String as a reference

  **)
  Procedure AddToken(Var strToken : String);

  Const
    iSecondChar = 2;
    strWordsToSkip : Array[1..4] Of String = ('an', 'is', 'of', 'the');

  Begin
    If (Length(strToken) > 1) And Not IsKeyWord(strToken, strWordsToSkip) Then
      Begin
        strToken := UpperCase(Copy(strToken, 1, 1)) + Copy(strToken, iSecondChar, Length(strToken) - 1);
        Result := Result + strToken;
      End;
    strToken := '';
  End;
  
Const
  strValidChars = ['a' .. 'z', 'A' .. 'Z', '_'];
  iMaxIdentLen = 63;

Var
  iChar: Integer;
  strToken : String;

Begin
  Result := '';
  For iChar := 1 To Length(strText) Do
    If CharInSet(strText[iChar], strValidChars) Then
      strToken := strToken + strText[iChar]
    Else
        AddToken(strToken);
  If Length(Result) >= iMaxIdentLen Then
    Result := Copy(Result, 1, iMaxIdentLen);
End;

(**

  This method attempts to find the BEGIN of a program, library or package.

  @precon  None.
  @postcon The token corresponding to the BEGIN of a program, library or package is returned if found
           else NIL is returned.

  @return  a TTokenInfo

**)
Function TBADIRefactoringInfo.FindBeginNotInMethod: TTokenInfo;

Const
  strBEGIN = 'BEGIN';

Var
  iToken: Integer;

Begin
  Result := Nil;
  For iToken := 0 To FModule.TokenCount - 1 Do
    If (FModule.Tokens[iToken].UToken = strBEGIN) And Not IsInMethod(FModule.Tokens[iToken].Line) Then
      Begin
        Result := FModule.Tokens[iToken];
        Break;
      End;
End;

(**

  This method searches for the constant or resource string section in the given container and returns the
  found section container if there are items in the given scope.

  @precon  Container must be a valid instance.
  @postcon Returns the searched for declaration of there are sub items of the correct scope.

  @param   Container as a TElementContainer as a constant
  @param   iLine     as an Integer as a reference
  @return  a TElementContainer

**)
Function TBADIRefactoringInfo.FindCRSElement(Const Container: TElementContainer;
  Var iLine : Integer): TElementContainer;

Begin
  Result := Container.FindElement(strSectionNames[RefactoringType]);
  If Assigned(Result) Then
    Begin
      iLine := NewMaxLinePosition(Result, Scope);
      If iLine = 0 Then
        Begin
          Result := Nil;
          iLine := 0;
        End;
    End;
End;

(**

  This method finds a token based on a line number and column number.

  @precon  None.
  @postcon Returns the token index of the token at the given line and column if found else returns -1.

  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant
  @return  a TTokenIndex

**)
Function TBADIRefactoringInfo.FindToken(Const iLine, iColumn: Integer) : TTokenIndex;

Const
  iDivisor = 2;

Var
  iFirst, iMid, iLast: Integer;
  T: TTokenInfo;

Begin
  Result := - 1;
  iFirst := 0;
  iLast := FModule.TokenCount - 1;
  While iFirst <= iLast Do
    Begin
      iMid := (iFirst + iLast) Div iDivisor;
      T := FModule.Tokens[iMid];
      If (iLine = T.Line) Then
        Begin
          If (iColumn >= T.Column) And (iColumn <= T.Column + T.Length - 1) Then
            Begin
              Result := iMid;
              FToken := FModule.Tokens[iMid];
              Break;
            End
          Else
            If iColumn < T.Column Then
            iLast := iMid - 1
          Else
            iFirst := iMid + 1;
        End
      Else
        If iLine < T.Line Then
        iLast := iMid - 1
      Else
        iFirst := iMid + 1;
    End;
End;

(**

  This is a getter method for the Scope property.

  @precon  Converts a TBADIRefactoringScopes set into a Scope based on the first scope found.
  @postcon Returns the scope the refactoring is to be inserted into.

  @return  a TScope

**)
Function TBADIRefactoringInfo.GetScope: TScope;

Var
  eScope : TBADIRefactoringScope;
  
Begin
  Result := scNone;
  For eScope := Low(TBADIRefactoringScope) To High(TBADIRefactoringScope) Do
    If eScope In FScopes Then
      Begin
        Case eScope Of
          rsLocal:          Result := scLocal;
          rsImplementation: Result := scPrivate;
          rsInterface:      Result := scPublic;
        End;
        Break;
      End;
End;

(**

  This method returns true of the given line number is in a method implementation.

  @precon  None.
  @postcon Returns true of the given line number is in a method implementation.

  @param   iLine as an Integer as a constant
  @return  a Boolean

**)
Function TBADIRefactoringInfo.IsInMethod(Const iLine: Integer) : Boolean;

Var
  IM: TElementContainer;

Begin
  Result := False;
  IM := FModule.FindElement(strImplementedMethodsLabel);
  If Assigned(IM) Then
    Result := RecurseMethods(IM, iLine);
End;

(**

  This method attempts to find a uses clause and return the position a new declaration should appear ( 
  after the uses clause).

  @precon  Container must be a valid instance.
  @postcon Returns the line number for the new declaration else returns zero.

  @param   Container as a TElementContainer as a constant
  @param   setScopes as a TScopes as a constant
  @return  an Integer

**)
Function TBADIRefactoringInfo.NewDeclarationAfterUses(Const Container : TElementContainer;
  Const setScopes : TScopes): Integer;

Const
  strInterface = 'Interface';
  strImplementation = 'Implementation';

Var
  strSection: String;
  E: TElementContainer;

Begin
  Result := 0;
  If Not (scLocal In setScopes) Then
    Begin   
      E := Container.FindElement(strUses);
      If Assigned(E) Then
        Begin
          If (FModule As TPascalModule).ModuleType = mtUnit Then
            Begin
              strSection := strInterface;
              If setScopes * [scPrivate] <> [] Then
                strSection := strImplementation;
              E := E.FindElement(strSection);
            End;
          If Assigned(E) Then
            Begin
              Result := NewMaxLinePosition(E, GetScope);
              If Result > 0 Then
                Exit;
            End;
        End;  
    End;
End;

(**

  This method tries to find where the declaration should be if there is an existing constant or resource 
  string section.

  @precon  Container must be a valid instance.
  @postcon Returns the line number of the new declaration else 0 or Max Integer.

  @param   Container as a TElementContainer as a constant
  @param   setScopes as a TScopes as a constant
  @param   ePosition as a TBADIRefactoringInsertPosition as a reference
  @return  an Integer

**)
Function TBADIRefactoringInfo.NewDeclarationFromExistingCRS(Const Container : TElementContainer;
  Const setScopes : TScopes; Var ePosition : TBADIRefactoringInsertPosition) : Integer;

Var
  E: TElementContainer;

Begin
  Result := 0;
  Case RefactoringType Of
    rtConstant: // After a resource string
      Begin
        E := Container.FindElement(strResourceStringsLabel);
        If Assigned(E) Then
          Begin
            Result := NewMaxLinePosition(E, GetScope);
            If Result > 0 Then
              Begin
                ePosition := ripAfter;
                Exit;
              ENd;
          End;         
      End;
    rtResourceString: // Before Constant
      Begin
        E := Container.FindElement(strConstantsLabel);
        If Assigned(E) Then
          Begin
            Result := NewMinLinePosition(E, setScopes);
            If Result < MaxInt Then
              Begin
                ePosition := ripBefore;
                Exit;
              End;
          End;       
      End;
  End;
End;

(**

  This method attempts to find the correct location for the insertion of the declaration.

  @precon  Container must be a valid instance.
  @postcon The new position of the section declaration is returned.

  @param   Container         as a TElementContainer as a constant
  @param   iDefaultStartLine as an Integer as a constant
  @param   setScopes         as a TScopes as a constant
  @return  a TBADIRefactoringInsertionInfo

**)
Function TBADIRefactoringInfo.NewDeclarationPosition(Const Container : TElementContainer;
  Const iDefaultStartLine: Integer; Const setScopes : TScopes): TBADIRefactoringInsertionInfo;

Var
  E: TElementContainer;
  iLine: Integer;

Begin
  Result.FLine := iDefaultStartLine;
  Result.FType := ritCreate;
  iLine := NewDeclarationFromExistingCRS(Container, setScopes, Result.FPosition);
  If (iLine > 0) And (iLine < MaxInt) Then
    Result.FLine := iLine;
  E := Container.FindElement(strVarsLabel); // Before Var
  If Assigned(E) Then
    Begin
      iLine := NewMinLinePosition(E, setScopes);
      If iLine < MaxInt Then
        Begin
          Result.FLine := iLine;
          Result.FPosition := ripBefore;
          Exit;
        End;
    End;       
  E := Container.FindElement(strTypesLabel); // After Type
  If Assigned(E) Then
    Begin
      iLine := NewMaxLinePosition(E, GetScope);
      If iLine > 0 Then
        Begin
          Result.FLine := iLine;
          Result.FPosition := ripAfter;
          Exit;
        End;
    End; 
  iLine := NewDeclarationAfterUses(Container, setScopes);
  If iLine > 0 Then
    Begin
      Result.FLine := iLine;
      Result.FPosition := ripAfter;
    End;
End;

(**

  This method gets the next new line number for the scoped items in the container.

  @precon  Container must be a valid instance.
  @postcon Returns the line number for the new refactoring.

  @param   Container as a TElementContainer as a constant
  @param   eScope    as a TScope as a constant
  @return  an Integer

**)
Function TBADIRefactoringInfo.NewMaxLinePosition(Const Container: TElementContainer;
  Const eScope : TScope): Integer;

Var
  iElement: Integer;
  E : TElementContainer;
  iToken : Integer;

Begin
  Result := 0;
  For iElement := 1 To Container.ElementCount Do
    Begin
      E := Container.Elements[iElement];
      If E.Scope = eScope Then
        Begin
          If E.Line > Result Then
            Result := E.Line;
          If E Is TRecordDecl Then
            If (E As TRecordDecl).EndLine > Result Then
              Result := (E As TRecordDecl).EndLine;
          For iToken := 0 To E.TokenCount - 1 Do
            If (E.Tokens[iToken].Line > 0) And (E.Tokens[iToken].Line > Result) Then
              Result := E.Tokens[iToken].Line;
        End;
    End;
  If Result > 0 Then
    Inc(Result);
End;

(**

  This method gets the previous new line number for the scoped items in the container.

  @precon  Container must be a valid instance.
  @postcon Returns the line number for the new refactoring.

  @param   Container as a TElementContainer as a constant
  @param   setScope  as a TScopes as a constant
  @return  an Integer

**)
Function TBADIRefactoringInfo.NewMinLinePosition(Const Container: TElementContainer;
  Const setScope : TScopes): Integer;

Var
  iElement: Integer;
  E : TElementContainer;
  iToken : Integer;

Begin
  Result := MaxInt;
  For iElement := 1 To Container.ElementCount Do
    Begin
      E := Container.Elements[iElement];
      If E.Scope In setScope Then
        Begin
          If E.Line < Result Then
            Result := E.Line;
          For iToken := 0 To E.TokenCount - 1 Do
            If (E.Tokens[iToken].Line > 0) And (E.Tokens[iToken].Line < Result) Then
              Result := E.Tokens[iToken].Line;
        End;
    End;
  If Result <> MaxInt Then
    Dec(Result);
End;

(**

  This function recurses the implemented methods and returns true if the line number occurs in a method 
  implementation.

  @precon  Container must be valid.
  @postcon Returns true if the line number is in a method implementation.

  @param   Container as a TElementContainer as a constant
  @param   iLine     as an Integer as a constant
  @return  a Boolean

**)
Function TBADIRefactoringInfo.RecurseMethods(Const Container: TElementContainer;
  Const iLine : Integer): Boolean;

Var
  iElement: Integer;
  E: TElementContainer;
  M: TGenericFunction;

Begin
  Result := False;
  For iElement := 1 To Container.ElementCount Do
    Begin
      E := Container.Elements[iElement];
      If E Is TGenericFunction Then
        Begin
          M := E As TGenericFunction;
          Result := (M.StartLine <= iLine) And (M.EndLine >= iLine);
          If Result Then
            FMethod := M
          Else
            Result := RecurseMethods(E, iLine);
        End
      Else
        Result := RecurseMethods(E, iLine);
      If Result Then
        Break;
    End;
End;

(**

  This method inserts the refactoring into the code either at the end of the existing declarations or in 
  a new declaration.

  @precon  None.
  @postcon A new refactoring is added to the code.

  @param   Container    as a TElementContainer as a constant
  @param   eScope       as a TScope as a constant
  @return  a TBADIRefactoringInsertionInfo

**)
Function TBADIRefactoringInfo.RefactorConstResStr(Const Container : TElementContainer;
  Const eScope : TScope) : TBADIRefactoringInsertionInfo;

Var
  CRS : TElementContainer;
  iLine : Integer;

Begin
  Case eScope Of
    scLocal:   iLine := FMethod.StartLine - 1;
    scPrivate: iLine := FImplementation.Line + 1;
    scPublic:  iLine := FImplementation.Line;
  Else
    iLine := 0;
  End;
  Case eScope Of
    scLocal:   Result.FPosition := ripBefore;
    scPrivate: Result.FPosition := ripAfter;
    scPublic:  Result.FPosition := ripBefore;
  Else
    Result.FPosition := ripBefore;
  End;
  CRS := FindCRSElement(Container, iLine);
  If Assigned(CRS) Then
    Begin
      Result.FLine := iLine;
      Result.FType := ritAppend;
      Result.FPosition := ripAfter;
    End Else
    Begin
      Result := NewDeclarationPosition(Container, iLine, [eScope]);
    End;
End;

(**

  This method returns the refactoring type that was returned from the refactoring form in the set.

  @precon  None.
  @postcon The refactoring type selected is returned.

  @return  a TBADIRefactoringType

**)
Function TBADIRefactoringInfo.RefactoringType: TBADIRefactoringType;

Var
  eType: TBADIRefactoringType;

Begin
  Result := rtConstant;
  For eType := Low(TBADIRefactoringType) To High(TBADIRefactoringType) Do
    If eType In FTypes Then
      Begin
        Result := eType;
        Break;
      End;
End;

(**

  This method updates the scope and types based on where the token is in the file.

  @precon  None.
  @postcon The scope and type are updated based on where the token is in the file.

  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Procedure TBADIRefactoringInfo.UpdateScopeAndType(Const iLine, iColumn : Integer);

Const
  strInterfaceKeywordByModuleType : Array[Low(TModuleType)..High(TModuleType)] Of String  = (
    'PROGRAM', 'PACKAGE', 'LIBRARY', 'INTERFACE');
  strDefaultDecimal = 'dblDecimal';
  strDefaultInteger = 'iInteger';
  strStringPrefix = 'str';
  strIMPLEMENTATION = 'IMPLEMENTATION';
  
  (**

    This procedure updates the refactoring types that are available.

    @precon  None.
    @postcon The FTypes field is updated.

  **)
  Procedure UpdateRefactoringTypes;

  Begin
    FTypes := [rtConstant];
    Case FToken.TokenType Of
      ttNumber:
        If Pos('.', FToken.Token) > 0 Then
          FName := strDefaultDecimal
        Else
          FName := strDefaultInteger;
      ttSingleLiteral, ttDoubleLiteral:
        Begin
          FName := strStringPrefix + ExtractText(FToken.Token);
          Include(FTypes, rtResourceString);
        End;
    End;
  End;

  (**

    This procedure updates the refactoring scopes that are available.

    @precon  None.
    @postcon The FScopes field is updated.

  **)
  Procedure UpdateRefactoringScopes;

  Begin
    FScopes := [rsInterface];
    If FModule Is TPascalModule Then
      If (FModule As TPascalModule).ModuleType = mtUnit Then
        If Assigned(FImplementation) Then
          If (FImplementation.Line < iLine) Then
            Include(FScopes, rsImplementation);
  End;
  
ResourceString
  strInsertionExceptionMsg = 'Cannot find insertion point!';

var
  iToken: Integer;
  strKeyWord: String;

Begin
  FindToken(iLine, iColumn);
  FInterface := Nil;
  strKeyWord := strInterfaceKeyWordByModuleType[(FModule As TPascalModule).ModuleType];
  For iToken := 0 To FModule.TokenCount - 1 Do
    If FModule.Tokens[iToken].UToken = strKeyWord Then
      Begin
        FInterface := FModule.Tokens[iToken];
        Break;
      End;
  FImplementation := Nil;
  For iToken := 0 To FModule.TokenCount - 1 Do
    If FModule.Tokens[iToken].UToken = strIMPLEMENTATION Then
      Begin
        FImplementation := FModule.Tokens[iToken];
        Break;
      End;
  If Not Assigned(FImplementation) Then
    FImplementation := FindBeginNotInMethod;
  If Not Assigned(FImplementation) Then
    Raise EBADIParserError.Create(strInsertionExceptionMsg);
  UpdateRefactoringScopes;
  FMethod := Nil;
  If IsInMethod(iLine) Then
    Include(FScopes, rsLocal);
  UpdateRefactoringTypes;
End;

End.
