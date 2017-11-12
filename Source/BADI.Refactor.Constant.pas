(**
  
  This module contains cade to refactor a constant from Object Pascal code.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Nov 2017
  
**)
Unit BADI.Refactor.Constant;

Interface

Uses
  BADI.Base.Module,
  BADI.RefactorConstantForm,
  BADI.Generic.FunctionDecl,
  BADI.TokenInfo,
  BADI.ElementContainer,
  BADI.Types,
  ToolsAPI;

Type
  (** A class to handle the refactoring of constants in code. **)
  TBADIRefactorConstant = Class
  Strict Private
    FModule      : TBaseLanguageModule;
    FTokenIndex  : Integer;
    FToken       : TTokenInfo;
    FName        : String;
    FScopes      : TBADIRefactoringScopes;
    FTypes       : TBADIRefactoringTypes;
    FMethod      : TGenericFunction;
    FSourceEditor: IOTASourceEditor;
    FIndent      : Integer;
  Strict Protected
    Procedure Execute(Const SE: IOTASourceEditor; Const iLine, iColumn: Integer);
    Function  FindToken(Const iLine, iColumn: Integer): Integer;
    Procedure ReplaceLiteralWithRefactoring;
    Function  ExtractText(Const strText: String): String;
    Function  IsInMethod(Const iLine: Integer): Boolean;
    Function  RecurseMethods(Const Container: TElementContainer; Const iLine : Integer): Boolean;
    Procedure RefactorLocal;
    Procedure RefactorImplementation;
    Procedure RefactorInterface;
    Procedure RefactorConstResStr(Const Container : TElementContainer;
      Const iDefaultLine, iDeclIndent : Integer; Const eScope : TScope);
    Procedure ReplaceToken;
    Procedure UpdateScopeAndType(Const iLine : Integer);
    Function  RefactoringType : TBADIRefactoringType;
    Function  NewMaxLinePosition(Const Container: TElementContainer; Const setScope : TScopes) : Integer;
    Function  NewMinLinePosition(Const Container: TElementContainer; Const setScope : TScopes) : Integer;
    Function  NewDeclarationPosition(Const Container : TElementContainer;
      Const iDefaultStartLine: Integer; Const setScopes : TScopes): Integer;
    Function  NewDeclarationAfterUses(Const Container : TElementContainer;
      Const setScopes : TScopes) : Integer;
    Function  NewDeclarationFromExistingCRS(Const Container : TElementContainer;
      Const setScopes : TScopes) : Integer;
    Function  CheckForExistingDeclaration : Boolean;
    Function  FindCRSElement(Const Container: TElementContainer; Const eScope : TScope;
      Var iLine : Integer): TElementContainer;
  Public
    Constructor Create;
    Class Procedure Refactor(Const SE: IOTASourceEditor; Const iLine, iColumn: Integer);
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Module.Dispatcher,
  BADI.ToolsAPIUtils,
  Dialogs,
  SysUtils,
  BADI.ResourceStrings,
  BADI.CommonIDEFunctions, 
  BADI.Options, 
  BADI.Pascal.Module,
  Controls, 
  BADI.Functions, 
  BADI.Pascal.RecordDecl;

Const
  (** A constant array of section names to search for when trying to find the correct refactoring
      location. **)
  strSectionNames : Array[Low(TBADIRefactoringType)..High(TBADIRefactoringType)] Of String = (
    strConstantsLabel, strResourceStringsLabel);
  (** A constant array for the section keywords for new refactoring declarations. **)
  strSectionKeywords : Array[Low(TBADIRefactoringType)..High(TBADIRefactoringType)] Of String = (
    'Const', 'ResourceString');
  (** A constant to define the format of a constant / resource string declaration. **)
  strDeclaration = '%*s%s = %s;'#13#10;
    
(**

  This method checks the an existing constant or resource string that has been declared for the same
  literal so that is canbe promoted and reused.
  
  @precon  None.
  @postcon Returns true if an existing declarations is found and refactoring should not be done.

  @return  a Boolean

**)
Function TBADIRefactorConstant.CheckForExistingDeclaration: Boolean;

Const
  strMsg = 'The literal "%s" already existing! Do you want to go to that definition [Yes] or ' +
    'continue refactoring [No]?';

  (**

    This method recurses the implemented methods searching for an existing declaration and returns true
    if one is found.

    @precon  Container must be a valid instance.
    @postcon Returns true if an existing declaration of the literal is found.

    @param   Container as a TElementContainer as a constant
    @return  a Boolean

  **)
  Function RecurseMethods(Const Container : TElementContainer) : Boolean;

  Var
    iElement: Integer;
    E: TElementContainer;
    M: TGenericFunction;
    S: TElementContainer;
    iDeclaration: Integer;

  Begin
    Result := False;
    For iElement := 1 To Container.ElementCount Do
      Begin
        E := Container.Elements[iElement];
        If E Is TGenericFunction Then
          Begin
            M := E As TGenericFunction;
            S := M.FindElement(strSectionNames[RefactoringType]);
            If Assigned(S) Then
              For iDeclaration := 1 To S.ElementCount Do        
                If FToken.Token = S.Elements[iDeclaration].AsString(False, False) Then
                  Case MessageDlg(Format(strMsg, [FToken.Token]), mtWarning, [mbYes, mbNo, mbCancel],
                    0) Of
                    mrYes:
                      Begin
                        Result := True;
                        PositionCursor(M, S.Elements[iDeclaration].Line,
                          S.Elements[iDeclaration].Column, TBADIOptions.BADIOptions.BrowsePosition);                    
                        Break;
                      End;
                    mrNo:
                      Begin
                        Result := False;
                        Break;
                      End;
                    mrCancel: Abort;
                  End;
          End
        Else
          Result := RecurseMethods(E);
        If Result Then
          Break;
      End;
  End;

Var
  Container: TElementContainer;
  iDeclaration: Integer;

Begin
  Result := False;
  If Assigned(FMethod) Then
    Begin
      Container := FModule.FindElement(strImplementedMethodsLabel);
      If Assigned(Container) Then
        Result := RecurseMethods(Container);
    End Else
    Begin
      Container := FModule.FindElement(strSectionNames[RefactoringType]);
      If Assigned(Container) Then
        For iDeclaration := 1 To Container.ElementCount Do        
          If FToken.Token = Container.Elements[iDeclaration].AsString(False, False) Then
            Case MessageDlg(Format(strMsg, [FToken.Token]), mtWarning, [mbYes, mbNo, mbCancel],
              0) Of
              mrYes:
                Begin
                  PositionCursor(Container, Container.Elements[iDeclaration].Line,
                    Container.Elements[iDeclaration].Column, TBADIOptions.BADIOptions.BrowsePosition);                    
                End;
              mrNo:
                Begin
                  Result := False;
                  Break;
                End;
              mrCancel: Abort;
            End;
    End;
End;

(**

  A constructor for the TBADIRefactorConstant class.

  @precon  None.
  @postcon Gets the IDEs blockindent for the refactorings.

**)
Constructor TBADIRefactorConstant.Create;

Const
  iDefaultIndent = 2;
  
Var
  ES : IOTAEditorServices;

Begin
  FIndent := iDefaultIndent;
  If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
    FIndent := ES.EditOptions.BlockIndent;
End;

(**

  This method start the processing of the refactoring by presenting the user with the refactoring
  dialogue and then based on options chosen the refactoring proceeds.

  @precon  SE must be a valid instance.
  @postcon The user is presented with a refactoring dialogue to chose th refactoring and is accepted
           the refactoring proceeds.

  @param   SE      as an IOTASourceEditor as a constant
  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Procedure TBADIRefactorConstant.Execute(Const SE: IOTASourceEditor; Const iLine, iColumn: Integer);

ResourceString
  strTokenAtCursorIsNotLliteral = 'The token at the cursor is not a literal number or string!';
  strNoTokenFoundAtCursor = 'No token found at the cursor position!';
  strCannotRefactorErrors = 'Cannot refactor as the module has errors!';

Var
  boolNewLine: Boolean;

Begin
  FSourceEditor := SE;
  FModule := TBADIDispatcher.BADIDispatcher.Dispatcher(EditorAsString(SE), SE.FileName, SE.Modified,
    [moParse]);
  Try
    If Not Assigned(FModule.FindElement(strErrors)) Then
      Begin
        FTokenIndex := FindToken(iLine, iColumn);
        If FTokenIndex > - 1 Then
          Begin
            FToken := FModule.Tokens[FTokenIndex];
            If FToken.TokenType In [ttNumber, ttSingleLiteral, ttDoubleLiteral] Then
              Begin
                UpdateScopeAndType(iLine);
                boolNewLine := TBADIOptions.BADIOptions.RefactorConstNewLine;
                If Not CheckForExistingDeclaration Then
                  If TfrmBADIRefactorConstant.Execute(FToken.Token, FName, FScopes, FTypes, boolNewLine) Then
                    Begin
                      TBADIOptions.BADIOptions.RefactorConstNewLine := boolNewLine;
                      ReplaceLiteralWithRefactoring;
                    End;
              End
            Else
              MessageDlg(strTokenAtCursorIsNotLliteral, mtError, [mbOK], 0)
          End
        Else
          MessageDlg(strNoTokenFoundAtCursor, mtError, [mbOK], 0);
      End Else
        MessageDlg(strCannotRefactorErrors, mtError, [mbOK], 0);
  Finally
    FModule.Free;
  End;
End;

(**

  This method extracts the valid identifier characters from the given text for the name of the literal.

  @precon  None.
  @postcon The first 63 valid idenitifer characters are extracted from the given string.

  @param   strText as a String as a constant
  @return  a String

**)
Function TBADIRefactorConstant.ExtractText(Const strText: String): String;

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

  This method searches for the constant or resource string section in the given container and returns
  the found section container if there are items in the given scope.

  @precon  Container must be a valid instance.
  @postcon Returns the searched for declaration of there are subitems of the correct scope.

  @param   Container as a TElementContainer as a constant
  @param   eScope    as a TScope as a constant
  @param   iLine     as an Integer as a reference
  @return  a TElementContainer

**)
Function TBADIRefactorConstant.FindCRSElement(Const Container: TElementContainer; Const eScope : TScope;
  Var iLine : Integer): TElementContainer;

Begin
  Result := Container.FindElement(strSectionNames[RefactoringType]);
  If Assigned(Result) Then
    Begin
      iLine := NewMaxLinePosition(Result, [eScope]);
      If iLine = 0 Then
        Result := Nil
      Else
        iLine := 0;
    End;
End;
      

(**

  This method finds a token based on a line number and column number.

  @precon  None.
  @postcon Returns the token index of the token at the given line and column if found else returns -1.

  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant
  @return  an Integer

**)
Function TBADIRefactorConstant.FindToken(Const iLine, iColumn: Integer): Integer;

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

  This method returns true of the given line number is in a method implementation.

  @precon  None.
  @postcon Returns true of the given line number is in a method implementation.

  @param   iLine as an Integer as a constant
  @return  a Boolean

**)
Function TBADIRefactorConstant.IsInMethod(Const iLine: Integer): Boolean;

Var
  IM: TElementContainer;

Begin
  Result := False;
  IM := FModule.FindElement(strImplementedMethodsLabel);
  If Assigned(IM) Then
    Result := RecurseMethods(IM, iLine);
End;

(**

  This method attempts to find a uses clause and return the position a new declaration should appear
  (after the uses clause).

  @precon  Container must be a valid instance.
  @postcon Returns the line number for the new declartion ele returns zero.

  @param   Container as a TElementContainer as a constant
  @param   setScopes as a TScopes as a constant
  @return  an Integer

**)
Function TBADIRefactorConstant.NewDeclarationAfterUses(Const Container : TElementContainer;
  Const setScopes : TScopes): Integer;

Const
  strInterface = 'Interface';
  strImplementation = 'Implementation';

Var
  strSection: String;
  E: TElementContainer;

Begin
  If Not (scLocal In setScopes) Then
    Begin   
      E := Container.FindElement(strUses);
      If Assigned(E) Then
        Begin
          Result := NewMaxLinePosition(E, setScopes);
          If (FModule As TPascalModule).ModuleType = mtUnit Then
            Begin
              strSection := strInterface;
              If setScopes * [scPrivate] <> [] Then
                strSection := strImplementation;
              E := E.FindElement(strSection);
              If Assigned(E) Then
                Begin
                  Result := NewMaxLinePosition(E, setScopes);
                  If Result > 0 Then Exit;
                End;
            End Else
            Begin
              If Result > 0 Then Exit;
            End;
        End;  
    End;
End;

(**

  This method tries to find where the declaration should be if there is an existing constant or resource
  string section.

  @precon  Container must be a valid instance.
  @postcon Returns the line number of the new declaration else 0 or MaxInt.

  @param   Container as a TElementContainer as a constant
  @param   setScopes as a TScopes as a constant
  @return  an Integer

**)
Function TBADIRefactorConstant.NewDeclarationFromExistingCRS(Const Container : TElementContainer;
  Const setScopes : TScopes) : Integer;

Var
  E: TElementContainer;

Begin
  Case RefactoringType Of
    rtConstant: // After a resource string
      Begin
        E := Container.FindElement(strResourceStringsLabel);
        If Assigned(E) Then
          Begin
            Result := NewMaxLinePosition(E, setScopes);
            If Result > 0 Then Exit;
          End;         
      End;
    rtResourceString: // Before Constant
      Begin
        E := Container.FindElement(strConstantsLabel);
        If Assigned(E) Then
          Begin
            Result := NewMinLinePosition(E, setScopes);
            If Result < MaxInt Then Exit;
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
  @return  an Integer

**)
Function TBADIRefactorConstant.NewDeclarationPosition(Const Container : TElementContainer;
  Const iDefaultStartLine: Integer; Const setScopes : TScopes): Integer;

Var
  E: TElementContainer;
  iLine: Integer;

Begin
  Result := iDefaultStartLine;
  iLine := NewDeclarationFromExistingCRS(Container, setScopes);
  If (iLine > 0) And (iLine < MaxInt) Then
    Result := iLine;
  E := Container.FindElement(strVarsLabel); // Before Var
  If Assigned(E) Then
    Begin
      Result := NewMinLinePosition(E, setScopes);
      If Result < MaxInt Then Exit;
    End;       
  E := Container.FindElement(strTypesLabel); // After Type
  If Assigned(E) Then
    Begin
      Result := NewMaxLinePosition(E, setScopes);
      If Result > 0 Then Exit;
    End; 
  iLine := NewDeclarationAfterUses(Container, setScopes);
  If iLine > 0 Then
    Result := iLine;
End;

(**

  This method gets the next new line number for the scoped items in the container.

  @precon  Conatiner must be a valid instance.
  @postcon Returns the line number for the new refactoring.

  @param   Container as a TElementContainer as a constant
  @param   setScope  as a TScopes as a constant
  @return  an Integer

**)
Function TBADIRefactorConstant.NewMaxLinePosition(Const Container: TElementContainer;
  Const setScope : TScopes): Integer;

Var
  iElement: Integer;
  E : TElementContainer;
  iToken : Integer;

Begin
  Result := 0;
  For iElement := 1 To Container.ElementCount Do
    Begin
      E := Container.Elements[iElement];
      If E.Scope In setScope Then
        Begin
          If E.Line > Result Then
            Result := E.Line;
          If E Is TRecordDecl Then
            If (E As TRecordDecl).EndLine > Result Then
              Result := (E As TRecordDecl).EndLine;
          For iToken := 0 To E.TokenCount - 1 Do
            If E.Tokens[iToken].Line > Result Then
              Result := E.Tokens[iToken].Line;
        End;
    End;
  If Result > 0 Then
    Inc(Result);
End;

(**

  This method gets the previous new line number for the scoped items in the container.

  @precon  Conatiner must be a valid instance.
  @postcon Returns the line number for the new refactoring.

  @param   Container as a TElementContainer as a constant
  @param   setScope  as a TScopes as a constant
  @return  an Integer

**)
Function TBADIRefactorConstant.NewMinLinePosition(Const Container: TElementContainer;
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
            If E.Tokens[iToken].Line < Result Then
              Result := E.Tokens[iToken].Line;
        End;
    End;
  If Result <> MaxInt Then
    Begin
      Dec(Result);
      If TBADIOptions.BADIOptions.RefactorConstNewLine Then
        Dec(Result);
    End;
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
Function TBADIRefactorConstant.RecurseMethods(Const Container: TElementContainer;
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

  This method attempts to refactor the literal constant at the cursor position.

  @precon  SE must be a valid instance.
  @postcon Attempts to refactor a constant at the cursor position and if so the constant is replaced with
           a new constant.

  @param   SE      as an IOTASourceEditor as a constant
  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Class Procedure TBADIRefactorConstant.Refactor(Const SE: IOTASourceEditor; Const iLine,
  iColumn: Integer);

Var
  FC: TBADIRefactorConstant;

Begin
  FC := TBADIRefactorConstant.Create;
  Try
    FC.Execute(SE, iLine, iColumn);
  Finally
    FC.Free;
  End;
End;

(**

  This method inserts the refactoring into the code either at the end of thr existing declarations or in 
  a new declaration.

  @precon  None.
  @postcon A new refactoring is added to the code.

  @param   Container    as a TElementContainer as a constant
  @param   iDefaultLine as an Integer as a constant
  @param   iDeclIndent  as an Integer as a constant
  @param   eScope       as a TScope as a constant

**)
Procedure TBADIRefactorConstant.RefactorConstResStr(Const Container : TElementContainer;
  Const iDefaultLine, iDeclIndent : Integer; Const eScope : TScope);

Var
  CRS : TElementContainer;
  CharPos: TOTACharPos;
  iIndex: Integer;
  UR: IOTAEditWriter;
  iLine : Integer;

Begin
  CRS := FindCRSElement(Container, eScope, iLine);
  If Assigned(CRS) Then
    Begin
      CharPos.Line := iLine;
      CharPos.CharIndex := 0;
      iIndex := FSourceEditor.EditViews[0].CharPosToPos(CharPos);
      UR := FSourceEditor.CreateUndoableWriter;
      UR.CopyTo(iIndex);
      OutputText(UR, Format(strDeclaration, [FIndent + (iDeclIndent - 1), '', FName, FToken.Token]));
    End Else
    Begin
      CharPos.Line := NewDeclarationPosition(Container, iDefaultLine, [eScope]) + 1;
      CharPos.CharIndex := 0;
      iIndex := FSourceEditor.EditViews[0].CharPosToPos(CharPos);
      UR := FSourceEditor.CreateUndoableWriter;
      UR.CopyTo(iIndex);
      OutputText(UR, Format('%*s%s'#13#10, [iDeclIndent - 1, '', strSectionKeywords[RefactoringType]]));
      OutputText(UR, Format(strDeclaration, [FIndent + (iDeclIndent - 1), '', FName, FToken.Token]));
      If TBADIOptions.BADIOptions.RefactorConstNewLine Then
        OutputText(UR, #13#10);
    End;
End;

(**

  This method refactors the private implementation constant / resource string in the method.

  @precon  None.
  @postcon The constant or resource strings is created as a private implementation declaration.

**)
Procedure TBADIRefactorConstant.RefactorImplementation;

Const
  strIMPLEMENTATION = 'IMPLEMENTATION';

Var
  iToken: Integer;
  iLine: Integer;
  iDeclIndent: Integer;

Begin
  iLine := 0;
  For iToken := 0 To FModule.TokenCount - 1 Do
    If FModule.Tokens[iToken].UToken = strIMPLEMENTATION Then
      Begin
        iLine := FModule.Tokens[iToken].Line;
        Break;
      End;
  If iLine > 0 Then
    Begin
      iDeclIndent := FindIndentOfFirstTokenOnLine(FModule, iLine);
      Inc(iLine);
      If TBADIOptions.BADIOptions.RefactorConstNewLine Then
        Inc(iLine);
      ReplaceToken;
      RefactorConstResStr(FModule, iLine, iDeclIndent, scPrivate);
    End;
End;

(**

  This method returns the refactoring type that was returned from the refactoring form in the set.

  @precon  None.
  @postcon The refcatoring type selected is returned.

  @return  a TBADIRefactoringType

**)
Function TBADIRefactorConstant.RefactoringType: TBADIRefactoringType;

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

  This method refactors the public interface constant / resource string in the method.

  @precon  None.
  @postcon The constant or resource strings is created as a public interface declaration.

**)
Procedure TBADIRefactorConstant.RefactorInterface;

Const
  strInterfaceKeywordByModuleType : Array[Low(TModuleType)..High(TModuleType)] Of String  = (
    'PROGRAM', 'PACKAGE', 'LIBRARY', 'INTERFACE');

Var
  iToken: Integer;
  iLine: Integer;
  iDeclIndent: Integer;
  strKeyWord: String;

Begin
  iLine := 0;
  If FModule Is TPascalModule Then
    Begin
      strKeyWord := strInterfaceKeyWordByModuleType[(FModule As TPascalModule).ModuleType];
      For iToken := 0 To FModule.TokenCount - 1 Do
        If FModule.Tokens[iToken].UToken = strKeyWord Then
          Begin
            iLine := FModule.Tokens[iToken].Line;
            Break;
          End;
      If iLine > 0 Then
        Begin
          iDeclIndent := FindIndentOfFirstTokenOnLine(FModule, iLine);
          Inc(iLine);
          If TBADIOptions.BADIOptions.RefactorConstNewLine Then
            Inc(iLine);
          ReplaceToken;
          RefactorConstResStr(FModule, iLine, iDeclIndent, scPublic);
        End;
    End;
End;

(**

  This method refactors the local constant / resource string in the method.

  @precon  None.
  @postcon The constant or resource strings is created as a local declaration.

**)
Procedure TBADIRefactorConstant.RefactorLocal;

Var
  iMethodColumn: Integer;
  iLine : Integer;

Begin
  ReplaceToken;
  iMethodColumn := FindIndentOfFirstTokenOnLine(FModule, FMethod.Line);
  iLine := FMethod.StartLine - 1;
  If TBADIOptions.BADIOptions.RefactorConstNewLine Then
    Dec(iLine);
  RefactorConstResStr(FMethod, iLine, iMethodColumn, scLocal);
End;

(**

  This method proceeds with the scope and type of refactoring selected.

  @precon  None.
  @postcon The scope and type of refactoring is chosen.

**)
Procedure TBADIRefactorConstant.ReplaceLiteralWithRefactoring;

Var
  eScope : TBADIRefactoringScope;
  
Begin
  For eScope := Low(TBADIRefactoringScope) To High(TBADIRefactoringScope) Do
    If eScope In FScopes Then
      Case eScope Of
        rsLocal: RefactorLocal;
        rsImplementation: RefactorImplementation;
        rsInterface: RefactorInterface;
      End;
End;

(**

  This method replaces the token at the current cursor position with the identifier name for the 
  refactoring.

  @precon  None.
  @postcon The literal token at the cursor is replaced with the refactoring identifier name.

**)
Procedure TBADIRefactorConstant.ReplaceToken;

Var
  CharPos: TOTACharPos;
  UR: IOTAEditWriter;
  iIndex: Integer;

Begin
  CharPos.Line := FToken.Line;
  CharPos.CharIndex := FToken.Column - 1;
  iIndex := FSourceEditor.EditViews[0].CharPosToPos(CharPos);
  UR := FSourceEditor.CreateUndoableWriter;
  UR.CopyTo(iIndex);
  UR.DeleteTo(iIndex + FToken.Length);
  OutputText(UR, FName);
End;

(**

  This method updates the scope and types based on where the token is in the file.

  @precon  None.
  @postcon The scope and type are updated based on where the token is in the file.

  @param   iLine as an Integer as a constant

**)
Procedure TBADIRefactorConstant.UpdateScopeAndType(Const iLine : Integer);

Const
  strDefaultDecimal = 'dblDecimal';
  strDefaultInteger = 'iInteger';
  strStringPrefix = 'str';

Begin
  FScopes := [rsInterface];
  If FModule Is TPascalModule Then
    If (FModule As TPascalModule).ModuleType = mtUnit Then
      Include(FScopes, rsImplementation);
  FMethod := Nil;
  If IsInMethod(iLine) Then
    Include(FScopes, rsLocal);
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
      ENd;
  End;
End;

End.
