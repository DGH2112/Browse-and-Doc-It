(**
  
  This module contains cade to refactor a constant from Object Pascal code.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jan 2018

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
  BADI.Refactoring.Functions,
  ToolsAPI;

Type
  (** A class to handle the refactoring of constants in code. **)
  TBADIRefactorConstant = Class
  Strict Private
    FModule          : TBaseLanguageModule;
    FTokenIndex      : Integer;
    FSourceEditor    : IOTASourceEditor;
    FIndent          : Integer;
    FMaxWidth        : Integer;
    FRefactoringInfo : TBADIRefactoringInfo;
  Strict Protected
    Procedure Execute(Const SE: IOTASourceEditor; Const iLine, iColumn: Integer);
    Procedure ParseModule(Const iLine, iColumn: Integer);
    Procedure StartRefactoring(Const iLine, iColumn: Integer);
    Procedure ReplaceLiteralWithRefactoring;
    Function  RefactorLocal : TBADIRefactoringInsertionInfo;
    Function  RefactorImplementation: TBADIRefactoringInsertionInfo;
    Function  RefactorInterface : TBADIRefactoringInsertionInfo;
    Procedure ReplaceToken;
    Function  CheckForExistingDeclaration : Boolean;
    Function  BreakToken(Const iStartCol, iIndent : Integer): String;
  Public
    Constructor Create;
    Class Procedure Refactor(Const SE: IOTASourceEditor; Const iLine, iColumn: Integer);
  End;

Implementation

Uses
  {$IFDEF CODESITE}
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
  BADI.Pascal.RecordDecl,
  Classes;

Const
  (** A constant array for the section keywords for new refactoring declarations. **)
  strSectionKeywords : Array[Low(TBADIRefactoringType)..High(TBADIRefactoringType)] Of String = (
    'Const', 'ResourceString');
    
(**

  This method attempts to breal a long token down into smaller bits.

  @precon  None.
  @postcon If the token is too long a token broken down into bits is returned.

  @param   iStartCol as an Integer as a constant
  @param   iIndent   as an Integer as a constant
  @return  a String

**)
Function TBADIRefactorConstant.BreakToken(Const iStartCol, iIndent : Integer): String;

Const
  strConCat = ''' + ';

  (**

    This method searches backwards from the breapoint to find a more natural point in the text to break
    it up into piece. If one is foudn that position is returned.

    @precon  None.
    @postcon A new breakpoint position is returned.

    @param   strText     as a String as a constant
    @param   iBreakpoint as an Integer as a constant
    @return  an Integer

  **)
  Function FindNaturalBreak(Const strText : String; Const iBreakpoint : Integer) : Integer;

  Const
    strValidBreakPoints = [#32..#47, #58..#64, #94..#96, #123..#255];
    
  Var
    iChar: Integer;

  Begin
    Result := iBreakpoint;
    For iChar := iBreakpoint DownTo 1 Do
      If CharInSet(strText[iChar], strValidBreakPoints) Then
        Begin
          Result := iChar;
          Break;
        End;
  End;

Var
  iBreakPoint : Integer;
  sl : TStringList;
  iLine: Integer;
  
Begin
  iBreakPoint := FMaxWidth - iStartCol - Length(strConCat);
  sl := TStringList.Create;
  Try
    sl.Text := FRefactoringInfo.Token.Token;
    iLine := 0;
    While (iBreakPoint > 0) And (iLine < sl.Count) And (Length(sl[iLine]) > iBreakPoint) Do
      Begin
        iBreakPoint := FindNaturalBreak(sl[iLine], iBreakPoint);
        sl.Add(StringOfChar(#32, FIndent * (iIndent + 1 + 1)) + '''' +
          Copy(sl[iLine], iBreakPoint + 1, Length(sl[iLine]) - iBreakPoint));
        sl[iLine] := Copy(sl[iLine], 1, iBreakPoint) + strConCat;
        iBreakPoint := FMaxWidth - Length(strConCat);
        Inc(iLine);
      End;
    Result := Trim(sl.Text);
  Finally
    sl.Free;
  End;
End;

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
            S := M.FindElement(strSectionNames[FRefactoringInfo.RefactoringType]);
            If Assigned(S) Then
              For iDeclaration := 1 To S.ElementCount Do        
                If FRefactoringInfo.Token.Token = S.Elements[iDeclaration].AsString(False, False) Then
                  Case MessageDlg(Format(strMsg, [FRefactoringInfo.Token.Token]), mtWarning,
                    [mbYes, mbNo, mbCancel], 0) Of
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
  If Assigned(FRefactoringInfo.Method) Then
    Begin
      Container := FModule.FindElement(strImplementedMethodsLabel);
      If Assigned(Container) Then
        Result := RecurseMethods(Container);
    End Else
    Begin
      Container := FModule.FindElement(strSectionNames[FRefactoringInfo.RefactoringType]);
      If Assigned(Container) Then
        For iDeclaration := 1 To Container.ElementCount Do        
          If FRefactoringInfo.Token.Token = Container.Elements[iDeclaration].AsString(False, False) Then
            Case MessageDlg(Format(strMsg, [FRefactoringInfo.Token.Token]), mtWarning,
              [mbYes, mbNo, mbCancel], 0) Of
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
  iMaxWidth = 80;
  
Var
  ES : IOTAEditorServices;

Begin
  FIndent := iDefaultIndent;
  FMaxWidth := iMaxWidth;
  If Supports(BorlandIDEServices, IOTAEditorServices, ES) Then
    Begin
      FIndent := ES.EditOptions.BlockIndent;
      FMaxWidth := ES.EditOptions.BufferOptions.RightMargin;
    End;
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

Begin
  FSourceEditor := SE;
  ParseModule(iLine, iColumn);
End;

(**

  This method parses the module in preparation for refactoring.

  @precon  None.
  @postcon If the modules parses without error then the refactoring is started.

  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Procedure TBADIRefactorConstant.ParseModule(Const iLine, iColumn: Integer);

ResourceString
  strCannotRefactorErrors = 'Cannot refactor as the module has errors!';

Begin
  FModule := TBADIDispatcher.BADIDispatcher.Dispatcher(EditorAsString(FSourceEditor),
    FSourceEditor.FileName, fSourceEditor.Modified, [moParse]);
  Try
    If Not Assigned(FModule.FindElement(strErrors)) Then
      StartRefactoring(iLine, iColumn)
    Else
      MessageDlg(strCannotRefactorErrors, mtError, [mbOK], 0);
  Finally
    FModule.Free;
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

  This method refactors the private implementation constant / resource string in the method.

  @precon  None.
  @postcon The constant or resource strings is created as a private implementation declaration.

  @return  a TBADIRefactoringInsertionInfo

**)
Function TBADIRefactorConstant.RefactorImplementation : TBADIRefactoringInsertionInfo;

Begin
  If Assigned(FRefactoringInfo.ImplementationToken) Then
    Begin
      ReplaceToken;
      Result := FRefactoringInfo.RefactorConstResStr(FModule, scPrivate);
    End;
End;

(**

  This method refactors the public interface constant / resource string in the method.

  @precon  None.
  @postcon The constant or resource strings is created as a public interface declaration.

  @return  a TBADIRefactoringInsertionInfo

**)
Function TBADIRefactorConstant.RefactorInterface: TBADIRefactoringInsertionInfo;

Begin
  If FModule Is TPascalModule Then
    Begin
      If Assigned(FRefactoringInfo.InterfaceToken) Then
        Begin
          ReplaceToken;
          Result := FRefactoringInfo.RefactorConstResStr(FModule, scPublic);
        End;
    End;
End;

(**

  This method refactors the local constant / resource string in the method.

  @precon  None.
  @postcon The constant or resource strings is created as a local declaration.

  @return  a TBADIRefactoringInsertionInfo

**)
Function TBADIRefactorConstant.RefactorLocal: TBADIRefactoringInsertionInfo;

Begin
  ReplaceToken;
  Result := FRefactoringInfo.RefactorConstResStr(FRefactoringInfo.Method, scLocal);
End;

(**

  This method proceeds with the scope and type of refactoring selected.

  @precon  None.
  @postcon The scope and type of refactoring is chosen.

**)
Procedure TBADIRefactorConstant.ReplaceLiteralWithRefactoring;

Const
  strSection = '%*s%s'#13#10;
  strDeclaration = '%*s%s = ';

Var
  iIndex: Integer;
  CP : TOTACharPos;
  RII: TBADIRefactoringInsertionInfo;
  UR: IOTAEditWriter;
  strRefactoring : String;

Begin
  Case FRefactoringInfo.Scope Of
    scLocal: RII := RefactorLocal;
    scPrivate: RII := RefactorImplementation;
    scPublic: RII := RefactorInterface;
  End;
  CP.Line := RII.FLine;
  CP.CharIndex := 0;
  iIndex := FSourceEditor.EditViews[0].CharPosToPos(CP);
  Case FRefactoringInfo.Scope Of
    scLocal: CP.CharIndex := FindIndentOfFirstTokenOnLine(FModule, FRefactoringInfo.Method.Line) - 1;
    scPrivate: CP.CharIndex := FRefactoringInfo.ImplementationToken.Column - 1;
    scPublic: CP.CharIndex := FRefactoringInfo.InterfaceToken.Column - 1;
  End;
  UR := FSourceEditor.CreateUndoableWriter;
  UR.CopyTo(iIndex);
  Case RII.FType Of
    ritAppend:
      Begin
        strRefactoring := Format(strDeclaration, [FIndent + CP.CharIndex, '', FRefactoringInfo.Name]);
        OutputText(UR, strRefactoring + BreakToken(Length(strRefactoring), CP.CharIndex) + ';'#13#10);
      End;
    ritCreate:
      Begin
        If RII.FPosition = ripAfter Then
          If TBADIOptions.BADIOptions.RefactorConstNewLine Then
            OutputText(UR, #13#10);
        OutputText(UR, Format(strSection, [CP.CharIndex, '',
          strSectionKeywords[FRefactoringInfo.RefactoringType]]));
        strRefactoring := Format(strDeclaration, [FIndent + CP.CharIndex, '', FRefactoringInfo.Name]);
        OutputText(UR, strRefactoring + BreakToken(Length(strRefactoring), CP.CharIndex) + ';'#13#10);
        If RII.FPosition = ripBefore Then
          If TBADIOptions.BADIOptions.RefactorConstNewLine Then
            OutputText(UR, #13#10);
      End;
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
  CharPos.Line := FRefactoringInfo.Token.Line;
  CharPos.CharIndex := FRefactoringInfo.Token.Column - 1;
  iIndex := FSourceEditor.EditViews[0].CharPosToPos(CharPos);
  UR := FSourceEditor.CreateUndoableWriter;
  UR.CopyTo(iIndex);
  UR.DeleteTo(iIndex + FRefactoringInfo.Token.Length);
  OutputText(UR, FRefactoringInfo.Name);
End;

(**

  This method invokes the refactoring of the token at the current cursor position.

  @precon  None.
  @postcon The token at the current cursor position is refactored.

  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Procedure TBADIRefactorConstant.StartRefactoring(Const iLine, iColumn: Integer);

ResourceString
  strTokenAtCursorIsNotLliteral = 'The token at the cursor is not a literal number or string!';
  strNoTokenFoundAtCursor = 'No token found at the cursor position!';

Var
  boolNewLine: Boolean;

Begin
  FRefactoringInfo := TBADIRefactoringInfo.Create(FModule);
  Try
    FTokenIndex := FRefactoringInfo.FindToken(iLine, iColumn);
    If FTokenIndex > - 1 Then
      Begin
        FRefactoringInfo.UpdateScopeAndType(iLine, iColumn);
        If FRefactoringInfo.Token.TokenType In [ttNumber, ttSingleLiteral, ttDoubleLiteral] Then
          Begin
            boolNewLine := TBADIOptions.BADIOptions.RefactorConstNewLine;
            If Not CheckForExistingDeclaration Then
              If TfrmBADIRefactorConstant.Execute(FRefactoringInfo, boolNewLine) Then
                Begin
                  TBADIOptions.BADIOptions.RefactorConstNewLine := boolNewLine;
                  ReplaceLiteralWithRefactoring;
                End;
          End Else
            MessageDlg(strTokenAtCursorIsNotLliteral, mtError, [mbOK], 0)
      End
    Else
      MessageDlg(strNoTokenFoundAtCursor, mtError, [mbOK], 0);
  Finally
    FRefactoringInfo.Free;
  End;
End;

End.
