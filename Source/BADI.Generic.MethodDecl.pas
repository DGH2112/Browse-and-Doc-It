(**

  This module contains a class that represents a generic method declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.Generic.MethodDecl;

Interface

Uses
  Classes,
  BADI.Generic.FunctionDecl,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represents a method declaration. **)
  TGenericMethodDecl = Class {$IFDEF D2005} Abstract {$ENDIF} (TGenericFunction)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FMethodType: TMethodType;
    FClassNames: TStringList;
    FMsg: String;
    FExt: String;
    FClassMethod: Boolean;
    FAlias: String;
    FForwardDecl: Boolean;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure SetMsg(Const Value: String);
    Procedure SetExt(Const Value: String);
    Function GetQualifiedName: String; Override;
    Function RequiresReturn: Boolean; Override;
    Function FunctionType: String; Override;
    Procedure CheckMethodDocumentation;
    Procedure CheckMethodParamCount;
    Procedure CheckMethodParameters;
    Procedure CheckMethodReturns;
  Public
    Constructor Create(MethodType: TMethodType; const strName: String; AScope: TScope;
      iLine, iCol: Integer); ReIntroduce; Virtual;
    Destructor Destroy; Override;
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
    (**
      Returns the methods types, procedure, function, constructor, destructor.
      @precon  None.
      @postcon Returns the methods types, procedure, function, constructor, destructor.
      @return  a TMethodType
    **)
    Property MethodType: TMethodType Read FMethodType;
    (**
      Returns the methods class name.
      @precon  None.
      @postcon Returns the methods class name.
      @return  a TStringList
    **)
    Property ClassNames: TStringList Read FClassNames;
    (**
      Returns the associated message for the method if the method is a message
      @precon  None.
      @postcon Returns the associated message for the method if the method is a message
      handler.
      @return  a String
    **)
    Property Msg: String Read FMsg Write SetMsg;
    (**
      Returns the external reference for the method if there is one.
      @precon  None.
      @postcon Returns the external reference for the method if there is one.
      @return  a String
    **)
    Property Ext: String Read FExt Write SetExt;
    (**
      Return whether the method is a class method.
      @precon  None.
      @postcon Return whether the method is a class method.
      @return  a Boolean
    **)
    Property ClassMethod: Boolean Read FClassMethod Write FClassMethod;
    (**
      Returns the method alias name.
      @precon  None.
      @postcon Returns the method alias name.
      @return  a String
    **)
    Property Alias: String Read FAlias Write FAlias;
    (**
      This property returns whether the method is a forward declaration or not.
      @precon  None.
      @postcon Returns whether the method is a forward declaration or not.
      @return  a Boolean
    **)
    Property ForwardDecl: Boolean Read FForwardDecl Write FForwardDecl;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.Functions;

(**

  This method checks the method passed against the method comments tags and
  highlights missing parameter comments, return tags and missing descriptions.

  @precon  Method is the method declaration that requires checking for document
           conflicts.
  @postcon The passed method is systematicaly check for errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericMethodDecl.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If (Not FForwardDecl) And (Identifier <> '') Then
    Begin
      CheckMethodDocumentation;
      If doShowMethodMissingDocs In BrowseAndDocItOptions.Options Then
        If Comment <> Nil Then
          Begin
            CheckMethodParamCount;
            CheckMethodParameters;
            CheckMethodReturns;
          End;
      Inherited CheckDocumentation(boolCascade);
    End;
End;

(**

  This method check the given method for general document problems, i.e.
  missing or no description.

  @precon  Method is valid method declaration to be checked for documentation.
  @postcon Checks the passed method for docuemntation errors.

**)
Procedure TGenericMethodDecl.CheckMethodDocumentation;

Begin
  If doShowMethodMissingDocs In BrowseAndDocItOptions.Options Then
    Begin
      If Comment = Nil Then
        Begin
          AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
            Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumented]);
          Exit;
        End;
      If Comment.TokenCount = 0 Then
        AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
          Format(strFunctionDocumentation, [FunctionType]), DocConflictTable[dctFunctionHasNoDesc]);
    End;
End;

(**

  This method checks the given method for the correct number of parameters and
  tags.

  @precon  Method is a method declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed method for errors in the parameter count
           documentation.

**)
Procedure TGenericMethodDecl.CheckMethodParamCount;

Var
  i, j, k: Integer;
  boolMissing: Boolean;

Begin
  j := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'param' Then
      Inc(j);
  k := 0;
  boolMissing := True;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'precon' Then
      Begin
        Inc(k);
        boolMissing := boolMissing And (Comment.Tag[i].TokenCount = 0);
      End;
  If doShowMethodMissingPreCons In BrowseAndDocItOptions.Options Then
    If boolMissing Then
      AddDocumentConflict([FunctionType, QualifiedName], Comment.Line, Comment.Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionPreconNotDocumented]);
  If doShowMethodDiffParamCount In BrowseAndDocItOptions.Options Then
    If (ParameterCount <> j) Then
      AddDocumentConflict([FunctionType, QualifiedName, ParameterCount, j], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionDiffParamCount]);
  If doShowMethodMissingPreCons In BrowseAndDocItOptions.Options Then
    If k < 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPreCon]);
  If doShowMethodMissingPreCons In BrowseAndDocItOptions.Options Then
    If k > 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPrecons]);
End;

(**

  This method checks the given method for the correct parameter tags and
  pre conditions.

  @precon  Method is a method declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed method for errors in the parameter documentation.

**)
Procedure TGenericMethodDecl.CheckMethodParameters;

Var
  i, j: Integer;
  iFound: Integer;
  strType: String;
  strParam: String;

Begin
  For i := 0 To ParameterCount - 1 Do
    Begin
      // Parameter name
      iFound := -1;
      With Comment Do
        For j := 0 To TagCount - 1 Do
          If (LowerCase(Tag[j].TagName) = 'param') And (Tag[j].TokenCount > 0) And
            (LowerCase(Tag[j].Tokens[0].Token) = LowerCase(Parameters[i].Identifier)) Then
            Begin
              iFound := j;
              Break;
            End;
      If doShowMethodUndocumentedParams In BrowseAndDocItOptions.Options Then
        If iFound = -1 Then
          AddDocumentConflict([Parameters[i].Identifier, FunctionType, QualifiedName], Line, Column,
            Comment, Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumentedParam]);
      // Parameter type
      If iFound > -1 Then
        With Comment Do
          Begin
            strType := '';
            For j := 6 To Tag[iFound].TokenCount - 1 Do
              Begin
                If (Tag[iFound].Tokens[j].TokenType In [ttSymbol]) And
                  (Tag[iFound].Tokens[j].Token <> '.') Then
                  Break;
                strType := strType + Tag[iFound].Tokens[j].Token;
              End;
            strType := Trim(strType);
            strParam := BuildLangIndepRep(Parameters[i]);
            If doShowMethodIncorrectParamType In BrowseAndDocItOptions.Options Then
              If CompareText(strType, strParam) <> 0 Then
                AddDocumentConflict([Parameters[i].Identifier, FunctionType, QualifiedName,
                  strParam], Tag[iFound].Line, Tag[iFound].Column, Comment,
                  Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectParamType]);
          End;
    End;
End;

(**

  This method checks the given method for the correct return information and
  tags.

  @precon  Method is a method declaration that needs the be check for document
           conflicts.
  @postcon The passed method return is checked for errors.

**)
Procedure TGenericMethodDecl.CheckMethodReturns;

Var
  i, iNumOfPostCons: Integer;
  iReturnTagIndex: Integer;
  strType: String;
  strReturn: String;

Begin
  iReturnTagIndex := Comment.FindTag('return');
  iNumOfPostCons := 0;
  For i := 0 To Comment.TagCount - 1 Do
    If LowerCase(Comment.Tag[i].TagName) = 'postcon' Then
      Begin
        Inc(iNumOfPostCons);
        If doShowMethodMissingPostCons In BrowseAndDocItOptions.Options Then
          If Comment.Tag[i].TokenCount = 0 Then
            AddDocumentConflict([FunctionType, QualifiedName], Comment.Tag[i].Line,
              Comment.Tag[i].Column, Comment, Format(strFunctionDocumentation, [FunctionType]),
              DocConflictTable[dctFunctionPostconNotDocumented]);
      End;
  If RequiresReturn Then
    Begin;
      If iReturnTagIndex = -1 Then
        Begin
          If doShowMethodUndocumentedReturn In BrowseAndDocItOptions.Options Then
            AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
              Format(strFunctionDocumentation, [FunctionType]),
              DocConflictTable[dctFunctionUndocumentedReturn])
        End
      Else
        Begin
          If doShowMethodIncorrectReturnType In BrowseAndDocItOptions.Options Then
            Begin
              strType := '';
              strReturn := '';
              For i := 2 To Comment.Tag[iReturnTagIndex].TokenCount - 1 Do
                Begin
                  If (Comment.Tag[iReturnTagIndex].Tokens[i].TokenType In [ttSymbol]) And
                    (Comment.Tag[iReturnTagIndex].Tokens[i].Token <> '.') Then
                    Break;
                  strType := strType + Comment.Tag[iReturnTagIndex].Tokens[i].Token;
                End;
              strType := Trim(strType);
              If ReturnType.ElementCount > 0 Then
                strReturn := ReturnType.AsString(False, False);
              If CompareText(strReturn, strType) <> 0 Then
                AddDocumentConflict([FunctionType, QualifiedName, strReturn],
                  Comment.Tag[iReturnTagIndex].Line, Comment.Tag[iReturnTagIndex].Column, Comment,
                  Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectReturntype]);
            End;
        End;
    End
  Else If Comment.FindTag('return') >= 0 Then
    AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
      Format(strFunctionDocumentation, [FunctionType]),
      DocConflictTable[dctFunctionReturnNotRequired]);
  If doShowMethodMissingPostCons In BrowseAndDocItOptions.Options Then
    If iNumOfPostCons = 0 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPostCon]);
  If doShowMethodMissingPostCons In BrowseAndDocItOptions.Options Then
    If (iNumOfPostCons > 1) And (iReturnTagIndex <> -1) Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Comment,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPostCons]);
End;

(**

  This is the constructor for the TMethodDecl class. It initialises the method
  type, scope and line and col information. If also creates a colection to
  store the parameter objects and a string list for the method directives.

  @precon  MethodType is an enumerate indocating the type of the method, Scope
           is the scope of the method, iLine is the line number of the method,
           and iCol is the column number of the method.
  @postcon It initialises the method type, scope and line and col information.

  @param   MethodType as a TMethodType
  @param   strName    as a String as a constant
  @param   AScope     as a TScope
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
Constructor TGenericMethodDecl.Create(MethodType: TMethodType; const strName: String; AScope: TScope;
  iLine, iCol: Integer);

Var
  AImageIndex: TBADIImageIndex;

Begin
  Case MethodType Of
    mtConstructor:
      AImageIndex := iiPublicConstructor;
    mtDestructor:
      AImageIndex := iiPublicDestructor;
    mtProcedure:
      AImageIndex := iiPublicProcedure;
    mtFunction:
      AImageIndex := iiPublicFunction;
  Else
    AImageIndex := iiPublicProcedure;
  End;
  Inherited Create(strName, AScope, iLine, iCol, AImageIndex, Nil);
  FAlias := '';
  FClassMethod := False;
  FClassNames := TStringList.Create;
  Comment := Nil;
  FExt := '';
  FMsg := '';
  FMethodType := MethodType;
  FForwardDecl := False;
End;

(**

  This is the destructor method for the TMethodDecl class. It frees the
  parameters collection, the parameter and the directives.

  @precon  None.
  @postcon It frees the parameters collection, the parameter and the directives.

**)
Destructor TGenericMethodDecl.Destroy;

Begin
  FClassNames.Free;
  Inherited Destroy;
End;

(**

  This method returns the function type of 'Method' for the documentation
  of problems with methods.

  @precon  None.
  @postcon Returns the function type of 'Method' for the documentation
           of problems with methods.

  @return  a String

**)
Function TGenericMethodDecl.FunctionType: String;
Begin
  Result := 'Method';
End;

(**

  This method returns a fully qualified name for the method.

  @precon  None.
  @postcon Returns a fully qualified name for the method.

  @return  a String

**)
Function TGenericMethodDecl.GetQualifiedName: String;

Var
  i: Integer;

Begin
  Result := '';
  For i := 0 To FClassNames.Count - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + '.';
      Result := Result + FClassNames[i];
    End;
  If Result <> '' Then
    Result := Result + '.';
  Result := Result + Identifier;
End;

(**

  This method returns true for functions only.

  @precon  None.
  @postcon Returns true for function only.

  @return  a Boolean

**)
Function TGenericMethodDecl.RequiresReturn: Boolean;

Begin
  Result := MethodType = mtFunction;
End;

(**

  This is a setter method for the Msg property.

  @precon  Value is the new value to assign to the Msg property.
  @postcon Sets the Message property for the method.

  @param   Value as a String as a constant

**)
Procedure TGenericMethodDecl.SetMsg(Const Value: String);
Begin
  If FMsg <> Value Then
    FMsg := Value;
End;

(**

  This is a setter method for the Ext property.

  @precon  Value is the new value to assign to the Ext property.
  @postcon Setst the extension property for the method.

  @param   Value as a String as a constant

**)
Procedure TGenericMethodDecl.SetExt(Const Value: String);
Begin
  If FExt <> Value Then
    FExt := Value;
End;

End.
