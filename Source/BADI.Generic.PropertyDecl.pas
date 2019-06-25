(**

  This module contains a class to represent a generic property declaration.

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
Unit BADI.Generic.PropertyDecl;

Interface

Uses
  BADI.Generic.FunctionDecl;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class that represents properties of a class or interface. **)
  TGenericProperty = Class {$IFDEF D2005} Abstract {$ENDIF} (TGenericFunction)
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetQualifiedName: String; Override;
    Function RequiresReturn: Boolean; Override;
    Function FunctionType: String; Override;
    Procedure CheckPropertyDocumentation;
    Procedure CheckPropertyParamCount;
    Procedure CheckPropertyParameters;
    Procedure CheckPropertyReturns;
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
  End;

Implementation

Uses
  SysUtils,
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.Functions;

(**

  This method checks the property passed against the property comments tags and
  highlights missing parameter comments, return tags and missing descriptions.

  @precon  Method is the property declaration that requires checking for document
           conflicts.
  @postcon The passed property is systematicaly check for errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericProperty.CheckDocumentation(Var boolCascade: Boolean);
Begin
  If Identifier <> '' Then
    Begin
      CheckPropertyDocumentation;
      If doShowMethodMissingDocs In BADIOptions.Options Then
        If Comment <> Nil Then
          Begin
            CheckPropertyParamCount;
            CheckPropertyParameters;
            CheckPropertyReturns;
          End;
      Inherited CheckDocumentation(boolCascade);
    End;
End;

(**

  This method check the given property for general document problems, i.e.
  missing or no description.

  @precon  Method is valid property declaration to be checked for documentation.
  @postcon Checks the passed property for docuemntation errors.

**)
Procedure TGenericProperty.CheckPropertyDocumentation;
Begin
  If doShowPropertyMissingDoc In BADIOptions.Options Then
    Begin
      If Comment = Nil Then
        Begin
          AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
            Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumented]);
          Exit;
        End;
      If Comment.TokenCount = 0 Then
        AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
          Format(strFunctionDocumentation, [FunctionType]), DocConflictTable[dctFunctionHasNoDesc]);
    End;
End;

(**

  This method checks the given property for the correct number of parameters and
  tags.

  @precon  Method is a property declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed property for errors in the parameter count
           documentation.

**)
Procedure TGenericProperty.CheckPropertyParamCount;

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
  If doShowPropertyMissingPreCons In BADIOptions.Options Then
    If boolMissing Then
      AddDocumentConflict([FunctionType, QualifiedName], Comment.Line, Comment.Column, Self,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionPreconNotDocumented]);
  If doShowPropertyDiffPropParamCount In BADIOptions.Options Then
    If (ParameterCount <> j) Then
      AddDocumentConflict([FunctionType, QualifiedName, ParameterCount, j], Line, Column, Self,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionDiffParamCount]);
  If doShowPropertyMissingPreCons In BADIOptions.Options Then
    If k < 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPreCon]);
  If doShowPropertyMissingPreCons In BADIOptions.Options Then
    If k > 1 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPrecons]);
End;

(**

  This method checks the given Property for the correct parameter tags and
  pre conditions.

  @precon  Method is a property declaration that needs the be check for document
           conflicts.
  @postcon Checks the passed method for errors in the parameter documentation.

**)
Procedure TGenericProperty.CheckPropertyParameters;

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
      For j := 0 To Comment.TagCount - 1 Do
        If (LowerCase(Comment.Tag[j].TagName) = 'param') And (Comment.Tag[j].TokenCount > 0) And
          (LowerCase(Comment.Tag[j].Tokens[0].Token) = LowerCase(Parameters[i].Identifier)) Then
          Begin
            iFound := j;
            Break;
          End;
      If doShowPropertyUndocumentedParams In BADIOptions.Options Then
        If iFound = -1 Then
          AddDocumentConflict([Parameters[i].Identifier, FunctionType, QualifiedName], Line, Column,
            Self, Format(strFunctionDocumentation, [FunctionType]),
            DocConflictTable[dctFunctionUndocumentedParam]);
      // Parameter type
      If iFound > -1 Then
        Begin
          strType := '';
          For j := 6 To Comment.Tag[iFound].TokenCount - 1 Do
            Begin
              If (Comment.Tag[iFound].Tokens[j].TokenType In [ttSymbol]) And
                (Comment.Tag[iFound].Tokens[j].Token <> '.') Then
                Break;
              strType := strType + Comment.Tag[iFound].Tokens[j].Token;
            End;
          strType := Trim(strType);
          strParam := BuildLangIndepRep(Parameters[i]);
          If doShowPropertyIncorrectParamType In BADIOptions.Options Then
            If CompareText(strType, strParam) <> 0 Then
              AddDocumentConflict([Parameters[i].Identifier, FunctionType, QualifiedName,
                strParam], Comment.Tag[iFound].Line, Comment.Tag[iFound].Column, Self,
                Format(strFunctionDocumentation, [FunctionType]),
                DocConflictTable[dctFunctionIncorrectParamType]);
        End;
    End;
End;

(**

  This method checks the given property for the correct return information and
  tags.

  @precon  Method is a property declaration that needs the be check for
           document conflicts.
  @postcon The passed method return is checked for errors.

**)
Procedure TGenericProperty.CheckPropertyReturns;

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
        If doShowPropertyMissingPostCons In BADIOptions.Options Then
          If Comment.Tag[i].TokenCount = 0 Then
            AddDocumentConflict([FunctionType, QualifiedName], Comment.Tag[i].Line,
              Comment.Tag[i].Column, Self, Format(strFunctionDocumentation, [FunctionType]),
              DocConflictTable[dctFunctionPostconNotDocumented]);
      End;
  If RequiresReturn Then
    Begin;
      If iReturnTagIndex = -1 Then
        Begin
          If doShowPropertyUndocumentedReturn In BADIOptions.Options Then
            AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
              Format(strFunctionDocumentation, [FunctionType]),
              DocConflictTable[dctFunctionUndocumentedReturn])
        End
      Else
        Begin
          If doShowPropertyIncorrectReturnType In BADIOptions.Options Then
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
                  Comment.Tag[iReturnTagIndex].Line, Comment.Tag[iReturnTagIndex].Column, Self,
                  Format(strFunctionDocumentation, [FunctionType]),
                  DocConflictTable[dctFunctionIncorrectReturntype]);
            End;
        End;
    End
  Else If Comment.FindTag('return') >= 0 Then
    AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
      Format(strFunctionDocumentation, [FunctionType]),
      DocConflictTable[dctFunctionReturnNotRequired]);
  If doShowPropertyMissingPostCons In BADIOptions.Options Then
    If iNumOfPostCons = 0 Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionMissingPostCon]);
  If doShowPropertyMissingPostCons In BADIOptions.Options Then
    If (iNumOfPostCons > 1) And (iReturnTagIndex > -1) Then
      AddDocumentConflict([FunctionType, QualifiedName], Line, Column, Self,
        Format(strFunctionDocumentation, [FunctionType]),
        DocConflictTable[dctFunctionTooManyPostCons]);
End;

(**

  This method returns the function type of 'Property' for the documentation
  of problems with methods.

  @precon  None.
  @postcon Returns the function type of 'Property' for the documentation
           of problems with methods.

  @return  a String

**)
Function TGenericProperty.FunctionType: String;
Begin
  Result := 'Property';
End;

(**

  This is a getter method for the QualifiedName property.

  @precon  None.
  @postcon Returns the qualified name of the property.

  @return  a String

**)
Function TGenericProperty.GetQualifiedName: String;
Begin
  Result := Identifier;
End;

(**

  This method returns true as all properties require return types.

  @precon  None.
  @postcon Returns true.

  @return  a Boolean

**)
Function TGenericProperty.RequiresReturn: Boolean;

Begin
  Result := ReturnType.ElementCount > 0;
End;

End.
