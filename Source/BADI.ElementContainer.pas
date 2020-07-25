(**

  This module contains classes to represent an abstract element container (ancestor for all things)
  and a label container for tree view headers and the like.

  @Author  David Hoyle
  @Version 2.170
  @Date    25 Jul 2020

  @nospelling noXxxxx

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
Unit BADI.ElementContainer;

Interface

Uses
  Classes,
  Contnrs,
  BADI.Interfaces,
  BADI.Comment,
  BADI.Types,
  BADI.Base.Container,
  BADI.TokenInfo;

{$INCLUDE CompilerDefinitions.inc}

Type
  TLabelContainer = Class;

  (** An enumerate to define the result of adding a check or metric. **)
  TBADIIssueState = (isAdded, isOverride, isDisabled);

  (** This class implements the IElementCollection interface so that this
      element container can be rendered with the module browser. **)
  TElementContainer = Class Abstract (TBADIBaseContainer)
  Strict Private
    FElements : TObjectList;
    FComment : TComment;
    FImageIndex : TBADIImageIndex;
    FSorted  : Boolean;
    FReferenced : Boolean;
    FParent : TElementContainer;
    FBADIOptions : IBADIOptions;
  Strict Protected
    Function  GetElementCount : Integer;
    Function  GetElements(Const iIndex : Integer) : TElementContainer;
    Function  GetImageIndexAdjustedForScope : Integer;
    Function  Find(Const strName : String; Const FindType : TFindType = ftName) : Integer;
    Procedure SetSorted(Const boolValue : Boolean);
    Function  FindRoot : TElementContainer;
    Function  CheckCommentForNoMetric(Const eMetric : TBADIModuleMetric;
      Const Element: TElementContainer): Boolean;
    Function  CheckCommentForNoCheck(Const eCheck : TBADIModuleCheck;
      Const Element: TElementContainer): Boolean;
    Function  CheckCommentForNoEWH(Const strEWH: String; Const strParameters : Array of Const;
      Const Element: TElementContainer): Boolean;
    Function  CheckCommentForNoSpelling(Const Comment: TComment;
  Const strIdentifier : String): Boolean;
    Function  CheckIdentifier(Const Container: TElementContainer; Const strIdentifier,
      strTagName: String): Boolean;
    Function  DocConflictImage(Const DocConflictRec: TDocConflictTable): TBADIImageIndex;
    Procedure ModuleMetricPosition(Const Container : TElementContainer; Var iL, iC : Integer);
    Function  ModuleMetricImage(Const eMetric : TBADIModuleMetric): TBADIImageIndex;
    Function  ModuleCheckImage(Const eCheck : TBADIModuleCheck): TBADIImageIndex;
    Procedure DocConflictPosition(Var iL: Integer; Var iC: Integer;
      Const Container: TElementContainer);
    Function  CheckEWHOptions (Const ErrorType : TErrorType) : Boolean;
    Function  CheckForNoEWH(Const ErrorType : TErrorType; Const strParameters : Array of Const;
      Const Container: TElementContainer): Boolean;
    Function  AddCategory(Const Container: TElementContainer; Const strCategory : String;
      Const iImageIndex : TBADIImageIndex): TElementContainer;
    Function  AddRootContainer(Const Container: TElementContainer; Const strLabelText : String;
      Const iImageIndex : TBADIImageIndex): TElementContainer;
    Function  CheckForNoDocumentation (Const Container : TElementContainer) : Boolean;
    (**
      This property provide all descendant modules with a single point of access to the options.
      @precon  None.
      @postcon Returns a reference to the BADI Options class.
      @return  a IBADIOptions
    **)
    Property BADIOptions : IBADIOptions Read FBADIOptions;
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); 
        Reintroduce; overload; Virtual;
    Destructor Destroy; Override;
    Function  Add(Const AElement : TElementContainer) : TElementContainer; Overload; Virtual;
    Function  Add(Const Token : TTokenInfo; Const AScope : TScope; Const AImageIndex : TBADIImageIndex;
      Const AComment : TComment) : TElementContainer; Overload; Virtual;
    Function  Add(Const strToken : String; Const AImageIndex : TBADIImageIndex;
      Const AScope : TScope; Const iLine : Integer = 0; Const iColumn : Integer = 0;
      Const AComment : TComment = Nil) : TElementContainer; Overload; Virtual;
    Function  AddUnique(Const AElement : TElementContainer) : TElementContainer; Virtual;
    Procedure AddTokens(Const AElement : TElementContainer); Virtual;
    Function  FindElement(Const strName : String;
      Const FindType : TFindType = ftName) : TElementContainer;
    Procedure Assign(Const Source : TElementContainer); Virtual;
    Function  FindToken(Const strToken : String) : Integer;
    Procedure DeleteElement(Const iIndex : Integer);
    Procedure CheckDocumentation(Var boolCascade : Boolean); Virtual;
    Function  ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Virtual;
    Procedure AddIssue(Const strMsg: String; Const AScope: TScope; Const iLine, iCol: Integer;
      Const ErrorType: TErrorType; Const Container : TElementContainer); Overload;
    Procedure AddIssue(Const strFmtMsg: String; Const strParameters : Array of Const;
      Const AScope: TScope; Const iLine, iCol: Integer; Const ErrorType: TErrorType;
      Const Container : TElementContainer); Overload;
    Procedure AddDocumentConflict(Const Args: Array Of Const; Const iIdentLine, iIdentColumn: Integer;
      Const Container: TElementContainer; Const strCategory: String;
      Const DocConflictRec: TDocConflictTable);
    Function AddCheck(Const Args: Array of Const; Const iLine, iColumn : Integer;
      Const Container : TElementContainer; Const eCheck : TBADIModuleCheck) : TBADIIssueState;
    Function AddMetric(Const Args: Array of Const; Const iLine, iColumn : Integer;
      Const Container : TElementContainer; Const eMetric : TBADIModuleMetric) : TBADIIssueState;
    Function AddSpelling(Const strWord, strCategory : String; Const AScope : TScope;Const iWordLine,
      iWordColumn: Integer; Const Comment: TComment) : TBADIIssueState;
    Function  AsString(Const boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Virtual; Abstract;
    Procedure CheckReferences; Virtual;
    Function  ReferenceSection(Const AToken : TTokenInfo; Const Section: TLabelContainer) : Boolean;
    Function  ModuleFileName : String;
    (**
      This property returns the number of elements in the collection.
      @precon  None.
      @postcon Returns the number of elements in the collection.
      @return  an Integer
    **)
    Property ElementCount : Integer Read GetElementCount;
    (**
      This property returns an instance of the indexed element from the
      collection.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed element from the collection.
      @param   iIndex as       an Integer as a Constant
      @return  a TElementContainer
    **)
    Property Elements[Const iIndex : Integer] : TElementContainer Read GetElements; Default;
    (**
      This property returns the comment that is associated with this element.
      @precon  None.
      @postcon Returns the comment that is associated with this element.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      This property returns the Image Index of the element.
      @precon  None.
      @postcon Returns the Image Index of the element.
      @return  a TBADIImageIndex
    **)
    Property ImageIndex : TBADIImageIndex Read FImageIndex Write FImageIndex;
    (**
      This property returns the image index associated with the element based on
      its Image Index and Scope.
      @precon  None.
      @postcon Returns the image index associated with the element based on
               its Image Index and Scope.
      @return  an Integer
    **)
    Property ImageIndexAdjustedForScope : Integer Read GetImageIndexAdjustedForScope;
    (**
      This property determines if the collection is sorted or sequential.
      @precon  None.
      @postcon Sets or gets whether the collection is sorted.
      @return  a Boolean
    **)
    Property Sorted : Boolean Read FSorted Write SetSorted;
    (**
      This property determines if the symbol has been referenced in code.
      @precon  None.
      @postcon Gets or sets the referenced property.
      @return  a Boolean
    **)
    Property Referenced  : Boolean Read FReferenced Write FReferenced;
    (**
      This property returns the parent container of this object. A return of
      nil means that there is no parent.
      @precon  None.
      @postcon Returns the parent container of this object. A return of
               nil means that there is no parent.
      @return  a TElementContainer
    **)
    Property Parent  : TElementContainer Read FParent;
  End;

  (** This is a class reference for the TElementContainer class such that the
      descendant classes can clone themselves. **)
  TElementContainerClass = Class Of TElementContainer;

  (** A class to represent a label within the Module Explorer / Documentation **)
  TLabelContainer = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  {$IFDEF PROFILECODE}
  Profiler,
  {$ENDIF}
  SysUtils,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.DocIssue,
  BADI.Functions,
  BADI.Constants,
  BADI.Comment.Tag,
  BADI.Base.Module,
  BADI.SpellingIssue;

Type
  (** A record to describe error, warning, and hint messages. **)
  TIssueRec = Record
    FFolder: String;
    FFolderImage: TBADIImageIndex;
    FItemImage: TBADIImageIndex;
    FTooMany: String;
  End;

ResourceString
  (** A error resource string for adding a null element to the container **)
  strCanNotAddNullElement = 'Can not add a null element to the collection!';
  (** A error resource string for adding a null token to the container **)
  strCanNotAddNullToken = 'Can not add a null token to the collection!';
  (** A error resource string for adding a null string to the container **)
  strCanNotAddANullString = 'Can not add a null string to the collection!';
  (** A error resource string for sorted after adding elements. **)
  strCanNotSetSortedAfterAdding = 'Can not set sorted after adding elements.';
  (** A message for too many hints. **)
  strTooManyHints = 'Too many hints...';
  (** A message for too many warnings. **)
  strTooManyWarnings = 'Too many warnings...';
  (** A message for too many errors. **)
  strTooManyErrors = 'Too many errors...';

Const
  (** A constant to describe the format of a title with its child count. **)
  strTitleCountFmt = '%s (%d)';
  (** A constant array to describe the various resource strings got error, warning and hint messages. **)
  recIssues: Array [Low(TErrorType)..High(TErrorType)] Of TIssueRec = (
    (FFolder: strHints;    FFolderImage: iiHintFolder;    FItemImage: iiHint;    FTooMany: strTooManyHints),
    (FFolder: strWarnings; FFolderImage: iiWarningFolder; FItemImage: iiWarning; FTooMany: strTooManyWarnings),
    (FFolder: strErrors;   FFolderImage: iiErrorFolder;   FItemImage: iiError;   FTooMany: strTooManyErrors)
  );

(**

  This method adds a string token to the container as a sub container NOT a token.

  @precon  None.
  @postcon Returns an instance of the sub container created around the token.

  @param   strToken    as a String as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AComment    as a TComment as a constant
  @return  a TElementContainer

**)
Function TElementContainer.Add(Const strToken: String; Const AImageIndex: TBADIImageIndex;
  Const AScope: TScope; Const iLine : Integer; Const iColumn : Integer;
  Const AComment: TComment): TElementContainer;

Var
  i: Integer;

Begin
  Assert(strToken <> '', strCanNotAddANullString);
  i := Find(strToken);
  If i < 0 Then
    Begin
      Result := TLabelContainer.Create(strToken, AScope, iLine, iColumn, AImageIndex, AComment);
      Result.FParent := Self;
      FElements.Insert(Abs(i) - 1, Result);
    End
  Else
    Begin
      Result := FElements[i - 1] As TElementContainer;
      If Result.Comment = Nil Then
        Result.Comment := AComment
      Else
        Result.Comment.Assign(AComment);
    End;
End;

(**

  This method adds and passed element container to this classes element collection.

  @precon  AElement must be a valid TElementContainer.
  @postcon Adds and passed element container to this classes element collection.

  @param   AElement as a TElementContainer as a constant
  @return  a TElementContainer

**)
Function TElementContainer.Add(Const AElement: TElementContainer): TElementContainer;

Var
  i: Integer;
  E: TElementContainer;

Begin
  Result := AElement;
  Assert(AElement.Name <> '', strCanNotAddNullElement);
  AElement.FParent := Self;
  i := Find(AElement.Name);
  If i < 0 Then
    FElements.Insert(Abs(i) - 1, AElement)
  Else
    Try
      Result := FElements[i - 1] As TElementContainer;
      If Result.Comment = Nil Then
        Result.Comment := AElement.Comment
      Else
        Result.Comment.Assign(AElement.Comment);
      If Not AElement.ClassNameIs(Result.ClassName) Then
        Begin
          E := FindRoot.Add(strErrors, iiErrorFolder, scNone);
          E.Add(TDocIssue.Create(Format(strTryingToAddType, [AElement.ClassName, Result.ClassName,
            AElement.Name]), scNone, AElement.Line, AElement.Column, etError));
          Raise EBADIParserAbort.Create(strParsingAborted);
        End;
    Finally
      (** Free AElement after getting the comment as it will leak otherwise. **)
      AElement.Free;
    End;
End;

(**

  This method adds and passed Token to this classes element collection.

  @precon  Token must be a valid TTokenInfo and AComment must be either nil or a valid TComment instance
           .
  @postcon Adds a passed element container to this classes element collection.

  @param   Token       as a TTokenInfo as a constant
  @param   AScope      as a TScope as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant
  @return  a TElementContainer

**)
Function TElementContainer.Add(Const Token : TTokenInfo; Const AScope : TScope;
  Const AImageIndex : TBADIImageIndex; Const AComment : TComment): TElementContainer;

Var
  i: Integer;

Begin
  Assert(Token.Token <> '', strCanNotAddNullToken);
  i := Find(Token.Token);
  If i < 0 Then
    Begin
      Result := TLabelContainer.Create(Token.Token, AScope, Token.Line, Token.Column, AImageIndex,
        AComment);
      Result.FParent := Self;
      FElements.Insert(Abs(i) - 1, Result);
    End
  Else
    Begin
      Result := FElements[i - 1] As TElementContainer;
      Result.Comment.Assign(AComment);
    End;
End;

(**

  This method adds a category label to the given container if it does not already exist.

  @precon  Container must be a valid instance.
  @postcon A category container is added to the given container if it does not already exist.

  @param   Container   as a TElementContainer as a constant
  @param   strCategory as a String as a constant
  @param   iImageIndex as a TBADIImageIndex as a constant
  @return  a TElementContainer

**)
Function TElementContainer.AddCategory(Const Container: TElementContainer; Const strCategory : String;
      Const iImageIndex : TBADIImageIndex): TElementContainer;

Begin
  Result := Container.FindElement(strCategory);
  If Not Assigned(Result) Then
    Begin
      Result := TLabelContainer.Create(strCategory, scGlobal, 0, 0, iImageIndex, Nil);
      Result := Container.Add(Result);
    End;
End;

(**

  This method adds an out of limit check to the documentation tree.

  @precon  Container must be a valid instance.
  @postcon A check is added to the document tree.

  @param   Args      as an Array Of Const as a constant
  @param   iLine     as an Integer as a constant
  @param   iColumn   as an Integer as a constant
  @param   Container as a TElementContainer as a constant
  @param   eCheck    as a TBADIModuleCheck as a constant
  @return  a TBADIIssueState

**)
Function TElementContainer.AddCheck(Const Args: Array of Const; Const iLine, iColumn : Integer;
  Const Container : TElementContainer; Const eCheck : TBADIModuleCheck) : TBADIIssueState;

Var
  E: TElementContainer;
  iL, iC: Integer;
  iIcon: TBADIImageIndex;

Begin
  Result := isAdded;
  If BADIOptions.Exclusions.ShouldExclude(ModuleFileName, etChecks) Then
    Begin
      Result := isDisabled;
      Exit;
    End;
  If Not (doShowChecks In BADIOptions.Options) Or Not BADIOptions.ModuleCheck[eCheck].FEnabled Then
    Begin
      Result := isDisabled;
      Exit;
    End;
  If CheckCommentForNoCheck(eCheck, Self) Or CheckCommentForNoCheck(eCheck, Container) Then
    Begin
      Result := isOverride;
      Exit;
    End;
  ModuleMetricPosition(Container, iL, iC);
  iIcon := ModuleCheckImage(eCheck);
  E := FindRoot;
  E := AddRootContainer(E, strChecks, iiCheckFolder);
  E := AddCategory(E, ModuleChecks[eCheck].FCategory, iiCheckFolder);
  E.Add(TDocumentConflict.Create(Args, iLine, iColumn, iL, iC,
    ModuleChecks[eCheck].FMessage, ModuleChecks[eCheck].FDescription, iIcon, ctCheck));
End;

(**

  This method adds a specific documentation conflict to the Documentation conflict collection.

  @precon  None.
  @postcon Adds a specific documentation conflict to the Documentation conflict collection.

  @param   Args           as an Array Of Const as a constant
  @param   iIdentLine     as an Integer as a constant
  @param   iIdentColumn   as an Integer as a constant
  @param   Container      as a TElementContainer as a constant
  @param   strCategory    as a String as a constant
  @param   DocConflictRec as a TDocConflictTable as a constant

**)
Procedure TElementContainer.AddDocumentConflict(Const Args: Array Of Const; Const iIdentLine,
  iIdentColumn: Integer; Const Container: TElementContainer; Const strCategory: String;
  Const DocConflictRec: TDocConflictTable);

Var
  E : TElementContainer;
  iLine, iColumn: Integer;
  iIcon: TBADIImageIndex;

Begin
  If CheckForNoDocumentation(Container) Then
    Exit;
  DocConflictPosition(iLine, iColumn, Container);
  iIcon := DocConflictImage(DocConflictRec);
  E := FindRoot;
  E := AddRootContainer(E, strDocumentationConflicts, iiDocConflictFolder);
  E := AddCategory(E, strCategory, iiDocConflictFolder);
  E.Add(TDocumentConflict.Create(Args, iIdentLine, iIdentColumn, iLine, iColumn,
    DocConflictRec.FMessage, DocConflictRec.FDescription, iIcon, ctDocumentation));
End;

(**

  This method adds an error to the Base Language`s Element Collection under a sub folder of strCategory.

  @precon  Error must be a valid TElementContainer.
  @postcon Adds an error to the Base Language`s Element Collection under a sub folder of strCategory.

  @param   strFmtMsg     as a String as a constant
  @param   strParameters as an Array Of Const as a constant
  @param   AScope        as a TScope as a constant
  @param   iLine         as an Integer as a constant
  @param   iCol          as an Integer as a constant
  @param   ErrorType     as a TErrorType as a constant
  @param   Container     as a TElementContainer as a constant

**)
Procedure TElementContainer.AddIssue(Const strFmtMsg: String; Const strParameters : Array of Const;
  Const AScope: TScope; Const iLine, iCol: Integer; Const ErrorType: TErrorType;
  Const Container : TElementContainer);

Var
  i: TElementContainer;

Begin
  If Not CheckEWHOptions(ErrorType) Or CheckForNoEWH(ErrorType, strParameters, Container) Then
    Exit;
  I := FindRoot;
  I := AddRootContainer(I, recIssues[ErrorType].FFolder, recIssues[ErrorType].FFolderImage);
  i.Add(TDocIssue.Create(Format(strFmtMsg, strParameters), AScope, iLine, iCol, ErrorType));
End;

(**

  This method adds an error to the Base Language`s Element Collection under a sub folder of strCategory.

  @precon  Error must be a valid TElementContainer.
  @postcon Adds an error to the Base Language`s Element Collection under a sub folder of strCategory.

  @param   strMsg    as a String as a constant
  @param   AScope    as a TScope as a constant
  @param   iLine     as an Integer as a constant
  @param   iCol      as an Integer as a constant
  @param   ErrorType as a TErrorType as a constant
  @param   Container as a TElementContainer as a constant

**)
Procedure TElementContainer.AddIssue(Const strMsg: String; Const AScope: TScope; Const iLine,
  iCol: Integer; Const ErrorType: TErrorType; Const Container : TElementContainer);

Var
  i: TElementContainer;

Begin
  If Not CheckEWHOptions(ErrorType) Or CheckForNoEWH(ErrorType, [Container.Identifier], Container) Then
    Exit;
  I := FindRoot;
  I := AddRootContainer(I, recIssues[ErrorType].FFolder, recIssues[ErrorType].FFolderImage);
  i.Add(TDocIssue.Create(strMsg, AScope, iLine, iCol, ErrorType));
End;

(**

  This method adds an out of limit metric to the documentation tree.

  @precon  Container must be a valid instance.
  @postcon A metric is added to the document tree.

  @param   Args      as an Array Of Const as a constant
  @param   iLine     as an Integer as a constant
  @param   iColumn   as an Integer as a constant
  @param   Container as a TElementContainer as a constant
  @param   eMetric   as a TBADIModuleMetric as a constant
  @return  a TBADIIssueState

**)
Function TElementContainer.AddMetric(Const Args: Array of Const; Const iLine, iColumn : Integer;
  Const Container : TElementContainer; Const eMetric : TBADIModuleMetric) : TBADIIssueState;

Var
  E: TElementContainer;
  iL, iC: Integer;
  iIcon: TBADIImageIndex;

Begin
  Result := isAdded;
  If BADIOptions.Exclusions.ShouldExclude(ModuleFileName, etMetrics) Then
    Begin
      Result := isDisabled;
      Exit;
    End;
  If Not (doShowMetrics In BADIOptions.Options) Or Not BADIOptions.ModuleMetric[eMetric].FEnabled Then
    Begin
      Result := isDisabled;
      Exit;
    End;
  If CheckCommentForNoMetric(eMetric, Self) Or CheckCommentForNoMetric(eMetric, Container) Then
    Begin
      Result := isOverride;
      Exit;
    End;
  ModuleMetricPosition(Container, iL, iC);
  iIcon := ModuleMetricImage(eMetric);
  E := FindRoot;
  E := AddRootContainer(E, strMetrics, iiMetricFolder);
  E := AddCategory(E, ModuleMetrics[eMetric].FCategory, iiMetricFolder);
  E.Add(
    TDocumentConflict.Create(Args, iLine, iColumn, iL, iC, ModuleMetrics[eMetric].FMessage,
      ModuleMetrics[eMetric].FDescription, iIcon, ctMetric)
  );
End;

(**

  This method adds a root element container to the module for capturing errors, warnings, hints,
  metrics, etc.

  @precon  Container must be a valid instance.
  @postcon A container is added to the given container if it does not already exist.

  @param   Container    as a TElementContainer as a constant
  @param   strLabelText as a String as a constant
  @param   iImageIndex  as a TBADIImageIndex as a constant
  @return  a TElementContainer

**)
Function TElementContainer.AddRootContainer(Const Container: TElementContainer;
  Const strLabelText : String; Const iImageIndex : TBADIImageIndex): TElementContainer;

Begin
  Result := Container.FindElement(strLabelText);
  If Result = Nil Then
    Result := Container.Add(TLabelContainer.Create(strLabelText, scGlobal, 0, 0, iImageIndex,
      Nil)) As TLabelContainer;
End;

(**

  Added a method to add spelling mistakes to the modules list of issues.

  @precon  None.
  @postcon If spelling is not disabled the given word is added to the spelling mistakes list.

  @param   strWord     as a String as a constant
  @param   strCategory as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iWordLine   as an Integer as a constant
  @param   iWordColumn as an Integer as a constant
  @param   Comment     as a TComment as a constant
  @return  a TBADIIssueState

**)
Function TElementContainer.AddSpelling(Const strWord, strCategory : String; Const AScope : TScope;
  Const iWordLine, iWordColumn: Integer; Const Comment: TComment) : TBADIIssueState;

ResourceString
  strSpellingMistakes = 'Spelling Mistakes';

Var
  E: TElementContainer;
  iL, iC: Integer;

Begin
  Result := isAdded;
  If BADIOptions.Exclusions.ShouldExclude(ModuleFileName, etSpelling) Then
    Begin
      Result := isDisabled;
      Exit;
    End;
  If Not (doShowSpelling In BADIOptions.Options) Then
    Begin
      Result := isDisabled;
      Exit;
    End;
  If CheckCommentForNoSpelling(Self.Comment, strWord) Or CheckCommentForNoSpelling(Comment, strWord) Then
    Begin
      Result := isOverride;
      Exit;
    End;
  E := FindRoot;
  E := AddCategory(E, strSpellingMistakes, iiSpellingFolder);
  E := AddCategory(E, strCategory, iiSpellingFolder);
  iL := iWordLine;
  iC := iWordColumn;
  If Assigned(Comment) Then
    Begin
      iL := Comment.Line;
      iC := Comment.Column;
    End;
  E.Add(TBADISpellingIssue.Create(strWord, '', AScope, iWordLine, iWordColumn, iL, iC));
End;

(**

  This method adds the given elements tokens to the current containers tokens.

  @precon  None.
  @postcon Adds the given elements tokens to the current containers tokens.

  @param   AElement as a TElementContainer as a constant

**)
Procedure TElementContainer.AddTokens(Const AElement: TElementContainer);

Var
  i: Integer;

Begin
  Assert(AElement <> Nil, strCanNotAddNullElement);
  For i := 0 To AElement.TokenCount - 1 Do
    AppendToken(AElement.Tokens[i]);
End;

(**

  This method attempts to adds an element to the collection but raises an error IF there is already an
  object of the same name in the collection (duplicate detection).

  @precon  None.
  @postcon Attempts to adds an element to the collection but raises an error IF there is already an
           object of the same name in the collection (duplicate detection).

  @param   AElement as a TElementContainer as a constant
  @return  a TElementContainer

**)
Function TElementContainer.AddUnique(Const AElement: TElementContainer): TElementContainer;

ResourceString
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';

Var
  iLine, iCol: Integer;
  strI: String;
  E : TElementContainer;

Begin
  E := AElement;
  iLine := E.Line;
  iCol := E.Column;
  strI := E.Identifier;
  Result := Add(AElement);
  If Result <> E Then
    AddIssue(Format(strDuplicateIdentifierFound, [strI, iLine, iCol]), scNone, iLine, iCol, etError,
      Result);
End;

(**

  This method copies the tokens from the source into this element replacing any token that already exist.

  @precon  Source must be a valid TElementContainer.
  @postcon Copies the tokens from the source into this element replacing any token that already exist.

  @param   Source as a TElementContainer as a constant

**)
Procedure TElementContainer.Assign(Const Source: TElementContainer);

Var
  iToken: Integer;

Begin
  Name := Source.Name;
  Scope := Source.Scope;
  Line := Source.Line;
  Column := Source.Column;
  FComment := Source.Comment;
  FImageIndex := Source.FImageIndex;
  ClearTokens;
  For iToken := 0 To Source.TokenCount - 1 Do
    AppendToken(Source.Tokens[iToken]);
End;

(**

  This method checks for a noXxxxx(s) tag which would disable the type of messages and if found returns 
  true.

  @precon  None.
  @postcon Checks for a noXxxxx(s) tag which would disable the type of messages and if found returns 
           true.

  @param   eCheck  as a TBADIModuleCheck as a constant
  @param   Element as a TElementContainer as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckCommentForNoCheck(Const eCheck: TBADIModuleCheck;
  Const Element: TElementContainer): Boolean;

Var
  iTag : Integer;
  T: TTag;
  iToken: Integer;
  E: TElementContainer;

Begin
  Result := False;
  E := Element;
  While Assigned(E) Do
    Begin
      If Assigned(E.Comment) Then
        If E.Comment.FindTag(strNoChecks) > -1 Then
          Begin
            Result := True;
            Break;
          End
        Else
          Begin
            iTag := E.Comment.FindTag(strNoCheck);
            If iTag > -1 Then
              Begin
                T := E.Comment.Tag[iTag];
                For iToken := 0 To T.TokenCount - 1 Do
                  If CompareText(ModuleChecks[eCheck].FName, T.Tokens[iToken].Token) = 0 Then
                    Begin
                      Result := True;
                      Exit;
                    End;
              End;
          End;
        E := E.Parent;
      End;
End;

(**

  This method checks for a noXxxxx(s) tag which would disable the type of messages and if found returns 
  true.

  @precon  None.
  @postcon Checks for a noXxxxx(s) tag which would disable the type of messages and if found returns 
           true.

  @param   strEWH        as a String as a constant
  @param   strParameters as an Array Of Const as a constant
  @param   Element       as a TElementContainer as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckCommentForNoEWH(Const strEWH: String;
  Const strParameters : Array of Const; Const Element: TElementContainer): Boolean;

Var
  E: TElementContainer;
  
Begin
  Result := False;
  E := Element;
  While Assigned(E) Do
    Begin
      If Assigned(E.Comment) Then
        If (E.Comment.FindTag(strEWH + 's') > -1) Or CheckIdentifier(E,
          String(strParameters[0].VUnicodeString), strEWH) Then
          Begin
            Result := True;
            Break;
          End;
        E := E.Parent;
      End;
End;

(**

  This method changes to

  @precon  None.
  @postcon Returns return if @@nometric or @@nometrics is found in the element comment or parent comment
           .

  @nometric or
  @nometrics in the elements comment and all parent comments and if foudn and matches the metric being 
           checked returns return.

  @param   eMetric as a TBADIModuleMetric as a constant
  @param   Element as a TElementContainer as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckCommentForNoMetric(Const eMetric : TBADIModuleMetric;
  Const Element: TElementContainer): Boolean;

Var
  iTag : Integer;
  T: TTag;
  iToken: Integer;
  E: TElementContainer;

Begin
  Result := False;
  E := Element;
  While Assigned(E) Do
    Begin
      If Assigned(E.Comment) Then
        If E.Comment.FindTag(strNoMetrics) > -1 Then
          Begin
            Result := True;
            Break;
          End
        Else
          Begin
            iTag := E.Comment.FindTag(strNoMetric);
            If iTag > -1 Then
              Begin
                T := E.Comment.Tag[iTag];
                For iToken := 0 To T.TokenCount - 1 Do
                  If CompareText(ModuleMetrics[eMetric].FName, T.Tokens[iToken].Token) = 0 Then
                    Begin
                      Result := True;
                      Exit;
                    End;
              End;
          End;
        E := E.Parent;
      End;
End;

(**

  This method checks whether there are tag to disable spelling in general or on a specific word.

  @precon  None.
  @postcon Returns return if spelling should be disabled.

  @param   Comment       as a TComment as a constant
  @param   strIdentifier as a String as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckCommentForNoSpelling(Const Comment: TComment;
  Const strIdentifier : String): Boolean;

Var
  iTag: Integer;
  T: TTag;
  iToken: Integer;

Begin
  Result := False;
  If Assigned(Comment) Then
    If Comment.FindTag(strNoSpellings) > -1 Then
      Result := True
    Else
      Begin
        iTag := Comment.FindTag(strNoSpelling);
        If iTag > -1 Then
          Begin
            T := Comment.Tag[iTag];
            For iToken := 0 To T.TokenCount - 1 Do
              If CompareText(strIdentifier, T.Tokens[iToken].Token) = 0 Then
                Result := True
          End;
      End;
End;

(**

  This method recursively checks the documentation of the module. Descendants need to override this to
  implement document checking.

  @precon  None.
  @postcon Recursively checks the documentation of the module.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TElementContainer.CheckDocumentation(Var boolCascade: Boolean);

Var
  i: Integer;

Begin
  If boolCascade Then
    For i := 1 To ElementCount Do
      Elements[i].CheckDocumentation(boolCascade);
End;

(**

  This method check the documentation options for the type of error to display and if not found returns
  false.

  @precon  None.
  @postcon Check the documentation options for the type of error to display and if not found returns
           false.

  @param   ErrorType as a TErrorType as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckEWHOptions(Const ErrorType : TErrorType) : Boolean;

Begin
  REsult := True;
  Case ErrorType Of
    etHint:
      If Not(doShowHints In BADIOptions.Options) Then
        Result := False;
    etWarning:
      If Not(doShowWarnings In BADIOptions.Options) Then
        Result := False;
    etError:
      If Not(doShowErrors In BADIOptions.Options) Then
        Result := False;
  End;
End;

(**

  This method checks for a stop or no documentation tag and returns true if found in the elements comment
  of one of the parent comments.

  @precon  None.
  @postcon Checks for a stop or no documentation tag and returns true if found in the elements comment
           of one of the parent comments.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckForNoDocumentation(Const Container : TElementContainer) : Boolean;

Var
  E : TElementContainer;
  
Begin
  Result := False;
  If Not (doShowConflicts In BADIOptions.Options) Then
    Exit(True);
  E := Container;
  While Assigned(E) Do
    Begin
      If Assigned(E.Comment) Then
        If ((E.Comment.FindTag(strStopDocumentation) >= 0) Or
            (E.Comment.FindTag(strNoDocumentation) >= 0)) Then
          Exit(True);
      E := E.Parent;
    End;
End;

(**

  This method checks the container comment (and its parents) for noXxxxx(s) tags and returns true if they 
  were found to signify that an issue is not required.

  @precon  None.
  @postcon Checks the container comment (and its parents) for noXxxxx(s) tags and returns true if they 
           were found to signify that an issue is not required.

  @param   ErrorType     as a TErrorType as a constant
  @param   strParameters as an Array Of Const as a constant
  @param   Container     as a TElementContainer as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckForNoEWH(Const ErrorType : TErrorType;
  Const strParameters : Array of Const; Const Container: TElementContainer): Boolean;

Begin
  Result := False;
  If Assigned(Container) Then
    Begin
      Case ErrorType Of
        etError:
          If CheckCommentForNoEWH(strNoError, strParameters, Container) Then
            Result := True;
        etHint:
          If CheckCommentForNoEWH(strNoHint, strParameters, Container) Then
            Result := True;
        etWarning:
          If CheckCommentForNoEWH(strNoWarning, strParameters, Container) Then
            Result := True;
      End;
    End;
End;

(**

  This method looks for the given identifier in the given containers tag comment tokens.

  @precon  Container must be a valid instance.
  @postcon Returns true if the identifier is a token of the containers comment tag.

  @param   Container     as a TElementContainer as a constant
  @param   strIdentifier as a String as a constant
  @param   strTagName    as a String as a constant
  @return  a Boolean

**)
Function TElementContainer.CheckIdentifier(Const Container: TElementContainer;
  Const strIdentifier, strTagName: String): Boolean;

Var
  iTag: Integer;
  Tag: TTag;
  iToken: Integer;

Begin
  Result := False;
  If Assigned(Container.Comment) Then
    Begin
      iTag := Container.Comment.FindTag(strTagName);
      If iTag > -1 Then
        Begin
          Tag := Container.Comment.Tag[iTag];
          For iToken := 0 To Tag.TokenCount - 1 Do
            If CompareText(Tag.Tokens[iToken].Token, strIdentifier) = 0 Then
              Begin
                Result := True;
                Break;
              End;
        End;
    End;
End;

(**

  This method recursively checks the referenced property and outputs a hint if any element is not
  referenced which has a scope of Local or Private.

  @precon  None.
  @postcon Recursively checks the referenced property and outputs a hint if any element is not
           referenced which has a scope of Local or Private.

**)
Procedure TElementContainer.CheckReferences;

  (**

    This procedure recurses the elements parents building a fully qualified identifier.

    @precon  None.
    @postcon Return a fully qualified identifier.

    @param   E             as a TElementContainer as a reference
    @param   strIdentifier as a String as a reference

  **)
  Procedure RecurseParentIdentifiers(Var E : TElementContainer; Var strIdentifier : String);

  Begin
    While Assigned(E) And Assigned(E.Parent) Do
      Begin
        If Not(E Is TLabelContainer) Then
          Begin
            If strIdentifier <> '' Then
              strIdentifier := '.' + strIdentifier;
            strIdentifier := E.Identifier + strIdentifier;
          End;
        E := E.Parent;
      End;
  End;

Var
  i: Integer;
  strIdentifier: String;
  E: TElementContainer;

Begin
  If doShowUnReferencedSymbols In BADIOptions.Options Then
    Begin
      If Not Referenced  And (Scope In [scLocal, scPrivate]) Then
        Begin
          E := Self;
          If Identifier <> '' Then
            Begin
              RecurseParentIdentifiers(E, strIdentifier);
              AddIssue(Format(strUnreferencedLocal, [strIdentifier]), scNone, Line, Column, etHint,
                Self);
            End;
        End;
      For i := 1 To ElementCount Do
        Elements[i].CheckReferences;
    End;
End;

(**

  This is the constructor method for the TElementContainer class.

  @precon  AComment must be either nil or a valid TComment instance.
  @postcon Creates an instance of the element container.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TElementContainer.Create(Const strName: String; Const AScope: TScope;
  Const iLine, iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn);
  FElements := TObjectList.Create(True);
  FComment := AComment;
  FImageIndex := AImageIndex;
  FSorted := True;
  FReferenced := False;
  FParent := Nil;
  FBADIOptions := TBADIOptions.BADIOptions;
End;

(**

  This method deletes the indexed element from the collection.

  @precon  iIndex must be a valid index between 1 and ElementCount.
  @postcon Deletes the indexed element from the collection.

  @param   iIndex as an Integer as a constant

**)
Procedure TElementContainer.DeleteElement(Const iIndex: Integer);

Begin
  FElements.Delete(iIndex - 1);
End;

(**

  This is the destructor method for the TElementContainer class.

  @precon  None.
  @postcon Destroys the instance of the class.

**)
Destructor TElementContainer.Destroy;

Begin
  FBADIOptions := Nil;
  FElements.Free;
  Inherited Destroy;
End;

(**

  This method returns the conflict image for missing / incorrect documentation.

  @precon  None.
  @postcon The conflict image for missing / incorrect documentation is returned.

  @param   DocConflictRec as a TDocConflictTable as a constant
  @return  a TBADIImageIndex

**)
Function TElementContainer.DocConflictImage(Const DocConflictRec: TDocConflictTable) : TBADIImageIndex;

Begin
  Case DocConflictRec.FConflictType Of
    dciMissing:   Result := iiDocConflictMissing;
    dciIncorrect: Result := iiDocConflictIncorrect;
  Else
    Result := iiDocConflictItem;
  End;
End;

(**

  This method updates the cursor position for the conflict message based on the comment if found.

  @precon  None.
  @postcon The cursor position for the conflict message based on the comment if found is updated.

  @param   iL        as an Integer as a reference
  @param   iC        as an Integer as a reference
  @param   Container as a TElementContainer as a constant

**)
Procedure TElementContainer.DocConflictPosition(Var iL: Integer; Var iC: Integer;
  Const Container: TElementContainer);

Begin
  iL := 0;
  iC := 0;
  If Assigned(Container) Then
    Begin
      iL := Container.Line;
      iC := Container.Column;
      If Assigned(Container.Comment) Then
        Begin
          iL := Container.Comment.Line;
          iC := Container.Comment.Column;
        End;
    End;
End;

(**

  This method returns the position of the named container in the current containers collection if found
  else returns the position (as a negative) where the item should be inserted in the collection.

  @precon  None.
  @postcon Returns the position of the named container in the current containers collection if found
           else returns the position (as a negative) where the item should be inserted in the
           collection.

  @nometric HardCodedInteger

  @param   strName  as a String as a constant
  @param   FindType as a TFindType as a constant
  @return  an Integer

**)
Function TElementContainer.Find(Const strName: String; Const FindType: TFindType = ftName): Integer;

  (**

    This function implements a binary search for the element in the current element collection.

    @precon  None.
    @postcon The position of the element is returned if found else the position it should be inserted is
             returned as a negative number.

    @param   iFirst as an Integer as a reference
    @return  an Integer

  **)
  Function BinarySearch(Var iFirst : Integer) : Integer;

  Var
    iLast: Integer;
    iMid: Integer;
    iResult: Integer;

  Begin
    Result := -1;
    iFirst := 1;
    iLast := FElements.Count;
    While iFirst <= iLast Do
      Begin
        iMid := (iFirst + iLast) Div 2;
        If FindType = ftName Then
          iResult := CompareText(Elements[iMid].Name, strName)
        Else
          iResult := CompareText(Elements[iMid].Identifier, strName);
        If iResult = 0 Then
          Begin
            Result := iMid;
            Break;
          End
        Else If iResult > 0 Then
          iLast := iMid - 1
        Else
          iFirst := iMid + 1;
      End;
  End;

  (**

    This method implements a sequential search for the element in the current elements collection.

    @precon  None.
    @postcon The position of the element is returned if found else the position it should be inserted is
             returned as a negative number.

    @param   iFirst as an Integer as a reference
    @return  an Integer

  **)
  Function SequentialSearch(Var iFirst : Integer) : Integer;

  Var
     i : Integer;
   
  Begin
    Result := -1;
    For i := 1 To ElementCount Do
      If CompareText(Elements[i].Name, strName) = 0 Then
        Begin
          Result := i;
          Break;
        End;
    iFirst := ElementCount + 1;
  End;

Var
  iFirst : Integer;

Begin
  If FSorted Then
    Result := BinarySearch(iFirst)
  Else
    Result := SequentialSearch(iFirst);
  If Result < 0 Then
    Result := -iFirst;
End;

(**

  This method searches the elements collection of and instance matching the given name.

  @precon  None.
  @postcon Returns either instance of the found item or returns nil.

  @param   strName  as a String as a constant
  @param   FindType as a TFindType as a constant
  @return  a TElementContainer

**)
Function TElementContainer.FindElement(Const strName: String; Const FindType: TFindType = ftName)
  : TElementContainer;

Var
  i: Integer;

Begin
  Result := Nil;
  i := Find(strName, FindType);
  If i > 0 Then
    Result := Elements[i];
End;

(**

  This method finds the root element of the tree containing the current element.

  @precon  None.
  @postcon Finds the root element of the tree containing the current element.

  @return  a TElementContainer

**)
Function TElementContainer.FindRoot: TElementContainer;

Begin
  Result := Self;
  While Result.Parent <> Nil Do
    Result := Result.Parent;
End;

(**

  This function finds the occurrence of the token and returns its index if found else returns -1.

  @precon  None.
  @postcon Finds the occurrence of the token and returns its index if found else returns -1.

  @param   strToken as a String as a constant
  @return  an Integer

**)
Function TElementContainer.FindToken(Const strToken: String): Integer;

Var
  i: Integer;

Begin
  Result := -1;
  For i := 0 To TokenCount - 1 Do
    If CompareText(strToken, Tokens[i].Token) = 0 Then
      Begin
        Result := i;
        Break;
      End;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of Items in the Elements Collection.

  @return  an Integer

**)
Function TElementContainer.GetElementCount: Integer;
Begin
  Result := FElements.Count;
End;

(**

  This is a getter method for the Elements property.

  @precon  iIndex must be a valid index into the elements collection.
  @postcon Returns the instance of the indexed element.

  @param   iIndex as an Integer as a constant
  @return  a TElementContainer

**)
Function TElementContainer.GetElements(Const iIndex: Integer): TElementContainer;

Begin
  Result := FElements[iIndex - 1] As TElementContainer;
End;

(**

  This method returns the adjusted image index the element based on the scope.

  @precon  None.
  @postcon Returns the adjusted image index the element based on the scope.

  @return  an Integer

**)
Function TElementContainer.GetImageIndexAdjustedForScope: Integer;

Begin
  Result := BADIImageIndex(FImageIndex, Scope);
End;

(**

  This method returns the image index for the given check.

  @precon  None.
  @postcon The image index for the given check is returned.

  @param   eCheck as a TBADIModuleCheck as a constant
  @return  a TBADIImageIndex

**)
Function TElementContainer.ModuleCheckImage(Const eCheck: TBADIModuleCheck): TBADIImageIndex;

Begin
  Case ModuleChecks[eCheck].FConflictType Of
    dciMissing:   Result := iiCheckMissing;
    dciIncorrect: Result := iiCheckIncorrect;
  Else
    Result := iiCheckItem;
  End;
End;

(**

  This function finds the root module filename.

  @precon  None.
  @postcon Returns the filename of the root module.

  @return  a String

**)
Function TElementContainer.ModuleFileName: String;

Var
  RC : TElementContainer;
  
Begin
  RC := FindRoot;
  If Assigned(RC) Then
    If RC Is TBaseLanguageModule Then
      Result := (RC as TBaseLanguageModule).FileName;
End;

(**

  This method returns the image index for the given metric.

  @precon  None.
  @postcon The image index for the given metric is returned.

  @param   eMetric as a TBADIModuleMetric as a constant
  @return  a TBADIImageIndex

**)
Function TElementContainer.ModuleMetricImage(Const eMetric : TBADIModuleMetric): TBADIImageIndex;

Begin
  Case ModuleMetrics[eMetric].FConflictType Of
    dciMissing:   Result := iiMetricMissing;
    dciIncorrect: Result := iiMetricIncorrect;
  Else
    Result := iiMetricItem;
  End;
End;

(**

  This method updates the line and column based on the given container and comment.

  @precon  Container must be a valid instance.
  @postcon The line and column are updated.

  @param   Container as a TElementContainer as a constant
  @param   iL        as an Integer as a reference
  @param   iC        as an Integer as a reference

**)
Procedure TElementContainer.ModuleMetricPosition(Const Container : TElementContainer;
  Var iL, iC : Integer);

Begin
  iL := Container.Line;
  iC := Container.Column;
  If Assigned(Container.Comment) Then
    Begin
      iL := Container.Comment.Line;
      iC := Container.Comment.Column;
    End;
End;

(**

  This method searches for references to the passed symbol in the passed section.

  @precon  None.
  @postcon Returns true if the symbol was found.

  @param   AToken  as a TTokenInfo as a constant
  @param   Section as a TLabelContainer as a constant
  @return  a Boolean

**)
Function TElementContainer.ReferenceSection(Const AToken: TTokenInfo;
  Const Section: TLabelContainer): Boolean;

Var
  E: TElementContainer;

Begin
  Result := False;
  If Section <> Nil Then
    Begin
      E := Section.FindElement(AToken.Token);
      If E <> Nil Then
        Begin
          E.Referenced := True;
          AToken.Reference := trResolved;
          Result := True;
          Exit;
        End;
    End;
End;

(**

  This method does nothing other than call its parents ReferenceSymbol method. Descendant should override
  this method to resolve symbols in the code.

  @precon  None.
  @postcon Passes the processing of the symbol to its parent IF the parent exists.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TElementContainer.ReferenceSymbol(Const AToken: TTokenInfo): Boolean;

Begin
  Result := False;
  If Assigned(FParent) Then
    Result := FParent.ReferenceSymbol(AToken);
End;

(**

  This is a setter method for the Sorted property.

  @precon  None.
  @postcon Sets the class to be either sorted or not sorted. Must be set before adding elements to the
           collection.

  @param   boolValue as a Boolean as a constant

**)
Procedure TElementContainer.SetSorted(Const boolValue: Boolean);
Begin
  Assert(ElementCount = 0, strCanNotSetSortedAfterAdding);
  FSorted := boolValue;
End;

{ TLabelContainer }

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the name of the label as a string.

  @nohints

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TLabelContainer.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  If doShowChildCountInTitles In BADIOptions.Options Then
    Result := Format(strTitleCountFmt, [Name, ElementCount])
  Else
    Result := Name;
End;

(**

  This is an overridden constructor to ensure that label are not displayed in the unreferenced scope
  hints.

  @precon  None.
  @postcon Overridden constructor to ensure that label are not displayed in the unreferenced scope hints
           .

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TLabelContainer.Create(Const strName: String; Const AScope: TScope; Const iLine,
  iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  Referenced := True;
End;

End.
