(**

  This module contains classes to represent an asbtract element container (ancestor for all things)
  and a label container for tree view headers adn the like.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.ElementContainer;

Interface

Uses
  Classes,
  Contnrs,
  BADI.Comment,
  BADI.Types,
  BADI.Base.Container,
  BADI.TokenInfo;

{$INCLUDE CompilerDefinitions.inc}

Type
  TLabelContainer = Class;

  (** This class implements the IElementCollection interface so that this
      element container can be rendered with the module browser. **)
  TElementContainer = Class {$IFDEF D2005} Abstract {$ENDIF} (TBaseContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FElements : TObjectList;
    FComment : TComment;
    FScope : TScope;
    FImageIndex : TBADIImageIndex;
    FSorted  : Boolean;
    FReferenced : Boolean;
    FParent : TElementContainer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementCount : Integer;
    Function GetElements(iIndex : Integer) : TElementContainer;
    Function GetImageIndexAdjustedForScope : Integer;
    Function Find(const strName : String; FindType : TFindType = ftName) : Integer;
    Procedure SetSorted(boolValue : Boolean);
    Function FindRoot : TElementContainer;
  Public
    Constructor Create(const strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Virtual;
    Destructor Destroy; Override;
    Function  Add(AElement : TElementContainer) : TElementContainer; Overload; Virtual;
    Function  Add(Token : TTokenInfo; AScope : TScope; AImageIndex : TBADIImageIndex;
      AComment : TComment) : TElementContainer; Overload; Virtual;
    Function  Add(const strToken : String; AImageIndex : TBADIImageIndex;
      AScope : TScope; AComment : TComment) : TElementContainer; Overload; Virtual;
    Function AddUnique(AElement : TElementContainer) : TElementContainer; Virtual;
    Procedure AddTokens(AElement : TElementContainer); Virtual;
    Function  FindElement(const strName : String; FindType : TFindType = ftName) : TElementContainer;
    Procedure Assign(Source : TElementContainer); Virtual;
    Function  FindToken(const strToken : String) : Integer;
    Procedure DeleteElement(iIndex : Integer);
    Procedure CheckDocumentation(var boolCascade : Boolean); Virtual;
    Function  ReferenceSymbol(AToken : TTokenInfo) : Boolean; Virtual;
    Procedure AddIssue(const strMsg : String; AScope : TScope; const strMethod : String;
      iLine, iCol : Integer; ErrorType : TErrorType);
    Procedure AddDocumentConflict(Const Args: Array of Const;
      iIdentLine, iIdentColumn : Integer; AComment : TComment;
      const strCategory : String; DocConflictRec : TDocConflictTable);
    Function  AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Virtual; Abstract;
    Procedure CheckReferences; Virtual;
    Function ReferenceSection(AToken : TTokenInfo; Section: TLabelContainer) : Boolean;
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
      @param   iIndex as       an Integer
      @return  a TElementContainer
    **)
    Property Elements[iIndex : Integer] : TElementContainer Read GetElements; Default;
    (**
      This property returns the comment that is associated with this element.
      @precon  None.
      @postcon Returns the comment that is associated with this element.
      @return  a TComment
    **)
    Property Comment : TComment Read FComment Write FComment;
    (**
      This property returns the Scope of the element.
      @precon  None.
      @postcon Returns the Scope of the element.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
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
    Constructor Create(const strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TBADIImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  SysUtils,
  BADI.ResourceStrings,
  BADI.DocIssue, BADI.Options;

(**

  This method adds and passed elemtn container to this classes element
  collection.

  @precon  AElement must be a valid TElementContainer.
  @postcon Adds and passed elemtn container to this classes element
           collection.

  @param   AElement as a TElementContainer
  @return  a TElementContainer

**)
Function TElementContainer.Add(AElement: TElementContainer): TElementContainer;

Var
  i: Integer;
  E: TElementContainer;

Begin
  Result := AElement;
  Assert(AElement.Name <> '', 'Can not add a null element to the collection!');
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
          E := FindRoot.Add(strErrors, iiErrorFolder, scNone, Nil);
          E.Add(TDocIssue.Create(Format(strTryingToAddType, [AElement.ClassName, Result.ClassName,
            AElement.Name]), scNone, 'TElementContainer.Add', AElement.Line, AElement.Column,
            iiError));
          Raise EBADIParserAbort.Create('Parsing Aborted!');
        End;
    Finally
      (** Free AElement after getting the comment as it will leak otherwise. **)
      AElement.Free;
    End;
End;

(**

  This method adds and passed Token to this classes element collection.

  @precon  Token must be a valid TTokenInfo and AComment must be either nil or
           a valid TComment instance.
  @postcon Adds and passed elemtn container to this classes element
           collection.

  @param   Token       as a TTokenInfo
  @param   AScope      as a TScope
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment
  @return  a TElementContainer

**)
Function TElementContainer.Add(Token: TTokenInfo; AScope: TScope; AImageIndex: TBADIImageIndex;
  AComment: TComment): TElementContainer;

Var
  i: Integer;

Begin
  Assert(Token.Token <> '', 'Can not add a null token to the collection!');
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


  This method adds a string token to the container as a sub container NOT a
  token.


  @precon  None.

  @postcon Returns an instance of the sub container created around the token.


  @param   strToken    as a String as a constant
  @param   AImageIndex as a TBADIImageIndex
  @param   AScope      as a TScope
  @param   AComment    as a TComment
  @return  a TElementContainer

**)
Function TElementContainer.Add(const strToken: String; AImageIndex: TBADIImageIndex; AScope: TScope;
  AComment: TComment): TElementContainer;

Var
  i: Integer;

Begin
  Assert(strToken <> '', 'Can not add a null string to the collection!');
  i := Find(strToken);
  If i < 0 Then
    Begin
      Result := TLabelContainer.Create(strToken, AScope, 0, 0, AImageIndex, AComment);
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

  This method adds a specific documentation conflict to the Docuemntation
  conflict collection.

  @precon  None.
  @postcon Adds a specific documentation conflict to the Docuemntation
           conflict collection.

  @param   Args            as an Array Of Const as a constant
  @param   iIdentLine      as an Integer
  @param   iIdentColumn    as an Integer
  @param   AComment        as a TComment
  @param   strCategory     as a String as a constant
  @param   DocConflictRec  as a TDocConflictTable

**)
Procedure TElementContainer.AddDocumentConflict(Const Args: Array Of Const;
  iIdentLine, iIdentColumn: Integer; AComment: TComment; const strCategory: String;
  DocConflictRec: TDocConflictTable);

Var
  E, i, R, D: TElementContainer;
  iL, iC: Integer;
  iIcon: TBADIImageIndex;

Begin
  iL := 0;
  iC := 0;
  If AComment <> Nil Then
    Begin
      iL := AComment.Line;
      iC := AComment.Column;
    End;
  Case DocConflictRec.FConflictType Of
    dciMissing:
      iIcon := iiDocConflictMissing;
    dciIncorrect:
      iIcon := iiDocConflictIncorrect;
  Else
    iIcon := iiDocConflictItem;
  End;
  R := FindRoot;
  D := R.FindElement(strDocumentationConflicts);
  If D = Nil Then
    D := R.Add(TLabelContainer.Create(strDocumentationConflicts, scGlobal, 0, 0,
      iiDocConflictFolder, Nil)) As TLabelContainer;
  E := D;
  i := E.FindElement(strCategory);
  If i = Nil Then
    Begin
      i := TLabelContainer.Create(strCategory, scGlobal, 0, 0, iiDocConflictFolder, Nil);
      i := E.Add(i);
    End;
  If i.ElementCount < BrowseAndDocItOptions.IssueLimits[ltConflicts] Then
    i.Add(TDocumentConflict.Create(Args, iIdentLine, iIdentColumn, iL, iC, DocConflictRec.FMessage,
      DocConflictRec.FDescription, iIcon))
  Else If i.ElementCount = BrowseAndDocItOptions.IssueLimits[ltConflicts] Then
    i.Add(TDocumentConflict.Create([], 0, 0, 0, 0, strTooManyConflicts, strTooManyConflictsDesc,
      iiDocConflictMissing));
End;

(**


  This method adds an error to the Base Language`s Element Collection under a
  sub folder of strCategory.


  @precon  Error must be a valid TElementContainer.

  @postcon Adds an error to the Base Language`s Element Collection under a sub

           folder of strCategory.


  @param   strMsg    as a String as a constant
  @param   AScope    as a TScope
  @param   strMethod as a String as a constant
  @param   iLine     as an Integer
  @param   iCol      as an Integer
  @param   ErrorType as a TErrorType

**)
Procedure TElementContainer.AddIssue(const strMsg: String; AScope: TScope; const strMethod: String;
  iLine, iCol: Integer; ErrorType: TErrorType);

Type
  TIssueRec = Record
    FFolder: String;
    FFolderImage: TBADIImageIndex;
    FItemImage: TBADIImageIndex;
    FTooMany: String;
  End;

ResourceString
  strTooManyHints = 'Too many hints...';
  strTooManyWarnings = 'Too many warnings...';
  strTooManyErrors = 'Too many errors...';

Const
  recIssues: Array [Low(TErrorType) .. High(TErrorType)] Of TIssueRec = ((FFolder: strHints;
    FFolderImage: iiHintFolder; FItemImage: iiHint; FTooMany: strTooManyHints),
    (FFolder: strWarnings; FFolderImage: iiWarningFolder; FItemImage: iiWarning;
    FTooMany: strTooManyWarnings), (FFolder: strErrors; FFolderImage: iiErrorFolder;
    FItemImage: iiError; FTooMany: strTooManyErrors));

Var
  i: TElementContainer;
  iCount: Integer;
  iIssueLimit: Integer;

Begin
  Case ErrorType Of
    etHint:
      If Not(doShowHints In BrowseAndDocItOptions.Options) Then
        Exit;
    etWarning:
      If Not(doShowWarnings In BrowseAndDocItOptions.Options) Then
        Exit;
    etError:
      If Not(doShowErrors In BrowseAndDocItOptions.Options) Then
        Exit;
  End;
  If Comment <> Nil Then
    Begin
      Case ErrorType Of
        etError:
          If Comment.FindTag('noerror') > -1 Then
            Exit;
        etHint:
          If Comment.FindTag('nohint') > -1 Then
            Exit;
        etWarning:
          If Comment.FindTag('nowarning') > -1 Then
            Exit;
      End;
    End;
  i := FindRoot.Add(recIssues[ErrorType].FFolder, recIssues[ErrorType].FFolderImage, scNone, Nil);
  iCount := i.ElementCount;
  Case ErrorType Of
    etError:
      iIssueLimit := BrowseAndDocItOptions.IssueLimits[ltErrors];
    etWarning:
      iIssueLimit := BrowseAndDocItOptions.IssueLimits[ltWarnings];
    etHint:
      iIssueLimit := BrowseAndDocItOptions.IssueLimits[ltHints];
  Else
    iIssueLimit := BrowseAndDocItOptions.IssueLimits[ltErrors];
  End;
  If iCount < iIssueLimit Then
    i.Add(TDocIssue.Create(strMsg, AScope, strMethod, iLine, iCol, recIssues[ErrorType].FItemImage))
  Else If iCount = iIssueLimit Then
    i.Add(TDocIssue.Create(recIssues[ErrorType].FTooMany, scNone, 'AddIssue', 0, 0,
      recIssues[ErrorType].FItemImage));
End;

(**

  This methof adds the given elements tokens to the current containers tokens.

  @precon  None.
  @postcon Adds the given elements tokens to the current containers tokens.

  @param   AElement as a TElementContainer

**)
Procedure TElementContainer.AddTokens(AElement: TElementContainer);

Var
  i: Integer;

Begin
  Assert(AElement <> Nil, 'Can not add a null element to the collection!');
  For i := 0 To AElement.TokenCount - 1 Do
    AppendToken(AElement.Tokens[i]);
End;

(**

  This method attempts to adds an element to the collection but raises an error IF there
  is already an object of the same name in the collection (duplicate detection).

  @precon  None.
  @postcon Attempts to adds an element to the collection but raises an error IF there is
           already an object of the same name in the collection (duplicate detection).

  @param   AElement as a TElementContainer
  @return  a TElementContainer

**)
Function TElementContainer.AddUnique(AElement: TElementContainer): TElementContainer;

ResourceString
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';

Var
  iLine, iCol: Integer;
  strI: String;

Begin
  iLine := AElement.Line;
  iCol := AElement.Column;
  strI := AElement.Identifier;
  Result := Add(AElement);
  If Result <> AElement Then
    AddIssue(Format(strDuplicateIdentifierFound, [strI, iLine, iCol]), scNone, 'AddUnique', iLine,
      iCol, etError);
End;

(**

  This method copies the tokens from the source into this element replacing any
  token that already exist.

  @precon  Source must be a valid TElementContainer.
  @postcon Copies the tokens from the source into this element replacing any
           token that already exist.

  @param   Source as a TElementContainer

**)
Procedure TElementContainer.Assign(Source: TElementContainer);

Var
  iToken: Integer;

Begin
  Name := Source.Name;
  FScope := Source.FScope;
  Line := Source.Line;
  Column := Source.Column;
  FComment := Source.Comment;
  FImageIndex := Source.FImageIndex;
  ClearTokens;
  For iToken := 0 To Source.TokenCount - 1 Do
    AppendToken(Source.Tokens[iToken]);
End;

(**

  This method recrusively checks the documentation of the module. Descendants
  need to override this to implement document checking.

  @precon  None.
  @postcon Recrusively checks the documentation of the module.

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

  This is the constructor method for the TElementContainer class.

  @precon  AComment must be either nil or a valid TComment instance.
  @postcon Creates an instance of the element container.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TElementContainer.Create(const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, iLine, iColumn);
  FElements := TObjectList.Create(True);
  FComment := AComment;
  FScope := AScope;
  FImageIndex := AImageIndex;
  FSorted := True;
  FReferenced := False;
  FParent := Nil;
End;

(**

  This method deletes the indexed element from the collection.

  @precon  iIndex must be a valid index between 1 and ElementCount.
  @postcon Deletes the indexed element from the collection.

  @param   iIndex as an Integer

**)
Procedure TElementContainer.DeleteElement(iIndex: Integer);

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
  FElements.Free;
  Inherited Destroy;
End;

(**

  This method returns the position of the named container in the current
  containers collection if found else returns the position (as a negative)
  where the item should be inserted in the collection.

  @precon  None.
  @postcon Returns the position of the named container in the current
           containers collection if found else returns the position (as a
           negative) where the item should be inserted in the collection.

  @param   strName  as a String as a constant
  @param   FindType as a TFindType
  @return  an Integer

**)
Function TElementContainer.Find(const strName: String; FindType: TFindType = ftName): Integer;

Var
  iFirst: Integer;
  iMid: Integer;
  iLast: Integer;
  iResult: Integer;

Begin
  Result := -1;
  If FSorted Then
    Begin // Binary search...
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
    End
  Else
    Begin // Sequential search...
      For iFirst := 1 To ElementCount Do
        If CompareText(Elements[iFirst].Name, strName) = 0 Then
          Begin
            Result := iFirst;
            Break;
          End;
      iFirst := ElementCount + 1;
    End;
  If Result < 0 Then
    Result := -iFirst;
End;

(**

  This method searches the elements collection of and instance matching the
  given name.

  @precon  None.
  @postcon Returns either instance of the found item or returns nil.

  @param   strName  as a String as a constant
  @param   FindType as a TFindType
  @return  a TElementContainer

**)
Function TElementContainer.FindElement(const strName: String; FindType: TFindType = ftName)
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

  This function finds the occurance of the token and returns its index if found
  else returns -1.

  @precon  None.
  @postcon Finds the occurance of the token and returns its index if found
           else returns -1.

  @param   strToken as a String as a constant
  @return  an Integer

**)
Function TElementContainer.FindToken(const strToken: String): Integer;

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

  This method returns the adjusted image index the element based on the scope.

  @precon  None.
  @postcon Returns the adjusted image index the element based on the scope.

  @return  an Integer

**)
Function TElementContainer.GetImageIndexAdjustedForScope: Integer;

Begin
  Case FScope Of
    scPrivate:
      Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 1;
    scPublished:
      Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 2;
    scProtected:
      Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 3;
    scLocal:
      Result := Integer(ImageIndex) - Integer(Succ(iiNone)) + 4;
  Else
    // scPublic, scGlobal, scNone
    Result := Integer(ImageIndex) - Integer(Succ(iiNone));
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

  @param   iIndex as an Integer
  @return  a TElementContainer

**)
Function TElementContainer.GetElements(iIndex: Integer): TElementContainer;
Begin
  Result := FElements[iIndex - 1] As TElementContainer;
End;

(**

  This method searches for references to the passed symbol in the passed
  section.

  @precon  None.
  @postcon Returns true if the symbol was found.

  @param   AToken  as a TTokenInfo
  @param   Section as a TLabelContainer
  @return  a Boolean

**)
Function TElementContainer.ReferenceSection(AToken: TTokenInfo; Section: TLabelContainer): Boolean;

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

  This method does nothing other than call its parents ReferenceSymbol method.
  Descendant should override this method to resolve symbols in the code.

  @precon  None.
  @postcon Passes the processing of the symbol to its parent IF the parent
           exists.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TElementContainer.ReferenceSymbol(AToken: TTokenInfo): Boolean;

Begin
  Result := False;
  If FParent <> Nil Then
    Result := FParent.ReferenceSymbol(AToken);
End;

(**


  This method recursively checks the referenced property and outputs a hint if
  any element is not refrernced which has a scope of Local or Private.

  @precon  None.
  @postcon Recursively checks the referenced property and outputs a hint if
           any element is not refrernced which has a scope of Local or Private.


**)
Procedure TElementContainer.CheckReferences;

Var
  i: Integer;
  strIdentifier: String;
  E: TElementContainer;

Begin
  If doShowUnReferencedSymbols In BrowseAndDocItOptions.Options Then
    Begin
      If Scope In [scLocal, scPrivate] Then
        If Not Referenced Then
          Begin
            E := Self;
            If Identifier <> '' Then
              Begin
                While (E <> Nil) And (E.Parent <> Nil) Do
                  Begin
                    If Not(E Is TLabelContainer) Then
                      Begin
                        If strIdentifier <> '' Then
                          strIdentifier := '.' + strIdentifier;
                        strIdentifier := E.Identifier + strIdentifier;
                      End;
                    E := E.Parent;
                  End;
                AddIssue(Format(strUnreferencedLocal, [strIdentifier]), scNone, 'CheckReferences',
                  Line, Column, etHint);
              End;
          End;
      For i := 1 To ElementCount Do
        Elements[i].CheckReferences;
    End;
End;

(**

  This is a setter method for the Sorted property.

  @precon  None.
  @postcon Sets the class to be either sorted or not sorted. Must be set before
           adding elements to the collection.

  @param   boolValue as a Boolean

**)
Procedure TElementContainer.SetSorted(boolValue: Boolean);
Begin
  Assert(ElementCount = 0, 'Can not set sorted after adding elements.');
  FSorted := boolValue;
End;

{ TLabelContainer }

(**


  This is an overridden constructor to ensure that label are not displayed in
  the unreferenced scope hints.

  @precon  None.
  @postcon Overridden constructor to ensure that label are not displayed in
           the unreferenced scope hints.


  @param   AScope      as a TScope
  @param   strName     as a String as a constant
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TBADIImageIndex
  @param   AComment    as a TComment

**)
Constructor TLabelContainer.Create(const strName: String; AScope: TScope; iLine, iColumn: Integer;
  AImageIndex: TBADIImageIndex; AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  Referenced := True;
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the name of the label as a string .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TLabelContainer.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
Begin
  Result := Name;
End;

End.
