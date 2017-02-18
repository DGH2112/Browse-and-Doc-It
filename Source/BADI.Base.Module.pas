(**

  This module contains the base class for all language module to derived from
  and all standard constants across which all language modules have in common.

  @Date    18 Feb 2017
  @Version 1.0

  @Author  David Hoyle

**)
Unit BADI.Base.Module;

Interface

Uses
  Classes,
  Contnrs,
  BADI.Types,
  BADI.TokenInfo,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.CompilerConditionStack;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is an abtract class from which all language modules should be
      derived. **)
  TBaseLanguageModule = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOwnedItems : TObjectList;
    FTokenIndex : TTokenIndex;
    FDocErrors: TElementContainer;
    FTickList : TObjectList;
    FModuleName : String;
    FBodyComment : TObjectList;
    FModuleNameCol: Integer;
    FModuleNameLine: Integer;
    FFileName: String;
    FModified : Boolean;
    FCompilerDefs : TStringList;
    FPreviousTokenIndex : TTokenIndex;
    FCompilerConditionStack : TCompilerConditionStack;
    FCompilerConditionUndoStack : TCompilerConditionStack;
    FLastComment: TTokenInfo;
    FCommentClass : TCommentClass;
    FShouldUndoCompilerStack: Boolean;
    FLastBodyCommentLine: Integer;
    FModuleOptions : TModuleOptions;
    FTokenStack : TArrayOfInteger;
    FTokenStackTop : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetToken : TTokenInfo;
    function GetOpTickCountName(iIndex: Integer): String;
    function GetOpTickCountByIndex(iIndex: Integer): Double;
    function GetOpTickCounts: Integer;
    function GetOpTickCount(const strStart, strFinish : String): Double;
    Function GetBodyComment(iIndex : Integer) : TComment;
    Function GetBodyCommentCount : Integer;
    Function PrevToken : TTokenInfo;
    Procedure NextToken;
    Procedure PreviousToken;
    Function EndOfTokens : Boolean;
    Procedure NextNonCommentToken; Virtual;
    Procedure RollBackToken; deprecated;
    Procedure PushTokenPosition;
    Procedure PopTokenPosition;
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Virtual; Abstract;
    Procedure SetTokenIndex(iIndex : TTokenIndex);
    procedure AppendToLastToken(const strToken : String);
    procedure ProcessCompilerDirective(var iSkip : Integer); Virtual; Abstract;
    Function  GetModuleName : String; Virtual;
    function  GetBytes: Int64;
    function  GetLines: Integer;
    Procedure ErrorAndSeekToken(const strMsg, strMethod, strParam : String;
      SeekTokens: Array Of String; SeekToken : TSeekToken);
    Function GetHighPerformanceTickCount : Double;
    (**
      Returns a refernce the to owned items collection. This is used to manage
      the life time of all the ident lists and comments found in the module.
      @precon  None.
      @postcon Returns a refernce the to owned items collection. This is used to
               manage the life time of all the ident lists and comments found in
               the module.
      @return  a TObjectList
    **)
    Property OwnedItems : TObjectList Read FOwnedItems;
    (**
      Returns the current token with in the module. Also see
      {@link TPascalDocModule.SetTokenIndex SetTokenIndex}.
      @precon  None.
      @postcon Returns the current token with in the module. Also see
               {@link TPascalDocModule.SetTokenIndex SetTokenIndex}.
      @return  a TTokenInfo
    **)
    Property Token : TTokenInfo Read GetToken;
    (**
      This property provide access to a list of compiler defines as a string
      list.
      @precon  None.
      @postcon Provide a string list of compiler defines {$DEFINE xxxxx}
      @return  a TStringList
    **)
    Property CompilerDefines : TStringList Read FCompilerDefs;
    (**
      This property returns the current index of the current Token.
      @precon  None.
      @postcon Returns the current index of the current Token.
      @return  a TTokenIndex
    **)
    Property TokenIndex : TTokenIndex Read FTokenIndex;
    (**
      This property returns the comment class type for the parser.
      @precon  None.
      @postcon Returns the comment class type for the parser.
      @return  a TCommentClass
    **)
    Property CommentClass : TCommentClass Read FCommentClass Write FCommentClass;
  Public
    Constructor CreateParser(const Source, strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Virtual;
    Destructor Destroy; Override;
    Procedure AddTickCount(const strLabel : String);
    Procedure AddDef(const strDef : String);
    Procedure DeleteDef(const strDef : String);
    Function  IfDef(const strDef : String) : Boolean;
    Function  IfNotDef(const strDef : String) : Boolean;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
    Function  ReservedWords : TKeyWords; Virtual; Abstract;
    Function  Directives : TKeyWords; Virtual; Abstract;
    Function  AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure AddToExpression(Container : TElementContainer);
    function  IsToken(const strToken: String; Container: TElementContainer): Boolean;
    Procedure AddBodyComment(C : TComment);
    Function  DefaultProfilingTemplate : String; Virtual;
    { Properties }
    (**
      This property returns the tick count time between the 2 named tick counts previously stored
      using the AddTickCount() method.
      @precon  None.
      @postcon Returns the time between two counter if both the names are found.
      @param   strFinish as a String as a Constant
      @param   strStart  as a String as a Constant
      @return  a Double
    **)
    Property OpTickCount[const strStart, strFinish : String] : Double Read GetOpTickCount;
    (**
      Thie property returns the number of operation tick counter storeed in the
      collection.
      @precon  None.
      @postcon Returns the number of operation tick counter storeed in the
               collection.
      @return  an Integer
    **)
    Property OpTickCounts : Integer Read GetOpTickCounts;
    (**
      This property returns the tick count associated with the indexed item.
      @precon  iIndex must be a valid index between 0 and OpTickCount - 1.
      @postcon Returns the tick count associated with the indexed item.
      @param   iIndex as an Integer
      @return  a Double
    **)
    Property OpTickCountByIndex[iIndex : Integer] : Double Read GetOpTickCountByIndex;
    (**
      This property returns the name associated with the indexed item.
      @precon  iIndex must be a valid index between 0 and OpTickCount - 1.
      @postcon Returns the name associated with the indexed item.
      @param   iIndex as       an Integer
      @return  a String
    **)
    Property OpTickCountName[iIndex : Integer] : String Read GetOpTickCountName;
    (**
      Returns the module name as a string.
      @precon  None.
      @postcon Returns the module name as a string.
      @return  a String
    **)
    Property ModuleName : String Read GetModuleName Write FModuleName;
    (**
      Returns the specific indexed body comment from the collection.
      @precon  None.
      @postcon Returns the specific indexed body comment from the collection.
      @param   iIndex as       an Integer
      @return  a TComment
    **)
    Property BodyComment[iIndex : Integer] : TComment Read GetBodyComment;
    (**
      Returns a reference to the modules body comments collection.
      @precon  None.
      @postcon Returns a reference to the modules body comments collection.
      @return  an Integer
    **)
    Property BodyCommentCount : Integer Read GetBodyCommentCount;
    (**
      Returns the line number of the modules name.
      @precon  None.
      @postcon Returns the line number of the modules name.
      @return  an Integer
    **)
    Property ModuleNameLine : Integer Read FModuleNameLine Write FModuleNameLine;
    (**
      Returns the column number of the module name.
      @precon  None.
      @postcon Returns the column number of the module name.
      @return  an Integer
    **)
    Property ModuleNameCol : Integer Read FModuleNameCol Write FModuleNameCol;
    (**
      This property returns the file name of the module as passed to the
      constructor.
      @precon  None.
      @postcon This property returns the file name of the module as passed to the
      constructor.
      @return  a String
    **)
    Property FileName : String Read FFileName;
    (**
      This property returns whether the source code is modified or not.
      @precon  None.
      @postcon This property returns whether the source code is modified or not.
      @return  a Boolean
    **)
    Property Modified : Boolean Read FModified;
    (**
      This property defines a compiler condition stack for use in the
      ProcessCompilerDefintions method.
      @precon  None.
      @postcon Provides access to the compiler condition stack.
      @return  a TCompilerConditionStack
    **)
    Property CompilerConditionStack : TCompilerConditionStack
      Read FCompilerConditionStack;
    (**
      This property defines a compiler condition undo stack for use in the
      ProcessCompilerDefintions method.
      @precon  None.
      @postcon Provides access to the compiler condition undo stack.
      @return  a TCompilerConditionStack
    **)
    Property CompilerConditionUndoStack : TCompilerConditionStack
      Read FCompilerConditionUndoStack;
    (**
      This property returns the number of bytes in the file.
      @precon  None.
      @postcon Returns the number of bytes in the file.
      @return  an Int64
    **)
    Property Bytes : Int64 Read GetBytes;
    (**
      This property returns the number of lines in the file.
      @precon  None.
      @postcon Returns the number of lines in the file.
      @return  an Integer
    **)
    Property Lines : Integer Read GetLines;
    (**
      This property exposes the Module Options for the module.
      @precon  None.
      @postcon Returns the Module Options for the module.
      @return  a TModuleOptions
    **)
    Property ModOptions : TModuleOptions Read FModuleOptions;
  End;

  (** A class type to define classes in the record structure. **)
  TBaseLanguageModuleClass = Class Of TBaseLanguageModule;

Implementation

Uses
  SysUtils,
  Windows,
  DGHLibrary,
  INIFiles,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Options,
  BADI.Comment.Tag,
  BADI.Constants,
  BADI.TickOption;

(**

  This method adds the comment to the comment collection if it has content
  and is more than 1 line different from the last added comemnt, else appends
  the contents of the comment to the last added comment and frees the passed
  comment.

  @precon  None.
  @postcon Adds the comment to the comment collection if it has content
           and is more than 1 line different from the last added comemnt, else
           appends the contents of the comment to the last added comment and
           frees the passed comment.

  @param   C as a TComment

**)
procedure TBaseLanguageModule.AddBodyComment(C: TComment);

var
  Cmt: TComment;

begin
  If C <> Nil Then
    If (C.TokenCount > 0) Or (C.TagCount > 0) Then
      Begin
        If FBodyComment.Count > 0 Then
          Begin
            Cmt := BodyComment[BodyCommentCount - 1];
            If FLastBodyCommentLine + 1 = C.Line Then
              Begin
                Cmt.AppendComment(Cmt, C);
                Cmt.TrimTrailingWhiteSpace;
                FLastBodyCommentLine := C.Line;
                C.Free;
              End Else
              Begin
                FBodyComment.Add(C);
                FLastBodyCommentLine := C.Line;
              End;
          End Else
          Begin
            FBodyComment.Add(C);
            FLastBodyCommentLine := C.Line;
          End;
      End Else
        C.Free;
end;

(**

  This method adds a Compiler Definition to the sources internal list.

  @precon  None.
  @postcon Adds a Compiler Definition to the sources internal list.

  @param   strDef as a String as a Constant

**)
procedure TBaseLanguageModule.AddDef(const strDef : String);

begin
  FCompilerDefs.Add(strDef);
end;

(**

  This method adds a timer count to the modules OpTickCount collection. This
  can be used to provide timing / profiling information on operations.

  @precon  None.
  @postcon Adds a timer count to the modules OpTickCount collection. This
           can be used to provide timing / profiling information on operations.

  @param   strLabel as a String as a Constant

**)
procedure TBaseLanguageModule.AddTickCount(const strLabel: String);

begin
  FTickList.Add(TTickOption.Create(strLabel, GetHighPerformanceTickCount));
end;

(**

  This is the constructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Initialise this base class and Tokensizes the passed stream of
           characters.

  @param   Source        as a String as a Constant
  @param   strFileName   as a String as a Constant
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
constructor TBaseLanguageModule.CreateParser(const Source, strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

begin
  Inherited Create(strFileName, scGlobal, 0, 0, iiModule, Nil);
  FModuleOptions := ModuleOptions;
  FFileName := strFileName;
  FModified := IsModified;
  FOwnedItems := TObjectList.Create(True);
  FTokenIndex := 0;
  FPreviousTokenIndex := -1;
  FTickList := TObjectList.Create(True);
  FBodyComment := TObjectList.Create(True);
  FModuleName := strFileName;
  FModuleNameCol := 0;
  FModuleNameLine := 0;
  FCompilerDefs := TStringList.Create;
  FCompilerDefs.Duplicates := dupIgnore;
  {$IFDEF D0006}
  FCompilerDefs.CaseSensitive := False;
  {$ENDIF}
  FCompilerDefs.Sorted := True;
  FCompilerConditionStack := TCompilerConditionStack.Create;
  FCompilerConditionUndoStack := TCompilerConditionStack.Create;
  FCommentClass := CommentClass;
  FTokenStackTop := -1;
  SetLength(FTokenStack, 10);
end;

(**

  This method returns the default profilin template for this module.

  @precon  None.
  @postcon Returns the default profilin template for this module.

  @return  a String

**)
function TBaseLanguageModule.DefaultProfilingTemplate: String;

begin
  Result := '$METHODCODE$';
end;

(**

  This method deletes a definition from the source compiler definitions list.

  @precon  None.
  @postcon Deletes a definition from the source compiler definitions list.

  @param   strDef as a String as a Constant

**)
procedure TBaseLanguageModule.DeleteDef(const strDef : String);

Var
  iIndex : Integer;

begin
  If FCompilerDefs.Sorted Then
    FCompilerDefs.Find(strDef, iIndex)
  Else
    iIndex := FCompilerDefs.IndexOf(strDef);
  If iIndex > -1 Then
    FCompilerDefs.Delete(iIndex);
end;

(**

  This is the destructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Frees the memory used by all the collections.

**)
destructor TBaseLanguageModule.Destroy;
begin
  FCompilerConditionUndoStack.Free;
  FCompilerConditionStack.Free;
  FCompilerDefs.Free;
  FBodyComment.Free;
  FTickList.Free;
  FDocErrors.Free;
  FOwnedItems.Free;
  inherited;
end;

(**

  This method appends the pased token string to the previous token.

  @precon  None.
  @postcon Appends the pased token string to the previous token.

  @param   strToken as a String as a Constant

**)
Procedure TBaseLanguageModule.AppendToLastToken(const strToken : String);

Begin
  Tokens[TokenCount - 1].Append(strToken);
End;

(**

  This method checks to see the the given definition exists in the source list.

  @precon  None.
  @postcon Returns true if the definition exists.

  @param   strDef as a String as a Constant
  @return  a Boolean

**)
function TBaseLanguageModule.IfDef(const strDef : String) : Boolean;

Var
  iIndex : Integer;

Begin
  If FCompilerDefs.Sorted Then
    Result := FCompilerDefs.Find(strDef, iIndex)
  Else
    Result := (FCompilerDefs.IndexOf(strDef) > - 1);
End;

(**

  This method checks to see if a definition DOES NOT exist in the list.

  @precon  None.
  @postcon Returns true if the definition does not exist.

  @param   strDef as a String as a Constant
  @return  a Boolean

**)
function TBaseLanguageModule.IfNotDef(const strDef : String) : Boolean;

begin
  Result := Not IfDef(strDef);
end;

(**

  This is a getter method for the Lines property.

  @precon  None.
  @postcon Returns the number Lines in the file.

  @return  an Integer

**)
function TBaseLanguageModule.GetLines: Integer;
begin
  Result := 0;
  If TokenCount > 0 Then
    Result := (Tokens[TokenCount - 1] As TTokenInfo).Line;
end;

(**

  This is a setter method for the TokenIndex property.

  @precon  iIndex is the token index to set the parse to start at.
  @postcon Sets the TokenIndex position.

  @param   iIndex as a TTokenIndex

**)
Procedure TBaseLanguageModule.SetTokenIndex(iIndex : TTokenIndex);

Begin
  FTokenIndex := iIndex;
End;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Override and default GetAsString method and returns the name of the
           module.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TBaseLanguageModule.AsString(boolShowIdentifier,
     boolForDocumentation : Boolean) : String;

begin
  Result := ExtractFileName(Name);
end;

(**

  This method adds the current toen to the passed generic container if it is not
  nil and moves to the next non comment token.

  @precon  None.
  @postcon Adds the current toen to the passed generic container if it is not
           nil and moves to the next non comment token.

  @param   Container as a TElementContainer

**)
Procedure TBaseLanguageModule.AddToExpression(Container : TElementContainer);

Begin
  If Container <> Nil Then
    Container.AppendToken(Token);
  NextNonCommentToken;
End;

(**

  This method check the current token against the passed string and if true
  returns true and addeds the token to the generic container.

  @precon  None.
  @postcon Check the current token against the passed string and if true
           returns true and addeds the token to the generic container.

  @param   strToken  as a String as a Constant
  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TBaseLanguageModule.IsToken(const strToken : String; Container : TElementContainer): Boolean;

Begin
  Result := strToken = Token.Token;
  If Result Then
    AddToExpression(Container);
End;

(**

  This is a getter method for the BodyComment property.

  @precon  iIndex is the index of the body comment required.
  @postcon Return the requested comment object.

  @param   iIndex as an Integer
  @return  a TComment

**)
Function TBaseLanguageModule.GetBodyComment(iIndex : Integer) : TComment;

Begin
  Result := FBodyComment[iIndex] As TComment;
End;

(**

  This is a getter method for the BodyCommentCount property.

  @precon  None.
  @postcon Returns the number of body comment in the collection.

  @return  an Integer

**)
Function TBaseLanguageModule.GetBodyCommentCount : Integer;

Begin
  Result := FBodyComment.Count;
End;

(**

  This is a getter method for the Bytes property.

  @precon  None.
  @postcon Returns the number of bytes in the file.

  @return  an Int64

**)
function TBaseLanguageModule.GetBytes: Int64;

begin
  Result := 0;
  If TokenCount > 0 Then
    Result := (Tokens[TokenCount - 1] As TTokenInfo).BufferPos +
      (Tokens[TokenCount - 1] As TTokenInfo).Length - 1;
end;

(**

  This method returns a high performance tick count from the system for accurate time measurement.

  @precon  None.
  @postcon Returns the current tick count in milliseconds.

  @return  a Double

**)
Function TBaseLanguageModule.GetHighPerformanceTickCount: Double;

Var
  t, f: Int64;

Begin
  QueryPerformanceCounter(t);
  QueryPerformanceFrequency(f);
  Result := 1000.0 * Int(t) / Int(f);
End;

(**

  This is a getter method for the ModuleName property.

  @precon  None.
  @postcon Override this method to change its appearance.

  @return  a String

**)
Function TBaseLanguageModule.GetModuleName : String;

Begin
  Result := FModuleName;
End;


(**

  This is a getter method for the OpTickCount property.

  @precon  None.
  @postcon If both the start and end token are found in the collection of Tick Counts then the
           number of Tick Counts between them are returned.

  @param   strStart  as a String as a Constant
  @param   strFinish as a String as a Constant
  @return  a Double

**)
function TBaseLanguageModule.GetOpTickCount(const strStart, strFinish : String): Double;

Var
  i : Integer;
  iStart, iFinish : Integer;

begin
  Result := -1;
  iStart := 0;
  iFinish := 0;
  For i := 0 To FTickList.Count - 1 Do
    Begin
      If CompareText((FTickList[i] As TTickOption).Name, strStart) = 0 Then
        iStart := i;
      If CompareText((FTickList[i] As TTickOption).Name, strFinish) = 0 Then
        iFinish := i;
    End;
  If (iStart > -1) And (iFinish > -1) Then
    Result :=
      (FTickList[iFinish] As TTickOption).TickCount - (FTickList[iStart] As TTickOption).TickCount;
end;

(**

  This is a getter method for the OpTickCountByIndex property.

  @precon  iIndex must be a valid index.
  @postcon Returns the tick count associated with the passed index.

  @param   iIndex as an Integer
  @return  a Double

**)
function TBaseLanguageModule.GetOpTickCountByIndex(iIndex: Integer): Double;

begin
  Result := (FTickList[iIndex] As TTickOption).TickCount;
end;

(**

  This is a getter method for the OpTickCountName property.

  @precon  iIndex must be a valid integer index.
  @postcon Returns the name of the OpTickCount references by the index passed.

  @param   iIndex as an Integer
  @return  a String

**)
function TBaseLanguageModule.GetOpTickCountName(iIndex: Integer): String;

begin
  Result := (FTickList[iIndex] As TTickOption).Name;
end;

(**

  This is a getter method for the OpTickCounts property.

  @precon  None.
  @postcon Returns the number of items in the OpTickCount collection.

  @return  an Integer

**)
function TBaseLanguageModule.GetOpTickCounts: Integer;
begin
  Result := FTickList.Count;
end;

(**

  This is a getter method for the Token property.

  @precon  None.
  @postcon Returns a token info object for the current token.

  @return  a TTokenInfo

**)
Function TBaseLanguageModule.GetToken : TTokenInfo;

Begin
  If FTokenIndex >= TokenCount Then
    Begin
      AddIssue(strUnExpectedEndOfFile, scNone, 'GetToken', 0, 0, etError);
      Raise EBADIParserAbort.Create('Parsing Aborted!');
    End;
  Result := Tokens[FTokenIndex] As TTokenInfo;
End;

(**

  This method moves the toke to the next token in the token list or raises an
  EDocException.

  @precon  None.
  @postcon Moves the token to the next token in the token list or raises an
           EDocException.

**)
Procedure TBaseLanguageModule.NextToken;

begin
  Inc(FTokenIndex);
end;

(**

  This method checks for the end of the token list and returns true if it is
  found.

  @precon  None.
  @postcon Returns true is we are beyond the end of the token collection.

  @return  a Boolean

**)
Function TBaseLanguageModule.EndOfTokens : Boolean;

Begin
  Result := (Token.TokenType = ttFileEnd) Or (FTokenIndex >= TokenCount);
End;

(**

  This method seeks the first non-comment token in the source code which match
  one of the passed tokens.

  @precon  The Tokens passed MUST be sorted in lowercase and in ascending order.
  @postcon Seeks the first non-comment token in the source code which match
           one of the passed tokens.

  @param   strMsg      as a String as a Constant
  @param   strMethod   as a String as a Constant
  @param   strParam    as a String as a Constant
  @param   SeekTokens  as an Array Of string
  @param   SeekToken   as a TSeekToken

**)
Procedure TBaseLanguageModule.ErrorAndSeekToken(const strMsg, strMethod, strParam : String;
  SeekTokens: Array Of String; SeekToken : TSeekToken);

  (**

    This method counts the number of occurrances of "%s" in the string and
    returns that number.

    @precon  None.
    @postcon Returns the number of string parameters in the text.

    @param   strText as a String
    @return  an Integer

  **)
  Function StringCount(strText : String) : Integer;

  Var
    i : Integer;

  Begin
    Result := 0;
    For i := 1 To Length(strText) - 1 Do
      If Copy(strText, i, 2) = '%s' Then Inc(Result);
  End;

Begin
  Case StringCount(strMsg) Of
    0: AddIssue(Format(strMsg, [Token.Line, Token.Column]),
           scGlobal, strMethod, Token.Line, Token.Column, etError);
    1: AddIssue(Format(strMsg, [strParam, Token.Line, Token.Column]),
           scGlobal, strMethod, Token.Line, Token.Column, etError);
    2: AddIssue(Format(strMsg, [strParam, Token.Token, Token.Line,
         Token.Column]), scGlobal, strMethod, Token.Line, Token.Column, etError);
  Else
    AddIssue(strNotEnoughStrings, scGlobal, strMethod, Token.Line, Token.Column, etError);
  End;
  NextNonCommentToken;
  While Not IsKeyWord(Token.Token, SeekTokens) Do
    NextNonCommentToken;
  If SeekToken = stFirst Then
    NextNonCommentToken;
End;

(**

  This method move the token position to the next non comment token.

  @precon  None.
  @postcon Move the token position to the next non comment token.

**)
procedure TBaseLanguageModule.NextNonCommentToken;

Var
  boolContinue : Boolean;
  iSkip : Integer;
  C : TComment;

begin
  iSkip := 0;
  FShouldUndoCompilerStack := False;
  // Catch first token as directive
  If Token.TokenType = ttCompilerDirective Then
    Begin
      ProcessCompilerDirective(iSkip);
      FShouldUndoCompilerStack := True;
    End;
  Repeat
    // Get body comments and add to collection
    If (Token.TokenType In [ttLineComment, ttBlockComment]) And
      (FLastComment <> Token) And (FCommentClass <> Nil) Then
      Begin
        C := FCommentClass.CreateComment(Token.Token,
          Token.Line, Token.Column);
        If C <> Nil Then
          Begin
            AddBodyComment(C);
            FLastComment := Token;
          End;
      End;
    If Not (Tokens[FTokenIndex].TokenType In [ttLineComment, ttBlockComment,
      ttCompilerDirective]) And (iSkip = 0) Then
      FPreviousTokenIndex := FTokenIndex;
    NextToken;
    If Token.TokenType = ttCompilerDirective Then
      Begin
        ProcessCompilerDirective(iSkip);
        FShouldUndoCompilerStack := True;
      End;
    boolContinue := (
      (
        Token.TokenType In [ttLineComment, ttBlockComment, ttCompilerDirective]
      ) And
      Not EndOfTokens
    ) Or (iSkip > 0)
  Until Not boolContinue;
end;

(**

  This method removes the token position from the stack and sets the token position to
  that value.

  @precon  None.
  @postcon The token is moved back to the position of the token  on the top of the stack
           and the stack is decremented.

**)
Procedure TBaseLanguageModule.PopTokenPosition;

Begin
  If FTokenStackTop > -1 Then
    Begin
      FTokenIndex := FTokenStack[FTokenStackTop];
      Dec(FTokenStackTop);
      While CompilerConditionUndoStack.CanPop And
        (CompilerConditionUndoStack.Peek.TokenIndex > FTokenIndex) Do
        Begin
          CompilerConditionStack.Push(CompilerConditionUndoStack.Peek);
          CompilerConditionUndoStack.Pop;
        End;
    End Else
      Raise EParserError.Create(strCannotPopCompilerCondition);
End;

(**

  This method moves the toke to the previous token in the token list or raises
  an EDocException.

  @precon  None.
  @postcon Moves the token to the previous token in the token list or raises an
           EDocException.

**)
procedure TBaseLanguageModule.PreviousToken;
begin
  Dec(FTokenIndex);
end;

(**

  This method returns the previous token in the token list, else returns nil.

  @precon  None.
  @postcon Returns a token info object for the previous non comment token.

  @return  a TTokenInfo

**)
Function TBaseLanguageModule.PrevToken : TTokenInfo;

Var
  i : Integer;

begin
  Result := Nil;
  If FPreviousTokenIndex >= 0 Then
    Result := Tokens[FPreviousTokenIndex] As TTokenInfo
  Else
    For i := FTokenIndex - 1 DownTo 0 Do
      If Not ((Tokens[i] As TTokenInfo).TokenType In [ttLineComment,
        ttBlockComment, ttCompilerDirective]) Then
        Begin
          Result := Tokens[i] As TTokenInfo;
          Break;
        End;
end;

(**

  This method pushes the current token position on to the top of a stack.

  @precon  None.
  @postcon the current token position is pushed ont to the top of a stack.

**)
Procedure TBaseLanguageModule.PushTokenPosition;

Var
  T : TArrayOfInteger;
  i: Integer;

Begin
  Inc(FTokenStackTop);
  If FTokenStackTop > High(FTokenStack) Then
    Begin
      SetLength(T, Length(FTokenStack) + 10);
      For i := Low(FTokenStack) To High(FTokenStack) Do
        T[i] := FTokenStack[i];
      FTokenStack := T;
    End;
  FTokenStack[FTokenStackTop] := FTokenIndex;
End;

(**

  This method rolls back the current token to the previous valid token if there
  is one else searches for a previous token.

  @precon  None.
  @postcon Rolls back the current token to the previous valid token if there
           is one else searches for a previous token.

**)
Procedure TBaseLanguageModule.RollBackToken;

Begin
  If FShouldUndoCompilerStack Then
    Begin
      If CompilerConditionUndoStack.CanPop Then
        Begin
          CompilerConditionStack.Push(CompilerConditionUndoStack.Peek);
          CompilerConditionUndoStack.Pop;
        End;
    End;
  If FPreviousTokenIndex >= 0 Then
    FTokenIndex := FPreviousTokenIndex
  Else
    Begin
      Dec(FTokenIndex);
      While (FTokenIndex > 0) And (Tokens[FTokenIndex].TokenType In [ttLineComment,
        ttBlockComment, ttCompilerDirective]) Do
        Dec(FTokenIndex);
      If FTokenIndex < 0 Then
        Begin
          AddIssue(strUnExpectedStartOfFile, scNone, 'RollBackToken', 0, 0, etError);
          Raise EBADIParserAbort.Create('Parsing Aborted!');
        End;
    End;
End;

(**

  This method checks the module comment for various type of documentation
  errors.

  @precon  Module is the module to check.
  @postcon The modules comment is checked for errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TBaseLanguageModule.CheckDocumentation(var boolCascade : Boolean);

Var
  i : Integer;
  strDate : String;
  dtDate, dtFileDate : TDateTime;
  Tag : TTag;

Begin
  If Not (doShowConflicts In BrowseAndDocItOptions.Options) Then
    Exit;
  For i := 0 To BrowseAndDocItOptions.ExcludeDocFiles.Count -1 Do
    If Like(BrowseAndDocItOptions.ExcludeDocFiles[i], FFileName) Then
      Exit;
  If (Comment <> Nil) And (Comment.FindTag('stopdocumentation') >= 0) Then
    Begin
      boolCascade := False;
      Exit;
    End;
  If doShowUndocumentedModule In BrowseAndDocItOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([], ModuleNameLine, ModuleNameCol, Comment,
        strModuleDocumentation, DocConflictTable[dctModuleMissingDocumentation]);
  If Comment <> Nil Then
    Begin
      If (doShowMissingModuleDate In BrowseAndDocItOptions.Options) Then
        Begin
          i := Comment.FindTag('date');
          If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([FormatDateTime('dd mmm yyyy', Now)],
              ModuleNameLine, ModuleNameCol, Comment,
              strModuleDocumentation, DocConflictTable[dctModuleMissingDate])
          Else
            Begin
              Tag := Comment.Tag[i];
              strDate := Tag.AsString(80, False);
              If Modified Then
                dtFileDate := Now
              Else
                {$IFDEF D2006}
                FileAge(FileName, dtFileDate);
                {$ELSE}
                dtFileDate := FileDateToDateTime(FileAge(FileName));
                {$ENDIF}
              Try
                dtDate := ConvertDate(strDate);
                If Int(dtDate) <> Int(dtFileDate) Then
                  AddDocumentConflict([strDate, FormatDateTime('dd mmm yyyy', dtFileDate)],
                    Tag.Line, Tag.Column, Comment, strModuleDocumentation,
                    DocConflictTable[dctModuleIncorrectDate]);
              Except
                AddDocumentConflict([strDate, FormatDateTime('dd mmm yyyy', dtFileDate)],
                  Tag.Line, Tag.Column, Comment, strModuleDocumentation,
                  DocConflictTable[dctModuleCheckDateError]);
              End
            End;
        End;
      If (doShowMissingModuleVersion In BrowseAndDocItOptions.Options) Then
        Begin
          i := Comment.FindTag('version');
          If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, Comment,
              strModuleDocumentation, DocConflictTable[dctModuleMissingVersion])
        End;
      If (doShowMissingModuleAuthor In BrowseAndDocItOptions.Options) Then
        Begin
          i := Comment.FindTag('author');
          If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
            AddDocumentConflict([], ModuleNameLine, ModuleNameCol, Comment,
              strModuleDocumentation, DocConflictTable[dctModuleMissingAuthor])
        End;
    End;
  Inherited CheckDocumentation(boolCascade);
End;

End.
