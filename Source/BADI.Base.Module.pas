(**

  This module contains the base class for all language module to derived from
  and all standard constants across which all language modules have in common.

  @Author  David Hoyle
  @Version 2.688
  @Date    06 May 2021

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.Base.Module;

Interface

uses
  System.Classes,
  System.Contnrs,
  System.RegularExpressions,
  BADI.Options,
  BADI.Types,
  BADI.TokenInfo,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.CompilerConditionStack,
  BADI.Interfaces;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is an abstract class from which all language modules should be
      derived. **)
  TBaseLanguageModule = Class Abstract (TElementContainer)
  Strict Private
    FOwnedItems                 : TObjectList;
    FTokenIndex                 : TTokenIndex;
    FDocErrors                  : TElementContainer;
    FTickList                   : TObjectList;
    FModuleName                 : String;
    FBodyComment                : TObjectList;
    FFileName                   : String;
    FModified                   : Boolean;
    FCompilerDefs               : TStringList;
    FPreviousTokenIndex         : TTokenIndex;
    FCompilerConditionStack     : IBADICompilerConditionStack;
    FCompilerConditionUndoStack : IBADICompilerConditionStack;
    FLastComment                : TTokenInfo;
    FCommentClass               : TCommentClass;
    FShouldUndoCompilerStack    : Boolean;
    FLastBodyCommentLine        : Integer;
    FModuleOptions              : TModuleOptions;
    FTokenStack                 : TArrayOfInteger;
    FTokenStackTop              : Integer;
    FIdentifierList             : TStringList;
    FReplaceAmpersandRegEx      : TRegEx;
  Strict Protected
    Function  GetToken : TTokenInfo;
    function  GetOpTickCountName(Const iIndex: Integer): String;
    function  GetOpTickCountByIndex(Const iIndex: Integer): Double;
    function  GetOpTickCounts: Integer;
    function  GetOpTickCount(Const strStart, strFinish : String): Double;
    Function  GetBodyComment(Const iIndex : Integer) : TComment;
    Function  GetBodyCommentCount : Integer;
    Function  PrevToken : TTokenInfo;
    Procedure NextToken;
    Procedure PreviousToken;
    Function  EndOfTokens : Boolean;
    Procedure NextNonCommentToken; Virtual;
    Procedure ProcessBodyComments;
    Procedure RollBackToken; {$IFNDEF DEBUG} deprecated; {$ENDIF DEBUG}
    Procedure PushTokenPosition;
    Procedure PopTokenPosition;
    Function  GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Virtual; Abstract;
    Procedure SetTokenIndex(Const iIndex : TTokenIndex);
    procedure AppendToLastToken(Const strToken : String);
    Procedure CheckTagSpelling(Const Comment : TComment; Const strTagName : String);
    procedure ProcessCompilerDirective(); Virtual; Abstract;
    Function  GetModuleName : String; Virtual;
    function  GetBytes: Int64;
    function  GetLines: Integer;
    Procedure ErrorAndSeekToken(Const strMsg, strParam : String; Const SeekTokens: Array Of String;
      Const SeekToken : TSeekToken; Const Container : TElementContainer);
    Function GetHighPerformanceTickCount : Double;
    (**
      Returns a reference the to owned items collection. This is used to manage
      the life time of all the identifier lists and comments found in the module.
      @precon  None.
      @postcon Returns a reference the to owned items collection. This is used to
               manage the life time of all the identifier lists and comments found in
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
    Constructor CreateParser(Const Source, strFileName : String;
      Const IsModified : Boolean; Const ModuleOptions : TModuleOptions); Virtual;
    Destructor Destroy; Override;
    Procedure AddTickCount(Const strLabel : String);
    Procedure AddDef(Const strDef : String);
    Procedure DeleteDef(Const strDef : String);
    Function  IfDef(Const strDef : String) : Boolean;
    Function  IfNotDef(Const strDef : String) : Boolean;
    Procedure CheckDocumentation(Var boolCascade : Boolean); Override;
    Function  ReservedWords : TKeyWords; Virtual; Abstract;
    Function  Directives : TKeyWords; Virtual; Abstract;
    Function  AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure AddToExpression(Const Container : TElementContainer);
    function  IsToken(Const strToken: String; Const Container: TElementContainer): Boolean;
    Procedure AddBodyComment(Const C : TComment);
    Procedure CheckCommentSpelling; Virtual;
    Procedure CheckStringSpelling; Virtual;
    procedure CheckSpelling(Const strWord : String; Const AScope : TScope;
      Const eSpellingIssueType : TBADISpellingIssueType; Const iLine, iColumn : Integer;
      Const Comment : TComment);
    Procedure AddIdentifier(Const strIdentifier : String);
    Procedure ProcessLiteralsForSpelling(Const E: TElementContainer;
      Const eSpellingIssueType : TBADISpellingIssueType);
    Class Function  DefaultProfilingTemplate : String; Virtual;
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
    Property OpTickCount[Const strStart, strFinish : String] : Double Read GetOpTickCount;
    (**
      This property returns the number of operation tick counter stored in the
      collection.
      @precon  None.
      @postcon Returns the number of operation tick counter stored in the
               collection.
      @return  an Integer
    **)
    Property OpTickCounts : Integer Read GetOpTickCounts;
    (**
      This property returns the tick count associated with the indexed item.
      @precon  iIndex must be a valid index between 0 and OpTickCount - 1.
      @postcon Returns the tick count associated with the indexed item.
      @param   iIndex as an Integer as a constant
      @return  a Double
    **)
    Property OpTickCountByIndex[Const iIndex : Integer] : Double Read GetOpTickCountByIndex;
    (**
      This property returns the name associated with the indexed item.
      @precon  iIndex must be a valid index between 0 and OpTickCount - 1.
      @postcon Returns the name associated with the indexed item.
      @param   iIndex as an Integer as a constant
      @return  a String
    **)
    Property OpTickCountName[Const iIndex : Integer] : String Read GetOpTickCountName;
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
      @param   iIndex as an Integer as a constant
      @return  a TComment
    **)
    Property BodyComment[Const iIndex : Integer] : TComment Read GetBodyComment;
    (**
      Returns a reference to the modules body comments collection.
      @precon  None.
      @postcon Returns a reference to the modules body comments collection.
      @return  an Integer
    **)
    Property BodyCommentCount : Integer Read GetBodyCommentCount;
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
      ProcessCompilerDirective method.
      @precon  None.
      @postcon Provides access to the compiler condition stack.
      @return  a IBADICompilerConditionStack
    **)
    Property CompilerConditionStack : IBADICompilerConditionStack
      Read FCompilerConditionStack;
    (**
      This property defines a compiler condition undo stack for use in the
      ProcessCompilerDirective method.
      @precon  None.
      @postcon Provides access to the compiler condition undo stack.
      @return  a IBADICompilerConditionStack
    **)
    Property CompilerConditionUndoStack : IBADICompilerConditionStack
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
    (**
      This property defines the number of token pushes on the token position stack.
      @precon  None.
      @postcon Returns the number of token pushed on the token position stack.
      @return  an Integer
    **)
    Property TokenStackTop : Integer Read FTokenStackTop;
  End;

  (** A class type to define classes in the record structure. **)
  TBaseLanguageModuleClass = Class Of TBaseLanguageModule;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  System.StrUtils,
  System.INIFiles,
  Windows,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Comment.Tag,
  BADI.Constants,
  BADI.TickOption,
  BADI.Generic.Tokenizer;

Const
  (** A constant for the growth capacity of the compiler definitions stack. **)
  iStackCapacity = 10;

(**

  This method adds the comment to the comment collection if it has content and is more than 1 line 
  different from the last added comment, else appends the contents of the comment to the last added 
  comment and frees the passed comment.

  @precon  None.
  @postcon Adds the comment to the comment collection if it has content and is more than 1 line 
           different from the last added comment, else appends the contents of the comment to the last
           added comment and frees the passed comment.

  @param   C as a TComment as a constant

**)
procedure TBaseLanguageModule.AddBodyComment(Const C: TComment);

var
  Cmt: TComment;

begin
  If Assigned(C) Then
    If (C.TokenCount > 0) Or (C.TagCount > 0) Then
      Begin
        If FBodyComment.Count > 0 Then
          Begin
            Cmt := BodyComment[BodyCommentCount - 1];
            If FLastBodyCommentLine + 1 = C.Line Then
              Begin
                Cmt.AppendComment(Cmt, C);
                Cmt.TrimWhiteSpace;
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
procedure TBaseLanguageModule.AddDef(Const strDef : String);

begin
  FCompilerDefs.Add(strDef);
end;

(**

  This method adds an identifier to a list so that these can be omitted from the list of spelling
  mistakes.

  @precon  None.
  @postcon The given identifier is added to the list.

  @param   strIdentifier as a String as a constant

**)
Procedure TBaseLanguageModule.AddIdentifier(Const strIdentifier: String);

Begin
  FIdentifierList.Add(strIdentifier);
End;

(**

  This method adds a timer count to the modules OpTickCount collection. This
  can be used to provide timing / profiling information on operations.

  @precon  None.
  @postcon Adds a timer count to the modules OpTickCount collection. This
           can be used to provide timing / profiling information on operations.

  @param   strLabel as a String as a Constant

**)
procedure TBaseLanguageModule.AddTickCount(Const strLabel: String);

begin
  FTickList.Add(TTickOption.Create(strLabel, GetHighPerformanceTickCount));
end;

(**

  This method adds the current token to the passed generic container if it is not nil and moves to the 
  next non comment token.

  @precon  None.
  @postcon Adds the current token to the passed generic container if it is not nil and moves to the next 
           non comment token.

  @param   Container as a TElementContainer as a constant

**)
Procedure TBaseLanguageModule.AddToExpression(Const Container : TElementContainer);

Begin
  If Container <> Nil Then
    Container.AppendToken(Token);
  NextNonCommentToken;
End;

(**

  This method appends the passed token string to the previous token.

  @precon  None.
  @postcon Appends the passed token string to the previous token.

  @param   strToken as a String as a Constant

**)
Procedure TBaseLanguageModule.AppendToLastToken(Const strToken : String);

Begin
  Tokens[TokenCount - 1].Append(strToken);
End;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Override and default AsString method and returns the name of the module.

  @nohint boolShowIdentifier boolForDocumentation
  
  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TBaseLanguageModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := ExtractFileName(Name);
end;

(**

  This method cycles through each comment in the module and checks the spelling of each token.

  @precon  None.
  @postcon The tokens of the main comment are checked for spelling and if any fail they are added to the
           spelling mistakes list.

**)
Procedure TBaseLanguageModule.CheckCommentSpelling;

Const
  strPrecon = 'precon';
  strPostcon = 'postcon';

Var
  iComment : Integer;
  Cmt : TComment;
  iToken : Integer;
  Token : TTokenInfo;

Begin
  If doSpellCheckComments In TBADIOptions.BADIOptions.Options Then
    For iComment := 0 To BodyCommentCount - 1 Do
      Begin
        Cmt := BodyComment[iComment];
        For iToken := 0 To Cmt.TokenCount - 1 Do
          Begin
            Token := Cmt.Tokens[iToken];
            If (Token.TokenType In [ttIdentifier]) And (Token.Length > 1) Then
              CheckSpelling(Token.Token, scNone, sitComment, Token.Line, Token.Column, Cmt);
          End;
        If doSpellCheckTags In TBADIOptions.BADIOptions.Options Then
          Begin
            CheckTagSpelling(Cmt, strPrecon);
            CheckTagSpelling(Cmt, strPostcon);
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

Const
  strDate = 'date';
  strVersion = 'version';
  strAuthor = 'author';
  
  (**

    This procedure check the modules comment for a missing author tag.

    @precon  None.
    @postcon If the module comment is missing an author tag a documentation conflict message is output.

  **)
  Procedure CheckAuthor;

  Var
    i: Integer;

  Begin
    If (doShowMissingModuleAuthor In BADIOptions.Options) Then
      Begin
        i := Comment.FindTag(strAuthor);
        If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
          AddDocumentConflict([], Line, Column, Self,
            strModuleDocumentation, DocConflictTable[dctModuleMissingAuthor])
      End;
  End;

  (**

    This procedure check the modules comment for a missing version tag.

    @precon  None.
    @postcon If the module comment is missing an version tag a documentation conflict message is output.

  **)
  Procedure CheckVersion;

  Var
    i: Integer;

  Begin
    If (doShowMissingModuleVersion In BADIOptions.Options) Then
      Begin
        i := Comment.FindTag(strVersion);
        If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
          AddDocumentConflict([], Line, Column, Self,
            strModuleDocumentation, DocConflictTable[dctModuleMissingVersion])
      End;
  End;

  (**

    This procedure check the module comment for a missing or incorrect date tag.

    @precon  None.
    @postcon If the module comment is missing a date tag or the date is incorrect (against the modules
             last saved date and time stamp) then documentation conflict messages are output.

  **)
  Procedure CheckDate;

  Const 
    iDateWidth = 80;
    strDateFmt = 'dd mmm yyyy';
  
  Var
    i : Integer;
    strDateTime : String;
    dtDate, dtFileDate : TDateTime;
    Tag : TTag;

  Begin
    If (doShowMissingModuleDate In BADIOptions.Options) Then
      Begin
        i := Comment.FindTag(strDate);
        If (i = -1) Or (Comment.Tag[i].TokenCount = 0) Then
          AddDocumentConflict([FormatDateTime(strDateFmt, Now)],
            Line, Column, Self, strModuleDocumentation, DocConflictTable[dctModuleMissingDate])
        Else
          Begin
            Tag := Comment.Tag[i];
            strDateTime := Tag.AsString(iDateWidth, False);
            If Modified Then
              dtFileDate := Now
            Else
              {$IFDEF D2006}
              FileAge(FileName, dtFileDate);
              {$ELSE}
              dtFileDate := FileDateToDateTime(FileAge(FileName));
              {$ENDIF}
            Try
              dtDate := ConvertDate(strDateTime);
              If Int(dtDate) <> Int(dtFileDate) Then
                AddDocumentConflict([strDateTime, FormatDateTime(strDateFmt, dtFileDate)],
                  Tag.Line, Tag.Column, Self, strModuleDocumentation,
                  DocConflictTable[dctModuleIncorrectDate]);
            Except
              AddDocumentConflict([strDateTime, FormatDateTime(strDateFmt, dtFileDate)],
                Tag.Line, Tag.Column, Self, strModuleDocumentation,
                DocConflictTable[dctModuleCheckDateError]);
            End
          End;
      End;
  End;

Begin
  If BADIOptions.Exclusions.ShouldExclude(FFileName, etDocumentation) Then
      Exit;
  If doShowUndocumentedModule In BADIOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([], Line, Column, Self,
        strModuleDocumentation, DocConflictTable[dctModuleMissingDocumentation]);
  If Comment <> Nil Then
    Begin
      CheckDate;
      CheckVersion;
      CheckAuthor;
    End;
  Inherited CheckDocumentation(boolCascade);
End;

(**

  This method checks the given word against the various lists and if found to be not ignored and added to
  the local dictionary or in the main language dictionary, it is added to the list of spelling mistakes.

  @precon  None.
  @postcon The word is added to the spelling mistakes list of not in any dictionary or ignore list.

  @param   strWord            as a String as a constant
  @param   AScope             as a TScope as a constant
  @param   eSpellingIssueType as a TBADISpellingIssueType as a constant
  @param   iLine              as an Integer as a constant
  @param   iColumn            as an Integer as a constant
  @param   Comment            as a TComment as a constant

**)
procedure TBaseLanguageModule.CheckSpelling(Const strWord : String; Const AScope : TScope;
  Const eSpellingIssueType : TBADISpellingIssueType; Const iLine, iColumn : Integer;
  Const Comment : TComment);

Var
  boolFound: Boolean;
  iIndex : Integer;

Begin
  boolFound :=
    BADIOptions.LanguageDictionary.Find(strWord, iIndex) Or
    BADIOptions.LocalDictionary.Find(strWord, iIndex) Or
    BADIOptions.IgnoreDictionary.Find(strWord, iIndex) Or
    BADIOptions.ProjectDictionary.Find(strWord, iIndex) Or
    FIdentifierList.Find(strWord, iIndex);
  If Not boolFound And (strWord.ToUpper <> strWord) Then
    AddSpelling(strWord, AScope, eSpellingIssueType, iLine, iColumn, Comment);
End;

(**

  This method check string literals for spelling mistakes.

  @precon  None.
  @postcon Each string literal in the modules token list if parsed for individual words and then checked
           against various dictionary before being added the the spelling mistakes list.

**)
Procedure TBaseLanguageModule.CheckStringSpelling;

Var
  iToken: Integer;
  Token : TTokenInfo;
  sl : TStringList;
  i : Integer;

Begin
  If doSpellCheckLiterals In TBADIOptions.BADIOptions.Options Then
    For iToken := 0 To TokenCount - 1 Do
      Begin
        Token := Tokens[iToken];
        If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
          Begin
            sl := Tokenize(Token.Token.DeQuotedString, [], []);
            Try
              For i := 0 To sl.Count - 1 Do
                If (sl[i].Length > 1) And (sl[i][1] <> '#') Then
                  If TBADITokenType(sl.Objects[i]) In [ttIdentifier] Then
                    CheckSpelling(sl[i], scNone, sitLiteral, Token.Line, Token.Column, Nil);
            Finally
              sl.Free;
            End;
          End;
      End;
End;

(**

  This method checks the named tag in the given comment for spelling issues.

  @precon  Comment must be a valid instance.
  @postcon Any words in the tag tokens that are not in dictionaries or ignore lists will be added to the
           list of spelling mistakes.

  @param   Comment    as a TComment as a constant
  @param   strTagName as a String as a constant

**)
Procedure TBaseLanguageModule.CheckTagSpelling(Const Comment : TComment; Const strTagName : String);

Var
  iIndex: Integer;
  T: TTag;
  iToken: Integer;
  Token : TTokenInfo;

Begin
  iIndex := Comment.FindTag(strTagName);
  If iIndex > -1 Then
    Begin
      T := Comment.Tag[iIndex];
      For iToken := 0 To T.TokenCount - 1 Do
        Begin
          Token := T.Tokens[iToken];
          If (Token.TokenType In [ttIdentifier]) And (Token.Length > 1) Then
            CheckSpelling(Token.Token, scNone, sitTag, Token.Line, Token.Column, Comment);
        End;
    End;
End;

(**

  This is the constructor method for the TBaseLanguageModule class.

  @precon  None.
  @postcon Initialise this base class and Tokenises the passed stream of characters.

  @nohint  Source

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
constructor TBaseLanguageModule.CreateParser(Const Source, strFileName : String;
  Const IsModified : Boolean; Const ModuleOptions : TModuleOptions);

begin
  Inherited Create(strFileName, scGlobal, 0, 0, iiModule, Nil);
  FReplaceAmpersandRegEx := TRegEx.Create('&(\w)', [roIgnoreCase, roCompiled, roSingleLine]);
  FModuleOptions := ModuleOptions;
  FFileName := strFileName;
  FModified := IsModified;
  FOwnedItems := TObjectList.Create(True);
  FTokenIndex := 0;
  FPreviousTokenIndex := -1;
  FTickList := TObjectList.Create(True);
  FBodyComment := TObjectList.Create(True);
  FModuleName := strFileName;
  FCompilerDefs := TStringList.Create;
  FCompilerDefs.Duplicates := dupIgnore;
  {$IFDEF D0006}
  FCompilerDefs.CaseSensitive := False;
  {$ENDIF}
  FCompilerDefs.Sorted := True;
  FIdentifierList := TStringList.Create;
  FIdentifierList.Sorted := True;
  FIdentifierList.Duplicates := dupIgnore;
  FCompilerConditionStack := TCompilerConditionStack.Create;
  FCompilerConditionUndoStack := TCompilerConditionStack.Create;
  FCommentClass := CommentClass;
  FTokenStackTop := -1;
  SetLength(FTokenStack, iStackCapacity);
end;

(**

  This method returns the default profiling template for this module.

  @precon  None.
  @postcon Returns the default profiling template for this module.

  @return  a String

**)
Class Function TBaseLanguageModule.DefaultProfilingTemplate: String;

Const
  strTemplate = '$METHODCODE$';
  
begin
  Result := strTemplate;
end;

(**

  This method deletes a definition from the source compiler definitions list.

  @precon  None.
  @postcon Deletes a definition from the source compiler definitions list.

  @param   strDef as a String as a Constant

**)
procedure TBaseLanguageModule.DeleteDef(Const strDef : String);

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
  FIdentifierList.Free;
  FCompilerDefs.Free;
  FBodyComment.Free;
  FTickList.Free;
  FDocErrors.Free;
  FOwnedItems.Free;
  inherited;
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

  This method seeks the first non-comment token in the source code which match one of the passed tokens.

  @precon  The Tokens passed MUST be sorted in lowercase and in ascending order.
  @postcon Seeks the first non-comment token in the source code which match one of the passed tokens.

  @nometric HardCodedInteger
  
  @param   strMsg     as a String as a constant
  @param   strParam   as a String as a constant
  @param   SeekTokens as an Array Of String as a constant
  @param   SeekToken  as a TSeekToken as a constant
  @param   Container  as a TElementContainer as a constant

**)
Procedure TBaseLanguageModule.ErrorAndSeekToken(Const strMsg, strParam : String;
  Const SeekTokens: Array Of String; Const SeekToken : TSeekToken; Const Container : TElementContainer);

  (**

    This method counts the number of occurrences of "%s" in the string and returns that number.

    @precon  None.
    @postcon Returns the number of string parameters in the text.

    @param   strText as a String as a constant
    @return  an Integer

  **)
  Function StringCount(Const strText : String) : Integer;

  Const 
    iFmtLen = 2;
    
  Var
    i : Integer;

  Begin
    Result := 0;
    For i := 1 To Length(strText) - 1 Do
      If Copy(strText, i, iFmtLen) = '%s' Then Inc(Result);
  End;

Type
  TBADIErrorSeekType = (estZeroParams, estOneParam, estTwoParams);

Begin
  Case TBADIErrorSeekType(StringCount(strMsg)) Of
    estZeroParams:
      AddIssue(Format(strMsg, [Token.Line, Token.Column]), scGlobal, Token.Line, Token.Column, etError,
        Container);
    estOneParam:
      AddIssue(Format(strMsg, [strParam, Token.Line, Token.Column]), scGlobal, Token.Line, Token.Column,
        etError, Container);
    estTwoParams: AddIssue(Format(strMsg, [strParam, Token.Token, Token.Line, Token.Column]), scGlobal,
      Token.Line, Token.Column, etError, Container);
  Else
    AddIssue(strNotEnoughStrings, scGlobal, Token.Line, Token.Column, etError, Container);
  End;
  NextNonCommentToken;
  While Not IsKeyWord(Token.Token, SeekTokens) Do
    NextNonCommentToken;
  If SeekToken = stFirst Then
    NextNonCommentToken;
End;

(**

  This is a getter method for the BodyComment property.

  @precon  iIndex is the index of the body comment required.
  @postcon Return the requested comment object.

  @param   iIndex as an Integer as a constant
  @return  a TComment

**)
Function TBaseLanguageModule.GetBodyComment(Const iIndex : Integer) : TComment;

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

Const
  iMilliseconds = 1000.0;
  
Var
  t, f: Int64;

Begin
  QueryPerformanceCounter(t);
  QueryPerformanceFrequency(f);
  Result := iMilliseconds * Int(t) / Int(f);
End;

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
function TBaseLanguageModule.GetOpTickCount(Const strStart, strFinish : String): Double;

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

  @param   iIndex as an Integer as a constant
  @return  a Double

**)
function TBaseLanguageModule.GetOpTickCountByIndex(Const iIndex: Integer): Double;

begin
  Result := (FTickList[iIndex] As TTickOption).TickCount;
end;

(**

  This is a getter method for the OpTickCountName property.

  @precon  iIndex must be a valid integer index.
  @postcon Returns the name of the OpTickCount references by the index passed.

  @param   iIndex as an Integer as a constant
  @return  a String

**)
function TBaseLanguageModule.GetOpTickCountName(Const iIndex: Integer): String;

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
      AddIssue(strUnExpectedEndOfFile, scNone, 0, 0, etError, Self);
      Raise EBADIParserAbort.Create(strParsingAborted);
    End;
  Result := Tokens[FTokenIndex] As TTokenInfo;
End;

(**

  This method checks to see the the given definition exists in the source list.

  @precon  None.
  @postcon Returns true if the definition exists.

  @param   strDef as a String as a Constant
  @return  a Boolean

**)
function TBaseLanguageModule.IfDef(Const strDef : String) : Boolean;

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
function TBaseLanguageModule.IfNotDef(Const strDef : String) : Boolean;

begin
  Result := Not IfDef(strDef);
end;

(**

  This method check the current token against the passed string and if true returns true and adds the 
  token to the generic container.

  @precon  None.
  @postcon Check the current token against the passed string and if true returns true and adds the 
           token to the generic container.

  @param   strToken  as a String as a constant
  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TBaseLanguageModule.IsToken(Const strToken : String;
  Const Container : TElementContainer): Boolean;

Begin
  Result := strToken = Token.Token;
  If Result Then
    AddToExpression(Container);
End;


(**

  This method move the token position to the next non comment token.

  @precon  None.
  @postcon Move the token position to the next non comment token.

**)
procedure TBaseLanguageModule.NextNonCommentToken;

Var
  boolContinue : Boolean;

begin
  FShouldUndoCompilerStack := False;
  // Catch first token as directive
  If Token.TokenType = ttCompilerDirective Then
    Begin
      ProcessCompilerDirective;
      FShouldUndoCompilerStack := True;
    End;
  Repeat
    ProcessBodyComments;
    If Not (Tokens[FTokenIndex].TokenType In [ttLineComment, ttBlockComment,
      ttCompilerDirective]) And CompilerConditionStack.CanParse Then
      FPreviousTokenIndex := FTokenIndex;
    NextToken;
    If Token.TokenType = ttCompilerDirective Then
      Begin
        ProcessCompilerDirective;
        FShouldUndoCompilerStack := True;
      End;
    boolContinue := (
      (Token.TokenType In [ttLineComment, ttBlockComment, ttCompilerDirective]) And Not EndOfTokens
    ) Or Not CompilerConditionStack.CanParse;
  Until Not boolContinue;
end;

(**

  This method moves the token to the next token in the token list.

  @precon  None.
  @postcon Moves the token to the next token in the token list.

**)
Procedure TBaseLanguageModule.NextToken;

begin
  Inc(FTokenIndex);
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
      Raise EBADIParserError.Create(strCannotPopCompilerCondition);
End;

(**

  This method moves the token to the previous token in the token list.

  @precon  None.
  @postcon Moves the token to the previous token in the token list.

**)
Procedure TBaseLanguageModule.PreviousToken;

Begin
  Dec(FTokenIndex);
End;

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

  Process any body comments and add them to the body comment collection.

  @precon  None.
  @postcon Any body comments found are processed and added to the body comment collection.

**)
Procedure TBaseLanguageModule.ProcessBodyComments;

Var
  C : TComment;

Begin
  If (Token.TokenType In [ttLineComment, ttBlockComment]) And
    (FLastComment <> Token) And Assigned(FCommentClass) Then
    Begin
      C := FCommentClass.CreateComment(Token.Token, Token.Line, Token.Column);
      If Assigned(C) Then
        Begin
          AddBodyComment(C);
          FLastComment := Token;
        End;
    End;
End;

(**

  This method processes the the spell checking of the string literals for the given element.

  @precon  E must be a valid instance.
  @postcon All spelling mistakes in the string literals of the given element are output.

  @param   E                  as a TElementContainer as a constant
  @param   eSpellingIssueType as a TBADISpellingIssueType as a constant

**)
Procedure TBaseLanguageModule.ProcessLiteralsForSpelling(Const E: TElementContainer;
  Const eSpellingIssueType : TBADISpellingIssueType);

Var
  iToken: Integer;
  T: TTokenInfo;
  sl: TStringList;
  i: Integer;
  strToken: String;

Begin
  Begin
    For iToken := 0 To E.TokenCount - 1 Do
      If E.Tokens[iToken].TokenType In [ttSingleLiteral] Then
        Begin
          T := E.Tokens[iToken];
          strToken := T.Token.DeQuotedString;
          strToken := FReplaceAmpersandRegEx.Replace(strToken, '\1');
          sl := Tokenize(strToken, [], []);
          Try
            For i := 0 To sl.Count - 1 Do
              If (sl[i].Length > 1) And (sl[i][1] <> '#') Then
                If TBADITokenType(sl.Objects[i]) In [ttIdentifier] Then
                  CheckSpelling(sl[i], E.Scope, eSpellingIssueType, T.Line, T.Column, Nil);
          Finally
            sl.Free;
          End;
        End;
  End;
End;

(**

  This method pushes the current token position on to the top of a stack.

  @precon  None.
  @postcon the current token position is pushed on to the top of a stack.

**)
Procedure TBaseLanguageModule.PushTokenPosition;

Var
  T : TArrayOfInteger;
  i: Integer;

Begin
  Inc(FTokenStackTop);
  If FTokenStackTop > High(FTokenStack) Then
    Begin
      SetLength(T, Length(FTokenStack) + iStackCapacity);
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
          AddIssue(strUnExpectedStartOfFile, scNone, 0, 0, etError, Self);
          Raise EBADIParserAbort.Create(strParsingAborted);
        End;
    End;
End;

(**

  This is a setter method for the TokenIndex property.

  @precon  iIndex is the token index to set the parse to start at.
  @postcon Sets the TokenIndex position.

  @param   iIndex as a TTokenIndex as a constant

**)
Procedure TBaseLanguageModule.SetTokenIndex(Const iIndex : TTokenIndex);

Begin
  FTokenIndex := iIndex;
End;

End.
