(**

  This module contains code to parser VB/VBA code (and perhaps will be extended
  to parser VB.NET code later).

  @Version    1.0
  @Date       16 Nov 2008
  @Author     David Hoyle

**)
Unit VBModule;

Interface

Uses
  SysUtils, Classes, Contnrs, Controls, BaseLanguageModule;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TVBModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FTypesLabel: TLabelContainer;
    FConstantsLabel: TLabelContainer;
    FResourceStringsLabel: TLabelContainer;
    FVariablesLabel: TLabelContainer;
    FThreadVarsLabel: TLabelContainer;
    FImplementedMethodsLabel: TLabelContainer;
    FSourceStream: TStream;
    Procedure TokenizeStream;
    Procedure ParseTokens;
    { Grammer Parsers }
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
  Public
    Constructor CreateParser(Source : TStream; strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function KeyWords : TKeyWords; Override;
    { Properties }
  End;

Implementation

{$IFDEF PROFILECODE}
Uses
  Profiler;
{$ENDIF}

Const
  (** A set of characters for alpha characaters **)
  strTokenChars : Set Of Char = ['_', 'a'..'z', 'A'..'Z'];
  (** A set of numbers **)
  strNumbers : Set Of Char = ['&', '0'..'9'];
  (** A set of characters for general symbols **)
  strSymbols : Set Of Char = ['&', '(', ')', '*', '+',
    ',', '-', '.', '/', ':', ';', '<', '=', '>', '@', '[', ']', '^', '{', '}'];
  (** A set of characters for quotes **)
  strQuote : Set Of Char = ['"'];
  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[1..141] Of String = (
    'addhandler', 'addressof', 'andalso', 'alias', 'and', 'ansi', 'as', 'assembly',
    'auto', 'base', 'boolean', 'byref', 'byte', 'byval', 'call', 'case', 'catch',
    'cbool', 'cbyte', 'cchar', 'cdate', 'cdec', 'cdbl', 'char', 'cint', 'class',
    'clng', 'cobj', 'compare', 'const', 'cshort', 'csng', 'cstr', 'ctype', 'date',
    'decimal', 'declare', 'default', 'delegate', 'dim', 'directcast', 'do', 'double',
    'each', 'else', 'elseif', 'end', 'enum', 'erase', 'error', 'event', 'exit',
    'explicit', 'false', 'finally', 'for', 'friend', 'function', 'get', 'gettype',
    'gosub', 'goto', 'handles', 'if', 'implements', 'imports', 'in', 'inherits',
    'integer', 'interface', 'is', 'let', 'lib', 'like', 'long', 'loop', 'me', 'mod',
    'module', 'mustinherit', 'mustoverride', 'mybase', 'myclass', 'namespace', 'new',
    'next', 'not', 'nothing', 'notinheritable', 'notoverridable', 'object', 'on',
    'option', 'optional', 'or', 'orelse', 'overloads', 'overridable', 'overrides',
    'paramarray', 'preserve', 'private', 'property', 'protected', 'public', 'raiseevent',
    'readonly', 'redim', 'rem', 'removehandler', 'resume', 'return', 'select', 'set',
    'shadows', 'shared', 'short', 'single', 'static', 'step', 'stop', 'string',
    'structure', 'sub', 'synclock', 'then', 'throw', 'to', 'true', 'try', 'type',
    'typeof', 'unicode', 'until', 'variant', 'when', 'while', 'with', 'withevents',
    'writeonly', 'xor'
  );


(**

  This is the constructor method for the TVBDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text
           that is the contents of a source code module, Filename is the file
           name of the module being parsed and IsModified determines if the
           source code module has been modified since the last save to disk.
  @postcon Initialise the class and parses the text stream.

  @param   Source        as a TStream
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TVBModule.CreateParser(Source : TStream; strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

var
  boolCascade: Boolean;
  i: Integer;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TVBDocModule.Create');
  Try
  {$ENDIF}
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FTypesLabel              := Nil;
  FConstantsLabel          := Nil;
  FResourceStringsLabel    := Nil;
  FVariablesLabel          := Nil;
  FThreadVarsLabel         := Nil;
  FImplementedMethodsLabel := Nil;
  CompilerDefines.Assign(BrowseAndDocItOptions.Defines);
  FSourceStream := Source;
  AddTickCount('Start');
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount('Parse');
      //: @debug TidyUpEmptyElements;
      AddTickCount('Resolve');
      Add(strErrors, iiErrorFolder, scNone, Nil);
      Add(strWarnings, iiWarningFolder, scNone, Nil);
      Add(strHints, iiHintFolder, scNone, Nil);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone, Nil);
      If FindElement(strErrors).ElementCount = 0 Then
        CheckReferences;
      AddTickCount('Refs');
      boolCascade := True;
      If moCheckForDocumentConflicts In ModuleOptions Then
        CheckDocumentation(boolCascade);
      AddTickCount('Check');
      i := Find(strErrors);
      If (i > 0) And (Elements[i].ElementCount = 0) Then
        DeleteElement(i);
      i := Find(strWarnings);
      If (i > 0) And (Elements[i].ElementCount = 0) Then
        DeleteElement(i);
      i := Find(strHints);
      If (i > 0) And (Elements[i].ElementCount = 0) Then
        DeleteElement(i);
      i := Find(strDocumentationConflicts);
      If (i > 0) And (Elements[i].ElementCount = 0) Then
        DeleteElement(i);
      End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is the destructor method for the TVBDocModule class.

  @precon  None.
  @postcon Frees memory used by internal objects.

**)
Destructor TVBModule.Destroy;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TVBModule.Destroy');
  Try
  {$ENDIF}
  Inherited Destroy;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**


  This method returns an array of key words for use in the explorer module.


  @precon  None.

  @postcon Returns an array of key words for use in the explorer module.


  @return  a TKeyWords

**)
function TVBModule.KeyWords: TKeyWords;

Var
  i : Integer;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TVBModule.KeyWords');
  Try
  {$ENDIF}
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

procedure TVBModule.ParseTokens;
begin
end;

procedure TVBModule.ProcessCompilerDirective(var iSkip: Integer);
begin
  //: @todo Implement.
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into visual basic tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into visual basic tokens.

**)
Procedure TVBModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btLineComment);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  boolEOF : Boolean;
  (** Token buffer. **)
  strToken : String;
  CurToken : TTokenType;
  LastToken : TTokenType;
  BlockType : TBlockType;
  (** Current line number **)
  iLine : Integer;
  (** Current column number **)
  iColumn : Integer;
  (** Token stream position. Fast to inc this than read the stream position. **)
  iStreamPos : Integer;
  (** Token line **)
  iTokenLine : Integer;
  (** Token column **)
  iTokenColumn : Integer;
  (** Current character position **)
  iStreamCount : Integer;
  Ch : Char;
  (** Token size **)
  iTokenLen : Integer;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TVBModule.TokenizeStream');
  Try
  {$ENDIF}
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  boolEOF := False;
  CurToken := ttUnknown;
  LastToken := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  Ch := #0;
  strToken := '';
  
  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);
  
  If FSourceStream <> Nil Then
    Begin
      Repeat
        If FSourceStream.Read(ch, 1) > 0 Then
          Begin
            Inc(iStreamCount);
            LastToken := CurToken;
  
            If ch In strWhiteSpace Then
              CurToken := ttWhiteSpace
            Else If ch In strTokenChars Then
              CurToken := ttIdentifier
            Else If ch In strNumbers Then
              Begin
                CurToken := ttNumber;
                If LastToken = ttIdentifier Then
                  CurToken := ttIdentifier;
              End
            Else If ch In strLineEnd Then
              CurToken := ttLineEnd
            Else If ch In strQuote Then
              CurToken := ttStringLiteral
            Else If ch In strSymbols Then
              Begin
                CurToken := ttSymbol;
                If (Ch = '.') And (LastToken = ttNumber) Then
                  CurToken := ttNumber;
              End
            Else
              CurToken := ttUnknown;
  
            If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
              Begin
                If ((BlockType In [btStringLiteral, btLineComment]) And
                  (CurToken <> ttLineEnd)) Then
                  Begin
                    Inc(iTokenLen);
                    If iTokenLen > Length(strToken) Then
                      SetLength(strToken, iTokenCapacity + Length(strToken));
                    strToken[iTokenLen] := Ch;
                  End Else
                  Begin
                    SetLength(strToken, iTokenLen);
                    If iTokenLen > 0 Then
                      If Not (strToken[1] In strWhiteSpace) Then
                        Begin
                          If LastToken = ttIdentifier Then
                            Begin
                              If IsKeyWord(strToken, strReservedWords) Then
                                LastToken := ttReservedWord;
                            End;
                          If BlockType = btLineComment Then
                            LastToken := ttComment;
                          If (LastToken = ttComment) And (Length(strToken) > 2) Then
                            If (strToken[1] = '{') And (strToken[2] = '$') Then
                              LastToken := ttCompilerDirective;
                          AddToken(TTokenInfo.Create(strToken, iStreamPos,
                            iTokenLine, iTokenColumn, Length(strToken), LastToken));
                          //Inc(iCounter);
                        End;
                   // Store Stream position, line number and column of
                   // token start
                   iStreamPos := iStreamCount;
                   iTokenLine := iLine;
                   iTokenColumn := iColumn;
                   BlockType := btNoBlock;
                   iTokenLen := 1;
                   SetLength(strToken, iTokenCapacity);
                   strToken[iTokenLen] := Ch;
                  End;
              End Else
              Begin
                Inc(iTokenLen);
                If iTokenLen > Length(strToken) Then
                  SetLength(strToken, iTokenCapacity + Length(strToken));
                strToken[iTokenLen] := Ch;
              End;

            // Check for line comments
            If (BlockType = btNoBlock) And (Ch = '''') Then
              BlockType := btLineComment;

            // Check for string literals
            If CurToken = ttStringLiteral Then
              If BlockType = btStringLiteral Then
                BlockType := btNoBlock
              Else If BlockType = btNoBlock Then
                BlockType := btStringLiteral;

            Inc(iColumn);
            If Ch = #10 Then
              Begin
                Inc(iLine);
                iColumn := 1;
                If BlockType In [btLineComment, btStringLiteral] Then
                  BlockType := btNoBlock;
              End;
          End Else
            boolEOF := True;
      Until boolEOF;
      If iTokenLen > 0 Then
        Begin
          SetLength(strToken, iTokenLen);
          If iTokenLen > 0 Then
            If Not (strToken[1] In strWhiteSpace) Then
              Begin
                If LastToken = ttIdentifier Then
                  Begin
                    If IsKeyWord(strToken, strReservedWords) Then
                      LastToken := ttReservedWord;
                  End;
                AddToken(TTokenInfo.Create(strToken, iStreamPos,
                  iTokenLine, iTokenColumn, Length(strToken), LastToken));
              End;
        End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

End.


