(**

  This module contains common code that can be used within each of the IDE
  imlpementations (Delphi and VB).

  @Author  David Hoyle
  @Date    10 Feb 2015
  @Version 1.0

**)
Unit CommonIDEFunctions;

Interface

Uses
  SysUtils,
  Windows,
  Classes,
  BaselanguageModule;

Type
  {$INCLUDE 'CompilerDefinitions.inc'}
(** This is a procedure to return the success of the parse in the thread. **)
  TParserNotify = Procedure(boolSuccessfulParse: Boolean) Of Object;
  (** This is a procedure to allow the thread to get information from the
      calling IDE. **)
  TEditorInformation = Function(Var strFileName: String; Var boolModified: Boolean)
    : String Of Object;
  (** This is a procedure to allow the thread to render the module in the
      calling IDEs main thread. **)
  TRenderDocumentTree = Procedure(Module: TBaseLanguageModule) Of Object;
  (** This is a procedure to allow the thread to display an error message in
      the calling IDEs main thread. **)
  TThreadExceptionMsg = Procedure(strExceptionMsg: String) Of Object;

  (** This is a class to manage thread used to parse code. Its main aim is
      to ensure that only 1 thread is active at a time and provide a mechanism
      to terminate a working thread. **)
  TBrowseAndDocItThreadManager = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FThread             : TThread;
    FSuccessfulParseProc: TParserNotify;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure TerminateThread(Sender: TObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Parse(SuccessfulParseProc: TParserNotify; EditorInfo: TEditorInformation;
      RenderDocumentTree: TRenderDocumentTree;
      ThreadExceptionMsg: TThreadExceptionMsg): Boolean;
  End;

  (** A record to describe the start, end and markers for different comment
      types. **)
  TCommentTypeRec = Record
    FStart: String;
    FMiddle: String;
    FBlockEnd: String;
    FLineEnd: String;
  End;

Function FindFunction(iLine: Integer; Container: TElementContainer;
  ContainerClass: TGenericFunctionClass): TGenericFunction;
Function Description(Func: TGenericFunction; iIndent: Integer; boolPadOut: Boolean;
  Var CursorAdjust: TPoint; iMaxCommentWidth: Integer): String;
Function Indent(strText: String; iIndent: Integer): String;
Function OutputTag(iIndent: Integer; Tag: TTag; iMaxCommentWidth: Integer): String;
Function WriteComment(Func: TGenericFunction; CommentType: TCommentType; iIndent: Integer;
  boolPadOut: Boolean; Var CursorDelta: TPoint; iMaxCommentWidth: Integer): String;
Function FindIndentOfFirstTokenOnLine(Module: TBaseLanguageModule;
  iLine: Integer): Integer;
Function BuildBlockComment(CommentType: TCommentType; CommentStyle: TCommentStyle;
  iIndent: Integer; strSelectedText: String): String;

Const
  (** A simple array for outputting a or an. **)
  strAOrAn: Array [False .. True] Of String = ('a', 'an');
  (** An array of parameter modifier phases. **)
  strModifier: Array [pamNone .. pamOut] Of String = ('', ' as a reference',
    ' as a constant', ' as an out parameter');
  (** A list of vowels. **)
  strVowels: Set Of AnsiChar = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U'];
  (** A constant array of outputs for the ArrayOf property. **)
  strArrayOf: Array [False .. True] Of String = ('', 'Array Of ');

ResourceString
  (** This is a message for no methods to comment. **)
  strNoMethodFound = 'No method found on or above the current cursor position.';
  (** This is a message to confirm you wish to update the current comment. **)
  strMethodAlreadyExists = 'The method "%s" already has a comment. Do you' +
    ' want to update the comment with revised parameters and returns?';
  (** This is a message for no property to comment. **)
  strNoPropertyFound = 'No property found on or above the current cursor position.';
  (** This is a message to confirm you wish to update the current comment. **)
  strPropertyAlreadyExists = 'The property "%s" already has a comment. Do you' +
    ' want to continue?';

Implementation

Uses
  {$IFDEF EUREKALOG_VER7}
  ExceptionLog7,
  EExceptionManager,
  {$ENDIF}
  DGHLibrary;

Type
  (** This class defines a thread in which the parsing of the code and
      rendering of the module explorer is done. **)
  TBrowseAndDocItThread = Class(TThread)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FModule            : TBaseLanguageModule;
    FSource            : String;
    FFileName          : String;
    FType              : String;
    FModified          : Boolean;
    FRenderDocumentTree: TRenderDocumentTree;
    FThreadExceptionMsg: TThreadExceptionMsg;
    FSuccessfulParse   : Boolean;
    Procedure SetName;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure Execute; Override;
    Procedure RenderModuleExplorer;
    Procedure ShowException;
  Public
    Constructor CreateBrowseAndDocItThread(EditorInfo: TEditorInformation;
      RenderDocumentTree: TRenderDocumentTree; ThreadExceptionMsg: TThreadExceptionMsg;
      TerminateThread: TNotifyEvent);
    Destructor Destroy; Override;
    (**
      This property gets and sets the SuccessfulParse variable of the thread.
      @precon  None.
      @postcon Gets and sets the SuccessfulParse variable of the thread.
      @return  a Boolean
    **)
    Property SuccessfulParse: Boolean Read FSuccessfulParse Write FSuccessfulParse;
  End;

  (** This record defines information for use in naming threads. **)
  TThreadNameInfo = Record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  End;

Const
  (** A constant to define the with of the tag formatting in method / property
      comments. **)
  iTagWidth: Integer = 8;
  (** A constant array to define the comment start, end and markers for the
      different styles of source code. **)
  strCmtTerminals: Array [Low(TCommentType) .. High(TCommentType)
    ] Of TCommentTypeRec = ((FStart: ''; FMiddle: ''; FBlockEnd: ''; FLineEnd: ''),
    (FStart: '(**'; FMiddle: ''; FBlockEnd: '**)'; FLineEnd: '**)'), (FStart: '{:';
      FMiddle: ''; FBlockEnd: '}'; FLineEnd: '}'), (FStart: '/**'; FMiddle: '';
      FBlockEnd: '**/'; FLineEnd: '**/'), (FStart: '//:'; FMiddle: '//:'; FBlockEnd: '';
      FLineEnd: ''), (FStart: ''':'; FMiddle: ''':'; FBlockEnd: ''':'; FLineEnd: ''),
    (FStart: '<!--'; FMiddle: ''; FBlockEnd: '-->'; FLineEnd: '-->'));

(**

  This method recursively works throug the hierarchy of elements looking for the
  method which is closest to be on or just above the current cursor line.

  @precon  Container must be a valid TElementContainer instance.
  @postcon Recursively works throug the hierarchy of elements looking for the
           method which is closest to be on or just above the current cursor
           line.

  @param   iLine          as an Integer
  @param   Container      as a TElementContainer
  @param   ContainerClass as a TGenericFunctionClass
  @return  a TgenericFunction

**)
Function FindFunction(iLine: Integer; Container: TElementContainer;
  ContainerClass: TGenericFunctionClass): TGenericFunction;

Var
  i: Integer;
  M: TGenericFunction;

  (**

    This procedure updates the result with the new method if it is less than
    iLine but more than the last method found.

    @precon  None.
    @postcon Updates the result with the new method if it is less than
             iLine but more than the last method found.

  **)
  Procedure CheckLine;

  Begin
    If M <> Nil Then
      If (M.Line <= iLine) Then
        Begin
          If Result = Nil Then
            Result := M
          Else If M.Line > Result.Line Then
            Result := M;
        End;
  End;

Begin
  Result := Nil;
  For i  := 1 To Container.ElementCount Do
    Begin
      If Container.Elements[i] Is ContainerClass Then
        Begin
          M := Container.Elements[i] As TGenericFunction;
          CheckLine;
        End;
      If Container.Elements[i].ElementCount > 0 Then
        Begin
          M := FindFunction(iLine, Container.Elements[i], ContainerClass);
          CheckLine;
        End;
    End;
End;

(**

  This method returns a description for the method if it is a constructor,
  destructor, getter or setter method, else it returns an empty String.

  @precon  Method is a valid instance of a method declatation to be described.
  @postcon Returns a description of the method is applicable. CursorAdjust
           provide delta movements for the cursor from column 1 if the first
           line of the new comment.

  @param   Func             as a TGenericFunction
  @param   iIndent          as an Integer
  @param   boolPadOut       as a Boolean
  @param   CursorAdjust     as a TPoint as a reference
  @param   iMaxCommentWidth as an Integer
  @return  a String

**)
Function Description(Func: TGenericFunction; iIndent: Integer; boolPadOut: Boolean;
  Var CursorAdjust: TPoint; iMaxCommentWidth: Integer): String;

Var
  i             : Integer;
  boolCon       : Boolean;
  strDescription: String;
  j             : Integer;
  MD            : TStringList;
  C             : TComment;

Begin
  CursorAdjust.X := 0;
  CursorAdjust.Y := 0;
  If Func.Comment = Nil Then
    Begin
      MD    := BrowseAndDocItOptions.MethodDescriptions;
      For i := 0 To MD.Count - 1 Do
        If Like(MD.Names[i], Func.Identifier) Then
          Begin
            C := TComment.Create(MD.ValueFromIndex[i], 0, 0);
            Try
              strDescription := Indent(C.AsString(iMaxCommentWidth - iIndent - 2, True),
                iIndent + 2);
              If Pos('|', strDescription) > 0 Then
                For j := 1 To Length(strDescription) Do
                  Begin
                    If strDescription[j] = '|' Then
                      Begin
                        Delete(strDescription, j, 1);
                        Break;
                      End;
                    If strDescription[j] <> #10 Then
                      Inc(CursorAdjust.X);
                    If strDescription[j] = #13 Then
                      Begin
                        Inc(CursorAdjust.Y);
                        CursorAdjust.X := 0;
                      End;
                  End;
              Break;
            Finally
              C.Free;
            End;
          End;
      Result := Format('%s'#13#10, [strDescription]);
      If boolPadOut Then
        Result := Result + #13#10;
    End
  Else
    Begin
      Result := Format('%s'#13#10,
        [Indent(Func.Comment.AsString(iMaxCommentWidth - iIndent - 2, True),
            2 + iIndent)]);
      If boolPadOut Then
        Result := Result + #13#10;
      boolCon  := False;
      i        := Func.Comment.FindTag('precon');
      If i > -1 Then
        Begin
          Result := Result + OutputTag(2 + iIndent, Func.Comment.Tag[i],
            iMaxCommentWidth);
          boolCon := True;
        End;
      i := Func.Comment.FindTag('postcon');
      If i > -1 Then
        Begin
          Result := Result + OutputTag(2 + iIndent, Func.Comment.Tag[i],
            iMaxCommentWidth);
          boolCon := True;
        End;
      If boolCon Then
        If boolPadOut Then
          Result := Result + #13#10;
      boolCon    := False;
      For i      := 0 To Func.Comment.TagCount - 1 Do
        If Not IsKeyWord(Func.Comment.Tag[i].TagName, ['param', 'postcon', 'precon',
            'return']) Then
          Begin
            Result := Result + OutputTag(2 + iIndent, Func.Comment.Tag[i],
              iMaxCommentWidth);
            boolCon := True;
          End;
      If boolCon Then
        If boolPadOut Then
          Result := Result + #13#10;
    End;
  If strDescription = '' Then
    CursorAdjust.X := 2 + iIndent;
End;

(**

  This function indent the text for a description.

  @precon  None.
  @postcon Returns an indented version of the passed text.

  @param   strText as a String
  @param   iIndent as an Integer
  @return  a String

**)
Function Indent(strText: String; iIndent: Integer): String;

Begin
  Result := StringOfChar(#32, iIndent) + StringReplace(strText, #13#10,
    #13#10 + StringOfChar(#32, iIndent), [rfReplaceAll]);
End;

(**

  This function returns the tag information indented and broken into line no
  wider than iMaxCommentWidth characters.

  @precon  Tag must be a valid comment tag.
  @postcon Returns the tag information indented and broken into line no wider
           than iMaxCommentWidth characters.

  @param   iIndent          as an Integer
  @param   Tag              as a TTag
  @param   iMaxCommentWidth as an Integer
  @return  a String

**)
Function OutputTag(iIndent: Integer; Tag: TTag; iMaxCommentWidth: Integer): String;

Var
  str: String;
  i  : Integer;

Begin
  Result := '';
  str    := Format('%s@%-*s', [StringOfChar(#32, iIndent), iTagWidth, Tag.TagName]);
  For i  := 0 To Tag.TokenCount - 1 Do
    If Length(str + Tag.Tokens[i].Token) < iMaxCommentWidth Then
      str := str + Tag.Tokens[i].Token
    Else
      Begin
        Result := Result + str;
        str    := #13#10 + StringOfChar(#32, iIndent + 9);
        If Tag.Tokens[i].Token <> #32 Then
          str := str + Tag.Tokens[i].Token;
      End;
  Result := Result + str + #13#10;
End;

(**

  This method writes the method comment to the active editor.

  @precon  Method is a valid instance of a method declaration to be commented.
  @postcon The full comment to be inserted at the cursor is returns with the
           new cursor position in Cursor.

  @param   Func             as a TGenericFunction
  @param   CommentType      as a TCommentType
  @param   iIndent          as an Integer
  @param   boolPadOut       as a Boolean
  @param   CursorDelta      as a TPoint as a reference
  @param   iMaxCommentWidth as an Integer
  @return  a String

**)
Function WriteComment(Func: TGenericFunction; CommentType: TCommentType; iIndent: Integer;
  boolPadOut: Boolean; Var CursorDelta: TPoint; iMaxCommentWidth: Integer): String;

  (**

    This procedure adds text to the resulting comment string.

    @precon  None.
    @postcon Adds text to the resulting comment string.

    @param   strText as a String

  **)
  Procedure AddToComment(strText: String);

  Begin
    Result := Result + strText;
  End;

Var
  iLen         : Integer;
  i            : Integer;
  strType      : String;
  P            : TPoint;
  sl           : TStringList;
  strInsert    : String;
  boolExtraLine: Boolean;
  boolHasCons  : Boolean;

Begin
  CursorDelta.X := 0;
  CursorDelta.Y := 0;
  boolExtraLine := False;
  If CommentType In [ctPascalBlock .. ctCPPBlock] Then
    AddToComment(StringOfChar(#32, iIndent));
  If CommentType In [ctPascalBlock, ctPascalBrace, ctCPPBlock] Then
    AddToComment(strCmtTerminals[CommentType].FStart + #13#10);
  If boolPadOut Then
    AddToComment(#13#10);
  AddToComment(Description(Func, iIndent, boolPadOut, P, iMaxCommentWidth));
  boolHasCons := False;
  If Func.Comment <> Nil Then
    boolHasCons := Func.Comment.FindTag('precon') > -1;
  If (doAddPreAndPostToComment In BrowseAndDocItOptions.Options) And Not boolHasCons Then
    Begin
      AddToComment(StringOfChar(#32, iIndent) + '  @precon  '#13#10);
      AddToComment(StringOfChar(#32, iIndent) + '  @postcon '#13#10);
      If boolPadOut Then
        AddToComment(#13#10);
    End;
  iLen          := 0;
  boolExtraLine := boolExtraLine Or (Func.ParameterCount > 0);
  For i         := 0 To Func.ParameterCount - 1 Do
    If iLen < Length(Func.Parameters[i].Identifier) Then
      iLen := Length(Func.Parameters[i].Identifier);
  For i    := 0 To Func.ParameterCount - 1 Do
    Begin
      AddToComment(StringOfChar(#32, iIndent));
      AddToComment(Format('  @param   %-*s as ', [iLen, Func.Parameters[i].Identifier]));
      If Func.Parameters[i].ParamType <> Nil Then
        Begin
          strType := Func.Parameters[i].ParamType.AsString(False, False);
          AddToComment(Format('%s %s%s%s'#13#10,
              [strAOrAn[(IsInSet(strType[1], strVowels)) Or Func.Parameters[i].ArrayOf],
                strArrayOf[Func.Parameters[i].ArrayOf], strType,
                strModifier[Func.Parameters[i].ParamModifier]]));
        End;
    End;
  If Func.ReturnType <> Nil Then
    Begin
      boolExtraLine := boolExtraLine Or True;
      AddToComment(StringOfChar(#32, iIndent));
      AddToComment(Format('  @return  %s %s',
          [strAOrAn[IsInSet(Func.ReturnType.AsString(False, False)[1], strVowels)],
            Func.ReturnType.AsString(False, False)]));
      AddToComment(#13#10);
    End;
  // Block Footer
  If boolExtraLine Then
    If boolPadOut Then
      AddToComment(#13#10);
  If CommentType In [ctPascalBlock .. ctCPPBlock] Then
    AddToComment(StringOfChar(#32, iIndent));
  If CommentType In [ctPascalBlock, ctPascalBrace, ctCPPBlock] Then
    AddToComment(strCmtTerminals[CommentType].FBlockEnd + #13#10)
  Else
    Begin
      If CommentType In [ctCPPLine, ctVBLine] Then
        strInsert := strCmtTerminals[CommentType].FMiddle
      Else
        strInsert := '';
      sl          := TStringList.Create;
      Try
        sl.Text := Result;
        For i   := 0 To sl.Count - 1 Do
          If sl[i] <> '' Then
            sl[i] := Copy(sl[i], 1, iIndent) + strInsert + Copy(sl[i], iIndent + 1,
              Length(sl[i]) - iIndent)
          Else
            sl[i] := StringOfChar(#32, iIndent) + strInsert;
        Result    := sl.Text;
      Finally
        sl.Free;
      End;
    End;
  Inc(CursorDelta.X, P.X);
  Inc(CursorDelta.Y, 2 + P.Y);
  If CommentType In [ctVBLine] Then
    Inc(CursorDelta.X, 2);
  If CommentType In [ctCPPLine] Then
    Inc(CursorDelta.X, 3);
  If Not boolPadOut Then
    Dec(CursorDelta.Y);
  If CommentType In [ctVBLine, ctCPPLine] Then
    Dec(CursorDelta.Y);
End;

(**

  This function returns the column of the first token on the given line number,
  i.e. the indentation of the code.

  @precon  Module must be a valid instance of a module that is parsed.
  @postcon Returns the column of the first token on the given line number,
           i.e. the indentation of the code.

  @param   Module as a TBaseLanguageModule
  @param   iLine  as an Integer
  @return  an Integer

**)
Function FindIndentOfFirstTokenOnLine(Module: TBaseLanguageModule;
  iLine: Integer): Integer;

Var
  iToken: Integer;

Begin
  Result     := 1;
  For iToken := 0 To Module.TokenCount - 1 Do
    If Module.Tokens[iToken].Line = iLine Then
      Begin
        Result := Module.Tokens[iToken].Column;
        Break;
      End;
End;

{ TBrowseAndDocItThreadManager }

(**

  A constructor for the TBrowseAndDocItThreadManager class.

  @precon  None.
  @postcon Intialises the thread variable to null.

**)
Constructor TBrowseAndDocItThreadManager.Create;

Begin
  FThread := Nil;
End;

(**

  A destructor for the TBrowseAndDocItThreadManager class.

  @precon  None.
  @postcon Terminate any working thread.

**)
Destructor TBrowseAndDocItThreadManager.Destroy;

Begin
  If FThread <> Nil Then
    FThread.Terminate;
  Inherited Destroy;
End;

(**

  This method parses the given code reference ONLY IF there is no current
  parsing thread.

  @precon  None.
  @postcon Parses the given code reference ONLY IF there is no current
           parsing thread.

  @param   SuccessfulParseProc as a TParserNotify
  @param   EditorInfo          as a TEditorInformation
  @param   RenderDocumentTree  as a TRenderDocumentTree
  @param   ThreadExceptionMsg  as a TThreadExceptionMsg
  @return  a Boolean

**)
Function TBrowseAndDocItThreadManager.Parse(SuccessfulParseProc: TParserNotify;
  EditorInfo: TEditorInformation; RenderDocumentTree: TRenderDocumentTree;
  ThreadExceptionMsg: TThreadExceptionMsg): Boolean;

Begin
  Result               := False;
  FSuccessfulParseProc := SuccessfulParseProc;
  If FThread = Nil Then
    Begin
      FThread := TBrowseAndDocItThread.CreateBrowseAndDocItThread(EditorInfo,
        RenderDocumentTree, ThreadExceptionMsg, TerminateThread);
      Result := True;
    End;
End;

(**

  This method is an on terminate event handler for threads.

  @precon  None.
  @postcon Called by the freeing thread which sets the thread variable to nil.

  @param   Sender as a TObject

**)
Procedure TBrowseAndDocItThreadManager.TerminateThread(Sender: TObject);

Begin
  FThread := Nil;
  If Assigned(FSuccessfulParseProc) Then
    If Sender Is TBrowseAndDocItThread Then
      FSuccessfulParseProc((Sender As TBrowseAndDocItThread).SuccessfulParse);
End;

{ TBrowseAndDocItThread }

(**

  This is a constructor for the TBrowseAndDocItThread class.

  @precon  None.
  @postcon Creates a suspended thread and sets up a stream with the contents of
           the active editor and then resumed the thread in order to parse
           the contents.

  @param   EditorInfo          as a TEditorInformation
  @param   RenderDocumentTree  as a TRenderDocumentTree
  @param   ThreadExceptionMsg  as a TThreadExceptionMsg
  @param   TerminateThread     as a TNotifyEvent

**)
Constructor TBrowseAndDocItThread.CreateBrowseAndDocItThread
  (EditorInfo: TEditorInformation; RenderDocumentTree: TRenderDocumentTree;
  ThreadExceptionMsg: TThreadExceptionMsg; TerminateThread: TNotifyEvent);

Begin
  FSuccessfulParse    := False;
  FreeOnTerminate     := True; // Self Freeing...
  FRenderDocumentTree := RenderDocumentTree;
  FThreadExceptionMsg := ThreadExceptionMsg;
  OnTerminate         := TerminateThread;
  FSource             := '';
  If Assigned(EditorInfo) Then
    FSource := EditorInfo(FFileName, FModified);
  Inherited Create(False);
End;

(**

  This is a destructor for the TBrowseAndDocItThread class.

  @precon  None.
  @postcon Frees the stream memory.

**)
Destructor TBrowseAndDocItThread.Destroy;
Begin
  Inherited Destroy;
End;

(**

  This execute method parses the code of the active editor stored in the
  memory stream and render the information in the explorer module.

  @precon  FMemoryStream must be a valid stream of chars to parse.
  @postcon Parses the code of the active editor stored in the memory stream and
           render the information in the explorer module.

**)
Procedure TBrowseAndDocItThread.Execute;

Begin
  SetName;
  Try
    FType := 'Parsing';
    If FFileName <> '' Then
      FModule := ModuleDispatcher.Dispatcher(FSource, FFileName, FModified,
        [moParse, moCheckForDocumentConflicts])
    Else
      FModule := Nil;
    Try
      If Terminated Then
        Exit;
      FType := 'Rendering';
      Synchronize(RenderModuleExplorer);
      FSuccessfulParse := True;
    Finally
      FModule.Free;
    End;
  Except
    On E: EParserAbort Do
      Exit;
    On E: Exception Do
      Begin
        {$IFDEF EUREKALOG_VER7}
        ExceptionManager.StandardEurekaNotify(ExceptObject, ExceptAddr)
        {$ELSE}
        FFileName := E.Message;
        Synchronize(ShowException);
        {$ENDIF}
      End;
  End;
End;

(**

  This method synchronizes with the main IDE thread and renders the module
  explorer.

  @precon  FModule must be a valid TBaseLanguageModule instance.
  @postcon Synchronizes with the main IDE thread and renders the module
           explorer.

**)
Procedure TBrowseAndDocItThread.RenderModuleExplorer;

Begin
  If Assigned(FRenderDocumentTree) Then
    FRenderDocumentTree(FModule);
End;

(**

  This is a setter method for the  property.

  @precon  None.
  @postcon Sets the name of the thread.

**)
Procedure TBrowseAndDocItThread.SetName;

Var
  ThreadNameInfo: TThreadNameInfo;

Begin
  ThreadNameInfo.FType     := $1000;
  ThreadNameInfo.FName     := 'BrowseAndDocItThread';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags    := 0;
  Try
    RaiseException($406D1388, 0, sizeof(ThreadNameInfo) Div sizeof(LongWord),
      @ThreadNameInfo);
  Except
  End;
End;

(**

  This method displays the raised exception message pass via the FFileName
  field.

  @precon  None.
  @postcon Displays the raised exception message pass via the FFileName
           field.

**)
Procedure TBrowseAndDocItThread.ShowException;

Const
  strMsg = 'Exception in TBrowseAndDocItThread:'#13#10 + '  Type: %s'#13#10 +
    '  Exception: %s';
Begin
  If Assigned(FThreadExceptionMsg) Then
    FThreadExceptionMsg(Format(strMsg, [FType, FFileName]));
End;

(**

  This method returns a string representation of a comment of the type and
  styles given.

  @precon  None.
  @postcon Returns a string representation of a comment of the type and styles 
           given.

  @param   CommentType     as a TCommentType
  @param   CommentStyle    as a TCommentStyle
  @param   iIndent         as an Integer
  @param   strSelectedText as a String
  @return  a String

**)
Function BuildBlockComment(CommentType: TCommentType; CommentStyle: TCommentStyle;
  iIndent: Integer; strSelectedText: String): String;

Var
  strAllCmtStart, strBlockCmtEnd, strLineCmtEnd: String;
  strCmtMiddle                                 : String;

Begin
  Result         := '';
  strAllCmtStart := strCmtTerminals[CommentType].FStart;
  strCmtMiddle   := strCmtTerminals[CommentType].FMiddle;
  strBlockCmtEnd := strCmtTerminals[CommentType].FBlockEnd;
  strLineCmtEnd  := strCmtTerminals[CommentType].FLineEnd;
  Case CommentStyle Of
    csBlock:
      Begin
        Result := Result + strAllCmtStart + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strCmtMiddle + '  '#13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strCmtMiddle + '  ' +
          strSelectedText + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strCmtMiddle + '  '#13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strBlockCmtEnd + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1);
      End;
    csLine:
      Begin
        Result := Result + strAllCmtStart + #32 + strSelectedText + #32 +
          strLineCmtEnd + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1);
      End;
    csInSitu:
      Begin
        Result := Result + strAllCmtStart + #32 + strSelectedText + #32 +
          strLineCmtEnd + ' ';
      End;
  End;
End;

End.
