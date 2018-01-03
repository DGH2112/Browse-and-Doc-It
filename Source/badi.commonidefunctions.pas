(**

  This module contains common code that can be used within each of the IDE
  imlpementations (Delphi and VB).

  @Author  David Hoyle
  @Date    03 Jan 2018
  @Version 1.0

**)
Unit BADI.CommonIDEFunctions;

Interface

Uses
  SysUtils,
  Windows,
  Classes,
  BADI.Base.Module,
  BADI.ElementContainer,
  BADI.Generic.FunctionDecl,
  BADI.Comment.Tag,
  BADI.Types,
  BADI.Comment;

Type
  {$INCLUDE 'CompilerDefinitions.inc'}
(** This is a procedure to return the success of the parse in the thread. **)
  TParserNotify = Procedure(Const boolSuccessfulParse: Boolean) Of Object;
  (** This is a procedure to allow the thread to get information from the
      calling IDE. **)
  TEditorInformation = Function(Var strFileName: String; Var boolModified: Boolean)
    : String Of Object;
  (** This is a procedure to allow the thread to render the module in the
      calling IDEs main thread. **)
  TRenderDocumentTree = Procedure(Const Module: TBaseLanguageModule) Of Object;
  (** This is a procedure to allow the thread to display an error message in
      the calling IDEs main thread. **)
  TThreadExceptionMsg = Procedure(Const strExceptionMsg: String) Of Object;

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
    Function Parse(Const SuccessfulParseProc: TParserNotify; Const EditorInfo: TEditorInformation;
      Const RenderDocumentTree: TRenderDocumentTree;
      Const ThreadExceptionMsg: TThreadExceptionMsg): Boolean;
  End;

  (** A record to describe the start, end and markers for different comment
      types. **)
  TCommentTypeRec = Record
    FStart: String;
    FMiddle: String;
    FBlockEnd: String;
    FLineEnd: String;
  End;

  Function FindFunction(Const iLine: Integer; Const Container: TElementContainer;
    Const ContainerClass: TGenericFunctionClass): TGenericFunction;
  Function Description(Const Func: TGenericFunction; Const iIndent: Integer; Const boolPadOut: Boolean;
    Var CursorAdjust: TPoint; Const iMaxCommentWidth: Integer): String;
  Function Indent(Const strText: String; Const iIndent: Integer): String;
  Function OutputTag(Const iIndent: Integer; Const Tag: TTag; Const iMaxCommentWidth: Integer): String;
  Function WriteComment(Const Func: TGenericFunction; Const CommentType: TCommentType;
    Const iIndent: Integer; Const boolPadOut: Boolean; Var CursorDelta: TPoint;
    Const iMaxCommentWidth: Integer): String;
  Function FindIndentOfFirstTokenOnLine(Const Module: TBaseLanguageModule;
    Const iLine: Integer): Integer;
  Function BuildBlockComment(Const CommentType: TCommentType; Const CommentStyle: TCommentStyle;
    Const iIndent: Integer; Const strSelectedText: String): String;

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

Type
  (** This class defines a thread in which the parsing of the code and
      rendering of the module explorer is done. **)
  TBrowseAndDocItThread = Class(TThread)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FModule            : TBaseLanguageModule;
    FSource            : String;
    FFileName          : String;
    FModified          : Boolean;
    FRenderDocumentTree: TRenderDocumentTree;
    FThreadExceptionMsg: TThreadExceptionMsg;
    FSuccessfulParse   : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure Execute; Override;
    Procedure RenderModuleExplorer;
    Procedure ShowException;
  Public
    Constructor CreateBrowseAndDocItThread(Const EditorInfo: TEditorInformation;
      Const RenderDocumentTree: TRenderDocumentTree; Const ThreadExceptionMsg: TThreadExceptionMsg;
      Const TerminateThread: TNotifyEvent);
    Destructor Destroy; Override;
    (**
      This property gets and sets the SuccessfulParse variable of the thread.
      @precon  None.
      @postcon Gets and sets the SuccessfulParse variable of the thread.
      @return  a Boolean
    **)
    Property SuccessfulParse: Boolean Read FSuccessfulParse Write FSuccessfulParse;
  End;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  {$IFDEF EUREKALOG}
  EBase,
  {$ENDIF}
  BADI.Options,
  BADI.Functions,
  BADI.Module.Dispatcher,
  Dialogs;

Const
  (** A constant to define the with of the tag formatting in method / property
      comments. **)
  iTagWidth: Integer = 8;
  (** A constant array to define the comment start, end and markers for the
      different styles of source code. **)
  strCmtTerminals: Array [Low(TCommentType) .. High(TCommentType)] Of TCommentTypeRec = (
    (FStart: '';     FMiddle: '';    FBlockEnd: '';    FLineEnd: ''),
    (FStart: '(**';  FMiddle: '';    FBlockEnd: '**)'; FLineEnd: '**)'),
    (FStart: '{:';   FMiddle: '';    FBlockEnd: '}';   FLineEnd: '}'),
    (FStart: '/**';  FMiddle: '';    FBlockEnd: '**/'; FLineEnd: '**/'),
    (FStart: '//:';  FMiddle: '//:'; FBlockEnd: '';    FLineEnd: ''),
    (FStart: ''':';  FMiddle: ''':'; FBlockEnd: ''':'; FLineEnd: ''),
    (FStart: '<!--'; FMiddle: '';    FBlockEnd: '-->'; FLineEnd: '-->'));
  (** A pre-condition comment tag. **)
  strPreConTag = 'precon';
  (** A post-condition comment tag. **)
  strPostConTag = 'postcon';
  (** A parameter comment tag. **)
  strParamTag = 'param';
  (** A return comment tag. **)
  strReturnTag = 'return';
  (** A constant string for formating the type of a parameter **)
  strFormatAs = '   %-*s as ';
  (** A indentation padding for a comment. **)
  iCommentPadding = 2;

Procedure NewComment(Var strCommentText : String; Const Func : TGenericFunction; Const iIndent : Integer;
  Const iMaxCommentWidth : Integer; Var CursorAdjust : TPoint; Const boolPadOut : Boolean); Forward;
Procedure UpdateComment(Var strCommentText : String; Const Func : TGenericFunction;
  Const iMaxCommentWidth, iIndent : Integer; Const boolPadOut : Boolean); Forward;

(**

  This procedure adds text to the resulting comment string.

  @precon  None.
  @postcon Adds text to the resulting comment string.

  @param   strCommentText as a String as a reference
  @param   strText        as a String as a constant

**)
Procedure AddToComment(Var strCommentText : String; Const strText: String);

Begin
  strCommentText := strCommentText + strText;
End;

(**

  This procedure outputs the block pre and post conditions to the comment text.

  @precon  Func must be a valid instance.
  @postcon The pre and post conditions are output to the comment text.

  @param   strCommentText as a String as a reference
  @param   Func           as a TGenericFunction as a constant
  @param   iIndent        as an Integer as a constant
  @param   boolPadOut     as a Boolean as a constant

**)
Procedure BlockConditions(Var strCommentText : String; Const Func : TGenericFunction;
  Const iIndent : Integer; Const boolPadOut : Boolean);

Var
  boolHasCons: Boolean;

Begin
  boolHasCons := False;
  If Func.Comment <> Nil Then
    boolHasCons := Func.Comment.FindTag(strPreConTag) > -1;
  If (doAddPreAndPostToComment In TBADIOptions.BADIOptions.Options) And Not boolHasCons Then
    Begin
      AddToComment(strCommentText, StringOfChar(#32, iIndent) + #32#32'@' + strPreConTag + #32#32 +
        #13#10);
      AddToComment(strCommentText, StringOfChar(#32, iIndent) + #32#32'@' + strPostConTag + #32 +
        #13#10);
      If boolPadOut Then
        AddToComment(strCommentText, #13#10);
    End;
End;

(**

  This method outputs the comment footer.

  @precon  None.
  @postcon The comment footer is output.

  @param   strCommentText as a String as a reference
  @param   boolExtraLine  as a Boolean as a constant
  @param   boolPadOut     as a Boolean as a constant
  @param   CommentType    as a TCommentType as a constant
  @param   iIndent        as an Integer as a constant

**)
Procedure BlockFooter(Var strCommentText : String; Const boolExtraLine, boolPadOut : Boolean;
  Const CommentType : TCommentType; Const iIndent : Integer);

Var
  sl           : TStringList;
  strInsert : String;
  i : Integer;

Begin
  If boolExtraLine Then
    If boolPadOut Then
      AddToComment(strCommentText, #13#10);
  If CommentType In [ctPascalBlock .. ctCPPBlock] Then
    AddToComment(strCommentText, StringOfChar(#32, iIndent));
  If CommentType In [ctPascalBlock, ctPascalBrace, ctCPPBlock] Then
    AddToComment(strCommentText, strCmtTerminals[CommentType].FBlockEnd + #13#10)
  Else
    Begin
      If CommentType In [ctCPPLine, ctVBLine] Then
        strInsert := strCmtTerminals[CommentType].FMiddle
      Else
        strInsert := '';
      sl          := TStringList.Create;
      Try
        sl.Text := strCommentText;
        For i   := 0 To sl.Count - 1 Do
          If sl[i] <> '' Then
            sl[i] := Copy(sl[i], 1, iIndent) + strInsert + Copy(sl[i], iIndent + 1,
              Length(sl[i]) - iIndent)
          Else
            sl[i] := StringOfChar(#32, iIndent) + strInsert;
        strCommentText := sl.Text;
      Finally
        sl.Free;
      End;
    End;
End;

(**

  This procedure outputs the comment header to the comment text.

  @precon  None.
  @postcon The comment header is outptu to the comment text.

  @param   strCommentText as a String as a reference
  @param   CommentType    as a TCommentType as a constant
  @param   iIndent        as an Integer as a constant
  @param   boolPadOut     as a Boolean as a constant

**)
Procedure BlockHeader(Var strCommentText : String; Const CommentType : TCommentType;
  Const iIndent : Integer; Const boolPadOut : Boolean);

Begin
  If CommentType In [ctPascalBlock .. ctCPPBlock] Then
    AddToComment(strCommentText, StringOfChar(#32, iIndent));
  If CommentType In [ctPascalBlock, ctPascalBrace, ctCPPBlock] Then
    AddToComment(strCommentText, strCmtTerminals[CommentType].FStart + #13#10);
  If boolPadOut Then
    AddToComment(strCommentText, #13#10);
End;

(**

  This procedure outputs the parameter block to the comment text.

  @precon  Func must be a valid instance.
  @postcon The functions parameters are output to the comment text if they exist.

  @param   strCommentText as a String as a reference
  @param   Func           as a TGenericFunction as a constant
  @param   boolExtraLine  as a Boolean as a reference
  @param   iIndent        as an Integer as a constant

**)
Procedure BlockParameters(Var strCommentText : String; Const Func : TGenericFunction;
  Var boolExtraLine : Boolean; Const iIndent : Integer);

Var
  iLen         : Integer;
  i            : Integer;
  strType      : String;

Begin
  iLen          := 0;
  boolExtraLine := boolExtraLine Or (Func.ParameterCount > 0);
  For i         := 0 To Func.ParameterCount - 1 Do
    If iLen < Length(Func.Parameters[i].Identifier) Then
      iLen := Length(Func.Parameters[i].Identifier);
  For i    := 0 To Func.ParameterCount - 1 Do
    Begin
      AddToComment(strCommentText, StringOfChar(#32, iIndent));
      AddToComment(strCommentText, Format('  @' + strParamTag + strFormatAs, [iLen,
        Func.Parameters[i].Identifier]));
      If Func.Parameters[i].ParamType <> Nil Then
        Begin
          strType := Func.Parameters[i].ParamType.AsString(False, False);
          AddToComment(strCommentText, Format('%s %s%s%s' + #13#10,
              [strAOrAn[(IsInSet(strType[1], strVowels)) Or Func.Parameters[i].ArrayOf],
                strArrayOf[Func.Parameters[i].ArrayOf], strType,
                strModifier[Func.Parameters[i].ParamModifier]]));
        End;
    End;
End;

(**

  This method returns a string representation of a comment of the type and styles given.

  @precon  None.
  @postcon Returns a string representation of a comment of the type and styles given.

  @param   CommentType     as a TCommentType as a constant
  @param   CommentStyle    as a TCommentStyle as a constant
  @param   iIndent         as an Integer as a constant
  @param   strSelectedText as a String as a constant
  @return  a String

**)
Function BuildBlockComment(Const CommentType: TCommentType; Const CommentStyle: TCommentStyle;
  Const iIndent: Integer; Const strSelectedText: String): String;

Var
  strAllCmtStart : String;
  strBlockCmtEnd : String;
  strLineCmtEnd : String;
  strCmtMiddle : String;

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
        Result := Result + StringOfChar(#32, iIndent - 1) + strCmtMiddle + #32#32 + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strCmtMiddle + #32#32 +
          strSelectedText + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strCmtMiddle + #32#32 + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1) + strBlockCmtEnd + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1);
      End;
    csLine:
      Begin
        Result := Result + strAllCmtStart + #32 + strSelectedText + #32 + strLineCmtEnd + #13#10;
        Result := Result + StringOfChar(#32, iIndent - 1);
      End;
    csInSitu:
      Begin
        Result := Result + strAllCmtStart + #32 + strSelectedText + #32 + strLineCmtEnd + ' ';
      End;
  End;
End;

(**

  This method returns a description for the method if it is a constructor, destructor, getter or setter
  method, else it returns an empty String.

  @precon  Method is a valid instance of a method declatation to be described.
  @postcon Returns a description of the method is applicable. CursorAdjust provide delta movements for
           the cursor from column 1 if the first line of the new comment.

  @param   Func             as a TGenericFunction as a constant
  @param   iIndent          as an Integer as a constant
  @param   boolPadOut       as a Boolean as a constant
  @param   CursorAdjust     as a TPoint as a reference
  @param   iMaxCommentWidth as an Integer as a constant
  @return  a String

**)
Function Description(Const Func: TGenericFunction; Const iIndent: Integer; Const boolPadOut: Boolean;
  Var CursorAdjust: TPoint; Const iMaxCommentWidth: Integer): String;

Begin
  CursorAdjust.X := 0;
  CursorAdjust.Y := 0;
  If Func.Comment = Nil Then
    NewComment(Result, Func, iIndent, iMaxCommentWidth, CursorAdjust, boolPadOut)
  Else
    Begin
      UpdateComment(Result, Func, iMaxCommentWidth, iIndent, boolPadOut);
      CursorAdjust.X := iCommentPadding + iIndent;
    End;
End;

(**

  This method recursively works throug the hierarchy of elements looking for the method which is closest
  to be on or just above the current cursor line.

  @precon  Container must be a valid TElementContainer instance.
  @postcon Recursively works throug the hierarchy of elements looking for the method which is closest to
           be on or just above the current cursor line.

  @param   iLine          as an Integer as a constant
  @param   Container      as a TElementContainer as a constant
  @param   ContainerClass as a TGenericFunctionClass as a constant
  @return  a TGenericFunction

**)
Function FindFunction(Const iLine: Integer; Const Container: TElementContainer;
  Const ContainerClass: TGenericFunctionClass): TGenericFunction;

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

  This function returns the column of the first token on the given line number, i.e. the indentation of
  the code.

  @precon  Module must be a valid instance of a module that is parsed.
  @postcon Returns the column of the first token on the given line number, i.e. the indentation of the
           code.

  @param   Module as a TBaseLanguageModule as a constant
  @param   iLine  as an Integer as a constant
  @return  an Integer

**)
Function FindIndentOfFirstTokenOnLine(Const Module: TBaseLanguageModule; Const iLine: Integer): Integer;

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

(**

  This method outputs the function return type to the comment text.

  @precon Func must be a valid instance.
  @postcon The function return type is output if it exists.

  @param   strCommentText as a String as a reference
  @param   Func           as a TGenericFunction as a constant
  @param   boolExtraLine  as a Boolean as a reference
  @param   iIndent        as an Integer as a constant

**)
Procedure FunctionReturn(Var strCommentText : String; Const Func : TGenericFunction;
  Var boolExtraLine : Boolean; Const iIndent : Integer);

Var
  strReturnType : String;

Begin
  If Func.ReturnType.ElementCount > 0 Then
    Begin
      boolExtraLine := boolExtraLine Or True;
      AddToComment(strCommentText, StringOfChar(#32, iIndent));
      If True Then
      strReturnType := Func.ReturnType.AsString(False, False);
      If Length(strReturnType) > 0 Then
        AddToComment(strCommentText, Format('  @' + strReturnTag + '  %s %s', [
          strAOrAn[IsInSet(strReturnType[1], strVowels)],
          strReturnType]
        ));
      AddToComment(strCommentText, #13#10);
    End;
End;

(**

  This function indent the text for a description.

  @precon  None.
  @postcon Returns an indented version of the passed text.

  @param   strText as a String as a constant
  @param   iIndent as an Integer as a constant
  @return  a String

**)
Function Indent(Const strText: String; Const iIndent: Integer): String;

Begin
  Result := StringOfChar(#32, iIndent) + StringReplace(strText, #13#10,
    #13#10 + StringOfChar(#32, iIndent), [rfReplaceAll]);
End;

(**

  This method creates a new comment based on information in the given function.

  @precon  Func must be a valid instance.
  @postcon A new comment is returned in the comment text.

  @param   strCommentText   as a String as a reference
  @param   Func             as a TGenericFunction as a constant
  @param   iIndent          as an Integer as a constant
  @param   iMaxCommentWidth as an Integer as a constant
  @param   CursorAdjust     as a TPoint as a reference
  @param   boolPadOut       as a Boolean as a constant

**)
Procedure NewComment(Var strCommentText : String; Const Func : TGenericFunction; Const iIndent : Integer;
  Const iMaxCommentWidth : Integer; Var CursorAdjust : TPoint; Const boolPadOut : Boolean);

Var
  j : Integer;
  MD : TStringList;
  C : TComment;
  i : Integer;
  strDescription: String;

Begin
  MD := TBADIOptions.BADIOptions.MethodDescriptions;
  For i := 0 To MD.Count - 1 Do
    If Like(MD.Names[i], Func.Identifier) Then
      Begin
        C := TComment.Create(MD.ValueFromIndex[i], 0, 0);
        Try
          strDescription := Indent(C.AsString(iMaxCommentWidth - iIndent - iCommentPadding, True),
            iIndent + iCommentPadding);
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
  strCommentText := strDescription + #13#10;
  If boolPadOut Then
    strCommentText := strCommentText + #13#10;
  If strDescription = '' Then
    CursorAdjust.X := iCommentPadding + iIndent;
End;

(**

  This function returns the tag information indented and broken into line no wider than iMaxCommentWidth
  characters.

  @precon  Tag must be a valid comment tag.
  @postcon Returns the tag information indented and broken into line no wider than iMaxCommentWidth
           characters.

  @param   iIndent          as an Integer as a constant
  @param   Tag              as a TTag as a constant
  @param   iMaxCommentWidth as an Integer as a constant
  @return  a String

**)
Function OutputTag(Const iIndent: Integer; Const Tag: TTag; Const iMaxCommentWidth: Integer): String;

Const
  iMinTagWidth = 9;

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
        str    := #13#10 + StringOfChar(#32, iIndent + iMinTagWidth);
        If Tag.Tokens[i].Token <> #32 Then
          str := str + Tag.Tokens[i].Token;
      End;
  Result := Result + str + #13#10;
End;

(**

  This method creates a new comment using the existing comment information and returns it int he comment
  text.

  @precon  Func must be a valid instance.
  @postcon A new comment is created from the old comment information and returned in comment text.

  @param   strCommentText   as a String as a reference
  @param   Func             as a TGenericFunction as a constant
  @param   iMaxCommentWidth as an Integer as a constant
  @param   iIndent          as an Integer as a constant
  @param   boolPadOut       as a Boolean as a constant

**)
Procedure UpdateComment(Var strCommentText : String; Const Func : TGenericFunction;
  Const iMaxCommentWidth, iIndent : Integer; Const boolPadOut : Boolean);

  (**

    This procedure outputs the condition tag to the comment text.

    @precon  None.
    @postcon The condition tag is output to the comment text if found.

    @param   strTag  as a String as a constant
    @param   boolCon as a Boolean as a reference

  **)
  Procedure OutputConditionTag(Const strTag : String; Var boolCon : Boolean);

  Var
    i: Integer;

  Begin
    i := Func.Comment.FindTag(strTag);
    If i > -1 Then
      Begin
        strCommentText := strCommentText + OutputTag(iCommentPadding + iIndent, Func.Comment.Tag[i],
          iMaxCommentWidth);
        boolCon := True;
      End;
  End;

Var
  boolCon: Boolean;
  i: Integer;

Begin
  strCommentText := Indent(Func.Comment.AsString(iMaxCommentWidth - iIndent - iCommentPadding, True),
    iCommentPadding + iIndent) + #13#10;
  If boolPadOut Then
    strCommentText := strCommentText + #13#10;
  boolCon  := False;
  OutputConditionTag(strPreConTag, boolCon);
  OutputConditionTag(strPostConTag, boolCon);
  If boolCon Then
    If boolPadOut Then
      strCommentText := strCommentText + #13#10;
  boolCon    := False;
  For i      := 0 To Func.Comment.TagCount - 1 Do
    If Not IsKeyWord(Func.Comment.Tag[i].TagName, [strParamTag, strPostConTag, strPreConTag,
      strReturnTag]) Then
      Begin
        strCommentText := strCommentText + OutputTag(iCommentPadding + iIndent, Func.Comment.Tag[i],
          iMaxCommentWidth);
        boolCon := True;
      End;
  If boolCon Then
    If boolPadOut Then
      strCommentText := strCommentText + #13#10;
End;

(**

  This method position the cursor in the newly created comment.

  @precon  None.
  @postcon The cursor is positioned in the new comment where the code author can start typing to
           describe the element.

  @param   CursorDelta as a TPoint as a reference
  @param   P           as a TPoint as a constant
  @param   CommentType as a TCommentType as a constant
  @param   boolPadOut  as a Boolean as a constant

**)
Procedure UpdateCursor(Var CursorDelta : TPoint; Const P : TPoint; Const CommentType : TCommentType;
  Const boolPadOut : Boolean);

Begin
  Inc(CursorDelta.X, P.X);
  Inc(CursorDelta.Y, iCommentPadding + P.Y);
  If CommentType In [ctVBLine] Then
    Inc(CursorDelta.X, iCommentPadding);
  If CommentType In [ctCPPLine] Then
    Inc(CursorDelta.X, iCommentPadding + 1);
  If Not boolPadOut Then
    Dec(CursorDelta.Y);
  If CommentType In [ctVBLine, ctCPPLine] Then
    Dec(CursorDelta.Y);
End;

(**

  This method writes the method comment to the active editor.

  @precon  Method is a valid instance of a method declaration to be commented.
  @postcon The full comment to be inserted at the cursor is returns with the new cursor position in
           Cursor.

  @param   Func             as a TGenericFunction as a constant
  @param   CommentType      as a TCommentType as a constant
  @param   iIndent          as an Integer as a constant
  @param   boolPadOut       as a Boolean as a constant
  @param   CursorDelta      as a TPoint as a reference
  @param   iMaxCommentWidth as an Integer as a constant
  @return  a String

**)
Function WriteComment(Const Func: TGenericFunction; Const CommentType: TCommentType;
  Const iIndent: Integer; Const boolPadOut: Boolean; Var CursorDelta: TPoint;
  Const iMaxCommentWidth: Integer): String;

Var
  P            : TPoint;
  boolExtraLine: Boolean;

Begin
  Result := '';
  CursorDelta.X := 0;
  CursorDelta.Y := 0;
  boolExtraLine := False;
  BlockHeader(Result, CommentType, iIndent, boolPadOut);
  AddToComment(Result, Description(Func, iIndent, boolPadOut, P, iMaxCommentWidth));
  BlockConditions(Result, Func, iIndent, boolPadOut);
  BlockParameters(Result, Func, boolExtraLine, iIndent);
  FunctionReturn(Result, Func, boolExtraLine, iIndent);
  BlockFooter(Result, boolExtraLine, boolPadOut, CommentType, iIndent);
  UpdateCursor(CursorDelta, P, CommentType, boolPadOut);
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

  This method parses the given code reference ONLY IF there is no current parsing thread.

  @precon  None.
  @postcon Parses the given code reference ONLY IF there is no current parsing thread.

  @param   SuccessfulParseProc as a TParserNotify as a constant
  @param   EditorInfo          as a TEditorInformation as a constant
  @param   RenderDocumentTree  as a TRenderDocumentTree as a constant
  @param   ThreadExceptionMsg  as a TThreadExceptionMsg as a constant
  @return  a Boolean

**)
Function TBrowseAndDocItThreadManager.Parse(Const SuccessfulParseProc: TParserNotify;
  Const EditorInfo: TEditorInformation; Const RenderDocumentTree: TRenderDocumentTree;
  Const ThreadExceptionMsg: TThreadExceptionMsg): Boolean;

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
  If Assigned(FThread) Then
    If Assigned(FThread.FatalException) Then
      Begin
        {$IFDEF EUREKALOG}
        HandleException(FThread.FatalException);
        {$ELSE}
        If FThread.FatalException Is Exception Then
          ShowException(FThread.FatalException, Nil);
        {$ENDIF}
      End;
  FThread := Nil;
  If Assigned(FSuccessfulParseProc) Then
    If Sender Is TBrowseAndDocItThread Then
      FSuccessfulParseProc((Sender As TBrowseAndDocItThread).SuccessfulParse);
End;

{ TBrowseAndDocItThread }

(**

  This is a constructor for the TBrowseAndDocItThread class.

  @precon  None.
  @postcon Creates a suspended thread and sets up a stream with the contents of the active editor and
           then resumed the thread in order to parse the contents.

  @param   EditorInfo         as a TEditorInformation as a constant
  @param   RenderDocumentTree as a TRenderDocumentTree as a constant
  @param   ThreadExceptionMsg as a TThreadExceptionMsg as a constant
  @param   TerminateThread    as a TNotifyEvent as a constant

**)
Constructor TBrowseAndDocItThread.CreateBrowseAndDocItThread(Const EditorInfo: TEditorInformation;
  Const RenderDocumentTree: TRenderDocumentTree; Const ThreadExceptionMsg: TThreadExceptionMsg;
  Const TerminateThread: TNotifyEvent);

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
  //NameThreadForDebugging();
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

  @nometric ExceptionEating

**)
Procedure TBrowseAndDocItThread.Execute;

Begin
  Try
    If FFileName <> '' Then
      FModule := TBADIDispatcher.BADIDispatcher.Dispatcher(FSource, FFileName, FModified,
        [moParse, moCheckForDocumentConflicts])
    Else
      FModule := Nil;
    Try
      If Terminated Then
        Exit;
      Synchronize(RenderModuleExplorer);
      FSuccessfulParse := True;
    Finally
      FModule.Free;
    End;
  Except
    On E: EBADIParserAbort Do
      Exit;
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

  This method displays the raised exception message pass via the FFileName
  field.

  @precon  None.
  @postcon Displays the raised exception message pass via the FFileName
           field.

**)
Procedure TBrowseAndDocItThread.ShowException;

Const
  strMsg =
    'Exception in TBrowseAndDocItThread:'#13#10 +
    '  Exception: %s';
Begin
  If Assigned(FThreadExceptionMsg) Then
    FThreadExceptionMsg(Format(strMsg, [FFileName]));
End;

End.


