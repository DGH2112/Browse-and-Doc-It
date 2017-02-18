(**

  This module contains class to represent document issues and conflicts.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.DocIssue;

Interface

Uses
  BADI.ElementContainer,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class defines a document error. **)
  TDocIssue = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMsg: String;
    FMethod : String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    (**
      Returns the error method of the error stored.
      @precon  None.
      @postcon Returns the error method of the error stored.
      @return  a String
    **)
    Property Method : String Read FMethod;
    (**
      Returns the error message.
      @precon  None.
      @postcon Returns the error message.
      @return  a String
    **)
    Property Msg : String Read FMsg;
  Public
    Constructor Create(const strMsg : String; AScope : TScope; const strMethod : String; iLine,
      iCol : Integer; AImageIndex : TBADIImageIndex); Reintroduce; Overload;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a class to represent a module documentation conflict. **)
  TDocumentConflict = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMessage       : String;
    FCommentLine   : Integer;
    FCommentColumn : Integer;
  Public
    Constructor Create(Const Args: Array of Const; iIdentLine,
      iIdentColumn, iCommentLine, iCommentCol : Integer;
      const strDocConflictMsg, strDocConflictDesc : String;
      AImageIndex : TBADIImageIndex); ReIntroduce;
    Destructor Destroy; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property defines the line where the comment associated with the
      conflict starts.
      @precon  None.
      @postcon Return the line where the comment associated with the conflict
               starts.
      @return  an Integer
    **)
    Property CommentLine : Integer Read FCommentLine;
    (**
      This property defines the column where the comment associated with the
      conflict starts.
      @precon  None.
      @postcon Return the column where the comment associated with the conflict
               starts.
      @return  an Integer
    **)
    Property CommentColumn : Integer Read FCommentColumn;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options,
  BADI.Comment;

Var
  (** This variable provides an incremental number for making doc conflict
      messages unique. **)
  iDocConflictCounter: Integer;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Override the default method and returns the Document Error Message .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDocIssue.AsString(boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := FMsg;
  If doShowParserErrorOrigin In BrowseAndDocItOptions.Options Then
    Result := Result + Format(' [%s]', [FMethod]);
end;

(**


  This is the constructor method for the TDocError class.


  @precon  strMsg is the error message to create a doc error for, iLine is the

           line number of the error, iCol is the column number for the

           message, strExceptionMethod is the name of the method the

           error occurred in and ErrType determines if the mesage is a

           warning or an error.

  @postcon Initialises the class.


  @param   strMsg      as a String as a Constant
  @param   AScope      as a TScope
  @param   strMethod   as a String as a Constant
  @param   iLine       as an Integer
  @param   iCol        as an Integer
  @param   AImageIndex as a TBADIImageIndex

**)
Constructor TDocIssue.Create(const strMsg: String; AScope: TScope; const strMethod: String;
  iLine, iCol: Integer; AImageIndex: TBADIImageIndex);

Begin
  Inherited Create(Format('%4.4d', [iDocConflictCounter]), AScope, iLine, iCol, AImageIndex, Nil);
  Inc(iDocConflictCounter);
  FMsg := strMsg;
  FMethod := strMethod;
End;

(**

  This is the constructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Initialises the Conflict class.

  @param   Args               as an Array Of Const as a constant
  @param   iIdentLine         as an Integer
  @param   iIdentColumn       as an Integer
  @param   iCommentLine       as an Integer
  @param   iCommentCol        as an Integer
  @param   strDocConflictMsg  as a String as a Constant
  @param   strDocConflictDesc as a String as a Constant
  @param   AImageIndex        as a TBADIImageIndex

**)
Constructor TDocumentConflict.Create(Const Args: Array Of Const;
  iIdentLine, iIdentColumn, iCommentLine, iCommentCol: Integer;
  const strDocConflictMsg, strDocConflictDesc: String; AImageIndex: TBADIImageIndex);

Begin
  Inherited Create(Format('%4.4d', [iDocConflictCounter]), scGlobal, iIdentLine, iIdentColumn,
    AImageIndex, Nil);
  Inc(iDocConflictCounter);
  FMessage := Format(strDocConflictMsg, Args);
  FCommentLine := iCommentLine;
  FCommentColumn := iCommentCol;
  Comment := TComment.Create(strDocConflictDesc, iCommentLine, iCommentCol);
End;

(**

  This is the destructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Frees the comment.

**)
Destructor TDocumentConflict.Destroy;
Begin
  Comment.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Return the document conflict message .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TDocumentConflict.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := FMessage;
End;

(** Initialises the document conflict counter to 1. Each Issue or Conflict increments it so there
    is a unique number for each. **)
Initialization
  iDocConflictCounter := 1;
End.
