(**

  This module contains class to represent document issues and conflicts.

  @Author  David Hoyle
  @Version 1.1
  @Date    19 Jan 2020

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
Unit BADI.DocIssue;

Interface

Uses
  BADI.ElementContainer,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class defines a document error. **)
  TDocIssue = Class(TElementContainer)
  Strict Private
    FMsg      : String;
    FErrorType: TErrorType;
  Strict Protected
    (**
      Returns the error message.
      @precon  None.
      @postcon Returns the error message.
      @return  a String
    **)
    Property Msg : String Read FMsg;
  Public
    Constructor Create(Const strMsg: String; Const AScope: TScope; Const iLine, iCol: Integer;
      Const eErrorType : TErrorType); Reintroduce; Overload;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property returns the error type of the issue.
      @precon  None.
      @postcon Returns the error type of the issue.
      @return  a TErrorType
    **)
    Property ErrorType : TErrorType Read FErrorType;
  End;

  (** This is a class to represent a module documentation conflict. **)
  TDocumentConflict = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FMessage       : String;
    FCommentLine   : Integer;
    FCommentColumn : Integer;
    FConflictType  : TBADIConflictType;
  Public
    //: @nometric LongParameterList
    Constructor Create(Const Args: Array Of Const; Const iIdentLine, iIdentColumn, iCommentLine,
      iCommentCol : Integer; Const strDocConflictMsg, strDocConflictDesc : String;
      Const AImageIndex : TBADIImageIndex; Const eConflictType : TBADIConflictType); ReIntroduce;
    Destructor Destroy; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
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
    (**
      This property returns the type of the document conflict.
      @precon  None.
      @postcon Returns the type of the document conflict.
      @return  a TBADIConflictType
    **)
    Property ConflictType : TBADIConflictType Read FConflictType;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options,
  BADI.Comment, BADI.Functions;

Var
  (** This variable provides an incremental number for making doc conflict
      messages unique. **)
  iDocConflictCounter: Integer;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Override the default method and returns the Document Error Message.

  @nohint boolShowIdentifier boolForDocumentation

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDocIssue.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean): String;

Begin
  Result := FMsg;
End;

(**

  This is the constructor method for the TDocError class.

  @precon  strMsg is the error message to create a doc error for, iLine is the line number of the
           error, iCol is the column number for the message, strExceptionMethod is the name of
           the method the error occurred in and ErrType determines if the mesage is a warning or
           an error.
  @postcon Initialises the class.

  @param   strMsg     as a String as a constant
  @param   AScope     as a TScope as a constant
  @param   iLine      as an Integer as a constant
  @param   iCol       as an Integer as a constant
  @param   eErrorType as a TErrorType as a constant

**)
Constructor TDocIssue.Create(Const strMsg: String; Const AScope: TScope; Const iLine, iCol: Integer;
  Const eErrorType : TErrorType);

Const
  strOutputFmt = '%4.4d';

Var
  iImageIndex: TBADIImageIndex;

Begin
  Case eErrorType Of
    etHint:    iImageIndex := iiHint;
    etWarning: iImageIndex := iiWarning;
    etError:   iImageIndex := iiError;
  Else
    iImageIndex := iiNone;
  End;
  Inherited Create(Format(strOutputFmt, [iDocConflictCounter]), AScope, iLine, iCol, iImageIndex, Nil);
  Inc(iDocConflictCounter);
  FMsg := strMsg;
  FErrorType := eErrorType;
End;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Return the document conflict message .

  @nohint boolShowIdentifier boolForDocumentation

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDocumentConflict.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := FMessage;
End;

(**

  This is the constructor method for the TDocumentConflict class.

  @precon  None.
  @postcon Initialises the Conflict class.

  @nometricLongParameterList

  @param   Args               as an Array Of Const as a constant
  @param   iIdentLine         as an Integer as a constant
  @param   iIdentColumn       as an Integer as a constant
  @param   iCommentLine       as an Integer as a constant
  @param   iCommentCol        as an Integer as a constant
  @param   strDocConflictMsg  as a String as a constant
  @param   strDocConflictDesc as a String as a constant
  @param   AImageIndex        as a TBADIImageIndex as a constant
  @param   eConflictType      as a TBADIConflictType as a constant

**)
Constructor TDocumentConflict.Create(Const Args: Array Of Const; Const iIdentLine, iIdentColumn,
  iCommentLine, iCommentCol: Integer; Const strDocConflictMsg, strDocConflictDesc: String;
  Const AImageIndex: TBADIImageIndex; Const eConflictType : TBADIConflictType);

Const
  strOutputFmt = '%4.4d';

Begin
  Inherited Create(Format(strOutputFmt, [iDocConflictCounter]), scGlobal, iIdentLine, iIdentColumn,
    AImageIndex, Nil);
  FConflictType := eConflictType;
  Inc(iDocConflictCounter);
  If Length(Args) > 0 Then
    FMessage := Format(strDocConflictMsg, Args)
  Else
    FMessage := strDocConflictMsg;
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

(** Initialises the document conflict counter to 1. Each Issue or Conflict increments it so there
    is a unique number for each. **)
Initialization
  iDocConflictCounter := 1;
End.
