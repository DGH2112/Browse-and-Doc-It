(**
  
  This module contains a routine for generically parsing code for use with the
  module explorer and documentation engine.

  @Author  David Hoyle
  @Date    01 Oct 2008
  @Version 1.0

  @todo    Tokenize should also be language independant.

**)
Unit GenericTokenizer;

Interface

Uses
  Classes, BaseLanguageModule;

  Function Tokenize(strText : String; var KeyWords : TKeyWords;
    iLimit : Integer = 999999) : TStringList;

Implementation

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon  strText si the line of text to be tokenised
  @postcon Returns a new string list of the tokenized string

  @note    The string list returnsed must be destroyed be the calling method.

  @param   strText  as a String
  @param   KeyWords as a TKeyWords as a reference
  @param   iLimit   as an Integer
  @return  a TStringList

**)
Function Tokenize(strText : String; var KeyWords : TKeyWords;
  iLimit : Integer = 999999) : TStringList;


Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  (** Token buffer. **)
  strToken : String;
  CurToken : BaseLanguageModule.TTokenType;
  LastToken : BaseLanguageModule.TTokenType;
  BlockType : TBlockType;
  (** Token size **)
  iTokenLen : Integer;
  i : Integer;

Begin
  Result := TStringList.Create;
  BlockType := btNoBlock;
  strToken := '';
  CurToken := ttUnknown;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For i := 1 To Length(strText) Do
    Begin
      LastToken := CurToken;
      If strText[i] In [#32, #9] Then
        CurToken := ttWhiteSpace
      Else If strText[i] In ['#', '_', 'a'..'z', 'A'..'Z'] Then
        Begin
          If (LastToken = ttNumber) And (strText[i] In ['A'..'F', 'a'..'f']) Then
            CurToken := ttNumber
          Else
            CurToken := ttIdentifier;
        End
      Else If strText[i] In ['$', '0'..'9'] Then
        Begin
          CurToken := ttNumber;
          If LastToken = ttIdentifier Then
            CurToken := ttIdentifier;
        End
      Else If strText[i] In [#10, #13] Then
        CurToken := ttLineEnd
      Else If strText[i] In [''''] Then
        CurToken := ttStringLiteral
      Else If strText[i] In [#0..#255] - ['#', '_', 'a'..'z', 'A'..'Z', '$', '0'..'9'] Then
        CurToken := ttSymbol
      Else
        CurToken := ttUnknown;
      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If ((BlockType In [btStringLiteral]) And (CurToken <> ttLineEnd)) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := strText[i];
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  If KeyWords <> Nil Then
                    If IsKeyWord(strToken, KeyWords) Then
                      LastToken := ttReservedWord;
                  Result.AddObject(strToken, TObject(LastToken));
                  If Result.Count >= iLimit Then
                    Begin
                      Result.Add('...');
                      Exit;
                    End;
                End;
             BlockType := btNoBlock;
             iTokenLen := 1;
             SetLength(strToken, iTokenCapacity);
             strToken[iTokenLen] := strText[i];
            End;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strText[i];
        End;

      // Check for string literals
      If CurToken = ttStringLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If KeyWords <> Nil Then
        If IsKeyWord(strToken, KeyWords) Then
          CurToken := ttReservedWord;
      Result.AddObject(strToken, TObject(CurToken));
    End;
End;

End.
