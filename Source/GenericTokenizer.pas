(**
  
  This module contains a routine for generically parsing code for use with the
  module explorer and documentation engine.

  @Author  David Hoyle
  @Date    14 Sep 2008
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

  (**

    This function returns the token type for a given character and last token
    type.

    @precon  Ch is the character for which the token type assessment needs to be
             taken for and LastToken os the type of the last token as this has
             an effect on some characters.
    @postcon Returns the token type for the given character.

    @param   Ch           as a Char
    @param   LastCharType as a TTokenType
    @return  a TTokenType

  **)
  Function GetTokenType(Ch : Char; LastCharType : TTokenType) : TTokenType;

  Begin
    If ch In [#32, #9] Then
      Result := ttWhiteSpace
    Else If ch In ['#', '_', 'a'..'z', 'A'..'Z'] Then
      Begin
        If (LastCharType = ttNumber) And (Ch In ['A'..'F', 'a'..'f']) Then
          Result := ttNumber
        Else
          Result := ttIdentifier;
      End
    Else If ch In ['$', '0'..'9'] Then
      Begin
        Result := ttNumber;
        If LastCharType = ttIdentifier Then
          Result := ttIdentifier;
      End
    Else If ch In [#10, #13] Then
      Result := ttLineEnd
    Else If ch In [''''] Then
      Result := ttStringLiteral
    Else If ch In [#0..#255] - ['#', '_', 'a'..'z', 'A'..'Z', '$', '0'..'9'] Then
      Result := ttSymbol
    Else
      Result := ttUnknown;
  End;

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
      CurToken := GetTokenType(strText[i], LastToken);

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
