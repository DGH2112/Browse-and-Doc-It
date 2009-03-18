Unit TestCommonIDEFunctions;

Interface

Uses
  TestFramework, CommonIDEFunctions, TestBaseLanguageModule;

Type
  //
  // Test Class for the Functions Class Methods.
  //
  TestFunctions = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestFindMethod;
    Procedure TestIndent;
    Procedure TestOutputTag;
    Procedure TestGetDescription_Lookup;
    Procedure TestGetDescription_ReCreateFromComment;
    Procedure TestWriteComment;
  End;

Implementation

Uses
  BaseLanguageModule, Classes, ModuleDispatcher, SysUtils, Windows;

//
// Test Methods for Class Functions.
//
Procedure TestFunctions.TestFindMethod;

Const
  strSourceMethods =
    'Unit MyUnit;'#13#10 +             //  1
    ''#13#10 +                         //  2
    'Interface'#13#10 +                //  3
    ''#13#10 +                         //  4
    'Implementation'#13#10 +           //  5
    ''#13#10 +                         //  6
    'Procedure Hello;'#13#10 +         //  7
    'Begin End;'#13#10 +               //  8
    ''#13#10 +                         //  9
    'Procedure Hello2;'#13#10 +        // 10
      'Procedure Hello3;'#13#10 +      // 11
        'Procedure Hello4;'#13#10 +    // 12
        'Begin End;'#13#10 +           // 13
      'Begin End;'#13#10 +             // 14
      'Procedure Hello5;'#13#10 +      // 15
      'Begin End;'#13#10 +             // 16
    'Begin End;'#13#10 +               // 17
    ''#13#10 +                         // 18
    'End.'#13#10;                      // 19
  strSourceProperties =
    'Unit MyUnit;'#13#10 +             //  1
    ''#13#10 +                         //  2
    'Interface'#13#10 +                //  3
    ''#13#10 +                         //  4
    'Type'#13#10 +                     //  5
    '  TMyClass = Class'#13#10 +       //  6
    '  Private'#13#10 +                //  7
    '    Property MyProperty1;'#13#10 +//  8
    '    Property MyProperty2;'#13#10 +//  9
    '  Public'#13#10 +                 // 10
    '    Property MyProperty3;'#13#10 +// 11
    '    Property MyProperty4;'#13#10 +// 12
    '  End;'#13#10 +                   // 13
    ''#13#10 +                         // 14
    'Implementation'#13#10 +           // 15
    ''#13#10 +                         // 16
    'End.'#13#10;                      // 17

Var
  Source: TMemoryStream;
  Module: TBaseLanguageModule;
  M: TElementContainer;

begin
  Source := TMemoryStream.Create;
  Try
    Source.LoadBufferFromString(strSourceMethods);
    Module := Dispatcher(Source, 'MyPascalFile.pas', False, [moParse]);
    Try
      CheckEquals(0, Module.HeadingCount(strErrors), Module.FirstError);
      M := FindMethod(1, Module, TGenericMethodDecl);
      Check(M = Nil, 'Method found');
      M := FindMethod(7, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 7);
      Checkequals('Hello', M.Identifier);
      M := FindMethod(8, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 7);
      Checkequals('Hello', M.Identifier);
      M := FindMethod(9, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 7);
      Checkequals('Hello', M.Identifier);
      M := FindMethod(10, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 10);
      Checkequals('Hello2', M.Identifier);
      M := FindMethod(11, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 11);
      Checkequals('Hello3', M.Identifier);
      M := FindMethod(12, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 12);
      Checkequals('Hello4', M.Identifier);
      M := FindMethod(13, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 12);
      Checkequals('Hello4', M.Identifier);
      M := FindMethod(14, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 12);
      Checkequals('Hello4', M.Identifier);
      M := FindMethod(15, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 15);
      Checkequals('Hello5', M.Identifier);
      M := FindMethod(19, Module, TGenericMethodDecl);
      Check(M <> Nil, 'Method not found!');
      CheckEquals(M.Line, 15);
      Checkequals('Hello5', M.Identifier);
    Finally
      Module.Free;
    End;

    Source.LoadBufferFromString(strSourceProperties);
    Module := Dispatcher(Source, 'MyPascalFile.pas', False, [moParse]);
    Try
      CheckEquals(0, Module.HeadingCount(strErrors), Module.FirstError);
      M := FindMethod(1, Module, TGenericProperty);
      Check(M = Nil, 'Property found');
      M := FindMethod(8, Module, TGenericProperty);
      Check(M <> Nil, 'Property not found!');
      CheckEquals(M.Line, 8);
      Checkequals('MyProperty1', M.Identifier);
      M := FindMethod(9, Module, TGenericProperty);
      Check(M <> Nil, 'Property not found!');
      CheckEquals(M.Line, 9);
      Checkequals('MyProperty2', M.Identifier);
      M := FindMethod(10, Module, TGenericProperty);
      Check(M <> Nil, 'Property not found!');
      CheckEquals(M.Line, 9);
      Checkequals('MyProperty2', M.Identifier);
      M := FindMethod(11, Module, TGenericProperty);
      Check(M <> Nil, 'Property not found!');
      CheckEquals(M.Line, 11);
      Checkequals('MyProperty3', M.Identifier);
      M := FindMethod(12, Module, TGenericProperty);
      Check(M <> Nil, 'Property not found!');
      CheckEquals(M.Line, 12);
      Checkequals('MyProperty4', M.Identifier);
      M := FindMethod(17, Module, TGenericProperty);
      Check(M <> Nil, 'Property not found!');
      CheckEquals(M.Line, 12);
      Checkequals('MyProperty4', M.Identifier);
    Finally
      Module.Free;
    End;
  Finally
    Source.Free;
  End;
End;


procedure TestFunctions.TestGetDescription_Lookup;

Var
  M  :TGenericMethodDecl;
  CA : TPoint;
  strText: String;

begin
  M := TTestGenericMethodDecl.Create(mtFunction, 'MyFunction', scPrivate, 12, 23);
  Try
    strText := StringReplace(Description(M, 2, True, CA), #32, '.', [rfReplaceAll]);
    CheckEquals(#13#10#13#10, strText);
    CheckEquals(4, CA.X);
    CheckEquals(0, CA.Y);
  Finally
    M.Free;
  End;
  M := TTestGenericMethodDecl.Create(mtFunction, 'btnOKClick', scPrivate, 12, 23);
  Try
    strText := StringReplace(Description(M, 2, True, CA), #32, '.', [rfReplaceAll]);
    CheckEquals('....This.is.an.on.click.event.handler.for.the..button.'#13#10#13#10, strText);
    CheckEquals(46, CA.X);
    CheckEquals(0, CA.Y);
  Finally
    M.Free;
  End;
  M := TTestGenericMethodDecl.Create(mtFunction, 'MyLongFunction', scPrivate, 12, 23);
  Try
    strText := StringReplace(Description(M, 2, True, CA), #32, '.', [rfReplaceAll]);
    CheckEquals('....This.is.a.very.long.description.with.a.cursor.position.' +
      'not.on.the.first.line'#13#10'....so.that.we.can.test.the.Description.' +
      'function.'#13#10#13#10, strText);
    CheckEquals(28, CA.X);
    CheckEquals(1, CA.Y);
  Finally
    M.Free;
  End;
  M := TTestGenericMethodDecl.Create(mtFunction, 'MyFunction', scPrivate, 12, 23);
  Try
    strText := StringReplace(Description(M, 2, False, CA), #32, '.', [rfReplaceAll]);
    CheckEquals(#13#10, strText);
    CheckEquals(4, CA.X);
    CheckEquals(0, CA.Y);
  Finally
    M.Free;
  End;
  M := TTestGenericMethodDecl.Create(mtFunction, 'btnOKClick', scPrivate, 12, 23);
  Try
    strText := StringReplace(Description(M, 2, False, CA), #32, '.', [rfReplaceAll]);
    CheckEquals('....This.is.an.on.click.event.handler.for.the..button.'#13#10, strText);
    CheckEquals(46, CA.X);
    CheckEquals(0, CA.Y);
  Finally
    M.Free;
  End;
  M := TTestGenericMethodDecl.Create(mtFunction, 'MyLongFunction', scPrivate, 12, 23);
  Try
    strText := StringReplace(Description(M, 2, False, CA), #32, '.', [rfReplaceAll]);
    CheckEquals('....This.is.a.very.long.description.with.a.cursor.position.' +
      'not.on.the.first.line'#13#10'....so.that.we.can.test.the.Description.' +
      'function.'#13#10, strText);
    CheckEquals(28, CA.X);
    CheckEquals(1, CA.Y);
  Finally
    M.Free;
  End;
end;

Const
  strExistingComment =
    '  This is a longish comment used as a test for the Description function so that '#13#10 +
    '  the function can demonstrated that it can correctly extract the comment '#13#10 +
    '  information from the text along with the tags.'#13#10 +
    ''#13#10 +
    '  @precon  None.'#13#10 +
    '  @postcon Do something wonderful Hal.'#13#10 +
    ''#13#10 +
    '  @param  iParam   as an Integer'#13#10 +
    '  @param  strParam as a String'#13#10 +
    '  @return a Boolean'#13#10 +
    ''#13#10 +
    '  @note   This is a test tag.'#13#10 +
    '  @see    See how this works!'#13#10 +
    '  @refactor Oops tag too long.'#13#10 +
    ''#13#10;

procedure TestFunctions.TestGetDescription_ReCreateFromComment;

Var
  C: TComment;
  M: TGenericMethodDecl;
  P: TGenericParameter;
  T: TGenericTypeDecl;
  strText: String;
  CA: TPoint;
  strTestText: String;

begin
  C := TComment.CreateComment(strExistingComment, 0, 0);
  Try
    M := TTestGenericMethodDecl.Create(mtFunction, 'MyFunction', scPrivate, 12, 23);
    Try
      M.Comment := C;
      T := TTestGenericTypeDecl.Create('', scLocal, 0, 0, iiNone, Nil);
      Try
        T.AddToken('Int64');
        P := TTestGenericParameter.Create(pamNone, 'iParam', False, T, '', scLocal, 0, 0);
      Finally
        T.Free;
      End;
      M.AddParameter(P);
      T := TTestGenericTypeDecl.Create('', scLocal, 0, 0, iiNone, Nil);
      Try
        T.AddToken('String');
        P := TTestGenericParameter.Create(pamNone, 'strParam', False, T, '', scLocal, 0, 0);
      Finally
        T.Free;
      End;
      M.ReturnType := TTestGenericTypeDecl.Create('', scLocal, 0, 0, iiNone, Nil);
      M.ReturnType.AddToken('Boolean');
      M.AddParameter(P);
      strText := StringReplace(Description(M, 2, True, CA), #32, '.', [rfReplaceAll]);
      strTestText := StringReplace(
        '    This is a longish comment used as a test for the Description function so '#13#10 +
        '    that the function can demonstrated that it can correctly extract the comment'#13#10 +
        '    information from the text along with the tags.'#13#10 +
        ''#13#10 +
        '    @precon  None.'#13#10 +
        '    @postcon Do something wonderful Hal.'#13#10 +
        ''#13#10 +
        '    @note    This is a test tag.'#13#10 +
        '    @see     See how this works!'#13#10 +
        '    @refactorOops tag too long.'#13#10 +
        ''#13#10, #32, '.', [rfReplaceAll]);
      CheckEquals(strTestText, strText);
      CheckEquals(0, CA.X);
      CheckEquals(0, CA.Y);
      strText := StringReplace(Description(M, 2, False, CA), #32, '.', [rfReplaceAll]);
      strTestText := StringReplace(
        '    This is a longish comment used as a test for the Description function so '#13#10 +
        '    that the function can demonstrated that it can correctly extract the comment'#13#10 +
        '    information from the text along with the tags.'#13#10 +
        '    @precon  None.'#13#10 +
        '    @postcon Do something wonderful Hal.'#13#10 +
        '    @note    This is a test tag.'#13#10 +
        '    @see     See how this works!'#13#10 +
        '    @refactorOops tag too long.'#13#10, #32, '.', [rfReplaceAll]);
      CheckEquals(strTestText, strText);
      CheckEquals(0, CA.X);
      CheckEquals(0, CA.Y);
    Finally
      M.Free;
    End;
  Finally
    C.Free;
  End;
end;

Procedure TestFunctions.TestIndent;

Var
  strText : String;

Begin
  strText :=
    'This is some'#13#10 +
    'text'#13#10 +
    'that'#13#10 +
    '  should be indented'#13#10 +
    '  .'#13#10;
  strText := Indent(strText, 2);
  CheckEquals('  This is some'#13#10'  text'#13#10'  that'#13#10 +
    '    should be indented'#13#10'    .'#13#10'  ', strText);
End;

Procedure TestFunctions.TestOutputTag;

Const
  strTagTokens : Array[1..58] of String = ('This', #32, 'is', #32, 'an', #32,
    'example', #32, 'todo', #32, 'item', #32, 'that', #32, 'should', #32, 'span',
    #32, 'past', #32, 'the', #32, '80ish', #32, 'character', #32, 'in', #32,
    'the', #32, 'editor', #32, 'so', #32, 'that', #32, 'it', #32, 'can', #32,
    'be', #32, 'demonstrated', #32, 'that', #32, 'the', #32, 'wrapping', #32,
    'is', #32, 'being', #32, 'done', #32, 'properly', '.');

Var
  T : TTag;
  i: Integer;
  strText: String;

Begin
  T := TTag.Create('todo', 12, 23);
  Try
    For i := Low(strTagTokens) To High(strTagTokens) Do
      T.AddToken(strTagTokens[i]);
    // Replace space with period to ease debugging output
    strText := StringReplace(OutputTag(2, T), #32, '.', [rfReplaceAll]);
    CheckEquals(
      '..@todo....This.is.an.example.todo.item.that.should.span.past.the.80ish.'#13#10 +
      '...........character.in.the.editor.so.that.it.can.be.demonstrated.that.the.'#13#10 +
      '...........wrapping.is.being.done.properly.'#13#10, strText);
  Finally
    T.Free;
  End;
End;

Procedure TestFunctions.TestWriteComment;

Var
  C: TComment;
  M: TGenericMethodDecl;
  P: TGenericParameter;
  T: TGenericTypeDecl;
  strText: String;
  CursorDelta: TPoint;
  strTestText: String;

begin
  C := TComment.CreateComment(strExistingComment, 0, 0);
  Try
    M := TTestGenericMethodDecl.Create(mtFunction, 'MyFunction', scPrivate, 12, 23);
    Try
      M.Comment := C;
      T := TTestGenericTypeDecl.Create('', scLocal, 0, 0, iiNone, Nil);
      Try
        T.AddToken('Int64');
        P := TTestGenericParameter.Create(pamNone, 'iParam', False, T, '', scLocal, 0, 0);
      Finally
        T.Free;
      End;
      M.AddParameter(P);
      T := TTestGenericTypeDecl.Create('', scLocal, 0, 0, iiNone, Nil);
      Try
        T.AddToken('String');
        P := TTestGenericParameter.Create(pamNone, 'strParam', False, T, '', scLocal, 0, 0);
      Finally
        T.Free;
      End;
      M.ReturnType := TTestGenericTypeDecl.Create('', scLocal, 0, 0, iiNone, Nil);
      M.ReturnType.AddToken('Boolean');
      M.AddParameter(P);
      strText := StringReplace(WriteComment(M, ctPascalBlock, 2, True, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..(**'#13#10 +
        ''#13#10 +
        '....This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '....that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '....information.from.the.text.along.with.the.tags.'#13#10 +
        ''#13#10 +
        '....@precon..None.'#13#10 +
        '....@postcon.Do.something.wonderful.Hal.'#13#10 +
        ''#13#10 +
        '....@note....This.is.a.test.tag.'#13#10 +
        '....@see.....See.how.this.works!'#13#10 +
        '....@refactorOops.tag.too.long.'#13#10 +
        ''#13#10 +
        '....@param...iParam...as.an.Int64'#13#10 +
        '....@param...strParam.as.a.String'#13#10 +
        '....@return..a.Boolean'#13#10 +
        ''#13#10 +
        '..**)'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctPascalBrace, 2, True, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..{:'#13#10 +
        ''#13#10 +
        '....This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '....that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '....information.from.the.text.along.with.the.tags.'#13#10 +
        ''#13#10 +
        '....@precon..None.'#13#10 +
        '....@postcon.Do.something.wonderful.Hal.'#13#10 +
        ''#13#10 +
        '....@note....This.is.a.test.tag.'#13#10 +
        '....@see.....See.how.this.works!'#13#10 +
        '....@refactorOops.tag.too.long.'#13#10 +
        ''#13#10 +
        '....@param...iParam...as.an.Int64'#13#10 +
        '....@param...strParam.as.a.String'#13#10 +
        '....@return..a.Boolean'#13#10 +
        ''#13#10 +
        '..}'#13#10;
      strText := StringReplace(WriteComment(M, ctCPPBlock, 2, True, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '../**'#13#10 +
        ''#13#10 +
        '....This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '....that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '....information.from.the.text.along.with.the.tags.'#13#10 +
        ''#13#10 +
        '....@precon..None.'#13#10 +
        '....@postcon.Do.something.wonderful.Hal.'#13#10 +
        ''#13#10 +
        '....@note....This.is.a.test.tag.'#13#10 +
        '....@see.....See.how.this.works!'#13#10 +
        '....@refactorOops.tag.too.long.'#13#10 +
        ''#13#10 +
        '....@param...iParam...as.an.Int64'#13#10 +
        '....@param...strParam.as.a.String'#13#10 +
        '....@return..a.Boolean'#13#10 +
        ''#13#10 +
        '..**/'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctVBLine, 2, True, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..'':'#13#10 +
        '..'':..This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '..'':..that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '..'':..information.from.the.text.along.with.the.tags.'#13#10 +
        '..'':'#13#10 +
        '..'':..@precon..None.'#13#10 +
        '..'':..@postcon.Do.something.wonderful.Hal.'#13#10 +
        '..'':'#13#10 +
        '..'':..@note....This.is.a.test.tag.'#13#10 +
        '..'':..@see.....See.how.this.works!'#13#10 +
        '..'':..@refactorOops.tag.too.long.'#13#10 +
        '..'':'#13#10 +
        '..'':..@param...iParam...as.an.Int64'#13#10 +
        '..'':..@param...strParam.as.a.String'#13#10 +
        '..'':..@return..a.Boolean'#13#10 +
        '..'':'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctCPPLine, 2, True, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..//:'#13#10 +
        '..//:..This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '..//:..that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '..//:..information.from.the.text.along.with.the.tags.'#13#10 +
        '..//:'#13#10 +
        '..//:..@precon..None.'#13#10 +
        '..//:..@postcon.Do.something.wonderful.Hal.'#13#10 +
        '..//:'#13#10 +
        '..//:..@note....This.is.a.test.tag.'#13#10 +
        '..//:..@see.....See.how.this.works!'#13#10 +
        '..//:..@refactorOops.tag.too.long.'#13#10 +
        '..//:'#13#10 +
        '..//:..@param...iParam...as.an.Int64'#13#10 +
        '..//:..@param...strParam.as.a.String'#13#10 +
        '..//:..@return..a.Boolean'#13#10 +
        '..//:'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctPascalBlock, 2, False, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..(**'#13#10 +
        '....This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '....that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '....information.from.the.text.along.with.the.tags.'#13#10 +
        '....@precon..None.'#13#10 +
        '....@postcon.Do.something.wonderful.Hal.'#13#10 +
        '....@note....This.is.a.test.tag.'#13#10 +
        '....@see.....See.how.this.works!'#13#10 +
        '....@refactorOops.tag.too.long.'#13#10 +
        '....@param...iParam...as.an.Int64'#13#10 +
        '....@param...strParam.as.a.String'#13#10 +
        '....@return..a.Boolean'#13#10 +
        '..**)'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctPascalBrace, 2, False, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..{:'#13#10 +
        '....This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '....that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '....information.from.the.text.along.with.the.tags.'#13#10 +
        '....@precon..None.'#13#10 +
        '....@postcon.Do.something.wonderful.Hal.'#13#10 +
        '....@note....This.is.a.test.tag.'#13#10 +
        '....@see.....See.how.this.works!'#13#10 +
        '....@refactorOops.tag.too.long.'#13#10 +
        '....@param...iParam...as.an.Int64'#13#10 +
        '....@param...strParam.as.a.String'#13#10 +
        '....@return..a.Boolean'#13#10 +
        '..}'#13#10;
      strText := StringReplace(WriteComment(M, ctCPPBlock, 2, False, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '../**'#13#10 +
        '....This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '....that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '....information.from.the.text.along.with.the.tags.'#13#10 +
        '....@precon..None.'#13#10 +
        '....@postcon.Do.something.wonderful.Hal.'#13#10 +
        '....@note....This.is.a.test.tag.'#13#10 +
        '....@see.....See.how.this.works!'#13#10 +
        '....@refactorOops.tag.too.long.'#13#10 +
        '....@param...iParam...as.an.Int64'#13#10 +
        '....@param...strParam.as.a.String'#13#10 +
        '....@return..a.Boolean'#13#10 +
        '..**/'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctVBLine, 2, False, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..'':..This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '..'':..that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '..'':..information.from.the.text.along.with.the.tags.'#13#10 +
        '..'':..@precon..None.'#13#10 +
        '..'':..@postcon.Do.something.wonderful.Hal.'#13#10 +
        '..'':..@note....This.is.a.test.tag.'#13#10 +
        '..'':..@see.....See.how.this.works!'#13#10 +
        '..'':..@refactorOops.tag.too.long.'#13#10 +
        '..'':..@param...iParam...as.an.Int64'#13#10 +
        '..'':..@param...strParam.as.a.String'#13#10 +
        '..'':..@return..a.Boolean'#13#10;
      CheckEquals(strTestText, strText);
      strText := StringReplace(WriteComment(M, ctCPPLine, 2, False, CursorDelta),
        #32, '.', [rfReplaceAll]);
      strTestText :=
        '..//:..This.is.a.longish.comment.used.as.a.test.for.the.Description.function.so.'#13#10+
        '..//:..that.the.function.can.demonstrated.that.it.can.correctly.extract.the.comment'#13#10 +
        '..//:..information.from.the.text.along.with.the.tags.'#13#10 +
        '..//:..@precon..None.'#13#10 +
        '..//:..@postcon.Do.something.wonderful.Hal.'#13#10 +
        '..//:..@note....This.is.a.test.tag.'#13#10 +
        '..//:..@see.....See.how.this.works!'#13#10 +
        '..//:..@refactorOops.tag.too.long.'#13#10 +
        '..//:..@param...iParam...as.an.Int64'#13#10 +
        '..//:..@param...strParam.as.a.String'#13#10 +
        '..//:..@return..a.Boolean'#13#10;
      CheckEquals(strTestText, strText);
    Finally
      M.Free;
    End;
  Finally
    C.Free;
  End;
End;

Initialization
  With BrowseAndDocItOptions.MethodDescriptions Do
    Begin
      Clear;
      Add('create*=This is a constructor for the | class.');
      Add('destroy=This is a destructor for the | class.');
      Add('get*=This is a getter method for the | property.');
      Add('set*=This is a setter method for the | property.');
      Add('btn*Click=This is an on click event handler for the | button.');
      Add('act*Execute=This is an on execute event handler for the | action.');
      Add('*long*=This is a very long description with a cursor position not ' +
        'on the first line so that we can test the |Description function.');
    End;
  RegisterTest('IDE Common Function Tests', TestFunctions.Suite);
End.