(**

  This module contains a documentation engine for producing static HTML
  information.

  @Author  David Hoyle
  @Date    29 Apr 2017
  @Version 1.0

**)
Unit BADI.HTMLDocumentation;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  Classes,
  BADI.Base.Module,
  BADI.Base.Documentation,
  Contnrs,
  BADI.Types,
  BADI.ElementContainer;

Type
  (** This class represent a set of documenation conflicts for a module. **)
  TSumDocCon = Class
  Private
    FModule    : String;
    FConflicts : TStringList;
  Protected
  Public
    Constructor Create(Const strModule : String);
    Destructor Destroy; Override;
    (**
      This property provides access to the string list of conflicts.
      @precon  None.
      @postcon Provides access to the string list of conflicts.
      @return  a TStringList
    **)
    Property Conflicts : TStringList Read FConflicts;
    (**
      This property returns the name of the module the conflicts belong to.
      @precon  None.
      @postcon Returns the name of the module the conflicts belong to.
      @return  a String
    **)
    Property Module : String Read FModule;
  End;

  (** This is a class to produce HTML documentation. **)
  THTMLDocumentation = Class(TBaseDocumentation)
  Strict Private
    FCurrentModule          : TBaseLanguageModule;
    FProgressIndex          : Integer;
    FModuleSpecialTagNodes  : TObjectList;
    FSummarySpecialTagNodes : TObjectList;
    FErrors                 : TStringList;
    FWarnings               : TStringList;
    FHints                  : TStringList;
    FHeaderLevel            : Integer;
    FSummaryContent         : TStringList;
    FTitle                  : String;
    FReservedWords          : TKeyWords;
    FDirectives             : TKeyWords;
    FScopesToDocument       : TScopes;
    FIndex                  : TStringList;
    FHTMLFileName           : String;
    FSummaryDocConflicts    : TObjectList;
    FSections               : TStringList;
    FPerfCounters           : TStringList;
  Strict Protected
    procedure GenerateImages(Const strImageDirectory: String);
    function ExpandLinkTag(Const strToken: String): String;
    Procedure OutputHTMLDocumentation(Const strFileName : String);
    Procedure GenerateSummary;
    Procedure GenerateCSS;
    Procedure GenerateIndex;
    procedure GenerateSchema;
    Procedure GenerateHTML(Const slHTMLFile : TStringList; Const strTitle : String);
    Procedure GenerateModuleList(Const slHTMLFile : TStringList);
    Procedure GenerateSectionList(Const slHTMLFile : TStringList);
    Procedure GenerateContent(Const slHTMLFile : TStringList);
    Function FindInsertionPoint(Const slHTMLFile : TStringList;
      Const strText : String) : Integer;
    Function GetStringResource(Const strName : String) : String;
    Function GetMainDocument : String; Override;
    Procedure GenerateErrorsWarningsHints(Const slContents : TStringList);
    Procedure GenerateDocumentConflicts(Const slContents : TStringList);
    Procedure GenerateSpecialTags(Const slContents : TStringList);
    Procedure OutputComment(Const slContents : TStringList; Const E : TElementContainer;
      Const iIndentLevel : Integer);
    Procedure OutputContainers(Const slContents : TStringList;
      Const Container : TElementContainer; Const iIndentLevel : Integer;
      Const strContainerLabel : String);
    Function N(Const strText : String) : String;
    Function P(Const strText : String) : String;
    Procedure InitialiseSummary;
    Procedure OutputErrorsWarningsAndHints(Const slEWH : TStringList; Const strSectionTitle,
      strLIType : String; Const AImageIndex : TBADIImageIndex);
    Procedure OutputSummaryDocumentationConflicts;
    { HTML Tag Outputs }
    Function A(Const strText, strHREF : String; Const strName : String = '') : String;
    Function H(Const strText : String; Const iLevel : Integer; Const AImage : TBADIImageIndex;
      Const AScope : TScope) : String;
    Function IMG(Const AImageIndex : TBADIImageIndex; Const AScope : TScope) : String;
    Function LI(Const strClass, strText : String) : String;
    Procedure OutputSummarySpecialTags;
    Function GetSumDocCons(Const iIndex: Integer): TSumDocCon;
    Function ImageFileName(eImageIndex : TBADIImageIndex; eScope : TScope) : String;
    (**
      This property provides access to an array of documentation conflicts for all the modules.
      @precon  iIndex must be a valid index into the array 0 to count - 1.
      @postcon Provides access to an array of documentation conflicts for all the modules.
      @param   iIndex as an Integer as a constant
      @return  a TSumDocCon
    **)
    Property SumDocCons[Const iIndex : Integer] : TSumDocCon Read GetSumDocCons;
  Public
    Constructor Create(Const strOutputDirectory, strTitle : String); Override;
    Destructor Destroy; Override;
    Procedure OutputDocumentation; Override;
  End;

Implementation

Uses
  SysUtils,
  Windows,
  Graphics,
  {$IFNDEF D2007} GIFImage {$ELSE} GIFImg {$ENDIF},
  Controls,
  StrUtils,
  BADI.Generic.Tokenizer,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.Functions;

(**

  This method output anchor tags into the HTML information.

  @precon  None.
  @postcon Output anchor tags into the HTML information.

  @param   strText as a String as a constant
  @param   strHREF as a String as a constant
  @param   strName as a String as a constant
  @return  a String

**)
Function THTMLDocumentation.A(const strText, strHREF : String;
  const strName : String = '') : String;

Var
  strHREFText: String;
  strNameText: String;

Begin
  strHREFText := StringReplace(strHREF, #32, '', [rfReplaceAll]);
  strNameText := StringReplace(strName, #32, '', [rfReplaceAll]);
  If strHREFText <> '' Then
    Result := Format('<a href="%s">%s</a>', [strHREFText, strText])
  Else
    Result := Format('<a name="%s"></a>%s', [strNameText, strText]);
End;

(**


  This is a constructor for the THTMLDocumentation class.


  @precon  None.

  @postcon Initialises the Special Tag string lists.


  @param   strOutputDirectory as a String as a constant
  @param   strTitle           as a String as a constant

**)
constructor THTMLDocumentation.Create(const strOutputDirectory, strTitle: String);

Var
  i : Integer;

begin
  Inherited Create(strOutputDirectory, strTitle);
  FScopesToDocument := TBADIOptions.BADIOptions.ScopesToDocument + [scNone, scGlobal];
  FTitle := strTitle;
  FModuleSpecialTagNodes := TObjectList.Create(true);
  For i := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do //FI:W528
    FModuleSpecialTagNodes.Add(TStringList.Create);
  FSummarySpecialTagNodes := TObjectList.Create(true);
  For i := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do //FI:W528
    FSummarySpecialTagNodes.Add(TStringList.Create);
  FIndex := TStringList.Create;
  FErrors := TStringList.Create;
  FWarnings := TStringList.Create;
  FHints := TStringList.Create;
  FSummaryDocConflicts := TObjectList.Create(True);
  FSections := TStringList.Create;
  FPerfCounters := TStringList.Create;
  InitialiseSummary;
end;

(**


  This is a destructor for the THTMLDocumentation class.

  @precon  None.
  @postcon Frees the Special Tags string lists.


**)
destructor THTMLDocumentation.Destroy;
begin
  FPerfCounters.Free;
  FSections.Free;
  FSummaryDocConflicts.Free;
  FHints.Free;
  FWarnings.Free;
  FErrors.Free;
  FIndex.Free;
  FSummaryContent.Free;
  FSummarySpecialTagNodes.Free;
  FModuleSpecialTagNodes.Free;
  Inherited Destroy;
end;

(**

  This method expands a link tag into a HTML anchor tag. The format of the link
  is &#123;@link HREF LABEL}. Label may be missed out and the HREF used
  instead as a label.

  <P>The gramer for the HREF value is as follows:<BR>
  HREF ::= [ module '#' ] symbol [ '.' subsymbol ]

  <P>If no label is supplied then the label is derived from the HREF.

  @precon  strToken is the link tag to be expanded.
  @postcon Returns an expanded HTML anchor tag.

  @param   strToken as a String as a constant
  @return  a String

**)
Function THTMLDocumentation.ExpandLinkTag(const strToken : String) : String;

  (**

    This function inspects the HREF value and extras the module, symbol and
    subsymbol reference and formats the link accordingly.

    @precon  strHREF is the hyper text reference for the link as detailed in
             {@link TDocHTMLOutput.ExpandLinkTag ExpandLinkTag} and strLabel is
             the label for the link. If null a label is derived from the HREF.
    @postcon Returns a formatted HTML hypertext reference.

    @param   strHREF  as a String as a constant
    @param   strLabel as a String as a constant
    @return  a String

  **)
  Function FormatTag(const strHREF, strLabel : String) : String;

  Var
    strModule : String;
    strSymbol : String;
    strSubSymbol : String;
    i : Integer;
    strHREFText: String;
    strLabelText: String;

  Begin
    strHREFText := strHREF;
    i := Pos('#', strHREFText);
    If i <> 0 Then
      Begin
        strModule := Copy(strHREFText, 1, i - 1);
        Delete(strHREFText, 1, i);
      End Else
        strModule := ChangeFileExt(ExtractFileName(FCurrentModule.FileName), '.html');
    i := Pos('.', strHREFText);
    If i <> 0 Then
      Begin
        strSymbol := Copy(strHREFText, 1, i - 1);
        strSubSymbol := '.' + Copy(strHREFText, i + 1, Length(strHREFText) - i);
      End Else
      Begin
        strSymbol := strHREFText;
        strSubSymbol := '';
      End;
    strLabelText := strLabel;
    If strLabelText = '' Then
      strLabelText := strSymbol + strSubSymbol;
    Result := Format('<a href="%s.html#%s%s">%s</a>', [strModule, strSymbol,
      strSubSymbol, strLabelText]);
  End;

Var
  i : Integer;
  strHREF : String;
  strLabel : String;
  strTokenText : String;

Begin
  Result := strToken;
  If LowerCase(Copy(strToken, 1, 7)) <> '{@link ' Then
    Exit;
  strTokenText := strToken;
  Delete(strTokenText, 1, 7);
  Delete(strTokenText, Length(strTokenText), 1);
  i := Pos(#32, strTokenText);
  If i <> 0 Then
    Begin
      strHREF := Copy(strTokenText, 1, i - 1);
      strLabel := N(Copy(strTokenText, i + 1, Length(strTokenText) - i));
    End Else
    Begin
      strHREF := strTokenText;
      If (Length(strTokenText) > 0) And (strTokenText[1] = '#') Then
        Delete(strTokenText, 1, 1);
      strLabel := '';
    End;
  Result := FormatTag(strHREF, strLabel);
End;

(**


  This method generates the finalisation information for the summary and outputs
  the file.

  @precon  None.
  @postcon Generates the finalisation information for the summary and outputs
           the file.

**)
procedure THTMLDocumentation.GenerateSummary;

var
  strHTMLName: String;
  iIns: Integer;
  i: Integer;
  slSummary : TStringList;
  strIndent: String;
  slSections : TStringList;

begin
  slSummary := TStringList.Create;
  Try
    If doShowPrefCountersInDocSummary In TBADIOptions.BADIOptions.Options Then
      Begin
        iIns := FindInsertionPoint(FSummaryContent, '*$PERFCOUNTERS$');
        For i := 0 To FPerfCounters.Count - 1 Do
          FPerfCounters[i] := '              <th>' + FPerfCounters[i] + '</th>';
        FSummaryContent[iIns] := FPerfCounters.Text;
      End;
    GenerateHTML(slSummary, 'Summary');
    GenerateModuleList(slSummary);
    iIns := FindInsertionPoint(slSummary, '*$CONTENT$');
    i := Pos('$', slSummary[iIns]);
    strIndent := StringOfChar(#32, i - 1);
    FSummaryContent.Add('    </tbody>');
    FSummaryContent.Add('  </table>');
    FSummaryContent.Add('</div>');
    OutputErrorsWarningsAndHints(FErrors, strErrors, 'Error', iiErrorFolder);
    OutputErrorsWarningsAndHints(FWarnings, strWarnings, 'Warning', iiWarningFolder);
    OutputErrorsWarningsAndHints(FHints, strHints, 'Hint', iiHintFolder);
    OutputSummaryDocumentationConflicts;
    OutputSummarySpecialTags;
    For i := 0 To FSummaryContent.Count - 1 Do
      FSummaryContent[i] := strIndent + FSummaryContent[i];
    slSummary[iIns] := FSummaryContent.Text;
    iIns := FindInsertionPoint(slSummary, '*$SECTIONLIST$');
    i := Pos('$', slSummary[iIns]);
    strIndent := StringOfChar(#32, i - 1);
    slSections := TStringList.Create;
    Try
      For i := 0 To FSections.Count - 1 Do
        slSections.Add(Format('<div class="Section">%s&nbsp;%s</div>', [
          IMG(TBADIImageIndex(FSections.Objects[i]), scNone),
          A(FSections[i], '#' + FSections[i])]));
      For i := 0 To slSections.Count - 1 Do
        slSections[i] := strIndent + slSections[i];
      slSummary[iIns] := slSections.text;
    Finally
      slSections.Free;
    End;
    strHTMLName := FOutputDirectory + 'Summary.html';
    slSummary.SaveToFile(strHTMLName);
  Finally
    slSummary.Free;
  End;
end;

(**

  This method attempts to find the insert point in the doucmentation template.

  @precon  slHTML must be a valid TStringList instance.
  @postcon Return the index of the insert point line if found else returns -1.

  @param   slHTMLFile as a TStringList as a constant
  @param   strText    as a String as a constant
  @return  an Integer

**)
Function THTMLDocumentation.FindInsertionPoint(Const slHTMLFile : TStringList;
  Const strText : String) : Integer;

ResourceString
  strInsertionPointNotFound = 'Insertion Point "%s" not found!';

Var
  i : Integer;

Begin
  For i := 0 To slHTMLFile.Count - 1 Do
    If Like(strText, slHTMLFile[i]) Then
      Begin
        Result := i;
        Exit;
      End;
  Raise Exception.CreateFmt(strInsertionPointNotFound, [strText]);
End;

(**

  This method outputs the contents of the html to a string list which is then inserted into the
  template HTML file.

  @precon  slContents must be a valid instance of a string list.
  @postcon Outputs the contents of the html to a string list which is then inserted into the
           template HTML file.

  @param   slHTMLFile as a TStringList as a constant

**)
Procedure THTMLDocumentation.GenerateContent(Const slHTMLFile : TStringList);

Const
  strSections : Array[1..4] Of String = (strDocumentationConflicts, strErrors,
    strHints, strWarnings);

Var
  iIns : Integer;
  i, j : Integer;
  strIndent : String;
  slContents : TStringList;
  boolIgnore: Boolean;

Begin
  iIns := FindInsertionPoint(slHTMLFile, '*$CONTENT$');
  i := Pos('$', slHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  slContents := TStringList.Create;
  Try
    slContents.Add(Format('<!-- %s -->', [FCurrentModule.AsString(True, True)]));
    slContents.Add(H(A('Documentation for ' +
      FCurrentModule.AsString(True, True), '',
      'ModuleOverview'), FHeaderLevel, FCurrentModule.ImageIndex,
      FCurrentModule.Scope));
    Inc(FHeaderLevel);
    OutputComment(slContents, FCurrentModule, 0);
    slContents.Add('');
    GenerateErrorsWarningsHints(slContents);
    GenerateDocumentConflicts(slContents);
    GenerateSpecialTags(slContents);
    For i := 1 To FCurrentModule.ElementCount Do
      Begin
        boolIgnore := False;
        For j := Low(strSections) To High(strSections) Do
          If CompareText(FCurrentModule[i].AsString(True, True), strSections[j]) = 0 Then
            Begin
              boolIgnore := True;
              Break;
            End;
        If Not boolIgnore Then
          OutputContainers(slContents, FCurrentModule[i], 0,
            FCurrentModule[i].Identifier);
      End;
    For i := 0 To slContents.Count - 1 Do
      slContents[i] := strIndent + slContents[i];
    slHTMLFile[iIns] := slContents.Text;
  Finally
    slContents.Free;
  End;
End;

(**


  This method outputs the CSS for the HTML files from a resource.

  @precon  None.
  @postcon Outputs the CSS for the HTML files from a resource.


**)
Procedure THTMLDocumentation.GenerateCSS;

Var
  sl : TStringList;

  (**


    This method returns a HTML hexidecimal colour from the given TColor.

    @precon  None.
    @postcon Returns a HTML hexidecimal colour from the given TColor.


    @param   AColour as a TColor
    @return  a String

  **)
  Function HTMLColour(AColour : TColor) : String;

  Begin
    Result := Format('%p', [Pointer(ColorToRGB(AColour))]);
    Result := Copy(Result, 7, 2) + Copy(Result, 5, 2) + Copy(Result, 3, 2);
  End;

  (**


    This procedure outputs the code styles to the CSS file.

    @precon  None.
    @postcon Outputs the code styles to the CSS file.


  **)
  Procedure OutputCodeStyles;
  Var
    i : TBADITokenType;

  Begin
    With TBADIOptions.BADIOptions Do
      For i := Low(TBADITokenType) to High(TBADITokenType) Do
        Begin
          sl.Add(Format('span.%s {', [StringReplace(strTokenType[i], #32, '', [rfReplaceAll])]));
          sl.Add(Format('  color            : #%s;', [HTMLColour(TokenFontInfo[i].FForeColour)]));
          If (TokenFontInfo[i].FBackColour <> clNone) And
             (TokenFontInfo[i].FBackColour <> clWindow) Then
            sl.Add(Format('  background       : #%s;', [HTMLColour(TokenFontInfo[i].FBackColour)]));
          If fsBold In TokenFontInfo[i].FStyles Then
            sl.Add('  font-weight      : bold;');
          If fsItalic In TokenFontInfo[i].FStyles Then
            sl.Add('  font-style       : italic;');
          If fsUnderline In TokenFontInfo[i].FStyles Then
            sl.Add('  font-decoration  : underline;');
          If fsStrikeout In TokenFontInfo[i].FStyles Then
            sl.Add('  font-decoration  : line-through;');
          sl.Add('}');
          sl.Add('');
        End;
  End;

Type
  TCSSInfo = Record
    FResource : String;
    FFileName : String;
  End;

Const
  CSSFiles : Array[1..2] Of TCSSInfo = (
    (FResource : 'BrowseAndDocItCSSScreen'; FFileName: 'BrowseAndDocItScreen.CSS'),
    (FResource : 'BrowseAndDocItCSSPrint';  FFileName: 'BrowseAndDocItPrint.CSS')
  );

Var
  Buffer : Array[0..MAX_PATH] Of Char;
  strFileName : String;
  i : Integer;

Begin
  Update(FProgressIndex, 'Generating CSS...');
  Inc(FProgressIndex);
  GetModuleFileName(hInstance, Buffer, MAX_PATH);
  strFileName := StrPas(Buffer);
  strFileName := ExtractFilePath(strFileName) + 'Styles\';
  ForceDirectories(strFileName);
  ForceDirectories(FOutputDirectory + '\Styles');
  sl := TStringList.Create;
  Try
    For i := Low(CSSFiles) to High(CSSFiles) Do
      Begin
        If Not FileExists(strFileName + CSSFiles[i].FFileName) Then
          Begin
            sl.Text := GetStringResource(CSSFiles[i].FResource);
            sl.SaveToFile(strFileName + CSSFiles[i].FFileName);
          End Else
            sl.LoadFromFile(strFileName + CSSFiles[i].FFileName);
        sl.Text := StringReplace(sl.Text, '$PREBGCOLOUR$', HTMLColour(
          TBADIOptions.BADIOptions.BGColour), []);
        OutputCodeStyles;
        sl.SaveToFile(FOutputDirectory + 'Styles\' + CSSFiles[i].FFileName);
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method outputs the document conflicts to the html file.

  @precon  slContents must be a valid instance of a string list.
  @postcon Outputs the document conflicts to the html file as an unordered list.

  @param   slContents as a TStringList as a constant

**)
procedure THTMLDocumentation.GenerateDocumentConflicts(Const slContents : TStringList);

Var
  i, j : Integer;
  E : TElementContainer;
  SDC: TSumDocCon;

begin
  E := FCurrentModule.FindElement(strDocumentationConflicts);
  If E = Nil Then
    Exit;
  slContents.Add(H(A(strDocumentationConflicts, '', strDocumentationConflicts),
    FHeaderLevel, E.ImageIndex, E.Scope));
  SDC := TSumDocCon.Create(ExtractFileName(FCurrentModule.FileName));
  FSummaryDocConflicts.Add(SDC);
  For i := 1 To E.ElementCount Do
    Begin
      slContents.Add(Format('<!-- %s -->', [strDocumentationConflicts]));
      slContents.Add('<div class="Indent">');
      slContents.Add(#32#32 + H(E[i].AsString(True, True), FHeaderLevel + 1, E[i].ImageIndex,
        E[i].Scope));
      slContents.Add('  <ul>');
      For j := 1 To E[i].ElementCount Do
        Begin
          slContents.Add('    ' + LI(BADIImageList[E[i][j].ImageIndex].FResourcename,
            N(E[i][j].AsString(True, True))));
          SDC.Conflicts.Add(Format('%s=%s', [E[i].AsString(True, True),
            E[i][j].AsString(True, True)]));
        End;
      slContents.Add('  </ul>');
      slContents.Add('</div>');
      slContents.Add('');
    End;
end;

(**

  This method output Errors, Hints and Warnings to the html file.

  @precon  slContents must be a valid instance of a string list.
  @postcon Output Errors, Hints and Warnings to the html file as an unordered list.

  @param   slContents as a TStringList as a constant

**)
procedure THTMLDocumentation.GenerateErrorsWarningsHints(Const slContents : TStringList);

Const
  Sections : Array[1..3] Of String = (strErrors, strWarnings, strHints);

Var
  i, j : Integer;
  E : TElementContainer;
  sls : Array[1..3] Of TStringList;

begin
  sls[1] := FErrors;
  sls[2] := FWarnings;
  sls[3] := FHints;
  For i := Low(Sections) to High(Sections) Do
    Begin
      E := FCurrentModule.FindElement(Sections[i]);
      If E = Nil Then
        Continue;
      slContents.Add(Format('<!-- %s -->', [Sections[i]]));
      slContents.Add(H(A(Sections[i], '', Sections[i]), FHeaderLevel,
        E.ImageIndex, E.Scope));
      slContents.Add('<ul>');
      For j := 1 To E.ElementCount Do
        Begin
          slContents.Add(#32#32 + LI(BADIImageList[E[j].ImageIndex].FResourcename,
          N(E[j].AsString(True, True))));
          sls[i].Add(Format('%s=%s', [ExtractFileName(FCurrentModule.FileName),
            E[j].AsString(True, True)]));
        End;
      slContents.Add('</ul>');
      slContents.Add('');
    End;
end;

(**

  This method gets the HTML template from the windows resource and sets its title.

  @precon  None.
  @postcon Gets the HTML template from the windows resource and sets its title.

  @param   slHTMLFile as a TStringList as a constant
  @param   strTitle   as a String as a constant

**)
Procedure THTMLDocumentation.GenerateHTML(Const slHTMLFile : TStringList; Const strTitle : String);

Begin
 slHTMLFile.Text := StringReplace(GetStringResource(
   'BrowseAndDocItHTMLTemplate'), '$TITLE$', strTitle, [rfReplaceAll]);
End;

(**


  This method outputs the module explorer images to jpeg files in the given
  directory.


  @precon  None.

  @postcon Outputs the module explorer images to jpeg files in the given

           directory.


  @param   strImageDirectory as a String as a constant

**)
Procedure THTMLDocumentation.GenerateImages(const strImageDirectory : String);

Var
  R: TRect;
  MainImage: TBitmap;
  ScopeImage: TBitmap;
  eImage: TBADIImageIndex;
  eScope: TScope;
  x, y : Integer;
  GIF: TGIFImage;
  strFileName: string;

Begin
  ForceDirectories(strImageDirectory);
  R := Rect(0, 0, 11, 11);
  MainImage := Graphics.TBitMap.Create;
  Try
    ScopeImage := Graphics.TBitmap.Create;
    Try
      For eImage := Succ(Low(TBADIImageIndex)) To High(TBADIImageIndex) Do
        For eScope := Low(TScope) To High(TScope) Do
          Begin
            MainImage.LoadFromResourceName(hInstance, BADIImageList[eImage].FResourceName);
            ScopeImage.LoadFromResourceName(hInstance, BADIScopeList[eScope].FResourceName);
            For x := 0 To 11 Do
              For y := 0 To 11 Do
                If ScopeImage.Canvas.Pixels[x, y] <> BADIScopeList[eScope].FMaskColour Then
                  MainImage.Canvas.Pixels[x, y] := ScopeImage.Canvas.Pixels[x, y];
            Update(FProgressIndex, Format('Generating Image %s...',
              [ImageFileName(eImage, eScope)]));
            Inc(FProgressIndex);
            MainImage.Transparent := True;
            MainImage.TransparentColor := BADIImageList[eImage].FMaskColour;
            MainImage.TransparentMode := tmFixed;
            GIF := TGIFImage.Create;
            Try
              GIF.Transparent := True;
              GIF.Assign(MainImage);
              strFileName := Format('%s%s.gif', [
                strImageDirectory,
                ImageFileName(eImage, eScope)
              ]);
              If Not FileExists(strFileName) Then
                GIF.SaveToFile(strFileName);
            Finally
              GIF.Free;
            End;
          End;
    Finally
      ScopeImage.Free;
    End;
  Finally
    MainImage.Free;
  End;
  {
  }
End;

(**


  This method generates an index from all the identifiers stored in the FIndex
  string list.

  @precon  None.
  @postcon Generates an index from all the identifiers stored in the FIndex
           string list.


**)
procedure THTMLDocumentation.GenerateIndex;

var
  sl, slC, slSections: TStringList;
  iIns: Integer;
  i: Integer;
  strIndent: String;
  strID: String;
  strRef: String;
  strCurAlpha, strLastAlpha : String;

begin
  Update(FProgressIndex, 'Generating Index...');
  Inc(FProgressIndex);
  sl := TStringList.Create;
  Try
    GenerateHTML(sl, 'Index');
    GenerateModuleList(sl);
    iIns := FindInsertionPoint(sl, '*$CONTENT$');
    i := Pos('$', sl[iIns]);
    strIndent := StringOfChar(#32, i - 1);
    slSections := TStringList.Create;
    Try
      slC := TstringList.Create;
      Try
        slC.Add(H('Index for ' + FTitle, 1, iiNone, scNone));
        FIndex.Sort;
        For i := 0 To FIndex.Count - 1 Do
          Begin
            strCurAlpha := UpperCase(FIndex[i][1]);
            If (strLastAlpha <> '') And (CompareText(strCurAlpha, strLastAlpha) <> 0) Then
              Begin
                slC.Add('  </table>');
                slC.Add('</div>');
              End;
            If CompareText(strCurAlpha, strLastAlpha) <> 0 Then
              Begin
                slC.Add('<hr/>');
                slC.Add(H(A(strCurAlpha, '', strCurAlpha), 2, iiNone, scNone));
                slSections.Add(strCurAlpha);
                slC.Add('<div class="Indent">');
                slC.Add('  <table>');
              End;
            strID := FIndex.Names[i];
            strRef := StringReplace(ChangeFileExt(ExtractFileName(
              FIndex.ValueFromIndex[i]), ''), 'html#', '', [rfReplaceAll]);
            slC.Add(Format('    <tr><td>%s</td><td>%s</td></tr>', [A(strID,
              ExtractFileName(FIndex.ValueFromIndex[i]), ''), strRef]));
            strLastAlpha := strCurAlpha;
          End;
        slC.Add('  </table>');
        slC.Add('</div>');
        For i := 0 To slC.Count - 1 Do
          slC[i] := strIndent + slC[i];
        sl[iIns] := slC.Text;
      Finally
        slC.Free;
      End;
      // Section list of Alphabet Hyperlinks
      iIns := FindInsertionPoint(sl, '*$SECTIONLIST$');
      i := Pos('$', sl[iIns]);
      strIndent := StringOfChar(#32, i - 1);
      For i := 0 To slSections.Count - 1 Do
        slSections[i] := strIndent + Format('<div class="Section">%s</div>', [
          H(A(UpperCase(slSections[i]), '#' + UpperCase(slSections[i]), ''), 2,
          iiNone, scNone)]);
      sl[iIns] := slSections.Text;
    Finally
      slSections.Free;
    End;
    sl.SaveToFile(FOutputDirectory + 'Index.html');
  Finally
    sl.Free;
  End;

end;

(**

  This method outputs a menu of all the module in this package of documentation.

  @precon  None.
  @postcon Outputs a menu of all the module in this package of documentation.

  @param   slHTMLFile as a TStringList as a constant

**)
Procedure THTMLDocumentation.GenerateModuleList(Const slHTMLFile : TStringList);

Var
  iIns : Integer;
  sl : TStringList;
  strIndent : String;
  i : Integer;
  strFileName : String;

Begin
  iIns := FindInsertionPoint(slHTMLFile, '*$MODULELIST$');
  i := Pos('$', slHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  sl := TStringList.Create;
  Try
    sl.Add(strIndent + '<div class="List">');
    sl.Add(strIndent + '  <div class="ModuleTitle">Modules</div>');
    sl.Add(strIndent + Format('  <div class="Module">%s&nbsp;%s</div>',
      [IMG(iiModule, scNone), A('Summary', 'Summary.html')]));
    For i := 0 To FFileNames.Count - 1 Do
      Begin
        strFileName := ExtractFileName(FFileNames[i]);
        sl.Add(strIndent + Format('  <div class="Module">%s&nbsp;%s</div>',
          [IMG(iiModule, scNone), A(ExtractFileName(strFileName),
          ChangeFileExt(strFileName, '.html'))]));
      End;
    sl.Add(strIndent + Format('  <div class="Module">%s&nbsp;%s</div>',
      [IMG(iiModule, scNone), A('Index', 'Index.html')]));
    sl.Add(strIndent + '</div>');
    slHTMLFile[iIns] := sl.Text;
  Finally
    sl.Free;
  End;
End;

(**

  This method generates a menu for the various section contained in this module.

  @precon  None.
  @postcon Generates a menu for the various section contained in this module.

  @param   slHTMLFile as a TStringList as a constant

**)
Procedure THTMLDocumentation.GenerateSectionList(Const slHTMLFile : TStringList);

Var
  iIns : Integer;
  sl : TStringList;
  strIndent : String;
  i : Integer;
  E : TElementContainer;
  strName: String;
  strIdent: String;

Begin
  iIns := FindInsertionPoint(slHTMLFile, '*$SECTIONLIST$');
  i := Pos('$', slHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  sl := TStringList.Create;
  Try
    sl.Add(strIndent + Format('<div class="Section">%s&nbsp;%s</div>', [
      IMG(iiModule, scNone), A('Module Overview', '#ModuleOverview')]));
    For i := 0 To FModuleSpecialTagNodes.Count - 1 Do
      If (FModuleSpecialTagNodes[i] As TStringList).Count > 0 Then
        Begin
          strName := TBADIOptions.BADIOptions.SpecialTags[i].FDescription;
          strIdent := TBADIOptions.BADIOptions.SpecialTags[i].FName;
          sl.Add(strIndent + Format('<div class="Section">%s&nbsp;%s</div>', [
            IMG(iiToDoFolder, scNone), A(strName, '#' + strIdent)]));
        End;
    For i := 1 To FCurrentModule.ElementCount Do
      Begin
        E := FCurrentModule.Elements[i];
        sl.Add(strIndent + Format('<div class="Section">%s&nbsp;%s</div>', [
          IMG(E.ImageIndex, E.Scope), A(E.AsString(True, True),
          '#' + E.AsString(True, True) + '2')]));
      End;
    slHTMLFile[iIns] := sl.Text;
  Finally
    sl.Free;
  End;
End;

(**

  This method outputs special tags (todos, bugs, etc) to the html file.

  @precon  slContents must be a valid instance of a string list.
  @postcon Outputs special tags (todos, bugs, etc) to the html file as an unordered list.

  @param   slContents as a TStringList as a constant

**)
procedure THTMLDocumentation.GenerateSpecialTags(Const slContents: TStringList);

Var
  i: Integer;
  j: Integer;
  k: Integer;
  sl : TStringList;
  strSection: String;

begin
  For i := 0 To FModuleSpecialTagNodes.Count - 1 Do
    (FModuleSpecialTagNodes[i] As TStringList).Clear;
  For i := 0 To FCurrentModule.BodyCommentCount - 1 Do
    With FCurrentModule.BodyComment[i] Do
      For j := 0 To TagCount - 1 Do
        For k := 0 To FModuleSpecialTagNodes.Count - 1 Do
          If tpShowInDoc In TBADIOptions.BADIOptions.SpecialTags[k].FTagProperties Then
            If CompareText(Tag[j].TagName, TBADIOptions.BADIOptions.SpecialTags[k].FName) = 0 Then
              Begin
                (FSummarySpecialTagNodes[k] As TStringList).Add(
                  Format('%s=%s', [ExtractFileName(FCurrentModule.FileName),
                  Tag[j].AsString(MaxInt, False)]));
                (FModuleSpecialTagNodes[k] As TStringList).Add(
                  Tag[j].AsString(MaxInt, False));
              End;
  For i := 0 To FModuleSpecialTagNodes.Count - 1 Do
    Begin
      sl := FModuleSpecialTagNodes[i] As TStringList;
      If sl.Count = 0 Then
        Continue;
      strSection := TBADIOptions.BADIOptions.SpecialTags[i].FDescription;
      slContents.Add(Format('<!-- %s -->', [strSection]));
      slContents.Add(H(A(strSection, '', TBADIOptions.BADIOptions.SpecialTags[i].FName),
        FHeaderLevel, iiToDoFolder, scNone));
      slContents.Add('<ul>');
      For j := 0 To sl.Count - 1 Do
        If tpFixed In TBADIOptions.BADIOptions.SpecialTags[i].FTagProperties Then
          slContents.Add(#32#32 + LI('ToDoItem', '<pre>' + N(sl[j]) + '</pre>'))
        Else
          slContents.Add(#32#32 + LI('ToDoItem', N(sl[j])));
      slContents.Add('</ul>');
      slContents.Add('');
    End;
  For j := 0 To FModuleSpecialTagNodes.Count - 1 Do
    Begin
      If tpShowInDoc In TBADIOptions.BADIOptions.SpecialTags[j].FTagProperties Then
        Begin
          i := (FModuleSpecialTagNodes[j] As TStringList).Count;
          If i = 0 Then
            FSummaryContent.Add(Format('        <td class="Green">%d</td>', [i]))
          Else
            FSummaryContent.Add(Format('        <td class="Yellow">%d</td>', [i]));
        End;
    End;
  If doShowPrefCountersInDocSummary In TBADIOptions.BADIOptions.Options Then
    Begin
      For i := 1 To FCurrentModule.OpTickCounts - 1 Do
        FSummaryContent.Add(Format('        <td>%1.1n</td>', [
          FCurrentModule.OpTickCountByIndex[i] -
          FCurrentModule.OpTickCountByIndex[i - 1]]));
      FSummaryContent.Add(Format('        <td>%1.1n</td>', [
        FCurrentModule.OpTickCountByIndex[FCurrentModule.OpTickCounts - 1] -
        FCurrentModule.OpTickCountByIndex[0]]));
    End;
  FSummaryContent.Add('      </tr>');
end;

(**


  This method generates the schema files for the xml checking of the xhtml
  output.

  @precon  None.
  @postcon Generates the schema files for the xml checking of the xhtml output.


**)
Procedure THTMLDocumentation.GenerateSchema;
Var
  sl : TStringList;

Begin
  Update(FProgressIndex, 'Generating Schema...');
  Inc(FProgressIndex);
  ForceDirectories(FOutputDirectory + '\Schema');
  sl := TStringList.Create;
  Try
    If Not FileExists(FOutputDirectory + 'Schema\xhtml1-strict.dtd') Then
      Begin
        sl.Text := GetStringResource('BrowseAndDocItXHTMLStrict');
        sl.SaveToFile(FOutputDirectory + 'Schema\xhtml1-strict.dtd');
      End;
    If Not FileExists(FOutputDirectory + 'Schema\xhtml-symbol.ent') Then
      Begin
        sl.Text := GetStringResource('BrowseAndDocItXHTMLSymbol');
        sl.SaveToFile(FOutputDirectory + 'Schema\xhtml-symbol.ent');
      End;
    If Not FileExists(FOutputDirectory + 'Schema\xhtml-special.ent') Then
      Begin
        sl.Text := GetStringResource('BrowseAndDocItXHTMLSpecial');
        sl.SaveToFile(FOutputDirectory + 'Schema\xhtml-special.ent');
      End;
    If Not FileExists(FOutputDirectory + 'Schema\xhtml-lat1.ent') Then
      Begin
        sl.Text := GetStringResource('BrowseAndDocItXHTMLLat1');
        sl.SaveToFile(FOutputDirectory + 'Schema\xhtml-lat1.ent');
      End;
  Finally
    sl.Free;
  End;
End;

(**


  This is a getter method for the property MainDocument.

  @precon  None.
  @postcon This returns the first HTML document generates as the main document
           to be opened.


  @return  a String

**)
function THTMLDocumentation.GetMainDocument: String;
begin
  Result := FOutputDirectory + 'Summary.html';
end;

(**


  This method returns a string containing the resource information in the given
  windows resource.

  @precon  None.
  @postcon Returns a string containing the resource information in the given
           windows resource.


  @param   strName as a String as a constant
  @return  a String

**)
function THTMLDocumentation.GetStringResource(const strName: String): String;

Var
  Res: TResourceStream;
  {$IFDEF D2009}
  strRes : AnsiString;
  {$ENDIF}

begin
  Res := TResourceStream.Create(HInstance, strName, RT_RCDATA);
  Try
    If Res.Size = 0 Then
      Raise Exception.CreateFmt(strName, [strName]);
    {$IFNDEF D2009}
    SetLength(Result, Res.Size);
    Res.ReadBuffer(Result[1], Res.Size);
    {$ELSE}
    SetLength(strRes, Res.Size);
    Res.ReadBuffer(strRes[1], Res.Size);
    Result := String(strRes);
    {$ENDIF}
  Finally
    Res.Free;
  End;
end;

(**

  This is a getter method for the SumDocCons property.

  @precon  iIndex must be a valid integer between 0 and Count - 1.
  @postcon Returns the instance of the TSumDocCons class.

  @param   iIndex as an Integer as a constant
  @return  a TSumDocCon

**)
function THTMLDocumentation.GetSumDocCons(Const iIndex : Integer): TSumDocCon;
begin
  Result := FSummaryDocConflicts[iIndex]  As TSumDocCon;
end;

(**

  This method returns a string representing a header tag.

  @precon  None.
  @postcon Returns a string representing a header tag.

  @param   strText as a String as a constant
  @param   iLevel  as an Integer as a constant
  @param   AImage  as a TBADIImageIndex as a constant
  @param   AScope  as a TScope as a constant
  @return  a String

**)
function THTMLDocumentation.H(Const strText: String; Const iLevel: Integer;
  Const AImage : TBADIImageIndex; Const AScope : TScope): String;

begin
  Result := Format('<h%d>%s&nbsp;%s</h%d>', [iLevel, IMG(AImage, AScope),
   strText, iLevel]);
end;

(**

  This method returns a file name composed of the scope plus the image name.

  @precon  None.
  @postcon Returns a file name composed of the scope plus the image name.

  @param   eImageIndex as a TBADIImageIndex
  @param   eScope      as a TScope
  @return  a String

**)
Function THTMLDocumentation.ImageFileName(eImageIndex: TBADIImageIndex; eScope: TScope): String;

Begin
  Result :=
    StringReplace(BADIScopeList[eScope].FResourcename, 'Mask', '', [rfReplaceAll]) +
    StringReplace(BADIImageList[eImageIndex].FResourcename, 'Public', '', [rfReplaceAll])End;

(**

  This method outputs an img tag with the image vertically aligned central.

  @precon  None.
  @postcon Outputs an img tag with the image vertically aligned central.

  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AScope      as a TScope as a constant
  @return  a String

**)
function THTMLDocumentation.IMG(Const AImageIndex: TBADIImageIndex; Const AScope : TScope): String;

begin
  Result := Format('<img class="verticallymiddle" src="Images/%s.gif" alt=""/>', [
    ImageFileName(AImageIndex, AScope)
  ]);
end;

(**


  This method returns a list item tag as a string.


  @precon  None.

  @postcon Returns a list item tag as a string.


  @param   strClass as a String as a constant
  @param   strText  as a String as a constant
  @return  a String

**)
function THTMLDocumentation.LI(const strClass, strText: String): String;
begin
  Result := Format('<li class="%s">%s</li>', [strClass, strText]);
end;

(**

  This method outputs the comment text along with the tag information.

  @precon  None.
  @postcon Outputs the comment text along with the tag information.

  @param   slContents   as a TStringList as a constant
  @param   E            as a TElementContainer as a constant
  @param   iIndentLevel as an Integer as a constant

**)
procedure THTMLDocumentation.OutputComment(Const slContents : TStringList;
  Const E: TElementContainer; Const iIndentLevel : Integer);

var
  i: Integer;
  strComment: String;
  k: Integer;
  strTags: String;
  strIndent: String;
  iTagIndex: Integer;
  strMask: String;

begin
  If (E = Nil) Or (E.Comment = Nil) Then
    Exit;
  For i := 0 To E.Comment.TokenCount - 1 Do
    If E.Comment.Tokens[i].TokenType = ttLinkTag Then
      strComment := strComment + Format('%s ', [ExpandLinkTag(E.Comment.Tokens[i].Token)])
    Else
      strComment := strComment + N(Format('%s ', [E.Comment.Tokens[i].Token]));
  strIndent := StringOfChar(#32, 6 * iIndentLevel);
  slContents.Add(Format('%s  <p class="Comment">%s</p>', [strIndent, strComment]));
  If E.Comment.TagCount = 0 Then
    Exit;
  With TBADIOptions.BADIOptions Do
    For i := 0 To SpecialTags.Count - 1 Do
      Begin
        iTagIndex := E.Comment.FindTag(SpecialTags[i].FName);
        If iTagIndex > -1 Then
          Begin
            slContents.Add(Format('%s  <p class="TagHeader">%s</p>', [
              strIndent, SpecialTags[i].FDescription]));
            slContents.Add(Format('%s  <ul>', [strIndent]));
            strTags := '';
            If E.Comment.Tag[iTagIndex].Fixed Then
              strMask := '%s'
            Else
              strMask := '%s ';
            For k := 0 To E.Comment.Tag[iTagIndex].TokenCount - 1 Do
              If E.Comment.Tag[iTagIndex].Tokens[k].TokenType = ttLinkTag Then
                strTags := strTags + Format(strMask, [ExpandLinkTag(E.Comment.Tag[iTagIndex].Tokens[k].Token)])
              Else
                strTags := strTags + Format(strMask, [N(E.Comment.Tag[iTagIndex].Tokens[k].Token)]);
            If E.Comment.Tag[iTagIndex].Fixed Then
              slContents.Add(Format('%s    <div style="margin-top: 0.5em;"><pre class="Tag">%s</pre></div>', [strIndent, LI('SpecialTag', strTags)]))
            Else
              slContents.Add(Format('%s    %s', [strIndent, LI('SpecialTag', strTags)]));
            slContents.Add(Format('%s  </ul>', [strIndent]));
          End;
      End;
end;

(**

  This method output the tree of information stored in the module recursively.

  @precon  slContents must be a valid indtance of a string list and Container must ba a valid
           descendant of TElement Container.
  @postcon Output the tree of information stored in the module recursively.

  @param   slContents        as a TStringList as a constant
  @param   Container         as a TElementContainer as a constant
  @param   iIndentLevel      as an Integer as a constant
  @param   strContainerLabel as a String as a constant

**)
procedure THTMLDocumentation.OutputContainers(Const slContents: TStringList;
  Const Container: TElementContainer; Const iIndentLevel : Integer;
  Const strContainerLabel : String);

var
  i: Integer;
  boolHeader : Boolean;
  boolIncHeader: Boolean;
  strIndent : String;

begin
  strIndent := StringOfChar(#32, 4 * iIndentLevel);
  boolHeader := False;
  boolIncHeader := False;
  slContents.Add(Format('%s<!-- %s -->', [strIndent, N(Container.AsString(True, True))]));
  If Container Is TLabelContainer Then
    Begin
      slContents.Add(strIndent + H(A(Container.AsString(True, True), '',
        Container.AsString(True, True) + IntToStr(FHeaderLevel)), FHeaderLevel,
        Container.ImageIndex, Container.Scope));
      OutputComment(slContents, Container, iIndentLevel);
      Inc(FHeaderLevel);
      boolIncHeader := True;
    End;
  slContents.Add(strIndent + '<div class="Indent">');
  For i := 1 To Container.ElementCount Do
    If Not (Container[i] Is TLabelContainer) Then
      If Container[i].Scope In FScopesToDocument Then
        Begin
          FIndex.Add(Format('%s=%s#%s', [
            Format('%s', [Container[i].Identifier]),
            FHTMLFileName,
            Format('%s.%s', [strContainerLabel, Container[i].Identifier])]));
          If Not boolHeader Then
            Begin
              slContents.Add(strIndent + '  <table>');
              slContents.Add(strIndent + '    <thead>');
              slContents.Add(strIndent + '      <tr>');
              slContents.Add(strIndent + '        <th>Name</th>');
              slContents.Add(strIndent + '        <th>Data / Type</th>');
              slContents.Add(strIndent + '      </tr>');
              slContents.Add(strIndent + '    </thead>');
              slContents.Add(strIndent + '    <tbody>');
              boolHeader := True;
            End;
          slContents.Add(strIndent + '      <tr>');
          slContents.Add(strIndent + Format('        <td>%s&nbsp;%s</td>', [
            IMG(Container[i].ImageIndex, Container[i].Scope),
            A(Container[i].Identifier, '',
            strContainerLabel + '.' + Container[i].Identifier)]));
          slContents.Add(strIndent + '        <td>');
          slContents.Add(strIndent + Format('          <pre class="code">%s</pre>', [
            P(Container[i].AsString(True, True))]));
          OutputComment(slContents, Container[i], iIndentLevel + 1);
          OutputContainers(slContents, Container[i], iIndentLevel + 1,
            strContainerLabel + '.' + Container[i].Identifier);
          slContents.Add(strIndent + '        </td>');
          slContents.Add(strIndent + '      </tr>');
        End;
  If boolHeader Then
    Begin
      slContents.Add(strIndent + '    </tbody>');
      slContents.Add(strIndent + '  </table>');
      slContents.Add('');
    End;
  For i := 1 To Container.ElementCount Do
    If Container[i] Is TLabelContainer Then
      If Container[i].Scope In FScopesToDocument Then
        Begin
          OutputComment(slContents, Container[i], iIndentLevel);
          OutputContainers(slContents, Container[i], iIndentLevel + 1,
            strContainerLabel + '.' + Container[i].Identifier);
        End;
  If boolIncHeader Then
    Dec(FHeaderLevel);
  slContents.Add(strIndent + '</div>');
  slContents.Add('');
end;

(**


  This is an overridden method to generate a HTML document for each module in
  the package.

  @precon  None.
  @postcon Generates a HTML document for each module in the package.


**)
Procedure THTMLDocumentation.OutputDocumentation;

Var
  i : Integer;

Begin
  Initialise(
    5 + Integer(Succ(High(TBADIImageIndex))) * Integer(Succ(High(TScope))) + FFileNames.Count,
    'HTML Documentation',
    'Processing source code, please wait...');
  Try
    GenerateCSS;
    GenerateSchema;
    Update(FProgressIndex, 'Generating Images...');
    Inc(FProgressIndex);
    GenerateImages(FOutputDirectory + 'Images\');
    For i := 0 To FFileNames.Count - 1 Do
      Begin
        Update(FProgressIndex, Format('Processing : %s...', [ExtractFileName(FFileNames[i])]));
        Inc(FProgressIndex);
        OutputHTMLDocumentation(FFileNames[i]);
      End;
    GenerateSummary;
    GenerateIndex;
  Finally
    Finalise;
  End;
End;

(**


  This method outputs the documentation conflicts for all the modules to the
  summary html page.

  @precon  None.
  @postcon Outputs the documentation conflicts for all the modules to the
           summary html page.


**)
procedure THTMLDocumentation.OutputSummaryDocumentationConflicts;

var
  i: Integer;
  j: Integer;
  strDocSection, strLastDocSection : String;

begin
  If FSummaryDocConflicts.Count > 0 Then
      Begin
        FSummaryContent.Add(Format('<!-- %s -->', ['Documentation Conflicts']));
        FSections.AddObject(strDocumentationConflicts,
          TObject(iiDocConflictFolder));
        FSummaryContent.Add(H(A(strDocumentationConflicts, '',
          strDocumentationConflicts), 2, iiDocConflictFolder, scNone));
        FSummaryContent.Add('<div class="Indent">');
        For i := 0 To FSummaryDocConflicts.Count - 1 Do
          Begin
            strDocSection := '';
            strLastDocSection := '';
            FSummaryContent.Add('  ' + H(SumDocCons[i].Module, 3, iiModule, scNone));
            FSummaryContent.Add('  <div class="Indent">');
            For j := 0 To SumDocCons[i].Conflicts.Count - 1 Do
              Begin
                strDocSection := SumDocCons[i].Conflicts.Names[j];
                If strDocSection <> strLastDocSection Then
                  Begin
                    If strLastDocSection <> '' Then
                      FSummaryContent.Add('    </ul>');
                    FSummaryContent.Add('    ' + H(strDocSection, 4,
                      iiDocConflictFolder, scNone));
                    FSummaryContent.Add('    <ul>');
                  End;
                FSummaryContent.Add('      ' + LI('ToDoItem',
                  SumDocCons[i].Conflicts.ValueFromIndex[j]));
                strLastDocSection := strDocSection;
              End;
            FSummaryContent.Add('    </ul>');
            FSummaryContent.Add('  </div>');
          End;
        FSummaryContent.Add('</div>');
        FSummaryContent.Add('');
      End;
end;

(**

  This method outputs the Errors, Warnings and Hints for all modules to the summary html page.

  @precon  slEWH must be a valid instance of a string list.
  @postcon Outputs the Errors, Warnings and Hints for all modules to the summary html page.

  @param   slEWH           as a TStringList as a constant
  @param   strSectionTitle as a String as a constant
  @param   strLIType       as a String as a constant
  @param   AImageIndex     as a TBADIImageIndex as a constant

**)
procedure THTMLDocumentation.OutputErrorsWarningsAndHints(Const slEWH: TStringList;
  Const strSectionTitle, strLIType : String; Const AImageIndex : TBADIImageIndex);

var
  strModuleName: String;
  i: Integer;
  strLastModuleName: String;

begin
  If slEWH.Count = 0 Then
    Exit;
  FSummaryContent.Add(Format('<!-- %s -->', [strSectionTitle]));
  FSections.AddObject(strSectionTitle, TObject(AImageIndex));
  FSummaryContent.Add(H(A(strSectionTitle, '', strSectionTitle), 2, AImageIndex,
    scNone));
  strLastModuleName := '';
  strModuleName := '';
  FSummaryContent.Add('<div class="Indent">');
  For i := 0 To slEWH.Count - 1 Do
    Begin
      strModuleName := slEWH.Names[i];
      If strModuleName <> strLastModuleName Then
        Begin
          If strLastModuleName <> '' Then
            FSummaryContent.Add('  </ul>');
          FSummaryContent.Add('  ' + H(strModuleName, 3, iiModule, scNone));
          FSummaryContent.Add('  <ul>');
        End;
      FSummaryContent.Add('    ' + LI(strLIType, slEWH.ValueFromIndex[i]));
      strLastModuleName := strModuleName;
    End;
  FSummaryContent.Add('  </ul>');
  FSummaryContent.Add('</div>');
  FSummaryContent.Add('');
end;

(**


  This method outputs the Special tags from all the modules to the summary
  html page.

  @precon  None.
  @postcon Outputs the Special tags from all the modules to the summary
           html page.


**)
procedure THTMLDocumentation.OutputSummarySpecialTags;

var
  strLastModuleName: string;
  sl: TStringList;
  strModuleName: string;
  j: Integer;
  i: Integer;

begin
  For i := 0 To FSummarySpecialTagNodes.Count - 1 Do
    If (FSummarySpecialTagNodes[i] as TStringList).Count > 0 Then
      Begin
        FSummaryContent.Add(Format('<!-- %s -->', [
          TBADIOptions.BADIOptions.SpecialTags[i].FDescription]));
        FSections.AddObject(TBADIOptions.BADIOptions.SpecialTags[i].FDescription,
          TObject(iiTodoFolder));
        sl := (FSummarySpecialTagNodes[i] as TStringList);
        FSummaryContent.Add(H(A(TBADIOptions.BADIOptions.SpecialTags[i].FDescription, '',
          TBADIOptions.BADIOptions.SpecialTags[i].FDescription), 2, iiToDoFolder, scNone));
        strLastModuleName := '';
        strModuleName := '';
        FSummaryContent.Add('<div class="Indent">');
        For j := 0 To sl.Count - 1 Do
          Begin
            strModuleName := sl.Names[j];
            If strModuleName <> strLastModuleName Then
              Begin
                If strLastModuleName <> '' Then
                  FSummaryContent.Add('  </ul>');
                FSummaryContent.Add('  ' + H(strModuleName, 3, iiModule, scNone));
                FSummaryContent.Add('  <ul>');
              End;
            FSummaryContent.Add('    ' + LI('ToDoItem', sl.ValueFromIndex[j]));
            strLastModuleName := strModuleName;
          End;
        FSummaryContent.Add('  </ul>');
        FSummaryContent.Add('</div>');
        FSummaryContent.Add('');
      end;
end;

(**


  This method initialise the summary content so that it is ready to accept the
  summary information generate during the processing of the modules.

  @precon  None.
  @postcon Sets up the table header for the summary information.


**)
procedure THTMLDocumentation.InitialiseSummary;

var
  i: Integer;

begin
  FSections.AddObject('Summary', TObject(iiModule));
  FSummaryContent := TStringList.Create;
  FSummaryContent.Add(H(Format('Project %s', [FTitle]), 1, iiNone, scNone));
  FSummaryContent.Add(H(A('Summary', '', 'Summary'), 2, iiModule, scNone));
  FSummaryContent.Add('<div class="Indent">');
  FSummaryContent.Add('  <table>');
  FSummaryContent.Add('    <thead>');
  FSummaryContent.Add('      <tr>');
  FSummaryContent.Add(Format('        <th>%s</th>', ['Module']));
  FSummaryContent.Add(Format('        <th>%s</th>', ['Errors']));
  FSummaryContent.Add(Format('        <th>%s</th>', ['Warnings']));
  FSummaryContent.Add(Format('        <th>%s</th>', ['Hints']));
  FSummaryContent.Add(Format('        <th>%s</th>', ['Document Conflicts']));
  With TBADIOptions.BADIOptions Do
    For i := 0 to SpecialTags.Count - 1 Do
      If tpShowInDoc In SpecialTags[i].FTagProperties Then
        FSummaryContent.Add(Format('        <th>%s</th>', [SpecialTags[i].FDescription]));
  With TBADIOptions.BADIOptions Do
    If doShowPrefCountersInDocSummary In Options Then
      FSummaryContent.Add('$PERFCOUNTERS$');
  FSummaryContent.Add('      </tr>');
  FSummaryContent.Add('    </thead>');
  FSummaryContent.Add('    <tbody>');
end;

(**


  This method creates a HTML document, gets a source module document and
  generates documentation for that source code.

  @precon  None.
  @postcon Creates a HTML document, gets a source module document and
           generates documentation for that source code.


  @param   strFileName as a String as a constant

**)
Procedure THTMLDocumentation.OutputHTMLDocumentation(const strFileName : String);

Const
  strSections : Array[1..4] Of String = (strErrors, strWarnings, strHints,
    strDocumentationConflicts);

Var
  Source : TStringList;
  i: Integer;
  E: TElementContainer;
  j: Integer;
  k: Integer;
  CurrentHTMLFile: TStringList;

Begin
  Source := TStringList.Create;
  Try
    Source.LoadFromFile(strFileName);
    FCurrentModule := TBADIDispatcher.BADIDispatcher.Dispatcher(Source.Text, strFileName, False,
      [moParse, moCheckForDocumentConflicts]);
    If FCurrentModule <> Nil Then
      Try
        If doShowPrefCountersInDocSummary In TBADIOptions.BADIOptions.Options Then
          If FPerfCounters.Count = 0 Then
            Begin
              For i := 1 To FCurrentModule.OpTickCounts - 1 Do
                FPerfCounters.Add(FCurrentModule.OpTickCountName[i]);
              FPerfCounters.Add('Total');
            End;
        FReservedWords := FCurrentModule.ReservedWords;
        FDirectives := FCurrentModule.Directives;
        FSummaryContent.Add('      <tr>');
        FSummaryContent.Add(Format('        <td>%s</td>', [
          A(FCurrentModule.AsString(True, True),
          ChangeFileExt(ExtractFileName(FCurrentModule.FileName), '.html'),
          '')]));
        For j := Low(strSections) to High(strSections) Do
          Begin
            i := 0;
            E := FCurrentModule.FindElement(strSections[j]);
            If E <> Nil Then
              Begin
                If j = High(strSections) Then
                  Begin
                    For k := 1 To E.ElementCount Do
                      Inc(i, E[k].ElementCount);
                  End
                Else
                  Inc(i, E.ElementCount);
              End;
            If i = 0 Then
              FSummaryContent.Add(Format('        <td class="Green">%d</td>', [i]))
            Else
              FSummaryContent.Add(Format('        <td class="Red">%d</td>', [i]));
          End;
        CurrentHTMLFile := TStringList.Create;
        Try
          GenerateHTML(CurrentHTMLFile, ExtractFileName(strFileName));
          GenerateModuleList(CurrentHTMLFile);
          FHeaderLevel := 1;
          FHTMLFileName := FOutputDirectory + ChangeFileExt(ExtractFileName(strFileName), '.html');
          GenerateContent(CurrentHTMLFile);
          GenerateSectionList(CurrentHTMLFile);
          CurrentHTMLFile.SaveToFile(FHTMLFileName);
        Finally
          CurrentHTMLFile.Free;
        End;
    Finally
      FCurrentModule.Free;
    End;
  Finally
    Source.Free;
  End;
End;

(**


  This method parses the code and highlights it with syntax information.

  @precon  None.
  @postcon Parses the code and highlights it with syntax information.


  @param   strText as a String as a constant
  @return  a String

**)
function THTMLDocumentation.P(const strText: String): String;

Var
  sl : TStringList;
  i: Integer;
  strTokenName : String;

begin
  Result := '';
  sl := Tokenize(strText, FReservedWords, FDirectives);
  Try
    For i := 0 To sl.Count - 1 Do
      Begin
        strTokenName := StringReplace(strTokenType[TBADITokenType(sl.Objects[i])], #32, '',
          [rfReplaceAll]);
        Result := Result + Format('<span class="%s">%s</span>', [strTokenName, N(sl[i])]);
      End;
  Finally
    sl.Free;
  End;
end;

(**


  This method removed characters from the passed string which are considered
  invalid in HTML.

  @precon  None.
  @postcon Removed characters from the passed string which are considered
           invalid in HTML.


  @param   strText as a String as a constant
  @return  a String

**)
function THTMLDocumentation.N(const strText : String): String;

Const
  strValidHTMLChars : Set of AnsiChar = ['a'..'z', 'A'..'Z', '0'..'9', #32, '.',
    ',', '!', '"', '£', '$', '%', '^', '*', '(', ')', '_', '+', '-', '=', '{',
    '}', '[', ']', ':', ';', '@', '''', '~', '?', '/', '\', '|', #13, #10, '#'];

Var
  i : Integer;

begin
  Result := strText;
  For i := 1 To Length(strText) Do
    Begin
      If Not (IsInSet(strText[i], strValidHTMLChars)) Then
        Result := StringReplace(Result, strText[i],
          Format('&#%d;', [Ord(strText[i])]), [rfReplaceAll]);
    End;
end;

{ TSumDocCon }

(**


  This is a constructor for the TSumDocCon class.

  @precon  None.
  @postcon Initialises the class.


  @param   strModule as a String as a constant

**)
constructor TSumDocCon.Create(const strModule : String);
begin
  FModule := strModule;
  FConflicts := TStringList.Create;
end;

(**


  This is a destructor for the TSumDocCon class.

  @precon  None.
  @postcon Frees the memory used by the class.


**)
destructor TSumDocCon.Destroy;
begin
  FConflicts.Free;
  Inherited Destroy;
end;

End.
