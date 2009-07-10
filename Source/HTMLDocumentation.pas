(**

  This module contains a documentation engine for producing static HTML
  information.

  @Author  David Hoyle
  @Date    10 Jul 2009
  @Version 1.0

**)
Unit HTMLDocumentation;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  Classes, BaseLanguageModule, BaseDocumentation, Contnrs;

Type
  (** This class represent a set of documenation conflicts for a module. **)
  TSumDocCon = Class
  Private
    FModule    : String;
    FConflicts : TStringList;
  Protected
  Public
    Constructor Create(strModule : String);
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
  Private
    FCurrentModule   : TBaseLanguageModule;
    FProgressIndex: Integer;
    FModuleSpecialTagNodes: TObjectList;
    FSummarySpecialTagNodes: TObjectList;
    FErrors: TStringList;
    FWarnings: TStringList;
    FHints: TStringList;
    FHeaderLevel: Integer;
    FSummaryContent: TStringList;
    FTitle: String;
    FKeyWords : TKeyWords;
    FScopesToDocument: TScopes;
    FIndex : TStringList;
    FHTMLFileName: String;
    FSummaryDocConflicts : TObjectList;
    FSections : TStringList;
    FPerfCounters : TStringList;
    procedure OutputSummarySpecialTags;
    function GetSumDocCons(iIndex : Integer): TSumDocCon;
  Protected
    procedure GenerateImages(strImageDirectory: String);
    function ExpandLinkTag(strToken: String): String;
    Procedure OutputHTMLDocumentation(strFileName : String);
    Procedure GenerateSummary;
    Procedure GenerateCSS;
    Procedure GenerateIndex;
    procedure GenerateSchema;
    Procedure GenerateHTML(slHTMLFile : TStringList; strTitle : String);
    Procedure GenerateModuleList(slHTMLFile : TStringList);
    Procedure GenerateSectionList(slHTMLFile : TStringList);
    Procedure GenerateContent(slHTMLFile : TStringList);
    Function FindInsertionPoint(slHTMLFile : TStringList;
      strText : String) : Integer;
    Function GetStringResource(strName : String) : String;
    Function GetMainDocument : String; Override;
    Procedure GenerateErrorsWarningsHints(slContents : TStringList);
    Procedure GenerateDocumentConflicts(slContents : TStringList);
    Procedure GenerateSpecialTags(slContents : TStringList);
    Procedure OutputComment(slContents : TStringList; E : TElementContainer;
      iIndentLevel : Integer);
    Procedure OutputContainers(slContents : TStringList;
      Container : TElementContainer; iIndentLevel : Integer;
      strContainerLabel : String);
    Function N(strText : String) : String;
    Function P(strText : String) : String;
    Procedure InitialiseSummary;
    Procedure OutputErrorsWarningsAndHints(slEWH : TStringList; strSectionTitle,
      strLIType : String; AImageIndex : TImageIndex);
    Procedure OutputSummaryDocumentationConflicts;
    { HTML Tag Outputs }
    Function A(strText, strHREF : String; strName : String = '') : String;
    Function H(strText : String; iLevel : Integer; AImage : TImageIndex;
      AScope : TScope) : String;
    Function IMG(AImageIndex : TImageIndex; AScope : TScope) : String;
    Function LI(strClass, strText : String) : String;
    (**
      This property provides access to an array of documentation conflicts for
      all the modules.
      @precon  iIndex must be a valid index into the array 0 to count - 1.
      @postcon Provides access to an array of documentation conflicts for
               all the modules.
      @param   iIndex as       an Integer
      @return  a TSumDocCon
    **)
    Property SumDocCons[iIndex : Integer] : TSumDocCon Read GetSumDocCons;
  Public
    Constructor Create(strOutputDirectory, strTitle : String); Override;
    Destructor Destroy; Override;
    Procedure OutputDocumentation; Override;
  End;

Implementation

Uses
  SysUtils, Windows, ModuleDispatcher, DGHLibrary, Graphics,
  {$IFNDEF D2007} GIFImage {$ELSE} GIFImg {$ENDIF}, Controls, StrUtils,
  GenericTokenizer;

(**


  This method output anchor tags into the HTML information.


  @precon  None.

  @postcon Output anchor tags into the HTML information.


  @param   strText as a String
  @param   strHREF as a String
  @param   strName as a String
  @return  a String

**)
Function THTMLDocumentation.A(strText, strHREF : String;
  strName : String = '') : String;

Begin
  strHREF := StringReplace(strHREF, #32, '', [rfReplaceAll]);
  strName := StringReplace(strName, #32, '', [rfReplaceAll]);
  If strHREF <> '' Then
    Result := Format('<a href="%s">%s</a>', [strHREF, strText])
  Else
    Result := Format('<a name="%s"></a>%s', [strName, strText]);
End;

(**

  This is a constructor for the THTMLDocumentation class.

  @precon  None. 
  @postcon Initialises the Special Tag string lists. 

  @param   strOutputDirectory as a String
  @param   strTitle           as a String

**)
constructor THTMLDocumentation.Create(strOutputDirectory, strTitle: String);

Var
  i : Integer;

begin
  Inherited Create(strOutputDirectory, strTitle);
  FScopesToDocument := BrowseAndDocItOptions.ScopesToDocument + [scNone, scGlobal];
  FTitle := strTitle;
  FModuleSpecialTagNodes := TObjectList.Create(true);
  For i := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
    FModuleSpecialTagNodes.Add(TStringList.Create);
  FSummarySpecialTagNodes := TObjectList.Create(true);
  For i := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
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

  @param   strToken as a String
  @return  a String

**)
Function THTMLDocumentation.ExpandLinkTag(strToken : String) : String;

  (**

    This function inspects the HREF value and extras the module, symbol and
    subsymbol reference and formats the link accordingly.

    @precon  strHREF is the hyper text reference for the link as detailed in
             {@link TDocHTMLOutput.ExpandLinkTag ExpandLinkTag} and strLabel is
             the label for the link. If null a label is derived from the HREF.
    @postcon Returns a formatted HTML hypertext reference.

    @param   strHREF  as a String
    @param   strLabel as a String
    @return  a String

  **)
  Function FormatTag(strHREF, strLabel : String) : String;

  Var
    strModule : String;
    strSymbol : String;
    strSubSymbol : String;
    i : Integer;

  Begin
    i := Pos('#', strHREF);
    If i <> 0 Then
      Begin
        strModule := Copy(strHREF, 1, i - 1);
        Delete(strHREF, 1, i);
      End Else
        strModule := ChangeFileExt(ExtractFileName(FCurrentModule.FileName), '.html');
    i := Pos('.', strHREF);
    If i <> 0 Then
      Begin
        strSymbol := Copy(strHREF, 1, i - 1);
        strSubSymbol := '.' + Copy(strHREF, i + 1, Length(strHREF) - i);
      End Else
      Begin
        strSymbol := strHREF;
        strSubSymbol := '';
      End;
    If strLabel = '' Then strLabel := strSymbol + strSubSymbol;
    Result := Format('<a href="%s.html#%s%s">%s</a>', [strModule, strSymbol,
      strSubSymbol, strLabel]);
  End;

Var
  i : Integer;
  strHREF : String;
  strLabel : String;

Begin
  Result := strToken;
  If LowerCase(Copy(strToken, 1, 7)) <> '{@link ' Then
    Exit;
  Delete(strToken, 1, 7);
  Delete(strToken, Length(strToken), 1);
  i := Pos(#32, strToken);
  If i <> 0 Then
    Begin
      strHREF := Copy(strToken, 1, i - 1);
      strLabel := N(Copy(strToken, i + 1, Length(strToken) - i));
    End Else
    Begin
      strHREF := strToken;
      If (Length(strToken) > 0) And (strToken[1] = '#') Then
        Delete(strToken, 1, 1);
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
    If doShowPrefCountersInDocSummary In BrowseAndDocItOptions.Options Then
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
          IMG(TImageIndex(FSections.Objects[i]), scNone),
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

  @param   slHTMLFile as a TStringList
  @param   strText    as a String
  @return  an Integer

**)
Function THTMLDocumentation.FindInsertionPoint(slHTMLFile : TStringList;
  strText : String) : Integer;

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


  This method outputs the contents of the html to a string list which is then
  inserted into the template HTML file.

  @precon  slContents must be a valid instance of a string list.
  @postcon Outputs the contents of the html to a string list which is then
           inserted into the template HTML file.

  @param   slHTMLFile as a TStringList

**)
Procedure THTMLDocumentation.GenerateContent(slHTMLFile : TStringList);

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
    slContents.Add(Format('<!-- %s -->', [FCurrentModule.ModuleName]));
    slContents.Add(H(A('Documentation for ' +
      ExtractFileName(FCurrentModule.ModuleName), '',
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
          If AnsiCompareText(FCurrentModule[i].AsString(True, True), strSections[j]) = 0 Then
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
    With BrowseAndDocItOptions Do
      For i := Low(TBADITokenType) to High(TBADITokenType) Do
        Begin
          sl.Add(Format('span.%s {', [strTokenType[i]]));
          sl.Add(Format('  color            : #%s;', [HTMLColour(TokenFontInfo[i].FColour)]));
          If fsBold In TokenFontInfo[i].FStyles Then
            sl.Add('  font-weight      : bold;');
          If fsItalic In TokenFontInfo[i].FStyles Then
            sl.Add('  font-stylet      : italic;');
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
          BrowseAndDocItOptions.BGColour), []);
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
  @postcon Outputs the document conflicts to the html file as an
           unordered list.


  @param   slContents as a TStringList

**)
procedure THTMLDocumentation.GenerateDocumentConflicts(slContents : TStringList);

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
          slContents.Add('    ' + LI(ImageList[E[i][j].ImageIndex].FResourcename,
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
  @postcon Output Errors, Hints and Warnings to the html file as an
           unordered list.


  @param   slContents as a TStringList

**)
procedure THTMLDocumentation.GenerateErrorsWarningsHints(slContents : TStringList);

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
          slContents.Add(#32#32 + LI(ImageList[E[j].ImageIndex].FResourcename,
          N(E[j].AsString(True, True))));
          sls[i].Add(Format('%s=%s', [ExtractFileName(FCurrentModule.FileName),
            E[j].AsString(True, True)]));
        End;
      slContents.Add('</ul>');
      slContents.Add('');
    End;
end;

(**


  This method gets the HTML template from the windows resource and sets its
  title.

  @precon  None.
  @postcon Gets the HTML template from the windows resource and sets its
           title.


  @param   slHTMLFile as a TStringList
  @param   strTitle as a String

**)
Procedure THTMLDocumentation.GenerateHTML(slHTMLFile : TStringList; strTitle : String);

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


  @param   strImageDirectory as a String

**)
Procedure THTMLDocumentation.GenerateImages(strImageDirectory : String);

Var
  i : TImageIndex;
  BitMap : TBitMap;
  GIF : TGIFImage;
  strFileName: String;


Begin
  ForceDirectories(strImageDirectory);
  For i := Succ(Low(TImageIndex)) To High(TImageIndex) Do
    Begin
      Update(FProgressIndex, Format('Generating Image %s...', [
        ImageList[i].FResourcename]));
      Inc(FProgressIndex);
      BitMap := TBitMap.Create;
      try
        BitMap.LoadFromResourceName(hInstance, ImageList[i].FResourcename);
        BitMap.Transparent := True;
        BitMap.TransparentColor := ImageList[i].FMaskColour;
        BitMap.TransparentMode := tmFixed;
        GIF := TGIFImage.Create;
        Try
          GIF.Transparent := True;
          GIF.Assign(BitMap);
          strFileName := Format('%s%s.gif', [strImageDirectory,
            ImageList[i].FResourcename]);
          If Not FileExists(strFileName) Then
            GIF.SaveToFile(strFileName);
        Finally
          GIF.Free;
        End;
      finally
        BitMap.Free;
      end;
    End;
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
            If (strLastAlpha <> '') And (AnsiCompareText(strCurAlpha, strLastAlpha) <> 0) Then
              Begin
                slC.Add('  </table>');
                slC.Add('</div>');
              End;
            If AnsiCompareText(strCurAlpha, strLastAlpha) <> 0 Then
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

  @param   slHTMLFile as a TStringList

**)
Procedure THTMLDocumentation.GenerateModuleList(slHTMLFile : TStringList);

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

  @param   slHTMLFile as a TStringList

**)
Procedure THTMLDocumentation.GenerateSectionList(slHTMLFile : TStringList);

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
          strName := BrowseAndDocItOptions.SpecialTags.Values[
            BrowseAndDocItOptions.SpecialTags.Names[i]];
          strIdent := BrowseAndDocItOptions.SpecialTags.Names[i];
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
  @postcon Outputs special tags (todos, bugs, etc) to the html file as an
           unordered list.

  @param   slContents as a TStringList

**)
procedure THTMLDocumentation.GenerateSpecialTags(slContents: TStringList);

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
          If Integer(BrowseAndDocItOptions.SpecialTags.Objects[k]) And iShowInDoc > 0 Then
            If AnsiCompareText(Tag[j].TagName,
              BrowseAndDocItOptions.SpecialTags.Names[k]) = 0 Then
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
      strSection := BrowseAndDocItOptions.SpecialTags.ValueFromIndex[i];
      slContents.Add(Format('<!-- %s -->', [strSection]));
      slContents.Add(H(A(strSection, '', BrowseAndDocItOptions.SpecialTags.Names[i]),
        FHeaderLevel, iiToDoFolder, scNone));
      slContents.Add('<ul>');
      For j := 0 To sl.Count - 1 Do
        slContents.Add(#32#32 + LI('ToDoItem', N(sl[j])));
      slContents.Add('</ul>');
      slContents.Add('');
    End;
  For j := 0 To FModuleSpecialTagNodes.Count - 1 Do
    Begin
      If Integer(BrowseAndDocItOptions.SpecialTags.Objects[j]) And
        iShowInDoc > 0 Then
        Begin
          i := (FModuleSpecialTagNodes[j] As TStringList).Count;
          If i = 0 Then
            FSummaryContent.Add(Format('        <td class="Green">%d</td>', [i]))
          Else
            FSummaryContent.Add(Format('        <td class="Yellow">%d</td>', [i]));
        End;
    End;
  If doShowPrefCountersInDocSummary In BrowseAndDocItOptions.Options Then
    Begin
      For i := 1 To FCurrentModule.OpTickCounts - 1 Do
        FSummaryContent.Add(Format('        <td>%d</td>', [
          FCurrentModule.OpTickCountByIndex[i] -
          FCurrentModule.OpTickCountByIndex[i - 1]]));
      FSummaryContent.Add(Format('        <td>%d</td>', [
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


  @param   strName as a String
  @return  a String 

**)
function THTMLDocumentation.GetStringResource(strName: String): String;

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

  @param   iIndex as an Integer
  @return  a TSumDocCon

**)
function THTMLDocumentation.GetSumDocCons(iIndex : Integer): TSumDocCon;
begin
  Result := FSummaryDocConflicts[iIndex]  As TSumDocCon;
end;

(**

  This method returns a string representing a header tag. 

  @precon  None. 
  @postcon Returns a string representing a header tag. 

  @param   strText as a String
  @param   iLevel  as an Integer
  @param   AImage  as a TImageIndex
  @param   AScope  as a TScope
  @return  a String 

**)
function THTMLDocumentation.H(strText: String; iLevel: Integer;
  AImage : TImageIndex; AScope : TScope): String;

begin
  Result := Format('<h%d>%s&nbsp;%s</h%d>', [iLevel, IMG(AImage, AScope),
   strText, iLevel]);
end;

(**

  This method outputs an img tag with the image vertically aligned central. 

  @precon  None. 
  @postcon Outputs an img tag with the image vertically aligned central. 

  @param   AImageIndex as a TImageIndex
  @param   AScope      as a TScope
  @return  a String     

**)
function THTMLDocumentation.IMG(AImageIndex: TImageIndex; AScope : TScope): String;

Var
  iImage : TImageIndex;

begin
  Result := '';
  If AImageIndex <> iiNone then
    Begin
      Case AScope Of
        scPrivate   : iImage := Succ(AImageIndex);
        scPublished : iImage := Succ(Succ(AImageIndex));
        scProtected : iImage := Succ(Succ(Succ(AImageIndex)));
        scLocal     : iImage := Succ(Succ(Succ(Succ(AImageIndex))));
      Else
        iImage := AImageIndex; // scPublic, scGlobal, scNone
      End;
      Result := Format('<img class="verticallymiddle" src="Images/%s.gif" alt=""/>',
        [ImageList[iImage].FResourcename]);
    End;
end;

(**


  This method returns a list item tag as a string. 


  @precon  None. 

  @postcon Returns a list item tag as a string. 


  @param   strClass as a String
  @param   strText  as a String
  @return  a String  

**)
function THTMLDocumentation.LI(strClass, strText: String): String;
begin
  Result := Format('<li class="%s">%s</li>', [strClass, strText]);
end;

(**


  This method outputs the comment text along with the tag information. 


  @precon  None. 

  @postcon Outputs the comment text along with the tag information. 


  @param   slContents   as a TStringList
  @param   E            as a TElementContainer
  @param   iIndentLevel as an Integer

**)
procedure THTMLDocumentation.OutputComment(slContents : TStringList;
  E: TElementContainer; iIndentLevel : Integer);

var
  i: Integer;
  strComment: String;
  k: Integer;
  strTags: String;
  j: Integer;
  strIndent: String;

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
  With BrowseAndDocItOptions Do
    For i := 0 To SpecialTags.Count - 1 Do
      Begin
        If E.Comment.FindTag(SpecialTags.Names[i]) > -1 Then
          Begin
            slContents.Add(Format('%s  <p class="TagHeader">%s</p>', [
              strIndent, SpecialTags.ValueFromIndex[i]]));
            slContents.Add(Format('%s  <ul>', [strIndent]));
            For j := 0 To E.Comment.TagCount - 1 Do
              If AnsiCompareText(E.Comment.Tag[j].TagName, SpecialTags.Names[i]) = 0 Then
                Begin
                  strTags := '';
                  For k := 0 To E.Comment.Tag[j].TokenCount - 1 Do
                    If E.Comment.Tag[j].Tokens[k].TokenType = ttLinkTag Then
                      strTags := strTags + Format('%s ', [ExpandLinkTag(E.Comment.Tag[j].Tokens[k].Token)])
                    Else
                      strTags := strTags + Format('%s ', [N(E.Comment.Tag[j].Tokens[k].Token)]);
                  slContents.Add(Format('%s    %s', [strIndent, LI('SpecialTag', strTags)]));
                End;
            slContents.Add(Format('%s  </ul>', [strIndent]));
          End;
      End;
end;

(**

  This method output the tree of information stored in the module recursively. 

  @precon  slContents must be a valid indtance of a string list and Container 
           must ba a valid descendant of TElement Container. 
  @postcon Output the tree of information stored in the module recursively. 

  @param   slContents        as a TStringList
  @param   Container         as a TElementContainer
  @param   iIndentLevel      as an Integer
  @param   strContainerLabel as a String

**)
procedure THTMLDocumentation.OutputContainers(slContents: TStringList;
  Container: TElementContainer; iIndentLevel : Integer;
  strContainerLabel : String);

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
          slContents.Add(strIndent + Format('          <pre>%s</pre>', [
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
  Initialise(5 + Integer(High(TImageIndex)) + FFileNames.Count, 'HTML Documentation',
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

  This method outputs the Errors, Warnings and Hints for all modules to the
  summary html page.

  @precon  slEWH must be a valid instance of a string list.
  @postcon Outputs the Errors, Warnings and Hints for all modules to the
           summary html page.

  @param   slEWH           as a TStringList
  @param   strSectionTitle as a String
  @param   strLIType       as a String
  @param   AImageIndex     as a TImageIndex

**)
procedure THTMLDocumentation.OutputErrorsWarningsAndHints(slEWH: TStringList;
  strSectionTitle, strLIType : String; AImageIndex : TImageIndex);

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
          BrowseAndDocItOptions.SpecialTags.ValueFromIndex[i]]));
        FSections.AddObject(BrowseAndDocItOptions.SpecialTags.ValueFromIndex[i],
          TObject(iiTodoFolder));
        sl := (FSummarySpecialTagNodes[i] as TStringList);
        FSummaryContent.Add(H(A(BrowseAndDocItOptions.SpecialTags.ValueFromIndex[i], '',
          BrowseAndDocItOptions.SpecialTags.ValueFromIndex[i]), 2, iiToDoFolder, scNone));
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
  With BrowseAndDocItOptions Do
    For i := 0 to SpecialTags.Count - 1 Do
      If Integer(SpecialTags.Objects[i]) And iShowInDoc > 0 Then
        FSummaryContent.Add(Format('        <th>%s</th>', [SpecialTags.ValueFromIndex[i]]));
  With BrowseAndDocItOptions Do
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


  @param   strFileName as a String

**)
Procedure THTMLDocumentation.OutputHTMLDocumentation(strFileName : String);

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
    FCurrentModule := Dispatcher(Source.Text, strFileName, False, [moParse,
      moCheckForDocumentConflicts]);
    If FCurrentModule <> Nil Then
      Try
        If doShowPrefCountersInDocSummary In BrowseAndDocItOptions.Options Then
          If FPerfCounters.Count = 0 Then
            Begin
              For i := 1 To FCurrentModule.OpTickCounts - 1 Do
                FPerfCounters.Add(FCurrentModule.OpTickCountName[i]);
              FPerfCounters.Add('Total');
            End;
        FKeyWords := FCurrentModule.KeyWords;
        FSummaryContent.Add('      <tr>');
        FSummaryContent.Add(Format('        <td>%s</td>', [
          A(FCurrentModule.ModuleName,
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

  @param   strText as a String
  @return  a String 

**)
function THTMLDocumentation.P(strText: String): String;

Var
  sl : TStringList;
  i: Integer;

begin
  Result := '';
  sl := Tokenize(strText, FKeyWords);
  Try
    For i := 0 To sl.Count - 1 Do
      Case TBADITokenType(sl.Objects[i]) Of
        ttReservedWord : Result := Result + Format('<span class="ReservedWord">%s</span>', [N(sl[i])]);
        ttIdentifier   : Result := Result + Format('<span class="Identifier">%s</span>', [N(sl[i])]);
        ttSymbol       : Result := Result + Format('<span class="Symbol">%s</span>', [N(sl[i])]);
        ttStringLiteral: Result := Result + Format('<span class="StringLiteral">%s</span>', [N(sl[i])]);
        ttNumber       : Result := Result + Format('<span class="Number">%s</span>', [N(sl[i])]);
      Else
        Result := Result + N(sl[i]);
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


  @param   strText as a String
  @return  a String

**)
function THTMLDocumentation.N(strText : String): String;

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
      {$IFNDEF D2009}
      If Not (strText[i] In strValidHTMLChars) Then
      {$ELSE}
      If Not (CharInSet(strText[i], strValidHTMLChars)) Then
      {$ENDIF}
        Result := StringReplace(Result, strText[i],
          Format('&#%d;', [Ord(strText[i])]), [rfReplaceAll]);
    End;
end;

{ TSumDocCon }

(**

  This is a constructor for the TSumDocCon class.

  @precon  None.
  @postcon Initialises the class.

  @param   strModule as a String

**)
constructor TSumDocCon.Create(strModule : String);
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
