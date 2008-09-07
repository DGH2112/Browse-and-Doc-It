(**

  This module contains a documentation engine for producing static HTML
  information.

  @Author  David Hoyle
  @Date    07 Sep 2008
  @Version 1.0

**)
Unit HTMLDocumentation;

Interface

Uses
  Classes, BaseLanguageModule, BaseDocumentation, Contnrs;

Type
  (** This is a class to produce HTML documentation. **)
  THTMLDocumentation = Class(TBaseDocumentation)
  Private
    FCurrentModule   : TBaseLanguageModule;
    FCurrentHTMLFile : TStringList;
    FProgressIndex: Integer;
    FSpecialTagNodes: TObjectList;
    FHeaderLevel: Integer;
    FSummaryContent: TStringList;
    FTitle: String;
    FKeyWords : TKeyWords;
  Protected
    procedure GenerateImages(strImageDirectory: String);
    function ExpandLinkTag(strToken: String): String;
    Procedure OutputHTMLDocumentation(strFileName : String);
    Procedure GenerateSummary;
    Procedure GenerateCSS;
    procedure GenerateSchema;
    Procedure GenerateHTML(strTitle : String);
    Procedure GenerateModuleList;
    Procedure GenerateSectionList;
    Procedure GenerateContent;
    Function FindInsertionPoint(strText : String) : Integer;
    Function GetStringResource(strName : String) : String;
    Function GetMainDocument : String; Override;
    Procedure GenerateErrorsWarningsHints(slContents : TStringList);
    Procedure GenerateDocumentConflicts(slContents : TStringList);
    Procedure GenerateSpecialTags(slContents : TStringList);
    Procedure OutputComment(slContents : TStringList; E : TElementContainer;
      iIndentLevel : Integer);
    Procedure OutputContainers(slContents : TStringList;
      Container : TElementContainer; iIndentLevel : Integer);
    Function N(strText : String) : String;
    { HTML Tag Outputs }
    Function A(strText, strHREF : String; strName : String = '') : String;
    Function H(strText : String; iLevel : Integer; AImage : TImageIndex) : String;
    Function IMG(AImageIndex : TImageIndex) : String;
    Function LI(strClass, strText : String) : String;
  Public
    Constructor Create(strOutputDirectory, strTitle : String); Override;
    Destructor Destroy; Override;
    Procedure OutputDocumentation; Override;
  End;

Implementation

Uses
  SysUtils, Windows, ModuleDispatcher, DGHLibrary, Graphics, GIFImage, Controls;

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
  FTitle := strTitle;
  FSpecialTagNodes := TObjectList.Create(true);
  For i := 1 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
    FSpecialTagNodes.Add(TStringList.Create);
  FSummaryContent := TStringList.Create;
  FSummaryContent.Add(H(Format('Summary for Project %s', [FTitle]), 1, iiModule));
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
    For i := 0 To SpecialTags.Count - 1 Do
      If Integer(SpecialTags.Objects[i]) And iShowInTree > 0 Then
        FSummaryContent.Add(Format('        <th>%s</th>', [
          SpecialTags.ValueFromIndex[i]]));
  FSummaryContent.Add('      </tr>');
  FSummaryContent.Add('    </thead>');
  FSummaryContent.Add('    <tbody>');
end;

(**


  This is a destructor for the THTMLDocumentation class.

  @precon  None.
  @postcon Frees the Special Tags string lists.


**)
destructor THTMLDocumentation.Destroy;
begin
  FSummaryContent.Free;
  FSpecialTagNodes.Free;
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
    Result := Format('<A HREF="%s.html#%s%s">%s</A>', [strModule, strSymbol,
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
      strLabel := Copy(strToken, i + 1, Length(strToken) - i);
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

begin
  slSummary := TStringList.Create;
  Try
    FCurrentHTMLFile := slSummary;
    GenerateHTML('Summary');
    GenerateModuleList;
    iIns := FindInsertionPoint('*$SECTIONLIST$');
    FCurrentHTMLFile[iIns] := '';
    iIns := FindInsertionPoint('*$CONTENT$');
    i := Pos('$', FCurrentHTMLFile[iIns]);
    strIndent := StringOfChar(#32, i - 1);
    FSummaryContent.Add('    </tbody>');
    FSummaryContent.Add('  <table>');
    FSummaryContent.Add('</div>');
    For i := 0 To FSummaryContent.Count - 1 Do
      FSummaryContent[i] := strIndent + FSummaryContent[i];
    slSummary[iIns] := FSummaryContent.Text;
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


  @param   strText as a String
  @return  an Integer

**)
Function THTMLDocumentation.FindInsertionPoint(strText : String) : Integer;

ResourceString
  strInsertionPointNotFound = 'Insertion Point "%s" not found!';

Var
  i : Integer;

Begin
  For i := 0 To FCurrentHTMLFile.Count - 1 Do
    If Like(strText, FCurrentHTMLFile[i]) Then
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


**)
Procedure THTMLDocumentation.GenerateContent;

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
  iIns := FindInsertionPoint('*$CONTENT$');
  i := Pos('$', FCurrentHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  slContents := TStringList.Create;
  Try
    slContents.Add(Format('<!-- %s -->', [FCurrentModule.ModuleName]));
    slContents.Add(H(A('Documentation for ' +
      ExtractFileName(FCurrentModule.ModuleName), '',
      'ModuleOverview'), FHeaderLevel, FCurrentModule.ImageIndex));
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
          If AnsiCompareText(FCurrentModule[i].AsString(True), strSections[j]) = 0 Then
            Begin
              boolIgnore := True;
              Break;
            End;
        If Not boolIgnore Then
          OutputContainers(slContents, FCurrentModule[i], 0);
      End;
    For i := 0 To slContents.Count - 1 Do
      slContents[i] := strIndent + slContents[i];
    FCurrentHTMLFile[iIns] := slContents.Text;
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

Begin
  Update(FProgressIndex, 'Generating CSS...');
  Inc(FProgressIndex);
  ForceDirectories(FOutputDirectory + '\Styles');
  sl := TStringList.Create;
  Try
    sl.Text := GetStringResource('BrowseAndDocItCSS');
    sl.SaveToFile(FOutputDirectory + 'Styles\BrowseAndDocIt.CSS');
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

begin
  E := FCurrentModule.FindElement(strDocumentationConflicts);
  If E = Nil Then
    Exit;
  slContents.Add(H(A(strDocumentationConflicts, '', strDocumentationConflicts),
    FHeaderLevel, E.ImageIndex));
  For i := 1 To E.ElementCount Do
    Begin
      slContents.Add(Format('<!-- %s -->', [strDocumentationConflicts]));
      slContents.Add(H(E[i].AsString(True), FHeaderLevel + 1, E[i].ImageIndex));
      slContents.Add('<ul>');
      For j := 1 To E[i].ElementCount Do
        slContents.Add(#32#32 + LI(ImageList[E[i][j].ImageIndex].FResourcename,
          N(E[i][j].AsString(True))));
      slContents.Add('</ul>');
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
  strSections : Array[1..3] Of String = (strErrors, strWarnings, strHints);

Var
  i, j : Integer;
  E : TElementContainer;

begin
  For i := Low(strSections) to High(strSections) Do
    Begin
      E := FCurrentModule.FindElement(strSections[i]);
      If E = Nil Then
        Continue;
      slContents.Add(Format('<!-- %s -->', [strSections[i]]));
      slContents.Add(H(A(strSections[i], '', strSections[i]), FHeaderLevel, E.ImageIndex));
      slContents.Add('<ul>');
      For j := 1 To E.ElementCount Do
        slContents.Add(#32#32 + LI(ImageList[E[j].ImageIndex].FResourcename,
        N(E[j].AsString(True))));
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


  @param   strTitle as a String

**)
Procedure THTMLDocumentation.GenerateHTML(strTitle : String);

Begin
 FCurrentHTMLFile.Text := StringReplace(GetStringResource(
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


  This method outputs a menu of all the module in this package of documentation.

  @precon  None.
  @postcon Outputs a menu of all the module in this package of documentation.


**)
Procedure THTMLDocumentation.GenerateModuleList;

Var
  iIns : Integer;
  sl : TStringList;
  strIndent : String;
  i : Integer;
  strFileName : String;

Begin
  iIns := FindInsertionPoint('*$MODULELIST$');
  i := Pos('$', FCurrentHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  sl := TStringList.Create;
  Try
    sl.Add(strIndent + '<div class="List">');
    sl.Add(strIndent + '  <div class="ModuleTitle">Modules</div>');
    sl.Add(strIndent + Format('  <div class="Module">%s&nbsp;%s</div>',
      [IMG(iiModule), A('Summary', 'Summary.html')]));
    For i := 0 To FFileNames.Count - 1 Do
      Begin
        strFileName := ExtractFileName(FFileNames[i]);
        sl.Add(strIndent + Format('  <div class="Module">%s&nbsp;%s</div>',
          [IMG(iiModule), A(ExtractFileName(strFileName),
          ChangeFileExt(strFileName, '.html'))]));
      End;
    sl.Add(strIndent + Format('  <div class="Module">%s&nbsp;%s</div>',
      [IMG(iiModule), A('Index', 'Index.html')]));
    sl.Add(strIndent + '</div>');
    FCurrentHTMLFile[iIns] := sl.Text;
  Finally
    sl.Free;
  End;
End;

(**


  This method generates a menu for the various section contained in this module.

  @precon  None.
  @postcon Generates a menu for the various section contained in this module.


**)
Procedure THTMLDocumentation.GenerateSectionList;

Var
  iIns : Integer;
  sl : TStringList;
  strIndent : String;
  i : Integer;
  E : TElementContainer;

Begin
  iIns := FindInsertionPoint('*$SECTIONLIST$');
  i := Pos('$', FCurrentHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  sl := TStringList.Create;
  Try
    sl.Add(strIndent + Format('<div class="Section">%s&nbsp;%s</div>', [
      IMG(iiModule), A('Module Overview', '#ModuleOverview')]));
    For i := 1 To FCurrentModule.ElementCount Do
      Begin
        E := FCurrentModule.Elements[i];
        sl.Add(strIndent + Format('<div class="Section">%s&nbsp;%s</div>', [
          IMG(E.ImageIndex), A(E.AsString(True), '#' + E.AsString(True) + '2')]));
      End;
    FCurrentHTMLFile[iIns] := sl.Text;
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
  For i := 0 To FSpecialTagNodes.Count - 1 Do
    (FSpecialTagNodes[i] As TStringList).Clear;
  For i := 0 To FCurrentModule.BodyCommentCount - 1 Do
    With FCurrentModule.BodyComment[i] Do
      For j := 0 To TagCount - 1 Do
        For k := 0 To FSpecialTagNodes.Count - 1 Do
          If Integer(BrowseAndDocItOptions.SpecialTags.Objects[k]) And iShowInTree > 0 Then
            If AnsiCompareText(Tag[j].TagName, BrowseAndDocItOptions.SpecialTags.Names[k]) = 0 Then
              (FSpecialTagNodes[k] As TStringList).Add(Tag[j].AsString(False));
  For i := 0 To FSpecialTagNodes.Count - 1 Do
    Begin
      sl := FSpecialTagNodes[i] As TStringList;
      If sl.Count = 0 Then
        Continue;
      strSection := BrowseAndDocItOptions.SpecialTags.ValueFromIndex[i];
      slContents.Add(Format('<!-- %s -->', [strSection]));
      slContents.Add(H(A(strSection, '', strSection), FHeaderLevel, iiToDoFolder));
      slContents.Add('<ul>');
      For j := 0 To sl.Count - 1 Do
        slContents.Add(#32#32 + LI('ToDoItem', N(sl[j])));
      slContents.Add('</ul>');
      slContents.Add('');
    End;
  For j := 0 To FSpecialTagNodes.Count - 1 Do
    Begin
      If Integer(BrowseAndDocItOptions.SpecialTags.Objects[j]) And
        iShowInTree > 0 Then
        Begin
          i := (FSpecialTagNodes[j] As TStringList).Count;
          If i = 0 Then
            FSummaryContent.Add(Format('        <td class="Green">%d</td>', [i]))
          Else
            FSummaryContent.Add(Format('        <td class="Yellow">%d</td>', [i]));
        End;
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
    sl.Text := GetStringResource('BrowseAndDocItXHTMLStrict');
    sl.SaveToFile(FOutputDirectory + 'Schema\xhtml1-strict.dtd');
    sl.Text := GetStringResource('BrowseAndDocItXHTMLSymbol');
    sl.SaveToFile(FOutputDirectory + 'Schema\xhtml-symbol.ent');
    sl.Text := GetStringResource('BrowseAndDocItXHTMLSpecial');
    sl.SaveToFile(FOutputDirectory + 'Schema\xhtml-special.ent');
    sl.Text := GetStringResource('BrowseAndDocItXHTMLLat1');
    sl.SaveToFile(FOutputDirectory + 'Schema\xhtml-lat1.ent');
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

ResourceString
  strResourceNotFound = 'Resource "%s" not found.';

Var
  Res: TResourceStream;

begin
  Res := TResourceStream.Create(HInstance, strName, RT_RCDATA);
  Try
    If Res.Size = 0 Then
      Raise Exception.CreateFmt(strName, [strName]);
    SetLength(Result, Res.Size);
    Res.ReadBuffer(Result[1], Res.Size);
  Finally
    Res.Free;
  End;
end;

(**


  This method returns a string representing a header tag. 


  @precon  None.

  @postcon Returns a string representing a header tag.


  @param   strText as a String
  @param   iLevel  as an Integer
  @param   AImage  as a TImageIndex
  @return  a String

**)
function THTMLDocumentation.H(strText: String; iLevel: Integer;
  AImage : TImageIndex): String;

begin
  Result := Format('<h%d>%s&nbsp;%s</h%d>', [iLevel, IMG(AImage), strText, iLevel]);
end;

(**


  This method outputs an img tag with the image vertically aligned central.

  @precon  None.
  @postcon Outputs an img tag with the image vertically aligned central.


  @param   AImageIndex as a TImageIndex
  @return  a String

**)
function THTMLDocumentation.IMG(AImageIndex: TImageIndex): String;
begin
  Result := Format('<img class="verticallymiddle" src="Images/%s.gif" alt=""/>',
    [ImageList[AImageIndex].FResourcename]);
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
    If E.Comment.TokenType[i] = ttLinkTag Then
      strComment := strComment + Format('%s ', [ExpandLinkTag(E.Comment.Token[i])])
    Else
      strComment := strComment + Format('%s ', [E.Comment.Token[i]]);
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
                    If E.Comment.Tag[j].TokenType[k] = ttLinkTag Then
                      strTags := strTags + Format('%s ', [ExpandLinkTag(E.Comment.Tag[j].Token[k])])
                    Else
                      strTags := strTags + Format('%s ', [E.Comment.Tag[j].Token[k]]);
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


  @param   slContents   as a TStringList
  @param   Container    as a TElementContainer
  @param   iIndentLevel as an Integer

**)
procedure THTMLDocumentation.OutputContainers(slContents: TStringList;
  Container: TElementContainer; iIndentLevel : Integer);

var
  i: Integer;
  boolHeader : Boolean;
  boolIncHeader: Boolean;
  strIndent : String;

begin
  strIndent := StringOfChar(#32, 4 * iIndentLevel);
  boolHeader := False;
  boolIncHeader := False;
  slContents.Add(Format('%s<!-- %s -->', [strIndent, Container.AsString(True)]));
  If Container Is TLabelContainer Then
    Begin
      slContents.Add(strIndent + H(A(Container.AsString(True), '',
        Container.AsString(True) + IntToStr(FHeaderLevel)), FHeaderLevel,
        Container.ImageIndex));
      OutputComment(slContents, Container, iIndentLevel);
      Inc(FHeaderLevel);
      boolIncHeader := True;
    End;
  slContents.Add(strIndent + '<div class="Indent">');
  For i := 1 To Container.ElementCount Do
    If Not (Container[i] Is TLabelContainer) Then
      Begin
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
          IMG(Container[i].ImageIndex), N(Container[i].Identifier)]));
        slContents.Add(strIndent + '        <td>');
        slContents.Add(strIndent + Format('          <pre wrap>%s</pre>', [
          N(Container[i].AsString(True))]));
        OutputComment(slContents, Container[i], iIndentLevel + 1);
        OutputContainers(slContents, Container[i], iIndentLevel + 1);
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
      Begin
        OutputComment(slContents, Container[i], iIndentLevel);
        OutputContainers(slContents, Container[i], iIndentLevel + 1);
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
  Initialise(4 + Integer(High(TImageIndex)) + FFileNames.Count, 'HTML Documentation',
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
  Finally
    Finalise;
  End;
End;

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
  Stream : TFileStream;
  strHTMLName : String;
  i: Integer;
  E: TElementContainer;
  j: Integer;

Begin
  Stream := TFileStream.Create(strFileName, fmOpenRead Or fmShareDenyNone);
  Try
    FCurrentModule := Dispatcher(Stream, strFileName, False, [moParse,
      moCheckForDocumentConflicts]);
    If FCurrentModule <> Nil Then
      Try
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
              i := E.ElementCount;
            If i = 0 Then
              FSummaryContent.Add(Format('        <td class="Green">%d</td>', [i]))
            Else
              FSummaryContent.Add(Format('        <td class="Red">%d</td>', [i]));
          End;
        FCurrentHTMLFile := TStringList.Create;
        Try
          GenerateHTML(ExtractFileName(strFileName));
          GenerateModuleList;
          GenerateSectionList;
          FHeaderLevel := 1;
          GenerateContent;
          strHTMLName := FOutputDirectory + ChangeFileExt(ExtractFileName(strFileName), '.html');
          FCurrentHTMLFile.SaveToFile(strHTMLName);
        Finally
          FCurrentHTMLFile.Free;
        End;
    Finally
      FCurrentModule.Free;
    End;
  Finally
    Stream.Free;
  End;
End;

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

begin
  Result := StringReplace(strText, '<', '&lt;', [rfReplaceAll]);
end;

End.
