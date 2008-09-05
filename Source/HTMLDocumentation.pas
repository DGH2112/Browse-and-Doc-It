(**

  This module contains a documentation engine for producing static HTML
  information.

  @Author  David Hoyle
  @Date    29 Aug 2008
  @Version 1.0

  @todo    Add Summary file with Doc Cons, Errors, Hints and warning table.
  @todo    This should treat "special" tags like the module explorer and
           put them at the top of the file.

**)
Unit HTMLDocumentation;

Interface

Uses
  Classes, BaseLanguageModule, BaseDocumentation, Contnrs;

Type
  (** This is a class to produce HTML documentation. **)
  THTMLDocumentation = Class(TBaseDocumentation)
  Private
    FFirstDocument: String;
    FCurrentModule   : TBaseLanguageModule;
    FCurrentHTMLFile : TStringList;
    FProgressIndex: Integer;
    FSpecialTagNodes: TObjectList;
    procedure GenerateImages(strImageDirectory: String);
  Protected
    Procedure OutputHTMLDocumentation(strFileName : String);
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
    Procedure OutputComment(slContents : TStringList; E : TElementContainer);
    Procedure OutputContainers(slContents : TStringList; Container : TElementContainer);
    Function N(strText : String) : String;
    { HTML Tag Outputs }
    Function A(strText, strHREF : String; strName : String = '') : String;
    Function H(strText : String; iLevel : Integer; AImage : TImageIndex) : String;
    Function IMG(AImageIndex : TImageIndex) : String;
    Function LI(strText : String) : String;
  Public
    Constructor Create(strOutputDirectory : String); Override;
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

**)
constructor THTMLDocumentation.Create(strOutputDirectory: String);

Var
  i : Integer;

begin
  Inherited Create(strOutputDirectory);
  FSpecialTagNodes := TObjectList.Create(true);
  For i := 1 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
    FSpecialTagNodes.Add(TStringList.Create);
end;

(**

  This is a destructor for the THTMLDocumentation class.

  @precon  None.
  @postcon Frees the Special Tags string lists.

**)
destructor THTMLDocumentation.Destroy;
begin
  FSpecialTagNodes.Free;
  Inherited Destroy;
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

Procedure THTMLDocumentation.GenerateContent;

Var
  iIns : Integer;
  i : Integer;
  strIndent : String;
  slContents : TStringList;

Begin
  iIns := FindInsertionPoint('*$CONTENT$');
  i := Pos('$', FCurrentHTMLFile[iIns]);
  strIndent := StringOfChar(#32, i - 1);
  slContents := TStringList.Create;
  Try
    slContents.Add(H(A(FCurrentModule.ModuleName, '', 'ModuleOverview'), 1,
      FCurrentModule.ImageIndex));
    OutputComment(slContents, FCurrentModule);
    GenerateErrorsWarningsHints(slContents);
    GenerateDocumentConflicts(slContents);
    GenerateSpecialTags(slContents);
    For i := 1 To FCurrentModule.ElementCount Do
      OutputContainers(slContents, FCurrentModule[i]);
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

procedure THTMLDocumentation.GenerateDocumentConflicts(slContents : TStringList);

Var
  i, j : Integer;
  E : TElementContainer;

begin
  E := FCurrentModule.FindElement(strDocumentationConflicts);
  If E = Nil Then
    Exit;
  slContents.Add(H(A(strDocumentationConflicts, '', strDocumentationConflicts), 2,
    E.ImageIndex));
  For i := 1 To E.ElementCount Do
    Begin
      slContents.Add(Format('<!-- %s -->', [strDocumentationConflicts]));
      slContents.Add(H(E[i].AsString, 3, E[i].ImageIndex));
      slContents.Add('<ul>');
      For j := 1 To E[i].ElementCount Do
        slContents.Add(#32#32 + LI(IMG(E[i][j].ImageIndex) + #32 + N(E[i][j].AsString)));
      slContents.Add('</ul>');
      slContents.Add('');
    End;
end;

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
      slContents.Add(H(A(strSections[i], '', strSections[i]), 2, E.ImageIndex));
      slContents.Add('<ul>');
      For j := 1 To E.ElementCount Do
        slContents.Add(#32#32 + LI(IMG(E[j].ImageIndex) + #32 + N(E[j].AsString)));
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
    sl.Add(strIndent + '  <div class="Text">Modules</div>');
    sl.Add(strIndent + Format('  <div class="Module">%s %s</div>',
      [IMG(iiModule), A('Summary', 'Summary.html')]));
    For i := 0 To FFileNames.Count - 1 Do
      Begin
        strFileName := ExtractFileName(FFileNames[i]);
        sl.Add(strIndent + Format('  <div class="Module">%s %s</div>',
          [IMG(iiModule), A(ExtractFileName(strFileName),
          ChangeFileExt(strFileName, '.html'))]));
      End;
    sl.Add(strIndent + Format('  <div class="Module">%s %s</div>',
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
    sl.Add(strIndent + Format('<div class="Section">%s %s</div>', [
      IMG(iiModule), A('Module Overview', '#ModuleOverview')]));
    For i := 1 To FCurrentModule.ElementCount Do
      Begin
        E := FCurrentModule.Elements[i];
        sl.Add(strIndent + Format('<div class="Section">%s %s</div>', [
          IMG(E.ImageIndex), A(E.AsString, '#' + E.AsString)]));
      End;
    FCurrentHTMLFile[iIns] := sl.Text;
  Finally
    sl.Free;
  End;
End;

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
      slContents.Add(H(A(strSection, '', strSection), 2, iiToDoFolder));
      slContents.Add('<ul>');
      For j := 0 To sl.Count - 1 Do
        slContents.Add(#32#32 + LI(IMG(iiToDoItem) + #32 + N(sl[j])));
      slContents.Add('</ul>');
      slContents.Add('');
    End;
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
  Result := FFirstDocument;
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
  Result := Format('<h%d>%s %s</h%d>', [iLevel, IMG(AImage), strText, iLevel]);
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

  @param   strText as a String
  @return  a String

**)
function THTMLDocumentation.LI(strText: String): String;
begin
  Result := Format('<li>%s</li>', [strText]);
end;

(**

  This method outputs the comment text along with the tag information.

  @precon  None.
  @postcon Outputs the comment text along with the tag information.

  @param   slContents as a TStringList
  @param   E          as a TElementContainer

**)
procedure THTMLDocumentation.OutputComment(slContents : TStringList;
  E: TElementContainer);

var
  i: Integer;

begin
  If (E = Nil) Or (E.Comment = Nil) Then
    Exit;
  slContents.Add(Format('<p class="Comment">%s</p>', [E.Comment.AsString(0, 999999, True)]));
  If E.Comment.TagCount = 0 Then
    Exit;
  slContents.Add('<table class="Tags">');
  slContents.Add('  <tbody>');
  For i := 0 To E.Comment.TagCount - 1 Do
    Begin
      slContents.Add('    <tr>');
      slContents.Add(Format('      <td class="TagName">%s</td>', [E.Comment.Tag[i].TagName]));
      slContents.Add(Format('      <td>%s</td>', [E.Comment.Tag[i].AsString(True)]));
      slContents.Add('    </tr>');
    End;
  slContents.Add('  </tbody>');
  slContents.Add('</table>');
end;

procedure THTMLDocumentation.OutputContainers(slContents: TStringList;
  Container: TElementContainer);

Const
  strSections : Array[1..4] Of String = (strDocumentationConflicts, strErrors,
    strHints, strWarnings);
var
  i: Integer;

begin
  For i := Low(strSections) To High(strSections) Do
    If AnsiCompareText(Container.AsString, strSections[i]) = 0 Then
      Exit;
  slContents.Add(H(A(Container.AsString, '', Container.AsString), 2,
    Container.ImageIndex));
  slContents.Add('<table>');
  For i := 1 To Container.ElementCount Do
    Begin
      slContents.Add('  <tr>');
      slContents.Add(Format('    <td>%s %s</td>', [IMG(Container[i].ImageIndex),
        N(Container[i].AsString)]));
      slContents.Add('    <td>');
      OutputComment(slContents, Container[i]);
      slContents.Add('    </td>');
      slContents.Add('  </tr>');
    End;
  slContents.Add('</table>');
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
  Initialise(3 + Integer(High(TImageIndex)) + FFileNames.Count, 'HTML Documentation',
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

Var
  Stream : TFileStream;
  strHTMLName : String;

Begin
  Stream := TFileStream.Create(strFileName, fmOpenRead Or fmShareDenyNone);
  Try
    FCurrentModule := Dispatcher(Stream, strFileName, False, [moParse,
      moCheckForDocumentConflicts]);
    If FCurrentModule <> Nil Then
      Try
        FCurrentHTMLFile := TStringList.Create;
        Try
          GenerateHTML(ExtractFileName(strFileName));
          GenerateModuleList;
          GenerateSectionList;
          GenerateContent;
          strHTMLName := FOutputDirectory + ChangeFileExt(ExtractFileName(strFileName), '.html');
          If FFirstDocument = '' Then
            FFirstDocument := strHTMLName;
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
