(**

  This module contains a class which loads and saves all the application options to an INI file.

  @Author  David Hoyle
  @Version 1.0
  @Date    05 Nov 2017

**)
Unit BADI.Options;

Interface

Uses
  Classes,
  Graphics,
  Generics.Collections,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class to define a set of options for the application. **)
  TBADIOptions = Class
  Strict Private
    Class Var
      (** This is a clas varaiable to hide and hold the BADI Options instance reference. **)
      FBADIOptionsInstance : TBADIOptions;
  Strict Private
    FOptions                : TDocOptions;
    FDefines                : TStringList;
    FSpecialTags            : TList<TBADISpecialTag>;
    FExpandedNodes          : TStringList;
    FINIFileName            : String;
    FUpdateInterval         : Cardinal;
    FScopesToRender         : TScopes;
    FBrowsePosition         : TBrowsePosition;
    FTreeFontName           : String;
    FTreeFontSize           : Integer;
    FFixedFontName          : String;
    FFixedFontSize          : Integer;
    FTokenFontInfo          : Array[Low(TBADITokenType)..High(TBADITokenType)] Of TTokenFontInfo;
    FExcludeDocFiles        : TStringList;
    FMethodDescriptions     : TStringList;
    FScopesToDocument       : TScopes;
    FModuleExplorerBGColour : TColor;
    FTokenLimit             : Integer;
    FMaxDocOutputWidth      : Integer;
    FManagedNodesLife       : Integer;
    FTreeColour             : TColor;
    FProfilingCode          : TStringList;
    FIssueLimits            : Array[Low(TLimitType)..High(TLimitType)] Of Integer;
    FBADIMenuShortCuts      : Array[Low(TBADIMenu)..High(TBADIMenu)] Of String;
    FModuleMetrics          : Array[Low(TBADIModuleMetric)..High(TBADIModuleMetric)] Of TBADIMetricRecord;
    FRefactorConstNewLine   : Boolean;
  Strict Protected
    Function  GetTokenFontInfo(Const ATokenType  : TBADITokenType) : TTokenFontInfo;
    Procedure SetTokenFontInfo(Const ATokenType  : TBADITokenType;
      Const ATokenFontInfo : TTokenFontInfo);
    Function  GetProfilingCode(Const strModuleName: String): String;
    Procedure SetProfilingCode(Const strModuleName: String; Const strValue: String);
    Function  GetIssueLimit(Const LimitType : TLimitType) : Integer;
    Procedure SetIssueLimit(Const LimitType : TLimitType; Const iValue : Integer);
    Function  GetMenuShortcut(Const iBADIMenu : TBADIMenu) : String;
    Procedure SetMenuShortcut(Const iBADIMenu : TBADIMenu; Const strShortcut : String);
    Function  GetModulelMetric(Const ModuleMetric: TBADIModuleMetric): TBADIMetricRecord;
    Procedure SetModuleMetric(Const ModuleMetric: TBADIModuleMetric;
      Const Value: TBADIMetricRecord);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Class Function BADIOptions : TBADIOptions;
    Procedure LoadSettings;
    Procedure SaveSettings;
    (**
      This property contains the basic toggleable options for the application.
      @precon  None.
      @postcon Contains the basic toggleable options for the application.
      @return  a TDocOptions
    **)
    Property Options : TDocOptions Read FOptions Write FOptions;
    (**
      This property provides a list of Compiler Conditional Defines.
      @precon  None.
      @postcon Provides a list of Compiler Conditional Defines.
      @return  a TStringList
    **)
    Property Defines : TStringList Read FDefines;
    (**
      This property provide access to the special tags string list.
      @precon  None.
      @postcon Provide access to the special tags string list.
      @return  a TList<TBADISpecialTag>
    **)
    Property SpecialTags : TList<TBADISpecialTag> Read FSpecialTags;
    (**
      This property provide access to the expanded nodes string list.
      @precon  None.
      @postcon Provide access to the expanded nodes string list.
      @return  a TStringList
    **)
    Property ExpandedNodes : TStringList Read FExpandedNodes;
    (**
      This property determines the amount of time in milliseonds between the
      last editor update and the next refresh. Interval only, the application
      needs to implement the logic.
      @precon  None.
      @postcon Gets and sets the update interval.
      @return  a Cardinal
    **)
    Property UpdateInterval : Cardinal Read FUpdateInterval Write FUpdateInterval;
    (**
      This property determines the scopes to render in the module explorer.
      @precon  None.
      @postcon Gets and sets the scopes to render.
      @return  a TScopes
    **)
    Property ScopesToRender : TScopes Read FScopesToRender Write FScopesToRender;
    (**
      This property determines the behaviour of the positioning of the cursor
      in the editor window.
      @precon  None.
      @postcon Gets and sets the browse position.
      @return  a TBrowsePosition
    **)
    Property BrowsePosition : TBrowsePosition Read FBrowsePosition Write FBrowsePosition;
    (**
      This property determines the Font Name of the Module Explorer Tree Nodes.
      @precon  None.
      @postcon Gets or sets the module explorer tree node font name.
      @return  a String
    **)
    Property TreeFontName : String Read FTreeFontName Write FTreeFontName;
    (**
      This property determines the font size of the module explorer tree node font.
      @precon  None.
      @postcon Gets or sets the module explorer tree node font size.
      @return  an Integer
    **)
    Property TreeFontSize : Integer Read FTreeFontSize Write FTreeFontSize;
    (**
      This property determines the Font Name of the Module Explorer Comment Fixed Fonts.
      @precon  None.
      @postcon Gets or sets the module explorer comment fixed font name.
      @return  a String
    **)
    Property FixedFontName : String Read FFixedFontName Write FFixedFontName;
    (**
      This property determines the font size of the module explorer comment fixed font.
      @precon  None.
      @postcon Gets or sets the module explorer comment fixed font size.
      @return  an Integer
    **)
    Property FixedFontSize : Integer Read FFixedFontSize Write FFixedFontSize;
    (**
      This property determines the colour and style attribute of a token in the
      module explorer
      @precon  None.
      @postcon Gets and sets the colour and style of the token.
      @param   ATokenType as a TBADITokenType as a constant
      @return  a TTokenFontInfo
    **)
    Property TokenFontInfo[Const ATokenType : TBADITokenType] : TTokenFontInfo Read
      GetTokenFontInfo Write SetTokenFontInfo;
    (**
      This properrty holds a list of files / partial or full which should not be
      documented.
      @precon  None.
      @postcon Gets and sets the list.
      @return  a TStringList
    **)
    Property ExcludeDocFiles : TStringList Read FExcludeDocFiles;
    (**
      This property stores a list of method descriptions related to pattern
      matches.
      @precon  None.
      @postcon Gets and sets the method list.
      @return  a TStringList
    **)
    Property MethodDescriptions : TStringList Read FMethodDescriptions;
    (**
      This property determines the scopes to document.
      @precon  None.
      @postcon Gets and sets the scopes to document.
      @return  a TScopes
    **)
    Property ScopesToDocument : TScopes Read FScopesToDocument Write FScopesToDocument;
    (**
      This gets and sets the background colour for the Module explorer.
      @precon  None.
      @postcon Gets and sets the background colour for the Module explorer.
      @return  a TColor
    **)
    Property BGColour : TColor Read FModuleExplorerBGColour Write FModuleExplorerBGColour;
    (**
      This property gets ans sets the token limit to the module explorer.
      @precon  None.
      @postcon Gets ans sets the token limit to the module explorer.
      @return  an Integer
    **)
    Property TokenLimit : Integer Read FTokenLimit Write FTokenLimit;
    (**
      This property gets and sets the maximum width of the code output in
      documentation.
      @precon  None.
      @postcon Gets and sets the maximum width of the code output in
               documentation.
      @return  an Integer
    **)
    Property MaxDocOutputWidth : Integer Read FMaxDocOutputWidth Write FMaxDocOutputWidth;
    (**
      This property gets and sets the period of time in days which managed nodes
      are alive for.
      @precon  None.
      @postcon Gets and sets the period of time in days which managed nodes
               are alive for.
      @return  an Integer
    **)
    Property ManagedNodesLife : Integer Read FManagedNodesLife Write FManagedNodesLife;
    (**
      This property gets and sets the colour of the explorer tree lines.
      @precon  None.
      @postcon Gets and sets the colour of the explorer tree lines.
      @return  a TColor
    **)
    Property TreeColour : TColor Read FTreeColour Write FTreeColour;
    (**
      This property returns the name of the inifile.
      @precon  None.
      @postcon Returns the name of the inifile.
      @return  a String
    **)
    Property INIFileName : String Read FINIFileName;
    (**
      This property gets and sets the profiling code for the given filenames.
      @precon  None.
      @postcon Gets and sets the profiling code for the given filenames.
      @param   strModuleName as a String as a constant
      @return  a String
    **)
    Property ProfilingCode[Const strModuleName : String] : String Read GetProfilingCode
      Write SetProfilingCode;
    (**
      This property gets and sets the numerical limits for the output of errors, warnings,
      hints and conflicts.
      @precon  None.
      @postcon Gets and sets the numerical limits for the output of errors, warnings,
               hints and conflicts.
      @param   LimitType as a TLimitType as a constant
      @return  an Integer
    **)
    Property IssueLimits[Const LimitType : TLimitType] : Integer Read GetIssueLimit
      Write SetIssueLimit;
    (**
      This property provide access to the string representation of the menu shortcuts.
      @precon  None.
      @postcon Gets and sets the string representation of the menu shortcuts.
      @param   BADIMenu as a TBADIMenu as a constant
      @return  a String
    **)
    Property MenuShortcut[Const BADIMenu : TBADIMenu] : String Read GetMenuShortcut
      Write SetMenuShortcut;
    (**
      A property to read and write module metric configuration information.
      @precon  None.
      @postcon Gets and sets the module metric information.
      @param   ModuleMetric as a TBADIModuleMetric as a constant
      @return  a TBADIMetricRecord
    **)
    Property ModuleMetric[Const ModuleMetric : TBADIModuleMetric] : TBADIMetricRecord
      Read GetModulelMetric Write SetModuleMetric;
    (**
      A property to determine of a constant refactoring should have a new line between declaration
      sections.
      @precon  None.
      @postcon Gets or set the boolena value.
      @return  a Boolean
    **)
    Property RefactorConstNewLine : Boolean Read FRefactorConstNewLine Write FRefactorConstNewLine;
  End;

Implementation

Uses
  SysUtils,
  BADI.Constants,
  IniFiles,
  BADI.Module.Dispatcher,
  BADI.Functions;

(**

  This class method returns the singleton instance of the BADI Options class.

  @precon  None.
  @postcon returns the instance of the BADI Options (and creates it if it hasnt alrady been done).

  @return  a TBADIOptions

**)
Class Function TBADIOptions.BADIOptions: TBADIOptions;

Begin
  If Not Assigned(FBADIOptionsInstance) Then
    FBADIOptionsInstance := TBADIOptions.Create;
  Result := FBADIOptionsInstance;
End;

(**

  This is the constructor method for the TBrowseAndDocItOptions class.

  @precon  None.
  @postcon Does nothing at the moment.

**)
Constructor TBADIOptions.Create;

Begin
  Inherited Create;
  FDefines := TStringList.Create;
  FSpecialTags := TList<TBADISpecialTag>.Create;
  // Create a default set of Special Tags.
  FSpecialTags.Add(TBADISpecialTag.Create('todo', 'Things To Do', [tpShowInTree..tpShowInDoc]));
  FSpecialTags.Add(TBADISpecialTag.Create('precon', 'Pre-Conditions', []));
  FSpecialTags.Add(TBADISpecialTag.Create('postcon', 'Post-Conditions', []));
  FSpecialTags.Add(TBADISpecialTag.Create('param', 'Parameters', []));
  FSpecialTags.Add(TBADISpecialTag.Create('return', 'Returns', []));
  FSpecialTags.Add(TBADISpecialTag.Create('note', 'Notes', []));
  FSpecialTags.Add(TBADISpecialTag.Create('see', 'Also See', []));
  FSpecialTags.Add(TBADISpecialTag.Create('exception', 'Exception Raised', []));
  FSpecialTags.Add(TBADISpecialTag.Create('bug', 'Known Bugs', [tpShowInTree..tpShowInDoc]));
  FSpecialTags.Add(TBADISpecialTag.Create('debug', 'Debugging Code', [tpShowInTree..tpShowInDoc]));
  FSpecialTags.Add(TBADISpecialTag.Create('date', 'Date Code Last Updated', []));
  FSpecialTags.Add(TBADISpecialTag.Create('author', 'Code Author', []));
  FSpecialTags.Add(TBADISpecialTag.Create('version', 'Code Version', []));
  FSpecialTags.Add(TBADISpecialTag.Create('refactor', 'Refactorings', [tpShowInTree..tpShowInDoc]));
  FSpecialTags.Add(TBADISpecialTag.Create('code', 'Code Example', [tpShowInTree..tpFixed]));
  FExpandedNodes := TStringList.Create;
  FExpandedNodes.Sorted := True;
  FExpandedNodes.Duplicates := dupIgnore;
  FINIFileName := BuildRootKey;
  FScopesToRender := [scPrivate, scProtected, scPublic, scPublished];
  FExcludeDocFiles := TStringList.Create;
  FMethodDescriptions := TStringList.Create;
  FScopesToDocument := [scPublished, scPublic, scProtected, scPrivate];
  FProfilingCode := TStringList.Create;
End;

(**

  This is the destructor method for the TBrowseAndDocItOptions class.

  @precon  none.
  @postcon Does onthing at the moment except call the inherited destroy method.

**)
Destructor TBADIOptions.Destroy;

Begin
  SaveSettings;
  FProfilingCode.Free;
  FMethodDescriptions.Free;
  FExcludeDocFiles.Free;
  FExpandedNodes.Free;
  FSpecialTags.Free;
  FDefines.Free;
  Inherited Destroy;
  FBADIOptionsInstance := Nil;
End;

(**

  This is a getter method for the IssueLimit property.

  @precon  None.
  @postcon Returns the numerical limit for the given limit type.

  @param   LimitType as a TLimitType as a constant
  @return  an Integer

**)
Function TBADIOptions.GetIssueLimit(Const LimitType: TLimitType): Integer;

Begin
  Result := FIssueLimits[LimitType];
End;

(**

  This is a getter method for the MenuShortcut property.

  @precon  None.
  @postcon Returns the string representation of the enumerated menu shortcut.

  @param   iBADIMenu as a TBADIMenu as a constant
  @return  a String

**)
Function TBADIOptions.GetMenuShortcut(Const iBADIMenu: TBADIMenu): String;

Begin
  Result := FBADIMenuShortCuts[iBADIMenu];
End;

(**

  This is a getter method for the ModuleMetric property.

  @precon  None.
  @postcon Returns the module metric configuration for the given metric.

  @param   ModuleMetric as a TBADIModuleMetric as a constant
  @return  a TBADIMetricRecord

**)
Function TBADIOptions.GetModulelMetric(Const ModuleMetric: TBADIModuleMetric): TBADIMetricRecord;

Begin
  Result := FModuleMetrics[ModuleMetric];
End;

(**

  This is a getter method for the ProfilingCode property.

  @precon  None.
  @postcon Returns the profiling code template for the given filename.

  @param   strModuleName as a String as a constant
  @return  a String

**)
Function TBADIOptions.GetProfilingCode(Const strModuleName : String): String;
var
  iModule: Integer;

Begin
  Result := StringReplace(FProfilingCode.Values[strModuleName], '|', #13#10, [rfReplaceAll]);
  If Result = '' Then
    For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
      If CompareText(TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName, strModuleName) = 0 Then
        Begin
          Result := TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.DefaultProfilingTemplate;
          Break;
        End;
End;

(**


  This is a getter method for the TokenFontInfo property.

  @precon  None.
  @postcon Retursn the record information for the token type.


  @param   ATokenType as a TBADITokenType as a constant
  @return  a TTokenFontInfo

**)
Function TBADIOptions.GetTokenFontInfo(Const ATokenType: TBADITokenType): TTokenFontInfo;
Begin
  Result := FTokenFontInfo[ATokenType];
End;

(**

  This method loads the applications settings from an ini file.

  @precon  None.
  @postcon Loads the applications settings from an ini file.

**)
Procedure TBADIOptions.LoadSettings;

Var
  sl: TStringList;
  i: TDocOption;
  j: Integer;
  iValue: Integer;
  T: TBADITokenType;
  strLine: String;
  iBADIMenu: TBADIMenu;
  iModule: Integer;
  iniFile : TMemIniFile;
  eMetric: TBADIModuleMetric;
  iSTIndex : Integer;
  ST: TBADISpecialTag;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    For i := Low(TDocOption) To High(TDocOption) Do
      If iniFile.ReadBool('Options', DocOptionInfo[i].FDescription, DocOptionInfo[i].FEnabled) Then
        FOptions := FOptions + [i]
      Else
        FOptions := FOptions - [i];
    sl := TStringList.Create;
    Try
      iniFile.ReadSection('SpecialTagNames', sl);
      If sl.Count > 0 Then
        FSpecialTags.Clear;
      For j := 0 To sl.Count - 1 Do
        Begin
          iSTIndex := FSpecialTags.Add(TBADISpecialTag.Create(
            sl[j],
            iniFile.ReadString('SpecialTagNames', sl[j], ''),
            TBADITagProperties(Byte(iniFile.ReadInteger('SpecialTags', sl[j], 0)))
          ));
          ST := FSpecialTags[iSTIndex];
          ST.FFontStyles := TFontStyles(Byte(iniFile.ReadInteger('SpecialTagFontStyles', sl[j], 0)));
          ST.FFontColour :=
            StringToColor(iniFile.ReadString('SpecialTagFontForeColours', sl[j], ColorToString(clNone)));
          ST.FBackColour :=
            StringToColor(iniFile.ReadString('SpecialTagFontBackColours', sl[j], ColorToString(clNone)));
          FSpecialTags[iSTIndex] := ST;
        ENd;
      iniFile.ReadSection('ManagedExpandedNodes', sl);
      For j := 0 To sl.Count - 1 Do
        Begin
          iValue := iniFile.ReadInteger('ManagedExpandedNodes', sl[j], 0);
          FExpandedNodes.AddObject(StringReplace(sl[j], '|', '=', [rfReplaceAll]),
            TObject(iValue));
        End;
    Finally
      sl.Free;
    End;
    FUpdateInterval := iniFile.ReadInteger('ModuleExplorer', 'UpdateInterval', 1000);
    FScopesToRender := TScopes(Byte(iniFile.ReadInteger('ModuleExplorer', 'ScopesToRender',
      Byte(FScopesToRender))));
    FBrowsePosition := TBrowsePosition(iniFile.ReadInteger('Setup', 'BrowsePosition',
      Integer(bpIdentifierCentreShowAllComment)));
    FTreeFontName := iniFile.ReadString('ModuleExplorer', 'Name', 'Tahoma');
    FTreeFontSize := iniFile.ReadInteger('ModuleExplorer', 'Size', 10);
    FFixedFontName := iniFile.ReadString('ModuleExplorer', 'FixedName', 'Courier New');
    FFixedFontSize := iniFile.ReadInteger('ModuleExplorer', 'FixedSize', 10);
    For T := Low(TBADITokenType) To High(TBADITokenType) Do
      Begin
        FTokenFontInfo[T].FForeColour :=
          StringToColor(iniFile.ReadString('TokenFontinfo', Format('%s.Colour', [strTokenType[T]]),
          ColorToString(strTokenTypeInfo[T].FForeColour)));
        FTokenFontInfo[T].FStyles :=
          TFontStyles(Byte(iniFile.ReadInteger('TokenFontinfo', Format('%s.Styles', [strTokenType[T]]),
          Byte(strTokenTypeInfo[T].FStyles))));
        FTokenFontInfo[T].FBackColour :=
          StringToColor(iniFile.ReadString('TokenFontinfo', Format('%s.BackColour', [strTokenType[T]]),
          ColorToString(strTokenTypeInfo[T].FBackColour)));
      End;
    FExcludeDocFiles.Text := StringReplace(iniFile.ReadString('Setup', 'ExcludeDocFiles', ''), '|',
      #13#10, [rfReplaceAll]);
    sl := TStringList.Create;
    Try
      iniFile.ReadSection('MethodDescriptions', sl);
      For j := 0 To sl.Count - 1 Do
        FMethodDescriptions.Add(Format('%s=%s', [sl[j], iniFile.ReadString('MethodDescriptions',
          sl[j], '')]));
    Finally
      sl.Free;
    End;
    FScopesToDocument :=
      TScopes(Byte(iniFile.ReadInteger('Documentation', 'Scopes', Byte(FScopesToDocument))));
    FModuleExplorerBGColour := StringToColor(iniFile.ReadString('ModuleExplorer', 'BGColour',
      ColorToString(clWindow)));
    FTokenLimit := iniFile.ReadInteger('ModuleExplorer', 'TokenLimit', 50);
    FMaxDocOutputWidth := iniFile.ReadInteger('Documentation', 'MaxDocOutputWidth', 80);
    FManagedNodesLife := iniFile.ReadInteger('ModuleExplorer', 'ManagedNodesLife', 90);
    FTreeColour := StringToColor(iniFile.ReadString('ModuleExplorer', 'TreeColour', 'clGray'));
    sl := TStringList.Create;
    Try
      iniFile.ReadSection('ProfilingCode', sl);
      For j := 0 To sl.Count - 1 Do
        Begin
          strLine := iniFile.ReadString('ProfilingCode', sl[j], '');
          If strLine <> '' Then
            FProfilingCode.Values[sl[j]] := strLine;
        End;
    Finally
      sl.Free;
    End;
    FIssueLimits[ltErrors] := iniFile.ReadInteger('Issues Limits', 'Errors', 10);
    FIssueLimits[ltWarnings] := iniFile.ReadInteger('Issues Limits', 'Warnings', 10);
    FIssueLimits[ltHints] := iniFile.ReadInteger('Issues Limits', 'Hints', 10);
    FIssueLimits[ltConflicts] := iniFile.ReadInteger('Issues Limits', 'Conflicts', 10);
    FIssueLimits[ltMetrics] := iniFile.ReadInteger('Issues Limits', 'Metrics', 10);
    For iBADIMenu := Low(TBADImenu) To High(TBADIMenu) Do
      FBADIMenuShortCuts[iBADIMenu] := iniFile.ReadString('BADIMenuShortcuts',
        BADIMenus[iBADIMenu].FName,
        BADIMenus[iBADIMenu].FShortCut);
    For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
      TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions :=
        iniFile.ReadString(
          'ModuleExtensions',
          TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName,
          TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions
        );
    For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
      Begin
        FModuleMetrics[eMetric].FEnabled := iniFile.ReadBool('Module Metrics',
          DefaultModuleMetrics[eMetric].FName + '.Enabled', DefaultModuleMetrics[eMetric].FEnabled);
        FModuleMetrics[eMetric].FLimit := iniFile.ReadFloat('Module Metrics',
          DefaultModuleMetrics[eMetric].FName + '.Limit', DefaultModuleMetrics[eMetric].FLimit);
      End;
    FRefactorConstNewLine := iniFile.ReadBool('Refactorings', 'NewLine', True);
  Finally
    iniFile.Free;
  End;
End;

(**

  This method saves the applications settings to an ini file.

  @precon  None.
  @postcon Saves the applications settings to an ini file.

**)
Procedure TBADIOptions.SaveSettings;

Var
  i: TDocOption;
  j: Integer;
  T: TBADITokenType;
  iBADIMenu : TBADIMenu;
  iModule: Integer;
  iniFile : TMemIniFile;
  eMetric: TBADIModuleMetric;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    For i := Low(TDocOption) To High(TDocOption) Do
      iniFile.WriteBool('Options', DocOptionInfo[i].FDescription, i In FOptions);
    iniFile.EraseSection('SpecialTags');
    iniFile.EraseSection('SpecialTagNames');
    For j := 0 To FSpecialTags.Count - 1 Do
      Begin
        iniFile.WriteInteger('SpecialTags', FSpecialTags[j].FName,
          Byte(FSpecialTags[j].FTagProperties));
        iniFile.WriteString('SpecialTagNames', FSpecialTags[j].FName, FSpecialTags[j].FDescription);
        iniFile.WriteInteger('SpecialTagFontStyles', FSpecialTags[j].FName,
          Byte(FSpecialTags[j].FFontStyles));
        iniFile.WriteString('SpecialTagFontForeColours', FSpecialTags[j].FName,
          ColorToString(FSpecialTags[j].FFontColour));
        iniFile.WriteString('SpecialTagFontBackColours', FSpecialTags[j].FName,
          ColorToString(FSpecialTags[j].FBackColour));
      End;
    iniFile.EraseSection('ManagedExpandedNodes');
    For j := 0 To ExpandedNodes.Count - 1 Do
      iniFile.WriteInteger('ManagedExpandedNodes', StringReplace(FExpandedNodes[j], '=', '|',
        [rfReplaceAll]), Integer(FExpandedNodes.Objects[j]));
    iniFile.WriteInteger('ModuleExplorer', 'UpdateInterval', FUpdateInterval);
    iniFile.WriteInteger('ModuleExplorer', 'ScopesToRender', Byte(FScopesToRender));
    iniFile.WriteInteger('Setup', 'BrowsePosition', Integer(FBrowsePosition));
    iniFile.WriteString('ModuleExplorer', 'Name', FTreeFontName);
    iniFile.WriteInteger('ModuleExplorer', 'Size', FTreeFontSize);
    iniFile.WriteString('ModuleExplorer', 'FixedName', FFixedFontName);
    iniFile.WriteInteger('ModuleExplorer', 'FixedSize', FFixedFontSize);
    For T := Low(TBADITokenType) To High(TBADITokenType) Do
      Begin
        iniFile.WriteString('TokenFontinfo', Format('%s.Colour', [strTokenType[T]]),
          ColorToString(FTokenFontInfo[T].FForeColour));
        iniFile.WriteInteger('TokenFontinfo', Format('%s.Styles', [strTokenType[T]]),
          Byte(FTokenFontInfo[T].FStyles));
        iniFile.WriteString('TokenFontinfo', Format('%s.BackColour', [strTokenType[T]]),
          ColorToString(FTokenFontInfo[T].FBackColour));
      End;
    iniFile.WriteString('Setup', 'ExcludeDocFiles', StringReplace(FExcludeDocFiles.Text, #13#10, '|',
      [rfReplaceAll]));
    iniFile.EraseSection('MethodDescriptions');
    For j := 0 To FMethodDescriptions.Count - 1 Do
      iniFile.WriteString('MethodDescriptions', FMethodDescriptions.Names[j],
        FMethodDescriptions.ValueFromIndex[j]);
    iniFile.WriteInteger('Documentation', 'Scopes', Byte(FScopesToDocument));
    iniFile.WriteString('ModuleExplorer', 'BGColour', ColorToString(FModuleExplorerBGColour));
    iniFile.WriteInteger('ModuleExplorer', 'TokenLimit', FTokenLimit);
    iniFile.WriteInteger('Documentation', 'MaxDocOutputWidth', FMaxDocOutputWidth);
    iniFile.WriteInteger('ModuleExplorer', 'ManagedNodesLife', FManagedNodesLife);
    iniFile.WriteString('ModuleExplorer', 'TreeColour', ColorToString(FTreeColour));
    iniFile.EraseSection('ProfilingCode');
    For j := 0 To FProfilingCode.Count - 1 Do
      If FProfilingCode.Names[j] <> '' Then
        iniFile.WriteString('ProfilingCode', FProfilingCode.Names[j], FProfilingCode.ValueFromIndex[j]);
    iniFile.WriteInteger('Issues Limits', 'Errors', FIssueLimits[ltErrors]);
    iniFile.WriteInteger('Issues Limits', 'Warnings', FIssueLimits[ltWarnings]);
    iniFile.WriteInteger('Issues Limits', 'Hints', FIssueLimits[ltHints]);
    iniFile.WriteInteger('Issues Limits', 'Conflicts', FIssueLimits[ltConflicts]);
    iniFile.WriteInteger('Issues Limits', 'Metrics', FIssueLimits[ltMetrics]);
    For iBADIMenu := Low(TBADImenu) To High(TBADIMenu) Do
      iniFile.WriteString('BADIMenuShortcuts',
        BADIMenus[iBADIMenu].FName,
        FBADIMenuShortCuts[iBADIMenu]);
    For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
      iniFile.WriteString(
        'ModuleExtensions',
        TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName,
        TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions
      );
    For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
      Begin
        iniFile.WriteBool('Module Metrics', DefaultModuleMetrics[eMetric].FName + '.Enabled',
          FModuleMetrics[eMetric].FEnabled);
        iniFile.WriteFloat('Module Metrics', DefaultModuleMetrics[eMetric].FName + '.Limit',
          FModuleMetrics[eMetric].FLimit);
      End;
    iniFile.WriteBool('Refactorings', 'NewLine', FRefactorConstNewLine);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This is a setter method for the IssueLimit property.

  @precon  None.
  @postcon Sets the numerical limit for the given limit type.

  @param   LimitType as a TLimitType as a constant
  @param   iValue as an Integer as a constant

**)
Procedure TBADIOptions.SetIssueLimit(Const LimitType: TLimitType; Const iValue: Integer);

Begin
  FIssueLimits[LimitType] := iValue;
End;

(**

  This is a setter method for the MenuShortcut property.

  @precon  None.
  @postcon Sets the string representation of the enumerated menu shortcut.

  @param   iBADIMenu   as a TBADIMenu as a constant
  @param   strShortcut as a String as a constant

**)
Procedure TBADIOptions.SetMenuShortcut(Const iBADIMenu: TBADIMenu;
  Const strShortcut: String);

Begin
  If FBADIMenuShortCuts[iBADIMenu] <> strShortcut Then
    FBADIMenuShortCuts[iBADIMenu] := strShortcut;
End;

(**

  This is a setter method for the ModuleMetric property.

  @precon  None.
  @postcon Sets the module metric configuration.

  @param   ModuleMetric as a TBADIModuleMetric as a constant
  @param   Value        as a TBADIMetricRecord as a constant

**)
Procedure TBADIOptions.SetModuleMetric(Const ModuleMetric: TBADIModuleMetric;
  Const Value: TBADIMetricRecord);

Begin
  FModuleMetrics[ModuleMetric] := Value;
End;

(**

  This is a setter method for the ProfilingCode property.

  @precon  None.
  @postcon saves the profiling code for the given filename.

  @param   strModuleName as a String as a constant
  @param   strValue      as a String as a constant

**)
Procedure TBADIOptions.SetProfilingCode(Const strModuleName : String; Const strValue: String);

Begin
  FProfilingCode.Values[strModuleName] := StringReplace(strValue, #13#10, '|', [rfReplaceAll]);
End;

(**


  This is a setter method for the TokenFontInfo property.

  @precon  None.
  @postcon Sets the indexed Token Font Information record.


  @param   ATokenType     as a TBADITokenType as a constant
  @param   ATokenFontInfo as a TTokenFontInfo as a constant

**)
Procedure TBADIOptions.SetTokenFontInfo(Const ATokenType: TBADITokenType;
  Const ATokenFontInfo: TTokenFontInfo);

Begin
  FTokenFontInfo[ATokenType] := ATokenFontInfo;
End;

End.
