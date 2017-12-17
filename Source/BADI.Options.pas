(**

  This module contains a class which loads and saves all the application options to an INI file.

  @Author  David Hoyle
  @Version 1.0
  @Date    16 Dec 2017

**)
Unit BADI.Options;

Interface

Uses
  Classes,
  Graphics,
  Generics.Collections,
  IniFiles,
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
    FModuleChecks           : Array[Low(TBADIModuleCheck)..High(TBADIModuleCheck)] Of TBADICheckRecord;
    FLowMetricMargin        : Double;
    FHighMetricMargin       : Double;
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
    Function  GetModulelMetric(Const eModuleMetric: TBADIModuleMetric): TBADIMetricRecord;
    Procedure SetModuleMetric(Const eModuleMetric: TBADIModuleMetric;
      Const recValue: TBADIMetricRecord);
    Function  GetModulelCheck(Const eModuleCheck: TBADIModuleCheck): TBADICheckRecord;
    Procedure SetModuleCheck(Const eModuleCheck: TBADIModuleCheck;
      Const recValue: TBADICheckRecord);
    Procedure LoadDocOptions(Const iniFile: TMemIniFile);
    Procedure LoadSpecialTags(Const iniFile: TMemIniFile);
    Procedure LoadManagedNodes(Const iniFile: TMemIniFile);
    Procedure LoadModuleExplorerOptions(Const iniFile: TMemIniFile);
    Procedure LoadMethodDescriptions(Const iniFile: TMemIniFile);
    Procedure LoadProfilingOptions(Const iniFile: TMemIniFile);
    Procedure LoadLimits(Const iniFile: TMemIniFile);
    Procedure LoadShortcuts(Const iniFile: TMemIniFile);
    Procedure LoadExtensions(Const iniFile: TMemIniFile);
    Procedure LoadMetrics(Const iniFile: TMemIniFile);
    Procedure LoadChecks(Const iniFile: TMemIniFile);
    Procedure SaveDocOptions(Const iniFile: TMemIniFile);
    Procedure SaveSpecialTags(Const iniFile: TMemIniFile);
    Procedure SaveManagedNodes(Const iniFile: TMemIniFile);
    Procedure SaveModuleExplorerOptions(Const iniFile: TMemIniFile);
    Procedure SaveMethodDescrpitions(Const iniFile: TMemIniFile);
    Procedure SaveProfilingOptions(Const iniFile: TMemIniFile);
    Procedure SaveLimits(Const iniFile: TMemIniFile);
    Procedure SaveShortcuts(Const iniFile: TMemIniFile);
    Procedure SaveExtensions(Const iniFile: TMemIniFile);
    Procedure SaveMetrics(Const iniFile: TMemIniFile);
    Procedure SaveChecks(Const iniFile: TMemIniFile);
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
      @param   eModuleMetric as a TBADIModuleMetric as a constant
      @return  a TBADIMetricRecord
    **)
    Property ModuleMetric[Const eModuleMetric : TBADIModuleMetric] : TBADIMetricRecord
      Read GetModulelMetric Write SetModuleMetric;
    (**
      A property to read and write module checks configuration information.
      @precon  None.
      @postcon Gets and sets the module check information.
      @param   eModuleCheck as a TBADIModuleCheck as a constant
      @return  a TBADICheckRecord
    **)
    Property ModuleCheck[Const eModuleCheck : TBADIModuleCheck] : TBADICheckRecord
      Read GetModulelCheck Write SetModuleCheck;
    (**
      A property to define the lower limit margin for metrics in the metrics views.
      @precon  None.
      @postcon Returns the lower margin limit for a metric (a percentage).
      @return  a Double
    **)
    Property LowMetricMargin : Double Read FLowMetricMargin Write FLowMetricMargin;
    (**
      A property to define the upper limit margin for metrics in the metrics views.
      @precon  None.
      @postcon Returns the upper margin limit for a metric (a percentage).
      @return  a Double
    **)
    Property HighMetricMargin : Double Read FHighMetricMargin Write FHighMetricMargin;
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
  BADI.Module.Dispatcher,
  BADI.Functions;

Const
  (** An INI section name for the special tag names. **)
  strSpecialTagNames = 'SpecialTagNames';
  (** An INI section name for the special tag informtion. **)
  strSpecialTags = 'SpecialTags';
  (** An INI key for special tag font styles. **)
  strSpecialTagFontStyles = 'SpecialTagFontStyles';
  (** An INI key for special tag font fore colours. **)
  strSpecialTagFontForeColours = 'SpecialTagFontForeColours';
  (** An INI key for special tag font back colours. **)
  strSpecialTagFontBackColours = 'SpecialTagFontBackColours';
  (** An ini section name for the managed nodes. **)
  strManagedExpandedNodes = 'ManagedExpandedNodes';
  (** An ini section name for the documentation options. **)
  strDocOptions = 'Options';
  (** An ini section name for the modules options. **)
  strModuleExplorer = 'ModuleExplorer';
  (** An ini key for the update interval **)
  strUpdateInterval = 'UpdateInterval';
  (** An ini key for the Scopes to render in the explorer **)
  strScopesToRender = 'ScopesToRender';
  (** An ini section name for general settings. **)
  strSetup = 'Setup';
  (** An ini key for the browse position **)
  strBrowsePosition = 'BrowsePosition';
  (** An ini key for the proportional font name **)
  strFontName = 'Name';
  (** An ini key for the proportional font size **)
  strFontSize = 'Size';
  (** An ini key for the fixed font name **)
  strFixedFontName = 'FixedName';
  (** An ini key for the fixed font size **)
  strFixedFontSize = 'FixedSize';
  (** An ini key for the token font information **)
  strTokenFontInfo = 'TokenFontInfo';
  (** An ini key for the token font colour **)
  strFontColour = '%s.Colour';
  (** An ini key for the token font style **)
  strFontStyles = '%s.Styles';
  (** An ini key for the token font back colour **)
  strFontBackColour = '%s.BackColour';
  (** An ini section name for the method descriptions. **)
  strMethodDescriptions = 'MethodDescriptions';
  (** An ini section name for the excluded documentation files. **)
  strExcludeDocFiles = 'ExcludeDocFiles';
  (**  An ini section name for documentation options. **)
  strDocumentation = 'Documentation';
  (** An ini key for the module explorer background colour **)
  strBGColour = 'BGColour';
  (** An ini key for token limit **)
  strTokenLimit = 'TokenLimit';
  (** An ini key for max documentation output width **)
  strMaxDocOutputWidth = 'MaxDocOutputWidth';
  (** An ini key for managed node life in days **)
  strManagedNodesLife = 'ManagedNodesLife';
  (** An ini key for explorer tree colour. **)
  strTreeColour = 'TreeColour';
  (** An ini key for scopes **)
  strScopes = 'Scopes';
  (** An ini section name for the profiling options **)
  strProfilingCode = 'ProfilingCode';
  (** An ini key for issue limits **)
  strIssuesLimits = 'Issues Limits';
  (** An ini key for errors **)
  strErrors = 'Errors';
  (** An ini key for warnings **)
  strWarnings = 'Warnings';
  (** An ini key for hints **)
  strHints = 'Hints';
  (** An ini key for conflicts **)
  strConflicts = 'Conflicts';
  (** An ini key for metrics **)
  strMetrics = 'Metrics';
  (** An ini section name for the module metrics **)
  strModuleMetrics = 'Module Metrics';
  (** An ini section name for the module checks **)
  strModuleChecks = 'Module Checks';
  (** An ini key for enabled metrics **)
  strEnabled = '.Enabled';
  (** An ini key for metrics limits **)
  strLimit = '.Limit';
  (** An ini section name for the metrics margins **)
  strMetricMargins = 'MetricMargins';
  (** An ini key for the low metric margin. **)
  strLowMargin = 'LowMargin';
  (** An ini key for the high metric margin. **)
  strHighMargin = 'HighMargin';
  (** An ini section name for the module extensions **)
  strModuleExtensions = 'ModuleExtensions';
  (** An ini section name for the refactorings **)
  strRefactorings = 'Refactorings';
  (** An ini key for new lines in refactoring. **)
  strNewLine = 'NewLine';
  (** An ini section name for the shortcuts **)
  strBADIMenuShortcuts = 'BADIMenuShortcuts';

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

Type
  TDefaultSpecialTag = Record
    FTagName : String;
    FTagDesc : String;
    FTagOps  : TBADITagProperties;
  End;
  
Const
  DefaultSpecialTags : Array[0..14] Of TDefaultSpecialTag = (
    (FTagName: 'todo';      FTagDesc: 'Things To Do';           FTagOps: [tpShowInTree..tpShowInDoc]),
    (FTagName: 'precon';    FTagDesc: 'Pre-Conditions';         FTagOps: []),
    (FTagName: 'postcon';   FTagDesc: 'Post-Conditions';        FTagOps: []),
    (FTagName: 'param';     FTagDesc: 'Parameters';             FTagOps: []),
    (FTagName: 'return';    FTagDesc: 'Returns';                FTagOps: []),
    (FTagName: 'note';      FTagDesc: 'Notes';                  FTagOps: []),
    (FTagName: 'see';       FTagDesc: 'Also See';               FTagOps: []),
    (FTagName: 'exception'; FTagDesc: 'Exception Raised';       FTagOps: []),
    (FTagName: 'bug';       FTagDesc: 'Known Bugs';             FTagOps: [tpShowInTree..tpShowInDoc]),
    (FTagName: 'debug';     FTagDesc: 'Debugging Code';         FTagOps: [tpShowInTree..tpShowInDoc]),
    (FTagName: 'date';      FTagDesc: 'Date Code Last Updated'; FTagOps: []),
    (FTagName: 'author';    FTagDesc: 'Code Author';            FTagOps: []),
    (FTagName: 'version';   FTagDesc: 'Code Version';           FTagOps: []),
    (FTagName: 'refactor';  FTagDesc: 'Refactorings';           FTagOps: [tpShowInTree..tpShowInDoc]),
    (FTagName: 'code';      FTagDesc: 'Code Example';           FTagOps: [tpShowInTree..tpFixed])
  );
var
  iTag: Integer;

Begin
  Inherited Create;
  FDefines := TStringList.Create;
  FSpecialTags := TList<TBADISpecialTag>.Create;
  // Create a default set of Special Tags.
  For iTag := Low(DefaultSpecialTags) To High(DefaultSpecialTags) Do
    FSpecialTags.Add(
      TBADISpecialTag.Create(
        DefaultSpecialTags[iTag].FTagName,
        DefaultSpecialTags[iTag].FTagDesc,
        DefaultSpecialTags[iTag].FTagOps)
      );
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

  This is a getter method for the ModuleCheck property.

  @precon  None.
  @postcon Returns the module check configuration for the given metric.

  @param   eModuleCheck as a TBADIModuleCheck as a constant
  @return  a TBADICheckRecord

**)
Function TBADIOptions.GetModulelCheck(Const eModuleCheck: TBADIModuleCheck): TBADICheckRecord;

Begin
  Result := FModuleChecks[eModuleCheck];
End;

(**

  This is a getter method for the ModuleMetric property.

  @precon  None.
  @postcon Returns the module metric configuration for the given metric.

  @param   eModuleMetric as a TBADIModuleMetric as a constant
  @return  a TBADIMetricRecord

**)
Function TBADIOptions.GetModulelMetric(Const eModuleMetric: TBADIModuleMetric): TBADIMetricRecord;

Begin
  Result := FModuleMetrics[eModuleMetric];
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

  This method loads the module checks from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The checks are loaded fromt the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadChecks(Const iniFile: TMemIniFile);

Var
  eCheck: TBADIModuleCheck;

Begin
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    Begin
      FModuleChecks[eCheck].FEnabled := iniFile.ReadBool(strModuleChecks,
        ModuleChecks[eCheck].FName + strEnabled, ModuleChecks[eCheck].FEnabled);
      FModuleChecks[eCheck].FLimit := iniFile.ReadFloat(strModuleChecks,
        ModuleChecks[eCheck].FName + strLimit, ModuleChecks[eCheck].FLimit);
    End;
End;

(**

  This method loads the documentation options.

  @precon  iniFile must be a valid instance.
  @postcon The documentation options are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadDocOptions(Const iniFile: TMemIniFile);

Var
  i: TDocOption;

Begin
  For i := Low(TDocOption) To High(TDocOption) Do
    If iniFile.ReadBool(strDocOptions, DocOptionInfo[i].FDescription, DocOptionInfo[i].FEnabled) Then
      FOptions := FOptions + [i]
    Else
      FOptions := FOptions - [i];
End;

(**

  This method loads the file extensions from the INI file that are associated with the parsers.

  @precon  iniFile must be a valid instance.
  @postcon The exteniions are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadExtensions(Const iniFile: TMemIniFile);

Var
  iModule: Integer;

Begin
  For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
    TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions := iniFile.ReadString(strModuleExtensions,
      TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName,
      TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions);
End;

(**

  This method loads the limit information from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The lmit imformation is loaded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadLimits(Const iniFile: TMemIniFile);

Const
  iDefaultLimit = 10;

Begin
  FIssueLimits[ltErrors] := iniFile.ReadInteger(strIssuesLimits, strErrors, iDefaultLimit);
  FIssueLimits[ltWarnings] := iniFile.ReadInteger(strIssuesLimits, strWarnings, iDefaultLimit);
  FIssueLimits[ltHints] := iniFile.ReadInteger(strIssuesLimits, strHints, iDefaultLimit);
  FIssueLimits[ltConflicts] := iniFile.ReadInteger(strIssuesLimits, strConflicts, iDefaultLimit);
  FIssueLimits[ltMetrics] := iniFile.ReadInteger(strIssuesLimits, strMetrics, iDefaultLimit);
End;

(**

  This method loads the managed nodes from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The managed nodes are loaded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadManagedNodes(Const iniFile: TMemIniFile);

Var
  iValue: Integer;
  iNode: Integer;
  sl : TStringList;

Begin
  sl := TStringList.Create;
  Try
    iniFile.ReadSection(strManagedExpandedNodes, sl);
    For iNode := 0 To sl.Count - 1 Do
      Begin
        iValue := iniFile.ReadInteger(strManagedExpandedNodes, sl[iNode], 0);
        FExpandedNodes.AddObject(StringReplace(sl[iNode], '|', '=', [rfReplaceAll]), TObject(iValue));
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the method descriptions from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The method descrpitions are loaded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadMethodDescriptions(Const iniFile: TMemIniFile);

Var
  iMethod: Integer;
  sl : TStringList;

Begin
  sl := TStringList.Create;
  Try
    iniFile.ReadSection(strMethodDescriptions, sl);
    For iMethod := 0 To sl.Count - 1 Do
      FMethodDescriptions.Add(Format('%s=%s', [sl[iMethod], iniFile.ReadString(strMethodDescriptions,
        sl[iMethod], '')]));
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the module metrics from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The metrics are loaded fromt the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadMetrics(Const iniFile: TMemIniFile);

Const
  iDefaultLowLimit = 95;
  iDefaultHighLimit = 105;

Var
  eMetric: TBADIModuleMetric;

Begin
  For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
    Begin
      FModuleMetrics[eMetric].FEnabled := iniFile.ReadBool(strModuleMetrics,
        ModuleMetrics[eMetric].FName + strEnabled, ModuleMetrics[eMetric].FEnabled);
      FModuleMetrics[eMetric].FLimit := iniFile.ReadFloat(strModuleMetrics,
        ModuleMetrics[eMetric].FName + strLimit, ModuleMetrics[eMetric].FLimit);
    End;
  FLowMetricMargin := iniFile.ReadInteger(strMetricMargins, strLowMargin, iDefaultLowLimit);
  FHighMetricMargin := iniFile.ReadInteger(strMetricMargins, strHighMargin, iDefaultHighLimit);
End;

(**

  This method loads the module explorer options from the INI file.

  @precon  iniFIle must be a valid instance.
  @postcon The module explorer options are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadModuleExplorerOptions(Const iniFile: TMemIniFile);

Const
  iDefaultUpdateInterval = 1000;
  iDefaultFontSize = 10;
  iDefaultTokenLimit = 50;
  iDefaultMaxDocumentationWidth = 80;
  iDefaultNodeLifeInDays = 90;
  strDefaultProportionalFontName = 'Tahoma';
  strDefaultFixedFontName = 'Courier New';
  strFDefaultTreeColour = 'clGray';

Var
  T: TBADITokenType;

Begin
  FUpdateInterval := iniFile.ReadInteger(strModuleExplorer, strUpdateInterval, iDefaultUpdateInterval);
  FScopesToRender := TScopes(Byte(iniFile.ReadInteger(strModuleExplorer, strScopesToRender,
    Byte(FScopesToRender))));
  FBrowsePosition := TBrowsePosition(iniFile.ReadInteger(strSetup, strBrowsePosition,
    Integer(bpIdentifierCentreShowAllComment)));
  FTreeFontName := iniFile.ReadString(strModuleExplorer, strFontName, strDefaultProportionalFontName);
  FTreeFontSize := iniFile.ReadInteger(strModuleExplorer, strFontSize, iDefaultFontSize);
  FFixedFontName := iniFile.ReadString(strModuleExplorer, strFixedFontName, strDefaultFixedFontName);
  FFixedFontSize := iniFile.ReadInteger(strModuleExplorer, strFixedFontSize, iDefaultFontSize);
  For T := Low(TBADITokenType) To High(TBADITokenType) Do
    Begin
      FTokenFontInfo[T].FForeColour :=
        StringToColor(iniFile.ReadString(strTokenFontInfo, Format(strFontColour, [strTokenType[T]]),
        ColorToString(strTokenTypeInfo[T].FForeColour)));
      FTokenFontInfo[T].FStyles :=
        TFontStyles(Byte(iniFile.ReadInteger(strTokenFontInfo, Format(strFontStyles, [strTokenType[T]]),
        Byte(strTokenTypeInfo[T].FStyles))));
      FTokenFontInfo[T].FBackColour :=
        StringToColor(iniFile.ReadString(strTokenFontInfo, Format(strFontBackColour, [strTokenType[T]]),
        ColorToString(strTokenTypeInfo[T].FBackColour)));
    End;
  FModuleExplorerBGColour := StringToColor(iniFile.ReadString(strModuleExplorer, strBGColour,
    ColorToString(clWindow)));
  FTokenLimit := iniFile.ReadInteger(strModuleExplorer, strTokenLimit, iDefaultTokenLimit);
  FMaxDocOutputWidth := iniFile.ReadInteger(strDocumentation, strMaxDocOutputWidth, iDefaultMaxDocumentationWidth);
  FManagedNodesLife := iniFile.ReadInteger(strModuleExplorer, strManagedNodesLife, iDefaultNodeLifeInDays);
  FTreeColour := StringToColor(iniFile.ReadString(strModuleExplorer, strTreeColour, strFDefaultTreeColour));
End;

(**

  This method loads the profiling options from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The profiling options are laoded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadProfilingOptions(Const iniFile: TMemIniFile);

Var
  sl: TStringList;
  j: Integer;
  strLine: String;

Begin
  sl := TStringList.Create;
  Try
    iniFile.ReadSection(strProfilingCode, sl);
    For j := 0 To sl.Count - 1 Do
      Begin
        strLine := iniFile.ReadString(strProfilingCode, sl[j], '');
        If strLine <> '' Then
          FProfilingCode.Values[sl[j]] := strLine;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the applications settings from an ini file.

  @precon  None.
  @postcon Loads the applications settings from an ini file.

**)
Procedure TBADIOptions.LoadSettings;

Var
  iniFile : TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    LoadDocOptions(iniFile);
    LoadSpecialTags(iniFile);
    LoadManagedNodes(iniFile);
    LoadModuleExplorerOptions(iniFile);
    FExcludeDocFiles.Text := StringReplace(iniFile.ReadString(strSetup, strExcludeDocFiles, ''), '|',
      #13#10, [rfReplaceAll]);
    LoadMethodDescriptions(iniFile);
    FScopesToDocument :=
      TScopes(Byte(iniFile.ReadInteger(strDocumentation, strScopes, Byte(FScopesToDocument))));
    LoadProfilingOptions(iniFile);
    LoadLimits(iniFile);
    LoadShortcuts(iniFile);
    LoadExtensions(iniFile);
    LoadMetrics(iniFile);
    LoadChecks(iniFile);
    FRefactorConstNewLine := iniFile.ReadBool(strRefactorings, strNewLine, True);
  Finally
    iniFile.Free;
  End;
End;

(**

  This method loads the shortcuts for the menu options from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The shoirtcuts are loaded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadShortcuts(Const iniFile: TMemIniFile);

Var
  iBADIMenu: TBADIMenu;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    FBADIMenuShortCuts[iBADIMenu] := iniFile.ReadString(strBADIMenuShortcuts, BADIMenus[iBADIMenu].FName,
      BADIMenus[iBADIMenu].FShortCut);
End;

(**

  This method loads ther special tag information from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The special tags are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadSpecialTags(Const iniFile: TMemIniFile);

Var
  iSTIndex: Integer;
  ST: TBADISpecialTag;
  iTag: Integer;
  sl : TstringList;

Begin
  sl := TStringList.Create;
  Try
    iniFile.ReadSection(strSpecialTagNames, sl);
    If sl.Count > 0 Then
      FSpecialTags.Clear;
    For iTag := 0 To sl.Count - 1 Do
      Begin
        iSTIndex := FSpecialTags.Add(TBADISpecialTag.Create(sl[iTag],
          iniFile.ReadString(strSpecialTagNames, sl[iTag], ''),
          TBADITagProperties(Byte(iniFile.ReadInteger(strSpecialTags, sl[iTag], 0)))));
        ST := FSpecialTags[iSTIndex];
        ST.FFontStyles := TFontStyles(Byte(iniFile.ReadInteger(strSpecialTagFontStyles, sl[iTag], 0)));
        ST.FFontColour := StringToColor(iniFile.ReadString(strSpecialTagFontForeColours, sl[iTag],
          ColorToString(clNone)));
        ST.FBackColour := StringToColor(iniFile.ReadString(strSpecialTagFontBackColours, sl[iTag],
          ColorToString(clNone)));
        FSpecialTags[iSTIndex] := ST;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method saves the checks to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The check setings are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveChecks(Const iniFile: TMemIniFile);

Var
  eCheck: TBADIModuleCheck;

Begin
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    Begin
      iniFile.WriteBool(strModuleChecks, ModuleChecks[eCheck].FName + strEnabled,
        FModuleChecks[eCheck].FEnabled);
      iniFile.WriteFloat(strModuleChecks, ModuleChecks[eCheck].FName + strLimit,
        FModuleChecks[eCheck].FLimit);
    End;
End;

(**

  This method saves the documentation options to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The documentation options are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveDocOptions(Const iniFile: TMemIniFile);

Var
  i: TDocOption;

Begin
  For i := Low(TDocOption) To High(TDocOption) Do
    iniFile.WriteBool(strDocOptions, DocOptionInfo[i].FDescription, i In FOptions);
End;

(**

  This methods saves the extensions associated with parsers to the ini file.

  @precon  iniFile must be a valid instance.
  @postcon The extensions are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveExtensions(Const iniFile: TMemIniFile);

Var
  iModule: Integer;

Begin
  For iModule := 0 To TBADIDispatcher.BADIDispatcher.Count - 1 Do
    iniFile.WriteString(strModuleExtensions,
      TBADIDispatcher.BADIDispatcher.Modules[iModule].Cls.ClassName,
      TBADIDispatcher.BADIDispatcher.Modules[iModule].Extensions);
End;

(**

  This method saves the limit information to the ini file.

  @precon  iniFile must be a valid instance.
  @postcon The limit information is saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveLimits(Const iniFile: TMemIniFile);

Begin
  iniFile.WriteInteger(strIssuesLimits, strErrors, FIssueLimits[ltErrors]);
  iniFile.WriteInteger(strIssuesLimits, strWarnings, FIssueLimits[ltWarnings]);
  iniFile.WriteInteger(strIssuesLimits, strHints, FIssueLimits[ltHints]);
  iniFile.WriteInteger(strIssuesLimits, strConflicts, FIssueLimits[ltConflicts]);
  iniFile.WriteInteger(strIssuesLimits, strMetrics, FIssueLimits[ltMetrics]);
End;

(**

  This method save the managed nodes to the ini file.

  @precon  iniFile must be a valid insatnce.
  @postcon The managed nodes are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveManagedNodes(Const iniFile: TMemIniFile);

Var
  iNode: Integer;

Begin
  iniFile.EraseSection(strManagedExpandedNodes);
  For iNode := 0 To ExpandedNodes.Count - 1 Do
    iniFile.WriteInteger(strManagedExpandedNodes, StringReplace(FExpandedNodes[iNode], '=', '|',
      [rfReplaceAll]), Integer(FExpandedNodes.Objects[iNode]));
End;

(**

  This method saves the method descriptions to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The method descriptions are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveMethodDescrpitions(Const iniFile: TMemIniFile);

Var
  iMethod: Integer;

Begin
  iniFile.EraseSection(strMethodDescriptions);
  For iMethod := 0 To FMethodDescriptions.Count - 1 Do
    iniFile.WriteString(strMethodDescriptions, FMethodDescriptions.Names[iMethod],
      FMethodDescriptions.ValueFromIndex[iMethod]);
End;

(**

  This method saves the metrics to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The metric setings are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveMetrics(Const iniFile: TMemIniFile);

Var
  eMetric: TBADIModuleMetric;

Begin
  For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
    Begin
      iniFile.WriteBool(strModuleMetrics, ModuleMetrics[eMetric].FName + strEnabled,
        FModuleMetrics[eMetric].FEnabled);
      iniFile.WriteFloat(strModuleMetrics, ModuleMetrics[eMetric].FName + strLimit,
        FModuleMetrics[eMetric].FLimit);
    End;
  iniFile.WriteInteger(strMetricMargins, strLowMargin, Trunc(FLowMetricMargin));
  iniFile.WriteInteger(strMetricMargins, strHighMargin, Trunc(FHighMetricMargin));
End;

(**

  This method saves the module explorer options to the ini file.

  @precon  iniFile must be a valid instance.
  @postcon The module explorer optins are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveModuleExplorerOptions(Const iniFile: TMemIniFile);

Var
  T: TBADITokenType;

Begin
  iniFile.WriteInteger(strModuleExplorer, strUpdateInterval, FUpdateInterval);
  iniFile.WriteInteger(strModuleExplorer, strScopesToRender, Byte(FScopesToRender));
  iniFile.WriteInteger(strSetup, strBrowsePosition, Integer(FBrowsePosition));
  iniFile.WriteString(strModuleExplorer, strFontName, FTreeFontName);
  iniFile.WriteInteger(strModuleExplorer, strFontSize, FTreeFontSize);
  iniFile.WriteString(strModuleExplorer, strFixedFontName, FFixedFontName);
  iniFile.WriteInteger(strModuleExplorer, strFixedFontSize, FFixedFontSize);
  For T := Low(TBADITokenType) To High(TBADITokenType) Do
    Begin
      iniFile.WriteString(strTokenFontInfo, Format(strFontColour, [strTokenType[T]]),
        ColorToString(FTokenFontInfo[T].FForeColour));
      iniFile.WriteInteger(strTokenFontInfo, Format(strFontStyles, [strTokenType[T]]),
        Byte(FTokenFontInfo[T].FStyles));
      iniFile.WriteString(strTokenFontInfo, Format(strFontBackColour, [strTokenType[T]]),
        ColorToString(FTokenFontInfo[T].FBackColour));
    End;
  iniFile.WriteString(strModuleExplorer, strBGColour, ColorToString(FModuleExplorerBGColour));
  iniFile.WriteInteger(strModuleExplorer, strTokenLimit, FTokenLimit);
  iniFile.WriteInteger(strDocumentation, strMaxDocOutputWidth, FMaxDocOutputWidth);
  iniFile.WriteInteger(strModuleExplorer, strManagedNodesLife, FManagedNodesLife);
  iniFile.WriteString(strModuleExplorer, strTreeColour, ColorToString(FTreeColour));
End;

(**

  This method saves the profiling options to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The profiling options are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveProfilingOptions(Const iniFile: TMemIniFile);

Var
  j: Integer;

Begin
  iniFile.EraseSection(strProfilingCode);
  For j := 0 To FProfilingCode.Count - 1 Do
    If FProfilingCode.Names[j] <> '' Then
      iniFile.WriteString(strProfilingCode, FProfilingCode.Names[j], FProfilingCode.ValueFromIndex[j]);
End;

(**

  This method saves the applications settings to an ini file.

  @precon  None.
  @postcon Saves the applications settings to an ini file.

**)
Procedure TBADIOptions.SaveSettings;

Var
  iniFile : TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    SaveDocOptions(iniFile);
    SaveSpecialTags(iniFile);
    SaveManagedNodes(iniFile);
    SaveModuleExplorerOptions(iniFile);
    iniFile.WriteString(strSetup, strExcludeDocFiles, StringReplace(FExcludeDocFiles.Text, #13#10, '|',
      [rfReplaceAll]));
    SaveMethodDescrpitions(iniFile);
    iniFile.WriteInteger(strDocumentation, strScopes, Byte(FScopesToDocument));
    SaveProfilingOptions(iniFile);
    SaveLimits(iniFile);
    SaveShortcuts(iniFile);
    SaveExtensions(iniFile);
    SaveMetrics(iniFile);
    SaveChecks(iniFile);
    iniFile.WriteBool(strRefactorings, strNewLine, FRefactorConstNewLine);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This methods saves the menu shortcuts to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The shortcuts are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveShortcuts(Const iniFile: TMemIniFile);

Var
  iBADIMenu: TBADIMenu;

Begin
  For iBADIMenu := Low(TBADIMenu) To High(TBADIMenu) Do
    iniFile.WriteString(strBADIMenuShortcuts, BADIMenus[iBADIMenu].FName, FBADIMenuShortCuts[iBADIMenu]);
End;

(**

  This method saves the special tags to the ini file.

  @precon  iniFile must be a valid instance.
  @postcon The special tags are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveSpecialTags(Const iniFile: TMemIniFile);

Var
  iTag: Integer;

Begin
  iniFile.EraseSection(strSpecialTags);
  iniFile.EraseSection(strSpecialTagNames);
  For iTag := 0 To FSpecialTags.Count - 1 Do
    Begin
      iniFile.WriteInteger(strSpecialTags, FSpecialTags[iTag].FName,
        Byte(FSpecialTags[iTag].FTagProperties));
      iniFile.WriteString(strSpecialTagNames, FSpecialTags[iTag].FName,
        FSpecialTags[iTag].FDescription);
      iniFile.WriteInteger(strSpecialTagFontStyles, FSpecialTags[iTag].FName,
        Byte(FSpecialTags[iTag].FFontStyles));
      iniFile.WriteString(strSpecialTagFontForeColours, FSpecialTags[iTag].FName,
        ColorToString(FSpecialTags[iTag].FFontColour));
      iniFile.WriteString(strSpecialTagFontBackColours, FSpecialTags[iTag].FName,
        ColorToString(FSpecialTags[iTag].FBackColour));
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

  This is a setter method for the ModuleCheck property.

  @precon  None.
  @postcon Sets the module check configuration.

  @param   eModuleCheck as a TBADIModuleCheck as a constant
  @param   recValue     as a TBADICheckRecord as a constant

**)
Procedure TBADIOptions.SetModuleCheck(Const eModuleCheck: TBADIModuleCheck;
  Const recValue: TBADICheckRecord);

Begin
  FModuleChecks[eModuleCheck] := recValue;
End;

(**

  This is a setter method for the ModuleMetric property.

  @precon  None.
  @postcon Sets the module metric configuration.

  @param   eModuleMetric as a TBADIModuleMetric as a constant
  @param   recValue      as a TBADIMetricRecord as a constant

**)
Procedure TBADIOptions.SetModuleMetric(Const eModuleMetric: TBADIModuleMetric;
  Const recValue: TBADIMetricRecord);

Begin
  FModuleMetrics[eModuleMetric] := recValue;
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
