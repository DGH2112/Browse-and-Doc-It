(**

  This module contains a class which loads and saves all the application options to an INI file.

  @Author  David Hoyle
  @Version 20.836
  @Date    12 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.Options;

Interface

Uses
  Classes,
  Generics.Collections,
  RegularExpressions,
  VCL.Graphics,
  VCL.Controls,
  IniFiles,
  BADI.Types,
  BADI.Interfaces;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class to define a set of options for the application. **)
  TBADIOptions = Class(TInterfacedObject, IBADIOptions)
  Strict Private
    FOptions                          : TDocOptions;
    FDefines                          : TStringList;
    FSpecialTags                      : TList<TBADISpecialTag>;
    FExpandedNodes                    : TStringList;
    FINIFileName                      : String;
    FUpdateInterval                   : Cardinal;
    FScopesToRender                   : TScopes;
    FBrowsePosition                   : TBrowsePosition;
    FTreeFontName                     : String;
    FTreeFontSize                     : Integer;
    FFixedFontName                    : String;
    FFixedFontSize                    : Integer;
    FIDEEditorColours                 : IBADIIDEEditorColours;
    FTokenFontInfo                    : Array[False..True] Of TBADITokenFontInfoTokenSet;
    FUseIDEEditorColours              : Boolean;
    FExclusions                       : IBADIExclusions;
    FMethodDescriptions               : TStringList;
    FScopesToDocument                 : TScopes;
    FModuleExplorerBGColour           : Array[False..True] Of TColor;
    FTokenLimit                       : Integer;
    FMaxDocOutputWidth                : Integer;
    FManagedNodesLife                 : Integer;
    FTreeColour                       : TColor;
    FProfilingCode                    : TStringList;
    FIssueLimits                      : Array[TLimitType] Of Integer;
    FBADIMenuShortCuts                : Array[TBADIMenu] Of String;
    FModuleMetrics                    : Array[TBADIModuleMetric] Of TBADIMetricRecord;
    FModuleMetricSubOps               : TBADIModuleMetricSubOps;
    FToxicityPower                    : Integer;
    FToxicitySummation                : TBADIToxicitySummation;
    FModuleChecks                     : Array[TBADIModuleCheck] Of TBADICheckRecord;
    FModuleCheckSubOps                : TBADIModuleCheckSubOps;
    FLowMetricMargin                  : Double;
    FHighMetricMargin                 : Double;
    FRefactorConstNewLine             : Boolean;
    FRequiresIDEEditorColoursUpdating : Boolean;
    FModuleDateFmt                    : String;
    FModuleVersionIncrement           : Double;
    FDoNotFollowEditor                : TLimitTypes;
    FScopeImageList                   : TImageList;
    FLanguageDictionaryFile           : String;
    FLanguageDictionary               : TStringList;
    FLocalDictionaryFile              : String;
    FLocalDictionary                  : TStringList;
    FProjectDictionaryFile            : String;
    FProjectDictionary                : TStringList;
    FIgnoreDictionaryFile             : String;
    FIgnoreDictionary                 : TStringList;
    FSpellingMistakeColour            : TColor;
    FCommentTagNames                  : TStringList;
    FCommentTypes                     : TStringList;
  Strict Protected
    // IBADIOptions
    Function  GetOptions : TDocOptions;
    Procedure SetOptions(Const setOptions : TDocOptions);
    Function  GetDefines : TStringList;
    Function  GetSpecialtags : TList<TBADISpecialTag>;
    Function  GetExpandedNodes : TStringList;
    Function  GetUpdateInterval : Cardinal;
    Procedure SetUpdateInterval(Const iInterval : Cardinal);
    Function  GetScopesToRender : TScopes;
    Procedure SetScopesToRender(Const setScopes : TScopes);
    Function  GetBrowsePosition : TBrowsePosition;
    Procedure SetBrowsePosition(Const eBrowsePosition : TBrowsePosition);
    Function  GetTreeFontName : String;
    Procedure SetTreeFontName(Const strFontName : String);
    Function  GetTreeFontSize : Integer;
    Procedure SetTreeFontSize(Const iFontSize : Integer);
    Function  GetFixedFontName : String;
    Procedure SetFixedFontName(Const strFontName : String);
    Function  GetFixedFontSize : Integer;
    Procedure SetFixedFontSize(Const iFontSize : Integer);
    Function  GetTokenFontInfo(Const boolUseIDEEditorColours : Boolean) : TBADITokenFontInfoTokenSet;
    Procedure SetTokenFontInfo(Const boolUseIDEEditorColours : Boolean;
      Const TokenFontInfo : TBADITokenFontInfoTokenSet);
    Function  GetExclusions : IBADIExclusions;
    Function  GetMethodDescriptions : TStringList;
    Function  GetScopestoDocument : TScopes;
    Procedure SetScopesToDocument(Const setScopes : TScopes);
    Function  GetModuleExplorerBGColour(Const boolUseIDEEditorColours : Boolean) : TColor;
    Procedure SetModuleExplorerBGColour(Const boolUseIDEEditorColours : Boolean; Const iColour : TColor);
    Function  GetTokenLimit : Integer;
    Procedure SetTokenLimit(Const iTokenLimit : Integer);
    Function  GetMaxDocOutputWidth : Integer;
    Procedure SetMaxDocOutputWidth(Const iMaxDocOutputWidth : Integer);
    Function  GetManagedNodesLife : Integer;
    Procedure SetManagedNodesLife(Const iNodeLife : Integer);
    Function  GetTreeColour : TColor;
    Procedure SetTreeColour(Const iColour : TColor);
    Function  GetINIFileName : String;
    Function  GetProfilingCode(Const strModuleName : String) : String;
    Procedure SetProfilingCode(Const strModuleName, strProfileCode : String);
    Function  GetIssueLimit(Const eLimitType : TLimitType) : Integer;
    Procedure SetIssueLimit(Const eLimitType : TLimitType; Const iLimit : Integer);
    Function  GetMenuShortcut(Const eBADIMenu : TBADIMenu) : String;
    Procedure SetMenuShortcut(Const eBADIMenu : TBADIMenu; Const strShortcut : String);
    Function  GetModuleMetric(Const eModuleMetric : TBADIModuleMetric) : TBADIMetricRecord;
    Procedure SetModuleMetric(Const eModuleMetric : TBADIModuleMetric;
      Const recMetric : TBADIMetricRecord);
    Function  GetModuleMetricSubOps : TBADIModuleMetricSubOps;
    Procedure SetModuleMetricSubOps(Const setModuleMetricSubOps : TBADIModuleMetricSubOps);
    Function  GetToxicityPower : Integer;
    Procedure SetToxicityPower(Const iPower : Integer);
    Function  GetToxicitySummation : TBADIToxicitySummation;
    Procedure SetToxicitySummation(Const eToxicitySummartion : TBADIToxicitySummation);
    Function  GetModuleCheck(Const eModuleCheck : TBADIModuleCheck) : TBADICheckRecord;
    Procedure SetModuleCheck(Const eModuleCheck : TBADIModuleCheck; Const recCheck : TBADICheckRecord);
    Function  GetModuleCheckSubOps : TBADIModuleCheckSubOps;
    Procedure SetModuleCheckSubOps(Const setModuleCheckSubOps : TBADIModuleCheckSubOps);
    Function  GetLowMetricMargin : Double;
    Procedure SetLowMetricMargin(Const dblMargin : Double);
    Function  GetHighMetricMargin : Double;
    Procedure SetHighMetricMargin(Const dblMargin : Double);
    Function  GetRefactorConstNewLine : Boolean;
    Procedure SetRefactorConstNewLine(Const boolNewLine : Boolean);
    Function  GetUseIDEEditorColours : Boolean;
    Procedure SetUseIDEEditorColours(Const boolUseIDEEditorColours : Boolean);
    Function  GetModuleDateFmt : String;
    Procedure SetModuleDateFmt(Const strValue : String);
    Function  GetModuleVersionIncrement : Double;
    Procedure SetModuleVersionIncrement(Const dblValue : Double);
    Function  GetDoNotFollowEditor : TLimitTypes;
    Procedure SetDoNotFollowEditor(Const setLimitTypes : TLimitTypes);
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure RequiresIDEEditorColoursUpdate;
    Procedure LoadIDEEditorColours;
    Procedure LoadDocOptions(Const iniFile: TMemIniFile);
    Procedure LoadSpecialTags(Const iniFile: TMemIniFile);
    Procedure LoadManagedNodes(Const iniFile: TMemIniFile);
    Procedure LoadModuleExplorerOptions(Const iniFile: TMemIniFile);
    Procedure LoadExclusions(Const iniFile: TMemIniFile);
    Procedure LoadMethodDescriptions(Const iniFile: TMemIniFile);
    Procedure LoadProfilingOptions(Const iniFile: TMemIniFile);
    Procedure LoadLimits(Const iniFile: TMemIniFile);
    Procedure LoadShortcuts(Const iniFile: TMemIniFile);
    Procedure LoadExtensions(Const iniFile: TMemIniFile);
    Procedure LoadMetrics(Const iniFile: TMemIniFile);
    Procedure LoadChecks(Const iniFile: TMemIniFile);
    Procedure LoadDictionaries(Const iniFile: TMemIniFile);
    Procedure LoadCommentTags(Const iniFile: TMemIniFile);
    Procedure LoadCommentTypes(Const iniFile: TMemIniFile);
    Procedure UpdateDoNotFollowEditor();
    Procedure SaveDocOptions(Const iniFile: TMemIniFile);
    Procedure SaveSpecialTags(Const iniFile: TMemIniFile);
    Procedure SaveManagedNodes(Const iniFile: TMemIniFile);
    Procedure SaveModuleExplorerOptions(Const iniFile: TMemIniFile);
    Procedure SaveExclusions(Const iniFile: TMemIniFile);
    Procedure SaveMethodDescrpitions(Const iniFile: TMemIniFile);
    Procedure SaveProfilingOptions(Const iniFile: TMemIniFile);
    Procedure SaveLimits(Const iniFile: TMemIniFile);
    Procedure SaveShortcuts(Const iniFile: TMemIniFile);
    Procedure SaveExtensions(Const iniFile: TMemIniFile);
    Procedure SaveMetrics(Const iniFile: TMemIniFile);
    Procedure SaveChecks(Const iniFile: TMemIniFile);
    Procedure SaveDictionaries(Const iniFile: TMemIniFile);
    Procedure SaveCommentTags(Const iniFile: TMemIniFile);
    Procedure SaveCommentTypes(Const iniFile: TMemIniFile);
    Function  GetScopeImageList : TImageList;
    Function  GetLanguageDictionary : TStringList;
    Function  GetLanguageDictionaryFile : String;
    Procedure SetLanguageDictionaryFile(Const strValue : String);
    Function  GetLocalDictionaryFile : String;
    Procedure SetLocalDictionaryFile(Const strValue : String);
    Function  GetLocalDictionary : TStringList;
    Function  GetProjectDictionaryFile : String;
    Procedure SetProjectDictionaryFile(Const strValue : String);
    Function  GetProjectDictionary : TStringList;
    Function  GetIgnoreDictionaryFile : String;
    Procedure SetIgnoreDictionaryFile(Const strValue : String);
    Function  GetIgnoreDictionary : TStringList;
    Function  GetSpellingMistakeColour : TColor;
    Procedure SetSpellingMistakeColour(Const iColour : TColor);
    Function  GetCommentTagName(Const strExt : String) : String;
    Procedure SetCommentTagName(Const strExt, strValue : String);
    Function  GetCommentType(Const strExt : String) : TCommentType;
    Procedure SetCommentType(Const strExt : String; Const eValue : TCommentType);
  Public
    Constructor Create(Const IDEEditorColours : IBADIIDEEditorColours);
    Destructor Destroy; Override;
    Class Function BADIOptions : IBADIOptions;
    Class Procedure Release;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  BADI.Exclusions,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.Functions,
  BADI.IDEEditorColours;

Const
  (** An INI section name for the special tag names. **)
  strSpecialTagNames = 'SpecialTagNames';
  (** An INI section name for the special tag information. **)
  strSpecialTags = 'SpecialTags';
  (** An INI key for special tag font styles. **)
  strSpecialTagFontStyles = 'SpecialTagFontStyles';
  (** An INI key for special tag font fore colours. **)
  strSpecialTagFontForeColours = 'SpecialTagFontForeColours';
  (** An INI key for special tag font back colours. **)
  strSpecialTagFontBackColours = 'SpecialTagFontBackColours';
  (** An INI key for special tag image indexes. **)
  strSpecialTagImageIndexes = 'SpecialTagImageIndexes';
  (** An INI section name for the managed nodes. **)
  strManagedExpandedNodes = 'ManagedExpandedNodes';
  (** An INI section name for the documentation options. **)
  strDocOptions = 'Options';
  (** An INI section name for the modules options. **)
  strModuleExplorer = 'ModuleExplorer';
  (** An INI key for the update interval **)
  strUpdateInterval = 'UpdateInterval';
  (** An INI key for the Scopes to render in the explorer **)
  strScopesToRender = 'ScopesToRender';
  (** An INI section name for general settings. **)
  strSetup = 'Setup';
  (** An INI key for the browse position **)
  strBrowsePosition = 'BrowsePosition';
  (** An INI key for the proportional font name **)
  strFontName = 'Name';
  (** An INI key for the proportional font size **)
  strFontSize = 'Size';
  (** An INI key for the fixed font name **)
  strFixedFontName = 'FixedName';
  (** An INI key for the fixed font size **)
  strFixedFontSize = 'FixedSize';
  (** An INI key for the token font information **)
  strTokenFontInfo = 'TokenFontInfo';
  (** An INI key for the token font colour **)
  strFontColour = '%s.Colour';
  (** An INI key for the token font style **)
  strFontStyles = '%s.Styles';
  (** An INI key for the token font back colour **)
  strFontBackColour = '%s.BackColour';
  (** An INI section name for the method descriptions. **)
  strMethodDescriptions = 'MethodDescriptions';
  (** An INI section name for the excluded documentation files. **)
  strExcludeDocFiles = 'ExcludeDocFiles';
  (** An INI section name for the exclusions for documentation, metrics and check files. **)
  strExclusions = 'Exclusions';
  (**  An INI section name for documentation options. **)
  strDocumentation = 'Documentation';
  (** An INI key for the module explorer background colour **)
  strBGColour = 'BGColour';
  (** An INI key for token limit **)
  strTokenLimit = 'TokenLimit';
  (** An INI key for max documentation output width **)
  strMaxDocOutputWidth = 'MaxDocOutputWidth';
  (** An INI key for managed node life in days **)
  strManagedNodesLife = 'ManagedNodesLife';
  (** An INI key for explorer tree colour. **)
  strTreeColour = 'TreeColour';
  (** An INI key for scopes **)
  strScopes = 'Scopes';
  (** An INI section name for the profiling options **)
  strProfilingCode = 'ProfilingCode';
  (** An INI key for issue limits **)
  strIssuesLimits = 'Issues Limits';
  (** An INI section name for the module metrics **)
  strModuleMetrics = 'Module Metrics';
  (** An INI section name for the module checks **)
  strModuleChecks = 'Module Checks';
  (** An INI key for enabled metrics **)
  strEnabled = '.Enabled';
  (** An INI key for metrics limits **)
  strLimit = '.Limit';
  (** An INI section name for the metrics margins **)
  strMetricMargins = 'MetricMargins';
  (** An INI key for the low metric margin. **)
  strLowMargin = 'LowMargin';
  (** An INI key for the high metric margin. **)
  strHighMargin = 'HighMargin';
  (** An INI section name for the module extensions **)
  strModuleExtensions = 'ModuleExtensions';
  (** An INI section name for the refactorings **)
  strRefactorings = 'Refactorings';
  (** An INI key for new lines in refactoring. **)
  strNewLine = 'NewLine';
  (** An INI section name for the shortcuts **)
  strBADIMenuShortcuts = 'BADIMenuShortcuts';
  (** A constant string to define the INI key for Toxicity Power **)
  strToxicityPower = 'ToxicityPower';
  (** A constant string to define the INI key for Toxicity Summation **)
  strToxicitySummation = 'ToxicitySummation';
  (** An INI Key for Using IDE Editor Colours rather than custom colours. **)
  strUseIDEEditorColours = 'UseIDEEditorColours';
  (** A constant to define the INI section for the automatic module settings. **)
  strAutomaticModuleUpdatesINISection = 'Automatic Module Updates';
  (** A constant for the module date format. **)
  strDateFormatINIKey = 'Date Format';
  (** A constant for the module version increment. **)
  strIncrementINIKey = 'Increment';
  (** A constant for the language dictionary INI Key. **)
  strLanguageDictionaryINIKey = 'LanguageDictionary';
  (** A constant for the local dictionary INI Key. **)
  strLocalDictionaryINIKey = 'LocalDictionary';
  (** A constant for the ignore dictionary INI Key. **)
  strIgnoreDictionaryINIKey = 'IgnoreDictionary';
  (** A constant for the Spelling Mistake Colour INI Key. **)
  strSpellingMistakeColourINIKey = 'Spelling Mistake Colour';
  (** A constant for the Comment Tag Names INI Section **)
  strCommentTagNamesINISection = 'CommentTagNames';
  (** A constant for the Comment Types INI Section **)
  strCommentTypesINISection = 'CommentTypes';

Var
  (** This is a class variable to hide and hold the BADI Options instance reference. **)
  FBADIOptionsInstance  : IBADIOptions;

(**

  This class method returns the singleton instance of the BADI Options class.

  @precon  None.
  @postcon returns the instance of the BADI Options (and creates it if it hasn`t already been done).

  @return  an IBADIOptions

**)
Class Function TBADIOptions.BADIOptions: IBADIOptions;

Begin
  If Not Assigned(FBADIOptionsInstance) Then
    FBADIOptionsInstance := TBADIOptions.Create(TBADIIDEEditorColours.Create);
  Result := FBADIOptionsInstance;
End;

(**

  This is the constructor method for the TBADIOptions class.

  @precon  None.
  @postcon Does nothing at the moment.

  @param   IDEEditorColours as an IBADIIDEEditorColours as a constant

**)
Constructor TBADIOptions.Create(Const IDEEditorColours : IBADIIDEEditorColours);

Type
  TDefaultSpecialTag = Record
    FTagName : String;
    FTagDesc : String;
    FTagOps  : TBADITagProperties;
    FTagImage: TBADIImageIndex;
  End;
  
Const
  DefaultSpecialTags : Array[0..15] Of TDefaultSpecialTag = (
    (FTagName: 'todo';      FTagDesc: 'Things To Do';           FTagOps: [tpShowInTree..tpShowInDoc]; FTagImage: iiRedToDoCross),
    (FTagName: 'done';      FTagDesc: 'Completed Things To Do'; FTagOps: [tpShowInTree..tpShowInDoc]; FTagImage: iiGreenToDoCross),
    (FTagName: 'precon';    FTagDesc: 'Pre-Conditions';         FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'postcon';   FTagDesc: 'Post-Conditions';        FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'param';     FTagDesc: 'Parameters';             FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'return';    FTagDesc: 'Returns';                FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'note';      FTagDesc: 'Notes';                  FTagOps: [];                          FTagImage: iiToDoItem),
    (FTagName: 'see';       FTagDesc: 'Also See';               FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'exception'; FTagDesc: 'Exception Raised';       FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'bug';       FTagDesc: 'Known Bugs';             FTagOps: [tpShowInTree..tpShowInDoc]; FTagImage: iiRedBug),
    (FTagName: 'debug';     FTagDesc: 'Debugging Code';         FTagOps: [tpShowInTree..tpShowInDoc]; FTagImage: iiYellowBug),
    (FTagName: 'date';      FTagDesc: 'Date Code Last Updated'; FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'author';    FTagDesc: 'Code Author';            FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'version';   FTagDesc: 'Code Version';           FTagOps: [];                          FTagImage: iiNone),
    (FTagName: 'refactor';  FTagDesc: 'Refactorings';           FTagOps: [tpShowInTree..tpShowInDoc]; FTagImage: iiBlueBookmark),
    (FTagName: 'code';      FTagDesc: 'Code Example';           FTagOps: [tpShowInTree..tpFixed];     FTagImage: iiNone)
  );
  
Var
  iTag: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create;
  FIDEEditorColours := IDEEditorColours;
  FDefines := TStringList.Create;
  FSpecialTags := TList<TBADISpecialTag>.Create;
  // Create a default set of Special Tags.
  For iTag := Low(DefaultSpecialTags) To High(DefaultSpecialTags) Do
    FSpecialTags.Add(
      TBADISpecialTag.Create(
        DefaultSpecialTags[iTag].FTagName,
        DefaultSpecialTags[iTag].FTagDesc,
        DefaultSpecialTags[iTag].FTagOps,
        DefaultSpecialTags[iTag].FTagImage
      )
    );
  FExpandedNodes := TStringList.Create;
  FExpandedNodes.Sorted := True;
  FExpandedNodes.Duplicates := dupIgnore;
  FLanguageDictionary := TStringList.Create;
  FLanguageDictionary.Sorted := True;
  FLanguageDictionary.Duplicates := dupIgnore;
  FLocalDictionary := TStringList.Create;
  FLocalDictionary.Sorted := True;
  FLocalDictionary.Duplicates := dupIgnore;
  FProjectDictionary := TStringList.Create;
  FProjectDictionary.Sorted := True;
  FProjectDictionary.Duplicates := dupIgnore;
  FignoreDictionary := TStringList.Create;
  FignoreDictionary.Sorted := True;
  FignoreDictionary.Duplicates := dupIgnore;
  FCommentTagNames := TStringList.Create;
  FCommentTypes := TStringList.Create;
  FINIFileName := BuildRootKey;
  FScopesToRender := [scPrivate, scProtected, scPublic, scPublished];
  FExclusions := TBADIExclusions.Create;
  FMethodDescriptions := TStringList.Create;
  FScopesToDocument := [scPublished, scPublic, scProtected, scPrivate];
  FProfilingCode := TStringList.Create;
  FRequiresIDEEditorColoursUpdating := True;
  FScopeImageList := TImageList.Create(Nil);
  LoadBADIImages(FScopeImageList);
End;

(**

  This is the destructor method for the TBADIOptions class.

  @precon  none.
  @postcon Does nothing at the moment except call the inherited destroy method.

**)
Destructor TBADIOptions.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  SaveSettings;
  FCommentTypes.Free;
  FCommentTagNames.Free;
  FScopeImageList.Free;
  FProfilingCode.Free;
  FMethodDescriptions.Free;
  FExpandedNodes.Free;
  FLanguageDictionary.Free;
  FLocalDictionary.Free;
  FProjectDictionary.Free;
  FIgnoreDictionary.Free;
  FSpecialTags.Free;
  FDefines.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Browse Position property.

  @precon  None.
  @postcon Returns the position the cursor should be placed when browsing.

  @return  a TBrowsePosition

**)
Function TBADIOptions.GetBrowsePosition: TBrowsePosition;

Begin
  Result := FBrowsePosition;
End;

(**

  This is a getter method for the Comment Tag Name property.

  @precon  None.
  @postcon Returns the comment tag name for the given file extension.

  @param   strExt as a String as a constant
  @return  a String

**)
Function TBADIOptions.GetCommentTagName(Const strExt: String): String;

Const
  strToDoDefaultTagName = 'todo';

Var
  iIndex: Integer;

Begin
  Result := strToDoDefaultTagName;
  iIndex := FCommentTagNames.IndexOfName(strExt);
  If iIndex > -1 Then
    Result := FCommentTagNames.ValueFromIndex[iIndex];
End;

(**

  This is a getter method for the Comment Type property.

  @precon  None.
  @postcon Returns the comment type associated with the given file extension.

  @param   strExt as a String as a constant
  @return  a TCommentType

**)
Function TBADIOptions.GetCommentType(Const strExt: String): TCommentType;

Var
  iIndex: Integer;

Begin
  Result := TBADIDispatcher.BADIDispatcher.GetCommentType(strExt, csBlock);
  iIndex := FCommentTypes.IndexOf(strExt);
  If iIndex > -1 Then
    Result := TCommentType(FCommentTypes.Objects[iIndex]);
End;

(**

  This is a getter method for the Defines property.

  @precon  None.
  @postcon Returns a strings list containing all the conditional defines for the current project.

  @return  a TStringList

**)
Function TBADIOptions.GetDefines: TStringList;

Begin
  Result := FDefines;
End;

(**

  This is a getter method for the Do Not Follow Editor property.

  @precon  None.
  @postcon Returns the set of limits which should prevent the module explorer following the editor.

  @return  a TLimitTypes

**)
Function TBADIOptions.GetDoNotFollowEditor: TLimitTypes;

Begin
  Result := FDoNotFollowEditor;
End;

(**

  This is a getter method for the Exclude Doc Files property.

  @precon  None.
  @postcon Returns a string list of filename / path patterns to be used to ignore documentation.

  @return  an IBADIExclusions

**)
Function TBADIOptions.GetExclusions: IBADIExclusions;

Begin
  Result := FExclusions;
End;

(**

  This is a getter method for the Expanded Nodes property.

  @precon  None.
  @postcon Returns a string list of the paths of all the expanded nodes.

  @return  a TStringList

**)
Function TBADIOptions.GetExpandedNodes: TStringList;

Begin
  Result := FExpandedNodes;
End;

(**

  This is a getter method for the Fixed Font Name property.

  @precon  None.
  @postcon Returns the font name to be used for fixed font output.

  @return  a String

**)
Function TBADIOptions.GetFixedFontName: String;

Begin
  Result := FFixedFontName;
End;

(**

  This is a getter method for the Fixed Font Size property.

  @precon  None.
  @postcon Returns the font size to be used for fixed font output.

  @return  an Integer

**)
Function TBADIOptions.GetFixedFontSize: Integer;

Begin
  Result := FFixedFontSize;
End;

(**

  This is a getter method for the High Metric Margin property.

  @precon  None.
  @postcon Returns the value to be used as the upper limit for the metric margin.

  @return  a Double

**)
Function TBADIOptions.GetHighMetricMargin: Double;

Begin
  Result := FHighMetricMargin;
End;

(**

  This is a getter method for the Ignore Dictionary property.

  @precon  None.
  @postcon Returns the string list for the ignore dictionary.

  @return  a TStringList

**)
Function TBADIOptions.GetIgnoreDictionary: TStringList;

Begin
  Result := FIgnoreDictionary;
End;

(**

  This is a getter method for the Ignore Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the ignore dictionary.

  @return  a String

**)
Function TBADIOptions.GetIgnoreDictionaryFile: String;

Begin
  Result := FIgnoreDictionaryFile;
End;

(**

  This is a getter method for the INI Filename property.

  @precon  None.
  @postcon Returns the filename and path for BADI INI file.

  @return  a String

**)
Function TBADIOptions.GetINIFileName: String;

Begin
  Result := FINIFileName;
End;

(**

  This is a getter method for the Issue Limit property.

  @precon  None.
  @postcon Returns the numerical limit for the given limit type.

  @param   eLimitType as a TLimitType as a constant
  @return  an Integer

**)
Function TBADIOptions.GetIssueLimit(Const eLimitType: TLimitType): Integer;

Begin
  Result := FIssueLimits[eLimitType];
End;

(**

  This is a getter method for the Language Dictionary property.

  @precon  None.
  @postcon Returns the string list for the language dictionary.

  @return  a TStringList

**)
Function TBADIOptions.GetLanguageDictionary: TStringList;

Begin
  Result := FLanguageDictionary;
End;

(**

  This is a getter method for the Language Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the language dictionary.

  @return  a String

**)
Function TBADIOptions.GetLanguageDictionaryFile: String;

Begin
  Result := FLanguageDictionaryFile;
End;

(**

  This is a getter method for the Local Dictionary property.

  @precon  None.
  @postcon Returns the string list for the local dictionary.

  @return  a TStringList

**)
Function TBADIOptions.GetLocalDictionary: TStringList;

Begin
  REsult := FLocalDictionary;
End;

(**

  This is a getter method for the Local Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the local dictionary.

  @return  a String

**)
Function TBADIOptions.GetLocalDictionaryFile: String;

Begin
  Result := FLocalDictionaryFile;
End;

(**

  This is a getter method for the Low Metric Margin property.

  @precon  None.
  @postcon Returns the value to be used for the lower metric margin.

  @return  a Double

**)
Function TBADIOptions.GetLowMetricMargin: Double;

Begin
  Result := FLowMetricMargin;
End;

(**

  This is a getter method for the Managed Nodes Life property.

  @precon  None.
  @postcon Returns the number of days a node is to be managed in the expanded nodes list.

  @return  an Integer

**)
Function TBADIOptions.GetManagedNodesLife: Integer;

Begin
  Result := FManagedNodesLife;
End;

(**

  This is a getter method for the Max Document Output Width property.

  @precon  None.
  @postcon Returns the maximum output width to be used for documentation be a line feed is inserted.

  @return  an Integer

**)
Function TBADIOptions.GetMaxDocOutputWidth: Integer;

Begin
  Result := FMaxDocOutputWidth;
End;

(**

  This is a getter method for the Menu Shortcut property.

  @precon  None.
  @postcon Returns the string representation of the enumerated menu shortcut.

  @param   eBADIMenu as a TBADIMenu as a constant
  @return  a String

**)
Function TBADIOptions.GetMenuShortcut(Const eBADIMenu: TBADIMenu): String;

Begin
  Result := FBADIMenuShortCuts[eBADIMenu];
End;

(**

  This is a getter method for the Method Descriptions property.

  @precon  None.
  @postcon Returns a string list of method descriptions name value pairs (method pattern = description).

  @return  a TStringList

**)
Function TBADIOptions.GetMethodDescriptions: TStringList;

Begin
  Result := FMethodDescriptions;
End;

(**

  This is a getter method for the Module Check property.

  @precon  None.
  @postcon Returns the module check configuration for the given metric.

  @param   eModuleCheck as a TBADIModuleCheck as a constant
  @return  a TBADICheckRecord

**)
Function TBADIOptions.GetModuleCheck(Const eModuleCheck: TBADIModuleCheck): TBADICheckRecord;

Begin
  Result := FModuleChecks[eModuleCheck];
End;

(**

  This is a getter method for the Module Check Sub Ops property.

  @precon  None.
  @postcon Returns the module check sub options set.

  @return  a TBADIModuleCheckSubOps

**)
Function TBADIOptions.GetModuleCheckSubOps: TBADIModuleCheckSubOps;

Begin
  Result := FModuleCheckSubOps;
End;

(**

  This is a getter method for the Module Date Format property.

  @precon  None.
  @postcon Returns the module date format.

  @return  a String

**)
Function TBADIOptions.GetModuleDateFmt: String;

Begin
  Result := FModuleDateFmt;
End;

(**

  This is a getter method for the Module Explorer Background Colour property.

  @precon  None.
  @postcon Returns the default background colour for the explorer rendering.

  @param   boolUseIDEEditorColours as a Boolean as a constant
  @return  a TColor

**)
Function TBADIOptions.GetModuleExplorerBGColour(Const boolUseIDEEditorColours : Boolean) : TColor;

Begin
  Result := FModuleExplorerBGColour[boolUseIDEEditorColours];
End;

(**

  This is a getter method for the Module Metric property.

  @precon  None.
  @postcon Returns the module metric configuration for the given metric.

  @param   eModuleMetric as a TBADIModuleMetric as a constant
  @return  a TBADIMetricRecord

**)
Function TBADIOptions.GetModuleMetric(Const eModuleMetric: TBADIModuleMetric): TBADIMetricRecord;

Begin
  Result := FModuleMetrics[eModuleMetric];
End;

(**

  This is a getter method for the Module Metric Sub Ops property.

  @precon  None.
  @postcon Returns the module metric sub options set.

  @return  a TBADIModuleMetricSubOps

**)
Function TBADIOptions.GetModuleMetricSubOps: TBADIModuleMetricSubOps;

Begin
  Result := FModuleMetricSubOps;
End;

(**

  This is a getter method for the Module Version Increment property.

  @precon  None.
  @postcon Returns the module version increment.

  @return  a Double

**)
Function TBADIOptions.GetModuleVersionIncrement: Double;

Begin
  Result := FModuleVersionIncrement;
End;

(**

  This is a getter method for the Options property.

  @precon  None.
  @postcon Returns a set of the document options to be used for rendering module information.

  @return  a TDocOptions

**)
Function TBADIOptions.GetOptions: TDocOptions;

Begin
  Result := FOptions;
End;

(**

  This is a getter method for the Profiling Code property.

  @precon  None.
  @postcon Returns the profiling code template for the given filename.

  @param   strModuleName as a String as a constant
  @return  a String

**)
Function TBADIOptions.GetProfilingCode(Const strModuleName : String): String;

Var
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

  This is a getter method for the Project Dictionary property.

  @precon  None.
  @postcon Returns string list for the Project Dictionary

  @return  a TStringList

**)
Function TBADIOptions.GetProjectDictionary: TStringList;

Begin
  Result := FProjectDictionary;
End;

(**

  This is a getter method for the Project Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the project dictionary.

  @return  a String

**)
Function TBADIOptions.GetProjectDictionaryFile: String;

Begin
  Result := FProjectDictionaryFile;
End;

(**

  This is a getter method for the Refactor Constant New Line property.

  @precon  None.
  @postcon Returns whether a new line should be inserted for constant refactoring.

  @return  a Boolean

**)
Function TBADIOptions.GetRefactorConstNewLine: Boolean;

Begin
  Result := FRefactorConstNewLine;
End;

(**

  This is a getter method for the Scope Image List property.

  @precon  None.
  @postcon Returns the Scope Image List.

  @return  a TImageList

**)
Function TBADIOptions.GetScopeImageList: TImageList;

Begin
  Result := FScopeImageList;
End;

(**

  This is a getter method for the Scopes To Document property.

  @precon  None.
  @postcon Returns the scopes to be documented.

  @return  a TScopes

**)
Function TBADIOptions.GetScopestoDocument: TScopes;

Begin
  Result := FScopesToDocument;
End;

(**

  This is a getter method for the ScopesToRender property.

  @precon  None.
  @postcon Returns the scopes to be rendered in the module explorer.

  @return  a TScopes

**)
Function TBADIOptions.GetScopesToRender: TScopes;

Begin
  Result := FScopesToRender;
End;

(**

  This is a getter method for the Special Tags property.

  @precon  None.
  @postcon Returns a list of the special tags.

  @return  a TList<TBADISpecialTag>

**)
Function TBADIOptions.GetSpecialtags: TList<TBADISpecialTag>;

Begin
  Result := FSpecialTags;
End;

(**

  This is a getter method for the Spelling Mistake Colour property.

  @precon  None.
  @postcon Returns the colour to be used for the spelling mistake colour.

  @return  a TColor

**)
Function TBADIOptions.GetSpellingMistakeColour: TColor;

Begin
  Result := FSpellingMistakeColour;
End;

(**

  This is a getter method for the Token Font Info property.

  @precon  None.
  @postcon Returns the record information for the token type.

  @param   boolUseIDEEditorColours as a Boolean as a constant
  @return  a TBADITokenFontInfoTokenSet

**)
Function TBADIOptions.GetTokenFontInfo(
  Const boolUseIDEEditorColours : Boolean) : TBADITokenFontInfoTokenSet;

Begin
  If FRequiresIDEEditorColoursUpdating Then
    LoadIDEEditorColours;
  Result := FTokenFontInfo[boolUseIDEEditorColours];
End;

(**

  This is a getter method for the Token Limit property.
                                         
  @precon  None.
  @postcon Returns the maximum number of tokens to render in the module explorer.

  @return  an Integer

**)
Function TBADIOptions.GetTokenLimit: Integer;

Begin
  Result := FTokenLimit;
End;

(**

  This is a getter method for the Toxicity Power property.

  @precon  None.
  @postcon Returns the power to be used to calculate the toxicity of method.

  @return  an Integer

**)
Function TBADIOptions.GetToxicityPower: Integer;

Begin
  Result := FToxicityPower;
End;

(**

  This is a getter method for the Toxicity Summation property.

  @precon  None.
  @postcon Returns the type of summation to be used in the toxicity calculation.

  @return  a TBADIToxicitySummation

**)
Function TBADIOptions.GetToxicitySummation: TBADIToxicitySummation;

Begin
  Result := FToxicitySummation;
End;

(**

  This is a getter method for the Tree Colour property.

  @precon  None.
  @postcon Returns the colour to be used to render the tree in the module explorer.

  @return  a TColor

**)
Function TBADIOptions.GetTreeColour: TColor;

Begin
  Result := FTreeColour;
End;

(**

  This is a getter method for the Tree Font Name property.

  @precon  None.
  @postcon Returns the name of the font to be used for the proportional font text.

  @return  a String

**)
Function TBADIOptions.GetTreeFontName: String;

Begin
  Result := FTreeFontName;
End;

(**

  This is a getter method for the Tree Font Size property.

  @precon  None.
  @postcon Returns the font size to be used for the tree proportional font in the module explorer.

  @return  an Integer

**)
Function TBADIOptions.GetTreeFontSize: Integer;

Begin
  Result := FTreeFontSize;
End;

(**

  This is a getter method for the Update Interval property.

  @precon  None.
  @postcon Returns the update interval for refreshing the module explorer (in milliseconds).

  @return  a Cardinal

**)
Function TBADIOptions.GetUpdateInterval: Cardinal;
 
Begin
  Result := FUpdateInterval;
End;

(**

  This is a getter method for the Use IDE Editor Colours property.

  @precon  None.
  @postcon Returns whether the module explorer uses custom colours or editor colours.

  @return  a Boolean

**)
Function TBADIOptions.GetUseIDEEditorColours: Boolean;

Begin
  Result := FUseIDEEditorColours;
End;

(**

  This method loads the module checks from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The checks are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadChecks(Const iniFile: TMemIniFile);

Var
  eCheck: TBADIModuleCheck;
  eChecksSubOp: TBADIModuleCheckSubOp;

Begin
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    Begin
      FModuleChecks[eCheck].FEnabled := iniFile.ReadBool(strModuleChecks,
        ModuleChecks[eCheck].FName + strEnabled, ModuleChecks[eCheck].FEnabled);
      FModuleChecks[eCheck].FLimit := iniFile.ReadFloat(strModuleChecks,
        ModuleChecks[eCheck].FName + strLimit, ModuleChecks[eCheck].FLimit);
    End;
  FModuleCheckSubOps := [];
  For eChecksSubOp := Low(TBADIModuleCheckSubOp) To High(TBADIModuleCheckSubOp) Do
    If iniFile.ReadBool(strModuleChecks, ModuleCheckSubOps[eChecksSubOp].FName + strEnabled, True) Then
      Include(FModuleCheckSubOps, eChecksSubOp);
End;

(**

  This method loads the comment tag names associated with the the file extensions in to the string list.

  @precon  None.
  @postcon The file extension = tag name pairs are loaded into the string list.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadCommentTags(Const iniFile: TMemIniFile);

Const
  strDefaultTagName = 'todo';

Var
  sl : TStringList;
  strTagName: String;
  
Begin
  sl := TstringList.Create;
  Try
    iniFile.ReadSection(strCommentTagNamesINISection, sl);
    For strTagName In sl Do
      FCommentTagNames.AddPair(
        strTagName,
        iniFile.ReadString(strCommentTagNamesINISection, strTagName, strDefaultTagName)
      );
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the comment types associated with file extensions.

  @precon  None.
  @postcon The comment types associated with file extensions are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadCommentTypes(Const iniFile: TMemIniFile);

Var
  sl: TStringList;
  strTagName: String;

Begin
  sl := TstringList.Create;
  Try
    iniFile.ReadSection(strCommentTypesINISection, sl);
    For strTagName In sl Do
      FCommentTypes.AddObject(
        strTagName,
        TObject(iniFile.ReadInteger(strCommentTypesINISection, strTagName, Integer(ctNone)))
      );
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the dictionary filenames from the INI File and loads the dictionaries into string
  lists.

  @precon  None.
  @postcon The dictionary filenames are set and the dictionaries loaded if the filenames are valid.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadDictionaries(Const iniFile: TMemIniFile);

Begin
  SetLanguageDictionaryFile(iniFile.ReadString(strSetup, strLanguageDictionaryINIKey, ''));
  SetLocalDictionaryFile(iniFile.ReadString(strSetup, strLocalDictionaryINIKey, ''));
  SetIgnoreDictionaryFile(iniFile.ReadString(strSetup, strIgnoreDictionaryINIKey, ''));
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

  This method loads the exclusions from the inifile. If old documentation exclusions exist they are
  migrated to the new system.

  @precon  iniFile must be a valid instance.
  @postcon The exclusions are loaded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadExclusions(Const iniFile: TMemIniFile);

Var
  sl : TStringList;
  recExclusion: TBADIExclusionRec;
  i: Integer;

Begin
  sl := TstringList.Create;
  Try
    If iniFile.ValueExists(strSetup, strExcludeDocFiles) Then
      Begin // Upgrade old exclusions
        sl.Text := StringReplace(iniFile.ReadString(strSetup, strExcludeDocFiles, ''), '|',
          #13#10, [rfReplaceAll]);
        For i := 0 To sl.Count - 1 Do
          Begin
            recExclusion.FExclusionPattern := sl[i];
            If Length(recExclusion.FExclusionPattern) > 0 Then
              Begin
                // Remove starting *
                If recExclusion.FExclusionPattern[1] = '*' Then
                  Delete(recExclusion.FExclusionPattern, 1, 1);
                // Remove ending *
                If recExclusion.FExclusionPattern[Length(recExclusion.FExclusionPattern)] = '*' Then
                  Delete(recExclusion.FExclusionPattern, Length(recExclusion.FExclusionPattern), 1);
                // Updated inner * to .* for regex
                recExclusion.FExclusionPattern := StringReplace(recExclusion.FExclusionPattern, '*',
                   '.*', [rfReplaceAll]);
                // Updated \ for RegEx
                recExclusion.FExclusionPattern := StringReplace(recExclusion.FExclusionPattern, '\',
                   '\\', [rfReplaceAll]);
              End;
            recExclusion.FExclusions := [etDocumentation];
            FExclusions.Add(recExclusion);
          End;
        SaveExclusions(iniFile);
        iniFile.DeleteKey(strSetup, strExcludeDocFiles);
        iniFile.UpdateFile;
      End Else
      Begin // Load new exclusions
        iniFile.ReadSection(strExclusions, sl);
        iniFile.ReadSection(strExclusions, sl);
        For i := 0 To sl.Count - 1 Do
          Begin
            recExclusion.FExclusionPattern := sl[i];
            recExclusion.FExclusions := TBADIExclusionTypes(Byte(iniFile.ReadInteger(strExclusions,
              sl[i], 0)));
            FExclusions.Add(recExclusion);
          End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the file extensions from the INI file that are associated with the parsers.

  @precon  iniFile must be a valid instance.
  @postcon The extensions are loaded from the INI file.

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

  This method loads the IDE Editor Colours using the given interface.

  @precon  None.
  @postcon The IDEs Editor Colours are loaded.

**)
Procedure TBADIOptions.LoadIDEEditorColours;
var
  iBGColour: TColor;

Begin
  If Assigned(FIDEEditorColours) Then
    Begin
      FTokenFontInfo[True] := FIDEEditorColours.GetIDEEditorColours(iBGColour);
      FModuleExplorerBGColour[True] := iBGColour;
      FTokenFontInfo[True][ttTreeHeader] := FTokenFontInfo[False][ttTreeHeader];
    End;
  FRequiresIDEEditorColoursUpdating := False;
End;

(**

  This method loads the limit information from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The limit information is loaded.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadLimits(Const iniFile: TMemIniFile);

Const
  iDefaultLimit = 10;

Var
  eLimitType : TLimitType;

Begin
  For eLimitType := Low(TLimitType) To High(TLimitType) Do
    FIssueLimits[eLimitType] :=
      iniFile.ReadInteger(strIssuesLimits, astrLimitType[eLimitType], iDefaultLimit);
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
  @postcon The method descriptions are loaded.

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
  @postcon The metrics are loaded from the INI file.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.LoadMetrics(Const iniFile: TMemIniFile);

Const
  iDefaultLowLimit = 95;
  iDefaultHighLimit = 105;
  iDefaultToxicityPower = 3;

Var
  eMetric: TBADIModuleMetric;
  eMetricSubOp: TBADIModuleMetricSubOp;

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
  FModuleMetricSubOps := [];
  For eMetricSubOp := Low(TBADIModuleMetricSubOp) To High(TBADIModuleMetricSubOp) Do
    If iniFile.ReadBool(strModuleMetrics, ModuleMetricSubOps[eMetricSubOp].FName + strEnabled, True) Then
      Include(FModuleMetricSubOps, eMetricSubOp);
  FToxicityPower := iniFile.ReadInteger(strModuleMetrics, strToxicityPower, iDefaultToxicityPower);
  FToxicitySummation := TBADIToxicitySummation(iniFile.ReadInteger(strModuleMetrics, strToxicitySummation,
    Integer(tsAddBeforePower)));
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
      FTokenFontInfo[False, T].FForeColour := StringToColor(iniFile.ReadString(
        strTokenFontInfo,
        Format(strFontColour, [strTokenType[T]]),
        ColorToString(strTokenTypeInfo[T].FForeColour)
      ));
      FTokenFontInfo[False, T].FStyles := TFontStyles(Byte(iniFile.ReadInteger(
        strTokenFontInfo,
        Format(strFontStyles, [strTokenType[T]]),
        Byte(strTokenTypeInfo[T].FStyles)
      )));
      FTokenFontInfo[False, T].FBackColour := StringToColor(iniFile.ReadString(
        strTokenFontInfo,
        Format(strFontBackColour, [strTokenType[T]]),
        ColorToString(strTokenTypeInfo[T].FBackColour)
      ));
    End;
  FUseIDEEditorColours := iniFile.ReadBool(strModuleExplorer, strUseIDEEditorColours, False);
  FModuleExplorerBGColour[False] := StringToColor(iniFile.ReadString(strModuleExplorer, strBGColour,
    ColorToString(clWindow)));
  FTokenLimit := iniFile.ReadInteger(strModuleExplorer, strTokenLimit, iDefaultTokenLimit);
  FMaxDocOutputWidth := iniFile.ReadInteger(strDocumentation, strMaxDocOutputWidth, iDefaultMaxDocumentationWidth);
  FManagedNodesLife := iniFile.ReadInteger(strModuleExplorer, strManagedNodesLife, iDefaultNodeLifeInDays);
  FTreeColour := StringToColor(iniFile.ReadString(strModuleExplorer, strTreeColour, strFDefaultTreeColour));
End;

(**

  This method loads the profiling options from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The profiling options are loaded.

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

  This method loads the applications settings from an INI file.

  @precon  None.
  @postcon Loads the applications settings from an INI file.

**)
Procedure TBADIOptions.LoadSettings;

Const
  strDefaultDateFmt = 'dd mmm yyyy';
  dblDefaultIncrement = 0.00001;
  strDefaultSpellingMistakeColour = 'clRed';

Var
  iniFile : TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FINIFileName);
  Try
    LoadDocOptions(iniFile);
    LoadSpecialTags(iniFile);
    LoadManagedNodes(iniFile);
    LoadModuleExplorerOptions(iniFile);
    LoadExclusions(iniFile);
    LoadMethodDescriptions(iniFile);
    FScopesToDocument :=
      TScopes(Byte(iniFile.ReadInteger(strDocumentation, strScopes, Byte(FScopesToDocument))));
    LoadProfilingOptions(iniFile);
    LoadLimits(iniFile);
    LoadShortcuts(iniFile);
    LoadExtensions(iniFile);
    LoadMetrics(iniFile);
    LoadChecks(iniFile);
    LoadDictionaries(iniFile);
    LoadCommentTags(iniFile);
    LoadCommentTypes(iniFile);
    UpdateDoNotFollowEditor();
    FRefactorConstNewLine := iniFile.ReadBool(strRefactorings, strNewLine, True);
    FModuleDateFmt := iniFile.ReadString(strAutomaticModuleUpdatesINISection, strDateFormatINIKey,
      strDefaultDateFmt);
    FModuleVersionIncrement := iniFile.ReadFloat(strAutomaticModuleUpdatesINISection, strIncrementINIKey,
      dblDefaultIncrement);
    FSpellingMistakeColour := StringToColor(iniFile.ReadString(strSetup, strSpellingMistakeColourINIKey,
      strDefaultSpellingMistakeColour));
  Finally
    iniFile.Free;
  End;
End;

(**

  This method loads the shortcuts for the menu options from the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The shortcuts are loaded.

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

  This method loads the special tag information from the INI file.

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
        iSTIndex := FSpecialTags.Add(
          TBADISpecialTag.Create(
            sl[iTag],
            iniFile.ReadString(strSpecialTagNames, sl[iTag], ''),
            TBADITagProperties(Byte(iniFile.ReadInteger(strSpecialTags, sl[iTag], 0))),
            TBADIImageIndex(iniFile.ReadInteger(strSpecialTagImageIndexes, sl[iTag], Integer(iiNone)))
          )
        );
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

  This method forces the release of the interface reference to allow the object to be destroyed.

  @precon  None.
  @postcon the interface is freed and thus destroyed.

**)
Class Procedure TBADIOptions.Release;

Begin
  FBADIOptionsInstance := Nil;
End;

(**

  This method registers that the options needs to reload the IDE Editor Colours. This cannot be done in
  the SaveSettings method as the IDE has not necessarily saved the Editor Colour changes at that time.

  @precon  None.
  @postcon The marker is updated so that the next call to GetTokenFontInfo will update the cached IDE
           Editor Colours.

**)
Procedure TBADIOptions.RequiresIDEEditorColoursUpdate;

Begin
  FRequiresIDEEditorColoursUpdating := True;
End;

(**

  This method saves the checks to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The check settings are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveChecks(Const iniFile: TMemIniFile);

Var
  eCheck: TBADIModuleCheck;
  eCheckSubOp: TBADIModuleCheckSubOp;

Begin
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    Begin
      iniFile.WriteBool(strModuleChecks, ModuleChecks[eCheck].FName + strEnabled,
        FModuleChecks[eCheck].FEnabled);
      iniFile.WriteFloat(strModuleChecks, ModuleChecks[eCheck].FName + strLimit,
        FModuleChecks[eCheck].FLimit);
    End;
  For eCheckSubOp := Low(TBADIModuleCheckSubOp) To High(TBADIModuleCheckSubOp) Do
    iniFile.WriteBool(strModuleChecks, ModuleCheckSubOps[eCheckSubOp].FName + strEnabled,
      eCheckSubOp In FModuleCheckSubOps);
End;

(**

  This method saves the comment tags associated with a file extension to the INI file.

  @precon  None.
  @postcon The comment tag associated with a file extension are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveCommentTags(Const iniFile: TMemIniFile);

Var
  iTag : Integer;
  
Begin
  For iTag := 0 To FCommentTagNames.Count - 1 Do
    iniFile.WriteString(
      strCommentTagNamesINISection,
      FCommentTagNames.Names[iTag],
      FCommentTagNames.ValueFromIndex[iTag]
    );  
End;

(**

  This method saves the comment types associated with a file extension to the INI file.

  @precon  None.
  @postcon The comment types associated with a file extension are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveCommentTypes(Const iniFile: TMemIniFile);

Var
  iTag: Integer;

Begin
  For iTag := 0 To FCommentTypes.Count - 1 Do
    iniFile.WriteInteger(
      strCommentTypesINISection,
      FCommentTypes[iTag],
      Integer(FCommentTypes.Objects[iTag])
    );  
End;

(**

  This method saves the dictionary filenames and dictionaries.

  @precon  None.
  @postcon Saves the dictionary filenames and dictionaries.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveDictionaries(Const iniFile: TMemIniFile);

Begin
  iniFile.WriteString(strSetup, strLanguageDictionaryINIKey, FLanguageDictionaryFile);
  iniFile.WriteString(strSetup, strLocalDictionaryINIKey, FLocalDictionaryFile);
  If FileExists(FLocalDictionaryFile) Then
    FLocalDictionary.SaveToFile(FLocalDictionaryFile);
  iniFile.WriteString(strSetup, strIgnoreDictionaryINIKey, FIgnoreDictionaryFile);
  If FileExists(FIgnoreDictionaryFile) Then
    FIgnoreDictionary.SaveToFile(FIgnoreDictionaryFile);
  If DirectoryExists(ExtractFilePath(FProjectDictionaryFile) )Then
    FProjectDictionary.SaveToFile(FProjectDictionaryFile);
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

  This method saves the new exclusions for documentation, metrics and checks to the INI file.

  @precon  iniFile must be a valid iniFile instance.
  @postcon The exclusions are saved to the INI File.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveExclusions(Const iniFile: TMemIniFile);

Var
  i : Integer;
  
Begin
  iniFile.EraseSection(strExclusions);
  For i := 0 To FExclusions.Count - 1 Do
    iniFile.WriteInteger(
      strExclusions,
      FExclusions[i].FExclusionPattern,
      Byte(FExclusions[i].FExclusions)
    );
End;

(**

  This methods saves the extensions associated with parsers to the INI file.

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

  This method saves the limit information to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The limit information is saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveLimits(Const iniFile: TMemIniFile);

Var
  eLimitType : TLimitType;

Begin
  For eLimitType := Low(TLimitType) To High(TLimitType) Do
    iniFile.WriteInteger(strIssuesLimits, astrLimitType[eLimitType], FIssueLimits[eLimitType]);
End;

(**

  This method save the managed nodes to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The managed nodes are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveManagedNodes(Const iniFile: TMemIniFile);

Var
  iNode: Integer;

Begin
  iniFile.EraseSection(strManagedExpandedNodes);
  For iNode := 0 To GetExpandedNodes.Count - 1 Do
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
  @postcon The metric settings are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveMetrics(Const iniFile: TMemIniFile);

Var
  eMetric: TBADIModuleMetric;
  eMetricSubOp: TBADIModuleMetricSubOp;

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
  For eMetricSubOp := Low(TBADIModuleMetricSubOp) To High(TBADIModuleMetricSubOp) Do
    iniFile.WriteBool(strModuleMetrics, ModuleMetricSubOps[eMetricSubOp].FName + strEnabled,
      eMetricSubOp In FModuleMetricSubOps);
  iniFile.WriteInteger(strModuleMetrics, strToxicityPower, FToxicityPower);
  iniFile.WriteInteger(strModuleMetrics, strToxicitySummation, Integer(FToxicitySummation))
End;

(**

  This method saves the module explorer options to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The module explorer options are saved.

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
      iniFile.WriteString(
        strTokenFontInfo,
        Format(strFontColour, [strTokenType[T]]),
        ColorToString(FTokenFontInfo[False, T].FForeColour)
      );
      iniFile.WriteInteger(
        strTokenFontInfo,
        Format(strFontStyles, [strTokenType[T]]),
        Byte(FTokenFontInfo[False, T].FStyles)
      );
      iniFile.WriteString(
        strTokenFontInfo,
        Format(strFontBackColour, [strTokenType[T]]),
        ColorToString(FTokenFontInfo[False, T].FBackColour)
      );
    End;
  iniFile.WriteBool(strModuleExplorer, strUseIDEEditorColours, FUseIDEEditorColours);
  iniFile.WriteString(strModuleExplorer, strBGColour, ColorToString(FModuleExplorerBGColour[False]));
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

  This method saves the applications settings to an INI file.

  @precon  None.
  @postcon Saves the applications settings to an INI file.

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
    SaveExclusions(iniFile);
    SaveMethodDescrpitions(iniFile);
    iniFile.WriteInteger(strDocumentation, strScopes, Byte(FScopesToDocument));
    SaveProfilingOptions(iniFile);
    SaveLimits(iniFile);
    SaveShortcuts(iniFile);
    SaveExtensions(iniFile);
    SaveMetrics(iniFile);
    SaveChecks(iniFile);
    SaveDictionaries(iniFile);
    SaveCommentTags(iniFile);
    SaveCommentTypes(iniFile);
    iniFile.WriteBool(strRefactorings, strNewLine, FRefactorConstNewLine);
    iniFile.WriteString(strAutomaticModuleUpdatesINISection, strDateFormatINIKey, FModuleDateFmt);
    iniFile.WriteFloat(strAutomaticModuleUpdatesINISection, strIncrementINIKey, FModuleVersionIncrement);
    iniFile.WriteString(strSetup, strSpellingMistakeColourINIKey, ColorToString(FSpellingMistakeColour));
    UpdateDoNotFollowEditor();
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

  This method saves the special tags to the INI file.

  @precon  iniFile must be a valid instance.
  @postcon The special tags are saved.

  @param   iniFile as a TMemIniFile as a constant

**)
Procedure TBADIOptions.SaveSpecialTags(Const iniFile: TMemIniFile);

Var
  iTag: Integer;
  ST: TBADISpecialTag;

Begin
  iniFile.EraseSection(strSpecialTags);
  iniFile.EraseSection(strSpecialTagNames);
  For iTag := 0 To FSpecialTags.Count - 1 Do
    Begin
      ST := FSpecialTags[iTag];
      iniFile.WriteInteger(strSpecialTags, ST.FName, Byte(ST.FTagProperties));
      iniFile.WriteString(strSpecialTagNames, ST.FName, ST.FDescription);
      iniFile.WriteInteger(strSpecialTagFontStyles, ST.FName, Byte(ST.FFontStyles));
      iniFile.WriteString(strSpecialTagFontForeColours, ST.FName, ColorToString(ST.FFontColour));
      iniFile.WriteString(strSpecialTagFontBackColours, ST.FName, ColorToString(ST.FBackColour));
      iniFile.WriteInteger(strSpecialTagImageIndexes, ST.FName, Integer(ST.FIconImage));
    End;
End;

(**

  This is a setter method for the Browse Position property.

  @precon  None.
  @postcon Sets the browsing position to be used.

  @param   eBrowsePosition as a TBrowsePosition as a constant

**)
Procedure TBADIOptions.SetBrowsePosition(Const eBrowsePosition: TBrowsePosition);

Begin
  FBrowsePosition := eBrowsePosition;
End;

(**

  This is a setter method for the Comment Tag Name property.

  @precon  None.
  @postcon Sets the tag name for the given file extension.

  @param   strExt   as a String as a constant
  @param   strValue as a String as a constant

**)
Procedure TBADIOptions.SetCommentTagName(Const strExt, strValue: String);

Var
  iIndex: Integer;

Begin
  iIndex := FCommentTagNames.IndexOfName(strExt);
  If iIndex > -1 Then
    FCommentTagNames.Values[strExt] := strValue
  Else
    FCommentTagNames.AddPair(strExt, strValue);
End;

(**

  This is a setter method for the Comment Types property.

  @precon  None.
  @postcon Sets the comments type associated with the given extension.

  @param   strExt as a String as a constant
  @param   eValue as a TCommentType as a constant

**)
Procedure TBADIOptions.SetCommentType(Const strExt: String; Const eValue: TCommentType);

Var
  iIndex: Integer;

Begin
  iIndex := FCommentTypes.IndexOf(strExt);
  If iIndex > -1 Then
    FCommentTypes.Objects[iIndex] := TObject(eValue)
  Else
    FCommentTypes.AddObject(strExt, TObject(eValue));
End;

(**

  This is a setter method for the Do Not Follow Editor property.

  @precon  None.
  @postcon Set the set of limits which should prevent the module explorer following the editor.

  @param   setLimitTypes as a TLimitTypes as a constant

**)
Procedure TBADIOptions.SetDoNotFollowEditor(Const setLimitTypes : TLimitTypes);

Begin
  FDoNotFollowEditor := setLimitTypes;
End;

(**

  This is a setter method for the Fixed Font Name property.

  @precon  None.
  @postcon Sets the name of the fixed font to be used.

  @param   strFontName as a String as a constant

**)
Procedure TBADIOptions.SetFixedFontName(Const strFontName: String);

Begin
  FFixedFontName := strFontName;
End;

(**

  This is a setter method for the Fixed Font Size property.

  @precon  None.
  @postcon Sets the size of the fixed font to be used.

  @param   iFontSize as an Integer as a constant

**)
Procedure TBADIOptions.SetFixedFontSize(Const iFontSize: Integer);

Begin
  FFixedFontSize := iFontSize;
End;

(**

  This is a setter method for the High Metric Margin property.

  @precon  None.
  @postcon Sets the value of the high metric margin over which issues should be highlighted and below
           which should be highlighted as marginal.

  @param   dblMargin as a Double as a constant

**)
Procedure TBADIOptions.SetHighMetricMargin(Const dblMargin: Double);

Begin
  FHighMetricMargin := dblMargin;
End;

(**

  This is a setter method for the Ignore Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the ignore dictionary.

  @param   strValue as a String as a constant

**)
Procedure TBADIOptions.SetIgnoreDictionaryFile(Const strValue: String);

Begin
  If FileExists(FIgnoreDictionaryFile) Then
    FIgnoreDictionary.SaveToFile(FIgnoreDictionaryFile);
  FIgnoreDictionaryFile := strValue;
  If FileExists(FIgnoreDictionaryFile) Then
    FIgnoreDictionary.LoadFromFile(FIgnoreDictionaryFile);
End;

(**

  This is a setter method for the Issue Limit property.

  @precon  None.
  @postcon Sets the numerical limit for the given limit type.

  @param   eLimitType as a TLimitType as a constant
  @param   iLimit as an Integer as a constant

**)
Procedure TBADIOptions.SetIssueLimit(Const eLimitType: TLimitType; Const iLimit: Integer);

Begin
  FIssueLimits[eLimitType] := iLimit;
End;

(**

  This is a setter method for the Language Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the language dictionary.

  @param   strValue as a String as a constant

**)
Procedure TBADIOptions.SetLanguageDictionaryFile(Const strValue: String);

Begin
  If CompareText(FLanguageDictionaryFile, strValue) <> 0 Then
    Begin
      FLanguageDictionaryFile := strValue;
      If FileExists(FLanguageDictionaryFile) Then
        FLanguageDictionary.LoadFromFile(FLanguageDictionaryFile);
    End;
End;

(**

  This is a setter method for the Local Dictionary File property.

  @precon  None.
  @postcon Returns the filename of the local dictionary.

  @param   strValue as a String as a constant

**)
Procedure TBADIOptions.SetLocalDictionaryFile(Const strValue: String);

Begin
  If CompareText(FLocalDictionaryFile, strValue) <> 0 Then
    Begin
      If FileExists(FLocalDictionaryFile) Then
        FLocalDictionary.SaveToFile(FLocalDictionaryFile);
      FLocalDictionaryFile := strValue;
      If FileExists(FLocalDictionaryFile) Then
        FLocalDictionary.LoadFromFile(FLocalDictionaryFile);
    End;
End;

(**

  This is a setter method for the Low Metric Margin property.

  @precon  None.
  @postcon Sets the valid of the low margin for metrics below which is okay and above should be
           highlighted.

  @param   dblMargin as a Double as a constant

**)
Procedure TBADIOptions.SetLowMetricMargin(Const dblMargin: Double);

Begin
  FLowMetricMargin := dblMargin;
End;

(**

  This is a setter method for the Managed Nodes Life property.

  @precon  None.
  @postcon Sets the value in days for how long a managed node should be kept.

  @param   iNodeLife as an Integer as a constant

**)
Procedure TBADIOptions.SetManagedNodesLife(Const iNodeLife: Integer);

Begin
  FManagedNodesLife := iNodeLife;
End;

(**

  This is a setter method for the Max Document Output Width property.

  @precon  None.
  @postcon Sets the maximum width of documentation output beyond which the information should be wrapped.

  @param   iMaxDocOutputWidth as an Integer as a constant

**)
Procedure TBADIOptions.SetMaxDocOutputWidth(Const iMaxDocOutputWidth: Integer);

Begin
  FMaxDocOutputWidth := iMaxDocOutputWidth;
End;

(**

  This is a setter method for the Menu Shortcut property.

  @precon  None.
  @postcon Sets the string representation of the enumerated menu shortcut.

  @param   eBADIMenu   as a TBADIMenu as a constant
  @param   strShortcut as a String as a constant

**)
Procedure TBADIOptions.SetMenuShortcut(Const eBADIMenu: TBADIMenu;
  Const strShortcut: String);

Begin
  If FBADIMenuShortCuts[eBADIMenu] <> strShortcut Then
    FBADIMenuShortCuts[eBADIMenu] := strShortcut;
End;

(**

  This is a setter method for the Module Check property.

  @precon  None.
  @postcon Sets the module check configuration.

  @param   eModuleCheck as a TBADIModuleCheck as a constant
  @param   recCheck     as a TBADICheckRecord as a constant

**)
Procedure TBADIOptions.SetModuleCheck(Const eModuleCheck: TBADIModuleCheck;
  Const recCheck: TBADICheckRecord);

Begin
  FModuleChecks[eModuleCheck] := recCheck;
End;

(**

  This is a setter method for the Module Check Sub Ops property.

  @precon  None.
  @postcon Sets the module check sub-options set.

  @param   setModuleCheckSubOps as a TBADIModuleCheckSubOps as a constant

**)
Procedure TBADIOptions.SetModuleCheckSubOps(Const setModuleCheckSubOps: TBADIModuleCheckSubOps);

Begin
  FModuleCheckSubOps := setModuleCheckSubOps;
End;

(**

  This is a setter method for the Module Date Format property.

  @precon  None.
  @postcon Updates the module date format.

  @param   strValue as a String as a constant

**)
Procedure TBADIOptions.SetModuleDateFmt(Const strValue: String);

Begin
  FModuleDateFmt := strValue;
End;

(**

  This is a setter method for the Module Explorer Background Colour property.

  @precon  None.
  @postcon Sets the background colours of the module explorer.

  @param   boolUseIDEEditorColours as a Boolean as a constant
  @param   iColour                 as a TColor as a constant

**)
Procedure TBADIOptions.SetModuleExplorerBGColour(Const boolUseIDEEditorColours : Boolean;
  Const iColour : TColor);

Begin
  FModuleExplorerBGColour[boolUseIDEEditorColours] := iColour;
End;

(**

  This is a setter method for the Module Metric property.

  @precon  None.
  @postcon Sets the module metric configuration.

  @param   eModuleMetric as a TBADIModuleMetric as a constant
  @param   recMetric     as a TBADIMetricRecord as a constant

**)
Procedure TBADIOptions.SetModuleMetric(Const eModuleMetric: TBADIModuleMetric;
  Const recMetric: TBADIMetricRecord);

Begin
  FModuleMetrics[eModuleMetric] := recMetric;
End;

(**

  This is a setter method for the Module Metric Sub Ops property.

  @precon  None.
  @postcon Sets the value of the Module Metric sub-options set.

  @param   setModuleMetricSubOps as a TBADIModuleMetricSubOps as a constant

**)
Procedure TBADIOptions.SetModuleMetricSubOps(Const setModuleMetricSubOps: TBADIModuleMetricSubOps);

Begin
  FModuleMetricSubOps := setModuleMetricSubOps;
End;

(**

  This is a setter method for the Module Version Increment property.

  @precon  None.
  @postcon Updates the module version increment.

  @param   dblValue as a Double as a constant

**)
Procedure TBADIOptions.SetModuleVersionIncrement(Const dblValue: Double);

Begin
  FModuleVersionIncrement := dblValue;
End;

(**

  This is a setter method for the Options property.

  @precon  None.
  @postcon Sets the main Documentation options set.

  @param   setOptions as a TDocOptions as a constant

**)
Procedure TBADIOptions.SetOptions(Const setOptions: TDocOptions);

Begin
  FOptions := setOptions;
End;

(**

  This is a setter method for the Profiling Code property.

  @precon  None.
  @postcon saves the profiling code for the given filename.

  @param   strModuleName  as a String as a constant
  @param   strProfileCode as a String as a constant

**)
Procedure TBADIOptions.SetProfilingCode(Const strModuleName : String; Const strProfileCode: String);

Begin
  FProfilingCode.Values[strModuleName] := StringReplace(strProfileCode, #13#10, '|', [rfReplaceAll]);
End;

(**

  This is a setter method for the Project Dictionary File property.

  @precon  None.
  @postcon If the filename has changed the old file is saved (if it exists) and the new file is loaded.

  @param   strValue as a String as a constant

**)
Procedure TBADIOptions.SetProjectDictionaryFile(Const strValue: String);

Begin
  If CompareText(FProjectDictionaryFile, strValue) <> 0 Then
    Begin
      If DirectoryExists(ExtractFilePath(FProjectDictionaryFile)) Then
        FProjectDictionary.SaveToFile(FProjectDictionaryFile);
      FProjectDictionaryFile := strValue;
      If FileExists(FProjectDictionaryFile) Then
        FProjectDictionary.LoadFromFile(FProjectDictionaryFile);
    End;
End;

(**

  This is a setter method for the Refactor Constant New Line property.

  @precon  None.
  @postcon Sets whether a new line should be added when inserted a constant refactoring.

  @param   boolNewLine as a Boolean as a constant

**)
Procedure TBADIOptions.SetRefactorConstNewLine(Const boolNewLine: Boolean);

Begin
  FRefactorConstNewLine := boolNewLine;
End;

(**

  This is a setter method for the Scopes To Document property.

  @precon  None.
  @postcon Sets the value of the scopes to document set.

  @param   setScopes as a TScopes as a constant

**)
Procedure TBADIOptions.SetScopesToDocument(Const setScopes: TScopes);

Begin
  FScopesToDocument := setScopes;
End;

(**

  This is a setter method for the Scopes To Render property.

  @precon  None.
  @postcon Sets the value of the scopes to Render set.

  @param   setScopes as a TScopes as a constant

**)
Procedure TBADIOptions.SetScopesToRender(Const setScopes: TScopes);

Begin
  FScopesToRender := setScopes;
End;

(**

  This is a setter method for the Spelling Mistake Colour property.

  @precon  None.
  @postcon Sets the colour to be used for spelling mistakes in the editor.

  @param   iColour as a TColor as a constant

**)
Procedure TBADIOptions.SetSpellingMistakeColour(Const iColour: TColor);

Begin
  FSpellingMistakeColour := iColour;
End;

(**

  This is a setter method for the Token Font Info property.

  @precon  None.
  @postcon Sets the indexed Token Font Information record.

  @param   boolUseIDEEditorColours as a Boolean as a constant
  @param   TokenFontInfo           as a TBADITokenFontInfoTokenSet as a constant

**)
Procedure TBADIOptions.SetTokenFontInfo(Const boolUseIDEEditorColours : Boolean;
  Const TokenFontInfo : TBADITokenFontInfoTokenSet);

Begin
  FTokenFontInfo[boolUseIDEEditorColours] := TokenFontInfo;
End;

(**

  This is a setter method for the Token Limit property.

  @precon  None.
  @postcon Sets the limit for the tokens to be rendered.

  @param   iTokenLimit as an Integer as a constant

**)
Procedure TBADIOptions.SetTokenLimit(Const iTokenLimit: Integer);

Begin
  FTokenLimit := iTokenLimit;
End;

(**

  This is a setter method for the Toxicity Power property.

  @precon  None.
  @postcon Sets the polynomial power to be used to combine metrics for the toxicity calculation.

  @param   iPower as an Integer as a constant

**)
Procedure TBADIOptions.SetToxicityPower(Const iPower: Integer);

Begin
  FToxicityPower := iPower;
End;

(**

  This is a setter method for the Toxicity Summation property.

  @precon  None.
  @postcon Sets the enumerate value which determines how the metrics are combined for the toxicity
           calculation.

  @param   eToxicitySummartion as a TBADIToxicitySummation as a constant

**)
Procedure TBADIOptions.SetToxicitySummation(Const eToxicitySummartion: TBADIToxicitySummation);

Begin
  FToxicitySummation := eToxicitySummartion;
End;

(**

  This is a setter method for the Tree Colour property.

  @precon  None.
  @postcon Sets the colour of the module explorer tree.

  @param   iColour as a TColor as a constant

**)
Procedure TBADIOptions.SetTreeColour(Const iColour: TColor);

Begin
  FTreeColour := iColour;
End;

(**

  This is a setter method for the Tree Font Name property.

  @precon  None.
  @postcon Sets the font name for the proportional font used in the tree.

  @param   strFontName as a String as a constant

**)
Procedure TBADIOptions.SetTreeFontName(Const strFontName: String);

Begin
  FTreeFontName := strFontName;
End;

(**

  This is a setter method for the Tree Font Size property.

  @precon  None.
  @postcon Sets the font size for the proportional font used in the tree.

  @param   iFontSize as a Integer as a constant

**)
Procedure TBADIOptions.SetTreeFontSize(Const iFontSize: Integer);

Begin
  FTreeFontSize := iFontSize;
End;

(**

  This is a setter method for the Update Interval property.

  @precon  None.
  @postcon Sets the update interval for the module explorer in milliseconds.

  @param   iInterval as a Cardinal as a constant

**)
Procedure TBADIOptions.SetUpdateInterval(Const iInterval: Cardinal);

Begin
  FUpdateInterval := iInterval;
End;

(**

  This is a setter method for the Use IDE Editor Colours property.

  @precon  None.
  @postcon Sets whether the IDE Editor colours should b e used for the module explorer or custom colours.

  @param   boolUseIDEEditorColours as a Boolean as a constant

**)
Procedure TBADIOptions.SetUseIDEEditorColours(Const boolUseIDEEditorColours: Boolean);

Begin
  FUseIDEEditorColours := boolUseIDEEditorColours;
End;

(**

  This method updates the Do not Follow Editor property based on the documentation options.

  @precon  None.
  @postcon The Do Not Follow Editor options are updated.

**)
Procedure TBADIOptions.UpdateDoNotFollowEditor();

Const
  aLimitOptions : Array[TLimitType] Of TDocOption = (
    doDoNotFollowEditorIfErrors,
    doDoNotFollowEditorIfWarnings,
    doDoNotFollowEditorIfHints,
    doDoNotFollowEditorIfConflicts,
    doDoNotFollowEditorIfChecks,
    doDoNotFollowEditorIfMetrics,
    doDoNotFollowEditorIfSpelling
  );

Var
  eDocIssue: TLimitType;
  
Begin
  FDoNotFollowEditor := [];
  For eDocIssue := Low(TLimitType) To High(TLimitType) Do
    If aLimitOptions[eDocIssue] In FOptions Then
      Include(FDoNotFollowEditor, eDocIssue);
End;

End.
