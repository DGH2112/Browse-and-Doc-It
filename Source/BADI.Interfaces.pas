(**

  This module contains interfaces for use throughout Browse and Doc It.

  @Author  David Hoyle
  @Version 2.439
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
Unit BADI.Interfaces;

Interface

Uses
  System.Classes,
  System.Generics.Collections,
  VCL.Graphics,
  VCL.Controls,
  BADI.Types;

Type
  (** An interface for the new global exclusions for documents, metrics and checks **)
  IBADIExclusions = Interface
  ['{58DC7B95-9F86-4831-9536-16F3DB859228}']
    // Getters and Setters
    Function  GetExclusion(Const iIndex : Integer) : TBADIExclusionRec;
    Procedure SetExclusion(Const iIndex : Integer; Const recValue : TBADIExclusionRec);
    Function  GetCount : Integer;
    // General Methods
    Function  ShouldExclude(Const strFileName : String;
      Const eExclusionType : TBADIExclusionType) : Boolean;
    Procedure Add(Const recExclusion : TBADIExclusionRec);
    Procedure Delete(Const iIndex : Integer);
    Procedure Clear;
    // Properties
    (**
      This method returns the exclusion record for the given index.
      @precon  iIndex must be between 0 and Count - 1.
      @postcon The exclusion record is returned.
      @param   iIndex as an Integer as a constant
      @return  a TBADIExclusionRec
    **)
    Property Exclusion[Const iIndex : Integer] : TBADIExclusionRec Read GetExclusion Write SetExclusion;
      Default;
    (**
      This property returns the number of items in the collection.
      @precon  None.
      @postcon The number of items in the exclusions collection is returned.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
  End;

  (** An interface for accessing Browse and Doc Its global settings. **)
  IBADIOptions = Interface
  ['{413040CC-BEA3-4C3F-AD10-A2AB656B646B}']
    // Getters adn Setters
    Function  GetOptions : TDocOptions;
    Procedure SetOptions(Const Options : TDocOptions);
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
      Const eTokenFontInfo : TBADITokenFontInfoTokenSet);
    Function  GetExclusions : IBADIExclusions;
    Function  GetMethodDescriptions : TStringList;
    Function  GetScopestoDocument : TScopes;
    Procedure SetScopesToDocument(Const setScopesToDocument : TScopes);
    Function  GetModuleExplorerBGColour(Const boolUseIDEEditorColours : Boolean) : TColor;
    Procedure SetModuleExplorerBGColour(Const boolUseIDEEditorColours : Boolean; Const iColour : TColor);
    Function  GetTokenLimit : Integer;
    Procedure SetTokenLimit(Const iTokenLimit : Integer);
    Function  GetMaxDocOutputWidth : Integer;
    Procedure SetMaxDocOutputWidth(Const iTokenLimit : Integer);
    Function  GetManagedNodesLife : Integer;
    Procedure SetManagedNodesLife(Const iTokenLimit : Integer);
    Function  GetTreeColour : TColor;
    Procedure SetTreeColour(Const iColour : TColor);
    Function  GetINIFileName : String;
    Function  GetProfilingCode(Const strModuleName : String) : String;
    Procedure SetProfilingCode(Const strModuleName, strProfileCode : String);
    Function  GetIssueLimit(Const eLimitType : TLimitType) : Integer;
    Procedure SetIssueLimit(Const eLimitType : TLimitType; Const iLimit : Integer);
    Function  GetMenuShortcut(Const BADIMenu : TBADIMenu) : String;
    Procedure SetMenuShortcut(Const BADIMenu : TBADIMenu; Const strShortcut : String);
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
    Procedure SetDoNotFollowEditor(Const setDoNotFollowTypes : TLimitTypes);
    Function  GetScopeImageList : TImageList;
    Function  GetLanguageDictionaryFile : String;
    Procedure SetLanguageDictionaryFile(Const strValue : String);
    Function  GetLanguageDictionary : TStringList;
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
    // General Methods
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure RequiresIDEEditorColoursUpdate;
    // Properties
    (**
      This property contains the basic boolean toggle options for the application.
      @precon  None.
      @postcon Contains the basic boolean toggle options for the application.
      @return  a TDocOptions
    **)
    Property Options : TDocOptions Read GetOptions Write SetOptions;
    (**
      This property provides a list of Compiler Conditional Defines.
      @precon  None.
      @postcon Provides a list of Compiler Conditional Defines.
      @return  a TStringList
    **)
    Property Defines : TStringList Read GetDefines;
    (**
      This property provide access to the special tags string list.
      @precon  None.
      @postcon Provide access to the special tags string list.
      @return  a TList<TBADISpecialTag>
    **)
    Property SpecialTags : TList<TBADISpecialTag> Read GetSpecialTags;
    (**
      This property provide access to the expanded nodes string list.
      @precon  None.
      @postcon Provide access to the expanded nodes string list.
      @return  a TStringList
    **)
    Property ExpandedNodes : TStringList Read GetExpandedNodes;
    (**
      This property determines the amount of time in milliseconds between the
      last editor update and the next refresh. Interval only, the application
      needs to implement the logic.
      @precon  None.
      @postcon Gets and sets the update interval.
      @return  a Cardinal
    **)
    Property UpdateInterval : Cardinal Read GetUpdateInterval Write SetUpdateInterval;
    (**
      This property determines the scopes to render in the module explorer.
      @precon  None.
      @postcon Gets and sets the scopes to render.
      @return  a TScopes
    **)
    Property ScopesToRender : TScopes Read GetScopesToRender Write SetScopesToRender;
    (**
      This property determines the behaviour of the positioning of the cursor
      in the editor window.
      @precon  None.
      @postcon Gets and sets the browse position.
      @return  a TBrowsePosition
    **)
    Property BrowsePosition : TBrowsePosition Read GetBrowsePosition Write SetBrowsePosition;
    (**
      This property determines the Font Name of the Module Explorer Tree Nodes.
      @precon  None.
      @postcon Gets or sets the module explorer tree node font name.
      @return  a String
    **)
    Property TreeFontName : String Read GetTreeFontName Write SetTreeFontName;
    (**
      This property determines the font size of the module explorer tree node font.
      @precon  None.
      @postcon Gets or sets the module explorer tree node font size.
      @return  an Integer
    **)
    Property TreeFontSize : Integer Read GetTreeFontSize Write SetTreeFontSize;
    (**
      This property determines the Font Name of the Module Explorer Comment Fixed Fonts.
      @precon  None.
      @postcon Gets or sets the module explorer comment fixed font name.
      @return  a String
    **)
    Property FixedFontName : String Read GetFixedFontName Write SetFixedFontName;
    (**
      This property determines the font size of the module explorer comment fixed font.
      @precon  None.
      @postcon Gets or sets the module explorer comment fixed font size.
      @return  an Integer
    **)
    Property FixedFontSize : Integer Read GetFixedFontSize Write SetFixedFontSize;
    (**
      This property determines the colour and style attribute of a token in the module explorer
      @precon  None.
      @postcon Gets and sets the colour and style of the token.
      @param   boolUseIDEEditorColours as a Boolean as a constant
      @return  a TBADITokenFontInfoTokenSet
    **)
    Property TokenFontInfo[Const boolUseIDEEditorColours : Boolean] : TBADITokenFontInfoTokenSet
      Read GetTokenFontInfo Write SetTokenFontInfo;
    (**
      This property holds a list of filename exclusions for documentation, metrics and checks.
      @precon  None.
      @postcon Gets and sets the list.
      @return  a IBADIExclusions
    **)
    Property Exclusions : IBADIExclusions Read GetExclusions;
    (**
      This property stores a list of method descriptions related to pattern
      matches.
      @precon  None.
      @postcon Gets and sets the method list.
      @return  a TStringList
    **)
    Property MethodDescriptions : TStringList Read GetMethodDescriptions;
    (**
      This property determines the scopes to document.
      @precon  None.
      @postcon Gets and sets the scopes to document.
      @return  a TScopes
    **)
    Property ScopesToDocument : TScopes Read GetScopesToDocument Write SetScopesToDocument;
    (**
      This gets and sets the background colour for the Module explorer.
      @precon  None.
      @postcon Gets and sets the background colour for the Module explorer.
      @param   boolUseIDEEditorColours as a Boolean as a constant
      @return  a TColor
    **)
    Property BGColour[Const boolUseIDEEditorColours : Boolean] : TColor Read GetModuleExplorerBGColour 
      Write SetModuleExplorerBGColour;
    (**
      This property gets and sets the token limit to the module explorer.
      @precon  None.
      @postcon Gets and sets the token limit to the module explorer.
      @return  an Integer
    **)
    Property TokenLimit : Integer Read GetTokenLimit Write SetTokenLimit;
    (**
      This property gets and sets the maximum width of the code output in
      documentation.
      @precon  None.
      @postcon Gets and sets the maximum width of the code output in
               documentation.
      @return  an Integer
    **)
    Property MaxDocOutputWidth : Integer Read GetMaxDocOutputWidth Write SetMaxDocOutputWidth;
    (**
      This property gets and sets the period of time in days which managed nodes
      are alive for.
      @precon  None.
      @postcon Gets and sets the period of time in days which managed nodes
               are alive for.
      @return  an Integer
    **)
    Property ManagedNodesLife : Integer Read GetManagedNodesLife Write SetManagedNodesLife;
    (**
      This property gets and sets the colour of the explorer tree lines.
      @precon  None.
      @postcon Gets and sets the colour of the explorer tree lines.
      @return  a TColor
    **)
    Property TreeColour : TColor Read GetTreeColour Write SetTreeColour;
    (**
      This property returns the name of the INI file.
      @precon  None.
      @postcon Returns the name of the INI file.
      @return  a String
    **)
    Property INIFileName : String Read GetINIFileName;
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
      Read GetModuleMetric Write SetModuleMetric;
    (**
      This property gets and sets the metric sub-options set.
      @precon  None.
      @postcon Gets and sets the metric sub-options set.
      @return  a TBADIModuleMetricSubOps
    **)
    Property ModuleMetricSubOptions : TBADIModuleMetricSubOps Read GetModuleMetricSubOps
      Write SetModuleMEtricSubOps;
    (**
      This property defines the power of the y = x ^ z equation used to combine the metrics for
      toxicity.
      @precon  None.
      @postcon Gets ad sets the power of the toxicity summation.
      @return  an Integer
    **)
    Property ToxicityPower : Integer Read GetToxicityPower Write SetToxicityPower;
    (**
      This property determines whether the metrics are summed before or after being powered.
      @precon  None.
      @postcon Gets and set the toxicity summation mechanism.
      @return  a TBADIToxicitySummation
    **)
    Property ToxicitySummartion : TBADIToxicitySummation Read GetToxicitySummation
      Write SetToxicitySummation;
    (**
      A property to read and write module checks configuration information.
      @precon  None.
      @postcon Gets and sets the module check information.
      @param   eModuleCheck as a TBADIModuleCheck as a constant
      @return  a TBADICheckRecord
    **)
    Property ModuleCheck[Const eModuleCheck : TBADIModuleCheck] : TBADICheckRecord
      Read GetModuleCheck Write SetModuleCheck;
    (**
      This property gets and sets the check sub-options set.
      @precon  None.
      @postcon Gets and sets the check sub-options set.
      @return  a TBADIModuleCheckSubOps
    **)
    Property ModuleCheckSubOptions : TBADIModuleCheckSubOps Read GetModuleCheckSubOps
      Write SetModuleCheckSubOps;
    (**
      A property to define the lower limit margin for metrics in the metrics views.
      @precon  None.
      @postcon Returns the lower margin limit for a metric (a percentage).
      @return  a Double
    **)
    Property LowMetricMargin : Double Read GetLowMetricMargin Write SetLowMetricMargin;
    (**
      A property to define the upper limit margin for metrics in the metrics views.
      @precon  None.
      @postcon Returns the upper margin limit for a metric (a percentage).
      @return  a Double
    **)
    Property HighMetricMargin : Double Read GetHighMetricMargin Write SetHighMetricMargin;
    (**
      A property to determine of a constant refactoring should have a new line between declaration
      sections.
      @precon  None.
      @postcon Gets or set the boolean value.
      @return  a Boolean
    **)
    Property RefactorConstNewLine : Boolean Read GetRefactorConstNewLine Write SetRefactorConstNewLine;
    (**
      This property determines whether the module explorer should use IDE editor themes of BAID themes.
      @precon  None.
      @postcon Gets and sets whether the module explorer should use IDE editor themes of BAID themes.
      @return  a Boolean
    **)
    Property UseIDEEditorColours : Boolean Read GetUseIDEEditorColours Write SetUseIDEEditorColours;
    (**
      This property defines the date format used when a module has its date updated automatically.
      @precon  None.
      @postcon Gets and sets the date format.
      @return  a String
    **)
    Property ModuleDateFmt : String Read GetModuleDateFmt Write SetModuleDateFmt;
    (**
      This property determines the conversion used for increment the number of bytes changed in a file
      into the increment of the version number.
      @precon  None.
      @postcon Gets and sets the increment factor.
      @return  a Double
    **)
    Property ModuleVersionIncrement : Double Read GetModuleVersionIncrement
      Write SetModuleVersionIncrement;
    (**
      This property gets and sets the limits type that if present in the module prevent the explorer 
      selection from following the editor cursor.
      @precon  None.
      @postcon Gets and sets the limits to not follow.
      @return  a TLimitTypes
    **)
    Property DoNotFollowEditor : TLimitTypes Read GetDoNotFollowEditor Write SetDoNotFollowEditor;
    (**
      This property returns a single scope image list for use throughout the application.
      @precon  None.
      @postcon Returns a single scope image list for use throughout the application.
      @return  a TImageList
    **)
    Property ScopeImageList : TImageList Read GetScopeImageList;
    (**
      This property returns a sorted string list of language specific dictionary words.
      @precon  None.
      @postcon Returns a sorted string list of language specific dictionary words.
      @return  a String
    **)
    Property LanguageDictionaryFile : String Read GetLanguageDictionaryFile Write SetLanguageDictionaryFile;
    (**
      This property gets and sets the filename for the language dictionary.
      @precon  None.
      @postcon Gets and sets the filename for the language dictionary.
      @return  a TStringList
    **)
    Property LanguageDictionary : TStringList Read GetLanguageDictionary;
    (**
      This property returns a sorted string list of local dictionary words.
      @precon  None.
      @postcon Returns a sorted string list of local dictionary words.
      @return  a String
    **)
    Property LocalDictionaryFile : String Read GetLocalDictionaryFile Write SetLocalDictionaryFile;
    (**
      This property gets and sets the filename for the local dictionary.
      @precon  None.
      @postcon Gets and sets the filename for the local dictionary.
      @return  a TStringList
    **)
    Property LocalDictionary : TStringList Read GetLocalDictionary;
    (**
      This property returns a sorted string list of project dictionary words.
      @precon  None.
      @postcon Returns a sorted string list of project dictionary words.
      @return  a String
    **)
    Property ProjectDictionaryFile : String Read GetProjectDictionaryFile Write SetProjectDictionaryFile;
    (**
      This property gets and sets the filename for the local dictionary.
      @precon  None.
      @postcon Gets and sets the filename for the local dictionary.
      @return  a TStringList
    **)
    Property ProjectDictionary : TStringList Read GetProjectDictionary;
    (**
      This property gets and sets the filename for the project dictionary.
      @precon  None.
      @postcon Gets and sets the filename for the project dictionary.
      @return  a String
    **)
    Property IgnoreDictionaryFile : String Read GetIgnoreDictionaryFile Write SetIgnoreDictionaryFile;
    (**
      This property returns a sorted string list of dictionary words to ignore.
      @precon  None.
      @postcon Returns a sorted string list of dictionary words to ignore.
      @return  a TStringList
    **)
    Property IgnoreDictionary : TStringList Read GetIgnoreDictionary;
    (**
      This property determines the colour of the highlighted text for spelling mistakes.
      @precon  None.
      @postcon Gets and sets the spelling mistake colours.
      @return  a TColor
    **)
    Property SpellingMistakeColour : TColor Read GetSpellingMistakeColour Write SetSpellingMistakeColour;
    (**
      This property determines the comment tag to be used when inserting comments with tags for a given
      file extension.
      @precon  None.
      @postcon Returns the comment tag to be used for the given module extension.
      @param   strExt as a String as a constant
      @return  a String
    **)
    Property CommentTagName[Const strExt : String] : String Read GetCommentTagName Write SetCommentTagName;
    (**
      This property determines the comment type to be used when inserting comments with tags for a given
      file extension.
      @precon  None.
      @postcon Returns the comment type to be used for the given module extension.
      @param   strExt as a String as a constant
      @return  a TCommentType
    **)
    Property CommentType[Const strExt : String] : TCommentType Read GetCommentType Write SetCommentType;
  End;

  (** An interface to get the IDE Editor Colours from the Registry. **)
  IBADIIDEEditorColours = Interface
  ['{F49776DF-C09A-4141-BAB7-AC166AC5FB35}']
    Function GetIDEEditorColours(Var iBGColour : TColor) : TBADITokenFontInfoTokenSet;
  End;

  (** This interface allows a module notifier to have the indexed file renamed for removing the
      notifier from the IDE. **)
  IBADIModuleNotifierList = Interface
  ['{60E0D688-F529-4798-A06C-C283F800B7FE}']
    Procedure Add(Const strFileName : String; Const iIndex : Integer);
    Function  Remove(Const strFileName: String): Integer;
    Procedure Rename(Const strOldFileName, strNewFileName : String);
  End;

  (** This is an event signature that need to the implemented by module and project notifiers so that
      the module notifier lists can be updated. **)
  TBADIModuleRenameEvent = Procedure(Const strOldFilename, strNewFilename : String) Of Object;

  (** An interface to manage the size of a file (module) and the size of the changes made to the
      file. **)
  IBADIModuleStats = Interface
  ['{525A3C3C-AB11-48C4-835D-ADA5A88B848C}']
    Procedure Update(Const iSize, iModifiedCount : Int64);
    Function  SizeChange : Int64;
    Procedure Reset();
    Procedure Rename(Const strFileName : String);
  End;

  (** An interface to manage a dictionary of IBADIModuleStats. **)
  IBADIModuleStatsList = Interface
  ['{C64AD7A8-8A15-4114-90DD-E689D8193CA5}']
    // Getter and Setters
    Function  GetModuleStats(Const strFileName : String) : IBADIModuleStats;
    // General Methods
    Procedure Rename(Const strOldFileName, strNewFileName : String);
    Procedure Remove(Const strFileName : String);
    // Properties
    (**
      This method returns an interfaces of the module statistics for the given filename.
      @precon  None.
      @postcon Returns an interfaces of the module statistics for the given filename.
      @param   strFileName as a String as a constant
      @return  an IBADIModuleStats
    **)
    Property  ModuleStats[Const strFileName : String] : IBADIModuleStats Read GetModuleStats; Default;
  End;

  (** A record to describe the document issue information. **)
  TBADIDocIssueInfo = Record
    FName       : String;
    FImageIndex : TBADIImageIndex;
    FForeColour : TColor;
    FBackColour : TColor;
    FMessage    : String;
  End;

  (** A record for the spelling mistakes: The WORD and the Column position. **)
  TBADISpellingMistake = Record
    FWord   : String;
    FColumn : Integer;
  End;

  (** An interface for the Documentation Issues for a line of code. **)
  IBADILineDocIssues = Interface
  ['{6D94887A-69E7-4736-A1B1-6BB533DE9D9F}']
    // Getters and Setters
    Function  GetLimitTypes : TArray<String>;
    Function  GetMessage(Const strDocIssueType : String) : TBADIDocIssueInfo;
    Function  GetSpellingMistakeCount : Integer;
    Function  GetSpellingMistake(Const iIndex : Integer) : TBADISpellingMistake;
    // General Methods
    Procedure AddIssue(Const strDocIssueType : String; Const DocIssueInfo : TBADIDocIssueInfo);
    Procedure AddSpellingMistake(Const strWord : String; Const iColumn : Integer);
    // Properties
    (**
      This property returns a set of enumerates which define the doc issues for the line.
      @precon  None.
      @postcon Returns a set of enumerates which define the doc issues for the line.
      @return  a TArray<String>
    **)
    Property Issues : TArray<String> Read GetLimitTypes;
    (**
      This method returns the text associated with the given doc issue type.
      @precon  None.
      @postcon Returns the text associated with the given doc issue type.
      @param   strDocIssueType as a String as a constant
      @return  a TBADIDocIssueInfo
    **)
    Property Message[Const strDocIssueType : String] : TBADIDocIssueInfo Read GetMessage; Default;
    (**
      This property returns the number of spelling mistakes for the line.
      @precon  None.
      @postcon Returns the number of spelling mistakes for the line.
      @return  an Integer
    **)
    Property SpellingMistakeCount : Integer Read GetSpellingMistakeCount;
    (**
      This property returns the spelling mistake information for the indexed item.
      @precon  iiNdex must be between 0 and SpellingMistakeCount - 1.
      @postcon Returns the spelling mistake information for the indexed item.
      @param   iIndex as an Integer as a constant
      @return  a TBADISpellingMistake
    **)
    Property SpellingMistake[Const iIndex : Integer] : TBADISpellingMistake Read GetSpellingMistake;
  End;

  (** A record to describe the information inside the Totals dictionary. **)
  TBADITotalInfo = Record
    FLabel      : String;
    FImageIndex : TBADIImageIndex;
    FForeColour : TColor;
    FBackColour : TColor;
    FFontStyles : TFontStyles;
    FCounter    : Integer;
    FFirstLine  : Integer;
    FFirstCol   : Integer;
  End;
  
  (** An interface to returning the doc issue totals for the whole module. **)
  IBADIDocIssueTotals = Interface
  ['{FE2ACA7E-03A5-4C12-A872-A8A4F33AC809}']
    // Getters and Setters
    Function  GetTotals : TDictionary<String, TBADITotalInfo>;
    Function  ContainsAny(Const setDocIssues : TLimitTypes) : Boolean;
    // General Methods
    Procedure IncDocIssue(Const strDocIssueType : String; Const TotalInfo : TBADITotalInfo);
    Procedure Clear;
    // Property
    (**
      This property returns the total for the given doc issue type.
      @precon  None.
      @postcon Returns the total for the given doc issue type.
      @return  a TDictionary<String, TBADITotalInfo>
    **)
    Property Totals : TDictionary<String, TBADITotalInfo> Read GetTotals;
  End;

Implementation

End.
