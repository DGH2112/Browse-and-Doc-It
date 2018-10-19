(**

  This module contains interfaces for use throughout Browse and Doc It.

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Oct 2018

**)
Unit BADI.Interfaces;

Interface

Uses
  System.Classes,
  System.Generics.Collections,
  VCL.Graphics,
  BADI.Types;

Type
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
    Function  GetExcludeDocFiles : TStringList;
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
    // General Methods
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure RequiresIDEEditorColoursUpdate;
    // Properties
    (**
      This property contains the basic toggleable options for the application.
      @precon  None.
      @postcon Contains the basic toggleable options for the application.
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
      This property determines the amount of time in milliseonds between the
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
      This properrty holds a list of files / partial or full which should not be
      documented.
      @precon  None.
      @postcon Gets and sets the list.
      @return  a TStringList
    **)
    Property ExcludeDocFiles : TStringList Read GetExcludeDocFiles;
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
      This property gets ans sets the token limit to the module explorer.
      @precon  None.
      @postcon Gets ans sets the token limit to the module explorer.
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
      This property returns the name of the inifile.
      @precon  None.
      @postcon Returns the name of the inifile.
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
      This property defines the power of the y = x ^ z equiation used to combine the metrics for
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
      @postcon Gets or set the boolena value.
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
  End;

  (** An interface to get the IDE Editor Colours from the Registry. **)
  IBADIIDEEditorColours = Interface
  ['{F49776DF-C09A-4141-BAB7-AC166AC5FB35}']
    Function GetIDEEditorColours(Var iBGColour : TColor) : TBADITokenFontInfoTokenSet;
  End;

Implementation

End.
