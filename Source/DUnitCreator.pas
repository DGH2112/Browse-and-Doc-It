(**

  This module contains code for getting OTA information and creating DUnit
  project and unit source files.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Oct 2009

**)
unit DUnitCreator;

interface

Uses
  Classes, ToolsAPI, BaseLanguageModule;

{$INCLUDE '..\..\..\Library\CompilerDefinitions.inc'}

Type
  (** A type for an array of OTA projects. **)
  TProjectsArray = Array Of IOTAProject;
  (** A type for an array of OTA Modules. **)
  TUnitsArray = Array Of IOTAModuleInfo;

  (** A procedural type for error event handlers. **)
  TErrorProc = Procedure(strMsg : String) Of Object;

  (** A class to handle the creation of the DUnit files. **)
  TDUnitCreator = Class
  private
    procedure CheckReadOnlyStatus(Module : IOTAModule; strModuleReadOnly: string);
  {$IFDEF D2005} Strict {$ENDIF} Private
    FProjects : TProjectsArray;
    FProjectCount : Integer;
    FUnits : TUnitsArray;
    FUnitCount : Integer;
    FProject : IOTAProject;
    FModule : TBaseLanguageModule;
    FUnit: IOTAModule;
    FErrors : TErrorProc;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure AddProject(Project : IOTAProject);
    Procedure AddUnit(AUnit : IOTAModuleInfo);
    Procedure AddNewTestSuites(M : TBaseLanguageModule; slTestCases : TStrings;
      strTestSuiteName : String);
    Procedure AddNewTestImplementations(M : TBaseLanguageModule;
      slTestCases : TStrings);
    Procedure AddNewTestClasses(M : TBaseLanguageModule; slTestCases : TStrings;
      strBaseClass : String);
    Function DoesClassExist(M : TBaseLanguageModule; strClassName : String) : Boolean;
    Function DoesMethodExist(M : TBaseLanguageModule; strClassName,
      strMethodName : String) : Boolean;
    Procedure RaiseError(strMsg : String);
    Procedure AddUnitToBeTestedToUsesClause(M : TBaseLanguageModule;
      strUnitToBeTested : String);
    Procedure AddNewTestMethodsToClass(M : TBaseLanguageModule;
      slTestCases : TStringList);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure GetExistingDUnitProjects;
    Procedure CreateTestProject(strNewProjectName : String);
    Procedure ActivateProject(iProject: Integer);
    Function GetProject(iProject: Integer): String;
    Function GetProjectCount: Integer;
    function GetUnit(iUnit: Integer): string;
    function GetUnitCount: Integer;
    Procedure GetExistingDUnitUnits(iProject: Integer);
    procedure CreateTestUnit(strNewUnitName, strUnitToBeTested : String;
      slTestCases : TStringList; strBaseClass, strTestSuiteName : String);
    procedure UpdateTestUnit(iUnit : Integer; strUnitToBeTested : String;
      slTestCases : TStringList; strBaseClass, strTestSuiteName : String);
    Function DoesProjectExist(strProjectName : String) : Boolean;
    Function DoesUnitExist(iProject : Integer; strUnitName : String) : Boolean;
    Procedure AddUnitToBeTested(strFileName : String);
    (**
      This property returns the indexed project reference.
      @precon  None.
      @postcon Returns the indexed project reference.
      @param   iProject as       an Integer
      @return  a String
    **)
    Property Projects[iProject : Integer] : String Read GetProject;
    (**
      This property returns the number of projects referenced.
      @precon  None.
      @postcon Returns the indexed project reference.
      @return  an Integer
    **)
    Property ProjectCount : Integer Read GetProjectCount;
    (**
      This property returns the indexed unit reference.
      @precon  None.
      @postcon Returns the indexed unit reference.
      @param   iUnit as       an Integer
      @return  a String
    **)
    Property Units[iUnit : Integer] : String Read GetUnit;
    (**
      This property returns the number of units referenced.
      @precon  None.
      @postcon Returns the indexed unit reference.
      @return  an Integer
    **)
    Property UnitCount : Integer Read GetUnitCount;
    (**
      This property returns the base language module interface.
      @precon  None.
      @postcon Returns the base language module interface.
      @return  a TBaseLanguageModule
    **)
    Property Module : TBaseLanguageModule Read FModule;
    (**
      This property gets and sets the event handler for errors.
      @precon  None.
      @postcon Gets and sets the event handler for errors.
      @return  a TErrorProc
    **)
    Property Errors : TErrorProc Read FErrors Write FErrors;
  End;

implementation

Uses
  SysUtils, Windows, ToolsAPIUtils, ModuleDispatcher, DGHLibrary;

Const
  (** A constant to define the growth capacity of the collections. **)
  iCAPACITY : Integer = 25;

Type
  (** A class to handle the creation of the DUnit project file. **)
  TProjectCreator = Class(TInterfacedObject, IOTACreator, IOTAProjectCreator
    {$IFDEF D0005}, IOTAProjectCreator50 {$ENDIF}
    {$IFDEF D2005}, IOTAProjectCreator80 {$ENDIF})
  {$IFDEF D2005} Strict {$ENDIF} Private
    FNewProjectName : String;
  Public
    Constructor Create(strNewProjectName : String);
    Function GetCreatorType: String;
    Function GetExisting: Boolean;
    Function GetFileSystem: String;
    Function GetOwner: IOTAModule;
    Function GetUnnamed: Boolean;
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    {$IFDEF D0005}
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    {$ENDIF}
    {$IFDEF D2005}
    function GetProjectPersonality: string;
    {$ENDIF}
  End;

  (** This class creates a IOTAFIle interface for generating the project file
      source. **)
  TProjectCreatorFile = Class(TInterfacedObject, IOTAFile)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FProjectName : String;
  Public
    Constructor Create(strProjectName : String);
    function GetAge: TDateTime;
    function GetSource: string;
  End;

  (** This class creates the test unit file. **)
  TUnitCreator = Class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FNewUnitName : String;
    FOwner : IOTAProject;
    FUnitToBeTested: String;
    FTestCases: TStringList;
    FBaseClass: String;
    FTestSuiteName: String;
  Public
    Constructor Create(strNewUnitName, strUnitToBeTested : String;
      slTestCases : TStringList; Owner : IOTAProject; strBaseClass,
      strTestSuiteName : String);
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
    function GetAncestorName: string;
    function GetFormName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent: string; const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent: string; const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
  End;

  (** This class creates a IOTAFile interface for generating the unit file
      source. **)
  TUnitCreatorFile = Class(TInterfacedObject, IOTAFile)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FUnitName : String;
    FUnitToBeTested: String;
    FTestCases: TStringList;
    FBaseClass: String;
    FTestSuiteName: String;
  Public
    Constructor Create(strUnitName, strUnitToBeTested : String;
      slTestCases : TStringList; strBaseClass, strTestSuiteName : String);
    function GetAge: TDateTime;
    function GetSource: string;
  End;

{ TDUnitCreator }

(**

  This function returns true if the project file contains the unit
  TestFramework.

  @precon  Project must be a valid instance of a project.
  @postcon Returns true if the project file contains the unit
           TestFramework.

  @param   Project as an IOTAProject
  @return  a Boolean

**)
Function IsTestFramework(Project : IOTAProject) : Boolean;

var
  S: IOTASourceEditor;
  M: TBaseLanguageModule;
  E: TElementContainer;
  strFileName : String;

Begin
  Result := False;

  S := SourceEditor(Project.CurrentEditor.Module);
  strFilename := Project.CurrentEditor.FileName;
  M := Dispatcher(EditorAsString(S), strFileName, True, [moParse]);
  Try
    If M <> Nil Then
      Begin
        E := M.FindElement(strUses);
        If E <> Nil Then
          Begin
            If E.FindElement('TestFramework') <> Nil Then
              Begin
                Result := True;
                Exit;
              End;
          End;
      End;
  Finally
    M.Free;
  End;
End;

(**

  This function gets the class name from the test cases string list and if the
  class is null returns "Functions".

  @precon  None.
  @postcon Gets the class name from the test cases string list and if the
           class is null returns "Functions".

  @param   strText as a String
  @return  a String

**)
Function TestClassName(strText : String) : String;

Begin
  Result := GetField(strText, '=', 1);
  If Result = '' Then
    Result := 'Functions';
End;

(**

  This function returns the code associated and to be inserted into the new unit
  for the classes which implement the tests selected.

  @precon  slTestCases must be a valid instance of a string list . 
  @postcon Returns the code associated and to be inserted into the new unit for 
           the classes which implement the tests selected .

  @param   slTestCases  as a TStringList
  @param   strBaseClass as a String
  @return  a String      

**)
Function TestClasses(slTestCases : TStringList; strBaseClass : String): String;

Var
  strLastClass: String;
  i: Integer;
  strClass : String;
  strMethod : String;

begin
  Result := '';
  strLastClass := '';
  For i := 0 to slTestCases.Count - 1 Do
    Begin
      strClass := TestClassName(slTestCases[i]);
      strMethod := GetField(slTestCases[i], '=', 2);
      If strLastClass <> strClass Then
        Begin
          If Result <> '' Then
            Result := Result + '  End;'#13#10#13#10;
          Result := Result + '  //'#13#10;
          Result := Result + Format('  // Test Class for the %s Class Methods.'#13#10,
            [strClass]);
          Result := Result + '  //'#13#10;
          Result := Result + Format('  Test%s = Class(%s)'#13#10,
            [strClass, strBaseClass]);
          Result := Result + '  Strict Private'#13#10;
          If strClass <> 'Functions' Then
            Result := Result + Format('    F%s : %s;'#13#10,
              [Copy(strClass, 2, Length(strClass)), strClass]);
          Result := Result + '  Public'#13#10;
          If strClass <> 'Functions' Then
            Result := Result + '    Procedure SetUp; Override;'#13#10;
          If strClass <> 'Functions' Then
            Result := Result + '    Procedure TearDown; Override;'#13#10;
          Result := Result + '  Published'#13#10;
        End;
      Result := Result + Format('    Procedure Test%s;'#13#10, [strMethod]);
      strLastClass := strClass;
    End;
  Result := Result + '  End;'#13#10;
end;

(**

  This function returns the implementation methods for the selected test cases.

  @precon  slTestCases must be a valid instance of a string list.
  @postcon Returns the implementation methods for the selected test cases.

  @param   slTestCases as a TStringList
  @return  a String

**)
Function TestImplementation(slTestCases : TStringList): String;

Var
  strLastClass: String;
  i: Integer;
  strClass : String;
  strMethod : String;

begin
  Result := #13#10;
  strLastClass := '';
  For i := 0 to slTestCases.Count - 1 Do
    Begin
      strClass := TestClassName(slTestCases[i]);
      strMethod := GetField(slTestCases[i], '=', 2);
      If strLastClass <> strClass Then
        Begin
          Result := Result + '//'#13#10;
          Result := Result + Format('// Test Methods for Class %s.'#13#10,
            [strClass]);
          Result := Result + '//'#13#10;
          If strClass <> 'Functions' Then
            Begin
              Result := Result + Format('Procedure Test%s.Setup;'#13#10, [strClass]);
              Result := Result + 'Begin'#13#10;
              Result := Result + Format(
                '  F%s := %s.Create; //: @debug Setup constructor for %s.'#13#10,
                [Copy(strClass, 2, Length(strClass)), strClass, strClass]);
              Result := Result + 'End;'#13#10;
              Result := Result + ''#13#10;
              Result := Result + Format('Procedure Test%s.TearDown;'#13#10,
                [strClass]);
              Result := Result + ''#13#10;
              Result := Result + 'Begin'#13#10;
              Result := Result + Format('  F%s.Free;'#13#10,
                [Copy(strClass, 2, Length(strClass))]);
              Result := Result + 'End;'#13#10;
              Result := Result + ''#13#10;
            End;
        End;
      Result := Result + Format('Procedure Test%s.Test%s;'#13#10, [strClass,
        strMethod]);
      Result := Result + ''#13#10;
      Result := Result + 'Begin'#13#10;
      Result := Result + Format('  //: @todo Implement Check for %s.%s.'#13#10,
        [strClass, strMethod]);
      Result := Result + 'End;'#13#10;
      Result := Result + ''#13#10;
      strLastClass := strClass;
    End;
end;

(**

  This function returns the initialisation code for the newly recreated test
  methods.

  @precon  slTestCases must be a valid instance of a string list .
  @postcon Returns the initialisation code for the newly recreated test methods
           .

  @param   slTestCases      as a TStringList
  @param   strTestSuiteName as a String
  @return  a String

**)
Function TestSuites(slTestCases : TStringList; strTestSuiteName : String): String;

Var
  i : Integer;
  strLastClass : String;
  strClass : String;

begin
  Result := '';
  strLastClass := '';
  For i := 0 to slTestCases.Count - 1 Do
    Begin
      strClass := TestClassName(slTestCases[i]);
      If strLastClass <> strClass Then
        Begin
          If Result <> '' Then
            Result := Result + #13#10;
          Result := Result + Format('  RegisterTest(''%s'', Test%s.Suite);',
            [strTestSuiteName, strClass]);
        End;
      strLastClass := strClass;
    End;
end;

(**

  This is a custom sort method for the Test Cases string list. If orders the
  items first by line number (stored in Objects[]) and then by text.

  @precon  None.
  @postcon Orders the items first by line number (stored in Objects[]) and then
           by text.

  @param   List   as a TStringList
  @param   Index1 as an Integer
  @param   Index2 as an Integer
  @return  an Integer

**)
Function SortClassEndTokens(List: TStringList; Index1, Index2: Integer): Integer;

Begin
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
  If Result = 0 Then
    Result := AnsiCompareText(List[Index1], List[Index2]);
End;

(**

  This method makes the indexed project the active project.

  @precon  iProject must be a valid index into the project array.
  @postcon Makes the indexed project the active project.

  @param   iProject as an Integer

**)
procedure TDUnitCreator.ActivateProject(iProject: Integer);

ResourceString
  strProjectReadOnly = 'Project source module is read only!';

begin
  FProject := FProjects[iProject];
  ProjectGroup.ActiveProject := FProject;
  CheckReadOnlyStatus(FProject, strProjectReadOnly);
end;

(**

  This method adds new test classes to the interface of the existing source
  module.

  @precon  M and slTestCases must be valid instances.
  @postcon Adds new test classes to the interface of the existing source
           module.

  @param   M            as a TBaseLanguageModule
  @param   slTestCases  as a TStrings
  @param   strBaseClass as a String

**)
Procedure TDUnitCreator.AddNewTestClasses(M : TBaseLanguageModule;
  slTestCases : TStrings; strBaseClass : String);

Var
  i : Integer;
  SE: IOTASourceEditor;
  Writer: IOTAEditWriter;
  strLastClass: String;
  iPos: Integer;
  CharPos: TOTACharPos;
  strClass : String;
  strMethod : String;
  iToken : Integer;
  boolEndClass : Boolean;

Begin
  iToken := M.FindToken('Implementation');
  if iToken > -1 Then
    Begin
      SE := SourceEditor(FUnit);
      Try
        Writer :=  SE.CreateUndoableWriter;
        Try
          CharPos.CharIndex := 0;
          CharPos.Line := M.Tokens[iToken].Line;
          iPos := SE.EditViews[0].CharPosToPos(CharPos);
          Writer.CopyTo(iPos);
          strLastClass := '';
          boolEndClass := False;
          For i := 0 To slTestCases.Count -1 Do
            Begin
              strClass := TestClassName(slTestCases[i]);
              strMethod := GetField(slTestCases[i], '=', 2);
              If Not DoesClassExist(M, strClass) Then
                Begin
                  If strLastClass <> strClass Then
                    Begin
                      If boolEndClass Then
                        OutputText(Writer, '  End;'#13#10#13#10);
                      OutputText(Writer, '  //'#13#10);
                      OutputText(Writer, Format('  // Test Class for the %s Class Methods.'#13#10, [strClass]));
                      OutputText(Writer, '  //'#13#10);
                      OutputText(Writer, Format('  Test%s = Class(%s)'#13#10, [strClass, strBaseClass]));
                      OutputText(Writer, '  Strict Private'#13#10);
                      If strClass <> 'Functions' Then
                        OutputText(Writer, Format('    F%s : %s;'#13#10, [Copy(strClass, 2, Length(strClass)), strClass]));
                      OutputText(Writer, '  Public'#13#10);
                      If strClass <> 'Functions' Then
                        OutputText(Writer, '    Procedure SetUp; Override;'#13#10);
                      If strClass <> 'Functions' Then
                        OutputText(Writer, '    Procedure TearDown; Override;'#13#10);
                      OutputText(Writer, '  Published'#13#10);
                    End;
                  OutputText(Writer, Format('    Procedure Test%s;'#13#10, [strMethod]));
                  boolEndClass := True;
                End;
              strLastClass := strClass;
            End;
          If boolEndClass Then
            OutputText(Writer, '  End;'#13#10#13#10);
        Finally
          Writer := Nil;
        End;
      Finally
        SE := Nil;
      End;
    End;
End;

(**

  This method adds new test suites to the initialisatio section assuming that the
  file has an initialisatin section.

  @precon  M must be a valid language module .
  @postcon Adds new test suites to the initialisatio section assuming that the
           file has an initialisatin section .

  @param   M                as a TBaseLanguageModule
  @param   slTestCases      as a TStrings
  @param   strTestSuiteName as a String

**)
procedure TDUnitCreator.AddNewTestSuites(M: TBaseLanguageModule;
  slTestCases : TStrings; strTestSuiteName : String);

ResourceString
  strFinalEndNotFound = 'Final reserved word ''End'' not found.';
  strFinalPeriodNotFound = 'Final period (.) not found.';

Var
  i : Integer;
  SE: IOTASourceEditor;
  Writer: IOTAEditWriter;
  strLastClass: String;
  iPos: Integer;
  CharPos: TOTACharPos;
  strClass : String;

begin
  If M.Tokens[M.TokenCount - 1].Token = '.' Then
    Begin
      If M.Tokens[M.TokenCount - 2].UToken = 'END' Then
        Begin
          SE := SourceEditor(FUnit);
          Try
            Writer :=  SE.CreateUndoableWriter;
            Try
              CharPos.CharIndex := 0;
              CharPos.Line := M.Tokens[M.TokenCount - 2].Line;
              iPos := SE.EditViews[0].CharPosToPos(CharPos);
              Writer.CopyTo(iPos);
              strLastClass := '';
              For i := 0 To slTestCases.Count -1 Do
                Begin
                  strClass := TestClassName(slTestCases[i]);
                  If strLastClass <> strClass Then
                    If Not DoesClassExist(M, strClass) Then
                      OutputText(Writer, Format('  RegisterTest(''%s'', Test%s.Suite);'#13#10,
                        [strTestSuiteName, strClass]));
                  strLastClass := strClass;
                End;
            Finally
              Writer := Nil;
            End;
          Finally
            SE := Nil;
          End;
        End Else
          RaiseError(strFinalEndNotFound);
    End Else
      RaiseError(strFinalPeriodNotFound);
end;

(**

  This method adds new method implementations to the existing source file.
  @note This method modifies the slTestCases method names IF a name clash is
  found with existing methods in the class. This means that there are no
  checks to be made when insert these methods into the existing class
  delcarations.

  @precon  M and slTestCases must be valid instances.
  @postcon Adds new method implementations to the existing source file.

  @param   M           as a TBaseLanguageModule
  @param   slTestCases as a TStrings

**)
procedure TDUnitCreator.AddNewTestImplementations(M: TBaseLanguageModule;
  slTestCases : TStrings);

Var
  i : Integer;
  SE: IOTASourceEditor;
  Writer: IOTAEditWriter;
  strLastClass: String;
  iPos: Integer;
  CharPos: TOTACharPos;
  strClass : String;
  strMethod : String;
  iToken : Integer;
  iIndex : Integer;

begin
  iToken := M.FindToken('Initialization');
  if iToken > -1 Then
    Begin
      SE := SourceEditor(FUnit);
      Try
        Writer :=  SE.CreateUndoableWriter;
        Try
          CharPos.CharIndex := 0;
          CharPos.Line := M.Tokens[iToken].Line;
          iPos := SE.EditViews[0].CharPosToPos(CharPos);
          Writer.CopyTo(iPos);
          strLastClass := '';
          For i := 0 To slTestCases.Count -1 Do
            Begin
              strClass := TestClassName(slTestCases[i]);
              strMethod := GetField(slTestCases[i], '=', 2);
              If strLastClass <> strClass Then
                Begin
                  If Not DoesClassExist(M, strClass) Then
                    Begin
                      OutputText(Writer, '//'#13#10);
                      OutputText(Writer, Format('// Test methods for the class %s.'#13#10,[strClass]));
                      OutputText(Writer, '//'#13#10);
                      If strClass <> 'Functions' Then
                        Begin
                          OutputText(Writer, Format('Procedure Test%s.Setup;'#13#10,[strClass]));
                          OutputText(Writer, ''#13#10);
                          OutputText(Writer, 'Begin'#13#10);
                          OutputText(Writer, Format('  F%s := %s.Create; //: @debug Setup constructor for %s.'#13#10,[Copy(strClass, 2, Length(strClass)), strClass, strClass]));
                          OutputText(Writer, 'End;'#13#10);
                          OutputText(Writer, ''#13#10);
                          OutputText(Writer, Format('Procedure Test%s.TearDown;'#13#10,[strClass]));
                          OutputText(Writer, ''#13#10);
                          OutputText(Writer, 'Begin'#13#10);
                          OutputText(Writer, Format('  F%s.Free;'#13#10,[Copy(strClass, 2, Length(strClass))]));
                          OutputText(Writer, 'End;'#13#10);
                          OutputText(Writer, ''#13#10);
                        End;
                    End;
                End;
              iIndex := 1;
              While DoesMethodExist(M, strClass, strMethod) Do
                Begin
                  strMethod := Format('%s%d', [GetField(slTestCases[i], '=', 2), iIndex]);
                  Inc(iIndex);
                End;
              slTestCases[i] := Format('%s=%s', [strClass, strMethod]);
              OutputText(Writer, Format('Procedure Test%s.Test%s;'#13#10,[strClass, strMethod]));
              OutputText(Writer, ''#13#10);
              OutputText(Writer, 'Begin'#13#10);
              OutputText(Writer, Format('  //: @todo Implement Check for %s.%s.'#13#10,
                [strClass, strMethod]));
              OutputText(Writer, 'End;'#13#10);
              OutputText(Writer, ''#13#10);
              strLastClass := strClass;
            End;
        Finally
          Writer := Nil;
        End;
      Finally
        SE := Nil;
      End;
    End;
end;

(**

  This method adds new test cases to existing class at the end of the class.
  The assumption is that the last section of the class is Published.

  @precon  M and slTestCases must eb valid instances.
  @postcon Adds new test cases to existing class at the end of the class.

  @param   M           as a TBaseLanguageModule
  @param   slTestCases as a TStringList

**)
procedure TDUnitCreator.AddNewTestMethodsToClass(M: TBaseLanguageModule;
  slTestCases: TStringList);

Var
  SE: IOTASourceEditor;
  Writer: IOTAEditWriter;
  iPos: Integer;
  CharPos: TOTACharPos;
  strClass : String;
  T: TElementContainer;
  iMethod: Integer;

  (**

    This function attempts to find the 'End' of the class as an insertion point
    for the new methods. The assumption is that the last section of the class
    is Published.

    @precon  None.
    @postcon Attempts to find the 'End' of the class as an insertion point
             for the new methods.

    @param   strClassName as a String
    @return  an Integer     

  **)
  Function FindEndOfClass(strClassName : String) : Integer;

  Var
    C : TElementContainer;
    i : Integer;

  Begin
    Result := -1;
    C := T.FindElement('Test' + strClassName);
    If C <> Nil Then
      For i := 0 To M.TokenCount - 1 Do
        If (M.Tokens[i].Line >= C.Line) And (M.Tokens[i].UToken = 'END') Then
          Begin
            Result := M.Tokens[i].Line;
            Break;
          End;
  End;

begin
  T := M.FindElement(strTypesLabel);
  If T <> Nil then
    Begin
      // Find class end tokens
      For iMethod := 0 To slTestCases.Count - 1 Do
        Begin
          strClass := TestClassName(slTestCases[iMethod]);
          slTestCases.Objects[iMethod] := TObject(FindEndOfClass(strClass));
        End;
      // Delete test with no end token (i.e. only ones to insert)
      For iMethod := slTestCases.Count - 1 DownTo 0 Do
        If Integer(slTestCases.Objects[iMethod]) = -1 Then
          slTestCases.Delete(iMethod);
      // Sort by end token line number descending
      slTestCases.CustomSort(SortClassEndTokens);
      For iMethod := 0 To slTestCases.Count - 1 Do
        slTestCases.Objects[iMethod] :=
          TObject(Integer(slTestCases.Objects[iMethod]) + iMethod);
      For iMethod := 0 To slTestCases.Count - 1 Do
        Begin
          SE := SourceEditor(FUnit);
          Try
            Writer :=  SE.CreateUndoableWriter;
            Try
              CharPos.CharIndex := 0;
              CharPos.Line := Integer(slTestCases.Objects[iMethod]);
              iPos := SE.EditViews[0].CharPosToPos(CharPos);
              Writer.CopyTo(iPos);
              OutputText(Writer, Format('    Procedure Test%s;'#13#10,
                [GetField(slTestCases[iMethod], '=', 2)]));
            Finally
              Writer := Nil;
            End;
          Finally
            SE := Nil;
          End;
        End;
    End Else
      RaiseError('Could not find Types!');
end;

(**

  This method adds the given project to the project array.

  @precon  None.
  @postcon Adds the given project to the project array.

  @param   Project as an IOTAProject

**)
procedure TDUnitCreator.AddProject(Project: IOTAProject);

Var
  T : TProjectsArray;
  i: Integer;

begin
  Inc(FProjectCount);
  If FProjectCount >= High(FProjects) Then
    Begin
      SetLength(T, Succ(High(FProjects)) + iCAPACITY);
      For i := Low(FProjects) To High(FProjects) Do
        T[i] := FProjects[i];
      FProjects := T;
    End;
  FProjects[FProjectCount - 1] := Project;
end;

(**

  This method adds the given unit to the unit array.

  @precon  None.
  @postcon Adds the given unit to the unit array.

  @param   AUnit as an IOTAModuleInfo

**)
procedure TDUnitCreator.AddUnit(AUnit: IOTAModuleInfo);

Var
  T : TUnitsArray;
  i: Integer;

begin
  Inc(FUnitCount);
  If FUnitCount >= High(FUnits) Then
    Begin
      SetLength(T, Succ(High(FUnits)) + iCAPACITY);
      For i := Low(FUnits) To High(FUnits) Do
        T[i] := FUnits[i];
      FUnits := T;
    End;
  FUnits[FUnitCount - 1] := AUnit;
end;

(**

  This method adds the unit file to be tested to the project IF it doesn`t
  already exist.

  @precon  None.
  @postcon Adds the unit file to be tested to the project IF it doesn`t
           already exist.

  @param   strFileName as a String

**)
procedure TDUnitCreator.AddUnitToBeTested(strFileName: String);

Var
  i : Integer;
  boolFound : Boolean;

begin
  boolFound := False;
  For i := 0 To FProject.GetModuleCount - 1 Do
    If AnsiCompareFileName(strFileName, FProject.GetModule(i).FileName)= 0 Then
      Begin
        boolFound := True;
        Break;
      End;
  If Not boolFound Then
    FProject.AddFile(strFileName, True);
end;

(**

  This method checks if the given module is readonly. If so outputs a message and
  aborts.

  @precon  Module must be a valid instance.
  @postcon Checks if the given module is readonly. If so outputs a message and
           aborts.

  @param   Module            as an IOTAModule
  @param   strModuleReadOnly as a string

**)
procedure TDUnitCreator.CheckReadOnlyStatus(Module : IOTAModule;
  strModuleReadOnly: string);

Var
  SE: IOTASourceEditor;

begin
  SE := SourceEditor(Module);
  if SE <> nil then
    begin
      if SE.EditViewCount > 0 then
        begin
          if SE.EditViews[0] <> nil then
            begin
              if SE.EditViews[0].Buffer <> nil then
                begin
                  if SE.EditViews[0].Buffer.IsReadOnly then
                    begin
                      RaiseError(strModuleReadOnly);
                      Abort;
                    end;
                end;
            end;
        end;
    end;
end;

(**

  This method inserts the unit to be tested into the uses clause IF it does not
  already exist.

  @precon  M must be a valid instance.
  @postcon Inserts the unit to be tested into the uses clause IF it does not
           already exist.

  @param   M                 as a TBaseLanguageModule
  @param   strUnitToBeTested as a String

**)
procedure TDUnitCreator.AddUnitToBeTestedToUsesClause(M: TBaseLanguageModule;
  strUnitToBeTested: String);

ResourceString
  strUsesClauseNotFound = 'No Uses clause found in Interface!';
  strInterfaceNotFound = 'Interface not found!';

Var
  U: TElementContainer;
  iToken : Integer;
  i: Integer;
  iImplLine : Integer;
  CharPos : TOTACharPos;
  SE: IOTASourceEditor;
  Writer: IOTAEditWriter;
  iPos: Integer;
  C: TElementContainer;

begin
  U := M.FindElement(strUses);
  If U = Nil Then
    Begin
      RaiseError(strUsesClauseNotFound);
      Exit;
    End;
  If U.FindElement(strUnitToBeTested) <> Nil Then
    Exit;
  CharPos.CharIndex := 0;
  CharPos.Line := 0;
  iToken := M.FindToken('Implementation');
  If iToken > -1 Then
    Begin
      iImplLine := M.Tokens[iToken].Line;
      For i := 1 To U.ElementCount Do
        Begin
          C := U.Elements[i];
          If C.Line < iImplLine Then
            Begin
              If CharPos.Line <= C.Line Then
                Begin
                  CharPos.Line := C.Line;
                  If CharPos.CharIndex < C.Column Then
                    CharPos.CharIndex := C.Column + Length(C.AsString(True, False));
                End;
            End;
        End;
      SE := SourceEditor(FUnit);
      Try
        Writer :=  SE.CreateUndoableWriter;
        Try
          Dec(CharPos.CharIndex, 1);
          iPos := SE.EditViews[0].CharPosToPos(CharPos);
          Writer.CopyTo(iPos);
          If CharPos.CharIndex + Length(strUnitToBeTested) < 80 Then
            OutputText(Writer, Format(', %s', [strUnitToBeTested]))
          Else
            OutputText(Writer, Format(','#13#10#32#32'%s', [strUnitToBeTested]));
        Finally
          Writer := Nil;
        End;
      Finally
        SE := Nil;
      End;
    End Else
      RaiseError(strInterfaceNotFound);
end;

(**

  This is a constructor for the TDUnitCreator class.

  @precon  None.
  @postcon Initialise the array sizes.

**)
constructor TDUnitCreator.Create;

begin
  FProjectCount := 0;
  FUnitCount := 0;
  With ActiveSourceEditor Do
    FModule := Dispatcher(EditorAsString(ActiveSourceEditor), FileName,
      Modified, [moParse]);
end;

(**

  This method creates a new DUnit test project in the project group.

  @precon  None .
  @postcon Creates a new DUnit test project in the project group .

  @param   strNewProjectName as a String

**)
procedure TDUnitCreator.CreateTestProject(strNewProjectName : String);

Var
  P: TProjectCreator;

begin
  P := TProjectCreator.Create(strNewProjectName);
  FProject := (BorlandIDEServices As IOTAModuleServices).CreateModule(P) As IOTAProject;
End;

(**

  This is a constructor for the TDUnitCreator class.

  @precon  None . 
  @postcon Creates the 

  @param   strNewUnitName    as a String
  @param   strUnitToBeTested as a String
  @param   slTestCases       as a TStringList
  @param   strBaseClass      as a String
  @param   strTestSuiteName  as a String

**)
procedure TDUnitCreator.CreateTestUnit(strNewUnitName,
  strUnitToBeTested : String; slTestCases : TStringList; strBaseClass,
    strTestSuiteName : String);
    
begin
  (BorlandIDEServices As IOTAModuleServices).CreateModule(
    TUnitCreator.Create(strNewUnitName, strUnitToBeTested, slTestCases, FProject,
      strBaseClass, strTestSuiteName));
end;

(**

  This is a destructor for the TDUnitCreator class.

  @precon  None.
  @postcon Frees the arrays.

**)
destructor TDUnitCreator.Destroy;
begin
  FModule.Free;
  FProjects := Nil;
  FUnits := Nil;
  Inherited Destroy;
end;

(**

  This method returns true IF the named class exists in the given module.
  
  @precon  M must be valid language module.
  @postcon Returns true IF the named class exists in the given module.
  
  @param   M            as a TBaseLanguageModule
  @param   strClassName as a String
  @return  a Boolean
  
**)
Function TDUnitCreator.DoesClassExist(M : TBaseLanguageModule;
  strClassName : String) : Boolean;
  
Var
  T : TElementContainer;

Begin
  Result := False;
  T := M.FindElement(strTypesLabel);
  If T <> Nil Then
    Result := T.FindElement('Test' + strClassName) <> Nil;
End;

(**

  This function returns true IF the given method name exists in the class.

  @precon  None.
  @postcon Returns true IF the given method name exists in the class.

  @param   M             as a TBaseLanguageModule
  @param   strClassName  as a String
  @param   strMethodName as a String
  @return  a Boolean

**)
Function TDUnitCreator.DoesMethodExist(M : TBaseLanguageModule;
  strClassName, strMethodName : String) : Boolean;

Var
  T, C, L : TElementContainer;

Begin
  Result := False;
  T := M.FindElement(strTypesLabel);
  If T <> Nil Then
    Begin
      C := T.FindElement('Test' + strClassName);
      If C <> Nil Then
        Begin
          L := C.FindElement(strMethodsLabel);
          If L <> Nil Then
            Result := L.FindElement('Test' + strMethodName) <> Nil;
        End;
    End;
End;

(**

  This method returns true if the project name currently exists in the current
  project group.

  @precon  None.
  @postcon Returns true if the project name currently exists in the current
           project group.

  @param   strProjectName as a String
  @return  a Boolean

**)
function TDUnitCreator.DoesProjectExist(strProjectName: String): Boolean;

Var
  G: IOTAProjectGroup;
  i: Integer;

begin
  Result := False;
  G := ProjectGroup;
  If G <> Nil Then
    For i := 0 To G.ProjectCount - 1 Do
      If AnsiCompareText(ExtractFileName(G.Projects[i].CurrentEditor.FileName),
        strProjectName) = 0 Then
        Begin
          Result := True;
          Exit;
        End;
end;

(**

  This method returns true if the indexed project contains a nuit with the given
  name.

  @precon  iProjecy must be a valid index into the project array.
  @postcon Returns true if the indexed project contains a nuit with the given
           name.

  @param   iProject    as an Integer
  @param   strUnitName as a String
  @return  a Boolean

**)
function TDUnitCreator.DoesUnitExist(iProject: Integer;
  strUnitName: String): Boolean;

Var
  P: IOTAProject;
  i: Integer;

begin
  Result := False;
  P := FProjects[iProject];
  For i := 0 To P.GetModuleCount - 1 Do
    If AnsiCompareText(ExtractFileName(P.GetModule(i).FileName),
      strUnitName) = 0 Then
      Begin
        Result := True;
        Exit;
      End;
end;

(**

  This method adds DUnit test projects to the project list.

  @precon  None.
  @postcon Adds DUnit test projects to the project list.

**)
procedure TDUnitCreator.GetExistingDUnitProjects;

Var
  i: Integer;

begin
  SetLength(FProjects, iCAPACITY);
  FProjectCount := 0;
  With ProjectGroup Do
    Begin
      For i := 0 To ProjectCount - 1 Do
        If IsTestFramework(Projects[i]) Then
          AddProject(Projects[i]);
    End;
end;

(**

  This method adds DUnit test units to the project unit list.

  @precon  None .
  @postcon Adds DUnit test units to the project unit list . 

  @param   iProject as an Integer

**)
procedure TDUnitCreator.GetExistingDUnitUnits(iProject: Integer);
var
  i: Integer;
begin
  SetLength(FUnits, iCAPACITY);
  FUnitCount := 0;
  If iProject > -1 Then
    Begin
      For i := 0 To FProjects[iProject].GetModuleCount - 1 Do
        If Like('Test*', ExtractFilename(FProjects[iProject].GetModule(i).FileName)) Then
          AddUnit(FProjects[iProject].GetModule(i));
    End;
end;

(**

  This is a getter method for the Project property.

  @precon  iProject must be a valid index into the project array.
  @postcon Returns the indexed project.

  @param   iProject as an Integer
  @return  a String

**)
function TDUnitCreator.GetProject(iProject: Integer): String;
begin
  Result := IOTAProject(FProjects[iProject]).CurrentEditor.FileName;
end;

(**

  This is a getter method for the ProjectCount property.

  @precon  None.
  @postcon Returns the number of projects in the array.

  @return  an Integer

**)
function TDUnitCreator.GetProjectCount: Integer;
begin
  Result := FProjectCount;
end;

(**

  This is a getter method for the Unit property.

  @precon  iUnit must be a valid index into the Unit array.
  @postcon Returns the indexed Unit from the array.

  @param   iUnit as an Integer
  @return  a string

**)
function TDUnitCreator.GetUnit(iUnit: Integer): string;
begin
  Result := IOTAModuleInfo(FUnits[iUnit]).FileName;
end;

(**

  This is a getter method for the UnitCount property.

  @precon  None.
  @postcon Returns the number of Units in the array.

  @return  an Integer

**)
function TDUnitCreator.GetUnitCount: Integer;
begin
  Result := FUnitCount;
end;

(**

  This method raises error events for the caller to handle.

  @precon  None.
  @postcon Raises error events for the caller to handle.

  @param   strMsg as a String

**)
Procedure TDUnitCreator.RaiseError(strMsg : String);

Begin
  If Assigned(FErrors) Then
    FErrors(strMsg);
End;

(**

  This method updates an existing unit file with the newly selected test cases.

  @precon  None.
  @postcon Updates an existing unit file with the newly selected test cases.

  @param   iUnit             as an Integer
  @param   strUnitToBeTested as a String
  @param   slTestCases       as a TStringList
  @param   strBaseClass      as a String
  @param   strTestSuiteName  as a String

**)
procedure TDUnitCreator.UpdateTestUnit(iUnit: Integer;
  strUnitToBeTested: String; slTestCases: TStringList; strBaseClass,
  strTestSuiteName: String);

ResourceString
  strUnitReadOnly = 'Unit source module is read only!';
  strExistingUnitInterfaceNil = 'Existing Unit Interface is Nil!';

Var
  M : TBaseLanguageModule;
  SE: IOTASourceEditor;

begin
  FUnit := FUnits[iUnit].OpenModule;
  FUnit.CurrentEditor.Show;
  SE := SourceEditor(FUnit);
  CheckReadOnlyStatus(FUnit, StrUnitReadOnly);
  SE.Show;
  If FUnit <> Nil Then
    Begin
      M := Dispatcher(EditorAsString(SourceEditor(FUnit)), FUnit.FileName,
        FUnit.CurrentEditor.Modified, [moParse]);
      Try
        AddNewTestSuites(M,
        slTestCases, strTestSuiteName);
        AddNewTestImplementations(M, slTestCases);
        AddNewTestClasses(M, slTestCases, strBaseClass);
        AddNewTestMethodsToClass(M, slTestCases);
        AddUnitToBeTestedToUsesClause(M, strUnitToBeTested);
      Finally
        M.Free;
      End;
    End Else
      RaiseError(strExistingUnitInterfaceNil);
end;

{ TProjectCreator }

(**

  This is a constructor for the TProjectCreator class.

  @precon  None.
  @postcon Sets the name of the new test project.

  @param   strNewProjectName as a String

**)
constructor TProjectCreator.Create(strNewProjectName: String);
begin
  FNewProjectName := strNewProjectName;
end;

(**

  This method sets the create type for the project.

  @precon  None.
  @postcon Sets the create type for the project.

  @return  a String

**)
function TProjectCreator.GetCreatorType: String;
begin
  Result := '' // Custom source code;
end;

(**

  This method determines that this project is new.

  @precon  None.
  @postcon Determines that this project is new.

  @return  a Boolean

**)
function TProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

(**

  This method determines the projects file name.

  @precon  None.
  @postcon Determines the projects file name.

  @return  a string

**)
function TProjectCreator.GetFileName: string;
begin
  Result := GetCurrentDir + '\' + FNewProjectName;
end;

(**

  This method determines the default file system to be used.

  @precon  None.
  @postcon Determines the default file system to be used.

  @return  a String

**)
function TProjectCreator.GetFileSystem: String;
begin
  Result := '';
end;

(**

  This method determines the options file name.

  @precon  None.
  @postcon Determines the options file name.

  @return  a string

**)
function TProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

(**

  This method determines the projects parent (project group).

  @precon  None.
  @postcon Determines the projects parent (project group).

  @return  an IOTAModule

**)
function TProjectCreator.GetOwner: IOTAModule;
begin
  Result := ProjectGroup;
end;

{$IFDEF D2005}
function TProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;
{$ENDIF}

(**

  This method determines the project shouldn`t be shown.

  @precon  None.
  @postcon Determines the project shouldn`t be shown.

  @return  a Boolean

**)
function TProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

(**

  This method determines the file is unnamed.

  @precon  None.
  @postcon Determines the file is unnamed.

  @return  a Boolean

**)
function TProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

(**

  This method determines that there are no new modules.

  @precon  None.
  @postcon Determines that there are no new modules.

**)
procedure TProjectCreator.NewDefaultModule;
begin
end;

{$IFDEF D0005}
procedure TProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
end;
{$ENDIF}

(**

  This method determines that there are no new options.

  @precon  None.
  @postcon Determines that there are no new options.

  @param   ProjectName as a string  as a constant
  @return  an IOTAFile

**)
function TProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := Nil;
end;

(**

  This method determines that there is no new project resource.

  @precon  None.
  @postcon Determines that there is no new project resource.

  @param   Project as an IOTAProject as a constant

**)
procedure TProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

(**

  This method creates the new project source file from a resource stream.

  @precon  None.
  @postcon Creates the new project source file from a resource stream.

  @param   ProjectName as a string as a constant
  @return  an IOTAFile

**)
function TProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;

begin
  Result := TProjectCreatorFile.Create(ProjectName);
end;

{ TUnitCreator }

(**

  This is a constructor for the TUnitCreator class.

  @precon  None .
  @postcon Sets the project file name and the project owner .

  @param   strNewUnitName    as a String
  @param   strUnitToBeTested as a String
  @param   slTestCases       as a TStringList
  @param   Owner             as an IOTAProject
  @param   strBaseClass      as a String
  @param   strTestSuiteName  as a String

**)
constructor TUnitCreator.Create(strNewUnitName, strUnitToBeTested : String;
  slTestCases : TStringList; Owner : IOTAProject; strBaseClass,
  strTestSuiteName : String);

begin
  FOwner := Owner;
  FNewUnitName := strNewUnitName;
  FUnitToBeTested := strUnitToBeTested;
  FTestCases := slTestCases;
  FBaseClass := strBaseClass;
  FTestSuiteName := strTestSuiteName;
end;

(**

  This method allow components to be added to a form.

  @precon  None.
  @postcon Not used in a unit.

  @param   FormEditor as an IOTAFormEditor as a constant

**)
procedure TUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

(**

  This is a getter method for the AncestorName property.

  @precon  None.
  @postcon Not used in a unit.

  @return  a string

**)
function TUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

(**

  This is a getter method for the CreatorType property.

  @precon  None.
  @postcon Returns a type of Unit for a unit file (no form).

  @return  a string

**)
function TUnitCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

(**

  This is a getter method for the Existing property.

  @precon  None.
  @postcon Returns false to indicate a new unUnit.
  @return  a Boolean

**)
function TUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

(**

  This is a getter method for the FileSystem property.

  @precon  None.
  @postcon Returns an empty string to signify the default filing system.

  @return  a string

**)
function TUnitCreator.GetFileSystem: string;
begin
  Result := '';
end;

(**

  This is a getter method for the FormName property.

  @precon  None.
  @postcon Not used in a unit file.

  @return  a string

**)
function TUnitCreator.GetFormName: string;
begin
  Result := '';
end;

(**

  This is a getter method for the ImplFileName property.

  @precon  None.
  @postcon Returns the fully qualified filename for the unit.

  @return  a string

**)
function TUnitCreator.GetImplFileName: string;
begin
  Result :=  GetCurrentDir + '\' + FNewUnitName;
end;

(**

  This is a getter method for the IntfFileName property.

  @precon  None.
  @postcon Not used in Delphi.

  @return  a string

**)
function TUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

(**

  This is a getter method for the MainForm property.

  @precon  None.
  @postcon Not used in a unit file.

  @return  a Boolean

**)
function TUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

(**

  This is a getter method for the Owner property.

  @precon  None.
  @postcon Returns the owner (project) of this new unit.

  @return  an IOTAModule

**)
function TUnitCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

(**

  This is a getter method for the ShowForm property.

  @precon  None.
  @postcon Not used in a unit file.

  @return  a Boolean

**)
function TUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

(**

  This is a getter method for the ShowSource property.

  @precon  None.
  @postcon Returns true to display the unit file source.

  @return  a Boolean

**)
function TUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

(**

  This is a getter method for the Unnamed property.

  @precon  None.
  @postcon Returns true to signify that the file is unsaved.

  @return  a Boolean

**)
function TUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

(**

  This method generates the form file definition.

  @precon  None.
  @postcon Not used in a unit file.

  @param   FormIdent     as a string as a constant
  @param   AncestorIdent as a string as a constant
  @return  an IOTAFile

**)
function TUnitCreator.NewFormFile(const FormIdent,
  AncestorIdent: string)  : IOTAFile;
begin
  Result := Nil;
end;

(**

  This method generates the source code for the unit.

  @precon  None.
  @postcon Generates the source code for this new unit file.

  @param   ModuleIdent   as a string as a constant
  @param   FormIdent     as a string as a constant
  @param   AncestorIdent as a string as a constant
  @return  an IOTAFile

**)
function TUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: String): IOTAFile;
begin
  Result := TUnitCreatorFile.Create(ModuleIdent, FUnitToBeTested, FTestCases,
    FBaseClass, FTestSuiteName);
end;

(**

  This method generates the source code for an interface file.

  @precon  None.
  @postcon Not used in Delphi.

  @param   ModuleIdent   as a string as a constant
  @param   FormIdent     as a string as a constant
  @param   AncestorIdent as a string as a constant
  @return  an IOTAFile

**)
function TUnitCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := Nil;
end;

{ TProjectCreatorFile }

(**

  This is a constructor for the TProjectCreatorFile class.

  @precon  None.
  @postcon Sets the project file name.

  @param   strProjectName as a String

**)
constructor TProjectCreatorFile.Create(strProjectName: String);
begin
  FProjectName := strProjectName;
end;

(**

  This is a getter method for the Age property.

  @precon  None.
  @postcon Returns an unknown date.

  @return  a TDateTime

**)
function TProjectCreatorFile.GetAge: TDateTime;
begin
  Result := -1;
end;

(**

  This is a getter method for the Source property.

  @precon  None.
  @postcon Returns the project source code with a parameter for the project name
           to be replaced.

  @return  a string

**)
function TProjectCreatorFile.GetSource: string;

ResourceString
  strDUnitProjectTemplate = 'DUnitProjectSource';
  strTheDUnitProjectMsg = 'The DUnit Project Template ''%s'' was not found.';

Var
  Res: TResourceStream;
  {$IFDEF D2009}
  strTemp: AnsiString;
  {$ENDIF}

begin
  Res := TResourceStream.Create(HInstance, strDUnitProjectTemplate, RT_RCDATA);
  Try
    If Res.Size = 0 Then
      Raise Exception.CreateFmt(strTheDUnitProjectMsg, [strDUnitProjectTemplate]);
    {$IFNDEF D2009}
    SetLength(Result, Res.Size);
    Res.ReadBuffer(Result[1], Res.Size);
    {$ELSE}
    SetLength(strTemp, Res.Size);
    Res.ReadBuffer(strTemp[1], Res.Size);
    Result := String(strTemp);
    {$ENDIF}
  Finally
    Res.Free;
  End;
  Result := Format(Result, [FProjectName]);
end;

{ TUnitCreatorFile }

(**

  This is a constructor for the TUnitCreatorFile class.

  @precon  None . 
  @postcon Creates an instance of the class . 

  @param   strUnitName       as a String
  @param   strUnitToBeTested as a String
  @param   slTestCases       as a TStringList
  @param   strBaseClass      as a String
  @param   strTestSuiteName  as a String

**)
constructor TUnitCreatorFile.Create(strUnitName, strUnitToBeTested : String;
  slTestCases : TStringList; strBaseClass, strTestSuiteName : String);

begin
  FUnitName := strUnitName;
  FUnitToBeTested := strUnitToBeTested;
  FTestCases := slTestCases;
  FBaseClass := strBaseClass;
  FTestSuiteName := strTestSuiteName;
end;

(**

  This is a getter method for the Age property.

  @precon  None.
  @postcon Returns an invalid file age.

  @return  a TDateTime

**)
function TUnitCreatorFile.GetAge: TDateTime;
begin
  Result := -1;
end;

(**

  This is a getter method for the Source property.

  @precon  None.
  @postcon Returns a string representing the source code for the file.

  @return  a string

**)
function TUnitCreatorFile.GetSource: string;

ResourceString
  strDUnitUnitTemplate = 'DUnitUnitSource';
  strTheDUnitUnitMsg = 'The DUnit Unit Template ''%s'' was not found.';

Var
  Res: TResourceStream;
  {$IFDEF D2009}
  strTemp : AnsiString;
  {$ENDIF}

begin
  Res := TResourceStream.Create(HInstance, strDUnitUnitTemplate, RT_RCDATA);
  Try
    If Res.Size = 0 Then
      Raise Exception.CreateFmt(strTheDUnitUnitMsg, [strDUnitUnitTemplate]);
    {$IFNDEF D2009}
    SetLength(Result, Res.Size);
    Res.ReadBuffer(Result[1], Res.Size);
    {$ELSE}
    SetLength(strTemp, Res.Size);
    Res.ReadBuffer(strTemp[1], Res.Size);
    Result := String(strTemp);
    {$ENDIF}
  Finally
    Res.Free;
  End;
  Result := StringReplace(Result, '$TESTUNITNAME$', FUnitName, []);
  Result := StringReplace(Result, '$UNITTOBETESTED$', FUnitToBeTested, []);
  Result := StringReplace(Result, '$TESTCLASSES$', TestClasses(FTestCases,
    FBaseClass), []);
  Result := StringReplace(Result, '$TESTIMPLEMENTATION$',
    TestImplementation(FTestCases), []);
  Result := StringReplace(Result, '$TESTSUITES$', TestSuites(FTestCases,
    FTestSuiteName), []);
end;

end.
