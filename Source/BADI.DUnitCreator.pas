(**

  This module contains code for getting OTA information and creating DUnit
  project and unit source files.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Oct 2018

**)
Unit BADI.DUnitCreator;

Interface

Uses
  Classes,
  ToolsAPI,
  BADI.Base.Module;

{$INCLUDE 'CompilerDefinitions.inc'}

Type
  (** A type for an array of OTA projects. **)
  TProjectsArray = Array Of IOTAProject;
  (** A type for an array of OTA Modules. **)
  TUnitsArray = Array Of IOTAModuleInfo;

  (** A procedural type for error event handlers. **)
  TErrorProc = Procedure(Const strMsg: String) Of Object;

  (** A class to handle the creation of the DUnit files. **)
  TDUnitCreator = Class
  Private
    Procedure CheckReadOnlyStatus(Module: IOTAModule; Const strModuleReadOnly: String);
    {$IFDEF D2005} Strict {$ENDIF} Private
    FProjects    : TProjectsArray;
    FProjectCount: Integer;
    FUnits       : TUnitsArray;
    FUnitCount   : Integer;
    FProject     : IOTAProject;
    FModule      : TBaseLanguageModule;
    FUnit        : IOTAModule;
    FErrors      : TErrorProc;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure AddProject(Project: IOTAProject);
    Procedure AddUnit(AUnit: IOTAModuleInfo);
    Procedure AddNewTestSuites(M: TBaseLanguageModule; slTestCases: TStrings;
      Const strTestSuiteName: String);
    Procedure AddNewTestImplementations(M: TBaseLanguageModule; slTestCases: TStrings;
      Const strClassNameMask, strMethodNameMask : String);
    Procedure AddNewTestClasses(M: TBaseLanguageModule; slTestCases: TStrings;
      Const strBaseClass, strClassNameMask: String);
    Function DoesClassExist(M: TBaseLanguageModule; Const strClassName: String): Boolean;
    Function DoesMethodExist(M: TBaseLanguageModule;
      Const strClassName, strMethodName: String): Boolean;
    Procedure RaiseError(Const strMsg: String);
    Procedure AddUnitToBeTestedToUsesClause(M: TBaseLanguageModule;
      Const strUnitToBeTested: String);
    Procedure AddNewTestMethodsToClass(M: TBaseLanguageModule; slTestCases: TStringList);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure GetExistingDUnitProjects;
    Procedure CreateTestProject(Const strNewProjectName: String);
    Procedure ActivateProject(iProject: Integer);
    Function GetProject(iProject: Integer): String;
    Function GetProjectCount: Integer;
    Function GetUnit(iUnit: Integer): String;
    Function GetUnitCount: Integer;
    Procedure GetExistingDUnitUnits(iProject: Integer);
    Procedure CreateTestUnit(Const strNewUnitName, strUnitToBeTested: String;
      slTestCases: TStringList; Const strBaseClass, strTestSuiteName, strClassnameMask,
      strMethodNameMask: String);
    Procedure UpdateTestUnit(iUnit: Integer; Const strUnitToBeTested: String;
      slTestCases: TStringList; Const strBaseClass, strTestSuiteName, strClassNameMask,
      strMethodNameMask : String);
    Function DoesProjectExist(Const strProjectName: String): Boolean;
    Function DoesUnitExist(iProject: Integer; Const strUnitName: String): Boolean;
    Procedure AddUnitToBeTested(Const strFileName: String);
    (**
      This property returns the indexed project reference.
      @precon  None.
      @postcon Returns the indexed project reference.
      @param   iProject as       an Integer
      @return  a String
    **)
    Property Projects[iProject: Integer]: String Read GetProject;
    (**
      This property returns the number of projects referenced.
      @precon  None.
      @postcon Returns the indexed project reference.
      @return  an Integer
    **)
    Property ProjectCount: Integer Read GetProjectCount;
    (**
      This property returns the indexed unit reference.
      @precon  None.
      @postcon Returns the indexed unit reference.
      @param   iUnit as       an Integer
      @return  a String
    **)
    Property Units[iUnit: Integer]: String Read GetUnit;
    (**
      This property returns the number of units referenced.
      @precon  None.
      @postcon Returns the indexed unit reference.
      @return  an Integer
    **)
    Property UnitCount: Integer Read GetUnitCount;
    (**
      This property returns the base language module interface.
      @precon  None.
      @postcon Returns the base language module interface.
      @return  a TBaseLanguageModule
    **)
    Property Module: TBaseLanguageModule Read FModule;
    (**
      This property gets and sets the event handler for errors.
      @precon  None.
      @postcon Gets and sets the event handler for errors.
      @return  a TErrorProc
    **)
    Property Errors: TErrorProc Read FErrors Write FErrors;
  End;

Implementation

Uses
  SysUtils,
  Windows,
  BADI.ToolsAPIUtils,
  BADI.ElementContainer,
  BADI.Module.Dispatcher,
  BADI.Types,
  BADI.ResourceStrings,
  BADI.Functions;

Const
  (** A constant to define the growth capacity of the collections. **)
  iCAPACITY: Integer = 25;

Type
  (** A class to handle the creation of the DUnit project file. **)
  TProjectCreator = Class(TInterfacedObject, IUnknown, IOTACreator, IOTAProjectCreator
    {$IFDEF D0005}, IOTAProjectCreator50 {$ENDIF}
    {$IFDEF D2005}, IOTAProjectCreator80 {$ENDIF})
    {$IFDEF D2005} Strict {$ENDIF} Private
    FNewProjectName: String;
  Public
    Constructor Create(Const strNewProjectName: String);
    Function GetCreatorType: String;
    Function GetExisting: Boolean;
    Function GetFileSystem: String;
    Function GetOwner: IOTAModule;
    Function GetUnnamed: Boolean;
    Function GetFileName: String;
    Function GetOptionFileName: String;
    Function GetShowSource: Boolean;
    Procedure NewDefaultModule;
    Function NewOptionSource(Const ProjectName: String): IOTAFile;
    Procedure NewProjectResource(Const Project: IOTAProject);
    Function NewProjectSource(Const ProjectName: String): IOTAFile;
    {$IFDEF D0005}
    Procedure NewDefaultProjectModule(Const Project: IOTAProject);
    {$ENDIF}
    {$IFDEF D2005}
    Function GetProjectPersonality: String;
    {$ENDIF}
  End;

  (** This class creates a IOTAFIle interface for generating the project file
      source. **)
  TProjectCreatorFile = Class(TInterfacedObject, IUnknown, IOTAFile)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FProjectName: String;
  Public
    Constructor Create(strProjectName: String);
    Function GetAge: TDateTime;
    Function GetSource: String;
  End;

  (** This class creates the test unit file. **)
  TUnitCreator = Class(TInterfacedObject, IUnknown, IOTACreator, IOTAModuleCreator)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FNewUnitName    : String;
    FOwner          : IOTAProject;
    FUnitToBeTested : String;
    FTestCases      : TStringList;
    FBaseClass      : String;
    FTestSuiteName  : String;
    FClassNameMask  : String;
    FMethodNameMask : String;
  Public
    Constructor Create(strNewUnitName, strUnitToBeTested: String;
      slTestCases: TStringList; Owner: IOTAProject;
      strBaseClass, strTestSuiteName, strClassNameMask, strMethodNameMask: String);
    Function GetCreatorType: String;
    Function GetExisting: Boolean;
    Function GetFileSystem: String;
    Function GetOwner: IOTAModule;
    Function GetUnnamed: Boolean;
    Procedure FormCreated(Const FormEditor: IOTAFormEditor);
    Function GetAncestorName: String;
    Function GetFormName: String;
    Function GetImplFileName: String;
    Function GetIntfFileName: String;
    Function GetMainForm: Boolean;
    Function GetShowForm: Boolean;
    Function GetShowSource: Boolean;
    Function NewFormFile(Const FormIdent: String; Const AncestorIdent: String): IOTAFile;
    Function NewImplSource(Const ModuleIdent: String; Const FormIdent: String;
      Const AncestorIdent: String): IOTAFile;
    Function NewIntfSource(Const ModuleIdent: String; Const FormIdent: String;
      Const AncestorIdent: String): IOTAFile;
  End;

  (** This class creates a IOTAFile interface for generating the unit file
      source. **)
  TUnitCreatorFile = Class(TInterfacedObject, IUnknown, IOTAFile)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FUnitName       : String;
    FUnitToBeTested : String;
    FTestCases      : TStringList;
    FBaseClass      : String;
    FTestSuiteName  : String;
    FClassNameMask  : String;
    FMethodNameMask : String;
  Public
    Constructor Create(strUnitName, strUnitToBeTested: String; slTestCases: TStringList;
      strBaseClass, strTestSuiteName, strClassNameMask, strMethodNameMask: String);
    Function GetAge: TDateTime;
    Function GetSource: String;
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
Function IsTestFramework(Project: IOTAProject): Boolean;

Const
  strTestUnits : Array[1..5] Of String = (
    'guitestrunner',
    'testframework',
    'testinsight.dunit',
    'testinsight.dunit2',
    'texttestrunner'
  );

Var
  S          : IOTASourceEditor;
  M          : TBaseLanguageModule;
  E          : TElementContainer;
  strFileName: String;
  iUnit: Integer;

Begin
  Result := False;
  S := TBADIToolsAPIFunctions.SourceEditor(Project.CurrentEditor.Module);
  strFileName := Project.CurrentEditor.FileName;
  M := TBADIDispatcher.BADIDispatcher.Dispatcher(TBADIToolsAPIFunctions.EditorAsString(S), strFileName,
    True, [moParse]);
  Try
    If M <> Nil Then
      Begin
        E := M.FindElement(strUses);
        If E <> Nil Then
          For iUnit := 1 To E.ElementCount Do
            Begin
              If IsKeyWord(LowerCase(E.Elements[iUnit].Identifier), strTestUnits) Then
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

  @param   strText as a String as a constant
  @return  a String

**)
Function TestClassName(Const strText: String): String;

Begin
  Result := GetField(strText, '=', 1);
  If Result = '' Then
    Result := 'Functions';
End;

(**

  This method returns a string representing the name of the Class to be Tested with prefixes and
  suffixes as required in the mask.

  @precon  None.
  @postcon Returns a string representing the name of the Class to be Tested.

  @param   strTestClass     as a String as a constant
  @param   strClassNameMask as a String as a constant
  @return  a String

**)
Function GetClassToTest(Const strTestClass, strClassNameMask : String) : String;

Var
  iPos: Integer;

Begin
  Result := strTestClass;
  iPos := Pos('%s', strClassNameMask);
  If iPos > 0 Then
    Begin
      Delete(Result, iPos , Length(strClassNameMask) - (iPos + 1));
      Delete(Result, 1, iPos - 1);
    End;
End;

(**

  This method returns a string representing the name of the Method to be Tested with prefixes and
  suffixes as required in the mask.

  @precon  None.
  @postcon Returns a string representing the name of the Method to be Tested.

  @param   strTestMethod     as a String as a constant
  @param   strMethodNameMask as a String as a constant
  @return  a String

**)
Function GetMethodToTest(Const strTestMethod, strMethodNameMask : String) : String;

Var
  iPos: Integer;

Begin
  Result := strTestMethod;
  iPos := Pos('%s', strMethodNameMask);
  If iPos > 0 Then
    Begin
      Delete(Result, iPos , Length(strMethodNameMask) - (iPos + 1));
      Delete(Result, 1, iPos - 1);
    End;
End;

(**

  This function returns the code associated and to be inserted into the new unit for the classes
  which implement the tests selected.

  @precon  slTestCases must be a valid instance of a string list .
  @postcon Returns the code associated and to be inserted into the new unit for the classes which
           implement the tests selected .

  @param   slTestCases      as a TStringList
  @param   strBaseClass     as a String as a constant
  @param   strClassNameMask as a String as a constant
  @return  a String

**)
Function TestClasses(slTestCases: TStringList; Const strBaseClass, strClassNameMask : String): String;

Var
  strLastClass    : String;
  i               : Integer;
  strTestClass    : String;
  strClassToTest  : String;
  strTestMethod   : String;

Begin
  Result       := '';
  strLastClass := '';
  For i        := 0 To slTestCases.Count - 1 Do
    Begin
      strTestClass  := TestClassName(slTestCases[i]);
      strClassToTest := GetClassToTest(strTestClass, strClassNameMask);
      strTestMethod := GetField(slTestCases[i], '=', 2);
      If strLastClass <> strTestClass Then
        Begin
          If Result <> '' Then
            Result := Result + '  End;'#13#10#13#10;
          Result   := Result + '  //'#13#10;
          Result   := Result + Format('  // Test Class for the %s Class Methods.'#13#10,
            [strClassToTest]);
          Result := Result + '  //'#13#10;
          Result := Result + Format('  %s = Class(%s)'#13#10, [strTestClass, strBaseClass]);
          Result := Result + '  Strict Private'#13#10;
          If strTestClass <> 'Functions' Then
            Result := Result + Format('    F%s : %s;'#13#10, [strClassToTest, strClassToTest]);
          Result := Result + '  Public'#13#10;
          If strTestClass <> 'Functions' Then
            Result := Result + '    Procedure SetUp; Override;'#13#10;
          If strTestClass <> 'Functions' Then
            Result := Result + '    Procedure TearDown; Override;'#13#10;
          Result   := Result + '  Published'#13#10;
        End;
      Result       := Result + Format('    Procedure %s;'#13#10, [strTestMethod]);
      strLastClass := strTestClass;
    End;
  Result := Result + '  End;'#13#10;
End;

(**

  This function returns the implementation methods for the selected test cases.

  @precon  slTestCases must be a valid instance of a string list.
  @postcon Returns the implementation methods for the selected test cases.

  @param   slTestCases       as a TStringList
  @param   strClassNameMask  as a String as a constant
  @param   strMethodNameMask as a String as a constant
  @return  a String

**)
Function TestImplementation(slTestCases: TStringList; Const strClassNameMask,
  strMethodNameMask : String): String;

Var
  strLastClass    : String;
  i               : Integer;
  strTestClass    : String;
  strClassToTest  : String;
  strTestMethod   : String;
  strMethodToTest : String;

Begin
  Result       := #13#10;
  strLastClass := '';
  For i        := 0 To slTestCases.Count - 1 Do
    Begin
      strTestClass  := TestClassName(slTestCases[i]);
      strClassToTest := GetClassToTest(strTestClass, strClassNameMask);
      strTestMethod := GetField(slTestCases[i], '=', 2);
      strMethodToTest := GetMethodToTest(strTestMethod, strMethodNameMask);
      If strLastClass <> strTestClass Then
        Begin
          Result := Result + '//'#13#10;
          Result := Result + Format('// Test Methods for Class %s.'#13#10, [strClassToTest]);
          Result := Result + '//'#13#10;
          If strTestClass <> 'Functions' Then
            Begin
              Result := Result + Format('Procedure %s.Setup;'#13#10, [strTestClass]);
              Result := Result + 'Begin'#13#10;
              Result := Result +
                Format('  F%s := %s.Create; //: @debug Setup constructor for %s.'#13#10,
                [strClassToTest, strClassToTest, strClassToTest]);
              Result := Result + 'End;'#13#10;
              Result := Result + ''#13#10;
              Result := Result + Format('Procedure %s.TearDown;'#13#10, [strTestClass]);
              Result := Result + ''#13#10;
              Result := Result + 'Begin'#13#10;
              Result := Result + Format('  F%s.Free;'#13#10, [strClassToTest]);
              Result := Result + 'End;'#13#10;
              Result := Result + ''#13#10;
            End;
        End;
      Result := Result + Format('Procedure %s.%s;'#13#10, [strTestClass, strTestMethod]);
      Result := Result + ''#13#10;
      Result := Result + 'Begin'#13#10;
      Result := Result + Format('  //: @todo Implement Check for F%s.%s.'#13#10,
        [strClassToTest, strMethodToTest]);
      Result       := Result + 'End;'#13#10;
      Result       := Result + ''#13#10;
      strLastClass := strTestClass;
    End;
End;

(**

  This function returns the initialisation code for the newly recreated test
  methods.

  @precon  slTestCases must be a valid instance of a string list .
  @postcon Returns the initialisation code for the newly recreated test methods
           .

  @param   slTestCases      as a TStringList
  @param   strTestSuiteName as a String as a constant
  @return  a String

**)
Function TestSuites(slTestCases: TStringList; Const strTestSuiteName: String): String;

Var
  i           : Integer;
  strLastClass: String;
  strClass    : String;

Begin
  Result       := '';
  strLastClass := '';
  For i        := 0 To slTestCases.Count - 1 Do
    Begin
      strClass := TestClassName(slTestCases[i]);
      If strLastClass <> strClass Then
        Begin
          If Result <> '' Then
            Result := Result + #13#10;
          Result   := Result + Format('  RegisterTest(''%s'', %s.Suite);',
            [strTestSuiteName, strClass]);
        End;
      strLastClass := strClass;
    End;
End;

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
    Result := CompareText(List[Index1], List[Index2]);
End;

(**

  This method makes the indexed project the active project.

  @precon  iProject must be a valid index into the project array.
  @postcon Makes the indexed project the active project.

  @param   iProject as an Integer

**)
Procedure TDUnitCreator.ActivateProject(iProject: Integer);

ResourceString
  strProjectReadOnly = 'Project source module is read only!';

Begin
  FProject                   := FProjects[iProject];
  TBADIToolsAPIFunctions.ProjectGroup.ActiveProject := FProject;
  CheckReadOnlyStatus(FProject, strProjectReadOnly);
End;

(**

  This method adds new test classes to the interface of the existing source module.

  @precon  M and slTestCases must be valid instances.
  @postcon Adds new test classes to the interface of the existing source module.

  @param   M                as a TBaseLanguageModule
  @param   slTestCases      as a TStrings
  @param   strBaseClass     as a String as a constant
  @param   strClassNameMask as a String as a constant

**)
Procedure TDUnitCreator.AddNewTestClasses(M: TBaseLanguageModule; slTestCases: TStrings;
  Const strBaseClass, strClassNameMask: String);

Var
  i           : Integer;
  SE          : IOTASourceEditor;
  Writer      : IOTAEditWriter;
  strLastClass: String;
  iPos        : Integer;
  CharPos     : TOTACharPos;
  strTestClass    : String;
  strMethod   : String;
  iToken      : Integer;
  boolEndClass: Boolean;
  strClassToTest : String;

Begin
  iToken := M.FindToken('Implementation');
  If iToken > -1 Then
    Begin
      SE := TBADIToolsAPIFunctions.SourceEditor(FUnit);
      Try
        Writer := SE.CreateUndoableWriter;
        Try
          CharPos.CharIndex := 0;
          CharPos.Line      := M.Tokens[iToken].Line;
          iPos              := SE.EditViews[0].CharPosToPos(CharPos);
          Writer.CopyTo(iPos);
          strLastClass := '';
          boolEndClass := False;
          For i        := 0 To slTestCases.Count - 1 Do
            Begin
              strTestClass  := TestClassName(slTestCases[i]);
              strClassToTest := GetClassToTest(strTestClass, strClassNameMask);
              strMethod := GetField(slTestCases[i], '=', 2);
              If Not DoesClassExist(M, strTestClass) Then
                Begin
                  If strLastClass <> strTestClass Then
                    Begin
                      If boolEndClass Then
                        TBADIToolsAPIFunctions.OutputText(Writer, '  End;'#13#10#13#10);
                      TBADIToolsAPIFunctions.OutputText(Writer, '  //'#13#10);
                      TBADIToolsAPIFunctions.OutputText(Writer,
                        Format('  // Test Class for the %s Class Methods.'#13#10,
                          [strClassToTest]));
                      TBADIToolsAPIFunctions.OutputText(Writer, '  //'#13#10);
                      TBADIToolsAPIFunctions.OutputText(Writer, Format('  %s = Class(%s)'#13#10,
                          [strTestClass, strBaseClass]));
                      TBADIToolsAPIFunctions.OutputText(Writer, '  Strict Private'#13#10);
                      If strTestClass <> 'Functions' Then
                        TBADIToolsAPIFunctions.OutputText(Writer, Format('    F%s : %s;'#13#10, [strClassToTest,
                          strClassToTest]));
                      TBADIToolsAPIFunctions.OutputText(Writer, '  Public'#13#10);
                      If strTestClass <> 'Functions' Then
                        TBADIToolsAPIFunctions.OutputText(Writer, '    Procedure SetUp; Override;'#13#10);
                      If strTestClass <> 'Functions' Then
                        TBADIToolsAPIFunctions.OutputText(Writer, '    Procedure TearDown; Override;'#13#10);
                      TBADIToolsAPIFunctions.OutputText(Writer, '  Published'#13#10);
                    End;
                  TBADIToolsAPIFunctions.OutputText(Writer, Format('    Procedure %s;'#13#10, [strMethod]));
                  boolEndClass := True;
                End;
              strLastClass := strTestClass;
            End;
          If boolEndClass Then
            TBADIToolsAPIFunctions.OutputText(Writer, '  End;'#13#10#13#10);
        Finally
          Writer := Nil;
        End;
      Finally
        SE := Nil;
      End;
    End;
End;

(**

  This method adds new test suites to the initialisatio section assuming that the file has an
  initialisatin section.

  @precon  M must be a valid language module .
  @postcon Adds new test suites to the initialisatio section assuming that the file has an
           initialisatin section .

  @param   M                as a TBaseLanguageModule
  @param   slTestCases      as a TStrings
  @param   strTestSuiteName as a String as a constant

**)
Procedure TDUnitCreator.AddNewTestSuites(M: TBaseLanguageModule; slTestCases: TStrings;
  Const strTestSuiteName: String);

ResourceString
  strFinalEndNotFound = 'Final reserved word ''End'' not found.';
  strFinalPeriodNotFound = 'Final period (.) not found.';

Var
  i            : Integer;
  SE           : IOTASourceEditor;
  Writer       : IOTAEditWriter;
  strLastClass : String;
  iPos         : Integer;
  CharPos      : TOTACharPos;
  strTestClass : String;

Begin
  If M.Tokens[M.TokenCount - 1].Token = '.' Then
    Begin
      If M.Tokens[M.TokenCount - 2].UToken = 'END' Then
        Begin
          SE := TBADIToolsAPIFunctions.SourceEditor(FUnit);
          Try
            Writer := SE.CreateUndoableWriter;
            Try
              CharPos.CharIndex := 0;
              CharPos.Line      := M.Tokens[M.TokenCount - 2].Line;
              iPos              := SE.EditViews[0].CharPosToPos(CharPos);
              Writer.CopyTo(iPos);
              strLastClass := '';
              For i        := 0 To slTestCases.Count - 1 Do
                Begin
                  strTestClass := TestClassName(slTestCases[i]);
                  If strLastClass <> strTestClass Then
                    If Not DoesClassExist(M, strTestClass) Then
                      TBADIToolsAPIFunctions.OutputText(Writer,
                        Format('  RegisterTest(''%s'', %s.Suite);'#13#10,
                          [strTestSuiteName, strTestClass]));
                  strLastClass := strTestClass;
                End;
            Finally
              Writer := Nil;
            End;
          Finally
            SE := Nil;
          End;
        End
      Else
        RaiseError(strFinalEndNotFound);
    End
  Else
    RaiseError(strFinalPeriodNotFound);
End;

(**

  This method adds new method implementations to the existing source file.

  @precon  M and slTestCases must be valid instances.
  @postcon Adds new method implementations to the existing source file.

  @note    This method modifies the slTestCases method names IF a name clash is found with existing
           methods in the class. This means that there are no checks to be made when insert these
           methods into the existing class delcarations.

  @param   M                 as a TBaseLanguageModule
  @param   slTestCases       as a TStrings
  @param   strClassNameMask  as a String as a constant
  @param   strMethodNameMask as a String as a constant

**)
Procedure TDUnitCreator.AddNewTestImplementations(M: TBaseLanguageModule;
  slTestCases: TStrings; Const strClassNameMask, strMethodNameMask : String);

Var
  i               : Integer;
  SE              : IOTASourceEditor;
  Writer          : IOTAEditWriter;
  strLastClass    : String;
  iPos            : Integer;
  CharPos         : TOTACharPos;
  strTestClass    : String;
  strTestMethod   : String;
  iToken          : Integer;
  iIndex          : Integer;
  strClassToTest  : String;
  strMethodToTest : String;

Begin
  iToken := M.FindToken('Initialization');
  If iToken > -1 Then
    Begin
      SE := TBADIToolsAPIFunctions.SourceEditor(FUnit);
      Try
        Writer := SE.CreateUndoableWriter;
        Try
          CharPos.CharIndex := 0;
          CharPos.Line      := M.Tokens[iToken].Line;
          iPos              := SE.EditViews[0].CharPosToPos(CharPos);
          Writer.CopyTo(iPos);
          strLastClass := '';
          For i        := 0 To slTestCases.Count - 1 Do
            Begin
              strTestClass  := TestClassName(slTestCases[i]);
              strClassToTest := GetClassToTest(strTestClass, strClassNameMask);
              strTestMethod := GetField(slTestCases[i], '=', 2);
              strMethodToTest := GetMethodToTest(strTestMethod, strMethodNameMask);
              If strLastClass <> strTestClass Then
                Begin
                  If Not DoesClassExist(M, strTestClass) Then
                    Begin
                      TBADIToolsAPIFunctions.OutputText(Writer, '//'#13#10);
                      TBADIToolsAPIFunctions.OutputText(Writer, Format('// Test methods for the class %s.'#13#10,
                          [strClassToTest]));
                      TBADIToolsAPIFunctions.OutputText(Writer, '//'#13#10);
                      If strTestClass <> 'Functions' Then
                        Begin
                          TBADIToolsAPIFunctions.OutputText(Writer, Format('Procedure %s.Setup;'#13#10,
                              [strTestClass]));
                          TBADIToolsAPIFunctions.OutputText(Writer, ''#13#10);
                          TBADIToolsAPIFunctions.OutputText(Writer, 'Begin'#13#10);
                          TBADIToolsAPIFunctions.OutputText(Writer,
                            Format('  F%s := %s.Create; //: @debug Setup constructor for %s.'#13#10,
                              [strClassToTest, strClassToTest, strClassToTest]));
                          TBADIToolsAPIFunctions.OutputText(Writer, 'End;'#13#10);
                          TBADIToolsAPIFunctions.OutputText(Writer, ''#13#10);
                          TBADIToolsAPIFunctions.OutputText(Writer, Format('Procedure %s.TearDown;'#13#10,
                              [strTestClass]));
                          TBADIToolsAPIFunctions.OutputText(Writer, ''#13#10);
                          TBADIToolsAPIFunctions.OutputText(Writer, 'Begin'#13#10);
                          //: @todo The below need work.
                          TBADIToolsAPIFunctions.OutputText(Writer, Format('  F%s.Free;'#13#10, [strClassToTest]));
                          TBADIToolsAPIFunctions.OutputText(Writer, 'End;'#13#10);
                          TBADIToolsAPIFunctions.OutputText(Writer, ''#13#10);
                        End;
                    End;
                End;
              iIndex := 1;
              While DoesMethodExist(M, strTestClass, strTestMethod) Do
                Begin
                  strTestMethod := Format('%s%d', [GetField(slTestCases[i], '=', 2), iIndex]);
                  Inc(iIndex);
                End;
              slTestCases[i] := Format('%s=%s', [strTestClass, strTestMethod]);
              TBADIToolsAPIFunctions.OutputText(Writer, Format('Procedure %s.%s;'#13#10,
                  [strTestClass, strTestMethod]));
              TBADIToolsAPIFunctions.OutputText(Writer, ''#13#10);
              TBADIToolsAPIFunctions.OutputText(Writer, 'Begin'#13#10);
              TBADIToolsAPIFunctions.OutputText(Writer, Format('  //: @todo Implement Check for F%s.%s.'#13#10,
                  [strClassToTest, strMethodToTest]));
              TBADIToolsAPIFunctions.OutputText(Writer, 'End;'#13#10);
              TBADIToolsAPIFunctions.OutputText(Writer, ''#13#10);
              strLastClass := strTestClass;
            End;
        Finally
          Writer := Nil;
        End;
      Finally
        SE := Nil;
      End;
    End;
End;

(**

  This method adds new test cases to existing class at the end of the class.
  The assumption is that the last section of the class is Published.

  @precon  M and slTestCases must eb valid instances.
  @postcon Adds new test cases to existing class at the end of the class.

  @param   M           as a TBaseLanguageModule
  @param   slTestCases as a TStringList

**)
Procedure TDUnitCreator.AddNewTestMethodsToClass(M: TBaseLanguageModule;
  slTestCases: TStringList);

Var
  SE      : IOTASourceEditor;
  Writer  : IOTAEditWriter;
  iPos    : Integer;
  CharPos : TOTACharPos;
  strTestClass: String;
  T       : TElementContainer;
  iMethod : Integer;

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
  Function FindEndOfClass(strClassName: String): Integer;

  Var
    C: TElementContainer;
    i: Integer;

  Begin
    Result := -1;
    C      := T.FindElement(strClassName);
    If C <> Nil Then
      For i := 0 To M.TokenCount - 1 Do
        If (M.Tokens[i].Line >= C.Line) And (M.Tokens[i].UToken = 'END') Then
          Begin
            Result := M.Tokens[i].Line;
            Break;
          End;
  End;

Begin
  T := M.FindElement(strTypesLabel);
  If T <> Nil Then
    Begin
      // Find class end tokens
      For iMethod := 0 To slTestCases.Count - 1 Do
        Begin
          strTestClass                 := TestClassName(slTestCases[iMethod]);
          slTestCases.Objects[iMethod] := TObject(FindEndOfClass(strTestClass));
        End;
      // Delete test with no end token (i.e. only ones to insert)
      For iMethod := slTestCases.Count - 1 DownTo 0 Do
        If Integer(slTestCases.Objects[iMethod]) = -1 Then
          slTestCases.Delete(iMethod);
      // Sort by end token line number descending
      slTestCases.CustomSort(SortClassEndTokens);
      For iMethod                    := 0 To slTestCases.Count - 1 Do
        slTestCases.Objects[iMethod] :=
          TObject(Integer(slTestCases.Objects[iMethod]) + iMethod);
      For iMethod := 0 To slTestCases.Count - 1 Do
        Begin
          SE := TBADIToolsAPIFunctions.SourceEditor(FUnit);
          Try
            Writer := SE.CreateUndoableWriter;
            Try
              CharPos.CharIndex := 0;
              CharPos.Line      := Integer(slTestCases.Objects[iMethod]);
              iPos              := SE.EditViews[0].CharPosToPos(CharPos);
              Writer.CopyTo(iPos);
              TBADIToolsAPIFunctions.OutputText(Writer, Format('    Procedure %s;'#13#10,
                  [GetField(slTestCases[iMethod], '=', 2)]));
            Finally
              Writer := Nil;
            End;
          Finally
            SE := Nil;
          End;
        End;
    End
  Else
    RaiseError('Could not find Types!');
End;

(**

  This method adds the given project to the project array.

  @precon  None.
  @postcon Adds the given project to the project array.

  @param   Project as an IOTAProject

**)
Procedure TDUnitCreator.AddProject(Project: IOTAProject);

Var
  T: TProjectsArray;
  i: Integer;

Begin
  Inc(FProjectCount);
  If FProjectCount >= High(FProjects) Then
    Begin
      SetLength(T, Succ(High(FProjects)) + iCAPACITY);
      For i     := Low(FProjects) To High(FProjects) Do
        T[i]    := FProjects[i];
      FProjects := T;
    End;
  FProjects[FProjectCount - 1] := Project;
End;

(**

  This method adds the given unit to the unit array.

  @precon  None.
  @postcon Adds the given unit to the unit array.

  @param   AUnit as an IOTAModuleInfo

**)
Procedure TDUnitCreator.AddUnit(AUnit: IOTAModuleInfo);

Var
  T: TUnitsArray;
  i: Integer;

Begin
  Inc(FUnitCount);
  If FUnitCount >= High(FUnits) Then
    Begin
      SetLength(T, Succ(High(FUnits)) + iCAPACITY);
      For i  := Low(FUnits) To High(FUnits) Do
        T[i] := FUnits[i];
      FUnits := T;
    End;
  FUnits[FUnitCount - 1] := AUnit;
End;

(**

  This method adds the unit file to be tested to the project IF it doesn`t
  already exist.

  @precon  None.
  @postcon Adds the unit file to be tested to the project IF it doesn`t
           already exist.

  @param   strFileName as a String as a constant

**)
Procedure TDUnitCreator.AddUnitToBeTested(Const strFileName: String);

Var
  i        : Integer;
  boolFound: Boolean;

Begin
  boolFound := False;
  For i     := 0 To FProject.GetModuleCount - 1 Do
    If AnsiCompareFileName(strFileName, FProject.GetModule(i).FileName) = 0 Then
      Begin
        boolFound := True;
        Break;
      End;
  If Not boolFound Then
    FProject.AddFile(strFileName, True);
End;

(**

  This method checks if the given module is readonly. If so outputs a message and
  aborts.

  @precon  Module must be a valid instance.
  @postcon Checks if the given module is readonly. If so outputs a message and
           aborts.

  @param   Module            as an IOTAModule
  @param   strModuleReadOnly as a string as a constant

**)
Procedure TDUnitCreator.CheckReadOnlyStatus(Module: IOTAModule;
  Const strModuleReadOnly: String);

Var
  SE: IOTASourceEditor;

Begin
  SE := TBADIToolsAPIFunctions.SourceEditor(Module);
  If SE <> Nil Then
    Begin
      If SE.EditViewCount > 0 Then
        Begin
          If SE.EditViews[0] <> Nil Then
            Begin
              If SE.EditViews[0].Buffer <> Nil Then
                Begin
                  If SE.EditViews[0].Buffer.IsReadOnly Then
                    Begin
                      RaiseError(strModuleReadOnly);
                      Abort;
                    End;
                End;
            End;
        End;
    End;
End;

(**

  This method inserts the unit to be tested into the uses clause IF it does not
  already exist.

  @precon  M must be a valid instance.
  @postcon Inserts the unit to be tested into the uses clause IF it does not
           already exist.

  @param   M                 as a TBaseLanguageModule
  @param   strUnitToBeTested as a String as a constant

**)
Procedure TDUnitCreator.AddUnitToBeTestedToUsesClause(M: TBaseLanguageModule;
  Const strUnitToBeTested: String);

ResourceString
  strUsesClauseNotFound = 'No Uses clause found in Interface!';
  strInterfaceNotFound = 'Interface not found!';

Var
  U        : TElementContainer;
  iToken   : Integer;
  i        : Integer;
  iImplLine: Integer;
  CharPos  : TOTACharPos;
  SE       : IOTASourceEditor;
  Writer   : IOTAEditWriter;
  iPos     : Integer;
  C        : TElementContainer;

Begin
  U := M.FindElement(strUses);
  If U = Nil Then
    Begin
      RaiseError(strUsesClauseNotFound);
      Exit;
    End;
  If U.FindElement(strUnitToBeTested) <> Nil Then
    Exit;
  CharPos.CharIndex := 0;
  CharPos.Line      := 0;
  iToken            := M.FindToken('Implementation');
  If iToken > -1 Then
    Begin
      iImplLine := M.Tokens[iToken].Line;
      For i     := 1 To U.ElementCount Do
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
      SE := TBADIToolsAPIFunctions.SourceEditor(FUnit);
      Try
        Writer := SE.CreateUndoableWriter;
        Try
          Dec(CharPos.CharIndex, 1);
          iPos := SE.EditViews[0].CharPosToPos(CharPos);
          Writer.CopyTo(iPos);
          If CharPos.CharIndex + Length(strUnitToBeTested) < 80 Then
            TBADIToolsAPIFunctions.OutputText(Writer, Format(', %s', [strUnitToBeTested]))
          Else
            TBADIToolsAPIFunctions.OutputText(Writer, Format(','#13#10#32#32'%s', [strUnitToBeTested]));
        Finally
          Writer := Nil;
        End;
      Finally
        SE := Nil;
      End;
    End
  Else
    RaiseError(strInterfaceNotFound);
End;

(**

  This is a constructor for the TDUnitCreator class.

  @precon  None.
  @postcon Initialise the array sizes.

**)
Constructor TDUnitCreator.Create;

Begin
  FProjectCount := 0;
  FUnitCount    := 0;
  With TBADIToolsAPIFunctions.ActiveSourceEditor Do
    FModule := TBADIDispatcher.BADIDispatcher.Dispatcher(
      TBADIToolsAPIFunctions.EditorAsString(TBADIToolsAPIFunctions.ActiveSourceEditor), FileName,
      Modified, [moParse]);
End;

(**

  This method creates a new DUnit test project in the project group.

  @precon  None .
  @postcon Creates a new DUnit test project in the project group .

  @param   strNewProjectName as a String as a constant

**)
Procedure TDUnitCreator.CreateTestProject(Const strNewProjectName: String);

Var
  P: TProjectCreator;

Begin
  P        := TProjectCreator.Create(strNewProjectName);
  FProject := (BorlandIDEServices As IOTAModuleServices).CreateModule(P) As IOTAProject;
End;

(**

  This is a constructor for the TDUnitCreator class.

  @precon  None .
  @postcon Creates the

  @param   strNewUnitName    as a String as a constant
  @param   strUnitToBeTested as a String as a constant
  @param   slTestCases       as a TStringList
  @param   strBaseClass      as a String as a constant
  @param   strTestSuiteName  as a String as a constant
  @param   strClassnameMask  as a String as a constant
  @param   strMethodNameMask as a String as a constant

**)
Procedure TDUnitCreator.CreateTestUnit(Const strNewUnitName, strUnitToBeTested: String;
  slTestCases: TStringList; Const strBaseClass, strTestSuiteName, strClassnameMask,
  strMethodNameMask: String);

Begin
  (BorlandIDEServices As IOTAModuleServices)
    .CreateModule(TUnitCreator.Create(strNewUnitName, strUnitToBeTested, slTestCases,
      FProject, strBaseClass, strTestSuiteName, strClassNameMask, strMethodNameMask));
End;

(**

  This is a destructor for the TDUnitCreator class.

  @precon  None.
  @postcon Frees the arrays.

**)
Destructor TDUnitCreator.Destroy;
Begin
  FModule.Free;
  FProjects := Nil;
  FUnits    := Nil;
  Inherited Destroy;
End;

(**

  This method returns true IF the named class exists in the given module.
  
  @precon  M must be valid language module.
  @postcon Returns true IF the named class exists in the given module.
  
  @param   M            as a TBaseLanguageModule
  @param   strClassName as a String as a constant
  @return  a Boolean
  
**)
Function TDUnitCreator.DoesClassExist(M: TBaseLanguageModule;
  Const strClassName: String): Boolean;

Var
  T: TElementContainer;

Begin
  Result := False;
  T      := M.FindElement(strTypesLabel);
  If T <> Nil Then
    Result := T.FindElement(strClassName) <> Nil;
End;

(**

  This function returns true IF the given method name exists in the class.

  @precon  None.
  @postcon Returns true IF the given method name exists in the class.

  @param   M             as a TBaseLanguageModule
  @param   strClassName  as a String as a constant
  @param   strMethodName as a String as a constant
  @return  a Boolean

**)
Function TDUnitCreator.DoesMethodExist(M: TBaseLanguageModule;
  Const strClassName, strMethodName: String): Boolean;

Var
  T, C, L: TElementContainer;

Begin
  Result := False;
  T      := M.FindElement(strTypesLabel);
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

  @param   strProjectName as a String as a constant
  @return  a Boolean

**)
Function TDUnitCreator.DoesProjectExist(Const strProjectName: String): Boolean;

Var
  G: IOTAProjectGroup;
  i: Integer;

Begin
  Result := False;
  G      := TBADIToolsAPIFunctions.ProjectGroup;
  If G <> Nil Then
    For i := 0 To G.ProjectCount - 1 Do
      If CompareText(ExtractFileName(G.Projects[i].CurrentEditor.FileName),
        strProjectName) = 0 Then
        Begin
          Result := True;
          Exit;
        End;
End;

(**

  This method returns true if the indexed project contains a nuit with the given
  name.

  @precon  iProjecy must be a valid index into the project array.
  @postcon Returns true if the indexed project contains a nuit with the given
           name.

  @param   iProject    as an Integer
  @param   strUnitName as a String as a constant
  @return  a Boolean

**)
Function TDUnitCreator.DoesUnitExist(iProject: Integer; Const strUnitName: String): Boolean;

Var
  P: IOTAProject;
  i: Integer;

Begin
  Result := False;
  P      := FProjects[iProject];
  For i  := 0 To P.GetModuleCount - 1 Do
    If CompareText(ExtractFileName(P.GetModule(i).FileName), strUnitName) = 0 Then
      Begin
        Result := True;
        Exit;
      End;
End;

(**

  This method adds DUnit test projects to the project list.

  @precon  None.
  @postcon Adds DUnit test projects to the project list.

**)
Procedure TDUnitCreator.GetExistingDUnitProjects;

Var
  i: Integer;

Begin
  SetLength(FProjects, iCAPACITY);
  FProjectCount := 0;
  With TBADIToolsAPIFunctions.ProjectGroup Do
    Begin
      For i := 0 To ProjectCount - 1 Do
        If IsTestFramework(Projects[i]) Then
          AddProject(Projects[i]);
    End;
End;

(**

  This method adds DUnit test units to the project unit list.

  @precon  None .
  @postcon Adds DUnit test units to the project unit list .

  @param   iProject as an Integer

**)
Procedure TDUnitCreator.GetExistingDUnitUnits(iProject: Integer);
Var
  i: Integer;
Begin
  SetLength(FUnits, iCAPACITY);
  FUnitCount := 0;
  If iProject > -1 Then
    Begin
      For i := 0 To FProjects[iProject].GetModuleCount - 1 Do
        If Like('Test*', ExtractFileName(FProjects[iProject].GetModule(i).FileName)) Then
          AddUnit(FProjects[iProject].GetModule(i));
    End;
End;

(**

  This is a getter method for the Project property.

  @precon  iProject must be a valid index into the project array.
  @postcon Returns the indexed project.

  @param   iProject as an Integer
  @return  a String

**)
Function TDUnitCreator.GetProject(iProject: Integer): String;
Begin
  Result := IOTAProject(FProjects[iProject]).CurrentEditor.FileName;
End;

(**

  This is a getter method for the ProjectCount property.

  @precon  None.
  @postcon Returns the number of projects in the array.

  @return  an Integer

**)
Function TDUnitCreator.GetProjectCount: Integer;
Begin
  Result := FProjectCount;
End;

(**

  This is a getter method for the Unit property.

  @precon  iUnit must be a valid index into the Unit array.
  @postcon Returns the indexed Unit from the array.

  @param   iUnit as an Integer
  @return  a string

**)
Function TDUnitCreator.GetUnit(iUnit: Integer): String;
Begin
  Result := IOTAModuleInfo(FUnits[iUnit]).FileName;
End;

(**

  This is a getter method for the UnitCount property.

  @precon  None.
  @postcon Returns the number of Units in the array.

  @return  an Integer

**)
Function TDUnitCreator.GetUnitCount: Integer;
Begin
  Result := FUnitCount;
End;

(**

  This method raises error events for the caller to handle.

  @precon  None.
  @postcon Raises error events for the caller to handle.

  @param   strMsg as a String as a constant

**)
Procedure TDUnitCreator.RaiseError(Const strMsg: String);

Begin
  If Assigned(FErrors) Then
    FErrors(strMsg);
End;

(**

  This method updates an existing unit file with the newly selected test cases.

  @precon  None.
  @postcon Updates an existing unit file with the newly selected test cases.

  @param   iUnit             as an Integer
  @param   strUnitToBeTested as a String as a constant
  @param   slTestCases       as a TStringList
  @param   strBaseClass      as a String as a constant
  @param   strTestSuiteName  as a String as a constant
  @param   strClassNameMask  as a String as a constant
  @param   strMethodNameMask as a String as a constant

**)
Procedure TDUnitCreator.UpdateTestUnit(iUnit: Integer; Const strUnitToBeTested: String;
  slTestCases: TStringList; Const strBaseClass, strTestSuiteName, strClassNameMask,
  strMethodNameMask : String);

ResourceString
  strUnitReadOnly = 'Unit source module is read only!';
  strExistingUnitInterfaceNil = 'Existing Unit Interface is Nil!';

Var
  M : TBaseLanguageModule;
  SE: IOTASourceEditor;

Begin
  FUnit := FUnits[iUnit].OpenModule;
  FUnit.CurrentEditor.Show;
  SE := TBADIToolsAPIFunctions.SourceEditor(FUnit);
  CheckReadOnlyStatus(FUnit, strUnitReadOnly);
  SE.Show;
  If FUnit <> Nil Then
    Begin
      M := TBADIDispatcher.BADIDispatcher.Dispatcher(
        TBADIToolsAPIFunctions.EditorAsString(TBADIToolsAPIFunctions.SourceEditor(FUnit)),
        FUnit.FileName, FUnit.CurrentEditor.Modified, [moParse]);
      Try
        AddNewTestSuites(M, slTestCases, strTestSuiteName);
        AddNewTestImplementations(M, slTestCases, strClassNameMask, strMethodNameMask);
        AddNewTestClasses(M, slTestCases, strBaseClass, strClassNameMask);
        AddNewTestMethodsToClass(M, slTestCases);
        AddUnitToBeTestedToUsesClause(M, strUnitToBeTested);
      Finally
        M.Free;
      End;
    End
  Else
    RaiseError(strExistingUnitInterfaceNil);
End;

{ TProjectCreator }

(**

  This is a constructor for the TProjectCreator class.

  @precon  None.
  @postcon Sets the name of the new test project.

  @param   strNewProjectName as a String as a constant

**)
Constructor TProjectCreator.Create(Const strNewProjectName: String);
Begin
  FNewProjectName := strNewProjectName;
End;

(**

  This method sets the create type for the project.

  @precon  None.
  @postcon Sets the create type for the project.

  @return  a String

**)
Function TProjectCreator.GetCreatorType: String;
Begin
  Result := '' // Custom source code;
End;

(**

  This method determines that this project is new.

  @precon  None.
  @postcon Determines that this project is new.

  @return  a Boolean

**)
Function TProjectCreator.GetExisting: Boolean;
Begin
  Result := False;
End;

(**

  This method determines the projects file name.

  @precon  None.
  @postcon Determines the projects file name.

  @return  a string

**)
Function TProjectCreator.GetFileName: String;
Begin
  Result := GetCurrentDir + '\' + FNewProjectName;
End;

(**

  This method determines the default file system to be used.

  @precon  None.
  @postcon Determines the default file system to be used.

  @return  a String

**)
Function TProjectCreator.GetFileSystem: String;
Begin
  Result := '';
End;

(**

  This method determines the options file name.

  @precon  None.
  @postcon Determines the options file name.

  @return  a string

**)
Function TProjectCreator.GetOptionFileName: String;
Begin
  Result := '';
End;

(**

  This method determines the projects parent (project group).

  @precon  None.
  @postcon Determines the projects parent (project group).

  @return  an IOTAModule

**)
Function TProjectCreator.GetOwner: IOTAModule;
Begin
  Result := TBADIToolsAPIFunctions.ProjectGroup;
End;

{$IFDEF D2005}

(**

  This is a getter method for the Project Personality property.

  @precon  None.
  @postcon Returns the personality that the project create belongs to.

  @return  a String

**)
Function TProjectCreator.GetProjectPersonality: String;
Begin
  Result := sDelphiPersonality;
End;
{$ENDIF}

(**

  This method determines the project shouldn`t be shown.

  @precon  None.
  @postcon Determines the project shouldn`t be shown.

  @return  a Boolean

**)
Function TProjectCreator.GetShowSource: Boolean;
Begin
  Result := False;
End;

(**

  This method determines the file is unnamed.

  @precon  None.
  @postcon Determines the file is unnamed.

  @return  a Boolean

**)
Function TProjectCreator.GetUnnamed: Boolean;
Begin
  Result := True;
End;

(**

  This method determines that there are no new modules.

  @precon  None.
  @postcon Determines that there are no new modules.

**)
Procedure TProjectCreator.NewDefaultModule;
Begin
End;

{$IFDEF D0005}

(**

  This method is not used in this creator.

  @precon  None.
  @postcon None.

  @param   Project as an IOTAProject as a constant

**)
Procedure TProjectCreator.NewDefaultProjectModule(Const Project: IOTAProject);
Begin
End;
{$ENDIF}

(**

  This method determines that there are no new options.

  @precon  None.
  @postcon Determines that there are no new options.

  @param   ProjectName as a string  as a constant
  @return  an IOTAFile

**)
Function TProjectCreator.NewOptionSource(Const ProjectName: String): IOTAFile;
Begin
  Result := Nil;
End;

(**

  This method determines that there is no new project resource.

  @precon  None.
  @postcon Determines that there is no new project resource.

  @param   Project as an IOTAProject as a constant

**)
Procedure TProjectCreator.NewProjectResource(Const Project: IOTAProject);
Begin
End;

(**

  This method creates the new project source file from a resource stream.

  @precon  None.
  @postcon Creates the new project source file from a resource stream.

  @param   ProjectName as a string as a constant
  @return  an IOTAFile

**)
Function TProjectCreator.NewProjectSource(Const ProjectName: String): IOTAFile;

Begin
  Result := TProjectCreatorFile.Create(ProjectName);
End;

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
  @param   strClassNameMask  as a String
  @param   strMethodNameMask as a String

**)
Constructor TUnitCreator.Create(strNewUnitName, strUnitToBeTested: String;
  slTestCases: TStringList; Owner: IOTAProject; strBaseClass, strTestSuiteName,
  strClassNameMask, strMethodNameMask: String);

Begin
  FOwner          := Owner;
  FNewUnitName    := strNewUnitName;
  FUnitToBeTested := strUnitToBeTested;
  FTestCases      := slTestCases;
  FBaseClass      := strBaseClass;
  FTestSuiteName  := strTestSuiteName;
  FClassNameMask  := strClassNameMask;
  FMethodNameMask := strMethodNameMask;
End;

(**

  This method allow components to be added to a form.

  @precon  None.
  @postcon Not used in a unit.

  @param   FormEditor as an IOTAFormEditor as a constant

**)
Procedure TUnitCreator.FormCreated(Const FormEditor: IOTAFormEditor);
Begin
End;

(**

  This is a getter method for the AncestorName property.

  @precon  None.
  @postcon Not used in a unit.

  @return  a string

**)
Function TUnitCreator.GetAncestorName: String;
Begin
  Result := '';
End;

(**

  This is a getter method for the CreatorType property.

  @precon  None.
  @postcon Returns a type of Unit for a unit file (no form).

  @return  a string

**)
Function TUnitCreator.GetCreatorType: String;
Begin
  Result := sUnit;
End;

(**

  This is a getter method for the Existing property.

  @precon  None.
  @postcon Returns false to indicate a new unUnit.
  @return  a Boolean

**)
Function TUnitCreator.GetExisting: Boolean;
Begin
  Result := False;
End;

(**

  This is a getter method for the FileSystem property.

  @precon  None.
  @postcon Returns an empty string to signify the default filing system.

  @return  a string

**)
Function TUnitCreator.GetFileSystem: String;
Begin
  Result := '';
End;

(**

  This is a getter method for the FormName property.

  @precon  None.
  @postcon Not used in a unit file.

  @return  a string

**)
Function TUnitCreator.GetFormName: String;
Begin
  Result := '';
End;

(**

  This is a getter method for the ImplFileName property.

  @precon  None.
  @postcon Returns the fully qualified filename for the unit.

  @return  a string

**)
Function TUnitCreator.GetImplFileName: String;
Begin
  Result := GetCurrentDir + '\' + FNewUnitName;
End;

(**

  This is a getter method for the IntfFileName property.

  @precon  None.
  @postcon Not used in Delphi.

  @return  a string

**)
Function TUnitCreator.GetIntfFileName: String;
Begin
  Result := '';
End;

(**

  This is a getter method for the MainForm property.

  @precon  None.
  @postcon Not used in a unit file.

  @return  a Boolean

**)
Function TUnitCreator.GetMainForm: Boolean;
Begin
  Result := False;
End;

(**

  This is a getter method for the Owner property.

  @precon  None.
  @postcon Returns the owner (project) of this new unit.

  @return  an IOTAModule

**)
Function TUnitCreator.GetOwner: IOTAModule;
Begin
  Result := FOwner;
End;

(**

  This is a getter method for the ShowForm property.

  @precon  None.
  @postcon Not used in a unit file.

  @return  a Boolean

**)
Function TUnitCreator.GetShowForm: Boolean;
Begin
  Result := False;
End;

(**

  This is a getter method for the ShowSource property.

  @precon  None.
  @postcon Returns true to display the unit file source.

  @return  a Boolean

**)
Function TUnitCreator.GetShowSource: Boolean;
Begin
  Result := True;
End;

(**

  This is a getter method for the Unnamed property.

  @precon  None.
  @postcon Returns true to signify that the file is unsaved.

  @return  a Boolean

**)
Function TUnitCreator.GetUnnamed: Boolean;
Begin
  Result := True;
End;

(**

  This method generates the form file definition.

  @precon  None.
  @postcon Not used in a unit file.

  @param   FormIdent     as a string as a constant
  @param   AncestorIdent as a string as a constant
  @return  an IOTAFile

**)
Function TUnitCreator.NewFormFile(Const FormIdent, AncestorIdent: String): IOTAFile;
Begin
  Result := Nil;
End;

(**

  This method generates the source code for the unit.

  @precon  None.
  @postcon Generates the source code for this new unit file.

  @param   ModuleIdent   as a string as a constant
  @param   FormIdent     as a string as a constant
  @param   AncestorIdent as a string as a constant
  @return  an IOTAFile

**)
Function TUnitCreator.NewImplSource(Const ModuleIdent, FormIdent, AncestorIdent: String)
  : IOTAFile;
Begin
  Result := TUnitCreatorFile.Create(ModuleIdent, FUnitToBeTested, FTestCases, FBaseClass,
    FTestSuiteName, FClassNameMask, FMethodNameMask);
End;

(**

  This method generates the source code for an interface file.

  @precon  None.
  @postcon Not used in Delphi.

  @param   ModuleIdent   as a string as a constant
  @param   FormIdent     as a string as a constant
  @param   AncestorIdent as a string as a constant
  @return  an IOTAFile

**)
Function TUnitCreator.NewIntfSource(Const ModuleIdent, FormIdent, AncestorIdent: String)
  : IOTAFile;
Begin
  Result := Nil;
End;

{ TProjectCreatorFile }

(**

  This is a constructor for the TProjectCreatorFile class.

  @precon  None.
  @postcon Sets the project file name.

  @param   strProjectName as a String

**)
Constructor TProjectCreatorFile.Create(strProjectName: String);
Begin
  FProjectName := strProjectName;
End;

(**

  This is a getter method for the Age property.

  @precon  None.
  @postcon Returns an unknown date.

  @return  a TDateTime

**)
Function TProjectCreatorFile.GetAge: TDateTime;
Begin
  Result := -1;
End;

(**

  This is a getter method for the Source property.

  @precon  None.
  @postcon Returns the project source code with a parameter for the project name
           to be replaced.

  @return  a string

**)
Function TProjectCreatorFile.GetSource: String;

ResourceString
  strDUnitProjectTemplate = 'DUnitProjectSource';
  strTheDUnitProjectMsg = 'The DUnit Project Template ''%s'' was not found.';

Var
  Res: TResourceStream;
  {$IFDEF D2009}
  strTemp: AnsiString;
  {$ENDIF}
Begin
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
End;

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
  @param   strClassNameMask  as a String
  @param   strMethodNameMask as a String

**)
Constructor TUnitCreatorFile.Create(strUnitName, strUnitToBeTested: String;
  slTestCases: TStringList; strBaseClass, strTestSuiteName, strClassNameMask,
  strMethodNameMask: String);

Begin
  FUnitName       := strUnitName;
  FUnitToBeTested := strUnitToBeTested;
  FTestCases      := slTestCases;
  FBaseClass      := strBaseClass;
  FTestSuiteName  := strTestSuiteName;
  FClassNameMask  := strClassNameMask;
  FMethodNameMask := strMethodNameMask;
End;

(**

  This is a getter method for the Age property.

  @precon  None.
  @postcon Returns an invalid file age.

  @return  a TDateTime

**)
Function TUnitCreatorFile.GetAge: TDateTime;
Begin
  Result := -1;
End;

(**

  This is a getter method for the Source property.

  @precon  None.
  @postcon Returns a string representing the source code for the file.

  @return  a string

**)
Function TUnitCreatorFile.GetSource: String;

ResourceString
  strDUnitUnitTemplate = 'DUnitUnitSource';
  strTheDUnitUnitMsg = 'The DUnit Unit Template ''%s'' was not found.';

Var
  Res: TResourceStream;
  {$IFDEF D2009}
  strTemp: AnsiString;
  {$ENDIF}
Begin
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
  Result := StringReplace(Result, '$TESTCLASSES$', TestClasses(FTestCases, FBaseClass,
    FClassNameMask), []);
  Result := StringReplace(Result, '$TESTIMPLEMENTATION$', TestImplementation(FTestCases,
    FClassNameMask, FMethodNameMask), []);
  Result := StringReplace(Result, '$TESTSUITES$', TestSuites(FTestCases, FTestSuiteName), []);
End;

End.
