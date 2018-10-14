//: @stopdocumentation
Program BrowseAndDocItTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{ $APPTYPE CONSOLE}

{$INCLUDE '..\Source\CompilerDefinitions.inc'}

{$R 'BADIVerInfo.res' '..\BADIVerInfo.RC'}

uses
  DUnitTestRunner,
  TestInsight.DUnit,
  SysUtils,
  Forms,
  Windows,
  BADI.Initialisation in '..\Source\BADI.Initialisation.pas',
  BADI.Module.Dispatcher in '..\Source\BADI.Module.Dispatcher.pas',
  BADI.BackusNaur.Module in '..\Source\BADI.BackusNaur.Module.pas',
  BADI.CPP.Module in '..\Source\BADI.CPP.Module.pas',
  BADI.DFM.Module in '..\Source\BADI.DFM.Module.pas',
  BADI.Eidolon.Module in '..\Source\BADI.Eidolon.Module.pas',
  BADI.Eidolon.TLSSchematic.Module in '..\Source\BADI.Eidolon.TLSSchematic.Module.pas',
  BADI.INI.Module in '..\Source\BADI.INI.Module.pas',
  BADI.Pascal.Module in '..\Source\BADI.Pascal.Module.pas',
  BADI.VB.Module in '..\Source\BADI.VB.Module.pas',
  BADI.VB.ModuleFull in '..\Source\BADI.VB.ModuleFull.pas',
  BADI.XML.Module in '..\Source\BADI.XML.Module.pas',
  BADI.Options in '..\Source\BADI.Options.pas',
  BADI.Base.Module in '..\Source\BADI.Base.Module.pas',
  Test.BADI.Base.Module in 'Source\Test.BADI.Base.Module.pas',
  Test.BADI.VB.Module in 'Source\Test.BADI.VB.Module.pas',
  Test.BADI.VB.ModuleFull in 'Source\Test.BADI.VB.ModuleFull.pas',
  Test.BADI.Pascal.Module in 'Source\Test.BADI.Pascal.Module.pas',
  Test.BADI.Common.IDE.Functions in 'Source\Test.BADI.Common.IDE.Functions.pas',
  Test.BADI.BackusNaur.Module in 'Source\Test.BADI.BackusNaur.Module.pas',
  Test.BADI.Generic.Tokenizer in 'Source\Test.BADI.Generic.Tokenizer.pas',
  BADI.Generic.Tokenizer in '..\Source\BADI.Generic.Tokenizer.pas',
  Test.BADI.XML.Module in 'Source\Test.BADI.XML.Module.pas',
  Test.BADI.DFM.Module in 'Source\Test.BADI.DFM.Module.pas',
  Test.BADI.Eidolon.Module in 'Source\Test.BADI.Eidolon.Module.pas',
  Test.BADI.Module.Dispatcher in 'Source\Test.BADI.Module.Dispatcher.pas',
  BADI.CommonIDEFunctions in '..\Source\BADI.CommonIDEFunctions.pas',
  BADI.Eidolon.Types in '..\Source\BADI.Eidolon.Types.pas',
  BADI.Base.Container in '..\Source\BADI.Base.Container.pas',
  BADI.Comment in '..\Source\BADI.Comment.pas',
  BADI.Comment.Tag in '..\Source\BADI.Comment.Tag.pas',
  BADI.CompilerConditionData in '..\Source\BADI.CompilerConditionData.pas',
  BADI.CompilerConditionStack in '..\Source\BADI.CompilerConditionStack.pas',
  BADI.Constants in '..\Source\BADI.Constants.pas',
  BADI.DocIssue in '..\Source\BADI.DocIssue.pas',
  BADI.ElementContainer in '..\Source\BADI.ElementContainer.pas',
  BADI.Generic.Constant in '..\Source\BADI.Generic.Constant.pas',
  BADI.Generic.FunctionDecl in '..\Source\BADI.Generic.FunctionDecl.pas',
  BADI.Generic.MethodDecl in '..\Source\BADI.Generic.MethodDecl.pas',
  BADI.Generic.Parameter in '..\Source\BADI.Generic.Parameter.pas',
  BADI.Generic.PropertyDecl in '..\Source\BADI.Generic.PropertyDecl.pas',
  BADI.Generic.TypeDecl in '..\Source\BADI.Generic.TypeDecl.pas',
  BADI.Generic.Variable in '..\Source\BADI.Generic.Variable.pas',
  BADI.ModuleInfo in '..\Source\BADI.ModuleInfo.pas',
  BADI.ResourceStrings in '..\Source\BADI.ResourceStrings.pas',
  BADI.TickOption in '..\Source\BADI.TickOption.pas',
  BADI.TokenInfo in '..\Source\BADI.TokenInfo.pas',
  BADI.Types in '..\Source\BADI.Types.pas',
  BADI.Functions in '..\Source\BADI.Functions.pas',
  Test.BADI.TokenInfo in 'Source\Test.BADI.TokenInfo.pas',
  Test.BADI.Comment.Tag in 'Source\Test.BADI.Comment.Tag.pas',
  Test.BADI.Comment in 'Source\Test.BADI.Comment.pas',
  Test.BADI.ElementContainer in 'Source\Test.BADI.ElementContainer.pas',
  Test.BADI.DocIssue in 'Source\Test.BADI.DocIssue.pas',
  Test.BADI.Generic.TypeDecl in 'Source\Test.BADI.Generic.TypeDecl.pas',
  Test.BADI.Generic.Constant in 'Source\Test.BADI.Generic.Constant.pas',
  Test.BADI.Generic.Variable in 'Source\Test.BADI.Generic.Variable.pas',
  Test.BADI.Generic.Parameter in 'Source\Test.BADI.Generic.Parameter.pas',
  Test.BADI.Generic.MethodDecl in 'Source\Test.BADI.Generic.MethodDecl.pas',
  Test.BADI.Generic.PropertyDecl in 'Source\Test.BADI.Generic.PropertyDecl.pas',
  Test.BADI.DocumentConflicts in 'Source\Test.BADI.DocumentConflicts.pas',
  Test.BADI.LabelContainer in 'Source\Test.BADI.LabelContainer.pas',
  BADI.Pascal.Comment in '..\Source\BADI.Pascal.Comment.pas',
  BADI.Pascal.Types in '..\Source\BADI.Pascal.Types.pas',
  BADI.Pascal.IdentList in '..\Source\BADI.Pascal.IdentList.pas',
  BADI.Pascal.TempCntr in '..\Source\BADI.Pascal.TempCntr.pas',
  BADI.Pascal.RecordDecl in '..\Source\BADI.Pascal.RecordDecl.pas',
  BADI.Pascal.ParameterDecl in '..\Source\BADI.Pascal.ParameterDecl.pas',
  BADI.Pascal.MethodDecl in '..\Source\BADI.Pascal.MethodDecl.pas',
  BADI.Pascal.PropertyDecl in '..\Source\BADI.Pascal.PropertyDecl.pas',
  BADI.Pascal.TypeDecl in '..\Source\BADI.Pascal.TypeDecl.pas',
  BADI.Pascal.PropertySpec in '..\Source\BADI.Pascal.PropertySpec.pas',
  BADI.Pascal.ObjectDecl in '..\Source\BADI.Pascal.ObjectDecl.pas',
  BADI.Pascal.ClassDecl in '..\Source\BADI.Pascal.ClassDecl.pas',
  BADI.Pascal.InterfaceDecl in '..\Source\BADI.Pascal.InterfaceDecl.pas',
  BADI.Pascal.DispInterfaceDecl in '..\Source\BADI.Pascal.DispInterfaceDecl.pas',
  BADI.Pascal.ConstantDecl in '..\Source\BADI.Pascal.ConstantDecl.pas',
  BADI.Pascal.ResourceStringDecl in '..\Source\BADI.Pascal.ResourceStringDecl.pas',
  BADI.Pascal.VariableDecl in '..\Source\BADI.Pascal.VariableDecl.pas',
  BADI.Pascal.FieldDecl in '..\Source\BADI.Pascal.FieldDecl.pas',
  BADI.Pascal.ExportsItem in '..\Source\BADI.Pascal.ExportsItem.pas',
  BADI.Pascal.InitializationDecl in '..\Source\BADI.Pascal.InitializationDecl.pas',
  BADI.Pascal.FinalizationDecl in '..\Source\BADI.Pascal.FinalizationDecl.pas',
  BADI.Pascal.Constants in '..\Source\BADI.Pascal.Constants.pas',
  BADI.Pascal.Functions in '..\Source\BADI.Pascal.Functions.pas',
  BADI.Pascal.ResourceStrings in '..\Source\BADI.Pascal.ResourceStrings.pas',
  Test.BADI.Pascal.Comment in 'Source\Test.BADI.Pascal.Comment.pas',
  Test.BADI.Pascal.IdentList in 'Source\Test.BADI.Pascal.IdentList.pas',
  Test.BADI.Pascal.TypeDecl in 'Source\Test.BADI.Pascal.TypeDecl.pas',
  Test.BADI.Pascal.ArrayType in 'Source\Test.BADI.Pascal.ArrayType.pas',
  Test.BADI.Pascal.RecordDecl in 'Source\Test.BADI.Pascal.RecordDecl.pas',
  Test.BADI.Pascal.Parameter in 'Source\Test.BADI.Pascal.Parameter.pas',
  Test.BADI.Pascal.MethodDecl in 'Source\Test.BADI.Pascal.MethodDecl.pas',
  Test.BADI.Pascal.PropertyDecl in 'Source\Test.BADI.Pascal.PropertyDecl.pas',
  Test.BADI.Pascal.PropertySpec in 'Source\Test.BADI.Pascal.PropertySpec.pas',
  Test.BADI.Pascal.ObjectDecl in 'Source\Test.BADI.Pascal.ObjectDecl.pas',
  Test.BADI.Pascal.ClassDecl in 'Source\Test.BADI.Pascal.ClassDecl.pas',
  Test.BADI.Pascal.InterfaceDecl in 'Source\Test.BADI.Pascal.InterfaceDecl.pas',
  Test.BADI.Pascal.DispInterfaceDecl in 'Source\Test.BADI.Pascal.DispInterfaceDecl.pas',
  Test.BADI.Pascal.ConstantDecl in 'Source\Test.BADI.Pascal.ConstantDecl.pas',
  Test.BADI.Pascal.ResourceStringDecl in 'Source\Test.BADI.Pascal.ResourceStringDecl.pas',
  Test.BADI.Pascal.VariableDecl in 'Source\Test.BADI.Pascal.VariableDecl.pas',
  Test.BADI.Pascal.ThreadVariable in 'Source\Test.BADI.Pascal.ThreadVariable.pas',
  Test.BADI.Pascal.FieldDecl in 'Source\Test.BADI.Pascal.FieldDecl.pas',
  Test.BADI.Pascal.ExportsItem in 'Source\Test.BADI.Pascal.ExportsItem.pas',
  Test.BADI.Pascal.InitializationDecl in 'Source\Test.BADI.Pascal.InitializationDecl.pas',
  Test.BADI.Pascal.FinalizationDecl in 'Source\Test.BADI.Pascal.FinalizationDecl.pas',
  BADI.Pascal.ThreadVariableDecl in '..\Source\BADI.Pascal.ThreadVariableDecl.pas',
  BADI.Pascal.UsesList in '..\Source\BADI.Pascal.UsesList.pas',
  BADI.BackusNaur.Rule in '..\Source\BADI.BackusNaur.Rule.pas',
  BADI.BackusNaur.Comment in '..\Source\BADI.BackusNaur.Comment.pas',
  Test.BADI.BackusNaur.Comment in 'Source\Test.BADI.BackusNaur.Comment.pas',
  BADI.DFM.Types in '..\Source\BADI.DFM.Types.pas',
  BADI.DFM.ObjectDecl in '..\Source\BADI.DFM.ObjectDecl.pas',
  BADI.DFM.PropertyDecl in '..\Source\BADI.DFM.PropertyDecl.pas',
  BADI.DFM.Item in '..\Source\BADI.DFM.Item.pas',
  Test.BADI.DFM.ObjectDecl in 'Source\Test.BADI.DFM.ObjectDecl.pas',
  Test.BADI.DFM.PropertyDecl in 'Source\Test.BADI.DFM.PropertyDecl.pas',
  BADI.INI.Comment in '..\Source\BADI.INI.Comment.pas',
  BADI.INI.KeyValuePair in '..\Source\BADI.INI.KeyValuePair.pas',
  BADI.Eidolon.TLSSchematic.TLSShape in '..\Source\BADI.Eidolon.TLSSchematic.TLSShape.pas',
  BADI.Eidolon.TLSSchematic.TLSRoad in '..\Source\BADI.Eidolon.TLSSchematic.TLSRoad.pas',
  BADI.Eidolon.TLSSchematic.TLSObject in '..\Source\BADI.Eidolon.TLSSchematic.TLSObject.pas',
  BADI.Eidolon.TLSSchematic.SchematicSetting in '..\Source\BADI.Eidolon.TLSSchematic.SchematicSetting.pas',
  BADI.Eidolon.TLSSchematic.NoText in '..\Source\BADI.Eidolon.TLSSchematic.NoText.pas',
  BADI.Eidolon.TLSSchematic.Comment in '..\Source\BADI.Eidolon.TLSSchematic.Comment.pas',
  BADI.Eidolon.TLSSchematic.TLSStatic in '..\Source\BADI.Eidolon.TLSSchematic.TLSStatic.pas',
  BADI.Eidolon.TLSSchematic.ResourceStrings in '..\Source\BADI.Eidolon.TLSSchematic.ResourceStrings.pas',
  BADI.Eidolon.TLSSchematic.Constants in '..\Source\BADI.Eidolon.TLSSchematic.Constants.pas',
  BADI.XML.Comment in '..\Source\BADI.XML.Comment.pas',
  BADI.XML.BaseElement in '..\Source\BADI.XML.BaseElement.pas',
  BADI.XML.DocType in '..\Source\BADI.XML.DocType.pas',
  BADI.XML.XMLElement in '..\Source\BADI.XML.XMLElement.pas',
  BADI.XML.XMLElemDecl in '..\Source\BADI.XML.XMLElemDecl.pas',
  BADI.XML.XMLDecl in '..\Source\BADI.XML.XMLDecl.pas',
  BADI.XML.XMLPI in '..\Source\BADI.XML.XMLPI.pas',
  BADI.XML.XMLPERef in '..\Source\BADI.XML.XMLPERef.pas',
  BADI.XML.XMLIncludeElement in '..\Source\BADI.XML.XMLIncludeElement.pas',
  BADI.XML.XMLIgnoreElement in '..\Source\BADI.XML.XMLIgnoreElement.pas',
  BADI.XML.ResourceStrings in '..\Source\BADI.XML.ResourceStrings.pas',
  Test.BADI.XML.XMLDecl in 'Source\Test.BADI.XML.XMLDecl.pas',
  Test.BADI.XML.XMLDocType in 'Source\Test.BADI.XML.XMLDocType.pas',
  Test.BADI.XML.XMLElemDecl in 'Source\Test.BADI.XML.XMLElemDecl.pas',
  Test.BADI.XML.XMLElement in 'Source\Test.BADI.XML.XMLElement.pas',
  BADI.Eidolon.Comment in '..\Source\BADI.Eidolon.Comment.pas',
  BADI.Eidolon.Constants in '..\Source\BADI.Eidolon.Constants.pas',
  BADI.Eidolon.FieldDef in '..\Source\BADI.Eidolon.FieldDef.pas',
  BADI.Eidolon.BaseTable in '..\Source\BADI.Eidolon.BaseTable.pas',
  BADI.Eidolon.TextTable in '..\Source\BADI.Eidolon.TextTable.pas',
  BADI.Eidolon.DBConnection in '..\Source\BADI.Eidolon.DBConnection.pas',
  BADI.Eidolon.DBTable in '..\Source\BADI.Eidolon.DBTable.pas',
  BADI.Eidolon.OutputTable in '..\Source\BADI.Eidolon.OutputTable.pas',
  BADI.Eidolon.Association in '..\Source\BADI.Eidolon.Association.pas',
  BADI.Eidolon.RequirementsTable in '..\Source\BADI.Eidolon.RequirementsTable.pas',
  BADI.Eidolon.Symbol in '..\Source\BADI.Eidolon.Symbol.pas',
  BADI.Eidolon.TimeLocationTable in '..\Source\BADI.Eidolon.TimeLocationTable.pas',
  BADI.Eidolon.TextTableDef in '..\Source\BADI.Eidolon.TextTableDef.pas',
  BADI.Eidolon.DatabaseDef in '..\Source\BADI.Eidolon.DatabaseDef.pas',
  BADI.Eidolon.ConnectionDef in '..\Source\BADI.Eidolon.ConnectionDef.pas',
  BADI.Eidolon.TableNameDef in '..\Source\BADI.Eidolon.TableNameDef.pas',
  BADI.Eidolon.Line in '..\Source\BADI.Eidolon.Line.pas',
  BADI.Eidolon.CustomFillSymbol in '..\Source\BADI.Eidolon.CustomFillSymbol.pas',
  BADI.Eidolon.Rectangle in '..\Source\BADI.Eidolon.Rectangle.pas',
  BADI.Eidolon.Bar in '..\Source\BADI.Eidolon.Bar.pas',
  BADI.Eidolon.SuperBar in '..\Source\BADI.Eidolon.SuperBar.pas',
  BADI.Eidolon.Diamond in '..\Source\BADI.Eidolon.Diamond.pas',
  BADI.Eidolon.Triangle in '..\Source\BADI.Eidolon.Triangle.pas',
  BADI.Eidolon.Ellipse in '..\Source\BADI.Eidolon.Ellipse.pas',
  BADI.Eidolon.ResourceStrings in '..\Source\BADI.Eidolon.ResourceStrings.pas',
  BADI.Eidolon.Functions in '..\Source\BADI.Eidolon.Functions.pas',
  Test.BADI.Eidolon.Comment in 'Source\Test.BADI.Eidolon.Comment.pas',
  BADI.VB.Attribute in '..\Source\BADI.VB.Attribute.pas',
  BADI.VB.Comment in '..\Source\BADI.VB.Comment.pas',
  BADI.VB.ConstantDecl in '..\Source\BADI.VB.ConstantDecl.pas',
  BADI.VB.Constants in '..\Source\BADI.VB.Constants.pas',
  BADI.VB.EnumerateDecl in '..\Source\BADI.VB.EnumerateDecl.pas',
  BADI.VB.EnumIdent in '..\Source\BADI.VB.EnumIdent.pas',
  BADI.VB.EventDecl in '..\Source\BADI.VB.EventDecl.pas',
  BADI.VB.ExceptionHandling in '..\Source\BADI.VB.ExceptionHandling.pas',
  BADI.VB.FieldDecl in '..\Source\BADI.VB.FieldDecl.pas',
  BADI.VB.ImplementedItem in '..\Source\BADI.VB.ImplementedItem.pas',
  BADI.VB.Interfaces in '..\Source\BADI.VB.Interfaces.pas',
  BADI.VB.MethodDecl in '..\Source\BADI.VB.MethodDecl.pas',
  BADI.VB.Option in '..\Source\BADI.VB.Option.pas',
  BADI.VB.Parameter in '..\Source\BADI.VB.Parameter.pas',
  BADI.VB.PropertyDecl in '..\Source\BADI.VB.PropertyDecl.pas',
  BADI.VB.RecordDecl in '..\Source\BADI.VB.RecordDecl.pas',
  BADI.VB.ResourceStrings in '..\Source\BADI.VB.ResourceStrings.pas',
  BADI.VB.TypeDecl in '..\Source\BADI.VB.TypeDecl.pas',
  BADI.VB.Types in '..\Source\BADI.VB.Types.pas',
  BADI.VB.VariableDecl in '..\Source\BADI.VB.VariableDecl.pas',
  BADI.VB.Version in '..\Source\BADI.VB.Version.pas',
  Test.BADI.VB.ParameterDecl in 'Source\Test.BADI.VB.ParameterDecl.pas',
  Test.BADI.VB.MethodDecl in 'Source\Test.BADI.VB.MethodDecl.pas',
  Test.BADI.VB.ConstantDecl in 'Source\Test.BADI.VB.ConstantDecl.pas',
  Test.BADI.VB.VariableDecl in 'Source\Test.BADI.VB.VariableDecl.pas',
  Test.BADI.VB.PropertyDecl in 'Source\Test.BADI.VB.PropertyDecl.pas',
  Test.BADI.VB.RecordDecl in 'Source\Test.BADI.VB.RecordDecl.pas',
  Test.BADI.VB.TypeDecl in 'Source\Test.BADI.VB.TypeDecl.pas',
  Test.BADI.VB.Comment in 'Source\Test.BADI.VB.Comment.pas',
  Test.BADI.VB.ExceptionHandling in 'Source\Test.BADI.VB.ExceptionHandling.pas',
  Test.BADI.VB.EnumerateDecl in 'Source\Test.BADI.VB.EnumerateDecl.pas',
  Test.BADI.VB.EnumIdent in 'Source\Test.BADI.VB.EnumIdent.pas',
  Test.BADI.VB.FieldDecl in 'Source\Test.BADI.VB.FieldDecl.pas',
  Test.BADI.VB.Option in 'Source\Test.BADI.VB.Option.pas',
  Test.BADI.VB.Version in 'Source\Test.BADI.VB.Version.pas',
  Test.BADI.VB.Attribute in 'Source\Test.BADI.VB.Attribute.pas',
  BADI.Refactoring.Functions in '..\Source\BADI.Refactoring.Functions.pas',
  Test.BADI.Refactoring.Functions in 'Source\Test.BADI.Refactoring.Functions.pas',
  BADI.Interfaces in '..\Source\BADI.Interfaces.pas',
  BADI.IDEEditorColours in '..\Source\BADI.IDEEditorColours.pas';

{$R *.RES}

//Var
//  T : TTestResult;
//  iErrors : Integer;
//  lpMode : Cardinal;

begin
//  {$WARN SYMBOL_PLATFORM OFF}
//  {$IFDEF D2006}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
//  {$ENDIF}
//  {$IFDEF EUREKALOG}
//  SetEurekaLogState(DebugHook = 0);
//  {$ENDIF}
//  Application.Initialize;
//  If IsConsole Then
//    Begin
//      //: @debug JclDebug.RemoveIgnoredException(EAbort);
//      T := TextTestRunner.RunRegisteredTests;
//      Try
//        iErrors := T.FailureCount + T.ErrorCount;
//      Finally
//        T.Free;
//      End;
//      If DebugHook <> 0 Then                                           // Pause in IDE
//        If GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), lpMode) Then // Check redirection
//          Begin
//            Writeln('Press <Enter> to finish...');
//            Readln;
//          End;
//      If iErrors > 0 Then
//        Halt(iErrors);
//    End else
//      GUITestRunner.RunRegisteredTests;
  //RunRegisteredTests;
  TestInsight.DUnit.RunRegisteredTests();
end.



